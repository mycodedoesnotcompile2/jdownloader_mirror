/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.net.httpconnection.trust;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLSession;

import org.appwork.JNAHelper;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils;
import org.appwork.utils.net.httpconnection.IllegalSSLHostnameException;
import org.appwork.utils.net.httpconnection.NativeHTTPConnectionImpl.HostnameVerifiedDelegate;
import org.appwork.utils.net.httpconnection.TrustResult;
import org.appwork.utils.net.httpconnection.TrustResult.TrustType;
import org.appwork.utils.os.CrossSystem;

import com.sun.jna.Memory;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.platform.win32.Crypt32;
import com.sun.jna.platform.win32.WTypes.LPSTR;
import com.sun.jna.platform.win32.WinCrypt;
import com.sun.jna.platform.win32.WinCrypt.CERT_CHAIN_CONTEXT;
import com.sun.jna.platform.win32.WinCrypt.CERT_CHAIN_ELEMENT;
import com.sun.jna.platform.win32.WinCrypt.CERT_CHAIN_PARA;
import com.sun.jna.platform.win32.WinCrypt.CERT_CHAIN_POLICY_PARA;
import com.sun.jna.platform.win32.WinCrypt.CERT_CHAIN_POLICY_STATUS;
import com.sun.jna.platform.win32.WinCrypt.CERT_CONTEXT;
import com.sun.jna.platform.win32.WinCrypt.CERT_SIMPLE_CHAIN;
import com.sun.jna.platform.win32.WinCrypt.HCERTCHAINENGINE;
import com.sun.jna.platform.win32.WinCrypt.HCERTSTORE;
import com.sun.jna.ptr.PointerByReference;

/**
 * Trust provider that delegates each validation to the Windows native certificate chain API (CertGetCertificateChain +
 * CertVerifyCertificateChainPolicy) instead of copying the Windows trust store. No reload is needed when the Windows store changes; every
 * validate call uses the current state of the Windows ROOT store (user + machine).
 * <p>
 * Implements {@link TrustProviderInterface} directly (no {@link AbstractTrustProvider} / TrustManager delegate): the SSL stack uses
 * {@link org.appwork.utils.net.httpconnection.trust.bridge.TrustBridge}, which calls {@link #checkServerTrusted} /
 * {@link #checkClientTrusted} on this provider.
 * </p>
 * <p>
 * Requires JNA and runs only on Windows. Uses the same trust semantics as {@link WindowsTrustProvider} but without caching a KeyStore
 * snapshot.
 * </p>
 *
 * <h3>JNA / Windows Crypto API (crypt32.dll)</h3>
 * <p>
 * Validation is implemented via JNA calls into Windows crypt32.dll. The flow mirrors what the Windows SSL stack does:
 * </p>
 * <ol>
 * <li>Convert the Java leaf certificate (chain[0]) into a Windows CERT_CONTEXT via CertCreateCertificateContext.</li>
 * <li>Optionally provide intermediate certs in a temporary in-memory store so the chain builder can use them.</li>
 * <li>Call CertGetCertificateChain with the default engine (null = current user + local machine ROOT). Windows builds the chain from leaf
 * to a trusted root.</li>
 * <li>Call CertVerifyCertificateChainPolicy with policy CERT_CHAIN_POLICY_SSL to apply SSL-specific checks (e.g. usage, revocation).</li>
 * <li>If policyStatus.dwError is 0, the chain is trusted; otherwise we return an untrusted result.</li>
 * </ol>
 * <p>
 * All native handles (CERT_CONTEXT, CERT_CHAIN_CONTEXT, HCERTSTORE) are freed in a finally block to avoid leaks.
 * </p>
 *
 * <h3>Welche CA wurde für die Validierung verwendet?</h3>
 * <p>
 * Die Windows-API liefert die gebaute Kette inkl. der vertrauenswürdigen Root-CA. In <code>CERT_CHAIN_CONTEXT</code> gilt:
 * <code>rgpChain[0]</code> ist die erste „simple chain“ (vom Endzertifikat bis zur Root); das <b>letzte Element</b> dieser Kette ist die
 * Root-CA, gegen die erfolgreich validiert wurde. Jedes Element ist ein <code>CERT_CHAIN_ELEMENT</code> mit <code>pCertContext</code>
 * (CERT_CONTEXT mit <code>pbCertEncoded</code> / <code>cbCertEncoded</code>).
 * </p>
 * <p>
 * Über JNA: <code>chainContext.getRgpChain()[0]</code> liefert die erste Kette, das letzte Element darin enthält die Root. Aus dessen
 * <code>pCertContext</code> kann man die DER-Bytes lesen und per <code>CertificateFactory.generateCertificate()</code> ein
 * <code>X509Certificate</code> erzeugen. Diese Klasse liefert die Root derzeit nicht mit (z. B. in TrustResult); bei Bedarf kann vor dem
 * Freigeben des <code>CERT_CHAIN_CONTEXT</code> die Root ausgelesen und zurückgegeben werden.
 * </p>
 */
public class JNAWindowsTrustProvider implements TrustProviderInterface {
    /**
     * Windows policy for SSL/TLS server certificate chain verification. Passed to CertVerifyCertificateChainPolicy();
     * ensures the chain is valid for server authentication (usage, revocation, trusted root). Value 4 = (LPCSTR)4 in wincrypt.h.
     */
    private static final int                     CERT_CHAIN_POLICY_SSL = 4;
    private static final int                     CERT_STORE_ADD_ALWAYS = 4;
    private static final JNAWindowsTrustProvider INSTANCE              = new JNAWindowsTrustProvider();
    protected JNAWindowsTrustResult              latestTrustResult     = null;

    public static JNAWindowsTrustProvider getInstance() {
        return INSTANCE;
    }

    public JNAWindowsTrustProvider() {
    }

    @Override
    public TrustResult checkServerTrusted(final X509Certificate[] chain, final String authType, final Object context) {
        try {
            ensureWindowsAndJNA();
            final X509Certificate trustedRoot = validateChainViaWindows(chain);
            return createTrustInfo(chain, null, TrustType.SERVER, trustedRoot);
        } catch (final Exception e) {
            return createTrustInfo(chain, e, TrustType.SERVER, null);
        }
    }

    @Override
    public TrustResult checkClientTrusted(final X509Certificate[] chain, final String authType, final Object context) {
        try {
            ensureWindowsAndJNA();
            final X509Certificate trustedRoot = validateChainViaWindows(chain);
            return createTrustInfo(chain, null, TrustType.CLIENT, trustedRoot);
        } catch (final Exception e) {
            return createTrustInfo(chain, e, TrustType.CLIENT, null);
        }
    }

    @Override
    public X509Certificate[] getAcceptedIssuers() {
        return new X509Certificate[0];
    }

    @Override
    public String getId() {
        return "JNAWindowsTrustProvider";
    }

    @Override
    public void verifyHostname(final SSLSession session, final String host, final Object context) throws IllegalSSLHostnameException {
        try {
            HostnameVerifier nativeVerifier = null;
            if (context instanceof HttpsURLConnection) {
                nativeVerifier = ((HttpsURLConnection) context).getHostnameVerifier();
            }
            if (nativeVerifier instanceof HostnameVerifiedDelegate) {
                nativeVerifier = null;
            }
            if (!Boolean.TRUE.equals(HTTPConnectionUtils.verifySSLHostname(nativeVerifier, session, host))) {
                throw new IllegalSSLHostnameException(host, "Failed");
            }
        } catch (final IllegalSSLHostnameException e) {
            throw e.setTrustResult(latestTrustResult);
        } catch (final IOException e) {
            throw new IllegalSSLHostnameException(host, e).setTrustResult(latestTrustResult);
        }
    }

    private static void ensureWindowsAndJNA() throws CertificateException {
        if (!CrossSystem.isWindows()) {
            throw new CertificateException("JNAWindowsTrustProvider is only available on Windows");
        }
        if (!JNAHelper.isJNAAvailable()) {
            throw new CertificateException("JNAWindowsTrustProvider requires JNA");
        }
    }

    private JNAWindowsTrustResult createTrustInfo(final X509Certificate[] chain, final Exception e, final TrustType type, final X509Certificate trustedRoot) {
        if (chain == null || chain.length == 0) {
            throw new org.appwork.exceptions.WTFException();
        }
        return latestTrustResult = new JNAWindowsTrustResult(this, chain, e, type, trustedRoot);
    }

    /**
     * Validates the certificate chain using the Windows Crypto API (crypt32.dll) via JNA. Uses the current Windows ROOT store (user +
     * machine); no snapshot, so store changes are visible immediately.
     *
     * <h4>JNA / Windows API flow</h4>
     * <ol>
     * <li><b>CertCreateCertificateContext</b> ({@link WindowsCertChainCrypt32Api}): Builds a Windows CERT_CONTEXT from the leaf cert's DER bytes. The context is
     * not stored anywhere; it is only used as input for the chain builder. Must be freed with CertFreeCertificateContext.</li>
     * <li><b>openMemoryStoreAndAddIntermediates</b>: Optionally creates an in-memory store (CERT_STORE_PROV_MEMORY) and adds chain[1..n-1]
     * so CertGetCertificateChain can resolve the full chain. If this fails or is skipped, Windows may still build the chain using AIA or
     * only the leaf (and fail if the root is not in ROOT).</li>
     * <li><b>CertGetCertificateChain</b> (Crypt32): Builds a chain from the leaf to a trusted root. We pass hChainEngine=null (default
     * engine = HCCE_CURRENT_USER, which uses the current user's and local machine's ROOT stores), pTime=null (current time),
     * hAdditionalStore=our memory store with intermediates if any. The result is a CERT_CHAIN_CONTEXT; must be freed with
     * CertFreeCertificateChain.</li>
     * <li><b>CertVerifyCertificateChainPolicy</b> (Crypt32): Applies the SSL policy (CERT_CHAIN_POLICY_SSL = (LPCSTR)4) to the chain. We
     * pass CERT_CHAIN_POLICY_PARA and CERT_CHAIN_POLICY_STATUS with cbSize set (required by the API). On return, policyStatus.dwError is 0
     * if the chain is valid for SSL; otherwise it contains the error code (e.g. untrusted root, revoked, wrong usage).</li>
     * <li><b>Cleanup</b>: In finally we free the cert context, the chain context, and the optional additional store so that no native
     * handles leak.</li>
     * </ol>
     * <p>
     * <b>Welche CA wurde verwendet?</b> Die gebaute Kette (CERT_CHAIN_CONTEXT) enthält die Root-CA: erste Simple-Chain
     * <code>rgpChain[0]</code>, letztes Element = vertrauenswürdige Root. Diese wird ausgelesen und in
     * {@link JNAWindowsTrustResult#getTrustedRootCertificate()} zurückgegeben.
     * </p>
     *
     * @return the trusted root certificate from the built chain, or null if extraction fails
     */
    private static X509Certificate validateChainViaWindows(final X509Certificate[] chain) throws CertificateException {
        if (chain == null || chain.length == 0) {
            throw new CertificateException("Certificate chain is null or empty");
        }
        CERT_CONTEXT.ByReference pCertContext = null;
        PointerByReference ppChainContext = new PointerByReference();
        HCERTSTORE hAdditionalStore = null;
        final X509Certificate[] trustedRootHolder = new X509Certificate[1];
        try {
            // 1) Leaf cert → Windows CERT_CONTEXT (JNA: Memory buffer + CertCreateCertificateContext)
            byte[] leafEncoded = chain[0].getEncoded();
            Memory buf = new Memory(leafEncoded.length);
            buf.write(0, leafEncoded, 0, leafEncoded.length);
            pCertContext = WindowsCertChainCrypt32Api.INSTANCE.CertCreateCertificateContext(WinCrypt.X509_ASN_ENCODING | WinCrypt.PKCS_7_ASN_ENCODING, buf, leafEncoded.length);
            if (pCertContext == null || pCertContext.getPointer() == null) {
                throw new CertificateException("CertCreateCertificateContext failed: " + com.sun.jna.Native.getLastError());
            }
            // 2) Optional: in-memory store with intermediate certs for chain building
            hAdditionalStore = openMemoryStoreAndAddIntermediates(chain);
            // 3) Build chain (default engine = current user + machine ROOT)
            CERT_CHAIN_PARA chainPara = new CERT_CHAIN_PARA();
            chainPara.cbSize = chainPara.size();
            chainPara.write();
            boolean ok = Crypt32.INSTANCE.CertGetCertificateChain((HCERTCHAINENGINE) null, pCertContext, null, hAdditionalStore, chainPara, 0, null, ppChainContext);
            if (!ok || ppChainContext.getValue() == null) {
                int err = com.sun.jna.Native.getLastError();
                throw new CertificateException("CertGetCertificateChain failed: " + err);
            }
            CERT_CHAIN_CONTEXT chainContext = Structure.newInstance(CERT_CHAIN_CONTEXT.class, ppChainContext.getValue());
            chainContext.read();
            // 4) SSL policy verification: CERT_CHAIN_POLICY_SSL validates the chain for server auth (usage, revocation, trusted root)
            CERT_CHAIN_POLICY_PARA policyPara = new CERT_CHAIN_POLICY_PARA();
            policyPara.cbSize = policyPara.size();
            policyPara.write();
            CERT_CHAIN_POLICY_STATUS policyStatus = new CERT_CHAIN_POLICY_STATUS();
            policyStatus.cbSize = policyStatus.size();
            policyStatus.write();
            ok = Crypt32.INSTANCE.CertVerifyCertificateChainPolicy(new LPSTR(Pointer.createConstant(CERT_CHAIN_POLICY_SSL)), chainContext, policyPara, policyStatus);
            policyStatus.read();
            if (!ok) {
                throw new CertificateException("CertVerifyCertificateChainPolicy failed");
            }
            if (policyStatus.dwError != 0) {
                throw new CertificateException("Certificate chain policy validation failed: " + policyStatus.dwError);
            }
            // Extract trusted root from chain (copy DER before finally frees the context)
            trustedRootHolder[0] = extractTrustedRootFromChain(chainContext);
        } finally {
            // 5) Free all native handles (required to avoid leaks)
            if (pCertContext != null && pCertContext.getPointer() != null) {
                Crypt32.INSTANCE.CertFreeCertificateContext(pCertContext);
            }
            Pointer pChain = ppChainContext.getValue();
            if (pChain != null) {
                CERT_CHAIN_CONTEXT ctx = Structure.newInstance(CERT_CHAIN_CONTEXT.class, pChain);
                Crypt32.INSTANCE.CertFreeCertificateChain(ctx);
            }
            if (hAdditionalStore != null) {
                Crypt32.INSTANCE.CertCloseStore(hAdditionalStore, 0);
            }
        }
        return trustedRootHolder[0];
    }

    /**
     * Reads the trusted root certificate from the first simple chain (last element) of the Windows-built CERT_CHAIN_CONTEXT. The DER bytes
     * are copied so the result is valid after the chain context is freed.
     *
     * @param chainContext
     *            the chain context after successful policy verification
     * @return the root X509Certificate, or null if the structure cannot be read or parsing fails
     */
    private static X509Certificate extractTrustedRootFromChain(final CERT_CHAIN_CONTEXT chainContext) {
        if (chainContext == null || chainContext.cChain == 0 || chainContext.rgpChain == null) {
            return null;
        }
        try {
            Pointer pFirstChain = chainContext.rgpChain.getPointer(0);
            CERT_SIMPLE_CHAIN firstChain = Structure.newInstance(CERT_SIMPLE_CHAIN.class, pFirstChain);
            firstChain.read();
            if (firstChain.cElement == 0 || firstChain.rgpElement == null) {
                return null;
            }
            int lastIdx = firstChain.cElement - 1;
            Pointer pLastElem = firstChain.rgpElement.getPointer(lastIdx * Native.POINTER_SIZE);
            CERT_CHAIN_ELEMENT lastElem = Structure.newInstance(CERT_CHAIN_ELEMENT.class, pLastElem);
            lastElem.read();
            if (lastElem.pCertContext == null || lastElem.pCertContext.getPointer() == null) {
                return null;
            }
            CERT_CONTEXT ctx = Structure.newInstance(CERT_CONTEXT.class, lastElem.pCertContext.getPointer());
            ctx.read();
            if (ctx.pbCertEncoded == null || ctx.cbCertEncoded <= 0) {
                return null;
            }
            byte[] der = ctx.pbCertEncoded.getByteArray(0, ctx.cbCertEncoded);
            return (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate(new ByteArrayInputStream(der));
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Optionally opens a Windows in-memory certificate store and adds the intermediate certificates (chain[1..n-1]) so that
     * CertGetCertificateChain can use them when building the chain. If the store cannot be opened (e.g. MEMORY provider not available in
     * this JNA version) or chain has only one cert, returns null; validation then relies on the leaf only and Windows ROOT (and possibly
     * AIA).
     *
     * <h4>JNA / Windows API</h4>
     * <ul>
     * <li><b>CertOpenStore</b> (Crypt32): Opens a store with provider CERT_STORE_PROV_MEMORY (name "Memory"). No persistent storage; certs
     * are only in process memory. pvPara is null.</li>
     * <li><b>CertAddEncodedCertificateToStore</b> ({@link WindowsCertChainCrypt32Api}): Adds each intermediate's DER bytes to the store with CERT_STORE_ADD_ALWAYS
     * so duplicates do not cause errors. ppCertContext is null (we don't need the created context back).</li>
     * </ul>
     * <p>
     * The caller must call CertCloseStore(hStore, 0) when done (done in validateChainViaWindows finally).
     * </p>
     */
    private static HCERTSTORE openMemoryStoreAndAddIntermediates(final X509Certificate[] chain) {
        if (chain == null || chain.length <= 1) {
            return null;
        }
        try {
            Object memoryProvider = WinCrypt.CertStoreProviderName.class.getField("MEMORY").get(null);
            HCERTSTORE hStore = Crypt32.INSTANCE.CertOpenStore((com.sun.jna.platform.win32.WinCrypt.CertStoreProviderName) memoryProvider, 0, null, 0, (Pointer) null);
            if (hStore == null) {
                return null;
            }
            for (int i = 1; i < chain.length; i++) {
                byte[] encoded = chain[i].getEncoded();
                Memory buf = new Memory(encoded.length);
                buf.write(0, encoded, 0, encoded.length);
                WindowsCertChainCrypt32Api.INSTANCE.CertAddEncodedCertificateToStore(hStore, WinCrypt.X509_ASN_ENCODING | WinCrypt.PKCS_7_ASN_ENCODING, buf, encoded.length, CERT_STORE_ADD_ALWAYS, null);
            }
            return hStore;
        } catch (Throwable t) {
            return null;
        }
    }
}
