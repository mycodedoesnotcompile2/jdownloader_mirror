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
import java.security.KeyStore;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;

import javax.net.ssl.SSLException;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509TrustManager;

import org.appwork.JNAHelper;
import org.appwork.utils.os.CrossSystem;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.PointerByReference;

public class JNAMacTrustProvider extends AbstractTrustProvider {
    protected interface SecurityFramework extends Library {
        final static SecurityFramework INSTANCE                     = Native.load("Security", SecurityFramework.class);
        final int                      kSecTrustSettingsDomainUser  = 0;
        final int                      kSecTrustSettingsDomainAdmin = 1;

        int SecTrustSettingsCopyCertificates(int domain, PointerByReference certArrayOut);

        Pointer SecCertificateCopyData(Pointer certRef);
    }

    protected interface CoreFoundation extends Library {
        final static CoreFoundation INSTANCE = Native.load("CoreFoundation", CoreFoundation.class);

        long CFArrayGetCount(Pointer arrayRef);

        Pointer CFArrayGetValueAtIndex(Pointer arrayRef, long index);

        long CFDataGetLength(Pointer dataRef);

        Pointer CFDataGetBytePtr(Pointer dataRef);

        void CFRelease(Pointer cfTypeRef);
    }

    private static final JNAMacTrustProvider INSTANCE = new JNAMacTrustProvider();
    private volatile X509TrustManager        trustManager;
    private volatile KeyStore                trustStore;

    public static JNAMacTrustProvider getInstance() {
        return INSTANCE;
    }

    public JNAMacTrustProvider() {
    }

    public void reload() {
        super.reload();
        trustManager = null;
        trustStore = null;
    }

    protected KeyStore loadMacTrustStore() throws Exception {
        final KeyStore ret = KeyStore.getInstance(KeyStore.getDefaultType());
        ret.load(null, null);
        final CertificateFactory certFactory = CertificateFactory.getInstance("X.509");
        final SecurityFramework sec = SecurityFramework.INSTANCE;
        final CoreFoundation cf = CoreFoundation.INSTANCE;
        final int[] domains = { SecurityFramework.kSecTrustSettingsDomainUser, SecurityFramework.kSecTrustSettingsDomainAdmin };
        int aliasCounter = 0;
        for (int domain : domains) {
            final PointerByReference certArrayRef = new PointerByReference();
            final int status = sec.SecTrustSettingsCopyCertificates(domain, certArrayRef);
            if (status == 0 && certArrayRef.getValue() != null) {
                final Pointer certArray = certArrayRef.getValue();
                try {
                    final long count = cf.CFArrayGetCount(certArray);
                    for (long i = 0; i < count; i++) {
                        final Pointer certRef = cf.CFArrayGetValueAtIndex(certArray, i);
                        final Pointer dataRef = sec.SecCertificateCopyData(certRef);
                        if (dataRef != null) {
                            final long length = cf.CFDataGetLength(dataRef);
                            final Pointer bytePtr = cf.CFDataGetBytePtr(dataRef);
                            final byte[] certBytes = bytePtr.getByteArray(0, (int) length);
                            final X509Certificate cert = (X509Certificate) certFactory.generateCertificate(new ByteArrayInputStream(certBytes));
                            ret.setCertificateEntry("mac-trust-" + (++aliasCounter), cert);
                            cf.CFRelease(dataRef);
                        }
                    }
                } finally {
                    cf.CFRelease(certArray);
                }
            }
        }
        return ret;
    }

    protected void ensureEnvironment() throws UnsupportedOperationException {
        if (!CrossSystem.isMac()) {
            throw new UnsupportedOperationException(getId() + " is only available on macOS");
        }
        if (!JNAHelper.isJNAAvailable()) {
            throw new UnsupportedOperationException(getId() + " requires JNA");
        }
    }

    @Override
    public X509TrustManager getTrustManager() throws SSLException, UnsupportedOperationException {
        ensureEnvironment();
        if (trustManager == null) {
            synchronized (this) {
                if (trustManager == null) {
                    try {
                        final KeyStore ks = getTrustStore();
                        final TrustManagerFactory tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
                        tmf.init(ks);
                        final TrustManager[] managers = tmf.getTrustManagers();
                        if (managers != null) {
                            for (final TrustManager m : managers) {
                                if (m instanceof X509TrustManager) {
                                    trustManager = (X509TrustManager) m;
                                    break;
                                }
                            }
                        }
                        if (trustManager == null) {
                            throw new SSLException("No X509TrustManager from macOS Keychain");
                        }
                    } catch (final SSLException e) {
                        throw e;
                    } catch (final Exception e) {
                        throw new SSLException(e);
                    }
                }
            }
        }
        return trustManager;
    }

    @Override
    public KeyStore getTrustStore() throws SSLException {
        if (trustStore == null) {
            synchronized (this) {
                if (trustStore == null) {
                    try {
                        trustStore = loadMacTrustStore();
                    } catch (SSLException e) {
                        throw e;
                    } catch (Exception e) {
                        throw new SSLException(e);
                    }
                }
            }
        }
        return trustStore;
    }
}
