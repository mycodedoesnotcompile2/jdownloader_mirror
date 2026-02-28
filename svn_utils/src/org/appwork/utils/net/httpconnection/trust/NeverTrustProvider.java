/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.net.httpconnection.trust;

import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;

import javax.net.ssl.SSLSession;

import org.appwork.utils.net.httpconnection.IllegalSSLHostnameException;
import org.appwork.utils.net.httpconnection.TrustResult;
import org.appwork.utils.net.httpconnection.TrustResult.TrustType;

/**
 * Trust provider that rejects all certificates. Use for testing or when no trust is desired.
 */
public class NeverTrustProvider implements TrustProviderInterface {
    private static final NeverTrustProvider INSTANCE = new NeverTrustProvider();

    public static NeverTrustProvider getInstance() {
        return INSTANCE;
    }

    private NeverTrustProvider() {
    }

    @Override
    public TrustResult checkServerTrusted(final X509Certificate[] chain, final String authType, final Object context) {
        return createTrustResult(chain, null, TrustType.SERVER);
    }

    @Override
    public TrustResult checkClientTrusted(final X509Certificate[] chain, final String authType, final Object context) {
        return createTrustResult(chain, null, TrustType.CLIENT);
    }

    private TrustResult createTrustResult(final X509Certificate[] chain, final Exception e, final TrustType trustType) {
        if (e == null) {
            return new TrustResult(this, chain != null ? chain : new X509Certificate[0], new CertificateException("TrustNeverProvider rejects all certificates"), trustType);
        } else {
            return new TrustResult(this, chain != null ? chain : new X509Certificate[0], e, trustType);
        }
    }

    @Override
    public X509Certificate[] getAcceptedIssuers() {
        return new X509Certificate[0];
    }

    @Override
    public void verifyHostname(TrustResult result, final SSLSession session, final String host, final Object context) throws IllegalSSLHostnameException {
        final IllegalSSLHostnameException e = new IllegalSSLHostnameException(host, "TrustNeverProvider does not verify hostnames");
        result.exception(e);
        throw e.setTrustResult(result);
    }

    @Override
    public String getId() {
        return "TrustNeverProvider";
    }
}
