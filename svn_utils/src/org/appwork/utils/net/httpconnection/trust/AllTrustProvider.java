/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.net.httpconnection.trust;

import java.security.cert.X509Certificate;

import javax.net.ssl.SSLSession;

import org.appwork.utils.net.httpconnection.IllegalSSLHostnameException;
import org.appwork.utils.net.httpconnection.TrustResult;
import org.appwork.utils.net.httpconnection.TrustResult.TrustType;

/**
 * Trust provider that accepts all certificates (no verification). Use only for testing or trusted environments.
 */
public class AllTrustProvider implements TrustProviderInterface {
    private static final AllTrustProvider INSTANCE = new AllTrustProvider();

    public static AllTrustProvider getInstance() {
        return INSTANCE;
    }

    private AllTrustProvider() {
    }

    @Override
    public TrustResult checkServerTrusted(final X509Certificate[] chain, final String authType, final Object context) {
        return createTrustResult(chain, TrustType.SERVER);
    }

    @Override
    public TrustResult checkClientTrusted(final X509Certificate[] chain, final String authType, final Object context) {
        return createTrustResult(chain, TrustType.CLIENT);
    }

    private TrustResult createTrustResult(final X509Certificate[] chain, TrustType trustType) {
        return new TrustResult(this, chain != null ? chain : new X509Certificate[0], null, trustType);
    }

    @Override
    public X509Certificate[] getAcceptedIssuers() {
        return new X509Certificate[0];
    }

    /**
     * TrustAllProvider accepts all hostnames without verification.
     */
    @Override
    public void verifyHostname(TrustResult result, final SSLSession session, final String host, final Object context) throws IllegalSSLHostnameException {
        // Accept all hostnames - no verification
    }

    /**
     * @return Provider ID for caching
     */
    @Override
    public String getId() {
        return "TrustAllProvider";
    }
}
