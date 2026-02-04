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

/**
 * Interface for SSL trust providers. Implementations supply TrustManagers for SSL/TLS connections.
 */
public interface TrustProviderInterface {
    /**
     * @return Array of accepted issuer certificates
     */
    X509Certificate[] getAcceptedIssuers();

    /**
     * Checks if the server certificate chain is trusted.
     *
     * @param chain
     *            Certificate chain
     * @param authType
     *            Authentication type
     * @param context
     *            Context (Socket, SSLEngine, or null)
     * @return SSLTrustInfo if trusted, null or throws CertificateException if not trusted
     * @throws CertificateException
     *             if certificate is not trusted
     */
    TrustResult checkServerTrusted(X509Certificate[] chain, String authType, Object context);

    /**
     * Checks if the client certificate chain is trusted.
     *
     * @param chain
     *            Certificate chains
     * @param authType
     *            Authentication type
     * @param context
     *            Context (Socket, SSLEngine, or null) - may be null for java 1.6 or below
     * @return SSLTrustInfo if trusted, null or throws CertificateException if not trusted
     * @throws CertificateException
     *             if certificate is not trusted
     */
    TrustResult checkClientTrusted(X509Certificate[] chain, String authType, Object context);

    /**
     * @return Provider ID for caching/identification
     */
    String getId();

    /**
     * @param session
     * @param host
     * @param context
     *            may be the native urlconnection
     */
    void verifyHostname(SSLSession session, String host, Object context) throws IllegalSSLHostnameException;
}
