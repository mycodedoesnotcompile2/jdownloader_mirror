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

import org.appwork.utils.net.httpconnection.TrustResult;

/**
 * {@link TrustResult} returned by {@link JNAWindowsTrustProvider} when validation is done via the Windows Crypto API.
 * <p>
 * When validation succeeded, {@link #getTrustedRootCertificate()} returns the root CA certificate from the Windows-built
 * chain that was used to establish trust (the last element of the first simple chain in {@code CERT_CHAIN_CONTEXT}).
 * When validation failed, it returns {@code null}.
 * </p>
 */
public class JNAWindowsTrustResult extends TrustResult {

    private final X509Certificate trustedRootCertificate;

    /**
     * @param trustProvider the provider that produced this result
     * @param chain         the certificate chain that was validated
     * @param exception     null if trusted, otherwise the validation error
     * @param type         SERVER or CLIENT
     * @param trustedRootCertificate the root CA that granted trust, or null if validation failed
     */
    public JNAWindowsTrustResult(final TrustProviderInterface trustProvider, final X509Certificate[] chain, final Exception exception, final TrustResult.TrustType type, final X509Certificate trustedRootCertificate) {
        super(trustProvider, chain, exception, type);
        this.trustedRootCertificate = trustedRootCertificate;
    }

    /**
     * The root CA certificate from the Windows chain that was used to establish trust.
     *
     * @return the trusted root certificate, or {@code null} if validation failed or the root could not be determined
     */
    public X509Certificate getTrustedRootCertificate() {
        return trustedRootCertificate;
    }
}
