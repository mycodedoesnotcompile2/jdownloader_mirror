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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.appwork.utils.net.httpconnection.TrustResult;

/**
 * Extended SSLTrustInfo for CompositeTrustProvider that includes information about which internal providers failed before the successful
 * one.
 */
public class CompositeTrustResult extends TrustResult {
    private List<TrustResult> results;

    public List<TrustResult> getResults() {
        return results;
    }

    public List<TrustResult> getFails() {
        final List<TrustResult> ret = new ArrayList<TrustResult>();
        for (final TrustResult result : getResults()) {
            if (!result.isTrusted()) {
                ret.add(result);
            }
        }
        return ret;
    }

    public TrustResult getSuccess() {
        for (final TrustResult trust : getTrust()) {
            if (trust.isTrusted()) {
                return trust;
            }
        }
        return null;
    }

    public List<TrustResult> getTrust() {
        final List<TrustResult> ret = new ArrayList<TrustResult>();
        for (final TrustResult result : getResults()) {
            if (result.isTrusted()) {
                ret.add(result);
            }
        }
        return ret;
    }

    @Override
    public boolean isTrusted() {
        return super.isTrusted() && getSuccess() != null;
    }

    /**
     * Creates a CompositeSSLTrustInfo with information about failed providers.
     *
     * @param trustProvider
     *            The provider that successfully accepted the certificate (the actual delegate, not the composite)
     * @param trustedCertificateSubject
     *            Subject of the trusted certificate
     * @param trustedCertificateSerial
     *            Serial number of the trusted certificate
     * @param trustedCertificateSha256
     *            SHA-256 hash of the trusted certificate
     * @param failedProviders
     *            List of providers that failed before the successful one (may be empty, not null)
     */
    public CompositeTrustResult(final CompositeTrustProvider composite, final X509Certificate[] chain, final TrustType type, final TrustResult... results) {
        super(composite, chain, null, type);
        this.results = Collections.unmodifiableList(Arrays.asList(results));
    }
}
