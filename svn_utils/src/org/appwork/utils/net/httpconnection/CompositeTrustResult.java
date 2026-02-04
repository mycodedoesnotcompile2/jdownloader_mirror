/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.net.httpconnection;

import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.appwork.utils.net.httpconnection.trust.CompositeTrustProvider;

/**
 * Extended SSLTrustInfo for CompositeTrustProvider that includes information about which internal providers failed before the successful
 * one.
 */
public class CompositeTrustResult extends TrustResult {
    private List<TrustResult> fails;

    /**
     * @return the fails
     */
    public List<TrustResult> getFails() {
        return fails;
    }

    /**
     * @return the success
     */
    public TrustResult getSuccess() {
        return success;
    }

    private TrustResult success;

    /**
     * @see org.appwork.utils.net.httpconnection.TrustResult#isTrusted()
     */
    @Override
    public boolean isTrusted() {
        TrustResult ls=success;
        return ls != null && ls.isTrusted();
    }

    /**
     * @see org.appwork.utils.net.httpconnection.TrustResult#getException()
     */
    @Override
    public Exception getException() {
        if (success != null) {
            return null;
        }
        return fails.get(0).getException();
    }

    /**
     * @see org.appwork.utils.net.httpconnection.TrustResult#exception(java.lang.Exception)
     */
    @Override
    public void exception(Exception e) {
        if (success != null) {
            success.exception(e);
            ArrayList<TrustResult> newFails = new ArrayList<TrustResult>();
            newFails.add(success);
            newFails.addAll(fails);
            fails = Collections.unmodifiableList(newFails);
            success = null;
        } else {
            super.exception(e);
        }
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
    public CompositeTrustResult(final CompositeTrustProvider composite, TrustResult success, X509Certificate[] chain, final List<TrustResult> fails, TrustType type) {
        super(composite, chain, success == null ? fails.get(0).getException() : null, type);
        this.fails = Collections.unmodifiableList(fails);
        this.success = success;
    }
}
