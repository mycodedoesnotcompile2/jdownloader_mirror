/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.net.httpconnection.trust;

import java.security.KeyStore;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.net.ssl.SSLException;
import javax.net.ssl.SSLSession;
import javax.net.ssl.X509TrustManager;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.utils.net.httpconnection.IllegalSSLHostnameException;
import org.appwork.utils.net.httpconnection.TrustResult;
import org.appwork.utils.net.httpconnection.TrustResult.TrustType;

/**
 * Composite trust provider that tries delegates in order; the first to accept a certificate wins.
 * <p>
 * Returns {@link CompositeTrustResult} which contains:
 * <ul>
 * <li>The actual delegate provider that successfully accepted the certificate (not the composite itself)</li>
 * <li>A list of providers that failed before the successful one</li>
 * </ul>
 * <p>
 * Example: new CompositeTrustProvider(TrustCurrentJREProvider.getInstance(), TrustWindowsProvider.getInstance(),
 * TrustAllProvider.getInstance()) <br>
 * If JRE and Windows fail, TrustAllProvider accepts and the returned CompositeSSLTrustInfo will contain JRE and Windows in the failed
 * providers list.
 */
public class CompositeTrustProvider extends AbstractTrustProvider {
    private final TrustProviderInterface[] delegates;

    public CompositeTrustProvider(final TrustProviderInterface... delegates) {
        this.delegates = delegates != null && delegates.length > 0 ? delegates.clone() : new TrustProviderInterface[0];
        if (delegates.length == 0) {
            throw new IllegalArgumentException("at least one TrustProviderInterface must be declared");
        }
    }

    public TrustProviderInterface[] getDelegates() {
        return delegates.clone();
    }

    @Override
    public void verifyHostname(TrustResult trustResult, SSLSession session, String host, Object context) throws IllegalSSLHostnameException {
        final CompositeTrustResult compositeTrustResult = (CompositeTrustResult) trustResult;
        for (final TrustResult trust : compositeTrustResult.getTrust()) {
            try {
                trust.getTrustProvider().verifyHostname(trust, session, host, context);
            } catch (IllegalSSLHostnameException e) {
            }
        }
        if (trustResult.isTrusted()) {
            return;
        }
        for (final TrustResult trust : compositeTrustResult.getResults()) {
            if (trust.getException() instanceof IllegalSSLHostnameException) {
                final IllegalSSLHostnameException ie = new IllegalSSLHostnameException(host, trust.getException());
                trustResult.exception(ie);
                throw ie.setTrustResult(trustResult);
            }
        }
        for (final TrustProviderInterface p : delegates) {
            p.verifyHostname(trustResult, session, host, context);
        }
    }

    @Override
    public TrustResult checkServerTrusted(final X509Certificate[] chain, final String authType, final Object context) {
        final List<TrustResult> results = new ArrayList<TrustResult>();
        try {
            for (final TrustProviderInterface p : delegates) {
                final TrustResult result = p.checkServerTrusted(chain, authType, context);
                results.add(result);
            }
        } catch (Exception e) {
            return createResult(chain, TrustType.SERVER, results.toArray(new TrustResult[0])).exception(e);
        }
        return createResult(chain, TrustType.SERVER, results.toArray(new TrustResult[0]));
    }

    protected CompositeTrustResult createResult(final X509Certificate[] chain, final TrustType type, TrustResult... results) {
        return new CompositeTrustResult(this, chain, type, results);
    }

    @Override
    public TrustResult checkClientTrusted(final X509Certificate[] chain, final String authType, final Object context) {
        final List<TrustResult> results = new ArrayList<TrustResult>();
        try {
            for (final TrustProviderInterface p : delegates) {
                final TrustResult result = p.checkClientTrusted(chain, authType, context);
                results.add(result);
            }
        } catch (Exception e) {
            return createResult(chain, TrustType.CLIENT, results.toArray(new TrustResult[0])).exception(e);
        }
        return createResult(chain, TrustType.CLIENT, results.toArray(new TrustResult[0]));
    }

    @Override
    public X509Certificate[] getAcceptedIssuers() {
        final Set<X509Certificate> all = new HashSet<X509Certificate>();
        for (final TrustProviderInterface p : delegates) {
            try {
                final X509Certificate[] issuers = p.getAcceptedIssuers();
                if (issuers != null && issuers.length > 0) {
                    for (final X509Certificate cert : issuers) {
                        all.add(cert);
                    }
                }
            } catch (final Exception e) {
                // Ignore - continue with other providers
                LogV3.log(e);
            }
        }
        return all.toArray(new X509Certificate[0]);
    }

    /**
     * @return Provider ID for caching
     */
    @Override
    public String getId() {
        final StringBuilder sb = new StringBuilder("CompositeTrustProvider");
        for (final TrustProviderInterface p : delegates) {
            sb.append(":").append(p.getId());
        }
        return sb.toString();
    }

    /**
     * @see org.appwork.utils.net.httpconnection.trust.AbstractTrustProvider#getTrustManager()
     */
    @Override
    protected X509TrustManager getTrustManager() throws SSLException {
        throw new WTFException();
    }

    /**
     * @see org.appwork.utils.net.httpconnection.trust.AbstractTrustProvider#getTrustStore()
     */
    @Override
    public KeyStore getTrustStore() throws SSLException {
        return null;
    }

    /**
     * Reloads revocation handlers. Also propagates reload to delegates that extend AbstractTrustProvider.
     */
    @Override
    public void reload() {
        super.reload();
        // Propagate reload to delegates that extend AbstractTrustProvider
        for (final TrustProviderInterface p : delegates) {
            if (p instanceof AbstractTrustProvider) {
                ((AbstractTrustProvider) p).reload();
            }
        }
    }
}
