/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.net.httpconnection.trust;

import java.io.IOException;
import java.net.Socket;
import java.security.KeyStore;
import java.security.cert.X509Certificate;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.SSLException;
import javax.net.ssl.SSLSession;
import javax.net.ssl.X509TrustManager;

import org.appwork.exceptions.WTFException;
import org.appwork.testframework.AWTestValidateClassReference;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils;
import org.appwork.utils.net.httpconnection.IllegalSSLHostnameException;
import org.appwork.utils.net.httpconnection.NativeHTTPConnectionImpl.HostnameVerifiedDelegate;
import org.appwork.utils.net.httpconnection.TrustResult;
import org.appwork.utils.net.httpconnection.TrustResult.TrustType;

/**
 * Abstract base class for trust providers that provides common functionality for creating SSLTrustInfo.
 */
public abstract class AbstractTrustProvider implements TrustProviderInterface {
    /**
     *
     */
    @AWTestValidateClassReference
    private static final String JAVAX_NET_SSL_X509_EXTENDED_TRUST_MANAGER = "javax.net.ssl.X509ExtendedTrustManager";

    /**
     * @see org.appwork.utils.net.httpconnection.trust.TrustProviderInterface#verifyHostname(javax.net.ssl.SSLSession, java.lang.String)
     */
    @Override
    public void verifyHostname(TrustResult result, SSLSession session, String host, Object context) throws IllegalSSLHostnameException {
        try {
            // Do not use the connection's HostnameVerifier when context is HttpsURLConnection:
            // that verifier delegates back to this trust provider, which would cause infinite recursion.
            // Use null so HTTPConnectionUtils falls back to default verifier + certificate CN/SAN check.
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
        } catch (IllegalSSLHostnameException e) {
            result.setException(e);
            throw e.setTrustResult(result);
        } catch (IOException e) {
            result.setException(e);
            throw new IllegalSSLHostnameException(host, e).setTrustResult(result);
        }
    }

    /**
     * Creates SSLTrustInfo from a certificate chain. Extracts subject, serial number, and SHA-256 hash.
     *
     * @param chain
     *            Certificate chain (may be null or empty)
     * @param e
     * @param type
     * @return SSLTrustInfo with this provider and certificate information, or null if chain is null/empty
     */
    protected TrustResult createTrustInfo(final X509Certificate[] chain, Exception e, TrustType type) {
        if (chain == null || chain.length == 0) {
            throw new WTFException();
        }
        return new TrustResult(this, chain, e, type);
    }

    @Override
    public TrustResult checkServerTrusted(final X509Certificate[] chain, final String authType, final Object context) {
        try {
            final X509TrustManager tm = getTrustManager();
            if (ReflectionUtils.isInstanceOf(JAVAX_NET_SSL_X509_EXTENDED_TRUST_MANAGER, tm)) {
                if (context instanceof Socket) {
                    ((javax.net.ssl.X509ExtendedTrustManager) tm).checkServerTrusted(chain, authType, (Socket) context);
                } else if (context instanceof SSLEngine) {
                    ((javax.net.ssl.X509ExtendedTrustManager) tm).checkServerTrusted(chain, authType, (SSLEngine) context);
                } else {
                    tm.checkServerTrusted(chain, authType);
                }
            } else {
                tm.checkServerTrusted(chain, authType);
            }
            return createTrustInfo(chain, null, TrustType.SERVER);
        } catch (Exception e) {
            return createTrustInfo(chain, e, TrustType.SERVER);
        }
    }

    @Override
    public TrustResult checkClientTrusted(final X509Certificate[] chain, final String authType, final Object context) {
        try {
            final X509TrustManager tm = getTrustManager();
            if (ReflectionUtils.isInstanceOf(JAVAX_NET_SSL_X509_EXTENDED_TRUST_MANAGER, tm)) {
                if (context instanceof Socket) {
                    ((javax.net.ssl.X509ExtendedTrustManager) tm).checkClientTrusted(chain, authType, (Socket) context);
                } else if (context instanceof SSLEngine) {
                    ((javax.net.ssl.X509ExtendedTrustManager) tm).checkClientTrusted(chain, authType, (SSLEngine) context);
                } else {
                    tm.checkClientTrusted(chain, authType);
                }
            } else {
                tm.checkClientTrusted(chain, authType);
            }
            return createTrustInfo(chain, null, TrustType.CLIENT);
        } catch (Exception e) {
            return createTrustInfo(chain, e, TrustType.CLIENT);
        }
    }

    /**
     * @return
     * @throws SSLException
     */
    protected abstract X509TrustManager getTrustManager() throws Exception;

    @Override
    public X509Certificate[] getAcceptedIssuers() {
        try {
            return getTrustManager().getAcceptedIssuers();
        } catch (final Exception e) {
            return new X509Certificate[0];
        }
    }

    /**
     * Default implementation returns class name. Subclasses should override.
     */
    @Override
    public String getId() {
        return getClass().getSimpleName();
    }

    /**
     * @return
     * @throws SSLException
     */
    abstract public KeyStore getTrustStore() throws Exception;

    /**
     *
     */
    public void reload() {
    }
}
