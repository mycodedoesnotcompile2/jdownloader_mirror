/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
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
    void verifyHostname(TrustResult trustResult, SSLSession session, String host, Object context) throws IllegalSSLHostnameException;
}
