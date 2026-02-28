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
package org.appwork.utils.net.httpconnection;

import java.security.cert.X509Certificate;

import org.appwork.utils.Exceptions;
import org.appwork.utils.net.httpconnection.trust.TrustProviderInterface;

public class TrustResult {
    public static enum TrustType {
        CLIENT,
        SERVER
    }

    private final TrustType type;

    /**
     * @return the type
     */
    public TrustType getType() {
        return type;
    }

    private final TrustProviderInterface trustProvider;
    private final X509Certificate[]      chain;
    private Exception                    exception;

    /**
     * @param exception
     *            the exception to set
     */
    public void setException(Exception exception) {
        this.exception = exception;
    }

    public boolean isTrusted() {
        return getException() == null;
    }

    /**
     * @return the exception
     */
    public Exception getException() {
        return exception;
    }

    /**
     * @return the certificates
     */
    public X509Certificate[] getChain() {
        return chain;
    }

    /**
     * @param abstractTrustProvider
     * @param chain
     * @param e
     */
    public TrustResult(TrustProviderInterface trustProvider, X509Certificate[] chain, Exception e, TrustType type) {
        this.trustProvider = trustProvider;
        this.chain = chain;
        this.exception = e;
        this.type = type;
    }

    public TrustProviderInterface getTrustProvider() {
        return trustProvider;
    }

    /**
     * @param e
     */
    public TrustResult exception(Exception e) {
        if (exception == e) {
            return this;
        } else if (exception == null) {
            exception = e;
        } else {
            Exceptions.addSuppressed(exception, e);
        }
        return this;
    }
}
