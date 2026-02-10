/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.net.httpconnection;

import java.io.IOException;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * @author thomas
 * @date 29.01.2026
 *
 */
public class IllegalSSLHostnameException extends IOException implements TrustResultProvider {
    public final String expected;

    /**
     * @return the expected
     */
    public String getExpected() {
        return expected;
    }

    /**
     * @return the is
     */
    public Set<String> getSubjects() {
        return subjects;
    }

    public final Set<String> subjects;
    private TrustResult      trustResult;

    /**
     * @return the trustResult
     */
    public TrustResult getTrustResult() {
        return trustResult;
    }

    /**
     * @param host
     * @param subjects
     */
    public IllegalSSLHostnameException(String host, Set<String> subjects) {
        super("HTTPS hostname wrong:  should be <" + host + "> != " + subjects);
        this.expected = host;
        this.subjects = subjects != null ? Collections.unmodifiableSet(new HashSet<String>(subjects)) : null;
    }

    /**
     * @param string
     */
    public IllegalSSLHostnameException(String host, String string) {
        super(string);
        this.expected = host;
        this.subjects = null;
    }

    /**
     * @param host
     * @param e
     */
    public IllegalSSLHostnameException(String host, Exception e) {
        super("Illegal Hostname", e);
        this.expected = host;
        this.subjects = null;
    }

    /**
     * @param trustResult
     */
    public IllegalSSLHostnameException setTrustResult(TrustResult trustResult) {
        this.trustResult = trustResult;
        return this;
    }
}