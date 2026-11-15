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
