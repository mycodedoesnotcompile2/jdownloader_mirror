/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
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
package org.appwork.utils.net;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.appwork.utils.Application;
import org.appwork.utils.URLStream;

/**
 * @author daniel
 *
 *         https://publicsuffix.org/list/
 */
public final class PublicSuffixList {
    private static PublicSuffixList INSTANCE;
    static {
        try {
            PublicSuffixList.INSTANCE = new PublicSuffixList();
        } catch (Throwable e) {
            e.printStackTrace();
        }
    }

    public static PublicSuffixList getInstance() {
        return PublicSuffixList.INSTANCE;
    }

    public enum RuleType {
        NORMAL_ICANN {
            @Override
            public boolean isICANN() {
                return true;
            }
        },
        NORMAL_PRIVATE,
        WILDCARD_ICANN {
            @Override
            public boolean isICANN() {
                return true;
            }

            @Override
            public boolean isWildcard() {
                return true;
            }
        },
        WILDCARD_PRIVATE {

            @Override
            public boolean isWildcard() {
                return true;
            }
        },
        EXCEPTION_ICANN {
            @Override
            public boolean isICANN() {
                return true;
            }

            @Override
            public boolean isException() {
                return true;
            }
        },
        EXCEPTION_PRIVATE {
            @Override
            public boolean isException() {
                return true;
            }
        };

        public boolean isICANN() {
            return false;
        }

        public boolean isNormal() {
            return !isWildcard() && !isException();
        }

        public boolean isWildcard() {
            return false;
        }

        public boolean isException() {
            return false;
        }

    }

    public Map<String, RuleType> getRules() {
        return rules;
    }

    private final Map<String, RuleType> rules;

    public PublicSuffixList(URL publicSuffixList) throws IOException {
        this.rules = Collections.unmodifiableMap(parse(publicSuffixList));
    }

    /**
     * Implementation for https://publicsuffix.org/list/
     */
    private PublicSuffixList() throws IOException {
        this(Application.getRessourceURL("org/appwork/utils/net/effective_tld_names.dat"));
    }

    public final class InternetDomainName {
        private final String domain;

        private InternetDomainName(final String domain) {
            this.domain = domain;
        }

        public String getDomain() {
            return domain;
        }

        public String publicSuffix() {
            return findSuffix(getDomain(), false);
        }

        public String registrySuffix() {
            return findSuffix(getDomain(), true);
        }

        public boolean hasPublicSuffix() {
            return publicSuffix() != null;
        }

        public boolean hasRegistrySuffix() {
            return registrySuffix() != null;
        }

        // --- 2. Exakte Suffix-Prüfungen ---

        public boolean isPublicSuffix() {
            String suffix = publicSuffix();
            return suffix != null && suffix.equals(getDomain());
        }

        public boolean isRegistrySuffix() {
            String suffix = registrySuffix();
            return suffix != null && suffix.equals(getDomain());
        }

        // --- 3. Registrierbare Hauptdomain (eTLD+1) ---

        /**
         * Entspricht Guava topPrivateDomain(): "www.google.co.uk" -> "google.co.uk" "user.blogspot.com" -> "user.blogspot.com"
         * "blogspot.com" -> null (ist selbst ein Public Suffix, hat keine eTLD+1)
         */
        public String topPrivateDomain() {
            return extractTopDomain(getDomain(), publicSuffix());
        }

        /**
         * Entspricht Guava topDomainUnderRegistrySuffix(): "user.blogspot.com" -> "blogspot.com" "blogspot.com" -> "blogspot.com" "com" ->
         * null
         */
        public String topDomainUnderRegistrySuffix() {
            return extractTopDomain(getDomain(), registrySuffix());
        }

        public boolean isTopPrivateDomain() {
            String top = topPrivateDomain();
            return top != null && top.equals(getDomain());
        }

        public boolean isTopDomainUnderRegistrySuffix() {
            String top = topDomainUnderRegistrySuffix();
            return top != null && top.equals(getDomain());
        }

        /**
         * Gibt den Subdomain-Teil links von topPrivateDomain() zurück (oder null, falls keiner existiert): "www.google.co.uk" -> "www"
         * "a.b.google.co.uk" -> "a.b" "google.co.uk" -> null "www.user.blogspot.com" -> "www" "user.blogspot.com" -> null "blogspot.com" ->
         * null (kein topPrivateDomain vorhanden)
         */
        public String subDomain() {
            return extractSubDomain(domain, topPrivateDomain());
        }

        /**
         * Gibt den Subdomain-Teil links von topDomainUnderRegistrySuffix() zurück: "www.user.blogspot.com" -> "www.user"
         * "user.blogspot.com" -> "user" "blogspot.com" -> null
         */
        public String subDomainUnderRegistrySuffix() {
            return extractSubDomain(domain, topDomainUnderRegistrySuffix());
        }
    }

    public InternetDomainName from(final URL url) {
        return from(url.getHost());
    }

    public InternetDomainName from(final String domain) {
        String lookupDomain = domain;
        if (lookupDomain.endsWith(".")) {
            lookupDomain = lookupDomain.substring(0, lookupDomain.length() - 1);
        }
        lookupDomain = Punycode.toASCII(lookupDomain);
        return new InternetDomainName(lookupDomain);
    }

    public String getSubDomain(final String fullDomain) {
        return from(fullDomain).subDomain();
    }

    public String getDomain(String fullDomain) {
        return from(fullDomain).topPrivateDomain();
    }

    public String getTopLevelDomain(String fullDomain) {
        return from(fullDomain).publicSuffix();
    }

    protected String extractSubDomain(String normalizedDomain, String topDomain) {
        if (normalizedDomain == null || topDomain == null) {
            return null;
        }
        // Keine Subdomain vorhanden (domain == topDomain)
        if (normalizedDomain.length() <= topDomain.length()) {
            return null;
        }
        // Erwarteter Punkt unmittelbar vor dem topDomain: "<subDomain>.<topDomain>"
        final int dotBeforeTop = normalizedDomain.length() - topDomain.length() - 1;
        if (dotBeforeTop <= 0 || normalizedDomain.charAt(dotBeforeTop) != '.') {
            return null;
        }
        return normalizedDomain.substring(0, dotBeforeTop);
    }

    protected String extractTopDomain(String normalizedDomain, String suffix) {
        if (suffix == null || normalizedDomain == null) {
            return null;
        }
        // Domain ist identisch mit dem Suffix (z. B. "co.uk" oder "blogspot.com") -> kein +1 Label vorhanden
        if (normalizedDomain.length() <= suffix.length()) {
            return null;
        }
        // Erwartete Position des Punktes unmittelbar vor dem Suffix: "...<label>.<suffix>"
        final int dotBeforeSuffix = normalizedDomain.length() - suffix.length() - 1;
        if (dotBeforeSuffix <= 0 || normalizedDomain.charAt(dotBeforeSuffix) != '.') {
            return null;
        }
        // Den Punkt links vor diesem einen Label suchen (falls Subdomains davor liegen)
        final int prevDot = normalizedDomain.lastIndexOf('.', dotBeforeSuffix - 1);
        return prevDot < 0 ? normalizedDomain : normalizedDomain.substring(prevDot + 1);
    }

    protected String findSuffix(String domain, boolean registryOnly) {
        int start = 0;
        while (true) {
            final String candidate = domain.substring(start);
            final RuleType exact = rules.get(candidate);
            if (exact != null && (!registryOnly || exact.isICANN())) {
                if (exact.isException()) {
                    int nextDot = candidate.indexOf('.');
                    return nextDot > 0 ? candidate.substring(nextDot + 1) : null;
                }
                if (exact.isNormal()) {
                    return candidate;
                }
            }

            final int nextDot = candidate.indexOf('.');
            if (nextDot > 0) {
                final String parent = candidate.substring(nextDot + 1);
                final RuleType parentRule = rules.get(parent);
                if (parentRule != null && parentRule.isWildcard() && (!registryOnly || parentRule.isICANN())) {
                    return candidate;
                }
                start += nextDot + 1;
            } else {
                break;
            }
        }
        return null;
    }

    protected Map<String, RuleType> parse(final URL publicSuffixList) throws IOException {
        final Map<String, RuleType> map = new HashMap<String, RuleType>();
        if (publicSuffixList != null) {
            final InputStream is = URLStream.openStream(publicSuffixList);
            if (is != null) {
                final BufferedReader reader = new BufferedReader(new InputStreamReader(is, StandardCharsets.UTF_8));
                try {
                    String line = null;
                    Boolean isICANN = null;
                    while ((line = reader.readLine()) != null) {
                        line = line.trim();
                        if (line.length() == 0) {
                            continue;
                        }
                        if (line.startsWith("//")) {
                            if (line.contains("===BEGIN ICANN DOMAINS===")) {
                                if (isICANN != null) {
                                    throw new IOException("invalid/broken list?");
                                }
                                isICANN = true;
                            } else if (line.contains("===END ICANN DOMAINS===")) {
                                if (!Boolean.TRUE.equals(isICANN)) {
                                    throw new IOException("invalid/broken list?");
                                }
                                isICANN = null;
                            } else if (line.contains("===BEGIN PRIVATE DOMAINS===")) {
                                if (isICANN != null) {
                                    throw new IOException("invalid/broken list?");
                                }
                                isICANN = false;
                            } else if (line.contains("===END PRIVATE DOMAINS===")) {
                                if (!Boolean.FALSE.equals(isICANN)) {
                                    throw new IOException("invalid/broken list?");
                                }
                                isICANN = null;
                            }
                            continue;
                        }
                        if (isICANN == null) {
                            throw new IOException("invalid/broken list?");
                        }
                        final RuleType ruleType;
                        if (line.startsWith("*")) {
                            ruleType = isICANN ? RuleType.WILDCARD_ICANN : RuleType.WILDCARD_PRIVATE;
                            line = line.substring(2);
                        } else if (line.startsWith("!")) {
                            ruleType = isICANN ? RuleType.EXCEPTION_ICANN : RuleType.EXCEPTION_PRIVATE;
                            line = line.substring(1);
                        } else {
                            ruleType = isICANN ? RuleType.NORMAL_ICANN : RuleType.NORMAL_PRIVATE;
                        }
                        final String normalized = Punycode.toASCII(line);
                        if (map.put(normalized, ruleType) != null) {
                            throw new IOException("invalid/broken list?:" + normalized);
                        }
                    }
                } finally {
                    reader.close();
                }
            }
        }
        return map;
    }
}
