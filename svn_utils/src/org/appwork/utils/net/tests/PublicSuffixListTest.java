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
package org.appwork.utils.net.tests;

import java.util.Map;

import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestDependency;
import org.appwork.utils.Application;
import org.appwork.utils.net.PublicSuffixList;
import org.appwork.utils.net.PublicSuffixList.InternetDomainName;
import org.appwork.utils.net.PublicSuffixList.RuleType;

/**
 * @author daniel
 * @date Mar 28, 2022
 *
 */
@TestDependency({ "org.appwork.utils.net.PublicSuffixList" })
public class PublicSuffixListTest extends AWTest {
    public static void main(String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        final PublicSuffixList suffixList = PublicSuffixList.getInstance();
        assertNotNull(Application.getRessourceURL("org/appwork/utils/net/effective_tld_names.dat"));
        {
            InternetDomainName entry = suffixList.from("google.com");
            assertEquals("com", entry.publicSuffix());
            assertEquals("com", entry.registrySuffix());
            assertEquals("google.com", entry.topPrivateDomain());
            assertEquals(false, entry.isPublicSuffix());
            assertEquals(null, entry.subDomain());
        }
        {
            InternetDomainName entry = suffixList.from("co.uk");
            assertEquals("co.uk", entry.publicSuffix());
            assertEquals("co.uk", entry.registrySuffix());
            assertEquals(null, entry.topPrivateDomain());
            assertEquals(true, entry.isPublicSuffix());
            assertEquals(null, entry.subDomain());
        }
        {
            InternetDomainName entry = suffixList.from("www.google.co.uk");
            assertEquals("co.uk", entry.publicSuffix());
            assertEquals("co.uk", entry.registrySuffix());
            assertEquals("google.co.uk", entry.topPrivateDomain());
            assertEquals(false, entry.isPublicSuffix());
            assertEquals("www", entry.subDomain());
        }
        {
            InternetDomainName entry = suffixList.from("foo.blogspot.com");
            assertEquals("blogspot.com", entry.publicSuffix());
            assertEquals("com", entry.registrySuffix());
            assertEquals("foo.blogspot.com", entry.topPrivateDomain());
            assertEquals(false, entry.isPublicSuffix());
            assertEquals(null, entry.subDomain());
        }
        {
            InternetDomainName entry = suffixList.from("google.invalid");
            assertEquals(null, entry.publicSuffix());
            assertEquals(null, entry.registrySuffix());
            assertEquals(null, entry.topPrivateDomain());
            assertEquals(false, entry.isPublicSuffix());
            assertEquals(null, entry.subDomain());
        }
        if (true) {
            assertEquals("example.net", suffixList.getDomain("example.net"));
            assertEquals("example.net", suffixList.getDomain("www.example.net"));
            assertEquals("www", suffixList.getSubDomain("www.example.net"));
            assertEquals("example.co.uk", suffixList.getDomain("example.co.uk"));
            assertEquals("example.co.uk", suffixList.getDomain("www.example.co.uk"));
            assertEquals("www", suffixList.getSubDomain("www.example.co.uk"));
            assertEquals("xn--zfr164b.com.cn", suffixList.getDomain("政务.com.cn"));
            assertEquals("test.xn--9tfky.id", suffixList.getDomain("test.ᬩᬮᬶ.id"));

            assertEquals("my", suffixList.getSubDomain("my.jdownloader.org"));
            assertEquals(null, suffixList.getSubDomain("jdownloader.org"));
            final String mydns = "127-0-0-1.mydns.jdownloader.org";
            assertEquals("jdownloader.org", suffixList.getDomain(mydns));
            assertEquals("127-0-0-1.mydns", suffixList.getSubDomain(mydns));
            assertEquals("org", suffixList.getTopLevelDomain(mydns));
            final String dyn = "ip-127-0-0-1.dyn.luxdsl.pt.lu";
            assertEquals("lu", suffixList.getTopLevelDomain(dyn));
            assertEquals("pt.lu", suffixList.getDomain(dyn));
            assertEquals("ip-127-0-0-1.dyn.luxdsl", suffixList.getSubDomain(dyn));
            assertEquals("xn--strae-oqa.de", suffixList.getDomain("straße.de"));

        }
        testAllPslRulesExhaustively(suffixList);
    }

    public void testAllPslRulesExhaustively(PublicSuffixList list) throws Exception {
        final Map<String, PublicSuffixList.RuleType> rules = list.getRules(); // oder direkter Zugriff im selben Package
        assertNotNull(rules);
        assertFalse(rules.isEmpty());
        for (Map.Entry<String, RuleType> entry : rules.entrySet()) {
            final String key = entry.getKey();
            final RuleType type = entry.getValue();

            // 1. NORMAL (z. B. "co.uk" oder "blogspot.com")
            if (type.isNormal()) {
                // Exakter Suffix selbst
                {
                    final InternetDomainName suffixList = list.from(key);
                    assertEquals(key, suffixList.publicSuffix());
                    assertTrue(suffixList.isPublicSuffix());
                    assertNull(suffixList.topPrivateDomain());
                    assertNull(suffixList.subDomain());

                    if (type.isICANN()) {
                        assertEquals(key, suffixList.registrySuffix());
                        assertTrue(suffixList.isRegistrySuffix());
                    } else {
                        assertFalse(suffixList.isRegistrySuffix());
                        assertEqualsNot(key, suffixList.registrySuffix());
                    }
                }
                {
                    // Synthetische registrierbare Domain: "Blabla." + key (inkl. Case-Insensitivity-Test)
                    final String regDomain = "Blabla." + key;
                    final String expectedReg = "blabla." + key;
                    final InternetDomainName suffixList = list.from(regDomain);
                    assertEquals(key, suffixList.publicSuffix());
                    assertFalse(suffixList.isPublicSuffix());
                    assertEquals(expectedReg, suffixList.topPrivateDomain());
                    assertNull(suffixList.subDomain());
                }
                {
                    // Synthetische Subdomain: "www.sub.Blabla." + key
                    final String fullDomain = "www.sub.Blabla." + key;
                    final String expectedReg = "blabla." + key;
                    final InternetDomainName suffixList = list.from(fullDomain);
                    assertEquals(key, suffixList.publicSuffix());
                    assertEquals(expectedReg, suffixList.topPrivateDomain());
                    assertEquals("www.sub", suffixList.subDomain());
                }
            }
            // 2. WILDCARD (key ist "kawasaki.jp" aus "*.kawasaki.jp")
            if (type.isWildcard()) {
                // Prüfen, ob für "star." + key nicht zufällig eine EXCEPTION existiert (z. B. "!star.kawasaki.jp")
                final String wildcardLabel = "zzwildcardtest";
                final String dynamicSuffix = wildcardLabel + "." + key;
                if (rules.get(dynamicSuffix) != null && rules.get(dynamicSuffix).isException()) {
                    continue;
                }
                {
                    final InternetDomainName suffixList = list.from(dynamicSuffix);
                    // "zzwildcardtest.kawasaki.jp" ist selbst das Public Suffix!
                    assertEquals(dynamicSuffix, suffixList.publicSuffix());
                    assertTrue(suffixList.isPublicSuffix());
                    assertNull(suffixList.topPrivateDomain());
                }

                {
                    // "Blabla.zzwildcardtest.kawasaki.jp" -> topPrivateDomain = "blabla.zzwildcardtest.kawasaki.jp"
                    final String regDomain = "Blabla." + dynamicSuffix;
                    final String expectedReg = "blabla." + dynamicSuffix;
                    final InternetDomainName suffixList = list.from(regDomain);
                    assertEquals(dynamicSuffix, suffixList.publicSuffix());
                    assertEquals(expectedReg, suffixList.topPrivateDomain());
                    assertNull(suffixList.subDomain());

                }
                {
                    // "www.Blabla.zzwildcardtest.kawasaki.jp" -> subDomain = "www"
                    final String regDomain = "Blabla." + dynamicSuffix;
                    final String fullDomain = "www." + regDomain;
                    final String expectedReg = "blabla." + dynamicSuffix;
                    final InternetDomainName suffixList = list.from(fullDomain);
                    assertEquals(dynamicSuffix, suffixList.publicSuffix());
                    assertEquals(expectedReg, suffixList.topPrivateDomain());
                    assertEquals("www", suffixList.subDomain());
                }
            }

            // 3. EXCEPTION (key ist "city.kawasaki.jp" aus "!city.kawasaki.jp")
            if (type.isException()) {
                final int dot = key.indexOf('.');
                assertTrue(dot > 0);
                final String expectedParentSuffix = key.substring(dot + 1);

                {
                    // Die Exception-Domain selbst ("city.kawasaki.jp") ist KEIN Public Suffix, sondern bereits eTLD+1!
                    final InternetDomainName suffixList = list.from(key);
                    assertFalse(suffixList.isPublicSuffix());
                    assertEquals(expectedParentSuffix, suffixList.publicSuffix());
                    assertEquals(key, suffixList.topPrivateDomain());
                    assertNull(suffixList.subDomain());
                }
                {
                    // Subdomain unter Exception: "www." + key -> topPrivateDomain bleibt key ("city.kawasaki.jp")
                    final String subUnderException = "www." + key;
                    final InternetDomainName suffixList = list.from(subUnderException);
                    assertEquals(expectedParentSuffix, suffixList.publicSuffix());
                    assertEquals(key, suffixList.topPrivateDomain());
                    assertEquals("www", suffixList.subDomain());
                }
            }

        }
    }
}
