/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
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
package org.appwork.utils.tests;

import org.appwork.storage.JSonStorage;
import org.appwork.testframework.AWTest;
import org.appwork.utils.StringUtils;

/**
 * @author thomas
 * @date 26.10.2022
 *
 */
public class StringUtilsTest extends AWTest {
    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        assertEquals(StringUtils.getCommonPostfix("a", "v"), "");
        assertEquals(StringUtils.getCommonPrefix("abc", "abtown", "apple"), "a");
        assertEquals(StringUtils.getCommonPrefix("a", "v"), "");
        assertEquals(StringUtils.getCommonPrefix("a", "a"), "a");
        assertEquals(StringUtils.getCommonPrefix("aaa", "aaa"), "aaa");
        assertEquals(StringUtils.getCommonPrefix("abc", "abtown", "abple"), "ab");
        assertEquals(StringUtils.getCommonPostfix("google.de", "appwork.de", "heise.de"), ".de");
        assertEquals(StringUtils.getCommonPostfix("aaa", "aaa"), "aaa");
        assertEquals(StringUtils.getCommonPostfix("a", "a"), "a");
        {
            assertEquals(StringUtils.upperLowerAfterRegex("(^|\\s+|-|/|_)", "mein", true, false), "Mein");
        }
        {
            assertEquals(StringUtils.upperLowerAfterRegex("(^|\\s+|-|/|_)", "mEiN", null, null), "mEiN");
        }
        {
            assertEquals(StringUtils.upperLowerAfterRegex("(^|\\s+|-|/|_)", "mein Text-min_zeug", true, false), "Mein Text-Min_Zeug");
        }
        {
            assertEquals(StringUtils.toCamelCase("mein Text-min_zeug", true), "MeinTextMinZeug");
        }
        {
            assertEquals(StringUtils.toCamelCase("mein Text-min_zeug", false), "meinTextMinZeug");
        }
        {
            String before = "a\rb\nc\r\nd";
            String result = StringUtils.multiLineIntend(before, " * ", "   ");
            assertThat(result).is(" * a\r   b\n   c\r\n   d");
        }
        {
            String before = "\r\na\rb\n\r\nc\r\nd\r\n";
            String result = StringUtils.multiLineIntend(before, " * ", "   ");
            assertThat(result).is(" * \r\n   a\r   b\n   \r\n   c\r\n   d\r\n   ");
        }
        {
            String before = "split                                                                                                       something at end";
            String wrapped = StringUtils.wrapText(before, 40, "([\\s]+)", null, true);
            assertThat(wrapped).is("split\r\n" + "something at end");
        }
        {
            String before = "split                                                                                                       ";
            String wrapped = StringUtils.wrapText(before, 40, "([\\s]+)", null, true);
            assertThat(wrapped).is("split");
        }
        {
            String before = "split a text with many space at end                                                                                                       ";
            String wrapped = StringUtils.wrapText(before, 40, null, null, true);
            assertThat(wrapped).is("split a text with many space at end     \r\n" + "                                        \r\n" + "                                        \r\n" + "               ");
        }
        {
            String before = "     split\r\n     longtext at spaces, but keep extra spaces";
            String wrapped = StringUtils.wrapText(before, 20, null, null, true);
            assertThat(wrapped).is("     split\r\n" + "     longtext at\r\n" + "spaces, but keep\r\n" + "extra spaces");
        }
        {
            String before = "longtext to split at space or othersa. replace space split at whitespace oroooanyother";
            String wrapped = StringUtils.wrapText(before, 24, "([\\s]{1,})|(ooo)|oth|(irgendwas)", null, true);
            assertThat(wrapped).is("longtext to split at\r\n" + "space or othersa.\r\n" + "replace space split at\r\n" + "whitespace oroooany\r\n" + "other");
        }
        {
            String before = "     string with\r\n         \t spaces and wrapping";
            String wrapped = StringUtils.wrapText(before, 60, null, null, true);
            assertThat(wrapped).is(before);
        }
        {
            assertTrue(StringUtils.isEmptyAfterTrim(" "));
            assertTrue(StringUtils.isEmptyAfterTrim("  "));
            assertFalse(StringUtils.isEmptyAfterTrim(" 1"));
            assertFalse(StringUtils.isEmptyAfterTrim(" 1 "));
            assertFalse(StringUtils.isEmptyAfterTrim("1 "));
        }
        {
            for (int minCount = 1; minCount < 30; minCount++) {
                for (String fill : new String[] { "a", " ", "!" }) {
                    assertEquals(StringUtils.fillPost("test", fill, minCount), "test" + (minCount > "test".length() ? (String.format("%0" + (minCount - "test".length()) + "d", 0).replace("0", fill)) : ""));
                    assertEquals(StringUtils.fillPre("test", fill, minCount), (minCount > "test".length() ? (String.format("%0" + (minCount - "test".length()) + "d", 0).replace("0", fill)) : "") + "test");
                }
            }
        }
        {
            assertTrue(StringUtils.isAllEmpty(" "));
            assertFalse(StringUtils.isAllNotEmpty(" "));
            assertTrue(StringUtils.isAllNotEmpty("1"));
            assertFalse(StringUtils.isAllEmpty("1"));
            assertTrue(StringUtils.isAllEmpty(" ", " "));
            assertFalse(StringUtils.isAllNotEmpty(" ", " "));
            assertTrue(StringUtils.isAllNotEmpty("1", "2"));
            assertFalse(StringUtils.isAllEmpty("1", "2"));
        }
        String textWithLongWordAtEnd = "Nihil corporis consequatur harum porro. Soluta quod dolores consectetur fugiat ipsa. Qui nostrum eos veritatis est sunt quia. Perspiciatis alias aut enim harum et officiis. Ipsam est atque libero quam eos impedit. Modi occaecati autem cumque deleniti quo itaque. dasisteinsehrlangeswordohnespaceundsowasichkannesnurschwerrrennen";
        String wrapped = StringUtils.wrapText(textWithLongWordAtEnd, 30, null, null, true);
        assertEquals("Nihil corporis consequatur\r\nharum porro. Soluta quod\r\ndolores consectetur fugiat\r\nipsa. Qui nostrum eos\r\nveritatis est sunt quia.\r\nPerspiciatis alias aut enim\r\nharum et officiis. Ipsam est\r\natque libero quam eos impedit.\r\nModi occaecati autem cumque\r\ndeleniti quo itaque.\r\ndasisteinsehrlangeswordohnespa\r\nceundsowasichkannesnurschwerrr\r\nennen", wrapped);
        wrapped = StringUtils.wrapText(textWithLongWordAtEnd, 30, null, null, false);
        assertEquals("Nihil corporis consequatur\r\nharum porro. Soluta quod\r\ndolores consectetur fugiat\r\nipsa. Qui nostrum eos\r\nveritatis est sunt quia.\r\nPerspiciatis alias aut enim\r\nharum et officiis. Ipsam est\r\natque libero quam eos impedit.\r\nModi occaecati autem cumque\r\ndeleniti quo itaque.\r\ndasisteinsehrlangeswordohnespaceundsowasichkannesnurschwerrrennen", wrapped);
        wrapped = StringUtils.wrapText(textWithLongWordAtEnd + "  ", 30, null, null, true);
        assertEquals("Nihil corporis consequatur\r\n" + "harum porro. Soluta quod\r\n" + "dolores consectetur fugiat\r\n" + "ipsa. Qui nostrum eos\r\n" + "veritatis est sunt quia.\r\n" + "Perspiciatis alias aut enim\r\n" + "harum et officiis. Ipsam est\r\n" + "atque libero quam eos impedit.\r\n" + "Modi occaecati autem cumque\r\n" + "deleniti quo itaque.\r\n" + "dasisteinsehrlangeswordohnespa\r\n" + "ceundsowasichkannesnurschwerrr\r\n" + "ennen  ", wrapped);
        wrapped = StringUtils.wrapText(textWithLongWordAtEnd + "  ", 30, null, null, false);
        assertEquals("Nihil corporis consequatur\r\nharum porro. Soluta quod\r\ndolores consectetur fugiat\r\nipsa. Qui nostrum eos\r\nveritatis est sunt quia.\r\nPerspiciatis alias aut enim\r\nharum et officiis. Ipsam est\r\natque libero quam eos impedit.\r\nModi occaecati autem cumque\r\ndeleniti quo itaque.\r\ndasisteinsehrlangeswordohnespaceundsowasichkannesnurschwerrrennen  ", wrapped);
        wrapped = StringUtils.wrapText("  " + textWithLongWordAtEnd + ".", 30, null, null, true);
        assertEquals("  Nihil corporis consequatur\r\nharum porro. Soluta quod\r\ndolores consectetur fugiat\r\nipsa. Qui nostrum eos\r\nveritatis est sunt quia.\r\nPerspiciatis alias aut enim\r\nharum et officiis. Ipsam est\r\natque libero quam eos impedit.\r\nModi occaecati autem cumque\r\ndeleniti quo itaque.\r\ndasisteinsehrlangeswordohnespa\r\nceundsowasichkannesnurschwerrr\r\nennen.", wrapped);
        wrapped = StringUtils.wrapText("  " + textWithLongWordAtEnd + ".", 30, null, null, false);
        assertEquals("  Nihil corporis consequatur\r\nharum porro. Soluta quod\r\ndolores consectetur fugiat\r\nipsa. Qui nostrum eos\r\nveritatis est sunt quia.\r\nPerspiciatis alias aut enim\r\nharum et officiis. Ipsam est\r\natque libero quam eos impedit.\r\nModi occaecati autem cumque\r\ndeleniti quo itaque.\r\ndasisteinsehrlangeswordohnespaceundsowasichkannesnurschwerrrennen.", wrapped);
        wrapped = StringUtils.wrapText(",  .", 30, null, null, false);
        assertEquals(",  .", wrapped);
        wrapped = StringUtils.wrapText("397238n94ufhdknfm498curlfhgafdlcnnmsdfhjdasiasifsbsafsdhfjkabmnvbcdfshdjhsmdhdbduHRTJBH", 30, null, null, false);
        assertEquals("397238n94ufhdknfm498curlfhgafdlcnnmsdfhjdasiasifsbsafsdhfjkabmnvbcdfshdjhsmdhdbduHRTJBH", wrapped);
        wrapped = StringUtils.wrapText("397238n94ufhdknfm498curlfhgafdlcnnmsdfhjdasiasifsbsafsdhfjkabmnvbcdfshdjhsmdhdbduHRTJBH", 30, null, null, true);
        System.out.println(JSonStorage.serializeToJson(wrapped));
        assertEquals("397238n94ufhdknfm498curlfhgafd\r\nlcnnmsdfhjdasiasifsbsafsdhfjka\r\nbmnvbcdfshdjhsmdhdbduHRTJBH", wrapped);
        wrapped = StringUtils.wrapText("Dieses Element ist nur in Connect Client/Server/Admintool Builds verfügbar, die älter sind als 2022-06-27T17:42:00+0200.\r\n" + "Bitte sicherstellen, dass #minimumTargetCodeBase entsprechend gesetzt ist.", 98, null, null, true);
        System.out.println(JSonStorage.serializeToJson(wrapped));
        assertEquals("Dieses Element ist nur in Connect Client/Server/Admintool Builds verfügbar, die älter sind als\r\n2022-06-27T17:42:00+0200.\r\nBitte sicherstellen, dass #minimumTargetCodeBase entsprechend gesetzt ist.", wrapped);
    }

    public static void main(String[] args) {
        run();
    }
}
