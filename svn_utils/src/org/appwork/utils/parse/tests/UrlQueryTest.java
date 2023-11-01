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
package org.appwork.utils.parse.tests;

import java.util.ArrayList;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.testframework.AWTest;
import org.appwork.utils.KeyValueStringEntry;
import org.appwork.utils.parser.UrlQuery;

/**
 * @author thomas
 * @date 04.02.2022
 *
 */
public class UrlQueryTest extends AWTest {
    public static void main(String[] args) {
        run();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        {
            UrlQuery parsed = UrlQuery.parse("http://domain_with_underscore.de/?query=true");
            assertEquals(parsed.get("query"), "true");
            assertEquals(parsed.list().size(), 1);

        }
        {
            UrlQuery parsed = UrlQuery.parse("domain_with_underscore.de/?query=true");
            assertEquals(parsed.get("query"), "true");
            assertEquals(parsed.list().size(), 1);

        }
        {
            UrlQuery parsed = UrlQuery.parse("st=0&rt=SO&pv=12&cv=20210716001&pkh=c4622595eb512a&app=AWAdminTool&os=WINDOWS&osr=WINDOWS_10&arch=X86&os64=1&jvm64=1&java=18261012&uid=&hidNew=027ef5524a37b04ac638660eac27102f38811af717d1b99eca490a06bf535de&hidSta=0044f0990e5da993d314db308328261e140782fb32db43a&ct=ALPHA&rg=null&cBuild=&gui=0&repos=&mcnt=311820&rcnt=1494&scnt=1115&stm=1643707780775&bc=46&rc=0&bt=1640685241000&ut=3297989953&sit=1595088146755&cycle=739&csVer=1639060415000&csDep=&csPro=&awfcxz=1&rev=46&lng=en&chlg=1&jdiff=1&dst=-1&dedup=INTER&1643983194392.1643983195370");
            // System.out.println(JSonStorage.serializeToJson(JSonStorage.serializeToJson(parsed.list())));
            ArrayList<KeyValueStringEntry> correct = JSonStorage.restoreFromString(
                    "[\r\n {\r\n  \"key\"   : \"st\",\r\n  \"value\" : \"0\"\r\n },\r\n {\r\n  \"key\"   : \"rt\",\r\n  \"value\" : \"SO\"\r\n },\r\n {\r\n  \"key\"   : \"pv\",\r\n  \"value\" : \"12\"\r\n },\r\n {\r\n  \"key\"   : \"cv\",\r\n  \"value\" : \"20210716001\"\r\n },\r\n {\r\n  \"key\"   : \"pkh\",\r\n  \"value\" : \"c4622595eb512a\"\r\n },\r\n {\r\n  \"key\"   : \"app\",\r\n  \"value\" : \"AWAdminTool\"\r\n },\r\n {\r\n  \"key\"   : \"os\",\r\n  \"value\" : \"WINDOWS\"\r\n },\r\n {\r\n  \"key\"   : \"osr\",\r\n  \"value\" : \"WINDOWS_10\"\r\n },\r\n {\r\n  \"key\"   : \"arch\",\r\n  \"value\" : \"X86\"\r\n },\r\n {\r\n  \"key\"   : \"os64\",\r\n  \"value\" : \"1\"\r\n },\r\n {\r\n  \"key\"   : \"jvm64\",\r\n  \"value\" : \"1\"\r\n },\r\n {\r\n  \"key\"   : \"java\",\r\n  \"value\" : \"18261012\"\r\n },\r\n {\r\n  \"key\"   : \"uid\",\r\n  \"value\" : \"\"\r\n },\r\n {\r\n  \"key\"   : \"hidNew\",\r\n  \"value\" : \"027ef5524a37b04ac638660eac27102f38811af717d1b99eca490a06bf535de\"\r\n },\r\n {\r\n  \"key\"   : \"hidSta\",\r\n  \"value\" : \"0044f0990e5da993d314db308328261e140782fb32db43a\"\r\n },\r\n {\r\n  \"key\"   : \"ct\",\r\n  \"value\" : \"ALPHA\"\r\n },\r\n {\r\n  \"key\"   : \"rg\",\r\n  \"value\" : \"null\"\r\n },\r\n {\r\n  \"key\"   : \"cBuild\",\r\n  \"value\" : \"\"\r\n },\r\n {\r\n  \"key\"   : \"gui\",\r\n  \"value\" : \"0\"\r\n },\r\n {\r\n  \"key\"   : \"repos\",\r\n  \"value\" : \"\"\r\n },\r\n {\r\n  \"key\"   : \"mcnt\",\r\n  \"value\" : \"311820\"\r\n },\r\n {\r\n  \"key\"   : \"rcnt\",\r\n  \"value\" : \"1494\"\r\n },\r\n {\r\n  \"key\"   : \"scnt\",\r\n  \"value\" : \"1115\"\r\n },\r\n {\r\n  \"key\"   : \"stm\",\r\n  \"value\" : \"1643707780775\"\r\n },\r\n {\r\n  \"key\"   : \"bc\",\r\n  \"value\" : \"46\"\r\n },\r\n {\r\n  \"key\"   : \"rc\",\r\n  \"value\" : \"0\"\r\n },\r\n {\r\n  \"key\"   : \"bt\",\r\n  \"value\" : \"1640685241000\"\r\n },\r\n {\r\n  \"key\"   : \"ut\",\r\n  \"value\" : \"3297989953\"\r\n },\r\n {\r\n  \"key\"   : \"sit\",\r\n  \"value\" : \"1595088146755\"\r\n },\r\n {\r\n  \"key\"   : \"cycle\",\r\n  \"value\" : \"739\"\r\n },\r\n {\r\n  \"key\"   : \"csVer\",\r\n  \"value\" : \"1639060415000\"\r\n },\r\n {\r\n  \"key\"   : \"csDep\",\r\n  \"value\" : \"\"\r\n },\r\n {\r\n  \"key\"   : \"csPro\",\r\n  \"value\" : \"\"\r\n },\r\n {\r\n  \"key\"   : \"awfcxz\",\r\n  \"value\" : \"1\"\r\n },\r\n {\r\n  \"key\"   : \"rev\",\r\n  \"value\" : \"46\"\r\n },\r\n {\r\n  \"key\"   : \"lng\",\r\n  \"value\" : \"en\"\r\n },\r\n {\r\n  \"key\"   : \"chlg\",\r\n  \"value\" : \"1\"\r\n },\r\n {\r\n  \"key\"   : \"jdiff\",\r\n  \"value\" : \"1\"\r\n },\r\n {\r\n  \"key\"   : \"dst\",\r\n  \"value\" : \"-1\"\r\n },\r\n {\r\n  \"key\"   : \"dedup\",\r\n  \"value\" : \"INTER\"\r\n },\r\n {\r\n  \"key\"   : null,\r\n  \"value\" : \"1643983194392.1643983195370\"\r\n }\r\n]",
                    new TypeRef<ArrayList<KeyValueStringEntry>>() {
                    });
            assertEqualsDeep(parsed.list(), correct);

        }

    }

}
