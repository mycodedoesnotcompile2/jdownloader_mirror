/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
 *         Germany
 * ,= Preamble ,=
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * ,= 3rd Party Licences ,=
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * ,= Definition: Commercial Usage ,=
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * ,= Dual Licensing ,=
 * ,= Commercial Usage ,=
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * ,= Non-Commercial Usage ,=
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
 * ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,, */
package org.appwork.utils.test;

import org.appwork.testframework.AWTest;
import org.appwork.utils.JVMVersion;
import org.appwork.utils.JVMVersion.JavaVersionInterface;
import org.appwork.utils.JavaVersion;

/**
 * @author daniel
 * @date 06.02.2018
 *
 */
public class JVMTest extends AWTest {
    public static void main(String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        check(("1.8.0_112-b16"), 18112016l);
        check(("1.7.0_72-b14"), 17072014);
        check(("1.5.1_144"), 15144000);
        check(("1.8.1_144"), 18144000);
        check(("9"), 9000000000000l);
        check(("9.0.1+11"), 9000001000011l);
        check(("9.0.1+11-LTS"), 9000001000011l);
        check(("9.0.1+11  - LTS"), 9000001000011l);
        check(("10.0.1+13  - LTS"), 10000001000013l);
        check(("10-ea+42"), 10000000000042l);
        check(("11.0.2"), 11000002000000l);
        checkgt(("11.0.2"), JVMVersion.JAVA19);
        check(("12+33"), 12000000000033l);
        check(("13-ea+25"), 13000000000025l);
        check(("11.0.3+7"), 11000003000007l);
        check(("11.0.9.1"), 11000009000000l);
        check("7u331", JVMVersion.JAVA_1_7 + 331000);
        check("6u331", JVMVersion.JAVA_1_6 + 331000);
        check("8u331", JVMVersion.JAVA_1_8 + 331000);
        check("9u331", JVMVersion.JAVA_9 + 331000);
        check("11u331", JVMVersion.JAVA_11 + 331000);
        check("17u331", JVMVersion.JAVA_17 + 331000);
        check("17.0.2", 17000002000000l);
        checkgt("17.0.2", JVMVersion.JAVA_17);
        checkgt("7u331", JVMVersion.JAVA_1_7);
        checkgt("6u331", JVMVersion.JAVA_1_6);
        checkgt("5u331", JVMVersion.JAVA_1_5);
        checkgt("8u331", JVMVersion.JAVA_1_8);
        checkgt("9u331", JVMVersion.JAVA_9);
        checkgt("11u331", JVMVersion.JAVA_11);
        checkgt("17u331", JVMVersion.JAVA_17);
        assertTrue(JavaVersion.JVM_1_0.longID == 10000000);
        assertTrue(JavaVersion.JVM_1_1.longID == 11000000);
        assertTrue(JavaVersion.JVM_1_2.longID == 12000000);
        assertTrue(JavaVersion.JVM_1_3.longID == 13000000);
        assertTrue(JavaVersion.JVM_1_4.longID == 14000000);
        assertTrue(JavaVersion.JVM_1_8.longID == 18000000);
        assertTrue(JavaVersion.JVM_9_0.longID == 9000000000000l);
        assertTrue(JavaVersion.JVM_1_0.is("1.0.1"));
        assertTrue(!JavaVersion.JVM_22_0.is("23.1"));
        assertTrue(JavaVersion.JVM_22_0.is("22.0.1"));
        assertTrue(JVMVersion.isAtLeast(JavaVersion.JVM_1_8));
        assertTrue(JVMVersion.getVersion().isMinimum(JVMVersion.getVersion()));
        assertTrue(JVMVersion.getVersion().isMaximum(JVMVersion.getVersion()));
        assertTrue(JVMVersion.getVersion().isHigherThan(JavaVersion.JVM_1_5));
        assertTrue(JVMVersion.getVersion().isHigherThan(JavaVersion.JVM_1_6));
        assertTrue(JVMVersion.getVersion().isHigherThan(JavaVersion.JVM_1_7));
        assertFalse(JVMVersion.getVersion().isHigherThan(JVMVersion.getVersion()));
        assertTrue(JVMVersion.getVersion().isMinimum(JavaVersion.JVM_1_8));
        assertTrue(JVMVersion.getVersion().isMinimum(JVMVersion.getVersion().getBase()));
        final JavaVersionInterface current = JVMVersion.getVersion();
        boolean found = false;
        final JavaVersion[] versions = JavaVersion.values();
        for (JavaVersion version : versions) {
            if (found || version.is(current)) {
                if (found) {
                    assertTrue(version.isHigherThan(current.getBase()));
                }
                found = true;
                assertTrue(version.isMinimum(current.getBase()));
                if (version.ordinal() + 1 < versions.length) {
                    final JavaVersion next = versions[version.ordinal() + 1];
                    assertTrue(current.isLowerThan(next));
                    assertTrue(current.isMaximum(next));
                    assertTrue(!current.isHigherThan(next));
                    assertTrue(!current.isMinimum(next));
                    assertTrue(next.isHigherThan(current));
                    assertTrue(next.isMinimum(current));
                    assertTrue(!next.isLowerThan(current));
                    assertTrue(!next.isMaximum(current));
                    assertTrue(!current.is(next));
                }
            }
        }
        for (JavaVersion version : versions) {
            if (JavaVersion.UNKNOWN.equals(version)) {
                continue;
            }
            final JavaVersionInterface check = JVMVersion.toJavaVersion(version.getVersionString());
            assertTrue(check.getBase().equals(version.getBase()));
            assertTrue(check.isMinimum(version));
            assertTrue(check.is(version));
            if (version.ordinal() + 1 < versions.length) {
                final JavaVersion next = versions[version.ordinal() + 1];
                assertTrue(check.isLowerThan(next));
                assertTrue(check.isMaximum(next));
                assertTrue(!check.isHigherThan(next));
                assertTrue(!check.isMinimum(next));
                assertTrue(next.isHigherThan(check));
                assertTrue(next.isMinimum(check));
                assertTrue(!next.isLowerThan(check));
                assertTrue(!next.isMaximum(check));
                assertTrue(!check.is(next));
            }
        }
        final JavaVersionInterface farFarAway = JVMVersion.toJavaVersion("100");
        assertTrue(JavaVersion.UNKNOWN.equals(farFarAway.getBase()));
        assertTrue(farFarAway.is(farFarAway));
        for (JavaVersion version : versions) {
            if (JavaVersion.UNKNOWN.equals(version)) {
                continue;
            }
            assertTrue(farFarAway.isMinimum(version));
            assertTrue(farFarAway.isHigherThan(version));
            assertTrue(!farFarAway.isLowerThan(version));
            assertTrue(!farFarAway.isMaximum(version));
            assertTrue(version.isLowerThan(farFarAway));
            assertTrue(version.isMaximum(farFarAway));
            assertTrue(!version.isHigherThan(farFarAway));
            assertTrue(!version.isMinimum(farFarAway));
            assertTrue(!version.is(farFarAway));
            assertTrue(!farFarAway.is(version));
        }
        final JavaVersionInterface isThisJava = JVMVersion.toJavaVersion("Punkt Punkt Komma Strich Fertig Ist das Mondgesicht");
        assertTrue(JavaVersion.UNKNOWN.equals(isThisJava.getBase()));
        for (JavaVersion version : versions) {
            if (JavaVersion.UNKNOWN.equals(version)) {
                continue;
            }
            assertTrue(!isThisJava.isMinimum(version));
            assertTrue(!isThisJava.isHigherThan(version));
            assertTrue(!isThisJava.isLowerThan(version));
            assertTrue(!isThisJava.isMaximum(version));
            assertTrue(!version.isLowerThan(isThisJava));
            assertTrue(!version.isMaximum(isThisJava));
            assertTrue(!version.isHigherThan(isThisJava));
            assertTrue(!version.isMinimum(isThisJava));
            assertTrue(!version.is(isThisJava));
            assertTrue(!isThisJava.is(version));
        }
    }

    private void check(String string, long i) throws Exception {
        final long result = JVMVersion.parseJavaVersionString(string);
        if (result != i) {
            throw new Exception("Failed to parse JVM Version " + string + "|" + result + "!=" + i);
        }
    }

    private void checkgt(String string, long i) throws Exception {
        final long result = JVMVersion.parseJavaVersionString(string);
        if (result <= i) {
            throw new Exception("Failed to parse JVM Version  " + string + "|" + result + "!=" + i);
        }
    }
}
