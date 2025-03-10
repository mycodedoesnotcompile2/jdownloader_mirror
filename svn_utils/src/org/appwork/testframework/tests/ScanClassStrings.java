/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
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
package org.appwork.testframework.tests;

import java.io.File;
import java.lang.management.ManagementFactory;
import java.lang.management.RuntimeMXBean;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.regex.Pattern;

import org.appwork.builddecision.BuildDecisionRequired;
import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.AWTestValidateClassReference;
import org.appwork.testframework.TestDependency;
import org.appwork.utils.Application;
import org.appwork.utils.ClassPathScanner;
import org.appwork.utils.StringUtils;

/**
 * @author thomas
 * @date 18.10.2024
 *
 */
public class ScanClassStrings extends AWTest {
    public static void main(String[] args) {
        run();
    }

    /**
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        if (isPostBuildTest()) {
            return;
        }
        new ClassPathScanner<Exception>() {
            protected void handle(java.lang.Class<?> cls, File root) throws Exception {
                if (!Application.isJared(null) && root.isFile()) {
                    // jar in ide. this is probabl now our own class
                    return;
                }
                super.handle(cls, root);
            };

            @Override
            public void handle(Class<?> type) throws Exception {
                BuildDecisionRequired buildDec = type.getAnnotation(BuildDecisionRequired.class);
                if (buildDec != null) {
                    for (String str : buildDec.imports()) {
                        for (String s : str.split(";")) {
                            if (StringUtils.isEmpty(s)) {
                                continue;
                            }
                            try {
                                Class.forName(s);
                            } catch (Throwable e) {
                                LogV3.warning("Invalid Class Definition in  " + buildDec + " - " + type);
                                LogV3.log(e);
                                throw new WTFException(e);
                            }
                        }
                    }
                }
                TestDependency testDep = type.getAnnotation(TestDependency.class);
                if (testDep != null) {
                    for (String str : testDep.value()) {
                        try {
                            Class.forName(str);
                        } catch (Throwable e) {
                            LogV3.warning("Invalid Class Definition in  " + testDep + " - " + type);
                            LogV3.log(e);
                            throw new WTFException(e);
                        }
                    }
                }
                try {
                    for (Field f : type.getDeclaredFields()) {
                        if (Modifier.isFinal(f.getModifiers())) {
                            if (Modifier.isStatic(f.getModifiers())) {
                                if (f.getType().equals(String.class)) {
                                    AWTestValidateClassReference anno = f.getAnnotation(AWTestValidateClassReference.class);
                                    if (anno != null) {
                                        f.setAccessible(true);
                                        String str = (String) f.get(null);
                                        try {
                                            // logInfoAnyway("Check class reference: " + (String) f.get(null) + " in: " + type.getName());
                                            Class.forName(str);
                                        } catch (Exception e) {
                                            if (!checkPackage(anno, str)) {
                                                logInfoAnyway("Skip Class Check: " + type.getName() + "." + f.getName() + " because none of " + anno.classpath() + " is in classpath");
                                                return;
                                            }
                                            LogV3.log(e);
                                            throw e;
                                        }
                                        if (!checkPackage(anno, str)) {
                                            throw new WTFException("Something is wrong with the dependsOnPackages declarion. Class is availaible, but non of its jar.: \r\n" + type.getName() + "." + f.getName() + "\r\n" + ManagementFactory.getRuntimeMXBean().getClassPath());
                                        }
                                    }
                                }
                            }
                        }
                    }
                } catch (Throwable e) {
                    throw new WTFException(e);
                }
            }

            protected boolean checkPackage(AWTestValidateClassReference anno, String classRef) {
                if (StringUtils.isEmpty(anno.classpath())) {
                    return true;
                }
                final RuntimeMXBean runtimeMXBean = ManagementFactory.getRuntimeMXBean();
                final String[] cp = runtimeMXBean.getClassPath().split(File.pathSeparator);
                for (String required : anno.classpath().split(";")) {
                    if (StringUtils.isEmpty(required)) {
                        continue;
                    }
                    for (String c : cp) {
                        if (Pattern.compile(required, Pattern.CASE_INSENSITIVE).matcher(c.replace("\\", "/")).matches()) {
                            return true;
                        }
                    }
                }
                return false;
            }
        }.run();
    }

    /**
     * @return
     */
    private boolean isPostBuildTest() {
        return System.getProperty(org.appwork.testframework.PostBuildRunner.POSTBUILDTEST) != null;
    }
}
