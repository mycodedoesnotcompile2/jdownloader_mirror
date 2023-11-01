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
package org.appwork.testframework.tests;

import static org.appwork.testframework.AWTest.logInfoAnyway;

import java.io.File;
import java.io.IOException;
import java.util.Locale;
import java.util.concurrent.atomic.AtomicInteger;

import org.appwork.app.launcher.parameterparser.CommandSwitch;
import org.appwork.app.launcher.parameterparser.ParameterParser;
import org.appwork.testframework.PostBuildTestInterface;
import org.appwork.utils.Files;
import org.appwork.utils.Files.Handler;
import org.appwork.utils.IO;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.os.CrossSystem;

public class SourceParserPostBuildTest implements PostBuildTestInterface {
    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.PostBuildTestInterface#runPostBuildTest(java.lang.String[], java.io.File)
     */
    @Override
    public void runPostBuildTest(String[] args, File applicationRoot) throws Exception {
        final ParameterParser pp = new ParameterParser(args).parse();
        final CommandSwitch sourceP = pp.getCommandSwitch("source");
        if (sourceP != null) {
            final String folders = sourceP.getParameters()[0];
            for (String folder : StringUtils.splitNoEmpty(folders, ";")) {
                logInfoAnyway(folder);
                if (!new File(folder).isDirectory()) {
                    throw new Exception(new File(folder).getCanonicalPath() + " does not exist");
                }
                final AtomicInteger count = new AtomicInteger(0);
                Files.walkThroughStructure(new Handler<Exception>() {
                    @Override
                    public void intro(File f) throws Exception {
                    }

                    @Override
                    public void onFile(File f) throws Exception {
                        if (f.isFile() && f.getName().endsWith(".java")) {
                            scanJavaFile(f);
                            count.incrementAndGet();
                        }
                    }

                    @Override
                    public void outro(File f) throws Exception {
                    }
                }, new File(folder));
                if (count.get() == 0) {
                    throw new Exception("No Source Files in " + folder);
                }
            }
        }
    }

    /**
     * @param f
     * @throws IOException
     */
    protected void scanJavaFile(File f) throws Exception {
        // System.out.println("Scan " + f);
        String code = IO.readFileToString(f);
        String packageName = new Regex(code, "package ([^;]+);").getMatch(0);
        String className = new Regex(code, "class (\\w+)\\s+").getMatch(0);
        // do not test yourself
        if (SourceParserPostBuildTest.class.getSimpleName().equals(className)) {
            return;
        }
        {
            code = code.replaceAll("\\/\\/[^\r\n]*DebugMode\\.TRIGGER_BUILD_ERROR", "IGNORE IN TEST-comment");
            // else it will find itself
            int index = code.indexOf("DebugMode.TRIGGER_BUILD_ERROR");
            if (index >= 0) {
                CrossSystem.openFile(f);
                throw new Exception("DebugMode.TRIGGER_BUILD_ERROR in " + packageName + ". (" + className + ".java:" + code.substring(0, index).split("[\r\n]{1,2}").length + ")");
            }
        }
        {
            int index = code.toLowerCase(Locale.ROOT).indexOf("todo");
            if (index >= 0) {
                logInfoAnyway("TODO in " + packageName + ". (" + className + ".java:" + code.substring(0, index).split("[\r\n]{1,2}").length + ")");
            }
        }
    }

    public static void main(String[] args) throws Exception {
        new SourceParserPostBuildTest().runPostBuildTest(args, null);
    }
}
