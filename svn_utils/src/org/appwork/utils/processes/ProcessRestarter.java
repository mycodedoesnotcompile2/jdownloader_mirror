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
package org.appwork.utils.processes;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;

import org.appwork.exceptions.WTFException;
import org.appwork.shutdown.ShutdownController;
import org.appwork.shutdown.ShutdownEvent;
import org.appwork.shutdown.ShutdownRequest;
import org.appwork.utils.os.CrossSystem;

/**
 * @author thomas
 * @date 04.04.2022
 *
 */
public class ProcessRestarter {

    /**
     * @param class1
     */
    public static void restartApplication(final File jar, final String... parameters) {
        try {
            org.appwork.loggingv3.LogV3.info("restartApplication " + jar + " " + parameters.length);
            final java.util.List<String> nativeParameters = new ArrayList<String>();
            File runin = null;
            if (CrossSystem.isMac()) {
                // find .app
                File rootpath = jar;
                final HashSet<File> loopMap = new HashSet<File>();
                while (rootpath != null && loopMap.add(rootpath)) {
                    if (rootpath.getName().endsWith(".app")) {
                        break;
                    }
                    rootpath = rootpath.getParentFile();
                }
                if (rootpath.getName().endsWith(".app")) {
                    // found app.- restart it.
                    nativeParameters.add("open");
                    nativeParameters.add("-n");
                    nativeParameters.add(rootpath.getAbsolutePath());
                    runin = rootpath.getParentFile();
                }
            }
            if (nativeParameters.isEmpty()) {
                org.appwork.loggingv3.LogV3.info("Find Jarfile");
                final File jarFile = jar;
                org.appwork.loggingv3.LogV3.info("Find Jarfile " + jarFile);
                runin = jarFile.getParentFile();
                if (CrossSystem.isWindows() || CrossSystem.isOS2()) {
                    final File exeFile = new File(jarFile.getParentFile(), jarFile.getName().substring(0, jarFile.getName().length() - 4) + ".exe");
                    if (exeFile.exists()) {
                        nativeParameters.add(exeFile.getAbsolutePath());
                    } else {
                        nativeParameters.add(CrossSystem.getJavaBinary());
                        nativeParameters.add("-jar");
                        nativeParameters.add(jarFile.getAbsolutePath());
                    }
                } else {
                    nativeParameters.add(CrossSystem.getJavaBinary());
                    nativeParameters.add("-jar");
                    nativeParameters.add(jarFile.getAbsolutePath());
                }
            }
            if (parameters != null) {
                for (final String s : parameters) {
                    nativeParameters.add(s);
                }
            }
            org.appwork.loggingv3.LogV3.info("Start " + nativeParameters);
            final ProcessBuilder pb = ProcessBuilderFactory.create(nativeParameters.toArray(new String[] {}));
            /*
             * needed because the root is different for jre/class version
             */
            org.appwork.loggingv3.LogV3.info("Root: " + runin);
            if (runin != null) {
                pb.directory(runin);
            }
            ShutdownController.getInstance().addShutdownEvent(new ShutdownEvent() {
                {
                    this.setHookPriority(Integer.MIN_VALUE);
                }
    
                @Override
                public void onShutdown(final ShutdownRequest shutdownRequest) {
                    try {
                        pb.start();
                    } catch (final IOException e) {
                        org.appwork.loggingv3.LogV3.log(e);
                    }
                }
            });
            org.appwork.loggingv3.LogV3.info("Start " + ShutdownController.getInstance().requestShutdown(true));
        } catch (final Throwable e) {
            throw new WTFException(e);
        }
    }

}
