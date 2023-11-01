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
package org.appwork.utils.os;

import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;

import org.appwork.utils.processes.ProcessBuilderFactory;

/**
 * @author Thomas
 *
 */
public class DesktopSupportMac extends DesktopSupportJavaDesktop {
    @Override
    public boolean isBrowseURLSupported() {
        /* either Desktop or FallBack method */
        return true;
    }

    @Override
    public void browseURL(URL url) throws IOException, URISyntaxException {
        try {
            super.browseURL(url);
        } catch (Throwable e) {
            try {
                Class.forName("com.apple.eio.FileManager").getDeclaredMethod("openURL", new Class[] { String.class }).invoke(null, new Object[] { url.toExternalForm() });
            } catch (final Throwable ignore) {
                if (e instanceof IOException) {
                    throw (IOException) e;
                } else {
                    throw new IOException(e);
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.os.DesktopSupportJavaDesktop#reboot(boolean, int)
     */
    @Override
    public boolean reboot(boolean force, int waitms) throws InterruptedException {
        if (force) {
            /* force shutdown */
            try {
                ProcessBuilderFactory.runCommand(new String[] { "sudo", "reboot" });
            } catch (Exception e) {
                org.appwork.loggingv3.LogV3.log(e);
            }

            return true;
        } else {
            /* normal shutdown */
            try {
                ProcessBuilderFactory.runCommand(new String[] { "/usr/bin/osascript", "-e", "tell application \"System Events\" restart" });
                return true;
            } catch (Exception e) {
                org.appwork.loggingv3.LogV3.log(e);
            }
        }
        return false;
    }

    @Override
    public boolean shutdown(boolean force) {
        if (force) {
            /* force shutdown */
            try {
                ProcessBuilderFactory.runCommand(new String[] { "sudo", "shutdown", "-p", "now" });
            } catch (Exception e) {
                org.appwork.loggingv3.LogV3.log(e);
            }
            try {
                ProcessBuilderFactory.runCommand(new String[] { "sudo", "shutdown", "-h", "now" });
            } catch (Exception e) {
                org.appwork.loggingv3.LogV3.log(e);
            }
            return true;
        } else {
            /* normal shutdown */
            try {
                ProcessBuilderFactory.runCommand(new String[] { "/usr/bin/osascript", "-e", "tell application \"Finder\" to shut down" });
                return true;
            } catch (Exception e) {
                org.appwork.loggingv3.LogV3.log(e);
            }
        }
        return false;
    }

    @Override
    public int getPrefixLength(String pathname) {
        if (pathname.length() == 0) {
            return 0;
        } else {
            return (pathname.charAt(0) == '/') ? 1 : 0;
        }
    }

    @Override
    public boolean standby() {
        try {
            ProcessBuilderFactory.runCommand(new String[] { "/usr/bin/osascript", "-e", "tell application \"Finder\" to sleep" });
            return true;
        } catch (Exception e) {
            org.appwork.loggingv3.LogV3.log(e);
        }
        org.appwork.loggingv3.LogV3.info("no standby support, use shutdown");
        return false;
    }

    @Override
    public boolean hibernate() {
        org.appwork.loggingv3.LogV3.info("no hibernate support, use shutdown");
        return false;
    }
}
