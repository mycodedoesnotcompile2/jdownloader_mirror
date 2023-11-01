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

import java.awt.GraphicsEnvironment;
import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;

import org.appwork.utils.processes.ProcessBuilderFactory;
import org.appwork.utils.processes.ProcessOutput;

/**
 * @author daniel
 *
 */
public class DesktopSupportLinux implements DesktopSupport {
    public static enum WINDOW_MANAGER {
        XFCE,
        GNOME,
        MATE,
        UNITY,
        KDE,
        UNKNOWN
    }

    private final DesktopSupportJavaDesktop fallBack = new DesktopSupportJavaDesktop();
    private final String[]                  linuxFileOpen;
    private final String[]                  linuxBrowseUrl;
    private final WINDOW_MANAGER            windowManager;
    private final boolean                   waylandDetected;

    public boolean isWayland() {
        return waylandDetected;
    }

    private final boolean contains(String string, String value) {
        return string != null && value != null && string.toLowerCase(Locale.ENGLISH).contains(value.toLowerCase(Locale.ENGLISH));
    }

    private final static boolean isEmpty(final String ip) {
        return ip == null || ip.length() == 0 || ip.trim().length() == 0;
    }

    protected void log(String message) {
        org.appwork.loggingv3.LogV3.info(message);
    }

    protected void log(Throwable throwable) {
        org.appwork.loggingv3.LogV3.log(throwable);
    }

    public DesktopSupportLinux() {
        /* java vm property */
        final List<String> log = new ArrayList<String>();
        try {
            final String sunDesktop = System.getProperty("sun.desktop");
            /* see http://standards.freedesktop.org/menu-spec/latest/apb.html */
            final String XDG_CURRENT_DESKTOP = System.getenv("XDG_CURRENT_DESKTOP");
            final String XDG_SESSION_TYPE = System.getenv("XDG_SESSION_TYPE");
            final String WAYLAND_DISPLAY = System.getenv("WAYLAND_DISPLAY");
            final String GNOME_DESKTOP_SESSION_ID = System.getenv("GNOME_DESKTOP_SESSION_ID");
            /* returns true in case we have running KDE */
            final String KDE_FULL_SESSION = System.getenv("KDE_FULL_SESSION");
            final String KDE_SESSION_VERSION = System.getenv("KDE_SESSION_VERSION");
            /* gnome session */
            final String GDMSESSION = System.getenv("GDMSESSION");
            final String DESKTOP_SESSION = System.getenv("DESKTOP_SESSION");
            final String[] openCommand;
            /* WAYLAND? */
            if (contains(XDG_CURRENT_DESKTOP, "wayland")) {
                log.add("Wayland detected:XDG_CURRENT_DESKTOP=" + XDG_CURRENT_DESKTOP);
                this.waylandDetected = true;
            } else if (contains(GDMSESSION, "wayland")) {
                log.add("Wayland detected:GDMSESSION=" + GDMSESSION);
                this.waylandDetected = true;
            } else if (contains(XDG_SESSION_TYPE, "wayland")) {
                log.add("Wayland detected:XDG_SESSION_TYPE=" + XDG_SESSION_TYPE);
                this.waylandDetected = true;
            } else if (contains(WAYLAND_DISPLAY, "wayland")) {
                log.add("Wayland detected:WAYLAND_DISPLAY=" + WAYLAND_DISPLAY);
                this.waylandDetected = true;
            } else {
                this.waylandDetected = false;
            }
            if (contains(XDG_CURRENT_DESKTOP, "Unity") || contains(GDMSESSION, "ubuntu")) {
                if (contains(GDMSESSION, "ubuntu-2d")) {
                    log.add("Unity-2D Desktop detected");
                } else {
                    log.add("Unity-3D Desktop detected");
                }
                this.windowManager = WINDOW_MANAGER.UNITY;
                openCommand = new String[] { "gnome-open", "%s" };
            } else if (contains(XDG_CURRENT_DESKTOP, "GNOME") || (!isEmpty(GNOME_DESKTOP_SESSION_ID) && !"this-is-deprecated".equals(GNOME_DESKTOP_SESSION_ID)) || contains(GDMSESSION, "GNOME") || contains(GDMSESSION, "gnome-shell") || contains(GDMSESSION, "gnome-classic") || contains(GDMSESSION, "gnome-fallback") || contains(GDMSESSION, "cinnamon")) {
                log.add("Gnome Desktop detected");
                this.windowManager = WINDOW_MANAGER.GNOME;
                openCommand = new String[] { "gnome-open", "%s" };
            } else if (contains(XDG_CURRENT_DESKTOP, "mate") || contains(DESKTOP_SESSION, "mate")) {
                log.add("Mate Desktop detected");
                this.windowManager = WINDOW_MANAGER.MATE;
                openCommand = new String[] { "gnome-open", "%s" };
            } else if (contains(XDG_CURRENT_DESKTOP, "kde") || contains(KDE_FULL_SESSION, "true") || contains(DESKTOP_SESSION, "kde-plasma")) {
                if (KDE_SESSION_VERSION != null) {
                    log.add("KDE Version " + KDE_SESSION_VERSION + " detected");
                } else {
                    log.add("KDE detected");
                }
                this.windowManager = WINDOW_MANAGER.KDE;
                String kdeOpenCommand = "kde-open";
                if (KDE_SESSION_VERSION != null) {
                    try {
                        if (Integer.parseInt(KDE_SESSION_VERSION) >= 5) {
                            kdeOpenCommand = "kde-open5";
                        }
                    } catch (final Throwable e) {
                    }
                }
                openCommand = new String[] { kdeOpenCommand, "%s" };
            } else if (contains(XDG_CURRENT_DESKTOP, "XFCE")) {
                log.add("XFCE detected");
                this.windowManager = WINDOW_MANAGER.XFCE;
                openCommand = new String[] { "xdg-open", "%s" };
            } else {
                log.add("sun.Desktop: " + sunDesktop);
                log.add("XDG_CURRENT_DESKTOP: " + XDG_CURRENT_DESKTOP);
                log.add("XDG_SESSION_TYPE: " + XDG_SESSION_TYPE);
                log.add("KDE_FULL_SESSION: " + KDE_FULL_SESSION);
                log.add("KDE_SESSION_VERSION: " + KDE_SESSION_VERSION);
                log.add("DESKTOP_SESSION: " + DESKTOP_SESSION);
                log.add("GNOME_DESKTOP_SESSION_ID: " + GNOME_DESKTOP_SESSION_ID);
                this.windowManager = WINDOW_MANAGER.UNKNOWN;
                openCommand = null;
            }
            if (GraphicsEnvironment.isHeadless()) {
                this.linuxBrowseUrl = null;
                this.linuxFileOpen = null;
            } else {
                if (!isEmpty(XDG_CURRENT_DESKTOP)) {
                    this.linuxFileOpen = new String[] { "xdg-open", "%s" };
                    this.linuxBrowseUrl = new String[] { "xdg-open", "%s" };
                } else {
                    this.linuxFileOpen = openCommand;
                    this.linuxBrowseUrl = openCommand;
                }
                log.add("linuxFileOpen:" + (linuxFileOpen == null ? null : Arrays.asList(this.linuxFileOpen)));
                log.add("linuxBrowseUrl:" + (linuxBrowseUrl == null ? null : Arrays.asList(this.linuxBrowseUrl)));
            }
        } finally {
            log(log.toString());
        }
    }

    @Override
    public void browseURL(final URL url) throws IOException, URISyntaxException {
        if (!this.openCustom(this.linuxBrowseUrl, url.toExternalForm())) {
            this.fallBack.browseURL(url);
        }
    }

    public WINDOW_MANAGER getWindowManager() {
        return this.windowManager;
    }

    @Override
    public boolean isBrowseURLSupported() {
        if (this.linuxBrowseUrl != null && this.linuxBrowseUrl.length >= 2) {
            return true;
        } else if (this.fallBack.isBrowseURLSupported()) {
            return true;
        } else {
            return false;
        }
    }

    public boolean isGnomeDesktop() {
        switch (this.windowManager) {
        case GNOME:
        case MATE:
        case UNITY:
            return true;
        default:
            return false;
        }
    }

    public boolean isKDEDesktop() {
        switch (this.windowManager) {
        case KDE:
            return true;
        default:
            return false;
        }
    }

    @Override
    public boolean isOpenFileSupported() {
        if (this.linuxFileOpen != null && this.linuxFileOpen.length >= 2 || this.fallBack.isOpenFileSupported()) {
            return true;
        } else {
            return false;
        }
    }

    public boolean isXFCEDesktop() {
        switch (this.windowManager) {
        case XFCE:
            return true;
        default:
            return false;
        }
    }

    private boolean openCustom(final String[] custom, final String what) throws IOException {
        try {
            return CrossSystem.openCustom(custom, what);
        } catch (IOException e) {
            return false;
        }
    }

    @Override
    public void openFile(final File file) throws IOException {
        if (!this.openCustom(this.linuxFileOpen, file.getAbsolutePath())) {
            this.fallBack.openFile(file);
        }
    }

    @Override
    public boolean shutdown(boolean force) {
        try {
            dbusPowerState("Shutdown");
        } catch (Throwable e) {
            org.appwork.loggingv3.LogV3.log(e);
        }
        try {
            ProcessBuilderFactory.runCommand(new String[] { "dcop", "--all-sessions", "--all-users", "ksmserver", "ksmserver", "logout", "0", "2", "0" });
        } catch (Throwable e) {
            org.appwork.loggingv3.LogV3.log(e);
        }
        try {
            ProcessBuilderFactory.runCommand("poweroff");
        } catch (Throwable e) {
            org.appwork.loggingv3.LogV3.log(e);
        }
        try {
            ProcessBuilderFactory.runCommand(new String[] { "sudo", "shutdown", "-P", "now" });
        } catch (Throwable e) {
            org.appwork.loggingv3.LogV3.log(e);
        }
        try {
            ProcessBuilderFactory.runCommand(new String[] { "sudo", "shutdown", "-Ph", "now" });
        } catch (Throwable e) {
            org.appwork.loggingv3.LogV3.log(e);
        }
        return true;
    }

    private void dbusPowerState(String command) {
        try {
            ProcessOutput output = ProcessBuilderFactory.runCommand(new String[] { "dbus-send", "--session", "--dest=org.freedesktop.PowerManagement", "--type=method_call", "--print-reply", "--reply-timeout=2000", "/org/freedesktop/PowerManagement", "org.freedesktop.PowerManagement." + command });
            if (output.getErrOutString().contains("org.freedesktop.DBus.Error.ServiceUnknown")) {
                // compatible to newer dbus versions
                ProcessBuilderFactory.runCommand("dbus-send", "--system", "--print-reply", "--dest=org.freedesktop.login1", "/org/freedesktop/login1", "org.freedesktop.login1.Manager." + command, "boolean:true");
            }
        } catch (Throwable e) {
            org.appwork.loggingv3.LogV3.log(e);
        }
    }

    @Override
    public boolean standby() {
        try {
            dbusPowerState("Suspend");
            return true;
        } catch (Throwable e) {
            log(e);
            log("no standby support, use shutdown");
            return false;
        }
    }

    @Override
    public boolean hibernate() {
        try {
            dbusPowerState("Hibernate");
            return true;
        } catch (Throwable e) {
            log(e);
            log("no hibernate support, use shutdown");
            return false;
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.os.DesktopSupport#getDefaultDownloadDirectory()
     */
    @Override
    public String getDefaultDownloadDirectory() {
        /**
         * http://freedesktop.org/wiki/Software/xdg-user-dirs/
         */
        final String XDG_DOWNLOAD_DIR = System.getenv("XDG_DOWNLOAD_DIR");
        if (!isEmpty(XDG_DOWNLOAD_DIR)) {
            if (XDG_DOWNLOAD_DIR.startsWith("/") && !XDG_DOWNLOAD_DIR.contains("$")) {
                return XDG_DOWNLOAD_DIR;
            } else if (XDG_DOWNLOAD_DIR.contains("$")) {
                final String HOME = System.getenv("HOME");
                String downloadDirectory = null;
                if (!isEmpty(HOME) && XDG_DOWNLOAD_DIR.contains("$HOME")) {
                    downloadDirectory = XDG_DOWNLOAD_DIR.replaceFirst("\\$HOME", HOME);
                }
                if (downloadDirectory != null && !downloadDirectory.contains("$")) {
                    //
                    return downloadDirectory;
                }
            }
        }
        return null;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.os.DesktopSupport#getProcessExecutablePathByPID(int)
     */
    @Override
    public String getProcessExecutablePathByPID(long pid) throws NotSupportedException {
        throw new NotSupportedException("Operating System not supported");
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.os.DesktopSupport#getProcessCommandlineByPID(int)
     */
    @Override
    public String getProcessCommandlineByPID(long pid) throws NotSupportedException {
        throw new NotSupportedException("Operating System not supported");
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.os.DesktopSupport#killProcessesByExecutablePath(java.lang.String, int)
     */
    @Override
    public long[] killProcessesByExecutablePath(String path, int exitCode) throws InterruptedException, NotSupportedException {
        throw new NotSupportedException("Operating System not supported");
    }

    @Override
    public int getPrefixLength(String pathname) {
        if (pathname.length() == 0) {
            return 0;
        } else {
            return (pathname.charAt(0) == '/') ? 1 : 0;
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.os.DesktopSupport#reboot(boolean, int)
     */
    @Override
    public boolean reboot(boolean force, int waitms) throws InterruptedException {
        try {
            // https://fhackts.wordpress.com/2019/08/08/shutting-down-or-rebooting-over-dbus-programmatically-from-a-non-root-user/
            dbusPowerState("Reboot");
        } catch (Throwable e) {
            org.appwork.loggingv3.LogV3.log(e);
        }

        return true;
    }
}
