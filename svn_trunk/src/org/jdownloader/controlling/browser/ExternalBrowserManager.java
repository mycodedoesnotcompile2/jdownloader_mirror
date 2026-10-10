package org.jdownloader.controlling.browser;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.appwork.utils.os.CrossSystem;

/**
 * Central access point for everything related to external (system) browsers.
 *
 * It offers a best-effort "lazy" name lookup that turns a browser executable path (as configured e.g. in the captcha browser solver
 * commandline) into a nice human readable name, and a file system based scan for installed browsers.
 */
public class ExternalBrowserManager {
    private static final ExternalBrowserManager INSTANCE = new ExternalBrowserManager();

    public static ExternalBrowserManager getInstance() {
        return INSTANCE;
    }

    /**
     * Maps a lower cased executable base name (extension already stripped) to a nice display name.
     */
    private final Map<String, String> knownBrowsers;

    private ExternalBrowserManager() {
        final Map<String, String> map = new HashMap<String, String>();
        /* Mozilla family */
        map.put("firefox", "Firefox");
        map.put("firefox-bin", "Firefox");
        map.put("firefox-esr", "Firefox ESR");
        map.put("librewolf", "LibreWolf");
        map.put("waterfox", "Waterfox");
        map.put("palemoon", "Pale Moon");
        map.put("seamonkey", "SeaMonkey");
        map.put("tor browser", "Tor Browser");
        /* Chromium family */
        map.put("chrome", "Google Chrome");
        map.put("google-chrome", "Google Chrome");
        map.put("google-chrome-stable", "Google Chrome");
        map.put("google chrome", "Google Chrome");
        map.put("chromium", "Chromium");
        map.put("chromium-browser", "Chromium");
        map.put("brave", "Brave");
        map.put("brave-browser", "Brave");
        map.put("vivaldi", "Vivaldi");
        map.put("vivaldi-stable", "Vivaldi");
        map.put("opera", "Opera");
        map.put("opera_gx", "Opera GX");
        map.put("launcher", "Opera GX");
        /* Microsoft */
        map.put("msedge", "Microsoft Edge");
        map.put("microsoft edge", "Microsoft Edge");
        /* Apple */
        map.put("safari", "Safari");
        this.knownBrowsers = map;
    }

    /**
     * Returns a nice human readable browser name for the given browser commandline (as e.g. stored by the captcha browser solver).
     *
     * The first non-null element of the commandline is treated as the executable path; the remaining elements (e.g. "%s") are ignored.
     *
     * @param commandline
     *            the configured browser commandline, e.g. { "C:\\Program Files\\Mozilla Firefox\\firefox.exe", "%s" }
     * @return a nice browser name (e.g. "Firefox"), or null if none could be derived.
     */
    public String getLazyBrowserName(final String[] commandline) {
        if (commandline == null) {
            return null;
        }
        for (final String arg : commandline) {
            if (arg != null && arg.trim().length() > 0) {
                return getLazyBrowserName(arg);
            }
        }
        return null;
    }

    /**
     * Returns a nice human readable browser name for the given browser executable path.
     *
     * Example: "C:\\Program Files\\Mozilla Firefox\\firefox.exe" -> "Firefox".
     *
     * If the executable is unknown, a capitalized version of the plain executable name is returned as a fallback. Returns null only for
     * null/empty input.
     *
     * @param cmdpath
     *            the browser executable path.
     * @return a nice browser name, or null if the input is null/empty.
     */
    public String getLazyBrowserName(final String cmdpath) {
        if (cmdpath == null || cmdpath.trim().length() == 0) {
            return null;
        }
        final String baseName = getExecutableBaseName(cmdpath);
        if (baseName.length() == 0) {
            return null;
        }
        final String known = knownBrowsers.get(baseName.toLowerCase(java.util.Locale.ENGLISH));
        if (known != null) {
            return known;
        }
        /* Unknown browser -> fall back to a capitalized version of the plain executable name. */
        return capitalize(baseName);
    }

    /**
     * Extracts the plain executable name from a path: strips any directory part, trailing path separators and a trailing ".exe" or ".app"
     * extension.
     *
     * Handles both windows ("\\") and unix ("/") separators as well as macOS ".app" bundle paths.
     */
    private String getExecutableBaseName(final String cmdpath) {
        String path = cmdpath.trim();
        /* Remove surrounding quotes that may wrap a path containing spaces. */
        if (path.length() >= 2 && path.startsWith("\"") && path.endsWith("\"")) {
            path = path.substring(1, path.length() - 1).trim();
        }
        /* Drop trailing path separators. */
        while (path.length() > 0 && (path.endsWith("/") || path.endsWith("\\"))) {
            path = path.substring(0, path.length() - 1);
        }
        /* Reduce to the last path element (handle both separator styles). */
        final int lastSlash = Math.max(path.lastIndexOf('/'), path.lastIndexOf('\\'));
        if (lastSlash >= 0) {
            path = path.substring(lastSlash + 1);
        }
        /* Strip a trailing executable/bundle extension. */
        final String lower = path.toLowerCase(java.util.Locale.ENGLISH);
        if (lower.endsWith(".exe") || lower.endsWith(".app")) {
            path = path.substring(0, path.length() - 4);
        }
        return path.trim();
    }

    /**
     * Capitalizes the first character of the given text and leaves the rest untouched.
     */
    private String capitalize(final String text) {
        if (text.length() == 0) {
            return text;
        }
        return Character.toUpperCase(text.charAt(0)) + text.substring(1);
    }

    /**
     * A browser installation found on this system.
     */
    public static final class InstalledBrowser {
        private final String name;
        private final String path;

        /**
         * @param name
         *            human readable name
         * @param path
         *            absolute path of the executable, or null for the "OS default" dummy entry
         */
        public InstalledBrowser(final String name, final String path) {
            this.name = name;
            this.path = path;
        }

        public String getName() {
            return name;
        }

        /** Absolute path of the executable, or null if this is the "OS default" dummy entry. */
        public String getPath() {
            return path;
        }

        @Override
        public String toString() {
            return name + " (" + path + ")";
        }
    }

    /*
     * Windows: {display name, path relative to one of the install roots}. Roots are %ProgramFiles%, %ProgramFiles(x86)%, %ProgramW6432%,
     * %LocalAppData% and %LocalAppData%\Programs.
     */
    private static final String[][] WINDOWS_BROWSERS = new String[][] { { "Firefox", "Mozilla Firefox/firefox.exe" }, { "Firefox Developer Edition", "Firefox Developer Edition/firefox.exe" }, { "Firefox Nightly", "Firefox Nightly/firefox.exe" }, { "LibreWolf", "LibreWolf/librewolf.exe" }, { "Waterfox", "Waterfox/waterfox.exe" }, { "Pale Moon", "Moonchild Productions/Pale Moon/palemoon.exe" }, { "SeaMonkey", "SeaMonkey/seamonkey.exe" }, { "Google Chrome", "Google/Chrome/Application/chrome.exe" }, { "Google Chrome Beta", "Google/Chrome Beta/Application/chrome.exe" }, { "Google Chrome Dev", "Google/Chrome Dev/Application/chrome.exe" }, { "Google Chrome Canary", "Google/Chrome SxS/Application/chrome.exe" }, { "Chromium", "Chromium/Application/chrome.exe" }, { "Microsoft Edge", "Microsoft/Edge/Application/msedge.exe" }, { "Microsoft Edge Beta", "Microsoft/Edge Beta/Application/msedge.exe" }, { "Microsoft Edge Dev", "Microsoft/Edge Dev/Application/msedge.exe" }, { "Brave", "BraveSoftware/Brave-Browser/Application/brave.exe" }, { "Vivaldi", "Vivaldi/Application/vivaldi.exe" }, { "Opera", "Opera/opera.exe" }, { "Opera GX", "Opera GX/opera.exe" } };
    /* Linux: {display name, executable name}. Searched in $PATH and a few well known directories. */
    private static final String[][] LINUX_BROWSERS   = new String[][] { { "Firefox", "firefox" }, { "Firefox", "firefox-bin" }, { "Firefox ESR", "firefox-esr" }, { "Firefox", "org.mozilla.firefox" }, { "LibreWolf", "librewolf" }, { "LibreWolf", "io.gitlab.librewolf-community" }, { "Waterfox", "waterfox" }, { "Waterfox", "net.waterfox.waterfox" }, { "Pale Moon", "palemoon" }, { "SeaMonkey", "seamonkey" }, { "Google Chrome", "google-chrome" }, { "Google Chrome", "google-chrome-stable" }, { "Google Chrome", "com.google.Chrome" }, { "Google Chrome Beta", "google-chrome-beta" }, { "Google Chrome Dev", "google-chrome-unstable" }, { "Chromium", "chromium" }, { "Chromium", "chromium-browser" }, { "Chromium", "org.chromium.Chromium" }, { "Microsoft Edge", "microsoft-edge" }, { "Microsoft Edge", "microsoft-edge-stable" }, { "Microsoft Edge", "com.microsoft.Edge" }, { "Microsoft Edge Beta", "microsoft-edge-beta" }, { "Microsoft Edge Dev", "microsoft-edge-dev" }, { "Brave", "brave" }, { "Brave", "brave-browser" }, { "Brave", "com.brave.Browser" }, { "Vivaldi", "vivaldi" }, { "Vivaldi", "vivaldi-stable" }, { "Vivaldi", "com.vivaldi.Vivaldi" }, { "Opera", "opera" }, { "Opera", "com.opera.Opera" } };
    /* macOS: {display name, path inside /Applications or ~/Applications}. */
    private static final String[][] MAC_BROWSERS     = new String[][] { { "Firefox", "Firefox.app/Contents/MacOS/firefox" }, { "Firefox Developer Edition", "Firefox Developer Edition.app/Contents/MacOS/firefox" }, { "Firefox Nightly", "Firefox Nightly.app/Contents/MacOS/firefox" }, { "LibreWolf", "LibreWolf.app/Contents/MacOS/librewolf" }, { "Waterfox", "Waterfox.app/Contents/MacOS/waterfox" }, { "Google Chrome", "Google Chrome.app/Contents/MacOS/Google Chrome" }, { "Chromium", "Chromium.app/Contents/MacOS/Chromium" }, { "Microsoft Edge", "Microsoft Edge.app/Contents/MacOS/Microsoft Edge" }, { "Brave", "Brave Browser.app/Contents/MacOS/Brave Browser" }, { "Vivaldi", "Vivaldi.app/Contents/MacOS/Vivaldi" }, { "Opera", "Opera.app/Contents/MacOS/Opera" } };

    /**
     * Scans the system for installed browsers and returns them. The Windows registry is intentionally not used, only the file system is
     * checked. If multiple installations of the same browser exist (different executables), all of them are returned.
     *
     * This does file system access and should not be called from the EDT.
     *
     * @return the installed browsers, never null.
     */
    public List<InstalledBrowser> getInstalledBrowsers() {
        final List<InstalledBrowser> ret = new ArrayList<InstalledBrowser>();
        /* Used to filter out the same executable found via different paths (symlinks, duplicate roots). */
        final Set<String> dupes = new HashSet<String>();
        if (CrossSystem.isWindows()) {
            final List<File> roots = new ArrayList<File>();
            final String[] envs = new String[] { "ProgramFiles", "ProgramFiles(x86)", "ProgramW6432", "LocalAppData" };
            for (final String env : envs) {
                final String value = System.getenv(env);
                if (value != null && value.trim().length() > 0) {
                    roots.add(new File(value));
                    if ("LocalAppData".equals(env)) {
                        roots.add(new File(value, "Programs"));
                    }
                }
            }
            for (final String[] browser : WINDOWS_BROWSERS) {
                for (final File root : roots) {
                    addIfExecutable(ret, dupes, browser[0], new File(root, browser[1]));
                }
            }
        } else if (CrossSystem.isMac()) {
            final List<File> roots = new ArrayList<File>();
            roots.add(new File("/Applications"));
            roots.add(new File(System.getProperty("user.home"), "Applications"));
            for (final String[] browser : MAC_BROWSERS) {
                for (final File root : roots) {
                    addIfExecutable(ret, dupes, browser[0], new File(root, browser[1]));
                }
            }
        } else {
            /* Linux and other unix like systems */
            final List<File> dirs = new ArrayList<File>();
            final String pathEnv = System.getenv("PATH");
            if (pathEnv != null) {
                for (final String dir : pathEnv.split(File.pathSeparator)) {
                    if (dir.trim().length() > 0) {
                        dirs.add(new File(dir));
                    }
                }
            }
            dirs.add(new File("/usr/bin"));
            dirs.add(new File("/usr/local/bin"));
            dirs.add(new File("/snap/bin"));
            dirs.add(new File("/var/lib/flatpak/exports/bin"));
            dirs.add(new File(System.getProperty("user.home"), ".local/share/flatpak/exports/bin"));
            for (final String[] browser : LINUX_BROWSERS) {
                for (final File dir : dirs) {
                    addIfExecutable(ret, dupes, browser[0], new File(dir, browser[1]));
                }
            }
        }
        return ret;
    }

    private void addIfExecutable(final List<InstalledBrowser> list, final Set<String> dupes, final String name, final File file) {
        if (!file.isFile()) {
            return;
        }
        String key;
        try {
            key = file.getCanonicalPath();
        } catch (final IOException e) {
            key = file.getAbsolutePath();
        }
        if (CrossSystem.isWindows()) {
            key = key.toLowerCase(Locale.ENGLISH);
        }
        if (dupes.add(key)) {
            list.add(new InstalledBrowser(name, file.getAbsolutePath()));
        }
    }
}
