package org.jdownloader.controlling.browser;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Central access point for everything related to external (system) browsers.
 *
 * For now this manager only offers a best-effort "lazy" name lookup that turns a browser executable path (as configured e.g. in the captcha
 * browser solver commandline) into a nice human readable name.
 *
 * In the future this class is meant to also scan the system for installed browsers and return them.
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
     * Scans the system for installed browsers and returns them.
     *
     * Not implemented yet; currently always returns an empty list.
     */
    public List<String> getInstalledBrowsers() {
        return new ArrayList<String>();
    }
}
