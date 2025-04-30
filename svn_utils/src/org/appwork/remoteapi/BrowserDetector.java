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
package org.appwork.remoteapi;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.appwork.storage.Storable;
import org.appwork.utils.net.httpserver.requests.HttpRequestInterface;

public class BrowserDetector {
    // Enum for supported browsers.
    public static enum Browser {
        CHROME,
        FIREFOX,
        SAFARI,
        EDGE,
        IE,
        OPERA,
        CHROMIUM,
        OTHER
    }

    // Class to hold browser info.
    public static class BrowserInfo implements Storable {
        /**
         *
         */
        public BrowserInfo() {
        };

        private Browser browser;
        private double  version;

        public void setBrowser(Browser browser) {
            this.browser = browser;
        }

        public void setVersion(double version) {
            this.version = version;
        }

        public BrowserInfo(Browser browser, double version) {
            this.browser = browser;
            this.version = version;
        }

        public Browser getBrowser() {
            return browser;
        }

        public double getVersion() {
            return version;
        }
    }

    private final static Pattern EDGE     = Pattern.compile("Edg.*?(\\d+(?:\\.\\d+)?)", Pattern.CASE_INSENSITIVE);
    private final static Pattern OPERA    = Pattern.compile("OP.*?(\\d+(?:\\.\\d+)?)", Pattern.CASE_INSENSITIVE);
    private final static Pattern CHROME   = Pattern.compile("Google.*?Chrome.*?(\\d+(?:\\.\\d+)?)", Pattern.CASE_INSENSITIVE);
    private final static Pattern FIREFOX  = Pattern.compile("Firefox.*?(\\d+(?:\\.\\d+)?)", Pattern.CASE_INSENSITIVE);
    private final static Pattern SAFARI   = Pattern.compile("Version/(\\d+(?:\\.\\d+)?).*Safari", Pattern.CASE_INSENSITIVE);
    private final static Pattern IE       = Pattern.compile("(?:MSIE|Trident).*?(\\d+(?:\\.\\d+)?)", Pattern.CASE_INSENSITIVE);
    private final static Pattern CHROMIUM = Pattern.compile("Chromium.*?(\\d+(?:\\.\\d+)?)", Pattern.CASE_INSENSITIVE);

    /**
     * Detects the browser and its version using common headers. It first checks the "Sec-CH-UA" header and falls back to "User-Agent".
     *
     * @param request
     *            HTTP request containing browser headers.
     * @return BrowserInfo with detected Browser enum and version as double.
     */
    public static BrowserInfo detectBrowser(HttpRequestInterface request) {
        // Use Sec-CH-UA if available; otherwise, use User-Agent.
        String header = request.getRequestHeaders().getValue("Sec-CH-UA");
        if (header == null || header.isEmpty()) {
            header = request.getRequestHeaders().getValue("User-Agent");
        }
        return getBrowser(header);
    }

    public static void main(String[] args) {
        System.out.println(getBrowser("Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:136.0) Gecko/20100101 Firefox/136.0"));
    }

    public static BrowserInfo getBrowser(String header) {
        Browser browser = Browser.OTHER;
        double version = 0.0;
        Matcher matcher = EDGE.matcher(header);
        if (matcher.find()) {
            browser = Browser.EDGE;
            version = parseVersion(matcher.group(1));
            return new BrowserInfo(browser, version);
        }
        matcher = OPERA.matcher(header);
        if (matcher.find()) {
            browser = Browser.OPERA;
            version = parseVersion(matcher.group(1));
            return new BrowserInfo(browser, version);
        }
        matcher = CHROME.matcher(header);
        if (matcher.find()) {
            browser = Browser.CHROME;
            version = parseVersion(matcher.group(1));
            return new BrowserInfo(browser, version);
        }
        matcher = FIREFOX.matcher(header);
        if (matcher.find()) {
            browser = Browser.FIREFOX;
            version = parseVersion(matcher.group(1));
            return new BrowserInfo(browser, version);
        }
        matcher = SAFARI.matcher(header);
        if (matcher.find()) {
            browser = Browser.SAFARI;
            version = parseVersion(matcher.group(1));
            return new BrowserInfo(browser, version);
        }
        matcher = IE.matcher(header);
        if (matcher.find()) {
            browser = Browser.IE;
            version = parseVersion(matcher.group(1));
            return new BrowserInfo(browser, version);
        }
        matcher = CHROMIUM.matcher(header);
        if (matcher.find()) {
            browser = Browser.CHROMIUM;
            version = parseVersion(matcher.group(1));
            return new BrowserInfo(browser, version);
        }
        return new BrowserInfo(browser, version);
    }

    // Parses the version string to a double (major.minor).
    private static double parseVersion(String versionStr) {
        try {
            String[] parts = versionStr.split("\\.");
            String versionNumber = parts[0] + (parts.length > 1 ? "." + parts[1] : "");
            return Double.parseDouble(versionNumber);
        } catch (Exception e) {
            return 0.0;
        }
    }
}
