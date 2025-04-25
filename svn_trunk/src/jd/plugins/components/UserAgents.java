package jd.plugins.components;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.regex.Pattern;

import org.appwork.utils.net.HTTPHeader;

/**
 * Enhanced User-Agent generator that creates realistic browser headers Dynamically creates user agents instead of using static lists
 * Maintains backward compatibility through the stringUserAgent() method
 */
public class UserAgents {
    private static final Random   RANDOM                = new Random();
    // Device types
    public static final String    DEVICE_DESKTOP        = "desktop";
    public static final String    DEVICE_MOBILE         = "mobile";
    public static final String[]  DEVICES               = { DEVICE_DESKTOP, DEVICE_MOBILE };
    // Platform types
    public static final String    PLATFORM_WINDOWS      = "windows";
    public static final String    PLATFORM_MACOS        = "macos";
    public static final String    PLATFORM_LINUX        = "linux";
    public static final String    PLATFORM_ANDROID      = "android";
    public static final String    PLATFORM_IOS          = "ios";
    public static final String[]  PLATFORMS             = { PLATFORM_WINDOWS, PLATFORM_MACOS, PLATFORM_LINUX, PLATFORM_ANDROID, PLATFORM_IOS };
    public static final String[]  PLATFORMS_DESKTOP     = { PLATFORM_WINDOWS, PLATFORM_MACOS, PLATFORM_LINUX };
    public static final String[]  PLATFORMS_MOBILE      = { PLATFORM_ANDROID, PLATFORM_IOS };
    // Browser types
    public static final String    BROWSER_CHROME        = "chrome";
    public static final String    BROWSER_FIREFOX       = "firefox";
    public static final String    BROWSER_SAFARI        = "safari";
    public static final String    BROWSER_EDGE          = "edge";
    public static final String[]  BROWSERS              = { BROWSER_CHROME, BROWSER_FIREFOX, BROWSER_SAFARI, BROWSER_EDGE };
    public static final String[]  BROWSERS_SUPPORT_CH   = { BROWSER_CHROME, BROWSER_EDGE };
    // Version ranges for browsers
    private static final int[]    CHROME_VERSION_RANGE  = { 96, 115 };                                                                                                                                                                                                                                            // min,
    // max
    private static final int[]    FIREFOX_VERSION_RANGE = { 95, 115 };
    private static final int[]    SAFARI_VERSION_RANGE  = { 14, 17 };
    private static final int[]    EDGE_VERSION_RANGE    = { 96, 115 };
    // Version ranges for platforms
    private static final String[] WINDOWS_VERSIONS      = { "Windows NT 10.0; Win64; x64", "Windows NT 10.0; WOW64", "Windows NT 10.0" };
    private static final String[] MACOS_VERSIONS        = { "Macintosh; Intel Mac OS X 10_15_7", "Macintosh; Intel Mac OS X 10_15_6", "Macintosh; Intel Mac OS X 10_15", "Macintosh; Intel Mac OS X 10_14_6" };
    private static final String[] LINUX_VERSIONS        = { "X11; Linux x86_64", "X11; Ubuntu; Linux x86_64" };
    private static final String[] ANDROID_VERSIONS      = { "Linux; Android 13", "Linux; Android 12", "Linux; Android 11", "Linux; Android 10" };
    private static final String[] IOS_VERSIONS          = { "iPhone; CPU iPhone OS 16_5_1 like Mac OS X", "iPhone; CPU iPhone OS 16_4 like Mac OS X", "iPhone; CPU iPhone OS 15_7_1 like Mac OS X", "iPad; CPU OS 16_5_1 like Mac OS X", "iPad; CPU OS 16_4 like Mac OS X", "iPad; CPU OS 15_7_1 like Mac OS X" };
    private static final String[] ANDROID_MOBILE_MODELS = { "SM-S918B", "SM-G991B", // Samsung
            // S23,
            // S21
            "Pixel 7 Pro", "Pixel 7", // Google
            // Pixels
            "OnePlus 11", "OnePlus 10 Pro", // OnePlus
            "Redmi Note 12 Pro", "Mi 13 Pro" // Xiaomi
                                                        };
    private static final String[] ANDROID_TABLET_MODELS = { "SM-X910", "SM-X810", // Samsung
            // Galaxy
            // Tab
            // S9
            "SM-T870", "SM-T970", // Samsung
            // Galaxy
            // Tab
            // S7
            "Lenovo TB-X606F", "HUAWEI MatePad"        };

    /**
     * Browser Names, this is used for defining stringUserAgent(BrowserName); Maintained for backward compatibility
     */
    public enum BrowserName {
        Chrome(" Chrome/"),
        Firefox(" Firefox/"),
        Safari(" Version/[0-9]\\.[0-9]\\.[0-9] Safari/[0-9]{3}\\.[0-9]\\.[0-9]");

        private final Pattern pattern;

        public Pattern getPattern() {
            return this.pattern;
        }

        private BrowserName(final String s) {
            this.pattern = Pattern.compile(s);
        }
    }

    /**
     * Options for user agent generation
     */
    public static class Options {
        private boolean  randomizeUserAgent = true;
        private String[] deviceTypes        = null;
        private String[] platformTypes      = null;
        private String[] browserTypes       = null;

        public Options() {
        }

        public Options setDeviceTypes(String... deviceTypes) {
            this.deviceTypes = deviceTypes;
            return this;
        }

        public Options setPlatformTypes(String... platformTypes) {
            this.platformTypes = platformTypes;
            return this;
        }

        public Options setBrowserTypes(String... browserTypes) {
            this.browserTypes = browserTypes;
            return this;
        }

        public Options setRandomizeUserAgent(boolean randomize) {
            this.randomizeUserAgent = randomize;
            return this;
        }

        public String[] getDeviceTypes() {
            return deviceTypes;
        }

        public String[] getPlatformTypes() {
            return platformTypes;
        }

        public String[] getBrowserTypes() {
            return browserTypes;
        }

        public boolean isRandomizeUserAgent() {
            return randomizeUserAgent;
        }
    }

    /**
     * Holds the generated user agent information
     */
    public static class UserAgent {
        private final String              device;
        private final String              platform;
        private final String              browser;
        private final String              userAgentString;
        private final Map<String, String> headers;
        private final Map<String, String> clientHints;
        private final List<HTTPHeader>    httpHeaders;

        public UserAgent(String device, String platform, String browser, String userAgentString, Map<String, String> headers, Map<String, String> clientHints) {
            this.device = device;
            this.platform = platform;
            this.browser = browser;
            this.userAgentString = userAgentString;
            this.headers = headers;
            this.clientHints = clientHints;
            // Convert all headers to HTTPHeader objects
            this.httpHeaders = new ArrayList<HTTPHeader>();
            for (Map.Entry<String, String> entry : headers.entrySet()) {
                httpHeaders.add(new HTTPHeader(entry.getKey(), entry.getValue(), true));
            }
        }

        public String getDevice() {
            return device;
        }

        public String getPlatform() {
            return platform;
        }

        public String getBrowser() {
            return browser;
        }

        public String getUserAgentString() {
            return userAgentString;
        }

        public Map<String, String> getHeaders() {
            return headers;
        }

        public Map<String, String> getClientHints() {
            return clientHints;
        }

        /**
         * Returns a list of HTTPHeader objects representing all headers
         *
         * @return List of HTTPHeader objects with overwriteAllowed set to true
         */
        public List<HTTPHeader> getHTTPHeaders() {
            return httpHeaders;
        }

        @Override
        public String toString() {
            return userAgentString;
        }
    }

    /**
     * Generator class that creates user agents with headers
     */
    public static class Generator {
        private final String              device;
        private final String              platform;
        private final String              browser;
        private final Options             options;
        private final String              userAgent;
        private final Map<String, String> headers     = new HashMap<String, String>();
        private final Map<String, String> clientHints = new HashMap<String, String>();
        private final List<HTTPHeader>    httpHeaders = new ArrayList<HTTPHeader>();

        public Generator(String device, String platform, String browser, Options options) {
            this.device = device;
            this.platform = platform;
            this.browser = browser;
            this.options = options;
            this.userAgent = buildUserAgentString();
            buildHeaders();
            if (supportsClientHints()) {
                buildClientHints();
            }
            buildHTTPHeaders();
        }

        private String buildUserAgentString() {
            StringBuilder ua = new StringBuilder("Mozilla/5.0 (");
            // Add OS/platform info
            if (PLATFORM_WINDOWS.equals(platform)) {
                ua.append(getRandomItem(WINDOWS_VERSIONS));
            } else if (PLATFORM_MACOS.equals(platform)) {
                ua.append(getRandomItem(MACOS_VERSIONS));
            } else if (PLATFORM_LINUX.equals(platform)) {
                ua.append(getRandomItem(LINUX_VERSIONS));
            } else if (PLATFORM_ANDROID.equals(platform)) {
                ua.append(getRandomItem(ANDROID_VERSIONS));
                if (DEVICE_MOBILE.equals(device)) {
                    ua.append("; ").append(getRandomItem(ANDROID_MOBILE_MODELS));
                } else {
                    ua.append("; ").append(getRandomItem(ANDROID_TABLET_MODELS));
                }
            } else if (PLATFORM_IOS.equals(platform)) {
                ua.append(getRandomItem(IOS_VERSIONS));
            }
            // Add browser info
            ua.append(") AppleWebKit/537.36 (KHTML, like Gecko) ");
            if (BROWSER_CHROME.equals(browser)) {
                int chromeVersion = getRandomInt(CHROME_VERSION_RANGE[0], CHROME_VERSION_RANGE[1]);
                int chromeBuild = getRandomInt(4500, 5999);
                ua.append("Chrome/").append(chromeVersion).append(".0.").append(chromeBuild).append(".").append(getRandomInt(10, 99));
                ua.append(" Safari/537.36");
            } else if (BROWSER_FIREFOX.equals(browser)) {
                int firefoxVersion = getRandomInt(FIREFOX_VERSION_RANGE[0], FIREFOX_VERSION_RANGE[1]);
                ua.append("Firefox/").append(firefoxVersion).append(".0");
            } else if (BROWSER_EDGE.equals(browser)) {
                int edgeVersion = getRandomInt(EDGE_VERSION_RANGE[0], EDGE_VERSION_RANGE[1]);
                int edgeBuild = getRandomInt(1000, 1500);
                ua.append("Chrome/").append(edgeVersion).append(".0.").append(getRandomInt(4500, 5999)).append(".").append(getRandomInt(10, 99));
                ua.append(" Safari/537.36 Edg/").append(edgeVersion).append(".0.").append(edgeBuild).append(".").append(getRandomInt(10, 99));
            } else if (BROWSER_SAFARI.equals(browser)) {
                if (PLATFORM_IOS.equals(platform)) {
                    ua.append("Version/").append(getRandomInt(15, 17)).append(".").append(getRandomInt(0, 6)).append(" Mobile/15E148 Safari/604.1");
                } else {
                    int safariVersion = getRandomInt(SAFARI_VERSION_RANGE[0], SAFARI_VERSION_RANGE[1]);
                    ua.append("Version/").append(safariVersion).append(".").append(getRandomInt(0, 6)).append(".").append(getRandomInt(1, 15));
                    ua.append(" Safari/605.1.15");
                }
            }
            return ua.toString();
        }

        private void buildHeaders() {
            headers.put("User-Agent", userAgent);
            // Common headers for all browsers
            headers.put("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8");
            headers.put("Accept-Language", "en-US,en;q=0.9");
            headers.put("Accept-Encoding", "gzip, deflate, br");
            headers.put("Connection", "keep-alive");
            // Browser-specific headers
            if (BROWSER_FIREFOX.equals(browser)) {
                headers.put("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8");
                headers.put("Upgrade-Insecure-Requests", "1");
                headers.put("TE", "trailers");
            }
        }

        private boolean supportsClientHints() {
            for (String supportedBrowser : BROWSERS_SUPPORT_CH) {
                if (browser.equals(supportedBrowser)) {
                    return true;
                }
            }
            return false;
        }

        private void buildClientHints() {
            clientHints.put("Sec-CH-UA-Mobile", device.equals(DEVICE_MOBILE) ? "?1" : "?0");
            clientHints.put("Sec-CH-UA-Platform", "\"" + capitalizeFirstLetter(platform) + "\"");
            // Chrome and Edge specific
            if (BROWSER_CHROME.equals(browser) || BROWSER_EDGE.equals(browser)) {
                int majorVersion;
                if (BROWSER_CHROME.equals(browser)) {
                    majorVersion = getRandomInt(CHROME_VERSION_RANGE[0], CHROME_VERSION_RANGE[1]);
                } else {
                    majorVersion = getRandomInt(EDGE_VERSION_RANGE[0], EDGE_VERSION_RANGE[1]);
                }
                String browserBrand = BROWSER_CHROME.equals(browser) ? "Google Chrome" : "Microsoft Edge";
                String uaList = "\"" + browserBrand + "\";v=\"" + majorVersion + "\", ";
                if (majorVersion >= 100) {
                    uaList += "\"Not=A?Brand\";v=\"99\", \"Chromium\";v=\"" + majorVersion + "\"";
                } else {
                    uaList += "\" Not A;Brand\";v=\"99\", \"Chromium\";v=\"" + majorVersion + "\"";
                }
                clientHints.put("Sec-CH-UA", uaList);
                // Add these headers to the main headers
                headers.put("Sec-CH-UA", clientHints.get("Sec-CH-UA"));
                headers.put("Sec-CH-UA-Mobile", clientHints.get("Sec-CH-UA-Mobile"));
                headers.put("Sec-CH-UA-Platform", clientHints.get("Sec-CH-UA-Platform"));
                // Add additional client hint headers commonly used
                headers.put("sec-ch-ua-full-version-list", uaList);
                if (BROWSER_CHROME.equals(browser)) {
                    headers.put("sec-ch-ua-arch", "\"x86\"");
                    headers.put("sec-ch-ua-bitness", "\"64\"");
                    headers.put("sec-ch-ua-full-version", "\"" + majorVersion + ".0.0.0\"");
                    headers.put("sec-ch-ua-model", "\"\"");
                    headers.put("sec-ch-ua-wow64", "?0");
                    headers.put("sec-fetch-dest", "document");
                    headers.put("sec-fetch-mode", "navigate");
                    headers.put("sec-fetch-site", "none");
                    headers.put("sec-fetch-user", "?1");
                    headers.put("upgrade-insecure-requests", "1");
                }
            }
        }

        private void buildHTTPHeaders() {
            // Convert all headers to HTTPHeader objects
            for (Map.Entry<String, String> entry : headers.entrySet()) {
                httpHeaders.add(new HTTPHeader(entry.getKey(), entry.getValue(), true));
            }
        }

        public String getUserAgent() {
            return userAgent;
        }

        public Map<String, String> getHeaders() {
            return headers;
        }

        public Map<String, String> getClientHints() {
            return clientHints;
        }

        /**
         * Returns a list of HTTPHeader objects representing all headers
         *
         * @return List of HTTPHeader objects with overwriteAllowed set to true
         */
        public List<HTTPHeader> getHTTPHeaders() {
            return httpHeaders;
        }
    }

    /**
     * Generate a random user agent with default options
     *
     * @return UserAgent object containing the generated user agent
     */
    public static UserAgent generate() {
        return generate(new Options());
    }

    /**
     * Generate a random user agent with custom options
     *
     * @param options
     *            Configuration options for user agent generation
     * @return UserAgent object containing the generated user agent
     */
    public static UserAgent generate(Options options) {
        String device = selectDevice(options);
        String platform = selectPlatform(device, options);
        String browser = selectBrowser(platform, options);
        Generator generator = new Generator(device, platform, browser, options);
        return new UserAgent(device, platform, browser, generator.getUserAgent(), generator.getHeaders(), generator.getClientHints());
    }

    /**
     * Generate a list of HTTPHeader objects for the user agent
     *
     * @return List of HTTPHeader objects with overwriteAllowed set to true
     */
    public static List<HTTPHeader> generateHTTPHeaders() {
        return generateHTTPHeaders(new Options());
    }

    /**
     * Generate a list of HTTPHeader objects for the user agent with custom options
     *
     * @param options
     *            Configuration options for user agent generation
     * @return List of HTTPHeader objects with overwriteAllowed set to true
     */
    public static List<HTTPHeader> generateHTTPHeaders(Options options) {
        String device = selectDevice(options);
        String platform = selectPlatform(device, options);
        String browser = selectBrowser(platform, options);
        Generator generator = new Generator(device, platform, browser, options);
        return generator.getHTTPHeaders();
    }

    /**
     * Generate just a user agent string without additional headers
     *
     * @return Random user agent string
     */
    public static String randomUserAgent() {
        return generate().getUserAgentString();
    }

    /**
     * Generate a user agent string for a specific browser
     *
     * @param browser
     *            Browser type to generate for
     * @return User agent string
     */
    public static String randomUserAgent(String browser) {
        Options options = new Options();
        options.setBrowserTypes(new String[] { browser });
        return generate(options).getUserAgentString();
    }

    /**
     * Generate a user agent string for a specific device type and browser
     *
     * @param device
     *            Device type to generate for
     * @param browser
     *            Browser type to generate for
     * @return User agent string
     */
    public static String randomUserAgent(String device, String browser) {
        Options options = new Options();
        options.setDeviceTypes(new String[] { device });
        options.setBrowserTypes(new String[] { browser });
        return generate(options).getUserAgentString();
    }

    /**
     * Returns a random User-Agent String Original method maintained for backward compatibility
     */
    public static String stringUserAgent() {
        return stringUserAgent(null);
    }

    /**
     * Returns a random User-Agent String of BrowserName Original method maintained for backward compatibility
     */
    public static String stringUserAgent(BrowserName browser) {
        final Options options = new Options();
        options.setPlatformTypes(PLATFORMS_DESKTOP);
        if (browser != null) {
            if (BrowserName.Chrome.equals(browser)) {
                options.setBrowserTypes(new String[] { BROWSER_CHROME });
            } else if (BrowserName.Firefox.equals(browser)) {
                options.setBrowserTypes(new String[] { BROWSER_FIREFOX });
            } else if (BrowserName.Safari.equals(browser)) {
                options.setBrowserTypes(new String[] { BROWSER_SAFARI });
            }
        }
        return generate(options).getUserAgentString();
    }

    /**
     * Returns a random User-Agent String from a portable device Original method maintained for backward compatibility
     */
    public static String portableUserAgent() {
        Options options = new Options();
        options.setDeviceTypes(new String[] { DEVICE_MOBILE });
        return generate(options).getUserAgentString();
    }

    /**
     * Select a device based on options
     */
    private static String selectDevice(Options options) {
        if (options.getDeviceTypes() != null && options.getDeviceTypes().length > 0) {
            return getRandomItem(options.getDeviceTypes());
        }
        return getRandomItem(DEVICES);
    }

    /**
     * Select a compatible platform based on device and options
     */
    private static String selectPlatform(String device, Options options) {
        String[] platformPool;
        if (options.getPlatformTypes() != null && options.getPlatformTypes().length > 0) {
            platformPool = options.getPlatformTypes();
        } else if (DEVICE_DESKTOP.equals(device)) {
            platformPool = PLATFORMS_DESKTOP;
        } else if (DEVICE_MOBILE.equals(device)) {
            platformPool = PLATFORMS_MOBILE;
        } else {
            platformPool = PLATFORMS;
        }
        return getRandomItem(platformPool);
    }

    /**
     * Select a compatible browser based on platform and options
     */
    private static String selectBrowser(String platform, Options options) {
        if (options.getBrowserTypes() != null && options.getBrowserTypes().length > 0) {
            String browser = getRandomItem(options.getBrowserTypes());
            // Handle Safari compatibility
            if (BROWSER_SAFARI.equals(browser) && !PLATFORM_MACOS.equals(platform) && !PLATFORM_IOS.equals(platform)) {
                return BROWSER_CHROME; // Fallback to Chrome for platforms not supporting Safari
            }
            return browser;
        }
        // Safari only works on macOS and iOS
        if (PLATFORM_MACOS.equals(platform) || PLATFORM_IOS.equals(platform)) {
            return getRandomItem(BROWSERS);
        } else {
            // All browsers except Safari for other platforms
            List<String> compatibleBrowsers = new ArrayList<String>();
            for (String browser : BROWSERS) {
                if (!BROWSER_SAFARI.equals(browser)) {
                    compatibleBrowsers.add(browser);
                }
            }
            return getRandomItem(compatibleBrowsers.toArray(new String[0]));
        }
    }

    /**
     * Get a random item from an array
     */
    private static <T> T getRandomItem(T[] array) {
        if (array == null || array.length == 0) {
            return null;
        }
        return array[RANDOM.nextInt(array.length)];
    }

    /**
     * Get a random integer in a range (inclusive)
     */
    private static int getRandomInt(int min, int max) {
        return RANDOM.nextInt(max - min + 1) + min;
    }

    /**
     * Capitalize the first letter of a string
     */
    private static String capitalizeFirstLetter(String input) {
        if (input == null || input.isEmpty()) {
            return input;
        }
        return input.substring(0, 1).toUpperCase() + input.substring(1);
    }

    public static void main(String[] args) {
        // Backward compatibility method
        String backwardCompatString = UserAgents.stringUserAgent();
        System.out.println("Backward compatibility - stringUserAgent(): " + backwardCompatString);
        // Get a string for a mobile device
        String mobileString = UserAgents.portableUserAgent();
        System.out.println("Mobile user agent: " + mobileString);
        // Generate specific browser types
        String chromeAgent = UserAgents.randomUserAgent(UserAgents.BROWSER_CHROME);
        System.out.println("\nChrome agent: " + chromeAgent);
        String firefoxAgent = UserAgents.randomUserAgent(UserAgents.BROWSER_FIREFOX);
        System.out.println("\nFirefox agent: " + firefoxAgent);
        String safariAgent = UserAgents.randomUserAgent(UserAgents.BROWSER_SAFARI);
        System.out.println("\nSafari agent: " + safariAgent);
        // Generate full user agent with headers
        UserAgent fullAgent = UserAgents.generate();
        System.out.println("\n=== Full User Agent ===");
        System.out.println("Device: " + fullAgent.getDevice());
        System.out.println("Platform: " + fullAgent.getPlatform());
        System.out.println("Browser: " + fullAgent.getBrowser());
        System.out.println("User-Agent: " + fullAgent.getUserAgentString());
        System.out.println("\n=== Headers ===");
        Map<String, String> headers = fullAgent.getHeaders();
        for (Map.Entry<String, String> header : headers.entrySet()) {
            System.out.println(header.getKey() + ": " + header.getValue());
        }
        // Specific device/platform/browser combinations
        Options options = new Options().setDeviceTypes(UserAgents.DEVICE_DESKTOP).setPlatformTypes(UserAgents.PLATFORM_WINDOWS).setBrowserTypes(UserAgents.BROWSER_EDGE);
        UserAgent specificAgent = UserAgents.generate(options);
        System.out.println("\n=== Windows Edge Browser ===");
        System.out.println(specificAgent.getUserAgentString());
    }
}