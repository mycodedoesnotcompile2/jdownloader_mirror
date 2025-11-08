/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
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
package org.appwork.jna.windows;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.appwork.JNAHelper;
import org.appwork.loggingv3.LogV3;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.net.httpconnection.HTTPProxy;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.NotSupportedException;

import com.sun.jna.platform.win32.Advapi32Util;
import com.sun.jna.platform.win32.Win32Exception;
import com.sun.jna.platform.win32.WinReg;

/**
 * Windows Proxy Utils using JNA for registry access
 *
 * @author thomas
 * @date 04.04.2022
 *
 */
public class JNAWindowsProxyUtils {
    private static final String INTERNET_SETTINGS_KEY = "Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings";

    /**
     * Gets the PAC script URL from Windows registry (AutoConfigURL) using JNA
     *
     * @return The PAC script URL if configured, null otherwise
     * @throws NotSupportedException
     *             if not running on Windows
     */
    public static String getWindowsPACScriptURL() throws NotSupportedException {
        if (!CrossSystem.isWindows()) {
            throw new NotSupportedException("JNAWindowsProxyUtils is only supported on Windows");
        }
        if (!JNAHelper.isJNAAvailable()) {
            return null;
        }
        try {
            final String autoProxy = Advapi32Util.registryGetStringValue(WinReg.HKEY_CURRENT_USER, INTERNET_SETTINGS_KEY, "AutoConfigURL");
            if (!StringUtils.isEmpty(autoProxy)) {
                LogV3.info("AutoProxy.pac Script found: " + autoProxy);
                return autoProxy;
            }
        } catch (final Win32Exception e) {
            if (e.getErrorCode() != com.sun.jna.platform.win32.WinError.ERROR_FILE_NOT_FOUND) {
                LogV3.log(e);
            }
        } catch (final Throwable e) {
            LogV3.log(e);
        }
        return null;
    }

    /**
     * Checks if a PAC script is configured in Windows system settings
     *
     * @return true if PAC script is configured, false otherwise
     * @throws NotSupportedException
     *             if not running on Windows
     */
    public static boolean isWindowsPACScriptConfigured() throws NotSupportedException {
        return getWindowsPACScriptURL() != null;
    }

    /**
     * Checks windows registry for proxy settings using JNA
     *
     * @throws NotSupportedException
     *             if not running on Windows
     */
    public static List<HTTPProxy> getWindowsRegistryProxies() throws NotSupportedException {
        final java.util.List<HTTPProxy> ret = new ArrayList<HTTPProxy>();
        if (!CrossSystem.isWindows()) {
            throw new NotSupportedException("JNAWindowsProxyUtils is only supported on Windows");
        }
        if (!JNAHelper.isJNAAvailable()) {
            return ret;
        }
        try {
            // Check for PAC script
            try {
                final String autoProxy = Advapi32Util.registryGetStringValue(WinReg.HKEY_CURRENT_USER, INTERNET_SETTINGS_KEY, "AutoConfigURL");
                if (!StringUtils.isEmpty(autoProxy)) {
                    LogV3.info("AutoProxy.pac Script found: " + autoProxy);
                }
            } catch (final Win32Exception e) {
                // AutoConfigURL not found, that's okay
            }
            // Check if proxy is enabled
            try {
                final int proxyEnable = Advapi32Util.registryGetIntValue(WinReg.HKEY_CURRENT_USER, INTERNET_SETTINGS_KEY, "ProxyEnable");
                if (proxyEnable == 0) {
                    // proxy disabled
                    return ret;
                }
            } catch (final Win32Exception e) {
                // ProxyEnable not found, assume disabled
                return ret;
            }
            // Get proxy server string
            String val = null;
            try {
                val = Advapi32Util.registryGetStringValue(WinReg.HKEY_CURRENT_USER, INTERNET_SETTINGS_KEY, "ProxyServer");
            } catch (final Win32Exception e) {
                // ProxyServer not found
            }
            // val might be
            // 1. the proxy host only e.g. 127.0.0.1 or myproxy.de
            // 2. a full url http://127.0.0.1 2. a key=value list of ; sep. entries protocol=<1.> or <2.>
            // http=proxyxy:8000;https=proxyxysec:8000;ftp=proxyxyftp:8000
            if (val != null && !val.isEmpty()) {
                for (String vals : val.split(";")) {
                    final String lowerCaseVals = vals.toLowerCase(Locale.ENGLISH);
                    if (lowerCaseVals.startsWith("ftp=")) {
                        continue;
                    }
                    vals = vals.replaceAll("^.*=", "");
                    /* parse ip */
                    String proxyurl = new Regex(vals, "(\\d+\\.\\d+\\.\\d+\\.\\d+)").getMatch(0);
                    if (proxyurl == null) {
                        try {
                            URL url = new URL(vals);
                            proxyurl = url.getHost();
                        } catch (MalformedURLException e) {
                        }
                        if (proxyurl == null) {
                            /* parse domain name */
                            proxyurl = new Regex(vals, ".+=(.*?)(:\\d+$|:\\d+/|/|$)").getMatch(0);
                            if (proxyurl == null) {
                                /* parse domain name */
                                proxyurl = new Regex(vals, "=?(.*?)(:\\d+$|:\\d+/|/|$)").getMatch(0);
                            }
                        }
                    }
                    final String port = new Regex(vals, ":(\\d+)(/|$)").getMatch(0);
                    if (proxyurl != null) {
                        if (lowerCaseVals.startsWith("socks")) {
                            final int rPOrt = port != null ? Integer.parseInt(port) : 1080;
                            final HTTPProxy pd = new HTTPProxy(HTTPProxy.TYPE.SOCKS5);
                            pd.setHost(proxyurl);
                            pd.setPort(rPOrt);
                            ret.add(pd);
                        } else if (lowerCaseVals.startsWith("https")) {
                            final int rPOrt = port != null ? Integer.parseInt(port) : 443;
                            final HTTPProxy pd = new HTTPProxy(HTTPProxy.TYPE.HTTPS);
                            pd.setHost(proxyurl);
                            pd.setPort(rPOrt);
                            ret.add(pd);
                        } else if (lowerCaseVals.startsWith("http")) {
                            final int rPOrt = port != null ? Integer.parseInt(port) : 8080;
                            final HTTPProxy pd = new HTTPProxy(HTTPProxy.TYPE.HTTP);
                            pd.setHost(proxyurl);
                            pd.setPort(rPOrt);
                            ret.add(pd);
                        } else {
                            final int rPOrt = port != null ? Integer.parseInt(port) : 8080;
                            final HTTPProxy pd = new HTTPProxy(HTTPProxy.TYPE.HTTP);
                            pd.setHost(proxyurl);
                            pd.setPort(rPOrt);
                            LogV3.info("Use HTTP as default Proxy Type " + pd);
                            ret.add(pd);
                        }
                    }
                }
            }
        } catch (final Throwable e) {
            LogV3.log(e);
        }
        return ret;
    }
}
