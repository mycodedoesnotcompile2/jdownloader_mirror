/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
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
package org.appwork.utils.os.windows.webview2;

import java.util.List;

import org.appwork.utils.net.httpconnection.HTTPProxy;

/**
 * Chooses the proxies for the connections of a {@link WindowsWebView2Browser}, see {@link WindowsWebView2Browser.Options#proxySelector}.
 * <p>
 * The browser sends all its traffic through a filter inside this process ({@link WebView2FilterProxy}). The filter asks this selector
 * for every connection and then connects itself: directly, or through the HTTP, HTTPS or SOCKS proxy the selector names. The proxy
 * settings of Windows are not used unless the selector returns them (for example from
 * {@code org.appwork.utils.net.httpconnection.proxy.WindowsProxyUtils}).
 */
public interface WebView2ProxySelector {
    /**
     * Called on a background thread for every connection the browser opens.
     *
     * @param url
     *            the target of the connection; path and scheme are never known, only host and port. Port 80 is passed as
     *            {@code http://host:80/}, every other port as {@code https://host:port/}.
     * @return the proxies to try, in this order, until one connects. {@code null}, an empty list, or a proxy of type {@code NONE} or
     *         {@code DIRECT} mean a direct connection.
     */
    List<HTTPProxy> getProxiesByURL(String url);
}
