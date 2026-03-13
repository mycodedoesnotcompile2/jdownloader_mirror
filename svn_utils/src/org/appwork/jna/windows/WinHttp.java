/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     See full license in project root.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.jna.windows;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.WinDef.DWORD;
import com.sun.jna.ptr.PointerByReference;
import com.sun.jna.win32.W32APIOptions;

/**
 * JNA interface to Windows WinHTTP API (winhttp.dll) for URL-based system proxy lookup via
 * {@link #WinHttpGetProxyForUrl}. Used by {@link org.appwork.utils.net.httpconnection.proxy.WindowsProxyHelper}.
 *
 * @see <a href="https://learn.microsoft.com/en-us/windows/win32/winhttp/winhttp-start-page">WinHTTP (Microsoft Learn)</a>
 * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/winhttp/nf-winhttp-winhttpgetproxyforurl">WinHttpGetProxyForUrl</a>
 */
public interface WinHttp extends Library {
    WinHttp INSTANCE = Native.load("winhttp", WinHttp.class, W32APIOptions.UNICODE_OPTIONS);

    /** Use default proxy (registry / system). */
    int WINHTTP_ACCESS_TYPE_DEFAULT_PROXY = 0;

    /** Use DHCP to locate PAC file (WPAD). */
    int WINHTTP_AUTO_DETECT_TYPE_DHCP   = 0x00000001;
    /** Use DNS to locate PAC file (WPAD). */
    int WINHTTP_AUTO_DETECT_TYPE_DNS_A  = 0x00000002;

    /**
     * Finds the PAC file URL using WPAD (DHCP and/or DNS). Does not download the file.
     * Caller must GlobalFree the string pointer written to ppwstrAutoConfigUrl.
     *
     * @param dwAutoDetectFlags WINHTTP_AUTO_DETECT_TYPE_DHCP and/or WINHTTP_AUTO_DETECT_TYPE_DNS_A.
     * @param ppwstrAutoConfigUrl Receives a pointer to the allocated URL string; must be freed with GlobalFree.
     * @return true if a PAC URL was found, false otherwise.
     * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/winhttp/nf-winhttp-winhttpdetectautoproxyconfigurl">WinHttpDetectAutoProxyConfigUrl</a>
     */
    boolean WinHttpDetectAutoProxyConfigUrl(int dwAutoDetectFlags, PointerByReference ppwstrAutoConfigUrl);

    /**
     * Opens a WinHTTP session. Use with WINHTTP_ACCESS_TYPE_DEFAULT_PROXY to use system proxy settings for
     * WinHttpGetProxyForUrl.
     *
     * @param pswzUserAgent   User agent string (can be null).
     * @param dwAccessType    WINHTTP_ACCESS_TYPE_DEFAULT_PROXY (0) for system proxy.
     * @param pswzProxyName   Proxy name (null when using default).
     * @param pswzProxyBypass Bypass list (null when using default).
     * @param dwFlags         Reserved, use 0.
     * @return Session handle (HINTERNET), or null on failure.
     */
    Pointer WinHttpOpen(String pswzUserAgent, int dwAccessType, Pointer pswzProxyName, Pointer pswzProxyBypass, DWORD dwFlags);

    /**
     * Closes a WinHTTP handle.
     *
     * @param hInternet Handle returned by WinHttpOpen or other WinHTTP functions.
     * @return true on success.
     */
    boolean WinHttpCloseHandle(Pointer hInternet);

    /**
     * Retrieves the proxy configuration for the given URL using system/IE proxy settings and PAC scripts.
     * Caller must GlobalFree the lpszProxy and lpszProxyBypass members of pProxyInfo after use.
     *
     * @param hSession           Session from WinHttpOpen.
     * @param lpcwszUrl          Target URL (e.g. "http://google.com").
     * @param pAutoProxyOptions  Options for PAC detection (auto-detect and/or config URL).
     * @param pProxyInfo         Receives proxy info; lpszProxy and lpszProxyBypass must be freed with GlobalFree.
     * @return true if proxy info was retrieved, false otherwise.
     */
    boolean WinHttpGetProxyForUrl(Pointer hSession, String lpcwszUrl, WinHttpAutoProxyOptions pAutoProxyOptions, WinHttpProxyInfo pProxyInfo);
}
