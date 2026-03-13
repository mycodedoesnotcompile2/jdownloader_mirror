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

import java.util.Arrays;
import java.util.List;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.platform.win32.WinDef.DWORD;
import com.sun.jna.win32.W32APITypeMapper;

/**
 * WINHTTP_AUTOPROXY_OPTIONS structure for {@link WinHttp#WinHttpGetProxyForUrl}. Specifies how to obtain the PAC file
 * (auto-detect and/or explicit config URL).
 *
 * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/winhttp/ns-winhttp-winhttp_autoproxy_options">WINHTTP_AUTOPROXY_OPTIONS (Microsoft Learn)</a>
 */
public class WinHttpAutoProxyOptions extends Structure {
    /** Use WPAD (DHCP and/or DNS) to locate PAC file. */
    public static final int WINHTTP_AUTOPROXY_AUTO_DETECT       = 0x00000001;
    /** Download PAC from URL in lpszAutoConfigUrl. */
    public static final int WINHTTP_AUTOPROXY_CONFIG_URL         = 0x00000002;

    /** Use DHCP to locate PAC. */
    public static final int WINHTTP_AUTO_DETECT_TYPE_DHCP       = 0x00000001;
    /** Use DNS to locate PAC. */
    public static final int WINHTTP_AUTO_DETECT_TYPE_DNS_A      = 0x00000002;

    public DWORD   dwFlags;
    public DWORD   dwAutoDetectFlags;
    public Pointer lpszAutoConfigUrl;
    public Pointer lpvReserved;
    public DWORD   dwReserved;
    public boolean fAutoLogonIfChallenged;

    public WinHttpAutoProxyOptions() {
        super(W32APITypeMapper.UNICODE);
        dwFlags = new DWORD(0);
        dwAutoDetectFlags = new DWORD(0);
        lpszAutoConfigUrl = null;
        lpvReserved = null;
        dwReserved = new DWORD(0);
        fAutoLogonIfChallenged = false;
    }

    @Override
    protected List getFieldOrder() {
        return Arrays.asList(new String[] { "dwFlags", "dwAutoDetectFlags", "lpszAutoConfigUrl", "lpvReserved", "dwReserved", "fAutoLogonIfChallenged" });
    }
}
