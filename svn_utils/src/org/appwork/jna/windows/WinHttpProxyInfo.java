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

import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.win32.W32APITypeMapper;

/**
 * WINHTTP_PROXY_INFO structure: receives proxy configuration from {@link WinHttp#WinHttpGetProxyForUrl}. The caller
 * must call GlobalFree on lpszProxy and lpszProxyBypass after use.
 *
 * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/winhttp/ns-winhttp-winhttp_proxy_info">WINHTTP_PROXY_INFO (Microsoft Learn)</a>
 */
public class WinHttpProxyInfo extends Structure {
    /** No proxy / direct connection. */
    public static final int WINHTTP_ACCESS_TYPE_NO_PROXY    = 1;
    /** Use proxy; server list in lpszProxy. */
    public static final int WINHTTP_ACCESS_TYPE_NAMED_PROXY = 3;

    public int     dwAccessType;
    public Pointer lpszProxy;
    public Pointer lpszProxyBypass;

    public WinHttpProxyInfo() {
        super(W32APITypeMapper.UNICODE);
    }

    @Override
    protected List getFieldOrder() {
        return Arrays.asList(new String[] { "dwAccessType", "lpszProxy", "lpszProxyBypass" });
    }
}
