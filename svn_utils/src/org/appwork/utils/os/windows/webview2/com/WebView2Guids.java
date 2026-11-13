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
package org.appwork.utils.os.windows.webview2.com;

import com.sun.jna.platform.win32.Guid;
import com.sun.jna.platform.win32.Ole32Util;

/**
 * COM IIDs for the WebView2 Win32 API subset used by this binding.
 */
public final class WebView2Guids {
    public static final Guid.GUID IID_IUnknown                         = Ole32Util.getGUIDFromString("{00000000-0000-0000-C000-000000000046}");
    public static final Guid.GUID IID_EnvironmentCompletedHandler      = Ole32Util.getGUIDFromString("{4E8A3389-C9D8-4BD2-B6B5-124FEE6CC14D}");
    public static final Guid.GUID IID_ControllerCompletedHandler       = Ole32Util.getGUIDFromString("{6C4819F3-C9B7-4260-8127-C9F5BDE7F68C}");
    public static final Guid.GUID IID_ExecuteScriptCompletedHandler    = Ole32Util.getGUIDFromString("{49511172-CC67-4BCA-9923-137112F4C4CC}");
    public static final Guid.GUID IID_CallDevToolsProtocolMethodCompletedHandler = Ole32Util.getGUIDFromString("{5C4889F0-5EF6-4C5A-952C-D8F1B92D0574}");
    public static final Guid.GUID IID_WebMessageReceivedEventHandler   = Ole32Util.getGUIDFromString("{57213F19-00E6-49FA-8E07-898EA01ECBD2}");
    public static final Guid.GUID IID_NavigationStartingEventHandler   = Ole32Util.getGUIDFromString("{9ADBE429-F36D-432B-9DDC-F8881FBD76E3}");
    public static final Guid.GUID IID_PermissionRequestedEventHandler  = Ole32Util.getGUIDFromString("{15E1C6A3-C72A-4DF3-91D7-D097FBEC6BFD}");
    public static final Guid.GUID IID_DevToolsProtocolEventReceivedEventHandler = Ole32Util.getGUIDFromString("{E2FDA4BE-5456-406C-A261-3D452138362C}");
    public static final Guid.GUID IID_NavigationCompletedEventHandler  = Ole32Util.getGUIDFromString("{D33A35BF-1C49-4F98-93AB-006E0533FE1C}");
    public static final Guid.GUID IID_SourceChangedEventHandler        = Ole32Util.getGUIDFromString("{3C067F9F-5388-4772-8B48-79F7EF1AB37C}");
    public static final Guid.GUID IID_NewWindowRequestedEventHandler   = Ole32Util.getGUIDFromString("{D4C185FE-C81C-4989-97AF-2D3FA7AB5651}");
    public static final Guid.GUID IID_DocumentTitleChangedEventHandler = Ole32Util.getGUIDFromString("{F5F2B923-953E-4042-9F95-F3A118E1AFD4}");
    public static final Guid.GUID IID_WindowCloseRequestedEventHandler = Ole32Util.getGUIDFromString("{5C19E9E0-092F-486B-AFFA-CA8231913039}");
    public static final Guid.GUID IID_WebResourceRequestedEventHandler = Ole32Util.getGUIDFromString("{AB00B74C-15F1-4646-80E8-E76341D25D71}");
    /** {@code ICoreWebView2_2}, which adds {@code NavigateWithWebResourceRequest}. */
    public static final Guid.GUID IID_ICoreWebView2_2                  = Ole32Util.getGUIDFromString("{9E8F0CF8-E670-4B5E-B2BC-73E061E3184C}");
    /** {@code ICoreWebView2Environment2}, which adds {@code CreateWebResourceRequest}. */
    public static final Guid.GUID IID_ICoreWebView2Environment2        = Ole32Util.getGUIDFromString("{41F3632B-5EF4-404F-AD82-2D606C5A9A21}");
    /** {@code ICoreWebView2_4}, which adds {@code DownloadStarting}. */
    public static final Guid.GUID IID_ICoreWebView2_4                  = Ole32Util.getGUIDFromString("{20D02D59-6DF2-42DC-BD06-F98A694B1302}");
    public static final Guid.GUID IID_DownloadStartingEventHandler     = Ole32Util.getGUIDFromString("{EFEDC989-C396-41CA-83F7-07F845A55724}");
    /** {@code ICoreWebView2_11}, which adds {@code ContextMenuRequested}. */
    public static final Guid.GUID IID_ICoreWebView2_11                 = Ole32Util.getGUIDFromString("{0BE78E56-C193-4051-B943-23B460C08BDB}");
    /** {@code ICoreWebView2Environment9}, which adds {@code CreateContextMenuItem}. */
    public static final Guid.GUID IID_ICoreWebView2Environment9        = Ole32Util.getGUIDFromString("{F06F41BF-4B5A-49D8-B9F6-FA16CD29F274}");
    public static final Guid.GUID IID_ContextMenuRequestedEventHandler = Ole32Util.getGUIDFromString("{04D3FE1D-AB87-42FB-A898-DA241D35B63C}");
    public static final Guid.GUID IID_CustomItemSelectedEventHandler   = Ole32Util.getGUIDFromString("{49E1D0BC-FE9E-4481-B7C2-32324AA21998}");
    /** {@code ICoreWebView2_15}, which adds the favicon. */
    public static final Guid.GUID IID_ICoreWebView2_15                 = Ole32Util.getGUIDFromString("{517B2D1D-7DAE-4A66-A4F4-10352FFB9518}");
    public static final Guid.GUID IID_FaviconChangedEventHandler       = Ole32Util.getGUIDFromString("{2913DA94-833D-4DE0-8DCA-900FC524A1A4}");
    public static final Guid.GUID IID_GetFaviconCompletedHandler       = Ole32Util.getGUIDFromString("{A2508329-7DA8-49D7-8C05-FA125E4AEE8D}");

    private WebView2Guids() {
    }
}
