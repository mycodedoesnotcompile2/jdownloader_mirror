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

import org.appwork.jna.windows.JNAOptions;

import com.sun.jna.Function;
import com.sun.jna.Memory;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.WString;
import com.sun.jna.platform.win32.Guid;
import com.sun.jna.platform.win32.Ole32;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;
import com.sun.jna.win32.StdCallLibrary;

/**
 * Raw COM vtable access (stdcall / Win64 ABI via JNA ALT_CONVENTION).
 * <p>
 * Indices follow the declaration order in {@code WebView2.idl} (IUnknown occupies 0..2).
 */
public final class WebView2VTable {
    public static final int IUNKNOWN_ADD_REF                           = 1;
    public static final int IUNKNOWN_RELEASE                           = 2;

    /* ICoreWebView2Environment */
    public static final int ENV_CREATE_CONTROLLER                      = 3;
    public static final int ENV_CREATE_WEB_RESOURCE_RESPONSE           = 4;
    /** {@code ICoreWebView2Environment2.CreateWebResourceRequest}. */
    public static final int ENV2_CREATE_WEB_RESOURCE_REQUEST           = 8;

    /* ICoreWebView2Controller */
    public static final int CTRL_PUT_IS_VISIBLE                        = 4;
    public static final int CTRL_PUT_BOUNDS                            = 6;
    public static final int CTRL_PUT_PARENT_WINDOW                     = 22;
    public static final int CTRL_NOTIFY_PARENT_WINDOW_POSITION_CHANGED = 23;
    public static final int CTRL_CLOSE                                 = 24;
    public static final int CTRL_GET_CORE_WEBVIEW2                     = 25;

    /* ICoreWebView2 */
    public static final int WV_GET_SETTINGS                            = 3;
    public static final int WV_GET_SOURCE                              = 4;
    public static final int WV_NAVIGATE                                = 5;
    public static final int WV_NAVIGATE_TO_STRING                      = 6;
    public static final int WV_ADD_NAVIGATION_STARTING                 = 7;
    public static final int WV_ADD_SOURCE_CHANGED                      = 11;
    public static final int WV_ADD_NAVIGATION_COMPLETED                = 15;
    /** Same handler and arguments as {@code NavigationStarting}, raised for iframes. */
    public static final int WV_ADD_FRAME_NAVIGATION_STARTING           = 17;
    public static final int WV_ADD_PERMISSION_REQUESTED                = 23;
    public static final int WV_ADD_SCRIPT_ON_DOCUMENT_CREATED          = 27;
    public static final int WV_EXECUTE_SCRIPT                          = 29;
    public static final int WV_RELOAD                                  = 31;
    public static final int WV_CALL_DEVTOOLS_PROTOCOL_METHOD           = 36;
    public static final int WV_ADD_WEB_MESSAGE_RECEIVED                = 34;
    public static final int WV_GET_BROWSER_PROCESS_ID                  = 37;
    public static final int WV_GO_BACK                                 = 40;
    public static final int WV_GO_FORWARD                              = 41;
    public static final int WV_GET_DEVTOOLS_PROTOCOL_EVENT_RECEIVER    = 42;
    public static final int WV_STOP                                    = 43;
    /* ICoreWebView2DevToolsProtocolEventReceiver / ...EventReceivedEventArgs */
    public static final int DEVTOOLS_RECEIVER_ADD_EVENT_RECEIVED       = 3;
    public static final int DEVTOOLS_EVENT_ARGS_GET_PARAMETER_JSON     = 3;
    public static final int WV_ADD_NEW_WINDOW_REQUESTED                = 44;
    public static final int WV_ADD_DOCUMENT_TITLE_CHANGED              = 46;
    public static final int WV_GET_DOCUMENT_TITLE                      = 48;
    public static final int WV_OPEN_DEV_TOOLS_WINDOW                   = 51;
    public static final int WV_ADD_WEB_RESOURCE_REQUESTED              = 55;
    public static final int WV_ADD_WEB_RESOURCE_REQUESTED_FILTER       = 57;
    public static final int WV_ADD_WINDOW_CLOSE_REQUESTED              = 59;
    /** {@code ICoreWebView2_2.NavigateWithWebResourceRequest}. The base pointer must be queried first. */
    public static final int WV2_NAVIGATE_WITH_WEB_RESOURCE_REQUEST     = 63;

    /** {@code ICoreWebView2_4.add_DownloadStarting}. The base {@code ICoreWebView2} pointer must be queried first. */
    public static final int WV4_ADD_DOWNLOAD_STARTING                  = 75;

    /* ICoreWebView2DownloadStartingEventArgs */
    public static final int DL_ARGS_GET_OPERATION                      = 3;
    public static final int DL_ARGS_PUT_CANCEL                         = 5;
    public static final int DL_ARGS_PUT_HANDLED                        = 9;

    /* ICoreWebView2DownloadOperation */
    public static final int DL_OP_GET_URI                              = 9;

    /** {@code ICoreWebView2_11.add_ContextMenuRequested}. */
    public static final int WV11_ADD_CONTEXT_MENU_REQUESTED            = 100;
    /** {@code ICoreWebView2Environment9.CreateContextMenuItem}. */
    public static final int ENV9_CREATE_CONTEXT_MENU_ITEM              = 19;
    /** {@code COREWEBVIEW2_CONTEXT_MENU_ITEM_KIND_COMMAND}. */
    public static final int MENU_KIND_COMMAND                          = 0;
    /** {@code COREWEBVIEW2_CONTEXT_MENU_ITEM_KIND_SEPARATOR}. */
    public static final int MENU_KIND_SEPARATOR                        = 3;

    /* ICoreWebView2ContextMenuRequestedEventArgs */
    public static final int CTX_GET_MENU_ITEMS                         = 3;
    /** {@code ICoreWebView2ContextMenuRequestedEventArgs.get_ContextMenuTarget}. */
    public static final int CTX_GET_CONTEXT_MENU_TARGET                = 4;
    public static final int CTX_GET_LOCATION                           = 5;
    /* ICoreWebView2ContextMenuTarget */
    public static final int CTX_TARGET_HAS_LINK_URI                    = 8;
    public static final int CTX_TARGET_GET_LINK_URI                    = 9;
    /** {@code ICoreWebView2ContextMenuTarget.get_HasSourceUri} (image / media source). */
    public static final int CTX_TARGET_HAS_SOURCE_URI                  = 12;
    /** {@code ICoreWebView2ContextMenuTarget.get_SourceUri}. */
    public static final int CTX_TARGET_GET_SOURCE_URI                  = 13;

    /* ICoreWebView2ContextMenuItemCollection */
    public static final int MENU_INSERT                                = 6;

    /* ICoreWebView2ContextMenuItem */
    public static final int MENU_ITEM_ADD_SELECTED                     = 14;

    /** {@code ICoreWebView2_15.add_FaviconChanged}. */
    public static final int WV15_ADD_FAVICON_CHANGED                   = 109;
    /** {@code ICoreWebView2_15.GetFavicon}. */
    public static final int WV15_GET_FAVICON                           = 112;
    /** {@code COREWEBVIEW2_FAVICON_IMAGE_FORMAT_PNG}. */
    public static final int FAVICON_FORMAT_PNG                         = 0;
    /** {@code ISequentialStream.Read}. */
    public static final int STREAM_READ                                = 3;
    /** {@code IStream::Seek(LARGE_INTEGER move, DWORD origin, ULARGE_INTEGER* newPosition)} */
    public static final int STREAM_SEEK                                = 5;

    /** {@code COREWEBVIEW2_WEB_RESOURCE_CONTEXT_ALL} */
    public static final int WEB_RESOURCE_CONTEXT_ALL                   = 0;
    /** {@code COREWEBVIEW2_WEB_RESOURCE_CONTEXT_DOCUMENT} */
    public static final int WEB_RESOURCE_CONTEXT_DOCUMENT              = 1;

    /* ICoreWebView2Settings (getter/setter pairs) */
    public static final int SETTINGS_PUT_IS_SCRIPT_ENABLED             = 4;
    public static final int SETTINGS_PUT_IS_WEB_MESSAGE_ENABLED        = 6;
    public static final int SETTINGS_PUT_DEFAULT_SCRIPT_DIALOGS        = 8;
    public static final int SETTINGS_PUT_IS_STATUS_BAR_ENABLED         = 10;
    public static final int SETTINGS_PUT_DEV_TOOLS_ENABLED             = 12;
    public static final int SETTINGS_PUT_DEFAULT_CONTEXT_MENUS         = 14;
    public static final int SETTINGS_PUT_HOST_OBJECTS_ALLOWED          = 16;

    /* ICoreWebView2NavigationCompletedEventArgs */
    public static final int NAV_ARGS_GET_IS_SUCCESS                    = 3;

    /* ICoreWebView2NavigationStartingEventArgs */
    /* ICoreWebView2PermissionRequestedEventArgs */
    public static final int PERMISSION_ARGS_GET_URI                    = 3;
    public static final int PERMISSION_ARGS_GET_PERMISSION_KIND        = 4;
    public static final int PERMISSION_ARGS_PUT_STATE                  = 7;
    /** {@code COREWEBVIEW2_PERMISSION_STATE_DENY} */
    public static final int PERMISSION_STATE_DENY                      = 2;
    public static final int NAV_START_ARGS_GET_URI                     = 3;
    public static final int NAV_START_ARGS_PUT_CANCEL                  = 8;

    /* ICoreWebView2WebMessageReceivedEventArgs (4 = get_WebMessageAsJson) */
    public static final int MSG_ARGS_GET_SOURCE                        = 3;
    public static final int MSG_ARGS_TRY_GET_WEB_MESSAGE_AS_STRING     = 5;

    /* ICoreWebView2NewWindowRequestedEventArgs */
    public static final int NEW_WINDOW_ARGS_GET_URI                    = 3;
    public static final int NEW_WINDOW_ARGS_PUT_HANDLED                = 6;
    public static final int NEW_WINDOW_ARGS_GET_IS_USER_INITIATED      = 8;

    /* ICoreWebView2WebResourceRequestedEventArgs */
    public static final int WR_ARGS_GET_REQUEST                        = 3;
    public static final int WR_ARGS_PUT_RESPONSE                       = 5;
    public static final int WR_ARGS_GET_RESOURCE_CONTEXT               = 7;

    /* ICoreWebView2WebResourceRequest */
    public static final int WR_REQUEST_GET_URI                         = 3;
    public static final int WR_REQUEST_GET_METHOD                      = 5;
    public static final int WR_REQUEST_GET_CONTENT                     = 7;

    private WebView2VTable() {
    }

    public static boolean succeeded(int hr) {
        return hr >= 0;
    }

    public static int invokeHR(Pointer comObject, int methodIndex, Object... args) {
        if (comObject == null) {
            throw new IllegalArgumentException("comObject is null");
        }
        final Pointer vtable = comObject.getPointer(0);
        final Pointer fnPtr = vtable.getPointer((long) methodIndex * Native.POINTER_SIZE);
        final Function fn = Function.getFunction(fnPtr, Function.ALT_CONVENTION);
        final Object[] full = new Object[args.length + 1];
        full[0] = comObject;
        System.arraycopy(args, 0, full, 1, args.length);
        return fn.invokeInt(full);
    }

    public static int addRef(Pointer comObject) {
        return invokeHR(comObject, IUNKNOWN_ADD_REF);
    }

    public static int release(Pointer comObject) {
        /* COM out-parameters are allowed to stay null (for example a request without an upload body). Cleanup paths */
        /* deliberately use SAFE_RELEASE semantics so an optional interface cannot turn into an uncaught callback error. */
        if (comObject == null) {
            return 0;
        }
        return invokeHR(comObject, IUNKNOWN_RELEASE);
    }

    /** {@code QueryInterface}. The returned pointer is AddRef'd; caller must {@link #release(Pointer)}. */
    public static Pointer queryInterface(Pointer comObject, Guid.GUID iid) {
        if (comObject == null || iid == null) {
            return null;
        }
        final PointerByReference ref = new PointerByReference();
        if (!succeeded(invokeHR(comObject, 0, iid, ref))) {
            return null;
        }
        return ref.getValue();
    }

    /** Calls a {@code [propget] HRESULT X(IUnknown** value)} and returns the (AddRef'ed) interface or {@code null}. */
    public static Pointer getInterface(Pointer comObject, int methodIndex) {
        final PointerByReference ref = new PointerByReference();
        if (!succeeded(invokeHR(comObject, methodIndex, ref))) {
            return null;
        }
        return ref.getValue();
    }

    /** Calls a {@code HRESULT X(LPWSTR* value)} method and frees the returned CoTaskMem string. */
    public static String getString(Pointer comObject, int methodIndex) {
        final PointerByReference ref = new PointerByReference();
        if (!succeeded(invokeHR(comObject, methodIndex, ref)) || ref.getValue() == null) {
            return null;
        }
        try {
            return ref.getValue().getWideString(0);
        } finally {
            Ole32.INSTANCE.CoTaskMemFree(ref.getValue());
        }
    }

    /** Calls a {@code [propget] HRESULT X(BOOL* value)}; returns {@code fallback} on failure. */
    public static boolean getBoolean(Pointer comObject, int methodIndex, boolean fallback) {
        final IntByReference ref = new IntByReference();
        if (!succeeded(invokeHR(comObject, methodIndex, ref))) {
            return fallback;
        }
        return ref.getValue() != 0;
    }

    /** Calls a {@code [propget] HRESULT X(INT* / enum* value)}; returns {@code fallback} on failure. */
    public static int getInt(Pointer comObject, int methodIndex, int fallback) {
        final IntByReference ref = new IntByReference();
        if (!succeeded(invokeHR(comObject, methodIndex, ref))) {
            return fallback;
        }
        return ref.getValue();
    }

    /**
     * Builds an {@code ICoreWebView2WebResourceResponse} whose body is {@code utf8Body}. Caller must
     * {@link #release(Pointer)} the returned response when done.
     */
    public static Pointer createWebResourceResponse(Pointer environment, byte[] utf8Body, int statusCode, String reasonPhrase, String headers) {
        if (environment == null) {
            return null;
        }
        final Pointer stream = createMemoryStream(utf8Body != null ? utf8Body : new byte[0]);
        if (stream == null) {
            return null;
        }
        try {
            final PointerByReference response = new PointerByReference();
            final int hr = invokeHR(environment, ENV_CREATE_WEB_RESOURCE_RESPONSE, stream, Integer.valueOf(statusCode), new WString(reasonPhrase != null ? reasonPhrase : "OK"), new WString(headers != null ? headers : ""), response);
            if (!succeeded(hr)) {
                return null;
            }
            return response.getValue();
        } finally {
            /* CreateWebResourceResponse AddRefs the stream; drop our SHCreateMemStream reference. */
            release(stream);
        }
    }

    /** {@code SHCreateMemStream}: returns an {@code IStream*} that owns a copy of {@code data}. */
    private static Pointer createMemoryStream(byte[] data) {
        final Memory buffer = new Memory(Math.max(1, data.length));
        if (data.length > 0) {
            buffer.write(0, data, 0, data.length);
        }
        return ShlwapiExt.INSTANCE.SHCreateMemStream(buffer, data.length);
    }

    private interface ShlwapiExt extends StdCallLibrary {
        ShlwapiExt INSTANCE = Native.load("shlwapi", ShlwapiExt.class, JNAOptions.SYSTEM_DLLS_ONLY);

        Pointer SHCreateMemStream(Pointer pInit, int cbInit);
    }

    /** Registers an event handler; the EventRegistrationToken (int64) is not needed because handlers live until Close. */
    public static int addEventHandler(Pointer comObject, int methodIndex, Pointer handler) {
        return invokeHR(comObject, methodIndex, handler, new Memory(8));
    }

    /**
     * @param enableDevTools
     *            DevTools (F12, "Inspect") available
     * @param enableContextMenus
     *            right-click menu of the page available
     */
    public static void applyDefaultSettings(Pointer settings, boolean enableDevTools, boolean enableContextMenus) {
        if (settings == null) {
            return;
        }
        invokeHR(settings, SETTINGS_PUT_IS_SCRIPT_ENABLED, Integer.valueOf(1));
        invokeHR(settings, SETTINGS_PUT_IS_WEB_MESSAGE_ENABLED, Integer.valueOf(1));
        invokeHR(settings, SETTINGS_PUT_DEFAULT_SCRIPT_DIALOGS, Integer.valueOf(1));
        /* Chromium status bar: shows the link target URL at the bottom-left on hover. */
        invokeHR(settings, SETTINGS_PUT_IS_STATUS_BAR_ENABLED, Integer.valueOf(1));
        invokeHR(settings, SETTINGS_PUT_DEV_TOOLS_ENABLED, Integer.valueOf(enableDevTools ? 1 : 0));
        invokeHR(settings, SETTINGS_PUT_DEFAULT_CONTEXT_MENUS, Integer.valueOf(enableContextMenus ? 1 : 0));
        /* This binding never exposes Java objects to pages; keep the feature switched off as well. */
        invokeHR(settings, SETTINGS_PUT_HOST_OBJECTS_ALLOWED, Integer.valueOf(0));
    }
}
