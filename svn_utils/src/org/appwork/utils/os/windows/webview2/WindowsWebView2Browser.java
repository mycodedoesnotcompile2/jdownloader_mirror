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

import java.awt.Component;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.nio.channels.FileLock;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Callable;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.loggingv3.LogV3;
import org.appwork.shutdown.ShutdownController;
import org.appwork.shutdown.ShutdownEvent;
import org.appwork.shutdown.ShutdownRequest;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Application;
import org.appwork.utils.Files;
import org.appwork.utils.IO;
import org.appwork.utils.StringUtils;
import org.appwork.utils.os.windows.webview2.com.WebView2ComHandler;
import org.appwork.utils.os.windows.webview2.com.WebView2Guids;
import org.appwork.utils.os.windows.webview2.com.WebView2VTable;

import com.sun.jna.Memory;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.WString;
import com.sun.jna.platform.win32.Guid;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.User32;
import com.sun.jna.platform.win32.WinDef.HWND;
import com.sun.jna.platform.win32.WinDef.POINT;
import com.sun.jna.platform.win32.WinDef.RECT;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;

/**
 * Microsoft Edge WebView2 controller bound to a realized heavyweight AWT {@link Component} (e.g. {@link java.awt.Canvas}). Windows only.
 * <p>
 * Most applications use {@link WindowsWebView2Panel}, which creates and sizes this browser. Use this class directly only when you own the
 * heavyweight host component yourself.
 * <p>
 * All native calls are marshalled to {@link WebView2StaThread}, so every method may be called from any thread, but should not be called
 * from the EDT because it waits for the browser. {@link Listener}, {@link RequestHandler}, {@link NavigationInterceptor} and
 * {@link ContextMenuProvider} callbacks run on that STA thread: keep them short and hand off longer work to another thread.
 */
public final class WindowsWebView2Browser {
    /**
     * Browser events. All methods are called on the WebView2 STA thread, after the native event was handled, so a listener may call
     * the browser (e.g. {@link WindowsWebView2Browser#executeScript(String)}). Keep them short: while a listener runs, the browser
     * handles no other event.
     */
    public interface Listener {
        void onNavigationCompleted(boolean success);

        void onSourceChanged(String url);

        /**
         * A script of the host posted {@code message} from the isolated world ({@code window.__awHostPost(...)} in a script run by
         * {@link WindowsWebView2Browser#executeScript(String)} and its relatives). This channel is private: the page cannot read,
         * change or forge these messages. The data a host script collected from the page is of course still untrusted.
         *
         * @param frameUrl
         *            address of the document the script ran in
         * @param topFrame
         *            {@code true} for the top document, {@code false} for an iframe
         */
        void onHostScriptMessage(String frameUrl, boolean topFrame, String message);

        /**
         * The top document posted {@code message} to the host from its own JavaScript world (page code, or host code that was run with
         * {@link WindowsWebView2Browser#executeScriptInMainWorld(String)}). {@code sourceUrl} is the address of that document as
         * reported by the browser. Everything about such a message is untrusted input: any script of the page can send it.
         */
        void onWebMessage(String sourceUrl, String message);

        /**
         * A frame inside the page posted {@code message} to the host. Frames cannot reach the host themselves; the top document passes
         * their messages on. {@code frameOrigin} (e.g. {@code https://ads.example.net}) is therefore only as trustworthy as the top
         * document {@code pageUrl}, and a frame message must never be given more trust than a message of an unknown website.
         */
        void onFrameMessage(String pageUrl, String frameOrigin, String message);

        /**
         * The browser refused to load {@code url}: forbidden by the {@link WebView2UrlFilter}, a local network address, a
         * {@code file:} address or a scheme that would start another program, see {@link Options}.
         *
         * @param document
         *            {@code true} for a page or iframe, {@code false} for a resource of a page
         */
        void onRequestBlocked(String url, boolean document);

        void onDocumentTitleChanged(String title);

        /**
         * A page asked for a new window. WebView2 opens none; the listener decides what to do with {@code url}. The address comes from
         * the page. It already passed the same checks as a navigation of the page (scheme, local network, URL filter); a refused one
         * is reported through {@link #onRequestBlocked(String, boolean)} instead. To open it, use
         * {@link WindowsWebView2Browser#navigateFromPage(String, String)}, not {@code navigate}: {@code navigate} means that the
         * application itself chose the address.
         */
        void onNewWindowRequested(String url, boolean userInitiated);

        void onWindowCloseRequested();

        /** A download was cancelled because of {@link Options#interceptDownloads}; {@code url} may be empty. */
        void onDownloadStarting(String url);

        /** Favicon of {@code pageUrl} as PNG, or {@code null} when the page has none. */
        void onFavicon(String pageUrl, byte[] png);
    }

    public static class ListenerAdapter implements Listener {
        public void onNavigationCompleted(boolean success) {
        }

        public void onSourceChanged(String url) {
        }

        public void onHostScriptMessage(String frameUrl, boolean topFrame, String message) {
        }

        public void onWebMessage(String sourceUrl, String message) {
        }

        public void onFrameMessage(String pageUrl, String frameOrigin, String message) {
        }

        public void onRequestBlocked(String url, boolean document) {
        }

        public void onDocumentTitleChanged(String title) {
        }

        public void onNewWindowRequested(String url, boolean userInitiated) {
        }

        public void onWindowCloseRequested() {
        }

        public void onDownloadStarting(String url) {
        }

        public void onFavicon(String pageUrl, byte[] png) {
        }
    }

    /**
     * Sees every top-level document navigation before it is committed. Called on the WebView2 STA thread inside the native event:
     * answer quickly and do not run scripts or DevTools calls from here.
     */
    public interface NavigationInterceptor {
        /** @return {@code true} to cancel the navigation; the currently visible document stays intact */
        boolean intercept(String url);
    }

    /**
     * Sees every request of the page (documents and resources, main frame and iframes) that passed the security checks and the
     * {@link WebView2UrlFilter}, and may answer it itself. Called on the WebView2 STA thread inside the native event: answer quickly
     * and do not run scripts or DevTools calls from here.
     */
    public interface RequestHandler {
        /**
         * @param request
         *            only valid until this method returns
         * @return the response to serve instead of loading the request, or {@code null} to let the request continue
         */
        Response handle(Request request);
    }

    /** A request of the page as seen by a {@link RequestHandler}. Only valid during {@link RequestHandler#handle(Request)}. */
    public final class Request {
        private final Pointer nativeRequest;
        private final String  url;
        private final boolean document;
        private boolean       closed;
        private boolean       pageUrlRead;
        private String        pageUrl;

        private Request(Pointer nativeRequest, String url, boolean document) {
            this.nativeRequest = nativeRequest;
            this.url = url;
            this.document = document;
        }

        public String getUrl() {
            return url;
        }

        /** {@code true} when this is a document navigation (main frame or iframe), {@code false} for a resource. */
        public boolean isDocument() {
            return document;
        }

        /** URL of the top-level document that sent the request, {@code null} if unknown. */
        public String getPageUrl() {
            if (!pageUrlRead) {
                pageUrlRead = true;
                final Pointer current = webview;
                pageUrl = current != null ? WebView2VTable.getString(current, WebView2VTable.WV_GET_SOURCE) : null;
            }
            return pageUrl;
        }

        public String getMethod() {
            ensureOpen();
            return WebView2VTable.getString(nativeRequest, WebView2VTable.WR_REQUEST_GET_METHOD);
        }

        /**
         * Reads at most {@code limit} bytes of the request body.
         *
         * @return the body, or {@code null} when the request has none
         */
        public byte[] readBody(int limit) {
            ensureOpen();
            final Pointer content = WebView2VTable.getInterface(nativeRequest, WebView2VTable.WR_REQUEST_GET_CONTENT);
            if (content == null) {
                return null;
            }
            try {
                return readStream(content, limit);
            } finally {
                /*
                 * The stream belongs to the request. Put it back to its start, or a request that the handler lets continue would be
                 * sent with what is left of the body, i.e. nothing.
                 */
                final int seek = WebView2VTable.invokeHR(content, WebView2VTable.STREAM_SEEK, Long.valueOf(0L), Integer.valueOf(0), Pointer.NULL);
                if (!WebView2VTable.succeeded(seek)) {
                    LogV3.warning("WebView2: could not rewind the request body: 0x" + Integer.toHexString(seek));
                }
                WebView2VTable.release(content);
            }
        }

        private void ensureOpen() {
            if (closed) {
                throw new IllegalStateException("The request is only valid during RequestHandler.handle");
            }
        }
    }

    /** Answer of a {@link RequestHandler}. */
    public static final class Response {
        private final int    status;
        private final String reason;
        private final String headers;
        private final byte[] body;

        /**
         * @param headers
         *            raw header lines, each terminated by {@code \r\n}, e.g. {@code "Content-Type: text/html; charset=utf-8\r\n"}
         */
        public Response(int status, String reason, String headers, byte[] body) {
            this.status = status;
            this.reason = reason != null ? reason : "";
            this.headers = headers != null ? headers : "";
            this.body = body != null ? body : new byte[0];
        }

        public int getStatus() {
            return status;
        }

        public String getReason() {
            return reason;
        }

        public String getHeaders() {
            return headers;
        }

        public byte[] getBody() {
            return body;
        }
    }

    /**
     * Adds own entries to the top of the context menu of the page. Called on the WebView2 STA thread inside the native event: answer
     * quickly and do not run scripts or DevTools calls from here.
     */
    public interface ContextMenuProvider {
        /** @return the entries for this click, {@code null} or empty for none */
        List<ContextMenuItem> getItems(ContextMenuTarget target);
    }

    /** What the user right-clicked. */
    public static final class ContextMenuTarget {
        private final String  linkUrl;
        private final String  sourceUrl;
        private final boolean location;
        private final int     x;
        private final int     y;

        private ContextMenuTarget(String linkUrl, String sourceUrl, boolean location, int x, int y) {
            this.linkUrl = linkUrl;
            this.sourceUrl = sourceUrl;
            this.location = location;
            this.x = x;
            this.y = y;
        }

        /** Address of the clicked http(s) link, otherwise {@code null}. */
        public String getLinkUrl() {
            return linkUrl;
        }

        /** Address of the clicked http(s) image or media element, otherwise {@code null}. */
        public String getSourceUrl() {
            return sourceUrl;
        }

        /** {@code false} when WebView2 did not report where the menu was opened; {@link #getX()} and {@link #getY()} are 0 then. */
        public boolean hasLocation() {
            return location;
        }

        /** CSS pixels relative to the top-left corner of the view. */
        public int getX() {
            return x;
        }

        public int getY() {
            return y;
        }
    }

    public static final class ContextMenuItem {
        private final String   label;
        private final Runnable action;

        /**
         * @param action
         *            runs on the WebView2 STA thread after the user chose the entry; it may call the browser
         */
        public ContextMenuItem(String label, Runnable action) {
            this.label = label;
            this.action = action;
        }

        public String getLabel() {
            return label;
        }

        public Runnable getAction() {
            return action;
        }
    }

    /**
     * Settings of a browser. The defaults are the safe choice for showing websites that are not trusted: a website gets no access to
     * this PC. Its downloads are cancelled, it gets no permissions, it cannot start other programs, cannot open local files and cannot
     * open windows. Every {@code allow...} switch gives a part of that away.
     * <p>
     * The local network is protected by {@link #blockLocalNetwork} together with {@link #filterProxy}; without the filter proxy that
     * protection has gaps, see there.
     */
    public static class Options {
        /** Optional absolute path to WebView2Loader.dll; empty = installed Evergreen runtime. */
        public String           loaderPath;
        /**
         * Whitelist/blacklist of the addresses the browser may load, {@code null} for no restriction. See
         * {@link WindowsWebView2Browser#setUrlFilter(WebView2UrlFilter)}.
         */
        public WebView2UrlFilter urlFilter;
        /**
         * {@code false} (default): every permission a page asks for is denied without a prompt: camera, microphone, location,
         * notifications, reading the clipboard, and all others. {@code true}: WebView2 asks the user as a browser does.
         */
        public boolean          allowPermissionRequests = false;
        /**
         * {@code false} (default): a page cannot open addresses that are handed to Windows and may start another program
         * ({@code mailto:}, {@code ms-settings:}, {@code steam:}, any registered protocol). Only {@code http}, {@code https},
         * {@code about}, {@code data} and {@code blob} are loaded. {@code true}: WebView2 asks the user as a browser does.
         */
        public boolean          allowExternalSchemes    = false;
        /** {@code false} (default): {@code file:} addresses are not loaded, so no page is read from the local disk. */
        public boolean          allowFileUrls           = false;
        /**
         * {@code true} (default): http(s) requests of a website to this PC or the local network are refused ({@code localhost},
         * loopback, private and link-local addresses, {@code *.local}, names without a dot). This also holds for the application itself:
         * {@link WindowsWebView2Browser#navigate(String)} to a local address shows the blocked page. Nothing is allowed
         * automatically; name the local hosts the browser may reach in {@link #allowedLocalHosts}.
         * <p>
         * With {@link #filterProxy} (default) this covers every connection, also WebSockets and workers, and it is decided by the
         * address a name really resolves to. Without the filter proxy it is a partial protection only: a WebSocket to a local address
         * still connects, and so does any request to a public name that resolves to a local address (measured against runtime 157;
         * WebView2 offers no event for either).
         */
        public boolean          blockLocalNetwork       = true;
        /**
         * Hosts on this PC or in the local network that {@link #blockLocalNetwork} does not block, exactly as they are written in
         * the address ({@code "localhost"}, {@code "192.168.1.1"}, {@code "nas.local"}); every port. {@code null} (default): none.
         * <p>
         * Risk: the exception is for the whole browser. Every page this browser shows can send requests to these hosts, because
         * neither WebView2 nor the filter proxy can tell which page or frame a connection comes from. So do not show foreign
         * websites in a browser that has such an exception, or limit it with a whitelist in {@link #urlFilter}. The
         * {@link #urlFilter} still applies to these hosts.
         */
        public String[]         allowedLocalHosts;

        /**
         * Asked for every iframe of another host inside a local page, i.e. a page of a host from {@link #allowedLocalHosts} (only
         * with {@link #blockLocalNetwork}). {@code true} (default): the iframe is not loaded.
         * <p>
         * Reason: a foreign website inside an iframe of the local page could send requests to the local host, see
         * {@link #allowedLocalHosts}. Override and return {@code false} only for local pages whose iframes you trust; such an
         * iframe can then reach the allowed local hosts (not the rest of the local network).
         *
         * @param pageUrl
         *            address of the local page
         * @param iframeUrl
         *            http(s) address the iframe is about to load; its host differs from the host of the page
         */
        public boolean isIframeBlockedOnLocalWebsite(String pageUrl, String iframeUrl) {
            return true;
        }

        /**
         * {@code true} (default): everything the browser sends goes through a filter inside this process ({@link WebView2FilterProxy}),
         * also WebSockets, workers and connections to loopback addresses. Only with it {@link #blockLocalNetwork} and
         * {@link #urlFilter} are complete, and only with it a {@link #proxySelector} is used. {@code false}: the browser connects by
         * itself with the proxy settings of Windows, and the two protections only cover what WebView2 reports as a request.
         */
        public boolean          filterProxy             = true;
        /**
         * Chooses the proxy for every connection of the browser, {@code null} for direct connections. The proxy settings of Windows
         * are not used. Needs {@link #filterProxy}. Browsers that share a persistent profile use the selector of the first of them.
         */
        public WebView2ProxySelector proxySelector;
        /**
         * Folder for the browser profile (cookies, cache, logins, history). Empty = {@code cfg/webview2} in the application folder for a
         * persistent profile, the temp folder of the application for a non-persistent one. Browsers that share a persistent folder
         * share one profile.
         */
        public String   userDataFolder;
        /**
         * {@code false} (default): every browser gets a fresh, empty profile in a {@code session-...} subfolder of
         * {@link #userDataFolder}, which is deleted after the browser was closed. Only that subfolder is deleted, never
         * {@link #userDataFolder} itself. A browser that is still open when the application exits is closed by a
         * {@link ShutdownController} hook, which deletes the profile too; what is left after a crash is deleted the next time a
         * non-persistent browser is created in the same folder. {@code true}: the profile is kept, so cookies and logins survive a
         * restart.
         */
        public boolean  persistentProfile  = false;
        /** Debug mode: {@code true} enables the DevTools of the page (F12, "Inspect", {@link WindowsWebView2Browser#openDevTools()}). */
        public boolean  debugMode          = false;
        /**
         * {@code true} enables the right-click menu of the page. A {@link ContextMenuProvider} can only add entries while the menu is
         * enabled.
         */
        public boolean  contextMenu        = false;
        /** Language tag such as {@code en-US}, or empty to keep the language of the PC. */
        public String   language;
        /**
         * Script that is put in front of every host script and lets it post messages to the host. {@code null} = the bundled bridge
         * ({@link WindowsWebView2Browser#getDefaultBridgeScript()}), which defines {@code window.__awHostPost(stringOrObject)}. Empty =
         * no bridge. Messages of host scripts in the isolated world arrive as {@link Listener#onHostScriptMessage}, messages from
         * the page as {@link Listener#onWebMessage} or {@link Listener#onFrameMessage}.
         * <p>
         * Do not replace the bundled bridge with a different implementation; extend it (see {@code getDefaultBridgeScript}). It
         * keeps two security rules: messages of the isolated world never pass through the page, and a frame can never speak as the
         * top document.
         */
        public String   bridgeScript;
        /**
         * Name of the isolated JavaScript world that {@link WindowsWebView2Browser#executeScript(String)} runs in. Empty =
         * {@link WebView2DevTools#DEFAULT_WORLD}.
         */
        public String   isolatedWorldName;
        /**
         * {@code true}: the browser never saves a file itself. Every download is cancelled and reported through
         * {@link Listener#onDownloadStarting(String)}.
         */
        public boolean  interceptDownloads = true;
        /**
         * Runs on the WebView2 STA thread after the native browser was closed, also when its creation failed. Use it to clean up the
         * profile folder.
         */
        public Runnable closedCallback;
    }

    private interface Event {
        void fire(Listener listener);
    }

    private enum Target {
        CONTROLLER,
        WEBVIEW
    }

    /** RECT passed by value as required by {@code put_Bounds(RECT)}; JNA applies the x86/x64 ABI rules. */
    public static class RectByValue extends RECT implements Structure.ByValue {
    }

    private static final long                              SCRIPT_TIMEOUT_MS      = TimeUnit.SECONDS.toMillis(30);
    private static final long                              CREATE_TIMEOUT_MS      = TimeUnit.SECONDS.toMillis(60);
    /** Function that exists only in the isolated world and carries messages of host scripts to the host. */
    private static final String                            HOST_BINDING           = "__awWebView2Binding";
    private static final String                            BROWSER_ARGUMENTS_VARIABLE = "WEBVIEW2_ADDITIONAL_BROWSER_ARGUMENTS";
    private static final String                            SELF_TEST_PREFIX       = "http://" + WebView2FilterProxy.SELF_TEST_HOST + "/";
    private static final long                              SELF_TEST_TIMEOUT_MS   = TimeUnit.SECONDS.toMillis(15);
    private static final String                            SESSION_PREFIX         = "session-";
    /** Key and start of the envelope in which the bridge passes on the message of a frame, see {@link Options#bridgeScript}. */
    private static final String                            FRAME_MESSAGE_KEY      = "__awWebView2Frame";
    private static final String                            FRAME_MESSAGE_PREFIX   = "{\"" + FRAME_MESSAGE_KEY + "\":";
    private static final String                            SESSION_LOCK           = "session.lock";
    private static final String                            HTML_RESPONSE_HEADERS  = "Content-Type: text/html; charset=utf-8\r\nCache-Control: no-store\r\n";
    private static final AtomicReference<String>           DEFAULT_BRIDGE         = new AtomicReference<String>();
    private static volatile boolean                        available;
    private static volatile String                         originalArguments;
    private static boolean                                 originalArgumentsRead;
    private final WebView2StaThread                        sta                    = WebView2StaThread.getInstance();
    private final CopyOnWriteArrayList<Listener>           listeners              = new CopyOnWriteArrayList<Listener>();
    private final CopyOnWriteArrayList<WebView2ComHandler> handlers               = new CopyOnWriteArrayList<WebView2ComHandler>();
    private final String                                   bridgeScript;
    private final WebView2DevTools                         devTools;
    private final Runnable                                 closedCallback;
    private final boolean                                  allowPermissionRequests;
    private final boolean                                  allowExternalSchemes;
    private final boolean                                  allowFileUrls;
    private final boolean                                  blockLocalNetwork;
    /** Asked through {@link Options#isIframeBlockedOnLocalWebsite(String, String)}. */
    private final Options                                  iframePolicy;
    private volatile WebView2UrlFilter                     urlFilter;
    private final WebView2ProxySelector                    proxySelector;
    /** The filter all traffic of this browser goes through, {@code null} when {@link Options#filterProxy} is off. */
    private volatile WebView2FilterProxy                   filterProxy;
    private boolean                                        ownsFilterProxy;
    private final WebView2FilterProxy.Policy               proxyPolicy            = new WebView2FilterProxy.Policy() {
                                                                                      public boolean isBlockLocalNetwork() {
                                                                                          return blockLocalNetwork;
                                                                                      }

                                                                                      public boolean isLocalTargetAllowed(String host) {
                                                                                          return host != null && allowedLocalHosts.contains(host);
                                                                                      }

                                                                                      public WebView2UrlFilter getUrlFilter() {
                                                                                          return urlFilter;
                                                                                      }

                                                                                      public WebView2ProxySelector getProxySelector() {
                                                                                          return proxySelector;
                                                                                      }

                                                                                      public void onBlocked(String url) {
                                                                                          fireBlocked(url, false);
                                                                                      }
                                                                                  };
    /** True when host scripts in the isolated world can post to the host through the private binding. */
    private volatile boolean                               hostBinding;
    /** {@link Options#allowedLocalHosts} in the spelling of {@link WebView2UrlFilter#hostOf(String)}. Fixed at creation. */
    private final Set<String>                              allowedLocalHosts;
    /** Counts navigations requested through this object, see {@link #getNavigationRequestCount()}. */
    private final AtomicInteger                            navigationRequests     = new AtomicInteger();
    /** True when host scripts run in an isolated world. Decided once at creation; a later failure never falls back to the page. */
    private volatile boolean                               isolatedWorld;
    /**
     * Greater than 0 while a host callback runs inside a native WebView2 event. Calls that wait for another native callback cannot
     * complete then. Only touched on the STA thread.
     */
    private int                                            nativeCallbackDepth;
    /** Profile folder to delete after the browser was closed, {@code null} for a persistent profile. */
    private volatile File                                  temporaryProfile;
    /** Held while the temporary profile is in use, so another process does not mistake it for a leftover. */
    private volatile RandomAccessFile                      temporaryProfileLock;
    /** Closes the browser and deletes the temporary profile when the application exits while the browser is still open. */
    private volatile ShutdownEvent                         shutdownCleanup;
    private Pointer                                        environment;
    private Pointer                                        controller;
    private Pointer                                        webview;
    private volatile boolean                               disposed;
    /** True once {@code FaviconChanged} could be subscribed. */
    private volatile boolean                               nativeFavicon;
    private volatile RequestHandler                        requestHandler;
    private volatile NavigationInterceptor                 navigationInterceptor;
    private volatile ContextMenuProvider                   contextMenuProvider;
    /**
     * One-shot document body served via {@link #loadHtmlWithBaseUrl} (Android {@code loadDataWithBaseURL} equivalent). Cleared after the
     * first matching DOCUMENT response.
     */
    private volatile String                                documentOverrideHtml;
    /** Origin prefix that the override applies to, e.g. {@code https://host/}. */
    private volatile String                                documentOverrideOrigin;
    /** Absolute path of the WebView2 user-data folder used for this instance. */
    private volatile String                                userDataFolder;

    private WindowsWebView2Browser(Options options) {
        this.bridgeScript = options.bridgeScript != null ? options.bridgeScript : getDefaultBridgeScript();
        this.closedCallback = options.closedCallback;
        this.allowPermissionRequests = options.allowPermissionRequests;
        this.allowExternalSchemes = options.allowExternalSchemes;
        this.allowFileUrls = options.allowFileUrls;
        this.blockLocalNetwork = options.blockLocalNetwork;
        this.iframePolicy = options;
        final Set<String> localHosts = new HashSet<String>();
        if (options.allowedLocalHosts != null) {
            for (String host : options.allowedLocalHosts) {
                /* The same normalisation as for the host of a request. */
                final String normalized = host != null ? WebView2UrlFilter.hostOf("http://" + host.trim() + "/") : null;
                if (normalized != null) {
                    localHosts.add(normalized);
                }
            }
        }
        this.allowedLocalHosts = Collections.unmodifiableSet(localHosts);
        this.urlFilter = options.urlFilter;
        this.proxySelector = options.proxySelector;
        this.devTools = new WebView2DevTools(new WebView2DevTools.Transport() {
            public Map<String, Object> call(String method, Map<String, Object> parameters) throws WebView2Exception {
                return callDevToolsOnSta(method, parameters);
            }
        }, bridgeScript, options.isolatedWorldName);
    }

    /** Cheap check (no DLL loading) whether WebView2 can be used on this PC: Windows with an installed Edge WebView2 Runtime. */
    public static boolean isAvailable() {
        if (available) {
            return true;
        }
        /* Reads the registry and the disk. A runtime that was found stays; one that is missing may be installed later. */
        final boolean found = WebView2Runtime.isAvailable(null);
        available = found;
        return found;
    }

    /**
     * Creates the browser inside {@code host} and waits until it is ready.
     *
     * @param host
     *            displayable heavyweight component; the browser fills it
     * @param options
     *            {@code null} for the defaults
     */
    public static WindowsWebView2Browser create(final Component host, final Options options) throws WebView2Exception {
        if (host == null || !host.isDisplayable()) {
            throw new WebView2Exception("Host component must be displayable");
        }
        final Pointer peer = Native.getComponentPointer(host);
        if (peer == null) {
            throw new WebView2Exception("No native window handle (a heavyweight component such as Canvas is required)");
        }
        final Options opts = options != null ? options : new Options();
        final WindowsWebView2Browser browser = new WindowsWebView2Browser(opts);
        browser.init(new HWND(peer), host.getWidth(), host.getHeight(), opts);
        return browser;
    }

    /**
     * Creates a browser without any window and waits until it is ready. Nothing is ever shown, no component is needed and no AWT/Swing
     * is involved; pages load, run scripts and fire events as usual. The page gets a viewport of 1280 x 800 pixels.
     *
     * <pre>
     * WindowsWebView2Browser browser = WindowsWebView2Browser.createInvisible(null);
     * try {
     *     browser.navigate("https://example.com/");
     *     ...
     *     String title = browser.executeScript("document.title");
     * } finally {
     *     browser.dispose();
     * }
     * </pre>
     *
     * Do not call this on the EDT; it waits for the browser. The caller must {@link #dispose()} the browser.
     *
     * @param options
     *            {@code null} for the defaults
     */
    public static WindowsWebView2Browser createInvisible(final Options options) throws WebView2Exception {
        return createInvisible(options, 1280, 800);
    }

    /**
     * Like {@link #createInvisible(Options)} with a viewport of {@code width} x {@code height} pixels.
     */
    public static WindowsWebView2Browser createInvisible(final Options options, int width, int height) throws WebView2Exception {
        final Options opts = options != null ? options : new Options();
        final WindowsWebView2Browser browser = new WindowsWebView2Browser(opts);
        /* HWND_MESSAGE: WebView2 supports a message-only parent for a browser that never becomes visible. */
        browser.init(new HWND(Pointer.createConstant(-3L)), Math.max(1, width), Math.max(1, height), opts);
        return browser;
    }

    /**
     * The bundled bridge script, see {@link Options#bridgeScript}. An application that wants the function under another name passes
     * this script plus an alias as its bridge, e.g. {@code getDefaultBridgeScript() + "\nwindow.myPost = window.__awHostPost;"}.
     */
    public static String getDefaultBridgeScript() {
        String script = DEFAULT_BRIDGE.get();
        if (script == null) {
            final URL url = WindowsWebView2Browser.class.getResource("js/webview2-bridge.js");
            if (url == null) {
                throw new IllegalStateException("Missing resource js/webview2-bridge.js next to " + WindowsWebView2Browser.class.getName());
            }
            try {
                script = new String(IO.readURL(url), "UTF-8");
            } catch (Exception e) {
                throw new IllegalStateException("Failed to read resource " + url, e);
            }
            DEFAULT_BRIDGE.set(script);
        }
        return script;
    }

    private void init(final HWND hwnd, final int width, final int height, final Options opts) throws WebView2Exception {
        final WebView2Runtime runtime = WebView2Runtime.open(opts.loaderPath);
        final String userData = resolveUserDataFolder(opts);
        this.userDataFolder = userData;
        if (opts.filterProxy) {
            try {
                filterProxy = WebView2FilterProxy.forProfile(userData, opts.persistentProfile);
                ownsFilterProxy = !opts.persistentProfile;
                filterProxy.addPolicy(proxyPolicy);
            } catch (IOException e) {
                releaseTemporaryProfile();
                /* Never start the browser without the filter it was configured with. */
                throw new WebView2Exception("Could not start the filter proxy", e);
            }
        }
        try {
            sta.call(new Callable<Void>() {
                public Void call() throws WebView2Exception {
                    createOnSta(runtime, hwnd, width, height, userData, opts);
                    return null;
                }
            });
        } catch (WebView2Exception e) {
            /* Usually done by disposeOnSta already; this covers a failure before the STA thread ran anything. */
            releaseFilterProxy();
            releaseTemporaryProfile();
            throw e;
        }
    }

    private void createOnSta(WebView2Runtime runtime, final HWND hwnd, final int width, final int height, String userData, final Options opts) throws WebView2Exception {
        final String previousArguments = applyBrowserArguments();
        try {
            createWithArgumentsOnSta(runtime, hwnd, width, height, userData, opts);
        } finally {
            Kernel32.INSTANCE.SetEnvironmentVariable(BROWSER_ARGUMENTS_VARIABLE, previousArguments);
        }
    }

    private void createWithArgumentsOnSta(WebView2Runtime runtime, final HWND hwnd, final int width, final int height, String userData, final Options opts) throws WebView2Exception {
        final AtomicBoolean ready = new AtomicBoolean(false);
        /* Set when creation was given up while a completion callback was still outstanding. */
        final AtomicBoolean abandoned = new AtomicBoolean(false);
        final AtomicReference<String> failure = new AtomicReference<String>();
        final WebView2ComHandler controllerHandler = register(WebView2ComHandler.createHR(WebView2Guids.IID_ControllerCompletedHandler, new WebView2ComHandler.InvokeHRObject() {
            public int invoke(Pointer thisPtr, int hresult, Pointer result) {
                if (abandoned.get()) {
                    /* Too late: nobody owns this browser any more. Close it instead of leaving it running. */
                    if (WebView2VTable.succeeded(hresult) && result != null) {
                        WebView2VTable.invokeHR(result, WebView2VTable.CTRL_CLOSE);
                    }
                    return 0;
                }
                try {
                    if (!WebView2VTable.succeeded(hresult) || result == null) {
                        failure.set("CreateCoreWebView2Controller failed HRESULT=0x" + Integer.toHexString(hresult));
                        return 0;
                    }
                    WebView2VTable.addRef(result);
                    controller = result;
                    webview = WebView2VTable.getInterface(controller, WebView2VTable.CTRL_GET_CORE_WEBVIEW2);
                    if (webview == null) {
                        failure.set("get_CoreWebView2 failed");
                        return 0;
                    }
                    final WebView2FilterProxy proxy = filterProxy;
                    if (proxy != null) {
                        /* From now on the filter accepts connections of this browser process and its children. */
                        proxy.allowProcess(WebView2VTable.getInt(webview, WebView2VTable.WV_GET_BROWSER_PROCESS_ID, 0));
                    }
                    final Pointer settings = WebView2VTable.getInterface(webview, WebView2VTable.WV_GET_SETTINGS);
                    if (settings != null) {
                        WebView2VTable.applyDefaultSettings(settings, opts.debugMode, opts.contextMenu);
                        WebView2VTable.release(settings);
                    }
                    wireEvents(opts);
                    WebView2VTable.invokeHR(controller, WebView2VTable.CTRL_PUT_BOUNDS, rect(0, 0, width, height));
                    WebView2VTable.invokeHR(controller, WebView2VTable.CTRL_PUT_IS_VISIBLE, Integer.valueOf(1));
                } catch (Throwable t) {
                    LogV3.log(t);
                    failure.set(String.valueOf(t.getMessage()));
                } finally {
                    ready.set(true);
                }
                return 0;
            }
        }));
        final WebView2ComHandler envHandler = register(WebView2ComHandler.createHR(WebView2Guids.IID_EnvironmentCompletedHandler, new WebView2ComHandler.InvokeHRObject() {
            public int invoke(Pointer thisPtr, int hresult, Pointer result) {
                if (abandoned.get()) {
                    return 0;
                }
                try {
                    if (!WebView2VTable.succeeded(hresult) || result == null) {
                        failure.set("CreateCoreWebView2Environment failed HRESULT=0x" + Integer.toHexString(hresult));
                        ready.set(true);
                        return 0;
                    }
                    WebView2VTable.addRef(result);
                    environment = result;
                    final int hr = WebView2VTable.invokeHR(environment, WebView2VTable.ENV_CREATE_CONTROLLER, hwnd, controllerHandler.getPointer());
                    if (WebView2VTable.succeeded(hr)) {
                        controllerHandler.detach();
                    } else {
                        controllerHandler.releaseKeepAlive();
                    }
                    if (!WebView2VTable.succeeded(hr)) {
                        failure.set("CreateCoreWebView2Controller invoke failed HRESULT=0x" + Integer.toHexString(hr));
                        ready.set(true);
                    }
                } catch (Throwable t) {
                    LogV3.log(t);
                    failure.set(String.valueOf(t.getMessage()));
                    ready.set(true);
                }
                return 0;
            }
        }));
        final int hr = createEnvironment(runtime, userData, envHandler);
        if (!WebView2VTable.succeeded(hr)) {
            envHandler.releaseKeepAlive();
            controllerHandler.releaseKeepAlive();
            disposeOnSta();
            throw new WebView2Exception("CreateCoreWebView2Environment invoke failed", hr);
        }
        try {
            sta.pumpUntil(ready, CREATE_TIMEOUT_MS);
        } catch (WebView2Exception e) {
            /*
             * Timeout or interrupt: WebView2 may still call the completion handlers later. They stay alive for that call because
             * WebView2 holds a COM reference to them, and are freed when it drops that reference. A browser that still arrives is
             * closed by the handler.
             */
            abandoned.set(true);
            disposeOnSta();
            throw e;
        }
        if (failure.get() != null || controller == null || webview == null) {
            disposeOnSta();
            throw new WebView2Exception(failure.get() != null ? failure.get() : "WebView2 controller was not created");
        }
        /* Async DevTools callbacks are not dispatched while the controller-completed callback itself is still open. */
        try {
            verifyFilterProxy();
        } catch (WebView2Exception e) {
            disposeOnSta();
            throw e;
        }
        installBridge();
        applyPageLanguage(opts.language);
    }

    /** Tells the page and its requests to use {@code language}. Empty keeps the language of the PC. */
    private void applyPageLanguage(String language) {
        if (StringUtils.isEmpty(language)) {
            return;
        }
        try {
            callDevToolsOnSta("Emulation.setLocaleOverride", WebView2DevTools.mapOf("locale", language));
        } catch (WebView2Exception e) {
            LogV3.info("WebView2: Emulation.setLocaleOverride unavailable: " + e.getMessage());
        }
        try {
            final Map<String, Object> headers = new HashMap<String, Object>();
            headers.put("Accept-Language", acceptLanguageHeader(language));
            callDevToolsOnSta("Network.setExtraHTTPHeaders", WebView2DevTools.mapOf("headers", headers));
        } catch (WebView2Exception e) {
            LogV3.info("WebView2: Accept-Language override unavailable: " + e.getMessage());
        }
    }

    /** A regional tag also offers the base language, so {@code en-US} becomes {@code en-US,en;q=0.9}. */
    private static String acceptLanguageHeader(String language) {
        final int dash = language.indexOf('-');
        if (dash < 2 || dash > 8) {
            return language;
        }
        return language + "," + language.substring(0, dash) + ";q=0.9";
    }

    private void wireEvents(Options opts) {
        final int navigationStarting = addEventHandler(WebView2VTable.WV_ADD_NAVIGATION_STARTING, WebView2Guids.IID_NavigationStartingEventHandler, new WebView2ComHandler.Invoke2() {
            public int invoke(Pointer thisPtr, Pointer sender, Pointer args) {
                devTools.documentChanged();
                if (args == null) {
                    return 0;
                }
                final String uri = WebView2VTable.getString(args, WebView2VTable.NAV_START_ARGS_GET_URI);
                if (uri == null) {
                    return 0;
                }
                if (!isSchemeAllowed(uri)) {
                    WebView2VTable.invokeHR(args, WebView2VTable.NAV_START_ARGS_PUT_CANCEL, Integer.valueOf(1));
                    fireBlocked(uri, true);
                    return 0;
                }
                final NavigationInterceptor interceptor = navigationInterceptor;
                if (interceptor == null) {
                    return 0;
                }
                final boolean cancel;
                nativeCallbackDepth++;
                try {
                    cancel = interceptor.intercept(uri);
                } finally {
                    nativeCallbackDepth--;
                }
                if (cancel) {
                    /* Cancellation happens before commit, so the currently visible document remains intact. */
                    WebView2VTable.invokeHR(args, WebView2VTable.NAV_START_ARGS_PUT_CANCEL, Integer.valueOf(1));
                }
                return 0;
            }
        });
        if (!WebView2VTable.succeeded(navigationStarting)) {
            throw new IllegalStateException("WebView2 NavigationStarting registration failed: 0x" + Integer.toHexString(navigationStarting));
        }
        final int frameNavigationStarting = addEventHandler(WebView2VTable.WV_ADD_FRAME_NAVIGATION_STARTING, WebView2Guids.IID_NavigationStartingEventHandler, new WebView2ComHandler.Invoke2() {
            public int invoke(Pointer thisPtr, Pointer sender, Pointer args) {
                /* An iframe must not open file: or external program addresses either. */
                final String uri = args != null ? WebView2VTable.getString(args, WebView2VTable.NAV_START_ARGS_GET_URI) : null;
                if (uri != null && (!isSchemeAllowed(uri) || isForbiddenIframeOfLocalPage(uri))) {
                    WebView2VTable.invokeHR(args, WebView2VTable.NAV_START_ARGS_PUT_CANCEL, Integer.valueOf(1));
                    fireBlocked(uri, true);
                }
                return 0;
            }
        });
        if (!WebView2VTable.succeeded(frameNavigationStarting)) {
            throw new IllegalStateException("WebView2 FrameNavigationStarting registration failed: 0x" + Integer.toHexString(frameNavigationStarting));
        }
        if (!allowPermissionRequests) {
            final int permissionRequested = addEventHandler(WebView2VTable.WV_ADD_PERMISSION_REQUESTED, WebView2Guids.IID_PermissionRequestedEventHandler, new WebView2ComHandler.Invoke2() {
                public int invoke(Pointer thisPtr, Pointer sender, Pointer args) {
                    if (args != null) {
                        WebView2VTable.invokeHR(args, WebView2VTable.PERMISSION_ARGS_PUT_STATE, Integer.valueOf(WebView2VTable.PERMISSION_STATE_DENY));
                        LogV3.info("WebView2: denied permission " + WebView2VTable.getInt(args, WebView2VTable.PERMISSION_ARGS_GET_PERMISSION_KIND, -1) + " for " + WebView2VTable.getString(args, WebView2VTable.PERMISSION_ARGS_GET_URI));
                    }
                    return 0;
                }
            });
            if (!WebView2VTable.succeeded(permissionRequested)) {
                /* Without this handler WebView2 would ask the user; do not run a browser that is less safe than configured. */
                throw new IllegalStateException("WebView2 PermissionRequested registration failed: 0x" + Integer.toHexString(permissionRequested));
            }
        }
        addEventHandler(WebView2VTable.WV_ADD_WEB_MESSAGE_RECEIVED, WebView2Guids.IID_WebMessageReceivedEventHandler, new WebView2ComHandler.Invoke2() {
            public int invoke(Pointer thisPtr, Pointer sender, Pointer args) {
                final String msg = WebView2VTable.getString(args, WebView2VTable.MSG_ARGS_TRY_GET_WEB_MESSAGE_AS_STRING);
                if (msg != null) {
                    final String sourceUrl = WebView2VTable.getString(args, WebView2VTable.MSG_ARGS_GET_SOURCE);
                    fireMessage(sourceUrl, msg);
                }
                return 0;
            }
        });
        addEventHandler(WebView2VTable.WV_ADD_NAVIGATION_COMPLETED, WebView2Guids.IID_NavigationCompletedEventHandler, new WebView2ComHandler.Invoke2() {
            public int invoke(Pointer thisPtr, Pointer sender, Pointer args) {
                final boolean success = WebView2VTable.getBoolean(args, WebView2VTable.NAV_ARGS_GET_IS_SUCCESS, true);
                if (hostBinding) {
                    /* After this native event: give the new document its private channel and deliver what its scripts queued. */
                    sta.post(new Runnable() {
                        public void run() {
                            if (disposed || webview == null) {
                                return;
                            }
                            try {
                                devTools.prepareRootWorld();
                            } catch (WebView2Exception e) {
                                LogV3.info("WebView2: no isolated world for the new document: " + e.getMessage());
                            }
                        }
                    });
                }
                fire(new Event() {
                    public void fire(Listener l) {
                        l.onNavigationCompleted(success);
                    }
                });
                return 0;
            }
        });
        addEventHandler(WebView2VTable.WV_ADD_SOURCE_CHANGED, WebView2Guids.IID_SourceChangedEventHandler, new WebView2ComHandler.Invoke2() {
            public int invoke(Pointer thisPtr, Pointer sender, Pointer args) {
                final String url = WebView2VTable.getString(webview, WebView2VTable.WV_GET_SOURCE);
                fire(new Event() {
                    public void fire(Listener l) {
                        l.onSourceChanged(url);
                    }
                });
                return 0;
            }
        });
        addEventHandler(WebView2VTable.WV_ADD_DOCUMENT_TITLE_CHANGED, WebView2Guids.IID_DocumentTitleChangedEventHandler, new WebView2ComHandler.Invoke2() {
            public int invoke(Pointer thisPtr, Pointer sender, Pointer args) {
                final String title = WebView2VTable.getString(webview, WebView2VTable.WV_GET_DOCUMENT_TITLE);
                fire(new Event() {
                    public void fire(Listener l) {
                        l.onDocumentTitleChanged(title);
                    }
                });
                return 0;
            }
        });
        addEventHandler(WebView2VTable.WV_ADD_NEW_WINDOW_REQUESTED, WebView2Guids.IID_NewWindowRequestedEventHandler, new WebView2ComHandler.Invoke2() {
            public int invoke(Pointer thisPtr, Pointer sender, Pointer args) {
                /* Handled without NewWindow: WebView2 opens no window at all and window.open() gets a dummy proxy. */
                WebView2VTable.invokeHR(args, WebView2VTable.NEW_WINDOW_ARGS_PUT_HANDLED, Integer.valueOf(1));
                final String url = WebView2VTable.getString(args, WebView2VTable.NEW_WINDOW_ARGS_GET_URI);
                final boolean userInitiated = WebView2VTable.getBoolean(args, WebView2VTable.NEW_WINDOW_ARGS_GET_IS_USER_INITIATED, false);
                if (url != null && url.length() > 0 && !isPageNavigationAllowed(url)) {
                    /* The page must not get through a new window what it cannot get by navigating. */
                    fireBlocked(url, true);
                    return 0;
                }
                fire(new Event() {
                    public void fire(Listener l) {
                        l.onNewWindowRequested(url, userInitiated);
                    }
                });
                return 0;
            }
        });
        addEventHandler(WebView2VTable.WV_ADD_WINDOW_CLOSE_REQUESTED, WebView2Guids.IID_WindowCloseRequestedEventHandler, new WebView2ComHandler.Invoke2() {
            public int invoke(Pointer thisPtr, Pointer sender, Pointer args) {
                fire(new Event() {
                    public void fire(Listener l) {
                        l.onWindowCloseRequested();
                    }
                });
                return 0;
            }
        });
        addEventHandler(WebView2VTable.WV_ADD_WEB_RESOURCE_REQUESTED, WebView2Guids.IID_WebResourceRequestedEventHandler, new WebView2ComHandler.Invoke2() {
            public int invoke(Pointer thisPtr, Pointer sender, Pointer args) {
                /*
                 * Security checks and URL filter first, for everything (incl. iframe documents/resources). Then the host handler,
                 * then a pending document override.
                 */
                if (blockForbiddenRequest(args)) {
                    return 0;
                }
                if (!handleRequest(args)) {
                    handleDocumentOverride(args);
                }
                return 0;
            }
        });
        /* ALL contexts: main frame and iframes (documents + subresources). */
        WebView2VTable.invokeHR(webview, WebView2VTable.WV_ADD_WEB_RESOURCE_REQUESTED_FILTER, new WString("*"), Integer.valueOf(WebView2VTable.WEB_RESOURCE_CONTEXT_ALL));
        if (opts.interceptDownloads) {
            installDownloadRedirect();
        }
        installContextMenu();
        installFaviconChanged();
    }

    /** Installs the host-message bridge in the main world and prepares the isolated world. */
    private void installBridge() {
        if (StringUtils.isNotEmpty(bridgeScript)) {
            /* Main-world copy: owns chrome.webview.postMessage and forwards messages received from isolated frames. */
            final int nativeBridge = WebView2VTable.invokeHR(webview, WebView2VTable.WV_ADD_SCRIPT_ON_DOCUMENT_CREATED, new WString(bridgeScript), Pointer.NULL);
            if (!WebView2VTable.succeeded(nativeBridge)) {
                LogV3.warning("WebView2: native host bridge registration failed: 0x" + Integer.toHexString(nativeBridge));
            }
        }
        try {
            /* Without Page.enable the browser accepts scripts for new documents but never runs them. */
            callDevToolsOnSta("Page.enable", null);
            devTools.addScriptOnDocumentCreated("void 0;");
            isolatedWorld = true;
            installHostBinding();
        } catch (WebView2Exception error) {
            /*
             * An old runtime without the needed CDP Page methods. From now on host scripts run in the main world of the page. This is
             * decided once, here: a later script failure never moves a script from the isolated world into the page.
             */
            isolatedWorld = false;
            LogV3.warning("WebView2: isolated world unavailable, host scripts run in the main world: " + error.getMessage());
        }
    }

    /**
     * Gives the isolated world a private way to post to the host: a function that only exists in that world and whose calls arrive
     * here as a DevTools event. Nothing of it passes through the page, so the page can neither read nor forge these messages. Without
     * it, host scripts in the isolated world cannot post at all; they are never sent through {@code window.postMessage} of the page.
     */
    private void installHostBinding() {
        final PointerByReference receiverRef = new PointerByReference();
        final int receiverHr = WebView2VTable.invokeHR(webview, WebView2VTable.WV_GET_DEVTOOLS_PROTOCOL_EVENT_RECEIVER, new WString("Runtime.bindingCalled"), receiverRef);
        final Pointer receiver = receiverRef.getValue();
        if (!WebView2VTable.succeeded(receiverHr) || receiver == null) {
            LogV3.severe("WebView2: no private channel for host scripts (event receiver 0x" + Integer.toHexString(receiverHr) + "); __awHostPost is unavailable in the isolated world");
            return;
        }
        try {
            final int added = addEventHandler(receiver, WebView2VTable.DEVTOOLS_RECEIVER_ADD_EVENT_RECEIVED, WebView2Guids.IID_DevToolsProtocolEventReceivedEventHandler, new WebView2ComHandler.Invoke2() {
                public int invoke(Pointer thisPtr, Pointer sender, Pointer args) {
                    final String json = args != null ? WebView2VTable.getString(args, WebView2VTable.DEVTOOLS_EVENT_ARGS_GET_PARAMETER_JSON) : null;
                    if (json != null) {
                        fireHostScriptMessage(json);
                    }
                    return 0;
                }
            });
            if (!WebView2VTable.succeeded(added)) {
                LogV3.severe("WebView2: no private channel for host scripts (subscribe 0x" + Integer.toHexString(added) + ")");
                return;
            }
            /*
             * The function is installed per isolated world when the world is created (WebView2DevTools), not for the whole page by
             * name: that would need Runtime.enable, which page scripts can detect.
             */
            devTools.setBindingName(HOST_BINDING);
            hostBinding = true;
        } finally {
            WebView2VTable.release(receiver);
        }
    }

    /** {@code Runtime.bindingCalled}: {@code {"name": ..., "payload": ...}}; the payload is the envelope written by the bridge. */
    private void fireHostScriptMessage(String eventJson) {
        try {
            final Map<String, Object> event = JSonStorage.restoreFromString(eventJson, TypeRef.MAP);
            if (event == null || !HOST_BINDING.equals(event.get("name")) || !(event.get("payload") instanceof String)) {
                return;
            }
            final Map<String, Object> envelope = JSonStorage.restoreFromString((String) event.get("payload"), TypeRef.MAP);
            if (envelope == null || !(envelope.get("payload") instanceof String)) {
                return;
            }
            final String message = (String) envelope.get("payload");
            final String frameUrl = envelope.get("url") instanceof String ? (String) envelope.get("url") : null;
            final boolean topFrame = Boolean.TRUE.equals(envelope.get("top"));
            fire(new Event() {
                public void fire(Listener l) {
                    l.onHostScriptMessage(frameUrl, topFrame, message);
                }
            });
        } catch (RuntimeException e) {
            LogV3.log(e);
        }
    }

    /** True when host scripts in the isolated world can post to the host ({@link Listener#onHostScriptMessage}). */
    public boolean hasHostScriptChannel() {
        return hostBinding;
    }

    /** Subscribes to the favicon the browser already resolved, so the host does not download it again. */
    private void installFaviconChanged() {
        final Pointer webview15 = WebView2VTable.queryInterface(webview, WebView2Guids.IID_ICoreWebView2_15);
        if (webview15 == null) {
            LogV3.warning("WebView2: FaviconChanged is unavailable");
            return;
        }
        try {
            addEventHandler(webview15, WebView2VTable.WV15_ADD_FAVICON_CHANGED, WebView2Guids.IID_FaviconChangedEventHandler, new WebView2ComHandler.Invoke2() {
                public int invoke(Pointer thisPtr, Pointer sender, Pointer args) {
                    requestFaviconOnSta();
                    return 0;
                }
            });
            nativeFavicon = true;
        } finally {
            WebView2VTable.release(webview15);
        }
    }

    /** True when this browser reports favicons through {@link Listener#onFavicon}. */
    public boolean hasNativeFavicon() {
        return nativeFavicon;
    }

    /** Asks WebView2 for the current favicon; the answer arrives through {@link Listener#onFavicon}. */
    public void requestFavicon() {
        if (!nativeFavicon || disposed) {
            return;
        }
        try {
            sta.execute(new Runnable() {
                public void run() {
                    requestFaviconOnSta();
                }
            });
        } catch (WebView2Exception e) {
            LogV3.log(e);
        }
    }

    private void requestFaviconOnSta() {
        if (webview == null || disposed) {
            return;
        }
        final String pageUrl = WebView2VTable.getString(webview, WebView2VTable.WV_GET_SOURCE);
        final Pointer webview15 = WebView2VTable.queryInterface(webview, WebView2Guids.IID_ICoreWebView2_15);
        if (webview15 == null) {
            return;
        }
        try {
            final WebView2ComHandler[] holder = new WebView2ComHandler[1];
            holder[0] = WebView2ComHandler.createHR(WebView2Guids.IID_GetFaviconCompletedHandler, new WebView2ComHandler.InvokeHRObject() {
                public int invoke(Pointer thisPtr, int hresult, Pointer stream) {
                    try {
                        final byte[] png = WebView2VTable.succeeded(hresult) ? readStream(stream, 256 * 1024) : null;
                        fire(new Event() {
                            public void fire(Listener listener) {
                                listener.onFavicon(pageUrl, png);
                            }
                        });
                    } catch (Throwable t) {
                        LogV3.log(t);
                    }
                    return 0;
                }
            });
            final int hr = WebView2VTable.invokeHR(webview15, WebView2VTable.WV15_GET_FAVICON, Integer.valueOf(WebView2VTable.FAVICON_FORMAT_PNG), holder[0].getPointer());
            if (WebView2VTable.succeeded(hr)) {
                holder[0].detach();
            } else {
                holder[0].releaseKeepAlive();
            }
        } finally {
            WebView2VTable.release(webview15);
        }
    }

    /** Reads an {@code IStream} into a byte array of at most {@code limit} bytes. The stream is not released. */
    private static byte[] readStream(Pointer stream, int limit) {
        if (stream == null) {
            return null;
        }
        final int chunk = 8192;
        final ByteArrayOutputStream out = new ByteArrayOutputStream();
        final Memory buffer = new Memory(chunk);
        while (out.size() < limit) {
            final int wanted = Math.min(chunk, limit - out.size());
            final IntByReference read = new IntByReference();
            final int hr = WebView2VTable.invokeHR(stream, WebView2VTable.STREAM_READ, buffer, Integer.valueOf(wanted), read);
            final int n = read.getValue();
            /* A stream may return fewer bytes than asked for without being at its end; only 0 bytes (or an error) ends it. */
            if (!WebView2VTable.succeeded(hr) || n <= 0) {
                break;
            }
            out.write(buffer.getByteArray(0, n), 0, n);
        }
        return out.size() > 0 ? out.toByteArray() : null;
    }

    private void installContextMenu() {
        final Pointer webview11 = WebView2VTable.queryInterface(webview, WebView2Guids.IID_ICoreWebView2_11);
        if (webview11 == null) {
            LogV3.warning("WebView2: ContextMenuRequested is unavailable");
            return;
        }
        try {
            addEventHandler(webview11, WebView2VTable.WV11_ADD_CONTEXT_MENU_REQUESTED, WebView2Guids.IID_ContextMenuRequestedEventHandler, new WebView2ComHandler.Invoke2() {
                public int invoke(Pointer thisPtr, Pointer sender, Pointer args) {
                    addContextMenuItems(args);
                    return 0;
                }
            });
        } finally {
            WebView2VTable.release(webview11);
        }
    }

    /** {@code null} removes the own entries from the context menu of the page. */
    public void setContextMenuProvider(ContextMenuProvider provider) {
        this.contextMenuProvider = provider;
    }

    /** Inserts the entries of the {@link ContextMenuProvider} at the top of the menu that WebView2 is about to show. */
    private void addContextMenuItems(Pointer args) {
        final ContextMenuProvider provider = contextMenuProvider;
        if (provider == null || args == null || environment == null) {
            return;
        }
        try {
            final POINT point = new POINT();
            final boolean hasPoint = WebView2VTable.succeeded(WebView2VTable.invokeHR(args, WebView2VTable.CTX_GET_LOCATION, point));
            if (hasPoint) {
                point.read();
            }
            final ContextMenuTarget target = new ContextMenuTarget(httpLinkTarget(args), httpImageTarget(args), hasPoint, hasPoint ? point.x : 0, hasPoint ? point.y : 0);
            final List<ContextMenuItem> custom;
            nativeCallbackDepth++;
            try {
                custom = provider.getItems(target);
            } finally {
                nativeCallbackDepth--;
            }
            if (custom == null || custom.isEmpty()) {
                return;
            }
            final Pointer env9 = WebView2VTable.queryInterface(environment, WebView2Guids.IID_ICoreWebView2Environment9);
            if (env9 == null) {
                return;
            }
            try {
                final Pointer items = WebView2VTable.getInterface(args, WebView2VTable.CTX_GET_MENU_ITEMS);
                if (items == null) {
                    return;
                }
                try {
                    int index = 0;
                    for (final ContextMenuItem item : custom) {
                        if (item == null || item.getAction() == null) {
                            continue;
                        }
                        if (insertCommand(env9, items, index, item.getLabel(), new WebView2ComHandler.Invoke2() {
                            public int invoke(Pointer thisPtr, Pointer sender, Pointer unused) {
                                /* Run after this native event, so the action may call the browser. */
                                sta.post(new Runnable() {
                                    public void run() {
                                        try {
                                            item.getAction().run();
                                        } catch (Throwable t) {
                                            LogV3.log(t);
                                        }
                                    }
                                });
                                return 0;
                            }
                        })) {
                            index++;
                        }
                    }
                    if (index > 0) {
                        final Pointer separator = createContextMenuItem(env9, "", WebView2VTable.MENU_KIND_SEPARATOR);
                        if (separator != null) {
                            try {
                                WebView2VTable.invokeHR(items, WebView2VTable.MENU_INSERT, Integer.valueOf(index), separator);
                            } finally {
                                WebView2VTable.release(separator);
                            }
                        }
                    }
                } finally {
                    WebView2VTable.release(items);
                }
            } finally {
                WebView2VTable.release(env9);
            }
        } catch (Throwable t) {
            LogV3.log(t);
        }
    }

    /** Link address when the menu was opened on an http(s) link, otherwise {@code null}. */
    private static String httpLinkTarget(Pointer args) {
        return httpTargetUri(args, WebView2VTable.CTX_TARGET_HAS_LINK_URI, WebView2VTable.CTX_TARGET_GET_LINK_URI);
    }

    /** Image/media source when the menu was opened on an http(s) image, otherwise {@code null}. */
    private static String httpImageTarget(Pointer args) {
        return httpTargetUri(args, WebView2VTable.CTX_TARGET_HAS_SOURCE_URI, WebView2VTable.CTX_TARGET_GET_SOURCE_URI);
    }

    private static String httpTargetUri(Pointer args, int hasSlot, int getSlot) {
        final Pointer target = WebView2VTable.getInterface(args, WebView2VTable.CTX_GET_CONTEXT_MENU_TARGET);
        if (target == null) {
            return null;
        }
        try {
            if (!WebView2VTable.getBoolean(target, hasSlot, false)) {
                return null;
            }
            final String uri = WebView2VTable.getString(target, getSlot);
            if (uri == null) {
                return null;
            }
            final String lower = uri.toLowerCase(Locale.ENGLISH);
            if (lower.startsWith("https://") || lower.startsWith("http://")) {
                return uri;
            }
            return null;
        } finally {
            WebView2VTable.release(target);
        }
    }

    /** @return {@code true} when the command was inserted */
    private boolean insertCommand(Pointer environment9, Pointer items, int index, String label, WebView2ComHandler.Invoke2 onSelected) {
        final Pointer item = createContextMenuItem(environment9, label, WebView2VTable.MENU_KIND_COMMAND);
        if (item == null) {
            return false;
        }
        try {
            /* The handler lives as long as the menu item references it and is freed with the menu. */
            final WebView2ComHandler selected = WebView2ComHandler.create(WebView2Guids.IID_CustomItemSelectedEventHandler, onSelected);
            final int added = WebView2VTable.addEventHandler(item, WebView2VTable.MENU_ITEM_ADD_SELECTED, selected.getPointer());
            if (!WebView2VTable.succeeded(added)) {
                selected.releaseKeepAlive();
                return false;
            }
            selected.detach();
            return WebView2VTable.succeeded(WebView2VTable.invokeHR(items, WebView2VTable.MENU_INSERT, Integer.valueOf(index), item));
        } finally {
            WebView2VTable.release(item);
        }
    }

    private static Pointer createContextMenuItem(Pointer environment9, String label, int kind) {
        final PointerByReference created = new PointerByReference();
        final int hr = WebView2VTable.invokeHR(environment9, WebView2VTable.ENV9_CREATE_CONTEXT_MENU_ITEM, new WString(label != null ? label : ""), Pointer.NULL, Integer.valueOf(kind), created);
        if (!WebView2VTable.succeeded(hr)) {
            return null;
        }
        return created.getValue();
    }

    /**
     * Cancels every browser download, including context-menu "Save as", and reports the URL. WebView2 keeps writing the file unless
     * {@code Cancel} is set before this callback returns, so the hand-off to the host happens afterwards.
     */
    private void installDownloadRedirect() {
        final Pointer webview4 = WebView2VTable.queryInterface(webview, WebView2Guids.IID_ICoreWebView2_4);
        if (webview4 == null) {
            LogV3.warning("WebView2: DownloadStarting is unavailable");
            return;
        }
        try {
            addEventHandler(webview4, WebView2VTable.WV4_ADD_DOWNLOAD_STARTING, WebView2Guids.IID_DownloadStartingEventHandler, new WebView2ComHandler.Invoke2() {
                public int invoke(Pointer thisPtr, Pointer sender, Pointer args) {
                    final String url = cancelBrowserDownload(args);
                    fire(new Event() {
                        public void fire(Listener l) {
                            l.onDownloadStarting(url);
                        }
                    });
                    return 0;
                }
            });
        } finally {
            WebView2VTable.release(webview4);
        }
    }

    /** Stops WebView2 from saving the file or showing its download dialog. Returns the download URI, if any. */
    private String cancelBrowserDownload(Pointer args) {
        if (args == null) {
            return null;
        }
        String uri = null;
        final Pointer operation = WebView2VTable.getInterface(args, WebView2VTable.DL_ARGS_GET_OPERATION);
        if (operation != null) {
            try {
                uri = WebView2VTable.getString(operation, WebView2VTable.DL_OP_GET_URI);
            } finally {
                WebView2VTable.release(operation);
            }
        }
        WebView2VTable.invokeHR(args, WebView2VTable.DL_ARGS_PUT_CANCEL, Integer.valueOf(1));
        WebView2VTable.invokeHR(args, WebView2VTable.DL_ARGS_PUT_HANDLED, Integer.valueOf(1));
        return uri;
    }

    /** {@code null} lets every request load normally. */
    public void setRequestHandler(RequestHandler handler) {
        this.requestHandler = handler;
    }

    /** {@code null} lets every navigation proceed. */
    public void setNavigationInterceptor(NavigationInterceptor interceptor) {
        this.navigationInterceptor = interceptor;
    }

    /**
     * If a {@link #loadHtmlWithBaseUrl} override is pending and {@code args} is a matching main document request, substitutes the HTML so
     * the document keeps the origin of the URL without loading the real page (none of its headers apply).
     *
     * @return {@code true} if this request was answered with the override document
     */
    private boolean handleDocumentOverride(Pointer args) {
        final String html = documentOverrideHtml;
        final String origin = documentOverrideOrigin;
        if (html == null || origin == null || args == null) {
            return false;
        }
        final int context = WebView2VTable.getInt(args, WebView2VTable.WR_ARGS_GET_RESOURCE_CONTEXT, -1);
        if (context != WebView2VTable.WEB_RESOURCE_CONTEXT_DOCUMENT) {
            return false;
        }
        final Pointer request = WebView2VTable.getInterface(args, WebView2VTable.WR_ARGS_GET_REQUEST);
        if (request == null) {
            return false;
        }
        try {
            final String uri = WebView2VTable.getString(request, WebView2VTable.WR_REQUEST_GET_URI);
            /* Exactly the document that was asked for, not just any document (e.g. an iframe) of the same site. */
            if (uri == null || !origin.equals(documentKey(uri))) {
                return false;
            }
            /* Consume the override before building the response so a nested request cannot double-serve. */
            documentOverrideHtml = null;
            documentOverrideOrigin = null;
            final byte[] body;
            try {
                body = html.getBytes("UTF-8");
            } catch (UnsupportedEncodingException e) {
                /* UTF-8 is required on every Java platform. */
                throw new RuntimeException(e);
            }
            if (!respond(args, new Response(200, "OK", HTML_RESPONSE_HEADERS, body))) {
                LogV3.warning("WebView2: CreateWebResourceResponse failed for document override");
                return false;
            }
            return true;
        } finally {
            WebView2VTable.release(request);
        }
    }

    /**
     * Offers the request to the {@link RequestHandler}.
     *
     * @return {@code true} when the handler answered the request (main frame or iframe)
     */
    private boolean handleRequest(Pointer args) {
        final RequestHandler handler = requestHandler;
        if (handler == null || args == null || environment == null) {
            return false;
        }
        final Pointer nativeRequest = WebView2VTable.getInterface(args, WebView2VTable.WR_ARGS_GET_REQUEST);
        if (nativeRequest == null) {
            return false;
        }
        try {
            final String uri = WebView2VTable.getString(nativeRequest, WebView2VTable.WR_REQUEST_GET_URI);
            if (uri == null) {
                return false;
            }
            final int context = WebView2VTable.getInt(args, WebView2VTable.WR_ARGS_GET_RESOURCE_CONTEXT, -1);
            final Request request = new Request(nativeRequest, uri, context == WebView2VTable.WEB_RESOURCE_CONTEXT_DOCUMENT);
            final Response response;
            nativeCallbackDepth++;
            try {
                response = handler.handle(request);
            } finally {
                nativeCallbackDepth--;
                request.closed = true;
            }
            return response != null && respond(args, response);
        } catch (Throwable t) {
            LogV3.log(t);
            return false;
        } finally {
            WebView2VTable.release(nativeRequest);
        }
    }

    /** @return {@code true} when {@code response} was set as the answer of the request */
    private boolean respond(Pointer args, Response response) {
        final Pointer nativeResponse = WebView2VTable.createWebResourceResponse(environment, response.getBody(), response.getStatus(), response.getReason(), response.getHeaders());
        if (nativeResponse == null) {
            return false;
        }
        try {
            WebView2VTable.invokeHR(args, WebView2VTable.WR_ARGS_PUT_RESPONSE, nativeResponse);
            return true;
        } finally {
            WebView2VTable.release(nativeResponse);
        }
    }

    private int addEventHandler(int addMethodIndex, Guid.GUID iid, WebView2ComHandler.Invoke2 callback) {
        return addEventHandler(webview, addMethodIndex, iid, callback);
    }

    private int addEventHandler(Pointer target, int addMethodIndex, Guid.GUID iid, WebView2ComHandler.Invoke2 callback) {
        final WebView2ComHandler handler = WebView2ComHandler.create(iid, callback);
        final int hr = WebView2VTable.addEventHandler(target, addMethodIndex, handler.getPointer());
        if (WebView2VTable.succeeded(hr)) {
            /* WebView2 now references the handler and frees it when the browser is closed. */
            handler.detach();
        } else {
            handler.releaseKeepAlive();
        }
        return hr;
    }

    /**
     * Delivers {@code event} to the listeners from the message loop of the STA thread, in the order the events were fired. It is never
     * delivered inside the native WebView2 event that caused it: WebView2 does not support waiting for another callback while one is
     * still open, so a listener that runs a script from inside the event would only time out.
     */
    private void fire(final Event event) {
        sta.post(new Runnable() {
            public void run() {
                for (Listener l : listeners) {
                    try {
                        event.fire(l);
                    } catch (Throwable t) {
                        LogV3.log(t);
                    }
                }
            }
        });
    }

    private void fireBlocked(final String url, final boolean document) {
        LogV3.info("WebView2: blocked " + (document ? "document " : "resource ") + url);
        fire(new Event() {
            public void fire(Listener l) {
                l.onRequestBlocked(url, document);
            }
        });
    }

    /** Tells a message of the top document from one that the bridge passed on for another frame. */
    private void fireMessage(final String pageUrl, final String message) {
        if (message.startsWith(FRAME_MESSAGE_PREFIX)) {
            try {
                final Map<String, Object> envelope = JSonStorage.restoreFromString(message, TypeRef.MAP);
                final Object origin = envelope != null ? envelope.get(FRAME_MESSAGE_KEY) : null;
                final Object payload = envelope != null ? envelope.get("payload") : null;
                if (origin instanceof String && payload instanceof String) {
                    fire(new Event() {
                        public void fire(Listener l) {
                            l.onFrameMessage(pageUrl, (String) origin, (String) payload);
                        }
                    });
                    return;
                }
            } catch (RuntimeException notAnEnvelope) {
                /* Handled as an ordinary message of the top document below. */
            }
        }
        fire(new Event() {
            public void fire(Listener l) {
                l.onWebMessage(pageUrl, message);
            }
        });
    }

    /**
     * {@code false} for an address that must not be opened: {@code file:} unless allowed, and every scheme that is not handled by the
     * browser itself but handed to Windows, where it may start another program.
     */
    private boolean isSchemeAllowed(String uri) {
        final int colon = uri.indexOf(':');
        if (colon <= 0) {
            return false;
        }
        final String scheme = uri.substring(0, colon).toLowerCase(Locale.ENGLISH);
        if ("http".equals(scheme) || "https".equals(scheme) || "about".equals(scheme) || "data".equals(scheme) || "blob".equals(scheme)) {
            return true;
        }
        if ("file".equals(scheme)) {
            return allowFileUrls;
        }
        return allowExternalSchemes;
    }

    /**
     * {@code true} when {@code uri} points to this PC or the local network and its host is not in {@link Options#allowedLocalHosts}.
     * Who asks makes no difference: a navigation of the application is judged like a request of a page.
     */
    private boolean isForbiddenLocalRequest(String uri) {
        final String host = WebView2UrlFilter.hostOf(uri);
        if (!WebView2UrlFilter.isLocalNetworkHost(host)) {
            return false;
        }
        return !allowedLocalHosts.contains(host);
    }

    /**
     * {@code true} when the browser shows a local page and {@code frameUrl} is an iframe of another host that the application did not
     * allow, see {@link Options#isIframeBlockedOnLocalWebsite(String, String)}. Runs inside the native FrameNavigationStarting event.
     */
    private boolean isForbiddenIframeOfLocalPage(String frameUrl) {
        if (!blockLocalNetwork) {
            return false;
        }
        final String frameHost = WebView2UrlFilter.hostOf(frameUrl);
        if (frameHost == null) {
            /* about:, data: and blob: documents belong to the document that created them. */
            return false;
        }
        final String pageUrl = WebView2VTable.getString(webview, WebView2VTable.WV_GET_SOURCE);
        final String pageHost = pageUrl != null ? WebView2UrlFilter.hostOf(pageUrl) : null;
        if (pageHost == null || !WebView2UrlFilter.isLocalNetworkHost(pageHost) || frameHost.equals(pageHost)) {
            return false;
        }
        nativeCallbackDepth++;
        try {
            return iframePolicy.isIframeBlockedOnLocalWebsite(pageUrl, frameUrl);
        } catch (Throwable t) {
            /* Fail closed. */
            LogV3.log(t);
            return true;
        } finally {
            nativeCallbackDepth--;
        }
    }

    /** The checks a navigation of the page has to pass: scheme, local network, URL filter. */
    private boolean isPageNavigationAllowed(String url) {
        if (!isSchemeAllowed(url)) {
            return false;
        }
        if (blockLocalNetwork && isForbiddenLocalRequest(url)) {
            return false;
        }
        final WebView2UrlFilter filter = urlFilter;
        return filter == null || filter.isAllowed(url, true);
    }

    private void releaseFilterProxy() {
        final WebView2FilterProxy proxy = filterProxy;
        filterProxy = null;
        if (proxy != null) {
            proxy.removePolicy(proxyPolicy);
            if (ownsFilterProxy) {
                proxy.close();
            }
        }
    }

    /** Port of the filter proxy of this browser on the loopback interface, or -1 when {@link Options#filterProxy} is off. */
    public int getFilterProxyPort() {
        final WebView2FilterProxy proxy = filterProxy;
        return proxy != null ? proxy.getPort() : -1;
    }

    /**
     * Fails closed: a browser that was configured with the filter proxy must really send its traffic through it. The empty start
     * document requests an address that only the filter answers; if that request does not arrive there, the browser is not used.
     */
    private void verifyFilterProxy() throws WebView2Exception {
        final WebView2FilterProxy proxy = filterProxy;
        if (proxy == null) {
            return;
        }
        final String nonce = UUID.randomUUID().toString();
        final AtomicBoolean arrived = proxy.expectSelfTest(nonce);
        try {
            executeScriptInMainWorldOnSta("fetch('" + SELF_TEST_PREFIX + nonce + "',{mode:'no-cors',cache:'no-store'}).catch(function(){});void 0;");
            sta.pumpUntil(arrived, SELF_TEST_TIMEOUT_MS);
        } catch (WebView2Exception e) {
            throw new WebView2Exception("The browser does not send its traffic through the filter proxy; it was not started (" + e.getMessage() + ")", e);
        } finally {
            proxy.cancelSelfTest(nonce);
        }
    }

    private int createEnvironment(WebView2Runtime runtime, String userData, WebView2ComHandler envHandler) {
        final int hr = runtime.createEnvironment(userData, envHandler.getPointer());
        if (WebView2VTable.succeeded(hr)) {
            envHandler.detach();
        }
        return hr;
    }

    /**
     * WebView2 takes extra browser arguments from an environment variable of this process, and it reads the variable at some point
     * while the browser is being created, not at a fixed call. So the variable holds the arguments of this browser for the whole
     * creation and is put back afterwards. Creations run one after the other on the STA thread; one that starts while another one
     * waits (nested) sets its own value and restores the value of the outer one when it is done.
     *
     * @return the value to restore after the creation
     */
    private String applyBrowserArguments() {
        final String previous = readEnvironmentVariable(BROWSER_ARGUMENTS_VARIABLE);
        synchronized (BROWSER_ARGUMENTS_VARIABLE) {
            if (!originalArgumentsRead) {
                /* What the application or the user set, before any browser of this class changed it. */
                originalArguments = previous;
                originalArgumentsRead = true;
            }
        }
        final WebView2FilterProxy proxy = filterProxy;
        final String original = originalArguments;
        final String own = proxy != null ? proxy.getBrowserArguments() : null;
        final String arguments;
        if (own == null) {
            arguments = original;
        } else {
            arguments = original != null && original.length() > 0 ? original + " " + own : own;
        }
        Kernel32.INSTANCE.SetEnvironmentVariable(BROWSER_ARGUMENTS_VARIABLE, arguments);
        return previous;
    }

    private static String readEnvironmentVariable(String name) {
        final char[] buffer = new char[32767];
        final int length = Kernel32.INSTANCE.GetEnvironmentVariable(name, buffer, buffer.length);
        return length > 0 && length < buffer.length ? new String(buffer, 0, length) : null;
    }

    /**
     * Refuses a request to the local network or one that the {@link WebView2UrlFilter} forbids.
     *
     * @return {@code true} when the request was answered with "blocked"
     */
    private boolean blockForbiddenRequest(Pointer args) {
        final WebView2UrlFilter filter = urlFilter;
        if ((filter == null && !blockLocalNetwork) || args == null || environment == null) {
            return false;
        }
        final Pointer request = WebView2VTable.getInterface(args, WebView2VTable.WR_ARGS_GET_REQUEST);
        if (request == null) {
            return false;
        }
        try {
            final String uri = WebView2VTable.getString(request, WebView2VTable.WR_REQUEST_GET_URI);
            if (uri == null) {
                return false;
            }
            if (uri.startsWith(SELF_TEST_PREFIX)) {
                /* The self test of the filter proxy; it is answered by the filter and reaches no server. */
                return false;
            }
            final boolean document = WebView2VTable.getInt(args, WebView2VTable.WR_ARGS_GET_RESOURCE_CONTEXT, -1) == WebView2VTable.WEB_RESOURCE_CONTEXT_DOCUMENT;
            final boolean forbidden = (blockLocalNetwork && isForbiddenLocalRequest(uri)) || (filter != null && !filter.isAllowed(uri, document));
            if (!forbidden) {
                return false;
            }
            final Response response;
            if (document) {
                final String host = WebView2UrlFilter.hostOf(uri);
                final String html = "<!doctype html><meta charset=\"utf-8\"><title>Blocked</title><body style=\"font-family:sans-serif;margin:2em\"><h3>This address is blocked</h3><p>" + escapeHtml(host != null ? host : "") + "</p></body>";
                response = new Response(403, "Blocked", HTML_RESPONSE_HEADERS, html.getBytes("UTF-8"));
            } else {
                response = new Response(403, "Blocked", "Content-Type: text/plain\r\n", new byte[0]);
            }
            if (!respond(args, response)) {
                /* Must not happen. Failing closed is not possible here, so at least make it visible. */
                LogV3.severe("WebView2: could not block " + uri);
                return false;
            }
            fireBlocked(uri, document);
            return true;
        } catch (Throwable t) {
            LogV3.log(t);
            return false;
        } finally {
            WebView2VTable.release(request);
        }
    }

    private static String escapeHtml(String text) {
        return text.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quot;");
    }

    /**
     * Sets the whitelist/blacklist of the addresses the browser may load; {@code null} removes the restriction. Takes effect for
     * every request from now on. The filter object itself can also be changed while the browser runs.
     */
    public void setUrlFilter(WebView2UrlFilter filter) {
        this.urlFilter = filter;
    }

    public WebView2UrlFilter getUrlFilter() {
        return urlFilter;
    }

    /**
     * Number of page loads requested through this object so far ({@link #navigate(String)}, {@link #setHtml(String)},
     * {@link #loadHtmlWithBaseUrl(String, String)}). Lets a caller see whether somebody else requested a page in the meantime.
     */
    public int getNavigationRequestCount() {
        return navigationRequests.get();
    }

    /**
     * Scripts and DevTools calls wait for a native callback, which WebView2 cannot deliver while another native event is still being
     * answered. Fail at once instead of running into the timeout.
     */
    private void ensureNotInNativeCallback() throws WebView2Exception {
        if (nativeCallbackDepth > 0 && sta.isStaThread()) {
            throw new WebView2Exception("Not possible inside a RequestHandler, NavigationInterceptor or ContextMenuProvider; do it from a Listener or another thread");
        }
    }

    private WebView2ComHandler register(WebView2ComHandler handler) {
        handlers.add(handler);
        return handler;
    }

    public void addListener(Listener listener) {
        if (listener != null) {
            listeners.addIfAbsent(listener);
        }
    }

    public void removeListener(Listener listener) {
        listeners.remove(listener);
    }

    public void navigate(String url) throws WebView2Exception {
        navigate(url, null);
    }

    /**
     * Navigates to {@code url}. When {@code referrer} is an http(s) address, it is sent as the {@code Referer} header, the same way a real
     * {@code target=_blank} tab carries the opener page. Plain {@link #navigate(String)} has no previous document, so sites that require a
     * Referer answer with "Not allowed".
     */
    public void navigate(final String url, final String referrer) throws WebView2Exception {
        if (url == null) {
            throw new IllegalArgumentException("url is null");
        }
        navigationRequests.incrementAndGet();
        navigateInternal(url, referrer);
    }

    /**
     * Navigates to an address that came from a page, e.g. from {@link Listener#onNewWindowRequested(String, boolean)} or a web
     * message. Unlike {@link #navigate(String, String)} it gives the address no rights: a local network address stays blocked.
     */
    public void navigateFromPage(final String url, final String referrer) throws WebView2Exception {
        if (url == null) {
            throw new IllegalArgumentException("url is null");
        }
        navigationRequests.incrementAndGet();
        if (!isPageNavigationAllowed(url)) {
            fireBlocked(url, true);
            throw new WebView2Exception("Blocked: " + url);
        }
        navigateInternal(url, referrer);
    }

    private void navigateInternal(final String url, final String referrer) throws WebView2Exception {
        /* A pending document override must not leak into ordinary navigations. */
        documentOverrideHtml = null;
        documentOverrideOrigin = null;
        final String header = refererHeader(referrer);
        if (header == null) {
            invoke(Target.WEBVIEW, WebView2VTable.WV_NAVIGATE, new WString(url));
            return;
        }
        ensureAlive();
        final Integer hr = sta.call(new Callable<Integer>() {
            public Integer call() {
                return Integer.valueOf(navigateWithReferrerOnSta(url, header));
            }
        });
        if (hr == null || !WebView2VTable.succeeded(hr.intValue())) {
            LogV3.warning("WebView2: navigation with Referer failed (0x" + (hr == null ? "null" : Integer.toHexString(hr.intValue())) + "), retrying without it");
            invoke(Target.WEBVIEW, WebView2VTable.WV_NAVIGATE, new WString(url));
        }
    }

    /** {@code Referer} header line for {@code NavigateWithWebResourceRequest}, or {@code null} when there is nothing to send. */
    private static String refererHeader(String referrer) {
        if (StringUtils.isEmpty(referrer)) {
            return null;
        }
        final int hash = referrer.indexOf('#');
        if (hash >= 0) {
            referrer = referrer.substring(0, hash);
        }
        if (!StringUtils.startsWithCaseInsensitive(referrer, "http://") && !StringUtils.startsWithCaseInsensitive(referrer, "https://")) {
            return null;
        }
        if (referrer.indexOf('\r') >= 0 || referrer.indexOf('\n') >= 0) {
            return null;
        }
        return "Referer: " + referrer + "\r\n";
    }

    /** Caller is the WebView2 STA thread. Returns an HRESULT. */
    private int navigateWithReferrerOnSta(String url, String header) {
        if (webview == null || environment == null) {
            return 0x80004005;
        }
        final Pointer env2 = WebView2VTable.queryInterface(environment, WebView2Guids.IID_ICoreWebView2Environment2);
        final Pointer webview2 = WebView2VTable.queryInterface(webview, WebView2Guids.IID_ICoreWebView2_2);
        Pointer request = null;
        try {
            if (env2 == null || webview2 == null) {
                return 0x80004002;
            }
            final PointerByReference created = new PointerByReference();
            final int createHr = WebView2VTable.invokeHR(env2, WebView2VTable.ENV2_CREATE_WEB_RESOURCE_REQUEST, new WString(url), new WString("GET"), Pointer.NULL, new WString(header), created);
            if (!WebView2VTable.succeeded(createHr)) {
                return createHr;
            }
            request = created.getValue();
            return WebView2VTable.invokeHR(webview2, WebView2VTable.WV2_NAVIGATE_WITH_WEB_RESOURCE_REQUEST, request);
        } finally {
            if (request != null) {
                WebView2VTable.release(request);
            }
            if (webview2 != null) {
                WebView2VTable.release(webview2);
            }
            if (env2 != null) {
                WebView2VTable.release(env2);
            }
        }
    }

    /** Replaces the document with {@code html} ({@code NavigateToString}, origin {@code about:blank}). */
    public void setHtml(String html) throws WebView2Exception {
        navigationRequests.incrementAndGet();
        documentOverrideHtml = null;
        documentOverrideOrigin = null;
        invoke(Target.WEBVIEW, WebView2VTable.WV_NAVIGATE_TO_STRING, new WString(html != null ? html : ""));
    }

    /**
     * Android {@code loadDataWithBaseURL} equivalent: navigates to {@code url} but serves {@code html} as the main document response. The
     * document {@code location} / origin stay those of {@code url} (needed for widgets that check the domain) without loading the real
     * page or its CSP.
     *
     * @param url
     *            absolute http(s) URL used as the document address
     */
    public void loadHtmlWithBaseUrl(final String url, final String html) throws WebView2Exception {
        if (StringUtils.isEmpty(url)) {
            throw new WebView2Exception("loadHtmlWithBaseUrl requires a base URL");
        }
        ensureAlive();
        navigationRequests.incrementAndGet();
        final String origin = documentKey(url);
        if (origin == null) {
            setHtml(html);
            return;
        }
        sta.call(new Callable<Void>() {
            public Void call() throws WebView2Exception {
                documentOverrideOrigin = origin;
                documentOverrideHtml = html != null ? html : "";
                final int hr = WebView2VTable.invokeHR(webview, WebView2VTable.WV_NAVIGATE, new WString(url));
                if (!WebView2VTable.succeeded(hr)) {
                    documentOverrideHtml = null;
                    documentOverrideOrigin = null;
                    throw new WebView2Exception("Navigate for loadHtmlWithBaseUrl failed", hr);
                }
                return null;
            }
        });
    }

    /**
     * Adds a script that runs before any page script on every new document (incl. iframes) of this browser, in the isolated world of the
     * host.
     */
    public void addScriptOnDocumentCreated(final String script) throws WebView2Exception {
        if (StringUtils.isEmpty(script)) {
            return;
        }
        ensureAlive();
        sta.call(new Callable<Void>() {
            public Void call() throws WebView2Exception {
                if (isolatedWorld) {
                    devTools.addScriptOnDocumentCreated(script);
                } else {
                    final int hr = WebView2VTable.invokeHR(webview, WebView2VTable.WV_ADD_SCRIPT_ON_DOCUMENT_CREATED, new WString(bridgeScript + "\n" + script), Pointer.NULL);
                    if (!WebView2VTable.succeeded(hr)) {
                        throw new WebView2Exception("AddScriptToExecuteOnDocumentCreated failed", hr);
                    }
                }
                return null;
            }
        });
    }

    /**
     * Hard-reloads the current page and bypasses the HTTP cache ({@code Page.reload} with {@code ignoreCache}), matching Ctrl+Shift+R in
     * Edge. Falls back to a normal {@code Reload} if the DevTools call cannot be issued.
     */
    public void reload() throws WebView2Exception {
        ensureAlive();
        sta.call(new Callable<Void>() {
            public Void call() throws WebView2Exception {
                reloadIgnoringCacheOnSta();
                return null;
            }
        });
    }

    private void reloadIgnoringCacheOnSta() throws WebView2Exception {
        if (webview == null) {
            throw new WebView2Exception("WindowsWebView2Browser is disposed");
        }
        try {
            callDevToolsOnSta("Page.reload", WebView2DevTools.mapOf("ignoreCache", Boolean.TRUE));
            return;
        } catch (WebView2Exception error) {
            LogV3.info("WebView2: Page.reload fallback: " + error.getMessage());
        }
        final int fallback = WebView2VTable.invokeHR(webview, WebView2VTable.WV_RELOAD);
        if (!WebView2VTable.succeeded(fallback)) {
            throw new WebView2Exception("Reload failed", fallback);
        }
    }

    /**
     * Calls one method of the Chrome DevTools Protocol, e.g. {@code "Network.clearBrowserCache"}.
     *
     * @param parameters
     *            {@code null} for none
     * @return the parsed JSON result, empty when the method returns nothing
     */
    public Map<String, Object> callDevToolsProtocolMethod(final String method, final Map<String, Object> parameters) throws WebView2Exception {
        ensureAlive();
        return sta.call(new Callable<Map<String, Object>>() {
            public Map<String, Object> call() throws WebView2Exception {
                return callDevToolsOnSta(method, parameters);
            }
        });
    }

    /** Calls one WebView2 DevTools method. Caller must be the WebView2 STA thread. */
    private Map<String, Object> callDevToolsOnSta(String method, Map<String, Object> parameters) throws WebView2Exception {
        if (webview == null || disposed) {
            throw new WebView2Exception("WindowsWebView2Browser is disposed");
        }
        ensureNotInNativeCallback();
        final AtomicBoolean done = new AtomicBoolean(false);
        final AtomicReference<String> result = new AtomicReference<String>();
        final AtomicReference<String> error = new AtomicReference<String>();
        final WebView2ComHandler handler = WebView2ComHandler.createHR(WebView2Guids.IID_CallDevToolsProtocolMethodCompletedHandler, new WebView2ComHandler.InvokeHRObject() {
            public int invoke(Pointer thisPtr, int hresult, Pointer jsonResult) {
                try {
                    if (!WebView2VTable.succeeded(hresult)) {
                        error.set("DevTools method failed HRESULT=0x" + Integer.toHexString(hresult));
                    } else if (jsonResult != null) {
                        result.set(jsonResult.getWideString(0));
                    }
                } finally {
                    done.set(true);
                }
                return 0;
            }
        });
        final String json = JSonStorage.serializeToJson(parameters != null ? parameters : Collections.<String, Object> emptyMap());
        final int hr = WebView2VTable.invokeHR(webview, WebView2VTable.WV_CALL_DEVTOOLS_PROTOCOL_METHOD, new WString(method), new WString(json), handler.getPointer());
        if (!WebView2VTable.succeeded(hr)) {
            handler.releaseKeepAlive();
            throw new WebView2Exception("CallDevToolsProtocolMethod " + method + " failed", hr);
        }
        /* WebView2 holds the handler until it has called it, also if we stop waiting (timeout). */
        handler.detach();
        sta.pumpUntil(done, SCRIPT_TIMEOUT_MS);
        if (error.get() != null) {
            throw new WebView2Exception(method + ": " + error.get());
        }
        if (StringUtils.isEmpty(result.get())) {
            return Collections.emptyMap();
        }
        try {
            final Map<String, Object> parsed = JSonStorage.restoreFromString(result.get(), TypeRef.MAP);
            return parsed != null ? parsed : Collections.<String, Object> emptyMap();
        } catch (RuntimeException parseError) {
            throw new WebView2Exception("Invalid DevTools response for " + method, parseError);
        }
    }

    public void stop() throws WebView2Exception {
        invoke(Target.WEBVIEW, WebView2VTable.WV_STOP);
    }

    public void goBack() throws WebView2Exception {
        invoke(Target.WEBVIEW, WebView2VTable.WV_GO_BACK);
    }

    public void goForward() throws WebView2Exception {
        invoke(Target.WEBVIEW, WebView2VTable.WV_GO_FORWARD);
    }

    /** Opens the DevTools window of the page. Needs {@link Options#debugMode}. */
    public void openDevTools() throws WebView2Exception {
        invoke(Target.WEBVIEW, WebView2VTable.WV_OPEN_DEV_TOOLS_WINDOW);
    }

    public void setVisible(boolean visible) throws WebView2Exception {
        invoke(Target.CONTROLLER, WebView2VTable.CTRL_PUT_IS_VISIBLE, Integer.valueOf(visible ? 1 : 0));
    }

    /** Must be called when the window that contains the host component was moved, so popups of the page open at the right place. */
    public void notifyParentWindowPositionChanged() throws WebView2Exception {
        invoke(Target.CONTROLLER, WebView2VTable.CTRL_NOTIFY_PARENT_WINDOW_POSITION_CHANGED);
    }

    /** Current document URL (native {@code get_Source}). */
    public String getSource() throws WebView2Exception {
        return getString(WebView2VTable.WV_GET_SOURCE);
    }

    /** Current document title (native {@code get_DocumentTitle}). */
    public String getDocumentTitle() throws WebView2Exception {
        return getString(WebView2VTable.WV_GET_DOCUMENT_TITLE);
    }

    /** Absolute path of the profile folder this browser uses. */
    public String getUserDataFolder() {
        return userDataFolder;
    }

    /**
     * Sizes the WebView to the native client area of {@code parentHwnd} and sets its visibility (hidden WebViews are throttled by the
     * runtime). Uses physical pixels, so it stays correct under Windows DPI scaling (Swing sizes are logical pixels).
     * <p>
     * Returns at once; the browser is resized as soon as its thread gets to it. Safe to call on the EDT.
     */
    public void setBoundsToParentClientArea(final HWND parentHwnd, final boolean visible) {
        if (disposed) {
            return;
        }
        sta.post(new Runnable() {
            public void run() {
                final RECT client = new RECT();
                if (disposed || controller == null || !User32.INSTANCE.GetClientRect(parentHwnd, client)) {
                    return;
                }
                final int w = client.right - client.left;
                final int h = client.bottom - client.top;
                /* A 0-size controller stays permanently gray, even after later resizes. */
                if (w < 2 || h < 2) {
                    return;
                }
                WebView2VTable.invokeHR(controller, WebView2VTable.CTRL_PUT_BOUNDS, rect(0, 0, w, h));
                WebView2VTable.invokeHR(controller, WebView2VTable.CTRL_PUT_IS_VISIBLE, Integer.valueOf(visible ? 1 : 0));
                WebView2VTable.invokeHR(controller, WebView2VTable.CTRL_NOTIFY_PARENT_WINDOW_POSITION_CHANGED);
            }
        });
    }

    /**
     * Moves the browser into another parent window, e.g. after the component that hosts it was removed from one window and added to
     * another, which gives the component a new native window. Returns at once. Not possible for a browser that was created with
     * {@link #createInvisible(Options)}.
     *
     * @param parentHwnd
     *            the new parent window
     */
    public void setParentWindow(final HWND parentHwnd) {
        if (disposed || parentHwnd == null) {
            return;
        }
        sta.post(new Runnable() {
            public void run() {
                if (disposed || controller == null) {
                    return;
                }
                final int hr = WebView2VTable.invokeHR(controller, WebView2VTable.CTRL_PUT_PARENT_WINDOW, parentHwnd);
                if (!WebView2VTable.succeeded(hr)) {
                    LogV3.warning("WebView2: could not change the parent window: 0x" + Integer.toHexString(hr));
                }
            }
        });
    }

    /**
     * Hides the browser and moves it into {@code parkingHwnd}, and waits until that is done. Call it before the current parent window
     * is destroyed if the browser shall live on; {@link #setParentWindow(HWND)} brings it back into a visible window.
     *
     * @param parkingHwnd
     *            a window that stays alive, e.g. a frame that is never shown
     */
    public void parkInWindow(final HWND parkingHwnd) throws WebView2Exception {
        ensureAlive();
        final AtomicInteger result = new AtomicInteger();
        sta.execute(new Runnable() {
            public void run() {
                if (controller == null) {
                    return;
                }
                WebView2VTable.invokeHR(controller, WebView2VTable.CTRL_PUT_IS_VISIBLE, Integer.valueOf(0));
                result.set(WebView2VTable.invokeHR(controller, WebView2VTable.CTRL_PUT_PARENT_WINDOW, parkingHwnd));
            }
        });
        if (!WebView2VTable.succeeded(result.get())) {
            throw new WebView2Exception("Could not park the browser", result.get());
        }
    }

    /** Like {@link #notifyParentWindowPositionChanged()}, but returns at once. Safe to call on the EDT. */
    public void notifyParentWindowPositionChangedLater() {
        if (disposed) {
            return;
        }
        sta.post(new Runnable() {
            public void run() {
                if (!disposed && controller != null) {
                    WebView2VTable.invokeHR(controller, WebView2VTable.CTRL_NOTIFY_PARENT_WINDOW_POSITION_CHANGED);
                }
            }
        });
    }

    /**
     * Runs {@code script} once in the top document, in the isolated world of the host: page scripts cannot see its variables. Only an
     * old runtime without the needed DevTools methods runs host scripts in the main world; that is decided when the browser is
     * created, never because a script failed.
     *
     * @throws WebView2ScriptException
     *             when the script threw a JavaScript error
     * @return the value of the last expression: a string as is, anything else as JSON, {@code null} for {@code null}/{@code undefined}
     */
    public String executeScript(final String script) throws WebView2Exception {
        ensureAlive();
        return sta.call(new Callable<String>() {
            public String call() throws WebView2Exception {
                if (isolatedWorld) {
                    return devTools.evaluate(script);
                }
                return executeScriptInMainWorldOnSta(bridgeScript + "\n" + script);
            }
        });
    }

    /**
     * Runs {@code script} in the main JavaScript world of the page, where page scripts see it. Use it only when the page must receive
     * the call; prefer {@link #executeScript(String)}.
     */
    public String executeScriptInMainWorld(final String script) throws WebView2Exception {
        ensureAlive();
        return sta.call(new Callable<String>() {
            public String call() throws WebView2Exception {
                return executeScriptInMainWorldOnSta(script);
            }
        });
    }

    /** Runs {@code script} in the top document and every iframe, in the isolated world of the host. */
    public void executeScriptInAllFrames(final String script) throws WebView2Exception {
        if (StringUtils.isEmpty(script)) {
            return;
        }
        ensureAlive();
        sta.call(new Callable<Void>() {
            public Void call() throws WebView2Exception {
                if (isolatedWorld) {
                    devTools.evaluateInAllFrames(script);
                } else {
                    /* Old runtime without frame-level CDP methods: only the top document can be reached. */
                    executeScriptInMainWorldOnSta(bridgeScript + "\n" + script);
                }
                return null;
            }
        });
    }

    private String executeScriptInMainWorldOnSta(String script) throws WebView2Exception {
        ensureNotInNativeCallback();
        final AtomicBoolean done = new AtomicBoolean(false);
        final AtomicReference<String> result = new AtomicReference<String>();
        final AtomicReference<String> error = new AtomicReference<String>();
        final WebView2ComHandler handler = WebView2ComHandler.createHR(WebView2Guids.IID_ExecuteScriptCompletedHandler, new WebView2ComHandler.InvokeHRObject() {
            public int invoke(Pointer thisPtr, int hresult, Pointer jsonResult) {
                try {
                    if (!WebView2VTable.succeeded(hresult)) {
                        error.set("ExecuteScript failed HRESULT=0x" + Integer.toHexString(hresult));
                    } else if (jsonResult != null) {
                        result.set(jsonResult.getWideString(0));
                    }
                } finally {
                    done.set(true);
                }
                return 0;
            }
        });
        final int hr = WebView2VTable.invokeHR(webview, WebView2VTable.WV_EXECUTE_SCRIPT, new WString(script), handler.getPointer());
        if (!WebView2VTable.succeeded(hr)) {
            handler.releaseKeepAlive();
            throw new WebView2Exception("ExecuteScript invoke failed", hr);
        }
        /* WebView2 holds the handler until it has called it, also if we stop waiting (timeout). */
        handler.detach();
        sta.pumpUntil(done, SCRIPT_TIMEOUT_MS);
        if (error.get() != null) {
            throw new WebView2Exception(error.get());
        }
        final String json = result.get();
        if (StringUtils.isEmpty(json) || "null".equals(json) || "undefined".equals(json)) {
            return null;
        }
        final Object value = JSonStorage.restoreFromString(json, TypeRef.OBJECT);
        if (value instanceof String) {
            return (String) value;
        }
        return value != null ? JSonStorage.serializeToJson(value) : null;
    }

    public boolean isDisposed() {
        return disposed;
    }

    /** Closes the native browser. Safe to call more than once and from any thread. */
    public void dispose() {
        if (disposed) {
            return;
        }
        disposed = true;
        try {
            sta.execute(new Runnable() {
                public void run() {
                    disposeOnSta();
                }
            });
        } catch (WebView2Exception e) {
            LogV3.log(e);
        }
    }

    private void disposeOnSta() {
        listeners.clear();
        documentOverrideHtml = null;
        documentOverrideOrigin = null;
        try {
            if (controller != null) {
                WebView2VTable.invokeHR(controller, WebView2VTable.CTRL_CLOSE);
                WebView2VTable.release(controller);
            }
            if (webview != null) {
                WebView2VTable.release(webview);
            }
            if (environment != null) {
                WebView2VTable.release(environment);
            }
        } catch (Throwable t) {
            LogV3.log(t);
        } finally {
            controller = null;
            webview = null;
            environment = null;
            /* The COM handlers are freed by WebView2 releasing them; nothing is unpinned by force while it may still call. */
            handlers.clear();
            releaseFilterProxy();
            if (closedCallback != null) {
                try {
                    closedCallback.run();
                } catch (Throwable t) {
                    LogV3.log(t);
                }
            }
            releaseTemporaryProfile();
        }
    }

    private void invoke(final Target target, final int methodIndex, final Object... args) throws WebView2Exception {
        ensureAlive();
        final Integer hr = sta.call(new Callable<Integer>() {
            public Integer call() throws WebView2Exception {
                final Pointer object;
                switch (target) {
                case CONTROLLER:
                    object = controller;
                    break;
                case WEBVIEW:
                default:
                    object = webview;
                    break;
                }
                if (object == null) {
                    throw new WebView2Exception("WindowsWebView2Browser is disposed");
                }
                return Integer.valueOf(WebView2VTable.invokeHR(object, methodIndex, args));
            }
        });
        if (!WebView2VTable.succeeded(hr.intValue())) {
            throw new WebView2Exception("WebView2 call #" + methodIndex + " failed", hr.intValue());
        }
    }

    private String getString(final int methodIndex) throws WebView2Exception {
        ensureAlive();
        return sta.call(new Callable<String>() {
            public String call() {
                return webview != null ? WebView2VTable.getString(webview, methodIndex) : null;
            }
        });
    }

    private void ensureAlive() throws WebView2Exception {
        if (disposed || controller == null || webview == null) {
            throw new WebView2Exception("WindowsWebView2Browser is disposed or not ready");
        }
    }

    private static RectByValue rect(int x, int y, int width, int height) {
        final RectByValue r = new RectByValue();
        r.left = x;
        r.top = y;
        r.right = x + Math.max(1, width);
        r.bottom = y + Math.max(1, height);
        r.write();
        return r;
    }

    private String resolveUserDataFolder(Options opts) {
        File folder;
        if (StringUtils.isNotEmpty(opts.userDataFolder)) {
            folder = new File(opts.userDataFolder.trim());
        } else if (opts.persistentProfile) {
            folder = Application.getResource("cfg/webview2");
        } else {
            folder = Application.getTempResource("webview2");
        }
        if (!opts.persistentProfile) {
            final File base = folder;
            folder = new File(base, SESSION_PREFIX + UUID.randomUUID().toString());
            folder.mkdirs();
            temporaryProfile = folder;
            temporaryProfileLock = lockSession(folder);
            deleteLeftoverSessionsLater(base, folder);
            registerShutdownCleanup(folder);
        }
        if (!folder.exists()) {
            folder.mkdirs();
        }
        return folder.getAbsolutePath();
    }

    /**
     * Locks {@code session.lock} in a temporary profile for as long as this process uses it.
     *
     * @return the open lock file, or {@code null} if it could not be locked
     */
    private static RandomAccessFile lockSession(File session) {
        RandomAccessFile file = null;
        try {
            file = new RandomAccessFile(new File(session, SESSION_LOCK), "rw");
            if (file.getChannel().tryLock() != null) {
                return file;
            }
        } catch (Throwable t) {
            LogV3.log(t);
        }
        closeQuietly(file);
        return null;
    }

    private static void closeQuietly(RandomAccessFile file) {
        if (file != null) {
            try {
                file.close();
            } catch (Throwable ignored) {
                /* Nothing to do; the lock ends with the process at the latest. */
            }
        }
    }

    /**
     * A temporary profile cannot be deleted when the application exits or crashes while its browser is open. Those leftovers are
     * removed here, the next time a temporary profile is created in the same folder. A profile is a leftover when nobody holds its
     * lock file any more.
     */
    private static void deleteLeftoverSessionsLater(final File base, final File own) {
        final Thread thread = new Thread(new Runnable() {
            public void run() {
                final File[] sessions = base.listFiles();
                if (sessions == null) {
                    return;
                }
                for (File session : sessions) {
                    if (!session.isDirectory() || !session.getName().startsWith(SESSION_PREFIX) || session.equals(own)) {
                        continue;
                    }
                    if (isLeftoverSession(session)) {
                        try {
                            Files.deleteRecursive(session, false);
                        } catch (Throwable ignored) {
                            /* Tried again when the next temporary profile is created. */
                        }
                    }
                }
            }
        }, "WebView2-ProfileCleanup");
        thread.setDaemon(true);
        thread.start();
    }

    private static boolean isLeftoverSession(File session) {
        final File lock = new File(session, SESSION_LOCK);
        if (!lock.isFile()) {
            /* Either being created right now by another process, or left without a lock: only an old one is a leftover. */
            return System.currentTimeMillis() - session.lastModified() > TimeUnit.HOURS.toMillis(1);
        }
        RandomAccessFile file = null;
        try {
            file = new RandomAccessFile(lock, "rw");
            final FileLock acquired = file.getChannel().tryLock();
            if (acquired == null) {
                return false;
            }
            acquired.release();
            return true;
        } catch (Throwable inUse) {
            /* Locked by a browser of this process (OverlappingFileLockException) or not accessible: leave it alone. */
            return false;
        } finally {
            closeQuietly(file);
        }
    }

    /**
     * Deletes a non-persistent profile. The browser processes release their files a moment after the browser was closed, so the
     * deletion is retried for a while on a background thread. The shutdown hook stays registered until the profile is gone, so an
     * application that exits right after closing the browser still deletes it.
     */
    private static void deleteProfileLater(final File folder, final ShutdownEvent shutdownEvent) {
        final Thread thread = new Thread(new Runnable() {
            public void run() {
                final boolean deleted = deleteProfile(folder, 40);
                if (shutdownEvent != null) {
                    try {
                        ShutdownController.getInstance().removeShutdownEvent(shutdownEvent);
                    } catch (IllegalStateException shuttingDown) {
                        /* The shutdown has begun; the event runs and finds the profile already deleted. */
                    }
                }
                if (!deleted) {
                    LogV3.warning("WebView2: could not delete the temporary profile " + folder.getAbsolutePath());
                }
            }
        }, "WebView2-ProfileCleanup");
        thread.setDaemon(true);
        thread.start();
    }

    /**
     * Tries up to {@code attempts} times, half a second apart, to delete {@code folder}.
     *
     * @return {@code true} when the folder is gone
     */
    private static boolean deleteProfile(File folder, int attempts) {
        for (int attempt = 0; attempt < attempts && folder.exists(); attempt++) {
            try {
                Files.deleteRecursive(folder, false);
            } catch (Throwable ignored) {
                /* Still locked by the browser processes; try again. */
            }
            if (folder.exists()) {
                try {
                    Thread.sleep(500L);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    break;
                }
            }
        }
        return !folder.exists();
    }

    /**
     * An application that exits while a browser with a temporary profile is still open, or right after closing it, would leave that
     * profile behind. This hook closes the browser at shutdown and deletes the profile before the JVM ends.
     */
    private void registerShutdownCleanup(final File folder) {
        final ShutdownController controller = ShutdownController.getInstance();
        if (controller.isShuttingDown()) {
            return;
        }
        final ShutdownEvent event = new ShutdownEvent() {
            @Override
            public void onShutdown(ShutdownRequest shutdownRequest) {
                dispose();
                deleteProfile(folder, 10);
            }

            @Override
            public boolean supportsIllegalStateException() {
                /* Adding or removing while the shutdown has already begun is reported to us instead of being logged as an error. */
                return true;
            }

            @Override
            public String toString() {
                return "Close WebView2 browser and delete its temporary profile " + folder;
            }
        };
        try {
            controller.addShutdownEvent(event);
            shutdownCleanup = event;
        } catch (IllegalStateException shuttingDown) {
            /* The application is ending; the profile is removed as a leftover the next time. */
        }
    }

    /** Ends the use of a temporary profile: unlocks it and deletes it in the background; the shutdown hook ends with the deletion. */
    private void releaseTemporaryProfile() {
        final ShutdownEvent event = shutdownCleanup;
        shutdownCleanup = null;
        final File temporary = temporaryProfile;
        temporaryProfile = null;
        closeQuietly(temporaryProfileLock);
        temporaryProfileLock = null;
        if (temporary != null) {
            deleteProfileLater(temporary, event);
        }
    }

    /**
     * An http(s) address in the form the browser reports it for a request, so that two spellings of the same document compare equal:
     * scheme and host in lower case, a path of at least {@code /}, no fragment. {@code null} if not http(s).
     */
    private static String documentKey(String url) {
        if (url == null) {
            return null;
        }
        String rest = url.trim();
        final int fragment = rest.indexOf('#');
        if (fragment >= 0) {
            rest = rest.substring(0, fragment);
        }
        final String lower = rest.toLowerCase(Locale.ENGLISH);
        if (!lower.startsWith("http://") && !lower.startsWith("https://")) {
            return null;
        }
        final int authorityStart = lower.indexOf("://") + 3;
        int authorityEnd = rest.length();
        for (int i = authorityStart; i < rest.length(); i++) {
            final char c = rest.charAt(i);
            if (c == '/' || c == '?') {
                authorityEnd = i;
                break;
            }
        }
        String tail = rest.substring(authorityEnd);
        if (!tail.startsWith("/")) {
            tail = "/" + tail;
        }
        return lower.substring(0, authorityEnd) + tail;
    }
}
