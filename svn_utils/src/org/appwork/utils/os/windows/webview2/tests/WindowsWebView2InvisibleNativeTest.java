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
package org.appwork.utils.os.windows.webview2.tests;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.Inet4Address;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.util.Enumeration;
import java.util.List;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.testframework.AWTest;
import org.appwork.utils.Application;
import org.appwork.utils.IO;
import org.appwork.utils.Time;
import org.appwork.utils.net.httpconnection.HTTPProxy;
import org.appwork.utils.os.windows.webview2.WebView2Exception;
import org.appwork.utils.os.windows.webview2.WebView2FilterProxy;
import org.appwork.utils.os.windows.webview2.WebView2ProxySelector;
import org.appwork.utils.os.windows.webview2.WebView2ScriptException;
import org.appwork.utils.os.windows.webview2.WebView2UrlFilter;
import org.appwork.utils.os.windows.webview2.WindowsWebView2Browser;

/**
 * Opt-in native test of the invisible browser and of the security defaults against the installed Edge WebView2 Runtime. Execute with
 * {@code -Dwebview2.nativeTest=true} on Windows. No window is shown and no network is used: every page is served by a
 * {@link WindowsWebView2Browser.RequestHandler}.
 */
public class WindowsWebView2InvisibleNativeTest extends AWTest {
    private static final String                FRAME_PAGE = "<script>window.top.postMessage({type:'__awWebView2HostForward',payload:'from-frame'},'*');</script>";
    private final CopyOnWriteArrayList<String> blocked    = new CopyOnWriteArrayList<String>();
    private final CopyOnWriteArrayList<String> messages   = new CopyOnWriteArrayList<String>();
    private final CopyOnWriteArrayList<String> selected   = new CopyOnWriteArrayList<String>();

    public static void main(String[] args) {
        AWTest.run();
    }

    @Override
    public void runTest() throws Exception {
        if (!Boolean.getBoolean("webview2.nativeTest") || !WindowsWebView2Browser.isAvailable()) {
            return;
        }
        final WindowsWebView2Browser.Options options = new WindowsWebView2Browser.Options();
        options.userDataFolder = Application.getResource("tmp/webview2-awtest").getAbsolutePath();
        options.urlFilter = new WebView2UrlFilter().block(".blocked.example");
        options.allowedLocalHosts = new String[] { "192.168.77.77" };
        options.proxySelector = new WebView2ProxySelector() {
            public List<HTTPProxy> getProxiesByURL(String url) {
                selected.add(url);
                return null;
            }
        };
        final WindowsWebView2Browser browser = WindowsWebView2Browser.createInvisible(options);
        try {
            final AtomicReference<String> listenerScriptResult = new AtomicReference<String>();
            browser.addListener(new WindowsWebView2Browser.ListenerAdapter() {
                @Override
                public void onHostScriptMessage(String frameUrl, boolean topFrame, String message) {
                    messages.add("host|" + frameUrl + "|" + topFrame + "|" + message);
                }

                @Override
                public void onWebMessage(String sourceUrl, String message) {
                    messages.add("web|" + sourceUrl + "|" + message);
                }

                @Override
                public void onFrameMessage(String pageUrl, String frameOrigin, String message) {
                    messages.add("frame|" + pageUrl + "|" + frameOrigin + "|" + message);
                }

                @Override
                public void onRequestBlocked(String url, boolean document) {
                    blocked.add(url);
                }

                @Override
                public void onDocumentTitleChanged(String title) {
                    if ("FromScript".equals(title)) {
                        /* A listener may run a script; this used to end in a timeout. */
                        try {
                            listenerScriptResult.set(browser.executeScript("String(1+1)"));
                        } catch (Throwable e) {
                            listenerScriptResult.set("failed: " + e);
                        }
                    }
                }
            });
            browser.setRequestHandler(new WindowsWebView2Browser.RequestHandler() {
                public WindowsWebView2Browser.Response handle(WindowsWebView2Browser.Request request) {
                    final String url = request.getUrl();
                    if (url.startsWith("https://a.invalid/post")) {
                        /* Reads method and body through the COM slots of ICoreWebView2WebResourceRequest. */
                        final byte[] body = request.readBody(1024);
                        final String answer = request.getMethod() + ":" + (body != null ? new String(body) : "null");
                        return new WindowsWebView2Browser.Response(200, "OK", "Content-Type: text/plain\r\nAccess-Control-Allow-Origin: *\r\n", answer.getBytes(StandardCharsets.UTF_8));
                    }
                    if (url.startsWith("https://a.invalid/")) {
                        return html("<title>A</title><iframe src='https://b.invalid/frame'></iframe>");
                    }
                    if (url.startsWith("https://b.invalid/")) {
                        return html(FRAME_PAGE);
                    }
                    if (url.startsWith("http://192.168.77.77/inner")) {
                        return html("<title>Inner</title><script>parent.document.title='LocalInnerLoaded';</script>");
                    }
                    if (url.startsWith("http://192.168.77.77/")) {
                        return html("<title>Local</title><iframe src='http://192.168.77.77/inner'></iframe><iframe src='https://b.invalid/frame-in-local'></iframe>");
                    }
                    if (url.startsWith("http://plain.invalid/")) {
                        return html("<title>Plain</title>");
                    }
                    if (url.startsWith("https://allowed.example/")) {
                        return html("<title>Allowed</title>");
                    }
                    return null;
                }
            });
            testInvisiblePage(browser);
            testRequestMethodAndBody(browser);
            testFrameCannotSpeakAsPage(browser);
            testScriptErrorRunsOnce(browser);
            testScriptFromListener(browser, listenerScriptResult);
            testPermissionsDenied(browser);
            testExternalSchemesAndFilesBlocked(browser);
            testLocalNetworkBlocked(browser);
            testIframeOfLocalPageBlocked(browser);
            testFilterProxy(browser);
            testWebRtcSendsNoUdp(browser);
            testWebRtcPolicyInExistingProfile();
            testUrlFilter(browser);
            testFailsClosedWithoutFilter();
        } finally {
            WebView2FilterProxy.setResolver(null);
            browser.dispose();
        }
    }

    private static WindowsWebView2Browser.Response html(String html) {
        return new WindowsWebView2Browser.Response(200, "OK", "Content-Type: text/html; charset=utf-8\r\n", html.getBytes(StandardCharsets.UTF_8));
    }

    private void testInvisiblePage(WindowsWebView2Browser browser) throws Exception {
        browser.navigate("https://a.invalid/top");
        awaitTitle(browser, "A");
        assertEquals("A", browser.executeScript("document.title"));
        assertEquals("https://a.invalid/top", browser.getSource());
        /* 1280 physical pixels; the CSS width depends on the display scaling of the PC. */
        final int innerWidth = Integer.parseInt(browser.executeScript("String(window.innerWidth)"));
        assertTrue(innerWidth >= 400 && innerWidth <= 1280);
    }

    private void testRequestMethodAndBody(WindowsWebView2Browser browser) throws Exception {
        assertEquals("POST:hello-body", browser.executeScript("fetch('https://a.invalid/post',{method:'POST',body:'hello-body'}).then(function(r){return r.text();})"));
        assertEquals("GET:null", browser.executeScript("fetch('https://a.invalid/post').then(function(r){return r.text();})"));
    }

    private void testFrameCannotSpeakAsPage(WindowsWebView2Browser browser) throws Exception {
        /* The cross-origin iframe posted to the top document while the page loaded. */
        awaitContains(messages, "frame|https://a.invalid/top|https://b.invalid|from-frame");
        for (String message : messages) {
            assertFalse(message.startsWith("web|") && message.endsWith("from-frame"));
        }
        /* The page listens to window messages. A host script in the isolated world posts; the page must not see anything. */
        assertTrue(browser.hasHostScriptChannel());
        browser.executeScriptInMainWorld("window.__seen=0;window.addEventListener('message',function(){window.__seen++;});");
        assertEquals("true", browser.executeScript("String(window.__awHostPost('from-isolated'))"));
        awaitContains(messages, "host|https://a.invalid/top|true|from-isolated");
        assertEquals("0", browser.executeScriptInMainWorld("String(window.__seen)"));
        for (String message : messages) {
            assertFalse(message.startsWith("web|") && message.endsWith("from-isolated"));
        }
        /* The page itself can only post as a web message, never as a host script. */
        browser.executeScriptInMainWorld("window.__awHostPost('from-page');");
        awaitContains(messages, "web|https://a.invalid/top|from-page");
        assertEquals("undefined", browser.executeScriptInMainWorld("typeof window.__awWebView2Binding"));
        /* A script that runs when a document is created posts before the channel exists; the message is kept and delivered. */
        browser.addScriptOnDocumentCreated("window.__awHostPost('from-document-start');");
        browser.navigate("https://a.invalid/top?again");
        awaitContains(messages, "host|https://a.invalid/top?again|true|from-document-start");
        browser.navigate("https://a.invalid/top");
        awaitTitle(browser, "A");
    }

    private void testScriptErrorRunsOnce(WindowsWebView2Browser browser) throws Exception {
        try {
            browser.executeScript("window.__runs=(window.__runs||0)+1; throw new Error('boom');");
            throw new Exception("A script error must be reported");
        } catch (WebView2ScriptException expected) {
            assertTrue(expected.getMessage().contains("boom"));
        }
        assertEquals("1", browser.executeScript("String(window.__runs)"));
        /* The failed script did not leak into the page. */
        assertEquals("undefined", browser.executeScriptInMainWorld("typeof window.__runs"));
    }

    private void testScriptFromListener(WindowsWebView2Browser browser, AtomicReference<String> result) throws Exception {
        browser.executeScriptInMainWorld("document.title='FromScript';");
        final long end = Time.systemIndependentCurrentJVMTimeMillis() + TimeUnit.SECONDS.toMillis(10);
        while (Time.systemIndependentCurrentJVMTimeMillis() < end && result.get() == null) {
            Thread.sleep(25L);
        }
        assertEquals("2", result.get());
    }

    private void testPermissionsDenied(WindowsWebView2Browser browser) throws Exception {
        /* Without the deny handler WebView2 would show a prompt and this script would run into its timeout. */
        assertEquals("denied:1", browser.executeScript("new Promise(function(done){navigator.geolocation.getCurrentPosition(function(){done('granted');},function(e){done('denied:'+e.code);});})"));
        assertEquals("denied", browser.executeScript("Notification.requestPermission().then(function(p){return String(p);})"));
    }

    private void testExternalSchemesAndFilesBlocked(WindowsWebView2Browser browser) throws Exception {
        final String before = browser.getSource();
        browser.executeScriptInMainWorld("location.href='aw-test-scheme://start/something';");
        awaitContains(blocked, "aw-test-scheme://start/something");
        try {
            browser.navigate("file:///C:/Windows/win.ini");
        } catch (Exception cancelled) {
            /* WebView2 may report the cancelled navigation as a failed call. */
        }
        awaitContains(blocked, "file:///C:/Windows/win.ini");
        assertEquals(before, browser.getSource());
    }

    private void testLocalNetworkBlocked(WindowsWebView2Browser browser) throws Exception {
        browser.executeScriptInMainWorld("fetch('http://127.0.0.1:9666/flash',{mode:'no-cors'}).catch(function(){});new Image().src='http://192.168.1.1/admin.png';");
        awaitContains(blocked, "http://127.0.0.1:9666/flash");
        awaitContains(blocked, "http://192.168.1.1/admin.png");
    }

    /**
     * What WebView2 does not report as a request: WebSockets (also from a worker) and a public name that resolves to a local address.
     * A local server counts what reaches it.
     */
    /**
     * Nothing local is allowed automatically: the application itself cannot open a local address that is not in
     * {@code allowedLocalHosts}. A page of an allowed local host: its own iframe loads, an iframe of another host does not, because
     * that website could reach the local host. On the public page of {@link #testFrameCannotSpeakAsPage} the same iframe loads.
     */
    private void testIframeOfLocalPageBlocked(WindowsWebView2Browser browser) throws Exception {
        browser.navigate("http://192.168.77.78/not-allowed");
        awaitTitle(browser, "Blocked");
        assertTrue(blocked.contains("http://192.168.77.78/not-allowed"));
        browser.navigate("http://192.168.77.77/local");
        awaitTitle(browser, "LocalInnerLoaded");
        awaitContains(blocked, "https://b.invalid/frame-in-local");
        assertFalse(blocked.contains("http://192.168.77.77/inner"));
        browser.navigate("https://a.invalid/top");
        awaitTitle(browser, "A");
    }

    private void testFilterProxy(final WindowsWebView2Browser browser) throws Exception {
        assertTrue(browser.getFilterProxyPort() > 0);
        final CopyOnWriteArrayList<String> arrived = new CopyOnWriteArrayList<String>();
        final ServerSocket server = new ServerSocket(0, 50, InetAddress.getByName("127.0.0.1"));
        final Thread accept = new Thread(new Runnable() {
            public void run() {
                while (true) {
                    try {
                        final Socket socket = server.accept();
                        arrived.add(String.valueOf(new BufferedReader(new InputStreamReader(socket.getInputStream(), "ISO-8859-1")).readLine()));
                        socket.close();
                    } catch (Exception closed) {
                        return;
                    }
                }
            }
        }, "WebView2-Test-LocalServer");
        accept.setDaemon(true);
        accept.start();
        final int port = server.getLocalPort();
        try {
            /* A name that looks public but points to this PC, as in a DNS rebinding attack. */
            WebView2FilterProxy.setResolver(new WebView2FilterProxy.Resolver() {
                public InetAddress[] resolve(String host) throws IOException {
                    if ("rebind.example".equals(host)) {
                        return new InetAddress[] { InetAddress.getByName("127.0.0.1") };
                    }
                    return InetAddress.getAllByName(host);
                }
            });
            /* An https page may open ws:// only to loopback; a plain http page may open it to any name. Try both. */
            browser.executeScriptInMainWorld("try{new WebSocket('ws://127.0.0.1:" + port + "/ws-literal');}catch(e){}" + "try{new Worker(URL.createObjectURL(new Blob([\"try{new WebSocket('ws://127.0.0.1:" + port + "/ws-worker');}catch(e){}\"])));}catch(e){}");
            awaitContains(blocked, "https://127.0.0.1:" + port + "/");
            browser.navigate("http://plain.invalid/page");
            awaitTitle(browser, "Plain");
            browser.executeScriptInMainWorld("try{new WebSocket('ws://rebind.example:" + port + "/ws-name');}catch(e){}" + "fetch('http://rebind.example:" + port + "/fetch-name',{mode:'no-cors'}).catch(function(){});" + "try{new WebSocket('ws://127.0.0.1:" + port + "/ws-literal-plain');}catch(e){}" + "try{new WebSocket('wss://ads.blocked.example/ws-filtered');}catch(e){}");
            /* The filter knows host and port only: WebSocket and fetch to rebind.example are reported with the same target. */
            awaitContains(blocked, "https://rebind.example:" + port + "/");
            awaitContains(blocked, "https://ads.blocked.example:443/");
            Thread.sleep(1500L);
            if (!arrived.isEmpty()) {
                throw new Exception("A website reached the local server: " + arrived);
            }
            /* Another program on this PC (here: this JVM) is not served by the filter: it gets no SOCKS answer, only a closed connection. */
            final Socket foreign = new Socket("127.0.0.1", browser.getFilterProxyPort());
            try {
                foreign.setSoTimeout((int) TimeUnit.SECONDS.toMillis(10));
                foreign.getOutputStream().write(new byte[] { 0x05, 0x01, 0x00 });
                foreign.getOutputStream().flush();
                int answer;
                try {
                    answer = foreign.getInputStream().read();
                } catch (SocketException reset) {
                    /* Closed with the greeting unread. */
                    answer = -1;
                }
                assertEquals(-1, answer);
            } finally {
                foreign.close();
            }
            if (Boolean.getBoolean("webview2.nativeTest.network")) {
                /* Real traffic passes the filter and the proxy selector is asked for it. */
                browser.navigate("https://example.com/");
                awaitTitle(browser, "Example Domain");
                assertTrue(selected.contains("https://example.com:443/"));
            }
            browser.navigate("https://a.invalid/top");
            awaitTitle(browser, "A");
        } finally {
            server.close();
        }
    }

    /**
     * WebRTC must not send UDP: it would bypass the filter (local network, real address behind the upstream proxy). A page asks a
     * STUN server on this PC (its LAN address if it has one) for its address; with the filter nothing may arrive there. A browser
     * without the filter proxy (and so without the WebRTC policy) is the counter test: its STUN request must arrive, otherwise this
     * test could not see UDP at all and would prove nothing.
     */
    private void testWebRtcSendsNoUdp(WindowsWebView2Browser browser) throws Exception {
        final InetAddress address = stunAddress();
        final DatagramSocket stun = new DatagramSocket(0, address);
        try {
            browser.executeScriptInMainWorld(webRtcScript(address, stun.getLocalPort()));
            if (receivesUdp(stun, TimeUnit.SECONDS.toMillis(5))) {
                throw new Exception("WebRTC of the filtered browser sent UDP to " + address.getHostAddress());
            }
        } finally {
            stun.close();
        }
        final WindowsWebView2Browser.Options options = new WindowsWebView2Browser.Options();
        options.userDataFolder = Application.getResource("tmp/webview2-awtest-unfiltered").getAbsolutePath();
        options.filterProxy = false;
        final WindowsWebView2Browser unfiltered = WindowsWebView2Browser.createInvisible(options);
        final DatagramSocket control = new DatagramSocket(0, address);
        try {
            unfiltered.setRequestHandler(new WindowsWebView2Browser.RequestHandler() {
                public WindowsWebView2Browser.Response handle(WindowsWebView2Browser.Request request) {
                    return html("<title>Rtc</title>");
                }
            });
            unfiltered.navigate("https://rtc.invalid/");
            awaitTitle(unfiltered, "Rtc");
            unfiltered.executeScriptInMainWorld(webRtcScript(address, control.getLocalPort()));
            if (!receivesUdp(control, TimeUnit.SECONDS.toMillis(10))) {
                throw new Exception("Counter test failed: a browser without the filter sent no STUN request to " + address.getHostAddress() + ", so the WebRTC test proves nothing");
            }
        } finally {
            control.close();
            unfiltered.dispose();
        }
    }

    /**
     * A persistent profile whose {@code Preferences} allow WebRTC UDP: the setting is changed before the start, the rest of the file is
     * kept, and no UDP is sent.
     */
    private void testWebRtcPolicyInExistingProfile() throws Exception {
        final File folder = Application.getResource("tmp/webview2-awtest-persistent");
        final File preferences = new File(folder, "EBWebView/Default/Preferences");
        preferences.getParentFile().mkdirs();
        IO.secureWrite(preferences, "{\"aw_test_marker\":\"kept\",\"webrtc\":{\"ip_handling_policy\":\"default\"}}".getBytes("UTF-8"));
        final WindowsWebView2Browser.Options options = new WindowsWebView2Browser.Options();
        options.userDataFolder = folder.getAbsolutePath();
        options.persistentProfile = true;
        final WindowsWebView2Browser persistent = WindowsWebView2Browser.createInvisible(options);
        final InetAddress address = stunAddress();
        final DatagramSocket stun = new DatagramSocket(0, address);
        try {
            final String written = IO.readFileToString(preferences);
            assertTrue(written.contains("aw_test_marker"));
            assertTrue(written.contains("disable_non_proxied_udp"));
            persistent.setRequestHandler(new WindowsWebView2Browser.RequestHandler() {
                public WindowsWebView2Browser.Response handle(WindowsWebView2Browser.Request request) {
                    return html("<title>Rtc</title>");
                }
            });
            persistent.navigate("https://rtc.invalid/");
            awaitTitle(persistent, "Rtc");
            persistent.executeScriptInMainWorld(webRtcScript(address, stun.getLocalPort()));
            if (receivesUdp(stun, TimeUnit.SECONDS.toMillis(5))) {
                throw new Exception("WebRTC of a browser with an existing profile sent UDP to " + address.getHostAddress());
            }
        } finally {
            stun.close();
            persistent.dispose();
        }
    }

    private static String webRtcScript(InetAddress address, int port) {
        return "window.__awPc=new RTCPeerConnection({iceServers:[{urls:'stun:" + address.getHostAddress() + ":" + port + "'}]});" + "window.__awPc.createDataChannel('aw');" + "window.__awPc.createOffer().then(function(o){return window.__awPc.setLocalDescription(o);}).catch(function(){});void 0;";
    }

    /** An IPv4 address of this PC in the local network, or loopback when there is none. */
    private static InetAddress stunAddress() throws IOException {
        final Enumeration<NetworkInterface> interfaces = NetworkInterface.getNetworkInterfaces();
        while (interfaces != null && interfaces.hasMoreElements()) {
            final NetworkInterface networkInterface = interfaces.nextElement();
            if (!networkInterface.isUp() || networkInterface.isLoopback()) {
                continue;
            }
            final Enumeration<InetAddress> addresses = networkInterface.getInetAddresses();
            while (addresses.hasMoreElements()) {
                final InetAddress candidate = addresses.nextElement();
                if (candidate instanceof Inet4Address && candidate.isSiteLocalAddress()) {
                    return candidate;
                }
            }
        }
        return InetAddress.getByName("127.0.0.1");
    }

    private static boolean receivesUdp(DatagramSocket socket, long timeoutMillis) throws IOException {
        socket.setSoTimeout((int) timeoutMillis);
        try {
            socket.receive(new DatagramPacket(new byte[2048], 2048));
            return true;
        } catch (SocketTimeoutException nothing) {
            return false;
        }
    }

    /** A browser that does not send its traffic through the filter must not be started at all. */
    private void testFailsClosedWithoutFilter() throws Exception {
        System.setProperty(WebView2FilterProxy.TEST_OMIT_ARGUMENTS, "true");
        try {
            final WindowsWebView2Browser.Options options = new WindowsWebView2Browser.Options();
            options.userDataFolder = Application.getResource("tmp/webview2-awtest").getAbsolutePath();
            WindowsWebView2Browser unfiltered = null;
            try {
                unfiltered = WindowsWebView2Browser.createInvisible(options);
                throw new Exception("A browser without the filter proxy was started");
            } catch (WebView2Exception expected) {
                assertTrue(expected.getMessage().contains("filter proxy"));
            } finally {
                if (unfiltered != null) {
                    unfiltered.dispose();
                }
            }
        } finally {
            System.clearProperty(WebView2FilterProxy.TEST_OMIT_ARGUMENTS);
        }
    }

    private void testUrlFilter(WindowsWebView2Browser browser) throws Exception {
        browser.executeScriptInMainWorld("new Image().src='https://ads.blocked.example/pixel.png';");
        awaitContains(blocked, "https://ads.blocked.example/pixel.png");
        browser.setUrlFilter(new WebView2UrlFilter().allow("allowed.example"));
        browser.navigate("https://allowed.example/page");
        awaitTitle(browser, "Allowed");
        browser.navigate("https://a.invalid/top");
        awaitTitle(browser, "Blocked");
        assertTrue(blocked.contains("https://a.invalid/top"));
    }

    private void awaitTitle(WindowsWebView2Browser browser, String expected) throws Exception {
        final long end = Time.systemIndependentCurrentJVMTimeMillis() + TimeUnit.SECONDS.toMillis(15);
        while (Time.systemIndependentCurrentJVMTimeMillis() < end) {
            if (expected.equals(browser.getDocumentTitle())) {
                return;
            }
            Thread.sleep(50L);
        }
        assertEquals(expected, browser.getDocumentTitle());
    }

    private void awaitContains(CopyOnWriteArrayList<String> list, String expected) throws Exception {
        final long end = Time.systemIndependentCurrentJVMTimeMillis() + TimeUnit.SECONDS.toMillis(15);
        while (Time.systemIndependentCurrentJVMTimeMillis() < end && !list.contains(expected)) {
            Thread.sleep(25L);
        }
        if (!list.contains(expected)) {
            throw new Exception("Missing " + expected + " in " + list);
        }
    }
}
