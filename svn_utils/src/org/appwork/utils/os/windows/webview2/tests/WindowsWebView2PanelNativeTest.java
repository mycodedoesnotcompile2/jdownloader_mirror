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

import java.awt.BorderLayout;
import java.awt.GraphicsEnvironment;
import java.io.File;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import javax.swing.JFrame;
import javax.swing.SwingUtilities;

import org.appwork.testframework.AWTest;
import org.appwork.utils.Application;
import org.appwork.utils.Time;
import org.appwork.utils.os.windows.webview2.WindowsWebView2Browser;
import org.appwork.utils.os.windows.webview2.WindowsWebView2Panel;

/**
 * Opt-in native smoke test for the installed Edge WebView2 Runtime. Normal test runs stay headless; execute with
 * {@code -Dwebview2.nativeTest=true} on Windows to verify the panel end to end: COM creation, the queued first page, isolated
 * all-frame injection with the host bridge, pre-commit navigation cancellation, the request handler and the closed callback.
 */
public class WindowsWebView2PanelNativeTest extends AWTest {
    public static void main(String[] args) {
        AWTest.run();
    }

    @Override
    public void runTest() throws Exception {
        if (!Boolean.getBoolean("webview2.nativeTest") || GraphicsEnvironment.isHeadless() || !WindowsWebView2Panel.isAvailable()) {
            return;
        }
        final AtomicBoolean closed = new AtomicBoolean(false);
        final WindowsWebView2Browser.Options options = new WindowsWebView2Browser.Options();
        options.userDataFolder = Application.getResource("tmp/webview2-awtest").getAbsolutePath();
        options.persistentProfile = false;
        options.closedCallback = new Runnable() {
            public void run() {
                closed.set(true);
            }
        };
        final WindowsWebView2Panel panel = new WindowsWebView2Panel(options);
        final CountDownLatch frameMessages = new CountDownLatch(2);
        panel.addBrowserListener(new WindowsWebView2Browser.ListenerAdapter() {
            @Override
            public void onWebMessage(String sourceUrl, String message) {
                if (message != null && message.indexOf("webview2-test-frame") >= 0) {
                    frameMessages.countDown();
                }
            }

            @Override
            public void onHostScriptMessage(String frameUrl, boolean topFrame, String message) {
                /* Host scripts of the isolated world report through the private channel, from the top document and from the iframe. */
                if (message != null && message.indexOf("webview2-test-frame") >= 0) {
                    frameMessages.countDown();
                }
            }

            @Override
            public void onFrameMessage(String pageUrl, String frameOrigin, String message) {
                /* The iframe reports through the top document and arrives marked as a frame message. */
                if (message != null && message.indexOf("webview2-test-frame") >= 0 && message.indexOf("\"top\":false") >= 0) {
                    frameMessages.countDown();
                }
            }
        });
        final CountDownLatch ready = new CountDownLatch(1);
        final AtomicReference<Throwable> failure = new AtomicReference<Throwable>();
        panel.addReadyListener(new WindowsWebView2Panel.ReadyListener() {
            public void onBrowserReady(WindowsWebView2Browser browser) {
                ready.countDown();
            }

            public void onBrowserFailed(Throwable error) {
                failure.set(error);
                ready.countDown();
            }
        });
        /* Requested before the panel is visible: the page is queued and loaded once the browser exists. */
        panel.setHtml("<!doctype html><title>TestLoading</title><iframe srcdoc=\"<p>child</p>\" onload=\"document.title='TestReady'\"></iframe>");
        final AtomicReference<JFrame> frameRef = new AtomicReference<JFrame>();
        SwingUtilities.invokeAndWait(new Runnable() {
            public void run() {
                final JFrame frame = new JFrame("WebView2 AWTest");
                frame.setLayout(new BorderLayout());
                frame.add(panel, BorderLayout.CENTER);
                frame.setSize(520, 360);
                frame.setLocationRelativeTo(null);
                frame.setVisible(true);
                frameRef.set(frame);
            }
        });
        try {
            assertTrue(ready.await(90, TimeUnit.SECONDS));
            if (failure.get() != null) {
                throw new Exception("WebView2 could not be created", failure.get());
            }
            assertEquals(WindowsWebView2Panel.State.READY, panel.getState());
            final WindowsWebView2Browser browser = panel.getBrowser();
            awaitTitle(browser, "TestReady", TimeUnit.SECONDS.toMillis(15));
            /* The isolated world is invisible to the page. */
            assertEquals("set", browser.executeScript("window.__testSecret=41;'set';"));
            assertEquals("undefined", browser.executeScriptInMainWorld("typeof window.__testSecret"));
            browser.executeScriptInAllFrames("window.__awHostPost(JSON.stringify({type:'webview2-test-frame',top:window.top===window}));");
            assertTrue(frameMessages.await(10, TimeUnit.SECONDS));
            /* A cancelled navigation keeps the visible document. */
            final String sourceBefore = browser.getSource();
            final AtomicInteger intercepted = new AtomicInteger();
            browser.setNavigationInterceptor(new WindowsWebView2Browser.NavigationInterceptor() {
                public boolean intercept(String url) {
                    if (url != null && url.indexOf("blocked.invalid") >= 0) {
                        intercepted.incrementAndGet();
                        return true;
                    }
                    return false;
                }
            });
            browser.navigate("https://blocked.invalid/test");
            awaitCount(intercepted, TimeUnit.SECONDS.toMillis(10));
            assertEquals(sourceBefore, browser.getSource());
            assertEquals("TestReady", browser.getDocumentTitle());
            browser.setNavigationInterceptor(null);
            /* The request handler answers a document without any network access. */
            browser.setRequestHandler(new WindowsWebView2Browser.RequestHandler() {
                public WindowsWebView2Browser.Response handle(WindowsWebView2Browser.Request request) {
                    if (request.isDocument() && request.getUrl().indexOf("handled.invalid") >= 0) {
                        return new WindowsWebView2Browser.Response(200, "OK", "Content-Type: text/html; charset=utf-8\r\n", "<title>Handled</title>".getBytes(StandardCharsets.UTF_8));
                    }
                    return null;
                }
            });
            panel.navigate("https://handled.invalid/page");
            awaitTitle(browser, "Handled", TimeUnit.SECONDS.toMillis(15));
            panel.setHtml("<!doctype html><title>LastPage</title>");
            awaitTitle(browser, "LastPage", TimeUnit.SECONDS.toMillis(15));
            /* Removing the panel from its window closes the browser and deletes the throw-away profile. */
            final File profile = new File(browser.getUserDataFolder());
            assertTrue(profile.isDirectory());
            assertTrue(profile.getName().startsWith("session-"));
            SwingUtilities.invokeAndWait(new Runnable() {
                public void run() {
                    frameRef.get().remove(panel);
                    frameRef.get().validate();
                }
            });
            awaitDisposed(browser, TimeUnit.SECONDS.toMillis(15));
            /* isDisposed() turns true before the native browser is closed; the callback and the cleanup follow. */
            awaitDeleted(profile, TimeUnit.SECONDS.toMillis(30));
            assertTrue(closed.get());
            assertTrue(profile.getParentFile().isDirectory());
            /* Back in the window, a new browser loads the page that was requested last. */
            SwingUtilities.invokeAndWait(new Runnable() {
                public void run() {
                    frameRef.get().add(panel, BorderLayout.CENTER);
                    frameRef.get().validate();
                }
            });
            final WindowsWebView2Browser second = awaitBrowser(panel, TimeUnit.SECONDS.toMillis(90));
            assertTrue(second != browser);
            awaitTitle(second, "LastPage", TimeUnit.SECONDS.toMillis(15));
            final File secondProfile = new File(second.getUserDataFolder());
            assertFalse(secondProfile.equals(profile));
            panel.disposeBrowser();
            assertEquals(WindowsWebView2Panel.State.STOPPED, panel.getState());
            assertTrue(second.isDisposed());
            awaitDeleted(secondProfile, TimeUnit.SECONDS.toMillis(30));
            /* A page that a ready listener loads must not be replaced by the page that was queued before. */
            final WindowsWebView2Panel listenerPanel = new WindowsWebView2Panel(options);
            listenerPanel.setHtml("<!doctype html><title>Queued</title>");
            listenerPanel.addReadyListener(new WindowsWebView2Panel.ReadyListener() {
                public void onBrowserReady(WindowsWebView2Browser ready) {
                    try {
                        ready.setHtml("<!doctype html><title>FromReadyListener</title>");
                    } catch (Exception e) {
                        failure.set(e);
                    }
                }

                public void onBrowserFailed(Throwable error) {
                    failure.set(error);
                }
            });
            SwingUtilities.invokeAndWait(new Runnable() {
                public void run() {
                    frameRef.get().remove(panel);
                    frameRef.get().add(listenerPanel, BorderLayout.CENTER);
                    frameRef.get().validate();
                }
            });
            try {
                final WindowsWebView2Browser third = awaitBrowser(listenerPanel, TimeUnit.SECONDS.toMillis(90));
                awaitTitle(third, "FromReadyListener", TimeUnit.SECONDS.toMillis(15));
                Thread.sleep(1500L);
                assertEquals("FromReadyListener", third.getDocumentTitle());
                assertTrue(failure.get() == null);
            } finally {
                listenerPanel.disposeBrowser();
            }
            /* A panel that keeps its browser: removed from the window and added again, it shows the same browser and page. */
            final WindowsWebView2Panel keepPanel = new WindowsWebView2Panel(options);
            keepPanel.setDisposeBrowserOnRemove(false);
            keepPanel.setHtml("<!doctype html><title>Kept</title>");
            SwingUtilities.invokeAndWait(new Runnable() {
                public void run() {
                    frameRef.get().remove(listenerPanel);
                    frameRef.get().add(keepPanel, BorderLayout.CENTER);
                    frameRef.get().validate();
                }
            });
            try {
                final WindowsWebView2Browser kept = awaitBrowser(keepPanel, TimeUnit.SECONDS.toMillis(90));
                awaitTitle(kept, "Kept", TimeUnit.SECONDS.toMillis(15));
                assertEquals("marked", kept.executeScript("window.__keptMarker=7;'marked';"));
                SwingUtilities.invokeAndWait(new Runnable() {
                    public void run() {
                        frameRef.get().remove(keepPanel);
                        frameRef.get().validate();
                    }
                });
                Thread.sleep(500L);
                assertFalse(kept.isDisposed());
                SwingUtilities.invokeAndWait(new Runnable() {
                    public void run() {
                        frameRef.get().add(keepPanel, BorderLayout.CENTER);
                        frameRef.get().validate();
                    }
                });
                Thread.sleep(500L);
                assertTrue(keepPanel.getBrowser() == kept);
                /* Same document, not a reload: the variable of the isolated world is still there. */
                assertEquals("7", kept.executeScript("String(window.__keptMarker)"));
                assertEquals("Kept", kept.getDocumentTitle());
            } finally {
                keepPanel.disposeBrowser();
            }
        } finally {
            panel.disposeBrowser();
            SwingUtilities.invokeAndWait(new Runnable() {
                public void run() {
                    if (frameRef.get() != null) {
                        frameRef.get().dispose();
                    }
                }
            });
        }
    }

    private void awaitTitle(WindowsWebView2Browser browser, String expected, long timeout) throws Exception {
        final long end = Time.systemIndependentCurrentJVMTimeMillis() + timeout;
        while (Time.systemIndependentCurrentJVMTimeMillis() < end) {
            if (expected.equals(browser.getDocumentTitle())) {
                return;
            }
            Thread.sleep(50L);
        }
        assertEquals(expected, browser.getDocumentTitle());
    }

    private void awaitDisposed(WindowsWebView2Browser browser, long timeout) throws Exception {
        final long end = Time.systemIndependentCurrentJVMTimeMillis() + timeout;
        while (Time.systemIndependentCurrentJVMTimeMillis() < end && !browser.isDisposed()) {
            Thread.sleep(50L);
        }
        assertTrue(browser.isDisposed());
    }

    private void awaitDeleted(File folder, long timeout) throws Exception {
        final long end = Time.systemIndependentCurrentJVMTimeMillis() + timeout;
        while (Time.systemIndependentCurrentJVMTimeMillis() < end && folder.exists()) {
            Thread.sleep(100L);
        }
        assertFalse(folder.exists());
    }

    private WindowsWebView2Browser awaitBrowser(WindowsWebView2Panel panel, long timeout) throws Exception {
        final long end = Time.systemIndependentCurrentJVMTimeMillis() + timeout;
        while (Time.systemIndependentCurrentJVMTimeMillis() < end) {
            final WindowsWebView2Browser current = panel.getBrowser();
            if (current != null) {
                return current;
            }
            Thread.sleep(50L);
        }
        throw new Exception("No browser after the panel returned to the window: " + panel.getState() + " " + panel.getFailure());
    }

    private void awaitCount(AtomicInteger count, long timeout) throws Exception {
        final long end = Time.systemIndependentCurrentJVMTimeMillis() + timeout;
        while (Time.systemIndependentCurrentJVMTimeMillis() < end) {
            if (count.get() > 0) {
                return;
            }
            Thread.sleep(25L);
        }
        assertTrue(count.get() > 0);
    }
}
