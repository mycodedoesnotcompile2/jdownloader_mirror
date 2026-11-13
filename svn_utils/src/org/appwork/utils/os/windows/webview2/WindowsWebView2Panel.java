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

import java.awt.BorderLayout;
import java.awt.Canvas;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import org.appwork.loggingv3.LogV3;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.WinDef.HWND;

/**
 * Swing panel that shows a web page with Microsoft Edge WebView2. Windows only; needs the Edge WebView2 Runtime, see
 * {@link #isAvailable()}.
 *
 * <pre>
 * WindowsWebView2Panel browser = new WindowsWebView2Panel();
 * frame.add(browser, BorderLayout.CENTER);
 * browser.navigate("https://example.com/");
 * </pre>
 *
 * The native browser is created in the background, as soon as a page was requested ({@link #navigate(String)}, {@link #setHtml(String)}
 * or {@link #present()}) and the panel is showing. A page requested earlier is loaded once the browser is ready.
 * <p>
 * The browser is closed automatically when the panel is removed from its window or the window is disposed, so nothing has to be cleaned
 * up by hand. When the panel is added to a window again, a new browser loads the page that was requested last. See
 * {@link #setDisposeBrowserOnRemove(boolean)}.
 * <p>
 * Profile folder, persistent or throw-away profile, debug mode (DevTools) and the right-click menu are set through
 * {@link WindowsWebView2Browser.Options}. By default the profile is thrown away when the browser is closed, and debug mode and the
 * right-click menu are off.
 * <p>
 * All methods may be called from any thread, also from the EDT: there they never wait for the browser. A call on the EDT that has to
 * talk to the browser ({@link #navigate(String)}, {@link #setHtml(String)}, {@link #disposeBrowser()}) is handed to a background
 * thread and returns at once; a failure is then logged instead of thrown. The page is drawn by a heavyweight component, so lightweight
 * Swing components cannot be painted on top of it.
 */
public class WindowsWebView2Panel extends JPanel {
    public static enum State {
        /** No browser exists and none is being created. */
        STOPPED,
        STARTING,
        READY,
        /** Creation failed, see {@link WindowsWebView2Panel#getFailure()}. {@link WindowsWebView2Panel#retry()} tries again. */
        FAILED
    }

    /** Both methods are always called on a background thread, never on the EDT and never on the thread that added the listener. */
    public interface ReadyListener {
        /** The browser is ready and the browser listeners are attached. A page that was requested before is loaded after this call. */
        void onBrowserReady(WindowsWebView2Browser browser);

        /** The browser could not be created. */
        void onBrowserFailed(Throwable error);
    }

    private static final long                                           serialVersionUID = 1L;
    private static final int                                            MIN_CREATE_SIZE  = 50;
    private final Canvas                                                hostCanvas;
    private final WindowsWebView2Browser.Options                        options;
    private final CopyOnWriteArrayList<WindowsWebView2Browser.Listener> browserListeners = new CopyOnWriteArrayList<WindowsWebView2Browser.Listener>();
    private final CopyOnWriteArrayList<ReadyListener>                   readyListeners   = new CopyOnWriteArrayList<ReadyListener>();
    private final Object                                                lifecycleLock    = new Object();
    /** Runs, in order, what must not run on the EDT: calls that wait for the browser and listener notifications. */
    private final ExecutorService                                       background       = createBackground();
    private volatile WindowsWebView2Browser                             browser;
    private volatile Throwable                                          failure;
    private volatile State                                              state            = State.STOPPED;
    private boolean                                                     creating;
    private int                                                         generation;
    private PendingPage                                                 pendingPage;
    /** The page that was requested last; loaded again when the panel returns to a window after its browser was closed. */
    private PendingPage                                                 lastPage;
    private volatile boolean                                            disposeBrowserOnRemove = true;
    /** Only used on the EDT. */
    private Frame                                                       parkingFrame;
    /**
     * Set by {@link #present()} when a caller asked for a browser. Geometry listeners must not create one before that, or merely showing
     * the panel would start the browser.
     */
    private volatile boolean                                            startRequested;

    public WindowsWebView2Panel() {
        this(null);
    }

    /**
     * @param options
     *            {@code null} for the defaults
     */
    public WindowsWebView2Panel(WindowsWebView2Browser.Options options) {
        super(new BorderLayout());
        this.options = options != null ? options : new WindowsWebView2Browser.Options();
        setBackground(Color.WHITE);
        hostCanvas = new Canvas() {
            private static final long serialVersionUID = 1L;

            @Override
            public Dimension getPreferredSize() {
                return new Dimension(800, 600);
            }

            @Override
            public Dimension getMinimumSize() {
                return new Dimension(MIN_CREATE_SIZE, MIN_CREATE_SIZE);
            }
        };
        hostCanvas.setBackground(Color.WHITE);
        hostCanvas.setFocusable(false);
        add(hostCanvas, BorderLayout.CENTER);
        hostCanvas.addComponentListener(new ComponentAdapter() {
            @Override
            public void componentResized(ComponentEvent event) {
                presentIfRequested();
            }

            @Override
            public void componentShown(ComponentEvent event) {
                presentIfRequested();
            }

            @Override
            public void componentMoved(ComponentEvent event) {
                notifyParentMoved();
            }
        });
        hostCanvas.addHierarchyListener(new HierarchyListener() {
            public void hierarchyChanged(HierarchyEvent event) {
                if ((event.getChangeFlags() & (HierarchyEvent.DISPLAYABILITY_CHANGED | HierarchyEvent.SHOWING_CHANGED)) != 0) {
                    presentIfRequested();
                }
            }
        });
    }

    /**
     * Whether WebView2 can be used on this PC: Windows with an installed Edge WebView2 Runtime. The first call reads the registry and
     * the disk; do not make that first call on the EDT.
     */
    public static boolean isAvailable() {
        return WindowsWebView2Browser.isAvailable();
    }

    private static ExecutorService createBackground() {
        final ThreadPoolExecutor executor = new ThreadPoolExecutor(1, 1, 30L, TimeUnit.SECONDS, new LinkedBlockingQueue<Runnable>(), new ThreadFactory() {
            public Thread newThread(Runnable r) {
                final Thread thread = new Thread(r, "WebView2-Panel");
                thread.setDaemon(true);
                return thread;
            }
        });
        executor.allowCoreThreadTimeOut(true);
        return executor;
    }

    public boolean isDisposeBrowserOnRemove() {
        return disposeBrowserOnRemove;
    }

    /**
     * {@code true} (default): the browser is closed when the panel is removed from its window or the window is disposed.
     * {@code false}: the browser is kept. While the panel is in no window the browser is parked invisibly, and it is moved into the
     * panel again when the panel is added to a window. The owner must call {@link #disposeBrowser()} itself. Parking the browser is
     * the one operation that waits for the browser on the EDT, because it has to be finished before the native window of the panel
     * is destroyed.
     */
    public void setDisposeBrowserOnRemove(boolean disposeBrowserOnRemove) {
        this.disposeBrowserOnRemove = disposeBrowserOnRemove;
    }

    /** A frame that is never shown. Its native window holds a kept browser while the panel is in no window. */
    private Frame parkingWindow() {
        Frame frame = parkingFrame;
        if (frame == null) {
            frame = new Frame();
            frame.setUndecorated(true);
            frame.setFocusableWindowState(false);
            /* Creates the native window without showing it. */
            frame.addNotify();
            parkingFrame = frame;
        }
        return frame;
    }

    @Override
    public void addNotify() {
        super.addNotify();
        final WindowsWebView2Browser current = getBrowser();
        if (current != null && hostCanvas.isDisplayable()) {
            /* The canvas has a new native window; move the kept browser into it. */
            final Pointer peer = Native.getComponentPointer(hostCanvas);
            if (peer != null) {
                current.setParentWindow(new HWND(peer));
                syncBounds(current);
            }
        }
    }

    @Override
    public void removeNotify() {
        if (!disposeBrowserOnRemove) {
            final WindowsWebView2Browser current = getBrowser();
            if (current != null) {
                try {
                    /* Must be done before super.removeNotify() destroys the native window that is the parent of the browser. */
                    final Pointer parking = Native.getComponentPointer(parkingWindow());
                    if (parking == null) {
                        throw new WebView2Exception("No parking window");
                    }
                    current.parkInWindow(new HWND(parking));
                } catch (Throwable error) {
                    /* The browser cannot be kept; it ends with the native window. The next requested page creates a new one. */
                    LogV3.log(error);
                    disposeBrowser(true);
                }
            }
            super.removeNotify();
            return;
        }
        super.removeNotify();
        /* Closing the browser waits for its native thread; keep that wait off the EDT. */
        final Thread thread = new Thread(new Runnable() {
            public void run() {
                disposeBrowser(true);
                /* The panel may already be back in a window (it was only moved): start again right away. */
                presentIfRequested();
            }
        }, "WebView2-Dispose");
        thread.setDaemon(true);
        thread.start();
    }

    /** The heavyweight component that the page is drawn in. */
    public Component getHostComponent() {
        return hostCanvas;
    }

    public final State getState() {
        return state;
    }

    /** Why the browser could not be created, {@code null} unless the state is {@link State#FAILED}. */
    public final Throwable getFailure() {
        return failure;
    }

    /** The listener is attached to the current browser and to every browser this panel creates later. */
    public final void addBrowserListener(WindowsWebView2Browser.Listener listener) {
        if (listener == null) {
            return;
        }
        synchronized (lifecycleLock) {
            browserListeners.addIfAbsent(listener);
            final WindowsWebView2Browser current = getBrowserLocked();
            if (current != null) {
                current.addListener(listener);
            }
        }
    }

    public final void removeBrowserListener(WindowsWebView2Browser.Listener listener) {
        if (listener == null) {
            return;
        }
        synchronized (lifecycleLock) {
            browserListeners.remove(listener);
            final WindowsWebView2Browser current = getBrowserLocked();
            if (current != null) {
                current.removeListener(listener);
            }
        }
    }

    /** A listener added while the browser is ready (or failed) is told so at once. */
    public final void addReadyListener(ReadyListener listener) {
        if (listener == null) {
            return;
        }
        final WindowsWebView2Browser current;
        final Throwable currentFailure;
        synchronized (lifecycleLock) {
            readyListeners.addIfAbsent(listener);
            current = getBrowserLocked();
            currentFailure = failure;
        }
        if (current == null && currentFailure == null) {
            return;
        }
        /* Never on the thread of the caller, which may be the EDT. */
        background.execute(new Runnable() {
            public void run() {
                if (!readyListeners.contains(listener)) {
                    return;
                }
                if (current != null) {
                    if (isCurrent(current)) {
                        notifyReady(listener, current);
                    }
                } else {
                    notifyFailed(listener, currentFailure);
                }
            }
        });
    }

    public final void removeReadyListener(ReadyListener listener) {
        readyListeners.remove(listener);
    }

    /** @return the browser, or {@code null} while it is not created yet */
    public final WindowsWebView2Browser getBrowser() {
        synchronized (lifecycleLock) {
            return getBrowserLocked();
        }
    }

    private WindowsWebView2Browser getBrowserLocked() {
        if (browser != null && browser.isDisposed()) {
            browser = null;
            if (!creating) {
                state = State.STOPPED;
            }
        }
        return browser;
    }

    public final void navigate(String url) throws WebView2Exception {
        navigate(url, null);
    }

    /**
     * Loads {@code url}, now or as soon as the browser is ready.
     *
     * @param referrer
     *            http(s) address sent as {@code Referer}, or {@code null}
     */
    public final void navigate(final String url, final String referrer) throws WebView2Exception {
        if (url == null) {
            throw new IllegalArgumentException("url is null");
        }
        final WindowsWebView2Browser current = getBrowserOrQueue(new PendingPage(url, null, referrer));
        if (current == null) {
            return;
        }
        if (SwingUtilities.isEventDispatchThread()) {
            background.execute(new Runnable() {
                public void run() {
                    try {
                        current.navigate(url, referrer);
                    } catch (Throwable error) {
                        LogV3.log(error);
                    }
                }
            });
        } else {
            current.navigate(url, referrer);
        }
    }

    /** Shows {@code html} as the document, now or as soon as the browser is ready. */
    public final void setHtml(String html) throws WebView2Exception {
        final String document = html != null ? html : "";
        final WindowsWebView2Browser current = getBrowserOrQueue(new PendingPage(null, document, null));
        if (current == null) {
            return;
        }
        if (SwingUtilities.isEventDispatchThread()) {
            background.execute(new Runnable() {
                public void run() {
                    try {
                        current.setHtml(document);
                    } catch (Throwable error) {
                        LogV3.log(error);
                    }
                }
            });
        } else {
            current.setHtml(document);
        }
    }

    private WindowsWebView2Browser getBrowserOrQueue(PendingPage page) {
        synchronized (lifecycleLock) {
            lastPage = page;
            final WindowsWebView2Browser current = getBrowserLocked();
            if (current != null) {
                return current;
            }
            pendingPage = page;
        }
        present();
        return null;
    }

    /** Creates the browser if there is none yet, otherwise fits it to the current size of the panel. */
    public final void present() {
        final WindowsWebView2Browser current = getBrowser();
        if (current != null) {
            syncBounds(current);
            return;
        }
        startRequested = true;
        createAsync();
    }

    /**
     * Geometry callback. Syncs an existing browser, or finishes a start that {@link #present()} already asked for once the panel has a
     * size. Does not start a browser on its own.
     */
    private void presentIfRequested() {
        if (getBrowser() != null || startRequested) {
            present();
        }
    }

    /** Clears a previous startup failure and tries to create the browser again. */
    public final void retry() {
        synchronized (lifecycleLock) {
            switch (state) {
            case FAILED:
                failure = null;
                state = State.STOPPED;
                break;
            case READY:
            case STARTING:
            case STOPPED:
            default:
                break;
            }
        }
        present();
    }

    /** Closes the browser. The panel can be used again afterwards; the next requested page creates a new browser. */
    public final void disposeBrowser() {
        disposeBrowser(false);
    }

    /**
     * @param restartWhenShown
     *            {@code true} keeps the page that was requested last, so a new browser loads it once the panel is showing again
     */
    private void disposeBrowser(boolean restartWhenShown) {
        final WindowsWebView2Browser current;
        synchronized (lifecycleLock) {
            generation++;
            current = browser;
            browser = null;
            failure = null;
            state = State.STOPPED;
            creating = false;
            if (restartWhenShown && lastPage != null) {
                pendingPage = lastPage;
                startRequested = true;
            } else {
                pendingPage = null;
                lastPage = null;
                startRequested = false;
            }
        }
        if (current != null) {
            if (SwingUtilities.isEventDispatchThread()) {
                /* Closing waits for the browser thread. */
                background.execute(new Runnable() {
                    public void run() {
                        disposeCreated(current);
                    }
                });
            } else {
                disposeCreated(current);
            }
        }
        releaseParkingWindow();
        notifyStateChanged(State.STOPPED, null);
    }

    /** The parking window is only needed while a browser is kept. A native window that stays would keep AWT, and so the JVM, alive. */
    private void releaseParkingWindow() {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                final Frame frame = parkingFrame;
                if (frame != null && getBrowser() == null) {
                    parkingFrame = null;
                    frame.dispose();
                }
            }
        });
    }

    private void createAsync() {
        final int createGeneration;
        synchronized (lifecycleLock) {
            if (getBrowserLocked() != null || creating || failure != null || !isCreationReady()) {
                return;
            }
            creating = true;
            state = State.STARTING;
            createGeneration = ++generation;
        }
        notifyStateChanged(State.STARTING, null);
        final Thread thread = new Thread(new Runnable() {
            public void run() {
                create(createGeneration);
            }
        }, "WebView2-Create");
        thread.setDaemon(true);
        thread.start();
    }

    private boolean isCreationReady() {
        /* isShowing is false while the panel sits in a tab that is not selected, so restoring such a tab does not launch WebView2. */
        return hostCanvas.isShowing() && hostCanvas.getWidth() >= MIN_CREATE_SIZE && hostCanvas.getHeight() >= MIN_CREATE_SIZE;
    }

    private void create(int createGeneration) {
        final WindowsWebView2Browser created;
        try {
            created = WindowsWebView2Browser.create(hostCanvas, options);
        } catch (Throwable error) {
            handleCreationFailure(createGeneration, error);
            return;
        }
        boolean stale;
        synchronized (lifecycleLock) {
            stale = createGeneration != generation;
        }
        if (stale) {
            disposeCreated(created);
            return;
        }
        syncBounds(created);
        ReadyListener[] notify = null;
        PendingPage page = null;
        synchronized (lifecycleLock) {
            stale = createGeneration != generation;
            if (!stale) {
                for (WindowsWebView2Browser.Listener listener : browserListeners) {
                    created.addListener(listener);
                }
                browser = created;
                creating = false;
                failure = null;
                state = State.READY;
                page = pendingPage;
                pendingPage = null;
                notify = readyListeners.toArray(new ReadyListener[0]);
            }
        }
        if (stale) {
            disposeCreated(created);
            return;
        }
        if (!isCurrent(created)) {
            return;
        }
        notifyStateChanged(State.READY, null);
        /* Ready listeners run before the queued page is loaded, so they can install filters and scripts for it. */
        final int requestsBefore = created.getNavigationRequestCount();
        for (ReadyListener listener : notify) {
            if (!isCurrent(created)) {
                return;
            }
            notifyReady(listener, created);
        }
        if (!isCurrent(created)) {
            return;
        }
        if (created.getNavigationRequestCount() != requestsBefore) {
            /* A ready listener (or another thread) already asked for a newer page; the queued one is outdated. */
            return;
        }
        try {
            if (page != null && page.html != null) {
                created.setHtml(page.html);
            } else if (page != null && page.url != null) {
                created.navigate(page.url, page.referrer);
            }
        } catch (WebView2Exception error) {
            LogV3.log(error);
        }
    }

    private boolean isCurrent(WindowsWebView2Browser current) {
        synchronized (lifecycleLock) {
            return browser == current && !current.isDisposed();
        }
    }

    private static void disposeCreated(WindowsWebView2Browser created) {
        try {
            created.dispose();
        } catch (Throwable error) {
            LogV3.log(error);
        }
    }

    private void handleCreationFailure(int createGeneration, Throwable error) {
        final ReadyListener[] notify;
        synchronized (lifecycleLock) {
            if (createGeneration != generation) {
                return;
            }
            failure = error;
            creating = false;
            state = State.FAILED;
            notify = readyListeners.toArray(new ReadyListener[0]);
        }
        LogV3.log(error);
        notifyStateChanged(State.FAILED, error);
        for (ReadyListener listener : notify) {
            notifyFailed(listener, error);
        }
    }

    private static void notifyReady(ReadyListener listener, WindowsWebView2Browser browser) {
        try {
            listener.onBrowserReady(browser);
        } catch (Throwable error) {
            LogV3.log(error);
        }
    }

    private static void notifyFailed(ReadyListener listener, Throwable error) {
        try {
            listener.onBrowserFailed(error);
        } catch (Throwable callbackError) {
            LogV3.log(callbackError);
        }
    }

    private void notifyStateChanged(State newState, Throwable error) {
        try {
            onStateChanged(newState, error);
        } catch (Throwable callbackError) {
            LogV3.log(callbackError);
        }
    }

    /**
     * Hook for subclasses, e.g. to show a loading or error message. May be called on any thread.
     *
     * @param error
     *            the failure for {@link State#FAILED}, otherwise {@code null}
     */
    protected void onStateChanged(State newState, Throwable error) {
    }

    private void syncBounds(WindowsWebView2Browser current) {
        if (current == null || !hostCanvas.isDisplayable()) {
            return;
        }
        try {
            final Pointer peer = Native.getComponentPointer(hostCanvas);
            if (peer == null) {
                return;
            }
            /* Does not wait for the browser thread: this runs on the EDT for every resize. */
            current.setBoundsToParentClientArea(new HWND(peer), hostCanvas.isShowing());
        } catch (Throwable error) {
            LogV3.log(error);
        }
    }

    private void notifyParentMoved() {
        final WindowsWebView2Browser current = getBrowser();
        if (current == null) {
            return;
        }
        current.notifyParentWindowPositionChangedLater();
    }

    private static final class PendingPage {
        private final String url;
        private final String html;
        private final String referrer;

        private PendingPage(String url, String html, String referrer) {
            this.url = url;
            this.html = html;
            this.referrer = html != null ? null : referrer;
        }
    }
}
