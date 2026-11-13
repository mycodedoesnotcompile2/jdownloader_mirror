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

import java.util.concurrent.Callable;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.Time;

import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.Ole32;
import com.sun.jna.platform.win32.User32;
import com.sun.jna.platform.win32.WinDef.HWND;
import com.sun.jna.platform.win32.WinDef.LPARAM;
import com.sun.jna.platform.win32.WinDef.WPARAM;
import com.sun.jna.platform.win32.WinUser.MSG;

/**
 * Dedicated STA thread with a Win32 message pump. All WebView2 COM calls must run here.
 * <p>
 * Tasks are queued and the thread is woken with a posted {@code WM_APP} message, so the pump blocks in {@code GetMessage} while idle.
 * If the pump ever ends (it received {@code WM_QUIT} or failed), the browsers that lived on it are gone; the next call starts a new
 * pump thread for new browsers.
 */
public final class WebView2StaThread {
    private static final int               WM_APP_WAKE         = 0x8000;
    private static final int               WM_QUIT             = 0x0012;
    private static final int               PM_REMOVE           = 1;
    private static final long              TASK_TIMEOUT_MILLIS = TimeUnit.MINUTES.toMillis(2);
    private static final WebView2StaThread INSTANCE            = new WebView2StaThread();

    public static WebView2StaThread getInstance() {
        return INSTANCE;
    }

    private final LinkedBlockingQueue<Runnable> queue = new LinkedBlockingQueue<Runnable>();
    private final Object                        startLock = new Object();
    private volatile Thread                     thread;
    private volatile CountDownLatch             started;
    private volatile int                        nativeThreadId;

    private WebView2StaThread() {
    }

    private void ensureStarted() throws WebView2Exception {
        final CountDownLatch latch;
        synchronized (startLock) {
            final Thread current = thread;
            if (current == null || !current.isAlive()) {
                if (current != null) {
                    LogV3.warning("WebView2: the STA thread ended; starting a new one");
                    /* Tasks of the dead pump would never complete. */
                    queue.clear();
                }
                final CountDownLatch newLatch = new CountDownLatch(1);
                final Thread t = new Thread(new Runnable() {
                    public void run() {
                        pump(newLatch);
                    }
                }, "WebView2-STA");
                t.setDaemon(true);
                started = newLatch;
                thread = t;
                t.start();
            }
            latch = started;
        }
        try {
            if (!latch.await(10, TimeUnit.SECONDS)) {
                throw new WebView2Exception("WebView2 STA thread did not start");
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new WebView2Exception("Interrupted while starting the WebView2 STA thread", e);
        }
    }

    public boolean isStaThread() {
        return Thread.currentThread() == thread;
    }

    /** Runs {@code task} on the STA thread and waits for it. Runs inline when already on the STA thread. */
    public <T> T call(final Callable<T> task) throws WebView2Exception {
        if (isStaThread()) {
            return invoke(task);
        }
        ensureStarted();
        final Thread pumpThread = thread;
        final CountDownLatch done = new CountDownLatch(1);
        final AtomicReference<T> result = new AtomicReference<T>();
        final AtomicReference<WebView2Exception> error = new AtomicReference<WebView2Exception>();
        queue.offer(new Runnable() {
            public void run() {
                try {
                    result.set(invoke(task));
                } catch (WebView2Exception e) {
                    error.set(e);
                } finally {
                    done.countDown();
                }
            }
        });
        wake();
        try {
            final long deadline = Time.systemIndependentCurrentJVMTimeMillis() + TASK_TIMEOUT_MILLIS;
            while (!done.await(250, TimeUnit.MILLISECONDS)) {
                if (!pumpThread.isAlive()) {
                    throw new WebView2UnknownOutcomeException("The WebView2 STA thread ended");
                }
                if (Time.systemIndependentCurrentJVMTimeMillis() > deadline) {
                    throw new WebView2UnknownOutcomeException("Timed out waiting for WebView2 STA task");
                }
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new WebView2UnknownOutcomeException("Interrupted waiting for WebView2 STA task", e);
        }
        if (error.get() != null) {
            throw error.get();
        }
        return result.get();
    }

    public void execute(final Runnable task) throws WebView2Exception {
        call(new Callable<Void>() {
            public Void call() {
                task.run();
                return null;
            }
        });
    }

    /**
     * Queues {@code task} for the STA thread and returns at once. The task runs from the message loop, never inside a native
     * callback, so it may wait for other WebView2 calls. A task that is posted while no pump thread runs is dropped: there is no
     * browser it could apply to.
     */
    public void post(Runnable task) {
        final Thread current = thread;
        if (current == null || !current.isAlive()) {
            return;
        }
        queue.offer(task);
        wake();
    }

    /**
     * Pumps messages (and queued tasks) until {@code ready} becomes true. Must run on the STA thread; used to wait for asynchronous
     * COM completion callbacks.
     */
    public void pumpUntil(final AtomicBoolean ready, long timeoutMs) throws WebView2Exception {
        if (!isStaThread()) {
            throw new WebView2Exception("pumpUntil must run on the WebView2 STA thread");
        }
        final long deadline = Time.systemIndependentCurrentJVMTimeMillis() + timeoutMs;
        final MSG msg = new MSG();
        while (!ready.get()) {
            drainQueue();
            if (ready.get()) {
                return;
            }
            if (Time.systemIndependentCurrentJVMTimeMillis() > deadline) {
                throw new WebView2UnknownOutcomeException("Timed out waiting for a WebView2 callback");
            }
            boolean dispatched = false;
            while (User32.INSTANCE.PeekMessage(msg, (HWND) null, 0, 0, PM_REMOVE)) {
                if (msg.message == WM_QUIT) {
                    /* Not ours to swallow: hand it back to the outer loop, which ends the pump. */
                    User32.INSTANCE.PostQuitMessage(msg.wParam.intValue());
                    throw new WebView2Exception("The WebView2 STA thread is shutting down");
                }
                if (msg.message != WM_APP_WAKE) {
                    dispatch(msg);
                }
                dispatched = true;
            }
            if (!dispatched) {
                try {
                    Thread.sleep(5L);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    throw new WebView2Exception("Interrupted while waiting for a WebView2 callback", e);
                }
            }
        }
    }

    private static <T> T invoke(Callable<T> task) throws WebView2Exception {
        try {
            return task.call();
        } catch (WebView2Exception e) {
            throw e;
        } catch (Exception e) {
            throw new WebView2Exception("WebView2 STA call failed", e);
        }
    }

    private void wake() {
        final int id = nativeThreadId;
        if (id != 0) {
            User32.INSTANCE.PostThreadMessage(id, WM_APP_WAKE, new WPARAM(0), new LPARAM(0));
        }
    }

    private void pump(CountDownLatch startedLatch) {
        Ole32.INSTANCE.CoInitializeEx(null, Ole32.COINIT_APARTMENTTHREADED);
        final MSG msg = new MSG();
        /* PeekMessage creates the thread message queue, so PostThreadMessage works once started is released. */
        User32.INSTANCE.PeekMessage(msg, (HWND) null, 0, 0, 0);
        nativeThreadId = Kernel32.INSTANCE.GetCurrentThreadId();
        startedLatch.countDown();
        try {
            while (true) {
                drainQueue();
                final int ret = User32.INSTANCE.GetMessage(msg, (HWND) null, 0, 0);
                if (ret == 0 || ret == -1) {
                    LogV3.warning("WebView2: the STA message loop ended (GetMessage=" + ret + ")");
                    break;
                }
                if (msg.message != WM_APP_WAKE) {
                    dispatch(msg);
                }
            }
        } catch (Throwable t) {
            LogV3.log(t);
        } finally {
            nativeThreadId = 0;
            Ole32.INSTANCE.CoUninitialize();
        }
    }

    private static void dispatch(MSG msg) {
        User32.INSTANCE.TranslateMessage(msg);
        User32.INSTANCE.DispatchMessage(msg);
    }

    private void drainQueue() {
        Runnable r;
        while ((r = queue.poll()) != null) {
            try {
                r.run();
            } catch (Throwable t) {
                LogV3.log(t);
            }
        }
    }
}
