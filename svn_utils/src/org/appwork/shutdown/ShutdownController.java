/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.shutdown;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.Application;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.Exceptions;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.Time;
import org.appwork.utils.logging2.LogInterface;

public class ShutdownController extends Thread {
    class ShutdownEventWrapper extends ShutdownEvent {
        private final Thread orgThread;

        /**
         * @param value
         */
        public ShutdownEventWrapper(final Thread value) {
            this.orgThread = value;
            // call "Nativ" hooks at the end.
            this.setHookPriority(Integer.MIN_VALUE);
        }

        @Override
        public boolean equals(final Object obj) {
            if (obj instanceof ShutdownEventWrapper) {
                return this.orgThread == ((ShutdownEventWrapper) obj).orgThread;
            } else {
                return false;
            }
        }

        @Override
        public boolean supportsIllegalStateException() {
            return true;
        }

        @Override
        public int hashCode() {
            return this.orgThread.hashCode();
        }

        /*
         * (non-Javadoc)
         *
         * @see org.appwork.shutdown.ShutdownEvent#run()
         */
        @Override
        public void onShutdown(final ShutdownRequest shutdownRequest) {
            this.orgThread.run();
        }

        @Override
        public String toString() {
            return "ShutdownEventWrapper " + this.orgThread + " - " + this.orgThread.getClass().getName() + " Priority: " + this.getHookPriority();
        }
    }

    private static final ShutdownController INSTANCE = new ShutdownController();
    static {
        org.appwork.utils.Application.warnInit();
    }

    /**
     * get the only existing instance of ShutdownController. This is a singleton
     *
     * @return
     */
    public static ShutdownController getInstance() {
        return ShutdownController.INSTANCE;
    }

    private final ArrayList<ShutdownEvent>   hooks                 = new ArrayList<ShutdownEvent>();
    private final ArrayList<ShutdownEvent>   originalShutdownHooks = new ArrayList<ShutdownEvent>();
    private final List<ShutdownVetoListener> vetoListeners         = new ArrayList<ShutdownVetoListener>();
    private int                              exitCode              = 0;
    private final AtomicInteger              requestedShutDowns    = new AtomicInteger(0);
    private volatile Thread                  exitThread            = null;
    private final AtomicBoolean              shutDown              = new AtomicBoolean(false);
    private final AtomicBoolean              shutDownHookRunning   = new AtomicBoolean(false);
    protected volatile ShutdownRequest       shutdownRequest;
    private final boolean                    hooksDelegatedFlag;

    public final boolean isHooksDelegated() {
        return hooksDelegatedFlag;
    }

    /**
     * Create a new instance of ShutdownController. This is a singleton class. Access the only existing instance by using
     * {@link #getInstance()}.
     */
    private ShutdownController() {
        super(ShutdownController.class.getSimpleName());
        boolean hooksDelegatedFlag = false;
        try {
            // add ShutdownController as normal ShutdownHook in Runtime/ApplicationShutdownHooks
            // may throw IllegalStateException('Shutdown in progress')
            Runtime.getRuntime().addShutdownHook(this);
            // synchronize on ApplicationShutdownHooks for synchronized replacement of original hook map
            synchronized (Class.forName("java.lang.ApplicationShutdownHooks")) {
                // try to replace the original hook map to "disable" the original hook manager
                // we overwrite the actual hook list with our own one, and redirect all registered hooks to ShutdownController.
                // WARNING:
                // all illegal access operations may be denied >=Java9
                // and will be denied >=Java16(java.lang.reflect.InaccessibleObjectException), https://openjdk.java.net/jeps/396
                final Field field = ReflectionUtils.getField("java.lang.ApplicationShutdownHooks", "hooks", null, Map.class);
                field.setAccessible(true);
                final Map<Thread, Thread> hooks = (Map<Thread, Thread>) field.get(null);
                if (hooks != null) {
                    // hooks must not be null else we are already in shutdown!
                    final IdentityHashMap<Thread, Thread> hookDelegater = new IdentityHashMap<Thread, Thread>() {
                        /**
                        *
                        */
                        private static final long serialVersionUID = 8334628124340671103L;
                        {
                            // ShutdownController should be the only hook in Runtime/ApplicationShutdownHooks!!
                            super.put(ShutdownController.this, ShutdownController.this);
                        }

                        @Override
                        public Thread put(final Thread key, final Thread value) {
                            final ShutdownEventWrapper hook = new ShutdownEventWrapper(value);
                            ShutdownController.this.addShutdownEvent(hook);
                            return null;
                        }

                        @Override
                        public Thread remove(final Object key) {
                            if (ShutdownController.this.removeShutdownEvent(new ShutdownEventWrapper((Thread) key))) {
                                return (Thread) key;
                            } else {
                                return null;
                            }
                        }
                    };
                    for (final Thread hook : hooks.keySet()) {
                        if (ShutdownController.this != hook) {
                            this.addShutdownEvent(new ShutdownEventWrapper(hook));
                        }
                    }
                    // replace original hook map with hookDelegater
                    field.set(null, hookDelegater);
                    hooksDelegatedFlag = true;
                }
            }
        } catch (final Throwable e) {
            // do NOT call any logger(LogV4,LoggerFactory...) because we are still in ShutdownController constructor causing
            // subsequent calls to ShutdownController.getInstance to return null resulting in NullpointerException
        }
        this.hooksDelegatedFlag = hooksDelegatedFlag;
    }

    public void addShutdownEvent(final ShutdownEvent event) {
        if (this.isAlive()) {
            final IllegalStateException e = new IllegalStateException("Cannot add hook during shutdown:" + event);
            if (event.supportsIllegalStateException()) {
                throw e;
            } else {
                LogV3.log(e);
                return;
            }
        }
        if (event instanceof ShutdownEventWrapper) {
            synchronized (this.originalShutdownHooks) {
                if (!originalShutdownHooks.contains(event)) {
                    this.originalShutdownHooks.add(event);
                }
            }
        } else {
            synchronized (this.hooks) {
                int i = 0;
                // add event sorted
                for (final Iterator<ShutdownEvent> it = this.hooks.iterator(); it.hasNext();) {
                    final ShutdownEvent next = it.next();
                    if (next.getHookPriority() <= event.getHookPriority()) {
                        this.hooks.add(i, event);
                        return;
                    }
                    i++;
                }
                this.hooks.add(event);
            }
        }
    }

    public void addShutdownVetoListener(final ShutdownVetoListener listener) {
        synchronized (this.vetoListeners) {
            if (this.vetoListeners.contains(listener)) {
                log("Add ShutdownVetoListener:" + listener + ":" + false);
                return;
            } else {
                this.vetoListeners.add(listener);
                log("Add ShutdownVetoListener:" + listener + ":" + true);
                try {
                    Collections.sort(this.vetoListeners, new Comparator<ShutdownVetoListener>() {
                        @Override
                        public int compare(final ShutdownVetoListener o1, final ShutdownVetoListener o2) {
                            return CompareUtils.compareLong(o1.getShutdownVetoPriority(), o2.getShutdownVetoPriority());
                        }
                    });
                } catch (final Throwable e) {
                    LogV3.log(e);
                }
            }
        }
    }

    /**
     * @return
     * @return
     */
    public ShutdownRequest collectVetos(final ShutdownRequest request) {
        final ShutdownVetoListener[] localList;
        synchronized (this.vetoListeners) {
            localList = this.vetoListeners.toArray(new ShutdownVetoListener[] {});
        }
        for (final ShutdownVetoListener v : localList) {
            try {
                if (request != null && request.askForVeto(v) == false) {
                    continue;
                }
                v.onShutdownVetoRequest(request);
            } catch (final ShutdownVetoException e) {
                if (request != null) {
                    try {
                        request.addVeto(e);
                    } catch (final Throwable e2) {
                        e2.printStackTrace();
                    }
                }
            } catch (final Throwable e) {
                e.printStackTrace();
            }
        }
        return request;
    }

    public int getExitCode() {
        return this.exitCode;
    }

    public ShutdownRequest getShutdownRequest() {
        return this.shutdownRequest;
    }

    public List<ShutdownVetoListener> getShutdownVetoListeners() {
        synchronized (this.vetoListeners) {
            return new ArrayList<ShutdownVetoListener>(this.vetoListeners);
        }
    }

    /**
     * Same function as org.appwork.utils.Exceptions.getStackTrace(Throwable)<br>
     * <b>DO NOT REPLACE IT EITHER!</b> Exceptions.class my be unloaded. This would cause Initialize Exceptions during shutdown.
     *
     * @param thread
     * @return
     */
    private String getStackTrace(final Thread thread) {
        try {
            final StackTraceElement[] st = thread.getStackTrace();
            final StringBuilder sb = new StringBuilder("");
            for (final StackTraceElement element : st) {
                sb.append(element);
                sb.append("\r\n");
            }
            return sb.toString();
        } catch (final Throwable e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * @param instance2
     * @return
     */
    public boolean hasShutdownEvent(final ShutdownEvent event) {
        if (event == null) {
            return false;
        } else if (event instanceof ShutdownEventWrapper) {
            synchronized (this.originalShutdownHooks) {
                return this.originalShutdownHooks.contains(event);
            }
        } else {
            synchronized (this.hooks) {
                return this.hooks.contains(event);
            }
        }
    }

    public boolean isShutDownRequested() {
        return this.requestedShutDowns.get() > 0;
    }

    public boolean removeShutdownEvent(final ShutdownEvent event) {
        if (event == null) {
            return false;
        } else if (this.isAlive()) {
            final IllegalStateException e = new IllegalStateException("Cannot remove hook during shutdown:" + event);
            if (event.supportsIllegalStateException()) {
                throw e;
            } else {
                LogV3.log(e);
                return false;
            }
        } else if (event instanceof ShutdownEventWrapper) {
            synchronized (this.originalShutdownHooks) {
                return originalShutdownHooks.remove(event);
            }
        } else {
            boolean ret = false;
            synchronized (this.hooks) {
                // add event sorted
                for (final Iterator<ShutdownEvent> it = this.hooks.iterator(); it.hasNext();) {
                    final ShutdownEvent next = it.next();
                    if (next == event) {
                        it.remove();
                        ret = true;
                    }
                }
            }
            return ret;
        }
    }

    public void removeShutdownVetoListener(final ShutdownVetoListener listener) {
        synchronized (this.vetoListeners) {
            final boolean ret = this.vetoListeners.remove(listener);
            log("Remove ShutdownVetoListener:" + listener + ":" + ret);
        }
    }

    public boolean requestShutdown() {
        return this.requestShutdown(false);
    }

    // /**
    // *
    // */
    public boolean requestShutdown(final boolean silent) {
        return this.requestShutdown(new BasicShutdownRequest(silent));
    }

    public boolean isShuttingDown() {
        return shutDown.get();
    }

    public boolean requestShutdown(final ShutdownRequest request) {
        if (request == null) {
            throw new NullPointerException();
        }
        if (!Application.isHeadless()) {
            try {
                if (javax.swing.SwingUtilities.isEventDispatchThread()) {
                    LogV3.log(new Exception("This call should be in an own thread - else the shutdown hooks may wait for the edt and create a 'deadlock'"));
                }
            } catch (Throwable e) {
                LogV3.log(e);
            }
        }
        this.log("Request Shutdown: " + request);
        this.requestedShutDowns.incrementAndGet();
        try {
            this.collectVetos(request);
            final List<ShutdownVetoException> vetos = request.getVetos();
            if (vetos.size() == 0) {
                LogV3.info("No Vetos");
                final ShutdownVetoListener[] localList;
                synchronized (this.vetoListeners) {
                    localList = this.vetoListeners.toArray(new ShutdownVetoListener[] {});
                }
                LogV3.info("Fire onShutDownEvents:" + localList.length);
                for (final ShutdownVetoListener v : localList) {
                    try {
                        LogV3.info("Call onShutdown: " + v);
                        v.onShutdown(request);
                    } catch (final Throwable e) {
                        LogV3.log(e);
                    } finally {
                        LogV3.info("Call onShutdown done: " + v);
                    }
                }
                if (this.shutDown.compareAndSet(false, true)) {
                    ShutdownController.this.shutdownRequest = request;
                    LogV3.info("Create ExitThread");
                    try {
                        request.onShutdown();
                    } catch (final Throwable e) {
                        LogV3.severe(Exceptions.getStackTrace(e));
                    }
                    this.exitThread = new Thread("ShutdownThread:" + System.currentTimeMillis() + "|" + request) {
                        @Override
                        public void run() {
                            Integer exitCode = request.getExitCode();
                            try {
                                if (exitCode == null) {
                                    exitCode = ShutdownController.this.getExitCode();
                                    log("Exit Now(Controller): Code: " + exitCode);
                                } else {
                                    log("Exit Now(Request): Code: " + exitCode);
                                }
                                ShutdownController.this.runHooks();
                            } finally {
                                System.exit(exitCode != null ? exitCode.intValue() : -1);
                            }
                        }
                    };
                    this.exitThread.start();
                }
                long waitDuration = 0;
                while (this.exitThread.isAlive()) {
                    log("Wait for ShutdownThread:" + exitThread + "|waitDuration:" + waitDuration);
                    try {
                        Thread.sleep(500);
                        waitDuration += 500;
                    } catch (final InterruptedException e) {
                        log("Wait for ShutdownThread interrupted:" + exitThread + "|waitDuration:" + waitDuration);
                        return true;
                    }
                }
                log("ShutdownThread finished:" + exitThread + "|waitDuration:" + waitDuration);
                return true;
            } else {
                final ShutdownVetoListener[] localList;
                synchronized (this.vetoListeners) {
                    localList = this.vetoListeners.toArray(new ShutdownVetoListener[] {});
                }
                LogV3.info("Vetos found:" + localList.length);
                for (final ShutdownVetoListener v : localList) {
                    try {
                        /* make sure noone changes content of vetos */
                        v.onShutdownVeto(request);
                    } catch (final Throwable e) {
                        LogV3.log(e);
                    }
                }
                request.onShutdownVeto();
                return false;
            }
        } finally {
            this.requestedShutDowns.decrementAndGet();
        }
    }

    private LogInterface logger;

    public LogInterface getLogger() {
        return this.logger;
    }

    public void setLogger(final LogInterface logger) {
        this.logger = logger;
    }

    /**
     * @param string
     */
    private void log(final String string) {
        try {
            // code may run in the shutdown hook. Classloading problems are
            // possible
            final LogInterface logger = getLogger();
            if (logger != null) {
                logger.info(string);
            } else {
                System.out.println(string);
            }
        } catch (final Throwable e) {
        }
    }
    // private boolean enabled = true;
    //
    // public boolean isEnabled() {
    // return enabled;
    // }
    //
    // public void setEnabled(boolean enabled) {
    // this.enabled = enabled;
    // }

    private void runHooks() {
        // if (!isEnabled()) {
        // return;
        // }
        if (shutDownHookRunning.compareAndSet(false, true)) {
            /*
             * Attention. This runs in shutdownhook. make sure, that we do not have to load previous unloaded classes here.
             */
            try {
                final List<ShutdownEvent> list;
                synchronized (this.hooks) {
                    list = new ArrayList<ShutdownEvent>(this.hooks);
                }
                synchronized (this.originalShutdownHooks) {
                    list.addAll(this.originalShutdownHooks);
                }
                final ShutdownRequest shutdownRequest = getShutdownRequest();
                int i = 0;
                for (final ShutdownEvent e : list) {
                    try {
                        i++;
                        final long started = Time.systemIndependentCurrentJVMTimeMillis();
                        log("[" + i + "/" + list.size() + "|Priority: " + e.getHookPriority() + "]" + "ShutdownController: start item->" + e);
                        final Thread thread = new Thread(new Runnable() {
                            @Override
                            public void run() {
                                e.onShutdown(shutdownRequest);
                            }
                        });
                        thread.setName("ShutdownHook [" + i + "/" + list.size() + "|Priority: " + e.getHookPriority() + "]");
                        thread.start();
                        try {
                            e.waitFor();
                            thread.join(Math.max(0, e.getMaxDuration()));
                        } catch (final Throwable e1) {
                            e1.printStackTrace();
                        }
                        if (thread.isAlive()) {
                            log("[" + i + "/" + list.size() + "|Priority: " + e.getHookPriority() + "]" + "ShutdownController: " + e + "->is still running after " + e.getMaxDuration() + " ms");
                            log("[" + i + "/" + list.size() + "|Priority: " + e.getHookPriority() + "]" + "ShutdownController: " + e + "->StackTrace:\r\n" + this.getStackTrace(thread));
                        } else {
                            log("[" + i + "/" + list.size() + "|Priority: " + e.getHookPriority() + "]" + "ShutdownController: item ended after->" + (Time.systemIndependentCurrentJVMTimeMillis() - started));
                        }
                        log("[Done:" + i + "/" + list.size() + "]");
                    } catch (final Throwable e1) {
                        e1.printStackTrace();
                    }
                }
                log("Shutdown Hooks Finished");
            } catch (final Throwable e1) {
                e1.printStackTrace();
                // do not use Log here. If LogV3.log(e1); throws an
                // exception,
                // we have to catch it here without the risk of another exception.
            }
        }
    }

    @Override
    public void run() {
        runHooks();
    }

    /**
     * @param i
     */
    public void setExitCode(final int i) {
        this.exitCode = i;
    }
}
