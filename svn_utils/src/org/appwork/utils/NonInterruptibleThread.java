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
package org.appwork.utils;

import java.nio.channels.ClosedByInterruptException;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.loggingv3.LogV3;

/**
 * @author daniel
 * @date 10.12.2018
 *
 */
public class NonInterruptibleThread extends Thread {
    final protected AtomicReference<Thread> callingThread = new AtomicReference<Thread>(null);

    /**
     * @return the callingThread
     */
    public Thread getCallingThread() {
        return callingThread.get();
    }

    protected NonInterruptibleThread(Runnable r) {
        super(r, "NonInterruptibleThread_" + System.currentTimeMillis());
    }

    private static final ThreadPoolExecutor POOL = new ThreadPoolExecutor(0, Integer.MAX_VALUE, 15, TimeUnit.SECONDS, new SynchronousQueue<Runnable>(), new ThreadFactory() {
        @Override
        public Thread newThread(Runnable r) {
            final NonInterruptibleThread ret = new NonInterruptibleThread(r);
            ret.setDaemon(true);
            return ret;
        }
    });

    private static final StackTraceElement getCaller(final Throwable throwable) {
        if (throwable != null && throwable.getStackTrace() != null && throwable.getStackTrace().length > 0) {
            final StackTraceElement[] stackTrace = throwable.getStackTrace();
            if (stackTrace.length > 1) {
                return stackTrace[1];
            } else {
                return stackTrace[0];
            }
        } else {
            return null;
        }
    }

    @SuppressWarnings("unchecked")
    public static <T, E extends Exception> T execute(final Throwable caller, final NonInterruptibleRunnable<T, E> run) throws E {
        if (Thread.currentThread() instanceof NonInterruptibleThread) {
            try {
                return run.run();
            } catch (InterruptedException e) {
                if (!Exceptions.containsInstanceOf(e, InterruptedIntentionalException.class)) {
                    LogV3.defaultLogger().exception("InterruptException in NonInterruptable Thread. This must not happen!", e);
                }
                // this actually cannot happen! since the thread cannot get interrupted. The only way this could happen, if the process
                // throws a InterruptedException without the thread beeing interrupted
                throw new IllegalStateException(e);
            }
        } else {
            final StackTraceElement callerMethod = DebugMode.TRUE_IN_IDE_ELSE_FALSE ? getCaller(caller) : null;
            final Thread callingThread = Thread.currentThread();
            final Future<T> fut = POOL.submit(new Callable<T>() {
                @Override
                public T call() throws Exception {
                    try {
                        if (callerMethod != null) {
                            Thread.currentThread().setName("NonInterruptibleThread:" + callingThread.getName() + "|" + callingThread.getId() + "=" + callerMethod.getClassName() + "(" + callerMethod.getMethodName() + ":" + callerMethod.getLineNumber() + ")");
                        } else {
                            Thread.currentThread().setName("NonInterruptibleThread:" + callingThread.getName() + "|" + callingThread.getId() + "=Active");
                        }
                        ((NonInterruptibleThread) Thread.currentThread()).callingThread.set(callingThread);
                        return run.run();
                    } catch (InterruptedException e) {
                        LogV3.defaultLogger().exception("InterruptException in NonInterruptable Thread. This must not happen!", e);
                        // this actually cannot happen! since the thread cannot get interrupted. The only way this could happen, if the
                        // process
                        // throws a InterruptedException without the thread beeing interrupted
                        throw new IllegalStateException(e);
                    } finally {
                        Thread.currentThread().setName("NonInterruptibleThread:Idle");
                        if (!((NonInterruptibleThread) Thread.currentThread()).callingThread.compareAndSet(callingThread, null)) {
                            LogV3.log(new IllegalStateException());
                        }
                    }
                }
            });
            boolean interruptedFlag = false;
            try {
                while (true) {
                    try {
                        return fut.get();
                    } catch (InterruptedException e) {
                        interruptedFlag = true;
                    } catch (ExecutionException e) {
                        if (Exceptions.containsInstanceOf(e, InterruptedException.class, ClosedByInterruptException.class)) {
                            if (!Exceptions.containsInstanceOf(e, InterruptedIntentionalException.class)) {
                                LogV3.defaultLogger().exception("InterruptException in NonInterruptable Thread. This must not happen!", e);
                            }
                            // this actually cannot happen! since the thread cannot get interrupted. The only way this could happen, if the
                            // process
                            // throws a InterruptedException without the thread being interrupted
                            throw new IllegalStateException(e);
                        }
                        if (e.getCause() instanceof RuntimeException) {
                            throw Exceptions.addSuppressed((RuntimeException) e.getCause(), caller);
                        } else {
                            throw Exceptions.addSuppressed((E) e.getCause(), caller);
                        }
                    }
                }
            } finally {
                if (interruptedFlag) {
                    Thread.currentThread().interrupt();
                }
            }
        }
    }

    @Override
    public void interrupt() {        
    }

    /**
     * @param runnable
     */
    public static void execute(final Runnable runnable) {
        final Throwable caller = new Exception();
        execute(caller, new NonInterruptibleRunnable<Void, RuntimeException>() {
            @Override
            public Void run() throws RuntimeException {
                runnable.run();
                return null;
            }
        });
    }

    /**
     * returns the currentthread. if the thread is a NonInterruptableThread, this method returns the caller instead
     *
     * @return
     */
    public static Thread currentOrCallerThread() {
        Thread thread = Thread.currentThread();
        while (thread instanceof NonInterruptibleThread) {
            final Thread callingThread = ((NonInterruptibleThread) thread).getCallingThread();
            if (callingThread instanceof NonInterruptibleThread) {
                thread = callingThread;
            } else {
                return callingThread;
            }
        }
        return thread;
    }
}
