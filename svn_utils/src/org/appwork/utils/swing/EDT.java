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
package org.appwork.utils.swing;

import java.awt.HeadlessException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import javax.swing.SwingUtilities;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.utils.Application;
import org.appwork.utils.Exceptions;
import org.appwork.utils.reflection.CompiledType;

/**
 * This class should be used to run gui code in the edt and return the generic datatype to the parent thread.
 *
 * Implement edtRun to asure Thread safe executio of this gui code.
 *
 * @author $Author: Thomas$
 *
 * @param <T>
 */
public abstract class EDT<T, ExceptionType extends Throwable> implements Runnable {
    private final static boolean             HEADLESS = Application.isHeadless();
    /**
     * flag. If Runnable has terminated yet
     */
    private volatile boolean                 done     = false;
    /**
     * flag, has runnable already started, invoked in edt
     */
    private final AtomicBoolean              started  = new AtomicBoolean(false);
    private volatile AtomicReference<Object> runMode  = new AtomicReference<Object>(null);
    /**
     * lock used for EDT waiting
     */
    /**
     * Stores The returnvalue. This Value if of the Generic Datatype T
     */
    private volatile T                       returnValue;
    private volatile Throwable               exception;
    private volatile Exception               caller;

    /**
     * LIke {@link #waitFor()}, but swallows InterruptedException and resets the interrupt flag instead. Returns null in case of an
     * interrupt.
     *
     * @return
     * @throws ExceptionType
     */
    public T waitForAndSwallowInterrupt() throws ExceptionType {
        try {
            return waitFor();
        } catch (InterruptedException e) {
            Exceptions.resetInterruptFlag(e);
            return null;
        }
    }

    /**
     * Implement this method. Gui code should be used ONLY in this Method.
     *
     * @return
     */
    protected abstract T runInEDT() throws ExceptionType;

    /**
     * Run the runnable
     */
    public void run() {
        if (started.compareAndSet(false, true)) {
            try {
                runMode.compareAndSet(null, this);
                this.returnValue = this.runInEDT();
            } catch (final Throwable e) {
                this.exception = e;
                if (this.caller != null && e instanceof HeadlessException) {
                    LogV3.exception(this, caller, "Unhandled Headless Exception in EDT");
                }
            } finally {
                this.done = true;
            }
        }
    }

    public EDT<T, ExceptionType> start() {
        return start(false);
    }

    public EDT<T, ExceptionType> invokeLater() {
        return start(true);
    }

    protected EDT<T, ExceptionType> start(final boolean invokeLater) {
        if (caller == null && HEADLESS) {
            this.caller = new Exception("EventDispatchThread in headless mode!?");
        }
        if (!invokeLater && SwingUtilities.isEventDispatchThread()) {
            if (runMode.compareAndSet(null, Boolean.FALSE) || runMode.compareAndSet(Boolean.TRUE, Boolean.FALSE)) {
                this.run();
            }
        } else {
            if (runMode.compareAndSet(null, Boolean.TRUE)) {
                SwingUtilities.invokeLater(this);
            }
        }
        return this;
    }

    public T waitFor() throws InterruptedException, ExceptionType {
        InterruptedException interrupted = null;
        try {
            if (this.done) {
                return returnValue;
            }
            this.start(false);
            // The following loop will never be reached if the edthelper has
            // been called from within the edt.
            // interruptException will never be set in this case
            // Workaround:
            // a pretty dirty workaround would be to read the
            // EventDispatchThread.class.shutdown field. if it is true, the edt
            // has been interrupted and will be shut down.
            // I guess it would be best practise not ti interrupt the edt
            if (this.done) {
                return returnValue;
            }
            // c = System.currentTimeMillis();
            try {
                while (this.done == false) {
                    /* ASK daniel why we use Sleep(1) here */
                    /* Thread.yield can use too much cpu */
                    /*
                     * Thread.interrupt can cause side-effects! overwritten interrupt method in current Thread can do bad things
                     */
                    /* Object.wait releases all locks! */
                    Thread.sleep(1);
                }
            } catch (InterruptedException e) {
                interrupted = e;
            }
            return returnValue;
        } finally {
            if (interrupted != null) {
                throw interrupted;
            } else if (this.exception != null) {
                Exceptions.addSuppressed(exception, new Exception("caused here").fillInStackTrace());
                if (exception instanceof Error) {
                    throw (Error) this.exception;
                } else if (exception instanceof RuntimeException) {
                    throw (RuntimeException) this.exception;
                } else {
                    final CompiledType ct = CompiledType.create(this.getClass());
                    final CompiledType actualExceptionType = ct.getComponentTypes(EDT.class)[1];
                    if (actualExceptionType.isInstanceOf(exception.getClass())) {
                        throw (ExceptionType) this.exception;
                    } else {
                        // something went wrong
                        throw new WTFException(exception);
                    }
                }
            }
        }
    }

    /**
     * @param runnable
     */
    public static EDT<Object, RuntimeException> run(final Runnable runnable) {
        return new EDT<Object, RuntimeException>() {
            @Override
            protected Object runInEDT() throws RuntimeException {
                runnable.run();
                return null;
            }
        }.start();
    }

    public static EDT<Object, RuntimeException> invokeLater(final Runnable runnable) {
        return new EDT<Object, RuntimeException>() {
            @Override
            protected Object runInEDT() throws RuntimeException {
                runnable.run();
                return null;
            }
        }.invokeLater();
    }
}
