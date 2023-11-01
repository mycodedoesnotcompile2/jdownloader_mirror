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

import javax.swing.SwingUtilities;

import org.appwork.utils.Application;


/**
 * This class should be used to run gui code in the edt and return the generic
 * datatype to the parent thread.
 * 
 * Implement edtRun to asure Thread safe executio of this gui code.
 * 
 * @author $Author: Thomas$
 * 
 * @param <T>
 */

public abstract class EDTHelper<T> implements Runnable {
    /**
     * flag. If Runnable has terminated yet
     */
    private volatile boolean     done    = false;

    /**
     * flag, has runnable already started, invoked in edt
     */
    private volatile boolean     started = false;
    /**
     * lock used for EDT waiting
     */

    /**
     * Stores The returnvalue. This Value if of the Generic Datatype T
     */
    private T                    returnValue;

    private RuntimeException     exception;
    private InterruptedException interruptException;

    private Error                error;

    private Exception            caller;

    /**
     * Implement this method. Gui code should be used ONLY in this Method.
     * 
     * @return
     */
    public abstract T edtRun();

    public InterruptedException getInterruptException() {
        return this.interruptException;
    }

    /**
     * Call this method if you want to wait for the EDT to finish the runnable.
     * It is assured that the returnvalue is available after this methjod has
     * returned.
     * 
     * @return
     */
    public T getReturnValue() {

        this.waitForEDT();
        return this.returnValue;
    }

    public boolean isInterrupted() {
        return this.interruptException != null;
    }

    /**
     * Run the runnable
     */
    public void run() {
        this.started = true;
        try {
            if (Application.isHeadless()) {
                if (this.caller != null) {
                    org.appwork.loggingv3.LogV3.log(this.caller);
                }
            }
            this.returnValue = this.edtRun();
        } catch (HeadlessException e) {
            this.exception = e;
            org.appwork.loggingv3.LogV3.log(e);
                  org.appwork.loggingv3.LogV3.severe("Unhandled Headless Exception in EDT");
            if (this.caller != null) {
                org.appwork.loggingv3.LogV3.log(this.caller);
            }
        } catch (final RuntimeException e) {
            this.exception = e;
            org.appwork.loggingv3.LogV3.log(e);
        } catch (final Error e) {
            this.error = e;
            org.appwork.loggingv3.LogV3.log(e);
        } finally {
            this.done = true;
        }
    }

    public void start() {
        this.start(false);
    }

    /**
     * starts the runnable
     * 
     * returns true in case we are in EDT or false if it got invoked later
     */
    public void start(final boolean invokeLater) {
        if (this.started) { return; }
        if (org.appwork.utils.Application.isHeadless()) {
            this.caller = new Exception("EventDispatchThread in headless mode!?");
        }
        this.started = true;
        if (!invokeLater && SwingUtilities.isEventDispatchThread()) {
            this.run();
        } else {
            SwingUtilities.invokeLater(this);
        }
    }

    /**
     * Wait until the runnable has been finished by the EDT. If the Runnable has
     * not started yet, it gets started.
     */
    public void waitForEDT() {
        // long c = -1;
        try {
            if (this.done) { return; }
            this.start(false);
            // The following loop will never be reached if the edthelper has
            // been called from within the edt.
            // interruptException will never be set in this case
            // Workaround:
            // a pretty dirty workaround would be to read the
            // EventDispatchThread.class.shutdown field. if it is true, the edt
            // has been interrupted and will be shut down.
            // I guess it would be best practise not ti interrupt the edt
            if (this.done) { return; }
            // c = System.currentTimeMillis();
            try {
                while (this.done == false) {
                    /* ASK daniel why we use Sleep(1) here */
                    /* Thread.yield can use too much cpu */
                    /*
                     * Thread.interrupt can cause side-effects! overwritten
                     * interrupt method in current Thread can do bad things
                     */
                    /* Object.wait releases all locks! */
                    Thread.sleep(1);
                }
            } catch (final InterruptedException e) {
                this.interruptException = e;
                return;
            }
        } finally {
            // if (c != -1 && System.currentTimeMillis() - c > 1000) {
            // new
            // WTFException("EDT blocked longer than 1sec!").printStackTrace();
            // }
            /* make sure we remove the interrupted flag */
            if (this.exception != null) { throw this.exception; }
            if (this.error != null) { throw this.error; }
        }
    }
}
