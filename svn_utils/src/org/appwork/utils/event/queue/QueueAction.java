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
package org.appwork.utils.event.queue;

import org.appwork.utils.event.queue.Queue.QueuePriority;

/**
 * @author daniel
 *
 */
public abstract class QueueAction<T, E extends Throwable> {

    private Throwable        exeption;

    private volatile boolean finished         = false;
    private volatile boolean killed           = false;
    private QueuePriority    prio             = QueuePriority.NORM;
    private Queue            queue            = null;
    private T                result           = null;
    private String           callerStackTrace = null;

    private volatile boolean started          = false;

    private Thread           thread           = null;

    public QueueAction() {
    }

    public QueueAction(final QueuePriority prio) {
        this.prio = prio;
    }

    protected boolean allowAsync() {
        return false;
    }

    /**
     * @param e
     * @return
     */
    protected synchronized boolean callExceptionHandler() {
        if (this.exeption == null) {
            return true;
        }
        if (this.exeption != null && this.handleException(this.exeption)) {
            this.exeption = null;
            return true;
        }
        return false;
    }

    protected String getCallerStackTrace() {
        return this.callerStackTrace;
    }

    protected Thread getCallerThread() {
        return this.thread;
    }

    /**
     * @return the exeption
     */
    public Throwable getExeption() {
        return this.exeption;
    }

    protected Queue getQueue() {
        return this.queue;
    }

    public QueuePriority getQueuePrio() {
        return this.prio;
    }

    public T getResult() {
        return this.result;
    }

    public boolean gotKilled() {
        return this.killed;
    }

    public boolean gotStarted() {
        return this.started;
    }

    /**
     * Callback for asynchron queuecalls if exceptions occured. has to return true if exception got handled
     *
     * @param e
     * @return
     */
    public boolean handleException(final Throwable e) {
        org.appwork.loggingv3.LogV3.log(e);
        return false;
    }

    public boolean isFinished() {
        return this.finished;
    }

    public void kill() {
        synchronized (this) {
            if (this.finished == true) {
                return;
            }
            this.killed = true;
            this.finished = true;
            this.notifyAll();
        }
    }

    protected void onEnqueu(Queue queue) {
    }

    protected void postRun() {
    }

    protected void preRun() {
    }

    public void reset() {
        this.exeption = null;
        this.killed = false;
        this.finished = false;
        this.callerStackTrace = null;
        this.thread = null;
        this.queue = null;
    }

    protected abstract T run() throws E;

    public void setCallerThread(final Queue queue, final Thread thread) {
        this.thread = thread;
        this.queue = queue;
        if (queue != null && queue.isDebug() && thread != null) {
            StringBuilder sb = new StringBuilder();
            for (final StackTraceElement elem : thread.getStackTrace()) {
                sb.append(elem.toString() + "\r\n");
            }
            this.callerStackTrace = sb.toString();
            sb = null;
        }
    }

    /**
     * @param finished
     *            the finished to set
     */
    public void setFinished(final boolean finished) {
        synchronized (this) {
            this.finished = finished;
            this.notifyAll();
        }
    }

    public void setQueuePrio(final QueuePriority prio) {
        this.prio = prio;
    }

    @SuppressWarnings("unchecked")
    final public void start(final Queue queue) throws E {
        this.queue = queue;
        this.started = true;
        try {
            this.preRun();
        } catch (final Throwable e) {
            org.appwork.loggingv3.LogV3.log(e);
        }
        try {
            this.result = this.run();
        } catch (final Throwable th) {

            if (queue != null && queue.isDebug()) {
                org.appwork.loggingv3.LogV3.severe("QueueActionCallerStackTrace:\r\n" + this.callerStackTrace);
            }
            this.exeption = th;
            if (th instanceof RuntimeException) {
                throw (RuntimeException) th;
            } else {
                throw (E) th;
            }
        } finally {
            try {
                this.postRun();
            } catch (final Throwable e) {
                org.appwork.loggingv3.LogV3.log(e);
            }
        }
    }
}
