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
package org.appwork.utils.swing.dialog;

import java.awt.event.ActionEvent;
import java.util.concurrent.atomic.AtomicLong;

import org.appwork.uio.UIOManager;
import org.appwork.utils.swing.EDTHelper;

/**
 * @author daniel
 * 
 */
public class AbstractTimerThread extends Thread {

    public static void main(final String[] args) throws DialogClosedException, DialogCanceledException {
        Dialog.getInstance().setDefaultTimeout(30000);
        Dialog.getInstance().showDialog(new ConfirmDialog(UIOManager.LOGIC_COUNTDOWN, "TimerTest", "TimerTest", null, "close", "reset") {
            /*
             * (non-Javadoc)
             * 
             * @see org.appwork.utils.swing.dialog.AbstractDialog#actionPerformed (java.awt.event.ActionEvent)
             */
            @Override
            public void actionPerformed(final ActionEvent e) {
                if (e.getSource() == this.okButton) {
                    super.actionPerformed(e);
                } else {
                    this.resetTimer();
                }
            }
        });
    }

    private final AbstractDialog<?> dialog;

    private final AtomicLong        counter = new AtomicLong(0);

    public AbstractTimerThread(final AbstractDialog<?> dialog) {
        this.dialog = dialog;
        this.reset();
        this.setDaemon(true);
        this.setName("DialogTimer: " + dialog);
    }

    protected boolean isCurrentTimer() {
        return this.dialog.getTimer().get() == this;
    }

    protected boolean isVisible() {
        return Boolean.TRUE.equals(new EDTHelper<Boolean>() {

            @Override
            public Boolean edtRun() {
                return AbstractTimerThread.this.dialog.isVisible();
            }
        }.getReturnValue());
    }

    public void reset() {
        final long timeout = this.dialog.getCountdown();
        if (timeout <= 0) {
            throw new IllegalArgumentException("timeout is invalid " + timeout);
        }
        this.counter.set(timeout);
    }

    @Override
    public void run() {
        try {
            // sleep while dialog is invisible
            while (!this.isVisible() && this.isCurrentTimer()) {
                try {
                    Thread.sleep(200);
                } catch (final InterruptedException e) {
                    return;
                }
            }
            long currentTimeout = this.counter.get();
            while (!dialog.isExpired(currentTimeout) && this.isCurrentTimer() && this.isVisible()) {
                final String left = dialog.formatCountdown(currentTimeout);
                new EDTHelper<Object>() {

                    @Override
                    public Object edtRun() {
                        AbstractTimerThread.this.dialog.timerLbl.setText(left);
                        return null;
                    }

                }.start();
                Thread.sleep(1000);
                if (this.counter.compareAndSet(currentTimeout, currentTimeout - 1000)) {
                    currentTimeout = currentTimeout - 1000;
                } else {
                    currentTimeout = this.counter.get();
                }
            }
            if (this.isCurrentTimer() && this.isVisible()) {
                if (!this.isInterrupted()) {
                    this.dialog.onTimeout();
                }
            }
        } catch (final InterruptedException e) {
            return;
        } finally {
            this.dialog.getTimer().compareAndSet(AbstractTimerThread.this, null);
        }
    }
}
