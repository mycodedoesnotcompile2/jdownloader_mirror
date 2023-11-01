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
package org.appwork.swing;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Timer;



/**
 * tiny animation class
 * 
 * @author thomas
 * 
 */
public class SwingAnimator {

    public class AnimatorListener implements ActionListener {
        private int            step;
        private final long     startTime;
        private final Getter   getter;
        private final Setter   setter;
        private final Runnable finalizer;
        private final int      startValue;
        private final int      steps;

        protected AnimatorListener(final Getter getter, final Setter setter, final Runnable finalizer) {
            this.step = 0;
            this.startTime = System.currentTimeMillis();
            this.getter = getter;
            this.setter = setter;
            this.finalizer = finalizer;
            this.steps = SwingAnimator.this.duration / (1000 / SwingAnimator.this.fps);

            this.startValue = getter.getStartValue();
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
         * )
         */
        @Override
        public void actionPerformed(final ActionEvent e) {
            try {
                this.step++;
                final int cu = (int) (System.currentTimeMillis() - this.startTime);

                if (this.step <= this.steps) {

                    this.setter.set(this.getter.get(this, cu));
                } else {
                    SwingAnimator.this.timer.stop();
                    SwingAnimator.this.timer = null;
                    this.finalizer.run();

                }
            } catch (final Throwable t) {
                org.appwork.loggingv3.LogV3.log(t);
                SwingAnimator.this.timer.stop();
                SwingAnimator.this.timer = null;
                this.finalizer.run();
            }

        }

        public int getDuration() {
            return SwingAnimator.this.duration;
        }

        public long getStartTime() {
            return this.startTime;
        }

        public int getStartValue() {
            return this.startValue;
        }

        public int getStep() {
            return this.step;
        }

        public int getSteps() {
            return this.steps;
        }

    }

    public static abstract class Getter {
        /**
         * @param animatorListener
         * @param cu
         * @return
         */
        public abstract int get(final AnimatorListener animatorListener, final int cu);

        /**
         * @return
         */
        public abstract int getStartValue();
    }

    public static abstract class Setter {
        public abstract void set(int i);
    }

    private final int duration;

    private final int fps;

    private Timer     timer;

    /**
     * @param i
     * @param j
     */
    public SwingAnimator(final int duratation, final int fps) {
        this.duration = duratation;
        this.fps = fps;
    }

    /**
     * @param getter
     * @param setter
     * @param runnable
     */
    public synchronized void run(final Getter getter, final Setter setter, final Runnable finalizer) {
        if (this.timer != null) {
            this.timer.stop();
        }
        this.timer = new Timer(1000 / this.fps, new AnimatorListener(getter, setter, finalizer));
        this.timer.setInitialDelay(0);
        this.timer.setRepeats(true);
        this.timer.start();
    }

}
