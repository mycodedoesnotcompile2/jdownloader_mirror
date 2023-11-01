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
package org.appwork.utils.swing.windowflasher;

import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.List;

import javax.swing.JFrame;
import javax.swing.Timer;

import org.appwork.utils.swing.EDTRunner;

public class WindowFlasher {

    private final JFrame                window;

    private final java.util.List<Image> flashIcons;
    private List<Image>                 icons;
    private Timer                       iconFlashTimer;

    private volatile boolean            running = false;

    public WindowFlasher(final JFrame frame, final java.util.List<Image> list) {
        this.window = frame;
        this.flashIcons = list;

        final WindowAdapter windowWindowAdapter = new WindowAdapter() {

            @Override
            public void windowGainedFocus(final WindowEvent e) {
                if (WindowFlasher.this.running) {
                    WindowFlasher.this.stop();
                }
            }

        };

        this.window.addWindowFocusListener(windowWindowAdapter);
    }

    /**
     * @return
     */
    public boolean hasFocus() {
        if (this.window.isFocused()) {
            return true;
        }

        return false;
    }

    public boolean isRunning() {
        return this.running;
    }

    /**
     * @param flashy
     */
    protected void set(final boolean flashy) {
        if (flashy) {

            WindowFlasher.this.window.setIconImages(WindowFlasher.this.flashIcons);
        } else {
            WindowFlasher.this.window.setIconImages(WindowFlasher.this.icons);
        }
    }

    public synchronized void start() {
        if (!this.hasFocus()) {
            this.running = true;
            if (this.flashIcons != null) {
                if (this.iconFlashTimer == null) {
                    this.icons = this.window.getIconImages();
                    this.iconFlashTimer = new Timer(500, new ActionListener() {
                        private boolean flashy = false;

                        @Override
                        public void actionPerformed(final ActionEvent e) {
                            this.flashy = !this.flashy;
                            WindowFlasher.this.set(this.flashy);

                        }
                    });
                    this.iconFlashTimer.setRepeats(true);
                    this.iconFlashTimer.start();
                }
            }
        }
    }

    /**
     *
     */
    public void stop() {
        this.running = false;
        new EDTRunner() {

            @Override
            protected void runInEDT() {

                if (WindowFlasher.this.iconFlashTimer != null) {

                    WindowFlasher.this.iconFlashTimer.stop();
                    WindowFlasher.this.iconFlashTimer = null;
                    new EDTRunner() {

                        @Override
                        protected void runInEDT() {
                            WindowFlasher.this.set(false);
                        }
                    };

                }
            }
        };
    }

}
