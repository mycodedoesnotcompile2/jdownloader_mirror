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
package org.appwork.app.gui;

import java.awt.Dimension;
import java.awt.Image;
import java.awt.Point;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowFocusListener;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.UIManager;
import javax.swing.WindowConstants;

import org.appwork.shutdown.ShutdownController;
import org.appwork.swing.ExtJFrame;
import org.appwork.swing.action.BasicAction;
import org.appwork.swing.components.ExtButton;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.SwingUtils;
import org.appwork.utils.swing.dialog.AbstractDialog;
import org.appwork.utils.swing.dimensor.RememberLastDimensor;
import org.appwork.utils.swing.locator.RememberAbsoluteLocator;
import org.appwork.utils.swing.windowmanager.WindowManager;
import org.appwork.utils.swing.windowmanager.WindowManager.FrameState;

public abstract class BasicGui {
    public static void main(final String[] args) {
        try {
            for (int i = 2; i >= 0; i--) {
                Thread.sleep(1000);
                System.out.println(i);
            }
        } catch (final InterruptedException e1) {
            e1.printStackTrace();
        }
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                final BasicGui bg = new BasicGui("Test") {
                    @Override
                    protected void layoutPanel() {
                        final ExtButton bt;
                        getFrame().add(bt = new ExtButton(new BasicAction(" button") {
                            /**
                             *
                             */
                            private static final long serialVersionUID = -4007724735998967065L;

                            @Override
                            public void actionPerformed(final ActionEvent e) {
                                new Thread() {
                                    @Override
                                    public void run() {
                                        try {
                                            for (int i = 2; i >= 0; i--) {
                                                Thread.sleep(1000);
                                                System.out.println(i);
                                            }
                                            getFrame().setAlwaysOnTop(false);
                                            WindowManager.getInstance().setZState(getFrame(), FrameState.TO_FRONT_FOCUSED);
                                            // WindowManager.getInstance().toFront(getFrame());
                                            // WindowManager.getInstance().setZState(getFrame(),
                                            // FrameState.TO_FRONT_FOCUSED);
                                            //
                                            // WindowManager.getInstance().toFront(getFrame());
                                            // WindowManager.getInstance().toFront(getFrame());
                                            // Thread.sleep(200);
                                            // WindowManager.getInstance().toFront(getFrame());
                                            // WindowManager.getInstance().toFront(getFrame());
                                            // WindowManager.getInstance().toFront(getFrame());
                                            // WindowManager.getInstance().toFront(getFrame(),
                                            // FrameState.FOCUS);
                                            // Thread.sleep(200);
                                            // WindowManager.getInstance().toFront(getFrame());
                                            // WindowManager.getInstance().toFront(getFrame());
                                            // WindowManager.getInstance().toFront(getFrame());
                                            // WindowManager.getInstance().toFront(getFrame(),
                                            // FrameState.FOCUS);
                                            // Thread.sleep(200);
                                            // WindowManager.getInstance().toFront(getFrame());
                                            // WindowManager.getInstance().toFront(getFrame());
                                            // WindowManager.getInstance().toFront(getFrame());
                                            // WindowManager.getInstance().toFront(getFrame(),
                                            // FrameState.FOCUS);
                                            // Thread.sleep(200);
                                            // WindowManager.getInstance().toFront(getFrame());
                                            // WindowManager.getInstance().toFront(getFrame());
                                            // WindowManager.getInstance().toFront(getFrame());
                                            // WindowManager.getInstance().toFront(getFrame(),
                                            // FrameState.FOCUS);
                                            // Thread.sleep(200);
                                            // WindowManager.getInstance().toFront(getFrame());
                                            // WindowManager.getInstance().toFront(getFrame());
                                            // WindowManager.getInstance().toFront(getFrame());
                                            // WindowManager.getInstance().toFront(getFrame(),
                                            // FrameState.FOCUS);
                                        } catch (final InterruptedException e) {
                                            e.printStackTrace();
                                        }
                                    }
                                }.start();
                            }
                        }));
                        getFrame().addWindowFocusListener(new WindowFocusListener() {
                            @Override
                            public void windowGainedFocus(final WindowEvent windowevent) {
                                bt.setText("JIPPIE! I Got Focused");
                            }

                            @Override
                            public void windowLostFocus(final WindowEvent windowevent) {
                                bt.setText(" :( No Focus para mi");
                            }
                        });
                    }

                    @Override
                    protected void requestExit() {
                        ShutdownController.getInstance().requestShutdown();
                    }
                };
            }
        };
        try {
            Thread.sleep(1000000);
        } catch (final InterruptedException e) {
            e.printStackTrace();
        }
    }

    /**
     * The Mainframe
     */
    private final JFrame            frame;
    private RememberAbsoluteLocator locator;
    private RememberLastDimensor    dimensor;

    protected BasicGui(final String title) {
        frame = new ExtJFrame(title) {
            /**
             *
             */
            private static final long serialVersionUID = -8325715174242107194L;

            @Override
            public void setVisible(final boolean b) {
                // if we hide a frame which is locked by an active modal dialog,
                // we get in problems. avoid this!
                if (!b) {
                    for (final Window w : getOwnedWindows()) {
                        if (w instanceof JDialog && ((JDialog) w).isModal() && w.isActive()) {
                            UIManager.getLookAndFeel().provideErrorFeedback(this);
                            throw new ActiveDialogException((JDialog) w);
                        }
                    }
                    locator.onClose(frame, null);
                    dimensor.onClose(frame, null);
                }
                super.setVisible(b);
            }

            private void superPack() {
                super.pack();
            }

            @Override
            public void pack() {
                superPack();
                if (CrossSystem.isLinux()) {
                    // LinuxWindowManager does invokeLater some setVisible/setZState stuff
                    SwingUtils.invokeLater(new Runnable() {
                        @Override
                        public void run() {
                            superPack();
                        }
                    }, 100);
                }
            }

            @Override
            public void toFront() {
                // if (notToFront) { return; }
                super.toFront();
            }
        };
        // dilaog init
        locator = new RememberAbsoluteLocator(null);
        dimensor = new RememberLastDimensor(null);
        AbstractDialog.setDefaultRoot(frame);
        frame.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(final WindowEvent arg0) {
                locator.onClose(frame, arg0);
                dimensor.onClose(frame, arg0);
                if (!CrossSystem.isMac()) {
                    new Thread("Closer") {
                        @Override
                        public void run() {
                            BasicGui.this.requestExit();
                        }
                    }.start();
                } else {
                    if (BasicGui.this.getFrame().isVisible()) {
                        WindowManager.getInstance().setVisible(BasicGui.this.getFrame(), false);
                    }
                }
            }
        });
        frame.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        // set appicon
        frame.setIconImages(getAppIconList());
        // Set Application dimensions and locations
        frame.setMinimumSize(new Dimension(100, 100));
        final Dimension dim = dimensor.getDimension(frame);
        // restore size
        if (dim != null) {
            frame.setPreferredSize(dim);
        }
        //
        layoutPanel();
        // setGlasPane();
        frame.pack();
        final Point loc = locator.getLocationOnScreen(frame);
        if (loc != null) {
            frame.setLocation(loc);
        }
        WindowManager.getInstance().setVisible(frame, true);
        WindowManager.getInstance().show(frame, FrameState.TO_FRONT_FOCUSED);
    }

    public void dispose() {
        WindowManager.getInstance().setVisible(frame, false);
        frame.dispose();
    }

    /**
     * @return
     */
    protected List<? extends Image> getAppIconList() {
        final java.util.List<Image> list = new ArrayList<Image>();
        return list;
    }

    public JFrame getFrame() {
        return frame;
    }

    /**
     * Creates the whole mainframework panel
     *
     * @throws IOException
     */
    protected abstract void layoutPanel();

    protected abstract void requestExit();
}
