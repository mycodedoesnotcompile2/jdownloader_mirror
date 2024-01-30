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
package org.appwork.swing.components.circlebar.ide;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.BoundedRangeModel;
import javax.swing.JButton;
import javax.swing.JProgressBar;

import org.appwork.app.gui.BasicGui;
import org.appwork.resources.IconRefImpl;
import org.appwork.swing.components.circlebar.CircledProgressBar;
import org.appwork.swing.components.circlebar.ImagePainter;
import org.appwork.swing.components.tooltips.ExtTooltip;
import org.appwork.swing.components.tooltips.TooltipFactory;
import org.appwork.swing.components.tooltips.TooltipPanel;
import org.appwork.utils.Application;
import org.appwork.utils.swing.EDTRunner;

import net.miginfocom.swing.MigLayout;

/**
 * @author thomas
 *
 */
public class CircledProgressBarExample {
    private static boolean RUNNING = true;

    public static void main(final String[] args) {
        Application.setApplication(".test");
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                new BasicGui("CircledProgressBar") {
                    @Override
                    protected void layoutPanel() {
                        final JProgressBar bar = new JProgressBar(0, 100);
                        bar.setToolTipText("BLA");
                        final BoundedRangeModel model = bar.getModel();
                        final CircledProgressBar cbar = new CircledProgressBar(model);
                        cbar.setTooltipFactory(new TooltipFactory() {
                            @Override
                            public ExtTooltip createTooltip() {
                                final ExtTooltip tt = new ExtTooltip() {
                                    /**
                                     *
                                     */
                                    private static final long serialVersionUID = -1978297969679347066L;

                                    @Override
                                    public TooltipPanel createContent() {
                                        final TooltipPanel p = new TooltipPanel("ins 5,wrap 1", "[]", "[]");
                                        p.add(new JButton(new AbstractAction() {
                                            /**
                                             *
                                             */
                                            private static final long serialVersionUID = 5385975776993345514L;
                                            {
                                                putValue(Action.NAME, "Drück mich alder!");
                                            }

                                            @Override
                                            public void actionPerformed(final ActionEvent e) {
                                            }
                                        }));
                                        return p;
                                    }

                                    @Override
                                    public String toText() {
                                        return null;
                                    }
                                };
                                return tt;
                            }
                        });
                        cbar.setOpaque(false);
                        final CircledProgressBar iconBar = new CircledProgressBar(model);
                        iconBar.setPreferredSize(new Dimension(48, 32));
                        final ImagePainter painter = new ImagePainter(new IconRefImpl("close").icon(32), 1.0f);
                        iconBar.setValueClipPainter(painter);
                        painter.setBackground(Color.GREEN);
                        iconBar.setNonvalueClipPainter(new ImagePainter(new IconRefImpl("close").icon(32), 0.3f));
                        final CircledProgressBar test = new CircledProgressBar();
                        final ImagePainter valuePainter = new ImagePainter(new IconRefImpl("dev").icon(32), 1.0f);
                        // valuePainter.setForeground(Color.BLACK);
                        final ImagePainter nonvaluePainter = new ImagePainter(new IconRefImpl("dev").icon(32), 0.3f);
                        test.setValueClipPainter(valuePainter);
                        test.setNonvalueClipPainter(nonvaluePainter);
                        test.setMaximum(360);
                        test.setToolTipText("Blabla Leberkäs");
                        test.setValue(90);
                        test.setBorder(BorderFactory.createMatteBorder(1, 1, 1, 1, Color.ORANGE));
                        iconBar.setBorder(BorderFactory.createMatteBorder(1, 1, 1, 1, Color.ORANGE));
                        bar.setBorder(BorderFactory.createMatteBorder(1, 1, 1, 1, Color.ORANGE));
                        cbar.setBorder(BorderFactory.createMatteBorder(1, 1, 1, 1, Color.ORANGE));
                        painter.setForeground(Color.RED);
                        getFrame().getContentPane().setLayout(new MigLayout("ins 4, wrap 3", "[][][grow,fill]", "[grow,fill,32!]"));
                        getFrame().getContentPane().add(cbar, "height 32!,width 32!");
                        getFrame().getContentPane().add(iconBar, "height 32!,width 128!");
                        getFrame().getContentPane().add(bar);
                        getFrame().getContentPane().add(test, "height 64!,width 64!");
                        getFrame().getContentPane().add(new JButton(new AbstractAction() {
                            /**
                             *
                             */
                            private static final long serialVersionUID = -7967957296219315456L;
                            {
                                putValue(Action.NAME, "Toggle Indeterminated");
                            }

                            @Override
                            public void actionPerformed(final ActionEvent e) {
                                final boolean in = !iconBar.isIndeterminate();
                                iconBar.setIndeterminate(in);
                                bar.setIndeterminate(in);
                                cbar.setIndeterminate(in);
                            }
                        }));
                        JButton bt;
                        getFrame().getContentPane().add(bt = new JButton(new AbstractAction() {
                            /**
                             *
                             */
                            private static final long serialVersionUID = -7726007502976853379L;
                            {
                                putValue(Action.NAME, "Toggle RUN");
                            }

                            @Override
                            public void actionPerformed(final ActionEvent e) {
                                CircledProgressBarExample.RUNNING = !CircledProgressBarExample.RUNNING;
                            }
                        }));
                        bt.setToolTipText("BLA2");
                        new Thread(new Runnable() {
                            @Override
                            public void run() {
                                final int direction = 1;
                                while (true) {
                                    try {
                                        Thread.sleep(200);
                                        if (CircledProgressBarExample.RUNNING) {
                                            new EDTRunner() {
                                                @Override
                                                protected void runInEDT() {
                                                    model.setValue(model.getValue() + direction);
                                                    System.out.println(model.getValue());
                                                    iconBar.setToolTipText((int) (Math.random() * 100) + " %");
                                                    if (Math.random() < 0.1) {
                                                        iconBar.setToolTipText("lfdsifgsdkbfd sdkf jhdsafjhsafgj sdafsjdhfga jsdfgahjd gkj");
                                                    }
                                                    if (Math.random() < 0.1) {
                                                        iconBar.setToolTipText("lfd\r\nsifgs\r\ndkb\r\nfd sdkf\r\n jhdsafj\r\nhsafgj\r\n sdafsjd\r\nhfga \r\njsdfgahj\r\nd gkj");
                                                    }
                                                    if (model.getValue() == model.getMaximum() || model.getValue() == model.getMinimum()) {
                                                        model.setValue(0);
                                                    }
                                                }
                                            };
                                        }
                                    } catch (final InterruptedException e) {
                                        e.printStackTrace();
                                    }
                                }
                            }
                        }).start();
                    }

                    @Override
                    protected void requestExit() {
                        System.exit(1);
                    }
                };
            }
        };
    }
}
