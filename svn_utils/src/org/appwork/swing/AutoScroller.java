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

import java.awt.Dimension;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Insets;
import java.awt.MouseInfo;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Window;
import java.util.ArrayList;

import org.appwork.utils.swing.EDTHelper;

public class AutoScroller extends Thread {

    private Window  window;
    private Point   lastPoint;
    private long    lastMoveTime;
    private boolean editing;

    /**
     * @param dialog
     */
    public AutoScroller(Window dialog) {
        this.window = dialog;
    }

    @Override
    public void run() {

        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        final java.util.List<ExtScreen> screens = new ArrayList<ExtScreen>();
        for (GraphicsDevice gs : ge.getScreenDevices()) {
           
            screens.add(ExtScreen.create(gs.getDefaultConfiguration()));

        }
        lastPoint = new Point(0, 0);
        try {
            final Dimension dim = new Dimension();
            final Rectangle rec = new Rectangle();
            while (true) {
                Thread.sleep(50);

                final Point m = MouseInfo.getPointerInfo().getLocation();
                if (!editing && (m.x != lastPoint.x || m.y != lastPoint.y)) {
                    lastMoveTime = System.currentTimeMillis();
                    lastPoint = m;
                }
                if (System.currentTimeMillis() - lastMoveTime < getDelay()) continue;
                editing = false;
                new EDTHelper<Object>() {

                    @Override
                    public Object edtRun() {
                        if (window.isVisible()) {

                            Point wLoc = window.getLocationOnScreen();
                            window.getSize(dim);
                            Insets insets = window.getInsets();

                            rec.x = wLoc.x;
                            rec.y = wLoc.y;
                            rec.width = dim.width;
                            rec.height = dim.height;
                            for (ExtScreen screen : screens) {

                                int x = wLoc.x;
                                int y = wLoc.y;

                                if (screen.getX() + screen.getWidth() - m.x >= 0 && screen.getX() + screen.getWidth() - m.x < getActiveBorder() + screen.getInsets().right) {
                                    // right

                                    // move to left;
                                    if (rec.x + rec.width + insets.left + insets.right > screen.getX() + screen.getWidth() - screen.getInsets().right) {
                                        int dest = screen.getX() + screen.getWidth() - screen.getInsets().right - rec.width;

                                        if ((dest - wLoc.x) / getSpeed() != 0) {
                                            x = wLoc.x + Math.min(( getActiveBorder() + screen.getInsets().right-(screen.getX() + screen.getWidth() - m.x))/2, (dest - wLoc.x) / getSpeed());

                                        } else {
                                            x = dest;

                                        }

                                    }

                                } else if (m.x - screen.getX() >= 0 && m.x - screen.getX() < getActiveBorder() + screen.getInsets().left) {
                                    // left

                                    if (rec.x - insets.left < screen.getX() + screen.getInsets().left) {
                                        int dest = screen.getX() + screen.getInsets().left;
                                        if ((dest - wLoc.x) / getSpeed() != 0) {
                                            x = wLoc.x + Math.min((getActiveBorder() + screen.getInsets().left-(m.x - screen.getX()))/2, (dest - wLoc.x) / getSpeed());
                                        } else {
                                            x = dest;
                                        }
                                    }
                                }

                                if (screen.getY() + screen.getHeight() - m.y >= 0 && screen.getY() + screen.getHeight() - m.y < getActiveBorder() + screen.getInsets().bottom) {
                                    // bottom
                                    if (rec.y + rec.height > screen.getY() + screen.getHeight() - screen.getInsets().bottom) {
                                        int dest = screen.getY() + screen.getHeight() - rec.height - screen.getInsets().bottom;

                                        if ((dest - wLoc.y) / getSpeed() != 0) {

                                            y = wLoc.y - Math.min((getActiveBorder() + screen.getInsets().bottom - (screen.getY() + screen.getHeight() - m.y))/2, (wLoc.y - dest) / getSpeed());
                                        } else {
                                            y = dest;
                                        }
                                        // y=dest;
                                    }
                                } else if (m.y - screen.getY() >= 0 && m.y - screen.getY() < getActiveBorder() + screen.getInsets().top) {
                                    // top
                                    if (rec.y < screen.getY() + screen.getInsets().top) {
                                        int dest = screen.getY() + screen.getInsets().top;
                                        if ((dest - wLoc.y) / getSpeed() != 0) {

                                            
                                            y = wLoc.y + Math.min((getActiveBorder() + screen.getInsets().top-(m.y - screen.getY()))/2, (dest - wLoc.y) / getSpeed());
                                        } else {
                                            y = dest;
                                        }
                                    }
                                }

                                if (x != wLoc.x || y != wLoc.y) {
                                    window.setBounds(x, y, rec.width, rec.height);
                                    editing = true;
                                    break;
                                }
                            }

                        }
                        return null;
                    }
                }.waitForEDT();

            }
        } catch (InterruptedException e) {
            return;
        }
    }

    /**
     * @return
     */
    protected int getActiveBorder() {

        return 64;
    }

    /**
     * @return
     */
    private long getDelay() {
      
        return 200;
    }


    /**
     * @return
     */
    protected int getSpeed() {

        return 4;
    }
}
