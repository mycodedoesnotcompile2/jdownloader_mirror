//    jDownloader - Downloadmanager
//    Copyright (C) 2008  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package org.appwork.swing.trayicon;

import java.awt.MouseInfo;
import java.awt.Point;
import java.awt.PointerInfo;
import java.awt.Window;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.JPopupMenu;
import javax.swing.Popup;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.swing.EDTHelper;
import org.appwork.utils.swing.EDTRunner;

public final class TrayIconPopup extends JPopupMenu implements MouseListener {
    private static final long serialVersionUID  = 2623190748929934409L;
    private boolean           enteredPopup;
    private boolean           hideThreadrunning = false;
    private transient Thread  hideThread;

    public void setVisible(boolean b) {
        super.setVisible(b);
        if (b) {
            startAutoHide();
        } else {
            hideThreadrunning = false;
        }
    }

    public TrayIconPopup() {
        super();
        setInvoker(this);
        addMouseListener(this);
    }

    /**
     * start autohide in 3 secs if mouse did not enter popup before
     */
    public void startAutoHide() {
        new Thread() {
            @Override
            public void run() {
                try {
                    Thread.sleep(5000);
                } catch (InterruptedException e) {
                }
                if (!enteredPopup) {
                    new EDTHelper<Object>() {
                        @Override
                        public Object edtRun() {
                            setVisible(false);
                            return null;
                        }
                    }.start();
                }
            }
        }.start();
        hideThread = new Thread() {
            /*
             * this thread handles closing of popup because enter/exit/move events are too slow and can miss the exitevent
             */
            public void run() {
                while (hideThreadrunning) {
                    try {
                        sleep(500);
                    } catch (InterruptedException e) {
                    }
                    new EDTRunner() {
                        @Override
                        protected void runInEDT() {
                            if (enteredPopup && hideThreadrunning) {
                                PointerInfo mouse = MouseInfo.getPointerInfo();
                                try {
                                    Point location = getLocationOnScreen();
                                    Point mouseLocation = mouse.getLocation();
                                    boolean activeFrame = false;
                                    // find sub menus in the try icon
                                    for (Window w : Window.getWindows()) {
                                        try {
                                            if (!w.isShowing()) {
                                                continue;
                                            }
                                            String cls = w.getClass().getName();
                                            if (cls.startsWith(Popup.class.getName())) {
                                                boolean active = w.getBounds().contains(mouseLocation);
                                                if (active) {
                                                    activeFrame = true;
                                                    break;
                                                }
                                            }
                                        } catch (Throwable e) {
                                            LogV3.log(e);
                                        }
                                    }
                                    if (activeFrame) {
                                        return;
                                    }
                                    if (mouse.getLocation().x < location.x || mouse.getLocation().x > location.x + TrayIconPopup.this.getSize().width) {
                                        setVisible(false);
                                    } else if (mouse.getLocation().y < location.y || mouse.getLocation().y > location.y + TrayIconPopup.this.getSize().height) {
                                        setVisible(false);
                                    }
                                } catch (Exception e) {
                                    setVisible(false);
                                }
                            }
                        }
                    }.waitForEDT();
                }
            }
        };
        hideThreadrunning = true;
        hideThread.start();
    }

    public void mouseClicked(MouseEvent e) {
    }

    public void mouseEntered(MouseEvent e) {
        enteredPopup = true;
    }

    public void mouseExited(MouseEvent e) {
    }

    public void mousePressed(MouseEvent e) {
    }

    public void mouseReleased(MouseEvent e) {
    }

    public void dispose() {
        setVisible(false);
    }

    public void show(int x, int y) {
        setLocation(x, y);
        setVisible(true);
    }
}