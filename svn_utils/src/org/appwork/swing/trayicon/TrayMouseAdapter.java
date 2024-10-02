//    jDownloader - Downloadmanager
//    Copyright (C) 2009  JD-Team support@jdownloader.org
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

import java.awt.Component;
import java.awt.Dimension;
import java.awt.MouseInfo;
import java.awt.Point;
import java.awt.PointerInfo;
import java.awt.Rectangle;
import java.awt.TrayIcon;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;

import javax.swing.SwingUtilities;

import org.appwork.utils.Time;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.SwingUtils;

public class TrayMouseAdapter implements MouseListener, MouseMotionListener {
    private final TrayMouseListener deligate;
    private boolean                 mouseover;
    protected volatile Thread       mouseLocationObserver;
    // private TrayIcon trayIcon;
    private Point                   min;
    private Point                   max;
    private final Dimension         size;
    private MouseEvent              lastEvent;
    private final Component         dummy;
    private Rectangle               bounds;
    private TrayIcon                trayIcon;
    private static int              TOOLTIP_DELAY = 1000;

    public TrayMouseAdapter(TrayMouseListener lightTray, TrayIcon trayIcon) {
        this.deligate = lightTray;
        this.trayIcon = trayIcon;
        this.dummy = new Component() {
            private static final long serialVersionUID = 1L;
        };
        this.size = trayIcon.getSize();
    }

    @Override
    public void mouseClicked(MouseEvent e) {
        this.deligate.mouseClicked(e);
    }

    @Override
    public void mouseEntered(MouseEvent e) {
        this.mouseover = true;
        final long enterTime = Time.systemIndependentCurrentJVMTimeMillis();
        this.mouseLocationObserver = new Thread("Mouse Over Observer") {
            private boolean mouseStay;
            private boolean loop;

            @Override
            public void run() {
                try {
                    mouseStay = false;
                    loop = true;
                    while (loop) {
                        new EDTRunner() {
                            @Override
                            protected void runInEDT() {
                                final PointerInfo pi = MouseInfo.getPointerInfo();
                                if (pi == null) {
                                    return;
                                }
                                if (bounds == null) {
                                    return;
                                }
                                final Point point = SwingUtils.convertToUnscaled(pi.getDevice(), pi.getLocation());
                                if (!TrayMouseAdapter.this.isOver(point)) {
                                    final MouseEvent me = new MouseEvent(TrayMouseAdapter.this.dummy, 0, Time.systemIndependentCurrentJVMTimeMillis(), 0, point.x, point.y, 0, false);
                                    me.setSource(TrayMouseAdapter.this.lastEvent.getSource());
                                    TrayMouseAdapter.this.mouseExited(me);
                                    loop = false;
                                    return;
                                } else {
                                    if ((Time.systemIndependentCurrentJVMTimeMillis() - enterTime) >= TOOLTIP_DELAY && !mouseStay) {
                                        mouseStay = true;
                                        final MouseEvent me = new MouseEvent(TrayMouseAdapter.this.dummy, 0, Time.systemIndependentCurrentJVMTimeMillis(), 0, point.x, point.y, 0, false);
                                        me.setSource(TrayMouseAdapter.this);
                                        TrayMouseAdapter.this.deligate.mouseStay(me);
                                    }
                                }
                            }
                        }.waitForEDT();
                        Thread.sleep(100);
                    }
                } catch (InterruptedException e) {
                    e.printStackTrace();
                    return;
                } finally {
                    TrayMouseAdapter.this.mouseLocationObserver = null;
                }
            }
        };
        this.mouseLocationObserver.start();
        this.deligate.mouseEntered(e);
    }

    @Override
    public void mouseExited(MouseEvent e) {
        if (!SwingUtilities.isEventDispatchThread()) {
            new Exception().printStackTrace();
        }
        this.mouseover = false;
        System.out.println("Exit");
        this.min = this.max = null;
        bounds = null;
        this.deligate.mouseExited(e);
    }

    @Override
    public void mousePressed(MouseEvent e) {
        if (!SwingUtilities.isEventDispatchThread()) {
            new Exception().printStackTrace();
        }
        this.deligate.mousePressed(e);
    }

    @Override
    public void mouseReleased(MouseEvent e) {
        if (!SwingUtilities.isEventDispatchThread()) {
            new Exception().printStackTrace();
        }
        this.deligate.mouseReleased(e);
    }

    @Override
    public void mouseDragged(MouseEvent e) {
        if (!SwingUtilities.isEventDispatchThread()) {
            new Exception().printStackTrace();
        }
        this.deligate.mouseDragged(e);
    }

    @Override
    public void mouseMoved(MouseEvent e) {
        if (!SwingUtilities.isEventDispatchThread()) {
            new Exception().printStackTrace();
        }
        if (e == null || e.getPoint() == null) {
            // workaround. It seems like sometimes e.getPoint()==null
            return;
        }
        this.lastEvent = e;
        /**
         * the more the user moves over the tray, the better we know it's location *
         */
        if (this.min == null) {
            this.min = new Point(e.getPoint().x, e.getPoint().y);
            this.max = new Point(e.getPoint().x, e.getPoint().y);
        } else {
            this.min.x = Math.min(e.getPoint().x, this.min.x);
            this.min.y = Math.min(e.getPoint().y, this.min.y);
            this.max.x = Math.max(e.getPoint().x, this.max.x);
            this.max.y = Math.max(e.getPoint().y, this.max.y);
            //
        }
        bounds = new Rectangle(min.x, min.y, max.x - min.x, max.y - min.y);
        if (bounds.width < size.width) {
            bounds.x -= (size.width - bounds.width) / 2;
            bounds.width = size.width;
        }
        if (bounds.height < size.height) {
            bounds.y -= (size.height - bounds.height) / 2;
            bounds.height = size.height;
        }
        if (!this.mouseover) {
            this.mouseEntered(e);
        } else {
            this.deligate.mouseMoved(e);
        }
    }

    /**
     *
     *
     * @param point
     * @return
     */
    protected boolean isOver(Point point) {
        final Rectangle bounds = this.bounds;
        if (bounds == null) {
            return false;
        } else {
            return bounds.contains(point);
        }
    }

    public Rectangle getUnscaledBounds() {
        return bounds;
    }
}
