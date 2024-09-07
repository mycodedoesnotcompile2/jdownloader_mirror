package org.appwork.swing.trayicon;

import java.awt.AWTException;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.SystemTray;
import java.awt.Toolkit;
import java.awt.TrayIcon;
import java.awt.TrayIcon.MessageType;
import java.awt.event.MouseEvent;
import java.awt.geom.AffineTransform;
import java.awt.geom.NoninvertibleTransformException;
import java.awt.geom.Point2D;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;

import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenuItem;
import javax.swing.JSeparator;
import javax.swing.SwingUtilities;

import org.appwork.loggingv3.LogV3;
import org.appwork.resources.IconRef;
import org.appwork.scheduler.DelayedRunnable;
import org.appwork.swing.action.BasicAction;
import org.appwork.utils.images.IconIO;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.EDTRunner;

public abstract class AbstractTray implements TrayMouseListener {
    private static final int POPUP_INSETS = 5;
    protected TrayIcon       trayIcon;
    private TrayMouseAdapter ma;
    private TrayIconPopup    jpopup;
    protected BasicAction[]  actions;
    private DelayedRunnable  doubleclickDelayer;
    private Runnable         executeSingleClick;

    public AbstractTray(BasicAction... basicActions) {
        this.actions = basicActions;
    }

    public void closePopup() {
        final TrayIconPopup localJpopup = this.jpopup;
        if (localJpopup != null) {
            this.jpopup = null;
            localJpopup.setVisible(false);
            localJpopup.dispose();
        }
    }

    public void run() throws AWTException {
        this.runTray();
    }

    public void setToolTip(final String tt) {
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                trayIcon.setToolTip(tt);
            }
        };
    }

    private static final ScheduledExecutorService EXECUTER = Executors.newSingleThreadScheduledExecutor();

    private void runTray() throws AWTException {
        final SystemTray systemTray = SystemTray.getSystemTray();
        this.trayIcon = initTray();
        this.ma = new TrayMouseAdapter(this, this.trayIcon);
        this.trayIcon.addMouseListener(this.ma);
        this.trayIcon.addMouseMotionListener(this.ma);
        initTrayIconImpl(trayIcon);
        this.doubleclickDelayer = new DelayedRunnable(EXECUTER, getDoubleClickDelay()) {
            @Override
            public void delayedrun() {
                if (AbstractTray.this.executeSingleClick != null) {
                    AbstractTray.this.executeSingleClick.run();
                }
            }
        };
        systemTray.add(this.trayIcon);
    }

    protected TrayIcon initTray() {
        final Image img = this.createTrayImage(TrayIconRef.trayicon);
        final TrayIcon trayIcon = new TrayIcon(img, null, null);
        trayIcon.setImageAutoSize(true);
        return trayIcon;
    }

    /**
     * @param trayIcon2
     */
    protected abstract void initTrayIconImpl(TrayIcon trayIcon);

    /**
     * @return
     */
    protected long getDoubleClickDelay() {
        return 300;
    }

    /**
     * @param systemTray
     * @return
     */
    protected Image createTrayImage(IconRef id) {
        final SystemTray systemTray = SystemTray.getSystemTray();
        final Image img = IconIO.getScaledInstance(getImage(id), (int) systemTray.getTrayIconSize().getWidth(), (int) systemTray.getTrayIconSize().getHeight());
        return img;
    }

    protected Image getImage(IconRef id) {
        return id.image(-1);
    }

    protected Image getIcon(IconRef id) {
        return id.image(-1);
    }

    public void showAbout(MouseEvent mouseevent) {
    }

    @Override
    public void mouseClicked(final MouseEvent e) {
        if (!SwingUtilities.isEventDispatchThread()) {
            new Exception().printStackTrace();
        }
        if (CrossSystem.isContextMenuTrigger(e)) {
            onContextClick(e);
        } else if (e.getClickCount() == 1) {
            this.executeSingleClick = new Runnable() {
                @Override
                public void run() {
                    new EDTRunner() {
                        @Override
                        protected void runInEDT() {
                            onSingleClick(e);
                        }
                    };
                }
            };
            this.doubleclickDelayer.resetAndStart();
        } else if (e.getClickCount() == 2) {
            this.doubleclickDelayer.stop();
            this.executeSingleClick = new Runnable() {
                @Override
                public void run() {
                    new EDTRunner() {
                        @Override
                        protected void runInEDT() {
                            onDoubleClick(e);
                        }
                    };
                }
            };
            this.doubleclickDelayer.resetAndStart();
        }
    }

    /**
     * @param e
     */
    protected void onContextClick(MouseEvent e) {
        showMenu(e);
    }

    /**
     * @param e
     */
    protected void onDoubleClick(MouseEvent e) {
        this.showAbout(e);
    }

    protected void onSingleClick(MouseEvent e) {
        showMenu(e);
    }

    public static Point calculateLocation(final Container popup, final TrayMouseAdapter ma, final MouseEvent e) {
        final Dimension ps = popup.getPreferredSize();
        final GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        final GraphicsDevice[] screens = ge.getScreenDevices();
        Point position = e.getPoint();
        for (final GraphicsDevice screen : screens) {
            final GraphicsConfiguration screenConfiguration = screen.getDefaultConfiguration();
            Point clickedPoint = e.getPoint();
            double scaleY = 1;
            double scaleX = 1;
            try {
                // OS DPI Scaling support.
                final AffineTransform transformation = screenConfiguration.getDefaultTransform();
                final Point2D p2d = transformation.inverseTransform(clickedPoint, null);
                clickedPoint = new Point((int) p2d.getX(), (int) p2d.getY());
                scaleY = transformation.getScaleY();
                scaleX = transformation.getScaleX();
            } catch (NoninvertibleTransformException e1) {
                LogV3.log(e1);
            }
            final Rectangle bounds = screenConfiguration.getBounds();
            if (bounds.contains(clickedPoint)) {
                final Insets insets = Toolkit.getDefaultToolkit().getScreenInsets(screenConfiguration);
                if (clickedPoint.getX() - bounds.getX() > bounds.getX() + bounds.getWidth() - clickedPoint.getX()) {
                    // right
                    position.x = (int) (bounds.x + bounds.width - ps.width - POPUP_INSETS / scaleX - insets.right / scaleX);
                    if (position.x > clickedPoint.x) {
                        // don't show at right corner, better place at mouse position
                        position.x = clickedPoint.x;
                    }
                } else {
                    // left
                    position.x = (int) (bounds.x + POPUP_INSETS / scaleX + insets.left / scaleX);
                    if (position.x < clickedPoint.x) {
                        // don't show at left corner, better place at mouse position
                        position.x = clickedPoint.x;
                    }
                }
                if (clickedPoint.getY() - bounds.getY() > (bounds.getY() + bounds.getHeight() - clickedPoint.getY())) {
                    // bottom
                    position.y = (int) (bounds.y + bounds.height - ps.height - POPUP_INSETS / scaleY - insets.bottom / scaleY);
                } else {
                    // top
                    position.y = (int) (bounds.y + POPUP_INSETS / scaleY + insets.top / scaleY);
                    if (position.y < clickedPoint.y) {
                        // better place at mouse position
                        position.y = clickedPoint.y;
                    }
                }
                break;
            }
        }
        return position;
    }

    public void showMenu(final MouseEvent e) {
        final TrayIconPopup localJpopup = this.jpopup;
        if (localJpopup != null && localJpopup.isShowing()) {
            localJpopup.setVisible(false);
            localJpopup.dispose();
            this.jpopup = null;
        } else {
            AbstractTray.this.jpopup = AbstractTray.this.createMenu(e);
            int countMenuItems = 0;
            JMenuItem theOneAndOnly = null;
            for (Component c : AbstractTray.this.jpopup.getComponents()) {
                if (c instanceof JMenuItem) {
                    countMenuItems++;
                    if (countMenuItems == 1) {
                        theOneAndOnly = (JMenuItem) c;
                    } else {
                        theOneAndOnly = null;
                    }
                }
            }
            if (theOneAndOnly != null) {
                theOneAndOnly.doClick();
            } else {
                final Point position = calculateLocation(AbstractTray.this.jpopup, ma, e);
                if (position != null) {
                    AbstractTray.this.jpopup.show(position.x, position.y);
                }
            }
        }
    }

    @Override
    public void mouseDragged(MouseEvent arg0) {
    }

    @Override
    public void mouseMoved(MouseEvent arg0) {
    }

    @Override
    public void mouseEntered(MouseEvent arg0) {
    }

    @Override
    public void mouseExited(MouseEvent arg0) {
    }

    @Override
    public void mousePressed(MouseEvent e) {
    }

    protected TrayIconPopup createMenu(MouseEvent e) {
        TrayIconPopup jpopup = createPopup(e);
        MenuHeaderWrapper header;
        jpopup.add(header = new MenuHeaderWrapper(createMenuHeader(e)));
        header.setOpaque(false);
        header.setBackground(null);
        createMenuNormal(e, jpopup);
        return jpopup;
    }

    protected void addMenuEntry(final TrayIconPopup jpopup, MouseEvent e, BasicAction a) {
        JMenuItem m = createMenuItem(a);
        jpopup.add(m);
    }

    protected void createMenuNormal(MouseEvent e, TrayIconPopup jpopup) {
        for (BasicAction a : this.actions) {
            if (a == null) {
                jpopup.add(new JSeparator());
                continue;
            }
            if (isActionHidden(a)) {
                continue;
            }
            addMenuEntry(jpopup, e, a);
        }
    }

    public boolean isActionHidden(BasicAction a) {
        return Boolean.TRUE.equals(a.getValue("debug"));
    }

    protected JMenuItem createMenuItem(BasicAction a) {
        final JMenuItem m;
        if (a.isToggle()) {
            m = new JCheckBoxMenuItem(a);
        } else {
            m = new JMenuItem(a);
        }
        m.setPreferredSize(new Dimension(m.getPreferredSize().width, 24));
        return m;
    }

    protected TrayIconPopup createPopup(MouseEvent e) {
        return new TrayIconPopup();
    }

    /**
     * @param e
     * @return
     */
    public abstract MenuHeader createMenuHeader(MouseEvent e);

    @Override
    public void mouseReleased(MouseEvent e) {
        // if (e.isPopupTrigger()) {
        // Dimension ps = jpopup.getPreferredSize();
        //
        // jpopup.setLocation(e.getX(), e.getY()-(int)ps.getHeight());
        // jpopup.setInvoker(jpopup);
        // jpopup.setVisible(true);
        // }
    }

    public void mouseStay(MouseEvent me) {
    }

    public void setName(final String trayTitle) {
        AbstractTray.this.trayIcon.setToolTip(trayTitle);
    }

    /**
     * @param string
     * @param msg
     */
    public void showMessage(String title, String msg) {
        trayIcon.displayMessage(title, msg, MessageType.WARNING);
    }
}
