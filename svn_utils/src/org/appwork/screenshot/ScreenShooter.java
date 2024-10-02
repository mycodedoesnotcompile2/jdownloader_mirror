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
package org.appwork.screenshot;

import java.awt.AWTException;
import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Composite;
import java.awt.Cursor;
import java.awt.DisplayMode;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.Toolkit;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.geom.Area;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferStrategy;
import java.awt.image.BufferedImage;
import java.awt.image.MemoryImageSource;
import java.awt.image.VolatileImage;

import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JWindow;

import org.appwork.swing.components.tooltips.ToolTipController;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.EDTHelper;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;

/**
 * @author thomas
 *
 */
public class ScreenShooter extends JWindow implements MouseListener, MouseMotionListener {
    private static final long    serialVersionUID = 3184465232251321247L;
    /**
     * Size of the Mag Glass
     */
    private static final int     SIZE             = 150;
    /**
     * Mag resize factor
     */
    private static final int     FACTOR           = 5;
    private static final int     SCALED_SIZE      = ScreenShooter.SIZE / ScreenShooter.FACTOR;
    protected static final int   FPS              = 50;
    private static ScreenShooter layover;

    /**
     * Creates a screenshot of all available screens. and returns the ScreenShooter
     *
     * @return
     * @throws AWTException
     */
    public static ScreenShooter create() throws AWTException {
        final GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        final GraphicsDevice[] screens = ge.getScreenDevices();
        // for (final GraphicsDevice screen : screens) {
        int xMax = 0;
        int xMin = 0;
        int yMax = 0;
        int yMin = 0;
        for (final GraphicsDevice screen : screens) {
            final Rectangle bounds = screen.getDefaultConfiguration().getBounds();
            xMax = Math.max(xMax, bounds.x + bounds.width);
            yMax = Math.max(bounds.y + bounds.height, yMax);
            yMin = Math.min(yMin, bounds.y);
            xMin = Math.min(xMin, bounds.x);
        }
        // final BufferedImage complete = new BufferedImage(xMax - xMin, yMax -
        // yMin, Transparency.TRANSLUCENT);
        Image complete = null;
        Graphics2D g2 = null;
        if (CrossSystem.isUnix()) {
            final BufferedImage img = new BufferedImage(xMax - xMin, yMax - yMin, BufferedImage.TYPE_INT_RGB);
            g2 = img.createGraphics();
            complete = img;
        } else {
            final VolatileImage img = ge.getDefaultScreenDevice().getDefaultConfiguration().createCompatibleVolatileImage(xMax - xMin, yMax - yMin);
            g2 = img.createGraphics();
            complete = img;
        }
        // we create a normal screenshot and a grayed screenshot
        // final BufferedImage completeGrayed = new BufferedImage(xMax - xMin,
        // yMax - yMin, Transparency.TRANSLUCENT);
        final VolatileImage completeGrayed = ge.getDefaultScreenDevice().getDefaultConfiguration().createCompatibleVolatileImage(xMax - xMin, yMax - yMin);
        Graphics2D g2gray = completeGrayed.createGraphics();
        for (final GraphicsDevice screen : screens) {
            final DisplayMode dm = screen.getDisplayMode();
            // bounds are used to gete the position and size of this screen in
            // the complete screen configuration
            final Rectangle bounds = screen.getDefaultConfiguration().getBounds();
            final int screenWidth = dm.getWidth();
            final int screenHeight = dm.getHeight();
            final Rectangle rect = new Rectangle(screenWidth, screenHeight);
            final Robot robot = new Robot(screen);
            final BufferedImage image = robot.createScreenCapture(rect);
            g2.drawImage(image, bounds.x - xMin, bounds.y - yMin, null);
            g2gray.drawImage(image, bounds.x - xMin, bounds.y - yMin, null);
            final Composite comp = g2gray.getComposite();
            g2gray.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.3f));
            g2gray.setColor(Color.BLACK);
            g2gray.fillRect(bounds.x - xMin, bounds.y - yMin, screenWidth, screenHeight);
            g2gray.drawImage(image, bounds.x - xMin, bounds.y - yMin, null);
            g2gray.setComposite(comp);
        }
        g2.dispose();
        g2gray.dispose();
        g2 = null;
        g2gray = null;
        final ScreenShooter layover = new ScreenShooter(xMin, yMin, xMax, yMax);
        layover.setImage(complete, completeGrayed);
        return layover;
    }

    public static void main(final String[] args) throws AWTException, InterruptedException {
        new EDTHelper<ScreenShooter>() {
            @Override
            public ScreenShooter edtRun() {
                try {
                    ScreenShooter.layover = ScreenShooter.create();
                } catch (final AWTException e) {
                    e.printStackTrace();
                }
                ScreenShooter.layover.start();
                return null;
            }
        }.waitForEDT();
        final BufferedImage screenshot = ScreenShooter.layover.getScreenshot();
        if (screenshot != null) {
            try {
                Dialog.getInstance().showConfirmDialog(0, "", "", new ImageIcon(screenshot), null, null);
            } catch (final DialogClosedException e) {
                e.printStackTrace();
            } catch (final DialogCanceledException e) {
                e.printStackTrace();
            }
        }
        System.exit(0);
    }

    private Image             image;
    private boolean           isDragging = false;
    private Point             dragStart;
    private Point             dragEnd;
    private Image             grayedImage;
    private final Rectangle[] bounds;
    private BufferedImage     screenshot;
    private final JFrame      frame;
    private Point             mouse;
    private boolean           disposed   = false;
    private int               xMin;
    private int               xMax;
    private int               yMin;
    private int               yMax;

    public ScreenShooter(int xMin, int yMin, int xMax, int yMax) {
        super();
        this.xMin = xMin;
        this.xMax = xMax;
        this.yMin = yMin;
        this.yMax = yMax;
        // we extends from a JFrame because JWindow cannot get focus and this
        // cannot listen on key events
        // this.setUndecorated(true);
        this.addMouseListener(this);
        // we need an extra frame to listen for keyevents. jwindow cannot catch
        // key events
        this.frame = new JFrame();
        this.frame.addKeyListener(new KeyListener() {
            @Override
            public void keyPressed(final KeyEvent e) {
            }

            @Override
            public void keyReleased(final KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_ESCAPE) {
                    ScreenShooter.this.cancel();
                }
            }

            @Override
            public void keyTyped(final KeyEvent e) {
            }
        });
        this.frame.setUndecorated(false);
        // this.frame.setSize(0, 0);
        // see
        // http://www.javalobby.org/forums/thread.jspa?threadID=16867&tstart=0
        final GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        final GraphicsDevice[] screens = ge.getScreenDevices();
        // store screen device bounds to find current screen later easily
        this.bounds = new Rectangle[screens.length];
        for (int i = 0; i < this.bounds.length; i++) {
            this.bounds[i] = screens[i].getDefaultConfiguration().getBounds();
        }
    }

    /**
     *
     */
    private void cancel() {
        if (this.isDragging) {
            this.stopDrag();
            this.dragStart = null;
            this.dragEnd = null;
        } else {
            new EDTRunner() {
                @Override
                protected void runInEDT() {
                    ScreenShooter.this.frame.setVisible(false);
                    ScreenShooter.this.frame.dispose();
                    ScreenShooter.this.setVisible(false);
                    ScreenShooter.this.dispose();
                }
            };
        }
    }

    /**
     * Converts yourPoint to a coresponding point in the mag glass
     *
     * @param mouselocation
     * @param mag
     *            glass location
     * @param yourPoint
     * @return
     */
    private Point convertToMagnifier(final Point mag, final Point p) {
        final int tx = (p.x - this.mouse.x) * ScreenShooter.FACTOR + this.mouse.x + mag.x + ScreenShooter.SIZE / 2 - this.mouse.x;
        final int ty = (p.y - this.mouse.y) * ScreenShooter.FACTOR + this.mouse.y + mag.y + ScreenShooter.SIZE / 2 - this.mouse.y;
        return new Point(tx, ty);
    }

    /**
     * Cuts the given range from the image(screenshot)
     *
     * @param x
     * @param y
     * @param x2
     * @param y2
     * @return
     */
    private BufferedImage cut(final int x1, final int y1, final int x2, final int y2) {
        final int width = Math.abs(x1 - x2) + 1;
        final int height = Math.abs(y1 - y2) + 1;
        final int sX = Math.min(x1, x2);
        final int sY = Math.min(y1, y2);
        if (width <= 0 || height <= 0) {
            return null;
        }
        final BufferedImage ret = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        final Graphics2D gb = (Graphics2D) ret.getGraphics();
        gb.drawImage(this.image, 0, 0, width, height, sX, sY, sX + width, sY + height, null);
        gb.dispose();
        return ret;
    }

    @Override
    public void dispose() {
        super.dispose();
        this.disposed = true;
    }

    /**
     * @param gb
     */
    private void drawBigCross(final Graphics2D gb) {
        gb.setStroke(new BasicStroke(1));
        gb.setColor(Color.GRAY);
        final Area clip = new Area(new Rectangle2D.Double(0, 0, this.getWidth(), this.getHeight()));
        final Area subClip = new Area(new Rectangle2D.Double(this.mouse.x - 15, this.mouse.y - 15, 30, 30));
        clip.subtract(subClip);
        gb.setClip(clip);
        final float dash[] = { 10.0f };
        gb.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10.0f, dash, System.currentTimeMillis() / ScreenShooter.FPS % 20));
        gb.drawLine(0, this.mouse.y, this.image.getWidth(null), this.mouse.y);
        gb.drawLine(this.mouse.x, 0, this.mouse.x, this.image.getHeight(null));
        gb.setClip(null);
        // draw tiny cross at mouse location
        gb.setColor(Color.BLACK);
        gb.setStroke(new BasicStroke(1));
        // gb.drawLine(this.mouse.x - 10, this.mouse.y, this.mouse.x + 10,
        // this.mouse.y);
        // gb.drawLine(this.mouse.x, this.mouse.y - 10, this.mouse.x,
        // this.mouse.y + 10);
        gb.drawLine(this.mouse.x, this.mouse.y, this.mouse.x, this.mouse.y);
    }

    /**
     * get the device bounds of the device l is in. Use to find the currently used screen
     *
     *
     * @return
     */
    private Rectangle getDeviceBounds() {
        for (final Rectangle r : this.bounds) {
            if (this.mouse.x >= r.x && this.mouse.x <= r.x + r.width) {
                // x correct
                if (this.mouse.y >= r.y && this.mouse.y <= r.y + r.height) {
                    // y correct
                    return r;
                }
            }
        }
        return null;
    }

    /**
     * @return
     */
    public BufferedImage getFullScreenShot() {
        if (this.image instanceof BufferedImage) {
            return (BufferedImage) this.image;
        }
        final BufferedImage img = new BufferedImage(this.getWidth(), this.getHeight(), BufferedImage.TYPE_INT_RGB);
        Graphics gd = img.getGraphics();
        gd.drawImage(this.image, 0, 0, null);
        gd.dispose();
        return img;
    }

    /**
     * calculates the position of the mag. mag position relative to the mouseposition changes if we reach the screen devices bounds
     *
     * @param l
     * @return
     */
    private Point getMagnifierPosition() {
        final Rectangle bounds = this.getDeviceBounds();
        if (bounds == null) {
            return null;
        }
        int x = this.mouse.x + 20;
        if (x + ScreenShooter.SIZE > bounds.x + bounds.width) {
            x = this.mouse.x - ScreenShooter.SIZE - 20;
        }
        int y = this.mouse.y - ScreenShooter.SIZE - 20;
        if (y < bounds.y) {
            y = this.mouse.y + 20;
        }
        return new Point(x, y);
    }

    /**
     * gets the selected Screenshot. Blocks until a screenshot is available, or the user canceled
     *
     * @return
     * @throws InterruptedException
     */
    public BufferedImage getScreenshot() throws InterruptedException {
        while (this.screenshot == null && !this.disposed) {
            Thread.sleep(100);
        }
        return this.screenshot;
    }

    /*
     * (non-Javadoc)
     *
     * @see java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent)
     */
    @Override
    public void mouseClicked(final MouseEvent e) {
        if (e.isPopupTrigger() || e.getButton() == MouseEvent.BUTTON3) {
            return;
        }
        if (this.isDragging) {
            this.stopDrag();
            this.setVisible(false);
            this.frame.setVisible(false);
            new Thread() {
                @Override
                public void run() {
                    ScreenShooter.this.screenshot = ScreenShooter.this.cut(ScreenShooter.this.dragStart.x, ScreenShooter.this.dragStart.y, ScreenShooter.this.dragEnd.x, ScreenShooter.this.dragEnd.y);
                    ScreenShooter.this.frame.dispose();
                    ScreenShooter.this.dispose();
                }
            }.start();
        } else if (!this.isDragging) {
            this.startDrag(e);
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see java.awt.event.MouseMotionListener#mouseDragged(java.awt.event.MouseEvent )
     */
    @Override
    public void mouseDragged(final MouseEvent e) {
        if (e != null) {
            this.mouse = e.getPoint();
        } else {
            this.mouse = ToolTipController.getMouseLocation();
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see java.awt.event.MouseListener#mouseEntered(java.awt.event.MouseEvent)
     */
    @Override
    public void mouseEntered(final MouseEvent e) {
    }

    /*
     * (non-Javadoc)
     *
     * @see java.awt.event.MouseListener#mouseExited(java.awt.event.MouseEvent)
     */
    @Override
    public void mouseExited(final MouseEvent e) {
    }

    /*
     * (non-Javadoc)
     *
     * @see java.awt.event.MouseMotionListener#mouseMoved(java.awt.event.MouseEvent)
     */
    @Override
    public void mouseMoved(final MouseEvent e) {
        if (e != null) {
            this.mouse = e.getPoint();
        } else {
            this.mouse = ToolTipController.getMouseLocation();
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see java.awt.event.MouseListener#mousePressed(java.awt.event.MouseEvent)
     */
    @Override
    public void mousePressed(final MouseEvent e) {
        if (e.isPopupTrigger() || e.getButton() == MouseEvent.BUTTON3) {
            this.cancel();
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see java.awt.event.MouseListener#mouseReleased(java.awt.event.MouseEvent)
     */
    @Override
    public void mouseReleased(final MouseEvent e) {
        if (e.isPopupTrigger() || e.getButton() == MouseEvent.BUTTON3) {
            return;
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see java.awt.Window#paint(java.awt.Graphics)
     */
    @Override
    public void paint(final Graphics g) {
        // if we do not override this, this might resuzlt in edt freezes for
        // some seconds
    }

    /**
     * Paints the mag, and the position values
     *
     * @param gb
     * @param l
     */
    private void paintMagnifier(final Graphics2D gb) {
        if (mouse == null) {
            return;
        }
        final Point pos = this.getMagnifierPosition();
        // draw and resize the mag image
        gb.drawImage(this.image, pos.x, pos.y, pos.x + ScreenShooter.SIZE, pos.y + ScreenShooter.SIZE, this.mouse.x - ScreenShooter.SCALED_SIZE / 2, this.mouse.y - ScreenShooter.SCALED_SIZE / 2, this.mouse.x + ScreenShooter.SCALED_SIZE / 2, this.mouse.y + ScreenShooter.SCALED_SIZE / 2, Color.BLACK, null);
        // Draws the black alpha cross
        Composite comp = gb.getComposite();
        gb.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.3f));
        gb.setColor(Color.BLACK);
        gb.setStroke(new BasicStroke(5));
        gb.drawLine(pos.x + 2, pos.y + ScreenShooter.SIZE / 2 + 2, pos.x + ScreenShooter.SIZE - 2, pos.y + ScreenShooter.SIZE / 2 + 2);
        // gb.setColor(Color.RED);
        gb.drawLine(pos.x + ScreenShooter.SIZE / 2 + 2, pos.y + 2, pos.x + ScreenShooter.SIZE / 2 + 2, pos.y + ScreenShooter.SIZE / 2 - 3);
        gb.drawLine(pos.x + ScreenShooter.SIZE / 2 + 2, pos.y + ScreenShooter.SIZE / 2 + 7, pos.x + ScreenShooter.SIZE / 2 + 2, pos.y + ScreenShooter.SIZE - 2);
        gb.setComposite(comp);
        //
        if (this.isDragging) {
            // Paint the blue selection rectangle in the mag.
            final int startX = Math.min(this.dragStart.x, this.mouse.x);
            final int startY = Math.min(this.dragStart.y, this.mouse.y);
            final int endX = Math.max(this.mouse.x, this.dragStart.x) + 1;
            final int endY = Math.max(this.mouse.y, this.dragStart.y) + 1;
            final Point start = this.convertToMagnifier(pos, new Point(startX, startY));
            final Point end = this.convertToMagnifier(pos, new Point(endX, endY));
            gb.setStroke(new BasicStroke(1));
            gb.setColor(Color.BLUE);
            final int x = Math.max(pos.x, start.x);
            final int y = Math.max(start.y, pos.y);
            final int width = Math.min(ScreenShooter.SIZE / 2 + (x == pos.x ? ScreenShooter.FACTOR : 0), end.x - start.x);
            final int height = Math.min(ScreenShooter.SIZE / 2 + (y == pos.y ? ScreenShooter.FACTOR : 0), end.y - start.y);
            gb.drawRect(x, y, width, height);
            comp = gb.getComposite();
            gb.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.1f));
            gb.fillRect(x, y, width, height);
            gb.setComposite(comp);
            // if we are dragging we paint a white area to paint the selected
            // are size
            gb.setStroke(new BasicStroke(1));
            gb.setColor(Color.WHITE);
            gb.fillRect(pos.x + 1, pos.y + ScreenShooter.SIZE - gb.getFontMetrics().getHeight(), ScreenShooter.SIZE - 1, gb.getFontMetrics().getHeight());
            gb.setColor(Color.GRAY);
            gb.drawLine(pos.x, pos.y + ScreenShooter.SIZE - gb.getFontMetrics().getHeight(), pos.x + ScreenShooter.SIZE, pos.y + ScreenShooter.SIZE - gb.getFontMetrics().getHeight());
            final String dimension = "(px)W:" + (Math.abs(this.mouse.x - this.dragStart.x) + 1) + "; H:" + (Math.abs(this.mouse.y - this.dragStart.y) + 1);
            gb.getFontMetrics().stringWidth(dimension);
        }
        // paint black border
        gb.setColor(Color.BLACK);
        gb.setStroke(new BasicStroke(1));
        gb.drawRect(pos.x, pos.y, ScreenShooter.SIZE, ScreenShooter.SIZE);
    }

    /**
     * @param complete
     * @param completeGrayed
     */
    private void setImage(final Image complete, final Image completeGrayed) {
        this.image = complete;
        this.grayedImage = completeGrayed;
        this.setSize(complete.getWidth(null), complete.getHeight(null));
    }

    /**
     *
     */
    public void start() {
        new EDTHelper<Object>() {
            @Override
            public Object edtRun() {
                // avoid that this frame shows up
                ScreenShooter.this.frame.setLocation(-10000, -10000);
                ScreenShooter.this.frame.setVisible(true);
                ScreenShooter.this.frame.requestFocus();
                ScreenShooter.this.setVisible(true);
                ScreenShooter.this.setLocation(xMin, yMin);
                ScreenShooter.this.setAlwaysOnTop(false);
                ScreenShooter.this.requestFocus();
                ScreenShooter.this.requestFocusInWindow();
                ScreenShooter.this.createBufferStrategy(2);
                // invisible cursor
                final int[] pixels = new int[16 * 16];
                final Image image = Toolkit.getDefaultToolkit().createImage(new MemoryImageSource(16, 16, pixels, 0, 16));
                final Cursor transparentCursor = Toolkit.getDefaultToolkit().createCustomCursor(image, new Point(0, 0), "invisibleCursor");
                ScreenShooter.this.setCursor(transparentCursor);
                // it might happen, that our window loses focus and does not
                // lsten to mousevents any more. This timer kills the window if
                // there is no mousemove for >15 seconds
                ScreenShooter.this.addMouseMotionListener(ScreenShooter.this);
                ScreenShooter.this.mouseMoved(null);
                // this.timer.start();
                new Thread("Asynchpainter") {
                    @Override
                    public void run() {
                        long t = System.currentTimeMillis();
                        final int frame = 1000 / ScreenShooter.FPS;
                        // Point oldMouse = ScreenShooter.this.mouse;
                        ScreenShooter.this.updateGUI(ScreenShooter.this.getBufferStrategy());
                        try {
                            Thread.sleep(Math.max(0, frame - System.currentTimeMillis() - t));
                        } catch (final InterruptedException e) {
                            e.printStackTrace();
                        }
                        while (!ScreenShooter.this.disposed) {
                            t = System.currentTimeMillis();
                            // if (ScreenShooter.this.mouse != oldMouse) {
                            ScreenShooter.this.updateGUI(ScreenShooter.this.getBufferStrategy());
                            // }
                            // oldMouse = ScreenShooter.this.mouse;
                            try {
                                final long wait = frame - (System.currentTimeMillis() - t);
                                if (wait > 0) {
                                    Thread.sleep(wait);
                                }
                            } catch (final InterruptedException e) {
                                e.printStackTrace();
                            }
                        }
                    }
                }.start();
                return null;
            }
        }.getReturnValue();
    }

    /**
     *
     */
    private void startDrag(final MouseEvent e) {
        if (e != null) {
            this.dragStart = e.getPoint();
        } else {
            this.dragStart = ToolTipController.getMouseLocation();
        }
        this.mouse = dragStart;
        this.isDragging = true;
    }

    /**
     *
     */
    private void stopDrag() {
        this.isDragging = false;
        this.dragEnd = this.mouse;
    }

    /**
     * Paints the complete screen
     *
     * @param bufferStrategy
     */
    private void updateGUI(final BufferStrategy bufferStrategy) {
        Graphics2D gb = null;
        try {
            gb = (Graphics2D) bufferStrategy.getDrawGraphics();
            final Point tempDrag = this.dragStart;
            if (this.isDragging && tempDrag != null) {
                final int startX = Math.min(tempDrag.x, this.mouse.x);
                final int startY = Math.min(tempDrag.y, this.mouse.y);
                // draw grayed image over full screen
                gb.drawImage(this.grayedImage, 0, 0, null);
                final int endX = Math.max(this.mouse.x, tempDrag.x);
                final int endY = Math.max(this.mouse.y, tempDrag.y);
                // draw ungrayed icon as selection
                gb.drawImage(this.image, startX, startY, endX, endY, startX, startY, endX, endY, null);
                gb.setColor(Color.GRAY);
                // draw BIG dashed hair cross
                this.drawBigCross(gb);
                // Draw selection Border
                gb.setColor(Color.BLACK);
                gb.setStroke(new BasicStroke(1));
                gb.drawRect(startX, startY, endX - startX, endY - startY);
                gb.setStroke(new BasicStroke(1));
            } else {
                // draw screenshot image
                gb.drawImage(this.image, 0, 0, null);
                // draw dashed cross
                this.drawBigCross(gb);
            }
            this.paintMagnifier(gb);
            // paint the position marker
            String str = this.mouse.y + " px";
            final Rectangle db = this.getDeviceBounds();
            int width = gb.getFontMetrics().stringWidth(str) + 10;
            int height = gb.getFontMetrics().getHeight() + 5;
            gb.setStroke(new BasicStroke(1));
            gb.setColor(Color.white);
            int y = this.mouse.y - height / 2;
            if (y < db.y) {
                // dock marker on top
                y = db.y;
            }
            if (y + height + 5 > db.height + db.y) {
                y = db.height + db.y - height - 5;
                // dock marker on bottom
            }
            // paint marker
            gb.fillRect(db.x + db.width - width - 10, y, width, height);
            gb.setColor(Color.GRAY);
            gb.drawRect(db.x + db.width - width - 10, y, width, height);
            gb.drawString(str, db.x + db.width - width - 5, y + height - 5);
            //
            str = this.mouse.x + " px";
            width = gb.getFontMetrics().stringWidth(str) + 10;
            height = gb.getFontMetrics().getHeight() + 5;
            gb.setStroke(new BasicStroke(1));
            gb.setColor(Color.white);
            int x = this.mouse.x - width / 2;
            if (x < db.x + 5) {
                x = db.x + 5;
                // marker reached left margin we dock here
            }
            if (x + width + 5 > db.x + db.width) {
                // marker reached right margin. we dock here
                x = db.x + db.width - width - 5;
            }
            // avoid that marker overlap at the bottom right corner. set X
            // marker to
            // top if x and y markers share the same y position
            if (db.y + db.height - height - 5 - y <= height) {
                // on mac, we cannot override the topbar which is 22 px height
                gb.fillRect(x, db.y + (CrossSystem.isMac() ? 22 + 5 : 5), width, height);
                gb.setColor(Color.GRAY);
                gb.drawRect(x, +(CrossSystem.isMac() ? 22 + 5 : 5), width, height);
                gb.drawString(str, x + 5, (CrossSystem.isMac() ? 22 + height : height));
            } else {
                gb.fillRect(x, db.y + db.height - height - 5, width, height);
                gb.setColor(Color.GRAY);
                gb.drawRect(x, db.y + db.height - height - 5, width, height);
                gb.drawString(str, x + 5, db.y + db.height - 10);
            }
            try {
                bufferStrategy.show();
            } catch (final Exception e) {
            }
        } catch (final Exception e) {
            e.printStackTrace();
        } finally {
            try {
                gb.dispose();
            } catch (final Throwable e) {
            }
        }
    }
}
