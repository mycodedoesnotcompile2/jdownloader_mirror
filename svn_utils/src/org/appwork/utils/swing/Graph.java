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
package org.appwork.utils.swing;

import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.Timer;

import org.appwork.swing.components.tooltips.ExtTooltip;
import org.appwork.swing.components.tooltips.ToolTipController;
import org.appwork.swing.components.tooltips.ToolTipHandler;
import org.appwork.swing.components.tooltips.TooltipTextDelegateFactory;
import org.appwork.utils.NullsafeAtomicReference;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.locale._AWU;
import org.appwork.utils.swing.graph.Limiter;

/**
 * @author thomas
 *
 */
abstract public class Graph extends JPanel implements ToolTipHandler {

    private static final long                         serialVersionUID = 6943108941655020136L;
    private volatile int                              valueIndex;

    private transient NullsafeAtomicReference<Thread> fetcherThread    = new NullsafeAtomicReference<Thread>(null);
    private int                                       interval         = 1000;

    private final Object                              LOCK             = new Object();

    private Color                                     currentColorTop;
    private Color                                     currentColorBottom;

    protected volatile long                           average;
    protected volatile long                           all;
    protected volatile int                            value;
    protected long[]                                  averageCache;
    protected long[]                                  cache;

    private Color                                     averageColor     = new Color(0x333333);
    private Color                                     averageTextColor = new Color(0);
    private int                                       capacity         = 0;
    private Color                                     textColor        = new Color(0);

    private Font                                      textFont;

    private Limiter[]                                 limiter;
    private TooltipTextDelegateFactory                tooltipFactory;
    private boolean                                   antiAliasing     = false;

    public Graph() {
        this(60, 1000);
    }

    public Graph(final int capacity, final int interval) {
        this.tooltipFactory = new TooltipTextDelegateFactory(this);
        // ToolTipController.getInstance().
        setCurrentColorTop(new Color(100, 100, 100, 40));
        setCurrentColorBottom(new Color(100, 100, 100, 80));
        this.average = 0;
        this.setInterval(interval);
        this.setCapacity(capacity);
        this.setOpaque(false);
        setTextFont(new JLabel().getFont());
    }

    public ExtTooltip createExtTooltip(final Point mousePosition) {
        return this.getTooltipFactory().createTooltip();
    }

    /**
     * @return
     */
    protected String createTooltipText() {

        return this.getAverageSpeedString() + "  " + this.getSpeedString();
    }

    /**
     * @return the averageColor
     */
    public Color getAverageColor() {
        return this.averageColor;
    }

    public long getAverageSpeed() {
        final long all = this.all;
        if (all == 0) {
            return 0;
        } else {
            return this.average / all;
        }
    }

    /**
     * @return
     */
    public String getAverageSpeedString() {
        final long all = this.all;
        if (all <= 0) {
            return null;
        } else {
            return _AWU.T.AppWorkUtils_Graph_getAverageSpeedString2(SizeFormatter.formatBytes(this.average / all));
        }
    }

    /**
     * @return the averageTextColor
     */
    public Color getAverageTextColor() {
        return this.averageTextColor;
    }

    /**
     * @return the colorB
     */
    public Color getCurrentColorBottom() {
        return this.currentColorBottom;
    }

    /**
     * @return the colorA
     */
    public Color getCurrentColorTop() {
        return this.currentColorTop;
    }

    public int getInterval() {
        return this.interval;
    }

    /**
     * @return
     */
    public Limiter[] getLimiter() {
        return this.limiter;
    }

    /**
     * @return
     */
    protected int getPaintHeight() {
        return this.getHeight();
    }

    /**
     * @return
     */
    public String getSpeedString() {        
        if (this.all <= 0) {
            return null;
        }
        return _AWU.T.AppWorkUtils_Graph_getSpeedString(SizeFormatter.formatBytes(this.value));
    }

    /**
     * @return the textColor
     */
    public Color getTextColor() {
        return this.textColor;
    }

    /**
     * @return the textFont
     */
    public Font getTextFont() {
        return this.textFont;
    }

    public int getTooltipDelay(final Point mousePositionOnScreen) {
        return 0;
    }

    public TooltipTextDelegateFactory getTooltipFactory() {
        return this.tooltipFactory;
    }

    /**
     * @return
     */
    abstract public int getValue();

    /**
     * @return the antiAliasing
     */
    public boolean isAntiAliasing() {
        return this.antiAliasing;
    }

    public boolean isTooltipDisabledUntilNextRefocus() {
        return false;
    }

    @Override
    public boolean isTooltipWithoutFocusEnabled() {        
        return true;
    }

    @Override
    public void paintComponent(final Graphics g) {
        super.paintComponent(g);
        this.paintComponent(g, true);
    }

    /**
     * @param g
     * @param b
     */
    public void paintComponent(final Graphics g, final boolean paintText) {
        final Thread thread = this.fetcherThread.get();
        if (thread != null) {
            final Graphics2D g2 = (Graphics2D) g;
            if (!this.antiAliasing) {
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF);
            } else {
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            }
            g2.setStroke(new BasicStroke(1));
            int id = this.valueIndex;
            long max = 10;
            for (final long element : this.cache) {
                max = Math.max(element, max);
            }
            for (final long element : this.averageCache) {
                max = Math.max(element, max);
            }
            final Limiter[] limitertmp = this.getLimiter();
            if (limitertmp != null) {
                for (final Limiter l : limitertmp) {
                    max = Math.max(l.getValue(), max);
                }
            }
            final int polyHeight = this.getPaintHeight();
            final Polygon poly = new Polygon();
            poly.addPoint(0, this.getHeight());
            final Polygon apoly = new Polygon();
            apoly.addPoint(0, this.getHeight());
            final long[] lCache = this.cache;
            final long[] laverageCache = this.averageCache;
            for (int x = 0; x < lCache.length; x++) {
                final int polyX = (x * this.getWidth()) / (lCache.length - 1);
                poly.addPoint(polyX, this.getHeight() - (int) ((polyHeight * lCache[id] * 0.9) / max));
                if (this.averageColor != null) {
                    apoly.addPoint(polyX, this.getHeight() - (int) ((polyHeight * laverageCache[id] * 0.9) / max));
                }
                id++;
                id = id % lCache.length;
            }
            poly.addPoint(this.getWidth(), this.getHeight());
            if (this.averageColor != null) {
                apoly.addPoint(this.getWidth(), this.getHeight());
            }
            g2.setPaint(new GradientPaint(this.getWidth() / 2, this.getHeight() - this.getPaintHeight(), this.currentColorTop, this.getWidth() / 2, this.getHeight(), this.currentColorBottom));
            g2.fill(poly);
            g2.setColor(this.currentColorBottom);
            g2.draw(poly);
            if (this.averageColor != null) {
                ((Graphics2D) g).setColor(this.averageColor);
                final AlphaComposite ac5 = AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.5f);
                g2.setComposite(ac5);
                g2.fill(apoly);
                g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 1.0f));
                g2.draw(apoly);
            }
            if (this.textFont != null) {
                g2.setFont(this.textFont);
            }
            int fontHeight = g2.getFontMetrics().getHeight();
            int textHeight = fontHeight;
            // g2.drawRect(0, 0, getWidth(), getHeight());
            if (limitertmp != null) {
                int h;
                for (final Limiter l : limitertmp) {
                    if (l.getValue() > 0) {
                        h = this.getHeight() - (int) ((polyHeight * l.getValue() * 0.9) / max);
                        h = Math.min(getHeight() - 2, h);
                        // h += (System.currentTimeMillis() / 20) % 20;
                        g2.setPaint(new GradientPaint(this.getWidth() / 2, h, l.getColorA(), this.getWidth() / 2, h + polyHeight / 10, l.getColorB()));
                        g2.fillRect(0, h, this.getWidth(), polyHeight / 10);
                        String str = l.getString();
                        if (StringUtils.isNotEmpty(str)) {
                            int xText = getWidth() - 3 - g2.getFontMetrics().stringWidth(str);
                            g2.setColor(l.getColorA());
                            if (getHeight() - h < fontHeight - 3) {
                                g2.drawString(str, xText, h - fontHeight + 5);
                                textHeight = h - 1;
                            } else if (h > fontHeight - 6) {
                                g2.drawString(str, xText, h - 1);
                                textHeight = h + fontHeight - 4;
                            } else {
                                g2.drawString(str, xText, h + fontHeight + 5);
                                textHeight = h + fontHeight - 4;
                            }
                        }
                    }
                }
            }
            // Draw speed string
            if (paintText) {
                int xText = this.getWidth();
                if (this.textFont != null) {
                    g2.setFont(this.textFont);
                }
                // current speed
                String speedString = this.getSpeedString();
                if (speedString != null && thread != null) {
                    g2.setColor(this.getTextColor());
                    // align left. move right
                    xText = xText - 3 - g2.getFontMetrics().stringWidth(speedString);
                    g2.drawString(speedString, xText, textHeight);
                }
                // average speed
                if (this.getAverageTextColor() != null) {
                    speedString = this.getAverageSpeedString();
                    if (speedString != null && thread != null) {
                        g2.setColor(this.getAverageTextColor());
                        // xText = xText - 3 - g2.getFontMetrics().stringWidth(speedString);
                        g2.drawString(speedString, 0, textHeight);
                    }
                }
            }
        }
    }

    /**
     * resets the average cache and makes sure, that the average recalculates within a few cycles
     */
    protected void resetAverage() {
        final long tmp = this.all;
        if (tmp == 0) {
            this.average = 0;
        } else {
            this.average /= tmp;
        }
        this.average *= 3;
        this.all = 3;
    }

    /**
     * @param antiAliasing
     *            the antiAliasing to set
     */
    public void setAntiAliasing(final boolean antiAliasing) {
        this.antiAliasing = antiAliasing;
    }

    /**
     * @param averageColor
     *            the averageColor to set
     */
    public void setAverageColor(final Color averageColor) {
        this.averageColor = averageColor;
        if (averageColor == null || averageColor.getAlpha() == 0) {
            this.averageColor = null;
        }
    }

    /**
     * @param averageTextColor
     *            the averageTextColor to set
     */
    public void setAverageTextColor(final Color averageTextColor) {
        this.averageTextColor = averageTextColor;
        if (averageTextColor == null || averageTextColor.getAlpha() == 0) {
            this.averageTextColor = null;
        }
    }

    /**
     * @param j
     */
    protected void setCapacity(final int cap) {
        if (this.fetcherThread.get() != null) {
            throw new IllegalStateException("Already started");
        }
        this.capacity = cap;
        this.averageCache = new long[cap];
        this.cache = new long[cap];
    }

    /**
     * @param colorB
     *            the colorB to set
     */
    public void setCurrentColorBottom(final Color colorB) {
        if (colorB != null) {
            this.currentColorBottom = colorB;
        } else {
            this.currentColorBottom = new Color(100, 100, 100, 80);
        }
    }

    /**
     * @param colorA
     *            the colorA to set
     */
    public void setCurrentColorTop(final Color colorA) {
        if (colorA != null) {
            this.currentColorTop = colorA;
        } else {
            this.currentColorTop = new Color(100, 100, 100, 40);
        }
    }

    public void setInterval(final int interval) {
        this.interval = interval;
    }

    public void setLimiter(final Limiter[] limiter) {
        this.limiter = limiter;
    }

    /**
     * @param textColor
     *            the textColor to set
     */
    public void setTextColor(final Color textColor) {
        this.textColor = textColor;
    }

    /**
     * @param textFont
     *            the textFont to set
     */
    public void setTextFont(final Font textFont) {
        this.textFont = textFont;
    }

    public void setTooltipFactory(final TooltipTextDelegateFactory tooltipFactory) {
        this.tooltipFactory = tooltipFactory;
    }

    @Override
    public void setToolTipText(final String text) {
        this.putClientProperty(JComponent.TOOL_TIP_TEXT_KEY, text);
        if (text == null || text.length() == 0) {
            ToolTipController.getInstance().unregister(this);
        } else {
            ToolTipController.getInstance().register(this);
        }
    }

    public void start() {
        synchronized (this.LOCK) {
            Thread thread = this.fetcherThread.get();
            if (thread != null && thread.isAlive()) {
                // already running
                return;
            }
            this.valueIndex = 0;
            thread = new Thread("Speedmeter updater") {

                @Override
                public void run() {
                    Timer painter = null;
                    try {
                        painter = new Timer(Graph.this.getInterval(), new ActionListener() {

                            public void actionPerformed(final ActionEvent e) {
                                synchronized (Graph.this.LOCK) {
                                    Graph.this.setToolTipText(Graph.this.createTooltipText());
                                    Graph.this.repaint();
                                }
                            }
                        });
                        painter.setRepeats(true);
                        painter.setInitialDelay(0);
                        painter.start();
                        Graph.this.all = 0;
                        Graph.this.average = 0;
                        while (true) {
                            synchronized (Graph.this.LOCK) {
                                Graph.this.value = Graph.this.getValue();
                                if (Graph.this.all == Graph.this.cache.length) {
                                    Graph.this.average = Graph.this.average - Graph.this.cache[Graph.this.valueIndex] + Graph.this.value;
                                } else {
                                    Graph.this.average = Graph.this.average + Graph.this.value;
                                }
                                Graph.this.all = Math.min(Graph.this.all + 1, Graph.this.cache.length);
                                Graph.this.averageCache[Graph.this.valueIndex] = (Graph.this.average / Graph.this.all);
                                Graph.this.cache[Graph.this.valueIndex] = Graph.this.value;
                                Graph.this.valueIndex++;
                                Graph.this.valueIndex = Graph.this.valueIndex % Graph.this.cache.length;
                            }
                            if (this.isInterrupted() || Thread.currentThread() != Graph.this.fetcherThread.get()) {
                                break;
                            } else {
                                try {
                                    Thread.sleep(Graph.this.getInterval());
                                } catch (final InterruptedException e) {
                                    break;
                                }
                            }
                        }
                    } finally {
                        synchronized (Graph.this.LOCK) {
                            Graph.this.fetcherThread.compareAndSet(Thread.currentThread(), null);
                            if (painter != null) {
                                painter.stop();
                            }
                        }
                        new EDTRunner() {

                            @Override
                            protected void runInEDT() {
                                Graph.this.repaint();
                            }
                        };
                    }
                }
            };
            thread.setDaemon(true);
            this.fetcherThread.set(thread);
            thread.start();
        }
    }

    public void stop() {
        synchronized (this.LOCK) {
            final Thread oldThread = this.fetcherThread.getAndSet(null);
            if (oldThread != null) {
                oldThread.interrupt();
            }
            Graph.this.repaint();
            this.setCapacity(this.capacity);
        }
    }

    @Override
    public boolean updateTooltip(final ExtTooltip activeToolTip, final MouseEvent e) {
        return false;
    }

}