package jd.gui.swing.dialog;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Font;
import java.awt.Component;
import java.awt.Composite;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.BorderLayout;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.awt.image.IndexColorModel;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;

import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JToggleButton;
import javax.swing.Scrollable;
import javax.swing.SwingConstants;
import javax.swing.Timer;

import org.appwork.utils.URLStream;
import org.appwork.utils.ImageProvider.ImageProvider;
import org.appwork.utils.images.IconIO;
import org.appwork.utils.images.Interpolation;
import org.appwork.utils.swing.dialog.DefaultButtonPanel;
import org.jdownloader.DomainInfo;
import org.jdownloader.captcha.v2.AbstractCaptchaDialog;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.ImageCaptchaChallenge;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.settings.staticreferences.CFG_GUI;
import org.appwork.swing.MigPanel;
import org.jdownloader.gui.views.components.HeaderScrollPane;
import org.jdownloader.updatev2.gui.LAFOptions;

import jd.captcha.utils.GifDecoder;
import net.miginfocom.swing.MigLayout;

public abstract class AbstractImageCaptchaDialog<T> extends AbstractCaptchaDialog<T> {
    public static Image[] getGifImages(InputStream openStream) {
        try {
            GifDecoder decoder = new GifDecoder();
            decoder.read(openStream);
            BufferedImage[] ret = new BufferedImage[decoder.getFrameCount()];
            for (int i = 0; i < decoder.getFrameCount(); i++) {
                ret[i] = decoder.getFrame(i);
                // ImageIO.write(ret[i], "png", Application.getResource("img_" +
                // i + ".png"));
            }
            return ret;
        } finally {
            try {
                openStream.close();
            } catch (final Throwable e) {
            }
        }
    }

    /**
     * @param url
     * @return
     */
    public static Image[] getGifImages(URL url) {
        InputStream stream = null;
        try {
            stream = URLStream.openStream(url);
            return getGifImages(stream);
        } catch (IOException e) {
            org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().log(e);
        } finally {
            try {
                stream.close();
            } catch (final Throwable e) {
            }
        }
        return null;
    }

    int                 fps;
    protected Image[]   images;
    protected Point     offset;
    private int         frame = 0;
    Timer               paintTimer;
    protected Rectangle bounds;
    protected double    scaleFaktor;
    private boolean     greyscale = false;
    private Image       greyscaleSource;
    private Image       greyscaleImage;

    public AbstractImageCaptchaDialog(ImageCaptchaChallenge<T> challenge, int flags, String title, DialogType type, DomainInfo domainInfo, String explain, Image... images) {
        super(challenge, flags, title, type, domainInfo, explain);
        // if we have gif images, but read them as non indexed images, we try to fix this here.
        java.util.List<Image> ret = new ArrayList<Image>();
        if (images != null) {
            for (int i = 0; i < images.length; i++) {
                if (images[i] instanceof BufferedImage) {
                    if (((BufferedImage) images[i]).getColorModel() instanceof IndexColorModel) {
                        ByteArrayOutputStream os = new ByteArrayOutputStream();
                        try {
                            ImageProvider.writeImage((BufferedImage) images[i], "gif", os);
                            InputStream is = new ByteArrayInputStream(os.toByteArray());
                            Image[] subImages = getGifImages(is);
                            for (Image ii : subImages) {
                                ret.add(ii);
                            }
                            continue;
                        } catch (IOException e) {
                            challenge.getPlugin().getLogger().log(e);
                        }
                    }
                }
                ret.add(images[i]);
            }
        }
        this.images = ret.toArray(new Image[] {});
        fps = 24;
    }

    /**
     * Shows the challenge description (explain text) as plain text above the image. Applies to all image captchas (text and click).
     */
    @Override
    protected void addBeforeImage(final MigPanel field) {
        final String explainText = getHelpText();
        if (explainText == null || explainText.length() == 0) {
            return;
        }
        /* Own first row for the description, all following rows (image) take the remaining space. */
        field.setLayout(new MigLayout("ins 0,wrap 1", "[grow,fill]", "[]5[grow,fill]"));
        final JTextArea description = new JTextArea(_GUI.T.AbstractImageCaptchaDialog_challengeDescription(explainText));
        description.setEditable(false);
        description.setFocusable(false);
        description.setLineWrap(true);
        description.setWrapStyleWord(true);
        description.setOpaque(false);
        description.setBorder(null);
        description.setFont(description.getFont().deriveFont(Font.BOLD, description.getFont().getSize2D() * 1.3f));
        field.add(description);
    }

    protected MigLayout getDialogLayout() {
        return new MigLayout("ins 0,wrap 1", "[fill,grow]", "[grow,fill]10[shrink 0]");
    }

    @Override
    protected JPanel createCaptchaPanel() {
        JPanel iconPanel = new JPanel(new MigLayout("ins 0", "[grow]", "[grow]")) {
            /**
             *
             */
            private static final long serialVersionUID = 1L;
            private Color             col              = (LAFOptions.getInstance().getColorForPanelBackground());

            protected void paintChildren(Graphics g) {
                Graphics2D g2 = (Graphics2D) g;
                Composite comp = g2.getComposite();
                try {
                    g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.85f));
                    super.paintChildren(g);
                } finally {
                    g2.setComposite(comp);
                }
            }

            @Override
            protected void paintComponent(Graphics g) {
                super.paintComponent(g);
                int insets = CFG_GUI.CFG.isCaptchaDialogBorderAroundImageEnabled() ? 10 : 0;
                Image scaled = IconIO.getScaledInstance(images[frame], getWidth() - insets, getHeight() - insets, Interpolation.BICUBIC, true);
                scaleFaktor = images[frame].getWidth(null) / (double) scaled.getWidth(null);
                offset = new Point((getWidth() - scaled.getWidth(null)) / 2, (getHeight() - scaled.getHeight(null)) / 2);
                AbstractImageCaptchaDialog.this.bounds = new Rectangle((getWidth() - scaled.getWidth(null)) / 2, (getHeight() - scaled.getHeight(null)) / 2, scaled.getWidth(null), scaled.getHeight(null));
                g.setClip(bounds);
                g.drawImage(greyscale ? getGreyscaleImage(scaled) : scaled, (getWidth() - scaled.getWidth(null)) / 2, (getHeight() - scaled.getHeight(null)) / 2, col, null);
                paintIconComponent(g, getWidth(), getHeight(), (getWidth() - scaled.getWidth(null)) / 2, (getHeight() - scaled.getHeight(null)) / 2, scaled);
            }
        };
        final int size = org.jdownloader.settings.staticreferences.CFG_GUI.CAPTCHA_SCALE_FACTOR.getValue();
        if (size != 100) {
            iconPanel.setPreferredSize(new Dimension((int) (images[0].getWidth(null) * size / 100.0f), (int) (images[0].getHeight(null) * size / 100.0f)));
        } else {
            iconPanel.setPreferredSize(new Dimension(images[0].getWidth(null), images[0].getHeight(null)));
        }
        return iconPanel;
    }

    /**
     * The captcha image panel scales the image to its own size (see createCaptchaPanel). That only works if the scroll pane gives it the
     * size of the viewport instead of its preferred (= unscaled image) size. Therefore the content tracks the viewport size, so the image
     * shrinks together with the dialog instead of overflowing it or getting scrollbars.
     */
    @Override
    protected HeaderScrollPane createHeaderScrollPane(final MigPanel field) {
        final class ViewportTrackingPanel extends JPanel implements Scrollable {
            private static final long serialVersionUID = 1L;

            ViewportTrackingPanel() {
                super(new BorderLayout());
                setOpaque(false);
                add(field, BorderLayout.CENTER);
            }

            @Override
            public Dimension getPreferredScrollableViewportSize() {
                return getPreferredSize();
            }

            @Override
            public int getScrollableUnitIncrement(final Rectangle visibleRect, final int orientation, final int direction) {
                return 10;
            }

            @Override
            public int getScrollableBlockIncrement(final Rectangle visibleRect, final int orientation, final int direction) {
                return orientation == SwingConstants.VERTICAL ? visibleRect.height : visibleRect.width;
            }

            @Override
            public boolean getScrollableTracksViewportWidth() {
                return true;
            }

            @Override
            public boolean getScrollableTracksViewportHeight() {
                return true;
            }
        }
        return new HeaderScrollPane(new ViewportTrackingPanel());
    }

    protected void paintIconComponent(Graphics g, int width, int height, int xOsset, int yOffset, Image scaled) {
    }

    /**
     * Returns a greyscale copy of the given (already scaled) image. The last result is cached, because the same scaled image is painted
     * again and again as long as the dialog is not resized.
     */
    private Image getGreyscaleImage(final Image scaled) {
        if (greyscaleSource == scaled && greyscaleImage != null) {
            return greyscaleImage;
        }
        final int w = scaled.getWidth(null);
        final int h = scaled.getHeight(null);
        final BufferedImage ret = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB);
        final Graphics2D g2 = ret.createGraphics();
        try {
            g2.drawImage(scaled, 0, 0, null);
        } finally {
            g2.dispose();
        }
        final int[] pixels = ret.getRGB(0, 0, w, h, null, 0, w);
        for (int i = 0; i < pixels.length; i++) {
            final int argb = pixels[i];
            final int grey = (int) Math.round(0.299 * ((argb >> 16) & 0xFF) + 0.587 * ((argb >> 8) & 0xFF) + 0.114 * (argb & 0xFF));
            pixels[i] = (argb & 0xFF000000) | (grey << 16) | (grey << 8) | grey;
        }
        ret.setRGB(0, 0, w, h, pixels, 0, w);
        greyscaleSource = scaled;
        greyscaleImage = ret;
        return ret;
    }

    @Override
    protected void addAdditionalButtons(final DefaultButtonPanel buttonPanel) {
        buttonPanel.add(createGreyscaleToggle(), "width 28!");
    }

    /**
     * Creates the toggle that shows the captcha image in greyscale. It only changes the display, the image that is sent to solvers is not
     * touched.
     */
    protected JToggleButton createGreyscaleToggle() {
        final JToggleButton ret = new JToggleButton(new Icon() {
            @Override
            public void paintIcon(final Component c, final Graphics g, final int x, final int y) {
                final Graphics2D g2 = (Graphics2D) g.create();
                try {
                    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                    g2.setColor(c.getForeground());
                    g2.drawOval(x + 1, y + 1, getIconWidth() - 3, getIconHeight() - 3);
                    g2.fillArc(x + 1, y + 1, getIconWidth() - 3, getIconHeight() - 3, 90, 180);
                } finally {
                    g2.dispose();
                }
            }

            @Override
            public int getIconWidth() {
                return 18;
            }

            @Override
            public int getIconHeight() {
                return 18;
            }
        });
        ret.setSelected(greyscale);
        ret.setFocusable(false);
        ret.setToolTipText(_GUI.T.AbstractImageCaptchaDialog_greyscaleToggle_tooltip());
        ret.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                greyscale = ret.isSelected();
                if (iconPanel != null) {
                    iconPanel.repaint();
                }
            }
        });
        return ret;
    }

    @Override
    public void dispose() {
        if (paintTimer != null) {
            paintTimer.stop();
        }
        super.dispose();
    }

    abstract protected JComponent createInputComponent();

    @Override
    public JComponent layoutDialogContent() {
        final JComponent ret = super.layoutDialogContent();
        if (images.length > 1) {
            paintTimer = new Timer(1000 / fps, new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    frame = (frame + 1) % images.length;
                    ret.repaint();
                }
            });
            paintTimer.setRepeats(true);
            paintTimer.start();
        }
        final JComponent inputComponent = createInputComponent();
        if (inputComponent != null) {
            if (mouseListener != null) {
                inputComponent.addMouseListener(mouseListener);
            }
            if (mouseMotionListener != null) {
                inputComponent.addMouseMotionListener(mouseMotionListener);
            }
            ret.add(inputComponent);
        }
        addExternalSolverPanel(ret);
        return ret;
    }

    public Image[] getImages() {
        return images;
    }

    public Point getOffset() {
        return offset;
    }

    public double getScaleFaktor() {
        return scaleFaktor;
    }

    @Override
    protected boolean isResizable() {
        return true;
    }
}
