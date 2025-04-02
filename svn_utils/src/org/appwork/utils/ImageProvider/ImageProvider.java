/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
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
package org.appwork.utils.ImageProvider;

import java.awt.Color;
import java.awt.Composite;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.WeakHashMap;
import java.util.concurrent.atomic.AtomicReference;

import javax.imageio.IIOException;
import javax.imageio.ImageIO;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;

import org.appwork.resources.MultiResolutionImageHelper;
import org.appwork.storage.config.MinTimeWeakReference;
import org.appwork.storage.config.MinTimeWeakReferenceCleanup;
import org.appwork.utils.Application;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Exceptions;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.images.DisabledIcon;
import org.appwork.utils.images.IconIO;

/**
 * This class grants easy access to images stored in APPROOT/images/**.png
 *
 * @author $Author: unknown$
 *
 */
public class ImageProvider {
    private static final long                                           MIN_LIFETIME            = 20000l;
    /**
     * Hashcashmap to cache images.
     */
    private static HashMap<String, MinTimeWeakReference<BufferedImage>> IMAGE_CACHE             = new HashMap<String, MinTimeWeakReference<BufferedImage>>();
    private static MinTimeWeakReferenceCleanup                          IMAGE_CACHE_CLEANUP     = new MinTimeWeakReferenceCleanup() {
                                                                                                    @Override
                                                                                                    public void onMinTimeWeakReferenceCleanup(final MinTimeWeakReference<?> minTimeWeakReference) {
                                                                                                        synchronized (ImageProvider.LOCK) {
                                                                                                            ImageProvider.IMAGE_CACHE.remove(minTimeWeakReference.getID());
                                                                                                        }
                                                                                                    }
                                                                                                };
    private static HashMap<String, MinTimeWeakReference<ImageIcon>>     IMAGEICON_CACHE         = new HashMap<String, MinTimeWeakReference<ImageIcon>>();
    private static MinTimeWeakReferenceCleanup                          IMAGEICON_CACHE_CLEANUP = new MinTimeWeakReferenceCleanup() {
                                                                                                    @Override
                                                                                                    public void onMinTimeWeakReferenceCleanup(final MinTimeWeakReference<?> minTimeWeakReference) {
                                                                                                        synchronized (ImageProvider.LOCK) {
                                                                                                            ImageProvider.IMAGEICON_CACHE.remove(minTimeWeakReference.getID());
                                                                                                        }
                                                                                                    }
                                                                                                };
    private static WeakHashMap<Icon, MinTimeWeakReference<Icon>>        DISABLED_ICON_CACHE     = new WeakHashMap<Icon, MinTimeWeakReference<Icon>>();
    private static Object                                               LOCK                    = new Object();
    // stringbuilder die concat strings fast
    static {
        /* we dont want images to get cached on disk */
        ImageIO.setUseCache(false);
    }

    /* Thx to flubshi */
    public static BufferedImage convertToGrayScale(final Image img) {
        BufferedImage bufferedImage = IconIO.toBufferedImage(img);
        final BufferedImage dest = new BufferedImage(bufferedImage.getWidth(), bufferedImage.getHeight(), BufferedImage.TYPE_INT_ARGB);
        Color tmp;
        int val, alpha;
        for (int y = 0; y < dest.getHeight(); y++) {
            for (int x = 0; x < dest.getWidth(); x++) {
                alpha = bufferedImage.getRGB(x, y) & 0xFF000000;
                tmp = new Color(bufferedImage.getRGB(x, y));
                // val = (int) (tmp.getRed()+tmp.getGreen()+tmp.getBlue())/3;
                // val =
                // Math.max(tmp.getRed(),Math.max(tmp.getGreen(),tmp.getBlue()));
                val = (int) (tmp.getRed() * 0.3 + tmp.getGreen() * 0.59 + tmp.getBlue() * 0.11);
                dest.setRGB(x, y, alpha | val | val << 8 & 0x0000FF00 | val << 16 & 0x00FF0000);
            }
        }
        return dest;
    }

    private static final AtomicReference<String> FONTNAME = new AtomicReference<String>(null);

    public static String getDrawFontName() {
        if (FONTNAME.get() != null) {
            return FONTNAME.get();
        }
        synchronized (FONTNAME) {
            if (FONTNAME.get() != null) {
                return FONTNAME.get();
            }
            try {
                if (!Application.isHeadless()) {
                    final String[] availableFontFamilyNames = GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames();
                    if (availableFontFamilyNames != null) {
                        for (final String fontFamilyName : availableFontFamilyNames) {
                            if ("Arial".equals(fontFamilyName)) {
                                FONTNAME.set(fontFamilyName);
                                return fontFamilyName;
                            }
                        }
                    }
                }
            } catch (final Throwable e) {
            }
            FONTNAME.set("Dialog");
            return "Dialog";
        }
    }

    /**
     * this creates a new BufferedImage from an existing Image. Is used for dereferencing the sourceImage in scaled Images created with
     * image.getScaledInstance, which always keeps a reference to its original image
     *
     * @param image
     * @return
     * @throws IOException
     */
    private static Image dereferenceImage(final Image image) throws IOException {
        final BufferedImage bu = IconIO.createEmptyImage(image.getHeight(null), image.getWidth(null), image);
        final Graphics g = bu.getGraphics();
        g.drawImage(image, 0, 0, null);
        g.dispose();
        return bu;
    }

    public static boolean isBuggyFontEnvironment(final Throwable throwable) {
        if (Application.isHeadless()) {
            if (Exceptions.getInstanceof(throwable, NoClassDefFoundError.class) != null) {
                // java.lang.NoClassDefFoundError: Could not initialize class sun.font.SunFontManager
                // at java.desktop/sun.font.FontDesignMetrics.getMetrics(FontDesignMetrics.java:265)
                // java.lang.UnsatisfiedLinkError: /usr/lib/jvm/jdk-11.0.2/lib/libfontmanager.so: libfreetype.so.6: cannot open shared
                // object file: No such file or directory
                return true;
            } else if (Exceptions.getInstanceof(throwable, ClassCastException.class) != null) {
                // >=Java11, java.lang.ClassCastException: class sun.font.CompositeFont cannot be cast to class sun.font.PhysicalFont
                // no longer ships default fonts with JDK and relies on proper system configuration(installed fonts) or application to
                // provide fonts
                // https://www.oracle.com/technetwork/java/javase/11-relnote-issues-5012449.html#JDK-8191522
                return true;
            } else if (Exceptions.getInstanceof(throwable, NullPointerException.class) != null || Exceptions.getInstanceof(throwable, InvocationTargetException.class) != null) {
                // java.lang.NullPointerException or java.lang.reflect.InvocationTargetException(jdk12)
                // at sun.awt.FontConfiguration.getVersion(FontConfiguration.java:1264)
                // at sun.awt.FontConfiguration.readFontConfigFile(FontConfiguration.java:219)
                // at sun.awt.FontConfiguration.init(FontConfiguration.java:107)
                return true;
            } else if (!DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                // unknown exception, do not crash standalone headless
                org.appwork.loggingv3.LogV3.log(throwable);
                return true;
            } else {
                return false;
            }
        } else {
            return false;
        }
    }

    public static Icon getDisabledIcon(final JComponent component, Icon icon) {
        if (icon != null) {
            synchronized (ImageProvider.LOCK) {
                final MinTimeWeakReference<Icon> cache = ImageProvider.DISABLED_ICON_CACHE.get(icon);
                final Icon cachedDisabledIcon = cache == null ? null : cache.get();
                if (cachedDisabledIcon != null) {
                    return cachedDisabledIcon;
                }
                final Icon disabledIcon = new DisabledIcon(icon);
                ImageProvider.DISABLED_ICON_CACHE.put(icon, new MinTimeWeakReference<Icon>(disabledIcon, ImageProvider.MIN_LIFETIME, "disabled icon"));
                return disabledIcon;
            }
        }
        return null;
    }

    public static void writeImage(RenderedImage im, String formatName, File file) throws IOException {
        if (!ImageIO.write(im, formatName, file)) {
            throw new IOException("no appropriate " + formatName + " writer found!");
        }
    }

    public static void writeImage(RenderedImage im, String formatName, OutputStream output) throws IOException {
        if (!ImageIO.write(im, formatName, output)) {
            throw new IOException("no appropriate " + formatName + " writer found!");
        }
    }

    /**
     * Taken from http://today.java.net/pub/a/today/2007/04/03/perils-of-image- getscaledinstance.html License: unknown Convenience method
     * that returns a scaled instance of the provided {@code BufferedImage}.
     *
     * @param img
     *            the original image to be scaled
     * @param targetWidth
     *            the desired width of the scaled instance, in pixels
     * @param targetHeight
     *            the desired height of the scaled instance, in pixels
     * @param hint
     *            one of the rendering hints that corresponds to {@code RenderingHints.KEY_INTERPOLATION} (e.g.
     *            {@code RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR}, {@code RenderingHints.VALUE_INTERPOLATION_BILINEAR},
     *            {@code RenderingHints.VALUE_INTERPOLATION_BICUBIC})
     * @param higherQuality
     *            if true, this method will use a multi-step scaling technique that provides higher quality than the usual one-step
     *            technique (only useful in downscaling cases, where {@code targetWidth} or {@code targetHeight} is smaller than the
     *            original dimensions, and generally only when the {@code BILINEAR} hint is specified)
     * @return a scaled version of the original {@code BufferedImage}
     */
    public static BufferedImage getScaledInstance(final BufferedImage img, int width, int height, final Object hint, final boolean higherQuality) {
        final double faktor = Math.max((double) img.getWidth() / width, (double) img.getHeight() / height);
        width = (int) (img.getWidth() / faktor);
        height = (int) (img.getHeight() / faktor);
        if (faktor == 1.0) {
            return img;
        }
        BufferedImage ret = img;
        int w, h;
        if (higherQuality) {
            // Use multi-step technique: start with original size, then
            // scale down in multiple passes with drawImage()
            // until the target size is reached
            w = Math.max(width, img.getWidth());
            h = Math.max(height, img.getHeight());
        } else {
            // Use one-step technique: scale directly from original
            // size to target size with a single drawImage() call
            w = width;
            h = height;
        }
        do {
            if (higherQuality && w > width) {
                w /= 2;
                if (w < width) {
                    w = width;
                }
            }
            if (higherQuality && h > height) {
                h /= 2;
                if (h < height) {
                    h = height;
                }
            }
            // use 6 as default image type. java versions <16 u17 return type 0
            // for loaded pngs
            final BufferedImage tmp = new BufferedImage(w, h, ret.getType() == 0 ? 6 : ret.getType());
            final Graphics2D g2 = tmp.createGraphics();
            g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, hint);
            g2.drawImage(ret, 0, 0, w, h, null);
            g2.dispose();
            ret = tmp;
        } while (w != width || h != height);
        return ret;
    }

    /**
     * @param back
     * @param imageIcon
     * @param i
     * @param j
     */
    public static Image merge(final Image back, final Image front, final int xoffset, final int yoffset) {
        int xoffsetTop, yoffsetTop, xoffsetBottom, yoffsetBottom;
        if (xoffset >= 0) {
            xoffsetTop = 0;
            xoffsetBottom = xoffset;
        } else {
            xoffsetTop = -xoffset;
            xoffsetBottom = 0;
        }
        if (yoffset >= 0) {
            yoffsetTop = 0;
            yoffsetBottom = yoffset;
        } else {
            yoffsetTop = -yoffset;
            yoffsetBottom = 0;
        }
        return ImageProvider.merge(back, front, xoffsetTop, yoffsetTop, xoffsetBottom, yoffsetBottom);
    }

    public static Image merge(final Icon back, final Icon front, final int xoffsetBack, final int yoffsetBack, final int xoffsetFront, final int yoffsetFront, final Composite backComposite, final Composite frontComposite) {
        Image backImage = IconIO.toImage(back);
        Image frontImage = IconIO.toImage(front);
        final int width = Math.max(xoffsetBack + back.getIconWidth(), xoffsetFront + front.getIconWidth());
        final int height = Math.max(yoffsetBack + back.getIconHeight(), yoffsetFront + front.getIconHeight());
        if (false && !Application.isHeadless() && MultiResolutionImageHelper.isInstanceOf(backImage) && MultiResolutionImageHelper.isInstanceOf(frontImage)) {
            ArrayList<Image> images = new ArrayList<Image>();
            HashSet<String> dupe = new HashSet<String>();
            for (GraphicsDevice sd : GraphicsEnvironment.getLocalGraphicsEnvironment().getScreenDevices()) {
                AffineTransform tx = sd.getDefaultConfiguration().getDefaultTransform();
                if (dupe.add(tx.getScaleX() + ":" + tx.getScaleY())) {
                    Image backVariant = MultiResolutionImageHelper.getResolutionVariant(backImage, tx.getScaleX() * backImage.getWidth(null), tx.getScaleY() * backImage.getHeight(null));
                    Image frontVariant = MultiResolutionImageHelper.getResolutionVariant(frontImage, tx.getScaleX() * frontImage.getWidth(null), tx.getScaleY() * frontImage.getHeight(null));
                    images.add(merge(backVariant, frontVariant, IconIO.clipScale(xoffsetBack, tx.getScaleX()), IconIO.clipScale(yoffsetBack, tx.getScaleY()), IconIO.clipScale(xoffsetFront, tx.getScaleX()), IconIO.clipScale(yoffsetFront, tx.getScaleY()), backComposite, frontComposite, IconIO.clipScale(width, tx.getScaleX()), IconIO.clipScale(height, tx.getScaleY())));
                }
            }
            if (images.size() == 1) {
                return images.get(0);
            } else if (images.size() > 0) {
                return MultiResolutionImageHelper.create(images);
            } else {
                // fallback
            }
        }
        return merge(backImage, frontImage, xoffsetBack, yoffsetBack, xoffsetFront, yoffsetFront, backComposite, frontComposite, width, height);
    }

    protected static Image merge(Image backVariant, Image frontVariant, final int xoffsetBack, final int yoffsetBack, final int xoffsetFront, final int yoffsetFront, final Composite backComposite, final Composite frontComposite, final int width, final int height) {
        BufferedImage target = IconIO.createEmptyImage(width, height);
        final Graphics2D g2 = target.createGraphics();
        g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        if (backComposite != null) {
            final Composite old = g2.getComposite();
            g2.setComposite(backComposite);
            g2.drawImage(backVariant, xoffsetBack, yoffsetBack, null);
            g2.setComposite(old);
        } else {
            g2.drawImage(backVariant, xoffsetBack, yoffsetBack, null);
        }
        if (frontComposite != null) {
            final Composite old = g2.getComposite();
            g2.setComposite(frontComposite);
            g2.drawImage(frontVariant, xoffsetFront, yoffsetFront, null);
            g2.setComposite(old);
        } else {
            g2.drawImage(frontVariant, xoffsetFront, yoffsetFront, null);
        }
        g2.dispose();
        return target;
    }

    public static Image merge(final Icon back, final Icon front, final int xoffsetBack, final int yoffsetBack, final int xoffsetFront, final int yoffsetFront) {
        return ImageProvider.merge(back, front, xoffsetBack, yoffsetBack, xoffsetFront, yoffsetFront, null, null);
    }

    public static Image merge(final Image back, final Image front, final int xoffsetBack, final int yoffsetBack, final int xoffsetFront, final int yoffsetFront) {
        return ImageProvider.merge(new ImageIcon(back), new ImageIcon(front), xoffsetBack, yoffsetBack, xoffsetFront, yoffsetFront);
    }

    /* copied from ImageIO, to close the inputStream */
    public static BufferedImage read(final File input) throws IOException {
        if (!input.canRead()) {
            throw new IIOException("Can't read input file!");
        }
        final FileInputStream is = new FileInputStream(input);
        try {
            return ImageIO.read(is);
        } finally {
            is.close();
        }
    }

    /**
     * Scales a buffered Image to the given size. This method is NOT cached. so take care to cache it externally if you use it frequently
     *
     * @param img
     * @param width
     * @param height
     * @return
     */
    @Deprecated
    public static Image scaleBufferedImage(final BufferedImage img, int width, int height) {
        return scaleImage(img, width, height);
    }

    /**
     * Scales a Image to the given size. This method is NOT cached. so take care to cache it externally if you use it frequently
     *
     * @param img
     * @param width
     * @param height
     * @return
     */
    public static Image scaleImage(final Image img, int width, int height) {
        if (img == null) {
            return null;
        }
        final int imgWidth = img.getWidth(null);
        final int imgHeight = img.getHeight(null);
        final double faktor = Math.max((double) imgWidth / width, (double) imgHeight / height);
        width = (int) (imgWidth / faktor);
        height = (int) (imgHeight / faktor);
        if (faktor == 1.0) {
            return img;
        }
        final Image image = img.getScaledInstance(width, height, Image.SCALE_SMOOTH);
        try {
            return ImageProvider.dereferenceImage(image);
        } catch (final IOException e) {
            org.appwork.loggingv3.LogV3.log(e);
            return null;
        }
    }

    /**
     * Scales an imageicon to w x h.<br>
     * like {@link #scaleBufferedImage(BufferedImage, int, int)}, this Function is NOT cached. USe an external cache if you use it
     * frequently
     *
     * @param img
     * @param w
     * @param h
     * @return
     */
    public static ImageIcon scaleImageIcon(final ImageIcon img, final int w, final int h) {
        // already has the desired size?
        if (img.getIconHeight() == h && img.getIconWidth() == w) {
            return img;
        } else {
            final Image dest;
            if (ReflectionUtils.isInstanceOf("sun.awt.image.ToolkitImage", img.getImage())) {
                dest = IconIO.createEmptyImage(w, h);
                final Graphics2D g2 = ((BufferedImage) dest).createGraphics();
                g2.drawImage(img.getImage(), 0, 0, null);
                g2.dispose();
            } else {
                dest = img.getImage();
            }
            return new ImageIcon(ImageProvider.scaleImage(dest, w, h));
        }
    }

    public static ImageIcon toImageIcon(final JComponent component, final Icon icon) {
        if (icon == null) {
            return null;
        } else if (icon instanceof ImageIcon) {
            return (ImageIcon) icon;
        } else {
            final int w = icon.getIconWidth();
            final int h = icon.getIconHeight();
            final BufferedImage image = IconIO.createEmptyImage(w, h);
            final Graphics2D g = image.createGraphics();
            g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
            // g.setColor(Color.RED);
            // g.fillRect(0, 0, w, h);
            try {
                icon.paintIcon(component, g, 0, 0);
            } catch (final NullPointerException e) {
                try {
                    e.printStackTrace();
                    final JFrame frame = new JFrame();
                    final JButton bt = new JButton();
                    frame.getContentPane().add(bt);
                    icon.paintIcon(bt, g, 0, 0);
                    // java.lang.NullPointerException when WindowDecoration is enabled in synthetica
                    // at
                    // de.javasoft.plaf.synthetica.simple2D.iconPainter.RootPaneMaximizeIconPainter.paint2D(RootPaneMaximizeIconPainter.java:42)
                    // at de.javasoft.plaf.synthetica.painter.SyntheticaBasicIconPainter.paintIcon_(SyntheticaBasicIconPainter.java:173)
                    // at de.javasoft.plaf.synthetica.painter.SyntheticaBasicIconPainter.paintIcon(SyntheticaBasicIconPainter.java:143)
                    // at de.javasoft.plaf.synthetica.painter.SyntheticaBasicIconPainter.paintIcon(SyntheticaBasicIconPainter.java:131)
                } catch (final Throwable ignore) {
                    ignore.printStackTrace();
                }
            }
            g.dispose();
            return new ImageIcon(image);
        }
    }

    /**
     * Converts an Icon to an Imageicon.
     *
     * @param icon
     * @return
     */
    public static ImageIcon toImageIcon(final Icon icon) {
        return toImageIcon(null, icon);
    }
}
