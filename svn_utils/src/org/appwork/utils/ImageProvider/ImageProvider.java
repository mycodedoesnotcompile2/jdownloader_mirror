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
package org.appwork.utils.ImageProvider;

import java.awt.Color;
import java.awt.Composite;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.Transparency;
import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.util.HashMap;
import java.util.Locale;
import java.util.WeakHashMap;
import java.util.concurrent.atomic.AtomicReference;

import javax.imageio.IIOException;
import javax.imageio.ImageIO;
import javax.swing.GrayFilter;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;

import org.appwork.storage.config.MinTimeWeakReference;
import org.appwork.storage.config.MinTimeWeakReferenceCleanup;
import org.appwork.utils.Application;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Exceptions;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.URLStream;

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
    public static BufferedImage convertToGrayScale(final BufferedImage bufferedImage) {
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
                final String[] availableFontFamilyNames = GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames();
                if (availableFontFamilyNames != null) {
                    for (final String fontFamilyName : availableFontFamilyNames) {
                        if ("Arial".equals(fontFamilyName)) {
                            FONTNAME.set(fontFamilyName);
                            return fontFamilyName;
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
     * Creates a dummy Icon
     *
     * @param string
     * @param i
     * @param j
     * @return
     */
    public static BufferedImage createIcon(final String string, final int width, final int height) {
        final int w = Math.max(1, width);
        final int h = Math.max(1, height);
        final BufferedImage image = new BufferedImage(w, h, Transparency.TRANSLUCENT);
        final Graphics2D g = image.createGraphics();
        int size = 1 + width / string.length();
        try {
            try {
                final String fontName = getDrawFontName();
                g.setFont(new Font(fontName, Font.BOLD, size));
                final FontMetrics fontMetrics = g.getFontMetrics();// check for missing fonts/headless java
                g.setColor(Color.WHITE);
                g.fillRect(0, 0, w - 1, h - 1);
                g.draw3DRect(0, 0, w - 1, h - 1, true);
                g.setColor(Color.BLACK);
                // find max font size
                int ww = 0;
                int hh = 0;
                while (size > 0) {
                    size--;
                    g.setFont(new Font(fontName, Font.BOLD, size));
                    ww = fontMetrics.stringWidth(string);
                    hh = fontMetrics.getAscent();
                    if (ww < w - 4 && hh < h - 2) {
                        break;
                    }
                }
                g.drawString(string, (w - ww) / 2, hh + (h - hh) / 2);
                return image;
            } catch (Throwable e) {
                if (isBuggyFontEnvironment(e)) {
                    g.setColor(Color.RED);
                    g.fillRect(0, 0, w - 1, h - 1);
                    return image;
                } else {
                    throw new RuntimeException(e);
                }
            }
        } finally {
            g.dispose();
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
    public static BufferedImage dereferenceImage(final Image image) throws IOException {
        final BufferedImage bu = new BufferedImage(image.getWidth(null), image.getHeight(null), BufferedImage.TYPE_INT_ARGB);
        final Graphics g = bu.getGraphics();
        g.drawImage(image, 0, 0, null);
        g.dispose();
        return bu;
    }

    /**
     *
     * @param name
     *            to the png file
     * @param createDummy
     *            TODO
     * @return
     * @throws IOException
     */
    public static BufferedImage getBufferedImage(final String name, final boolean createDummy) throws IOException {
        return ImageProvider.getBufferedImage(name, createDummy, true);
    }

    public static BufferedImage getBufferedImage(final String name, final boolean createDummy, final boolean putIntoCache) throws IOException {
        synchronized (ImageProvider.LOCK) {
            if (ImageProvider.IMAGE_CACHE.containsKey(name)) {
                final MinTimeWeakReference<BufferedImage> cache = ImageProvider.IMAGE_CACHE.get(name);
                if (cache.get() != null) {
                    return cache.get();
                }
            }
            final URL absolutePath = Application.getRessourceURL("images/" + name + ".png");
            try {
                org.appwork.loggingv3.LogV3.info("Init Image: " + name + ": " + absolutePath);
                final BufferedImage image = ImageProvider.read(absolutePath);
                if (putIntoCache) {
                    if (image.getHeight() * image.getWidth() > 100 * 100) {
                        // org.appwork.loggingv3.LogV3.log(new Throwable("BIG IMAGE IN CACHE: " +
                        // name));
                    }
                    ImageProvider.IMAGE_CACHE.put(name, new MinTimeWeakReference<BufferedImage>(image, ImageProvider.MIN_LIFETIME, name, ImageProvider.IMAGE_CACHE_CLEANUP));
                }
                return image;
            } catch (final IOException e) {
                org.appwork.loggingv3.LogV3.severe("Could not Init Image: " + absolutePath);
                if (createDummy) {
                    org.appwork.loggingv3.LogV3.log(e);
                    return ImageProvider.createIcon(name.toUpperCase(Locale.ENGLISH), 48, 48);
                } else {
                    throw e;
                }
            } catch (final Throwable e) {
                org.appwork.loggingv3.LogV3.severe("Could not Init Image: " + absolutePath);
                org.appwork.loggingv3.LogV3.log(e);
                return ImageProvider.createIcon(name.toUpperCase(Locale.ENGLISH), 48, 48);
            }
        }
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

    /**
     * Uses the uimanager to get a grayscaled disabled Icon
     *
     * @param icon
     * @return
     */
    public static Icon getDisabledIcon(final JComponent component, Icon icon) {
        if (icon != null) {
            synchronized (ImageProvider.LOCK) {
                final MinTimeWeakReference<Icon> cache = ImageProvider.DISABLED_ICON_CACHE.get(icon);
                final Icon cachedDisabledIcon = cache == null ? null : cache.get();
                if (cachedDisabledIcon != null) {
                    //
                    return cachedDisabledIcon;
                }
                if (!(icon instanceof ImageIcon)) {
                    // getDisabledIcon only works for imageicons
                    icon = ImageProvider.toImageIcon(component, icon);
                }
                final Icon disabledIcon = new ImageIcon(GrayFilter.createDisabledImage(((ImageIcon) icon).getImage()));
                ImageProvider.DISABLED_ICON_CACHE.put(icon, new MinTimeWeakReference<Icon>(disabledIcon, ImageProvider.MIN_LIFETIME, "disabled icon"));
                return disabledIcon;
            }
        }
        return null;
    }

    /**
     * @param string
     * @param i
     * @param j
     * @return
     */
    public static ImageIcon getImageIcon(final String name, final int x, final int y) {
        try {
            return ImageProvider.getImageIcon(name, x, y, true);
        } catch (final IOException e) {
            // can not happen. true creates a dummyicon in case of io errors
            org.appwork.loggingv3.LogV3.log(e);
            return null;
        }
    }

    /**
     * Loads the image, scales it to the desired size and returns it as an imageicon
     *
     * @param name
     * @param width
     * @param height
     * @param createDummy
     *            TODO
     * @return
     * @throws IOException
     */
    public static ImageIcon getImageIcon(final String name, final int width, final int height, final boolean createDummy) throws IOException {
        return ImageProvider.getImageIcon(name, width, height, createDummy, true);
    }

    public static ImageIcon getImageIcon(final String name, int width, int height, final boolean createDummy, final boolean putIntoCache) throws IOException {
        synchronized (ImageProvider.LOCK) {
            final StringBuilder SB = new StringBuilder();
            SB.append(name);
            SB.append('_');
            SB.append(width);
            SB.append('_');
            SB.append(height);
            String key;
            if (ImageProvider.IMAGEICON_CACHE.containsKey(key = SB.toString())) {
                final MinTimeWeakReference<ImageIcon> cache = ImageProvider.IMAGEICON_CACHE.get(key);
                if (cache.get() != null) {
                    return cache.get();
                }
            }
            final BufferedImage image = ImageProvider.getBufferedImage(name, createDummy, putIntoCache);
            final double faktor = Math.max((double) image.getWidth(null) / width, (double) image.getHeight(null) / height);
            width = (int) (image.getWidth(null) / faktor);
            height = (int) (image.getHeight(null) / faktor);
            /**
             * WARNING: getScaledInstance will return a scaled image, BUT keeps a reference to original unscaled image
             */
            final Image scaledWithFuckingReference = image.getScaledInstance(width, height, Image.SCALE_SMOOTH);
            final BufferedImage referencelessVersion = ImageProvider.dereferenceImage(scaledWithFuckingReference);
            final ImageIcon imageicon = new ImageIcon(referencelessVersion);
            if (putIntoCache) {
                ImageProvider.IMAGEICON_CACHE.put(key, new MinTimeWeakReference<ImageIcon>(imageicon, ImageProvider.MIN_LIFETIME, key, ImageProvider.IMAGEICON_CACHE_CLEANUP));
            }
            return imageicon;
        }
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

    public static ImageIcon getImageIconUnCached(final String name, final int x, final int y) {
        try {
            return ImageProvider.getImageIcon(name, x, y, true, false);
        } catch (final IOException e) {
            // can not happen. true creates a dummyicon in case of io errors
            org.appwork.loggingv3.LogV3.log(e);
            return null;
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
    public static BufferedImage merge(final Image back, final Image front, final int xoffset, final int yoffset) {
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

    public static BufferedImage merge(final Icon back, final Icon front, final int xoffsetBack, final int yoffsetBack, final int xoffsetFront, final int yoffsetFront, final Composite backComposite, final Composite frontComposite) {
        final int width = Math.max(xoffsetBack + back.getIconWidth(), xoffsetFront + front.getIconWidth());
        final int height = Math.max(yoffsetBack + back.getIconHeight(), yoffsetFront + front.getIconHeight());
        final BufferedImage dest = new BufferedImage(width, height, Transparency.TRANSLUCENT);
        final Graphics2D g2 = dest.createGraphics();
        if (backComposite != null) {
            final Composite old = g2.getComposite();
            g2.setComposite(backComposite);
            back.paintIcon(null, g2, xoffsetBack, yoffsetBack);
            g2.setComposite(old);
        } else {
            back.paintIcon(null, g2, xoffsetBack, yoffsetBack);
        }
        if (frontComposite != null) {
            final Composite old = g2.getComposite();
            g2.setComposite(frontComposite);
            front.paintIcon(null, g2, xoffsetFront, yoffsetFront);
            g2.setComposite(old);
        } else {
            front.paintIcon(null, g2, xoffsetFront, yoffsetFront);
        }
        g2.dispose();
        return dest;
    }

    public static BufferedImage merge(final Icon back, final Icon front, final int xoffsetBack, final int yoffsetBack, final int xoffsetFront, final int yoffsetFront) {
        return ImageProvider.merge(back, front, xoffsetBack, yoffsetBack, xoffsetFront, yoffsetFront, null, null);
    }

    public static BufferedImage merge(final Image back, final Image front, final int xoffsetBack, final int yoffsetBack, final int xoffsetFront, final int yoffsetFront) {
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
     * @param absolutePath
     * @return
     * @throws IOException
     */
    private static BufferedImage read(final URL absolutePath) throws IOException {
        if (absolutePath == null) {
            throw new IllegalArgumentException("input == null!");
        }
        InputStream is = null;
        BufferedImage bi = null;
        try {
            is = URLStream.openStream(absolutePath);
            if (is == null) {
                throw new IIOException("Can't create an ImageInputStream!");
            }
            bi = ImageIO.read(is);
        } finally {
            try {
                if (is != null) {
                    is.close();
                }
            } catch (final Throwable e) {
            }
        }
        return bi;
    }

    /**
     * @param scaleBufferedImage
     * @param width
     * @param height
     * @return
     */
    public static BufferedImage resizeWorkSpace(final Image scaleBufferedImage, final int width, final int height) {
        // final GraphicsEnvironment ge =
        // GraphicsEnvironment.getLocalGraphicsEnvironment();
        // final GraphicsDevice gd = ge.getDefaultScreenDevice();
        // final GraphicsConfiguration gc = gd.getDefaultConfiguration();
        // final BufferedImage image = gc.createCompatibleImage(width, height,
        // Transparency.BITMASK);
        final BufferedImage image = new BufferedImage(width, height, Transparency.TRANSLUCENT);
        final Graphics2D g = image.createGraphics();
        g.drawImage(scaleBufferedImage, (width - scaleBufferedImage.getWidth(null)) / 2, (height - scaleBufferedImage.getHeight(null)) / 2, null);
        g.dispose();
        return image;
    }

    /**
     * Scales a buffered Image to the given size. This method is NOT cached. so take care to cache it externally if you use it frequently
     *
     * @param img
     * @param width
     * @param height
     * @return
     */
    public static Image scaleBufferedImage(final BufferedImage img, int width, int height) {
        if (img == null) {
            return null;
        }
        final double faktor = Math.max((double) img.getWidth() / width, (double) img.getHeight() / height);
        width = (int) (img.getWidth() / faktor);
        height = (int) (img.getHeight() / faktor);
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
            final BufferedImage dest;
            if (ReflectionUtils.isInstanceOf("sun.awt.image.ToolkitImage", img.getImage())) {
                dest = new BufferedImage(w, h, Transparency.TRANSLUCENT);
                final Graphics2D g2 = dest.createGraphics();
                g2.drawImage(img.getImage(), 0, 0, null);
                g2.dispose();
            } else {
                dest = (BufferedImage) img.getImage();
            }
            return new ImageIcon(ImageProvider.scaleBufferedImage(dest, w, h));
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
            final BufferedImage image = new BufferedImage(w, h, Transparency.TRANSLUCENT);
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
