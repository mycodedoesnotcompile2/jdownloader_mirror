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
package org.appwork.utils.images;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Toolkit;
import java.awt.Transparency;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.awt.image.AffineTransformOp;
import java.awt.image.BufferedImage;
import java.awt.image.BufferedImageOp;
import java.awt.image.ConvolveOp;
import java.awt.image.FilteredImageSource;
import java.awt.image.ImageFilter;
import java.awt.image.ImageProducer;
import java.awt.image.Kernel;
import java.awt.image.RGBImageFilter;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;

import javax.imageio.ImageIO;
import javax.swing.GrayFilter;
import javax.swing.Icon;
import javax.swing.ImageIcon;

import org.appwork.JNAHelper;
import org.appwork.builddecision.BuildDecisionRequired;
import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.resources.MultiResolutionImageHelper;
import org.appwork.swing.components.IDIcon;
import org.appwork.swing.components.IconIdentifier;
import org.appwork.testframework.AWTestValidateClassReference;
import org.appwork.utils.DebugMode;
import org.appwork.utils.IO;
import org.appwork.utils.StringUtils;
import org.appwork.utils.URLStream;
import org.appwork.utils.ImageProvider.ImageProvider;
import org.appwork.utils.images.svg.NoSVGSupportFactory;
import org.appwork.utils.images.svg.SVGFactory;
import org.appwork.utils.net.Base64OutputStream;
import org.appwork.utils.os.CrossSystem;

@BuildDecisionRequired(tags = { IconIO.SVG_JSVG, IconIO.SVG_SALAMANDER, IconIO.SVG_NONE }, imports = { IconIO.CLASS_ORG_APPWORK_UTILS_IMAGES_SVG_WEISJ_JSVG_FACTORY, IconIO.CLASS_ORG_APPWORK_UTILS_IMAGES_SVG_KIT_FOX_FACTORY, "" })
public class IconIO {
    public static enum DataURLFormat {
        JPG,
        PNG
    }

    public static class ScaledIcon extends AbstractIconPipe implements Icon, IDIcon {
        private final int                          width;
        private final int                          height;
        private final Interpolation                interpolation;
        private final double                       faktor;
        private static final Set<ModificationType> MODIFICATIONS = Collections.unmodifiableSet(new HashSet<ModificationType>(Arrays.asList(ModificationType.SIZE)));

        /**
         * @see org.appwork.utils.images.IconPipe#getModifications()
         */
        @Override
        public Set<ModificationType> getModifications() {
            return MODIFICATIONS;
        }

        /**
         * @param icon
         * @param w
         * @param h
         * @param hint
         */
        public ScaledIcon(final Icon icon, final int width, final int height, final Interpolation interpolation) {
            super(icon);
            this.faktor = 1d / Math.max((double) icon.getIconWidth() / width, (double) icon.getIconHeight() / height);
            this.width = Math.max((int) Math.round(icon.getIconWidth() * this.faktor), 1);
            this.height = Math.max((int) Math.round(icon.getIconHeight() * this.faktor), 1);
            this.interpolation = interpolation;
        }

        /*
         * (non-Javadoc)
         *
         * @see javax.swing.Icon#getIconHeight()
         */
        @Override
        public int getIconHeight() {
            return this.height;
        }

        /*
         * (non-Javadoc)
         *
         * @see javax.swing.Icon#getIconWidth()
         */
        @Override
        public int getIconWidth() {
            return this.width;
        }

        /*
         * (non-Javadoc)
         *
         * @see org.appwork.swing.components.IdentifierInterface#toIdentifier()
         */
        @Override
        public IconIdentifier getIdentifier() {
            if (delegate instanceof IDIcon) {
                return ((IDIcon) delegate).getIdentifier();
            } else {
                return new IconIdentifier("unknown", delegate.toString());
            }
        }

        protected Icon getOrigin() {
            if (delegate instanceof ScaledIcon) {
                return ((ScaledIcon) delegate).getOrigin();
            } else {
                return delegate;
            }
        }

        /*
         * (non-Javadoc)
         *
         * @see javax.swing.Icon#paintIcon(java.awt.Component, java.awt.Graphics, int, int)
         */
        @Override
        public void paintIcon(final Component c, final Graphics g, final int x, final int y, List<Icon> parents) {
            if (delegate instanceof ScalableIcon) {
                ((ScalableIcon) delegate).paintIcon(c, g, x, y, getIconWidth(), getIconHeight());
                return;
            }
            final Graphics2D g2 = (Graphics2D) g;
            g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, this.interpolation.getHint());
            g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            final AffineTransform old = g2.getTransform();
            g2.translate(x, y);
            g2.scale(this.faktor, this.faktor);
            paintDelegate(c, g2, 0, 0, parents);
            g2.setTransform(old);
        }
    }

    @AWTestValidateClassReference
    protected static final String                    CLASS_ORG_APPWORK_UTILS_IMAGES_SVG_KIT_FOX_FACTORY    = "org.appwork.utils.images.svg.KitFoxFactory";
    @AWTestValidateClassReference
    protected static final String                    CLASS_ORG_APPWORK_UTILS_IMAGES_SVG_WEISJ_JSVG_FACTORY = "org.appwork.utils.images.svg.WeisjJSVGFactory";
    public static final String                       SVG_NONE                                              = "SVG-NONE";
    public static final String                       SVG_SALAMANDER                                        = "SVG-SALAMANDER";
    public static final String                       SVG_JSVG                                              = "SVG-JSVG";
    public static final String                       SVG_FACTORY_KEY                                       = "SVG_FACTORY";
    private static final AtomicReference<SVGFactory> SVG_FACTORY                                           = new AtomicReference<SVGFactory>();
    private static Boolean                           ICO_SUPPORTED;
    private volatile static Method                   ICO_DECODER;
    static {
        ImageIO.setUseCache(false);
    }

    /**
     * @param image
     * @return
     */
    public static BufferedImage blur(final BufferedImage image) {
        final float[] matrix = new float[400];
        for (int i = 0; i < 400; i++) {
            matrix[i] = 1.0f / 400.0f;
        }
        final BufferedImageOp op = new ConvolveOp(new Kernel(20, 20, matrix), ConvolveOp.EDGE_NO_OP, null);
        return op.filter(image, null);
    }

    public static Image centerImage(Image input) {
        return centerImage(input, Math.max(input.getWidth(null), input.getHeight(null)), Math.max(input.getWidth(null), input.getHeight(null)), null);
    }

    /**
     * crops or enlarges the image. does not scale, but crop or fill the background
     *
     * @param image
     * @param size
     * @param size2
     * @param object
     * @return
     */
    public static Image centerImage(Image input, int width, int height, Color background) {
        if (input.getWidth(null) == width && input.getHeight(null) == height) {
            return input;
        }
        if (MultiResolutionImageHelper.isSupported() && MultiResolutionImageHelper.isInstanceOf(input)) {
            // create a new MultiRes Image with all internal images cropped;
            int baseWidth = input.getWidth(null);
            int baseHeight = input.getHeight(null);
            List<Image> variants = MultiResolutionImageHelper.getResolutionVariants(input);
            Image[] newList = new Image[variants.size()];
            int baseIndex = 0;
            for (int i = 0; i < newList.length; i++) {
                Image v = variants.get(i);
                double wFactor = (double) v.getWidth(null) / baseWidth;
                double hFactor = (double) v.getHeight(null) / baseHeight;
                if (wFactor == 1d && hFactor == 1d) {
                    baseIndex = i;
                }
                newList[i] = centerImage(v, (int) (width * wFactor), (int) (height * hFactor), background);
            }
            return MultiResolutionImageHelper.create(baseIndex, newList);
        }
        BufferedImage newImage;
        newImage = createEmptyImage(width, height, input);
        Graphics2D g2d = newImage.createGraphics();
        double scale = g2d.getTransform().getScaleX();
        DebugMode.breakIf(scale > 1);
        g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, Interpolation.BILINEAR.getHint());
        g2d.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        if (background != null) {
            g2d.setColor(background);
            g2d.fillRect(0, 0, width, height);
        }
        int x = (width - input.getWidth(null)) / 2;
        int y = (height - input.getHeight(null)) / 2;
        g2d.drawImage(input, x, y, null);
        g2d.dispose();
        return newImage;
    }

    /**
     * Multiply the scale factor {@code sv} and the value {@code v} with appropriate clipping to the bounds of Integer resolution. If the
     * answer would be greater than {@code Integer.MAX_VALUE} then {@code Integer.MAX_VALUE} is returned. If the answer would be less than
     * {@code Integer.MIN_VALUE} then {@code Integer.MIN_VALUE} is returned. Otherwise the multiplication is returned.
     */
    public static int clipScale(final int v, final double sv) {
        if (sv == 1.0) {
            return v;
        }
        final double newv = v * sv;
        if (newv < Integer.MIN_VALUE) {
            return Integer.MIN_VALUE;
        }
        if (newv > Integer.MAX_VALUE) {
            return Integer.MAX_VALUE;
        }
        return (int) Math.round(newv);
    }

    public static BufferedImage colorRangeToTransparency(final BufferedImage image, final Color c1, final Color c2) {
        final int r1 = c1.getRed();
        final int g1 = c1.getGreen();
        final int b1 = c1.getBlue();
        final int r2 = c2.getRed();
        final int g2 = c2.getGreen();
        final int b2 = c2.getBlue();
        final ImageFilter filter = new RGBImageFilter() {
            @Override
            public final int filterRGB(final int x, final int y, final int rgb) {
                final int r = (rgb & 0xFF0000) >> 16;
                final int g = (rgb & 0xFF00) >> 8;
                final int b = rgb & 0xFF;
                if (r >= r1 && r <= r2 && g >= g1 && g <= g2 && b >= b1 && b <= b2) {
                    // Set fully transparent but keep color
                    // calculate a alpha value based on the distance between the
                    // range borders and the pixel color
                    final int dist = (Math.abs(r - (r1 + r2) / 2) + Math.abs(g - (g1 + g2) / 2) + Math.abs(b - (b1 + b2) / 2)) * 2;
                    return new Color(r, g, b, Math.min(255, dist)).getRGB();
                }
                return rgb;
            }
        };
        final ImageProducer ip = new FilteredImageSource(image.getSource(), filter);
        final Image img = Toolkit.getDefaultToolkit().createImage(ip);
        return IconIO.toBufferedImage(img);
    }

    /**
     * @param image
     * @param white
     * @param d
     * @return
     */
    public static BufferedImage colorRangeToTransparency(BufferedImage image, Color col, double tollerance) {
        final int r = col.getRed();
        final int g = col.getGreen();
        final int b = col.getBlue();
        final int a = col.getAlpha();
        return IconIO.colorRangeToTransparency(image, new Color(Math.max((int) (r * (1d - tollerance)), 0), Math.max((int) (g * (1d - tollerance)), 0), Math.max((int) (b * (1d - tollerance)), 0), a), new Color(Math.min(255, (int) (r * (1d + tollerance))), Math.min(255, (int) (g * (1d + tollerance))), Math.min(255, (int) (b * (1d + tollerance))), a));
    }

    public static BufferedImage convertIconToBufferedImage(final Icon icon) {
        if (icon == null) {
            return null;
        }
        if (icon instanceof ImageIcon) {
            final Image ret = ((ImageIcon) icon).getImage();
            if (ret instanceof BufferedImage) {
                return (BufferedImage) ret;
            }
        }
        final int w = icon.getIconWidth();
        final int h = icon.getIconHeight();
        final BufferedImage image = createEmptyImage(w, h, BufferedImage.TYPE_INT_ARGB, Transparency.TRANSLUCENT);
        final Graphics2D g = image.createGraphics();
        g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
        // g.setColor(Color.RED);
        // g.fillRect(0, 0, w, h);
        icon.paintIcon(null, g, 0, 0);
        g.dispose();
        return image;
    }

    public static BufferedImage createEmptyImage(final int w, final int h) {
        return createEmptyImage(w, h, -1, -1);
    }

    private static BufferedImage createEmptyImage(int width, int height, Image deriveFrom) {
        int imageType = BufferedImage.TYPE_INT_ARGB;
        int transparency = Transparency.TRANSLUCENT;
        if (deriveFrom instanceof BufferedImage) {
            imageType = ((BufferedImage) deriveFrom).getType();
            if (imageType <= 0) {
                imageType = BufferedImage.TYPE_INT_ARGB;
            }
            transparency = ((BufferedImage) deriveFrom).getTransparency();
            if (transparency <= 0) {
                transparency = Transparency.TRANSLUCENT;
            }
        }
        return createEmptyImage(width, height, imageType, transparency);
    }

    /**
     * @param width
     * @param height
     * @param imageType
     * @param transparency
     * @return
     */
    public static BufferedImage createEmptyImage(int width, int height, int imageType, int transparency) {
        if (imageType < 0) {
            imageType = BufferedImage.TYPE_INT_ARGB;
        }
        if (transparency < 0) {
            transparency = Transparency.TRANSLUCENT;
        }
        BufferedImage newImage;
        if (org.appwork.utils.Application.isHeadless()) {
            newImage = new BufferedImage(width, height, imageType);
        } else {
            final GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
            final GraphicsDevice gd = ge.getDefaultScreenDevice();
            final GraphicsConfiguration gc = gd.getDefaultConfiguration();
            newImage = gc.createCompatibleImage(width, height, transparency);
        }
        return newImage;
    }

    /**
     * @return
     */
    private static SVGFactory createSVGFactory() {
        final String fromSysProp = System.getProperty(SVG_FACTORY_KEY);
        if (fromSysProp != null && StringUtils.isEmpty(fromSysProp)) {
            return null;
        }
        final List<String> clazzes = Arrays.asList(fromSysProp, CLASS_ORG_APPWORK_UTILS_IMAGES_SVG_WEISJ_JSVG_FACTORY, CLASS_ORG_APPWORK_UTILS_IMAGES_SVG_KIT_FOX_FACTORY);
        for (final String clazz : clazzes) {
            if (StringUtils.isNotEmpty(clazz)) {
                try {
                    final SVGFactory factory = (SVGFactory) Class.forName(clazz, false, Thread.currentThread().getContextClassLoader()).newInstance();
                    if (factory.isSupported()) {
                        return factory;
                    }
                } catch (Throwable e) {
                    if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                        throw new WTFException(e);
                    }
                }
            }
        }
        return null;
    }

    /**
     * @param resource
     * @param w
     * @param h
     * @return
     */
    public static Icon getIcon(final URL resource, final int w, int h) {
        Icon ret = loadVectorIcon(resource, w, h);
        if (ret != null) {
            return ret;
        }
        Image image = loadImage(resource);
        if (image.getWidth(null) == w && image.getHeight(null) == h) {
            return new ImageIcon(image);
        } else if (w <= 0 && h <= 0) {
            return new ImageIcon(image);
        } else {
            return new ImageIcon(IconIO.getScaledInstance(image, w, h, Interpolation.BICUBIC, true));
        }
    }

    public static Icon getIconFromDataUrl(String dataURL) throws IOException {
        return new ImageIcon(getImageFromDataUrl(dataURL));
    }

    @Deprecated
    public static Image getImage(final URL resource, boolean Dummy) {
        return loadImage(resource);
    }

    /**
     * @param resource
     * @return
     */
    public static Image loadImage(final URL resource) {
        if (resource == null) {
            return null;
        }
        if (StringUtils.endsWithCaseInsensitive(resource.getPath(), ".exe") && isExeSupported()) {
            Image ret;
            try {
                ret = JNAImageHelper.getImageFromExe(resource);
                if (ret != null) {
                    return ret;
                }
            } catch (IOException e) {
                LogV3.log(e);
            }
        }
        if (StringUtils.endsWithCaseInsensitive(resource.getPath(), ".ico") && isIcoSupported()) {
            try {
                final InputStream is = resource.openStream();
                try {
                    List<BufferedImage> bufferedImages = (List<BufferedImage>) ICO_DECODER.invoke(null, is);
                    if (bufferedImages != null && bufferedImages.size() > 0) {
                        final ArrayList<Image> images = new ArrayList<Image>(bufferedImages);
                        MultiResolutionImageHelper.sortImagesBySize(images);
                        if (images.size() > 1 && MultiResolutionImageHelper.isSupported()) {
                            // biggest one as base image
                            return org.appwork.resources.MultiResolutionImageHelper.create(images.size() - 1, images.toArray(new Image[0]));
                        } else {
                            return images.get(images.size() - 1);
                        }
                    }
                } finally {
                    is.close();
                }
            } catch (InvocationTargetException e) {
                LogV3.log(e);
            } catch (IllegalAccessException e) {
                LogV3.log(e);
            } catch (IOException e) {
                LogV3.log(e);
            }
        }
        InputStream is = null;
        /*
         * workaround for http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=7166379
         */
        /*
         * http://stackoverflow.com/questions/10441276/jdk-1-7-too-many-open- files-due-to-posix-semaphores
         */
        try {
            is = URLStream.openStream(resource);
            final BufferedImage ret = ImageIO.read(is);
            if (ret != null) {
                return ret;
            }
        } catch (final IOException e) {
            org.appwork.loggingv3.LogV3.log(new IOException("URL:" + resource, e));
        } finally {
            try {
                is.close();
            } catch (final Throwable e) {
            }
        }
        return null;
    }

    /**
     * @param exampleDataUrl
     * @return
     * @throws IOException
     */
    public static Image getImageFromDataUrl(String dataURL) throws IOException {
        return ImageIO.read(IO.dataUrlToInputStream(dataURL));
    }

    /**
     * Returns an icon for the given resource, but only if the resource is a non-bitmap icon like a svg. Anything we cannot read as an image
     *
     * @param resource
     * @param w
     * @param h
     * @return
     */
    public static Icon loadVectorIcon(final URL resource, final int w, int h) {
        if (resource != null && StringUtils.endsWithCaseInsensitive(resource.getPath(), ".svg")) {
            if (getSvgFactory() != null) {
                try {
                    InputStream is = resource.openStream();
                    try {
                        return getSvgFactory().getIconFromSVG(is, w, h, null);
                    } finally {
                        is.close();
                    }
                } catch (IOException e) {
                    LogV3.log(e);
                }
            } else {
                LogV3.warning("SVG Factory not found!");
            }
        }
        return null;
    }

    public static Icon getScaledInstance(final Icon icon, final int width, final int height) {
        return IconIO.getScaledInstance(icon, width, height, Interpolation.BICUBIC);
    }

    /**
     * @param icon
     * @param width
     * @param height
     * @param bicubic
     * @param higherQuality
     * @return
     */
    public static Icon getScaledInstance(final Icon icon, final int width, final int height, final Interpolation bicubic) {
        if (icon.getIconHeight() == height && icon.getIconWidth() == width) {
            return icon;
        } else if (icon instanceof ScalableIcon) {
            return new ScaledIcon(icon, width, height, bicubic);
        } else if (icon instanceof ScaledIcon) {
            final ScaledIcon scaledIcon = (ScaledIcon) icon;
            if (scaledIcon.getIconHeight() == height && scaledIcon.getIconWidth() == width) {
                return icon;
            } else {
                final ScaledIcon newScaledIcon = new ScaledIcon(scaledIcon.getOrigin(), width, height, bicubic);
                if (newScaledIcon.getIconHeight() == scaledIcon.getIconHeight() && newScaledIcon.getIconWidth() == scaledIcon.getIconWidth()) {
                    return scaledIcon;
                } else {
                    return newScaledIcon;
                }
            }
        } else if (icon instanceof CanScaleItSelfIcon) {
            return ((CanScaleItSelfIcon) icon).getScaledInstance(width, height, bicubic);
        }
        return new ScaledIcon(icon, width, height, bicubic);
    }

    /**
     * @param image
     * @param i
     * @param j
     * @return
     */
    public static Image getScaledInstance(final Image img, final int width, final int height) {
        return IconIO.getScaledInstance(img, width, height, Interpolation.BICUBIC, true);
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
     * @param higherQuality
     *            if true, this method will use a multi-step scaling technique that provides higher quality than the usual one-step
     *            technique (only useful in downscaling cases, where {@code targetWidth} or {@code targetHeight} is smaller than the
     *            original dimensions, and generally only when the {@code BILINEAR} hint is specified)
     * @return a scaled version of the original {@code BufferedImage}
     */
    public static Image getScaledInstance(final Image img, int width, int height, final Interpolation interpolation, final boolean higherQuality) {
        return getScaledInstance(img, width, height, interpolation, higherQuality, true);
    }

    public static Image getScaledInstance(Image img, int width, int height, final Interpolation interpolation, final boolean higherQuality, boolean keepratio) {
        DebugMode.breakIf(img == null);
        final double faktor = Math.max((double) img.getWidth(null) / width, (double) img.getHeight(null) / height);
        if (keepratio || width <= 0 || height <= 0) {
            if (faktor == 1.0) {
                return img;
            }
            // if height or width is 0 or less, this means to keep ratio and scale based on the remaining dimension
            width = (int) Math.max(Math.round(img.getWidth(null) / faktor), 1);
            height = (int) Math.max(Math.round(img.getHeight(null) / faktor), 1);
        } else {
            if (height == img.getHeight(null) && width == img.getWidth(null)) {
                return img;
            }
        }
        if (MultiResolutionImageHelper.isInstanceOf(img)) {
            // find best base variant
            img = MultiResolutionImageHelper.getResolutionVariant(img, width, height);
        }
        Image ret = img;
        int w, h;
        if (higherQuality) {
            // Use multi-step technique: start with original size, then
            // scale down in multiple passes with drawImage()
            // until the target size is reached
            w = Math.max(width, img.getWidth(null));
            h = Math.max(height, img.getHeight(null));
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
            final BufferedImage tmp = createEmptyImage(w, h, img);
            final Graphics2D g2 = tmp.createGraphics();
            g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, interpolation.getHint());
            g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
            g2.drawImage(ret, 0, 0, w, h, null);
            g2.dispose();
            ret = tmp;
        } while (w != width || h != height);
        return ret;
    }

    // /**
    // * @param img
    // * @param bestMatch
    // * @return
    // */
    // private static Image createNewBaseMulti(Image img, Image bestMatch) {
    // List<Image> variants = ((MultiResolutionImageWrapper) img).getResolutionVariants();
    // int index = variants.indexOf(bestMatch);
    // if (index < 0) {
    // // not part of the image? maybe is not a baseMulti and the image was created on the fly
    // variants = new ArrayList<Image>(variants);
    // variants.add(bestMatch);
    // sortImagesBySize(variants);
    // index = variants.indexOf(bestMatch);
    // }
    // return new BaseMultiResolutionImageWrapper(index, variants.toArray(new Image[0]));
    // }
    public static SVGFactory getSvgFactory() {
        SVGFactory factory = SVG_FACTORY.get();
        if (factory == null) {
            synchronized (SVG_FACTORY) {
                factory = SVG_FACTORY.get();
                if (factory == null) {
                    factory = createSVGFactory();
                    if (factory == null) {
                        factory = new NoSVGSupportFactory();
                    }
                    SVG_FACTORY.set(factory);
                }
            }
        }
        if (!factory.isSupported()) {
            return null;
        } else {
            return factory;
        }
    }

    /**
     * @param icon
     * @param f
     * @return
     */
    public static Image getTransparent(final Image src, final float f) {
        final int w = src.getWidth(null);
        final int h = src.getHeight(null);
        final BufferedImage image = createEmptyImage(w, h, BufferedImage.TYPE_INT_ARGB, Transparency.TRANSLUCENT);
        final Graphics2D g = image.createGraphics();
        g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, f));
        g.drawImage(src, 0, 0, null);
        g.dispose();
        return image;
    }

    /**
     * @return
     */
    public static boolean isExeSupported() {
        return JNAHelper.isJNAAvailable() && CrossSystem.isWindows();
    }

    /**
     * @return
     */
    public static boolean isIcoSupported() {
        if (ICO_SUPPORTED != null) {
            return ICO_SUPPORTED == Boolean.TRUE;
        }
        try {
            Class<?> cls = Class.forName("net.sf.image4j.codec.ico.ICODecoder");
            Method method = cls.getMethod("read", new Class[] { InputStream.class });
            ParameterizedType type = (ParameterizedType) method.getGenericReturnType();
            if (type.getRawType() == List.class) {
                if (type.getActualTypeArguments()[0] == BufferedImage.class) {
                    ICO_DECODER = method;
                    ICO_SUPPORTED = true;
                }
            }
        } catch (ClassNotFoundException e) {
            LogV3.log(e);
            ICO_SUPPORTED = false;
        } catch (NoSuchMethodException e) {
            LogV3.log(e);
            ICO_SUPPORTED = false;
        } catch (SecurityException e) {
            LogV3.log(e);
            ICO_SUPPORTED = false;
        }
        return ICO_SUPPORTED == Boolean.TRUE;
    }

    /**
     * return true if the image is big enough to get scaled to the target viewport without upscaling or changing ratio
     *
     * @param width
     * @param height
     * @param baseImage
     * @return
     */
    public static boolean isImageCanGetDownscaled(int width, int height, Image img) {
        // if (MultiResolutionImageHelper.isInstanceOf(img)) {
        // img = MultiResolutionImageHelper.getResolutionVariant(img, width, height);
        // }
        final double faktor = Math.max((double) img.getWidth(null) / width, (double) img.getHeight(null) / height);
        if (faktor < 1d) {
            // we would have to upscale
            return false;
        }
        int targetWidth = (int) Math.max(Math.round(img.getWidth(null) / faktor), 1);
        int targetHeight = (int) Math.max(Math.round(img.getHeight(null) / faktor), 1);
        return (width <= 0 || img.getWidth(null) >= targetWidth) && (height <= 0 || img.getHeight(null) >= targetHeight);
    }

    /**
     * returns true, if the image size is exact the given width or height. if width or height is <=0, any value is accepted
     *
     */
    public static boolean isImageDimensionExact(int width, int height, Image baseImage) {
        return (width <= 0 || baseImage.getWidth(null) == width) && (height <= 0 || baseImage.getHeight(null) == height);
    }

    /**
     * This function removes the major color of the image and replaces it with transparency.
     *
     * @param image
     * @return
     */
    public static BufferedImage removeBackground(final BufferedImage image, final double tollerance) {
        final HashMap<Integer, Integer> map = new HashMap<Integer, Integer>();
        int biggestValue = 0;
        int color = -1;
        for (final int rgb : image.getRGB(0, 0, image.getWidth() - 1, image.getHeight() - 1, null, 0, image.getWidth())) {
            Integer v = map.get(rgb);
            if (v == null) {
                v = 0;
            }
            v++;
            map.put(rgb, v);
            if (v > biggestValue) {
                biggestValue = v;
                color = rgb;
            }
        }
        final Color col = new Color(color);
        final int r = col.getRed();
        final int g = col.getGreen();
        final int b = col.getBlue();
        final int a = col.getAlpha();
        return IconIO.colorRangeToTransparency(image, new Color(Math.max((int) (r * (1d - tollerance)), 0), Math.max((int) (g * (1d - tollerance)), 0), Math.max((int) (b * (1d - tollerance)), 0), a), new Color(Math.min(255, (int) (r * (1d + tollerance))), Math.min(255, (int) (g * (1d + tollerance))), Math.min(255, (int) (b * (1d + tollerance))), a));
    }

    /**
     * @param bufferedImage
     * @param color
     * @return
     */
    public static Image replaceColor(BufferedImage image, final Color search, final int tollerance, final Color replace, final boolean keepBrightness) {
        if (search == null) {
            final ImageFilter filter = new RGBImageFilter() {
                @Override
                public final int filterRGB(final int x, final int y, final int rgb) {
                    final int a = (rgb >> 24) & 0xff;
                    final int r = (rgb >> 16) & 0xff;
                    final int g = (rgb >> 8) & 0xff;
                    final int b = (rgb >> 0) & 0xff;
                    final double brightness = !keepBrightness ? 1 : (0.299 * r + 0.578 * g + 0.114 * b);
                    Color nc = new Color((int) (replace.getRed() * brightness), (int) (replace.getGreen() * brightness), (int) (replace.getBlue() * brightness), (replace.getAlpha() * a) / 255);
                    return nc.getRGB();
                }
            };
            final ImageProducer ip = new FilteredImageSource(image.getSource(), filter);
            final Image img = Toolkit.getDefaultToolkit().createImage(ip);
            return img;
        } else {
            final int a1 = search.getAlpha();
            final int r1 = search.getRed();
            final int g1 = search.getGreen();
            final int b1 = search.getBlue();
            final ImageFilter filter = new RGBImageFilter() {
                @Override
                public final int filterRGB(final int x, final int y, final int rgb) {
                    final int a = (rgb >> 24) & 0xff;
                    final int r = (rgb >> 16) & 0xff;
                    final int g = (rgb >> 8) & 0xff;
                    final int b = (rgb >> 0) & 0xff;
                    if (Math.abs(r - r1) <= tollerance && Math.abs(g - g1) <= tollerance && Math.abs(b - b1) <= tollerance && Math.abs(a - a1) <= tollerance) {
                        if (!keepBrightness) {
                            return replace.getRGB();
                        }
                        final double brightness = (0.299 * r + 0.578 * g + 0.114 * b) / 255d;
                        Color nc = new Color((int) (replace.getRed() * brightness), (int) (replace.getGreen() * brightness), (int) (replace.getBlue() * brightness), replace.getAlpha() * a / 255);
                        return nc.getRGB();
                    }
                    return rgb;
                }
            };
            final ImageProducer ip = new FilteredImageSource(image.getSource(), filter);
            final Image img = Toolkit.getDefaultToolkit().createImage(ip);
            return img;
        }
    }

    /**
     * @param keepBrightness
     *            TODO
     * @param checkBoxImage
     * @param i
     * @return
     */
    public static Icon replaceColor(Icon icon, final Color search, final int tollerance, final Color replace, boolean keepBrightness) {
        return new ImageIcon(replaceColor(toBufferedImage(icon), search, tollerance, replace, keepBrightness));
    }

    /**
     * @param drop
     * @param i
     * @return
     */
    public static BufferedImage rotate(final BufferedImage src, final int degree) {
        final int w = src.getWidth(null);
        final int h = src.getHeight(null);
        final AffineTransform at = new AffineTransform();
        at.rotate(degree * Math.PI / 180.0);
        Point2D p2din, p2dout;
        p2din = new Point2D.Double(0.0, 0.0);
        p2dout = at.transform(p2din, null);
        double ytrans = p2dout.getY();
        double xtrans = p2dout.getX();
        p2din = new Point2D.Double(0, h);
        p2dout = at.transform(p2din, null);
        ytrans = Math.min(ytrans, p2dout.getY());
        xtrans = Math.min(xtrans, p2dout.getX());
        p2din = new Point2D.Double(w, h);
        p2dout = at.transform(p2din, null);
        ytrans = Math.min(ytrans, p2dout.getY());
        xtrans = Math.min(xtrans, p2dout.getX());
        p2din = new Point2D.Double(w, 0);
        p2dout = at.transform(p2din, null);
        ytrans = Math.min(ytrans, p2dout.getY());
        xtrans = Math.min(xtrans, p2dout.getX());
        final AffineTransform tat = new AffineTransform();
        tat.translate(-xtrans, -ytrans);
        at.preConcatenate(tat);
        final AffineTransformOp bio = new AffineTransformOp(at, AffineTransformOp.TYPE_BILINEAR);
        final Rectangle r = bio.getBounds2D(src).getBounds();
        BufferedImage image = createEmptyImage(r.width, r.height, src);
        image = bio.filter(src, image);
        return image;
    }

    /**
     * @param fileIcon
     * @return
     */
    public static BufferedImage toBufferedImage(final Icon icon) {
        if (icon instanceof ImageIcon) {
            final Image img = ((ImageIcon) icon).getImage();
            if (img instanceof BufferedImage) {
                return (BufferedImage) img;
            }
        }
        final int w = icon.getIconWidth();
        final int h = icon.getIconHeight();
        BufferedImage image = createEmptyImage(w, h);
        final Graphics2D g = image.createGraphics();
        g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
        g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        icon.paintIcon(null, g, 0, 0);
        g.dispose();
        return image;
    }

    /**
     * Converts any image to a BufferedImage
     *
     * @param image
     * @return
     */
    public static BufferedImage toBufferedImage(final Image src) {
        if (src instanceof BufferedImage) {
            return (BufferedImage) src;
        } else {
            final int w = src.getWidth(null);
            final int h = src.getHeight(null);
            final BufferedImage image = createEmptyImage(w, h, src);
            final Graphics2D g = image.createGraphics();
            g.drawImage(src, 0, 0, null);
            g.dispose();
            return image;
        }
    }

    public static String toDataUrl(BufferedImage image, DataURLFormat dataURLFormat) throws IOException {
        final ByteArrayOutputStream bos = new ByteArrayOutputStream();
        final Base64OutputStream b64os = new Base64OutputStream(bos);
        final BufferedImage ouput;
        switch (dataURLFormat) {
        case JPG:
            // removes alpha channel
            ouput = createEmptyImage(image.getWidth(), image.getHeight(), BufferedImage.TYPE_INT_RGB, Transparency.OPAQUE);
            break;
        default:
        case PNG:
            // keeps alpha channel
            ouput = createEmptyImage(image.getWidth(), image.getHeight(), BufferedImage.TYPE_INT_ARGB, Transparency.TRANSLUCENT);
            break;
        }
        final Graphics g = ouput.getGraphics();
        g.drawImage(image, 0, 0, null);
        g.dispose();
        final String ret;
        switch (dataURLFormat) {
        case JPG:
            ImageProvider.writeImage(ouput, "jpg", b64os);
            b64os.close();
            ret = "image/jpeg;base64," + bos.toString("UTF-8");
            break;
        default:
        case PNG:
            ImageProvider.writeImage(ouput, "png", b64os);
            b64os.close();
            ret = "image/png;base64," + bos.toString("UTF-8");
            break;
        }
        return ret;
    }

    /**
     * @param image
     * @return
     */
    public static Image toGrayScale(Image input) {
        // GrayFilter.createDisabledImage has already multiressupport
        Image ret = GrayFilter.createDisabledImage(input);
        return ret;
    }

    public static Image toImage(final Icon icon) {
        Icon i = icon;
        while (i != null) {
            if (i.getIconHeight() != icon.getIconHeight() || i.getIconWidth() != icon.getIconWidth()) {
                break;
            }
            if (i instanceof ImageIcon) {
                return ((ImageIcon) i).getImage();
            }
            if (i instanceof IconPipe) {
                Set<ModificationType> mods = ((IconPipe) i).getModifications();
                if (mods == null || mods.contains(ModificationType.NONE)) {
                    i = ((IconPipe) i).getDelegate();
                    continue;
                }
            }
            break;
        }
        return toBufferedImage(icon);
    }

    /**
     * @param ico
     * @return
     */
    public static ImageIcon toImageIcon(final Icon icon) {
        if (icon == null) {
            return null;
        } else if (icon instanceof ImageIcon) {
            return (ImageIcon) icon;
        } else {
            return new ImageIcon(IconIO.toBufferedImage(icon));
        }
    }

    /**
     * Save image as a compresssed jpeg and returns the bytes
     *
     * @param read
     * @return
     * @throws IOException
     */
    public static byte[] toJpgBytes(Image image) throws IOException {
        final ByteArrayOutputStream bos = new ByteArrayOutputStream();
        final BufferedImage jpg = createEmptyImage(image.getWidth(null), image.getHeight(null), BufferedImage.TYPE_INT_RGB, Transparency.OPAQUE);
        final Graphics g = jpg.getGraphics();
        g.drawImage(image, 0, 0, null);
        g.dispose();
        ImageProvider.writeImage(jpg, "jpg", bos);
        bos.close();
        return bos.toByteArray();
    }

    public static byte[] toPngBytes(Image image) throws IOException {
        final ByteArrayOutputStream bos = new ByteArrayOutputStream();
        final BufferedImage jpg = createEmptyImage(image.getWidth(null), image.getHeight(null), BufferedImage.TYPE_INT_ARGB, Transparency.TRANSLUCENT);
        final Graphics g = jpg.getGraphics();
        g.drawImage(image, 0, 0, null);
        g.dispose();
        ImageProvider.writeImage(jpg, "png", bos);
        bos.close();
        return bos.toByteArray();
    }

    /**
     * @param icon
     * @return
     */
    public static IconIdentifier getPrimaryIdentifier(Icon icon) {
        while (icon != null) {
            if (icon instanceof IDIcon) {
                return ((IDIcon) icon).getIdentifier();
            }
            if (icon instanceof IconPipe) {
                icon = ((IconPipe) icon).getDelegate();
            } else {
                break;
            }
        }
        return null;
    }

    public static Image getPrimaryImage(Icon icon) {
        while (icon != null) {
            if (icon instanceof ImageIcon) {
                return ((ImageIcon) icon).getImage();
            }
            if (icon instanceof IconPipe) {
                icon = ((IconPipe) icon).getDelegate();
            } else {
                break;
            }
        }
        return null;
    }

    /**
     * parameters may be -1. what means : don't care
     *
     * @param tolerateFromWidth
     * @param tolerateToWidth
     * @param tolerateFromHeight
     * @param tolerateToHeight
     * @return true if the image is within the given range (incl.)
     */
    public static boolean isImageDimensionWithinRange(Image img, int fromWidth, int toWidth, int fromHeight, int toHeight) {
        boolean widthInTolerance = fromWidth <= 0 || img.getWidth(null) >= fromWidth;
        widthInTolerance &= toWidth <= 0 || img.getWidth(null) <= toWidth;
        if (widthInTolerance) {
            boolean heightInTolerance = fromHeight <= 0 || img.getHeight(null) >= fromHeight;
            heightInTolerance &= toHeight <= 0 || img.getHeight(null) <= toHeight;
            if (heightInTolerance) {
                return true;
            }
        }
        return false;
    }
}
