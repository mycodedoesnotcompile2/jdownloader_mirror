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
package org.appwork.resources;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.Transparency;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import javax.imageio.ImageIO;
import javax.swing.Icon;
import javax.swing.JComponent;

import org.appwork.loggingv3.LogV3;
import org.appwork.resources.ThemeContext.Target;
import org.appwork.storage.config.MinTimeWeakReference;
import org.appwork.storage.config.MinTimeWeakReferenceCleanup;
import org.appwork.swing.components.CheckBoxIcon;
import org.appwork.swing.components.HeadlessCheckboxIconRef;
import org.appwork.utils.Application;
import org.appwork.utils.DebugMode;
import org.appwork.utils.IO;
import org.appwork.utils.JavaVersion;
import org.appwork.utils.StringUtils;
import org.appwork.utils.images.DebugIcon;
import org.appwork.utils.images.IconDebugger;
import org.appwork.utils.images.IconIO;
import org.appwork.utils.images.IconPipe;
import org.appwork.utils.images.Interpolation;
import org.appwork.utils.images.ModificationType;
import org.appwork.utils.images.ScalableIcon;
import org.appwork.utils.reflection.Clazz;

/**
 *
 * @author thomas
 *
 */
public class Theme implements MinTimeWeakReferenceCleanup {
    /**
     *
     */
    private static final String _VECTOR                 = "?vector";
    /**
     * Warning. Tests may use this text
     */
    public static final String  NO_IMAGE_AVAILABLE      = "No Image/Icon available: ";
    /**
     *
     */
    public static final String  FINAL                   = "final_";
    /**
     *
     */
    private static final String ENHANCE_DOUBLE_SIZE_TAG = "@2x";

    /**
     * @author thomas
     * @date Feb 3, 2025
     *
     */
    private final class MinTimeWeakRefNamed<IconType> extends MinTimeWeakReference<IconType> {
        private String name;

        /**
         * @param ret
         * @param minLifeTime
         * @param id
         * @param cleanupMinTimeWeakReference
         */
        private MinTimeWeakRefNamed(IconType ret, long minLifeTime, String id, MinTimeWeakReferenceCleanup cleanupMinTimeWeakReference) {
            super(ret, minLifeTime, id, cleanupMinTimeWeakReference);
            if (ret instanceof Icon) {
                name = id + "_" + ((Icon) ret).getIconWidth() + ":" + ((Icon) ret).getIconHeight();
            } else {
                name = id + "_" + ((Image) ret).getWidth(null) + ":" + ((Image) ret).getHeight(null);
            }
        }

        /**
         * @see org.appwork.storage.config.MinTimeWeakReference#toString()
         */
        @Override
        public String toString() {
            return name;
        }
    }

    private String                                              path;
    // private final HashMap<String, MinTimeWeakReference<BufferedImage>>
    // imageCache = new HashMap<String, MinTimeWeakReference<BufferedImage>>();
    protected final HashMap<String, MinTimeWeakReference<Icon>> imageIconCache           = new HashMap<String, MinTimeWeakReference<Icon>>();
    private long                                                cacheLifetime            = 20000l;
    private String                                              theme;
    private String                                              nameSpace;
    private final boolean                                       doLogMissing             = "false".equals(System.getProperty("DO_NOT_LOG_MISSING_ICONS_EXCEPTION", "false"));
    private boolean                                             useHighDPI;
    private static final String                                 MAX_SIZE_KEY             = "ORIGINAL";
    public static final int[]                                   FRAME_ICON_SIZES_WINDOWS = new int[] { 128, 64, 48, 32, 24, 20, 16 };
    private static final boolean                                ICONDEBUGGER             = System.getProperty("ICONDEBUGGER") != null;

    public Theme(final String namespace) {
        this.setNameSpace(namespace);
        this.setTheme("standard");
        this.useHighDPI = false;
        if (JavaVersion.getVersion().isMinimum(JavaVersion.JVM_9_0)) {
            useHighDPI = true;
        }
    }

    public void cache(final Icon ret, final String key) {
        if (ret != null) {
            synchronized (this.imageIconCache) {
                this.imageIconCache.put(key, new MinTimeWeakRefNamed<Icon>(ret, this.getCacheLifetime(), key, this));
            }
        }
    }

    private Theme      delegate;
    private String     defaultPath;
    public static File RESOURCE_HELPER_ROOT;

    /**
     * @param i
     */
    public void setDelegate(Theme i) {
        if (i == this) {
            this.delegate = null;
        } else {
            this.delegate = i;
        }
    }

    /**
     *
     */
    public void clearCache() {
        synchronized (this.imageIconCache) {
            this.imageIconCache.clear();
        }
    }

    public Icon getCached(final String key) {
        synchronized (this.imageIconCache) {
            final MinTimeWeakReference<Icon> cache = this.imageIconCache.get(key);
            if (cache != null) {
                return cache.get();
            }
            return null;
        }
    }

    /**
     * @param relativePath
     * @param size
     * @return
     */
    protected String getCacheKey(final Object... objects) {
        final StringBuilder sb = new StringBuilder();
        for (Object o : objects) {
            if (o == null) {
                o = "null";
            }
            if (sb.length() > 0) {
                sb.append("_");
            }
            if (o instanceof ThemeContext) {
                sb.append(((ThemeContext) o).toKeyID());
            } else if (Clazz.isPrimitive(o.getClass())) {
                sb.append(String.valueOf(o));
            } else if (Clazz.isString(o.getClass())) {
                sb.append(String.valueOf(o));
            } else {
                DebugMode.debugger("Forbidden key element");
            }
        }
        return sb.toString();
    }

    public long getCacheLifetime() {
        return this.cacheLifetime;
    }

    public Icon getDisabledIcon(final Icon input) {
        return getDisabledIcon(null, input);
    }

    protected Theme getDelegate() {
        return delegate;
    }

    public Icon getDisabledIcon(final JComponent component, final Icon input) {
        if (input != null) {
            Icon ret = null;
            final Theme delegate = getDelegate();
            if (delegate != null) {
                ret = delegate.getDisabledIcon(input);
            }
            if (ret == null) {
                ret = FACTORY.getDisabled(component, input);
            }
            return ret;
        }
        return null;
    }

    public Icon getIcon(final String relativePath, final int size) {
        return this.getIcon(relativePath, size, size, null);
    }

    public Icon getIcon(final String relativePath, final int width, final int height) {
        return getIcon(relativePath, width, height, null);
    }

    protected Icon onIconNotAvailable(String relativePath, ThemeContext context, String cacheKey, int width, int height) {
        if (context.getTarget() != Target.ICON) {
            DebugMode.debugger();
            return null;
        }
        LogV3.info(NO_IMAGE_AVAILABLE + relativePath);
        if (doLogMissing) {
            org.appwork.loggingv3.LogV3.log(new Exception("Icon missing: " + this.buildPath("images/", relativePath, ".png|svg|ico|exe", false)));
        }
        DebugMode.debugger();
        final BufferedImage ret = createNotAvailableImage(width, height);
        final Icon icon = FACTORY.imageToIcon(ret, width, height);
        if (context.isUsecache()) {
            this.cache(icon, cacheKey);
        }
        return icon;
    }

    /**
     * @param width
     * @param height
     * @return
     */
    private BufferedImage createNotAvailableImage(int width, int height) {
        if (width <= 0) {
            width = 32;
        }
        if (height <= 0) {
            height = 32;
        }
        final BufferedImage ret = IconIO.createEmptyImage(width, height);
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            final Graphics2D g = (Graphics2D) ret.getGraphics();
            g.setColor(Color.RED);
            g.fillRect(0, 0, width, height);
            g.dispose();
        }
        return ret;
    }

    public Icon getIcon(final String relativePath, final int width, final int height, ThemeContext context) {
        Icon ret = null;
        final Theme delegate = getDelegate();
        if (delegate != null) {
            ret = delegate.getIcon(relativePath, width, height, context);
            if (ret != null) {
                return ret;
            }
        }
        String cacheKey = null;
        if (context == null || context.isUsecache()) {
            cacheKey = this.getCacheKey(relativePath, context, width, height);
            ret = this.getCached(cacheKey);
            if (ret == null) {
                // try vector
                Icon cachedRawIcon = this.getCached(relativePath);
                if (cachedRawIcon instanceof ScalableIcon) {
                    // re-use a cached svg
                    ret = FACTORY.scale(cachedRawIcon, width, height);
                    cache(ret, cacheKey);
                    return ret;
                }
            }
        }
        if (ret != null) {
            return ret;
        }
        // ^^^^^^^
        // the context might be null above. we create a default context only if the cache did not hit
        if (context == null) {
            context = createNewDefaultContext();
        }
        // jd webinterface
        if (StringUtils.equalsIgnoreCase(relativePath, "disabled") || (Target.ICON.equals(context.getTarget()) && HeadlessCheckboxIconRef.HEADLESS_checkbox_false.path().equals(relativePath))) {
            ret = new CheckBoxIcon(Math.max(width, height), false, true);
        } else if (StringUtils.equalsIgnoreCase(relativePath, "enabled") || (Target.ICON.equals(context.getTarget()) && HeadlessCheckboxIconRef.HEADLESS_checkbox_true.path().equals(relativePath))) {
            ret = new CheckBoxIcon(Math.max(width, height), true, true);
        } else if (StringUtils.equalsIgnoreCase(relativePath, "checkbox_undefined")) {
            ret = new CheckBoxIcon(Math.max(width, height), true, false);
        }
        context.ensureTarget(Target.ICON);
        if (ret == null) {
            final URL url = lookupImageUrl(relativePath, context);
            if (url == null) {
                return onIconNotAvailable(relativePath, context, cacheKey, width, height);
            }
            ret = FACTORY.urlToVectorIcon(url, width, height);
            if (ret != null) {
                Icon search = ret;
                // find the scalable icon to cache it. the icon might be wrapped as IDIcon or anything like this
                while (search != null) {
                    if (search instanceof ScalableIcon) {
                        cache(search, relativePath);
                        break;
                    }
                    if (search instanceof IconPipe) {
                        Set<ModificationType> mods = ((IconPipe) search).getModifications();
                        if (mods == null || mods.contains(ModificationType.NONE)) {
                            // just a wrapper icon - we can search deeper without risking to skip any important modifications
                            search = ((IconPipe) search).getDelegate();
                        }
                    } else {
                        break;
                    }
                }
                LogV3.info("Loaded vector icon: " + url);
            }
            // do not use the getImage fallback if the origin request came from the getImage Method
            if (ret == null && context.getTarget() == Target.ICON) {
                // fallback code if the requested icon is not a vector graphics, but a png or anything like this
                final Image image = getImage(relativePath, width, height, context);
                if (image != null) {
                    // we cache the icon as well, else we would get many lookupImageUrl calls - slow!
                    ret = FACTORY.imageToIcon(image, width, height);
                }
            }
        }
        if (ret == null) {
            if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                DebugMode.breakIf(ret == null && lookupImageUrl(relativePath, context) != null);
            }
            ret = onIconNotAvailable(relativePath, context, cacheKey, width, height);
            if (ret != null && context.isDebugDrawEnabled() && context.getTarget() == Target.ICON) {
                ret = new DebugIcon(ret);
            }
            return ret;
        } else {
            if (context.isDebugDrawEnabled() && context.getTarget() == Target.ICON) {
                ret = new DebugIcon(ret);
            }
            if (context.isUsecache()) {
                this.cache(ret, cacheKey);
            }
            return ret;
        }
    }

    private static IconFactory FACTORY = createIconFactory();

    public static IconFactory getFACTORY() {
        return FACTORY;
    }

    public static void setFACTORY(IconFactory fACTORY) {
        if (fACTORY == null) {
            throw new IllegalArgumentException();
        }
        FACTORY = fACTORY;
    }

    /**
     * @param relativePath
     * @param context
     *            TODO
     */
    protected URL lookupImageUrl(String relativePath, ThemeContext context) {
        URL url = context == null ? null : context.getURL(relativePath);
        if (url != null) {
            return url;
        }
        for (boolean useStandardPath : new boolean[] { false, true }) {
            url = this.getURL("images/", relativePath, "", useStandardPath);
            if (url == null && IconIO.getSvgFactory() != null) {
                url = this.getURL("images/", relativePath, ".svg", useStandardPath);
            }
            if (url == null) {
                url = this.getURL("images/", relativePath, ".png", useStandardPath);
            }
            if (url == null && IconIO.isIcoSupported()) {
                url = this.getURL("images/", relativePath, ".ico", useStandardPath);
            }
            if (url == null && IconIO.isExeSupported()) {
                url = this.getURL("images/", relativePath, ".exe", useStandardPath);
            }
            if (url != null) {
                if (context != null) {
                    context.putURL(relativePath, url);
                }
                return url;
            }
        }
        return null;
    }

    /**
     * @return
     */
    private static IconFactory createIconFactory() {
        String iconFactoryClass = System.getProperty("ICON_FACTORY");
        if (StringUtils.isEmpty(iconFactoryClass)) {
            iconFactoryClass = DefaultIconFactory.class.getName();
        }
        try {
            return (IconFactory) Class.forName(iconFactoryClass).getConstructor().newInstance();
        } catch (Exception e) {
            LogV3.log(e);
            return new DefaultIconFactory();
        }
    }

    public Image getImage(final String relativePath, final int size) {
        return this.getImage(relativePath, size, size);
    }

    public Image getImage(String key, int width, int height) {
        return getImage(key, width, height, null);
    }

    /**
     * @return
     */
    protected ThemeContext createNewDefaultContext() {
        return new ThemeContext();
    }

    public Image getImage(String key, int width, int height, ThemeContext context) {
        final Theme delegate = getDelegate();
        if (delegate != null) {
            Image image = delegate.getImage(key, width, height, context);
            if (image != null) {
                return image;
            }
        }
        HashMap<String, MinTimeWeakReference<Image>> cached = null;
        String finalKey = FINAL + width + "_" + height + "_" + (context == null ? null : context.toKeyID());
        if (context == null || context.isUsecache()) {
            synchronized (this.imageCache) {
                cached = imageCache.get(key);
                if (cached == null) {
                    cached = new HashMap<String, MinTimeWeakReference<Image>>();
                    imageCache.put(key, cached);
                } else {
                    MinTimeWeakReference<Image> finalCache = cached.get(finalKey);
                    if (finalCache != null) {
                        Image img = finalCache.get();
                        if (img != null) {
                            return img;
                        }
                    }
                }
            }
        }
        // ^^^^^
        // the context might be null above. we create a new default context only if the final cache did not hit
        if (context == null) {
            context = createNewDefaultContext();
        }
        context.ensureTarget(ThemeContext.Target.IMAGE);
        double highestRequiredScale = MultiResolutionImageHelper.getHighestMonitorScaling();
        int highDPIWidth = width;
        int highDPIHeight = height;
        if (context.isCreateMultiResolutionImages() && isUseHighDPI() && highestRequiredScale > 1) {
            // the internal highDPI logic searches for images that are bigger than the required size. the required size is calculated as
            // double, so java searches for an image with at least 23 pixel if 25.5 are required.
            // bei using ceil here, we ensure that we always have an image big enough and will not have to scale up anything.
            // this reduces issues with highDPI icons painted "between" physical pixels
            highDPIWidth = (int) Math.round(width * highestRequiredScale);
            highDPIHeight = (int) Math.round(height * highestRequiredScale);
        }
        boolean returnMultiResImage = width != highDPIWidth || height != highDPIHeight;
        ImageMatch cachedEntry = findBestMatch(cached, height, width);
        Image baseImage = cachedEntry == null ? null : cachedEntry.image;
        if (width <= 0 && height <= 0) {// shortcut. -1, -1 should simply return the biggest image available
            DebugMode.breakIf(cachedEntry != null && cachedEntry.sizeKey != MAX_SIZE_KEY);
            if (cachedEntry != null && cachedEntry.sizeKey.equals(MAX_SIZE_KEY + ENHANCE_DOUBLE_SIZE_TAG)) {
                return baseImage;
            }
            Image betterImage = loadImageFromDisc(key, ENHANCE_DOUBLE_SIZE_TAG, cached, width, height, context);
            if (betterImage != null) {
                putFinalCache(key, context, cached, finalKey, betterImage);
                return betterImage;
            }
            if (baseImage == null) {
                baseImage = loadImageFromDisc(key, "", cached, width, height, context);
                if (baseImage != null) {
                    putFinalCache(key, context, cached, finalKey, baseImage);
                }
            }
            if (baseImage != null) {
                debugDrawSizeInImages(context, width, height, baseImage);
                return baseImage;
            }
        } else {
            if (baseImage == null) {
                baseImage = loadImageFromDisc(key, "", cached, width, height, context);
            }
        }
        if (baseImage == null) {
            return onImageNotAvailable(key, context, width, height, cached, finalKey);
        }
        if (!IconIO.isImageCanGetDownscaled(width, height, baseImage)) {
            // upscale required check x2 option
            Image betterImage = loadImageFromDisc(key, ENHANCE_DOUBLE_SIZE_TAG, cached, width, height, context);
            if (betterImage != null) {
                baseImage = betterImage;
            }
        }
        Image baseImageHighDPIFinal = null;
        Image highDPIbase = null;
        double targetBaseFactor = Math.max((double) baseImage.getWidth(null) / width, (double) baseImage.getHeight(null) / height);
        double baseWidthFinal = Math.round(baseImage.getWidth(null) / targetBaseFactor);
        double baseHeightFinal = Math.round(baseImage.getHeight(null) / targetBaseFactor);
        if (returnMultiResImage) {
            double targetHighDPIWidth = (int) Math.round(baseWidthFinal * highestRequiredScale);
            double targetHighDPIHeight = (int) Math.round(baseHeightFinal * highestRequiredScale);
            cachedEntry = findBestMatch(cached, highDPIWidth, highDPIHeight);
            highDPIbase = cachedEntry == null ? null : cachedEntry.image;
            if (highDPIbase == null) {
                // May happen if the original image got removed from cache
                highDPIbase = loadImageFromDisc(key, "", cached, highDPIWidth, highDPIHeight, context);
            }
            if (cachedEntry == null || !cachedEntry.sizeKey.endsWith(ENHANCE_DOUBLE_SIZE_TAG)) {
                if (highDPIbase == null || !IconIO.isImageCanGetDownscaled(highDPIWidth, highDPIHeight, highDPIbase)) {
                    // upscale required check x2 option
                    Image betterImage = loadImageFromDisc(key, ENHANCE_DOUBLE_SIZE_TAG, cached, width, height, context);
                    if (betterImage != null) {
                        highDPIbase = betterImage;
                    }
                }
            }
            if (highDPIbase != null) {
                if (IconIO.isImageMaxSizeInViewport(highDPIWidth, highDPIHeight, highDPIbase)) {
                    baseImageHighDPIFinal = highDPIbase;
                } else {
                    // DO NOT UPSCALE!s
                    // if (isImageDimensionLargeEnoughForRequestedDimensions(highDPIWidth, highDPIHeight, highDPIbase)) {
                    if (context.isDoNotUpscaleButThrowException() && !IconIO.isImageCanGetDownscaled(highDPIWidth, highDPIHeight, highDPIbase)) {
                        throw new SourceImageTooSmallException(key, highDPIbase, highDPIWidth, highDPIHeight);
                    }
                    if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                        URL url = lookupImageUrl(key, context);
                        if (url != null && url.getPath().endsWith(".svg")) {
                            // we do not want to scale down immes of svg icons
                            DebugMode.debugger();
                        }
                    }
                    baseImageHighDPIFinal = scaleAndCache(key, context, cached, (int) Math.round(targetHighDPIWidth), (int) Math.round(targetHighDPIHeight), highDPIbase);
                    // }
                }
            }
        }
        if (highDPIbase == baseImage && baseImageHighDPIFinal != null) {
            // use already downscaled as basis instead of the large original
            baseImage = baseImageHighDPIFinal;
        }
        Image baseImageTargetFinal = null;
        if (IconIO.isImageMaxSizeInViewport(width, height, baseImage)) {
            baseImageTargetFinal = baseImage;
        } else {
            if (context.isDoNotUpscaleButThrowException() && !IconIO.isImageCanGetDownscaled(width, height, baseImage)) {
                throw new SourceImageTooSmallException(key, baseImage, width, height);
            }
            if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                URL url = lookupImageUrl(key, context);
                if (url != null && url.getPath().endsWith(".svg")) {
                    // we do not want to scale down immes of svg icons
                    DebugMode.debugger();
                }
            }
            baseImageTargetFinal = scaleAndCache(key, context, cached, (int) Math.round(baseWidthFinal), (int) Math.round(baseHeightFinal), baseImage);
        }
        if ((baseImageTargetFinal.getWidth(null) % 4 != 0) || (baseImageTargetFinal.getHeight(null) % 4 != 0)) {
            LogV3.warning("Image warning: width and height should be dividable by 4 for proper highDPI display!: " + key + ": " + baseImageTargetFinal.getWidth(null) + "x" + baseImageTargetFinal.getHeight(null));
        }
        if (returnMultiResImage && baseImageHighDPIFinal != null && baseImageTargetFinal != null && baseImageHighDPIFinal != baseImageTargetFinal) {
            if (baseImageHighDPIFinal.getWidth(null) != baseImageTargetFinal.getWidth(null) && baseImageHighDPIFinal.getHeight(null) != baseImageTargetFinal.getHeight(null)) {
                baseImageTargetFinal = MultiResolutionImageHelper.create(0, baseImageTargetFinal, baseImageHighDPIFinal);
            }
        }
        putFinalCache(key, context, cached, finalKey, baseImageTargetFinal);
        return baseImageTargetFinal;
    }

    private HashMap<String, AtomicInteger> debugLoadingMap = new HashMap<String, AtomicInteger>();

    /**
     * @param context
     *            TODO
     * @param width
     * @param height
     * @param cached
     * @param finalKey
     * @param key
     * @return
     */
    protected Image onImageNotAvailable(String relativePath, ThemeContext context, int width, int height, HashMap<String, MinTimeWeakReference<Image>> cached, String finalKey) {
        if (context.getTarget() != Target.IMAGE) {
            DebugMode.debugger();
            return null;
        }
        LogV3.info(NO_IMAGE_AVAILABLE + relativePath);
        if (doLogMissing) {
            org.appwork.loggingv3.LogV3.log(new Exception("Icon missing: " + this.buildPath("images/", relativePath, ".png|svg|ico", false)));
        }
        DebugMode.debugger();
        BufferedImage ret = createNotAvailableImage(width, height);
        putFinalCache(relativePath, context, cached, finalKey, ret);
        return ret;
    }

    protected void putFinalCache(String key, ThemeContext context, HashMap<String, MinTimeWeakReference<Image>> cached, String finalKey, Image image) {
        if (ICONDEBUGGER) {
            IconDebugger.show(image, "Image:" + key + " - " + finalKey + "- " + lookupImageUrl(key, context));
        }
        if (cached != null) {
            synchronized (cached) {
                if (cached != null) {
                    MinTimeWeakReference<Image> ref = new MinTimeWeakRefNamed<Image>(image, this.getCacheLifetime(), finalKey + getSizeKey(image.getWidth(null), image.getHeight(null)), this);
                    cached.put(finalKey, ref);
                }
            }
        }
    }

    protected void drawTextInViewport(int viewportWidth, int viewportHeight, String text, Graphics g) {
        {
            Graphics2D g2d = (Graphics2D) g.create();
            g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
            g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, Interpolation.BILINEAR.getHint());
            g2d.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
            g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            Font baseFont = new Font("SansSerif", Font.PLAIN, viewportWidth);
            g2d.setFont(baseFont);
            FontMetrics metrics = g2d.getFontMetrics();
            int textWidth = metrics.stringWidth(text);
            int textHeight = metrics.getAscent() + metrics.getDescent();
            double scaleX = (double) viewportWidth / textWidth;
            double scaleY = (double) viewportHeight / textHeight;
            double scale = Math.min(scaleX, scaleY);
            double scaledTextWidth = textWidth * scale;
            double scaledTextHeight = textHeight * scale;
            double offsetX = (viewportWidth - scaledTextWidth) / 2;
            double offsetY = (viewportHeight - scaledTextHeight) / 2;
            g2d.translate(offsetX, offsetY);
            g2d.scale(scale, scale);
            FontRenderContext frc = g2d.getFontRenderContext();
            GlyphVector gv = baseFont.createGlyphVector(frc, text);
            Shape textShape = gv.getOutline(0, metrics.getAscent());
            g2d.setStroke(new BasicStroke(3.0f / (float) scale, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
            Color color = g2d.getColor();
            g2d.setColor(Color.WHITE);
            g2d.draw(textShape);
            g2d.setColor(color);
            g2d.fill(textShape);
            g2d.dispose();
        }
    }

    /**
     * this method scales down the image, that it fits into the viewport targetWidth x targetHeight. This may result in images with
     * different aspect ratios
     *
     * @param key
     * @param cached
     * @param targetWidth
     * @param targetHeight
     * @param input
     * @param voidUpscaling
     * @return
     */
    protected Image scaleAndCache(String key, ThemeContext context, HashMap<String, MinTimeWeakReference<Image>> cached, int targetWidth, int targetHeight, Image input) {
        Image baseImageHighDPIFinal;
        double faktor = Math.max((double) input.getWidth(null) / targetWidth, (double) input.getHeight(null) / targetHeight);
        // do not keep aspect ratio. we need the icons exactly as calculated here
        baseImageHighDPIFinal = IconIO.getScaledInstance(input, targetWidth, targetHeight, faktor > 1 ? Interpolation.BICUBIC : Interpolation.BILINEAR, true, false);
        baseImageHighDPIFinal = debugDrawSizeInImages(context, targetWidth, targetHeight, baseImageHighDPIFinal);
        // if (baseImageHighDPIFinal.getWidth(null) * baseImageHighDPIFinal.getHeight(null) >
        // baseImageTargetFinal.getWidth(null) *
        // baseImageTargetFinal.getHeight(null)) {
        DebugMode.breakIf(!(baseImageHighDPIFinal.getWidth(null) == targetWidth || baseImageHighDPIFinal.getHeight(null) == targetHeight));
        DebugMode.breakIf(!((targetWidth <= 0 || baseImageHighDPIFinal.getWidth(null) <= targetWidth) && (targetHeight <= 0 || baseImageHighDPIFinal.getHeight(null) <= targetHeight)));
        debugWriteScaledImagesToDisk(key, targetWidth, targetHeight, input, baseImageHighDPIFinal);
        if (cached != null) {
            synchronized (cached) {
                String actualSizeKey = getSizeKey(baseImageHighDPIFinal.getWidth(null), baseImageHighDPIFinal.getHeight(null));
                MinTimeWeakReference<Image> ref = new MinTimeWeakRefNamed<Image>(baseImageHighDPIFinal, this.getCacheLifetime(), key + actualSizeKey, this);
                // we keep ratio. width or height might be different than the actual size;
                cached.put(getSizeKey(targetWidth, targetHeight), ref);
                // cached.put(getSizeKey(keepRatioWidth, keepRatioHeight), ref);
                cached.put(actualSizeKey, ref);
            }
        }
        return baseImageHighDPIFinal;
    }

    /**
     * @param key
     * @param targetWidth
     * @param targetHeight
     * @param input
     * @param baseImageHighDPIFinal
     */
    protected void debugWriteScaledImagesToDisk(String key, int targetWidth, int targetHeight, Image input, Image baseImageHighDPIFinal) {
        if (baseImageHighDPIFinal != input) {
            if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                File file = Application.getResource("tmp/scaledimage/" + key + "_" + getSizeKey(targetWidth, targetHeight) + ".png");
                file.delete();
                file.getParentFile().mkdirs();
                try {
                    ImageIO.write(IconIO.toBufferedImage(baseImageHighDPIFinal), "png", file);
                } catch (IOException e) {
                    LogV3.log(e);
                }
            }
        }
    }

    /**
     * @param context
     * @param targetWidth
     * @param targetHeight
     * @param baseImageHighDPIFinal
     * @return
     */
    protected Image debugDrawSizeInImages(ThemeContext context, int targetWidth, int targetHeight, Image src) {
        if (!context.isDebugDrawEnabled() || context.getTarget() != Target.IMAGE) {
            return src;
        }
        final int w = src.getWidth(null);
        final int h = src.getHeight(null);
        final BufferedImage image = IconIO.createEmptyImage(w, h, BufferedImage.TYPE_INT_ARGB, Transparency.TRANSLUCENT);
        final Graphics2D g = image.createGraphics();
        g.drawImage(src, 0, 0, null);
        g.setColor(Color.BLUE);
        // g.fillRect(0, 0, 5, 5);
        drawTextInViewport(src.getWidth(null), src.getHeight(null), targetWidth + "", g);
        g.dispose();
        return image;
    }

    private class ImageMatch {
        private final Image image;
        private String      sizeKey;

        private ImageMatch(Image img, String sizeKey) {
            this.image = img;
            this.sizeKey = sizeKey;
        }
    }

    /**
     * search the best match from cache to derive an image in the desired dimensions
     *
     * @param cached
     * @param height
     * @param width
     * @return
     */
    private ImageMatch findBestMatch(HashMap<String, MinTimeWeakReference<Image>> cached, int height, int width) {
        if (cached == null) {
            return null;
        }
        synchronized (cached) {
            if (cached.size() == 0) {
                return null;
            }
            String sizeKey = getSizeKey(width, height);
            MinTimeWeakReference<Image> ret = null;
            if (sizeKey == MAX_SIZE_KEY) {
                // check x2 first
                ret = cached.get(sizeKey + ENHANCE_DOUBLE_SIZE_TAG);
                if (ret != null) {
                    Image img = ret.get();
                    if (img != null) {
                        return new ImageMatch(img, sizeKey + ENHANCE_DOUBLE_SIZE_TAG);
                    }
                }
            }
            ret = cached.get(sizeKey);
            if (ret == null) {
                ret = cached.get(sizeKey + _VECTOR);
            }
            if (ret != null) {
                Image img = ret.get();
                if (img != null) {
                    return new ImageMatch(img, sizeKey);
                }
            }
            if (height <= 0 && width <= 0) {
                // original is not in cache any more - we won't find it.
                return null;
            }
            String bestKeyTarget = null;
            Image baseImageTarget = null;
            for (Entry<String, MinTimeWeakReference<Image>> es : cached.entrySet()) {
                // ORIGINAL_x2=null if there is no x2 image, and we already tried to read it from disk.
                if (es.getKey().startsWith(FINAL) || es.getValue() == null) {
                    continue;
                }
                MinTimeWeakReference<Image> ref = es.getValue();
                // do not update the timer
                Image img = ref.superget();
                if (img != null) {
                    if (IconIO.isImageCanGetDownscaled(width, height, img)) {
                        if (baseImageTarget == null || (img.getWidth(null) * img.getHeight(null)) < (baseImageTarget.getWidth(null) * baseImageTarget.getHeight(null))) {
                            // best match to downscale
                            if (es.getKey().endsWith(_VECTOR)) {
                                // the source has been a vector icon like svg. either return the exact match, or nothing - deriving new
                                // images from the cached vector icon will get better results.
                                if (IconIO.isImageMaxSizeInViewport(width, height, img)) {
                                    baseImageTarget = img;
                                    bestKeyTarget = es.getKey();
                                    // exact match
                                    ref.get();
                                    // the image has exact the requested dimensions.
                                    return new ImageMatch(img, bestKeyTarget);
                                }
                            } else {
                                baseImageTarget = img;
                                bestKeyTarget = es.getKey();
                                if (IconIO.isImageMaxSizeInViewport(width, height, img)) {
                                    // update the mintimeweek timer
                                    ref.get();
                                    // the image has exact the requested dimensions.
                                    return new ImageMatch(img, bestKeyTarget);
                                }
                            }
                        }
                    }
                }
            }
            if (baseImageTarget != null) {
                MinTimeWeakReference<Image> ref = cached.get(bestKeyTarget);
                if (ref != null) {
                    ref.get();
                }
                return new ImageMatch(baseImageTarget, bestKeyTarget);
            }
            // we did not find an image big enough. check if we have the original version - the biggest available
            {
                MinTimeWeakReference<Image> max = cached.get(MAX_SIZE_KEY + ENHANCE_DOUBLE_SIZE_TAG);
                if (max != null) {
                    baseImageTarget = max.get();
                    if (baseImageTarget != null) {
                        bestKeyTarget = MAX_SIZE_KEY + ENHANCE_DOUBLE_SIZE_TAG;
                        return new ImageMatch(baseImageTarget, bestKeyTarget);
                    }
                }
            }
            {
                MinTimeWeakReference<Image> max = cached.get(MAX_SIZE_KEY);
                if (max != null) {
                    baseImageTarget = max.get();
                    if (baseImageTarget != null) {
                        bestKeyTarget = MAX_SIZE_KEY;
                        return new ImageMatch(baseImageTarget, bestKeyTarget);
                    }
                }
            }
        }
        return null;
    }

    /**
     * maxWidth and
     *
     * @param key
     * @param cached
     * @param widthForVectorGraphics
     * @param heightForVectorGraphics
     * @param option
     * @return
     */
    protected Image loadImageFromDisc(String key, String enhanceTag, HashMap<String, MinTimeWeakReference<Image>> cached, int widthForVectorGraphics, int heightForVectorGraphics, ThemeContext context) {
        boolean cacheMaxSize = true;
        if (cached != null) {
            synchronized (cached) {
                if (cached.containsKey(MAX_SIZE_KEY + enhanceTag)) {
                    MinTimeWeakReference<Image> cachedEntry = cached.get(MAX_SIZE_KEY + enhanceTag);
                    if (cachedEntry == null) {
                        // image not available
                        return null;
                    }
                }
            }
        }
        // no base image. read from disk...
        final URL url = lookupImageUrl(key + enhanceTag, context);
        if (url == null) {
            if (cached != null) {
                synchronized (cached) {
                    // tell the cache that this image is not available
                    cached.put(MAX_SIZE_KEY + enhanceTag, null);
                }
            }
            return null;
        }
        Image baseImage = null;
        baseImage = FACTORY.urlToImage(url);
        boolean vectorIconSource = false;
        if (baseImage == null && context.getTarget() == Target.IMAGE) {
            // maybe a svg? fallback code if the requested image source is not a png or anything like this. do not execute this, if the
            // request came from the getIcon method - endless recursion!
            // DebugMode.debugger();
            Icon icon = getIcon(key + enhanceTag, widthForVectorGraphics, heightForVectorGraphics, context);
            if (icon != null) {
                vectorIconSource = true;
                LogV3.warning("Loaded an Vector graphic as Image. Use getIcon if possible! " + key + enhanceTag + " " + widthForVectorGraphics + ":" + heightForVectorGraphics);
                // there is no Max size for SVG...
                cacheMaxSize = false;
                baseImage = IconIO.toImage(icon);
            }
        } else {
            debugLogImageLoading(key + enhanceTag, url);
        }
        if (vectorIconSource) {
            enhanceTag += _VECTOR;
        }
        DebugMode.breakIf(baseImage == null && url != null);
        // FACTORY.urlToImage may return multiRes images e.g. if we read an ico, or icons from a file
        if (baseImage != null) {
            if (MultiResolutionImageHelper.isInstanceOf(baseImage)) {
                if (cached != null) {
                    synchronized (cached) {
                        Image largest = null;
                        // add each image in the cache . this way the cache can clean up unused variants
                        for (Image img : MultiResolutionImageHelper.getResolutionVariants(baseImage)) {
                            String actualSizeKey = getSizeKey(img.getWidth(null), img.getHeight(null));
                            MinTimeWeakReference<Image> ref = new MinTimeWeakRefNamed<Image>(img, this.getCacheLifetime(), key + actualSizeKey, this);
                            if (largest == null || img.getWidth(null) * img.getHeight(null) > largest.getWidth(null) * largest.getHeight(null)) {
                                largest = img;
                            }
                            cached.put(actualSizeKey + enhanceTag, ref);
                        }
                        if (cacheMaxSize && largest != null) {
                            String actualSizeKey = getSizeKey(largest.getWidth(null), largest.getHeight(null));
                            MinTimeWeakReference<Image> ref = new MinTimeWeakRefNamed<Image>(largest, this.getCacheLifetime(), key + actualSizeKey, this);
                            cached.put(MAX_SIZE_KEY + enhanceTag, ref);
                        }
                    }
                }
            } else {
                if (cached != null) {
                    synchronized (cached) {
                        String actualSizeKey = getSizeKey(baseImage.getWidth(null), baseImage.getHeight(null));
                        MinTimeWeakReference<Image> ref = new MinTimeWeakRefNamed<Image>(baseImage, this.getCacheLifetime(), key + actualSizeKey, this);
                        if (cacheMaxSize) {
                            cached.put(MAX_SIZE_KEY + enhanceTag, ref);
                        }
                        cached.put(actualSizeKey + enhanceTag, ref);
                    }
                }
            }
        }
        return baseImage;
    }

    protected void debugLogImageLoading(String key, final URL url) {
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            LogV3.warning("Read Icon from Disk: " + url);
            synchronized (debugLoadingMap) {
                String debugKey;
                AtomicInteger ex = debugLoadingMap.get(debugKey = "READ:" + key);
                if (ex != null) {
                    ex.incrementAndGet();
                    // DebugMode.debugger();
                    LogV3.warning("Read Icon from Disk AGAIN(" + ex + "): " + url);
                } else {
                    debugLoadingMap.put(debugKey, new AtomicInteger(1));
                }
            }
        }
    }

    /**
     * @return the useHighDPI
     */
    public boolean isUseHighDPI() {
        return useHighDPI;
    }

    /**
     * @param i
     * @param j
     * @return
     */
    private static String getSizeKey(int w, int h) {
        if (w <= 0) {
            w = -1;
        }
        if (h <= 0) {
            h = -1;
        }
        if (h < 0 && w < 0) {
            return MAX_SIZE_KEY;
        }
        return w + "x" + h;
    }

    protected final HashMap<String, HashMap<String, MinTimeWeakReference<Image>>> imageCache = new HashMap<String, HashMap<String, MinTimeWeakReference<Image>>>();

    public String getNameSpace() {
        return this.nameSpace;
    }

    /**
     * @return
     */
    public String getPath() {
        return this.path;
    }

    protected String buildPath(final String pre, final String path, final String ext, boolean fallback) {
        final Theme delegate = getDelegate();
        String ret = null;
        if (delegate != null) {
            ret = delegate.buildPath(pre, path, ext, fallback);
        }
        if (ret == null) {
            final StringBuilder sb = new StringBuilder();
            sb.append(fallback ? defaultPath : this.path);
            sb.append(pre);
            sb.append(path);
            sb.append(ext);
            ret = sb.toString();
        }
        return ret;
    }

    public String getText(final String string) {
        String ret = null;
        final Theme delegate = getDelegate();
        if (delegate != null) {
            ret = delegate.getText(string);
        }
        if (ret == null) {
            try {
                final URL url = this.getURL("", string, "");
                if (url != null) {
                    ret = IO.readURLToString(url);
                }
            } catch (final IOException e) {
                org.appwork.loggingv3.LogV3.log(e);
            }
        }
        return ret;
    }

    public String getTheme() {
        return this.theme;
    }

    /**
     * returns a valid resourceurl or null if no resource is available.
     *
     * @param pre
     *            subfolder. for exmaple "images/"
     * @param relativePath
     *            relative resourcepath
     * @param ext
     *            resource extension
     * @return
     */
    public URL getURL(final String pre, final String relativePath, final String ext) {
        URL ret = getURL(pre, relativePath, ext, false);
        if (ret == null) {
            ret = getURL(pre, relativePath, ext, true);
        }
        return ret;
    }

    private boolean isNonEmptyStream(URL url) {
        try {
            final InputStream is = url.openStream();
            try {
                return is.read() != -1;
            } finally {
                is.close();
            }
        } catch (IOException e) {
        }
        return false;
    }

    private URL getURL(final String pre, final String relativePath, final String ext, boolean useStandardIconPath) {
        URL url = null;
        final Theme delegate = getDelegate();
        if (delegate != null) {
            url = delegate.getURL(pre, relativePath, ext, useStandardIconPath);
        }
        if (url == null) {
            final String path = this.buildPath(pre, relativePath, ext, useStandardIconPath);
            try {
                File file = new File(path);
                if (!file.isAbsolute()) {
                    file = Application.getResource(path);
                }
                if (file.isFile() && file.length() > 0) {
                    url = file.toURI().toURL();
                }
            } catch (final MalformedURLException e) {
                e.printStackTrace();
            }
            if (url == null) {
                // afterwards, we lookup in classpath. jar or bin folders
                url = Theme.class.getResource((path.startsWith("/") ? "" : "/") + path);
                if (url != null && url.getPath().endsWith("/")) {
                    // the resource targets a folder not a file.
                    url = null;
                } else if (url != null && !isNonEmptyStream(url)) {
                    url = null;
                }
            }
        }
        return url;
    }

    public File getImagesDirectory() {
        File ret = null;
        final Theme delegate = getDelegate();
        if (delegate != null) {
            ret = delegate.getImagesDirectory();
        }
        if (ret == null) {
            ret = Application.getResource(buildPath("images/", "image", ".file", false)).getParentFile();
        }
        return ret;
    }

    public boolean hasIcon(final String key) {
        boolean hasIcon = false;
        final Theme delegate = getDelegate();
        if (delegate != null) {
            hasIcon = delegate.hasIcon(key);
        }
        if (!hasIcon) {
            hasIcon = lookupImageUrl(key, null) != null;
        }
        return hasIcon;
    }

    /**
     * Returns the file extension for a key. lower case. Might be null if no resource found or "" if there is no extension;
     *
     * @param string
     * @return
     */
    public String getIconType(String key) {
        URL url = getIconURL(key);
        if (url == null) {
            return null;
        }
        String path = url.getPath().toLowerCase(Locale.ROOT);
        int nameIndex = path.lastIndexOf("/");
        int index = path.lastIndexOf(".", nameIndex > 0 ? nameIndex + 1 : 0);
        if (index < 0) {
            return "";
        }
        return path.substring(index + 1);
    }

    public URL getIconURL(final String string) {
        URL ret = null;
        final Theme delegate = getDelegate();
        if (delegate != null) {
            ret = delegate.getIconURL(string);
        }
        if (ret == null) {
            ret = lookupImageUrl(string, null);
        }
        return ret;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.storage.config.MinTimeWeakReferenceCleanup# onMinTimeWeakReferenceCleanup
     * (org.appwork.storage.config.MinTimeWeakReference)
     */
    @Override
    public void onMinTimeWeakReferenceCleanup(final MinTimeWeakReference<?> minTimeWeakReference) {
        synchronized (this.imageIconCache) {
            this.imageIconCache.remove(minTimeWeakReference.getID());
        }
    }

    public void setCacheLifetime(final long cacheLifetime) {
        this.cacheLifetime = cacheLifetime;
    }

    public void setNameSpace(final String nameSpace) {
        if (!StringUtils.equals(getNameSpace(), nameSpace) && StringUtils.isNotEmpty(nameSpace)) {
            this.nameSpace = nameSpace;
            updatePath();
            this.clearCache();
        }
    }

    /**
     *
     */
    private void updatePath() {
        this.path = "themes/" + this.getTheme() + "/" + this.getNameSpace();
        this.defaultPath = "themes/" + "standard" + "/" + this.getNameSpace();
    }

    /**
     * @param theme
     */
    public void setTheme(final String theme) {
        if (!StringUtils.equals(getTheme(), theme) && StringUtils.isNotEmpty(theme)) {
            this.theme = theme;
            updatePath();
            this.clearCache();
        }
    }
}
