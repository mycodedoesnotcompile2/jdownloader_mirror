/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2024, AppWork GmbH <e-mail@appwork.org>
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

import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicInteger;

import javax.imageio.ImageIO;
import javax.swing.Icon;
import javax.swing.JComponent;

import org.appwork.loggingv3.LogV3;
import org.appwork.storage.config.MinTimeWeakReference;
import org.appwork.storage.config.MinTimeWeakReferenceCleanup;
import org.appwork.swing.components.CheckBoxIcon;
import org.appwork.utils.Application;
import org.appwork.utils.DebugMode;
import org.appwork.utils.IO;
import org.appwork.utils.JavaVersion;
import org.appwork.utils.StringUtils;
import org.appwork.utils.images.IconIO;
import org.appwork.utils.images.Interpolation;

/**
 *
 * @author thomas
 *
 */
public class Theme implements MinTimeWeakReferenceCleanup {
    /**
     *
     */
    public static final String  FINAL                   = "final_";
    /**
     *
     */
    private static final String ENHANCE_DOUBLE_SIZE_TAG = "_x2";

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
    protected final HashMap<String, MinTimeWeakReference<Icon>> imageIconCache       = new HashMap<String, MinTimeWeakReference<Icon>>();
    private long                                                cacheLifetime        = 20000l;
    private String                                              theme;
    private String                                              nameSpace;
    private final boolean                                       doNotLogMissingIcons = "false".equals(System.getProperty("DO_NOT_LOG_MISSING_ICONS_EXCEPTION", "false"));
    private boolean                                             useHighDPI;
    private static final String                                 MAX_SIZE_KEY         = "ORIGINAL";

    public Theme(final String namespace) {
        this.setNameSpace(namespace);
        this.setTheme("standard");
        this.useHighDPI = false;
        if (JavaVersion.getVersion().isMinimum(JavaVersion.JVM_9_0)) {
            useHighDPI = true;
        }
    }

    public void cache(final Icon ret, final String key) {
        synchronized (this.imageIconCache) {
            this.imageIconCache.put(key, new MinTimeWeakRefNamed<Icon>(ret, this.getCacheLifetime(), key, this));
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
        if (objects.length == 1) {
            return objects[0].toString();
        }
        final StringBuilder sb = new StringBuilder();
        for (final Object o : objects) {
            if (sb.length() > 0) {
                sb.append("_");
            }
            sb.append(o.toString());
        }
        return sb.toString();
    }

    public long getCacheLifetime() {
        return this.cacheLifetime;
    }

    public Icon getDisabledIcon(final Icon _getIcon) {
        return getDisabledIcon(null, _getIcon);
    }

    protected Theme getDelegate() {
        return delegate;
    }

    public Icon getDisabledIcon(final JComponent component, final Icon _getIcon) {
        if (_getIcon != null) {
            Icon ret = null;
            final Theme delegate = getDelegate();
            if (delegate != null) {
                ret = delegate.getDisabledIcon(_getIcon);
            }
            if (ret == null) {
                final String key = this.getCacheKey(_getIcon, "disabled");
                ret = this.getCached(key);
                if (ret == null) {
                    final Icon ico = FACTORY.getDisabled(component, _getIcon);
                    ret = ico;
                    ret = this.modify(ret, key);
                    this.cache(ret, key);
                }
            }
            return ret;
        }
        return null;
    }

    public Icon getIcon(final String relativePath, final int size) {
        return this.getIcon(relativePath, size, true);
    }

    /**
     * @param relativePath
     * @param size
     * @param b
     * @return
     */
    public Icon getIcon(final String relativePath, final int size, boolean useCache) {
        Icon ret = null;
        final Theme delegate = getDelegate();
        if (delegate != null) {
            ret = delegate.getIcon(relativePath, size, useCache);
        }
        if (ret == null) {
            String key = null;
            if (useCache) {
                key = this.getCacheKey(relativePath, size);
                ret = this.getCached(key);
            }
            if (ret == null) {
                if (StringUtils.equalsIgnoreCase(relativePath, "disabled") || StringUtils.equalsIgnoreCase(relativePath, "checkbox_false")) {
                    ret = CheckBoxIcon.FALSE;
                    if (ret != null) {
                        // may be null during calss loading static init of the CheckBoxIconClass
                        ret = FACTORY.scale(ret, size, size);
                    }
                } else if (StringUtils.equalsIgnoreCase(relativePath, "enabled") || StringUtils.equalsIgnoreCase(relativePath, "checkbox_true")) {
                    ret = CheckBoxIcon.TRUE;
                    if (ret != null) {// may be null during calss loading static init of the CheckBoxIconClass
                        ret = FACTORY.scale(ret, size, size);
                    }
                } else if (StringUtils.equalsIgnoreCase(relativePath, "checkbox_undefined")) {
                    ret = CheckBoxIcon.UNDEFINED;
                    if (ret != null) {// may be null during calss loading static init of the CheckBoxIconClass
                        ret = FACTORY.scale(ret, size, size);
                    }
                }
                if (ret == null) {
                    final URL url = lookupImageUrl(relativePath, size, size);
                    ret = FACTORY.urlToNonImageIcon(url, size, size);
                    if (ret == null) {
                        // DebugMode.debugger();
                        Image image = getImage(relativePath, size, size, useCache, true);
                        if (image != null) {
                            // no reason to cache an image icon - we cached the image
                            useCache = false;
                            ret = FACTORY.imageToIcon(image, size, size);
                        }
                    }
                    ret = this.modify(ret, relativePath);
                    if (url == null && doNotLogMissingIcons) {
                        org.appwork.loggingv3.LogV3.log(new Exception("Icon missing: " + this.buildPath("images/", relativePath, ".png", false)));
                    }
                }
                if (useCache && ret != null) {
                    this.cache(ret, key);
                }
            }
        }
        return ret;
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
     * @param size
     * @return
     */
    protected URL lookupImageUrl(String relativePath, int width, int height) {
        for (boolean useFallback : new boolean[] { false, true }) {
            URL url = this.getURL("images/", relativePath, "", useFallback);
            if (url == null && width == height && width > 0) {
                // TODO: discuss if we remove this.
                url = this.getURL("images/", relativePath + "_" + width, ".png", useFallback);
            }
            if (url == null && IconIO.getSvgFactory() != null) {
                url = this.getURL("images/", relativePath, ".svg", useFallback);
            }
            if (url == null) {
                url = this.getURL("images/", relativePath, ".png", useFallback);
            }
            if (url == null && IconIO.isIcoSupported()) {
                url = this.getURL("images/", relativePath, ".ico", useFallback);
            }
            if (url != null) {
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

    /**
     * @param check
     * @param to
     * @throws IOException
     */
    protected void copy(File check, File to) throws IOException {
        to.getParentFile().mkdirs();
        IO.copyFile(check, to);
        copy(check, to, ".txt");
        copy(check, to, ".license");
        copy(check, to, ".info");
        copy(check, to, ".nfo");
    }

    /**
     * @param check
     * @param to
     * @param ext
     * @throws IOException
     */
    protected void copy(File check, File to, String ext) throws IOException {
        File nfo = new File(check.getAbsolutePath() + ext);
        File t = new File(to.getAbsolutePath() + ext);
        if (nfo.exists() && !t.exists()) {
            t.getParentFile().mkdirs();
            IO.copyFile(nfo, t);
        }
    }

    /**
     * @param ret
     * @param relativePath
     */
    protected Icon modify(Icon ret, String relativePath) {
        return ret;
    }

    public Image getImage(final String relativePath, final int size) {
        // test if using the cache causes issues in the ide. if not, we may set it to true as default some day
        return this.getImage(relativePath, size, Application.isJared(null) ? false : true);
    }

    public Image getImage(final String key, final int size, final boolean useCache) {
        Image image = null;
        if (image == null) {
            image = getImage(key, size, size, useCache, true);
        }
        return image;
    }

    /**
     * @param key
     * @param useCache
     * @param highDPISupport
     *            TODO
     * @param size
     * @param size2
     * @return
     */
    public Image getImage(String key, int width, int height, boolean useCache, boolean highDPISupport) {
        final Theme delegate = getDelegate();
        if (delegate != null) {
            Image image = delegate.getImage(key, width, height, useCache, highDPISupport);
            if (image != null) {
                return image;
            }
        }
        Image scaled = getRawImage(key, width, height, useCache, highDPISupport);
        return scaled;
    }

    private HashMap<String, AtomicInteger> debugLoadingMap = new HashMap<String, AtomicInteger>();
    private static final Image             NO_IMAGE        = IconIO.createEmptyImage(1, 1);

    /**
     * @param key
     * @param width
     * @param height
     * @param useCache
     * @param highDPISupport
     * @return
     */
    private Image getRawImage(String key, int width, int height, boolean useCache, boolean highDPISupport) {
        HashMap<String, MinTimeWeakReference<Image>> cached = null;
        double highestRequiredScale = getHighestMonitorScaling();
        int highDPIWidth = width;
        int highDPIHeight = height;
        if (highDPISupport && isUseHighDPI() && highestRequiredScale > 1) {
            // the internal highDPI logic searches for images that are bigger than the required size. the required size is calculated as
            // double, so java searches for an image with at least 23 pixel if 25.5 are required.
            // bei using ceil here, we ensure that we always have an image big enough and will not have to scale up anything.
            // this reduces issues with highDPI icons painted "between" physical pixels
            highDPIWidth = (int) Math.ceil(width * highestRequiredScale);
            highDPIHeight = (int) Math.ceil(height * highestRequiredScale);
        }
        boolean returnMultiResImage = width != highDPIWidth || height != highDPIHeight;
        String finalKey = FINAL + width + "_" + height + "_" + highDPISupport;
        if (useCache) {
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
        Image x2Image = NO_IMAGE;
        Image baseImage = findBestMatch(cached, height, width);
        if (width < 0 && height < 0) {// shortcut. -1, -1 should simply return the biggest image available
            Image betterImage = x2Image != NO_IMAGE ? x2Image : loadImageFromDisc(key, ENHANCE_DOUBLE_SIZE_TAG, cached, width, height);
            if (betterImage != null) {
                putFinalCache(cached, finalKey, betterImage);
                return betterImage;
            }
            if (baseImage == null) {
                baseImage = loadImageFromDisc(key, "", cached, width, height);
            }
            if (baseImage != null) {
                putFinalCache(cached, finalKey, baseImage);
                return baseImage;
            }
        } else {
            if (baseImage == null) {
                baseImage = loadImageFromDisc(key, "", cached, width, height);
            }
        }
        if (baseImage == null) {
            baseImage = loadImageFromDisc(key, "", cached, width, height);
        }
        if (baseImage == null) {
            DebugMode.debugger();
            BufferedImage ret = IconIO.createEmptyImage(width, height);
            putFinalCache(cached, finalKey, ret);
        }
        if (!isImageDimensionLargeEnoughForRequestedDimensions(width, height, baseImage)) {
            // upscale required check x2 option
            Image betterImage = x2Image != NO_IMAGE ? x2Image : loadImageFromDisc(key, ENHANCE_DOUBLE_SIZE_TAG, cached, width, height);
            if (betterImage != null) {
                baseImage = betterImage;
            }
        }
        Image baseImageHighDPIFinal = null;
        Image highDPIbase = null;
        double targetBaseFactor = Math.max((double) baseImage.getWidth(null) / width, (double) baseImage.getHeight(null) / height);
        double baseWidthFinal = Math.ceil(baseImage.getWidth(null) / targetBaseFactor);
        double baseHeightFinal = Math.ceil(baseImage.getHeight(null) / targetBaseFactor);
        if (returnMultiResImage) {
            double targetHighDPIWidth = (int) Math.round(baseWidthFinal * highestRequiredScale);
            double targetHighDPIHeight = (int) Math.round(baseHeightFinal * highestRequiredScale);
            highDPIbase = findBestMatch(cached, highDPIWidth, highDPIHeight);
            if (highDPIbase == null) {
                // May happen if the original image got removed from cache
                highDPIbase = loadImageFromDisc(key, "", cached, width, height);
            }
            if (highDPIbase == null || !isImageDimensionLargeEnoughForRequestedDimensions(highDPIWidth, highDPIHeight, highDPIbase)) {
                // upscale required check x2 option
                Image betterImage = x2Image != NO_IMAGE ? x2Image : loadImageFromDisc(key, ENHANCE_DOUBLE_SIZE_TAG, cached, width, height);
                if (betterImage != null) {
                    highDPIbase = betterImage;
                }
            }
            if (highDPIbase != null) {
                if (isImageDimensionExactRequestedDimensions(highDPIWidth, highDPIHeight, highDPIbase)) {
                    baseImageHighDPIFinal = highDPIbase;
                } else {
                    // DO NOT UPSCALE!
                    // if (isImageDimensionLargeEnoughForRequestedDimensions(highDPIWidth, highDPIHeight, highDPIbase)) {
                    baseImageHighDPIFinal = scaleAndCache(key, cached, (int) Math.round(targetHighDPIWidth), (int) Math.round(targetHighDPIHeight), highDPIbase);
                    // }
                }
            }
        }
        if (highDPIbase == baseImage && baseImageHighDPIFinal != null) {
            // use already downscaled as basis instead of the large original
            baseImage = baseImageHighDPIFinal;
        }
        Image baseImageTargetFinal = scaleAndCache(key, cached, (int) Math.round(baseWidthFinal), (int) Math.round(baseHeightFinal), baseImage);
        if ((baseImageTargetFinal.getWidth(null) % 4 != 0) || (baseImageTargetFinal.getHeight(null) % 4 != 0)) {
            LogV3.warning("Image warning: width and height shoul be dividable by 4 for proper highDPI display!: " + key + ": " + baseImageTargetFinal.getWidth(null) + "x" + baseImageTargetFinal.getHeight(null));
        }
        if (returnMultiResImage && baseImageHighDPIFinal != null && baseImageTargetFinal != null && baseImageHighDPIFinal != baseImageTargetFinal) {
            if (baseImageHighDPIFinal.getWidth(null) != baseImageTargetFinal.getWidth(null) && baseImageHighDPIFinal.getHeight(null) != baseImageTargetFinal.getHeight(null)) {
                baseImageTargetFinal = MultiResolutionImageHelper.create(0, baseImageTargetFinal, baseImageHighDPIFinal);
            }
        }
        putFinalCache(cached, finalKey, baseImageTargetFinal);
        return baseImageTargetFinal;
    }

    protected void putFinalCache(HashMap<String, MinTimeWeakReference<Image>> cached, String finalKey, Image image) {
        if (cached != null) {
            MinTimeWeakReference<Image> ref = new MinTimeWeakRefNamed<Image>(image, this.getCacheLifetime(), finalKey + getSizeKey(image.getWidth(null), image.getHeight(null)), this);
            cached.put(finalKey, ref);
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
    protected Image scaleAndCache(String key, HashMap<String, MinTimeWeakReference<Image>> cached, int targetWidth, int targetHeight, Image input) {
        Image baseImageHighDPIFinal;
        double faktor = Math.max((double) input.getWidth(null) / targetWidth, (double) input.getHeight(null) / targetHeight);
        // do not keep aspect ratio. we need the icons exactly as calculated here
        baseImageHighDPIFinal = IconIO.getScaledInstance(input, targetWidth, targetHeight, faktor > 1 ? Interpolation.BICUBIC : Interpolation.BILINEAR, true, false);
        // if (baseImageHighDPIFinal.getWidth(null) * baseImageHighDPIFinal.getHeight(null) >
        // baseImageTargetFinal.getWidth(null) *
        // baseImageTargetFinal.getHeight(null)) {
        DebugMode.breakIf(!(baseImageHighDPIFinal.getWidth(null) == targetWidth || baseImageHighDPIFinal.getHeight(null) == targetHeight));
        DebugMode.breakIf(!((targetWidth <= 0 || baseImageHighDPIFinal.getWidth(null) <= targetWidth) && (targetHeight <= 0 || baseImageHighDPIFinal.getHeight(null) <= targetHeight)));
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
            LogV3.info("Downscaled Image: " + key + "/" + input.getWidth(null) + ":" + input.getHeight(null) + " --> View:" + targetWidth + ":" + targetHeight + " --> Actual: " + baseImageHighDPIFinal.getWidth(null) + ":" + baseImageHighDPIFinal.getHeight(null));
        }
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

    protected double getHighestMonitorScaling() {
        double highestRequiredScale = 1d;
        // find the monitor with the highest scaling
        for (GraphicsDevice sd : GraphicsEnvironment.getLocalGraphicsEnvironment().getScreenDevices()) {
            AffineTransform tx = sd.getDefaultConfiguration().getDefaultTransform();
            highestRequiredScale = Math.max(highestRequiredScale, tx.getScaleX());
            highestRequiredScale = Math.max(highestRequiredScale, tx.getScaleY());
        }
        return highestRequiredScale;
    }

    protected boolean isImageDimensionLargeEnoughForRequestedDimensions(int width, int height, Image baseImage) {
        DebugMode.breakIf(baseImage == null);
        return (width <= 0 || baseImage.getWidth(null) >= width) && (height <= 0 || baseImage.getHeight(null) >= height);
    }

    protected boolean isImageDimensionExactRequestedDimensions(int width, int height, Image baseImage) {
        return (width <= 0 || baseImage.getWidth(null) == width) && (height <= 0 || baseImage.getHeight(null) == height);
    }

    /**
     * search the best match from cache to derive an image in the desired dimensions
     *
     * @param cached
     * @param height
     * @param width
     * @return
     */
    private Image findBestMatch(HashMap<String, MinTimeWeakReference<Image>> cached, int height, int width) {
        if (cached == null || cached.size() == 0) {
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
                    return img;
                }
            }
        }
        ret = cached.get(sizeKey);
        if (ret != null) {
            Image img = ret.get();
            if (img != null) {
                return img;
            }
        }
        if (height < 0 && width < 0) {
            // original is not in cache any more - we won't find it.
            return null;
        }
        String bestKeyTarget = null;
        Image baseImageTarget = null;
        try {
            for (Entry<String, MinTimeWeakReference<Image>> es : cached.entrySet()) {
                // ORIGINAL_x2=null if there is no x2 image, and we already tried to read it from disk.
                if (es.getKey().startsWith(FINAL) || es.getValue() == null) {
                    continue;
                }
                MinTimeWeakReference<Image> ref = es.getValue();
                if (ref != null) {
                    // do not update the timer
                    Image img = ref.superget();
                    if (img != null) {
                        if (isImageDimensionLargeEnoughForRequestedDimensions(width, height, img)) {
                            if (baseImageTarget == null || (img.getWidth(null) * img.getHeight(null)) < (baseImageTarget.getWidth(null) * baseImageTarget.getHeight(null))) {
                                // best match to downscale
                                baseImageTarget = img;
                                bestKeyTarget = es.getKey();
                                if ((width <= 0 || img.getWidth(null) == width) && (height <= 0 || img.getHeight(null) == height)) {
                                    // the image has exact the requested dimensions.
                                    return img;
                                }
                            }
                        }
                    }
                }
            }
            if (baseImageTarget != null) {
                return baseImageTarget;
            }
            // we did not find an image big enough. check if we have the original version - the biggest available
            {
                MinTimeWeakReference<Image> max = cached.get(MAX_SIZE_KEY + ENHANCE_DOUBLE_SIZE_TAG);
                if (max != null) {
                    baseImageTarget = max.get();
                    if (baseImageTarget != null) {
                        bestKeyTarget = MAX_SIZE_KEY + ENHANCE_DOUBLE_SIZE_TAG;
                        return baseImageTarget;
                    }
                }
            }
            {
                MinTimeWeakReference<Image> max = cached.get(MAX_SIZE_KEY);
                if (max != null) {
                    baseImageTarget = max.get();
                    if (baseImageTarget != null) {
                        bestKeyTarget = MAX_SIZE_KEY;
                        return baseImageTarget;
                    }
                }
            }
        } finally {
            if (bestKeyTarget != null) {
                // update the mintimeweek timer
                cached.get(bestKeyTarget);
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
     * @return
     */
    protected Image loadImageFromDisc(String key, String enhanceTag, HashMap<String, MinTimeWeakReference<Image>> cached, int widthForVectorGraphics, int heightForVectorGraphics) {
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
        final URL url = lookupImageUrl(key + enhanceTag, -1, -1);
        if (url == null) {
            if (cached != null) {
                synchronized (cached) {
                    // tell the cache that this image is not available
                    cached.put(MAX_SIZE_KEY + enhanceTag, null);
                }
            }
            return null;
        }
        debugLogImageLoading(key + enhanceTag, url);
        Image baseImage = FACTORY.urlToImage(url);
        if (baseImage == null) {
            // maybe a svg?
            // DebugMode.debugger();
            Icon icon = FACTORY.urlToNonImageIcon(url, widthForVectorGraphics, heightForVectorGraphics);
            if (icon != null) {
                // there is no Max size for SVG...
                cacheMaxSize = false;
                baseImage = IconIO.toImage(icon);
            }
        }
        if (cached != null && baseImage != null) {
            synchronized (cached) {
                String actualSizeKey = getSizeKey(baseImage.getWidth(null), baseImage.getHeight(null));
                MinTimeWeakReference<Image> ref = new MinTimeWeakRefNamed<Image>(baseImage, this.getCacheLifetime(), key + actualSizeKey, this);
                if (cacheMaxSize) {
                    cached.put(MAX_SIZE_KEY + enhanceTag, ref);
                }
                cached.put(actualSizeKey, ref);
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

    public Icon getScaledInstance(final Icon imageIcon, final int size) {
        Icon ret = null;
        final Theme delegate = getDelegate();
        if (delegate != null) {
            ret = delegate.getScaledInstance(imageIcon, size);
        }
        if (ret == null) {
            final String key = this.getCacheKey(imageIcon, size);
            ret = this.getCached(key);
            if (ret == null) {
                ret = FACTORY.scale(imageIcon, size, size);
                this.cache(ret, key);
            }
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

    private URL getURL(final String pre, final String relativePath, final String ext, boolean fallback) {
        URL url = null;
        final Theme delegate = getDelegate();
        if (delegate != null) {
            url = delegate.getURL(pre, relativePath, ext, fallback);
        }
        if (url == null) {
            final String path = this.buildPath(pre, relativePath, ext, fallback);
            try {
                File file = new File(path);
                if (!file.isAbsolute()) {
                    file = Application.getResource(path);
                }
                if (file.isFile()) {
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

    public boolean hasIcon(final String string) {
        boolean hasIcon = false;
        final Theme delegate = getDelegate();
        if (delegate != null) {
            hasIcon = delegate.hasIcon(string);
        }
        if (!hasIcon) {
            hasIcon = lookupImageUrl(string, -1, -1) != null;
        }
        return hasIcon;
    }

    public URL getIconURL(final String string) {
        URL ret = null;
        final Theme delegate = getDelegate();
        if (delegate != null) {
            ret = delegate.getIconURL(string);
        }
        if (ret == null) {
            ret = lookupImageUrl(string, -1, -1);
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
