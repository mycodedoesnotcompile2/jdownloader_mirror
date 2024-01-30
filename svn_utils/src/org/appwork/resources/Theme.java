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
package org.appwork.resources;

import java.awt.Image;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;

import javax.swing.Icon;
import javax.swing.JComponent;

import org.appwork.loggingv3.LogV3;
import org.appwork.storage.config.MinTimeWeakReference;
import org.appwork.storage.config.MinTimeWeakReferenceCleanup;
import org.appwork.swing.components.CheckBoxIcon;
import org.appwork.utils.Application;
import org.appwork.utils.IO;
import org.appwork.utils.StringUtils;
import org.appwork.utils.images.IconIO;

/**
 *
 * @author thomas
 *
 */
public class Theme implements MinTimeWeakReferenceCleanup {
    private String                                              path;
    // private final HashMap<String, MinTimeWeakReference<BufferedImage>>
    // imageCache = new HashMap<String, MinTimeWeakReference<BufferedImage>>();
    protected final HashMap<String, MinTimeWeakReference<Icon>> imageIconCache       = new HashMap<String, MinTimeWeakReference<Icon>>();
    private long                                                cacheLifetime        = 20000l;
    private String                                              theme;
    private String                                              nameSpace;
    private final boolean                                       doNotLogMissingIcons = "false".equals(System.getProperty("DO_NOT_LOG_MISSING_ICONS_EXCEPTION", "false"));

    public Theme(final String namespace) {
        this.setNameSpace(namespace);
        this.setTheme("standard");
    }

    public void cache(final Icon ret, final String key) {
        synchronized (this.imageIconCache) {
            this.imageIconCache.put(key, new MinTimeWeakReference<Icon>(ret, this.getCacheLifetime(), key, this));
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
    public Icon getIcon(final String relativePath, final int size, final boolean useCache) {
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
                    final URL url = lookupImageUrl(relativePath, size);
                    ret = FACTORY.urlToIcon(url, size, size);
                    ret = this.modify(ret, relativePath);
                    if (url == null && doNotLogMissingIcons) {
                        org.appwork.loggingv3.LogV3.log(new Exception("Icon missing: " + this.getPath("images/", relativePath, ".png", false)));
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
    protected URL lookupImageUrl(String relativePath, int size) {
        URL url = this.getURL("images/", relativePath + "_" + size, ".png", false);
        if (url == null) {
            url = this.getURL("images/", relativePath, ".png", false);
        }
        if (url == null) {
            url = this.getURL("images/", relativePath, ".svg", false);
            if (url != null && IconIO.getSvgFactory() == null) {
                url = null;
            }
        }
        if (url == null) {
            url = this.getURL("images/", relativePath + "_" + size, ".png", true);
        }
        if (url == null) {
            url = this.getURL("images/", relativePath, ".png", true);
        }
        if (url == null) {
            url = this.getURL("images/", relativePath, ".svg", true);
            if (url != null && IconIO.getSvgFactory() == null) {
                url = null;
            }
        }
        return url;
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
        return this.getImage(relativePath, size, false);
    }

    public Image getImage(final String key, final int size, final boolean useCache) {
        Image image = null;
        final Theme delegate = getDelegate();
        if (delegate != null) {
            image = delegate.getImage(key, size, useCache);
        }
        if (image == null) {
            image = FACTORY.toImage(this.getIcon(key, size, useCache));
        }
        return image;
    }

    public String getNameSpace() {
        return this.nameSpace;
    }

    /**
     * @return
     */
    public String getPath() {
        return this.path;
    }

    private String getPath(final String pre, final String path, final String ext, boolean fallback) {
        final StringBuilder sb = new StringBuilder();
        sb.append(fallback ? defaultPath : this.path);
        sb.append(pre);
        sb.append(path);
        sb.append(ext);
        return sb.toString();
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
            final String path = this.getPath(pre, relativePath, ext, fallback);
            try {
                // first lookup in home dir. .jd_home or installdirectory
                final File file = Application.getResource(path);
                if (file.exists()) {
                    url = file.toURI().toURL();
                }
            } catch (final MalformedURLException e) {
                e.printStackTrace();
            }
            if (url == null) {
                // afterwards, we lookup in classpath. jar or bin folders
                url = Theme.class.getResource(path);
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
            ret = Application.getResource(getPath("images/", "image", ".file", false)).getParentFile();
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
            hasIcon = lookupImageUrl(string, -1) != null;
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
            ret = lookupImageUrl(string, -1);
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
        this.path = "/themes/" + this.getTheme() + "/" + this.getNameSpace();
        this.defaultPath = "/themes/" + "standard" + "/" + this.getNameSpace();
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
