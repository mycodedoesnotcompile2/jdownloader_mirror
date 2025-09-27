package org.jdownloader;

import java.awt.Component;
import java.awt.Graphics;
import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.Locale;

import javax.swing.Icon;

import org.appwork.swing.components.IDIcon;
import org.appwork.swing.components.IconIdentifier;
import org.appwork.utils.images.IconIO;
import org.jdownloader.gui.IconKey;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.images.NewTheme;

import jd.config.Property;
import jd.controlling.faviconcontroller.FavIconRequestor;
import jd.controlling.faviconcontroller.FavIcons;
import jd.http.Browser;
import jd.plugins.PluginForHost;
import jd.utils.JDUtilities;

public class DomainInfo implements FavIconRequestor, Comparable<DomainInfo>, Icon, IDIcon {
    // Inner interface
    public static interface DomainInfoFactory {
        public DomainInfo createDomainInfo(String tld, String domain);
    }

    // Static fields
    private static final HashMap<String, String>                    HARDCODEDFAVICONS = new HashMap<String, String>();
    private static final HashMap<String, WeakReference<DomainInfo>> CACHE             = new HashMap<String, WeakReference<DomainInfo>>();
    private static final int                                        HEIGHT            = 16;
    private static final int                                        WIDTH             = 16;
    // Static initializer
    static {
        HARDCODEDFAVICONS.put("http", IconKey.ICON_URL);// 'http links' results in 'http'
        HARDCODEDFAVICONS.put("ftp", IconKey.ICON_URL);
        HARDCODEDFAVICONS.put("filesystem", IconKey.ICON_HARDDISK);
        HARDCODEDFAVICONS.put("directhttp", IconKey.ICON_URL);
        HARDCODEDFAVICONS.put("f4m", IconKey.ICON_URL);
        HARDCODEDFAVICONS.put("m3u8", IconKey.ICON_URL);
        HARDCODEDFAVICONS.put("updaterequired", IconKey.ICON_URL);
        HARDCODEDFAVICONS.put("jdlog", IconKey.ICON_URL);
        HARDCODEDFAVICONS.put("cloudcache", IconKey.ICON_URL);
        HARDCODEDFAVICONS.put("usenet", IconKey.ICON_LOGO_NZB);
        HARDCODEDFAVICONS.put("genericusenet", IconKey.ICON_LOGO_NZB);
        HARDCODEDFAVICONS.put("captcha_type_image", IconKey.ICON_IMAGE);
    }
    // Instance fields
    private final String         domain;
    private final IconIdentifier iconIdentifier;
    private final String         tld;
    private String               name                       = null;
    protected Icon               hosterIcon                 = null;
    protected boolean            hosterIconUpdatePermission = false;

    // Constructor
    public DomainInfo(String tld, String domain) {
        this.tld = Property.dedupeString(tld);
        if (domain == null || domain.equals(tld)) {
            this.domain = tld;
        } else {
            this.domain = Property.dedupeString(domain);
        }
        this.iconIdentifier = new IconIdentifier("DomainInfo", domain);
    }

    // Static methods
    private static String getCacheID(final String domain) {
        String ret = domain.toLowerCase(Locale.ENGLISH);
        int index = ret.indexOf(" ");
        if (index > 0) {
            // for example recaptcha.com (google)
            ret = ret.substring(0, index);
        }
        index = ret.indexOf("/");
        if (index > 0) {
            // for examle recaptcha.com/bla
            ret = ret.substring(0, index);
        }
        return ret;
    }

    public static DomainInfo getInstance(final String domain) {
        return getInstance(domain, null);
    }

    public static DomainInfo getInstance(final String domain, final DomainInfoFactory builder) {
        if (domain == null) {
            return null;
        }
        final String lcaseTld = getCacheID(domain);
        synchronized (CACHE) {
            DomainInfo ret = null;
            final WeakReference<DomainInfo> domainInfo = CACHE.get(lcaseTld);
            if (domainInfo == null || (ret = domainInfo.get()) == null) {
                final String host = Browser.getHost(lcaseTld, true);
                if (builder != null) {
                    ret = builder.createDomainInfo(host, lcaseTld);
                }
                if (ret == null) {
                    ret = new DomainInfo(host, lcaseTld);
                }
                CACHE.put(lcaseTld, new WeakReference<DomainInfo>(ret));
            }
            return ret;
        }
    }

    // Public methods
    public int compareTo(DomainInfo o) {
        return getTld().compareTo(o.getTld());
    }

    @Deprecated
    public PluginForHost findPlugin() {
        return JDUtilities.getPluginForHost(getTld());
    }

    public String getDomain() {
        return domain;
    }

    public Icon getFavIcon() {
        return getFavIcon(true);
    }

    public Icon getFavIcon(final boolean updatePermission) {
        return getFavIcon(this, WIDTH, HEIGHT, updatePermission);
    }

    /**
     * WARNING: MAY RETURN null if size is too big returns a high quality icon for this domain. most domains do not support this and will
     * return null; the icon is NOT cached. use with care
     *
     * @param i
     * @return
     */
    public Icon getIcon(int size) {
        return getIcon(size, true);
    }

    public Icon getIcon(int size, final boolean updatePermission) {
        return getIcon(null, size, size, updatePermission);
    }

    @Override
    public int getIconHeight() {
        return HEIGHT;
    }

    @Override
    public int getIconWidth() {
        return WIDTH;
    }

    @Override
    public IconIdentifier getIdentifier() {
        return iconIdentifier;
    }

    public String getTld() {
        return tld;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        getFavIcon().paintIcon(c, g, x, y);
    }

    public Icon setFavIcon(Icon icon) {
        if (icon == null) {
            icon = getFavIcon(true);
        }
        setHosterIcon(icon, true);
        return icon;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getName() {
        return name != null ? name : domain;
    }

    public String toString() {
        return tld;
    }

    // Protected methods
    protected Icon getFavIcon(final FavIconRequestor requestor, int width, int height, final boolean updatePermission) {
        Icon ret = getHosterIcon();
        final boolean hasHosterIcon = ret != null;
        if (ret == null || (!hosterIconUpdatePermission && updatePermission)) {
            final String hardcodedFavIcon = HARDCODEDFAVICONS.get(domain);
            if (hardcodedFavIcon != null) {
                ret = NewTheme.I().getIcon(hardcodedFavIcon, -1);
            } else {
                ret = FavIcons.getFavIcon(domain, requestor, updatePermission);
                if (ret == null && !updatePermission) {
                    ret = FavIcons.getDefaultIcon(domain, false);
                }
            }
        }
        if (ret != null) {
            ret = IconIO.getScaledInstance(ret, width, height);
        }
        if (updatePermission || !hasHosterIcon) {
            setHosterIcon(ret, updatePermission);
        }
        return ret;
    }

    protected Icon getHosterIcon() {
        return hosterIcon;
    }

    protected Icon getIcon(final FavIconRequestor requestor, int width, int height, final boolean updatePermission) {
        Icon ret = null;
        final NewTheme theme = NewTheme.I();
        if (theme.hasIcon("fav/big." + domain)) {
            ret = new AbstractIcon("fav/big." + domain, -1);
        }
        if (ret == null && theme.hasIcon("fav/" + domain)) {
            ret = new AbstractIcon("fav/" + domain, -1);
        }
        if (ret != null && ret.getIconHeight() >= height && ret.getIconWidth() >= width) {
            return IconIO.getScaledInstance(ret, width, height);
        } else {
            return getFavIcon(requestor, width, height, updatePermission);
        }
    }

    protected void setHosterIcon(Icon icon, boolean updatePermission) {
        if (icon != null && icon != hosterIcon) {
            final int width;
            final int height;
            final Icon hosterIcon = this.hosterIcon;
            if (hosterIcon != null) {
                width = hosterIcon.getIconWidth();
                height = hosterIcon.getIconHeight();
            } else {
                width = WIDTH;
                height = HEIGHT;
            }
            icon = IconIO.getScaledInstance(icon, width, height);
        }
        this.hosterIcon = icon;
        this.hosterIconUpdatePermission = icon != null && (hosterIconUpdatePermission || updatePermission);
    }
}