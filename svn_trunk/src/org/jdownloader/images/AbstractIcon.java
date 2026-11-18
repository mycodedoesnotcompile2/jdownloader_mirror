package org.jdownloader.images;

import java.awt.AlphaComposite;
import java.awt.Component;
import java.awt.Composite;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.lang.ref.WeakReference;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import org.appwork.swing.components.IDIcon;
import org.appwork.swing.components.IconIdentifier;
import org.appwork.utils.LongUtils;
import org.appwork.utils.images.IconIO;

public class AbstractIcon implements Icon, IDIcon {
    private final String key;

    public String getKey() {
        return key;
    }

    public int getSize() {
        return size;
    }

    private final int           size;
    /* high=height, low=width */
    protected static final long UNKNOWN   = LongUtils.pack(-1, -1);
    private volatile long       dimension = UNKNOWN;
    private boolean             autoDisabledIconEnabled;
    private Composite           composite;

    public Composite getComposite() {
        return composite;
    }

    public void setComposite(Composite composite) {
        this.composite = composite;
    }

    public AbstractIcon(String key, int size) {
        this.key = key;
        this.size = size;
    }

    protected long lazyUpdate() {
        final long ret = dimension;
        if (ret != UNKNOWN) {
            return ret;
        }

        final Icon icon = loadIcon();
        if (icon == null) {
            return UNKNOWN;
        }
        return dimension = LongUtils.pack(icon.getIconHeight(), icon.getIconWidth());
    }

    /*
     * TODO: The cached icon is never invalidated. It stays reachable as long as Theme (or anyone else) holds the icon, so after a theme change
     * or a refreshed favicon file (see FavIcons, new AbstractIcon("fav/" + host, -1)) this instance may keep painting the old icon although
     * Theme already cleared its own cache. dimension is only updated in paintIcon and lazyUpdate, so getIconWidth/getIconHeight may also
     * return the old size until the next paint. Idea: invalidate on theme change, or drop this instance cache completely because Theme.getIcon
     * has its own cache and handles the invalidation.
     */
    private WeakReference<Icon> cachedIcon = null;

    protected Icon cacheIcon(Icon icon) {
        if (icon == null) {
            cachedIcon = null;
        } else {
            /*
             * Plain WeakReference on purpose: MinTimeWeakReference.get() resets a timer on a ScheduledExecutorService on every call, which is
             * far too expensive for paintIcon (called for every painted cell). Theme caches the icon itself, so it stays reachable.
             */
            cachedIcon = new WeakReference<Icon>(icon);
        }
        return icon;
    }

    protected Icon loadIcon() {
        final WeakReference<Icon> cachedIcon = this.cachedIcon;
        Icon ret = null;
        if (cachedIcon != null && ((ret = cachedIcon.get()) != null)) {
            return ret;
        }
        final int size = getSize();
        ret = NewTheme.I().getIcon(getKey(), size);
        if (ret != null) {
            return cacheIcon(ret);
        }
        /* size <= 0 means original size (e.g. -1), an empty image cannot have that size, use the same default as Theme (32) */
        final int fallbackSize = size > 0 ? size : 32;
        return new ImageIcon((IconIO.createEmptyImage(fallbackSize, fallbackSize)));
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null || !(obj instanceof AbstractIcon)) {
            return false;
        }
        return key.equals(((AbstractIcon) obj).key) && size == ((AbstractIcon) obj).size;
    }

    @Override
    public int hashCode() {
        return getKey().hashCode() + getSize();
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        final Icon icon = loadIcon();
        final long dimension = LongUtils.pack(icon.getIconHeight(), icon.getIconWidth());
        if (this.dimension != dimension) {
            this.dimension = dimension;
        }
        Composite old = null;
        if (composite != null) {
            old = ((Graphics2D) g).getComposite();
            ((Graphics2D) g).setComposite(composite);
        }
        try {
            if (c != null && !c.isEnabled() && isAutoDisabledIconEnabled()) {
                /* there is a cache in place for disabled icons with reference to this icon */
                org.jdownloader.images.NewTheme.I().getDisabledIcon(icon).paintIcon(c, g, x, y);
            } else {
                icon.paintIcon(c, g, x, y);
            }
        } finally {
            if (composite != null) {
                ((Graphics2D) g).setComposite(old);
            }
        }
    }

    @Override
    public int getIconWidth() {
        return LongUtils.getLowInt(lazyUpdate());
    }

    @Override
    public int getIconHeight() {
        return LongUtils.getHighInt(lazyUpdate());
    }

    @Override
    public IconIdentifier getIdentifier() {
        final long dimension = lazyUpdate();
        final IconIdentifier ret = new IconIdentifier(null, getKey());
        ret.addProperty("height", LongUtils.getHighInt(dimension));
        ret.addProperty("width", LongUtils.getLowInt(dimension));
        return ret;
    }

    public void setAutoDisabledIconEnabled(boolean b) {
        autoDisabledIconEnabled = b;
    }

    public boolean isAutoDisabledIconEnabled() {
        return autoDisabledIconEnabled;
    }

    public void setAlpha(float f) {
        setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, f));
    }
}
