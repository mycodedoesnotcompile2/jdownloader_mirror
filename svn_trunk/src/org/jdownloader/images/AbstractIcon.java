package org.jdownloader.images;

import java.awt.AlphaComposite;
import java.awt.Component;
import java.awt.Composite;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.util.Objects;

import javax.swing.Icon;

import org.appwork.swing.components.IDIcon;
import org.appwork.swing.components.IconIdentifier;
import org.appwork.utils.LongUtils;
import org.appwork.utils.images.CachableIcon;

public class AbstractIcon implements Icon, IDIcon, CachableIcon {
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
        return dimension = LongUtils.pack(icon.getIconHeight(), icon.getIconWidth());
    }

    /*
     * TODO: This method may return null, which causes a NullPointerException in lazyUpdate() and paintIcon() (both call it without a null
     * check, and both are hot paths: layout and painting).
     *
     * When: NewTheme.I().getIcon(key, size) returns null if the Theme has no icon for the key and the ThemeContext does not allow a "not
     * available" fallback image (Theme.onIconNotAvailable returns null if !context.isCreateFileNotFoundFallbackImage() or the target is not
     * Target.ICON). With the default context Theme creates a fallback image itself, so this is rare, but not impossible.
     *
     * Previously a fallback existed here (empty image, size <= 0 mapped to 32 like Theme.createNotAvailableImage does, because
     * IconIO.createEmptyImage throws an IllegalArgumentException for sizes <= 0, e.g. new AbstractIcon(key, -1)), and lazyUpdate() returned
     * UNKNOWN for null. Both were removed.
     *
     * Note: null is also stored in cache in that case (the assignment in the last branch), so the lookup is repeated on every call, which is
     * the desired behavior (the icon may become available later).
     *
     * Decide whether null is a bug of the caller/theme (then document it and fail loudly) or restore a fallback icon.
     */
    public Icon loadIcon() {
        final Object cache = this.cache;
        if (cache == this) {
            return NewTheme.I().getIcon(getKey(), getSize());
        } else if (cache instanceof Icon) {
            return (Icon) cache;
        } else {
            return (Icon) (this.cache = NewTheme.I().getIcon(getKey(), getSize()));
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof AbstractIcon)) {
            return false;
        } else if (obj == this) {
            return true;
        }
        final AbstractIcon other = (AbstractIcon) obj;
        if (!key.equals(other.key)) {
            return false;
        } else if (size != other.size) {
            return false;
        } else if (autoDisabledIconEnabled != other.autoDisabledIconEnabled) {
            return false;
        } else if (!Objects.equals(composite, other.composite)) {
            return false;
        } else {
            return true;
        }
    }

    @Override
    public int hashCode() {
        return (getKey() + getSize()).hashCode();
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Icon icon = loadIcon();
        final long dimension = LongUtils.pack(icon.getIconHeight(), icon.getIconWidth());
        if (this.dimension != dimension) {
            this.dimension = dimension;
        }
        Composite old = null;
        final Composite composite = this.getComposite();
        if (composite != null) {
            old = ((Graphics2D) g).getComposite();
            ((Graphics2D) g).setComposite(composite);
        }
        try {
            if (c != null && !c.isEnabled() && isAutoDisabledIconEnabled()) {
                /* there is a cache in place for disabled icons with reference to this icon */
                icon = org.jdownloader.images.NewTheme.I().getDisabledIcon(icon);
            }
            icon.paintIcon(c, g, x, y);
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

    private Object cache = null;

    @Override
    public boolean isCachable() {
        return cache != this;
    }

    @Override
    public void setCachable(boolean b) {
        if (b) {
            cache = null;
        } else {
            cache = this;
        }
    }
}
