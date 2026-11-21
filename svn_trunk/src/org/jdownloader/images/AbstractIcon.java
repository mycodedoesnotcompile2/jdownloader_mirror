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
     * NewTheme.getIcon will newer return null, always returns a dummy icon if icon with key is not found
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
