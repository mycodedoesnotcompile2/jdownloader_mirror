package org.jdownloader.images;

import java.awt.AlphaComposite;
import java.awt.Component;
import java.awt.Composite;
import java.awt.Graphics;
import java.awt.Graphics2D;

import javax.swing.Icon;

import org.appwork.swing.components.IDIcon;
import org.appwork.swing.components.IconIdentifier;
import org.appwork.utils.LongUtils;

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
        final Icon icon = NewTheme.I().getIcon(getKey(), getSize());
        return dimension = LongUtils.pack(icon.getIconHeight(), icon.getIconWidth());
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
        final Icon icon = NewTheme.I().getIcon(getKey(), getSize());
        dimension = LongUtils.pack(icon.getIconHeight(), icon.getIconWidth());
        Composite old = null;
        if (composite != null) {
            old = ((Graphics2D) g).getComposite();
            ((Graphics2D) g).setComposite(composite);
        }
        try {
            if (c != null && !c.isEnabled() && isAutoDisabledIconEnabled()) {
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
        final IconIdentifier ret = new IconIdentifier(null, getKey());
        ret.addProperty("height", getIconHeight());
        ret.addProperty("width", getIconWidth());
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
