package org.jdownloader.images;

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.util.HashSet;
import java.util.Set;

import javax.swing.Icon;
import javax.swing.JComponent;

import org.appwork.resources.AWUTheme;
import org.appwork.resources.Theme;
import org.appwork.swing.components.CheckBoxIcon;
import org.appwork.swing.components.ExtMergedIcon;
import org.appwork.swing.components.IDIcon;
import org.appwork.utils.images.ColoredIcon;
import org.appwork.utils.images.ColoredIcon.ColorLookup;
import org.appwork.utils.images.IconIO;
import org.jdownloader.gui.IconKey;
import org.jdownloader.updatev2.gui.LAFOptions;

/**
 * New JDownloader Icon Theme Support
 *
 * @author thomas
 *
 */
public class NewTheme extends Theme {
    private static final NewTheme INSTANCE = new NewTheme();

    /**
     * get the only existing instance of NewTheme.I(). This is a singleton
     *
     * @return
     */
    public static NewTheme getInstance() {
        return NewTheme.INSTANCE;
    }

    public static NewTheme I() {
        return NewTheme.INSTANCE;
    }

    /**
     * Create a new instance of NewTheme.I(). This is a singleton class. Access the only existing instance by using {@link #getInstance()}.
     */
    private NewTheme() {
        super("org/jdownloader/");
        AWUTheme.getInstance().setNameSpace(getNameSpace());
        AWUTheme.I().setDelegate(this);
        try {
            final LAFOptions inst = LAFOptions.getInstance();
            if (inst != null) {
                setTheme(inst.getCfg().getIconSetID());
            }
        } catch (Throwable e) {
            // LAFOPtions not initialized yet
        }
    }

    @Override
    public void setTheme(String theme) {
        super.setTheme(theme);
        AWUTheme.getInstance().setTheme(theme);
    }

    @Override
    public Icon getDisabledIcon(JComponent component, Icon input) {
        if (input instanceof IDIcon) {
            return new IdentifierWrapperIcon(super.getDisabledIcon(component, input), ((IDIcon) input).getIdentifier().getKey());
        }
        return super.getDisabledIcon(component, input);
    }

    @Override
    public Icon getIcon(String relativePath, int size) {
        if ("compress".equals(relativePath)) {
            relativePath = IconKey.ICON_EXTRACT;
        }
        Icon ret = super.getIcon(relativePath, size);
        if (ret != null) {
            ret = new IdentifierWrapperIcon(ret, relativePath);
        }
        return ret;
    }

    protected String getCacheKey(final Object... objects) {
        if (objects != null) {
            if (objects.length == 1 && objects[0] != null) {
                return objects[0].toString();
            }
            final StringBuilder sb = new StringBuilder();
            for (final Object o : objects) {
                if (o != null) {
                    if (sb.length() > 0) {
                        sb.append("_");
                    }
                    sb.append(o.toString());
                }
            }
            return sb.toString();
        }
        return null;
    }

    public Icon getCheckBoxImage(String path, boolean selected, int size) {
        return getCheckBoxImage(path, selected, size, null);
    }

    public static Set<Color> getInner25PercentColors(BufferedImage image) {
        Set<Color> colors = new HashSet<Color>();
        int width = image.getWidth();
        int height = image.getHeight();
        int startX = width / 4;
        int startY = height / 4;
        int innerWidth = width / 2;
        int innerHeight = height / 2;
        for (int y = startY; y < startY + innerHeight; y++) {
            for (int x = startX; x < startX + innerWidth; x++) {
                int rgb = image.getRGB(x, y);
                Color color = new Color(rgb, true);
                colors.add(color);
            }
        }
        return colors;
    }

    /**
     * Returns a Icon which contains a checkbox.
     *
     * @param path
     * @param selected
     * @param size
     * @param red
     * @return
     */
    public Icon getCheckBoxImage(String path, boolean selected, int size, Color red) {
        Icon ret = null;
        String key = this.getCacheKey(path + "/" + red, size, selected);
        ret = getCached(key);
        if (ret == null) {
            Icon back = getIcon(path, size);
            Icon checkBox = selected ? new CheckBoxIcon((int) (1 * (0.5d * size)), selected, true) : new CheckBoxIcon((int) (1 * (0.5d * size)), selected, true);
            if (red != null) {
                ColoredIcon colored = new ColoredIcon(checkBox);
                for (Color c : getInner25PercentColors(IconIO.toBufferedImage(CheckBoxIcon.FALSE))) {
                    colored.replace(new ColorLookup(c, 20, true), red);
                }
                checkBox = colored;
            }
            ret = new ExtMergedIcon(back, 0, 0).add(checkBox, 0, back.getIconHeight() - checkBox.getIconHeight() + 2);
            cache(ret, key);
        }
        return ret;
    }
}
