package org.jdownloader.images;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.RenderingHints;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.UIManager;

import org.appwork.resources.AWUTheme;
import org.appwork.resources.HighDPIIcon;
import org.appwork.resources.Theme;
import org.appwork.swing.components.CheckBoxIcon;
import org.appwork.swing.components.ExtMergedIcon;
import org.appwork.swing.components.IDIcon;
import org.appwork.swing.components.IconIdentifier;
import org.appwork.utils.DebugMode;
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
    public static class RedDotIcon implements Icon, IDIcon {
        private final Color red;
        private final Icon  fChck;

        public RedDotIcon(Color red, Icon fChck) {
            this.red = red;
            this.fChck = fChck;
        }

        @Override
        public void paintIcon(Component c, Graphics g, int x, int y) {
            Graphics2D g2 = (Graphics2D) g.create();
            g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2.setColor(red);
            g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.3f));
            int width = IconIO.clipScale(fChck.getIconWidth(), 0.6d);
            g2.fillOval(x + (fChck.getIconWidth() - width) / 2, y + (fChck.getIconHeight() - width) / 2, width, width);
        }

        @Override
        public int getIconWidth() {
            return fChck.getIconWidth();
        }

        @Override
        public int getIconHeight() {
            return fChck.getIconHeight();
        }

        @Override
        public IconIdentifier getIdentifier() {
            return new IconIdentifier(getClass().getName());
        }
    }

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

    protected Icon modify(Icon ret, String relativePath) {
        if (ret instanceof ImageIcon) {
            return new HighDPIIcon(new IdentifierImageIcon(((ImageIcon) ret).getImage(), relativePath));
        }
        return new HighDPIIcon(new IdentifierWrapperIcon(ret, relativePath));
    };

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
    public Icon getIcon(String relativePath, int size) {
        if ("compress".equals(relativePath)) {
            return super.getIcon(IconKey.ICON_EXTRACT, size);
        } else {
            return super.getIcon(relativePath, size);
        }
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

    @Override
    public Image getImage(String key, int size, boolean useCache) {
        Image img = super.getImage(key, size, useCache);
        DebugMode.breakIf(img == null);
        return img;
    }

    public static void main(String[] args) {
        long t = System.currentTimeMillis();
        for (int i = 1000; i >= 0; i--) {
            IconIO.getImageIcon(NewTheme.class.getResource("/themes/flat/org/jdownloader/images/add.svg"), 32);
        }
        System.out.println(System.currentTimeMillis() - t);
        t = System.currentTimeMillis();
        for (int i = 1000; i >= 0; i--) {
            IconIO.getImageIcon(NewTheme.class.getResource("/themes/standard/org/jdownloader/images/add.png"), 32);
        }
        System.out.println(System.currentTimeMillis() - t);
    }

    public Icon getCheckBoxImage(String path, boolean selected, int size) {
        return getCheckBoxImage(path, selected, size, null);
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
            Icon back = getIcon(path, size, true);
            // y back = new IdentifierImageIcon(IconIO.getCroppedImage(IconIO.toBufferedImage(back)), path);
            Icon checkBox = selected ? new CheckBoxIcon((int) (1 * (0.5d * size)), selected, true) : new CheckBoxIcon((int) (1 * (0.5d * size)), selected, false);
            // checkBox = IconIO.getScaledInstance(checkBox, (int) (size * 0.5), (int) (size * 0.5));
            // checkBox = new BorderedIcon(checkBox, Color.RED, 1);
            // try {
            // Dialog.getInstance().showConfirmDialog(0, path + selected + size + red + "", "", checkBox, null, null);
            // } catch (DialogClosedException e) {
            // e.printStackTrace();
            // } catch (DialogCanceledException e) {
            // e.printStackTrace();
            // }
            if (red != null) {
                if (UIManager.getLookAndFeel().getClass().getSimpleName().equals("PlainLookAndFeel")) {
                    checkBox = IconIO.replaceColor(checkBox, new Color(!selected ? 0xFFF0F0F0 : 0xFFEBEBEB), 50, red, true);
                } else if (UIManager.getLookAndFeel().getClass().getSimpleName().equals("SyntheticaPlainLookAndFeel")) {
                    checkBox = IconIO.replaceColor(checkBox, new Color(!selected ? 0xFFF0F0F0 : 0xFFEBEBEB), 50, red, true);
                } else if (UIManager.getLookAndFeel().getClass().getSimpleName().equals("JDDefaultLookAndFeel")) {
                    checkBox = IconIO.replaceColor(checkBox, new Color(!selected ? 0xFFF0F0F0 : 0xFFEBEBEB), 50, red, true);
                } else {
                    final Icon fChck = checkBox;
                    checkBox = new ExtMergedIcon(checkBox).add(new RedDotIcon(red, fChck));
                }
            }
            ret = new ExtMergedIcon(back, 0, 0).add(checkBox, 0, back.getIconHeight() - checkBox.getIconHeight() + 2);
            // ret = new ExtMergedIcon(back, 0, 0);
            // try {
            // Dialog.getInstance().showConfirmDialog(0, key, key, ret, null, null);
            // } catch (DialogClosedException e) {
            // e.printStackTrace();
            // } catch (DialogCanceledException e) {
            // e.printStackTrace();
            // }
            // ret = new ImageIcon(ImageProvider.merge(back, checkBox, 3, 0, 0, back.getIconHeight() - checkBox.getIconHeight() + 2));
            cache(ret, key);
        }
        return ret;
    }
}
