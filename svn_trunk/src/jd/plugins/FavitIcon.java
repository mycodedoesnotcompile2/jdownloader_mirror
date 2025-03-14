package jd.plugins;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Component;
import java.awt.Composite;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.geom.Ellipse2D;

import javax.swing.Icon;

import jd.controlling.faviconcontroller.FavIconRequestor;
import jd.controlling.faviconcontroller.FavIcons;

import org.appwork.swing.components.IDIcon;
import org.appwork.swing.components.IconIdentifier;
import org.appwork.utils.images.IconIO;
import org.jdownloader.DomainInfo;
import org.jdownloader.api.RemoteAPIController;

public class FavitIcon implements Icon, FavIconRequestor, IDIcon {
    private int        width;
    private int        height;
    private int        size  = 10;
    private final Icon icon;
    private Icon       badge = null;
    private DomainInfo domainInfo;

    public FavitIcon(Icon icon, DomainInfo domainInfo) {
        width = icon.getIconWidth();
        this.size = (width * 10) / 18;
        height = icon.getIconHeight();
        this.domainInfo = domainInfo;
        this.badge = IconIO.getScaledInstance(FavIcons.getFavIcon(domainInfo.getTld(), this), size, size);
        this.icon = icon;
    }

    @Override
    public IconIdentifier getIdentifier() {
        final IconIdentifier ret = new IconIdentifier("Favit");
        ret.add(new IconIdentifier("tld", domainInfo.getDomain()));
        if (icon instanceof IDIcon) {
            ret.add(((IDIcon) icon).getIdentifier());
        } else {
            ret.add(new IconIdentifier(null, RemoteAPIController.getInstance().getContentAPI().getIconKey(icon)));
        }
        return ret;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        final Icon back = icon;
        // badge = new ImageIcon(ImageProvider.getScaledInstance((BufferedImage) icon.getImage(), size, size,
        // RenderingHints.VALUE_INTERPOLATION_BILINEAR, true));
        // back = domainInfo.getIcon(icon.getIconHeight());
        back.paintIcon(c, g, x - 0, y - 0);
        Graphics2D g2d = (Graphics2D) g;
        g.setColor(Color.WHITE);
        Composite comp = g2d.getComposite();
        g2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.75f));
        final Icon badge = this.badge;
        int xx = x + width - badge.getIconWidth();
        int yy = y + height - badge.getIconHeight();
        g2d.fill(new Ellipse2D.Float(xx, yy, badge.getIconWidth(), badge.getIconHeight()));
        // g.fillRect(xx, yy, size, size);
        badge.paintIcon(c, g, xx, yy);
        if (comp != null) {
            g2d.setComposite(comp);
        }
    }

    @Override
    public int getIconWidth() {
        return width;
    }

    @Override
    public int getIconHeight() {
        return height;
    }

    @Override
    public Icon setFavIcon(Icon icon) {
        if (icon != null) {
            badge = IconIO.getScaledInstance(icon, size, size);
        }
        return this;
    }
}
