package org.appwork.swing.components;

import java.awt.Component;
import java.awt.Graphics;

import javax.swing.Icon;

public class IDWrapper implements Icon, IDIcon {

    private String id;
    private Icon   icon;

    public IDWrapper(Icon icon) {
        this(icon, null);
    }

    public IDWrapper(Icon icon, String relativePath) {
        this.icon = icon;
        this.id = relativePath;
    }

    @Override
    public IconIdentifier getIdentifier() {
        return new IconIdentifier(null, id);
    }

    /*
     * (non-Javadoc)
     *
     * @see javax.swing.Icon#paintIcon(java.awt.Component, java.awt.Graphics, int, int)
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        icon.paintIcon(c, g, x, y);

    }

    /*
     * (non-Javadoc)
     *
     * @see javax.swing.Icon#getIconWidth()
     */
    @Override
    public int getIconWidth() {
        return icon.getIconWidth();
    }

    /*
     * (non-Javadoc)
     *
     * @see javax.swing.Icon#getIconHeight()
     */
    @Override
    public int getIconHeight() {
        return icon.getIconHeight();
    }

}
