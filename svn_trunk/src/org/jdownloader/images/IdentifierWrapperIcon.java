package org.jdownloader.images;

import java.awt.Component;
import java.awt.Graphics;

import javax.swing.Icon;

import org.appwork.swing.components.IDIcon;
import org.appwork.swing.components.IconIdentifier;
import org.appwork.utils.images.AbstractIconPipe;

public class IdentifierWrapperIcon extends AbstractIconPipe implements IDIcon {
    private final String key;

    public IdentifierWrapperIcon(Icon ret, String relativePath) {
        super(ret);
        key = relativePath;
    }

    public void paintIcon(Component c, Graphics g, int x, int y, Icon parent) {
        paintDelegate(c, g, x, y);
    }

    @Override
    public String toString() {
        return "IDIcon: " + key;
    }

    @Override
    public IconIdentifier getIdentifier() {
        return new IconIdentifier(null, key);
    }
}
