package org.appwork.swing.components;

import java.awt.Image;

import javax.swing.Icon;
import javax.swing.ImageIcon;

public class IDImageIcon extends ImageIcon implements Icon, IDIcon {

    private String id;

    public IDImageIcon(Image image) {
        this(image, null);
    }

    public IDImageIcon(Image image, String relativePath) {
        super(image);
        this.id = relativePath;
    }

    @Override
    public IconIdentifier getIdentifier() {
        return new IconIdentifier(null, id);
    }

}
