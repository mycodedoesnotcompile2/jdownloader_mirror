package org.jdownloader.images;

import java.awt.Component;
import java.awt.Graphics;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.swing.Icon;

import org.appwork.swing.components.IDIcon;
import org.appwork.swing.components.IconIdentifier;
import org.appwork.utils.images.AbstractIconPipe;
import org.appwork.utils.images.ModificationType;

public class IdentifierWrapperIcon extends AbstractIconPipe implements IDIcon {
    private final String key;

    public IdentifierWrapperIcon(Icon ret, String relativePath) {
        super(ret);
        key = relativePath;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y, List<Icon> parents) {
        paintDelegate(c, g, x, y, parents);
    }

    @Override
    public String toString() {
        return "IDIcon: " + key;
    }

    @Override
    public IconIdentifier getIdentifier() {
        return new IconIdentifier(null, key);
    }

    private static final Set<ModificationType> MODIFICATIONS = Collections.unmodifiableSet(new HashSet<ModificationType>(Arrays.asList(ModificationType.NONE)));

    /**
     * @see org.appwork.utils.images.IconPipe#getModifications()
     */
    @Override
    public Set<ModificationType> getModifications() {
        return MODIFICATIONS;
    }

}
