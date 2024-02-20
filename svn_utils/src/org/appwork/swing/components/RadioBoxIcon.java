/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.swing.components;

import java.awt.Component;
import java.awt.Graphics;

import javax.swing.Icon;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import org.appwork.swing.synthetica.SyntheticaHelper;
import org.appwork.utils.ImageProvider.ImageProvider;

public final class RadioBoxIcon implements Icon {
    public static final RadioBoxIcon FALSE     = new RadioBoxIcon(false);
    public static final RadioBoxIcon TRUE      = new RadioBoxIcon(true);
    public static final RadioBoxIcon UNDEFINED = new RadioBoxIcon(true, false);
    private final JRadioButton       cb;
    private final JPanel             panel;
    private final Icon               internalIcon;
    private final int                size;

    public RadioBoxIcon(final boolean selected, final boolean enabled) {
        cb = new JRadioButton() {
            {
                setSelected(selected);
            }

            @Override
            public int getWidth() {
                return getPreferredSize().width;
            }

            @Override
            public int getHeight() {
                return getPreferredSize().height;
            }

            @Override
            public boolean isVisible() {
                return true;
            }
        };
        panel = new JPanel();
        panel.add(cb);// Substance laf, special handling SubstanceColorUtilities.getBackgroundFillColor, component.getParent
        // we need this workaround.
        // if we would use cb.paint(g); for every paintIcon call, this might habe sideeffects on the LAF painter.
        size = Math.max(cb.getPreferredSize().width, cb.getPreferredSize().height);
        if (!enabled) {
            internalIcon = org.appwork.resources.AWUTheme.I().getDisabledIcon(ImageProvider.toImageIcon(this));
        } else {
            internalIcon = ImageProvider.toImageIcon(this);
        }
    }

    public RadioBoxIcon(final boolean selected) {
        this(selected, true);
    }

    @Override
    public void paintIcon(final Component c, Graphics g, final int x, final int y) {
        if (internalIcon != null) {
            internalIcon.paintIcon(c, g, x, y);
        } else {
            g = g.create(x, y, size, size);
            if (SyntheticaHelper.isSynthetica()) {
                g.translate(-4, -4);
            }
            cb.paint(g);
            g.dispose();
        }
    }

    @Override
    public int getIconWidth() {
        return size;
    }

    @Override
    public int getIconHeight() {
        return size;
    }
}