package org.jdownloader.gui.toolbar.action;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;

import javax.swing.Icon;

import org.appwork.swing.components.CheckBoxIcon;
import org.appwork.swing.components.ExtMergedIcon;
import org.appwork.utils.images.IconIO;

/**
 * Paints a small number badge into the bottom right corner of the wrapped icon. Used by toolbar toggle actions that can target several
 * items and want to show how many.
 */
public class CountBadgeIcon implements Icon {
    private static final int TOOLBAR_ICON_SIZE = 24;
    private final Icon   base;
    private final String text;

    public CountBadgeIcon(final Icon base, final int count) {
        this.base = base;
        this.text = count > 99 ? "99+" : String.valueOf(count);
    }

    /**
     * The toolbar sets the icons of toggle buttons explicitly instead of reading them from the action. This creates the same look as
     * NewTheme.getCheckBoxImage (icon with a checkbox in the bottom left corner), but with an arbitrary icon as base.
     */
    public static Icon createToolbarCheckBoxIcon(final Icon icon, final boolean selected) {
        /*
         * The checkbox position is derived from the icon height. Favicons and solver icons come in arbitrary sizes, so the base icon is
         * fitted into a fixed box first. This way the checkbox is always in the bottom left corner of the toolbar icon.
         */
        final Icon base = fit(icon, TOOLBAR_ICON_SIZE);
        final Icon checkBox = new CheckBoxIcon(12, selected, true);
        return new ExtMergedIcon(base, 0, 0).add(checkBox, 0, base.getIconHeight() - checkBox.getIconHeight() + 2);
    }

    /** Scales the icon down if it does not fit and centers it in a square box of the given size. */
    private static Icon fit(final Icon icon, final int size) {
        final Icon scaled = icon.getIconWidth() > size || icon.getIconHeight() > size ? IconIO.getScaledInstance(icon, size, size) : icon;
        if (scaled.getIconWidth() == size && scaled.getIconHeight() == size) {
            return scaled;
        }
        return new Icon() {
            @Override
            public int getIconWidth() {
                return size;
            }

            @Override
            public int getIconHeight() {
                return size;
            }

            @Override
            public void paintIcon(final Component c, final Graphics g, final int x, final int y) {
                scaled.paintIcon(c, g, x + (size - scaled.getIconWidth()) / 2, y + (size - scaled.getIconHeight()) / 2);
            }
        };
    }

    @Override
    public int getIconWidth() {
        return base.getIconWidth();
    }

    @Override
    public int getIconHeight() {
        return base.getIconHeight();
    }

    @Override
    public void paintIcon(final Component c, final Graphics g, final int x, final int y) {
        base.paintIcon(c, g, x, y);
        final Graphics2D g2 = (Graphics2D) g.create();
        try {
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
            final int height = Math.max(8, getIconHeight() / 2 + 1);
            g2.setFont(g2.getFont().deriveFont(Font.BOLD, height - 3f));
            final FontMetrics fm = g2.getFontMetrics();
            final int textWidth = fm.stringWidth(text);
            final int width = Math.max(height, textWidth + 4);
            final int bx = x + getIconWidth() - width;
            final int by = y + getIconHeight() - height;
            g2.setColor(new Color(0xD03030));
            g2.fillRoundRect(bx, by, width, height, height, height);
            g2.setColor(Color.WHITE);
            g2.drawString(text, bx + (width - textWidth) / 2, by + (height + fm.getAscent() - fm.getDescent()) / 2);
        } finally {
            g2.dispose();
        }
    }
}
