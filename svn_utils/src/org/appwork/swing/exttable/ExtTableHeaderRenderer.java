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
package org.appwork.swing.exttable;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JTable;
import javax.swing.border.Border;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.JTableHeader;

import org.appwork.sunwrapper.sun.swing.DefaultLookupWrapper;

/**
 * @author thomas
 *
 */
public class ExtTableHeaderRenderer extends DefaultTableCellRenderer implements javax.swing.plaf.UIResource {
    /**
     *
     */
    private static final long  serialVersionUID = 1L;
    private final ExtColumn<?> column;
    private boolean            paintIcon;
    private Color              focusForeground;
    private Color              focusBackground;
    private Color              foregroundC;

    public Color getFocusForeground() {
        return focusForeground;
    }

    public void setFocusForeground(final Color focusForeground) {
        this.focusForeground = focusForeground;
    }

    public Color getForegroundC() {
        return foregroundC;
    }

    public void setForegroundC(final Color foregroundC) {
        this.foregroundC = foregroundC;
    }

    private Color  backgroundC;
    private Border focusBorder;

    public Color getFocusBackground() {
        return focusBackground;
    }

    public void setFocusBackground(final Color focusBackground) {
        this.focusBackground = focusBackground;
    }

    public Color getBackgroundC() {
        return backgroundC;
    }

    public void setBackgroundC(final Color backgroundC) {
        this.backgroundC = backgroundC;
    }

    private Border cellBorder;

    public Border getFocusBorder() {
        return focusBorder;
    }

    public void setFocusBorder(Border focusBorder) {
        this.focusBorder = focusBorder;
    }

    public Border getCellBorder() {
        return cellBorder;
    }

    public void setCellBorder(Border cellBorder) {
        this.cellBorder = cellBorder;
    }

    private final Icon lockedWidth;

    /**
     * @param extColumn
     * @param jTableHeader
     */
    public ExtTableHeaderRenderer(final ExtColumn<?> extColumn, final JTableHeader header) {
        column = extColumn;
        // this.setHorizontalTextPosition(10);
        lockedWidth = ExtTableIcon.TABLE_WIDTH_LOCKED.get(14);
        try {
            focusForeground = DefaultLookupWrapper.getColor(this, ui, "TableHeader.focusCellForeground");
            focusBackground = DefaultLookupWrapper.getColor(this, ui, "TableHeader.focusCellBackground");
        } catch (final Throwable e) {
            org.appwork.loggingv3.LogV3.log(e);
        }
        if (focusForeground == null) {
            focusForeground = header.getForeground();
        }
        if (focusBackground == null) {
            focusBackground = header.getBackground();
        }
        foregroundC = header.getForeground();
        backgroundC = header.getBackground();
        try {
            focusBorder = DefaultLookupWrapper.getBorder(this, ui, "TableHeader.focusCellBorder");
            cellBorder = DefaultLookupWrapper.getBorder(this, ui, "TableHeader.cellBorder");
        } catch (final Throwable e) {
            org.appwork.loggingv3.LogV3.log(e);
            // avoid that the block above kills edt
        }
        if (focusBorder == null) {
            focusBorder = BorderFactory.createEmptyBorder(0, 10, 0, 10);
        }
        if (cellBorder == null) {
            cellBorder = BorderFactory.createEmptyBorder(0, 10, 0, 10);
        }
        setFont(header.getFont());
    }

    @Override
    public Component getTableCellRendererComponent(final JTable table, final Object value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
        setForeground(hasFocus ? focusForeground : foregroundC);
        setBackground(hasFocus ? focusBackground : backgroundC);
        // sort column is no current column
        if (!this.column.isPaintSortIcon() || (this.column.getModel().getSortColumn() == null || this.column.getModel().getSortColumn() != this.column)) {
            paintIcon = false;
        } else {
            paintIcon = true;
        }
        setText(value == null ? "" : value.toString());
        configureBorder(value, hasFocus, row, column);
        // this.setBackground(Color.RED);
        // this.setOpaque(true);
        // System.out.println(this.getPreferredSize());
        return this;
    }

    protected void configureBorder(final Object value, final boolean hasFocus, final int row, final int column) {
        setBorder(hasFocus ? getFocusBorder() : getCellBorder());
    }

    @Override
    public void paintComponent(final Graphics g) {
        boolean paintLock = false;
        if (!column.isResizable() && column.isPaintWidthLockIcon() && column.getModel().getTable().isColumnLockingFeatureEnabled()) {
            paintLock = true;
        }
        final Border orgBorder = getBorder();
        final int widthDif = column.getWidth() - getPreferredSize().width;
        final boolean smallLockIcon = widthDif < lockedWidth.getIconWidth();
        final Icon icon = column.getModel().getSortColumn() == null ? null : column.getModel().getSortColumn().getSortIcon();
        try {
            if (paintLock && !smallLockIcon) {
                setBorder(BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(0, 0, 0, lockedWidth.getIconWidth()), orgBorder));
            }
            if (paintIcon && icon != null) {
                setBorder(BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(0, icon.getIconWidth(), 0, 0), getBorder()));
            }
            super.paintComponent(g);
        } finally {
            setBorder(orgBorder);
        }
        try {
            if (paintIcon) {
                final int left = 2;
                if (icon != null) {
                    final Graphics2D g2 = (Graphics2D) g;
                    // final Composite comp = g2.getComposite();
                    // g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER,
                    // 0.5f));
                    icon.paintIcon(this, g2, left, (getHeight() - icon.getIconHeight()) / 2);
                    // g2.setComposite(comp);
                }
            }
            if (paintLock) {
                // lockedWidth
                final Graphics2D g2 = (Graphics2D) g;
                // final Composite comp = g2.getComposite();
                // g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER,
                // 0.5f));
                if (smallLockIcon) {
                    g2.setColor(getBackground().darker());
                    // g2.setColor(Color.RED);
                    final int size = 6;
                    g2.fillPolygon(new int[] { getWidth(), getWidth() - size, getWidth(), getWidth() }, new int[] { getHeight(), getHeight(), getHeight() - size, getHeight() }, 4);
                } else {
                    lockedWidth.paintIcon(this, g2, getWidth() - lockedWidth.getIconWidth() - 2, (getHeight() - lockedWidth.getIconHeight()) / 2);
                }
                // g2.setComposite(comp);
            }
        } catch (final RuntimeException e) {
            org.appwork.loggingv3.LogV3.log(e);
        }
    }
}