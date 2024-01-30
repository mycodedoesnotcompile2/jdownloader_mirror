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
package org.appwork.utils.swing.renderer;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;

import javax.swing.border.Border;

import org.appwork.swing.MigPanel;

import net.miginfocom.swing.MigLayout;

/**
 * @author thomas
 *
 */
public class RendererMigPanel extends MigPanel {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    protected Boolean         _enabled         = null;
    protected Boolean         _opaque          = null;

    /**
     * @param constraints
     * @param columns
     * @param rows
     */
    public RendererMigPanel(final String constraints, final String columns, final String rows) {
        super(constraints, columns, rows);
    }

    /**
     * @param migLayout
     */
    public RendererMigPanel(MigLayout migLayout) {
        super(migLayout);
    }

    @Override
    public boolean isEnabled() {
        if (_enabled == null) {
            return super.isEnabled();
        }
        return _enabled;
    }

    /**
     * Has to return false to avoid a drag&Drop cursor flicker bug <vr> http://bugs.sun.com/view_bug.do?bug_id=6700748
     */
    @Override
    public boolean isVisible() {
        return false;
    }
    // /**
    // * * Overridden for performance reasons.
    // */
    // @Override
    // public void firePropertyChange(final String propertyName, final boolean
    // oldValue, final boolean newValue) {
    // /* we dont need propertychange events */
    // }

    /**
     * * Overridden for performance reasons.
     */
    @Override
    public void repaint() {
    }

    @Override
    protected void paintComponent(final Graphics g) {
        final Color bg = getBackground();
        if (isOpaque() && bg != null) {
            // Synthstyles paint a different background if panel is disabled. We want the renderer to decide about the background
            g.setColor(bg);
            g.fillRect(0, 0, getWidth(), getHeight());
        } else {
            super.paintComponent(g);
        }
    }

    @Override
    public void paint(final Graphics g) {        
        super.paint(g);
    }

    /**
     * * Overridden for performance reasons.
     */
    @Override
    public void repaint(final long tm, final int x, final int y, final int width, final int height) {
    }

    /**
     * * Overridden for performance reasons.
     */
    @Override
    public void repaint(final Rectangle r) {
    }

    /**
     * * Overridden for performance reasons.
     */
    @Override
    public void revalidate() {
    }

    @Override
    public void setBackground(final Color bg) {
        if (bg != null && bg.equals(getBackground())) {
            return;
        }
        super.setBackground(bg);
        for (final Component c : getComponents()) {
            c.setBackground(bg);
        }
    }

    @Override
    public void setBorder(final Border border) {
        if (border == getBorder()) {
            return;
        }
        super.setBorder(border);
    }

    @Override
    public void setEnabled(final boolean enabled) {
        if (_enabled != null && _enabled == enabled) {
            return;
        }
        _enabled = enabled;
        for (final Component c : getComponents()) {
            c.setEnabled(enabled);
        }
    }

    @Override
    public void setForeground(final Color fg) {
        if (fg != null && fg.equals(getForeground())) {
            return;
        }
        super.setForeground(fg);
        for (final Component c : getComponents()) {
            c.setForeground(fg);
        }
    }

    @Override
    public void setOpaque(final boolean isOpaque) {
        if (_opaque != null && _opaque == isOpaque) {
            return;
        }
        _opaque = isOpaque;
        super.setOpaque(isOpaque);
    }
}
