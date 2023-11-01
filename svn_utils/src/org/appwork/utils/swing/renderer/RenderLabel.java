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

import java.awt.Rectangle;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JLabel;

import org.appwork.resources.AWUTheme;

/**
 * A Label for use in Renderers.
 *
 * @author $Author: unknown$
 *
 */
public class RenderLabel extends JLabel {

    /**
     *
     */
    private static final long serialVersionUID = 1204940612879959884L;
    private boolean           _enabled         = true;
    private boolean           _visible         = true;

    private Icon              customDisabledIcon;

    /**
     * * Overridden for performance reasons.
     */
    @Override
    public void firePropertyChange(final String propertyName, final boolean oldValue, final boolean newValue) {
        /* we dont need propertychange events */
    }

    /**
     * * Overridden for performance reasons.
     */
    @Override
    public void invalidate() {
    }

    @Override
    public boolean isEnabled() {
        return _enabled;
    }

    @Override
    public boolean isVisible() {
        return _visible;
    }

    /**
     * * Overridden for performance reasons.
     */
    @Override
    public void repaint() {
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

    /**
     * WIth this workaround, we avoid the disabled icon getting painted by the look and feel.
     */
    public Icon getDisabledIcon() {
        if (customDisabledIcon == null) {
            final Icon ico = getIcon();
            if (ico != null && ico instanceof ImageIcon) {
                AWUTheme.I().getDisabledIcon(ico);
            } else {
                return ico;
            }
        }
        return customDisabledIcon;
    }

    @Override
    public void setDisabledIcon(final Icon disabledIcon) {
        if (disabledIcon == customDisabledIcon) {
            return;
        }
        // * WIth this workaround, we avoid the disabled icon getting painted by
        // the look and feel.
        customDisabledIcon = disabledIcon;
        super.setDisabledIcon(disabledIcon);
    }

    /**
     * * Overridden for performance reasons.
     */
    @Override
    public void setDisplayedMnemonicIndex(final int index) {
        /* we dont need mnemonic in a table */
    }

    /**
     * for renderer reasons, there is a bug in java, that disabled icons to not get cached properly. thats why we override the method here
     * and extend it to use a cached disabled icon
     */
    @Override
    public void setEnabled(final boolean b) {
        if (b == isEnabled()) {
            return;
        }
        _enabled = b;
        if (!b && getIcon() != null) {
            setDisabledIcon(org.appwork.resources.AWUTheme.I().getDisabledIcon(getIcon()));
        }
    }

    /**
     * for renderer reasons, there is a bug in java, that disabled icons to not get cached properly. thats why we override the method here
     * and extend it to use a cached disabled icon
     */
    @Override
    public void setIcon(final Icon icon) {
        if (icon == getIcon()) {
            return;
        }
        if (!isEnabled()) {
            setDisabledIcon(org.appwork.resources.AWUTheme.I().getDisabledIcon(icon));
        }
        if (icon == null) {
            setDisabledIcon(null);
        }
        super.setIcon(icon);
    }

    @Override
    public void setText(final String text) {
        if (text == null && getText() == null) {
            return;
        }
        if (text != null && text.equals(getText())) {
            return;
        }
        super.setText(text);
    }

    @Override
    public void setVisible(final boolean aFlag) {
        _visible = aFlag;
    }

    @Override
    public void show(final boolean b) {
    }

    /**
     * * Overridden for performance reasons.
     */
    @Override
    public void validate() {
    }
}
