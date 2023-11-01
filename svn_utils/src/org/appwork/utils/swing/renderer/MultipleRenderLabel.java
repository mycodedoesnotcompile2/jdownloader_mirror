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

import javax.swing.Icon;
import javax.swing.JPanel;

import net.miginfocom.swing.MigLayout;

/**
 * @author daniel
 * 
 */
public class MultipleRenderLabel extends JPanel {

    private static final long serialVersionUID = -378709535509849986L;
    private RenderLabel       left;
    private RenderLabel[]     rights           = null;
    private int               ICONCOUNT        = 0;

    public MultipleRenderLabel(final int size) {
        super(new MigLayout("ins 0", "[]0[fill,grow,align right]"));
        this.rights = new RenderLabel[size];
        this.ICONCOUNT = size;
        this.add(this.left = new RenderLabel());
        this.left.setOpaque(false);
        for (int i = 0; i < this.ICONCOUNT; i++) {
            this.add(this.rights[i] = new RenderLabel(), "dock east");
            this.rights[i].setOpaque(false);
        }
        this.setOpaque(false);
    }

    /**
     * Remember, that its always the same panel instance. so we have to reset to
     * defaults before each cellrenderer call.
     */
    public void clearIcons(final int counter) {
        for (int i = counter; i < this.ICONCOUNT; i++) {
            this.rights[i].setIcon(null);
            this.rights[i].setText(null);
            this.rights[i].setToolTipText(null);
        }
    }

    @Override
    public String getToolTipText() {
        final StringBuilder sb = new StringBuilder();
        if (this.left.getToolTipText() != null) {
            sb.append(this.left.getToolTipText());
        }
        for (int i = this.rights.length - 1; i >= 0; --i) {
            if (this.rights[i].getToolTipText() != null) {
                if (sb.length() > 0) {
                    sb.append(" | ");
                }
                sb.append(this.rights[i].getToolTipText());
            }
        }
        if (sb.length() > 0) { return sb.toString(); }
        return null;
    }

    @Override
    public void setEnabled(final boolean b) {
        if (this.left == null) { return; }
        if (b == false) {
            this.left.setDisabledIcon(org.appwork.resources.AWUTheme.I().getDisabledIcon(this.left.getIcon()));
        }
        this.left.setEnabled(b);
        for (int i = 0; i < this.ICONCOUNT; i++) {
            if (b == false) {
                this.rights[i].setDisabledIcon(org.appwork.resources.AWUTheme.I().getDisabledIcon(this.rights[i].getIcon()));
            }
            this.rights[i].setEnabled(b);
        }
    }

    @Override
    public void setForeground(final Color fg) {
        super.setForeground(fg);
        if (this.left == null) { return; }
        this.left.setForeground(fg);
        for (final RenderLabel right : this.rights) {
            right.setForeground(fg);
        }
    }

    public void setIcon(final int i, final Icon icon, final String text, final String tooltip) {
        if (i < 0 && this.ICONCOUNT > 0) {
            this.left.setIcon(icon);
            // left.setText(text);
            this.left.setToolTipText(tooltip);
        } else {
            if (i < 0 || i >= this.ICONCOUNT) { return; }
            this.rights[i].setIcon(icon);
            this.rights[i].setText(text);
            this.rights[i].setToolTipText(tooltip);
        }
    }

    /**
     * clears the icon for left, setIcon AFTER setText
     */
    public void setText(final String text, final Icon icon) {
        this.left.setIcon(icon);
        this.left.setText(text);
        this.left.setToolTipText(text);
    }

}