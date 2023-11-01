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

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Shape;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

import javax.swing.ImageIcon;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextPane;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import org.appwork.utils.swing.SwingUtils;

/**
 * @author thomas
 * 
 */
public class ExtTextPane extends JTextPane implements DocumentListener, FocusListener {
    /**
     * 
     */
    private static final long serialVersionUID = -8609711635557381385L;
    private Color       defaultColor;
    private Color       helpColor;

    {

        this.getDocument().addDocumentListener(this);
        this.addFocusListener(this);
        this.defaultColor = this.getForeground();
        this.helpColor = (Color) UIManager.get("TextField.disabledForeground");
        if (this.helpColor == null) {
            this.helpColor = Color.LIGHT_GRAY;
        }
    }
    private String      helpText = null;
    protected ImageIcon badgeIcon;

    /*
     * (non-Javadoc)
     * 
     * @see javax.swing.event.DocumentListener#changedUpdate(javax.swing.event.
     * DocumentEvent)
     */
    @Override
    public void changedUpdate(final DocumentEvent e) {
        this.onChanged();
    }

    public void focusGained(final FocusEvent arg0) {

        if (super.getText().equals(this.helpText)) {
            this.setText("");
            this.setForeground(this.defaultColor);
        }

    }

    public void focusLost(final FocusEvent arg0) {

        if (this.getDocument().getLength() == 0 || super.getText().equals(this.helpText)) {
            this.setText(this.helpText);
            this.setForeground(this.helpColor);
        }

    }

    public Color getHelpColor() {
        return this.helpColor;
    }

    public String getHelpText() {
        return this.helpText;
    }

    @Override
    public String getText() {
        String ret = super.getText();
        if (ret.equals(this.helpText)) {
            ret = "";
        }
        return ret;
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.swing.event.DocumentListener#insertUpdate(javax.swing.event.
     * DocumentEvent)
     */
    @Override
    public void insertUpdate(final DocumentEvent e) {
        this.onChanged();
    }

    public boolean isHelpTextVisible() {
        return this.helpText != null && this.helpText.equals(super.getText());
    }

    /**
     * 
     */
    public void onChanged() {
        // TODO Auto-generated method stub

    }

    public void paintBadge(final Graphics g) {
        if (this.badgeIcon != null) {
            if (this.getParent().getParent() instanceof JScrollPane) {
                final Point rec = SwingUtilities.convertPoint(this, new Point(0, 0), this.getParent().getParent());
                g.translate(-rec.x, -rec.y);
                g.drawImage(this.badgeIcon.getImage(), (int) (this.getParent().getParent().getWidth() - this.badgeIcon.getIconWidth() / 1.5), (int) (this.getParent().getParent().getHeight() - this.badgeIcon.getIconHeight() / 1.5), null);
                g.translate(rec.x, rec.y);
            } else {
                g.drawImage(this.badgeIcon.getImage(), (int) (this.getWidth() - this.badgeIcon.getIconWidth() / 1.5), (int) (this.getHeight() - this.badgeIcon.getIconHeight() / 1.5), null);

            }
        }
    }

    @Override
    public void paintComponent(final Graphics g) {
        super.paintComponent(g);
        final Shape clp = g.getClip();
        g.setClip(null);
        this.paintBadge(g);
        g.setClip(clp);

    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.swing.event.DocumentListener#removeUpdate(javax.swing.event.
     * DocumentEvent)
     */
    @Override
    public void removeUpdate(final DocumentEvent e) {
        this.onChanged();
    }

    /**
     * @param icon
     */
    public void setBadgeIcon(final ImageIcon icon) {
        this.badgeIcon = icon;
        this.repaint();

    }

    public void setHelpColor(final Color helpColor) {
        this.helpColor = helpColor;
    }

    /**
     * @param addLinksDialog_layoutDialogContent_input_help
     */
    public void setHelpText(final String helpText) {
        this.helpText = helpText;
        if (this.getText().length() == 0) {
            this.setText(this.helpText);
            this.setForeground(this.helpColor);
        }

    }

    /**
     * if label mode is enabled, the textfield will act like a MUltiline jlabel
     * 
     * @param b
     */
    public void setLabelMode(final boolean b) {
        this.setEditable(!b);
        this.setFocusable(!b);
        this.setBorder(b ? null : new JTextArea().getBorder());
        SwingUtils.setOpaque(this, !b);
    }

    @Override
    public void setText(String t) {
        if (!this.hasFocus() && this.helpText != null && (t == null || t.length() == 0)) {
            t = this.helpText;
        }

        super.setText(t);
        if (this.helpText != null) {
            if (this.helpText.equals(t)) {
                this.setForeground(this.helpColor);
            } else {

                this.setForeground(this.defaultColor);
            }
        }
    }
}
