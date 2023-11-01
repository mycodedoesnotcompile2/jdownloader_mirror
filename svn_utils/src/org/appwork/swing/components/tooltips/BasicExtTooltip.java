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
package org.appwork.swing.components.tooltips;

import java.awt.Color;
import java.awt.Graphics;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JComponent;
import javax.swing.JTextArea;
import javax.swing.event.AncestorEvent;

import org.appwork.swing.components.tooltips.config.ExtTooltipSettings;
import org.appwork.utils.swing.SwingUtils;

/**
 * @author thomas
 *
 */
public class BasicExtTooltip extends ExtTooltip implements PropertyChangeListener {

    /**
     *
     */
    private static final long serialVersionUID = 6869465876669412410L;
    private JTextArea         tf;
    private final JComponent  component;

    /**
     * @param circledProgressBar
     */
    public BasicExtTooltip(final JComponent circledProgressBar) {
        super();
        this.component = circledProgressBar;

        // this.tf.setText(this.component.getToolTipText());

        this.component.addPropertyChangeListener(JComponent.TOOL_TIP_TEXT_KEY, this);
        this.tf.setText(this.component.getToolTipText());
    }

    @Override
    public void ancestorRemoved(final AncestorEvent event) {

        this.component.removePropertyChangeListener(JComponent.TOOL_TIP_TEXT_KEY, this);
    }

    // @Override
    // public void dispose() {
    // this.component.removePropertyChangeListener(JComponent.TOOL_TIP_TEXT_KEY,
    // this);
    // super.dispose();
    //
    // }
    /*
     * (non-Javadoc)
     *
     * @see org.appwork.swing.components.tooltips.ExtTooltip#createContent()
     */
    @Override
    public TooltipPanel createContent() {
        final TooltipPanel p = new TooltipPanel("ins 2,wrap 1", "[]", "[]");
        this.tf = new JTextArea();
        // this.tf.setEnabled(false);
        ExtTooltipSettings cfg = this.getConfig();
        Color c = new Color(cfg.getForegroundColor());
        this.tf.setForeground(c);
        this.tf.setBackground(null);
        this.tf.setEditable(false);
        SwingUtils.setOpaque(this.tf, false);

        p.add(this.tf);

        return p;
    }

    @Override
    public void invalidate() {
        super.invalidate();
    }

    @Override
    public void paint(final Graphics g) {

        super.paint(g);

    }

    /*
     * (non-Javadoc)
     *
     * @see java.beans.PropertyChangeListener#propertyChange(java.beans. PropertyChangeEvent)
     */
    @Override
    public void propertyChange(final PropertyChangeEvent evt) {

        if (this.component.getToolTipText() == null || this.component.getToolTipText().length() == 0) {
            ToolTipController.getInstance().hideTooltip();
        } else {

            this.tf.setText(this.component.getToolTipText());

            System.out.println(this.component.getToolTipText());

            this.repaint();
        }

    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.swing.components.tooltips.ExtTooltip#toText()
     */
    @Override
    public String toText() {
        // TODO Auto-generated method stub
        return tf.getText();
    }
}
