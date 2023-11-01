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
import java.util.ArrayList;

import javax.swing.Icon;
import javax.swing.JLabel;

import org.appwork.utils.swing.SwingUtils;

public class MultiLineLabelTooltip extends ExtTooltip {
    public static class LabelInfo {
        private String text;

        public String getText() {
            return text;
        }

        public void setText(final String text) {
            this.text = text;
        }

        public Icon getIcon() {
            return icon;
        }

        public void setIcon(final Icon icon) {
            this.icon = icon;
        }

        public int getConstrains() {
            return constrains;
        }

        public void setConstrains(final int constrains) {
            this.constrains = constrains;
        }

        private Icon icon;

        /**
         * @param text
         * @param icon
         * @param constrains
         */
        public LabelInfo(final String text, final Icon icon, final int constrains) {
            super();
            this.text = text;
            this.icon = icon;
            this.constrains = constrains;
        }

        /**
         * @param description
         * @param icon2
         */
        public LabelInfo(final String text, final Icon icon) {
            this(text, icon, JLabel.LEADING);
        }

        private int constrains = JLabel.LEADING;
    }

    public MultiLineLabelTooltip(final LabelInfo... labels) {
        panel = new TooltipPanel("ins 3,wrap 1", "[grow,fill]", "[grow,fill]");

        for (final LabelInfo link : labels) {

            JLabel lbl;
            panel.add(lbl = new JLabel(link.getText(), link.getIcon(), link.getConstrains()));
            SwingUtils.setOpaque(lbl, false);
            lbl.setForeground(new Color(getConfig().getForegroundColor()));

        }
        panel.setOpaque(false);
        add(panel);
    }

    /**
     * @param lbls
     */
    public MultiLineLabelTooltip(final ArrayList<LabelInfo> lbls) {
        this(lbls.toArray(new LabelInfo[] {}));
    }

    @Override
    public TooltipPanel createContent() {

        return null;
    }

    @Override
    public String toText() {
        return null;
    }

}
