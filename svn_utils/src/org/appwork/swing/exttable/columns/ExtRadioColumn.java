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
package org.appwork.swing.exttable.columns;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JComponent;
import javax.swing.JRadioButton;
import javax.swing.SwingConstants;

import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtDefaultRowSorter;
import org.appwork.swing.exttable.ExtTableModel;

/**
 * @author daniel
 * 
 */
public abstract class ExtRadioColumn<E> extends ExtColumn<E> implements ActionListener {

    private static final long  serialVersionUID = -5391898292508477789L;

    private final JRadioButton renderer;
    private final JRadioButton editor;

    public ExtRadioColumn(final String name, final ExtTableModel<E> table) {
        super(name, table);

        this.renderer = new JRadioButton();

        this.editor = new JRadioButton();

        this.setRowSorter(new ExtDefaultRowSorter<E>() {
            @Override
            public int compare(final E o1, final E o2) {
                final boolean b1 = ExtRadioColumn.this.getBooleanValue(o1);
                final boolean b2 = ExtRadioColumn.this.getBooleanValue(o2);

                if (b1 == b2) { return 0; }
                if (this.getSortOrderIdentifier() == ExtColumn.SORT_ASC) {
                    return b1 && !b2 ? -1 : 1;
                } else {
                    return !b1 && b2 ? -1 : 1;
                }
            }

        });
    }

    public void actionPerformed(final ActionEvent e) {
        this.editor.removeActionListener(this);
        this.fireEditingStopped();
    }

    @Override
    public void configureEditorComponent(final E value, final boolean isSelected, final int row, final int column) {
        
        this.editor.removeActionListener(this);
        this.editor.setSelected(this.getBooleanValue(value));
        this.editor.addActionListener(this);

    }

    @Override
    public final void configureRendererComponent(final E value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
        this.renderer.setSelected(this.getBooleanValue(value));

    }

    protected abstract boolean getBooleanValue(E value);

    @Override
    public final Object getCellEditorValue() {
        return this.editor.isSelected();
    }

    @Override
    public int getClickcount() {
        return 1;
    }

    /**
     * @return
     */
    @Override
    public JComponent getEditorComponent(final E value, final boolean isSelected, final int row, final int column) {
        return this.editor;
    }

    @Override
    public int getMaxWidth() {
        return 70;
    }

    @Override
    public int getMinWidth() {
        return 30;
    }

    /**
     * @return
     */
    @Override
    public JComponent getRendererComponent(final E value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
        return this.renderer;
    }

    @Override
    public boolean isEditable(final E obj) {
        return false;
    }

    @Override
    public boolean isEnabled(final E obj) {
        return true;
    }

    @Override
    public boolean isSortable(final E obj) {
        return true;
    }

    @Override
    public void resetEditor() {
        this.editor.setHorizontalAlignment(SwingConstants.CENTER);
        this.editor.setOpaque(false);

    }

    @Override
    public void resetRenderer() {
        this.renderer.setHorizontalAlignment(SwingConstants.CENTER);
        this.renderer.setOpaque(false);

    }

    protected abstract void setBooleanValue(boolean value, E object);

    @Override
    public final void setValue(final Object value, final E object) {
        this.setBooleanValue((Boolean) value, object);
    }

}
