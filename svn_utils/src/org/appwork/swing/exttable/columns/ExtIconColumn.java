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

import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.SwingConstants;

import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtDefaultRowSorter;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.utils.swing.renderer.RenderLabel;

/**
 * Single icon column
 */
public abstract class ExtIconColumn<E> extends ExtColumn<E> {

    private RenderLabel       renderer;

    private static final long serialVersionUID = -5751315870107507714L;

    public ExtIconColumn(final String name) {
        this(name, null);
    }

    public ExtIconColumn(final String name, final ExtTableModel<E> table) {
        super(name, table);

        this.renderer = new RenderLabel() {
            /**
             * 
             */
            private static final long serialVersionUID = 3886046725960279287L;

            @Override
            public boolean isVisible() {
                return false;
            }
        };

        this.initIcons();

        this.setRowSorter(new ExtDefaultRowSorter<E>() {

            /**
             * sorts the icon by hashcode
             */
            @Override
            public int compare(final E o1, final E o2) {
                final Icon ic1 = ExtIconColumn.this.getIcon(o1);
                final Icon ic2 = ExtIconColumn.this.getIcon(o2);
                final int h1 = ic1 == null ? 0 : ic1.hashCode();
                final int h2 = ic2 == null ? 0 : ic2.hashCode();
                if (h1 == h2) { return 0; }
                if (this.getSortOrderIdentifier() == ExtColumn.SORT_ASC) {
                    return h1 > h2 ? -1 : 1;
                } else {
                    return h2 > h1 ? -1 : 1;
                }
            }

        });
    }

    @Override
    public void configureEditorComponent(final E value, final boolean isSelected, final int row, final int column) {
        // TODO Auto-generated method stub

    }

    @Override
    public void configureRendererComponent(final E value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
        this.renderer.setIcon(this.getIcon(value));

    }

    @Override
    public Object getCellEditorValue() {
        return null;
    }

    @Override
    public int getDefaultWidth() {

        return 30;
    }

    /**
     * @return
     */
    @Override
    public JComponent getEditorComponent(final E value, final boolean isSelected, final int row, final int column) {
        return null;
    }

    /**
     * Returns the icon for o1;
     */
    protected abstract Icon getIcon(E value);

    /**
     * Sets max width to 30. overwrite to set other maxsizes
     */
    @Override
    public int getMaxWidth() {
        return this.getDefaultWidth();
    }

    @Override
    public int getMinWidth() {
        return this.getDefaultWidth();
    }

    /**
     * @return
     */
    @Override
    public JComponent getRendererComponent(final E value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
        return this.renderer;
    }

    @Override
    protected String getTooltipText(final E obj) {
        return null;
    }

    protected void initIcons() {
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
        // TODO Auto-generated method stub

    }

    @Override
    public void resetRenderer() {
        this.renderer.setOpaque(false);
        this.renderer.setHorizontalAlignment(SwingConstants.CENTER);
        this.renderer.setBorder(ExtColumn.DEFAULT_BORDER);
    }

    @Override
    public void setValue(final Object value, final E object) {
    }

}
