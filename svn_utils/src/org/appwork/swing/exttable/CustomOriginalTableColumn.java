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

import javax.swing.table.TableColumn;

/**
 * @author thomas
 *
 */
public class CustomOriginalTableColumn extends TableColumn {
    private ExtColumn<?> extColumn;

    /**
     * @param ext
     * @param i
     */
    public CustomOriginalTableColumn(ExtColumn<?> ext, int i) {
        super(i);
        extColumn = ext;
    }

    /*
     * (non-Javadoc)
     *
     * @see javax.swing.table.TableColumn#getPreferredWidth()
     */
    @Override
    public int getPreferredWidth() {
        if (extColumn.getForcedWidth() > 0) {
            // if ("Column: Name".equals(this.toString())) {
            // System.out.println("Get PrefWidth forced" + extColumn.getForcedWidth());
            // }
            return extColumn.getForcedWidth();
        }
        // if ("Column: Name".equals(this.toString())) {
        // System.out.println("Get PrefWidth super" + super.getPreferredWidth());
        // }
        return super.getPreferredWidth();
    }

    /*
     * (non-Javadoc)
     *
     * @see javax.swing.table.TableColumn#getMaxWidth()
     */
    @Override
    public int getMaxWidth() {
        // if (getModelIndex() == 1) {
        // System.out.println("extColumn.getForcedWidth() > 0 " + (extColumn.getForcedWidth() > 0) + "- " + extColumn.getForcedWidth());
        // }
        if (extColumn.getForcedWidth() > 0) {
            // if ("Column: Name".equals(this.toString())) {
            // System.out.println("Get getMaxWidth forced" + extColumn.getForcedWidth());
            // }
            return extColumn.getForcedWidth();
        }
        // if ("Column: Name".equals(this.toString())) {
        // System.out.println("Get getMaxWidth super" + super.getMaxWidth());
        // }
        return super.getMaxWidth();
    }

    /*
     * (non-Javadoc)
     *
     * @see javax.swing.table.TableColumn#getMinWidth()
     */
    @Override
    public int getMinWidth() {
        // if (getModelIndex() == 1) {
        // System.out.println("extColumn.getForcedWidth() > 0 " + (extColumn.getForcedWidth() > 0) + "- " + extColumn.getForcedWidth());
        // }
        if (extColumn.getForcedWidth() > 0) {
            // if ("Column: Name".equals(this.toString())) {
            // System.out.println("Get getMinWidth forced" + extColumn.getForcedWidth());
            // }
            return extColumn.getForcedWidth();
        }
        // if ("Column: Name".equals(this.toString())) {
        // System.out.println("Get getMinWidth super" + super.getMinWidth());
        // }
        return super.getMinWidth();
    }

    /*
     * (non-Javadoc)
     *
     * @see javax.swing.table.TableColumn#setPreferredWidth(int)
     */
    @Override
    public void setPreferredWidth(int preferredWidth) {
        // int oldMax = getMaxWidth();
        // int oldMin = getMinWidth();
        // try {
        // setMaxWidth(Integer.MAX_VALUE);
        // setMinWidth(0);
        // if (getModelIndex() == 8) {
        // int now = getPreferredWidth();
        // System.out.println("pref " + getModelIndex() + " " + now + "->" + width);
        // }
        super.setPreferredWidth(preferredWidth);
        // if (getModelIndex() == 8) {
        // System.out.println("pref-->" + getPreferredWidth());
        // }
        // } finally {
        // setMaxWidth(oldMax);
        // setMinWidth(oldMin);
        // }
    }

    /*
     * (non-Javadoc)
     *
     * @see javax.swing.table.TableColumn#getWidth()
     */
    @Override
    public int getWidth() {
        // if ("Column: Name".equals(this.toString())) {
        // System.out.println("Get Width " + super.getWidth());
        // }
        return super.getWidth();
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return "Column: " + extColumn.getName();
    }

    /*
     * (non-Javadoc)
     *
     * @see javax.swing.table.TableColumn#setMinWidth(int)
     */
    @Override
    public void setMinWidth(int minWidth) {
        // if ("Modul".equals(extColumn.getName())) {
        // System.out.println("set max");
        // }
        super.setMinWidth(minWidth);
    }

    public void setWidth(int width) {
        // int oldMax = getMaxWidth();
        // int oldMin = getMinWidth();
        // try {
        // setMaxWidth(Integer.MAX_VALUE);
        // setMinWidth(0);
        // int now = getWidth();
        // if ("Column: Name".equals(this.toString()) && width == 10) {
        // System.out.println("w " + this + " " + now + "->" + width);
        // }
        // if ("Column: Name".equals(this.toString()) && width == 386) {
        // System.out.println("w " + this + " " + now + "->" + width);
        // }
        super.setWidth(width);
        if (width != getWidth() && extColumn.getModel().isColumnVisible(extColumn.getIndex())) {
            org.appwork.loggingv3.LogV3.severe("Bad Column Implementation: " + extColumn.getModel().getClass().getName() + "/" + extColumn.getName() + " Min: " + super.getMinWidth() + " Max: " + super.getMaxWidth() + " IS:" + width);
        }
        // if (getModelIndex() == 8) {
        // System.out.println("w-->" + getWidth());
        // }
        // } finally {
        // setMaxWidth(oldMax);
        // setMinWidth(oldMin);
        // }
    }
}
