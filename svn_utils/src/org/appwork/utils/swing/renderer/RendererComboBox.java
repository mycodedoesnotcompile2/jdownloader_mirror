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

import javax.swing.DefaultListCellRenderer;
import javax.swing.JComboBox;

/**
 * @author thomas
 * 
 */
public class RendererComboBox extends JComboBox {
    /**
     * 
     */
    private static final long serialVersionUID = -9016239285766238659L;

    /**
     * A subclass of DefaultListCellRenderer that implements UIResource.
     * DefaultListCellRenderer doesn't implement UIResource directly so that
     * applications can safely override the cellRenderer property with
     * DefaultListCellRenderer subclasses.
     * <p>
     * <strong>Warning:</strong> Serialized objects of this class will not be
     * compatible with future Swing releases. The current serialization support
     * is appropriate for short term storage or RMI between applications running
     * the same version of Swing. As of 1.4, support for long term storage of
     * all JavaBeans<sup><font size="-2">TM</font></sup> has been added to the
     * <code>java.beans</code> package. Please see {@link java.beans.XMLEncoder}.
     */
    public static class UIResource extends DefaultListCellRenderer implements javax.swing.plaf.UIResource {

        /**
         * 
         */
        private static final long serialVersionUID = -3522129530054102140L;
    }

    public RendererComboBox() {
        super(new RendererComboBoxModel());
        // this.setEditable(true);
    }

    /**
     * Overridden for performance reasons. See the <a
     * href="#override">Implementation Note</a> for more information.
     */
    @Override
    public void firePropertyChange(final String propertyName, final boolean oldValue, final boolean newValue) {
    }

    /**
     * Overridden for performance reasons. See the <a
     * href="#override">Implementation Note</a> for more information.
     */
    @Override
    public void firePropertyChange(final String propertyName, final byte oldValue, final byte newValue) {
    }

    /**
     * Overridden for performance reasons. See the <a
     * href="#override">Implementation Note</a> for more information.
     */
    @Override
    public void firePropertyChange(final String propertyName, final char oldValue, final char newValue) {
    }

    /**
     * Overridden for performance reasons. See the <a
     * href="#override">Implementation Note</a> for more information.
     */
    @Override
    public void firePropertyChange(final String propertyName, final double oldValue, final double newValue) {
    }

    /**
     * Overridden for performance reasons. See the <a
     * href="#override">Implementation Note</a> for more information.
     */
    @Override
    public void firePropertyChange(final String propertyName, final float oldValue, final float newValue) {
    }

    /**
     * Overridden for performance reasons. See the <a
     * href="#override">Implementation Note</a> for more information.
     */
    @Override
    public void firePropertyChange(final String propertyName, final int oldValue, final int newValue) {
    }

    /**
     * Overridden for performance reasons. See the <a
     * href="#override">Implementation Note</a> for more information.
     */
    @Override
    public void firePropertyChange(final String propertyName, final long oldValue, final long newValue) {
    }

    /**
     * Overridden for performance reasons. See the <a
     * href="#override">Implementation Note</a> for more information.
     */
    @Override
    protected void firePropertyChange(final String propertyName, final Object oldValue, final Object newValue) {
        // // Strings get interned...
        if ("UI".equals(propertyName) || "model".equals(propertyName) || "text".equals(propertyName) || "font".equals(propertyName) || "foreground".equals(propertyName)) {

            super.firePropertyChange(propertyName, oldValue, newValue);
        } else {
            // System.out.println(propertyName);
            // super.firePropertyChange(propertyName, oldValue, newValue);
        }
    }

    /**
     * Overridden for performance reasons. See the <a
     * href="#override">Implementation Note</a> for more information.
     */
    @Override
    public void firePropertyChange(final String propertyName, final short oldValue, final short newValue) {
    }

    /**
     * Overridden for performance reasons. See the <a
     * href="#override">Implementation Note</a> for more information.
     * 
     * @since 1.5
     */
    @Override
    public void invalidate() {
        super.invalidate();
    }

    /**
     * Overridden for performance reasons. See the <a
     * href="#override">Implementation Note</a> for more information.
     * 
     * @since 1.5
     */
    @Override
    public void repaint() {
    }

    /**
     * Overridden for performance reasons. See the <a
     * href="#override">Implementation Note</a> for more information.
     */
    @Override
    public void repaint(final long tm, final int x, final int y, final int width, final int height) {
    }

    /**
     * Overridden for performance reasons. See the <a
     * href="#override">Implementation Note</a> for more information.
     */
    @Override
    public void repaint(final Rectangle r) {
    }

    /**
     * Overridden for performance reasons. See the <a
     * href="#override">Implementation Note</a> for more information.
     */
    @Override
    public void revalidate() {
        super.revalidate();
    }

    /**
     * Overridden for performance reasons. See the <a
     * href="#override">Implementation Note</a> for more information.
     */
    @Override
    public void validate() {

        super.validate();
    }
}
