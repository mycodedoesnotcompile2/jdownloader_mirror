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
package org.appwork.utils.swing.input;

import java.awt.Color;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

public class JInputTextField extends javax.swing.JTextField implements DocumentListener, FocusListener {
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private final Color       defaultColor;
    private Color             infoColor        = Color.GRAY;
    private boolean           changed;
    private String            infoText;

    public JInputTextField() {
        super();
        defaultColor = getForeground();
        getDocument().addDocumentListener(this);
        addFocusListener(this);

    }

    /**
     * @param s
     */
    public JInputTextField(final String helpText) {
        this();
        infoText = helpText;
        focusLost(null);
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.swing.event.DocumentListener#changedUpdate(javax.swing.event.
     * DocumentEvent)
     */
    
    public void changedUpdate(final DocumentEvent e) {
        changed = true;

        setForeground(defaultColor);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.awt.event.FocusListener#focusGained(java.awt.event.FocusEvent)
     */
   
    public void focusGained(final FocusEvent e) {

        if (!changed) {
            setText("");
            setForeground(defaultColor);
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see java.awt.event.FocusListener#focusLost(java.awt.event.FocusEvent)
     */
  
    public void focusLost(final FocusEvent e) {
        if (getDocument().getLength() == 0 || getText().equals(getInfoText())) {
            setText(getInfoText());
            changed = false;
            setForeground(infoColor);
        }
    }

    public Color getInfoColor() {

        return infoColor;
    }

    /**
     * @return
     */
    protected String getInfoText() {
        // TODO Auto-generated method stub
        return infoText;
    }

    @Override
    public String getText() {
        if (!changed) { return ""; }
        return super.getText();
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.swing.event.DocumentListener#insertUpdate(javax.swing.event.
     * DocumentEvent)
     */
  
    public void insertUpdate(final DocumentEvent e) {
        changed = true;
        setForeground(defaultColor);
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.swing.event.DocumentListener#removeUpdate(javax.swing.event.
     * DocumentEvent)
     */
 
    public void removeUpdate(final DocumentEvent e) {
        changed = true;
        setForeground(defaultColor);
    }

    public void setInfoColor(final Color infoColor) {
        this.infoColor = infoColor;
    }

    public void setInfoText(final String infoText) {
        this.infoText = infoText;
    }
}
