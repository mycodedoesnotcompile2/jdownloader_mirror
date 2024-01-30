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
package org.appwork.utils.swing;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.beans.PropertyChangeListener;

import javax.swing.Action;
import javax.swing.TransferHandler;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.text.JTextComponent;

/**
 * @author daniel
 * 
 */
public class HelpNotifier implements FocusListener, CaretListener {

    public static void register(final JTextComponent field, final HelpNotifierCallbackListener owner, final String helpText) {
        new HelpNotifier(field, helpText, owner);
    }

    private final JTextComponent               field;
    protected Color                            defaultColor = null;
    protected Color                            watchColor   = Color.GRAY;
    private String                             infoTxt      = null;
    private final HelpNotifierCallbackListener listener;

    private HelpNotifier(final JTextComponent field, final String helpTxt, final HelpNotifierCallbackListener listener) {
        this.field = field;
        this.listener = listener;
        this.field.setText(helpTxt);
        this.infoTxt = helpTxt;
        this.defaultColor = field.getForeground();
        this.focusLost(null);
        this.caretUpdate(null);
        this.field.addCaretListener(this);
        this.field.addFocusListener(this);
        this.field.getActionMap().put("paste", new Action() {

            @Override
            public void actionPerformed(final ActionEvent e) {
                if (field.getText().equals(HelpNotifier.this.infoTxt)) {
                    field.setText("");
                    field.setForeground(HelpNotifier.this.defaultColor);
                }
                TransferHandler.getPasteAction().actionPerformed(e);
            }

            @Override
            public void addPropertyChangeListener(final PropertyChangeListener listener) {
            }

            @Override
            public Object getValue(final String key) {                
                return null;
            }

            @Override
            public boolean isEnabled() {
                return TransferHandler.getPasteAction().isEnabled();
            }

            @Override
            public void putValue(final String key, final Object value) {
            }

            @Override
            public void removePropertyChangeListener(final PropertyChangeListener listener) { 
            }

            @Override
            public void setEnabled(final boolean b) {  
            }

        });
    }

    public void caretUpdate(final CaretEvent arg0) {
        if (this.field != null) {
            if (this.field.getDocument().getLength() == 0 || this.field.getText().equals(this.infoTxt)) {
                if (this.listener != null) {
                    this.listener.onHelpNotifyShown(this.field);
                }
            } else {
                if (this.listener != null) {
                    this.listener.onHelpNotifyHidden(this.field);
                    /*
                     * if user sets text with setText, we want default color
                     * again
                     */
                    this.field.setForeground(this.defaultColor);
                }
            }
        }
    }

    public void focusGained(final FocusEvent arg0) {
        if (this.field != null) {
            if (this.field.getText().equals(this.infoTxt)) {
                this.field.setText("");
                this.field.setForeground(this.defaultColor);
            }
        }

    }

    public void focusLost(final FocusEvent arg0) {
        if (this.field != null) {
            if (this.field.getDocument().getLength() == 0 || this.field.getText().equals(this.infoTxt)) {
                this.field.setText(this.infoTxt);
                this.field.setForeground(this.watchColor);
            }
        }
    }

}
