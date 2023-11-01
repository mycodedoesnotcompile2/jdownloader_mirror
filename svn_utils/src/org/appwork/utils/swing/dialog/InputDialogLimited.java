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
package org.appwork.utils.swing.dialog;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.regex.Pattern;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JTextPane;
import javax.swing.text.JTextComponent;

import org.appwork.utils.swing.JTextFieldLimited;

import net.miginfocom.swing.MigLayout;

/**
 * @author daniel
 *
 */
public class InputDialogLimited extends AbstractDialog<String> implements KeyListener, MouseListener {
    private String         defaultMessage;
    private String         message;
    private JTextPane      messageArea;
    private JTextComponent input;
    private int            limit           = 0;
    private Pattern        validCharsRegex = null;

    public InputDialogLimited(final int flag, final String title, final String message, final String defaultMessage, final ImageIcon icon, final String okOption, final String cancelOption, final int limit, final Pattern validCharsRegex) {
        super(flag, title, icon, okOption, cancelOption);
        this.defaultMessage = defaultMessage;
        this.message = message;
        this.limit = limit;
        this.validCharsRegex = validCharsRegex;
    }

    @Override
    public JComponent layoutDialogContent() {
        final JPanel contentpane = createContentPane();
        messageArea = new JTextPane();
        messageArea.setBorder(null);
        messageArea.setBackground(null);
        messageArea.setOpaque(false);
        messageArea.setText(message);
        messageArea.setEditable(false);
        messageArea.putClientProperty("Synthetica.opaque", Boolean.FALSE);
        contentpane.add(messageArea);
        input = new JTextFieldLimited(limit, validCharsRegex);
        input.setBorder(BorderFactory.createEtchedBorder());
        input.setText(defaultMessage);
        input.addKeyListener(this);
        input.addMouseListener(this);
        contentpane.add(input, "pushy,growy,w 450");
        return contentpane;
    }

    protected JPanel createContentPane() {
        return new JPanel(new MigLayout("ins 0,wrap 1", "[fill,grow]"));
    }

    @Override
    protected void initFocus(final JComponent focus) {
        input.selectAll();
        input.requestFocusInWindow();
    }

    public String getReturnID() {
        if ((getReturnmask() & (Dialog.RETURN_OK | Dialog.RETURN_TIMEOUT)) == 0) {
            return null;
        }
        if (input == null || input.getText() == null) {
            return null;
        }
        return input.getText();
    }

    @Override
    protected String createReturnValue() {
        return getReturnID();
    }

    public void keyPressed(final KeyEvent e) {
        cancel();
    }

    public void keyReleased(final KeyEvent e) {
    }

    public void keyTyped(final KeyEvent e) {
    }

    public void mouseClicked(final MouseEvent e) {
        cancel();
    }

    public void mouseEntered(final MouseEvent e) {
    }

    public void mouseExited(final MouseEvent e) {
    }

    public void mousePressed(final MouseEvent e) {
    }

    public void mouseReleased(final MouseEvent e) {
    }
}
