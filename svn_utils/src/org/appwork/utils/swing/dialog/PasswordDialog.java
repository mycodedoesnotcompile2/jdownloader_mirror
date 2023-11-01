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

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.text.JTextComponent;

import org.appwork.utils.BinaryLogic;
import org.appwork.utils.StringUtils;
import org.appwork.utils.locale._AWU;

import net.miginfocom.swing.MigLayout;

public class PasswordDialog extends AbstractDialog<String> implements KeyListener, MouseListener {
    protected String         message;
    protected JTextComponent messageArea;
    protected JTextComponent input1;
    protected JTextComponent input2;
    protected JTextComponent input3;
    protected JLabel         input1Label;
    protected JLabel         input2Label;
    private JLabel           input3Label;

    public PasswordDialog(final int flag, final String title, final String message, final Icon icon, final String okOption, final String cancelOption) {
        super(flag, title, icon, okOption, cancelOption);
        this.message = message;
    }

    @Override
    public JComponent layoutDialogContent() {
        final JPanel contentpane = new JPanel(new MigLayout("ins 0,wrap 2", "[fill,grow]"));
        messageArea = new JTextPane();
        messageArea.setBorder(null);
        messageArea.setBackground(null);
        messageArea.setOpaque(false);
        messageArea.setText(message);
        messageArea.setEditable(false);
        messageArea.putClientProperty("Synthetica.opaque", Boolean.FALSE);
        contentpane.add("span 2", messageArea);
        contentpane.add(input1Label = new JLabel(_AWU.T.PASSWORDDIALOG_PASSWORDCHANGE_OLDPASSWORD()), "hidemode 3");
        if (BinaryLogic.containsAll(flagMask, Dialog.STYLE_LARGE)) {
            input1 = new JPasswordField();
            input1.addKeyListener(this);
            input1.addMouseListener(this);
            contentpane.add(new JScrollPane(input1), "height 20:60:n,pushy,growy,w 250,hidemode 3");
        } else {
            input1 = new JPasswordField();
            input1.setBorder(BorderFactory.createEtchedBorder());
            input1.addKeyListener(this);
            input1.addMouseListener(this);
            contentpane.add(input1, "pushy,growy,w 250,hidemode 3");
        }
        contentpane.add(input2Label = new JLabel(_AWU.T.PASSWORDDIALOG_PASSWORDCHANGE_NEWPASSWORD()));
        if (BinaryLogic.containsAll(flagMask, Dialog.STYLE_LARGE)) {
            input2 = createInputLine2Component();
            input2.addKeyListener(this);
            input2.addMouseListener(this);
            contentpane.add(new JScrollPane(input2), "height 20:60:n,pushy,growy,w 250");
        } else {
            input2 = createInputLine2Component();
            input2.setBorder(BorderFactory.createEtchedBorder());
            input2.addKeyListener(this);
            input2.addMouseListener(this);
            contentpane.add(input2, "pushy,growy,w 250");
        }
        addInput3Line(contentpane);
        return contentpane;
    }

    protected void addInput3Line(final JPanel contentpane) {
        contentpane.add(input3Label = new JLabel(_AWU.T.PASSWORDDIALOG_PASSWORDCHANGE_NEWPASSWORD_REPEAT()));
        if (BinaryLogic.containsAll(flagMask, Dialog.STYLE_LARGE)) {
            input3 = new JPasswordField();
            input3.addKeyListener(this);
            input3.addMouseListener(this);
            contentpane.add(new JScrollPane(input3), "height 20:60:n,pushy,growy,w 250");
        } else {
            input3 = new JPasswordField();
            input3.setBorder(BorderFactory.createEtchedBorder());
            input3.addKeyListener(this);
            input3.addMouseListener(this);
            contentpane.add(input3, "pushy,growy,w 250");
        }
    }

    /**
     * @return
     */
    protected JTextComponent createInputLine2Component() {
        return new JPasswordField();
    }

    public String getNewPassword() {
        if (StringUtils.equals(new String(((JPasswordField) input2).getPassword()), new String(((JPasswordField) input3).getPassword()))) {
            return new String(((JPasswordField) input2).getPassword());
        } else {
            throw new IllegalStateException("Passwords do not match");
        }
    }

    @Override
    protected void initFocus(final JComponent focus) {
        input1.selectAll();
        input1.requestFocusInWindow();
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

    @Override
    protected String createReturnValue() {
        if ((getReturnmask() & (Dialog.RETURN_OK | Dialog.RETURN_TIMEOUT)) == 0) {
            return null;
        }
        try {
            return new String(((JPasswordField) input1).getPassword()) + ";" + new String(((JPasswordField) input2).getPassword()) + ";" + new String(((JPasswordField) input3).getPassword());
        } catch (ClassCastException e) {
            // if inputs are no passwordfields
            return null;
        }
    }
}
