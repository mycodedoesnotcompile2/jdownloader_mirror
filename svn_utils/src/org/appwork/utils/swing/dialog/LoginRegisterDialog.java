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

import java.awt.Cursor;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.SwingConstants;

import org.appwork.utils.BinaryLogic;
import org.appwork.utils.locale._AWU;

import net.miginfocom.swing.MigLayout;

public class LoginRegisterDialog extends AbstractDialog<String[]> implements KeyListener, MouseListener {

    public static final int   REGISTER         = 1 << 20;
    public static final int   FORCE_REGISTER   = 1 << 21;

    public static void main(final String[] args) {
        try {
            Dialog.getInstance().showDialog(new LoginRegisterDialog(Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN, "itle", "message", "defaultMessage", null, null, null));
        } catch (final DialogClosedException e) {            
            e.printStackTrace();
        } catch (final DialogCanceledException e) {            
            e.printStackTrace();
        }
    }

    private final String   defaultMessage;
    private final String   message;
    private JTextPane      messageArea;
    private JTextField     login;
    private JPasswordField password;
    private final boolean  remember;
    private JCheckBox      rem;
    private final boolean  register;
    private JLabel         registerBtn;
    private JPasswordField rpassword;
    private JLabel         rpasswordLabel;

    public LoginRegisterDialog(final int flag, final String title, final String message, final String defaultMessage, final ImageIcon icon, final String okOption, final String cancelOption) {
        super(flag & 0xffffffff & ~Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN, title, icon, okOption, cancelOption);
        // remove do not show again flag and convert to remember flag
        remember = BinaryLogic.containsAll(flag, Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN);
        register = BinaryLogic.containsAll(flag, LoginRegisterDialog.REGISTER);

        this.defaultMessage = defaultMessage;
        this.message = message;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.utils.swing.dialog.AbstractDialog#getRetValue()
     */
    @Override
    protected String[] createReturnValue() {
        return getLogins();
    }

    /**
     * Returns an array login,password,repeated password,remember.<br>
     * repeated password is null if we are in login mode <br>
     * remember is null of the remember checkbox is unchecked
     * 
     * @return
     */
    public String[] getLogins() {
        if ((getReturnmask() & (Dialog.RETURN_OK | Dialog.RETURN_TIMEOUT)) == 0) { return null; }

        return new String[] { login.getText(), new String(password.getPassword()), rpassword.isVisible() ? new String(rpassword.getPassword()) : null, rem.isSelected() ? "yes" : null };
    }

    /**
     * returns if the "remember flag has been set"
     * 
     * @return
     */
    public boolean isRemember() {
        if ((getReturnmask() & (Dialog.RETURN_OK | Dialog.RETURN_TIMEOUT)) == 0) { return false; }

        return rem.isSelected();

    }

    public void keyPressed(final KeyEvent e) {
        cancel();
    }

    public void keyReleased(final KeyEvent e) {
    }

    public void keyTyped(final KeyEvent e) {
    }

    @Override
    public JComponent layoutDialogContent() {
        final JPanel cp = new JPanel(new MigLayout("ins 0,wrap 2", "[][fill,grow]"));
        messageArea = new JTextPane();
        messageArea.setBorder(null);
        messageArea.setBackground(null);
        messageArea.setOpaque(false);
        messageArea.setText(message);
        messageArea.setEditable(false);
        messageArea.putClientProperty("Synthetica.opaque", Boolean.FALSE);

        cp.add(messageArea, "spanx");
        cp.add(new JLabel(_AWU.T.LOGINDIALOG_LABEL_USERNAME()), "alignx right");

        login = new JTextField();
        login.setBorder(BorderFactory.createEtchedBorder());
        login.setText(defaultMessage);
        login.addKeyListener(this);
        login.addMouseListener(this);
        cp.add(login, "pushy,growy");
        // password

        cp.add(new JLabel(_AWU.T.LOGINDIALOG_LABEL_PASSWORD()), "alignx right");

        password = new JPasswordField();
        password.setBorder(BorderFactory.createEtchedBorder());
        password.addKeyListener(this);
        password.addMouseListener(this);
        cp.add(password, "pushy,growy");

        // register

        cp.add(rpasswordLabel = new JLabel(_AWU.T.LOGINDIALOG_LABEL_PASSWORD_REPEAT()), "alignx right,hidemode 3");

        rpassword = new JPasswordField();
        rpassword.setBorder(BorderFactory.createEtchedBorder());
        rpassword.addKeyListener(this);
        rpassword.addMouseListener(this);
        cp.add(rpassword, "pushy,growy,hidemode 3");
        rpassword.setVisible(BinaryLogic.containsAll(flagMask, LoginRegisterDialog.FORCE_REGISTER));
        rpasswordLabel.setVisible(BinaryLogic.containsAll(flagMask, LoginRegisterDialog.FORCE_REGISTER));
        if (register) {
            registerBtn = new JLabel(_AWU.T.LOGINDIALOG_BUTTON_REGISTER());
            registerBtn.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, cp.getBackground().darker().darker()));
            registerBtn.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
            registerBtn.addMouseListener(this);
            cp.add(registerBtn, "skip,split 2");
        } else {
            cp.add(Box.createHorizontalGlue(), "skip,split 2");
        }

        if (remember) {
            rem = new JCheckBox(_AWU.T.LOGINDIALOG_CHECKBOX_REMEMBER());
            rem.setHorizontalTextPosition(SwingConstants.LEFT);
            rem.setHorizontalAlignment(SwingConstants.RIGHT);
            cp.add(rem, "pushx,growx,alignx right");

        }
        return cp;
    }

    public void mouseClicked(final MouseEvent e) {
        if (e.getSource() == registerBtn) {
            if (rpassword.isVisible()) {
                rpassword.setVisible(false);
                rpasswordLabel.setVisible(false);
                registerBtn.setText(_AWU.T.LOGINDIALOG_BUTTON_REGISTER());
            } else {
                rpassword.setVisible(true);
                rpasswordLabel.setVisible(true);
                registerBtn.setText(_AWU.T.LOGINDIALOG_BUTTON_LOGIN());
            }

            pack();
        } else {
            cancel();
        }
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
    protected void initFocus(final JComponent focus) {
        login.selectAll();
        
        login.requestFocusInWindow();
    }
  

}
