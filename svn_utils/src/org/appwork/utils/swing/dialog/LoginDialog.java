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

import java.awt.Color;
import java.awt.event.ActionListener;

import javax.swing.Icon;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JTextField;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;

import org.appwork.utils.BinaryLogic;
import org.appwork.utils.locale._AWU;
import org.appwork.utils.swing.EDTHelper;
import org.appwork.utils.swing.dialog.LoginDialog.LoginData;

import net.miginfocom.swing.MigLayout;

/**
 * @author thomas
 *
 */
public class LoginDialog extends AbstractDialog<LoginData> implements ActionListener, CaretListener, LoginDialogInterface {
    public static class LoginData {
        private final String  username;
        private final String  password;
        private final boolean save;

        public LoginData(final String username, final String password, final boolean save) {
            super();
            this.username = username;
            this.password = password;
            this.save = save;
        }

        public String getPassword() {
            return password;
        }

        public String getUsername() {
            return username;
        }

        public boolean isSave() {
            return save;
        }
    }

    public static final int DISABLE_REMEMBER = 1 << 20;

    public static void main(final String[] args) {
        try {
            final LoginDialog d = new LoginDialog(0);
            final LoginData response = Dialog.getInstance().showDialog(d);
            System.out.println("Remember logins: " + response.isSave());
            System.out.println("Username: " + response.getUsername());
            System.out.println("Password: " + response.getPassword());
        } catch (final DialogClosedException e) {
            e.printStackTrace();
        } catch (final DialogCanceledException e) {
            e.printStackTrace();
        }
    }

    protected JTextField   accid;
    private JPasswordField pass;
    private Color          titleColor;
    private String         preUser;
    private String         prePass;
    private boolean        preSave = false;
    private JCheckBox      save;
    private final boolean  rememberDisabled;
    private String         message;

    public void setMessage(final String message) {
        this.message = message;
    }

    public LoginDialog(final int flag) {
        this(flag, _AWU.T.AccountNew_AccountNew_title(), _AWU.T.AccountNew_AccountNew_message(), DialogIcon.DIALOG_LOGIN.get(32));
    }

    public LoginDialog(final int flag, final String title, final String message, final Icon icon) {
        super(flag & 0xffffffff & ~Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN, title, icon, null, null);
        rememberDisabled = BinaryLogic.containsAll(flag, LoginDialog.DISABLE_REMEMBER);
        this.message = message;
    }

    protected JLabel addSettingName(final String name) {
        final JLabel lbl = new JLabel(name);
        lbl.setForeground(titleColor);
        return lbl;
    }

    public String getMessage() {
        return message;
    }

    public void caretUpdate(final CaretEvent e) {
        if (accid.getText().length() == 0) {
            okButton.setEnabled(false);
        } else {
            okButton.setEnabled(true);
        }
    }

    @Override
    protected LoginData createReturnValue() {
        if ((getReturnmask() & (Dialog.RETURN_OK | Dialog.RETURN_TIMEOUT)) == 0) {
            return null;
        }
        return new LoginData(accid.getText(), new String(pass.getPassword()), save.isSelected());
    }

    @Override
    public JComponent layoutDialogContent() {
        final JPanel contentpane = new JPanel();
        titleColor = Color.DARK_GRAY;
        accid = new JTextField(10);
        accid.addCaretListener(this);
        pass = new JPasswordField(10);
        save = new JCheckBox();
        if (rememberDisabled) {
            save.setEnabled(false);
        }
        contentpane.setLayout(new MigLayout("ins 5, wrap 2", "[]10[grow,fill]", "[][]"));
        addUserNameField(contentpane);
        addPasswordField(contentpane);
        addSave(contentpane);
        accid.setText(preUser);
        pass.setText(prePass);
        save.setSelected(preSave);
        return contentpane;
    }

    protected void addPasswordField(final JPanel contentpane) {
        contentpane.add(addSettingName(_AWU.T.AccountNew_layoutDialogContent_password()));
        contentpane.add(pass, "sizegroup g1");
    }

    protected void addUserNameField(final JPanel contentpane) {
        contentpane.add(new JLabel(message), "spanx");
        contentpane.add(addSettingName(getTranslationUserName()));
        contentpane.add(accid, "sizegroup g1,width 100:250:n");
    }

    protected String getTranslationUserName() {
        return _AWU.T.AccountNew_layoutDialogContent_accountname();
    }

    protected void addSave(final JPanel contentpane) {
        contentpane.add(addSettingName(_AWU.T.AccountNew_layoutDialogContent_save()));
        contentpane.add(save, "sizegroup g1");
    }

    @Override
    protected void packed() {
        super.packed();
        setResizable(false);
    }

    @Override
    protected void initFocus(final JComponent focus) {
        accid.selectAll();
        accid.requestFocusInWindow();
    }

    public void setPasswordDefault(final String password) {
        prePass = password;
    }

    public void setRememberDefault(final boolean preSave) {
        this.preSave = preSave;
    }

    public void setUsernameDefault(final String user) {
        preUser = user;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.dialog.LoginDialogInterface#getUsername()
     */
    @Override
    public String getUsername() {
        if ((getReturnmask() & (Dialog.RETURN_OK | Dialog.RETURN_TIMEOUT)) == 0) {
            return null;
        }
        return new EDTHelper<String>() {
            @Override
            public String edtRun() {
                return accid.getText();
            }
        }.getReturnValue();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.dialog.LoginDialogInterface#getPassword()
     */
    @Override
    public String getPassword() {
        if ((getReturnmask() & (Dialog.RETURN_OK | Dialog.RETURN_TIMEOUT)) == 0) {
            return null;
        }
        return new EDTHelper<String>() {
            @Override
            public String edtRun() {
                return new String(pass.getPassword());
            }
        }.getReturnValue();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.dialog.LoginDialogInterface#getDefaultUsername()
     */
    @Override
    public String getDefaultUsername() {
        return preUser;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.dialog.LoginDialogInterface#getDefaultPassword()
     */
    @Override
    public String getDefaultPassword() {
        return prePass;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.dialog.LoginDialogInterface#isDefaultRememberSelected()
     */
    @Override
    public boolean isDefaultRememberSelected() {
        return preSave;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.dialog.LoginDialogInterface#isRememberSelected()
     */
    @Override
    public boolean isRememberSelected() {
        if ((getReturnmask() & (Dialog.RETURN_OK | Dialog.RETURN_TIMEOUT)) == 0) {
            return false;
        }
        // return new LoginData(accid.getText(), new String(pass.getPassword()), );
        return new EDTHelper<Boolean>() {
            @Override
            public Boolean edtRun() {
                return save.isSelected();
            }
        }.getReturnValue() == Boolean.TRUE;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.dialog.LoginDialogInterface#isRememberOptionVisible()
     */
    @Override
    public boolean isRememberOptionVisible() {
        return !rememberDisabled;
    }
}
