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

import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.WindowConstants;
import javax.swing.text.JTextComponent;

import org.appwork.utils.BinaryLogic;
import org.appwork.utils.locale._AWU;
import org.appwork.utils.swing.SwingUtils;
import org.appwork.utils.swing.dialog.AbstractDialog;

import net.miginfocom.swing.MigLayout;

/**
 * @deprecated port to {@link AbstractDialog}
 * @author thomas
 *
 */
@Deprecated
public abstract class SearchDialog extends JDialog implements WindowListener, ActionListener, FocusListener {
    private static final long    serialVersionUID = 9206575398715006581L;
    public static final int      NO_REGEX_FLAG    = 1 << 0;
    public static final int      NO_CASE_FLAG     = 1 << 1;
    private final ExtTable<?>    owner;
    private final JTextComponent input;
    private final JCheckBox      caseSensitive;
    private final JCheckBox      regularExpression;
    private final JButton        okButton;

    public SearchDialog(final int flag, final ExtTable<?> owner) {
        super(SwingUtils.getWindowForComponent(owner), _AWU.T.EXTTABLE_SEARCH_DIALOG_TITLE());
        this.owner = owner;
        this.owner.addFocusListener(this);
        caseSensitive = new JCheckBox(_AWU.T.SEARCHDIALOG_CHECKBOX_CASESENSITIVE());
        regularExpression = new JCheckBox(_AWU.T.SEARCHDIALOG_CHECKBOX_REGULAREXPRESSION());
        try {
            caseSensitive.setSelected(owner.getStorage().get("caseSensitive", false));
            regularExpression.setSelected(owner.getStorage().get("regularExpression", false));
            final ActionListener saveListener = new ActionListener() {
                public void actionPerformed(final ActionEvent e) {
                    owner.getStorage().put("caseSensitive", caseSensitive.isSelected());
                    owner.getStorage().put("regularExpression", regularExpression.isSelected());
                }
            };
            caseSensitive.addActionListener(saveListener);
            regularExpression.addActionListener(saveListener);
        } catch (final Exception e) {
            org.appwork.loggingv3.LogV3.log(e);
        }
        caseSensitive.setVisible(BinaryLogic.containsNone(flag, SearchDialog.NO_CASE_FLAG));
        regularExpression.setVisible(BinaryLogic.containsNone(flag, SearchDialog.NO_REGEX_FLAG));
        setLayout(new MigLayout("ins 5", "[fill,grow]", "[fill,grow][][]"));
        // Dispose dialog on close
        setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        addWindowListener(this);
        okButton = new JButton(_AWU.T.SEARCHDIALOG_BUTTON_FIND());
        okButton.addActionListener(this);
        this.add(new JLabel(ExtTableIcon.TABLE_SEARCH_DIALOG_FIND.get(32)), "alignx left,aligny center,shrinkx,gapright 10,spany");
        input = new JTextField();
        input.setBorder(BorderFactory.createEtchedBorder());
        this.add(input, "pushy,growy,spanx,wrap");
        input.selectAll();
        this.add(regularExpression);
        this.add(caseSensitive);
        this.add(okButton, "skip 2,alignx right,wrap");
        // pack dialog
        invalidate();
        pack();
        setResizable(false);
        if (!getParent().isDisplayable() || !getParent().isVisible()) {
            final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
            this.setLocation(new Point((int) (screenSize.getWidth() - getWidth()) / 2, (int) (screenSize.getHeight() - getHeight()) / 2));
        } else if (getParent() instanceof Frame && ((Frame) getParent()).getExtendedState() == Frame.ICONIFIED) {
            // dock dialog at bottom right if mainframe is not visible
            final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
            this.setLocation(new Point((int) (screenSize.getWidth() - getWidth() - 20), (int) (screenSize.getHeight() - getHeight() - 60)));
        } else {
            this.setLocation(SwingUtils.getCenter(getParent(), this));
        }
        // register an escape listener to cancel the dialog
        KeyStroke ks = KeyStroke.getKeyStroke("ESCAPE");
        okButton.getInputMap().put(ks, "ESCAPE");
        okButton.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(ks, "ESCAPE");
        okButton.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(ks, "ESCAPE");
        okButton.getActionMap().put("ESCAPE", new AbstractAction() {
            private static final long serialVersionUID = -6666144330707394562L;

            public void actionPerformed(final ActionEvent e) {
                SearchDialog.this.close();
            }
        });
        ks = KeyStroke.getKeyStroke("ENTER");
        okButton.getInputMap().put(ks, "ENTER");
        okButton.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(ks, "ENTER");
        okButton.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(ks, "ENTER");
        okButton.getActionMap().put("ENTER", new AbstractAction() {
            private static final long serialVersionUID = -1331741306700505613L;

            public void actionPerformed(final ActionEvent e) {
                okButton.doClick();
            }
        });
        setVisible(true);
        this.requestFocus();
        input.requestFocusInWindow();
        input.requestFocus();
    }

    abstract public void actionPerformed(ActionEvent e);

    private void close() {
        owner.removeFocusListener(this);
        dispose();
    }

    public void focusGained(final FocusEvent e) {
    }

    public void focusLost(final FocusEvent e) {
        if (!e.isTemporary()) {
            close();
        }
    }

    public String getReturnID() {
        return input.getText();
    }

    public boolean isCaseSensitive() {
        return caseSensitive.isSelected();
    }

    public boolean isRegex() {
        return regularExpression.isSelected();
    }

    @Override
    public void requestFocus() {
        super.requestFocus();
        input.requestFocusInWindow();
        input.requestFocus();
    }

    public void windowActivated(final WindowEvent arg0) {
    }

    public void windowClosed(final WindowEvent arg0) {
    }

    public void windowClosing(final WindowEvent arg0) {
        close();
    }

    public void windowDeactivated(final WindowEvent arg0) {
    }

    public void windowDeiconified(final WindowEvent arg0) {
    }

    public void windowIconified(final WindowEvent arg0) {
    }

    public void windowOpened(final WindowEvent arg0) {
    }
}
