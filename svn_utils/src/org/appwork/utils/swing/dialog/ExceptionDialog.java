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
import java.awt.Cursor;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextPane;
import javax.swing.SwingConstants;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;

import org.appwork.resources.AWUTheme;
import org.appwork.uio.ExceptionDialogInterface;
import org.appwork.utils.BinaryLogic;
import org.appwork.utils.Exceptions;
import org.appwork.utils.locale._AWU;
import org.appwork.utils.os.CrossSystem;

import net.miginfocom.swing.MigLayout;

/**
 * @author thomas
 *
 */
public class ExceptionDialog extends AbstractDialog<Integer> implements ExceptionDialogInterface {
    private final String    message;
    private JTextPane       textField;
    private final Throwable exception;
    private JTextArea       logField;
    private JScrollPane     scrollPane;
    private JLabel          logLabel;
    private JButton         more;
    private boolean         expanded = false;
    private String          moreString;

    public boolean isExpanded() {
        return expanded;
    }

    @Override
    public boolean isRemoteAPIEnabled() {
        return true;
    }

    public void setExpanded(boolean expanded) {
        this.expanded = expanded;
    }

    public static final int EXPANDED = 1 << 20;

    public ExceptionDialog(final int flag, final String title, final String message, final Throwable exception, final String okOption, final String cancelOption) {
        super(flag, title, null, okOption, cancelOption);
        getLogger().fine("Dialog    [" + okOption + "][" + cancelOption + "]\r\nflag:  " + Integer.toBinaryString(flag) + "\r\ntitle: " + title + "\r\nmsg:   \r\n" + message);
        this.message = message;
        this.exception = exception;
        this.expanded = BinaryLogic.containsAll(flag, EXPANDED);
    }

    public String getMessage() {
        return message;
    }

    @Override
    protected void addButtons(final JPanel buttonBar) {
        more = new JButton(_AWU.T.ExceptionDialog_layoutDialogContent_more_button());
        more.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        // more.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0,
        // cp.getBackground().darker()));
        more.addActionListener(new ActionListener() {
            public void actionPerformed(final ActionEvent e) {
                expand();
            }
        });
        more.setHorizontalAlignment(SwingConstants.RIGHT);
        if (expanded) {
            expand();
        } else {
            buttonBar.add(more, "hidemode 3");
        }
    }

    @Override
    protected Integer createReturnValue() {
        return this.getReturnmask();
    }

    @Override
    public String getDontShowAgainKey() {
        return "ABSTRACTDIALOG_DONT_SHOW_AGAIN_" + this.exception.hashCode() + "_" + this.toString();
    }

    public Throwable getException() {
        return this.exception;
    }

    @Override
    public JComponent layoutDialogContent() {
        final JPanel cp = new JPanel(new MigLayout("ins 0,wrap 1", "[fill]", "[][]"));
        this.textField = new JTextPane() {
            private static final long serialVersionUID = 1L;

            @Override
            public boolean getScrollableTracksViewportWidth() {
                return !BinaryLogic.containsAll(ExceptionDialog.this.flagMask, Dialog.STYLE_LARGE);
            }
        };
        final Font font = textField.getFont();
        if (BinaryLogic.containsAll(this.flagMask, Dialog.STYLE_HTML)) {
            this.textField.setContentType("text/html");
            this.textField.addHyperlinkListener(new HyperlinkListener() {
                public void hyperlinkUpdate(final HyperlinkEvent e) {
                    if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
                        CrossSystem.openURL(e.getURL());
                    }
                }
            });
        } else {
            this.textField.setContentType("text/plain");
            // this.textField.setMaximumSize(new Dimension(450, 600));
        }
        textField.setFont(font);
        this.textField.setText(this.message);
        this.textField.setEditable(false);
        this.textField.setBackground(null);
        this.textField.setOpaque(false);
        this.textField.putClientProperty("Synthetica.opaque", Boolean.FALSE);
        this.textField.setCaretPosition(0);
        cp.add(new JLabel(AWUTheme.I().getIcon(Dialog.ICON_ERROR, 32)), "width 32!,split 2");
        if (BinaryLogic.containsAll(this.flagMask, Dialog.STYLE_LARGE)) {
            cp.add(new JScrollPane(this.textField), "pushx,growx");
        } else {
            cp.add(this.textField, "pushx,growx");
        }
        this.logField = new JTextArea();
        this.logField.setLineWrap(false);
        this.logField.setEditable(true);
        this.logField.setAutoscrolls(true);
        this.scrollPane = new JScrollPane(this.logField);
        this.scrollPane.setVisible(false);
        this.logField.setEditable(true);
        this.logField.setAutoscrolls(true);
        this.logField.setForeground(Color.RED);
        this.logLabel = new JLabel(_AWU.T.ExceptionDialog_layoutDialogContent_logLabel());
        this.logLabel.setVisible(false);
        cp.add(this.logLabel, "hidemode 3,gaptop 5");
        cp.add(this.scrollPane, "hidemode 3,height 100:300:n,width 200:600:n,pushx,growx,pushy,growy");
        CrossSystem.playErrorSound();
        return cp;
    }

    @Override
    public String toString() {
        return ("dialog-" + this.getTitle() + "_" + this.message).replaceAll("\\W", "_");
    }

    public void expand() {
        ExceptionDialog.this.scrollPane.setVisible(true);
        if (moreString != null) {
            logField.setText(moreString);
        } else {
            ExceptionDialog.this.logField.setText(stacktraceToString());
        }
        ExceptionDialog.this.logLabel.setVisible(true);
        more.setVisible(false);
        ExceptionDialog.this.setResizable(true);
        ExceptionDialog.this.pack();
    }

    protected String stacktraceToString() {
        return Exceptions.getStackTrace(ExceptionDialog.this.exception);
    }

    /**
     * @param string
     */
    public void setMore(String string) {
        moreString = string;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.uio.ExceptionDialogInterface#getStacktrace()
     */
    @Override
    public String getStacktrace() {
        return stacktraceToString();
    }
}
