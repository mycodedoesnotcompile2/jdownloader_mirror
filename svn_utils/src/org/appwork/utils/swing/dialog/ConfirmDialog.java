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

import java.awt.Font;

import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.text.JTextComponent;

import org.appwork.swing.MigPanel;
import org.appwork.uio.ConfirmDialogInterface;
import org.appwork.uio.UIOManager;
import org.appwork.utils.BinaryLogic;
import org.appwork.utils.os.CrossSystem;

public class ConfirmDialog extends AbstractDialog<Integer> implements ConfirmDialogInterface {
    public static final int  STYLE_HTML       = Dialog.STYLE_HTML;
    public static final int  STYLE_SCROLLPANE = Dialog.STYLE_LARGE;
    private String           message;
    protected JTextComponent textComponent;

    public void setMessage(final String message) {
        this.message = message;
    }

    @Override
    public boolean isRemoteAPIEnabled() {
        return true;
    }

    // /*
    // * (non-Javadoc)
    // *
    // * @see org.appwork.utils.swing.dialog.AbstractDialog#_initHeadless()
    // */
    // @Override
    // protected void _initHeadless(AbstractConsole console) {
    //
    // boolean stdBefore = false;
    // boolean errBefore = false;
    // try {
    // stdBefore = Application.STD_OUT.setBufferEnabled(true);
    // errBefore = Application.ERR_OUT.setBufferEnabled(true);
    // } catch (IOException e) {
    // e.printStackTrace();
    // // cannot happen for parameter=true;
    // }
    //
    // try {
    //
    // console.println("|--------------------------INPUT REQUIRED-------------------------------");
    //
    // console.println("|Dialog> " + getTitle());
    // console.println("|Dialog> " + getMessage());
    // if ((this.flagMask & UIOManager.BUTTONS_HIDE_OK) == 0 || (this.flagMask &
    // UIOManager.BUTTONS_HIDE_CANCEL) == 0) {
    // console.println("|Dialog> Press any key to continue");
    // console.print("|Input > ");
    // console.readLine();
    // if ((this.flagMask & UIOManager.BUTTONS_HIDE_OK) != 0) {
    // this.setReturnmask(true);
    // } else if ((this.flagMask & UIOManager.BUTTONS_HIDE_CANCEL) != 0) {
    // this.setReturnmask(false);
    // } else {
    // this.returnBitMask |= Dialog.RETURN_CLOSED;
    // }
    //
    // } else {
    // console.println("|Dialog> Enter y for " + ((getOKButtonText() == null) ?
    // "OK" : getOKButtonText()));
    // console.println("|Dialog> Enter n for " + ((getCancelButtonText() ==
    // null) ? "CANCEL" : getCancelButtonText()));
    // console.print("|Input > ");
    // if (console.readLine().trim().equalsIgnoreCase("y")) {
    // console.println("|Dialog> OK");
    // this.setReturnmask(true);
    // } else {
    // console.println("|Dialog> Cancel");
    // this.setReturnmask(false);
    // }
    // }
    //
    // console.println("|-----------------------------------------------------------------------");
    // } finally {
    // try {
    // Application.STD_OUT.setBufferEnabled(stdBefore);
    // } catch (Throwable e) {
    // e.printStackTrace();
    // }
    // try {
    // Application.ERR_OUT.setBufferEnabled(errBefore);
    // } catch (Throwable e) {
    // e.printStackTrace();
    //
    // }
    // }
    // }
    public ConfirmDialog(final int flag, final String title, final String message, final Icon icon, final String okOption, final String cancelOption) {
        super(flag, title, icon, okOption, cancelOption);
        getLogger().fine("Dialog    [" + okOption + "][" + cancelOption + "]\r\nflag:  " + Integer.toBinaryString(flag) + "\r\ntitle: " + title + "\r\nmsg:   \r\n" + message);
        this.message = message;
    }

    /**
     * @param i
     * @param name
     * @param gui_settings_extensions_show_now
     */
    public ConfirmDialog(final int flag, final String title, final String message) {
        this(flag, title, message, null, null, null);
    }

    public String getMessage() {
        return message;
    }

    public ConfirmDialogInterface show() {
        return UIOManager.I().show(ConfirmDialogInterface.class, this);
    }

    @Override
    protected Integer createReturnValue() {
        return getReturnmask();
    }

    protected boolean isResizable() {
        return true;
    }

    @Override
    public JComponent layoutDialogContent() {
        final MigPanel p = new MigPanel("ins 0", "[]", "[]");
        textComponent = addMessageComponent(p);
        return p;
    }

    protected JTextComponent addMessageComponent(final MigPanel p) {
        JTextPane textField = new JTextPane() {
            private static final long serialVersionUID = 1L;

            @Override
            public boolean getScrollableTracksViewportWidth() {
                return !BinaryLogic.containsAll(ConfirmDialog.this.flagMask, Dialog.STYLE_LARGE);
            }
        };
        modifyTextPane(textField);
        final Font font = textField.getFont();
        if (BinaryLogic.containsAll(flagMask, Dialog.STYLE_HTML)) {
            textField.setContentType("text/html");
            textField.addHyperlinkListener(new HyperlinkListener() {
                public void hyperlinkUpdate(final HyperlinkEvent e) {
                    if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
                        CrossSystem.openURL(e.getURL());
                    }
                }
            });
        } else {
            textField.setContentType("text/plain");
            // this.textField.setMaximumSize(new Dimension(450, 600));
        }
        textField.setFont(font);
        textField.setText(getMessage());
        textField.setEditable(false);
        textField.setBackground(null);
        textField.setOpaque(false);
        textField.setFocusable(false);
        textField.setForeground(new JLabel().getForeground());
        textField.putClientProperty("Synthetica.opaque", Boolean.FALSE);
        textField.setCaretPosition(0);
        if (BinaryLogic.containsAll(flagMask, Dialog.STYLE_LARGE)) {
            p.add(new JScrollPane(textField), "pushx,growx");
        } else {
            p.add(textField);
        }
        return textField;
    }

    /**
     * @param textField
     */
    protected void modifyTextPane(JTextPane textField) {        
    }

    @Override
    public String toString() {
        if (BinaryLogic.containsAll(flagMask, Dialog.LOGIC_DONOTSHOW_BASED_ON_TITLE_ONLY)) {
            return ("dialog-" + getTitle()).replaceAll("\\W", "_");
        } else {
            return ("dialog-" + getTitle() + "_" + getMessage()).replaceAll("\\W", "_");
        }
    }
}
