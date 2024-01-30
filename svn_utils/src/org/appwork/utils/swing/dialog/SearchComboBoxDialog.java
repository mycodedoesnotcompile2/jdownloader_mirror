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

import java.awt.Component;
import java.awt.event.WindowEvent;
import java.awt.event.WindowFocusListener;
import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Arrays;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JTextPane;
import javax.swing.ListCellRenderer;

import org.appwork.storage.JSonStorage;
import org.appwork.swing.MigPanel;
import org.appwork.swing.components.searchcombo.SearchComboBox;
import org.appwork.utils.ImageProvider.ImageProvider;
import org.appwork.utils.images.IconIO;
import org.appwork.utils.net.Base64OutputStream;

public class SearchComboBoxDialog<Type> extends AbstractDialog<Type> implements IconComboBoxDialogInterface {
    /**
     * Stores an additional message
     */
    private final String           message;
    /**
     * Textpane to display th {@link #message}
     */
    private JTextPane              textpane;
    /**
     * Defaultanswer. Answers are given as optionindex
     */
    private final Type             defaultAnswer;
    /**
     * Available options
     */
    private final Type[]           options;
    protected SearchComboBox<Type> box;

    @Override
    public boolean isRemoteAPIEnabled() {
        return true;
    }

    /**
     *
     * @see Dialog#showComboDialog(int, String, String, Object[], int, ImageIcon, String, String, ListCellRenderer)
     */
    public SearchComboBoxDialog(final int flag, final String title, final String question, final Type[] options, final Type defaultSelection, final ImageIcon icon, final String okText, final String cancelText) {
        super(flag, title, icon, okText, cancelText);
        getLogger().fine("Dialog    [" + okText + "][" + cancelText + "]\r\nflag:  " + Integer.toBinaryString(flag) + "\r\ntitle: " + title + "\r\nmsg:   \r\n" + question + "\r\noptions:   \r\n" + Arrays.toString(options) + "\r\ndef:" + defaultSelection);
        message = question;
        defaultAnswer = defaultSelection;
        this.options = options;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.dialog.AbstractDialog#getRetValue()
     */
    @Override
    protected Type createReturnValue() {
        return getSelectedItem();
    }

    /**
     * @param options2
     * @return
     */
    protected SearchComboBox<Type> getComboBox(final Type[] options2) {
        final ArrayList<Type> ret = new ArrayList<Type>();
        for (final Type t : options2) {
            ret.add(t);
        }
        final SearchComboBox<Type> box = createInputField(ret);
        if (defaultAnswer != null) {
            box.setSelectedItem(defaultAnswer);
        }
        return box;
    }

    public SearchComboBox<Type> createInputField(final ArrayList<Type> ret) {
        return new SearchComboBox<Type>(ret) {
            @Override
            protected Icon getIconForValue(final Type value) {
                if (value == null) {
                    return null;
                }
                return SearchComboBoxDialog.this.getIconByValue(value);
            }

            @Override
            public void onChanged() {
                super.onChanged();
                SearchComboBoxDialog.this.onChanged();
            }

            @Override
            protected String getTextForValue(final Type value) {
                if (value == null) {
                    return "";
                }
                return SearchComboBoxDialog.this.getStringByValue(value);
            }
        };
    }

    /**
     * @param value
     * @return
     */
    protected String getStringByValue(final Type value) {
        return value + "";
    }

    /**
     *
     */
    protected void onChanged() {
        
    }

    /**
     * @param value
     * @return
     */
    protected Icon getIconByValue(final Type value) {
        return null;
    }

    public Type getSelectedItem() {
        if ((getReturnmask() & Dialog.RETURN_OK) == 0) {
            return null;
        }
        return box == null ? defaultAnswer : box.getSelectedItem();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.dialog.AbstractDialog#layoutDialogContent()
     */
    @Override
    public JComponent layoutDialogContent() {
        final JPanel contentpane = new MigPanel("ins 0,wrap 1", "[fill,grow]", "[][]");
        textpane = new JTextPane();
        textpane.setBorder(null);
        textpane.setBackground(null);
        textpane.setOpaque(false);
        textpane.putClientProperty("Synthetica.opaque", Boolean.FALSE);
        textpane.setText(message);
        textpane.setEditable(false);
        contentpane.add(textpane);
        box = getComboBox(options);
        // no idea what this has been good for
        // if (this.getDesiredSize() != null) {
        // this.box.setBounds(0, 0, (int) this.getDesiredSize().getWidth(),
        // (int) this.getDesiredSize().getHeight());
        // this.box.setMaximumSize(this.getDesiredSize());
        // } else {
        // this.box.setBounds(0, 0, 450, 600);
        // this.box.setMaximumSize(new Dimension(450, 600));
        // }
        contentpane.add(box, "pushy,growy,height 24!");
        getDialog().addWindowFocusListener(new WindowFocusListener() {
            @Override
            public void windowLostFocus(final WindowEvent windowevent) {                
            }

            @Override
            public void windowGainedFocus(final WindowEvent windowevent) {
                final Component focusOwner = getDialog().getFocusOwner();
                if (focusOwner != null) {
                    // dialog component has already focus...
                    return;
                }
                /* we only want to force focus on first window open */
                getDialog().removeWindowFocusListener(this);
                box.requestFocus();
            }
        });
        return contentpane;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.uio.ComboBoxDialogInterface#getSelectedIndex()
     */
    @Override
    public int getSelectedIndex() {
        if ((getReturnmask() & Dialog.RETURN_OK) == 0) {
            return -1;
        }
        if (box != null) {
            return box.getSelectedIndex();
        }
        if (defaultAnswer == null) {
            return -1;
        }
        return Arrays.binarySearch(options, defaultAnswer);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.uio.ConfirmDialogInterface#getMessage()
     */
    @Override
    public String getMessage() {
        return message;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.uio.ComboBoxDialogInterface#getLabels()
     */
    @Override
    public String[] getLabels() {
        String[] ret = new String[options.length];
        for (int i = 0; i < ret.length; i++) {
            ret[i] = JSonStorage.toString(getStringByValue(options[i]));
        }
        return ret;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.uio.ComboBoxDialogInterface#getPreSelectedIndex()
     */
    @Override
    public int getPreSelectedIndex() {
        for (int i = 0; i < options.length; i++) {
            if (options[i].equals(defaultAnswer)) {
                return i;
            }
        }
        return -1;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.dialog.IconComboBoxDialogInterface#getIconDataUrls ()
     */
    @Override
    public String[] getIconDataUrls() {
        String[] ret = new String[options.length];
        for (int i = 0; i < ret.length; i++) {
            Icon ic = getIconByValue(options[i]);
            if (ic == null) {
                ret[i] = null;
            } else {
                Base64OutputStream b64os = null;
                ByteArrayOutputStream bos = null;
                try {
                    bos = new ByteArrayOutputStream();
                    b64os = new Base64OutputStream(bos);
                    ImageProvider.writeImage(IconIO.convertIconToBufferedImage(ic), "png", b64os);
                    b64os.flush(true);
                    ret[i] = "png;base64," + bos.toString("UTF-8");
                } catch (final Exception e) {
                    getLogger().log(e);
                    return null;
                } finally {
                    try {
                        b64os.close();
                    } catch (final Throwable e) {
                    }
                    try {
                        bos.close();
                    } catch (final Throwable e) {
                    }
                }
            }
        }
        return ret;
    }
}
