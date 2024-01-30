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

import java.util.Arrays;

import javax.swing.Icon;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextPane;
import javax.swing.ListCellRenderer;

import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.swing.MigPanel;
import org.appwork.uio.ComboBoxDialogInterface;

public class ComboBoxDialog extends AbstractDialog<Integer> implements ComboBoxDialogInterface {
    /**
     * The Comboxbox to display the options
     */
    private JComboBox              box;
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
    private final int              defaultAnswer;
    /**
     * Available options
     */
    private final Object[]         options;
    /**
     * Listrenderer to render the optionobjects
     */
    private final ListCellRenderer renderer;

    @Override
    public boolean isRemoteAPIEnabled() {
        return true;
    }

    /**
     *
     * @see Dialog#showComboDialog(int, String, String, Object[], int, Icon, String, String, ListCellRenderer)
     */
    public ComboBoxDialog(final int flag, final String title, final String question, final Object[] options, final int defaultSelection, final Icon icon, final String okText, final String cancelText, final ListCellRenderer renderer) {
        super(flag, title, icon, okText, cancelText);
        getLogger().fine("Dialog    [" + okText + "][" + cancelText + "]\r\nflag:  " + Integer.toBinaryString(flag) + "\r\ntitle: " + title + "\r\nmsg:   \r\n" + question + "\r\noptions:   \r\n" + Arrays.toString(options) + "\r\ndef:" + defaultSelection);
        message = question;
        this.renderer = renderer;
        defaultAnswer = defaultSelection < 0 ? 0 : defaultSelection;
        this.options = options;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.dialog.AbstractDialog#getRetValue()
     */
    @Override
    protected Integer createReturnValue() {
        return getReturnIndex();
    }

    /**
     * @param options2
     * @return
     */
    protected JComboBox getComboBox(final Object[] options2) {
        final JComboBox ret = new JComboBox(options2);
        final ListCellRenderer rend = getRenderer(ret.getRenderer());
        if (rend != null) {
            ret.setRenderer(rend);
        }
        try {
            if (defaultAnswer < options.length && defaultAnswer >= 0) {
                ret.setSelectedIndex(defaultAnswer);
            }
        } catch (final Exception e) {
            getLogger().log(e);
        }
        return ret;
    }

    /**
     * @param renderer2
     * @return
     */
    protected ListCellRenderer getRenderer(final ListCellRenderer orgRenderer) {        
        return renderer;
    }

    public Integer getReturnIndex() {
        if ((getReturnmask() & Dialog.RETURN_OK) == 0) {
            return Integer.valueOf(-1);
        }
        return Integer.valueOf(box.getSelectedIndex());
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
        textpane.setForeground(new JLabel().getForeground());
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
        return contentpane;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.dialog.ComboBoxDialogInterface#getSelectedIndex()
     */
    @Override
    public int getSelectedIndex() {
        return getReturnIndex();
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
            if (options[i] instanceof LabelInterface) {
                ret[i] = ((LabelInterface) options[i]).getLabel();
            } else {
                ret[i] = String.valueOf(options[i]);
            }
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
        return defaultAnswer;
    }
}
