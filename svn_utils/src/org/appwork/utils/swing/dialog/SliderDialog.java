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

import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.JTextPane;
import javax.swing.ListCellRenderer;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import org.appwork.swing.MigPanel;
import org.appwork.uio.SliderDialogInterface;

public class SliderDialog extends AbstractDialog<Integer> implements SliderDialogInterface, ChangeListener {
    /**
     * The Comboxbox to display the options
     */
    private JSlider      slider;
    /**
     * Stores an additional message
     */
    private final String message;
    /**
     * Textpane to display th {@link #message}
     */
    private JTextPane    textpane;
    /**
     * Defaultanswer. Answers are given as optionindex
     */
    private final int    defaultAnswer;
    protected JLabel     statusLabel;

    @Override
    public boolean isRemoteAPIEnabled() {
        return false;
    }

    /**
     *
     * @see Dialog#showComboDialog(int, String, String, Object[], int, Icon, String, String, ListCellRenderer)
     */
    public SliderDialog(final int flag, final String title, final String question, final int defaultValue, final Icon icon, final String okText, final String cancelText) {
        super(flag, title, icon, okText, cancelText);
        getLogger().fine("Dialog    [" + okText + "][" + cancelText + "]\r\nflag:  " + Integer.toBinaryString(flag) + "\r\ntitle: " + title + "\r\nmsg:   \r\n" + question + "\r\ndef:" + defaultValue);
        message = question;
        defaultAnswer = defaultValue;
    }

    /**
     * @param options2
     * @return
     */
    protected JSlider createSliderComponent() {
        JSlider slider = new JSlider(JSlider.HORIZONTAL, getMinValue(), getMaxValue(), defaultAnswer);
        slider.setMinorTickSpacing(2);
        slider.setMajorTickSpacing(10);
        slider.setPaintTicks(true);
        slider.setPaintLabels(true);
        // We'll just use the standard numeric labels for now...
        slider.setLabelTable(slider.createStandardLabels(10));
        return slider;
    }

    /**
     * @return
     */
    public int getMaxValue() {        
        return 100;
    }

    /**
     * @return
     */
    public int getMinValue() {        
        return 0;
    }

    public int getValue() {
        return slider.getValue();
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
        slider = createSliderComponent();
        slider.addChangeListener(this);
        contentpane.add(slider, "");
        statusLabel = new JLabel();
        contentpane.add(statusLabel, "");
        onSliderValueChanged();
        return contentpane;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.utils.swing.dialog.AbstractDialog#getPreferredWidth()
     */
    @Override
    protected int getPreferredWidth() {        
        return 600;
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
     * @see javax.swing.event.ChangeListener#stateChanged(javax.swing.event.ChangeEvent)
     */
    @Override
    public void stateChanged(ChangeEvent e) {
        onSliderValueChanged();
    }

    /**
     *
     */
    protected void onSliderValueChanged() {        
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.dialog.AbstractDialog#createReturnValue()
     */
    @Override
    protected Integer createReturnValue() {        
        return getValue();
    }
}
