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

import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.SwingConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import org.appwork.utils.BinaryLogic;
import org.appwork.utils.interfaces.ValueConverter;

import net.miginfocom.swing.MigLayout;

public class ValueDialog extends AbstractDialog<Long> implements KeyListener, MouseListener {

    private JTextArea            converted;
    private final long           defaultValue;
    private JTextField           editable;
    // faktor to downscale long to integervalues
    private int                  faktor = 1;
    private final long           max;
    private final String         message;
    private JTextPane            messageArea;
    private final long           min;
    private JSlider              slider;
    private final long           step;
    private final ValueConverter valueconverter;

    public ValueDialog(final int flag, final String title, final String message, final Icon icon, final String okOption, final String cancelOption, long defaultValue, long min, long max, long step, ValueConverter valueConverter) {
        super(flag, title, icon, okOption, cancelOption);
        getLogger().fine("Dialog    [" + okOption + "][" + cancelOption + "]\r\nflag:  " + Integer.toBinaryString(flag) + "\r\ntitle: " + title + "\r\nmsg:   \r\n" + message + " \r\n" + min + "<=" + defaultValue + "<=" + max + " [" + step + "]");
        this.message = message;
        while (max > Integer.MAX_VALUE) {
            max /= 2;
            defaultValue /= 2;
            min /= 2;
            step = Math.max(step / 2, 1);
            faktor *= 2;
        }
        this.defaultValue = defaultValue;
        this.min = min;
        this.max = max;
        this.step = step;
        if (valueConverter == null) {
            valueConverter = new ValueConverter() {
                public String toString(final long value) {
                    return value * faktor + "";
                }
            };
        }
        valueconverter = valueConverter;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.dialog.AbstractDialog#getRetValue()
     */
    @Override
    protected Long createReturnValue() {
        if ((getReturnmask() & (Dialog.RETURN_OK | Dialog.RETURN_TIMEOUT)) == 0) {
            return 0l;
        }
        updateSlider();
        return (long) slider.getValue() * (long) faktor;
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
        final JPanel contentpane = new JPanel(new MigLayout("ins 0,wrap 1", "[fill,grow]"));
        messageArea = new JTextPane();
        messageArea.setBorder(null);
        messageArea.setBackground(null);
        messageArea.setOpaque(false);
        messageArea.setText(message);
        messageArea.setEditable(false);
        messageArea.putClientProperty("Synthetica.opaque", Boolean.FALSE);

        contentpane.add(messageArea);
        if (BinaryLogic.containsAll(flagMask, Dialog.STYLE_LARGE)) {
            converted = new JTextArea(valueconverter.toString(defaultValue));
            converted.setEditable(false);
            converted.setBackground(null);
            slider = new JSlider(SwingConstants.HORIZONTAL, (int) min, (int) max, (int) defaultValue);
            slider.setMajorTickSpacing((int) step);
            slider.setSnapToTicks(true);
            slider.addKeyListener(this);
            slider.addMouseListener(this);
            editable = new JTextField();
            editable.addFocusListener(new FocusListener() {

                public void focusGained(final FocusEvent e) {

                }

                public void focusLost(final FocusEvent e) {
                    ValueDialog.this.updateSlider();

                }

            });
            editable.addKeyListener(new KeyListener() {

                public void keyPressed(final KeyEvent e) {

                }

                public void keyReleased(final KeyEvent e) {
                    if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                        ValueDialog.this.updateSlider();
                    }

                }

                public void keyTyped(final KeyEvent e) {

                }

            });
            slider.addChangeListener(new ChangeListener() {

                public void stateChanged(final ChangeEvent arg0) {
                    converted.setText(valueconverter.toString((slider.getValue() * faktor)));
                    editable.setText(slider.getValue() * faktor + "");
                }

            });
            editable.setText(defaultValue + "");
            contentpane.add(slider, "split 2,pushy,growy,w 250");
            contentpane.add(editable, "growx,pushx,width 80:n:n");
            contentpane.add(converted, "pushy,growy,w 250");
        } else {
            converted = new JTextArea(valueconverter.toString(defaultValue));
            slider = new JSlider(SwingConstants.HORIZONTAL, (int) min, (int) max, (int) defaultValue);
            slider.setMajorTickSpacing((int) step);
            slider.setSnapToTicks(true);
            slider.setBorder(BorderFactory.createEtchedBorder());
            slider.addKeyListener(this);
            slider.addMouseListener(this);
            slider.addChangeListener(new ChangeListener() {

                public void stateChanged(final ChangeEvent arg0) {
                    converted.setText(valueconverter.toString((slider.getValue() * faktor)));
                    editable.setText(slider.getValue() * faktor + "");
                }

            });

            contentpane.add(slider, "pushy,growy,w 250");
            contentpane.add(converted, "pushy,growy,w 250");
        }

        return contentpane;
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
    protected void initFocus(final JComponent focus) {
        slider.requestFocusInWindow();
    }

    private void updateSlider() {
        // new Thread() {
        // public void run() {
        // new EDTHelper<Object>() {
        //
        //
        // public Object edtRun() {
        try {
            final long value = Long.parseLong(editable.getText());
            slider.setValue((int) (value / faktor));
        } catch (final Exception e) {
            if (editable != null) {
                editable.setText(slider.getValue() * faktor + "");
            }
        }
        // return null;
        // }
        //
        // }.start();
        //
        // }
        // }.start();

    }

}
