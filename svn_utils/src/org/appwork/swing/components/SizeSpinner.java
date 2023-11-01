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
package org.appwork.swing.components;

import java.awt.Color;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.text.ParseException;

import javax.swing.JFormattedTextField.AbstractFormatter;
import javax.swing.JSpinner;
import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;
import javax.swing.Timer;
import javax.swing.text.DefaultFormatterFactory;

import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.formatter.SizeFormatter.Unit;

public class SizeSpinner extends ExtSpinner implements FocusListener, ActionListener {
    /**
     *
     */
    private static final long  serialVersionUID = -3983659343629867162L;
    private SpinnerNumberModel nm;

    public SizeSpinner(final long min, final long max, final long steps) {
        this(new SpinnerNumberModel(Long.valueOf(min), Long.valueOf(min), Long.valueOf(max), Long.valueOf(steps)));
    }

    /**
     * @param model
     */
    public SizeSpinner(final SpinnerNumberModel model) {
        super(model);
        // this.addFocusListener(this);
        this.nm = (SpinnerNumberModel) super.getModel();
        final DefaultFormatterFactory factory = new DefaultFormatterFactory(new AbstractFormatter() {
            private static final long serialVersionUID = 7808117078307243989L;

            @Override
            public Object stringToValue(final String text) throws ParseException {
                return SizeSpinner.this.textToObject(text);
            }

            @Override
            public String valueToString(final Object value) throws ParseException {
                return SizeSpinner.this.longToText(((Number) value).longValue());
            }
        });
        ((JSpinner.DefaultEditor) this.getEditor()).getTextField().setFormatterFactory(factory);
        ((JSpinner.DefaultEditor) this.getEditor()).getTextField().addFocusListener(this);
        ((JSpinner.DefaultEditor) this.getEditor()).getTextField().addActionListener(this);
    }

    /*
     * (non-Javadoc)
     *
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    @Override
    public void actionPerformed(final ActionEvent e) {
        this.correct();
    }

    /**
     *
     */
    private void beep() {
        Toolkit.getDefaultToolkit().beep();
        final Color bg = ((JSpinner.DefaultEditor) this.getEditor()).getTextField().getForeground();
        ((JSpinner.DefaultEditor) this.getEditor()).getTextField().setForeground(Color.RED);
        new Timer(1000, new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                ((JSpinner.DefaultEditor) SizeSpinner.this.getEditor()).getTextField().setForeground(bg);
                final Timer timer = (Timer) e.getSource();
                timer.stop();
            }
        }).start();
    }

    /**
     *
     */
    private void correct() {
        final long v = ((Number) this.getValue()).longValue();
        long newValue = v;
        if (this.nm.getMinimum() != null) {
            newValue = Math.max(v, ((Number) this.nm.getMinimum()).longValue());
        }
        if (this.nm.getMaximum() != null) {
            newValue = Math.min(((Number) this.nm.getMaximum()).longValue(), newValue);
        }
        if (newValue != v) {
            this.beep();
            this.setValue(newValue);
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see java.awt.event.FocusListener#focusGained(java.awt.event.FocusEvent)
     */
    @Override
    public void focusGained(final FocusEvent e) {
        // TODO Auto-generated method stub
    }

    /*
     * (non-Javadoc)
     *
     * @see java.awt.event.FocusListener#focusLost(java.awt.event.FocusEvent)
     */
    @Override
    public void focusLost(final FocusEvent e) {
        this.correct();
    }

    /**
     * @return
     */
    public long getBytes() {
        return ((Number) this.getValue()).longValue();
    }

    @Override
    public Object getNextValue() {
        Object ret = getValue();
        long num = ((Number) ret).longValue();
        Unit unit = SizeFormatter.getBestUnit(num);
        long add = getStep(num, unit);
        long newV;
        if (nm.getMaximum() != null) {
            newV = Math.min(((Number) nm.getMaximum()).longValue(), num + add);
        } else {
            newV = num + add;
        }
        Unit newUnit = SizeFormatter.getBestUnit(newV);
        if (newUnit == unit) {
            if (newV == num) {
                beep();
            }
            return newV;
        }
        newV = newUnit.getBytes1024() * 1;
        if (newV == num) {
            beep();
        }
        return newV;
    }

    public long getStep(long num, Unit unit) {
        long display = num / unit.getBytes1024();
        int log = display < 1 ? 0 : (int) Math.log10(display);
        log -= 1;
        long add = 0;
        if (log < 0) {
            add = (unit.getBytes1024() / 1024) * 100;
        } else {
            add = (long) (Math.pow(10, log) * unit.getBytes1024());
        }
        return Math.max(1, add);
    }

    @Override
    public Object getPreviousValue() {
        final Object ret = this.getValue();
        final long num = ((Number) ret).longValue();
        final Unit unit = SizeFormatter.getBestUnit(num);
        long add = getStep(num, unit);
        long nv;
        if (this.nm.getMinimum() != null) {
            nv = Math.max(((Number) this.nm.getMinimum()).longValue(), num - add);
        } else {
            nv = num - add;
        }
        final Unit nunit = SizeFormatter.getBestUnit(nv);
        if (nunit == unit) {
            if (nv == num) {
                this.beep();
            }
            return nv;
        }
        nv = nunit.getBytes1024() * 1000;
        if (nv == num) {
            this.beep();
        }
        return nv;
    }

    /**
     * @param longValue
     * @return
     */
    protected String longToText(final long longValue) {
        return SizeFormatter.formatBytes(longValue);
    }

    @Override
    public void setModel(final SpinnerModel model) {
        throw new IllegalStateException("Not available");
    }

    protected Object textToObject(final String text) {
        return SizeFormatter.getSize(text, true, true);
    }
}
