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
package org.appwork.utils.swing;

import java.text.ParseException;

import javax.swing.JFormattedTextField.AbstractFormatter;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;
import javax.swing.text.DefaultFormatterFactory;

public class JSimpleNumberSpinner extends JSpinner {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * @param i
     * @param j
     * @param k
     */
    public JSimpleNumberSpinner(final int steps, final int min, final int max, final int secondmin) {
        super(new SpinnerNumberModel(min, min, max, steps) {
            /**
             *
             */
            private static final long serialVersionUID = -5666000802809450936L;

            @Override
            public Object getNextValue() {
                final Number n = getNumber();
                if (n != null && (n.intValue() >= min && n.intValue() < secondmin)) {
                    return secondmin;
                } else {
                    return super.getNextValue();
                }
            }

            @Override
            public Object getPreviousValue() {
                final Number n = getNumber();
                if (n != null && (n.intValue() >= min && n.intValue() <= secondmin)) {
                    return min;
                } else {
                    return super.getPreviousValue();
                }
            }
        });
        final DefaultFormatterFactory factory = new DefaultFormatterFactory(new AbstractFormatter() {
            /**
             *
             */
            private static final long serialVersionUID = 3244976028578192576L;

            @Override
            public Object stringToValue(final String text) throws ParseException {
                try {
                    int i = Integer.parseInt(text);
                    if (i > min && i < secondmin) {
                        i = min;
                    }
                    if (i < min) {
                        i = min;
                    }
                    if (i > max) {
                        i = max;
                    }
                    return i;
                } catch (Throwable e) {
                    return null;
                }
            }

            @Override
            public String valueToString(final Object value) throws ParseException {
                return value + "";
            }
        });
        ((JSpinner.DefaultEditor) this.getEditor()).getTextField().setFormatterFactory(factory);
    }
}
