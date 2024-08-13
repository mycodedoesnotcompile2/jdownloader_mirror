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
package org.appwork.storage.config.swing.models;

import java.util.List;

import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;

import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.annotations.SpinnerValidator;
import org.appwork.storage.config.handler.ByteKeyHandler;
import org.appwork.storage.config.swing.ValueProvider;
import org.appwork.storage.config.swing.ValueProviderListener;
import org.appwork.utils.swing.EDTRunner;

public class ConfigByteSpinnerModel extends SpinnerNumberModel implements ValueProviderListener<Byte> {
    /**
     *
     */
    private static final long         serialVersionUID = 8542048212034642953L;
    private final ValueProvider<Byte> provider;

    public ConfigByteSpinnerModel(ByteKeyHandler keyHandler) {
        this(new KeyHandlerProviderBridge<Byte>(keyHandler));
    }

    public ConfigByteSpinnerModel(ValueProvider<Byte> provider) {
        this.provider = provider;
        provider.register(this, true);
        final List<SpinnerValidator> spinn = provider.getAnnotations(SpinnerValidator.class);
        if (spinn != null && spinn.size() > 0) {
            setMinimum(spinn.get(0).min());
            setMaximum(spinn.get(0).max());
            setStepSize(spinn.get(0).step());
        }
    }

    @Override
    public void setMinimum(Comparable minimum) {
        super.setMinimum(((Number) minimum).byteValue());
    }

    @Override
    public void setMaximum(Comparable maximum) {
        super.setMaximum(((Number) maximum).byteValue());
    }

    @Override
    public void setStepSize(Number stepSize) {
        super.setStepSize(stepSize.byteValue());
    }

    @Override
    public Number getNumber() {
        return provider.get();
    }

    /**
     * Returns the next number in the sequence.
     *
     * @return <code>value + stepSize</code> or <code>null</code> if the sum exceeds <code>maximum</code>.
     *
     * @see SpinnerModel#getNextValue
     * @see #getPreviousValue
     * @see #setStepSize
     */
    public Object getNextValue() {
        return incrValue(+1);
    }

    public Object getPreviousValue() {
        return incrValue(-1);
    }

    protected Number incrValue(final int i) {
        return ConfigIntSpinnerModel.incrValue(this, Byte.class, i);
    }

    @Override
    public Object getValue() {
        return provider.get();
    }

    @Override
    public void setValue(Object value) {
        try {
            if (value instanceof Number) {
                provider.set(((Number) value).byteValue());
            } else if (value instanceof String && ((String) value).matches("^-?\\d+$")) {
                provider.set(Byte.valueOf(String.valueOf(value)));
            }
        } catch (ValidationException e) {
            javax.swing.UIManager.getLookAndFeel().provideErrorFeedback(null);
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.config.swing.ValueProviderListener#onValueModified(org.appwork.storage.config.swing.ValueProvider,
     * java.lang.Object)
     */
    @Override
    public void onValueModified(ValueProvider<Byte> owner, Byte newValue) {
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                fireStateChanged();
            }
        };
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.config.swing.ValueProviderListener#onValueValidationError(org.appwork.storage.config.swing.ValueProvider,
     * java.lang.Object, java.lang.Exception)
     */
    @Override
    public void onValueValidationError(ValueProvider<Byte> owner, Byte invalidValue, Exception exception) {
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                fireStateChanged();
            }
        };
    }
}
