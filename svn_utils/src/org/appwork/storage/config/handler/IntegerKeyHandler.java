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
package org.appwork.storage.config.handler;

import java.lang.annotation.Annotation;

import org.appwork.storage.Storage;
import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.LookUpKeys;
import org.appwork.storage.config.annotations.SpinnerValidator;
import org.appwork.utils.StringUtils;

/**
 * @author Thomas
 *
 */
public class IntegerKeyHandler extends KeyHandler<Integer> {
    private SpinnerValidator validator;

    /**
     * @param storageHandler
     * @param key
     */
    public IntegerKeyHandler(final StorageHandler<?> storageHandler, final String key) {
        super(storageHandler, key);
    }

    @SuppressWarnings("unchecked")
    @Override
    protected Class<? extends Annotation>[] getAllowedAnnotations() {
        return (Class<? extends Annotation>[]) new Class<?>[] { LookUpKeys.class, SpinnerValidator.class };
    }

    @Override
    protected Class<? extends Annotation> getDefaultAnnotation() {
        return DefaultIntValue.class;
    }

    @Override
    protected void initDefaults() throws Throwable {
        this.setDefaultValue(Integer.valueOf(0));
    }

    @Override
    protected void initHandler() {
        this.validator = this.getAnnotation(SpinnerValidator.class);
        setStorageSyncMode(getDefaultStorageSyncMode());
    }

    @Override
    protected boolean equals(Object x, Object y) {
        if (x == y) {
            return true;
        } else if (x instanceof Integer && y instanceof Integer) {
            return x.equals(y);
        } else {
            return false;
        }
    }

    @Override
    protected Integer getValueStorage() {
        final Object rawValue = getRawValueStorage();
        if (rawValue instanceof Number) {
            return ((Number) rawValue).intValue();
        } else if (rawValue instanceof String) {
            final String stringValue = (String) rawValue;
            if (StringUtils.equalsIgnoreCase("null", stringValue)) {
                if (isRealPrimitive()) {
                    return 0;
                } else {
                    return null;
                }
            } else {
                return Integer.valueOf(stringValue);
            }
        } else {
            return (Integer) rawValue;
        }
    }

    @Override
    protected void putValue(final Integer object) {
        final Storage storage = this.storageHandler.getPrimitiveStorage(this);
        if (storage != null) {
            storage.put(this.getKey(), object);
        }
    }

    @Override
    protected void validateValue(final Integer object) throws Throwable {
        if (this.validator != null) {
            final int v = object.intValue();
            final long min = this.validator.min();
            final long max = this.validator.max();
            if (v < min) {
                throw new ValidationException("value=" + v + " < min=" + min);
            } else if (v > max) {
                throw new ValidationException("value=" + v + " > max=" + max);
            }
        }
    }
}
