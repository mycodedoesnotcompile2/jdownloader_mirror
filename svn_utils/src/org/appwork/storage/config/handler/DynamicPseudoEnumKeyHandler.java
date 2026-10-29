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

import org.appwork.exceptions.WTFException;
import org.appwork.storage.Storage;
import org.appwork.storage.config.annotations.DefaultEnumValue;

/**
 * @author Thomas
 *
 */
public class DynamicPseudoEnumKeyHandler<T extends AbstractDynamicPseudoEnum<T>> extends KeyHandler<T> {
    /**
     * @param storageHandler
     * @param key
     */
    public DynamicPseudoEnumKeyHandler(final StorageHandler<?> storageHandler, final String key) {
        super(storageHandler, key);
    }

    @Override
    protected Class<? extends Annotation> getDefaultAnnotation() {
        return DefaultEnumValue.class;
    }

    /**
     * @see org.appwork.storage.config.handler.KeyHandler#getValue()
     */
    @Override
    public T getValue() {
        return super.getValue();
    }

    @Override
    @SuppressWarnings("rawtypes")
    public T getDefaultValue() {
        if (getDefaultFactoryAnnotation() != null) {
            // dynamic
            return readDefaultValue();
        } else {
            return accessDefaultValue();
        }
    }

    @Override
    protected boolean equals(Object x, Object y) {
        if (x == null != (y == null)) {
            return false;
        } else if (x == y) {
            return true;
        } else {
            return x.equals(y);
        }
    }

    protected T readDefaultValue() {
        try {
            final T ret = values()[0];
            return ret;
        } catch (final Throwable e) {
            throw new RuntimeException(e);
        }
    }

    protected T referenceInstance = null;

    protected T getReferenceInstance() {
        final T ret = referenceInstance;
        if (ret != null) {
            return ret;
        }
        try {
            return referenceInstance = (T) (AbstractDynamicPseudoEnum) getRawClass().newInstance();
        } catch (Exception e) {
            throw new WTFException(e);
        }
    }

    public T[] values() {
        try {
            final T[] ret = (T[]) getReferenceInstance().values();
            return ret;
        } catch (Exception e) {
            return null;
        }
    }

    public T valueOf(final String name) {
        return (T) getReferenceInstance().valueOf(name);
    }

    @Override
    protected void initDefaults() throws Throwable {
        final T defaultValue = readDefaultValue();
        setDefaultValue(defaultValue);
    }

    @Override
    protected void initHandler() throws Throwable {
        setStorageSyncMode(getDefaultStorageSyncMode());
    }

    protected T fromName(String name) {
        try {
            final T ret = valueOf(name);
            return ret;
        } catch (Exception e) {
            return null;
        }
    }

    protected String toName(T value) {
        if (value == null) {
            return null;
        }
        return value.name();
    }

    @Override
    protected Object getRawValueStorage() {
        final Storage storage = this.getStorageHandler().getPrimitiveStorage(this);
        if (storage == null) {
            return getDefaultValue();
        } else {
            if (storage.hasProperty(this.getKey())) {
                // primitiveSTorage contains a value. we do not need to calculate
                // the defaultvalue.
                return fromName(storage.get(this.getKey(), toName(accessDefaultValue())));
            }
            // we have no value yet. call the getDefaultMethod to calculate the
            // default value
            final String[] backwardsCompatibilityLookupKeys = getBackwardsCompatibilityLookupKeys();
            if (backwardsCompatibilityLookupKeys != null) {
                for (final String key : backwardsCompatibilityLookupKeys) {
                    if (storage.hasProperty(key)) {
                        final boolean apv = storage.isAutoPutValues();
                        try {
                            if (!apv) {
                                storage.setAutoPutValues(true);
                            }
                            return fromName(storage.get(this.getKey(), storage.get(key, toName(accessDefaultValue()))));
                        } finally {
                            if (!apv) {
                                storage.setAutoPutValues(apv);
                            }
                        }
                    }
                }
            }
            return fromName(storage.get(this.getKey(), toName(this.getDefaultValue()), isAllowWriteDefaultObjects()));
        }
    }

    @Override
    protected boolean isPrimitiveType() {
        return false;
    }

    @Override
    protected boolean isRealPrimitive() {
        return false;
    };

    @Override
    protected void putValue(final T object) {
        final Storage storage = this.storageHandler.getPrimitiveStorage(this);
        if (storage != null) {
            storage.put(this.getKey(), toName(object));
        }
    }

    @Override
    protected void validateValue(final T object) throws Throwable {
    }
}
