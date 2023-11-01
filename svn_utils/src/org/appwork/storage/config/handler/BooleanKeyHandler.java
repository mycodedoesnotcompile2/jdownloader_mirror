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
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.utils.StringUtils;

/**
 * @author Thomas
 *
 */
public class BooleanKeyHandler extends KeyHandler<Boolean> {

    /**
     * @param storageHandler
     * @param key
     */
    public BooleanKeyHandler(final StorageHandler<?> storageHandler, final String key) {
        super(storageHandler, key);
    }

    @Override
    protected Class<? extends Annotation> getDefaultAnnotation() {
        return DefaultBooleanValue.class;
    }

    @Override
    protected void initDefaults() throws Throwable {
        this.setDefaultValue(Boolean.FALSE);
    }

    @Override
    protected void initHandler() {
        setStorageSyncMode(getDefaultStorageSyncMode());
    }

    public boolean isEnabled() {
        final Boolean value = this.getValue();
        return value != null && value.booleanValue();
    }

    @Override
    protected void putValue(final Boolean object) {
        final Storage storage = this.storageHandler.getPrimitiveStorage(this);
        if (storage != null) {
            storage.put(this.getKey(), object);
        }
    }

    public boolean toggle() {
        final boolean n = !this.isEnabled();
        this.setValue(n);
        return n;
    }

    @Override
    protected boolean equals(Object x, Object y) {
        if (x == y) {
            return true;
        } else if (x instanceof Boolean && y instanceof Boolean) {
            return x.equals(y);
        } else {
            return false;
        }
    }

    @Override
    protected Boolean getValueStorage() {
        final Object rawValue = getRawValueStorage();
        if (rawValue instanceof Boolean) {
            return (Boolean) rawValue;
        } else if (rawValue instanceof String) {
            final String stringValue = (String) rawValue;
            if (StringUtils.equalsIgnoreCase("true", stringValue) || StringUtils.equals("1", stringValue)) {
                return Boolean.TRUE;
            } else if (StringUtils.equalsIgnoreCase("false", stringValue) || StringUtils.equals("0", stringValue)) {
                return Boolean.FALSE;
            } else if (StringUtils.equalsIgnoreCase("null", stringValue)) {
                if (isRealPrimitive()) {
                    return Boolean.FALSE;
                } else {
                    return null;
                }
            }
        }
        return (Boolean) rawValue;
    }

    @Override
    protected void validateValue(final Boolean object) throws Throwable {
    }

}
