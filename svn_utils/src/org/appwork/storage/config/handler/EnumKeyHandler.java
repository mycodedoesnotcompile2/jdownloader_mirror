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
import java.util.ArrayList;
import java.util.List;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.Storage;
import org.appwork.storage.TypeRef;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DefaultFactory;
import org.appwork.storage.config.annotations.DefaultJsonObject;
import org.appwork.storage.config.defaults.AbstractDefaultFactory;

/**
 * @author Thomas
 *
 */
public class EnumKeyHandler extends KeyHandler<Enum> {
    /**
     * @param storageHandler
     * @param key
     */
    public EnumKeyHandler(final StorageHandler<?> storageHandler, final String key) {
        super(storageHandler, key);
    }

    @Override
    protected Class<? extends Annotation> getDefaultAnnotation() {
        return DefaultEnumValue.class;
    }

    @Override
    @SuppressWarnings("rawtypes")
    public Enum getDefaultValue() {
        if (getDefaultFactoryAnnotation() != null) {
            // dynamic
            return readDefaultValue();
        } else {
            return accessDefaultValue();
        }
    }

    @Override
    protected boolean equals(Object x, Object y) {
        if (x == y) {
            return true;
        } else if (x instanceof Enum && y instanceof Enum) {
            return x.equals(y);
        } else {
            return false;
        }
    }

    protected Enum readDefaultValue() {
        try {
            final DefaultFactory df = getDefaultFactoryAnnotation();
            if (df != null) {
                // dynamic
                final AbstractDefaultFactory<Enum> defaultFactory = (AbstractDefaultFactory<Enum>) df.value().newInstance();
                Enum defaultValue = defaultFactory.getDefaultValue(this);
                defaultValue = (Enum) storageHandler.runDefaultValueFactory(this, defaultValue);
                defaultValue = applyCustomValueGetter(defaultValue);
                return defaultValue;
            } else if (isFactoryDefaultValueSet()) {
                final Enum defaultValue = accessDefaultValue();
                return defaultValue;
            }
            final DefaultJsonObject defaultJson = this.getAnnotation(DefaultJsonObject.class);
            if (defaultJson != null) {
                // static
                Enum defaultValue = JSonStorage.restoreFromString(defaultJson.value(), new TypeRef<Enum>(this.getRawClass()) {
                }, null);
                defaultValue = (Enum) storageHandler.runDefaultValueFactory(this, defaultValue);
                defaultValue = applyCustomValueGetter(defaultValue);
                setFactoryDefaultValue(defaultValue);
                return defaultValue;
            }
            final DefaultEnumValue ann = this.getAnnotation(DefaultEnumValue.class);
            if (ann != null) {
                // static
                try {
                    Enum defaultValue = Enum.valueOf(this.getRawClass(), ann.value());
                    defaultValue = (Enum) storageHandler.runDefaultValueFactory(this, defaultValue);
                    defaultValue = applyCustomValueGetter(defaultValue);
                    setFactoryDefaultValue(defaultValue);
                    return defaultValue;
                } catch (Throwable e) {
                    e.printStackTrace();
                }
            }
            // static
            Enum defaultValue = this.getRawClass().getEnumConstants()[0];
            defaultValue = (Enum) storageHandler.runDefaultValueFactory(this, defaultValue);
            defaultValue = applyCustomValueGetter(defaultValue);
            setFactoryDefaultValue(defaultValue);
            return defaultValue;
        } catch (final Throwable e) {
            throw new RuntimeException(e);
        }
    }

    public Enum[] values() {
        final List<Enum> ret = new ArrayList<Enum>();
        for (final Object e : getRawClass().getEnumConstants()) {
            ret.add((Enum) e);
        }
        return ret.toArray(new Enum[0]);
    }

    @Override
    protected void initDefaults() throws Throwable {
        final Enum defaultValue = readDefaultValue();
        setDefaultValue(defaultValue);
    }

    @Override
    protected void initHandler() throws Throwable {
        setStorageSyncMode(getDefaultStorageSyncMode());
    }

    @Override
    protected void putValue(final Enum object) {
        final Storage storage = this.storageHandler.getPrimitiveStorage(this);
        if (storage != null) {
            storage.put(this.getKey(), object);
        }
    }

    @Override
    protected void validateValue(final Enum object) throws Throwable {
    }
}
