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

import java.io.File;
import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.List;

import org.appwork.remoteapi.annotations.AllowNonStorableObjects;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.Storage;
import org.appwork.storage.TypeRef;
import org.appwork.storage.config.ConfigInterface;
import org.appwork.storage.config.InterfaceParseException;
import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.AbstractCustomValueGetter;
import org.appwork.storage.config.annotations.AbstractValidator;
import org.appwork.storage.config.annotations.ConfigEntryKeywords;
import org.appwork.storage.config.annotations.CryptedStorage;
import org.appwork.storage.config.annotations.CustomValueGetter;
import org.appwork.storage.config.annotations.DefaultFactory;
import org.appwork.storage.config.annotations.DefaultJsonObject;
import org.appwork.storage.config.annotations.DefaultOnNull;
import org.appwork.storage.config.annotations.DefaultStorageSyncMode;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.DevConfig;
import org.appwork.storage.config.annotations.HexColorString;
import org.appwork.storage.config.annotations.LookUpKeys;
import org.appwork.storage.config.annotations.NoHeadless;
import org.appwork.storage.config.annotations.PlainStorage;
import org.appwork.storage.config.annotations.RequiresRestart;
import org.appwork.storage.config.annotations.ValidatorFactory;
import org.appwork.storage.config.defaults.AbstractDefaultFactory;
import org.appwork.storage.config.events.ConfigEvent;
import org.appwork.storage.config.events.ConfigEvent.Types;
import org.appwork.storage.config.events.ConfigEventSender;
import org.appwork.storage.simplejson.mapper.ClassCache;
import org.appwork.utils.IO;
import org.appwork.utils.IO.SYNC;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.reflection.Clazz;

/**
 * @author thomas
 *
 */
public abstract class KeyHandler<RawClass> {
    private final static Class<?>[]               OK_FOR_ALL              = new Class<?>[] { DefaultOnNull.class, HexColorString.class, DefaultStorageSyncMode.class, CustomValueGetter.class, ValidatorFactory.class, DefaultJsonObject.class, DefaultFactory.class, AboutConfig.class, NoHeadless.class, DevConfig.class, RequiresRestart.class, AllowNonStorableObjects.class, DescriptionForConfigEntry.class, ConfigEntryKeywords.class, CryptedStorage.class, PlainStorage.class };
    private static final String                   ANNOTATION_PACKAGE_NAME = CryptedStorage.class.getPackage().getName();
    private static final String                   PACKAGE_NAME            = PlainStorage.class.getPackage().getName();
    private final String                          key;
    protected Method                              getMethod               = null;
    protected Method                              setMethod               = null;
    protected final StorageHandler<?>             storageHandler;
    protected RawClass                            defaultValue;
    protected boolean                             factoryDefaultValueSet  = false;
    private ConfigEventSender<RawClass>           eventSender;
    private AbstractValidator<RawClass>           validatorFactory;
    protected AbstractCustomValueGetter<RawClass> customValueGetter;
    protected String[]                            backwardsCompatibilityLookupKeys;

    public String[] getBackwardsCompatibilityLookupKeys() {
        return backwardsCompatibilityLookupKeys;
    }

    private boolean   defaultOnNull               = false;
    private IO.SYNC   storageSyncMode             = IO.SYNC.NONE;
    protected Boolean hasDefaultFactoryAnnotation = null;

    public IO.SYNC getStorageSyncMode() {
        return storageSyncMode;
    }

    public void setStorageSyncMode(IO.SYNC storageSyncMode) {
        if (storageSyncMode == null) {
            this.storageSyncMode = SYNC.NONE;
        } else {
            this.storageSyncMode = storageSyncMode;
        }
        updateStorageSyncMode();
    }

    /**
     * @param storageHandler
     * @param key2
     */
    protected KeyHandler(final StorageHandler<?> storageHandler, final String key) {
        this.storageHandler = storageHandler;
        this.key = key;
    }

    protected void updateStorageSyncMode() {
        final StorageHandler<?> storageHandler = getStorageHandler();
        final Storage storage;
        if (storageHandler != null && (storage = storageHandler.getPrimitiveStorage(this)) != null) {
            final SYNC wish = getStorageSyncMode();
            if (wish != null) {
                SYNC is = storage.getStorageSyncMode();
                if (is == null) {
                    is = SYNC.NONE;
                }
                if (wish.ordinal() > is.ordinal()) {
                    storage.setStorageSyncMode(wish);
                }
            }
        }
    }

    public byte[] getCryptKey() {
        final Storage storage = getStorageHandler().getPrimitiveStorage(this);
        return storage != null ? storage.getCryptKey() : null;
    }

    protected boolean isDefaultOnNull() {
        return defaultOnNull || getRawClass().isPrimitive();
    }

    public File getPath() {
        return null;
    }

    protected void checkBadAnnotations(final Class<? extends Annotation>... class1) {
        int checker = 0;
        if (this.getAnnotation(this.getDefaultAnnotation()) != null) {
            checker++;
        }
        if (this.getAnnotation(DefaultJsonObject.class) != null) {
            checker++;
        }
        if (this.getAnnotation(DefaultFactory.class) != null) {
            checker++;
        }
        if (checker > 1) {
            throw new InterfaceParseException("Make sure that you use only one  of getDefaultAnnotation,DefaultObjectValue or DefaultValue ");
        } else {
            if (getMethod != null) {
                this.checkBadAnnotations(getMethod, class1);
            }
            if (setMethod != null) {
                this.checkBadAnnotations(setMethod, class1);
            }
        }
    }

    /**
     * @param m
     * @param class1
     */
    private void checkBadAnnotations(final Method m, final Class<? extends Annotation>... classes) {
        /**
         * This main mark is important!!
         */
        main: for (final Annotation a : m.getAnnotations()) {
            // all other Annotations are ok anyway
            if (a == null) {
                continue;
            } else if (!a.annotationType().getName().startsWith(KeyHandler.PACKAGE_NAME)) {
                continue;
            } else if (this.getDefaultAnnotation() != null && this.getDefaultAnnotation().isAssignableFrom(a.getClass())) {
                continue;
            } else {
                int index = 0;
                for (index = 0; index < OK_FOR_ALL.length; index++) {
                    if (OK_FOR_ALL[index].isAssignableFrom(a.getClass())) {
                        continue main;
                    }
                }
                for (index = 0; index < classes.length; index++) {
                    if (classes[index].isAssignableFrom(a.getClass())) {
                        continue main;
                    }
                }
                throw new InterfaceParseException("Bad Annotation: " + a + " for " + m);
            }
        }
    }

    /**
     * @param valueUpdated
     * @param keyHandler
     * @param object
     */
    protected void fireEvent(final Types type, final KeyHandler<?> keyHandler, final Object parameter) {
        this.storageHandler.fireEvent(type, keyHandler, parameter);
        if (this.hasEventListener()) {
            this.getEventSender().fireEvent(new ConfigEvent(type, this, parameter));
        }
    }

    @SuppressWarnings("unchecked")
    protected Class<? extends Annotation>[] getAllowedAnnotations() {
        return (Class<? extends Annotation>[]) new Class<?>[] { LookUpKeys.class };
    }

    /**
     * @param <T>
     * @param class1
     * @return
     */
    public <T extends Annotation> T getAnnotation(final Class<T> annotation) {
        if (annotation == null) {
            return null;
        } else {
            T ret = getMethod.getAnnotation(annotation);
            if (ret == null && setMethod != null) {
                ret = setMethod.getAnnotation(annotation);
            }
            if (ret == null) {
                try {
                    final List<T> annotations = ClassCache.getClassCache(getDeclaringClass()).getMethodAnnotations(getMethod, annotation);
                    if (annotations.size() > 0) {
                        ret = annotations.get(0);
                    } else {
                        return null;
                    }
                } catch (Throwable e) {
                    e.printStackTrace();
                }
            }
            return ret;
        }
    }

    /**
     * @return
     */
    public Class<?> getDeclaringClass() {
        if (this.getMethod != null) {
            return this.getMethod.getDeclaringClass();
        } else {
            return setMethod.getDeclaringClass();
        }
    }

    protected Class<? extends Annotation> getDefaultAnnotation() {
        return null;
    }

    public boolean hasDefaultValue() {
        try {
            final DefaultFactory df = getDefaultFactoryAnnotation();
            if (df != null) {
                return true;
            }
            final DefaultJsonObject defaultJson = this.getAnnotation(DefaultJsonObject.class);
            if (defaultJson != null && defaultJson.value() != null) {
                return true;
            }
            final Annotation ann = this.getAnnotation(this.getDefaultAnnotation());
            if (ann != null) {
                return true;
            }
            return false;
        } catch (final Throwable e) {
            throw new RuntimeException(e);
        }
    }

    public SYNC getDefaultStorageSyncMode() {
        try {
            final DefaultStorageSyncMode df = this.getAnnotation(DefaultStorageSyncMode.class);
            if (df != null) {
                return df.value();
            }
        } catch (final Throwable e) {
            throw new RuntimeException(e);
        }
        return SYNC.NONE;
    }

    protected DefaultFactory getDefaultFactoryAnnotation() {
        final DefaultFactory df = !Boolean.FALSE.equals(hasDefaultFactoryAnnotation) ? this.getAnnotation(DefaultFactory.class) : null;
        hasDefaultFactoryAnnotation = df != null;
        return df;
    }

    @SuppressWarnings("unchecked")
    public RawClass getDefaultValue() {
        try {
            final DefaultFactory df = getDefaultFactoryAnnotation();
            if (df != null) {
                // dynamic
                final AbstractDefaultFactory<RawClass> defaultFactory = (AbstractDefaultFactory<RawClass>) df.value().newInstance();
                RawClass defaultValue = defaultFactory.getDefaultValue(this);
                defaultValue = (RawClass) storageHandler.runDefaultValueFactory(this, defaultValue);
                defaultValue = applyCustomValueGetter(defaultValue);
                return defaultValue;
            } else if (isFactoryDefaultValueSet()) {
                final RawClass defaultValue = accessDefaultValue();
                return defaultValue;
            }
            final DefaultJsonObject defaultJson = this.getAnnotation(DefaultJsonObject.class);
            if (defaultJson != null) {
                // static
                RawClass defaultValue = JSonStorage.restoreFromString(defaultJson.value(), getTypeRef(), null);
                defaultValue = (RawClass) storageHandler.runDefaultValueFactory(this, defaultValue);
                defaultValue = applyCustomValueGetter(defaultValue);
                setFactoryDefaultValue(defaultValue);
                return defaultValue;
            }
            final Annotation ann = this.getAnnotation(this.getDefaultAnnotation());
            if (ann != null) {
                // static
                final Object defaultValueObject = ann.annotationType().getMethod("value", new Class[] {}).invoke(ann, new Object[] {});
                RawClass defaultValue = fixValueType(defaultValueObject);
                defaultValue = (RawClass) storageHandler.runDefaultValueFactory(this, defaultValue);
                defaultValue = applyCustomValueGetter(defaultValue);
                setFactoryDefaultValue(defaultValue);
                return defaultValue;
            }
            RawClass defaultValue = accessDefaultValue();
            defaultValue = (RawClass) storageHandler.runDefaultValueFactory(this, defaultValue);
            defaultValue = applyCustomValueGetter(defaultValue);
            setFactoryDefaultValue(defaultValue);
            return defaultValue;
        } catch (final Throwable e) {
            throw new RuntimeException(e);
        }
    }

    public TypeRef<RawClass> getTypeRef() {
        return new TypeRef<RawClass>(this.getRawType()) {
        };
    }

    protected RawClass fixValueType(final Object value) {
        if (value == null) {
            return null;
        } else if (value != null && !value.getClass().equals(getRawType())) {
            if (Clazz.isPrimitive(value.getClass()) && Clazz.isPrimitive(getRawType())) {
                // let JVM auto box/unbox
                return (RawClass) value;
            } else {
                // fix type via Object->JSON->Object conversion
                final byte[] jsonBytes = JSonStorage.serializeToJsonByteArray(value);
                return JSonStorage.restoreFromByteArray(jsonBytes, getTypeRef());
            }
        } else {
            return (RawClass) value;
        }
    }

    protected RawClass applyCustomValueGetter(final RawClass value) {
        if (this.customValueGetter != null) {
            return this.customValueGetter.getValue(this, value);
        } else {
            return value;
        }
    }

    /**
     * Lazy initialiser of the eventsender. we do not wnat to create an eventsender if nowbody uses it
     *
     * @return
     */
    public synchronized ConfigEventSender<RawClass> getEventSender() {
        if (this.eventSender == null) {
            this.eventSender = new ConfigEventSender<RawClass>();
        }
        return this.eventSender;
    }

    public Method getGetMethod() {
        return getMethod;
    }

    public String getKey() {
        return this.key;
    }

    /**
     * @return
     */
    @SuppressWarnings("unchecked")
    public Class<RawClass> getRawClass() {
        final Class<RawClass> ret;
        if (getMethod != null) {
            ret = (Class<RawClass>) getMethod.getReturnType();
        } else {
            ret = (Class<RawClass>) setMethod.getParameterTypes()[0];
        }
        return ret;
    }

    /**
     * @return
     */
    public Type getRawType() {
        if (this.getMethod != null) {
            return getMethod.getGenericReturnType();
        } else {
            return setMethod.getGenericParameterTypes()[0];
        }
    }

    public Method getSetMethod() {
        return setMethod;
    }

    public StorageHandler<?> getStorageHandler() {
        return this.storageHandler;
    }

    public static enum AbstractTypeDefinition {
        BOOLEAN,
        INT,
        LONG,
        STRING,
        OBJECT,
        OBJECT_LIST,
        STRING_LIST,
        ENUM,
        BYTE,
        CHAR,
        DOUBLE,
        FLOAT,
        SHORT,
        BOOLEAN_LIST,
        BYTE_LIST,
        SHORT_LIST,
        LONG_LIST,
        INT_LIST,
        FLOAT_LIST,
        ENUM_LIST,
        DOUBLE_LIST,
        CHAR_LIST,
        UNKNOWN,
        HEX_COLOR,
        HEX_COLOR_LIST;
    }

    public AbstractTypeDefinition getAbstractType() {
        final Type ret = getRawType();
        if (ret instanceof Class) {
            final Class<?> clazz = (Class<?>) ret;
            if (Clazz.isBoolean(ret)) {
                return AbstractTypeDefinition.BOOLEAN;
            } else if (Clazz.isByte(ret)) {
                return AbstractTypeDefinition.BYTE;
            } else if (Clazz.isCharacter(ret)) {
                return AbstractTypeDefinition.CHAR;
            } else if (Clazz.isDouble(ret)) {
                return AbstractTypeDefinition.DOUBLE;
            } else if (Clazz.isEnum(ret)) {
                return AbstractTypeDefinition.ENUM;
            } else if (Clazz.isFloat(ret)) {
                return AbstractTypeDefinition.FLOAT;
            } else if (Clazz.isInteger(ret)) {
                return AbstractTypeDefinition.INT;
            } else if (Clazz.isLong(ret)) {
                return AbstractTypeDefinition.LONG;
            } else if (Clazz.isShort(ret)) {
                return AbstractTypeDefinition.SHORT;
            } else if (Clazz.isString(ret)) {
                if (this.getAnnotation(HexColorString.class) != null) {
                    return AbstractTypeDefinition.HEX_COLOR;
                }
                return AbstractTypeDefinition.STRING;
            } else if (clazz.isArray()) {
                final Class aType = ((Class) ret).getComponentType();
                if (Clazz.isBoolean(aType)) {
                    return AbstractTypeDefinition.BOOLEAN_LIST;
                } else if (Clazz.isByte(aType)) {
                    return AbstractTypeDefinition.BYTE_LIST;
                } else if (Clazz.isCharacter(aType)) {
                    return AbstractTypeDefinition.CHAR_LIST;
                } else if (Clazz.isDouble(aType)) {
                    return AbstractTypeDefinition.DOUBLE_LIST;
                } else if (Clazz.isEnum(aType)) {
                    return AbstractTypeDefinition.ENUM_LIST;
                } else if (Clazz.isFloat(aType)) {
                    return AbstractTypeDefinition.FLOAT_LIST;
                } else if (Clazz.isInteger(aType)) {
                    return AbstractTypeDefinition.INT_LIST;
                } else if (Clazz.isLong(aType)) {
                    return AbstractTypeDefinition.LONG_LIST;
                } else if (Clazz.isShort(aType)) {
                    return AbstractTypeDefinition.SHORT_LIST;
                } else if (Clazz.isString(aType)) {
                    if (this.getAnnotation(HexColorString.class) != null) {
                        return AbstractTypeDefinition.HEX_COLOR_LIST;
                    }
                    return AbstractTypeDefinition.STRING_LIST;
                } else {
                    return AbstractTypeDefinition.OBJECT_LIST;
                }
            }
            // if(ret instanceof List){
            // return AbstractType.OBJECT_LIST;
            // }
        } else {
            if (ret instanceof ParameterizedType) {
                final Type raw = ((ParameterizedType) ret).getRawType();
                final Type[] acutal = ((ParameterizedType) ret).getActualTypeArguments();
                if (raw instanceof Class) {
                    final Class<?> rawClazz = (Class<?>) raw;
                    if (List.class.isAssignableFrom(rawClazz)) {
                        if (Clazz.isBoolean(acutal[0])) {
                            return AbstractTypeDefinition.BOOLEAN_LIST;
                        } else if (Clazz.isByte(acutal[0])) {
                            return AbstractTypeDefinition.BYTE_LIST;
                        } else if (Clazz.isCharacter(acutal[0])) {
                            return AbstractTypeDefinition.CHAR_LIST;
                        } else if (Clazz.isDouble(acutal[0])) {
                            return AbstractTypeDefinition.DOUBLE_LIST;
                        } else if (Clazz.isEnum(acutal[0])) {
                            return AbstractTypeDefinition.ENUM_LIST;
                        } else if (Clazz.isFloat(acutal[0])) {
                            return AbstractTypeDefinition.FLOAT_LIST;
                        } else if (Clazz.isInteger(acutal[0])) {
                            return AbstractTypeDefinition.INT_LIST;
                        } else if (Clazz.isLong(acutal[0])) {
                            return AbstractTypeDefinition.LONG_LIST;
                        } else if (Clazz.isShort(acutal[0])) {
                            return AbstractTypeDefinition.SHORT_LIST;
                        } else if (Clazz.isString(acutal[0])) {
                            if (this.getAnnotation(HexColorString.class) != null) {
                                return AbstractTypeDefinition.HEX_COLOR_LIST;
                            } else {
                                return AbstractTypeDefinition.STRING_LIST;
                            }
                        } else {
                            return AbstractTypeDefinition.OBJECT_LIST;
                        }
                    }
                } else {
                    return AbstractTypeDefinition.UNKNOWN;
                }
            } else {
                return AbstractTypeDefinition.UNKNOWN;
            }
        }
        return AbstractTypeDefinition.OBJECT;
    }

    public String getTypeString() {
        final Type ret = getRawType();
        if (ret instanceof Class) {
            return ((Class<?>) ret).getName();
        } else {
            return ret.toString();
        }
    }

    public RawClass getValue() {
        synchronized (this) {
            RawClass value = this.getValueStorage();
            if (value == null && isDefaultOnNull()) {
                value = getDefaultValue();
            }
            value = applyCustomValueGetter(value);
            return value;
        }
    }

    protected RawClass getValueStorage() {
        final Object rawValue = getRawValueStorage();
        return (RawClass) rawValue;
    }

    protected Object getRawValueStorage() {
        final Storage storage = this.getStorageHandler().getPrimitiveStorage(this);
        if (storage == null) {
            return getDefaultValue();
        } else {
            if (storage.hasProperty(this.getKey())) {
                // primitiveSTorage contains a value. we do not need to calculate
                // the defaultvalue.
                return storage.get(this.getKey(), accessDefaultValue());
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
                            return storage.get(this.getKey(), storage.get(key, accessDefaultValue()));
                        } finally {
                            if (!apv) {
                                storage.setAutoPutValues(apv);
                            }
                        }
                    }
                }
            }
            return storage.get(this.getKey(), this.getDefaultValue(), isAllowWriteDefaultObjects());
        }
    }

    protected RawClass accessDefaultValue() {
        return defaultValue;
    }

    private boolean allowWriteDefaultObjects = true;

    public boolean isAllowWriteDefaultObjects() {
        return allowWriteDefaultObjects;
    }

    public void setAllowWriteDefaultObjects(boolean allowWriteDefaultObjects) {
        this.allowWriteDefaultObjects = allowWriteDefaultObjects;
    }

    public synchronized boolean hasEventListener() {
        return this.eventSender != null && this.eventSender.hasListener();
    }

    /**
     * @throws Throwable
     *
     */
    @SuppressWarnings("unchecked")
    protected void init() throws Throwable {
        if (getMethod == null) {
            throw new InterfaceParseException("Getter Method is Missing for " + setMethod);
        }
        // read local cryptinfos
        final CryptedStorage cryptedStorage = this.getAnnotation(CryptedStorage.class);
        if (cryptedStorage != null) {
            final Storage storage = this.storageHandler.getPrimitiveStorage(this);
            if (storage != null && storage.getCryptKey() != null) {
                //
                throw new InterfaceParseException("No reason to mark " + this + " as @CryptedStorage. Parent is already CryptedStorage");
            } else if (!(this instanceof ListHandler)) {
                //
                throw new InterfaceParseException(this + " Cannot set @CryptedStorage on primitive fields. Use an object, or an extra plain config interface");
            }
            this.validateEncryptionKey(cryptedStorage.key());
        }
        final PlainStorage plainStorage = this.getAnnotation(PlainStorage.class);
        if (plainStorage != null) {
            if (cryptedStorage != null) {
                //
                throw new InterfaceParseException("Cannot Set CryptStorage and PlainStorage Annotation in " + this);
            }
            final Storage storage = this.storageHandler.getPrimitiveStorage(this);
            if (storage != null && storage.getCryptKey() == null) {
                //
                throw new InterfaceParseException("No reason to mark " + this + " as @PlainStorage. Parent is already Plain");
            } else if (!(this instanceof ListHandler)) {
                //
                throw new InterfaceParseException(this + " Cannot set @PlainStorage on primitive fields. Use an object, or an extra plain config interface");
                // primitive storage. cannot set single plain values in a en
                // crypted
                // primitive storage
            }
            // parent crypted, but plain for this single entry
        }
        try {
            final ValidatorFactory anno = this.getAnnotation(ValidatorFactory.class);
            if (anno != null) {
                this.validatorFactory = (AbstractValidator<RawClass>) anno.value().newInstance();
            }
        } catch (final Throwable e) {
        }
        try {
            final CustomValueGetter anno = this.getAnnotation(CustomValueGetter.class);
            if (anno != null) {
                this.customValueGetter = (AbstractCustomValueGetter<RawClass>) anno.value().newInstance();
            }
        } catch (final Throwable e) {
        }
        try {
            this.defaultOnNull = this.getAnnotation(DefaultOnNull.class) != null;
        } catch (final Throwable e) {
        }
        this.checkBadAnnotations(this.getAllowedAnnotations());
        this.initDefaults();
        this.initHandler();
        final String kk = "CFG:" + this.storageHandler.getConfigInterface().getName() + "." + this.key;
        final String sys = System.getProperty(kk);
        if (sys != null) {
            // Set configvalud because of JVM Parameter
            this.setValue((RawClass) JSonStorage.restoreFromString(sys, new TypeRef<Object>(this.getRawClass()) {
            }, null));
        }
        final LookUpKeys lookups = this.getAnnotation(LookUpKeys.class);
        if (lookups != null) {
            this.backwardsCompatibilityLookupKeys = lookups.value();
        }
    }

    protected boolean isDelayedWriteAllowed() {
        return true;
    }

    protected void initDefaults() throws Throwable {
        this.defaultValue = null;
    }

    /**
     * @throws Throwable
     *
     */
    protected abstract void initHandler() throws Throwable;

    /**
     * returns true of this keyhandler belongs to ConfigInterface
     *
     * @param settings
     * @return
     */
    public boolean isChildOf(final ConfigInterface settings) {
        return settings._getStorageHandler() == this.getStorageHandler();
    }

    /**
     * @param m
     * @return
     */
    protected boolean isGetter(final Method m) {
        return m != null && m.equals(getMethod);
    }

    protected boolean isPrimitiveType() {
        return JSonStorage.canStorePrimitive(getMethod.getReturnType());
    }

    protected boolean isRealPrimitive() {
        return getRawClass().isPrimitive();
    }

    /**
     * @param object
     */
    protected abstract void putValue(RawClass value);

    protected void setDefaultValue(final RawClass value) {
        this.defaultValue = value;
    }

    protected void setFactoryDefaultValue(final RawClass value) {
        this.defaultValue = value;
        factoryDefaultValueSet = true;
    }

    protected boolean isFactoryDefaultValueSet() {
        return factoryDefaultValueSet;
    }

    protected void resetFactoryDefaultValue() {
        this.factoryDefaultValueSet = false;
    }

    /**
     * @param h
     */
    protected void setGetMethod(final Method method) {
        this.getMethod = method;
    }

    /**
     * @param h
     */
    protected void setSetMethod(final Method method) {
        this.setMethod = method;
    }

    /**
     * this method compare newValue and getValue and returns false if newValue is different from getValue
     *
     * @param newValue
     * @return
     */
    protected boolean setValueEqualsGetValue(final RawClass newValue) {
        final RawClass oldValue = this.getValue();
        return equals(newValue, oldValue);
    }

    public void setValueAutoConvert(final Object newValue) throws ValidationException {
        final Class<RawClass> rawClass = getRawClass();
        if (isAutoValueConvertSupported(newValue, rawClass)) {
            final RawClass convertedNewValue = autoConvertValue(newValue, rawClass);
            setValue(convertedNewValue);
        } else {
            setValue((RawClass) newValue);
        }
    }

    protected boolean isAutoValueConvertSupported(Object newValue, Class<?> destClass) {
        return Clazz.isPrimitive(destClass);
    }

    protected <T> T autoConvertValue(Object newValue, Class<T> destClass) {
        if (Clazz.isPrimitive(destClass)) {
            final T ret = (T) ReflectionUtils.cast(newValue, destClass);
            return ret;
        } else {
            return null;
        }
    }

    /**
     * @param newValue
     */
    public void setValue(final RawClass newValue) throws ValidationException {
        try {
            synchronized (this) {
                if (setValueEqualsGetValue(newValue)) {
                    return;
                } else {
                    if (this.validatorFactory != null) {
                        this.validatorFactory.validate(this, newValue);
                    }
                    this.validateValue(newValue);
                    this.putValue(newValue);
                    getStorageHandler().requestSave();
                }
            }
            this.fireEvent(ConfigEvent.Types.VALUE_UPDATED, this, newValue);
        } catch (final ValidationException e) {
            e.setValue(newValue);
            this.fireEvent(ConfigEvent.Types.VALIDATOR_ERROR, this, e);
            throw e;
        } catch (final Throwable t) {
            final ValidationException e = new ValidationException(t);
            e.setValue(newValue);
            this.fireEvent(ConfigEvent.Types.VALIDATOR_ERROR, this, e);
            throw e;
        }
    }

    protected boolean equals(final Object x, final Object y) {
        if (x == null) {
            if (y != null) {
                return false;
            } else {
                return true;
            }
        } else if (y == null) {
            return false;
        } else {
            return false;
        }
    }

    @Override
    public String toString() {
        final RawClass ret = this.getValue();
        return ret == null ? null : ret.toString();
    }

    public void validateEncryptionKey(final byte[] key2) {
        if (key2 == null) {
            throw new InterfaceParseException("Key missing in " + this);
        } else if (key2.length != JSonStorage.KEY.length) {
            throw new InterfaceParseException("Crypt key for " + this + " is invalid. required length: " + JSonStorage.KEY.length);
        }
    }

    /**
     * @param object
     */
    protected abstract void validateValue(RawClass object) throws Throwable;

    /**
     * @return
     */
    public String getReadableName() {
        String getterName = getGetMethod().getName();
        if (getterName.startsWith("is")) {
            getterName = getterName.substring(2);
        } else if (getterName.startsWith("get")) {
            getterName = getterName.substring(3);
        }
        getterName = getterName.replaceAll("([a-z])([A-Z])", "$1 $2");
        getterName = getterName.replaceAll("(\\D)(\\d+)", "$1 $2");
        getterName = getterName.replaceAll("(\\d+)(\\D)", "$1 $2");
        getterName = getterName.replaceAll("(\\S)([A-Z][a-z])", "$1 $2");
        getterName = getterName.replaceAll("I P", "IP");// special case for IP
        getterName = getterName.replaceAll("(\\d{3,4}) ?(p|P)", "$1$2");// special case for 1080p
        getterName = getterName.replaceAll("(^| )(2|4|5|6|8|12) ?(k|K)($| )", "$1$2$3$4");// special case for 2k,4k,8k
        getterName = getterName.replaceAll("(?i)(MP) ?(3|4)", "$1$2");// special case for MP3/MP4
        if (getterName.endsWith(" Enabled")) {
            getterName = getterName.substring(0, getterName.length() - 8);
        }
        return getterName;
    }
}
