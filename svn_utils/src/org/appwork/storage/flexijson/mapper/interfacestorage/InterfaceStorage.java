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
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.storage.flexijson.mapper.interfacestorage;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.storage.SimpleTypeRef;
import org.appwork.storage.Storable;
import org.appwork.storage.TypeRef;
import org.appwork.storage.flexijson.FlexiJSONParser;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.FlexiJSonObject;
import org.appwork.storage.flexijson.FlexiParserException;
import org.appwork.storage.flexijson.FlexiUtils;
import org.appwork.storage.flexijson.JSPath;
import org.appwork.storage.flexijson.KeyValueElement;
import org.appwork.storage.flexijson.mapper.FlexiJSonMapper;
import org.appwork.storage.flexijson.mapper.FlexiMapperException;
import org.appwork.storage.flexijson.stringify.FlexiJSonPrettyStringify;
import org.appwork.storage.flexijson.utils.FlexiWalker;
import org.appwork.storage.simplejson.mapper.ClassCache;
import org.appwork.storage.simplejson.mapper.Getter;
import org.appwork.storage.simplejson.mapper.Property;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.DebugMode;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.event.basic.CoreDelegate;
import org.appwork.utils.event.basic.CoreEventSender;
import org.appwork.utils.reflection.Clazz;
import org.appwork.utils.reflection.CompiledType;

/**
 * @author thomas
 * @date 24.07.2022
 *
 */
public class InterfaceStorage<InterfaceType> implements InvocationHandler {
    public FlexiJSonObject backendNode;

    public FlexiJSonObject getBackendNode() {
        return backendNode;
    }

    public void setBackendNode(FlexiJSonObject target) {
        FlexiJSonObject old = backendNode;
        this.backendNode = target;
        if (old != null) {
            if (old.getParent() != null) {
                DebugMode.debugger("Check Me");
            }
        }
    }

    public final CompiledType                                        cType;
    public final FlexiJSonMapper                                     mapper;
    private Map<String, Object>                                      cache;
    private CoreEventSender<InterfaceStorageListener<InterfaceType>> eventSender;

    /**
     * @param mapper
     * @param cc
     * @param cType
     * @param obj
     */
    public InterfaceStorage(FlexiJSonMapper mapper, CompiledType cType, FlexiJSonObject obj) {
        backendNode = obj;
        this.cType = cType;
        this.mapper = mapper;
        cache = new HashMap<String, Object>();
    }

    public CoreEventSender<InterfaceStorageListener<InterfaceType>> getEventSender() {
        synchronized (this) {
            if (eventSender == null) {
                eventSender = new CoreEventSender<InterfaceStorageListener<InterfaceType>>();
            }
            return eventSender;
        }
    }

    @Override
    public Object invoke(Object proxy, Method method, final Object[] args) throws Throwable {
        if (proxy == null) {
            proxy = this.storage;
        }
        // storage may be null of we invoke this during the init - e.g. from within the mapper itself.
        if (this.storage != proxy && storage != null) {
            throw new WTFException();
        }
        final Class<?> returnType = method.getReturnType();
        final int parameterCount = ClassCache.getParameterCount(method);
        if ("toString".equals(method.getName()) && Clazz.isString(returnType) && parameterCount == 0) {
            return cType + "-proxy:\r\n" + new FlexiJSonPrettyStringify().toJSONString(new FlexiJSonMapper() {
                protected boolean isIncludeInterfaceStorageBackendNode() {
                    return false;
                };
            }.objectToJsonNode(proxy)) + "\r\n\r\nBackend Node:\r\n" + FlexiUtils.serializeToPrettyJson(backendNode);
        } else if ("hashCode".equals(method.getName()) && Clazz.isInteger(returnType) && parameterCount == 0) {
            return cType.hashCode();
        } else if ("equals".equals(method.getName()) && Clazz.isBoolean(returnType) && parameterCount == 1 && method.getParameterTypes()[0] == Object.class) {
            return equals(proxy, args);
        } else {
            final String key = ClassCache.createKey(method);
            if (parameterCount == 1 && returnType == void.class) {
                final Object oldValue;
                final Object newValue = args[0];
                final Property property = cType.getClassCache().getProperty(key);
                boolean requiresWrite = true;
                synchronized (this) {
                    oldValue = property.getter == null ? null : getWithUsingTheCache(property.getter, key);
                    if (oldValue == null && newValue == null) {
                        requiresWrite = false;
                    } else if ((oldValue == null && newValue != null) || (oldValue != null || newValue == null)) {
                        requiresWrite = true;
                    } else if (property.type.isPrimitive() || property.type.isEnum(true) || property.type.isString()) {
                        if (CompareUtils.equals(oldValue, newValue)) {
                            requiresWrite = false;
                        }
                    } else {
                        if (oldValue == newValue) {
                            // keep true - we cannot detect changes
                        } else if (property.type.isListContainer() || property.type.isMap()) {
                            if (property.type.getComponentType().isPrimitive() || property.type.getComponentType().isEnum(true) || property.type.getComponentType().isString()) {
                                if (CompareUtils.equalsDeep(oldValue, newValue)) {
                                    requiresWrite = false;
                                }
                            }
                        } else if (property.type.isInstanceOf(FlexiStorableInterface.class, Storable.class)) {
                            ClassCache cc = property.type.getClassCache();
                            boolean onlyPrimitiveLikeTypes = true;
                            for (String propKey : cc.getKeys()) {
                                CompiledType propType = CompiledType.create(cc.getType(propKey), property.type);
                                if (!(propType.isPrimitive() || propType.isEnum(true) || propType.isString())) {
                                    onlyPrimitiveLikeTypes = false;
                                    break;
                                }
                            }
                            if (onlyPrimitiveLikeTypes) {
                                if (CompareUtils.equalsDeep(oldValue, newValue)) {
                                    requiresWrite = false;
                                }
                            }
                        }
                    }
                    writeToCache(key, newValue);
                }
                if (requiresWrite) {
                    final CoreEventSender<InterfaceStorageListener<InterfaceType>> eventSender = this.eventSender;
                    if (eventSender != null) {
                        final Object fProxy = proxy;
                        eventSender.fireEvent(new CoreDelegate<InterfaceStorageListener<InterfaceType>>() {
                            @Override
                            protected void fireTo(InterfaceStorageListener<InterfaceType> listener) {
                                listener.onInterfaceValueSet((InterfaceType) fProxy, key, oldValue, newValue);
                            }
                        });
                    }
                    final PropertyHandler<InterfaceType, Object> propertyHandler = propertyHandlers.get(key);
                    if (propertyHandler != null) {
                        propertyHandler.fireEventSet(oldValue, newValue);
                    }
                    onSet(key, oldValue, newValue);
                }
                return null;
            } else if (parameterCount == 0 && returnType != void.class) {
                // getter
                synchronized (this) {
                    final Getter getter = cType.getClassCache().getGetter(key);
                    return getter == null ? null : getWithUsingTheCache(getter, key);
                }
            } else {
                throw new WTFException("Invalid method call:" + method);
            }
        }
    }

    /**
     * @param key
     * @param old
     * @param object
     */
    protected void onSet(String key, Object old, Object object) {        
    }

    private Object getWithUsingTheCache(final Getter getter, final String key) throws FlexiParserException, FlexiMapperException {
        final Object cachedValue = readCache(key);
        if (cachedValue != null) {
            if (cachedValue == NULL) {
                return null;
            } else {
                return cachedValue;
            }
        }
        KeyValueElement element = getElementByKey(key);
        if (element == null && getter != null) {
            final List<String> alts = getter.getAlternativeKeys();
            if (alts != null) {
                for (String altkey : alts) {
                    element = getElementByKey(altkey);
                    if (element != null) {
                        break;
                    }
                }
            }
        }
        final Object value;
        if (isReturnDefaultValue(element, getter.getMethod(), key)) {
            value = getDefaultValue(getter.getMethod(), key);
            writeToCache(key, value);
            // maybe event for default value resolved
            return value;
        } else {
            value = mapper.jsonToObject(element.getValue(), new SimpleTypeRef<Object>(getter.getMethod().getGenericReturnType()));
            writeToCache(key, value);
            return value;
        }
    }

    /**
     * @param element
     * @param method
     * @param key
     * @return
     */
    protected boolean isReturnDefaultValue(KeyValueElement element, Method method, String key) {
        return element == null;
    }

    protected KeyValueElement getElementByKey(final String key) {
        synchronized (this) {
            return getBackendNode().getElement(key);
        }
    }

    protected boolean equals(Object proxy, Object[] args) {
        Object compareTo = args[0];
        if (compareTo == null) {
            return false;
        } else if (!Proxy.isProxyClass(compareTo.getClass())) {
            return false;
        }
        InvocationHandler handler = Proxy.getInvocationHandler(compareTo);
        if (handler == null) {
            return false;
        } else if (handler == this) {
            return true;
        } else if (handler instanceof InterfaceStorage) {
            @SuppressWarnings("unchecked")
            InterfaceStorage<?> other = ((InterfaceStorage<Object>) handler);
            if (other.cType != this.cType) {
                return false;
            }
            boolean ret;
            try {
                FlexiJSonNode a = mapper.objectToJsonNode(proxy);
                FlexiJSonNode b = mapper.objectToJsonNode(compareTo);
                ret = a.equals(b);
            } catch (FlexiMapperException e) {
                return false;
            }
            return ret;
        }
        return false;
    }

    public synchronized void clearCache() {
        cache.clear();
    }

    protected synchronized Object readCache(String key) {
        return cache.get(key);
    }

    private final static Object NULL = new Object();

    protected synchronized void writeToCache(String key, Object ret) {
        if (ret == null) {
            cache.put(key, NULL);
        } else {
            cache.put(key, ret);
        }
    }

    private Object getDefaultValue(Method method, String key) throws FlexiParserException, FlexiMapperException {
        final List<FlexiInterfaceDefaultFactory> defaultFactoryAnnotations = cType.getClassCache().getAnnotations(key, FlexiInterfaceDefaultFactory.class);
        if (defaultFactoryAnnotations != null && defaultFactoryAnnotations.size() > 0) {
            // Default value from annotation factory
            try {
                final Class<? extends FlexiDefaultFactory> cls = defaultFactoryAnnotations.get(0).value();
                if (cls != null) {
                    return cls.newInstance().createDefault();
                }
            } catch (InstantiationException e) {
                throw new WTFException(e);
            } catch (IllegalAccessException e) {
                throw new WTFException(e);
            }
        }
        final List<FlexiInterfaceDefault> defaultAnnotations = cType.getClassCache().getAnnotations(key, FlexiInterfaceDefault.class);
        if (defaultAnnotations != null && defaultAnnotations.size() > 0) {
            // Default value from annotation
            // use creater from the mapper to ensure correct types (Example: ExtFlexiJson* in Admintool editor)
            final FlexiJSonNode defaultNode = new FlexiJSONParser(defaultAnnotations.get(0).value()) {
                protected org.appwork.storage.flexijson.FlexiJSonComments createCommentsContainer() {
                    return mapper.createFlexiJsonCommentsContainer();
                };

                protected org.appwork.storage.flexijson.FlexiCommentJsonNode createComment(String comment, org.appwork.storage.flexijson.FlexiComment.Type type, org.appwork.storage.flexijson.mapper.FlexiMapperTags tag, int startindex, int endIndex, Object path) {
                    return mapper.createFlexiJsonComment(comment, tag, type);
                };

                public org.appwork.storage.flexijson.FlexiJSonArray createJSonArray() {
                    return mapper.createFlexiJSonArray(0);
                };

                public FlexiJSonObject createJSonObject() {
                    return mapper.createFlexiJSonObject();
                };

                public org.appwork.storage.flexijson.FlexiJSonValue createJSonValue() {
                    return mapper.createFlexiJSonValue();
                };

                public org.appwork.storage.flexijson.FlexiJSonValue createJSonValue(Boolean value) {
                    return mapper.createFlexiJSonValue(value);
                };

                public org.appwork.storage.flexijson.FlexiJSonValue createJSonValue(Number longValue) {
                    return mapper.createFlexiJSonValue(longValue);
                };

                public org.appwork.storage.flexijson.FlexiJSonValue createJSonValue(String value) {
                    return mapper.createFlexiJSonValue(value);
                };
            }.setDebug(new StringBuilder()).setIgnoreIssues(FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES).parse();
            synchronized (this) {
                final FlexiJSonObject backendNode = getBackendNode();
                backendNode.add(new KeyValueElement(backendNode, key, defaultNode));
            }
            return mapper.jsonToObject(defaultNode, new SimpleTypeRef<Object>(method.getGenericReturnType()));
        }
        // internal defaultvalue
        final Class<?> returnType = method.getReturnType();
        if (Clazz.isByte(returnType)) {
            return (byte) 0;
        } else if (Clazz.isCharacter(returnType)) {
            return (char) 0;
        } else if (Clazz.isShort(returnType)) {
            return (short) 0;
        } else if (Clazz.isInteger(returnType)) {
            return (int) 0;
        } else if (Clazz.isLong(returnType)) {
            return (long) 0l;
        } else if (Clazz.isDouble(returnType)) {
            return (double) 0.0;
        } else if (Clazz.isFloat(returnType)) {
            return (float) 0.0;
        } else if (method.getReturnType() == Boolean.class) {
            return Boolean.FALSE;
        } else if (method.getReturnType() == boolean.class) {
            return false;
        } else {
            return null;
        }
    }

    private volatile HashMap<String, PropertyHandler<InterfaceType, Object>> propertyHandlers = new HashMap<String, PropertyHandler<InterfaceType, Object>>();
    private InterfaceType                                                    storage;

    /**
     * @param <ReturnType>
     * @param string
     * @param class1
     * @return
     */
    @SuppressWarnings("unchecked")
    public <ReturnType> PropertyHandler<InterfaceType, ReturnType> getPropertyAccessHandler(String key, Class<ReturnType> clazz) {
        return getPropertyAccessHandler(key, clazz, !DebugMode.TRUE_IN_IDE_ELSE_FALSE);
    }

    /**
     * @param <ReturnType>
     * @param string
     * @param class1
     * @return
     */
    @SuppressWarnings("unchecked")
    public <ReturnType> PropertyHandler<InterfaceType, ReturnType> getPropertyAccessHandler(String key, Class<ReturnType> clazz, boolean swallowException) {
        if (!cType.getClassCache().getKeys().contains(key)) {
            final IllegalArgumentException e = new IllegalArgumentException("No key " + key + " available in " + cType);
            if (swallowException) {
                LogV3.log(e);
            } else {
                throw e;
            }
        }
        if (!clazz.isAssignableFrom(ReflectionUtils.getRaw(cType.getClassCache().getType(key)))) {
            final IllegalArgumentException e = new IllegalArgumentException("Invalid type " + key + " has type " + ReflectionUtils.getRaw(cType.getClassCache().getType(key)) + " - not " + clazz);
            if (swallowException) {
                LogV3.log(e);
            } else {
                throw e;
            }
        }
        synchronized (this) {
            @SuppressWarnings("unchecked")
            PropertyHandler<InterfaceType, ReturnType> ret = (PropertyHandler<InterfaceType, ReturnType>) propertyHandlers.get(key);
            if (ret == null) {
                final HashMap<String, PropertyHandler<InterfaceType, Object>> newMap = new HashMap<String, PropertyHandler<InterfaceType, Object>>(propertyHandlers);
                ret = new PropertyHandlerImpl<InterfaceType, ReturnType>(this, key, clazz);
                newMap.put(key, (PropertyHandler<InterfaceType, Object>) ret);
                propertyHandlers = newMap;
            }
            return ret;
        }
    }

    @SuppressWarnings("unchecked")
    public <ReturnType> PropertyHandler<InterfaceType, ReturnType> getPropertyAccessHandler(String key, TypeRef<ReturnType> type) {
        return getPropertyAccessHandler(key, type, !DebugMode.TRUE_IN_IDE_ELSE_FALSE);
    }

    public <ReturnType> PropertyHandler<InterfaceType, ReturnType> getPropertyAccessHandler(String key, TypeRef<ReturnType> type, boolean swallowException) {
        if (!cType.getClassCache().getKeys().contains(key)) {
            final IllegalArgumentException e = new IllegalArgumentException("No key " + key + " available in " + cType);
            if (swallowException) {
                LogV3.log(e);
            } else {
                throw e;
            }
        }
        final Class<?> clazz = type.getRawClass();
        if (!clazz.isAssignableFrom(ReflectionUtils.getRaw(cType.getClassCache().getType(key)))) {
            final IllegalArgumentException e = new IllegalArgumentException("Invalid type " + key + " has type " + ReflectionUtils.getRaw(cType.getClassCache().getType(key)) + " - not " + clazz);
            if (swallowException) {
                LogV3.log(e);
            } else {
                throw e;
            }
        }
        synchronized (this) {
            @SuppressWarnings("unchecked")
            PropertyHandler<InterfaceType, ReturnType> ret = (PropertyHandler<InterfaceType, ReturnType>) propertyHandlers.get(key);
            if (ret == null) {
                final HashMap<String, PropertyHandler<InterfaceType, Object>> newMap = new HashMap<String, PropertyHandler<InterfaceType, Object>>(propertyHandlers);
                ret = new PropertyHandlerImpl<InterfaceType, ReturnType>(this, key, (Class<ReturnType>) clazz);
                newMap.put(key, (PropertyHandler<InterfaceType, Object>) ret);
                propertyHandlers = newMap;
            }
            return ret;
        }
    }

    /**
     * @param cfg
     * @param string
     * @param class1
     * @return
     */
    public static <InterfaceType, ReturnType> PropertyHandler<InterfaceType, ReturnType> getPropertyHandler(InterfaceType proxy, String key, Class<ReturnType> clazz) {
        if (proxy == null) {
            return null;
        } else {
            return get(proxy).getPropertyAccessHandler(key, clazz);
        }
    }

    public static <InterfaceType, ReturnType> PropertyHandler<InterfaceType, ReturnType> getPropertyHandler(InterfaceType proxy, String key, Class<ReturnType> clazz, boolean swallowExceptions) {
        if (proxy == null) {
            return null;
        } else {
            return get(proxy).getPropertyAccessHandler(key, clazz, swallowExceptions);
        }
    }

    /**
     * @param cfg
     * @param key
     * @param typeRef
     * @return
     */
    public static <InterfaceType, ReturnType> PropertyHandler<InterfaceType, ReturnType> getPropertyHandler(InterfaceType proxy, String key, TypeRef<ReturnType> typeRef) {
        if (proxy == null) {
            return null;
        } else {
            return get(proxy).getPropertyAccessHandler(key, typeRef);
        }
    }

    public static <InterfaceType, ReturnType> PropertyHandler<InterfaceType, ReturnType> getPropertyHandler(InterfaceType proxy, String key, TypeRef<ReturnType> typeRef, boolean swallowExceptions) {
        if (proxy == null) {
            return null;
        } else {
            return get(proxy).getPropertyAccessHandler(key, typeRef, swallowExceptions);
        }
    }

    /**
     * @param proxy
     */
    public void setStorage(InterfaceType proxy) {
        this.storage = proxy;
    }

    /**
     * @param storage
     * @return
     */
    @SuppressWarnings("unchecked")
    public static <InterfaceType> InterfaceStorage<InterfaceType> get(InterfaceType storage) {
        final InvocationHandler ret = Proxy.getInvocationHandler(storage);
        if (ret instanceof InterfaceStorage) {
            return (InterfaceStorage<InterfaceType>) ret;
        } else {
            return null;
        }
    }

    /**
     * @return
     */
    public InterfaceType getStorage() {
        return storage;
    }

    /**
     * reduces the backend storage to actual available properties
     *
     * @param proxy2
     * @return
     */
    public static <InterfaceType> InterfaceType shrink(InterfaceType storage, CompiledType type) {
        InterfaceStorage<InterfaceType> stori = get(storage);
        if (type == null) {
            type = stori.cType;
        }
        synchronized (stori) {
            FlexiJSonObject node = stori.backendNode;
            new FlexiWalker(node, type) {
                @Override
                public boolean onNode(FlexiJSonNode node, CompiledType type, JSPath path) {
                    if (type == null) {
                        return false;
                    }
                    return true;
                }
            }.run();
        }
        return storage;
    }

    /**
     * @param proxy2
     * @return
     */
    public static <InterfaceType> InterfaceType shrink(InterfaceType storage) {
        return shrink(storage, null);
    }
}
