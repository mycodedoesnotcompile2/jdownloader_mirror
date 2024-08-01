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
package org.appwork.storage.simplejson.mapper;

import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CopyOnWriteArraySet;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.MapperType;
import org.appwork.storage.StorableSupportedMappers;
import org.appwork.storage.TypeRef;
import org.appwork.storage.simplejson.JSonArray;
import org.appwork.storage.simplejson.JSonNode;
import org.appwork.storage.simplejson.JSonObject;
import org.appwork.storage.simplejson.JSonValue;
import org.appwork.storage.simplejson.JsonObjectLinkedHashMap;
import org.appwork.utils.Application;
import org.appwork.utils.DebugMode;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.StringUtils;
import org.appwork.utils.duration.TimeSpan;
import org.appwork.utils.reflection.Clazz;

/**
 * @author thomas
 *
 */
public class JSonMapper {
    private boolean                                  ignorePrimitiveNullMapping    = false;
    private boolean                                  ignoreIllegalArgumentMappings = false;
    /**
     * @param value
     * @param cType
     * @return
     */
    private boolean                                  ignoreIllegalEnumMappings     = false;
    protected final HashMap<Class<?>, TypeMapper<?>> typeMapper;
    protected Class<?>                               autoMapJsonObjectClass        = LinkedHashMap.class;
    protected Class<?>                               autoMapJsonArrayclass         = LinkedList.class;
    private ArrayList<MapperException>               exceptions;

    public JSonMapper() {
        typeMapper = new HashMap<Class<?>, TypeMapper<?>>();
        this.addMapper(Date.class, new DateMapper());
        this.addMapper(TimeSpan.class, new TimeSpanMapper());
        // TODO: keep in Sync with FlexiJsonMapper DefaultMapper
    }

    /**
     * @param <T>
     * @param class1
     * @param fileMapper
     */
    public <T> void addMapper(final Class<T> class1, final TypeMapper<T> fileMapper) {
        typeMapper.put(class1, fileMapper);
    }

    /**
     * @param obj
     * @return
     * @throws MapperException
     */
    @SuppressWarnings("unchecked")
    public JSonNode create(final Object obj) throws MapperException {
        if (obj == null) {
            return createJsonValue((String) null);
        }
        final Class<? extends Object> clazz = obj.getClass();
        TypeMapper<?> mapper;
        if (clazz.isPrimitive()) {
            if (clazz == boolean.class) {
                return createJsonValue((Boolean) obj);
            } else if (clazz == char.class) {
                return createJsonValue(0 + ((Character) obj).charValue());
            } else if (Clazz.isNumberType(clazz)) {
                return createJsonValue(((Number) obj));
            } else {
                throw new WTFException("Unknown Primitive Type: " + clazz);
            }
        } else if (clazz.isEnum() || Enum.class.isAssignableFrom(clazz)) {
            return createJsonValue(((Enum) obj).name());
        } else if (obj instanceof Boolean) {
            return createJsonValue(((Boolean) obj));
        } else if (obj instanceof Character) {
            return createJsonValue(0 + ((Character) obj).charValue());
        } else if (obj instanceof Number) {
            return createJsonValue(((Number) obj));
        } else if (obj instanceof String) {
            return createJsonValue((String) obj);
        } else if (obj instanceof CharSequence) {
            return createJsonValue(((CharSequence) obj).toString());
        } else if (obj instanceof Map) {
            final JSonObject ret = createJSonObject(null);
            Entry<Object, Object> next;
            for (final Iterator<Entry<Object, Object>> it = ((Map<Object, Object>) obj).entrySet().iterator(); it.hasNext();) {
                next = it.next();
                if (!(next.getKey() instanceof String)) {
                    returnFallbackOrThrowException(new MapperException(ret, obj.getClass(), "Map keys have to be Strings: " + clazz + " Keyclass:" + (next.getKey() == null ? "<null>" : next.getKey().getClass())));
                } else {
                    ret.put(next.getKey().toString(), create(next.getValue()));
                }
            }
            return createJSonObject(ret);
        } else if (obj instanceof Collection) {
            Collection<?> col = (Collection<?>) obj;
            final JSonArray ret = new JSonArray(col.size());
            for (final Object o : col) {
                ret.add(create(o));
            }
            return ret;
        } else if (clazz.isArray()) {
            final int length = Array.getLength(obj);
            final JSonArray ret = new JSonArray(length);
            for (int i = 0; i < length; i++) {
                ret.add(create(Array.get(obj, i)));
            }
            return ret;
        } else if (obj instanceof Class) {
            return createJsonValue(((Class<?>) obj).getName());
        } else if ((mapper = typeMapper.get(clazz)) != null) {
            return mapper.map(obj);
        } else/* if (obj instanceof Storable) */{
            final JSonObject ret = createJSonObject(null);
            try {
                final ClassCache cc = getClassCache(clazz);
                for (final Getter g : cc.getGetter()) {
                    try {
                        serialize(obj, ret, cc, g);
                    } catch (IllegalArgumentException e) {
                        returnFallbackOrThrowException(new MapperException(ret, clazz, e.getMessage(), e));
                    } catch (IllegalAccessException e) {
                        returnFallbackOrThrowException(new MapperException(ret, clazz, e.getMessage(), e));
                    } catch (InvocationTargetException e) {
                        returnFallbackOrThrowException(new MapperException(ret, clazz, e.getMessage(), e));
                    }
                }
            } catch (SecurityException e) {
                returnFallbackOrThrowException(new MapperException(ret, clazz, e.getMessage(), e));
            } catch (NoSuchMethodException e) {
                returnFallbackOrThrowException(new MapperException(ret, clazz, e.getMessage(), e));
            }
            return createJSonObject(ret);
        }
    }

    protected void serialize(final Object obj, final JSonObject ret, final ClassCache cc, final Getter getter) throws MapperException, IllegalAccessException, InvocationTargetException {
        final String key = getter.getKey();
        final Object value = getter.getValue(obj);
        final JSonNode node = create(value);
        ret.put(key, node);
    }

    public ClassCache getClassCache(final Class<? extends Object> clazz) throws SecurityException, NoSuchMethodException {
        return ClassCache.getClassCache(clazz);
    }

    protected JSonObject createJSonObject(JSonObject map) {
        if (map == null) {
            return new JsonObjectLinkedHashMap();
        } else {
            return new JsonObjectLinkedHashMap(map);
        }
    }

    protected JSonValue createJsonValue(Number longValue) {
        return new JSonValue(longValue);
    }

    protected JSonValue createJsonValue(Boolean value) {
        if (true) {
            if (value == null) {
                return JSonValue.BOOLEAN_NULL;
            } else if (value) {
                return JSonValue.BOOLEAN_TRUE;
            } else {
                return JSonValue.BOOLEAN_FALSE;
            }
        } else {
            return new JSonValue(value);
        }
    }

    /**
     * @param name
     * @return
     */
    protected JSonValue createJsonValue(String value) {
        if (true && value == null) {
            return JSonValue.NULL;
        } else {
            return new JSonValue(value);
        }
    }

    public boolean isIgnoreIllegalArgumentMappings() {
        return ignoreIllegalArgumentMappings;
    }

    public boolean isIgnoreIllegalEnumMappings() {
        return ignoreIllegalEnumMappings;
    }

    /**
     * if json maps null to a primitive field
     *
     * @return
     */
    public boolean isIgnorePrimitiveNullMapping() {
        return ignorePrimitiveNullMapping;
    }

    protected Collection<Object> optimizeCollection(Class mapClass, Collection<Object> col) {
        try {
            if (col instanceof CopyOnWriteArraySet || col instanceof CopyOnWriteArrayList) {
                // auto resize on modification
                return col;
            } else if (col instanceof ArrayList) {
                ((ArrayList) col).trimToSize();
                return col;
            } else if (col instanceof LinkedList) {
                // cannot be further optimized
                return col;
            } else {
                // create new Collection insance, Collection implementation likely will use optimized/smaller internal table
                final Constructor<?> constructor = mapClass.getConstructor(Collection.class);
                final Collection<Object> ret = (Collection<Object>) constructor.newInstance(col);
                return ret;
            }
        } catch (Exception e) {
            return col;
        }
    }

    protected Map<String, Object> optimizeMap(Class mapClass, Map<String, Object> map) {
        try {
            // create new Map insance, Map implementation likely will use optimized/smaller internal table
            final Constructor<?> constructor = mapClass.getConstructor(Map.class);
            final Map<String, Object> ret = (Map<String, Object>) constructor.newInstance(map);
            return ret;
        } catch (Exception e) {
            return map;
        }
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    public Object jsonToObject(final JSonNode json, Type type) throws MapperException {
        final ClassCache cc;
        try {
            Class<?> clazz = null;
            if (type instanceof ParameterizedType) {
                Type typ = ((ParameterizedType) type).getRawType();
                if (typ instanceof Class) {
                    clazz = (Class<?>) typ;
                }
            } else if (type instanceof Class) {
                clazz = (Class) type;
            } else if (type instanceof GenericArrayType) {
                // TODO: For example, List.toArray(T[]) -> T[]
                // TODO: public List<String>[] array;
                // it's not easy to implement proper support for this here. type my be something like "T[]" - an array of TypeVariable, and
                // we have no context/scope here to resolve the Typevariable to the proper type.
                // in FlexiJson, this is done by using CompiledType that stores its typehirarchy and thus can resolve all types.
                type = clazz = Array.newInstance((Class<?>) ((GenericArrayType) type).getGenericComponentType(), 0).getClass();
            }
            if (clazz == null || clazz == Object.class) {
                if (json instanceof JSonArray) {
                    type = clazz = autoMapJsonArrayclass;
                } else if (json instanceof JSonObject) {
                    type = clazz = autoMapJsonObjectClass;
                } else if (json instanceof JSonValue) {
                    final JSonValue jsonValue = ((JSonValue) json);
                    final Object value = jsonValue.getValue();
                    switch (jsonValue.getType()) {
                    case BOOLEAN:
                        type = clazz = boolean.class;
                        break;
                    case DOUBLE:
                        if (value != null && Clazz.isFloatingPointNumber(value.getClass())) {
                            type = clazz = value.getClass();
                        } else {
                            type = clazz = double.class;
                        }
                        break;
                    case LONG:
                        if (value != null && Clazz.isFixedPointNumber(value.getClass())) {
                            type = clazz = value.getClass();
                        } else {
                            type = clazz = long.class;
                        }
                        break;
                    case NULL:
                    case STRING:
                        type = clazz = String.class;
                        break;
                    case UNDEFINED:
                    default:
                        break;
                    }
                }
            }
            if (!Application.isJared(null)) {
                StorableSupportedMappers supported = clazz.getAnnotation(StorableSupportedMappers.class);
                if (supported != null) {
                    if (!Arrays.asList(supported.value()).contains(MapperType.AW_SIMPLE)) {
                        // This storable is not supported by simpleMapper
                        DebugMode.debugger();
                    }
                }
            }
            final TypeMapper<?> tm = typeMapper.get(clazz);
            if (tm != null) {
                return tm.reverseMap(json);
            }
            if (json instanceof JSonValue) {
                if (!Clazz.isPrimitive(type) && !Clazz.isString(type) && type != Object.class && ((JSonValue) json).getValue() != null && !Clazz.isEnum(type)) {
                    return returnFallbackOrThrowException(new ClassCastMappingException(json, type, null, null, null));
                }
                switch (((JSonValue) json).getType()) {
                case BOOLEAN:
                case DOUBLE:
                case LONG:
                    if (type instanceof Class) {
                        try {
                            return ReflectionUtils.cast(((JSonValue) json).getValue(), type);
                        } catch (ClassCastException e) {
                            if (isIgnoreIllegalArgumentMappings()) {
                                return null;
                            } else {
                                return returnFallbackOrThrowException(new ClassCastMappingException(json, type, null, e, null));
                            }
                        }
                    } else {
                        return ((JSonValue) json).getValue();
                    }
                case STRING:
                    if (type instanceof Class && ((Class<?>) type).isEnum()) {
                        try {
                            return Enum.valueOf((Class<Enum>) type, StringUtils.valueOfOrNull(((JSonValue) json).getValue()));
                        } catch (final IllegalArgumentException e) {
                            if (isIgnoreIllegalArgumentMappings() || isIgnoreIllegalEnumMappings()) {
                                return null;
                            } else {
                                return returnFallbackOrThrowException(new ClassCastMappingException(json, type, null, e, null));
                            }
                        }
                    } else if (type instanceof Class && (Clazz.isNumberType(type) || Clazz.isBoolean(type)) || Clazz.isCharacter(type)) {
                        try {
                            return ReflectionUtils.cast(((JSonValue) json).getValue(), type);
                        } catch (ClassCastException e) {
                            if (isIgnoreIllegalArgumentMappings()) {
                                return null;
                            } else {
                                return returnFallbackOrThrowException(new ClassCastMappingException(json, type, null, e, null));
                            }
                        }
                    } else {
                        return ((JSonValue) json).getValue();
                    }
                case NULL:
                    if (type instanceof Class && (Clazz.isNumberType(type) || Clazz.isBoolean(type))) {
                        try {
                            return ReflectionUtils.cast(((JSonValue) json).getValue(), type);
                        } catch (ClassCastException e) {
                            if (isIgnoreIllegalArgumentMappings()) {
                                return null;
                            } else {
                                return returnFallbackOrThrowException(new ClassCastMappingException(json, type, null, e, null));
                            }
                        }
                    } else {
                        return null;
                    }
                }
            }
            if (type instanceof ParameterizedType) {
                final ParameterizedType pType = (ParameterizedType) type;
                Type raw = pType.getRawType();
                if (raw instanceof Class && Collection.class.isAssignableFrom((Class) raw)) {
                    Collection<Object> inst;
                    final Class mapClass;
                    try {
                        mapClass = mapClasses((Class) raw);
                        inst = (Collection<Object>) mapClass.newInstance();
                    } catch (Exception e) {
                        return returnFallbackOrThrowException(new FailedToCreateInstanceMapperException(json, type, (Class) raw, e));
                    }
                    final JSonArray obj = (JSonArray) json;
                    for (final JSonNode n : obj) {
                        inst.add(this.jsonToObject(n, pType.getActualTypeArguments()[0]));
                    }
                    return optimizeCollection(mapClass, inst);
                } else if (raw instanceof Class && Map.class.isAssignableFrom((Class) raw)) {
                    Map<String, Object> inst;
                    final Class mapClass;
                    try {
                        mapClass = mapClasses((Class) raw);
                        inst = (Map<String, Object>) mapClass.newInstance();
                    } catch (Exception e) {
                        return returnFallbackOrThrowException(new FailedToCreateInstanceMapperException(json, type, (Class) raw, e));
                    }
                    final JSonObject obj = (JSonObject) json;
                    Entry<String, JSonNode> next;
                    for (final Iterator<Entry<String, JSonNode>> it = obj.entrySet().iterator(); it.hasNext();) {
                        next = it.next();
                        inst.put(next.getKey(), this.jsonToObject(next.getValue(), pType.getActualTypeArguments()[1]));
                    }
                    return optimizeMap(mapClass, inst);
                }
            }
            if (clazz != null) {
                if (clazz == Object.class) {
                    // guess type
                    if (json instanceof JSonArray) {
                        type = autoMapJsonArrayclass;
                    } else if (json instanceof JSonObject) {
                        type = autoMapJsonObjectClass;
                    }
                }
                if (Collection.class.isAssignableFrom(clazz)) {
                    Collection<Object> inst;
                    final Class mapClass;
                    try {
                        mapClass = mapClasses(clazz);
                        inst = (Collection<Object>) mapClass.newInstance();
                    } catch (Exception e) {
                        return returnFallbackOrThrowException(new FailedToCreateInstanceMapperException(json, type, clazz, e));
                    }
                    final JSonArray obj = (JSonArray) json;
                    final Type gs = clazz.getGenericSuperclass();
                    final Type gType;
                    if (gs instanceof ParameterizedType) {
                        gType = ((ParameterizedType) gs).getActualTypeArguments()[0];
                    } else {
                        gType = void.class;
                    }
                    for (final JSonNode n : obj) {
                        inst.add(this.jsonToObject(n, gType));
                    }
                    return optimizeCollection(mapClass, inst);
                } else if (Map.class.isAssignableFrom(clazz)) {
                    final Map<String, Object> inst;
                    final Class<?> mapClass;
                    try {
                        mapClass = mapClasses(clazz);
                        inst = (Map<String, Object>) mapClass.newInstance();
                    } catch (Exception e) {
                        return returnFallbackOrThrowException(new FailedToCreateInstanceMapperException(json, type, clazz, e));
                    }
                    final JSonObject obj = (JSonObject) json;
                    final Type gs = clazz.getGenericSuperclass();
                    final Type gType;
                    if (gs instanceof ParameterizedType) {
                        gType = ((ParameterizedType) gs).getActualTypeArguments()[1];
                    } else {
                        gType = void.class;
                    }
                    Entry<String, JSonNode> next;
                    for (final Iterator<Entry<String, JSonNode>> it = obj.entrySet().iterator(); it.hasNext();) {
                        next = it.next();
                        inst.put(next.getKey(), this.jsonToObject(next.getValue(), gType));
                    }
                    return optimizeMap(mapClass, inst);
                } else if (clazz.isArray()) {
                    final JSonArray obj = (JSonArray) json;
                    final Object arr = Array.newInstance(mapClasses(clazz.getComponentType()), obj.size());
                    for (int i = 0; i < obj.size(); i++) {
                        final Object v = this.jsonToObject(obj.get(i), clazz.getComponentType());
                        try {
                            Array.set(arr, i, v);
                        } catch (IllegalArgumentException e) {
                            returnFallbackOrThrowException(new MapperException(obj.get(i), mapClasses(clazz.getComponentType()), "Cannot convert index [" + i + "] = " + obj.get(i) + " to type " + mapClasses(clazz.getComponentType()), e));
                            Array.set(arr, i, null);
                        }
                    }
                    return arr;
                } else {
                    if (json instanceof JSonArray) {
                        final JSonArray obj = (JSonArray) json;
                        final java.util.List<Object> inst = new ArrayList<Object>(obj.size());
                        final Type gs = clazz.getGenericSuperclass();
                        final Type gType;
                        if (gs instanceof ParameterizedType) {
                            gType = ((ParameterizedType) gs).getActualTypeArguments()[0];
                        } else {
                            gType = Object.class;
                        }
                        for (final JSonNode n : obj) {
                            inst.add(this.jsonToObject(n, gType));
                        }
                        return inst;
                    } else {
                        final JSonObject obj = (JSonObject) json;
                        if (Clazz.isPrimitive(clazz)) {
                            //
                            if (isIgnoreIllegalArgumentMappings()) {
                                return null;
                            } else {
                                return returnFallbackOrThrowException(new ClassCastMappingException(json, type, "Cannot map JSonObject to a Primitive " + clazz, null, null));
                            }
                        } else if (Clazz.isString(clazz)) {
                            if (isIgnoreIllegalArgumentMappings()) {
                                return null;
                            } else {
                                return returnFallbackOrThrowException(new ClassCastMappingException(json, type, "Cannot map JSonObject to a String " + clazz, null, null));
                            }
                        } else if (clazz.isEnum() || Enum.class.isAssignableFrom(clazz)) {
                            if (isIgnoreIllegalArgumentMappings()) {
                                return null;
                            } else {
                                return returnFallbackOrThrowException(new ClassCastMappingException(json, type, "Cannot map JSonObject to a Enum " + clazz, null, null));
                            }
                        }
                        Object inst = null;
                        try {
                            cc = ClassCache.getClassCache(clazz);
                            inst = createNewInstance(cc);
                        } catch (Exception e1) {
                            return returnFallbackOrThrowException(new FailedToCreateInstanceMapperException(json, type, clazz, e1));
                        }
                        JSonNode value;
                        Object v;
                        for (Entry<String, JSonNode> es : obj.entrySet()) {
                            String key = es.getKey();
                            Setter s = getSetterByKey(cc, key);
                            if (s == null) {
                                onClassFieldMissing(inst, es.getKey(), es.getValue());
                                continue;
                            }
                            value = es.getValue();
                            //
                            Type fieldType = s.getType();
                            // this loop searches the next actual generic type. to find the actual field type.
                            // this loop solves situations like public class KeyValueStringEntry extends KeyValueEntry<String, String>,
                            // special handling for generic fields
                            Class cls = s.getMethod().getDeclaringClass();
                            w: while (fieldType instanceof TypeVariable) {
                                ParameterizedType parameterized = cc.getParameterizedType(cls);
                                if (parameterized == null && type instanceof ParameterizedType) {
                                    parameterized = (ParameterizedType) type;
                                }
                                Type[] actual = parameterized.getActualTypeArguments();
                                TypeVariable[] types = cls.getTypeParameters();
                                for (int i = 0; i < types.length; i++) {
                                    if (StringUtils.equals(((TypeVariable) fieldType).getName(), types[i].getName())) {
                                        // public class KeyValueEntry<KeyType, ValueType> extends ValueEntry<ValueType> { ... public void
                                        // setKey(KeyType key) {
                                        fieldType = actual[i];
                                        Type extendingClass = cc.getExtendedType(cls);
                                        if (extendingClass != null) {
                                            if (extendingClass instanceof Class) {
                                                cls = (Class) extendingClass;
                                            } else {
                                                cls = (Class) ((ParameterizedType) extendingClass).getRawType();
                                            }
                                        }
                                        continue w;
                                    }
                                }
                                returnFallbackOrThrowException(new MapperException(value, type, "Cannot resolve Type " + cls + "/" + Arrays.toString(types) + " - " + fieldType + " " + es.getKey() + "=" + value + " to type " + fieldType, null));
                                continue;
                            }
                            try {
                                v = this.jsonToObject(value, fieldType);
                            } catch (IllegalArgumentException e) {
                                returnFallbackOrThrowException(new MapperException(value, type, "Cannot convert " + es.getKey() + "=" + value + " to type " + fieldType, e));
                                continue;
                            }
                            try {
                                s.setValue(inst, v);
                            } catch (final NullPointerException e) {
                                if (isIgnoreIllegalArgumentMappings()) {
                                    continue;
                                } else if (v == null && isIgnorePrimitiveNullMapping()) {
                                    continue;
                                }
                                returnFallbackOrThrowException(new ClassCastMappingException(value, s.getType(), null, e, v));
                            } catch (final IllegalArgumentException e) {
                                if (isIgnoreIllegalArgumentMappings()) {
                                    continue;
                                } else if (v == null && isIgnorePrimitiveNullMapping()) {
                                    continue;
                                }
                                returnFallbackOrThrowException(new ClassCastMappingException(value, s.getType(), null, e, v));
                            } catch (Exception e) {
                                returnFallbackOrThrowException(new ClassCastMappingException(value, s.getType(), null, e, v));
                            }
                        }
                        return inst;
                    }
                }
            } else {
                System.err.println("TYPE?!");
            }
            // } catch (final SecurityException e) {
            // e.printStackTrace();
            // } catch (final NoSuchMethodException e) {
            // e.printStackTrace();
            // } catch (final IllegalArgumentException e) {
            // e.printStackTrace();
            // } catch (final InstantiationException e) {
            // e.printStackTrace();
            // } catch (final IllegalAccessException e) {
            // e.printStackTrace();
            // } catch (final InvocationTargetException e) {
            // e.printStackTrace();
        } catch (MapperException e) {
            // returnFallbackOrThrowException should be handled earlier.
            throw e;
        } catch (RuntimeException e) {
            return returnFallbackOrThrowException(new MapperException(json, type, e));
        } finally {
        }
        return null;
    }

    protected Object createNewInstance(final ClassCache cc) throws InstantiationException, IllegalAccessException, InvocationTargetException {
        return cc.getInstance();
    }

    /**
     * return true if the exception should not be thrown
     *
     * @param ex
     * @return
     * @throws MapperException
     */
    protected Object returnFallbackOrThrowException(MapperException ex) throws MapperException {
        if (isCollectExceptions()) {
            if (exceptions == null) {
                exceptions = new ArrayList<MapperException>();
            }
            exceptions.add(ex);
            return null;
        } else {
            throw ex;
        }
    }

    public ArrayList<MapperException> getExceptions() {
        return exceptions;
    }

    protected Setter getSetterByKey(final ClassCache cc, String key) {
        return cc.getSetter(key);
    }

    /**
     * @param inst
     * @param key
     * @param value
     * @throws MapperException
     */
    protected void onClassFieldMissing(Object inst, String key, JSonNode value) throws MapperException {
    }

    /**
     * @param <T>
     * @param json
     * @param typeRef
     * @throws MapperException
     */
    @SuppressWarnings("unchecked")
    public <T> T jsonToObject(final JSonNode json, final TypeRef<T> type) throws MapperException {
        return (T) this.jsonToObject(json, type.getType());
    }

    /**
     * @param class1
     * @return
     * @throws MapperException
     */
    protected Class<?> mapClasses(final Class<?> class1) throws MapperException {
        if (class1.isInterface()) {
            if (List.class.isAssignableFrom(class1)) {
                return ArrayList.class;
            } else if (Map.class.isAssignableFrom(class1)) {
                return LinkedHashMap.class;
            } else if (Set.class.isAssignableFrom(class1)) {
                // more lightweight in memory consumption than LinkedHashSet
                return CopyOnWriteArraySet.class;
            } else {
                throw new WTFException("Interface not supported: " + class1);
            }
        }
        return class1;
    }

    private boolean collectExceptions = false;

    public boolean isCollectExceptions() {
        return collectExceptions;
    }

    public void setCollectExceptions(boolean collectExceptions) {
        this.collectExceptions = collectExceptions;
    }

    public void setIgnoreIllegalArgumentMappings(final boolean ignoreIllegalArgumentMappings) {
        this.ignoreIllegalArgumentMappings = ignoreIllegalArgumentMappings;
    }

    public void setIgnoreIllegalEnumMappings(final boolean ignoreIllegalEnumMappings) {
        this.ignoreIllegalEnumMappings = ignoreIllegalEnumMappings;
    }

    public void setIgnorePrimitiveNullMapping(final boolean ignoreIllegalNullArguments) {
        ignorePrimitiveNullMapping = ignoreIllegalNullArguments;
    }
}
