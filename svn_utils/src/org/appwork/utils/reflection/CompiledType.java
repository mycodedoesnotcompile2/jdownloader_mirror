/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher StraÃŸe 117
 *         90763 FÃ¼rth
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
package org.appwork.utils.reflection;

import java.lang.annotation.Annotation;
import java.lang.ref.WeakReference;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.lang.reflect.WildcardType;
import java.util.AbstractList;
import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.WeakHashMap;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.storage.TypeRef;
import org.appwork.storage.flexijson.CannotResolvePathException;
import org.appwork.storage.flexijson.JSPath;
import org.appwork.storage.simplejson.mapper.ClassCache;
import org.appwork.storage.simplejson.mapper.Property;
import org.appwork.utils.DebugMode;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.StringUtils;

/**
 * @author thomas
 * @date 14.10.2022
 *
 */
public class CompiledType {
    /**
     * @param type
     * @param context
     */
    private final static ThreadLocal<HashMap<Type, List<CompiledType>>> INIT_CACHE = new ThreadLocal<HashMap<Type, List<CompiledType>>>();
    private static final CompiledType[]                                 EMPTY      = new CompiledType[0];
    private static final Map<Type, CompiledType>                        EMPTY_MAP  = Collections.unmodifiableMap(new HashMap<Type, CompiledType>());
    public final Class<? extends Type>                                  category;
    public final Class<?>                                               raw;
    /**
     * component types. String[] -> String ArrayList<String> ->String Map<String,Integer> -> String, Integer ...
     */
    public final CompiledType[]                                         componentTypes;

    public CompiledType[] getComponentTypes() {
        return componentTypes;
    }

    /**
     * The actual type
     */
    public final Type                            type;
    /**
     * Generic Type (e.g. T) to actual Type map
     */
    public final Map<Type, CompiledType>         genericTypesMap;
    /**
     * the class supertype
     */
    public final CompiledType                    superType;
    private final LinkedList<Type>               contextHirarchy;
    private LinkedList<CompiledType>             cachedTypeHirarchy;
    private ClassCache                           classCache;
    private boolean                              generic;
    private boolean                              genericsResolvedByContext;
    private Boolean                              genericsResolved;
    private Boolean                              isEnum = null;
    private static final ArrayList<CompiledType> PRIMITIVES_AND_BASICS;

    /*
     * (non-Javadoc)
     *
     * @see java.lang.reflect.Type#getTypeName()
     */
    public String getTypeName() {
        return "CompiledType " + toString();
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return toString(null);
    }

    private final static HashSet<Class>                                       BREAK_HIERARCHY_AT_TYPES = new HashSet<Class>();
    private volatile static WeakHashMap<Type, CompiledType>                   SIMPLE_CACHE             = new WeakHashMap<Type, CompiledType>();
    private static final WeakHashMap<Type, List<WeakReference<CompiledType>>> CACHE                    = new WeakHashMap<Type, List<WeakReference<CompiledType>>>();
    public static final CompiledType                                          BOOLEAN_PRIMITIVE        = CompiledType.create(boolean.class);
    public static final CompiledType                                          BOOLEAN_WRAPPER          = CompiledType.create(Boolean.class);
    public static final CompiledType                                          STRING                   = CompiledType.create(String.class);
    public static final CompiledType                                          DOUBLE_PRIMITIVE         = CompiledType.create(double.class);
    public static final CompiledType                                          DOUBLE_WRAPPER           = CompiledType.create(Double.class);
    public static final CompiledType                                          INT_PRIMITIVE            = CompiledType.create(int.class);
    public static final CompiledType                                          INT_WRAPPER              = CompiledType.create(Integer.class);
    public static final CompiledType                                          FLOAT_PRIMITIVE          = CompiledType.create(float.class);
    public static final CompiledType                                          FLOAT_WRAPPER            = CompiledType.create(Float.class);
    public static final CompiledType                                          LONG_PRIMITIVE           = CompiledType.create(long.class);
    public static final CompiledType                                          LONG_WRAPPER             = CompiledType.create(Long.class);
    public static final CompiledType                                          BYTE_PRIMITIVE           = CompiledType.create(byte.class);
    public static final CompiledType                                          BYTE_WRAPPER             = CompiledType.create(Byte.class);
    public static final CompiledType                                          SHORT_PRIMITIVE          = CompiledType.create(short.class);
    public static final CompiledType                                          SHORT_WRAPPER            = CompiledType.create(Short.class);
    public static final CompiledType                                          CHAR_PRIMITIVE           = CompiledType.create(char.class);
    public static final CompiledType                                          CHAR_WRAPPER             = CompiledType.create(Character.class);
    public static final CompiledType                                          OBJECT                   = CompiledType.create(Object.class);
    static {
        // BREAK_HIERARCHY_AT_TYPES.add(HashMap.class);
        // BREAK_HIERARCHY_AT_TYPES.add(HashSet.class);
        // BREAK_HIERARCHY_AT_TYPES.add(ArrayList.class);
        // BREAK_HIERARCHY_AT_TYPES.add(LinkedList.class);
        BREAK_HIERARCHY_AT_TYPES.add(AbstractMap.class);
        BREAK_HIERARCHY_AT_TYPES.add(AbstractSet.class);
        BREAK_HIERARCHY_AT_TYPES.add(AbstractList.class);
        // no reason to cache the statics above. the create method returns them without cache lookup
        ArrayList<CompiledType> staticList = new ArrayList<CompiledType>();
        for (Entry<Type, List<WeakReference<CompiledType>>> f : CACHE.entrySet()) {
            if (Clazz.isPrimitive(f.getKey()) || f.getKey() == String.class) {
                staticList.add(f.getValue().get(0).get());
                CACHE.remove(f.getKey());
                SIMPLE_CACHE.remove(f.getKey());
            }
        }
        staticList.add(OBJECT);
        PRIMITIVES_AND_BASICS = staticList;
    }

    public static enum PrimitiveWrapperStrategy {
        DEDICATED_NAME_FOR_EACH,
        PRIMITIVE_NAMES_FOR_BOTH,
        WRAPPER_NAMES_FOR_BOTH;
    }

    public abstract static class AbstractNameScheme {
        private boolean                  skipAnonymousClasses      = false;
        private PrimitiveWrapperStrategy primitiveWrapperStrategy  = PrimitiveWrapperStrategy.DEDICATED_NAME_FOR_EACH;
        private String                   nameForUnspecifiedGeneric = "?";

        /**
         *
         */
        public AbstractNameScheme() {
        }

        public String getNameForUnspecifiedGeneric() {
            return nameForUnspecifiedGeneric;
        }

        public void setNameForUnspecifiedGeneric(String nameForUnspecifiedGeneric) {
            this.nameForUnspecifiedGeneric = nameForUnspecifiedGeneric;
        }

        public boolean isSkipAnonymousClasses() {
            return skipAnonymousClasses;
        }

        public void setSkipAnonymousClasses(boolean skipAnonymousClasses) {
            this.skipAnonymousClasses = skipAnonymousClasses;
        }

        public AbstractNameScheme skipAnonymousClasses(boolean skipAnonymousClasses) {
            this.skipAnonymousClasses = skipAnonymousClasses;
            return this;
        }

        public PrimitiveWrapperStrategy getPrimitiveWrapperStrategy() {
            return primitiveWrapperStrategy;
        }

        public void setPrimitiveWrapperStrategy(PrimitiveWrapperStrategy primitiveWrapperStrategy) {
            this.primitiveWrapperStrategy = primitiveWrapperStrategy;
        }

        public AbstractNameScheme primitiveWrapperStrategy(PrimitiveWrapperStrategy primitiveWrapperStrategy) {
            this.primitiveWrapperStrategy = primitiveWrapperStrategy;
            return this;
        }

        /**
         * @param type
         * @param raw
         * @return
         */
        public abstract String getCollectionName(CompiledType type, Class<?> raw);

        /**
         * @param compiledType
         * @param raw
         * @return
         */
        public abstract String getMapName(CompiledType type, Class<?> raw);

        /**
         * @param type
         * @param raw
         * @return
         */
        public abstract String getClassName(CompiledType type, Class<?> raw);

        /**
         * @param compiledType
         * @param raw
         * @return
         */
        public abstract String getArrayName(CompiledType type, Class<?> raw);

        /**
         * @param compiledType
         * @param raw
         * @return
         */
        public abstract String getEnumName(CompiledType type, Class<?> raw);

        /**
         * @param compiledType
         * @return
         */
        public abstract CompiledType[] getComponentTypes(CompiledType type, Class<?> raw);

        /**
         * @param compiledType
         * @param raw
         * @param sb
         * @param actualComponents
         */
        public abstract void appendComponents(CompiledType type, Class<?> raw, StringBuilder sb, CompiledType[] components);
    }

    public String toString(AbstractNameScheme rule) {
        StringBuilder sb = new StringBuilder();
        if (rule == null) {
            rule = new JsonSyntax();
        }
        appendName(rule, sb);
        return sb.toString();
    }

    /**
     * @param rule
     * @param sb
     */
    public void appendName(AbstractNameScheme rule, StringBuilder sb) {
        if (raw != null && raw.isAnonymousClass() && rule.isSkipAnonymousClasses()) {
            superType.appendName(rule, sb);
            return;
        }
        if (raw == null) {
            if (type instanceof WildcardType) {
                if ("?".equals(type.toString())) {
                    sb.append(rule.getNameForUnspecifiedGeneric());
                } else if (((WildcardType) type).getUpperBounds().length == 1 && ((WildcardType) type).getLowerBounds().length == 0) {
                    sb.append(rule.getNameForUnspecifiedGeneric() + " extends " + CompiledType.create(((WildcardType) type).getUpperBounds()[0]).toString(rule));
                } else {
                    sb.append(((WildcardType) type).toString());
                }
            } else if (type instanceof TypeVariable) {
                Type[] bounds = ((TypeVariable) type).getBounds();
                sb.append(((TypeVariable) type).toString());
            } else {
                LogV3.log(new Exception("Unknown type " + type));
                sb.append("Unknown Type");
                return;
            }
        } else {
            if (Collection.class.isAssignableFrom(raw)) {
                sb.append(rule.getCollectionName(this, raw));
            } else if (Map.class.isAssignableFrom(raw)) {
                sb.append(rule.getMapName(this, raw));
            } else if (((Class) raw).isArray()) {
                sb.append(rule.getArrayName(this, raw));
            } else if (((Class) raw).isEnum()) {
                sb.append(rule.getEnumName(this, raw));
            } else if (Clazz.isPrimitive(raw)) {
                switch (rule.getPrimitiveWrapperStrategy()) {
                case DEDICATED_NAME_FOR_EACH:
                    sb.append(rule.getClassName(this, raw));
                    break;
                case PRIMITIVE_NAMES_FOR_BOTH:
                    if (!Clazz.isPrimitiveWrapper(raw)) {
                        sb.append(rule.getClassName(this, raw));
                    } else {
                        sb.append(rule.getClassName(this, (Clazz.wrapperToPrimitive(raw))));
                    }
                    break;
                case WRAPPER_NAMES_FOR_BOTH:
                    if (Clazz.isPrimitiveWrapper(raw)) {
                        sb.append(rule.getClassName(this, raw));
                    } else {
                        sb.append(rule.getClassName(this, ((Class) Clazz.primitiveToWrapper(raw))));
                    }
                    break;
                }
            } else {
                sb.append(rule.getClassName(this, raw));
            }
        }
        CompiledType[] actualComponents = componentTypes;
        actualComponents = rule.getComponentTypes(this, raw);
        rule.appendComponents(this, raw, sb, actualComponents);
    }

    /**
     * true if the type implements ALL of the given interfaces directly
     *
     * @param class1
     * @return
     */
    protected boolean isImplementing(Class<?>... interfaces) {
        NEXT_REQUESTED: for (Class<?> requested : interfaces) {
            for (Class<?> i : raw.getInterfaces()) {
                if (Clazz.isInstanceof(i, requested)) {
                    continue NEXT_REQUESTED;
                }
            }
            return false;
        }
        return true;
    }

    /**
     * @return return the extends type hierarchy. This does NOT include interfaces
     */
    private LinkedList<CompiledType> getTypeHirarchy() {
        if (cachedTypeHirarchy != null) {
            return cachedTypeHirarchy;
        }
        LinkedList<CompiledType> ret = new LinkedList<CompiledType>();
        CompiledType c = this;
        while (c != null) {
            ret.add(c);
            c = c.superType;
        }
        cachedTypeHirarchy = ret;
        return ret;
    }

    public boolean isGenericsResolved() {
        if (genericsResolved != null) {
            return genericsResolved == Boolean.TRUE;
        }
        genericsResolved = isGenericsResolvedInternal(new HashSet<CompiledType>());
        return genericsResolved == Boolean.TRUE;
    }

    /**
     * @param hashSet
     * @return
     */
    private boolean isGenericsResolvedInternal(HashSet<CompiledType> hashSet) {
        // reference loop detection. a generic class may have itself as component class
        if (!hashSet.add(this)) {
            return true;
        }
        if (type instanceof TypeVariable) {
            return false;
        }
        if (type instanceof WildcardType) {
            return false;
        }
        for (CompiledType c : componentTypes) {
            if (!c.isGenericsResolvedInternal(hashSet)) {
                return false;
            }
        }
        if (superType != null) {
            return superType.isGenericsResolvedInternal(hashSet);
        }
        return true;
    }

    private CompiledType(Type type, LinkedList<Type> contextHirarchy) {
        // fill the cache at the beginning of the constructur. This way, the cache is already filled in case of recursion
        if (contextHirarchy.getLast() != type) {
            contextHirarchy.add(type);
        }
        this.contextHirarchy = new LinkedList<Type>(contextHirarchy);
        this.type = type;
        this.category = type.getClass();
        putToThreadLocalCache(this);
        if (type instanceof Class) {
            this.raw = ReflectionUtils.getRaw(type);
            TypeVariable<?>[] types = raw.getTypeParameters();
            if (((Class) type).getComponentType() != null) {
                componentTypes = new CompiledType[] { CompiledType.create(((Class) type).getComponentType(), contextHirarchy) };
            } else if (types != null && types.length > 0) {
                // LinkedList.class - without generic definition
                componentTypes = new CompiledType[types.length];
                for (int i = 0; i < types.length; i++) {
                    componentTypes[i] = OBJECT;
                }
            } else {
                componentTypes = EMPTY;
            }
            genericTypesMap = EMPTY_MAP;
        } else if (type instanceof ParameterizedType) {
            this.raw = ReflectionUtils.getRaw(type);
            int i = 0;
            componentTypes = new CompiledType[((ParameterizedType) type).getActualTypeArguments().length];
            HashMap<Type, CompiledType> genericsMap = new HashMap<Type, CompiledType>();
            for (Type cType : ((ParameterizedType) type).getActualTypeArguments()) {
                componentTypes[i] = CompiledType.create(cType, contextHirarchy);
                genericsMap.put(raw.getTypeParameters()[i], componentTypes[i]);
                i++;
            }
            genericTypesMap = Collections.unmodifiableMap(genericsMap);
        } else if (type instanceof GenericArrayType) {
            // See #FlexiMapperTestGenericArrays
            componentTypes = new CompiledType[] { CompiledType.create(((GenericArrayType) type).getGenericComponentType(), contextHirarchy) };
            raw = Array.newInstance(componentTypes[0].raw, 0).getClass();
            genericTypesMap = EMPTY_MAP;
        } else if (type instanceof WildcardType) {
            componentTypes = EMPTY;
            raw = null;
            genericTypesMap = EMPTY_MAP;
        } else if (type instanceof TypeVariable) {
            componentTypes = EMPTY;
            raw = null;
            genericTypesMap = EMPTY_MAP;
        } else {
            throw new IllegalArgumentException("Unknown type " + type);
        }
        if (raw != null) {
            if (raw.isEnum()) {
                // DO not follow deeper.
                this.superType = null;
            } else if (isInterface()) {
                Type[] superC = raw.getGenericInterfaces();
                // TODO
                superType = null;
            } else {
                Type superC = raw.getGenericSuperclass();
                if (superC != null && superC != Object.class) {
                    if (BREAK_HIERARCHY_AT_TYPES.contains(ReflectionUtils.getRaw(superC))) {
                        this.superType = null;
                    } else {
                        this.superType = CompiledType.create(superC, contextHirarchy);
                    }
                } else {
                    this.superType = null;
                }
            }
        } else {
            this.superType = null;
        }
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            // System.out.println("INIT " + this);
            if (readFromSlowCache(type, contextHirarchy) != null) {
                throw new WTFException("DUPE FOUND");
            }
        }
    }

    private static void putToThreadLocalCache(final CompiledType cType) {
        final Type type = cType.type;
        HashMap<Type, List<CompiledType>> cache = INIT_CACHE.get();
        if (cache == null) {
            cache = new HashMap<Type, List<CompiledType>>();
        }
        List<CompiledType> list = cache.get(type);
        if (list == null) {
            list = new LinkedList<CompiledType>();
            list.add(cType);
            cache.put(type, list);
        } else {
            list.add(cType);
        }
        INIT_CACHE.set(cache);
    }

    public static boolean isThreadLocalCacheEmpty() {
        return INIT_CACHE.get() == null;
    }

    private static void removeFromThreadLocalCache(final CompiledType cType) {
        final Type type = cType.type;
        final HashMap<Type, List<CompiledType>> cache = INIT_CACHE.get();
        if (cache != null) {
            final List<CompiledType> list = cache.get(type);
            if (list != null) {
                list.remove(cType);
                if (list.size() == 0) {
                    cache.remove(type);
                }
            }
            if (cache.size() == 0) {
                INIT_CACHE.set(null);
            }
        }
    }

    /**
     * @param compiledType
     */
    private static void pushToSimpleCache(CompiledType cType) {
        removeFromThreadLocalCache(cType);
        if (SIMPLE_CACHE == null) {
            return;
        } else {
            synchronized (CACHE) {
                CACHE.remove(cType.type);
            }
            synchronized (SIMPLE_CACHE) {
                WeakHashMap<Type, CompiledType> newSimple = new WeakHashMap<Type, CompiledType>(SIMPLE_CACHE);
                newSimple.put(cType.type, cType);
                SIMPLE_CACHE = newSimple;
            }
        }
    }

    /**
     * @param contextHirarchy
     * @param compiledType
     */
    private static void putToCache(CompiledType entry) {
        removeFromThreadLocalCache(entry);
        // WARNING! cType is not yet fully initiated, because the method is called from the own constructor
        synchronized (CACHE) {
            List<WeakReference<CompiledType>> list = CACHE.get(entry.type);
            if (list == null) {
                list = new LinkedList<WeakReference<CompiledType>>();
                list.add(new WeakReference<CompiledType>(entry));
                CACHE.put(entry.type, list);
            } else {
                list.add(new WeakReference<CompiledType>(entry));
            }
        }
    }

    /**
     * @return
     */
    public boolean hasGenericsResolvedByContext() {
        return genericsResolvedByContext;
    }

    public static CompiledType createFromCompiledHirarchy(Type type, LinkedList<CompiledType> contextHirarchy) {
        return create(type, convert(contextHirarchy));
    }

    /**
     * @param genericComponentType
     * @param parent
     * @param contextHirarchy
     * @return
     */
    public static CompiledType create(Type type, LinkedList<Type> contextHirarchy) {
        // quick cacheless access to basic clases and primitives
        // PRIMITIVES_AND_BASICS is null during class init
        if (type instanceof Class && PRIMITIVES_AND_BASICS != null) {
            for (CompiledType c : PRIMITIVES_AND_BASICS) {
                if (c.type == type) {
                    return c;
                }
            }
        }
        if (contextHirarchy == null) {
            contextHirarchy = new LinkedList<Type>();
            contextHirarchy.add(type);
        }
        CompiledType cached = readFromCache(type, contextHirarchy);
        if (cached == null) {
            Type resolvedType = type;
            if (resolvedType instanceof TypeVariable) {
                m: while (resolvedType instanceof TypeVariable) {
                    h: for (Iterator<Type> it = contextHirarchy.descendingIterator(); it.hasNext();) {
                        Type context = it.next();
                        while (context != null) {
                            if (context instanceof ParameterizedType) {
                                Class<?> raw = ReflectionUtils.getRaw((context));
                                TypeVariable<?>[] typeParams = raw.getTypeParameters();
                                Type[] actualTypes = ((ParameterizedType) context).getActualTypeArguments();
                                for (int i = 0; i < typeParams.length; i++) {
                                    if (typeParams[i].equals(resolvedType)) {
                                        resolvedType = actualTypes[i];
                                        continue m;
                                    }
                                }
                                context = raw.getGenericSuperclass();
                            } else if (context instanceof Class) {
                                // Something like LInkedList.class
                                TypeVariable[] types = ((Class) context).getTypeParameters();
                                if (types != null) {
                                    for (TypeVariable pseudoObject : types) {
                                        if (pseudoObject == resolvedType) {
                                            resolvedType = Object.class;
                                            continue m;
                                        }
                                    }
                                }
                                context = ((Class) context).getGenericSuperclass();
                            } else {
                                continue h;
                            }
                        }
                    }
                    break;
                }
                if (contextHirarchy.getLast() != resolvedType) {
                    contextHirarchy.add(resolvedType);
                }
                // if (resolvedType instanceof TypeVariable) {
                // System.out.println(97928829312l);
                // }
                cached = readFromCache(resolvedType, contextHirarchy);
                if (cached != null) {
                    return cached;
                }
            }
            cached = new CompiledType(resolvedType, contextHirarchy);
            // DebugMode.logInfoInIDEOnly("Create new: " + type + "->" + cached);
            if (requiresContextToResolveGenerics(resolvedType)) {
                // push to simple cache
                // System.out.println("Simple " + resolvedType + " - " + cached);
                pushToSimpleCache(cached);
            } else {
                putToCache(cached);
                // System.out.println("Complex " + resolvedType + " - " + cached);
            }
        } else {
        }
        return cached;
    }

    /**
     * @param resolvedType
     * @return
     */
    private static boolean requiresContextToResolveGenerics(Type t) {
        if (t instanceof WildcardType) {
            return true;
        } else if (t instanceof Class) {
            return true;
        } else if (t instanceof ParameterizedType) {
            for (Type e : ((ParameterizedType) t).getActualTypeArguments()) {
                if (!requiresContextToResolveGenerics(e)) {
                    return false;
                }
            }
            return true;
        } else if (t instanceof GenericArrayType) {
            return requiresContextToResolveGenerics(((GenericArrayType) t).getGenericComponentType());
        } else {
            return false;
        }
    }

    /**
     * @param contextHirarchy
     * @param type2
     * @return
     */
    private static CompiledType readFromCache(Type type, LinkedList<Type> contextHirarchy) {
        if (SIMPLE_CACHE != null) {
            CompiledType direct = SIMPLE_CACHE.get(type);
            if (direct != null) {
                return direct;
            }
        }
        HashMap<Type, List<CompiledType>> threadLocalCache = INIT_CACHE.get();
        if (threadLocalCache != null) {
            List<CompiledType> list = threadLocalCache.get(type);
            if (list != null) {
                for (Iterator<CompiledType> it = list.iterator(); it.hasNext();) {
                    CompiledType eType = it.next();
                    if (type instanceof Class && ((Class) type).getTypeParameters().length == 0) {
                        // no generic resolving required - we don't need the context
                        return eType;
                    } else if (type instanceof WildcardType) {
                        // ? the context won't help us
                        return eType;
                    }
                    if (eType.contextHirarchy.size() == contextHirarchy.size()) {
                        if (eType.contextHirarchy.equals(contextHirarchy)) {
                            return eType;
                        }
                    }
                }
            }
        }
        return readFromSlowCache(type, contextHirarchy);
    }

    public static CompiledType readFromSlowCache(Type type, LinkedList<Type> contextHirarchy) {
        synchronized (CACHE) {
            List<WeakReference<CompiledType>> list = CACHE.get(type);
            if (list == null) {
                return null;
            }
            for (Iterator<WeakReference<CompiledType>> it = list.iterator(); it.hasNext();) {
                CompiledType eType = it.next().get();
                if (eType == null) {
                    it.remove();
                    continue;
                } else {
                    if (type instanceof Class && ((Class) type).getTypeParameters().length == 0) {
                        // no generic resolving required - we don't need the context
                        return eType;
                    } else if (type instanceof WildcardType) {
                        // ? the context won't help us
                        return eType;
                    }
                    if (eType.contextHirarchy.size() == contextHirarchy.size()) {
                        if (eType.contextHirarchy.equals(contextHirarchy)) {
                            return eType;
                        }
                    }
                }
            }
        }
        return null;
    }

    public static CompiledType create(Type type, Type context) {
        LinkedList<Type> contextHirarchy = new LinkedList<Type>();
        contextHirarchy.add(context);
        if (type != context) {
            contextHirarchy.add(type);
        }
        return create(type, contextHirarchy);
    }

    public static CompiledType create(Type type) {
        return create(type, (LinkedList<Type>) null);
    }

    public static CompiledType create(TypeRef<?> typeRef) {
        return create(typeRef.getType());
    }

    /**
     * @param fieldType
     * @return
     */
    public CompiledType resolveGenericType(Type fieldType) {
        CompiledType lookup = this;
        while (lookup != null) {
            CompiledType ret = lookup.genericTypesMap.get(fieldType);
            if (ret != null) {
                return ret;
            }
            lookup = lookup.superType;
        }
        return null;
    }

    /**
     * @param typeHirarchy
     * @return
     */
    public static CompiledType create(LinkedList<Type> typeHirarchy) {
        return create(typeHirarchy.getLast(), typeHirarchy);
    }

    /**
     * is exactly any of the given types
     *
     * @param class1
     * @return
     */
    public boolean isAnyOf(Type... classes) {
        for (Type c : classes) {
            if (c == this || c == type) {
                return true;
            }
        }
        return false;
    }

    /**
     * returns true of type is primitive OR primitive wrapper
     *
     * @return
     */
    public boolean isPrimitive() {
        return Clazz.isPrimitive(type);
    }

    /**
     * @param class1
     * @param class2
     * @return
     */
    public boolean isNoneOf(Type... classes) {
        for (Type c : classes) {
            if (c == this || c == type) {
                return false;
            }
        }
        return true;
    }

    /**
     * @param autoSkipAnonymous
     *            TODO
     * @return
     */
    public boolean isEnum(boolean autoSkipAnonymous) {
        if (raw == null) {
            return false;
        } else if (autoSkipAnonymous) {
            // somehow Class.isAnonymousClass is pretty slow
            if (isEnum == null) {
                isEnum = Clazz.isEnum(raw);
            }
            return isEnum;
        } else {
            return type instanceof Class && ((Class<?>) type).isEnum();
        }
    }

    /**
     * @return
     */
    public boolean isArray() {
        return raw != null && raw.isArray();
    }

    /**
     * @param b
     * @return
     */
    public boolean isInstanceOf(Type... types) {
        if (raw == null) {
            return false;
        } else {
            for (Type b : types) {
                if (b instanceof Class) {
                    if (((Class<?>) b).isAssignableFrom(raw)) {
                        return true;
                    }
                }
            }
            return false;
        }
    }

    /**
     * @return
     */
    public boolean isBoolean() {
        return this == BOOLEAN_PRIMITIVE || this == BOOLEAN_WRAPPER;
    }

    /**
     * @return
     */
    public boolean isNumber() {
        return Clazz.isNumberType(type);
    }

    /**
     * @return
     */
    public boolean isFixedPointNumber() {
        return Clazz.isFixedPointNumber(type);
    }

    /**
     * @return
     */
    public boolean isFloatingPointNumber() {
        return Clazz.isFloatingPointNumber(type);
    }

    /**
     * @return
     */
    public boolean isString() {
        return this == STRING;
    }

    /**
     * @return
     */
    public boolean isCollection() {
        return raw != null && Collection.class.isAssignableFrom(raw);
    }

    /**
     * @return
     */
    public boolean isInterface() {
        return raw != null && raw.isInterface();
    }

    /**
     * @return
     */
    public boolean isMap() {
        return raw != null && Map.class.isAssignableFrom(raw);
    }

    /**
     * @return
     */
    public boolean isSet() {
        return raw != null && Set.class.isAssignableFrom(raw);
    }

    public boolean isList() {
        return raw != null && List.class.isAssignableFrom(raw);
    }

    /**
     * returns the componentTypes for class1. Example: A<MyType> extends B<String> Extends ArrayList<T> extends AbstractList<T> implements
     * Collection... getComponentTypes(Collection.class) returns the component type of the lowest Collection in the type hirarchy. In this
     * example, we get the Resolved ComponentType of the ArrayList = String
     *
     * @param class1
     * @return
     */
    public CompiledType[] getComponentTypes(Class<?> class1) {
        final CompiledType superClass = getAssignableSuperClass(class1);
        if (superClass == null) {
            return null;
        } else {
            return superClass.componentTypes;
        }
    }

    /**
     * returns the deepest assignable superclass that can be assigned by class1. This may be null, or even this
     *
     * @param class1
     * @return
     */
    public CompiledType getAssignableSuperClass(Class<?> class1) {
        LinkedList<CompiledType> cl = getTypeHirarchy();
        for (Iterator<CompiledType> it = cl.descendingIterator(); it.hasNext();) {
            CompiledType next = it.next();
            if (next.raw != null && class1.isAssignableFrom(next.raw)) {
                return next;
            }
        }
        return null;
    }

    /**
     * @return
     * @throws IllegalAccessException
     * @throws InstantiationException
     */
    public Object newInstance() throws InstantiationException, IllegalAccessException {
        if (raw == null) {
            throw new InstantiationException("Cannot create an instance of " + this);
        } else {
            return raw.newInstance();
        }
    }

    /**
     * @param size
     * @return
     */
    public Object newArrayInstance(int size) {
        return Array.newInstance(raw, size);
    }

    /**
     * @return
     */
    public boolean isObject() {
        return this == OBJECT;
    }

    /**
     * @return
     */
    public ClassCache getClassCache() {
        if (classCache != null) {
            return classCache;
        } else {
            try {
                if (raw == null) {
                    // e.g. Type '?' - unresolved generic
                    return null;
                }
                classCache = ClassCache.getClassCache(raw);
            } catch (SecurityException e) {
                throw new IllegalStateException(e);
            } catch (NoSuchMethodException e) {
                throw new IllegalStateException(e);
            }
            return classCache;
        }
    }

    /**
     * @return
     */
    public boolean isCharacter() {
        return this == CHAR_PRIMITIVE || this == CHAR_WRAPPER;
    }

    /**
     * @return
     */
    public boolean isPrimitiveWrapper() {
        return Clazz.isPrimitiveWrapper(type);
    }

    /**
     * @param typeHirarchy
     * @return
     */
    public static CompiledType createFromCompiledHirarchy(List<CompiledType> typeHirarchy) {
        return create(convert(typeHirarchy));
    }

    public static LinkedList<Type> convert(List<CompiledType> typeHirarchy) {
        LinkedList<Type> hir = new LinkedList<Type>();
        for (CompiledType ct : typeHirarchy) {
            hir.add(ct.type);
        }
        return hir;
    }

    /**
     * @param type2
     * @param type3
     * @return
     */
    public static CompiledType create(Type type, CompiledType context) {
        return create(type, context == null ? null : convert(context.getTypeHirarchy()));
    }

    /**
     * returns if the type is a container for further properties - like array, list, collections, maps, or classes. this method is false if
     * the type is a string, primitive, primitive wrapper or enum
     *
     * @return
     */
    public boolean isContainer() {
        if (isString()) {
            return false;
        } else if (isPrimitive()) {
            return false;
        } else if (isEnum(true)) {
            return false;
        } else {
            return true;
        }
    }

    /**
     * true if this type would represent a js object {}
     *
     * @return
     */
    public boolean isObjectContainer() {
        if (isString()) {
            return false;
        } else if (isPrimitive()) {
            return false;
        } else if (isEnum(true)) {
            return false;
        } else if (isArray()) {
            return false;
        } else if (isCollection()) {
            return false;
        } else {
            return true;
        }
    }

    /**
     * returns true if the type is an array or a collection - represents an array in json - this includes Sets
     *
     * @return
     */
    public boolean isListContainer() {
        return isArray() || isCollection();
    }

    /**
     * @return
     */
    public CompiledType getComponentTypeFor(Class<?>... interfaces) {
        for (Class<?> t : interfaces) {
            if (t == Array.class) {
                if (isArray()) {
                    return componentTypes[0];
                }
            } else if (isInstanceOf(t)) {
                CompiledType[] types = getComponentTypes(t);
                if (CompiledType.create(t).isMap()) {
                    if (types.length == 2) {
                        return types[1];
                    }
                } else {
                    if (types.length == 1) {
                        return types[0];
                    }
                }
                return OBJECT;
            }
        }
        return null;
    }

    /**
     * @param path
     * @return
     */
    public CompiledType resolve(JSPath path) throws CannotResolvePathException {
        CompiledType ret = this;
        // DebugMode.breakIf(path == null, EMPTY);
        for (Object o : path.getElements()) {
            if (ret.isListContainer()) {
                if (o instanceof Number) {
                    ret = ret.getComponentTypeFor(Collection.class, Array.class);
                } else {
                    throw new CannotResolvePathException();
                }
            } else if (ret.isMap()) {
                if (o instanceof String) {
                    ret = ret.getComponentTypeFor(Map.class);
                } else {
                    throw new CannotResolvePathException();
                }
            } else if (ret.isObjectContainer()) {
                if (o instanceof String) {
                    Property prop = ret.getClassCache().getProperty((String) o);
                    if (prop == null) {
                        throw new CannotResolvePathException();
                    }
                    Type genType = prop.getGenericType();
                    ret = CompiledType.create(genType, ret.contextHirarchy);
                    DebugMode.breakIf(prop.type.raw != Object.class && prop.type.raw != ret.raw);
                } else {
                    throw new CannotResolvePathException();
                }
            } else {
                throw new CannotResolvePathException();
            }
        }
        return ret;
    }

    /**
     * @return
     */
    public Object[] getEnumValues() {
        CompiledType search = this;
        // Skip anonymous enum classes
        while (search.raw == null) {
            search = search.superType;
        }
        return search.raw.getEnumConstants();
    }

    /**
     * @param str
     * @return
     */
    public Field getField(String key) {
        if (StringUtils.isEmpty(key)) {
            return null;
        } else {
            CompiledType search = this;
            while (search != null) {
                final Field ret = search.getDeclaredField(key);
                if (ret != null) {
                    return ret;
                } else {
                    search = search.superType;
                }
            }
            return null;
        }
    }

    /**
     * @param key
     * @return
     */
    private Field getDeclaredField(String key) {
        if (raw == null) {
            return null;
        } else if (StringUtils.isEmpty(key)) {
            return null;
        } else {
            try {
                return raw.getDeclaredField(key);
            } catch (NoSuchFieldException e) {
                return null;
            } catch (SecurityException e) {
                return null;
            }
        }
    }

    public final int getListLength(final Object object) {
        if (isArray()) {
            return Array.getLength(object);
        } else if (isCollection()) {
            return ((Collection) object).size();
        } else {
            throw new IllegalStateException(object + " is no List");
        }
    }

    /**
     * @return
     */
    public List<Object> getListElements(final Object object) {
        if (isListContainer()) {
            ArrayList<Object> ret = new ArrayList<Object>();
            if (isArray()) {
                for (int i = 0; i < getListLength(object); i++) {
                    ret.add(Array.get(object, i));
                }
            } else if (isCollection()) {
                for (Object o : ((Collection) object)) {
                    ret.add(o);
                }
            }
            return ret;
        }
        return null;
    }

    /**
     * @param object2
     * @param i
     * @return
     */
    public Object getListElement(Object object, int i) {
        if (isListContainer()) {
            if (isArray()) {
                return Array.get(object, i);
            } else if (isInstanceOf(List.class)) {
                return ((List) object).get(i);
            } else if (isCollection()) {
                for (Object o : ((Collection) object)) {
                    if (i == 0) {
                        return 0;
                    }
                    i--;
                }
            }
        }
        return null;
    }

    /**
     * @param class1
     * @return
     */
    public boolean hasAnnotation(Class<? extends Annotation>... annotations) {
        if (raw == null) {
            return false;
        }
        for (Class<? extends Annotation> a : annotations) {
            if (getClassCache().getAnnotations(null, a).size() > 0) {
                return true;
            }
        }
        return false;
    }

    /**
     * * returns all methods declared in this class, in interfaces or subclasses - private protected and public This list may contain the
     * same method name several times, but with a different "owner"class
     *
     * @return
     */
    public List<Method> listMethods() {
        return listMethods(raw);
    }

    /**
     * returns all methods declared in this class, in interfaces or subclasses - private protected and public This list may contain the same
     * method name several times, but with a different "owner"class
     *
     * @param raw2
     * @return
     */
    private static List<Method> listMethods(Class<?> c) {
        final List<Method> ret = new ArrayList<Method>();
        while (c != null) {
            for (Class<?> i : c.getInterfaces()) {
                ret.addAll(listMethods(i));
            }
            ret.addAll(Arrays.asList(c.getDeclaredMethods()));
            c = c.getSuperclass();
        }
        return ret;
    }

    /**
     * @return
     */
    public CompiledType getComponentType() {
        return getComponentTypeFor(raw);
    }
}
