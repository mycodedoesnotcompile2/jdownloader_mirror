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

import java.lang.annotation.Annotation;
import java.lang.ref.WeakReference;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Proxy;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.WeakHashMap;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.storage.StorableAllowPrivateAccessModifier;
import org.appwork.storage.StorableAllowProtectedAccessModifier;
import org.appwork.storage.StorableDontSerializeDeeper;
import org.appwork.storage.flexijson.mapper.FlexiJsonProperty;
import org.appwork.storage.simplejson.Ignore;
import org.appwork.storage.simplejson.Ignores;
import org.appwork.utils.JVMVersion;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.reflection.CompiledType;

/**
 * @author thomas
 *
 */
public class ClassCache {
    private static final WeakHashMap<Class<?>, ClassCache> CACHE        = new WeakHashMap<Class<?>, ClassCache>();
    private static final Object[]                          EMPTY_OBJECT = new Object[] {};
    private static final Class<?>[]                        EMPTY_TYPES  = new Class[] {};

    protected static ClassCache create(final Class<? extends Object> clazz) throws SecurityException, NoSuchMethodException {
        return create(clazz, null);
    }

    public static class Rules {
        private Class<?> breakAtClass;
        private boolean  ignoreMissingConstructor;

        /**
         * @param ignoreMissingConstructor
         *            the ignoreMissingConstructor to set
         */
        public void setIgnoreMissingConstructor(boolean ignoreMissingConstructor) {
            this.ignoreMissingConstructor = ignoreMissingConstructor;
        }

        public Rules ignoreMissingConstructor(boolean ignoreMissingConstructor) {
            this.ignoreMissingConstructor = ignoreMissingConstructor;
            return this;
        }

        /**
         * @return the breakAtClass
         */
        public Class<?> getBreakAtClass() {
            return breakAtClass;
        }

        /**
         * @param breakAtClass
         *            the breakAtClass to set
         */
        public void setBreakAtClass(Class<?> breakAtClass) {
            this.breakAtClass = breakAtClass;
        }

        public Rules breakAtClass(Class<?> breakAtClass) {
            this.breakAtClass = breakAtClass;
            return this;
        }

        /**
         * @param cls
         * @return
         */
        public boolean isBreakAtClass(Class<? extends Object> cls) {
            final String name = cls.getName();
            if (name.startsWith("java.")) {
                return true;
            } else if (name.startsWith("sun.")) {
                return true;
            } else {
                return false;
            }
        }
    }

    public static String getGetterKey(final Method m) {
        if (m == null) {
            return null;
        } else if (m.getReturnType() == void.class) {
            return null;
        } else {
            if (getParameterCount(m) > 0) {
                return null;
            } else {
                final String name = m.getName();
                if (name.startsWith("get")) {
                    if (name.length() > 3) {
                        return createKey(name.substring(3));
                    } else {
                        return null;
                    }
                } else if (name.startsWith("is")) {
                    if (name.length() > 2) {
                        return createKey(name.substring(2));
                    } else {
                        return null;
                    }
                } else {
                    return null;
                }
            }
        }
    }

    public static int getParameterCount(final Method method) {
        final int ret;
        if (JVMVersion.isMinimum(JVMVersion.JAVA_1_8)) {
            ret = method.getParameterCount();
        } else {
            ret = method.getParameterTypes().length;
        }
        return ret;
    }

    public static String getSetterKey(final Method m) {
        if (m == null) {
            return null;
        } else {
            if (getParameterCount(m) != 1) {
                return null;
            } else {
                final String name = m.getName();
                if (name.startsWith("set")) {
                    if (name.length() > 3) {
                        return createKey(name.substring(3));
                    } else {
                        return null;
                    }
                } else {
                    return null;
                }
            }
        }
    }

    /**
     * @param clazz
     * @return
     * @throws NoSuchMethodException
     * @throws SecurityException
     */
    public static ClassCache create(final Class<? extends Object> clazz, Rules rules) throws SecurityException, NoSuchMethodException {
        final ClassCache cc = new ClassCache(clazz);
        final HashSet<String> ignores = new HashSet<String>();
        typeHierarchy: for (Type t : cc.getTypeHierarchy()) {
            if (t == Object.class) {
                break;
            }
            CompiledType ct = CompiledType.create(t);
            if (ct.raw == null) {
                continue;
            }
            if (rules != null) {
                if (rules.getBreakAtClass() == ct.raw) {
                    break;
                }
                if (rules.isBreakAtClass(ct.raw)) {
                    break;
                }
            }
            final Ignores ig = ct.raw.getAnnotation(Ignores.class);
            if (ig != null) {
                for (final String i : ig.value()) {
                    ignores.add(i);
                }
            }
            HashMap<String, Method> proxyLookup = null;
            if (Proxy.isProxyClass(ct.raw)) {
                proxyLookup = new HashMap<String, Method>();
                for (Class<?> i : ct.raw.getInterfaces()) {
                    for (Method method : i.getDeclaredMethods()) {
                        String id = method.getName();
                        for (Class<?> pt : method.getParameterTypes()) {
                            id += "," + pt.getName();
                        }
                        proxyLookup.put(id, method);
                    }
                }
            }
            final List<Method> methods = new ArrayList<Method>();
            try {
                for (Method m : ct.raw.getDeclaredMethods()) {
                    if (proxyLookup != null) {
                        String id = m.getName();
                        for (Class<?> pt : m.getParameterTypes()) {
                            id += "," + pt.getName();
                        }
                        Method lookedUp = proxyLookup.get(id);
                        if (lookedUp != null) {
                            m = lookedUp;
                        }
                    }
                    if (m.getAnnotation(Ignore.class) != null || ignores.contains(m.toString())) {
                        continue;
                    } else if (Modifier.isStatic(m.getModifiers())) {
                        continue;
                    } else if (getGetterKey(m) != null || getSetterKey(m) != null) {
                        methods.add(m);
                    }
                }
            } catch (NoClassDefFoundError e) {
                throw e;
            }
            java.util.Collections.sort(methods, new Comparator<Method>() {
                @Override
                public int compare(Method o1, Method o2) {
                    return o1.getName().compareTo(o2.getName());
                }
            });
            for (final Method m : methods) {
                final int mods = m.getModifiers();
                if (Modifier.isPrivate(mods) && m.getAnnotation(StorableAllowPrivateAccessModifier.class) == null) {
                    continue;
                } else if (!Modifier.isPrivate(mods) && !Modifier.isPublic(mods) && m.getAnnotation(StorableAllowProtectedAccessModifier.class) == null) {
                    continue;
                }
                final ArrayList<String> alternativeKeys = new ArrayList<String>();
                final Boolean setOrGet;
                String key = null;
                if ((key = getGetterKey(m)) != null) {
                    setOrGet = Boolean.TRUE;
                } else if ((key = getSetterKey(m)) != null) {
                    setOrGet = Boolean.FALSE;
                } else {
                    setOrGet = null;
                }
                if (setOrGet != null) {
                    for (Annotation a : m.getAnnotations()) {
                        addAlternativeKeyFromAnnotation(alternativeKeys, a, key);
                    }
                    final Field field = addAlternativeKeyFromField(ct.raw, m, alternativeKeys, key);
                    if (Boolean.TRUE.equals(setOrGet)) {
                        final Getter g = new Getter(cc, key, alternativeKeys, m, field);
                        cc.allGetter.add(g);
                        if (putMethod(cc.getterMap, g.getKey(), g)) {
                            cc.getter.add(g);
                        }
                        for (String aKey : alternativeKeys) {
                            putMethod(cc.getterMap, aKey, g);
                        }
                    } else {
                        final Setter s = new Setter(cc, key, alternativeKeys, m, field);
                        cc.allSetter.add(s);
                        if (putMethod(cc.setterMap, s.getKey(), s)) {
                            cc.setter.add(s);
                        }
                        for (String aKey : alternativeKeys) {
                            putMethod(cc.setterMap, aKey, s);
                        }
                    }
                }
            }
            if (ct.raw.getAnnotation(StorableDontSerializeDeeper.class) != null) {
                break typeHierarchy;
            }
        }
        //
        // we do not want to serialize object's getter
        for (final Constructor<?> c : clazz.getDeclaredConstructors()) {
            if (c.getParameterTypes().length == 0) {
                try {
                    // DebugMode.breakIf(!Modifier.isPublic(c.getModifiers()), "Forbidden. Please fix this");
                    if (Modifier.isPrivate(c.getModifiers()) && c.getAnnotation(StorableAllowPrivateAccessModifier.class) == null) {
                        continue;
                    }
                    if (!Modifier.isPrivate(c.getModifiers()) && !Modifier.isPublic(c.getModifiers()) && c.getAnnotation(StorableAllowProtectedAccessModifier.class) == null) {
                        continue;
                    }
                    c.setAccessible(true);
                    cc.constructor = c;
                } catch (final java.lang.SecurityException e) {
                    org.appwork.loggingv3.LogV3.log(e);
                }
                break;
            }
        }
        if (cc.constructor == null && (rules != null && !rules.ignoreMissingConstructor)) {
            final int lastIndex = clazz.getName().lastIndexOf(".");
            final String pkg = lastIndex > 0 ? clazz.getName().substring(0, lastIndex) : "";
            if (pkg.startsWith("java.") || pkg.startsWith("sun.")) {
                org.appwork.loggingv3.LogV3.warning("No Null Constructor in " + clazz + " found. De-Json-serial will fail");
            } else if (clazz.isAnonymousClass()) {
                org.appwork.loggingv3.LogV3.warning("No Null Constructor in inline-clazz " + clazz + " found. De-Json-serial will fail");
            } else {
                throw new NoSuchMethodException(" Class " + clazz + " requires a null constructor. please add private " + clazz.getSimpleName() + "(){}");
            }
        }
        return cc;
    }

    /**
     * @param <T>
     * @param getterMap2
     * @param key
     * @param g
     * @return
     */
    private static <T extends GetterOrSetter> boolean putMethod(HashMap<String, T> map, String key, T g) {
        GetterOrSetter exists = map.get(key);
        if (exists != null) {
            if (exists.getMethod().getName().equals(g.getMethod().getName())) {
                // overridden Method. thats ok. keep the "visible" most top level one.
                return false;
            } else {
                throw new WTFException("invalid Key Mapping: " + g + " vs. " + exists);
            }
        }
        map.put(key, g);
        return true;
    }

    protected static Field addAlternativeKeyFromField(Class<? extends Object> cls, final Method m, ArrayList<String> alternativeKeys, final String key) {
        Field field = null;
        final String methodName = m.getName();
        final String searchField = createKey(m);
        final boolean isSetMethod = methodName.startsWith("set");
        while (cls != null && cls != Object.class) {
            try {
                field = cls.getDeclaredField(key);
            } catch (NoSuchFieldException e) {
            }
            if (field == null) {
                try {
                    field = cls.getDeclaredField(methodName);
                } catch (NoSuchFieldException e) {
                }
            }
            if (field == null && searchField != null) {
                try {
                    field = cls.getDeclaredField(searchField);
                } catch (NoSuchFieldException e) {
                    try {
                        field = isSetMethod ? cls.getDeclaredField("is" + searchField) : null;
                    } catch (NoSuchFieldException e2) {
                    }
                }
            }
            if (field != null) {
                if (!key.equals(field.getName()) && alternativeKeys.indexOf(field.getName()) < 0) {
                    alternativeKeys.add(field.getName());
                }
                for (Annotation a : field.getAnnotations()) {
                    addAlternativeKeyFromAnnotation(alternativeKeys, a, key);
                }
                return field;
            }
            cls = cls.getSuperclass();
        }
        return field;
    }

    protected static void addAlternativeKeyFromAnnotation(ArrayList<String> alternativeKeys, Annotation a, String key) {
        if (a instanceof FlexiJsonProperty) {
            add(alternativeKeys, key, ((FlexiJsonProperty) a).value());
        }
        final Class<? extends Annotation> type = a.annotationType();
        try {
            if ("com.google.gson.annotations.SerializedName".equals(type.getName())) {
                // Support for GSON Annotations
                final Method valueMethod = ((Class) type).getMethod("value", new Class[] {});
                final Object value = valueMethod.invoke(a, new Object[] {});
                add(alternativeKeys, key, value);
            } else if ("com.fasterxml.jackson.annotation.JsonProperty".equals(type.getName())) {
                // Support for Jackson Annotation
                final Method valueMethod = ((Class) type).getMethod("value", new Class[] {});
                final Object value = valueMethod.invoke(a, new Object[] {});
                add(alternativeKeys, key, value);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * @param alternativeKeys
     * @param key
     * @param value
     */
    private static void add(ArrayList<String> alternativeKeys, String key, Object value) {
        if (value == null) {
            return;
        } else if (value.getClass() == String[].class) {
            alternativeKeys.addAll(Arrays.asList((String[]) value));
            return;
        } else if (!(value instanceof String)) {
            return;
        } else if (key.equals(value)) {
            return;
        } else if (alternativeKeys.indexOf((value)) >= 0) {
            return;
        } else {
            alternativeKeys.add((String) value);
        }
    }

    private List<Type> typeHierarchy;

    /**
     * This is the type hierarchy from top to bottom.
     *
     * A extends B extends C extends Object implements Inter ---> A,V,C,Object,Inter
     *
     * the hierarchy contains Parameterized types and raw classes [class org.appwork.utils.KeyValueStringEntry,
     * org.appwork.utils.KeyValueEntry<java.lang.String, java.lang.String>, interface org.appwork.storage.Storable, class
     * org.appwork.utils.KeyValueEntry, org.appwork.utils.ValueEntry<ValueType>, class org.appwork.utils.ValueEntry, class java.lang.Object]
     */
    public List<Type> getTypeHierarchy() {
        if (typeHierarchy == null) {
            initTypeHierarchy();
        }
        return typeHierarchy;
    }

    /**
     * @return the parameterizedTypesMap
     */
    public Map<Class, ParameterizedType> getParameterizedTypesMap() {
        if (typeHierarchy == null) {
            initTypeHierarchy();
        }
        return parameterizedTypesMap;
    }

    /**
     * this methods fills these fields:
     *
     * this.typeHierarchy - a list of all (gerneric) types in the class hierarchy and interfaces this.extendedTypesMap - a reverse extension
     * list. Maps superclass to its childclass. WARNING: This map is not correct for interfaces, because more than one class in the
     * typehierarchy may implement the same interface this.parameterizedTypesMap - maps a raw class to its generic type;
     *
     * @return
     */
    private void initTypeHierarchy() {
        LinkedHashMap<Class, ParameterizedType> parameterizedTypesMap = new LinkedHashMap<Class, ParameterizedType>();
        LinkedHashMap<Type, Type> extendedTypesMap = new LinkedHashMap<Type, Type>();
        ArrayList<Type> types = new ArrayList<Type>();
        Type start = getCachedClass();
        if (start == null) {
            throw new WTFException();
        }
        types.add(start);
        while (true) {
            Type sClass = null;
            if (start instanceof Class) {
                sClass = ((Class) start).getGenericSuperclass();
                if (sClass != null) {
                    if (extendedTypesMap.put(sClass, start) == null) {
                        types.add(sClass);
                    }
                } else {
                    // If this Class represents either the Objectclass, an interface, a primitive type, or void, then null isreturned
                }
                for (Type ifs : ((Class) start).getGenericInterfaces()) {
                    if (extendedTypesMap.put(ifs, start) == null) {
                        types.add(ifs);
                    }
                    followInterfaces(types, extendedTypesMap, ifs);
                }
            } else if (start instanceof ParameterizedType) {
                Type r = ((ParameterizedType) start).getRawType();
                if (r instanceof Class) {
                    sClass = ((Class) r).getGenericSuperclass();
                    parameterizedTypesMap.put((Class) r, (ParameterizedType) start);
                    if (extendedTypesMap.put(sClass, start) == null) {
                        types.add(sClass);
                    }
                    for (Type ifs : ((Class) r).getGenericInterfaces()) {
                        if (extendedTypesMap.put(ifs, start) == null) {
                            types.add(ifs);
                        }
                        followInterfaces(types, extendedTypesMap, ifs);
                    }
                } else {
                    LogV3.I().getDefaultLogger().log(new Exception("This should not happen. " + clazz.get()));
                    extendedTypesMap.put(r, start);
                }
            } else {
                // GenericArrayType
                // TODO: For example, List.toArray(T[]) -> T[]
                // TODO: public List<String>[] array;
                // TypeVariable
            }
            if (sClass != null && sClass instanceof ParameterizedType) {
                Type r = ((ParameterizedType) sClass).getRawType();
                if (extendedTypesMap.put(r, start) == null) {
                    types.add(r);
                }
                if (r instanceof Class) {
                    for (Type ifs : ((Class) r).getGenericInterfaces()) {
                        if (extendedTypesMap.put(ifs, start) == null) {
                            types.add(ifs);
                        }
                        followInterfaces(types, extendedTypesMap, ifs);
                    }
                }
            }
            if (sClass == null) {
                break;
            }
            start = sClass;
        }
        this.parameterizedTypesMap = Collections.unmodifiableMap(parameterizedTypesMap);
        this.extendedTypesMap = Collections.unmodifiableMap(extendedTypesMap);
        this.typeHierarchy = Collections.unmodifiableList(types);
    }

    /**
     * @param types
     * @param extendedTypesMap2
     * @param ifs
     */
    private void followInterfaces(ArrayList<Type> types, HashMap<Type, Type> extendedTypesMap, Type start) {
        if (start instanceof Class) {
            for (Class ifs : ((Class) start).getInterfaces()) {
                if (extendedTypesMap.put(ifs, start) == null) {
                    types.add(ifs);
                }
                followInterfaces(types, extendedTypesMap, ifs);
            }
        } else if (start instanceof ParameterizedType) {
            Type r = ((ParameterizedType) start).getRawType();
            if (r instanceof Class) {
                for (Class ifs : ((Class) r).getInterfaces()) {
                    if (extendedTypesMap.put(ifs, start) == null) {
                        types.add(ifs);
                    }
                    followInterfaces(types, extendedTypesMap, ifs);
                }
            } else {
                throw new WTFException();
            }
        }
    }

    /**
     *
     * Jackson maps methodnames to keys like this. setID becomes key "id" , setMethodName becomes "methodName". To keep compatibility
     * between jackson and simplemapper, we should do it the same way
     *
     * @param substring
     * @return
     */
    public static String createKey(final String key) {
        if (key.length() == 0) {
            return null;
        } else {
            final char[] ca = key.toCharArray();
            final StringBuilder sb = new StringBuilder(ca.length);
            boolean starter = true;
            boolean lowerCase = false;
            for (int i = 0; i < ca.length; i++) {
                if (starter && Character.isUpperCase(ca[i])) {
                    lowerCase = true;
                    sb.append(Character.toLowerCase(ca[i]));
                } else {
                    starter = false;
                    sb.append(ca[i]);
                }
            }
            if (lowerCase) {
                return sb.toString();
            } else {
                return key;
            }
        }
    }

    /**
     * @param clazz
     * @return
     * @throws NoSuchMethodException
     * @throws SecurityException
     */
    public synchronized static ClassCache getClassCache(final Type type) throws SecurityException, NoSuchMethodException {
        Class<?> clazz = ReflectionUtils.getRaw(type);
        if (clazz == null) {
            throw new IllegalStateException(type + " has now raw Class.");
        }
        ClassCache cc = ClassCache.CACHE.get(clazz);
        if (cc == null) {
            // LogV3.logger(ClassCache.class).finer("ClassCache: " + clazz);
            cc = ClassCache.create(clazz);
            ClassCache.CACHE.put(clazz, cc);
        }
        return cc;
    }

    protected Constructor<? extends Object>                constructor;
    protected final WeakReference<Class<? extends Object>> clazz;
    protected final java.util.List<Getter>                 getter;
    protected final java.util.List<Setter>                 setter;
    protected final java.util.List<Getter>                 allGetter;
    protected final java.util.List<Setter>                 allSetter;

    /**
     * Returns all getters - may contain the same key several times. the highest level in the inheritance hierarchy comes first
     *
     * @return
     */
    public java.util.List<Getter> getAllGetter() {
        return allGetter;
    }

    /**
     * Returns all setters - may contain the same key several times. the highest level in the inheritance hierarchy comes first
     *
     * @return
     */
    public java.util.List<Setter> getAllSetter() {
        return allSetter;
    }

    /**
     * Do not change to a map type that does not obey order
     */
    protected final LinkedHashMap<String, Getter> getterMap;
    /**
     * Do not change to a map type that does not obey order
     */
    protected final LinkedHashMap<String, Setter> setterMap;
    // * Gets the extended Type. example: class A extends B -> getExtendedType(B.class) = A.class
    // class a extends B extends C -> C-->B B-->A this works for multiple interfaces as well.
    /**
     * {org.appwork.utils.KeyValueEntry<java.lang.String, java.lang.String>=class org.appwork.utils.KeyValueStringEntry, interface
     * org.appwork.storage.Storable=class org.appwork.utils.KeyValueStringEntry, class org.appwork.utils.KeyValueEntry=class
     * org.appwork.utils.KeyValueStringEntry, org.appwork.utils.ValueEntry<ValueType>=org.appwork.utils.KeyValueEntry<java.lang.String,
     * java.lang.String>, class org.appwork.utils.ValueEntry=org.appwork.utils.KeyValueEntry<java.lang.String, java.lang.String>, class
     * java.lang.Object=org.appwork.utils.ValueEntry<ValueType>}
     *
     */
    private Map<Type, Type>                       extendedTypesMap;
    /**
     * Contains a map of raw class -> Parameterized Type {class
     * org.appwork.utils.KeyValueEntry=org.appwork.utils.KeyValueEntry<java.lang.String, java.lang.String>, class
     * org.appwork.utils.ValueEntry=org.appwork.utils.ValueEntry<ValueType>}
     */
    private Map<Class, ParameterizedType>         parameterizedTypesMap;

    /**
     * returns a key->Getter Map that contains each key only once. The key maps to the "visible/highest" method in the inheritance hierarchy
     *
     * @return
     */
    public Map<String, Getter> getGetterMap() {
        return getterMap;
    }

    /**
     * returns a key->Setter Map that contains each key only once. The key maps to the "visible/highest" method in the inheritance hierarchy
     *
     * @return
     */
    public Map<String, Setter> getSetterMap() {
        return setterMap;
    }

    /**
     * @param clazz
     */
    protected ClassCache(final Class<? extends Object> clazz) {
        this.clazz = new WeakReference<Class<? extends Object>>(clazz);
        getter = new ArrayList<Getter>();
        setter = new ArrayList<Setter>();
        allSetter = new ArrayList<Setter>();
        allGetter = new ArrayList<Getter>();
        getterMap = new LinkedHashMap<String, Getter>();
        setterMap = new LinkedHashMap<String, Setter>();
    }

    public Class<? extends Object> getCachedClass() {
        return clazz.get();
    }

    /**
     * @return the parentsMap
     */
    private Map<Type, Type> getExtendedTypesMap() {
        if (typeHierarchy == null) {
            initTypeHierarchy();
        }
        return extendedTypesMap;
    }

    public java.util.List<Getter> getGetter() {
        return getter;
    }

    public Getter getGetter(final String key) {
        return getterMap.get(key);
    }

    /**
     * @return
     * @throws InvocationTargetException
     * @throws IllegalAccessException
     * @throws InstantiationException
     * @throws IllegalArgumentException
     */
    public Object getInstance() throws IllegalArgumentException, InstantiationException, IllegalAccessException, InvocationTargetException {
        if (constructor == null) {
            throw new IllegalStateException("The class " + getCachedClass() + " has no empty constructor");
        } else {
            return constructor.newInstance(ClassCache.EMPTY_OBJECT);
        }
    }

    public java.util.List<Setter> getSetter() {
        return setter;
    }

    public Setter getSetter(final String key) {
        return setterMap.get(key);
    }

    /**
     * @return
     */
    public Set<String> getKeys() {
        final LinkedHashSet<String> ret = new LinkedHashSet<String>();
        ret.addAll(getterMap.keySet());
        ret.addAll(setterMap.keySet());
        return ret;
    }

    /**
     * If the parameter cls is type of this classcache (is part of the class hierarchy), this will return the parameterized instance.<br>
     *
     * example: A extends B<String> getParameterizedType(B.class) =B<String>
     *
     * @param cls
     * @return
     *
     */
    public ParameterizedType getParameterizedType(Class cls) {
        return getParameterizedTypesMap().get(cls);
    }

    /**
     * Gets the extended Type. example: class A extends B -> getExtendedType(B.class) = A.class
     *
     * @param cls
     * @return
     */
    public Type getExtendedType(Class cls) {
        return getExtendedTypesMap().get(cls);
    }

    public <TT extends Annotation> List<TT> getMethodAnnotations(final Method method, Class<TT> class1) {
        String key = createKey(method);
        if (key == null) {
            key = method.getName();
        }
        return getAnnotations(key, class1);
    }

    private HashMap<String, List<? extends Annotation>> annotationsCache = new HashMap<String, List<? extends Annotation>>();

    /**
     * returns all annotations found allong the class hierarchy. if key != null, method declarations are searched, else the classes and
     * interfaces. This method never returns null. If no annotations are found, a list of size 0 is returned.
     *
     * @param <TT>
     * @param key
     * @param class1
     * @return
     */
    public <TT extends Annotation> List<TT> getAnnotations(final String key, final Class<TT> class1) {
        final String cacheKey = key + "_" + class1.getName();
        synchronized (annotationsCache) {
            final List<? extends Annotation> ret = annotationsCache.get(cacheKey);
            if (ret != null) {
                return (List<TT>) ret;
            }
        }
        // TODO: Build cache.. because this is very slow!
        getExtendedTypesMap();
        final Getter g = key == null ? null : getGetter(key);
        final Setter s = key == null ? null : getSetter(key);
        final ArrayList<TT> ret = new ArrayList<TT>();
        for (Type t : typeHierarchy) {
            if (!(t instanceof Class)) {
                continue;
            }
            final Class<?> c = (Class<?>) t;
            if (g != null) {
                try {
                    final Method method = c.getDeclaredMethod(g.getMethod().getName(), g.getMethod().getParameterTypes());
                    if (method != null) {
                        final TT an = method.getAnnotation(class1);
                        if (an != null) {
                            ret.add(an);
                        }
                    }
                } catch (NoSuchMethodException e) {
                } catch (SecurityException e) {
                    throw new WTFException();
                }
            }
            if (s != null) {
                try {
                    final Method method = c.getDeclaredMethod(s.getMethod().getName(), s.getMethod().getParameterTypes());
                    if (method != null) {
                        final TT an = method.getAnnotation(class1);
                        if (an != null) {
                            ret.add(an);
                        }
                    }
                } catch (NoSuchMethodException e) {
                } catch (SecurityException e) {
                    throw new WTFException();
                }
            }
            if (key != null) {
                try {
                    final Field field = c.getDeclaredField(key);
                    if (field != null) {
                        final TT an = field.getAnnotation(class1);
                        if (an != null) {
                            ret.add(an);
                        }
                    }
                } catch (NoSuchFieldException e) {
                } catch (SecurityException e) {
                    throw new WTFException();
                }
            }
            if (key == null) {
                final TT an = ReflectionUtils.getAnnotation(t, class1);
                if (an != null) {
                    ret.add(an);
                }
            }
        }
        synchronized (annotationsCache) {
            annotationsCache.put(cacheKey, ret);
        }
        return ret;
    }

    /**
     * @return
     */
    public Type getType(String key) {
        final Getter g = getGetter(key);
        if (g != null) {
            return g.getMethod().getGenericReturnType();
        } else {
            final Setter s = getSetter(key);
            if (s != null) {
                return s.getMethod().getGenericParameterTypes()[0];
            } else {
                throw new WTFException(key);
            }
        }
    }

    /**
     * @param method
     * @return
     */
    public static String createKey(Method method) {
        final String name = method.getName();
        if (name.length() > 3 && (name.startsWith("get") || name.startsWith("set"))) {
            return createKey(name.substring(3));
        } else if (name.length() > 2 && name.startsWith("is")) {
            return createKey(name.substring(2));
        } else {
            return null;
        }
    }

    @Override
    public String toString() {
        return "ClassCache " + getCachedClass();
    }

    /**
     * @param value
     * @return
     */
    public Property getProperty(String key) {
        Getter g = getGetter(key);
        Setter s = getSetter(key);
        if (g == null && s == null) {
            return null;
        }
        return new Property(this, key, CompiledType.create(getType(key), this.clazz.get()), g, s);
    }
}
