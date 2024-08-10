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
package org.appwork.utils;

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.lang.reflect.WildcardType;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URL;
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.jar.JarEntry;
import java.util.jar.JarInputStream;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.simplejson.mapper.ClassCache;
import org.appwork.storage.simplejson.mapper.Getter;
import org.appwork.utils.reflection.Clazz;
import org.appwork.utils.reflection.CompiledType;

public class ReflectionUtils {
    public static interface WalkThroughObjectCallBack {
        /**
         * @param path
         * @param obj
         * @param obj2
         */
        public void onObject(String path, Type type, Object obj);
    }

    /**
     * Walk through an object and call the callback for every object found
     *
     * @param obj
     * @param path
     * @param callback
     */
    public static void walkThroughObject(Object obj, WalkThroughObjectCallBack callback) {
        walkThroughObject(obj, obj == null ? void.class : obj.getClass(), "", callback);
    }

    /**
     * Walk through an object and call the callback for every object found
     *
     * @param obj
     * @param path
     * @param callback
     */
    public static void walkThroughObject(Object obj, Type type, String path, WalkThroughObjectCallBack callback) {
        if (path == null) {
            path = "";
        }
        if (type == null) {
            if (obj != null) {
                type = obj.getClass();
            } else {
                type = Object.class;
            }
        }
        callback.onObject(path, type, obj);
        if (StringUtils.isNotEmpty(path)) {
            path += ".";
        }
        if (obj == null) {
        } else if (ReflectionUtils.isListOrArray(obj)) {
            final int l1 = ReflectionUtils.getListLength(obj);
            Type actualTypes = null;
            if (obj instanceof List) {
                if (type instanceof ParameterizedType) {
                    actualTypes = ((ParameterizedType) type).getActualTypeArguments()[0];
                }
            } else if (type instanceof Class && ((Class) type).isArray()) {
                actualTypes = ((Class) type).getComponentType();
            } else {
                throw new WTFException("Not Supported");
            }
            for (int i = 0; i < l1; i++) {
                walkThroughObject(ReflectionUtils.getListElement(obj, i), actualTypes, path + i, callback);
            }
        } else if (obj instanceof Map) {
            Type actualTypes = null;
            if (type instanceof ParameterizedType) {
                actualTypes = ((ParameterizedType) type).getActualTypeArguments()[1];
            }
            for (Entry<?, ?> es : ((Map<?, ?>) obj).entrySet()) {
                walkThroughObject(es.getValue(), actualTypes, path + es.getKey(), callback);
            }
        } else {
            if (Clazz.isPrimitive(obj.getClass()) || Clazz.isEnum(obj.getClass()) || Clazz.isString(obj.getClass())) {
                return;
            }
            ClassCache cc;
            try {
                cc = ClassCache.getClassCache(obj.getClass());
                for (Getter c : cc.getGetter()) {
                    walkThroughObject(c.getValue(obj), c.getMethod().getGenericReturnType(), path + c.getKey(), callback);
                }
            } catch (SecurityException e) {
                throw new WTFException(e);
            } catch (NoSuchMethodException e) {
                throw new WTFException(e);
            } catch (IllegalArgumentException e) {
                throw new WTFException(e);
            } catch (IllegalAccessException e) {
                throw new WTFException(e);
            } catch (InvocationTargetException e) {
                throw new WTFException(e);
            }
        }
    }

    // TODO: make weak
    /**
     * @param <T>
     * @param name
     * @param object
     * @param class1
     * @return
     */
    public static <T> List<Class<? extends T>> getClassesInPackage(final ClassLoader cl, final String name, final Pattern pattern, final Class<T> class1) {
        Enumeration<URL> found;
        final List<T> ret = new ArrayList<T>();
        try {
            final String finalName = name.replace(".", "/");
            found = cl.getResources(finalName);
            while (found.hasMoreElements()) {
                final URL url = found.nextElement();
                if (url.getProtocol().equalsIgnoreCase("jar")) {
                    final String path = url.getPath();
                    final File jarFile = new File(new URL(path.substring(0, path.lastIndexOf('!'))).toURI());
                    JarInputStream jis = null;
                    try {
                        jis = new JarInputStream(new FileInputStream(jarFile));
                        JarEntry e;
                        while ((e = jis.getNextJarEntry()) != null) {
                            if (!e.getName().endsWith(".class")) {
                                continue;
                            }
                            if (!e.getName().startsWith(finalName)) {
                                continue;
                            }
                            // try {
                            if (pattern != null) {
                                final Matcher matcher = pattern.matcher(e.getName());
                                if (!matcher.matches()) {
                                    continue;
                                }
                            }
                            String classPath = e.getName().replace("/", ".");
                            classPath = classPath.substring(0, classPath.length() - 6);
                            try {
                                final Class<?> clazz = cl.loadClass(classPath);
                                if (class1 == clazz) {
                                    continue;
                                }
                                if (class1 == null || class1.isAssignableFrom(clazz)) {
                                    ret.add((T) clazz);
                                }
                            } catch (final Throwable ee) {
                            }
                        }
                    } finally {
                        try {
                            jis.close();
                        } catch (final Throwable e) {
                        }
                    }
                } else {
                    final File path = new File(url.toURI());
                    final int i = path.getAbsolutePath().replace("\\", "/").indexOf(finalName);
                    final File root = new File(path.getAbsolutePath().substring(0, i));
                    final List<File> files = Files.getFiles(new FileFilter() {
                        @Override
                        public boolean accept(final File pathname) {
                            if (!pathname.getName().endsWith(".class")) {
                                return false;
                            }
                            final String rel = Files.getRelativePath(root, pathname);
                            if (pattern != null) {
                                final Matcher matcher = pattern.matcher(rel);
                                if (!matcher.matches()) {
                                    return false;
                                }
                            }
                            return true;
                        }
                    }, new File(url.toURI()));
                    for (final File classFile : files) {
                        String classPath = Files.getRelativePath(root, classFile).replace("/", ".").replace("\\", ".");
                        classPath = classPath.substring(0, classPath.length() - 6);
                        try {
                            final Class<?> clazz = cl.loadClass(classPath);
                            if (class1 == clazz) {
                                continue;
                            }
                            if (class1 == null || class1.isAssignableFrom(clazz)) {
                                ret.add((T) clazz);
                            }
                        } catch (final Throwable ee) {
                        }
                    }
                    //
                }
            }
        } catch (final Exception e2) {
            e2.printStackTrace();
        }
        return (List<Class<? extends T>>) ret;
    }

    private static final HashMap<Class<?>, Collection<GetterSetter>> GETTER_SETTER_CACHE = new HashMap<Class<?>, Collection<GetterSetter>>();

    /**
     * @return
     */
    public static Collection<GetterSetter> getGettersSetteres(Class<?> clazz) {
        Collection<GetterSetter> ret = GETTER_SETTER_CACHE.get(clazz);
        if (ret != null) {
            return ret;
        }
        final Class<?> org = clazz;
        synchronized (GETTER_SETTER_CACHE) {
            ret = GETTER_SETTER_CACHE.get(clazz);
            if (ret != null) {
                return ret;
            }
            final HashMap<String, GetterSetter> map = new HashMap<String, GetterSetter>();
            while (clazz != null) {
                for (final Method m : clazz.getDeclaredMethods()) {
                    String key = null;
                    boolean getter = false;
                    if (m.getName().startsWith("is") && Clazz.isBoolean(m.getReturnType()) && m.getParameterTypes().length == 0) {
                        key = (m.getName().substring(2));
                        getter = true;
                    } else if (m.getName().startsWith("get") && m.getParameterTypes().length == 0) {
                        key = (m.getName().substring(3));
                        getter = true;
                    } else if (m.getName().startsWith("set") && m.getParameterTypes().length == 1) {
                        key = (m.getName().substring(3));
                        getter = false;
                    }
                    if (StringUtils.isNotEmpty(key)) {
                        final String unmodifiedKey = key;
                        key = ClassCache.createKey(key);
                        GetterSetter v = map.get(key);
                        if (v == null) {
                            v = new GetterSetter(key);
                            map.put(key, v);
                        }
                        if (getter) {
                            v.setGetter(m);
                        } else {
                            v.setSetter(m);
                        }
                        Field field;
                        try {
                            field = clazz.getField(unmodifiedKey.substring(0, 1).toLowerCase(Locale.ENGLISH) + unmodifiedKey.substring(1));
                            v.setField(field);
                        } catch (final NoSuchFieldException e) {
                        }
                    }
                }
                clazz = clazz.getSuperclass();
            }
            GETTER_SETTER_CACHE.put(org, map.values());
            return GETTER_SETTER_CACHE.get(org);
        }
    }

    /**
     * @param type
     * @return
     * @throws SecurityException
     * @throws NoSuchMethodException
     * @throws InvocationTargetException
     * @throws IllegalArgumentException
     * @throws IllegalAccessException
     */
    public static Object[] getEnumValues(Class<? extends Enum> type) throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchMethodException, SecurityException {
        while (type.isAnonymousClass()) {
            type = (Class<? extends Enum>) type.getSuperclass();
        }
        return type.getEnumConstants();
    }

    /**
     * @param type
     * @param value
     * @return
     * @throws SecurityException
     * @throws NoSuchMethodException
     * @throws InvocationTargetException
     * @throws IllegalArgumentException
     * @throws IllegalAccessException
     */
    public static Object getEnumValueOf(Class<? extends Enum> type, final String value) throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchMethodException, SecurityException {
        while (type.isAnonymousClass()) {
            type = (Class<? extends Enum>) type.getSuperclass();
        }
        return Enum.valueOf(type, value);
    }

    public static interface InvokeMethodLookup<T> {
        public boolean lookup(Class<?> clazz, final String methodName, final Object instance, Class<T> returnType, Object... params);
    }

    public static <T> T getFieldValue(final String className, final String fieldName, final Object instance, Class<T> returnType) throws ClassNotFoundException, InvocationTargetException, NoSuchFieldException {
        final Class<?> clazz = Class.forName(className);
        return getFieldValue(clazz, fieldName, instance, returnType);
    }

    @SuppressWarnings("unchecked")
    public static <T> T getFieldValue(final Class<?> clazz, final String fieldName, final Object instance, Class<T> returnType) throws InvocationTargetException, NoSuchFieldException {
        final Field field;
        try {
            // java.langNoSuchFieldException
            field = clazz.getDeclaredField(fieldName);
            // java.lang.reflect.InaccessibleObjectException
            field.setAccessible(true);
        } catch (SecurityException e) {
            throw initCause(new NoSuchFieldException("Class:" + clazz + "|Field:" + fieldName + "|Type:" + returnType), e);
        } catch (RuntimeException e) {
            // java.lang.reflect.InaccessibleObjectException
            throw initCause(new NoSuchFieldException("Class:" + clazz + "|Field:" + fieldName + "|Type:" + returnType), e);
        }
        try {
            final Object returnValue = field.get(instance);
            if (returnValue == null) {
                return null;
            } else if (!returnType.isInstance(returnValue)) {
                return (T) returnValue;
            } else {
                return returnType.cast(returnValue);
            }
        } catch (IllegalAccessException e) {
            throw new InvocationTargetException(e);
        } catch (IllegalArgumentException e) {
            throw new InvocationTargetException(e);
        } catch (ClassCastException e) {
            throw new InvocationTargetException(e);
        }
    }

    // TODO: replace with Exceptions.initCause in far future to stay compatible
    private static <E extends Throwable> E initCause(E throwing, Throwable cause) {
        try {
            throwing.initCause(cause);
            return throwing;
        } catch (Throwable ex) {
            return Exceptions.addSuppressed(throwing, cause);
        }
    }

    public static Field getField(final String className, final String fieldName, final Object instance, Class<?> type) throws ClassNotFoundException, NoSuchFieldException {
        final Class<?> clazz = Class.forName(className);
        return getField(clazz, fieldName, instance, type);
    }

    public static Field getField(final Class<?> clazz, final String fieldName, final Object instance, Class<?> type) throws NoSuchFieldException {
        final Field field;
        try {
            // java.langNoSuchFieldException
            field = clazz.getDeclaredField(fieldName);
            // java.lang.reflect.InaccessibleObjectException
            field.setAccessible(true);
            if (type == null || type.isAssignableFrom(field.getType())) {
                return field;
            } else {
                return null;
            }
        } catch (SecurityException e) {
            throw initCause(new NoSuchFieldException("Class:" + clazz + "|Field:" + fieldName + "|Type:" + type), e);
        } catch (RuntimeException e) {
            throw initCause(new NoSuchFieldException("Class:" + clazz + "|Field:" + fieldName + "|Type:" + type), e);
        }
    }

    public static <T> T invoke(final String className, final String methodName, final Object instance, Class<T> returnType, Class<?> paramTypes[], Object... params) throws InvocationTargetException {
        return invoke(className, null, methodName, instance, returnType, paramTypes, params);
    }

    public static <T> T invoke(final String className, final String methodName, final Object instance, Class<T> returnType, Object... params) throws InvocationTargetException {
        return invoke(className, null, methodName, instance, returnType, null, params);
    }

    public static <T> T invoke(final Class<?> clazz, final String methodName, final Object instance, Class<T> returnType, Object... params) throws InvocationTargetException {
        return invoke(clazz, null, methodName, instance, returnType, null, params);
    }

    public static <T> T invoke(final String className, final InvokeMethodLookup<T> methodLookup, final String methodName, final Object instance, Class<T> returnType, Class<?> paramTypes[], Object... params) throws InvocationTargetException {
        final Class<?> clazz;
        try {
            clazz = Class.forName(className);
        } catch (ClassNotFoundException e) {
            throw new InvocationTargetException(e);
        }
        return invoke(clazz, methodLookup, methodName, instance, returnType, paramTypes, params);
    }

    @SuppressWarnings("unchecked")
    public static <T> T invoke(Class<?> clazz, final InvokeMethodLookup<T> methodLookup, final String methodName, final Object instance, Class<T> returnType, Class<?> paramTypes[], Object... params) throws InvocationTargetException {
        final Method method;
        try {
            method = findMatchingMethod(clazz, methodLookup, methodName, instance, returnType, paramTypes, params);
        } catch (NoSuchMethodException e) {
            throw new InvocationTargetException(e);
        }
        try {
            final Object returnValue = method.invoke(instance, params);
            if (returnValue == null) {
                return null;
            } else if (!returnType.isInstance(returnValue)) {
                return (T) returnValue;
            } else {
                return returnType.cast(returnValue);
            }
        } catch (IllegalAccessException e) {
            throw new InvocationTargetException(e);
        } catch (IllegalArgumentException e) {
            throw new InvocationTargetException(e);
        } catch (ClassCastException e) {
            throw new InvocationTargetException(e);
        }
    }

    public static <T> Method findMatchingMethod(Class<?> clazz, final InvokeMethodLookup<T> methodLookup, final String methodName, final Object instance, Class<T> returnType, Class<?> paramTypes[], Object... params) throws NoSuchMethodException {
        Method method = null;
        NoSuchMethodException firstNoSuchMethodException = null;
        while (true) {
            try {
                if (paramTypes != null && paramTypes.length > 0) {
                    method = clazz.getDeclaredMethod(methodName, paramTypes);
                } else {
                    method = findMatchinMethodInternal(clazz, methodName, params);
                }
                try {
                    // java.lang.reflect.InaccessibleObjectException
                    method.setAccessible(true);
                } catch (RuntimeException e) {
                    throw Exceptions.addSuppressed(new NoSuchMethodException("Class:" + clazz + "|MethodName:" + methodName + "|Type:" + returnType), e);
                }
                break;
            } catch (NoSuchMethodException e) {
                if (firstNoSuchMethodException == null) {
                    firstNoSuchMethodException = e;
                }
                final Class<?> next = clazz.getSuperclass();
                if (next == null || (methodLookup != null && !methodLookup.lookup(next, methodName, instance, returnType, params))) {
                    throw firstNoSuchMethodException;
                } else {
                    clazz = next;
                }
            }
        }
        return method;
    }

    public static Method findMatchingMethod(final String className, String methodName, Object[] params) throws ClassNotFoundException, NoSuchMethodException {
        final Class<?> clazz = Class.forName(className);
        return findMatchingMethod(clazz, methodName, params);
    }

    /**
     * Searched a method on class that matches the name and the given parameters
     *
     * @param clazz
     * @param params
     * @return
     * @throws NoSuchMethodException
     */
    public static Method findMatchingMethod(final Class<?> clazz, final String methodName, final Object[] params) throws NoSuchMethodException {
        return findMatchingMethod(clazz, null, methodName, null, null, null, params);
    }

    private static Method findMatchinMethodInternal(final Class<?> clazz, final String methodName, final Object[] params) throws NoSuchMethodException {
        final ArrayList<Method> methods = new ArrayList<Method>();
        main: for (Method m : clazz.getDeclaredMethods()) {
            Class<?>[] methodParameterTypes = null;
            if ((JVMVersion.isMinimum(JVMVersion.JAVA_1_8) ? params.length == m.getParameterCount() : ((methodParameterTypes = m.getParameterTypes()).length == params.length)) && m.getName().equals(methodName)) {
                if (methodParameterTypes == null) {
                    methodParameterTypes = m.getParameterTypes();
                }
                for (int i = 0; i < methodParameterTypes.length; i++) {
                    final Object param = params[i];
                    final Class<?> expected = methodParameterTypes[i];
                    if (!Clazz.objectIsTypeOf(param, expected)) {
                        continue main;
                    }
                }
                methods.add(m);
            }
        }
        if (methods.size() == 0) {
            throw new NoSuchMethodException("Could not find a matching method:'" + methodName + "|parameters:" + Arrays.asList(params) + "|class:" + clazz);
        } else if (methods.size() > 1) {
            throw new NoSuchMethodException("Ambigious method definitions: " + methods + "|class:" + clazz);
        } else {
            return methods.get(0);
        }
    }

    public static boolean isInstanceOf(final String className, final Class<?> clazz) {
        Class<?> walk = clazz;
        while (walk != null) {
            if (walk.getName().equals(className)) {
                return true;
            } else {
                final Class<?>[] interfaces = walk.getInterfaces();
                if (interfaces.length > 0) {
                    for (final Class<?> intf : interfaces) {
                        if (isInstanceOf(className, intf)) {
                            return true;
                        }
                    }
                }
                walk = walk.getSuperclass();
            }
        }
        return false;
    }

    public static boolean isInstanceOf(final String className, final Object instance) {
        final Class<?> clazz = instance.getClass();
        return isInstanceOf(className, clazz);
    }

    /**
     * @param value
     * @param clazz
     * @return
     */
    public static Number castNumber(Number value, Class<?> clazz) {
        if (Clazz.isByte(clazz)) {
            return value.byteValue();
        } else if (Clazz.isShort(clazz)) {
            return value.shortValue();
        } else if (Clazz.isInteger(clazz)) {
            return value.intValue();
        } else if (Clazz.isLong(clazz)) {
            return value.longValue();
        } else if (Clazz.isDouble(clazz)) {
            return value.doubleValue();
        } else if (Clazz.isFloat(clazz)) {
            return value.floatValue();
        } else if (Number.class.equals(clazz)) {
            return value;
        } else {
            throw new WTFException("Unsupported type: " + clazz);
        }
    }

    public static <T extends Object> Collection<T> wrapCollection(final Object object, final boolean unmodifiableCollection, final Class<T> elementType) {
        final Class<?> raw = object != null ? getRaw(object.getClass()) : null;
        final Collection<T> ret;
        if (raw == null) {
            return null;
        } else if (Collection.class.isAssignableFrom(raw)) {
            ret = (Collection<T>) object;
        } else if (raw.isArray()) {
            ret = new AbstractList<T>() {
                @Override
                public T get(int index) {
                    return (T) Array.get(object, index);
                }

                @Override
                public T set(int index, T element) {
                    final T ret = (T) Array.get(object, index);
                    Array.set(object, index, element);
                    return ret;
                }

                @Override
                public int size() {
                    return Array.getLength(object);
                }
            };
        } else {
            return null;
        }
        if (unmodifiableCollection) {
            return Collections.unmodifiableCollection(ret);
        } else {
            return ret;
        }
    }

    public static <T extends Object> Collection<T> wrapUnmodifiableCollection(final Object object, final Class<T> elementType) {
        return wrapCollection(object, true, elementType);
    }

    public static <T extends Object> List<T> wrapList(final Object object, final boolean unmodifiableList, final Class<T> elementType) {
        final Class<?> raw = object != null ? getRaw(object.getClass()) : null;
        final List<T> ret;
        if (raw == null) {
            return null;
        } else if (List.class.isAssignableFrom(raw)) {
            ret = (List<T>) object;
        } else if (Collection.class.isAssignableFrom(raw)) {
            // slow because of Collection.toArray
            ret = (List<T>) Arrays.asList(((Collection<Object>) object).toArray(new Object[0]));
        } else if (raw.isArray()) {
            ret = new AbstractList<T>() {
                @Override
                public T get(int index) {
                    return (T) Array.get(object, index);
                }

                @Override
                public T set(int index, T element) {
                    final T ret = (T) Array.get(object, index);
                    Array.set(object, index, element);
                    return ret;
                }

                @Override
                public int size() {
                    return Array.getLength(object);
                }
            };
        } else {
            return null;
        }
        if (unmodifiableList) {
            return Collections.unmodifiableList(ret);
        } else {
            return ret;
        }
    }

    public static <T extends Object> List<T> wrapUnmodifiableList(final Object object, final Class<T> elementType) {
        return wrapList(object, true, elementType);
    }

    /**
     * @param object
     * @param i
     * @return
     */
    public static final Object getListElement(Object object, int i) {
        if (object.getClass().isArray()) {
            return Array.get(object, i);
        } else {
            if (object instanceof List) {
                return ((List) object).get(i);
            } else {
                throw new IllegalStateException(object + " is no List");
            }
        }
    }

    /**
     * @param object
     * @return
     */
    public static final int getListLength(final Object object) {
        if (object.getClass().isArray()) {
            return Array.getLength(object);
        } else if (object instanceof List) {
            return ((List) object).size();
        } else {
            throw new IllegalStateException(object + " is no List");
        }
    }

    public static final boolean isListOrArray(final Type t) {
        final Class<?> raw;
        return t != null && (raw = getRaw(t)) != null && (raw.isArray() || List.class.isAssignableFrom(raw));
    }

    /**
     * @deprecated isIsListOrArray
     * @param object
     * @return
     */
    public static final boolean isList(final Object object) {
        return isListOrArray(object);
    }

    public static final boolean isListOrArray(final Object object) {
        return object != null && isListOrArray(object.getClass());
    }

    /**
     * Sets the value to the object
     *
     * @param de
     * @param string
     * @param d
     * @throws IllegalAccessException
     * @throws IllegalArgumentException
     * @throws NoSuchFieldException
     */
    public static void setField(Object o, String fieldname, Object value) throws IllegalArgumentException, IllegalAccessException, NoSuchFieldException {
        final Field f = getField(o.getClass(), fieldname);
        f.setAccessible(true);
        f.set(o, value);
    }

    /**
     * gets the next declared field in the inheritance hirarchy
     *
     * @param class1
     * @param fieldname
     * @return
     * @throws NoSuchFieldException
     */
    private static Field getField(final Class<? extends Object> clazz, final String fieldname) throws NoSuchFieldException {
        NoSuchFieldException first = null;
        Class<?> nextClazz = clazz;
        while (nextClazz != null) {
            try {
                final Field field = nextClazz.getDeclaredField(fieldname);
                return field;
            } catch (NoSuchFieldException e) {
                if (first == null) {
                    first = e;
                }
            }
            nextClazz = nextClazz.getSuperclass();
        }
        if (first != null) {
            throw first;
        } else {
            throw new NoSuchFieldException(clazz + "|" + fieldname);
        }
    }

    /**
     * returns a class annotation for type.
     *
     * @param <TT>
     * @param t
     * @param class1
     * @return
     */
    public static <TT> TT getAnnotation(Type t, Class<TT> class1) {
        final Class raw = getRaw(t);
        if (raw == null) {
            return null;
        } else {
            return (TT) raw.getAnnotation(class1);
        }
    }

    /**
     * return the raw class of type or null
     *
     * @param t
     * @return
     */
    public static Class<?> getRaw(Type t) {
        if (t instanceof Class) {
            return ((Class<?>) t);
        } else if (t instanceof ParameterizedType) {
            final Type raw = ((ParameterizedType) t).getRawType();
            if (raw instanceof Class) {
                return ((Class<?>) raw);
            }
        } else if (t instanceof TypeVariable) {
            // not a real type. class Test<MyTypeVariable>
            return null;
        } else if (t instanceof WildcardType) {
            // not a real type. class Test<?>
            return null;
        } else if (t instanceof GenericArrayType) {
            // For example, List.toArray(T[]) -> T[]
            // public List<String>[] array;
            return null;
        } else if (t instanceof CompiledType) {
            return ((CompiledType) t).raw;
        }
        DebugMode.logInIDEOnly(new WTFException("Unsupported? " + t.getClass() + " - " + t));
        return null;
    }

    /**
     * returns the component type. e.g. the array element type or the generic type of a list or the value type of a map
     *
     * @param cls
     * @return
     */
    public static Type getComponentClass(Type cls) throws IllegalArgumentException {
        if (cls instanceof Class) {
            return ((Class) cls).getComponentType();
        } else if (cls instanceof ParameterizedType) {
            if (List.class.isAssignableFrom(getRaw(cls))) {
                if (((ParameterizedType) cls).getActualTypeArguments().length != 1) {
                    throw new IllegalArgumentException("Method requires a list argument with a single generic declaration");
                } else {
                    return ((ParameterizedType) cls).getActualTypeArguments()[0];
                }
            } else if (Set.class.isAssignableFrom(getRaw(cls))) {
                if (((ParameterizedType) cls).getActualTypeArguments().length != 1) {
                    throw new IllegalArgumentException("Method requires a list argument with a single generic declaration");
                } else {
                    return ((ParameterizedType) cls).getActualTypeArguments()[0];
                }
            } else if (Map.class.isAssignableFrom(getRaw(cls))) {
                if (((ParameterizedType) cls).getActualTypeArguments().length != 2) {
                    throw new IllegalArgumentException("Method requires a map argument with 2 generic declarations");
                } else {
                    return ((ParameterizedType) cls).getActualTypeArguments()[1];
                }
            }
        } else if (cls instanceof GenericArrayType) {
            // See #FlexiMapperTestGenericArrays
            return ((GenericArrayType) cls).getGenericComponentType();
        }
        throw new IllegalArgumentException("Class or ParameterizedType are supported. You tried: " + cls);
    }

    /**
     * @param cls
     * @param class1
     * @return
     */
    public static boolean isTypeOf(Type cls, Class<?> class1) {
        if (cls == class1 && cls != null) {
            return true;
        } else {
            final Class<?> raw = getRaw(cls);
            return raw != null && class1 != null && class1.isAssignableFrom(raw);
        }
    }

    public static boolean hasDecimalPlaces(Number number) {
        if (Clazz.isFloatingPointNumber(number.getClass())) {
            if (true) {
                final boolean ret;
                if (number instanceof BigDecimal) {
                    try {
                        // Java 1.5+
                        ((BigDecimal) number).longValueExact();
                        return false;
                    } catch (ArithmeticException e) {
                        return true;
                    }
                } else if (number instanceof Float) {
                    ret = Float.compare(number.longValue() * 1.0f, number.floatValue()) != 0;
                } else {
                    ret = Double.compare(number.longValue() * 1.0d, number.doubleValue()) != 0;
                }
                return ret;
            } else {
                final boolean ret = number.longValue() != number.doubleValue();
                return ret;
            }
        } else {
            return false;
        }
    }

    /**
     * Returns the value of the specified number as a {@code long} if it has zero fractional part
     *
     * for BigDecimal and BigInteger it does not return the value if it has a nonzero fractional part, or will not fit in a long of the
     * value.
     *
     * @param number
     * @return
     */
    public static Long getLongValue(Number number) {
        if (number == null || hasDecimalPlaces(number)) {
            return null;
        } else if (number instanceof BigDecimal) {
            try {
                // Java 1.5+
                return ((BigDecimal) number).longValueExact();
            } catch (ArithmeticException e) {
                return null;
            }
        } else if (number instanceof BigInteger) {
            try {
                if (JVMVersion.isMinimum(JVMVersion.JAVA_1_8)) {
                    // Java 1.8+
                    return ((BigInteger) number).longValueExact();
                } else {
                    // provide Java 1.6 and 1.7 compatibility
                    return new BigDecimal((BigInteger) number).longValueExact();
                }
            } catch (ArithmeticException e) {
                return null;
            }
        } else {
            return number.longValue();
        }
    }

    /**
     * @param number
     * @return
     */
    public static boolean isCharRange(Number number) {
        final Long longValue = getLongValue(number);
        return longValue != null && longValue.longValue() <= Character.MAX_VALUE && longValue.longValue() >= Character.MIN_VALUE;
    }

    public static boolean isByteRange(Number number) {
        final Long longValue = getLongValue(number);
        return longValue != null && longValue.longValue() <= Byte.MAX_VALUE && longValue.longValue() >= Byte.MIN_VALUE;
    }

    public static boolean isShortRange(Number number) {
        final Long longValue = getLongValue(number);
        return longValue != null && longValue.longValue() <= Short.MAX_VALUE && longValue.longValue() >= Short.MIN_VALUE;
    }

    public static boolean isIntRange(Number number) {
        final Long longValue = getLongValue(number);
        return longValue != null && longValue.longValue() <= Integer.MAX_VALUE && longValue.longValue() >= Integer.MIN_VALUE;
    }

    public static boolean isLongRange(Number number) {
        final Long longValue = getLongValue(number);
        return longValue != null && longValue.longValue() <= Long.MAX_VALUE && longValue.longValue() >= Long.MIN_VALUE;
    }

    /**
     * Returns the value of the specified number as a {@code float}, which may involve rounding.
     *
     * for BigInteger and BigDecimal it only returns the value if the conversion to float does not lose any information about the precision
     * of the value.
     *
     * @param number
     * @return
     */
    public static Float getFloatValue(Number number) {
        if (number == null) {
            return null;
        } else if (number instanceof BigDecimal) {
            try {
                final BigDecimal bg = ((BigDecimal) number);
                final Float floatValue = bg.floatValue();
                final BigDecimal sub = bg.subtract(BigDecimal.valueOf(floatValue));
                if (sub.longValueExact() == 0) {
                    return floatValue;
                } else {
                    return null;
                }
            } catch (ArithmeticException e) {
                return null;
            } catch (NumberFormatException e) {
                return null;
            }
        } else if (number instanceof BigInteger) {
            return getFloatValue(new BigDecimal((BigInteger) number));
        } else {
            return number.floatValue();
        }
    }

    /**
     * Returns the value of the specified number as a {@code double}, which may involve rounding.
     *
     * for BigInteger and BigDecimal it only returns the value if the conversion to double does not lose any information about the precision
     * of the value.
     *
     * @param number
     * @return
     */
    public static Double getDoubleValue(Number number) {
        if (number == null) {
            return null;
        } else if (number instanceof BigDecimal) {
            try {
                final BigDecimal bg = ((BigDecimal) number);
                final Double doubleValue = bg.doubleValue();
                final BigDecimal sub = bg.subtract(BigDecimal.valueOf(doubleValue));
                if (sub.longValueExact() == 0) {
                    return doubleValue;
                } else {
                    return null;
                }
            } catch (ArithmeticException e) {
                return null;
            } catch (NumberFormatException e) {
                return null;
            }
        } else if (number instanceof BigInteger) {
            return getDoubleValue(new BigDecimal((BigInteger) number));
        } else {
            return number.doubleValue();
        }
    }

    public static boolean isFloatRange(Number number) { // do not check for floatingpoint number. fixed to floating is ok
        final Float floatValue = getFloatValue(number); // do not use doubleValue here. (double)3.4028235E38 != (float)3.4028235E38
        return floatValue != null && floatValue.floatValue() <= Float.MAX_VALUE && floatValue.floatValue() >= -Float.MAX_VALUE;
    }

    public static boolean isDoubleRange(Number number) {
        final Double doubleValue = getDoubleValue(number); // do not check for floatingpoint number. fixed to floating is ok
        return doubleValue != null && doubleValue.doubleValue() <= Double.MAX_VALUE && doubleValue.doubleValue() >= -Double.MAX_VALUE;
    }

    /**
     * @param value
     * @param destType
     * @return
     */
    public static Object cast(final Object value, Type destType) {
        if (value == null) {
            // Jackson, DeserializationFeature.FAIL_ON_NULL_FOR_PRIMITIVES is disabled by default
            if (Clazz.isNumberType(destType)) {
                if (((Class) destType).isPrimitive()) {
                    return cast(0, destType);
                } else {
                    return null;
                }
            } else if (Clazz.isBoolean(destType)) {
                if (((Class) destType).isPrimitive()) {
                    return false;
                } else {
                    return null;
                }
            } else {
                return null;
            }
        } else if (Clazz.isString(destType)) {
            if (value.getClass() == char[].class) {
                return new String((char[]) value);
            } else {
                return StringUtils.valueOfOrNull(value);
            }
        } else if (value instanceof CharSequence) {
            final String stringValue = String.valueOf(value);
            if (Clazz.isNumberType(destType)) {
                // string to number type
                if (Clazz.isFixedPointNumber(destType)) {
                    return cast(Long.valueOf(stringValue), destType);
                } else {
                    return cast(Double.valueOf(stringValue), destType);
                }
            } else if (Clazz.isBoolean(destType)) {
                // string to boolean
                if ("true".equals(stringValue)) {
                    return cast(Boolean.TRUE, destType);
                } else if ("false".equals(stringValue)) {
                    return cast(Boolean.FALSE, destType);
                }
            } else if (Number.class.equals(destType)) {
                // string to number class
                if (stringValue.matches("-?\\d+")) {
                    return Long.parseLong(stringValue);
                } else if (stringValue.matches("-?\\d+\\.\\d+")) {
                    return Double.valueOf(stringValue).longValue();
                }
            } else if (Clazz.isCharacter(destType)) {
                // string to char
                final char chars[] = stringValue.toCharArray();
                if (chars.length != 1) {
                    throw new ClassCastException(stringValue + " cannot be represented with single char");
                } else {
                    return chars[0];
                }
            } else if (destType == char[].class) {
                return stringValue.toCharArray();
            } else {
                // no cast
                return value;
            }
        } else if (value instanceof Number && Clazz.isBoolean(destType)) {
            // Number(0|1) to Boolean
            final int numValue = ((Number) value).intValue();
            if (numValue == 1) {
                return cast(Boolean.TRUE, destType);
            } else if (numValue == 0) {
                return cast(Boolean.FALSE, destType);
            }
        } else if (value instanceof Character) {
            if (Clazz.isBoolean(destType)) {
                // Character(0|1) to Boolean
                final char numValue = ((Character) value).charValue();
                if (numValue == '1') {
                    return cast(Boolean.TRUE, destType);
                } else if (numValue == '0') {
                    return cast(Boolean.FALSE, destType);
                }
            } else if (Clazz.isCharacter(destType)) {
                // char to char
                return value;
            } else if (Clazz.isFixedPointNumber(destType)) {
                // char to number
                return Character.codePointAt(new char[] { (Character) value }, 0);
            }
        }
        Object ret = value;
        if (destType instanceof Class && ((Class) destType).isPrimitive()) {
            if (destType == boolean.class) {
                ret = ((Boolean) value).booleanValue();
            } else if (destType == char.class) {
                if (!isCharRange(((Number) value))) {
                    if (isDoubleRange(((Number) value))) {
                        if (((Number) value).intValue() != ((Number) value).doubleValue() || !isCharRange(((Number) value).intValue())) {
                            throw new ClassCastException(value + " is not in int Range " + Character.MIN_VALUE + " to " + Character.MAX_VALUE);
                        } else {
                            // 1.0 is treated as 1 and valid integer
                        }
                    } else {
                        throw new ClassCastException(value + " is not in int Range " + Character.MIN_VALUE + " to " + Character.MAX_VALUE);
                    }
                }
                ret = (char) ((Number) value).intValue();
            } else if (destType == byte.class) {
                if (!isByteRange(((Number) value))) {
                    if (isDoubleRange(((Number) value))) {
                        if (((Number) value).intValue() != ((Number) value).doubleValue() || !isByteRange(((Number) value).intValue())) {
                            throw new ClassCastException(value + " is not in int Range " + Byte.MIN_VALUE + " to " + Byte.MAX_VALUE);
                        } else {
                            // 1.0 is treated as 1 and valid integer
                        }
                    } else {
                        throw new ClassCastException(value + " is not in int Range " + Byte.MIN_VALUE + " to " + Byte.MAX_VALUE);
                    }
                }
                ret = ((Number) value).byteValue();
            } else if (destType == short.class) {
                if (!isShortRange(((Number) value))) {
                    if (isDoubleRange(((Number) value))) {
                        if (((Number) value).intValue() != ((Number) value).doubleValue() || !isShortRange(((Number) value).intValue())) {
                            throw new ClassCastException(value + " is not in int Range " + Short.MIN_VALUE + " to " + Short.MAX_VALUE);
                        } else {
                            // 1.0 is treated as 1 and valid integer
                        }
                    } else {
                        throw new ClassCastException(value + " is not in int Range " + Short.MIN_VALUE + " to " + Short.MAX_VALUE);
                    }
                }
                ret = ((Number) value).shortValue();
            } else if (destType == int.class) {
                if (!isIntRange(((Number) value))) {
                    if (isDoubleRange(((Number) value))) {
                        if (((Number) value).intValue() != ((Number) value).doubleValue()) {
                            throw new ClassCastException(value + " is not in int Range " + Integer.MIN_VALUE + " to " + Integer.MAX_VALUE);
                        } else {
                            // 1.0 is treated as 1 and valid integer
                        }
                    } else {
                        throw new ClassCastException(value + " is not in int Range " + Integer.MIN_VALUE + " to " + Integer.MAX_VALUE);
                    }
                }
                ret = ((Number) value).intValue();
            } else if (destType == long.class) {
                if (!isLongRange(((Number) value))) {
                    throw new ClassCastException(value + " is not in long Range " + Long.MIN_VALUE + " to " + Long.MAX_VALUE);
                } else {
                    ret = ((Number) value).longValue();
                }
            } else if (destType == float.class) {
                if (!isFloatRange(((Number) value))) {
                    throw new ClassCastException(value + " is not in float Range " + Float.MIN_VALUE + " to " + Float.MAX_VALUE);
                } else {
                    ret = ((Number) value).floatValue();
                }
            } else if (destType == double.class) {
                if (!isDoubleRange(((Number) value))) {
                    throw new ClassCastException(value + " is not in double Range " + Double.MIN_VALUE + " to " + Double.MAX_VALUE);
                } else {
                    ret = ((Number) value).doubleValue();
                }
            }
        } else if (destType == Boolean.class) {
            ret = ((Boolean) value).booleanValue();
        } else if (destType == Character.class) {
            if (!isCharRange(((Number) value))) {
                throw new ClassCastException(value + " is not in char Range " + Character.MIN_VALUE + " to " + Character.MAX_VALUE);
            } else {
                ret = (char) ((Number) value).intValue();
            }
        } else if (destType == Byte.class) {
            if (!isByteRange(((Number) value))) {
                throw new ClassCastException(value + " is not in byte Range " + Byte.MIN_VALUE + " to " + Byte.MAX_VALUE);
            } else {
                ret = ((Number) value).byteValue();
            }
        } else if (destType == Short.class) {
            if (!isShortRange(((Number) value))) {
                throw new ClassCastException(value + " is not in short Range " + Short.MIN_VALUE + " to " + Short.MAX_VALUE);
            } else {
                ret = ((Number) value).shortValue();
            }
        } else if (destType == Integer.class) {
            if (!isIntRange(((Number) value))) {
                throw new ClassCastException(value + " is not in int Range " + Integer.MIN_VALUE + " to " + Integer.MAX_VALUE);
            } else {
                ret = ((Number) value).intValue();
            }
        } else if (destType == Long.class) {
            if (!isLongRange(((Number) value))) {
                throw new ClassCastException(value + " is not in long Range " + Long.MIN_VALUE + " to " + Long.MAX_VALUE);
            } else {
                ret = ((Number) value).longValue();
            }
        } else if (destType == Float.class) {
            if (!isFloatRange(((Number) value))) {
                throw new ClassCastException(value + " is not in float Range " + Float.MIN_VALUE + " to " + Float.MAX_VALUE);
            } else {
                ret = ((Number) value).floatValue();
            }
        } else if (destType == Double.class) {
            if (!isDoubleRange(((Number) value))) {
                throw new ClassCastException(value + " is not in double Range " + Double.MIN_VALUE + " to " + Double.MAX_VALUE);
            } else {
                ret = ((Number) value).doubleValue();
            }
        }
        return ret;
    }

    /**
     * @param last
     * @return
     */
    public static boolean isEnum(Type last) {
        if (last instanceof Class) {
            return ((Class) last).isEnum();
        } else {
            return false;
        }
    }

    /**
     * returns all exception types that class1 may throw (unordered)
     *
     * @param class1
     * @return
     */
    public static Set<Type> listExceptions(Class<?> class1) {
        final ClassCache cc;
        try {
            cc = ClassCache.getClassCache(class1);
        } catch (SecurityException e1) {
            throw new WTFException(e1);
        } catch (NoSuchMethodException e1) {
            throw new WTFException(e1);
        }
        final HashSet<Type> ret = new HashSet<Type>();
        for (final Type cl : cc.getTypeHierarchy()) {
            if (cl instanceof Class) {
                for (final Method m : ((Class) cl).getDeclaredMethods()) {
                    for (final Type e : /* java 1.5 */m.getGenericExceptionTypes()) {
                        ret.add(e);
                    }
                }
            }
        }
        return ret;
    }
}
