/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
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
package org.appwork.storage.tests;

import java.lang.annotation.Annotation;
import java.lang.reflect.Constructor;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.net.URL;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.moncompare.Condition;
import org.appwork.remoteapi.annotations.AllowNonStorableObjects;
import org.appwork.storage.InvalidTypeException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.MapperType;
import org.appwork.storage.Storable;
import org.appwork.storage.StorableAllowPrivateAccessModifier;
import org.appwork.storage.StorableAllowProtectedAccessModifier;
import org.appwork.storage.StorableSupportedMappers;
import org.appwork.storage.StorableValidatorIgnoresMissingGetter;
import org.appwork.storage.StorableValidatorIgnoresMissingSetter;
import org.appwork.storage.TypeRef;
import org.appwork.storage.config.annotations.DefaultEnumArrayValue;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DefaultJsonObject;
import org.appwork.storage.flexijson.mapper.interfacestorage.FlexiStorableInterface;
import org.appwork.storage.simplejson.mapper.ClassCache;
import org.appwork.storage.simplejson.mapper.ClassCache.Rules;
import org.appwork.storage.simplejson.mapper.Getter;
import org.appwork.storage.simplejson.mapper.Setter;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.IgnoreInAWTest;
import org.appwork.utils.ClassPathScanner;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.duration.TimeSpan;
import org.appwork.utils.reflection.Clazz;
import org.appwork.utils.reflection.CompiledType;

/**
 * @author thomas
 * @date 23.02.2022
 *
 */
public class StorableValidatorTest extends AWTest {
    public static interface CanStoreRules {
        /**
         * @param gType
         * @param method
         *            TODO
         * @return
         */
        boolean isTypeWithlisted(Type gType, Method method);

        /**
         * @param sc
         * @return
         */
        boolean isFollowSuperClass(Type sc);
    }

    public static void main(String[] args) {
        run();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        new ClassPathScanner<Exception>() {
            @Override
            public void handle(Class<?> cls) throws Exception {
                if (Storable.class.isAssignableFrom(cls) && Storable.class != cls) {
                    if (cls == BadCollectionExtends.class) {
                        try {
                            validateClass(cls);
                            throw new Exception("Expected Validation Exception");
                        } catch (Exception e) {
                            // expected
                            String message = e.getMessage();
                            if (!message.startsWith("WARNING: package org.appwork.storage.tests(BadCollectionExtends.java:1) is a List and will not de/serialize setters or getters")) {
                                throw new Exception("Expected a differend message", e);
                            }
                            return;
                        }
                    } else if (cls == BadMapExtends.class) {
                        try {
                            validateClass(cls);
                            throw new Exception("Expected Validation Exception");
                        } catch (Exception e) {
                            // expected
                            String message = e.getMessage();
                            if (!message.startsWith("WARNING: package org.appwork.storage.tests(BadMapExtends.java:1) is a Map and will not de/serialize setters or getters")) {
                                throw new Exception("Expected a differend message", e);
                            }
                            return;
                        }
                    }
                    validateClass(cls);
                }
            }
        }.run();
    }

    protected boolean skipValidation(Class<?> cls) throws Exception {
        if (cls == null) {
            return false;
        }
        if (CompiledType.create(cls).isInstanceOf(Condition.class)) {
            return true;
        }
        try {
            List<IgnoreInAWTest> ignore = ClassCache.getClassCache(cls).getAnnotations(null, IgnoreInAWTest.class);
            if (ignore.size() > 0) {
                return true;
            }
            List<StorableSupportedMappers> annotations = ClassCache.getClassCache(cls).getAnnotations(null, StorableSupportedMappers.class);
            if (annotations != null) {
                for (StorableSupportedMappers a : annotations) {
                    // does not support simple - skip test
                    if (!Arrays.asList(a.value()).contains(MapperType.AW_SIMPLE)) {
                        return true;
                    }
                }
            }
            if (org.appwork.storage.config.test.BadTestObject.class == cls) {
                return true;
            } else if (org.appwork.storage.simplejson.mapper.test.TestClass.class == cls) {
                return true;
            } else if (cls.getAnnotation(IgnoreInAWTest.class) != null) {
                return true;
            } else if (cls.isInterface() || Modifier.isAbstract(cls.getModifiers())) {
                return true;
            } else {
                return false;
            }
        } catch (Throwable e) {
            throw new WTFException("Error in " + cls, e);
        }
    }

    protected void checkMissingConstructor(Class<?> cls) throws Exception {
        if (!cls.isAnonymousClass()) {
            try {
                Constructor<?> c = cls.getDeclaredConstructor(new Class[] {});
                if (Modifier.isPrivate(c.getModifiers()) && c.getAnnotation(StorableAllowPrivateAccessModifier.class) == null) {
                    throw new Exception("Found private constructor. Change modifier or add @StorableAllowPrivateAccessModifier Annotation: " + c);
                }
                if (!Modifier.isPrivate(c.getModifiers()) && !Modifier.isPublic(c.getModifiers()) && c.getAnnotation(StorableAllowProtectedAccessModifier.class) == null) {
                    throw new Exception("Found protected constructor. Change modifier or add @StorableAllowProtectedAccessModifier Annotation: " + c);
                }
            } catch (java.lang.NoSuchMethodException e) {
                throw new Exception("Empty Constructor missing in " + cls.getPackage().toString() + " (" + cls.getSimpleName() + ".java:1)");
            }
        }
    }

    protected ClassCache createClassCache(Class<?> cls) throws Exception {
        return ClassCache.create(cls, new Rules());
    }

    protected void checkNoGetterSetter(ClassCache classCache, Class<?> cls) throws Exception {
        if (classCache.getKeys().size() == 0) {
            throw new Exception("The Storable " + link(cls) + " has no getters or setters!");
        }
    }

    protected void checkAnnotations(ClassCache classCache, Class<?> cls, final Method method) throws Exception {
        Type rawType = method.getReturnType();
        if (void.class.equals(rawType)) {
            rawType = method.getGenericParameterTypes()[0];
        }
        final Class<?> retClazz = ReflectionUtils.getRaw(rawType);
        if (retClazz == null) {
            return;
        }
        final DefaultJsonObject dfJSONObject = method.getAnnotation(DefaultJsonObject.class);
        if (dfJSONObject != null) {
            try {
                final Object defaultValue = JSonStorage.getMapper().stringToObject(dfJSONObject.value(), new TypeRef(rawType) {
                });
                if ("null".equals(dfJSONObject.value()) && defaultValue != null) {
                    throw new Exception("unexpected not null default object!");
                } else if (defaultValue == null && !"null".equals(dfJSONObject.value())) {
                    throw new Exception("unexpected null default object!");
                }
                if (defaultValue != null && Boolean.class.equals(defaultValue.getClass()) && boolean.class.equals(retClazz)) {
                    // Boolean to boolean is okay
                } else {
                    retClazz.cast(defaultValue);
                }
            } catch (Exception e) {
                throw new Exception("here:" + cls + "|method:" + method, e);
            }
        }
        if (Clazz.isEnum(retClazz)) {
            final DefaultEnumValue dfEnumValue = method.getAnnotation(DefaultEnumValue.class);
            if (dfEnumValue != null) {
                try {
                    Enum.valueOf((Class<Enum>) retClazz, dfEnumValue.value());
                } catch (Exception e) {
                    throw new Exception("here:" + cls + "|method:" + method, e);
                }
            }
        } else if (Enum[].class.isAssignableFrom(retClazz)) {
            final Class<?> arrayEnumType = retClazz.getComponentType();
            final DefaultEnumArrayValue dfEnumArrayValue = method.getAnnotation(DefaultEnumArrayValue.class);
            if (dfEnumArrayValue != null && dfEnumArrayValue.value() != null) {
                for (final String dfEnumValue : dfEnumArrayValue.value()) {
                    try {
                        final int index = dfEnumValue.lastIndexOf(".");
                        final String enumValue = dfEnumValue.substring(index + 1);
                        final String enumClazz = dfEnumValue.substring(0, index);
                        final Enum<?> value = Enum.valueOf((Class<Enum>) Class.forName(enumClazz), enumValue);
                        arrayEnumType.cast(value);
                    } catch (Exception e) {
                        throw new Exception("here:" + cls + "|method:" + method, e);
                    }
                }
            }
        }
    }

    /**
     * @param cls
     * @throws Exception
     */
    public void validateClass(Class<?> cls) throws Exception {
        if (skipValidation(cls)) {
            return;
        }
        checkMissingConstructor(cls);
        final ClassCache classCache = createClassCache(cls);
        if (Map.class.isAssignableFrom(cls)) {
            if (classCache.getKeys().size() > 0) {
                throw new Exception("WARNING: " + link(cls) + " is a Map and will not de/serialize setters or getters: " + classCache.getKeys());
            } else {
                return;
            }
        } else if (Set.class.isAssignableFrom(cls)) {
            if (classCache.getKeys().size() > 0) {
                throw new Exception("WARNING: " + link(cls) + " is a Set and will not de/serialize setters or getters: " + classCache.getKeys());
            } else {
                return;
            }
        } else if (Collection.class.isAssignableFrom(cls)) {
            if (classCache.getKeys().size() > 0) {
                throw new Exception("WARNING: " + link(cls) + " is a List and will not de/serialize setters or getters " + classCache.getKeys());
            } else {
                return;
            }
        }
        for (Method m : CompiledType.create(cls).listMethods()) {
            if (Modifier.isStatic(m.getModifiers())) {
                continue;
            }
            if (m.getName().startsWith("get") || m.getName().startsWith("is") || m.getName().startsWith("set")) {
                if (Modifier.isPrivate(m.getModifiers()) && m.getAnnotation(StorableAllowPrivateAccessModifier.class) == null) {
                    throw new Exception(m.getDeclaringClass() + " Found private getter or stetter without @StorableAllowPrivateAccessModifier Annotation " + m);
                }
                if (!Modifier.isPrivate(m.getModifiers()) && !Modifier.isPublic(m.getModifiers()) && m.getAnnotation(StorableAllowProtectedAccessModifier.class) == null) {
                    throw new Exception(m.getDeclaringClass() + " Found protected private getter or stetter without @StorableAllowProtectedAccessModifier Annotation " + m);
                }
            }
        }
        checkNoGetterSetter(classCache, cls);
        for (String key : classCache.getKeys()) {
            final Getter g = classCache.getGetter(key);
            final Setter s = classCache.getSetter(key);
            if (g != null && g.getMethod() != null && g.getMethod().getDeclaringClass().getAnnotation(IgnoreInAWTest.class) != null) {
                continue;
            } else if (s != null && s.getMethod() != null && s.getMethod().getDeclaringClass().getAnnotation(IgnoreInAWTest.class) != null) {
                continue;
            }
            int i = 0;
            // check if we use the same methodname with different parameters
            Class<?> layer = cls;
            while (layer != null) {
                if (layer.getAnnotation(IgnoreInAWTest.class) == null) {
                    for (Method m : cls.getDeclaredMethods()) {
                        if (Modifier.isStatic(m.getModifiers())) {
                            continue;
                        }
                        if (key.equals(ClassCache.createKey(m))) {
                            if (g != null && g.getMethod().equals(m)) {
                                continue;
                            } else if (s != null && s.getMethod().equals(m)) {
                                continue;
                            } else if ((g == null || s == null) && Modifier.isProtected(m.getModifiers())) {
                                // we handle private setters/getters somewhere else
                                continue;
                            } else if ((g == null || s == null) && Modifier.isPrivate(m.getModifiers())) {
                                // we handle private setters/getters somewhere else
                                continue;
                            } else {
                                throw new Exception("Method with name " + m + " exists multiple times with different parameters! " + (cls));
                            }
                        }
                    }
                }
                layer = layer.getSuperclass();
            }
            if (s == null && !hasAnnotation(classCache, key, StorableValidatorIgnoresMissingSetter.class)) {
                URL url = cls.getClassLoader().getResource(cls.getName().replace(".", "/") + ".class");
                throw new Exception("setter " + cls.getName() + "." + key + " is missing - (" + g.getMethod().getDeclaringClass().getSimpleName() + ".java:1) " + url);
            } else if (g == null && !hasAnnotation(classCache, key, StorableValidatorIgnoresMissingGetter.class)) {
                URL url = cls.getClassLoader().getResource(cls.getName().replace(".", "/") + ".class");
                throw new Exception("getter " + cls.getName() + "." + key + " is missing - (" + s.getMethod().getDeclaringClass().getSimpleName() + ".java:1) " + url);
            }
            if (g != null) {
                if (g.getMethod() == null) {
                    throw new Exception("Missing public getter " + cls + "." + key + " method. public field?");
                } else if (!Modifier.isPublic(g.getMethod().getModifiers())) {
                }
                checkAnnotations(classCache, cls, g.getMethod());
                try {
                    canStore(classCache, cls, g.getMethod(), g.getMethod().getGenericReturnType());
                } catch (InvalidTypeException e) {
                    LogV3.log(e);
                    throw new Exception(g.getMethod().getDeclaringClass() + " getter (" + g.getMethod().getDeclaringClass().getSimpleName() + ".java:1)" + "." + g.key + " returns a non-storable object " + (g.getMethod().getGenericReturnType()));
                }
            }
            if (s != null) {
                if (s.getMethod() == null) {
                    throw new Exception("Missing public getter " + cls + "." + g.key + " method. public field?");
                } else if (!Modifier.isPublic(s.getMethod().getModifiers())) {
                    throw new Exception("setter " + g.getMethod().getDeclaringClass().getName() + "." + g.key + " is not public " + (cls));
                }
                checkAnnotations(classCache, cls, s.getMethod());
                try {
                    canStore(classCache, cls, s.getMethod(), s.getMethod().getGenericParameterTypes()[0]);
                } catch (InvalidTypeException e) {
                    LogV3.log(e);
                    throw new Exception("setter " + g.getMethod().getDeclaringClass().getName() + "." + g.key + " has a non-storable parameter " + (cls));
                }
            }
            if (g != null && s != null) {
                try {
                    if (!Clazz.equalsIgnorePrimitive(g.type, s.type)) {
                        throw new Exception("setter and getter is different file types " + g.getMethod().getDeclaringClass().getName() + "." + g.key + " " + (cls));
                    } else if (!g.getMethod().getGenericReturnType().equals(s.getMethod().getGenericParameterTypes()[0])) {
                        logInfoAnyway("[WARNING] Primitive/Wrapper missmatch in " + s.getMethod() + "/" + g.getMethod());
                    }
                } catch (NullPointerException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    /**
     * @param cls
     * @return
     */
    private String link(Class<?> cls) {
        return cls.getPackage().toString() + "(" + cls.getSimpleName() + ".java:1)";
    }

    /**
     * @param classCache
     * @param cls
     * @param method
     * @param genericReturnType
     * @throws InvalidTypeException
     */
    protected void canStore(final ClassCache classCache, final Class<?> cls, final Method method, final Type type) throws InvalidTypeException {
        StorableValidatorTest.canStoreIntern(type, "", new StorableValidatorTest.CanStoreRules() {
            @Override
            public boolean isFollowSuperClass(Type sc) {
                return false;
            }

            @Override
            public boolean isTypeWithlisted(Type gType, Method method) {
                Class<?> raw = ReflectionUtils.getRaw(gType);
                if (raw == null) {
                    logInfoAnyway("NULL raw " + gType + " ." + gType.getTypeName());
                }
                try {
                    if (raw != null && skipValidation(raw)) {
                        return true;
                    }
                } catch (Exception e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
                if (ReflectionUtils.getAnnotation(gType, IgnoreInAWTest.class) != null) {
                    return true;
                }
                AllowNonStorableObjects allowNonStorable = ReflectionUtils.getAnnotation(gType, AllowNonStorableObjects.class);
                if (raw != null && allowNonStorable != null && Arrays.asList(allowNonStorable.value()).contains(raw)) {
                    return true;
                }
                if (method.getDeclaringClass().getAnnotation(IgnoreInAWTest.class) != null) {
                    // return true;
                }
                Class<?> layer = cls;
                while (layer != null) {
                    try {
                        ClassCache layerCC = ClassCache.getClassCache(layer);
                        if (ReflectionUtils.getAnnotation(layer, IgnoreInAWTest.class) != null) {
                            return true;
                        }
                        allowNonStorable = ReflectionUtils.getAnnotation(layer, AllowNonStorableObjects.class);
                        if (raw != null && allowNonStorable != null && Arrays.asList(allowNonStorable.value()).contains(raw)) {
                            return true;
                        }
                        if (layerCC.getAnnotations(ClassCache.createKey(method), IgnoreInAWTest.class).size() > 0) {
                            return true;
                        }
                        for (AllowNonStorableObjects anno : layerCC.getAnnotations(ClassCache.createKey(method), AllowNonStorableObjects.class)) {
                            if (raw != null && anno != null && Arrays.asList(anno.value()).contains(raw)) {
                                return true;
                            }
                        }
                    } catch (NoSuchMethodException e) {
                    } catch (SecurityException e) {
                    }
                    layer = layer.getSuperclass();
                }
                if (method != null) {
                    try {
                        ClassCache cc = ClassCache.getClassCache(method.getDeclaringClass());
                        if (cc.getAnnotations(ClassCache.createKey(method), IgnoreInAWTest.class).size() > 0) {
                            return true;
                        }
                        if (cc.getAnnotations(null, IgnoreInAWTest.class).size() > 0) {
                            return true;
                        }
                        for (AllowNonStorableObjects anno : cc.getAnnotations(ClassCache.createKey(method), AllowNonStorableObjects.class)) {
                            if (Arrays.asList(anno.value()).contains(raw)) {
                                return true;
                            }
                        }
                        for (AllowNonStorableObjects anno : cc.getAnnotations(null, AllowNonStorableObjects.class)) {
                            if (Arrays.asList(anno.value()).contains(raw)) {
                                return true;
                            }
                        }
                    } catch (SecurityException e) {
                    } catch (NoSuchMethodException e) {
                    }
                    if (method.getAnnotation(IgnoreInAWTest.class) != null) {
                        return true;
                    }
                    allowNonStorable = method.getAnnotation(AllowNonStorableObjects.class);
                    if (allowNonStorable != null && Arrays.asList(allowNonStorable.value()).contains(raw)) {
                        return true;
                    }
                }
                return false;
            }
        }, new HashSet<Object>(), method);
    }

    /**
     * @param classCache
     * @param class1
     * @return
     */
    private boolean hasAnnotation(ClassCache classCache, String key, Class<? extends Annotation> annotationClass) {
        if (classCache.getAnnotations(key, annotationClass).size() > 0) {
            return true;
        } else if (classCache.getAnnotations((String) null, annotationClass).size() > 0) {
            return true;
        } else {
            return false;
        }
    }

    public static String sourceMethodToString(Method method) {
        if (method == null) {
            return "Unknown Source";
        } else {
            return method.getDeclaringClass().getSimpleName() + "." + method.getName();
        }
    }

    /**
     * @param gType
     * @param dupeID
     * @param method
     *            TODO
     * @param allowNonStorableObjects
     *            TODO
     * @param string
     * @throws InvalidTypeException
     */
    public static void canStoreIntern(final Type gType, final String path, StorableValidatorTest.CanStoreRules rules, HashSet<Object> dupeID, Method method) throws InvalidTypeException {
        if (gType instanceof GenericArrayType) {
            throw new WTFException("Generic Array Type not supported: " + gType);
        }
        if (!dupeID.add(gType)) {
            return;
        } else if (rules.isTypeWithlisted(gType, method)) {
            return;
        } else if (gType == Object.class) {
            if (rules.isTypeWithlisted(gType, method)) {
                return;
            } else {
                throw new InvalidTypeException(gType, "Cannot store Object: " + path + "/" + gType + " " + sourceMethodToString(method));
            }
        } else if (gType instanceof Class) {
            final Class<?> type = (Class<?>) gType;
            if (type == void.class) {
                throw new InvalidTypeException(gType, "Void is not accepted: " + path + "/" + gType + " " + sourceMethodToString(method));
            } else if (type.isPrimitive()) {
                return;
            } else if (Date.class.isAssignableFrom(type)) {
                return;
            } else if (TimeSpan.class.isAssignableFrom(type)) {
                return;
            } else if (type == Boolean.class || type == Long.class || type == Integer.class || type == Byte.class || type == Double.class || type == Float.class || type == String.class || type == Short.class || type == Character.class || type == CharSequence.class) {
                return;
            } else if (type.isEnum()) {
                return;
            } else if (type.isArray()) {
                final Class<?> arrayType = type.getComponentType();
                StorableValidatorTest.canStoreIntern(arrayType, path + "[" + arrayType + "]", rules, dupeID, method);
                return;
            } else
            // we need an empty constructor
            if (List.class.isAssignableFrom(type)) {
                return;
            } else if (Map.class.isAssignableFrom(type)) {
                return;
            } else if (Set.class.isAssignableFrom(type)) {
                return;
            } else if (Storable.class.isAssignableFrom(type) || FlexiStorableInterface.class.isAssignableFrom(type)) {
                try {
                    if (!FlexiStorableInterface.class.isAssignableFrom(type)) {
                        type.getDeclaredConstructor(new Class[] {});
                    }
                    Class<?> layer = type;
                    while (layer != null && layer != Object.class) {
                        for (final Method m : layer.getDeclaredMethods()) {
                            if (Modifier.isStatic(m.getModifiers())) {
                                continue;
                            }
                            if (m.getName().startsWith("get")) {
                                if (m.getParameterTypes().length > 0) {
                                    throw new InvalidTypeException(gType, "Getter " + path + "." + m + " has parameters." + " " + sourceMethodToString(method));
                                }
                                StorableValidatorTest.canStoreIntern(m.getGenericReturnType(), path + "->" + m.getGenericReturnType(), rules, dupeID, m);
                            } else if (m.getName().startsWith("set")) {
                                if (m.getParameterTypes().length != 1) {
                                    throw new InvalidTypeException(gType, "Setter " + path + "." + m + " has != 1 Parameters." + " " + sourceMethodToString(method));
                                }
                                StorableValidatorTest.canStoreIntern(m.getGenericParameterTypes()[0], path + "->" + m.getGenericParameterTypes()[0], rules, dupeID, m);
                            } else if (m.getName().startsWith("is")) {
                                if (m.getParameterTypes().length > 0) {
                                    throw new InvalidTypeException(gType, "Getter " + path + "." + m + " has parameters." + " " + sourceMethodToString(method));
                                }
                                StorableValidatorTest.canStoreIntern(m.getGenericReturnType(), path + "->" + m.getGenericReturnType(), rules, dupeID, m);
                            }
                        }
                        layer = layer.getSuperclass();
                    }
                    Type sc = type.getGenericSuperclass();
                    if (sc != null && rules.isFollowSuperClass(sc)) {
                        canStoreIntern(sc, path + "<<<" + sc, rules, dupeID, method);
                    }
                    return;
                } catch (final NoSuchMethodException e) {
                    throw new InvalidTypeException(gType, "Storable " + path + " has no empty Constructor" + " " + sourceMethodToString(method));
                }
            }
        } else if (gType instanceof ParameterizedType) {
            final ParameterizedType ptype = (ParameterizedType) gType;
            final Type raw = ptype.getRawType();
            StorableValidatorTest.canStoreIntern(raw, path, rules, dupeID, method);
            for (final Type t : ptype.getActualTypeArguments()) {
                StorableValidatorTest.canStoreIntern(t, path + "(" + t + ")", rules, dupeID, method);
            }
            return;
        } else if (gType instanceof GenericArrayType) {
            final GenericArrayType atype = (GenericArrayType) gType;
            final Type t = atype.getGenericComponentType();
            StorableValidatorTest.canStoreIntern(t, path + "[" + t + "]", rules, dupeID, method);
            return;
        } else if (gType instanceof TypeVariable) {
            // type veriables are checked one layer above
            return;
        } else {
            throw new InvalidTypeException(gType, "Generic Type Structure not implemented: " + gType.getClass() + " in " + path + " " + sourceMethodToString(method));
        }
        throw new InvalidTypeException(gType, "Type " + path + "/" + gType + " is not supported." + " " + sourceMethodToString(method));
    }
}
