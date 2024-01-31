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
package org.appwork.storage.tests;

import java.lang.annotation.Annotation;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.remoteapi.annotations.AllowNonStorableObjects;
import org.appwork.remoteapi.annotations.ApiDoc;
import org.appwork.remoteapi.annotations.ApiDocExample;
import org.appwork.storage.InvalidTypeException;
import org.appwork.storage.SimpleTypeRef;
import org.appwork.storage.Storable;
import org.appwork.storage.StorableAllowPrivateAccessModifier;
import org.appwork.storage.StorableAllowProtectedAccessModifier;
import org.appwork.storage.StorableDoc;
import org.appwork.storage.StorableExample;
import org.appwork.storage.StorableHidden;
import org.appwork.storage.StorableLink;
import org.appwork.storage.StorableSee;
import org.appwork.storage.StorableValidateCondition;
import org.appwork.storage.StorableValidateMandatoryInJson;
import org.appwork.storage.StorableValidateNotNull;
import org.appwork.storage.StorableValidateRegex;
import org.appwork.storage.StorableValidator;
import org.appwork.storage.StorableValidator.ValidatorException;
import org.appwork.storage.StorableValidatorIgnoresMissingGetter;
import org.appwork.storage.StorableValidatorIgnoresMissingSetter;
import org.appwork.storage.flexijson.FlexiJSONParser;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.mapper.FlexiJSonMapper;
import org.appwork.storage.flexijson.mapper.FlexiJsonProperty;
import org.appwork.storage.flexijson.mapper.FlexiTypeMapper;
import org.appwork.storage.flexijson.mapper.interfacestorage.FlexiInterfaceDefault;
import org.appwork.storage.flexijson.mapper.interfacestorage.FlexiInterfaceDefaultFactory;
import org.appwork.storage.flexijson.mapper.interfacestorage.FlexiStorableInterface;
import org.appwork.storage.flexijson.mapper.typemapper.DateMapper;
import org.appwork.storage.flexijson.mapper.typemapper.TimeSpanMapper;
import org.appwork.storage.flexijson.mapper.typemapper.URLMapper;
import org.appwork.storage.simplejson.mapper.ClassCache;
import org.appwork.storage.simplejson.mapper.ClassCache.Rules;
import org.appwork.storage.simplejson.mapper.Getter;
import org.appwork.storage.simplejson.mapper.Setter;
import org.appwork.storage.validator.classvalidator.StorableClassValidator1;
import org.appwork.storage.validator.classvalidator.StorableClassValidator2;
import org.appwork.storage.validator.classvalidator.StorableClassValidator3;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.IgnoreInAWTest;
import org.appwork.utils.ClassPathScanner;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.StringUtils;
import org.appwork.utils.duration.TimeSpan;
import org.appwork.utils.reflection.Clazz;
import org.appwork.utils.reflection.CompiledType;

/**
 * @author thomas
 * @date 23.02.2022
 *
 */
public class FlexiStorageInterfaceValidatorTest extends AWTest {
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
                if (FlexiStorableInterface.class.isAssignableFrom(cls) && FlexiStorableInterface.class != cls) {
                    validateClass(cls);
                }
            }
        }.run();
    }

    protected boolean skipValidation(Class<?> cls) throws Exception {
        if (cls.getAnnotation(IgnoreInAWTest.class) != null) {
            return true;
        } else if (!cls.isInterface()) {
            return true;
        } else {
            return false;
        }
    }

    protected ClassCache createClassCache(Class<?> cls) throws Exception {
        return ClassCache.create(cls, new Rules().ignoreMissingConstructor(true));
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
        FlexiJSonNode defaultNode = null;
        HashSet<Class<?>> allowed = new HashSet<Class<?>>();
        allowed.add(StorableValidateCondition.class);
        allowed.add(StorableDoc.class);
        allowed.add(StorableExample.class);
        allowed.add(ApiDocExample.class);
        allowed.add(ApiDoc.class);
        allowed.add(StorableClassValidator1.class);
        allowed.add(StorableClassValidator2.class);
        allowed.add(StorableValidateRegex.class);
        allowed.add(StorableClassValidator3.class);
        allowed.add(StorableValidateMandatoryInJson.class);
        allowed.add(StorableValidateNotNull.class);
        allowed.add(StorableHidden.class);
        for (Annotation anno : method.getAnnotations()) {
            if (allowed.contains(anno.annotationType())) {
                continue;
            }
            String clsName = anno.annotationType().getName();
            if (clsName.startsWith("org.appwork.storage.config.annotations")) {
                throw new Exception("here:" + cls + "|method:" + method + ": " + clsName + " is not allowed for FlexiStorableInterface. This annotation is for ConfigInterface only");
            } else if (anno instanceof FlexiInterfaceDefault) {
                if (((FlexiInterfaceDefault) anno).validateInTests()) {
                    try {
                        defaultNode = new FlexiJSONParser(((FlexiInterfaceDefault) anno).value()).setIgnoreIssues(FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES).parse();
                    } catch (Exception e) {
                        throw new Exception("here:" + cls + "|method:" + method, e);
                    }
                }
            } else if (anno instanceof FlexiInterfaceDefaultFactory) {
                // TODO: check if factory generics match the actual type, and if we have factory AND FlexiInterfaceDefault
            } else if (anno instanceof FlexiJsonProperty) {
                if (StringUtils.isEmpty(((FlexiJsonProperty) anno).value())) {
                    throw new Exception("Empty JSONProperty name");
                }
            } else if (anno instanceof Deprecated) {
                // ok
            } else if (anno instanceof StorableValidatorIgnoresMissingGetter) {
                // ok
            } else if (anno instanceof StorableValidatorIgnoresMissingSetter) {
                // ok
            } else if (anno instanceof StorableValidateNotNull) {
                // ok
            } else if (anno instanceof StorableValidateMandatoryInJson) {
                // ok
            } else if (anno instanceof StorableLink) {
            } else if (anno instanceof StorableSee) {
            } else if (anno instanceof AllowNonStorableObjects) {
                // ok
            } else {
                throw new WTFException("Unexpected Annotation: " + clsName);
            }
        }
        if (defaultNode != null) {
            FlexiJSonMapper mapper = new FlexiJSonMapper();
            mapper.setIgnoreIllegalEnumMappings(false);
            mapper.setIgnoreIllegalArgumentMappings(false);
            mapper.setIgnorePrimitiveNullMapping(false);
            if (Clazz.isPrimitive(retClazz)) {
                mapper.jsonToObject(defaultNode, new SimpleTypeRef<Object>(retClazz));
            } else if (Clazz.isEnum(retClazz)) {
                mapper.jsonToObject(defaultNode, new SimpleTypeRef<Object>(retClazz));
            } else if (Clazz.isString(retClazz)) {
                mapper.jsonToObject(defaultNode, new SimpleTypeRef<Object>(retClazz));
            } else {
                List<ValidatorException> issues = new StorableValidator<Object>(defaultNode, new SimpleTypeRef<Object>(retClazz)).validate();
                if (issues.size() > 0) {
                    throw new Exception("here:" + cls + "|method:" + method + " - Storable Validation Issue: " + issues);
                }
                mapper.jsonToObject(defaultNode, new SimpleTypeRef<Object>(retClazz));
            }
        }
        // if (Clazz.isEnum(retClazz)) {
        //
        // if (defaultNode != null) {
        // try {
        // Enum.valueOf((Class<Enum>) retClazz, ((FlexiJSonValue) defaultNode).getValue().toString());
        // } catch (Exception e) {
        // throw new Exception("here:" + cls + "|method:" + method, e);
        // }
        // }
        // } else if (Enum[].class.isAssignableFrom(retClazz)) {
        // final Class<?> arrayEnumType = retClazz.getComponentType();
        //
        // if (dfEnumArrayValue != null && dfEnumArrayValue.value() != null) {
        // for (final String dfEnumValue : dfEnumArrayValue.value()) {
        // try {
        // final int index = dfEnumValue.lastIndexOf(".");
        // final String enumValue = dfEnumValue.substring(index + 1);
        // final String enumClazz = dfEnumValue.substring(0, index);
        // final Enum<?> value = Enum.valueOf((Class<Enum>) Class.forName(enumClazz), enumValue);
        // arrayEnumType.cast(value);
        // } catch (Exception e) {
        // throw new Exception("here:" + cls + "|method:" + method, e);
        // }
        // }
        // }
        // }
    }

    /**
     * @param cls
     * @throws Exception
     */
    public void validateClass(Class<?> cls) throws Exception {
        if (skipValidation(cls)) {
            return;
        }
        final ClassCache classCache = createClassCache(cls);
        if (Map.class.isAssignableFrom(cls)) {
            throw new Exception("Not Supported");
        }
        if (List.class.isAssignableFrom(cls)) {
            throw new Exception("Not Supported");
        }
        if (Set.class.isAssignableFrom(cls)) {
            throw new Exception("Not Supported");
        }
        checkNoGetterSetter(classCache, cls);
        for (Method m : CompiledType.create(cls).listMethods()) {
            if (m.getName().startsWith("get") || m.getName().startsWith("is") || m.getName().startsWith("set")) {
                if (Modifier.isPrivate(m.getModifiers()) && m.getAnnotation(StorableAllowPrivateAccessModifier.class) == null) {
                    throw new Exception("Found private getter or stetter without @StorableAllowPrivateAccessModifier Annotation " + m);
                }
                if (!Modifier.isPrivate(m.getModifiers()) && !Modifier.isPublic(m.getModifiers()) && m.getAnnotation(StorableAllowProtectedAccessModifier.class) == null) {
                    throw new Exception("Found protected private getter or stetter without  @StorableAllowProtectedAccessModifier Annotation " + m);
                }
            }
        }
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
                            } else {
                                throw new Exception("Method with name " + m + " exists multiple times with different parameters! " + (cls));
                            }
                        }
                    }
                }
                layer = layer.getSuperclass();
            }
            if (s == null && !hasAnnotation(classCache, key, StorableValidatorIgnoresMissingSetter.class)) {
                throw new Exception("setter " + cls.getName() + "." + key + " is missing - (" + g.getMethod().getDeclaringClass().getSimpleName() + ".java:1)");
            } else if (g == null && !hasAnnotation(classCache, key, StorableValidatorIgnoresMissingGetter.class)) {
                throw new Exception("getter " + cls.getName() + "." + key + " is missing - (" + s.getMethod().getDeclaringClass().getSimpleName() + ".java:1)");
            }
            if (g != null) {
                if (g.getMethod() == null) {
                    throw new Exception("Missing public getter " + cls + "." + key + " method. public field?");
                } else if (!Modifier.isPublic(g.getMethod().getModifiers())) {
                    throw new Exception("getter (" + g.getMethod().getDeclaringClass().getSimpleName() + ".java:1)" + "." + g.key + " is not public " + (cls));
                }
                checkAnnotations(classCache, cls, g.getMethod());
                try {
                    canStore(classCache, cls, g.getMethod(), g.getMethod().getGenericReturnType());
                } catch (InvalidTypeException e) {
                    LogV3.log(e);
                    throw new Exception("getter (" + g.getMethod().getDeclaringClass().getSimpleName() + ".java:1)" + "." + g.key + " returns a non-storable object " + (cls));
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
    private void canStore(final ClassCache classCache, final Class<?> cls, final Method method, final Type type) throws InvalidTypeException {
        FlexiStorageInterfaceValidatorTest.canStoreIntern(type, "", new FlexiStorageInterfaceValidatorTest.CanStoreRules() {
            @Override
            public boolean isFollowSuperClass(Type sc) {
                return false;
            }

            @Override
            public boolean isTypeWithlisted(Type gType, Method method) {
                CompiledType ct = CompiledType.create(gType, method.getDeclaringClass());
                if (ReflectionUtils.getAnnotation(gType, IgnoreInAWTest.class) != null) {
                    return true;
                }
                try {
                    if (ClassCache.getClassCache(method.getDeclaringClass()).getMethodAnnotations(method, AllowNonStorableObjects.class).size() > 0) {
                        return true;
                    }
                } catch (SecurityException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                } catch (NoSuchMethodException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
                AllowNonStorableObjects allowNonStorable = ReflectionUtils.getAnnotation(gType, AllowNonStorableObjects.class);
                if (ct.raw != null && allowNonStorable != null && Arrays.asList(allowNonStorable.value()).contains(ct.raw)) {
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
                        if (ct.raw != null && allowNonStorable != null && Arrays.asList(allowNonStorable.value()).contains(ct.raw)) {
                            return true;
                        }
                        if (layerCC.getAnnotations(ClassCache.createKey(method), IgnoreInAWTest.class).size() > 0) {
                            return true;
                        }
                        for (AllowNonStorableObjects anno : layerCC.getAnnotations(ClassCache.createKey(method), AllowNonStorableObjects.class)) {
                            if (ct.raw != null && anno != null && Arrays.asList(anno.value()).contains(ct.raw)) {
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
                            if (Arrays.asList(anno.value()).contains(ct.raw)) {
                                return true;
                            }
                        }
                        for (AllowNonStorableObjects anno : cc.getAnnotations(null, AllowNonStorableObjects.class)) {
                            if (Arrays.asList(anno.value()).contains(ct.raw)) {
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
                    if (allowNonStorable != null && Arrays.asList(allowNonStorable.value()).contains(ct.raw)) {
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
    private boolean hasAnnotation(ClassCache classCache, String key, Class<? extends Annotation> class1) {
        if (classCache.getAnnotations(key, StorableValidatorIgnoresMissingSetter.class).size() > 0) {
            return true;
        }
        if (classCache.getAnnotations(null, StorableValidatorIgnoresMissingSetter.class).size() > 0) {
            return true;
        }
        return false;
    }

    public static String sourceMethodToString(Method method) {
        if (method == null) {
            return "Unknown Source";
        }
        return method.getDeclaringClass().getSimpleName() + "." + method.getName();
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
    public static void canStoreIntern(final Type gType, final String path, FlexiStorageInterfaceValidatorTest.CanStoreRules rules, HashSet<Object> dupeID, Method method) throws InvalidTypeException {
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
            }
            if (type.isPrimitive()) {
                return;
            }
            if (type == Boolean.class || type == Long.class || type == Integer.class || type == Byte.class || type == Double.class || type == Float.class || type == String.class || type == Short.class) {
                return;
            }
            if (type.isEnum()) {
                return;
            }
            if (type.isArray()) {
                final Class<?> arrayType = type.getComponentType();
                FlexiStorageInterfaceValidatorTest.canStoreIntern(arrayType, path + "[" + arrayType + "]", rules, dupeID, method);
                return;
            }
            // we need an empty constructor
            if (List.class.isAssignableFrom(type)) {
                return;
            }
            if (Map.class.isAssignableFrom(type)) {
                return;
            }
            if (Set.class.isAssignableFrom(type)) {
                return;
            }
            if (FlexiStorableInterface.class.isAssignableFrom(type) || Storable.class.isAssignableFrom(type)) {
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
                                FlexiStorageInterfaceValidatorTest.canStoreIntern(m.getGenericReturnType(), path + "->" + m.getGenericReturnType(), rules, dupeID, m);
                            } else if (m.getName().startsWith("set")) {
                                if (m.getParameterTypes().length != 1) {
                                    throw new InvalidTypeException(gType, "Setter " + path + "." + m + " has != 1 Parameters." + " " + sourceMethodToString(method));
                                }
                                FlexiStorageInterfaceValidatorTest.canStoreIntern(m.getGenericParameterTypes()[0], path + "->" + m.getGenericParameterTypes()[0], rules, dupeID, m);
                            } else if (m.getName().startsWith("is")) {
                                if (m.getParameterTypes().length > 0) {
                                    throw new InvalidTypeException(gType, "Getter " + path + "." + m + " has parameters." + " " + sourceMethodToString(method));
                                }
                                FlexiStorageInterfaceValidatorTest.canStoreIntern(m.getGenericReturnType(), path + "->" + m.getGenericReturnType(), rules, dupeID, m);
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
                    throw new InvalidTypeException(gType, "FlexiStorableInterface " + path + " has no empty Constructor" + " " + sourceMethodToString(method));
                }
            }
        } else if (gType instanceof ParameterizedType) {
            final ParameterizedType ptype = (ParameterizedType) gType;
            final Type raw = ptype.getRawType();
            FlexiStorageInterfaceValidatorTest.canStoreIntern(raw, path, rules, dupeID, method);
            for (final Type t : ptype.getActualTypeArguments()) {
                FlexiStorageInterfaceValidatorTest.canStoreIntern(t, path + "(" + t + ")", rules, dupeID, method);
            }
            return;
        } else if (gType instanceof GenericArrayType) {
            final GenericArrayType atype = (GenericArrayType) gType;
            final Type t = atype.getGenericComponentType();
            FlexiStorageInterfaceValidatorTest.canStoreIntern(t, path + "[" + t + "]", rules, dupeID, method);
            return;
        } else if (gType instanceof TypeVariable) {
            // type veriables are checked one layer above
            return;
        } else {
            throw new InvalidTypeException(gType, "Generic Type Structure not implemented: " + gType.getClass() + " in " + path + " " + sourceMethodToString(method));
        }
        for (FlexiTypeMapper m : new FlexiJSonMapper().getTypeMapper()) {
            if (m instanceof DateMapper) {
                if (gType == Date.class) {
                    return;
                }
            } else if (m instanceof TimeSpanMapper) {
                if (gType == TimeSpan.class) {
                    return;
                }
            } else if (m instanceof URLMapper) {
                if (gType == URL.class) {
                    return;
                }
            }
        }
        ArrayList<FlexiTypeMapper> mappers = FlexiJSonMapper.getThreadMappers();
        if (mappers != null) {
            for (FlexiTypeMapper m : mappers) {
                if (m instanceof DateMapper) {
                    if (gType == Date.class) {
                        return;
                    }
                } else if (m instanceof TimeSpanMapper) {
                    if (gType == TimeSpan.class) {
                        return;
                    }
                } else if (m instanceof URLMapper) {
                    if (gType == URL.class) {
                        return;
                    }
                }
            }
        }
        throw new InvalidTypeException(gType, "Type " + path + "/" + gType + " is not supported." + " " + sourceMethodToString(method));
    }
}
