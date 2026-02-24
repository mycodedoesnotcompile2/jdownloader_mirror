package org.appwork.storage.tests;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.moncompare.Condition;
import org.appwork.remoteapi.RemoteAPIInterface;
import org.appwork.remoteapi.RemoteAPIRequest;
import org.appwork.remoteapi.RemoteAPIResponse;
import org.appwork.remoteapi.annotations.APIReturnTypes;
import org.appwork.remoteapi.annotations.AllowNonStorableObjects;
import org.appwork.storage.Storable;
import org.appwork.storage.simplejson.mapper.ClassCache;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.IgnoreInAWTest;
import org.appwork.testframework.TestDependency;
import org.appwork.utils.ClassPathScanner;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.reflection.Clazz;
import org.appwork.utils.reflection.CompiledType;

@TestDependency({ "org.appwork.remoteapi.RemoteAPIInterface" })
public class RemoteAPI1InterfaceValidator extends AWTest {
    private Set<Class<?>> scannedClassPathClasses = Collections.emptySet();

    public static void main(String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        final List<Class<?>> tested = new ArrayList<Class<?>>();
        final LinkedHashSet<Class<?>> allClasses = new LinkedHashSet<Class<?>>();
        final LinkedHashSet<Class<?>> apiInterfaces = new LinkedHashSet<Class<?>>();
        try {
            Class<?> api2 = null;
            try {
                api2 = Class.forName("org.appwork.apiserver.client.shared.interfaces.AbstractAPIServerInterface");
            } catch (Exception e) {
            }
            final Class<?> finalApi2 = api2;
            new ClassPathScanner<Throwable>() {
                @Override
                public void handle(Class<?> cls) throws Throwable {
                    allClasses.add(cls);
                    // there is an extra validator for AbstractAPIServerInterface
                    if (RemoteAPIInterface.class.isAssignableFrom(cls) && RemoteAPIInterface.class != cls && cls.isInterface()) {
                        if (finalApi2 != null && finalApi2.isAssignableFrom(cls)) {
                            // APi2 has a dedicated validator
                            return;
                        }
                        apiInterfaces.add(cls);
                    }
                }
            }.run();
            scannedClassPathClasses = allClasses;
            for (final Class<?> apiInterface : apiInterfaces) {
                validateClass(apiInterface);
                tested.add(apiInterface);
            }
        } catch (Throwable e) {
            if (e instanceof Exception) {
                throw ((Exception) e);
            }
            throw new WTFException(e);
        }
    }

    /**
     * @param cls
     * @throws Throwable
     */
    protected void validateClass(Class<?> cls) throws Throwable {
        LogV3.info("Test RemoteAPIInterface " + cls);
        Class<?> layer = cls;
        while (layer != null) {
            for (Method m : layer.getDeclaredMethods()) {
                Type type = m.getGenericReturnType();
                if (type != null && type != void.class) {
                    validateAPIReturnTypesCoverage(m, type);
                    checkTypeStorable(m, type);
                }
                for (Type paramType : m.getGenericParameterTypes()) {
                    if (paramType == RemoteAPIRequest.class) {
                        continue;
                    }
                    if (paramType == RemoteAPIResponse.class) {
                        continue;
                    }
                    checkTypeStorable(m, paramType);
                }
            }
            layer = layer.getSuperclass();
        }
    }

    private void validateAPIReturnTypesCoverage(final Method method, final Type returnType) throws Exception {
        final Class<?> rawReturnType = ReflectionUtils.getRaw(returnType);
        if (rawReturnType == null) {
            return;
        }
        if (!Storable.class.isAssignableFrom(rawReturnType)) {
            return;
        }
        if (rawReturnType.isPrimitive() || rawReturnType.isEnum() || rawReturnType.isArray() || Modifier.isFinal(rawReturnType.getModifiers())) {
            return;
        }
        if (rawReturnType.getName().startsWith("java.")) {
            return;
        }
        final Set<Class<?>> derivedConcreteTypes = findConcreteDerivedTypes(rawReturnType);
        if (derivedConcreteTypes.isEmpty()) {
            return;
        }
        final APIReturnTypes returnTypes = method.getAnnotation(APIReturnTypes.class);
        final LinkedHashSet<Class<?>> declared = new LinkedHashSet<Class<?>>();
        if (returnTypes != null) {
            declared.addAll(Arrays.asList(returnTypes.value()));
        }
        final List<String> missing = new ArrayList<String>();
        for (final Class<?> concreteType : derivedConcreteTypes) {
            if (!declared.contains(concreteType)) {
                missing.add(concreteType.getName());
            }
        }
        final List<String> invalid = new ArrayList<String>();
        for (final Class<?> declaredType : declared) {
            if (declaredType == null || declaredType == rawReturnType) {
                continue;
            }
            if (!rawReturnType.isAssignableFrom(declaredType)) {
                invalid.add(declaredType.getName());
            }
        }
        if (!missing.isEmpty() || !invalid.isEmpty()) {
            throw new Exception("Incomplete @APIReturnTypes on " + method.getDeclaringClass().getName() + "#" + method.getName() + " (base=" + rawReturnType.getName() + ")" + (missing.isEmpty() ? "" : " missing=" + missing) + (invalid.isEmpty() ? "" : " invalid=" + invalid));
        }
    }

    private Set<Class<?>> findConcreteDerivedTypes(final Class<?> baseType) {
        final LinkedHashSet<Class<?>> ret = new LinkedHashSet<Class<?>>();
        for (final Class<?> candidate : scannedClassPathClasses) {
            if (candidate == null || candidate == baseType) {
                continue;
            }
            if (!baseType.isAssignableFrom(candidate)) {
                continue;
            }
            if (candidate.isInterface() || Modifier.isAbstract(candidate.getModifiers())) {
                continue;
            }
            if (candidate.isAnonymousClass() || candidate.isLocalClass() || candidate.isSynthetic()) {
                continue;
            }
            final String name = candidate.getName();
            if (name.contains(".test.") || name.contains(".tests.")) {
                continue;
            }
            ret.add(candidate);
        }
        return ret;
    }

    /**
     * @param method
     * @param paramType
     * @throws Throwable
     */
    private void checkTypeStorable(Method method, final Type paramType) throws Throwable {
        try {
            if (Clazz.isPrimitive(paramType)) {
                return;
            }
            if (paramType == String.class) {
                return;
            }
            if (paramType == String[].class) {
                return;
            }
            if (paramType instanceof Class) {
                if (((Class) paramType).isArray()) {
                    if (Clazz.isPrimitive(paramType)) {
                        return;
                    }
                    if (paramType == String.class) {
                        return;
                    }
                }
            }
            CompiledType ct = CompiledType.create(paramType);
            if (ct.isInstanceOf(Condition.class)) {
                // skip generics check. Generics are no storables here
            } else {
                if (paramType instanceof ParameterizedType) {
                    for (Type type : ((ParameterizedType) paramType).getActualTypeArguments()) {
                        checkTypeStorable(method, type);
                    }
                }
            }
            Class<?> raw = ReflectionUtils.getRaw(paramType);
            if (List.class.isAssignableFrom(raw)) {
                return;
            }
            if (Map.class.isAssignableFrom(raw)) {
                return;
            }
            StorableValidatorTest.canStoreIntern(paramType, "", new StorableValidatorTest.CanStoreRules() {
                @Override
                public boolean isFollowSuperClass(Type sc) {
                    return false;
                }

                @Override
                public boolean isTypeWithlisted(Type gType, Method method) {
                    Class<?> raw = ReflectionUtils.getRaw(gType);
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
                    Class<?> layer = ReflectionUtils.getRaw(paramType);
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
            if (Storable.class.isAssignableFrom(raw)) {
                new StorableValidatorTest().validateClass(raw);
            }
        } catch (Throwable e) {
            throw e;
        }
    }
}
