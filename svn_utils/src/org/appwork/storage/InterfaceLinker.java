/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
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
package org.appwork.storage;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.Arrays;

import org.appwork.exceptions.WTFException;
import org.appwork.moncompare.Condition;
import org.appwork.moncompare.object.MapAccessorInterface;
import org.appwork.moncompare.typehandler.FlexiTypeHandler;
import org.appwork.serializer.Deser;
import org.appwork.storage.simplejson.mapper.ClassCache;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.DebugMode;
import org.appwork.utils.reflection.Clazz;
import org.appwork.utils.reflection.CompiledType;

/**
 * @author thomas
 * @date 28.05.2025
 *
 */
public class InterfaceLinker {
    public static class InterfaceLink<TargetType> implements InvocationHandler {
        private CompiledType         cType;
        private Object               backend;
        private MapAccessorInterface map;
        private Method[]             methods;

        /**
         * @param backend
         * @param target
         */
        public InterfaceLink(Object backend, TypeRef<TargetType> target) {
            this.backend = backend;
            this.cType = CompiledType.create(target.getType());
            Condition<TargetType> condition = new Condition<TargetType>();
            condition.setTypeHandler(Arrays.asList(new FlexiTypeHandler()));
            map = condition.getMapWrapper(backend);
            methods = backend.getClass().getMethods();
        }

        /**
         * @see java.lang.reflect.InvocationHandler#invoke(java.lang.Object, java.lang.reflect.Method, java.lang.Object[])
         */
        @Override
        public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
            try {
                if (proxy == null) {
                    proxy = this.backend;
                }
                final Class<?> returnType = method.getReturnType();
                final int parameterCount = ClassCache.getParameterCount(method);
                if ("toString".equals(method.getName()) && Clazz.isString(returnType) && parameterCount == 0) {
                    return cType.getTypeName() + "-proxy:\r\n" + backend;
                } else if ("hashCode".equals(method.getName()) && Clazz.isInteger(returnType) && parameterCount == 0) {
                    return backend.hashCode();
                } else if ("equals".equals(method.getName()) && Clazz.isBoolean(returnType) && parameterCount == 1 && method.getParameterTypes()[0] == Object.class) {
                    return equals(proxy, args);
                } else {
                    for (Method m : methods) {
                        if (m.getName().equals(method.getName())) {
                            Class<?>[] types = m.getParameterTypes();
                            Class<?>[] methodTypes = method.getParameterTypes();
                            if (types.length == methodTypes.length) {
                                boolean matches = m.getReturnType().equals(method.getReturnType());
                                for (int i = 0; i < types.length; i++) {
                                    if (!types[i].equals(methodTypes[i])) {
                                        matches = false;
                                        break;
                                    }
                                }
                                if (matches) {
                                    // exact same method
                                    Object ret = m.invoke(backend, args);
                                    if (ret == null) {
                                        return Clazz.getDefaultValue(returnType);
                                    }
                                    return Deser.convert(ret, new SimpleTypeRef<Object>(returnType));
                                }
                            }
                        }
                    }
                    final String key = ClassCache.createKey(method);
                    if (parameterCount == 1 && returnType == void.class) {
                        if (map != null) {
                            map.put(key, args[0]);
                            return null;
                        }
                        return null;
                    } else if (parameterCount == 0 && returnType != void.class) {
                        if (map != null) {
                            Object ret = map.get(key);
                            if (ret == Condition.KEY_DOES_NOT_EXIST) {
                                return Clazz.getDefaultValue(returnType);
                            }
                            if (ret == null) {
                                return Clazz.getDefaultValue(returnType);
                            }
                            return Deser.convert(ret, new SimpleTypeRef<Object>(returnType));
                        }
                    }
                    for (Method m : methods) {
                        if (m.getName().equals(method.getName())) {
                            Class<?>[] types = m.getParameterTypes();
                            Class<?>[] methodTypes = method.getParameterTypes();
                            if (types.length == methodTypes.length) {
                                try {
                                    boolean matches = true;
                                    Object[] convertArgs = new Object[args.length];
                                    for (int i = 0; i < types.length; i++) {
                                        convertArgs[i] = Deser.convert(args[i], new SimpleTypeRef<Object>(types[i]));
                                    }
                                    if (matches) {
                                        // exact same method
                                        Object ret = m.invoke(backend, convertArgs);
                                        if (ret == null) {
                                            return Clazz.getDefaultValue(returnType);
                                        }
                                        return Deser.convert(ret, new SimpleTypeRef<Object>(returnType));
                                    }
                                } catch (Exception e) {
                                    e.printStackTrace();
                                    // conversion failed;
                                }
                            }
                        }
                    }
                    throw new WTFException("Invalid method call:" + method);
                }
            } catch (Exception e) {
                e.printStackTrace();
                DebugMode.debugger();
                throw new WTFException(e);
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
            } else if (handler instanceof InterfaceLink) {
                @SuppressWarnings("unchecked")
                InterfaceLink other = (InterfaceLink) handler;
                return CompareUtils.equals(other.backend, this.backend);
            }
            return false;
        }
    }

    /**
     * @param <T>
     * @param e
     * @param class1
     * @return
     */
    public <T> T build(Object backend, TypeRef<T> target) {
        Class cls = ((Class) target.getType());
        T ret = (T) Proxy.newProxyInstance(cls.getClassLoader(), new Class[] { cls }, new InterfaceLink(backend, target));
        return ret;
    }
}
