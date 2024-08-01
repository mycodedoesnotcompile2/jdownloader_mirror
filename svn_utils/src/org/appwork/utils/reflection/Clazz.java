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
package org.appwork.utils.reflection;

import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;

/**
 * @author thomas
 *
 */
public class Clazz {
    private static final Map<Class<?>, Class<?>> PRIMITIVE_WRAPPER_MAP = new HashMap<Class<?>, Class<?>>();
    static {
        PRIMITIVE_WRAPPER_MAP.put(boolean.class, Boolean.class);
        PRIMITIVE_WRAPPER_MAP.put(byte.class, Byte.class);
        PRIMITIVE_WRAPPER_MAP.put(char.class, Character.class);
        PRIMITIVE_WRAPPER_MAP.put(double.class, Double.class);
        PRIMITIVE_WRAPPER_MAP.put(float.class, Float.class);
        PRIMITIVE_WRAPPER_MAP.put(int.class, Integer.class);
        PRIMITIVE_WRAPPER_MAP.put(long.class, Long.class);
        PRIMITIVE_WRAPPER_MAP.put(short.class, Short.class);
    }
    private static final Map<Class<?>, Class<?>> WRAPPER_PRIMITIVE_MAP = new HashMap<Class<?>, Class<?>>();
    static {
        WRAPPER_PRIMITIVE_MAP.put(Boolean.class, boolean.class);
        WRAPPER_PRIMITIVE_MAP.put(Byte.class, byte.class);
        WRAPPER_PRIMITIVE_MAP.put(Character.class, char.class);
        WRAPPER_PRIMITIVE_MAP.put(Double.class, double.class);
        WRAPPER_PRIMITIVE_MAP.put(Float.class, float.class);
        WRAPPER_PRIMITIVE_MAP.put(Integer.class, int.class);
        WRAPPER_PRIMITIVE_MAP.put(Long.class, long.class);
        WRAPPER_PRIMITIVE_MAP.put(Short.class, short.class);
    }

    /**
     * returns true if type is a boolean. No Matter if primitive or it's object wrapper
     *
     * @param type
     * @return
     */
    public static boolean isBoolean(final Type type) {
        return type == Boolean.class || type == boolean.class;
    }

    /**
     * returns true if type is a byte. No Matter if primitive or it's object wrapper
     *
     * @param type
     * @return
     */
    public static boolean isByte(final Type type) {
        return type == Byte.class || type == byte.class;
    }

    /**
     * returns true if type is a char. No Matter if primitive or it's object wrapper
     *
     * @param type
     * @return
     */
    public static boolean isCharacter(final Type type) {
        return type == Character.class || type == char.class;
    }

    /**
     * returns true if type is a double. No Matter if primitive or it's object wrapper
     *
     * @param type
     * @return
     */
    public static boolean isDouble(final Type type) {
        return type == Double.class || type == double.class;
    }

    /**
     * returns true if type is a float. No Matter if primitive or it's object wrapper
     *
     * @param type
     * @return
     */
    public static boolean isFloat(final Type type) {
        return type == Float.class || type == float.class;
    }

    /**
     * returns true if type is a int. No Matter if primitive or it's object wrapper
     *
     * @param type
     * @return
     */
    public static boolean isInteger(final Type type) {
        return type == Integer.class || type == int.class;
    }

    /**
     * returns true if type is a long. No Matter if primitive or it's object wrapper
     *
     * @param type
     * @return
     */
    public static boolean isLong(final Type type) {
        return type == Long.class || type == long.class;
    }

    /**
     * returns true if type is a primitive or a priomitive object wrapper
     *
     * @param type
     * @return
     */
    public static boolean isPrimitive(final Type type) {
        if (type instanceof Class) {
            return ((Class<?>) type).isPrimitive() || Clazz.isPrimitiveWrapper(type);
        } else {
            return false;
        }
    }

    /**
     * returns true if type os a primitive object wrapper
     *
     * @param type
     * @return
     */
    public static boolean isPrimitiveWrapper(final Type type) {
        return type == Boolean.class || type == Integer.class || type == Long.class || type == Byte.class || type == Short.class || type == Float.class || type == Double.class || type == Character.class || type == Void.class;
    }

    /**
     * returns true if type is a short. No Matter if primitive or it's object wrapper
     *
     * @param type
     * @return
     */
    public static boolean isShort(final Type type) {
        return type == Short.class || type == short.class;
    }

    /**
     * returns true if type is a void. No Matter if primitive or it's object wrapper
     *
     * @param type
     * @return
     */
    public static boolean isVoid(final Type type) {
        return type == Void.class || type == void.class;
    }

    /**
     * @param type
     * @return
     */
    public static boolean isString(final Type type) {
        return type == String.class;
    }

    /**
     * WARNING. returns true for anonymous classes like class MyEnum{A(){}} -> Clazz.isEnum(MyEnum.A.getClass())
     *
     * @param type
     * @return
     */
    public static boolean isEnum(Type type) {
        type = getNonAnonymousType(type);
        return type instanceof Class && ((Class<?>) type).isEnum();
    }

    /**
     * @param type
     * @return
     */
    public static Type getNonAnonymousType(Type type) {
        if (type instanceof Class && ((Class) type).isAnonymousClass()) {
            return ((Class) type).getGenericSuperclass();
        } else {
            return type;
        }
    }

    /**
     * is a instanceof b
     *
     * @param c
     * @param class1
     * @return
     */
    public static boolean isInstanceof(final Type a, final Class<?> b) {
        if (a instanceof Class) {
            final boolean ret = b.isAssignableFrom((Class) a);
            return ret;
        } else {
            return false;
        }
    }

    /**
     * @param enumClass
     * @return
     */
    public static boolean isArray(Type type) {
        return type instanceof Class && ((Class) type).isArray();
    }

    /**
     * @param a
     * @return
     */
    public static boolean isFixedPointNumber(Type a) {
        try {
            final Class ac = (Class) a;
            if (ac.isPrimitive()) {
                if (a == float.class || a == double.class || a == void.class || a == boolean.class || a == char.class) {
                    return false;
                } else {
                    return true;
                }
            } else if (Number.class.isAssignableFrom(ac)) {
                if (a == Float.class || a == Double.class || BigDecimal.class.isAssignableFrom(ac)) {
                    return false;
                } else {
                    return true;
                }
            } else {
                return false;
            }
        } catch (ClassCastException e) {
            return false;
        } catch (NullPointerException e) {
            return false;
        }
    }

    public static boolean isNumberType(Type a) {
        if (!(a instanceof Class)) {
            return false;
        } else if (((Class) a).isPrimitive()) {
            return !(a == void.class || a == boolean.class || a == char.class);
        } else if (Number.class.isAssignableFrom((Class) a)) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * @param a
     * @return
     */
    public static boolean isFloatingPointNumber(Type a) {
        try {
            final Class ac = (Class) a;
            if (ac.isPrimitive()) {
                return a == double.class || a == float.class;
            } else {
                return a == Double.class || a == Float.class || BigDecimal.class.isAssignableFrom(ac);
            }
        } catch (ClassCastException e) {
            return false;
        } catch (NullPointerException e) {
            return false;
        }
    }

    /**
     * returns true, if p is represented by expected - in other words: if object p could be assigned to a <expected> variable
     *
     * @param p
     * @param expected
     * @return
     */
    public static boolean objectIsTypeOf(Object p, Class<?> expected) {
        if (expected.isPrimitive() && p == null) {
            return false;
        } else if (p == null) {
            return true;
        } else {
            return isAssignable(p.getClass(), expected);
        }
    }

    public static boolean isAssignable(Class<?> from, Class<?> to) {
        if (to.isAssignableFrom(from)) {
            return true;
        }
        if (from.isPrimitive()) {
            if (PRIMITIVE_WRAPPER_MAP.get(from) == to) {
                return true;
            }
        }
        if (to.isPrimitive()) {
            if (PRIMITIVE_WRAPPER_MAP.get(to) == from) {
                return true;
            }
        }
        return false;
    }

    /**
     * @param genericReturnType
     * @param cType
     * @return
     */
    public static boolean equalsIgnorePrimitive(Type a, Type b) {
        return primitiveToWrapper(a).equals(primitiveToWrapper(b));
    }

    /**
     * @param a
     * @return
     */
    public static Type primitiveToWrapper(Type a) {
        final Class<?> ret = PRIMITIVE_WRAPPER_MAP.get(a);
        if (ret != null) {
            return ret;
        } else {
            return a;
        }
    }

    public static Class<?> wrapperToPrimitive(Class<?> a) {
        final Class<?> ret = WRAPPER_PRIMITIVE_MAP.get(a);
        if (ret != null) {
            return ret;
        } else {
            return a;
        }
    }
}
