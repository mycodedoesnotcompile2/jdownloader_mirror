package org.appwork.utils;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.storage.simplejson.mapper.ClassCache;
import org.appwork.storage.simplejson.mapper.Getter;
import org.appwork.utils.reflection.Clazz;

public class CompareUtils {
    /**
     *
     * @deprecated Use the compareInt instead
     */
    @Deprecated
    public static int compare(final int x, final int y) {
        return compareInt(x, y);
    }

    /**
     *
     * @deprecated Use the compareLong instead
     */
    @Deprecated
    public static int compare(final long x, final long y) {
        return compareLong(x, y);
    }

    /**
     *
     * @deprecated Use the compareBoolean instead
     */
    @Deprecated
    public static int compare(final boolean x, final boolean y) {
        return compareBoolean(x, y);
    }

    /**
     *
     * @deprecated Use the compareDouble instead
     */
    @Deprecated
    public static int compare(final double x, final double y) {
        return compareDouble(x, y);
    }

    /**
     *
     * @deprecated Use the compareFloat instead
     */
    @Deprecated
    public static int compare(final float x, final float y) {
        return compareFloat(x, y);
    }

    /**
     * @param x
     * @param y
     * @return
     */
    public static int compareBoolean(final boolean x, final boolean y) {
        return (x == y) ? 0 : (x ? 1 : -1);
    }

    /**
     * @param height
     * @param height2
     * @return
     *
     *
     */
    public static int compareInt(final int x, final int y) {
        return (x < y) ? -1 : ((x == y) ? 0 : 1);
    }

    public static int compareLong(final long x, final long y) {
        return (x < y) ? -1 : ((x == y) ? 0 : 1);
    }

    /**
     *
     * @param x
     * @param y
     * @return <0 if x<y >0 if x>y 0 if x==y
     */
    public static int compareDouble(final double x, final double y) {
        // since 1.4
        return Double.compare(x, y);
    }

    public static int compareFloat(final float x, final float y) {
        // since 1.4
        return Float.compare(x, y);
    }

    /**
     * @param <T>
     * @param projection
     * @param projection2
     * @return
     * @deprecated - Use compareTo instead to avoid type issues.
     */
    @Deprecated
    public static int compare(final Comparable x, final Comparable y) {
        return compareComparable(x, y);
    }

    public static <T> int compareComparable(final Comparable<T> x, final Comparable<T> y) {
        if (x == y) {
            return 0;
        } else if (x == null) {
            return -1;
        } else if (y == null) {
            return 1;
        } else {
            if (x instanceof Number && y instanceof Number) {
                return compareNumber((Number) x, (Number) y);
            } else {
                return x.compareTo((T) y);
            }
        }
    }

    /**
     * @param hash
     * @param hash2
     * @deprecated use Arrays.equals instead
     * @return
     */
    @Deprecated
    public static boolean equals(final byte[] hash, final byte[] hash2) {
        return Arrays.equals(hash, hash2);
    }

    /**
     * returns true of both objects are null or their.equals method match
     *
     * @param a
     * @param b
     * @return
     */
    public static boolean equals(final Object a, final Object b) {
        if (a == b) {
            return true;
        } else if (a == null || b == null) {
            return false;
        } else if (a instanceof Number && b instanceof Number) {
            return equalsNumber((Number) a, (Number) b);
        } else {
            // Objects.equals(a, b) is 1.7+
            return a.equals(b);
        }
    }

    /**
     * this.method does not go rekursive. the values of the maps are compared via org.appwork.utils.CompareUtils.equalsObjects(Object,
     * Object) (both null or equals
     *
     * @param extensions
     * @param extensions2
     * @return
     */
    public static boolean equals(final Map<?, ?> a, final Map<?, ?> b) {
        if (a == b) {
            return true;
        } else if (a == null || b == null) {
            return false;
        } else if (a.size() != b.size()) {
            return false;
        } else {
            for (final Entry<?, ?> es : a.entrySet()) {
                if (!equals(es.getValue(), b.get(es.getKey()))) {
                    return false;
                }
            }
            return false;
        }
    }

    public static boolean equals(final Set<?> a, final Set<?> b) {
        if (a == b) {
            return true;
        } else if (a == null || b == null) {
            return false;
        } else {
            // this must work for sets according to the interface docs
            return a.equals(b);
        }
    }

    public static boolean equalsDeep(final Object objectX, final Object objectY, final Equalator equalator) {
        return equalsDeep(objectX, objectY, equalator, new HashSet<Couple>());
    }

    public static boolean equalsDeep(final Object objectX, final Object objectY) {
        return equalsDeep(objectX, objectY, null, new HashSet<Couple>());
    }

    private static class Couple {
        /**
         * @param objectX
         * @param objectY
         */
        public Couple(final Object objectX, final Object objectY) {
            this.a = objectX;
            this.b = objectY;
        }

        private int          hashCode = -1;
        private final Object a;
        private final Object b;

        /*
         * (non-Javadoc)
         *
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            int ret = this.hashCode;
            if (ret == -1) {
                ret = 0;
                if (this.a != null) {
                    ret += this.a.hashCode();
                }
                if (this.b != null) {
                    ret += this.b.hashCode();
                }
                this.hashCode = ret;
            }
            return ret;
        }

        /*
         * (non-Javadoc)
         *
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(final Object obj) {
            if (obj == this) {
                return true;
            } else if (!(obj instanceof Couple)) {
                return false;
            } else {
                return CompareUtils.equals(this.a, ((Couple) obj).a) && CompareUtils.equals(this.b, ((Couple) obj).b);
            }
        }
    }

    public static boolean equalsDeep(final Object objectX, final Object objectY, final Equalator customEuualator, final HashSet<Couple> dupeCheck) {
        if (objectX == objectY) {
            return true;
        } else if (objectX == null || objectY == null) {
            return false;
        } else if (objectX instanceof Number && objectY instanceof Number) {
            return equalsNumber((Number) objectX, (Number) objectY);
        } else if (!dupeCheck.add(new Couple(objectX, objectY))) {
            // the object combination is already in the stack - no reason to reject
            return true;
        } else if (customEuualator != null) {
            final Object ret = customEuualator.equals(objectX, objectY);
            if (ret == Boolean.TRUE) {
                return true;
            } else if (ret == Boolean.FALSE) {
                return false;
            }
        }
        if (!(objectX instanceof ClassUsesDeepEquals) && objectX.equals(objectY)) {
            // if equals says these objects equal, we trust
            return true;
        } else if (ReflectionUtils.isListOrArray(objectX) && ReflectionUtils.isListOrArray(objectY)) {
            final List<Object> listX = ReflectionUtils.wrapUnmodifiableList(objectX, Object.class);
            final List<Object> listY = ReflectionUtils.wrapUnmodifiableList(objectY, Object.class);
            if (listX.size() != listY.size()) {
                return false;
            }
            final ListIterator<Object> e1 = listX.listIterator();
            final ListIterator<Object> e2 = listY.listIterator();
            while (e1.hasNext() && e2.hasNext()) {
                final Object o1 = e1.next();
                final Object o2 = e2.next();
                if (!(o1 == null ? o2 == null : equalsDeep(o1, o2, customEuualator, dupeCheck))) {
                    return false;
                }
            }
            return !(e1.hasNext() || e2.hasNext());
        } else if (objectX instanceof Set && objectY instanceof Set) {
            if (((Set) objectX).size() != ((Set) objectY).size()) {
                return false;
            }
            main: for (final Object x : ((Set) objectX)) {
                for (final Object y : ((Set) objectY)) {
                    if (equalsDeep(x, y, customEuualator, dupeCheck)) {
                        continue main;
                    }
                }
                // entry not found.
                return false;
            }
            return true;
        } else if (objectX instanceof Map && objectY instanceof Map) {
            if (((Map<?, ?>) objectX).size() != ((Map<?, ?>) objectY).size()) {
                return false;
            }
            for (final Entry<?, ?> es : ((Map<?, ?>) objectX).entrySet()) {
                final Object other = ((Map<?, ?>) objectY).get(es.getKey());
                if (other == null && !((Map<?, ?>) objectY).containsKey(es.getKey())) {
                    return false;
                } else if (!equalsDeep(es.getValue(), other, customEuualator, dupeCheck)) {
                    return false;
                }
            }
            return true;
        } else if (objectX.getClass() == objectY.getClass()) {
            if (Clazz.isPrimitive(objectX.getClass()) || Clazz.isEnum(objectX.getClass()) || Clazz.isString(objectX.getClass())) {
                // if true, this would have exited in the } else if (objectX.equals(objectY)) { block above
                return false;
            }
            try {
                if (!(objectX instanceof ClassUsesDeepEquals)) {
                    // if the class has equals implemented, there is no reason to go "deeper"
                    final Method hasEquals = objectX.getClass().getMethod("equals", Object.class);
                    if (hasEquals.getDeclaringClass() != Object.class) {
                        return false;
                    }
                }
            } catch (final NoSuchMethodException e1) {
                throw new WTFException(e1);
            } catch (final SecurityException e1) {
                throw new WTFException(e1);
            }
            try {
                final ClassCache cc = ClassCache.getClassCache(objectX.getClass());
                for (final Getter c : cc.getGetter()) {
                    if (!equalsDeep(c.getValue(objectX), c.getValue(objectY), customEuualator, dupeCheck)) {
                        return false;
                    }
                }
                return true;
            } catch (final SecurityException e) {
                throw new WTFException(e);
            } catch (final NoSuchMethodException e) {
                return false;
            } catch (final IllegalArgumentException e) {
                throw new WTFException(e);
            } catch (final IllegalAccessException e) {
                throw new WTFException(e);
            } catch (final InvocationTargetException e) {
                throw new WTFException(e);
            }
        } else {
            return objectX.equals(objectY);
        }
    }

    /**
     * @param number
     * @param number2
     * @return
     */
    public static int compareNumber(final Number a, final Number b) {
        final Class<?> ac = a.getClass();
        final Class<?> bc = b.getClass();
        if (ac == Double.class || bc == Double.class) {
            return compareDouble(a.doubleValue(), b.doubleValue());
        } else if (ac == Float.class || bc == Float.class) {
            return compareFloat(a.floatValue(), b.floatValue());
        } else {
            return compareLong(a.longValue(), b.longValue());
        }
    }

    /**
     * @param value
     * @param query
     * @return
     */
    public static boolean equalsNumber(final Number a, final Number b) {
        if (a == null && b == null) {
            return true;
        } else if (a == null || b == null) {
            return false;
        } else {
            return compareNumber(a, b) == 0;
        }
    }

    public static int hashCodeDeep(final Object obj) {
        return hashCodeDeep(obj, null);
    }

    /**
     * @param addressDetails
     * @return
     */
    public static int hashCodeDeep(final Object obj, final CompareUtilsHashCodeCalculator hashCode) {
        if (obj == null) {
            return 0;
        }
        try {
            obj.getClass().getDeclaredMethod("hashCode", new Class[0]);
            // has a dedicated hashcode method;
            if (!(obj instanceof ClassUsesDeepHashCode)) {
                return obj.hashCode();
            }
        } catch (final NoSuchMethodException e) {
            // continue deep
        }
        if (obj instanceof java.util.Collection) {
            // order is not important because we sum up the hashcodes. this loop handles Sets as well
            int ret = 0;
            int i = 0;
            for (final Object s : (java.util.Collection) obj) {
                i++;
                if (hashCode != null && hashCode.hasCustomHashCode(s, false)) {
                    ret = hashCode.calcValueHashCode(ret, s);
                } else {
                    ret += i * hashCodeDeep(s, hashCode);
                }
            }
            return ret;
        } else if (ReflectionUtils.isListOrArray(obj)) {
            int ret = 0;
            final List<Object> listX = ReflectionUtils.wrapUnmodifiableList(obj, Object.class);
            final ListIterator<Object> it = listX.listIterator();
            while (it.hasNext()) {
                final Object value = it.next();
                if (hashCode != null && hashCode.hasCustomHashCode(value, false)) {
                    ret = hashCode.calcValueHashCode(ret, value);
                } else {
                    ret += it.nextIndex() * hashCodeDeep(value, hashCode);
                }
            }
            return ret;
        } else if (obj instanceof Map) {
            int ret = 0;
            for (final Entry<?, ?> es : ((Map<?, ?>) obj).entrySet()) {
                final Object key = es.getKey();
                if (hashCode != null && hashCode.hasCustomHashCode(key, true)) {
                    ret = hashCode.calcKeyHashCode(ret, key);
                } else {
                    ret += hashCodeDeep(key, hashCode);
                }
                final Object value = es.getValue();
                if (hashCode != null && hashCode.hasCustomHashCode(value, false)) {
                    ret = hashCode.calcValueHashCode(ret, value);
                } else {
                    ret += hashCodeDeep(value, hashCode);
                }
            }
            return ret;
        } else {
            if (hashCode != null && hashCode.hasCustomHashCode(obj, false)) {
                return hashCode.calcValueHashCode(0, obj);
            } else if (Clazz.isPrimitive(obj.getClass()) || Clazz.isEnum(obj.getClass()) || Clazz.isString(obj.getClass())) {
                return obj.hashCode();
            }
            try {
                final ClassCache cc = ClassCache.getClassCache(obj.getClass());
                int ret = 0;
                for (final Getter c : cc.getGetter()) {
                    final Object key = c.key;
                    if (hashCode != null && hashCode.hasCustomHashCode(key, true)) {
                        ret = hashCode.calcKeyHashCode(ret, key);
                    } else {
                        ret += key.hashCode();
                    }
                    final Object value = c.getValue(obj);
                    if (hashCode != null && hashCode.hasCustomHashCode(value, false)) {
                        ret = hashCode.calcValueHashCode(ret, value);
                    } else {
                        ret += hashCodeDeep(value, hashCode);
                    }
                }
                return ret;
            } catch (final SecurityException e) {
                throw new WTFException(e);
            } catch (final NoSuchMethodException e) {
                throw new WTFException(e);
            } catch (final IllegalArgumentException e) {
                throw new WTFException(e);
            } catch (final IllegalAccessException e) {
                throw new WTFException(e);
            } catch (final InvocationTargetException e) {
                throw new WTFException(e);
            }
        }
    }

    /**
     * @param unwrapType
     * @param unwrapType2
     * @return
     */
    public static Integer tryToCompare(final Object a, final Object b) {
        if (a instanceof Number && b instanceof Number) {
            return compareNumber((Number) a, (Number) b);
        } else if (a instanceof Comparable && b instanceof Comparable) {
            try {
                return compareComparable((Comparable) a, (Comparable) b);
            } catch (final RuntimeException e) {
                LogV3.exception(CompareUtils.class, e);
            }
        }
        return null;
    }
}
