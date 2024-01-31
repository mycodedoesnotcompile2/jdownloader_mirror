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
import org.appwork.storage.simplejson.mapper.ClassCache;
import org.appwork.storage.simplejson.mapper.Getter;
import org.appwork.utils.reflection.Clazz;

public class CompareUtils {
    /**
     * @param x
     * @param y
     * @return
     */
    public static int compare(boolean x, boolean y) {
        return (x == y) ? 0 : (x ? 1 : -1);
    }

    /**
     * @param height
     * @param height2
     * @return
     */
    public static int compare(int x, int y) {
        return (x < y) ? -1 : ((x == y) ? 0 : 1);
    }

    public static int compare(long x, long y) {
        return (x < y) ? -1 : ((x == y) ? 0 : 1);
    }

    /**
     *
     * @param x
     * @param y
     * @return <0 if x<y >0 if x>y 0 if x==y
     */
    public static int compare(double x, double y) {
        // since 1.4
        return Double.compare(x, y);
    }

    public static int compare(float x, float y) {
        // since 1.4
        return Float.compare(x, y);
    }

    /**
     * @param projection
     * @param projection2
     * @return
     */
    public static int compare(Comparable x, Comparable y) {
        if (x == y) {
            return 0;
        } else if (x == null) {
            return -1;
        } else if (y == null) {
            return 1;
        } else {
            return x.compareTo(y);
        }
    }

    /**
     * @param hash
     * @param hash2
     * @deprecated use Arrays.equals instead
     * @return
     */
    public static boolean equals(byte[] hash, byte[] hash2) {
        return Arrays.equals(hash, hash2);
    }

    /**
     * returns true of both objects are null or their.equals method match
     *
     * @param a
     * @param b
     * @return
     */
    public static boolean equals(Object a, Object b) {
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
    public static boolean equals(Map<?, ?> a, Map<?, ?> b) {
        if (a == b) {
            return true;
        } else if (a == null || b == null) {
            return false;
        } else if (a.size() != b.size()) {
            return false;
        } else {
            for (Entry<?, ?> es : a.entrySet()) {
                if (!equals(es.getValue(), b.get(es.getKey()))) {
                    return false;
                }
            }
            return false;
        }
    }

    public static boolean equals(Set<?> a, Set<?> b) {
        if (a == b) {
            return true;
        } else if (a == null || b == null) {
            return false;
        } else {
            // this must work for sets according to the interface docs
            return a.equals(b);
        }
    }

    public static boolean equalsDeep(final Object objectX, final Object objectY, Equalator equalator) {
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
        public Couple(Object objectX, Object objectY) {
            a = objectX;
            b = objectY;
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
            int ret = hashCode;
            if (ret == -1) {
                ret = 0;
                if (a != null) {
                    ret += a.hashCode();
                }
                if (b != null) {
                    ret += b.hashCode();
                }
                hashCode = ret;
            }
            return ret;
        }

        /*
         * (non-Javadoc)
         *
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (obj == this) {
                return true;
            } else if (!(obj instanceof Couple)) {
                return false;
            } else {
                return CompareUtils.equals(a, ((Couple) obj).a) && CompareUtils.equals(b, ((Couple) obj).b);
            }
        }
    }

    public static boolean equalsDeep(final Object objectX, final Object objectY, Equalator customEuualator, HashSet<Couple> dupeCheck) {
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
            main: for (Object x : ((Set) objectX)) {
                for (Object y : ((Set) objectY)) {
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
            for (Entry<?, ?> es : ((Map<?, ?>) objectX).entrySet()) {
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
            } catch (NoSuchMethodException e1) {
                throw new WTFException(e1);
            } catch (SecurityException e1) {
                throw new WTFException(e1);
            }
            try {
                final ClassCache cc = ClassCache.getClassCache(objectX.getClass());
                for (Getter c : cc.getGetter()) {
                    if (!equalsDeep(c.getValue(objectX), c.getValue(objectY), customEuualator, dupeCheck)) {
                        return false;
                    }
                }
                return true;
            } catch (SecurityException e) {
                throw new WTFException(e);
            } catch (NoSuchMethodException e) {
                return false;
            } catch (IllegalArgumentException e) {
                throw new WTFException(e);
            } catch (IllegalAccessException e) {
                throw new WTFException(e);
            } catch (InvocationTargetException e) {
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
    public static int compareNumber(Number a, Number b) {
        final Class<?> ac = a.getClass();
        final Class<?> bc = b.getClass();
        if (ac == Double.class || bc == Double.class) {
            return compare(a.doubleValue(), b.doubleValue());
        } else if (ac == Float.class || bc == Float.class) {
            return compare(a.floatValue(), b.floatValue());
        } else {
            return compare(a.longValue(), b.longValue());
        }
    }

    /**
     * @param value
     * @param query
     * @return
     */
    public static boolean equalsNumber(Number a, Number b) {
        if (a == null && b == null) {
            return true;
        } else if (a == null || b == null) {
            return false;
        } else {
            return compareNumber(a, b) == 0;
        }
    }

    public static int hashCodeDeep(Object obj) {
        return hashCodeDeep(obj, null);
    }

    /**
     * @param addressDetails
     * @return
     */
    public static int hashCodeDeep(Object obj, CompareUtilsHashCodeCalculator hashCode) {
        if (obj == null) {
            return 0;
        }
        try {
            obj.getClass().getDeclaredMethod("hashCode", new Class[0]);
            // has a dedicated hashcode method;
            if (!(obj instanceof ClassUsesDeepHashCode)) {
                return obj.hashCode();
            }
        } catch (NoSuchMethodException e) {
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
            for (Entry<?, ?> es : ((Map<?, ?>) obj).entrySet()) {
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
                for (Getter c : cc.getGetter()) {
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

    /**
     * @param unwrapType
     * @param unwrapType2
     * @return
     */
    public static Integer compare(Object a, Object b) {
        if (a instanceof Number && b instanceof Number) {
            return compareNumber((Number) a, (Number) b);
        } else if (a instanceof Comparable && b instanceof Comparable) {
            return compare((Comparable) a, (Comparable) b);
        }
        return null;
    }
}
