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
package org.appwork.storage;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author thomas
 *
 */
public abstract class TypeRef<T> {
    public static final TypeRef<String>                             STRING         = new SimpleTypeRef<String>(String.class);
    public static final TypeRef<byte[]>                             BYTE_ARRAY     = new SimpleTypeRef<byte[]>(byte[].class);
    @Deprecated
    public static final TypeRef<HashMap<String, Object>>            HASHMAP        = new TypeRef<HashMap<String, Object>>() {
                                                                                   };
    @Deprecated
    public static final TypeRef<ArrayList<HashMap<String, Object>>> LIST_HASHMAP   = new TypeRef<ArrayList<HashMap<String, Object>>>() {
                                                                                   };
    /**
     * TypeRef to Map interface, do not enforce a specific Map implementation
     */
    public static final TypeRef<Map<String, Object>>                MAP            = new TypeRef<Map<String, Object>>() {
                                                                                   };
    /**
     * TypeRef to List interface, do not enforce a specific List implementation
     */
    public static final TypeRef<List<Object>>                       LIST           = new TypeRef<List<Object>>() {
                                                                                   };
    @Deprecated
    public static final TypeRef<HashMap<String, String>>            HASHMAP_STRING = new TypeRef<HashMap<String, String>>() {
                                                                                   };
    public static final TypeRef<Boolean>                            BOOLEAN        = new SimpleTypeRef<Boolean>(Boolean.class) {
                                                                                   };
    public static final TypeRef<String[]>                           STRING_ARRAY   = new SimpleTypeRef<String[]>(String[].class);
    public static final TypeRef<Object>                             OBJECT         = new SimpleTypeRef<Object>(Object.class);
    public static final TypeRef<int[]>                              INT_ARRAY      = new SimpleTypeRef<int[]>(int[].class);
    public static final TypeRef<Set<String>>                        STRING_SET     = new TypeRef<Set<String>>() {
                                                                                   };
    public static final TypeRef<Object[]>                           OBJECT_ARRAY   = new SimpleTypeRef<Object[]>(Object[].class);
    public static final TypeRef<boolean[]>                          BOOLEAN_ARRAY  = new SimpleTypeRef<boolean[]>(boolean[].class);
    public static final TypeRef<Long>                               LONG           = new SimpleTypeRef<Long>(Long.class);
    @Deprecated
    public static final TypeRef<ArrayList<String>>                  STRING_LIST    = new TypeRef<ArrayList<String>>() {
                                                                                   };
    public static final TypeRef<Integer>                            INT            = new SimpleTypeRef<Integer>(Integer.class);
    private final Type                                              type;
    private final Class<?>                                          rawClass;

    public TypeRef() {
        final Type superClass = this.getClass().getGenericSuperclass();
        if (superClass instanceof Class) {
            throw new IllegalArgumentException("Wrong TypeRef Construct");
        } else {
            this.type = ((ParameterizedType) superClass).getActualTypeArguments()[0];
        }
        rawClass = initRawClass(getType());
    }

    /**
     * @param type
     * @return
     */
    protected Class<?> initRawClass(Type type) {
        if (type instanceof Class) {
            return ((Class<?>) type);
        } else if (type instanceof ParameterizedType) {
            final Type raw = ((ParameterizedType) type).getRawType();
            if (raw instanceof Class) {
                return ((Class<?>) raw);
            } else {
                return null;
            }
        } else {
            return null;
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof TypeRef) {
            return getType().equals(((TypeRef) obj).getType());
        }
        return false;
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        return getType().hashCode();
    }

    public TypeRef(final Type t) {
        if (t == null) {
            throw new IllegalArgumentException("type is null");
        }
        this.type = t;
        rawClass = initRawClass(getType());
    }

    public Type getType() {
        return this.type;
    }

    /**
     * @return
     */
    public Class<?> getRawClass() {
        return rawClass;
    }
}
