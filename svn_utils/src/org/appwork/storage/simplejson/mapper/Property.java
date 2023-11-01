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
package org.appwork.storage.simplejson.mapper;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Type;

import org.appwork.utils.reflection.CompiledType;

/**
 * @author thomas
 * @date 20.10.2022
 *
 */
public class Property {
    public final String       key;
    public final CompiledType type;
    public final Getter       getter;
    public final Setter       setter;

    /**
     * @param key
     * @param type
     * @param getter
     * @param setter
     */
    public Property(String key, CompiledType type, Getter getter, Setter setter) {
        this.key = key;
        this.type = type;
        this.getter = getter;
        this.setter = setter;
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        return type.hashCode() + key.hashCode();
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Property) {
            Property other = (Property) obj;
            if (type.equals(other.type)) {
                if (key.equals(other.key)) {
                    if (getGetterMethod() == other.getGetterMethod()) {
                        if (getSetterMethod() == other.getSetterMethod()) {
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    }

    public Method getSetterMethod() {
        return setter == null ? null : setter.getMethod();
    }

    public Method getGetterMethod() {
        return getter == null ? null : getter.getMethod();
    }

    /**
     * @return
     *
     */
    public ClassCache getClassCache() {
        if (getter != null) {
            return getter.classCache;
        } else {
            return null;
        }
    }

    /**
     * @return
     */
    public Type getGenericType() {
        Field field = null;
        if (getter != null) {
            final Method method = getter.getMethod();
            if (method != null) {
                return method.getGenericReturnType();
            } else {
                field = getter.getField();
            }
        }
        if (setter != null) {
            final Method method = setter.getMethod();
            if (method != null) {
                return method.getGenericParameterTypes()[0];
            } else if (field == null) {
                field = setter.getField();
            }
        }
        if (field != null) {
            return field.getGenericType();
        } else {
            return null;
        }
    }
}
