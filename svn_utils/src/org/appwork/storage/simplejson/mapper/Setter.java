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
package org.appwork.storage.simplejson.mapper;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.appwork.utils.DebugMode;

/**
 * @author thomas
 *
 */
public class Setter implements GetterOrSetter {
    public final String       key;
    public final Method       method;
    public final Type         type;
    final public List<String> alternativeKeys;
    public final Field        field;
    public final ClassCache   cc;

    public Field getField() {
        return field;
    }

    public List<String> getAlternativeKeys() {
        return alternativeKeys;
    }

    public Setter(ClassCache cc, final String name, final Method m) {
        this(cc, name, null, m, null);
    }

    private static final List<String> NULL_LIST = Collections.unmodifiableList(new ArrayList<String>());

    /**
     * @param cc
     *            TODO
     * @param m
     * @param field
     *            TODO
     * @param substring
     */
    public Setter(ClassCache cc, final String name, List<String> alternativeKeys, final Method m, Field field) {
        this.cc = cc;
        this.key = name;
        this.field = field;
        this.method = m;
        m.setAccessible(true);
        this.alternativeKeys = alternativeKeys == null ? NULL_LIST : Collections.unmodifiableList(alternativeKeys);
        this.type = m.getGenericParameterTypes()[0];
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return method.toString();
    }

    public String getKey() {
        return this.key;
    }

    public Method getMethod() {
        return this.method;
    }

    public Type getType() {
        return this.type;
    }

    @SuppressWarnings("unchecked")
    public void setValue(final Object inst, Object parameter) throws IllegalArgumentException, IllegalAccessException, InvocationTargetException {
        if (this.type instanceof Class && ((Class<?>) this.type).isEnum() && parameter != null && !((Class) type).isAssignableFrom(parameter.getClass())) {
            parameter = Enum.valueOf((Class<Enum>) this.type, parameter + "");
            DebugMode.logInIDEOnly(new Exception("This should not happen! This is job of the mapper"));
        }
        // System.out.println(this.key + " = " + parameter + " " + this.type);
        try {
            this.method.invoke(inst, parameter);
        } catch (final IllegalArgumentException e) {
            // LogV3.severe(this.method + " " + parameter + " ->" + (parameter == null ? "null" : parameter.getClass()));
            throw e;
        }
    }
}
