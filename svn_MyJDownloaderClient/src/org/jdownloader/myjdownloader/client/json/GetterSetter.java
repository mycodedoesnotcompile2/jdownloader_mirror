/**
 * 
 * ====================================================================================================================================================
 *         "My JDownloader Client" License
 *         The "My JDownloader Client" will be called [The Product] from now on.
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
package org.jdownloader.myjdownloader.client.json;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Type;

/**
 * @author Thomas
 * 
 */
public class GetterSetter {
    private Method setter;
    private String key;

    public String getKey() {
        return key;
    }

    public void setKey(final String key) {
        this.key = key;
    }

    /**
     * @param key
     */
    public GetterSetter(final String key) {
        this.key = key;
    }

    public Method getSetter() {
        return setter;
    }

    public void setSetter(final Method setter) {
        this.setter = setter;
    }

    public Method getGetter() {
        return getter;
    }

    public void setGetter(final Method getter) {
        this.getter = getter;
    }

    private Method getter;
    private Field  field;

    public Field getField() {
        return field;
    }

    /**
     * @param field
     */
    public void setField(final Field field) {
        this.field = field;

    }

    public boolean hasField() {
        return field != null;
    }

    public boolean hasGetter() {
        return getter != null;
    }

    public boolean hasSetter() {
        return setter != null;
    }

    /**
     * @param class1
     * @return
     */
    public boolean hasAnnotation(final Class<? extends Annotation> class1) {

        return getAnnotation(class1) != null;
    }

    /**
     * @param class1
     * @return
     */
    public <T extends Annotation> T getAnnotation(final Class<T> class1) {
        if (getter != null) {
            final T ann = getter.getAnnotation(class1);
            if (ann != null) { return ann; }
        }

        if (setter != null) {
            final T ann = setter.getAnnotation(class1);
            if (ann != null) { return ann; }
        }

        if (field != null) {
            final T ann = field.getAnnotation(class1);
            if (ann != null) { return ann; }
        }
        return null;
    }

    /**
     * @return
     */
    public Type getType() {
        if (getter != null) { return getter.getGenericReturnType(); }

        if (setter != null) { return setter.getGenericParameterTypes()[0]; }

        return null;
    }

    /**
     * @param b
     */
    public void setAccessible(final boolean b) {
        if (getter != null) {
            getter.setAccessible(b);
        }

        if (setter != null) {
            setter.setAccessible(b);
        }

        if (field != null) {
            field.setAccessible(b);
        }
    }

    /**
     * @param actionClass
     * @return
     * @throws IllegalArgumentException
     * @throws IllegalAccessException
     */
    public Object get(final Object actionClass) throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        if (getter != null) {
            getter.setAccessible(true);
            return getter.invoke(actionClass, new Object[] {});
        }
        if (field != null) {

            field.setAccessible(true);
            return field.get(actionClass);
        }
        throw new NullPointerException("Field and getter not available");
    }

    /**
     * @param action
     * @param v
     * @throws InvocationTargetException
     * @throws IllegalArgumentException
     * @throws IllegalAccessException
     */
    public void set(final Object action, final Object v) throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        if (setter != null) {
            setter.setAccessible(true);
            setter.invoke(action, v);
            return;
        }
        if (field != null) {
            field.setAccessible(true);
            field.set(action, v);
        }

        throw new NullPointerException("Field and setter not available");

    }
}
