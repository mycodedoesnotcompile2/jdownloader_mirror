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
package org.appwork.storage.flexijson.mapper.interfacestorage;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import org.appwork.storage.simplejson.mapper.Getter;
import org.appwork.storage.simplejson.mapper.Setter;
import org.appwork.utils.event.basic.CoreDelegate;
import org.appwork.utils.event.basic.CoreEventSender;

/**
 * @author thomas
 * @date 27.07.2022
 *
 */
public class PropertyHandlerImpl<InterfaceType, ReturnType> implements PropertyHandler<InterfaceType, ReturnType> {
    /**
     *
     */
    private static final Object[]                                               EMPTY = new Object[] {};
    public final InterfaceStorage<InterfaceType>                                handler;
    public final String                                                         key;
    public final Class<ReturnType>                                              clazz;
    private Getter                                                              getterMethod;
    private Setter                                                              setterMethod;
    private CoreEventSender<PropertyHandlerListener<InterfaceType, ReturnType>> eventSender;

    public CoreEventSender<PropertyHandlerListener<InterfaceType, ReturnType>> getEventSender() {
        synchronized (this) {
            if (eventSender == null) {
                eventSender = new CoreEventSender<PropertyHandlerListener<InterfaceType, ReturnType>>();
            }
            return eventSender;
        }
    }

    /**
     * @param interfaceInvocationHandler
     * @param key
     * @param clazz
     */
    public PropertyHandlerImpl(InterfaceStorage<InterfaceType> handler, String key, Class<ReturnType> clazz) {
        this.handler = handler;
        this.key = key;
        this.clazz = clazz;
        getterMethod = handler.cType.getClassCache().getGetter(key);
        setterMethod = handler.cType.getClassCache().getSetter(key);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.mapper.interfacebridge.KeyHandler#get()
     */
    @SuppressWarnings("unchecked")
    @Override
    public ReturnType get() {
        try {
            if (getterMethod == null) {
                throw new IllegalStateException("No " + key + "-Getter Method available in " + handler.cType);
            }
            return (ReturnType) handler.invoke(null, getterMethod.getMethod(), EMPTY);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new KeyHandlerException(e);
        } catch (Throwable e) {
            throw new KeyHandlerException(e);
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.mapper.interfacebridge.KeyHandler#fireEventSet(java.lang.Object, java.lang.Object)
     */
    @Override
    public void fireEventSet(final ReturnType oldValue, final ReturnType newValue) {
        CoreEventSender<PropertyHandlerListener<InterfaceType, ReturnType>> es = eventSender;
        if (es != null) {
            es.fireEvent(new CoreDelegate<PropertyHandlerListener<InterfaceType, ReturnType>>() {
                @Override
                protected void fireTo(PropertyHandlerListener<InterfaceType, ReturnType> listener) {
                    listener.onInterfaceValueSet(handler.getStorage(), PropertyHandlerImpl.this, oldValue, newValue);
                }
            });
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.mapper.interfacestorage.PropertyAccessHandler#set(java.lang.Object)
     */
    @Override
    public void set(ReturnType value) {
        try {
            if (setterMethod == null) {
                throw new IllegalStateException("No " + key + "-Setter Method available in " + handler.cType);
            }
            handler.invoke(null, setterMethod.getMethod(), new Object[] { value });
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new KeyHandlerException(e);
        } catch (Throwable e) {
            throw new KeyHandlerException(e);
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.mapper.interfacestorage.PropertyHandler#getStorage()
     */
    @Override
    public InterfaceType getStorage() {
        return handler.getStorage();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.mapper.interfacestorage.PropertyHandler#getAnnotations(java.lang.Class)
     */
    @Override
    public <AnnoType extends Annotation> List<AnnoType> getAnnotations(Class<AnnoType> type) {
        ArrayList<AnnoType> ret = new ArrayList<AnnoType>();
        Field field = null;
        if (getterMethod != null) {
            // getannotationsbytype seems to be 1.8
            Annotation[] annos = getterMethod.getMethod().getAnnotations();
            collect(type, ret, annos);
            field = getterMethod.field;
        }
        if (setterMethod != null) { // getannotationsbytype seems to be 1.8
            Annotation[] annos = setterMethod.getMethod().getAnnotations();
            collect(type, ret, annos);
            if (field == null) {
                field = setterMethod.field;
            }
        }
        if (field != null) {
            // getannotationsbytype seems to be 1.8
            Annotation[] annos = field.getAnnotations();
            collect(type, ret, annos);
        }
        return ret;
    }

    public <AnnoType extends Annotation> void collect(Class<AnnoType> type, ArrayList<AnnoType> ret, Annotation[] annos) {
        if (annos != null) {
            for (Annotation a : annos) {
                if (type.isAssignableFrom(a.getClass())) {
                    ret.add((AnnoType) a);
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.mapper.interfacestorage.PropertyHandler#getKey()
     */
    @Override
    public String getKey() {
        return key;
    }
}
