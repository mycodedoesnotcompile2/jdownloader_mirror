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

/**
 * Copyright (c) 2009 - 2013 AppWork UG(haftungsbeschränkt) <e-mail@appwork.org>
 * 
 * This file is part of org.appwork.storage
 * 
 * This software is licensed under the Artistic License 2.0,
 * see the LICENSE file or http://www.opensource.org/licenses/artistic-license-2.0.php
 * for details
 */
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.util.Collection;
import java.util.HashMap;
import java.util.Locale;

/**
 * @author Thomas
 * 
 */
public abstract class AbstractJsonData implements JsonFactoryInterface {
    private static final HashMap<Class<?>, Collection<GetterSetter>> GETTER_SETTER_CACHE = new HashMap<Class<?>, Collection<GetterSetter>>();

    public static boolean isBoolean(final Type type) {
        return type == Boolean.class || type == boolean.class;
    }

    public static boolean isNotEmpty(final String ip) {
        return !(ip == null || ip.trim().length() == 0);
    }

    public static String createKey(final String key) {
        final StringBuilder sb = new StringBuilder();
        final char[] ca = key.toCharArray();
        boolean starter = true;
        for (final char element : ca) {
            if (starter && Character.isUpperCase(element)) {
                sb.append(Character.toLowerCase(element));
            } else {
                starter = false;
                sb.append(element);
            }
        }
        return sb.toString();
    }

    /**
     * @return
     */
    public static Collection<GetterSetter> getGettersSetteres(Class<?> clazz) {
        Collection<GetterSetter> ret = AbstractJsonData.GETTER_SETTER_CACHE.get(clazz);
        if (ret != null) {
            return ret;
        }
        final Class<?> org = clazz;
        synchronized (AbstractJsonData.GETTER_SETTER_CACHE) {
            ret = AbstractJsonData.GETTER_SETTER_CACHE.get(clazz);
            if (ret != null) {
                return ret;
            }
            final HashMap<String, GetterSetter> map = new HashMap<String, GetterSetter>();
            while (clazz != null) {
                for (final Method m : clazz.getDeclaredMethods()) {
                    String key = null;
                    boolean getter = false;
                    if (m.getName().startsWith("is") && AbstractJsonData.isBoolean(m.getReturnType()) && m.getParameterTypes().length == 0) {
                        key = m.getName().substring(2);
                        getter = true;
                    } else if (m.getName().startsWith("get") && m.getParameterTypes().length == 0) {
                        key = m.getName().substring(3);
                        getter = true;
                    } else if (m.getName().startsWith("set") && m.getParameterTypes().length == 1) {
                        key = m.getName().substring(3);
                        getter = false;
                    }
                    if (AbstractJsonData.isNotEmpty(key)) {
                        final String unmodifiedKey = key;
                        key = AbstractJsonData.createKey(key);
                        GetterSetter v = map.get(key);
                        if (v == null) {
                            v = new GetterSetter(key);
                            map.put(key, v);
                        }
                        if (getter) {
                            v.setGetter(m);
                        } else {
                            v.setSetter(m);
                        }
                        Field field;
                        try {
                            field = clazz.getField(unmodifiedKey.substring(0, 1).toLowerCase(Locale.ENGLISH) + unmodifiedKey.substring(1));
                            v.setField(field);
                        } catch (final NoSuchFieldException e) {
                        }
                    }
                }
                clazz = clazz.getSuperclass();
            }
            AbstractJsonData.GETTER_SETTER_CACHE.put(org, map.values());
            return AbstractJsonData.GETTER_SETTER_CACHE.get(org);
        }
    }

    public boolean equals(final Object pass, final Object pass2) {
        if (pass == pass2) {
            return true;
        }
        if (pass == null && pass2 != null) {
            return false;
        }
        return pass.equals(pass2);
    }

    @Override
    public String toJsonString() {
        final HashMap<String, Object> map = new HashMap<String, Object>();
        Object obj = null;
        try {
            final Constructor<? extends AbstractJsonData> c = this.getClass().getDeclaredConstructor(new Class[] {});
            c.setAccessible(true);
            final AbstractJsonData empty = c.newInstance();
            for (final GetterSetter gs : AbstractJsonData.getGettersSetteres(this.getClass())) {
                obj = gs.get(this);
                if (this.equals(obj, gs.get(empty))) {
                    continue;
                }
                map.put(Character.toLowerCase(gs.getKey().charAt(0)) + gs.getKey().substring(1), obj);
            }
        } catch (final Exception e) {
            throw new RuntimeException(e);
        }
        return MyJDJsonMapper.HANDLER.objectToJSon(map);
    }
}
