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
package org.appwork.storage.mapper;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.Type;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map.Entry;

import org.appwork.storage.AdapterSelfJSONDeSerializer;
import org.appwork.storage.AdapterSelfJSONSerializer;
import org.appwork.storage.DeSerializerInput;
import org.appwork.storage.JSONMapper;
import org.appwork.storage.JSonMapperException;
import org.appwork.storage.JsonDeSerializer;
import org.appwork.storage.JsonSerializer;
import org.appwork.storage.SelfJSONDeSerializer;
import org.appwork.storage.SelfJSONSerializer;
import org.appwork.storage.TypeRef;
import org.appwork.storage.converter.AdapterSelfJSONConverterForDeserialization;
import org.appwork.storage.converter.AdapterSelfJSONConverterForSerialization;
import org.appwork.storage.converter.ConverterInput;
import org.appwork.storage.converter.JsonConverterForDeserialization;
import org.appwork.storage.converter.JsonConverterForSerialization;
import org.appwork.storage.converter.SelfJsonConverterForDeserialization;
import org.appwork.storage.converter.SelfJsonConverterForSerialization;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.IO;
import org.appwork.utils.ReflectionUtils;

/**
 * @author thomas
 * @date 21.01.2022
 *
 */
public abstract class AbstractJsonMapper implements JSONMapper {
    protected static final Charset UTF8 = Charset.forName("UTF-8");

    public AbstractJsonMapper() {
        initMapper();
    }

    protected void initMapper() {
        putSerializer(SelfJSONSerializer.class, new AdapterSelfJSONSerializer());
        putDeSerializer(SelfJSONDeSerializer.class, new AdapterSelfJSONDeSerializer());
        putConverterForSerialization(SelfJsonConverterForSerialization.class, new AdapterSelfJSONConverterForSerialization());
        putConverterFromDeserialization(SelfJsonConverterForDeserialization.class, new AdapterSelfJSONConverterForDeserialization());
    }

    protected Class<?> getRawTypeForDeserialization(Type type) {
        Class<?> raw = ReflectionUtils.getRaw(type);
        if (raw == null) {
            raw = Object.class;
        }
        return raw;
    }

    protected volatile HashMap<Class<?>, JsonSerializer>                  serializer                = new HashMap<Class<?>, JsonSerializer>();
    protected volatile HashMap<Class<?>, JsonDeSerializer>                deSerializer              = new HashMap<Class<?>, JsonDeSerializer>();
    protected volatile HashMap<Class<?>, JsonConverterForSerialization>   convertForSerialization   = new HashMap<Class<?>, JsonConverterForSerialization>();
    protected volatile HashMap<Class<?>, JsonConverterForDeserialization> convertForDeserialization = new HashMap<Class<?>, JsonConverterForDeserialization>();

    public synchronized JsonConverterForSerialization putConverterForSerialization(final Class<?> clazz, final JsonConverterForSerialization converter) {
        if (clazz == null) {
            throw new IllegalArgumentException("clazz is null");
        } else {
            final HashMap<Class<?>, JsonConverterForSerialization> newMap = new HashMap<Class<?>, JsonConverterForSerialization>();
            newMap.putAll(convertForSerialization);
            final JsonConverterForSerialization old;
            if (converter == null) {
                old = newMap.remove(clazz);
            } else {
                old = newMap.put(clazz, converter);
            }
            convertForSerialization = newMap;
            resetBestMatchCache();
            return old;
        }
    }

    /**
     * Calculate an integer. the bigger the int is, the larger is the distance between assignable and target
     *
     * @param raw
     * @param lookup
     * @return
     */
    public int getHirarchyDepths(Class<?> assignable, Class<?> target) {

        int ret = 0;
        Class<?> now = target;
        while (now != null) {
            if (now == assignable) {
                break;
            }
            Class<?>[] interfaces = now.getInterfaces();
            if (interfaces != null) {
                int bestIntRet = -1;
                for (Class<?> i : now.getInterfaces()) {
                    if (assignable.isAssignableFrom(i)) {
                        int intRet = 0;
                        while (i != null) {
                            intRet++;
                            if (i == assignable) {
                                break;
                            }
                            i = i.getSuperclass();

                        }
                        if (bestIntRet < 0 || bestIntRet > intRet) {
                            bestIntRet = intRet;
                        }

                    }
                }
                if (bestIntRet >= 0) {
                    ret += bestIntRet;
                    return ret;
                }
            }

            now = now.getSuperclass();
            ret += 100;
            ret = 100 * (ret / 100);
        }
        return ret;
    }

    protected String handleSerializationMatches(Object input, List<JsonSerializer> matches) {
        for (JsonSerializer best : matches) {

            final String ret = best.toJSonString(input, this);

            if (ret != JsonSerializer.SKIP && ret != input) {
                return ret;
            }

        }
        return JsonSerializer.SKIP;
    }

    protected Object handleSerializationConverterMatches(Object input, List<JsonConverterForSerialization> matches) {
        for (JsonConverterForSerialization best : matches) {

            final Object ret = best.convertForSerialization(input, this);

            if (ret != JsonConverterForSerialization.SKIP && (ret != input)) {
                return ret;
            }

        }
        return input;
    }

    protected Object handleDeserializationMatches(DeSerializerInput input, TypeRef<Object> typeRef, List<JsonDeSerializer> matches) {
        for (JsonDeSerializer best : matches) {

            final Object ret = best.toObject(input, typeRef, this);

            if (ret != JsonDeSerializer.SKIP && ret != input) {
                return ret;
            }

        }
        return input;
    }

    protected Object handleDeserializationConverterMatches(ConverterInput input, TypeRef<Object> typeRef, List<JsonConverterForDeserialization> matches) {
        for (JsonConverterForDeserialization best : matches) {

            final Object ret = best.convertForDeserialization(input, typeRef, this);

            if (ret != JsonConverterForDeserialization.SKIP && !(ret instanceof ConverterInput)) {
                return ret;
            }

        }
        return input;
    }

    public synchronized JsonConverterForDeserialization putConverterFromDeserialization(final Class<?> clazz, final JsonConverterForDeserialization converter) {
        if (clazz == null) {
            throw new IllegalArgumentException("clazz is null");
        } else {
            final HashMap<Class<?>, JsonConverterForDeserialization> newMap = new HashMap<Class<?>, JsonConverterForDeserialization>();
            newMap.putAll(convertForDeserialization);
            final JsonConverterForDeserialization old;
            if (converter == null) {
                old = newMap.remove(clazz);
            } else {
                old = newMap.put(clazz, converter);
            }
            convertForDeserialization = newMap;
            resetBestMatchCache();
            return old;
        }
    }

    public synchronized JsonDeSerializer putDeSerializer(final Class<?> clazz, final JsonDeSerializer jsonDeSerializer) {
        if (clazz == null) {
            throw new IllegalArgumentException("clazz is null");
        } else {
            final HashMap<Class<?>, JsonDeSerializer> newMap = new HashMap<Class<?>, JsonDeSerializer>();
            newMap.putAll(deSerializer);
            final JsonDeSerializer old;
            if (jsonDeSerializer == null) {
                old = newMap.remove(clazz);
            } else {
                old = newMap.put(clazz, jsonDeSerializer);
            }
            deSerializer = newMap;
            resetBestMatchCache();
            return old;
        }
    }

    public synchronized JsonSerializer putSerializer(final Class<?> clazz, final JsonSerializer jsonSerializer) {
        final HashMap<Class<?>, JsonSerializer> newMap = new HashMap<Class<?>, JsonSerializer>();
        newMap.putAll(serializer);
        final JsonSerializer old;
        if (jsonSerializer == null) {
            old = newMap.remove(clazz);
        } else {
            old = newMap.put(clazz, jsonSerializer);
        }
        serializer = newMap;
        resetBestMatchCache();
        return old;
    }

    protected void resetBestMatchCache() {
        findBestTypeMatchCache = new HashMap<Class<?>, HashMap<Class<?>, List<?>>>();
    }

    public class KeyValue<T> {
        private Class<?> target;

        /**
         * @param key
         * @param decode
         * @param object
         */
        public KeyValue(Class<?> target, Class<?> key, T value) {
            this.key = key;
            this.value = value;
            this.target = target;
        }

        public Class<?> key;
        public T        value;
        private int     depths = -1;

        /**
         * @return
         */
        public int getTypeHirarchyDepths() {
            if (depths < 0) {
                depths = getHirarchyDepths(key, target);
            }
            return depths;
        }
    }

    private HashMap<Class<?>, HashMap<Class<?>, List<?>>> findBestTypeMatchCache = new HashMap<Class<?>, HashMap<Class<?>, List<?>>>();

    protected <T> List<T> findBestTypeMatch(final Class<?> raw, HashMap<Class<?>, T> lookup, Class<T> type) {
        if (lookup.size() == 0) {
            return null;
        }

        synchronized (findBestTypeMatchCache) {
            HashMap<Class<?>, List<?>> map = findBestTypeMatchCache.get(type);
            if (map != null) {
                List<?> ret = map.get(raw);
                if (ret != null) {
                    return (List<T>) ret;
                }
            }
        }

        T best = lookup.get(Object.class);

        ArrayList<KeyValue<T>> tmp = new ArrayList<KeyValue<T>>();
        if (best != null) {
            tmp.add(new KeyValue<T>(raw, Object.class, best));
        }

        // raw may be null if the input for serialization is null
        if (lookup.size() > 0) {

            for (final Entry<Class<?>, T> se : lookup.entrySet()) {
                if (se.getKey() == raw) {
                    tmp.add(new KeyValue<T>(raw, se.getKey(), se.getValue()));
                } else if (se.getKey() != null && raw != null && se.getKey().isAssignableFrom(raw)) {
                    tmp.add(new KeyValue<T>(raw, se.getKey(), se.getValue()));

                }
            }
        }
        Collections.sort(tmp, new Comparator<KeyValue<T>>() {

            @Override
            public int compare(KeyValue<T> o1, KeyValue<T> o2) {
                // best match first
                return CompareUtils.compareInt(o1.getTypeHirarchyDepths(), o2.getTypeHirarchyDepths());
            }
        });
        LinkedList<T> ret = new LinkedList<T>();
        for (KeyValue<T> e : tmp) {
            ret.add(e.value);
        }

        synchronized (findBestTypeMatchCache) {
            HashMap<Class<?>, List<?>> map = findBestTypeMatchCache.get(type);
            if (map == null) {
                findBestTypeMatchCache.put(type, map = new HashMap<Class<?>, List<?>>());
            }
            map.put(raw, ret);
        }

        return ret;
    }

    @Override
    public byte[] objectToByteArray(final Object value) throws JSonMapperException {
        final String ret = objectToString(value);
        if (ret == null) {
            return "null".getBytes(UTF8);
        } else {
            return ret.getBytes(UTF8);
        }
    }

    /**
     * closes outputStream
     */
    @Override
    public void writeObject(final OutputStream outputStream, final Object value) throws JSonMapperException {
        try {
            try {
                outputStream.write(objectToByteArray(value));
            } finally {
                if (outputStream != null) {
                    outputStream.close();
                }
            }
        } catch (IOException e) {
            throw new JSonMapperException(e);
        }
    }

    /**
     * closes inputStream
     *
     * @param inputStream
     * @param type
     * @return
     * @throws JSonMapperException
     */
    @Override
    public <T> T inputStreamToObject(final InputStream inputStream, final TypeRef<T> type) throws JSonMapperException {
        try {
            try {
                return byteArrayToObject(IO.readStream(-1, inputStream), type);
            } finally {
                if (inputStream != null) {
                    inputStream.close();
                }
            }
        } catch (IOException e) {
            throw new JSonMapperException(e);
        }
    }

    @Override
    public <T> T byteArrayToObject(final byte[] byteArray, final TypeRef<T> type) throws JSonMapperException {
        try {
            return stringToObject(IO.BOM.read(byteArray, UTF8), type);
        } catch (IOException e) {
            throw new JSonMapperException(e);
        }
    }

    @Override
    public <T> T byteArrayToObject(final byte[] byteArray, final Class<T> clazz) throws JSonMapperException {
        try {
            return stringToObject(IO.BOM.read(byteArray, UTF8), clazz);
        } catch (IOException e) {
            throw new JSonMapperException(e);
        }
    }

    /**
     * closes inputStream
     *
     * @param inputStream
     * @param clazz
     * @return
     * @throws JSonMapperException
     */
    @Override
    public <T> T inputStreamToObject(final InputStream inputStream, Class<T> clazz) throws JSonMapperException {
        try {
            try {
                return byteArrayToObject(IO.readStream(-1, inputStream), clazz);
            } finally {
                if (inputStream != null) {
                    inputStream.close();
                }
            }
        } catch (IOException e) {
            throw new JSonMapperException(e);
        }
    }
}
