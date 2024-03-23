package org.appwork.serializer;

import java.io.InputStream;
import java.io.OutputStream;

import org.appwork.storage.TypeRef;
import org.appwork.storage.commonInterface.SerializerException;
import org.appwork.storage.commonInterface.SerializerInterface;
import org.appwork.utils.reflection.Clazz;

public class Deser {
    private static final String                     AWU_SERIALIZER_CLASS = "AWU_SERIALIZER_CLASS";
    private static SerializerInterface              SERIALIZER           = createDefaultSerializer();
    private static ThreadLocal<SerializerInterface> THREAD_SERIALIZER    = new ThreadLocal<SerializerInterface>();

    public static SerializerInterface createDefaultSerializer() {
        final String cls = System.getProperty(AWU_SERIALIZER_CLASS, "org.appwork.storage.SimpleSerializer");
        try {
            return (SerializerInterface) Class.forName(cls).newInstance();
        } catch (InstantiationException e) {
            throw new RuntimeException("Could not instantiate the Appwork Default serializer: " + cls, e);
        } catch (IllegalAccessException e) {
            throw new RuntimeException("Could not instantiate the Appwork Default serializer: " + cls, e);
        } catch (ClassNotFoundException e) {
            throw new RuntimeException("Could not instantiate the Appwork Default serializer: " + cls, e);
        }
    }

    public static SerializerInterface setThreadDeser(SerializerInterface i) {
        final SerializerInterface ret = THREAD_SERIALIZER.get();
        if (i == SERIALIZER) {
            THREAD_SERIALIZER.set(null);
        } else {
            THREAD_SERIALIZER.set(i);
        }
        return ret;
    }

    public static SerializerInterface get() {
        final SerializerInterface ret = THREAD_SERIALIZER.get();
        if (ret != null) {
            return ret;
        } else {
            return SERIALIZER;
        }
    }

    public static void set(final SerializerInterface s) {
        if (s == null) {
            throw new IllegalArgumentException();
        } else {
            SERIALIZER = s;
        }
    }

    /**
     * @param updateClient
     * @return
     */
    public static SerializerInterface get(Object context) {
        return get();
    }

    // can be used if we need a special Serializer - this throws an exception if the currently set serializer is different
    public static <T extends SerializerInterface> T get(final Class<T> expected) {
        final SerializerInterface ret = get();
        if (!Clazz.isInstanceof(ret.getClass(), expected)) {
            throw new RuntimeException("We need a " + expected + " Serializer here!");
        } else {
            return (T) ret;
        }
    }

    /**
     * Serialize o to a byte array.
     *
     * @param context
     *            TODO
     * @param batchResponse
     *
     * @return
     */
    public static byte[] toByteArray(Object o, Object... context) throws SerializerException {
        return get().toByteArray(o, context);
    };

    /**
     * serialize o to a string
     *
     * @param context
     *            TODO
     * @param payload
     * @param b
     *
     * @return
     */
    public static String toString(Object o, Object... context) throws SerializerException {
        return get().toString(o, context);
    }

    /**
     * convert string to an object
     *
     * @param <T>
     * @param json
     * @param type
     * @return
     */
    public static <T> T fromString(String string, TypeRef<T> type) throws SerializerException {
        return get().fromString(string, type);
    }

    /**
     * read stream and convert the bytes to an object
     *
     * @param byteArrayInputStream
     * @param type
     * @return
     */
    public static <T> T fromStream(InputStream stream, TypeRef<T> type) throws SerializerException {
        return get().fromStream(stream, type);
    }

    /**
     * convert one objet to another - force new instances if the flag is set (Clone Mode)
     *
     * @param execute
     * @param b
     * @param class1
     * @return
     */
    public static <T> T convert(Object o, TypeRef<T> type, Object... context) throws SerializerException {
        return get().convert(o, type, context);
    }

    /**
     * convert byte array to object
     *
     * @param byteArray
     * @param type
     * @return
     */
    public static <T> T fromByteArray(byte[] byteArray, TypeRef<T> type) throws SerializerException {
        return get().fromByteArray(byteArray, type);
    }

    /**
     * @param o
     * @param os
     * @param context
     *            TODO
     */
    public static void toStream(Object o, OutputStream os, boolean closeOutputStream, Object... context) throws SerializerException {
        get().toStream(o, os, closeOutputStream, context);
    }
}
