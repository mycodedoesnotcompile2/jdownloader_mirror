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

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.lang.ref.WeakReference;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.WeakHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import javax.crypto.Cipher;
import javax.crypto.CipherInputStream;
import javax.crypto.CipherOutputStream;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import org.appwork.shutdown.ShutdownController;
import org.appwork.shutdown.ShutdownEvent;
import org.appwork.shutdown.ShutdownRequest;
import org.appwork.storage.simplejson.JSonFactory;
import org.appwork.utils.IO;
import org.appwork.utils.IO.SYNC;
import org.appwork.utils.IO.WriteToFileCallback;
import org.appwork.utils.StringUtils;
import org.appwork.utils.reflection.Clazz;

public class JSonStorage {
    /* hash map contains file location as string and the storage instance */
    private static final HashMap<String, Storage>                   MAP               = new HashMap<String, Storage>();
    private final static WeakHashMap<String, WeakReference<String>> DEDUPEMAP         = new WeakHashMap<String, WeakReference<String>>();
    public static final AtomicBoolean                               WRITE_ON_SHUTDOWN = new AtomicBoolean(true);

    public static String dedupeString(String string) {
        if (string != null) {
            synchronized (DEDUPEMAP) {
                String ret = null;
                WeakReference<String> ref = DEDUPEMAP.get(string);
                if (ref != null && (ret = ref.get()) != null) {
                    return ret;
                }
                ref = new WeakReference<String>(string);
                DEDUPEMAP.put(string, ref);
                return string;
            }
        } else {
            return string;
        }
    }

    private static JSONMapper                         JSON_MAPPER = new SimpleMapper() {
                                                                      protected JSonFactory newJsonFactory(String jsonString) {
                                                                          return new JSonFactory(jsonString) {
                                                                                                                                            @Override
                                                                                                                                            protected String dedupeString(String string) {
                                                                                                                                                return JSonStorage.dedupeString(string);
                                                                                                                                            };
                                                                                                                                        };
                                                                      };
                                                                  };
    /* default key for encrypted json */
    static public byte[]                              KEY         = new byte[] { 0x01, 0x02, 0x11, 0x01, 0x01, 0x54, 0x01, 0x01, 0x01, 0x01, 0x12, 0x01, 0x01, 0x01, 0x22, 0x01 };
    private static final HashMap<File, AtomicInteger> LOCKS       = new HashMap<File, AtomicInteger>();
    static {
        /* shutdown hook to save all open Storages */
        ShutdownController.getInstance().addShutdownEvent(new ShutdownEvent() {
            @Override
            public long getMaxDuration() {
                return 0;
            }

            @Override
            public int getHookPriority() {
                return 0;
            }

            @Override
            public void onShutdown(final ShutdownRequest shutdownRequest) {
                if (WRITE_ON_SHUTDOWN.get()) {
                    JSonStorage.close();
                }
            }

            @Override
            public String toString() {
                return "ShutdownEvent: Save JSonStorages";
            }
        });
    }

    /**
     * @param returnType
     * @return
     */
    public static boolean canStorePrimitive(final Class<?> type) {
        return Clazz.isPrimitive(type) || type == String.class || type.isEnum();
    }

    public static JSONMapper getMapper() {
        return JSonStorage.JSON_MAPPER;
    }

    /**
     * TODO: Difference to {@link #getStorage(String)} ?
     */
    public static Storage getPlainStorage(final String name) throws StorageException {
        final String id = name + "_plain";
        Storage ret = null;
        synchronized (JSonStorage.MAP) {
            ret = JSonStorage.MAP.get(id);
            if (ret != null) {
                return ret;
            }
        }
        ret = new JsonKeyValueStorage(name, true);
        synchronized (JSonStorage.MAP) {
            final Storage ret2 = JSonStorage.MAP.get(id);
            if (ret2 != null) {
                return ret2;
            }
            JSonStorage.MAP.put(id, ret);
        }
        return ret;
    }

    public static Storage getStorage(final String name) throws StorageException {
        final String id = name + "_crypted";
        Storage ret = null;
        synchronized (JSonStorage.MAP) {
            ret = JSonStorage.MAP.get(id);
            if (ret != null) {
                return ret;
            }
        }
        ret = new JsonKeyValueStorage(name);
        synchronized (JSonStorage.MAP) {
            final Storage ret2 = JSonStorage.MAP.get(id);
            if (ret2 != null) {
                return ret2;
            }
            JSonStorage.MAP.put(id, ret);
        }
        return ret;
    }

    public static synchronized Object requestLock(final File file) {
        AtomicInteger lock = JSonStorage.LOCKS.get(file);
        if (lock == null) {
            lock = new AtomicInteger(0);
            JSonStorage.LOCKS.put(file, lock);
        }
        lock.incrementAndGet();
        return lock;
    }

    @Deprecated
    /**
     * @deprecated don't mix JSON and IO
     *
     */
    public static <E> E restoreFrom(final File file, final boolean plain, final byte[] key, final TypeRef<E> type, final E def) {
        final Object lock = JSonStorage.requestLock(file);
        synchronized (lock) {
            FileInputStream fis = null;
            try {
                final File res = file;
                if (!res.isFile() || res.length() == 0) {
                    return def;
                }
                fis = new FileInputStream(res);
                final InputStream is;
                if (plain) {
                    is = fis;
                } else {
                    is = createCipherInputStream(fis, key, key);
                }
                return restoreFromInputStream(is, type);
            } catch (final Throwable e) {
                org.appwork.loggingv3.LogV3.log(new Exception("Failed to restore JSON Object from " + file + ". Fallback to default value. ", e));
                return def;
            } finally {
                try {
                    if (fis != null) {
                        fis.close();
                    }
                } catch (IOException ignore) {
                }
                JSonStorage.unLock(file);
            }
        }
    }

    @Deprecated
    /**
     * @deprecated don't mix JSON and IO
     *
     */
    public static <E> E restoreFromFile(final File file, final E def) {
        final E ret = JSonStorage.restoreFrom(file, true, null, null, def);
        if (ret == null) {
            return def;
        }
        return ret;
    }

    public static <E> E restoreFromByteArray(final byte[] jsonByteArray, final boolean plain, final byte[] key, final TypeRef<E> type, final E def) {
        if (jsonByteArray != null) {
            try {
                final byte[] byteArray;
                if (!plain) {
                    byteArray = decryptByteArray(jsonByteArray, key, key);
                } else {
                    byteArray = jsonByteArray;
                }
                return restoreFromByteArray(byteArray, type, def);
            } catch (final Exception e) {
                org.appwork.loggingv3.LogV3.log(e);
            }
        }
        return def;
    }

    private static byte[] decryptByteArray(final byte[] b, final byte[] key, final byte[] iv) {
        try {
            final IvParameterSpec ivSpec = new IvParameterSpec(iv);
            final SecretKeySpec skeySpec = new SecretKeySpec(key, "AES");
            final Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
            cipher.init(Cipher.DECRYPT_MODE, skeySpec, ivSpec);
            return cipher.doFinal(b);
        } catch (final Exception e) {
            org.appwork.loggingv3.LogV3.log(e);
            final IvParameterSpec ivSpec = new IvParameterSpec(iv);
            final SecretKeySpec skeySpec = new SecretKeySpec(key, "AES");
            try {
                final Cipher cipher = Cipher.getInstance("AES/CBC/nopadding");
                cipher.init(Cipher.DECRYPT_MODE, skeySpec, ivSpec);
                cipher.doFinal(b);
            } catch (final Exception e1) {
                org.appwork.loggingv3.LogV3.log(e1);
            }
        }
        return null;
    }

    public static CipherInputStream createCipherInputStream(final InputStream inputStream, final byte[] key, final byte[] iv) throws NoSuchAlgorithmException, NoSuchPaddingException, InvalidKeyException, InvalidAlgorithmParameterException {
        final IvParameterSpec ivSpec = new IvParameterSpec(iv);
        final SecretKeySpec skeySpec = new SecretKeySpec(key, "AES");
        final Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
        cipher.init(Cipher.DECRYPT_MODE, skeySpec, ivSpec);
        return new CipherInputStream(inputStream, cipher);
    }

    public static CipherOutputStream createCipherOutputStream(final OutputStream outputStream, final byte[] key, final byte[] iv) throws NoSuchAlgorithmException, NoSuchPaddingException, InvalidKeyException, InvalidAlgorithmParameterException {
        final IvParameterSpec ivSpec = new IvParameterSpec(iv);
        final SecretKeySpec skeySpec = new SecretKeySpec(key, "AES");
        final Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
        cipher.init(Cipher.ENCRYPT_MODE, skeySpec, ivSpec);
        return new CipherOutputStream(outputStream, cipher);
    }

    @Deprecated
    /**
     * @deprecated use public static <E> E restoreFromString(final String string, final TypeRef<E> type) {
     *
     */
    public static <T> T restoreFromString(final String string, final Class<T> class1) throws StorageException {
        try {
            return JSonStorage.JSON_MAPPER.stringToObject(string, class1);
        } catch (final Exception e) {
            throw new StorageException(string, e);
        }
    }

    public static <E> E restoreFromString(final String string, final TypeRef<E> type) {
        if (string == null || "".equals(string)) {
            return null;
        } else {
            return JSonStorage.JSON_MAPPER.stringToObject(string, type);
        }
    }

    public static <E> E restoreFromByteArray(final byte[] bytes, final TypeRef<E> type) {
        if (bytes == null || bytes.length == 0) {
            return null;
        } else {
            return JSonStorage.JSON_MAPPER.byteArrayToObject(bytes, type);
        }
    }

    /**
     * deserializes the inputstream to an object defined in type. This method throws an exception in case of any errors
     *
     * @param <E>
     * @param is
     * @param type
     * @return
     */
    @SuppressWarnings("unchecked")
    public static <E> E restoreFromInputStream(final InputStream is, final TypeRef<E> type) {
        if (is == null) {
            throw new IllegalArgumentException("Inputstream is null");
        } else if (type == null) {
            throw new IllegalArgumentException("Type is null");
        } else {
            return JSonStorage.JSON_MAPPER.inputStreamToObject(is, type);
        }
    }

    public static <E> E restoreFromString(final String string, final TypeRef<E> type, final E def) {
        if (string != null && string.length() > 0) {
            try {
                return restoreFromByteArray(string.getBytes("UTF-8"), type, def);
            } catch (UnsupportedEncodingException e) {
                org.appwork.loggingv3.LogV3.log(e);
            }
        }
        return def;
    }

    @SuppressWarnings("unchecked")
    public static <E> E restoreFromByteArray(final byte[] byteArray, final TypeRef<E> type, final E def) {
        if (byteArray != null && byteArray.length > 0) {
            try {
                if (type != null) {
                    return JSonStorage.JSON_MAPPER.byteArrayToObject(byteArray, type);
                } else {
                    return (E) JSonStorage.JSON_MAPPER.byteArrayToObject(byteArray, def.getClass());
                }
            } catch (final Exception e) {
                org.appwork.loggingv3.LogV3.log(e);
            }
        }
        return def;
    }

    /**
     * Deserializes the inpustream to a object defined in type. In case of any exceptions this method returns the default value def, logs
     * exceptions and swallows them.
     *
     * @param <E>
     * @param is
     * @param type
     * @param def
     * @return
     */
    @SuppressWarnings("unchecked")
    public static <E> E restoreFromInputStream(final InputStream is, final TypeRef<E> type, final E def) {
        if (is != null) {
            try {
                if (type != null) {
                    return JSonStorage.JSON_MAPPER.inputStreamToObject(is, type);
                } else {
                    return (E) JSonStorage.JSON_MAPPER.inputStreamToObject(is, def.getClass());
                }
            } catch (final Exception e) {
                org.appwork.loggingv3.LogV3.log(new Exception());
            }
        }
        return def;
    }

    private static void close() {
        org.appwork.loggingv3.LogV3.finer("Start Saving Storage");
        final List<Storage> storages;
        synchronized (JSonStorage.MAP) {
            storages = new ArrayList<Storage>(JSonStorage.MAP.values());
        }
        for (final Storage storage : storages) {
            try {
                storage.save();
            } catch (final Throwable e) {
                org.appwork.loggingv3.LogV3.log(e);
            } finally {
                try {
                    storage.close();
                } catch (final Throwable e2) {
                    org.appwork.loggingv3.LogV3.log(e2);
                }
            }
        }
        org.appwork.loggingv3.LogV3.finer("ENDED Saving Storage");
    }

    @Deprecated
    /**
     * @deprecated use IO.secureWrite instead
     *
     */
    public static void saveTo(final File file, final boolean plain, final byte[] key, final String json) throws StorageException {
        try {
            saveTo(file, plain, key, json.getBytes("UTF-8"), SYNC.META_AND_DATA);
        } catch (final IOException e) {
            throw new StorageException(e);
        }
    }

    @Deprecated
    public static void saveTo(final File file, final boolean plain, final byte[] key, final byte[] data, final IO.SYNC syncMode) throws StorageException {
        final Object lock = JSonStorage.requestLock(file);
        synchronized (lock) {
            try {
                file.getParentFile().mkdirs();
                final IO.WriteToFileCallback writeToFileCallback = new WriteToFileCallback() {
                    @Override
                    public void writeTo(OutputStream os) throws IOException {
                        if (plain) {
                            os.write(data);
                        } else {
                            try {
                                final CipherOutputStream cos = createCipherOutputStream(os, key, key);
                                cos.write(data);
                                cos.close();
                            } catch (IOException e) {
                                throw e;
                            } catch (Exception e) {
                                throw new IOException(e);
                            }
                        }
                    }

                    @Override
                    public void onIOException(IOException e) throws IOException {
                    }

                    @Override
                    public void onClosed() {
                    }
                };
                IO.secureWrite(file, writeToFileCallback, syncMode);
            } catch (final Exception e) {
                throw new StorageException("Can not write to " + file.getAbsolutePath(), e);
            } finally {
                JSonStorage.unLock(file);
            }
        }
    }

    /**
     * @deprecated use IO.secureWrite instead
     *
     */
    public static void saveTo(final File file, final boolean plain, final byte[] key, final byte[] data) throws StorageException {
        saveTo(file, plain, key, data, IO.SYNC.NONE);
    }

    public static String serializeToJson(final Object list) throws StorageException {
        try {
            return JSonStorage.JSON_MAPPER.objectToString(list);
        } catch (final Exception e) {
            throw new StorageException(e);
        }
    }

    public static byte[] serializeToJsonByteArray(final Object list) throws StorageException {
        try {
            return JSonStorage.JSON_MAPPER.objectToByteArray(list);
        } catch (final Exception e) {
            throw new StorageException(e);
        }
    }

    public static void setMapper(final JSONMapper mapper) {
        if (mapper == null) {
            throw new IllegalArgumentException("mapper is null");
        }
        JSonStorage.JSON_MAPPER = mapper;
    }

    @Deprecated
    /**
     * @deprecated ..check usage and give a better name.
     *
     *             This method throws Exceptions
     *
     * @param string
     * @param type
     * @param def
     * @return
     */
    @SuppressWarnings("unchecked")
    public static <E> E stringToObject(final String string, final TypeRef<E> type, final E def) {
        if (StringUtils.isEmpty(string)) {
            throw new IllegalArgumentException("cannot stringToObject from empty string");
        }
        if (type != null) {
            return JSonStorage.JSON_MAPPER.stringToObject(string, type);
        } else {
            return (E) JSonStorage.JSON_MAPPER.stringToObject(string, def.getClass());
        }
    }

    /**
     * USe this method for debug code only. It is NOT guaranteed that this method returns json formated text. Use
     * {@link #serializeToJson(Object)} instead
     *
     * @param list
     * @return
     */
    @Deprecated
    public static String toString(final Object list) {
        try {
            return JSonStorage.JSON_MAPPER.objectToString(list);
        } catch (final Throwable e) {
            e.printStackTrace();
        }
        return list.toString();
    }

    public static synchronized void unLock(final File file) {
        final AtomicInteger lock = JSonStorage.LOCKS.get(file);
        if (lock != null) {
            if (lock.decrementAndGet() == 0) {
                JSonStorage.LOCKS.remove(file);
            }
        }
    }

    /**
     * @param <E>
     * @param o
     * @param typeRef
     * @return
     */
    public static <E> E convert(Object o, TypeRef<E> typeRef) {
        return JSON_MAPPER.convert(o, typeRef);
    }
}
