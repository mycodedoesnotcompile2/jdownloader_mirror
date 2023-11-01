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
import java.io.IOException;
import java.lang.reflect.Array;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.atomic.AtomicLong;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.shutdown.ShutdownController;
import org.appwork.storage.config.handler.StorageHandler;
import org.appwork.utils.Application;
import org.appwork.utils.IO;
import org.appwork.utils.IO.SYNC;
import org.appwork.utils.ModifyLock;
import org.appwork.utils.logging2.LogInterface;

public class JsonKeyValueStorage extends Storage {
    private final Map<String, Object> internalMap;
    private final String              name;
    private final File                file;
    private final boolean             plain;
    private final byte[]              key;
    private boolean                   autoPutValues   = true;
    private volatile boolean          closed          = false;
    private final AtomicLong          setMark         = new AtomicLong(0);
    private final AtomicLong          writeMark       = new AtomicLong(0);
    private boolean                   enumCacheEnabled;
    private final ModifyLock          modifyLock      = new ModifyLock();
    private IO.SYNC                   storageSyncMode = IO.SYNC.NONE;
    private final static SimpleMapper MAPPER          = new SimpleMapper();
    {
        MAPPER.setPrettyPrintEnabled(false);
    }

    public IO.SYNC getStorageSyncMode() {
        return storageSyncMode;
    }

    public void setStorageSyncMode(IO.SYNC storageSyncMode) {
        if (storageSyncMode == null) {
            this.storageSyncMode = IO.SYNC.NONE;
        } else {
            this.storageSyncMode = storageSyncMode;
        }
    }

    private final Map<String, Object> getMap() {
        return internalMap;
    }

    private final ModifyLock getLock() {
        return modifyLock;
    }

    public JsonKeyValueStorage(final File file) throws StorageException {
        this(file, false);
    }

    public JsonKeyValueStorage(final File file, final boolean plain) throws StorageException {
        this(file, plain, JSonStorage.KEY);
    }

    public JsonKeyValueStorage(final File file, final boolean plain, final byte[] key) throws StorageException {
        this(file, null, plain, key);
    }

    public List<String> getKeys() {
        final boolean readL = getLock().readLock();
        try {
            return new ArrayList<String>(getMap().keySet());
        } finally {
            getLock().readUnlock(readL);
        }
    }

    /**
     * @param file
     * @param resource
     * @param b
     * @param key2
     */
    public JsonKeyValueStorage(final File file, final URL resource, final boolean plain, final byte[] key) {
        this.internalMap = new HashMap<String, Object>();
        this.plain = plain;
        this.file = file;
        this.name = file.getName();
        this.key = key;
        if (resource != null) {
            getDefaultLogger().info("Load JSon Storage from Classpath url: " + resource);
            try {
                final HashMap<String, Object> load = JSonStorage.restoreFromByteArray(IO.readURL(resource), plain, key, TypeRef.HASHMAP, new HashMap<String, Object>());
                this.putAll(load);
            } catch (final IOException e) {
                throw new WTFException(e);
            }
        }
        if (file.exists()) {
            getDefaultLogger().info("Prefer (merged) JSon Storage from File: " + file);
            final HashMap<String, Object> load = JSonStorage.restoreFrom(file, plain, key, TypeRef.HASHMAP, new HashMap<String, Object>());
            this.putAll(load);
        } else {
            getDefaultLogger().info("CFG File does not exist: " + file);
        }
    }

    /**
     * @return
     */
    protected LogInterface getDefaultLogger() {
        return LogV3.I().getDefaultLogger();
    }

    public JsonKeyValueStorage(final String name) throws StorageException {
        this(name, false);
    }

    public JsonKeyValueStorage(final String name, final boolean plain) throws StorageException {
        this(name, plain, JSonStorage.KEY);
    }

    public JsonKeyValueStorage(final String name, final boolean plain, final byte[] key) throws StorageException {
        this.internalMap = new HashMap<String, Object>();
        this.name = name;
        this.plain = plain;
        this.file = Application.getResource("cfg/" + name + (plain ? ".json" : ".ejs"));
        getDefaultLogger().finer("Read Config: " + this.file.getAbsolutePath());
        this.key = key;
        final HashMap<String, Object> load = JSonStorage.restoreFrom(this.file, plain, key, TypeRef.HASHMAP, new HashMap<String, Object>());
        this.putAll(load);
    }

    @Override
    public void clear() throws StorageException {
        getLock().writeLock();
        try {
            getMap().clear();
        } finally {
            getLock().writeUnlock();
            this.requestSave();
        }
    }

    @Override
    public void close() {
        this.closed = true;
    }

    public <E> E get(final String key, final E def) throws StorageException {
        return get(key, def, null);
    }

    @SuppressWarnings("unchecked")
    @Override
    public <E> E get(final String key, final E def, final Boolean autoPutValue) throws StorageException {
        final boolean contains;
        final boolean autoPutDefaultValue = autoPutValue == null ? isAutoPutValues() : Boolean.TRUE.equals(autoPutValue);
        Object ret = null;
        final boolean readL = getLock().readLock();
        try {
            ret = getMap().get(key);
            if (ret == null) {
                contains = getMap().containsKey(key);
            } else {
                contains = true;
            }
        } finally {
            getLock().readUnlock(readL);
        }
        if (ret != null && def != null && ret.getClass() != def.getClass()) {
            /* ret class different from def class, so we have to convert */
            if (def instanceof Byte) {
                if (ret instanceof Number) {
                    ret = ((Number) ret).byteValue();
                } else if (ret instanceof String) {
                    ret = Byte.parseByte((String) ret);
                }
            } else if (def instanceof Short) {
                if (ret instanceof Number) {
                    ret = ((Number) ret).shortValue();
                } else if (ret instanceof String) {
                    ret = Short.parseShort((String) ret);
                }
            } else if (def instanceof Long) {
                if (ret instanceof Number) {
                    ret = ((Number) ret).longValue();
                } else if (ret instanceof String) {
                    ret = Long.parseLong((String) ret);
                }
            } else if (def instanceof Integer) {
                if (ret instanceof Number) {
                    ret = ((Number) ret).intValue();
                } else if (ret instanceof String) {
                    ret = Integer.parseInt((String) ret);
                }
            } else if (def instanceof Double) {
                if (ret instanceof Number) {
                    ret = ((Number) ret).doubleValue();
                } else if (ret instanceof String) {
                    ret = Double.parseDouble((String) ret);
                }
            } else if (def instanceof Float) {
                if (ret instanceof Number) {
                    ret = ((Number) ret).floatValue();
                } else if (ret instanceof String) {
                    ret = Float.parseFloat((String) ret);
                }
            }
        }
        // put entry if we have no entry
        if (!contains) {
            ret = def;
            if (autoPutDefaultValue) {
                if (def instanceof Byte) {
                    this.put(key, (Byte) def);
                } else if (def instanceof Short) {
                    this.put(key, (Short) def);
                } else if (def instanceof Long) {
                    this.put(key, (Long) def);
                } else if (def instanceof Integer) {
                    this.put(key, (Integer) def);
                } else if (def instanceof Double) {
                    this.put(key, (Double) def);
                } else if (def instanceof Float) {
                    this.put(key, (Float) def);
                } else if (def instanceof Boolean) {
                    this.put(key, (Boolean) def);
                } else if (def instanceof String || def == null) {
                    this.put(key, (String) def);
                } else if (def instanceof Enum<?>) {
                    this.put(key, (Enum<?>) def);
                } else {
                    throw new StorageException("Invalid datatype: " + (def != null ? def.getClass() : "null"));
                }
            }
        }
        if (def instanceof Enum<?> && ret instanceof String) {
            try {
                ret = Enum.valueOf(((Enum<?>) def).getDeclaringClass(), (String) ret);
                if (autoPutDefaultValue && this.isEnumCacheEnabled()) {
                    this.put(key, (Enum<?>) ret);
                }
            } catch (final Throwable e) {
                if (e instanceof IllegalArgumentException) {
                    getDefaultLogger().exception("Could not restore the enum. There is no value for " + ret + " in " + ((Enum<?>) def).getDeclaringClass(), e);
                    if (autoPutDefaultValue) {
                        // enum no longer declares value, so we remove it and default will be used instead
                        this.remove(key);
                    }
                } else {
                    getDefaultLogger().log(e);
                }
                if (autoPutDefaultValue && this.isEnumCacheEnabled()) {
                    this.put(key, (Enum<?>) def);
                }
                ret = def;
            }
        }
        return (E) ret;
    }

    /**
     * @return the key
     */
    @Override
    public byte[] getCryptKey() {
        return this.key;
    }

    public File getFile() {
        return this.file;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.Storage#getID()
     */
    @Override
    public String getID() {
        return this.file.getAbsolutePath();
    }

    public String getName() {
        return this.name;
    }

    @Override
    public boolean hasProperty(final String key) {
        final boolean readL = getLock().readLock();
        try {
            return getMap().containsKey(key);
        } finally {
            getLock().readUnlock(readL);
        }
    }

    private Object internal_put(final String key, final Object value) {
        if (key == null) {
            throw new WTFException("key == null is forbidden!");
        }
        boolean requestSave = true;
        getLock().writeLock();
        try {
            final Object ret = getMap().put(key, value);
            requestSave = !equals(ret, value);
            return ret;
        } finally {
            getLock().writeUnlock();
            if (requestSave) {
                this.requestSave();
            }
        }
    }

    private static final Set<Class<?>> WRAPPER_TYPES = getWrapperTypes();

    public static boolean isWrapperType(final Class<?> clazz) {
        return WRAPPER_TYPES.contains(clazz);
    }

    private static Set<Class<?>> getWrapperTypes() {
        final Set<Class<?>> ret = new HashSet<Class<?>>();
        ret.add(Boolean.class);
        ret.add(Character.class);
        ret.add(Byte.class);
        ret.add(Short.class);
        ret.add(Integer.class);
        ret.add(Long.class);
        ret.add(Float.class);
        ret.add(Double.class);
        ret.add(Void.class);
        ret.add(String.class);
        return ret;
    }

    protected boolean equals(Object x, Object y) {
        try {
            if (x == null && y == null) {
                return true;
            } else if (x != null && y != null) {
                final Class<?> xC = x.getClass();
                final Class<?> yC = y.getClass();
                if (xC.isPrimitive() && yC.isPrimitive()) {
                    // primitives are safe to x.equals(y)
                    return x.equals(y);
                }
                if (isWrapperType(xC) && isWrapperType(yC)) {
                    // wrappers are safe to x.equals(y)
                    return x.equals(y);
                }
                if (xC.isEnum() && yC.isEnum()) {
                    // enums are safe to x.equals(y)
                    return x.equals(y);
                }
                final boolean xCList = List.class.isAssignableFrom(xC);
                final boolean yCList = List.class.isAssignableFrom(yC);
                if (xCList && yCList) {
                    final List<?> xL = (List) x;
                    final List<?> yL = (List) y;
                    final int xLL = xL.size();
                    final int yLL = yL.size();
                    if (xLL == yLL) {
                        for (int index = 0; index < xLL; index++) {
                            final Object xE = xL.get(index);
                            final Object yE = yL.get(index);
                            if (equals(xE, yE) == false) {
                                return false;
                            }
                        }
                        return true;
                    } else {
                        return false;
                    }
                }
                final boolean xCArray = xC.isArray();
                final boolean yCArray = yC.isArray();
                if (xCArray && yCArray) {
                    final int xL = Array.getLength(x);
                    final int yL = Array.getLength(y);
                    if (xL == yL) {
                        for (int index = 0; index < xL; index++) {
                            final Object xE = Array.get(x, index);
                            final Object yE = Array.get(y, index);
                            if (equals(xE, yE) == false) {
                                return false;
                            }
                        }
                        return true;
                    } else {
                        return false;
                    }
                }
            }
        } catch (final Throwable e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * @return the autoPutValues
     */
    @Override
    public boolean isAutoPutValues() {
        return this.autoPutValues;
    }

    /**
     * @return
     */
    private boolean isEnumCacheEnabled() {
        return this.enumCacheEnabled;
    }

    public boolean isPlain() {
        return this.plain;
    }

    public void put(final String key, final boolean value) throws StorageException {
        this.internal_put(key, value);
    }

    @Override
    public void put(final String key, final Boolean value) throws StorageException {
        this.internal_put(key, value);
    }

    @Override
    public void put(final String key, final Byte value) throws StorageException {
        this.internal_put(key, value);
    }

    @Override
    public void put(final String key, final Double value) throws StorageException {
        this.internal_put(key, value);
    }

    @Override
    public void put(final String key, final Enum<?> value) throws StorageException {
        if (value == null) {
            this.internal_put(key, null);
        } else {
            if (this.isEnumCacheEnabled()) {
                this.internal_put(key, value);
            } else {
                this.internal_put(key, value.name());
            }
        }
    }

    @Override
    public void put(final String key, final Float value) throws StorageException {
        this.internal_put(key, value);
    }

    public void put(final String key, final int value) throws StorageException {
        this.internal_put(key, value);
    }

    @Override
    public void put(final String key, final Integer value) throws StorageException {
        this.internal_put(key, value);
    }

    public void put(final String key, final long value) throws StorageException {
        this.internal_put(key, value);
    }

    @Override
    public void put(final String key, final Long value) throws StorageException {
        this.internal_put(key, value);
    }

    @Override
    public void put(String key, Short value) throws StorageException {
        this.internal_put(key, value);
    }

    public void put(String key, short value) throws StorageException {
        this.internal_put(key, value);
    }

    @Override
    public void put(final String key, final String value) throws StorageException {
        this.internal_put(key, value);
    }

    private void putAll(final Map<String, Object> map) {
        if (map != null) {
            getLock().writeLock();
            try {
                final Iterator<Entry<String, Object>> it = map.entrySet().iterator();
                while (it.hasNext()) {
                    final Entry<String, Object> next = it.next();
                    if (next.getKey() != null) {
                        getMap().put(next.getKey(), next.getValue());
                    }
                }
            } finally {
                getLock().writeUnlock();
            }
        }
    }

    @Override
    public Object remove(final String key) {
        if (key == null) {
            throw new WTFException("key ==null is forbidden!");
        }
        if (hasProperty(key)) {
            getLock().writeLock();
            try {
                return getMap().remove(key);
            } finally {
                getLock().writeUnlock();
                this.requestSave();
            }
        }
        return null;
    }

    public void requestSave() {
        final long mark = this.setMark.incrementAndGet();
        if (false) {
            new WTFException("requestSave:" + this.getID() + "|" + mark).printStackTrace();
        }
    }

    @Override
    public void save() throws StorageException {
        if (this.closed) {
            throw new StorageException("StorageChest already closed!");
        }
        final long lastSetMark = this.setMark.get();
        if (this.writeMark.getAndSet(lastSetMark) != lastSetMark) {
            final boolean readL = getLock().readLock();
            final byte[] jsonBytes;
            try {
                jsonBytes = MAPPER.objectToByteArray(getMap());
                writeMark.set(setMark.get());
            } finally {
                getLock().readUnlock(readL);
            }
            final Runnable run = new Runnable() {
                @Override
                public void run() {
                    final SYNC sync = ShutdownController.getInstance().isShuttingDown() ? SYNC.META_AND_DATA : getStorageSyncMode();
                    JSonStorage.saveTo(file, plain, key, jsonBytes, sync);
                }
            };
            StorageHandler.enqueueWrite(run, file.getAbsolutePath(), true);
        }
    }

    /**
     * @param autoPutValues
     *            the autoPutValues to set
     */
    @Override
    public void setAutoPutValues(final boolean autoPutValues) {
        this.autoPutValues = autoPutValues;
    }

    public void setEnumCacheEnabled(final boolean enumCacheEnabled) {
        this.enumCacheEnabled = enumCacheEnabled;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.Storage#size()
     */
    @Override
    public int size() {
        final boolean readL = getLock().readLock();
        try {
            return getMap().size();
        } finally {
            getLock().readUnlock(readL);
        }
    }

    @Override
    public String toString() {
        final boolean readL = getLock().readLock();
        try {
            return MAPPER.objectToString(getMap());
        } catch (final Throwable e) {
            return getMap().toString();
        } finally {
            getLock().readUnlock(readL);
        }
    }

}
