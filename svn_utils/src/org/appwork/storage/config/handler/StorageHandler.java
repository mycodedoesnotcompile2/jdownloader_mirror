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
package org.appwork.storage.config.handler;

import java.io.File;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicBoolean;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.scheduler.DelayedRunnable;
import org.appwork.shutdown.ShutdownController;
import org.appwork.shutdown.ShutdownEvent;
import org.appwork.shutdown.ShutdownRequest;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.JsonKeyValueStorage;
import org.appwork.storage.SimpleMapper;
import org.appwork.storage.Storage;
import org.appwork.storage.StorageException;
import org.appwork.storage.config.ConfigInterface;
import org.appwork.storage.config.InterfaceParseException;
import org.appwork.storage.config.annotations.CryptedStorage;
import org.appwork.storage.config.annotations.DefaultBooleanArrayValue;
import org.appwork.storage.config.annotations.DefaultByteArrayValue;
import org.appwork.storage.config.annotations.DefaultDoubleArrayValue;
import org.appwork.storage.config.annotations.DefaultFloatArrayValue;
import org.appwork.storage.config.annotations.DefaultIntArrayValue;
import org.appwork.storage.config.annotations.DefaultLongArrayValue;
import org.appwork.storage.config.annotations.DefaultShortArrayValue;
import org.appwork.storage.config.annotations.DefaultStorageSyncMode;
import org.appwork.storage.config.events.ConfigEvent;
import org.appwork.storage.config.events.ConfigEventSender;
import org.appwork.utils.Application;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Files;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.StringUtils;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.reflection.Clazz;

/**
 * @author thomas
 * @param <T>
 *
 */
public class StorageHandler<T extends ConfigInterface> implements InvocationHandler {
    private final static LinkedHashMap<String, Runnable>    DELAYEDWRITES     = new LinkedHashMap<String, Runnable>();
    protected static final DelayedRunnable                  SAVEDELAYER       = new DelayedRunnable(5000, 30000) {
                                                                                  @Override
                                                                                  public void delayedrun() {
                                                                                      StorageHandler.saveAll();
                                                                                  }
                                                                              };
    private static final HashMap<StorageHandler<?>, String> STORAGEMAP;
    // can be used to disable writes on shutdown
    public static final AtomicBoolean                       WRITE_ON_SHUTDOWN = new AtomicBoolean(true);
    static {
        // important, because getDefaultLogger might initialize StorageHandler and access to STORAGEMAP must be ensured in constructor of
        // StorageHandler
        STORAGEMAP = new HashMap<StorageHandler<?>, String>();
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
                    flushWrites();
                }
            }

            @Override
            public String toString() {
                synchronized (DELAYEDWRITES) {
                    return "ShutdownEvent: ProcessDelayedWrites num=" + DELAYEDWRITES.size();
                }
            }
        });
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
                    StorageHandler.saveAll();
                }
            }

            @Override
            public String toString() {
                return "ShutdownEvent: SaveAllStorageHandler";
            }
        });
    }

    public static JsonKeyValueStorage createPrimitiveStorage(final File filePath, final String classPath, final Class<? extends ConfigInterface> configInterface) {
        final CryptedStorage crypted = configInterface.getAnnotation(CryptedStorage.class);
        final JsonKeyValueStorage ret;
        if (crypted != null) {
            byte[] key = JSonStorage.KEY;
            if (crypted.key() != null) {
                key = crypted.key();
            }
            final URL urlClassPath;
            if (classPath != null) {
                // Do not use Application.getResourceUrl here! it might return urls to local files instead of classpath urls
                urlClassPath = Application.class.getClassLoader().getResource(classPath + ".ejs");
            } else {
                urlClassPath = null;
            }
            ret = new JsonKeyValueStorage(new File(filePath.getAbsolutePath() + ".ejs"), urlClassPath, false, key);
        } else {
            final URL urlClassPath;
            if (classPath != null) {
                // Do not use Application.getResourceUrl here! it might return urls to local files instead of classpath urls
                urlClassPath = Application.class.getClassLoader().getResource(classPath + ".json");
            } else {
                urlClassPath = null;
            }
            ret = new JsonKeyValueStorage(new File(filePath.getAbsolutePath() + ".json"), urlClassPath, true, null);
        }
        final DefaultStorageSyncMode defaultStorageSyncMode = configInterface.getAnnotation(DefaultStorageSyncMode.class);
        if (defaultStorageSyncMode != null) {
            ret.setStorageSyncMode(defaultStorageSyncMode.value());
        }
        return ret;
    }

    public static void flushWrites() {
        final LogInterface logger = org.appwork.loggingv3.LogV3.I().getDefaultLogger();
        while (true) {
            synchronized (DELAYEDWRITES) {
                final Iterator<Runnable> it = DELAYEDWRITES.values().iterator();
                if (it.hasNext()) {
                    final Runnable next = it.next();
                    try {
                        next.run();
                    } catch (final Throwable th) {
                        logger.log(th);
                    } finally {
                        it.remove();
                    }
                } else {
                    return;
                }
            }
        }
    }

    private static final AtomicBoolean DELAYED_WRITES = new AtomicBoolean(false);

    public static void setDelayedWritesEnabled(final boolean enabled) {
        if (DELAYED_WRITES.compareAndSet(!enabled, enabled)) {
            if (!enabled) {
                flushWrites();
            }
        }
    }

    public static boolean isDelayedWritesEnabled() {
        return DELAYED_WRITES.get();
    }

    public static void enqueueWrite(final Runnable run, final String ID, final boolean delayWriteAllowed) {
        final boolean write;
        synchronized (DELAYEDWRITES) {
            final boolean isShuttingDown = ShutdownController.getInstance().isShuttingDown();
            final boolean isDelayedWritesEnabled = isDelayedWritesEnabled();
            if (isShuttingDown || !delayWriteAllowed || !isDelayedWritesEnabled) {
                if (DELAYEDWRITES.size() > 0) {
                    DELAYEDWRITES.remove(ID);
                }
                write = true;
            } else {
                DELAYEDWRITES.put(ID, run);
                write = false;
            }
        }
        if (write) {
            run.run();
        }
    }

    /**
     * @param interfaceName
     * @param storage
     * @return
     */
    public static StorageHandler<?> getStorageHandler(final String interfaceName, final String storage) {
        synchronized (StorageHandler.STORAGEMAP) {
            final String ID = interfaceName + "." + storage;
            final Iterator<Entry<StorageHandler<?>, String>> it = StorageHandler.STORAGEMAP.entrySet().iterator();
            StorageHandler<?> ret = null;
            while (it.hasNext()) {
                final Entry<StorageHandler<?>, String> next = it.next();
                if (ID.equals(next.getValue()) && (ret = next.getKey()) != null) {
                    return ret;
                }
            }
        }
        return null;
    }

    public static void saveAll() {
        synchronized (StorageHandler.STORAGEMAP) {
            for (final StorageHandler<?> storageHandler : StorageHandler.STORAGEMAP.keySet()) {
                try {
                    if (storageHandler.isSaveInShutdownHookEnabled()) {
                        storageHandler.write();
                    }
                } catch (final Throwable e) {
                    e.printStackTrace();
                }
            }
        }
    }

    private final Class<T>                         configInterface;
    protected final HashMap<Method, KeyHandler<?>> method2KeyHandlerMap      = new HashMap<Method, KeyHandler<?>>();
    protected final HashMap<String, KeyHandler<?>> key2KeyHandlerMap         = new HashMap<String, KeyHandler<?>>();
    protected final Storage                        primitiveStorage;
    private final File                             path;
    private ConfigEventSender<Object>              eventSender               = null;
    private String                                 relativCPPath;
    // set externaly to start profiling
    public static HashMap<String, Long>            PROFILER_MAP              = null;
    public static HashMap<String, Long>            PROFILER_CALLNUM_MAP      = null;
    private volatile WriteStrategy                 writeStrategy             = null;
    private boolean                                objectCacheEnabled        = true;
    private final String                           storageID;
    boolean                                        saveInShutdownHookEnabled = true;
    private DefaultFactoryInterface                defaultFactory;
    private final static SimpleMapper              MAPPER                    = new SimpleMapper();
    {
        MAPPER.setPrettyPrintEnabled(false);
    }

    public DefaultFactoryInterface getDefaultFactory() {
        return defaultFactory;
    }

    public void setDefaultFactory(DefaultFactoryInterface defaultFactory) {
        this.defaultFactory = defaultFactory;
    }

    public StorageHandler() {
        // dummy instance. TODO: use interface instead
        this.configInterface = null;
        this.path = null;
        this.storageID = null;
        this.primitiveStorage = null;
    }

    /**
     * @param name
     * @param storage2
     * @param configInterface
     */
    public StorageHandler(final File filePath, final Class<T> configInterface) {
        this.configInterface = configInterface;
        this.path = filePath;
        preInit(path, configInterface);
        if (filePath.getName().endsWith(".json") || filePath.getName().endsWith(".ejs")) {
            org.appwork.loggingv3.LogV3.warning(filePath + " should not have an extension!!");
        }
        final File expected = Application.getResource("cfg/" + configInterface.getName());
        String storageID = null;
        if (!this.path.equals(expected)) {
            storageID = Files.getRelativePath(expected.getParentFile().getParentFile(), this.path);
            if (StringUtils.isEmpty(storageID)) {
                storageID = this.path.getAbsolutePath();
            }
        }
        this.storageID = storageID;
        String relativePath = null;
        try {
            // this way the config system reads default values from the classpath (bin folder or jar)
            relativePath = Files.getRelativePath(Application.getResource(""), filePath);
            this.relativCPPath = relativePath;
        } catch (Throwable e) {
            e.printStackTrace();
        }
        this.primitiveStorage = StorageHandler.createPrimitiveStorage(this.path, relativePath, configInterface);
        final CryptedStorage cryptedStorage = configInterface.getAnnotation(CryptedStorage.class);
        if (cryptedStorage != null) {
            this.validateKeys(cryptedStorage);
        }
        try {
            this.parseInterface();
        } catch (final InterfaceParseException e) {
            throw e;
        } catch (final Throwable e) {
            throw new InterfaceParseException(e);
        }
        this.addStorageHandler(this, configInterface.getName(), getStorageID());
    }

    /**
     * @param configInterfac
     * @param path
     *
     */
    protected void preInit(File path, Class<T> configInterfac) {
    }

    public StorageHandler(final Storage storage, final Class<T> configInterface) {
        String storagePath = storage.getID();
        if (storagePath.endsWith(".json")) {
            storagePath = storagePath.replaceFirst("\\.json$", "");
        } else if (storagePath.endsWith(".ejs")) {
            storagePath = storagePath.replaceFirst("\\.ejs$", "");
        }
        this.primitiveStorage = storage;
        this.path = new File(storagePath);
        this.configInterface = configInterface;
        preInit(path, configInterface);
        final File expected = Application.getResource("cfg/" + configInterface.getName());
        String storageID = null;
        if (!this.path.equals(expected)) {
            storageID = Files.getRelativePath(expected.getParentFile().getParentFile(), this.path);
            if (StringUtils.isEmpty(storageID)) {
                storageID = this.path.getAbsolutePath();
            }
        }
        this.storageID = storageID;
        final CryptedStorage cryptedStorage = configInterface.getAnnotation(CryptedStorage.class);
        if (cryptedStorage != null) {
            this.validateKeys(cryptedStorage);
        }
        try {
            org.appwork.loggingv3.LogV3.finer("Init StorageHandler for Interface:" + configInterface.getName() + "|Path:" + this.path);
            this.parseInterface();
        } catch (final Throwable e) {
            throw new InterfaceParseException(e);
        }
        this.addStorageHandler(this, configInterface.getName(), getStorageID());
    }

    protected StorageHandler(final Class<T> configInterface) {
        this.primitiveStorage = null;
        this.path = null;
        this.configInterface = configInterface;
        this.storageID = null;
        final CryptedStorage cryptedStorage = configInterface.getAnnotation(CryptedStorage.class);
        if (cryptedStorage != null) {
            this.validateKeys(cryptedStorage);
        }
        try {
            org.appwork.loggingv3.LogV3.finer("Init StorageHandler for Interface:" + configInterface.getName() + "|Path:" + this.path);
            this.parseInterface();
        } catch (final Throwable e) {
            throw new InterfaceParseException(e);
        }
        this.addStorageHandler(this, configInterface.getName(), getStorageID());
    }

    protected void requestSave() {
        SAVEDELAYER.resetAndStart();
    }

    /**
     * @param path2
     * @param configInterface2
     * @throws URISyntaxException
     */
    public StorageHandler(final String classPath, final Class<T> configInterface) throws URISyntaxException {
        this.configInterface = configInterface;
        this.relativCPPath = classPath;
        if (classPath.endsWith(".json") || classPath.endsWith(".ejs")) {
            org.appwork.loggingv3.LogV3.warning(classPath + " should not have an extension!!");
        }
        this.path = Application.getResource(classPath);
        preInit(path, configInterface);
        final File expected = Application.getResource("cfg/" + configInterface.getName());
        String storageID = null;
        if (!this.path.equals(expected)) {
            storageID = Files.getRelativePath(expected.getParentFile().getParentFile(), this.path);
            if (StringUtils.isEmpty(storageID)) {
                storageID = this.path.getAbsolutePath();
            }
        }
        this.storageID = storageID;
        this.primitiveStorage = StorageHandler.createPrimitiveStorage(Application.getResource(classPath), classPath, configInterface);
        final CryptedStorage cryptedStorage = configInterface.getAnnotation(CryptedStorage.class);
        if (cryptedStorage != null) {
            this.validateKeys(cryptedStorage);
        }
        try {
            org.appwork.loggingv3.LogV3.finer("Init StorageHandler for Interface:" + configInterface.getName() + "|Path:" + this.path);
            this.parseInterface();
        } catch (final Throwable e) {
            throw new InterfaceParseException(e);
        }
        this.addStorageHandler(this, configInterface.getName(), getStorageID());
    }

    protected void addStorageHandler(final StorageHandler<? extends ConfigInterface> storageHandler, final String interfaceName, final String storage) {
        synchronized (StorageHandler.STORAGEMAP) {
            final StorageHandler<?> existing = StorageHandler.getStorageHandler(interfaceName, storage);
            if (existing != null && existing != storageHandler) {
                throw new IllegalStateException("You cannot init the configinterface " + getConfigInterface() + " twice");
            }
            final String ID = interfaceName + "." + storage;
            StorageHandler.STORAGEMAP.put(storageHandler, ID);
        }
    }

    /**
     * @param _getStorageHandler
     */
    public static void clearFromCache(StorageHandler<?> handler) {
        synchronized (StorageHandler.STORAGEMAP) {
            final String ID = handler.getConfigInterface().getName() + "." + handler.getStorageID();
            final Iterator<Entry<StorageHandler<?>, String>> it = StorageHandler.STORAGEMAP.entrySet().iterator();
            while (it.hasNext()) {
                final Entry<StorageHandler<?>, String> next = it.next();
                final StorageHandler<?> ret;
                if (ID.equals(next.getValue()) && (ret = next.getKey()) != null) {
                    it.remove();
                }
            }
        }
    }

    public Object runDefaultValueFactory(KeyHandler<?> handler, Object o) {
        final DefaultFactoryInterface df = defaultFactory;
        if (df == null) {
            return o;
        } else {
            return df.getDefaultValue(handler, o);
        }
    }

    /**
     * @param key2
     * @param genericReturnType
     * @return
     */
    protected KeyHandler<?> createKeyHandler(final String key, final Type type) {
        if (Clazz.isBoolean(type)) {
            return new BooleanKeyHandler(this, key);
        } else if (Clazz.isByte(type)) {
            return new ByteKeyHandler(this, key);
        } else if (Clazz.isDouble(type)) {
            return new DoubleKeyHandler(this, key);
        } else if (Clazz.isFloat(type)) {
            return new FloatKeyHandler(this, key);
        } else if (Clazz.isInteger(type)) {
            return new IntegerKeyHandler(this, key);
        } else if (Clazz.isShort(type)) {
            return new ShortKeyHandler(this, key);
        } else if (type instanceof Class && ((Class<?>) type).isEnum()) {
            return new EnumKeyHandler(this, key);
        } else if (type == String.class) {
            return new StringKeyHandler(this, key);
        } else if (Clazz.isLong(type)) {
            return new LongKeyHandler(this, key);
        } else if (type instanceof Class && ((Class<?>) type).isArray()) {
            final Class<?> ct = ((Class<?>) type).getComponentType();
            final boolean p = ct.isPrimitive();
            if (Clazz.isNumberType(ct)) {
                if (Clazz.isFloatingPointNumber(ct)) {
                    if (Clazz.isDouble(ct)) {
                        if (p) {
                            return new ListHandler<double[]>(this, key, type, DefaultDoubleArrayValue.class);
                        } else {
                            return new ListHandler<Double[]>(this, key, type, DefaultDoubleArrayValue.class);
                        }
                    } else if (Clazz.isFloat(ct)) {
                        if (p) {
                            return new ListHandler<float[]>(this, key, type, DefaultFloatArrayValue.class);
                        } else {
                            return new ListHandler<Float[]>(this, key, type, DefaultFloatArrayValue.class);
                        }
                    }
                } else {
                    if (Clazz.isByte(ct)) {
                        if (p) {
                            return new ListHandler<byte[]>(this, key, type, DefaultByteArrayValue.class);
                        } else {
                            return new ListHandler<Byte[]>(this, key, type, DefaultByteArrayValue.class);
                        }
                    } else if (Clazz.isShort(ct)) {
                        if (p) {
                            return new ListHandler<short[]>(this, key, type, DefaultShortArrayValue.class);
                        } else {
                            return new ListHandler<Short[]>(this, key, type, DefaultShortArrayValue.class);
                        }
                    } else if (Clazz.isInteger(ct)) {
                        if (p) {
                            return new ListHandler<int[]>(this, key, type, DefaultIntArrayValue.class);
                        } else {
                            return new ListHandler<Integer[]>(this, key, type, DefaultIntArrayValue.class);
                        }
                    } else if (Clazz.isLong(ct)) {
                        if (p) {
                            return new ListHandler<long[]>(this, key, type, DefaultLongArrayValue.class);
                        } else {
                            return new ListHandler<Long[]>(this, key, type, DefaultLongArrayValue.class);
                        }
                    }
                }
                // just in case Clazz.isNumberType or others are failing/missing some types
                return new ObjectKeyHandler(this, key, type);
            } else if (Clazz.isBoolean(ct)) {
                if (p) {
                    return new ListHandler<boolean[]>(this, key, type, DefaultBooleanArrayValue.class);
                } else {
                    return new ListHandler<Boolean[]>(this, key, type, DefaultBooleanArrayValue.class);
                }
            } else if (Clazz.isString(ct)) {
                return new StringListHandler(this, key, type);
            } else if (Clazz.isEnum(ct)) {
                return new EnumListHandler(this, key, type);
            } else {
                return new ObjectKeyHandler(this, key, type);
            }
        } else {
            return new ObjectKeyHandler(this, key, type);
        }
    }

    /**
     * @param e
     */
    protected void error(final Throwable e) {
        LogV3.defaultLogger().log(e);
    }

    protected void fireEvent(final ConfigEvent.Types type, final KeyHandler<?> keyHandler, final Object parameter) {
        if (this.hasEventListener()) {
            this.getEventSender().fireEvent(new ConfigEvent(type, keyHandler, parameter));
        }
    }

    public Class<T> getConfigInterface() {
        return this.configInterface;
    }

    public synchronized ConfigEventSender<Object> getEventSender() {
        if (this.eventSender == null) {
            this.eventSender = new ConfigEventSender<Object>();
        }
        return this.eventSender;
    }

    /**
     * @param key2
     */
    @SuppressWarnings("unchecked")
    public KeyHandler<Object> getKeyHandler(final String key) {
        return this.getKeyHandler(key, KeyHandler.class);
    }

    /**
     * @param <RawClass>
     * @param string
     * @param class1
     * @return
     */
    @SuppressWarnings("unchecked")
    public <E extends KeyHandler<?>> E getKeyHandler(final String key, final Class<E> class1) {
        final String keyHandlerKey = key.toLowerCase(Locale.ENGLISH);
        final KeyHandler<?> ret = key2KeyHandlerMap.get(keyHandlerKey);
        if (ret != null) {
            return (E) ret;
        }
        try {
            throw new NullPointerException("No KeyHandler: " + key + " in " + getConfigInterface());
        } catch (NullPointerException e) {
            if (DebugMode.throwInIDEElse(e)) {
                LogV3.log(e);
            }
            return null;
        }
    }

    /**
     * @return
     */
    public File getPath() {
        return this.path;
    }

    /**
     * @param keyHandler
     * @return
     */
    public Object getPrimitive(final KeyHandler<?> keyHandler) {
        // only evaluate defaults of required
        final Storage storage = getPrimitiveStorage(keyHandler);
        if (storage != null && storage.hasProperty(keyHandler.getKey())) {
            if (Clazz.isBoolean(keyHandler.getRawClass())) {
                return this.getPrimitive(keyHandler, false);
            } else if (Clazz.isLong(keyHandler.getRawClass())) {
                return this.getPrimitive(keyHandler, 0l);
            } else if (Clazz.isInteger(keyHandler.getRawClass())) {
                return this.getPrimitive(keyHandler, 0);
            } else if (Clazz.isShort(keyHandler.getRawClass())) {
                return this.getPrimitive(keyHandler, 0);
            } else if (Clazz.isFloat(keyHandler.getRawClass())) {
                return this.getPrimitive(keyHandler, 0.0f);
            } else if (Clazz.isDouble(keyHandler.getRawClass())) {
                return this.getPrimitive(keyHandler, 0.0d);
            } else if (Clazz.isByte(keyHandler.getRawClass())) {
                return this.getPrimitive(keyHandler, (byte) 0);
            } else if (keyHandler.getRawClass() == String.class) {
                return this.getPrimitive(keyHandler, (String) null);
                // } else if (getter.getRawClass() == String[].class) {
                // return this.get(getter.getKey(),
                // getter.getDefaultStringArray());
            } else if (keyHandler.getRawClass().isEnum()) {
                return this.getPrimitive(keyHandler, keyHandler.getDefaultValue());
            } else {
                throw new StorageException("Invalid datatype: " + keyHandler.getRawClass());
            }
        } else {
            if (Clazz.isBoolean(keyHandler.getRawClass())) {
                return this.getPrimitive(keyHandler, keyHandler.getDefaultValue());
            } else if (Clazz.isLong(keyHandler.getRawClass())) {
                return this.getPrimitive(keyHandler, keyHandler.getDefaultValue());
            } else if (Clazz.isInteger(keyHandler.getRawClass())) {
                return this.getPrimitive(keyHandler, keyHandler.getDefaultValue());
            } else if (Clazz.isShort(keyHandler.getRawClass())) {
                return this.getPrimitive(keyHandler, keyHandler.getDefaultValue());
            } else if (Clazz.isFloat(keyHandler.getRawClass())) {
                return this.getPrimitive(keyHandler, keyHandler.getDefaultValue());
            } else if (Clazz.isDouble(keyHandler.getRawClass())) {
                return this.getPrimitive(keyHandler, keyHandler.getDefaultValue());
            } else if (Clazz.isByte(keyHandler.getRawClass())) {
                return this.getPrimitive(keyHandler, keyHandler.getDefaultValue());
            } else if (keyHandler.getRawClass() == String.class) {
                return this.getPrimitive(keyHandler, keyHandler.getDefaultValue());
                // } else if (getter.getRawClass() == String[].class) {
                // return this.get(getter.getKey(),
                // getter.getDefaultStringArray());
            } else if (keyHandler.getRawClass().isEnum()) {
                return this.getPrimitive(keyHandler, keyHandler.getDefaultValue());
            } else {
                throw new StorageException("Invalid datatype: " + keyHandler.getRawClass());
            }
        }
    }

    protected byte[] toJson(final ListHandler<?> keyHandler, final Object object) {
        return MAPPER.objectToByteArray(object);
    }

    protected void writeObject(final ListHandler<?> keyHandler, final Object object) {
        final byte[] jsonBytes = toJson(keyHandler, object);
        final byte[] cryptKey = keyHandler.getCryptKey();
        final File path = keyHandler.getPath();
        final Runnable run = new Runnable() {
            @Override
            public void run() {
                JSonStorage.saveTo(path, cryptKey == null, cryptKey, jsonBytes, keyHandler.getStorageSyncMode());
            }
        };
        StorageHandler.enqueueWrite(run, path.getAbsolutePath(), isDelayedWriteAllowed(keyHandler));
    }

    protected Object readObject(final ListHandler<?> keyHandler, final AtomicBoolean readFlag) {
        return null;
    }

    protected boolean isDelayedWriteAllowed(final KeyHandler<?> keyHandler) {
        return keyHandler.isDelayedWriteAllowed();
    }

    public <E> E getPrimitive(final KeyHandler<?> keyHandler, final E def) {
        final Storage storage = getPrimitiveStorage(keyHandler);
        return storage != null ? storage.get(keyHandler.getKey(), def) : def;
    }

    public Storage getPrimitiveStorage(final KeyHandler<?> keyHandler) {
        return getPrimitiveStorage();
    }

    public Storage getPrimitiveStorage() {
        return this.primitiveStorage;
    }

    public String getRelativCPPath() {
        return this.relativCPPath;
    }

    public String getStorageID() {
        return this.storageID;
    }

    /**
     * @throws NullPointerException
     *             if there is no keyhandler for key
     * @param string
     * @return
     */
    public Object getValue(final String key) {
        return this.getKeyHandler(key).getValue();
    }

    public WriteStrategy getWriteStrategy() {
        return this.writeStrategy;
    }

    public synchronized boolean hasEventListener() {
        return this.eventSender != null && this.eventSender.hasListener();
    }

    @SuppressWarnings("unchecked")
    public Object invoke(final Object instance, final Method m, final Object[] parameter) throws Throwable {
        if (m != null) {
            final long t = StorageHandler.PROFILER_MAP == null ? 0 : System.nanoTime();
            try {
                final KeyHandler<?> handler = this.method2KeyHandlerMap.get(m);
                if (handler != null) {
                    if (handler.isGetter(m)) {
                        final Object ret = handler.getValue();
                        if (ret instanceof Number) {
                            return ReflectionUtils.castNumber((Number) ret, handler.getRawClass());
                        } else {
                            return ret;
                        }
                    } else {
                        ((KeyHandler<Object>) handler).setValue(parameter[0]);
                        final WriteStrategy writeStrategy = getWriteStrategy();
                        if (writeStrategy != null) {
                            writeStrategy.write(this, handler);
                        }
                        return null;
                    }
                } else if (m.getName().equals("toString")) {
                    return this.toString();
                    // } else if (m.getName().equals("addListener")) {
                    // this.eventSender.addListener((ConfigEventListener)
                    // parameter[0]);
                    // return null;
                    // } else if (m.getName().equals("removeListener")) {
                    // this.eventSender.removeListener((ConfigEventListener)
                    // parameter[0]);
                    // return null;
                } else if (m.getName().equals("_getStorageHandler")) {
                    return this;
                } else if (m.getDeclaringClass() == Object.class) {
                    return m.invoke(this, parameter);
                } else {
                    throw new WTFException(m + " ??? no keyhandler. This is not possible!");
                }
            } finally {
                if (StorageHandler.PROFILER_MAP != null && m != null) {
                    final long dur = System.nanoTime() - t;
                    final String id = m.toString();
                    Long g = StorageHandler.PROFILER_MAP.get(id);
                    if (g == null) {
                        g = 0l;
                    }
                    StorageHandler.PROFILER_MAP.put(id, g + dur);
                }
                if (StorageHandler.PROFILER_CALLNUM_MAP != null && m != null) {
                    final String id = m.toString();
                    final Long g = StorageHandler.PROFILER_CALLNUM_MAP.get(id);
                    StorageHandler.PROFILER_CALLNUM_MAP.put(id, g == null ? 1 : g + 1);
                }
            }
        } else {
            // yes.... Method m may be null. this happens if we call a
            // method in the interface's own static init.
            return this;
        }
    }

    /**
     * @return
     */
    public boolean isObjectCacheEnabled() {
        return this.objectCacheEnabled;
    }

    public boolean isSaveInShutdownHookEnabled() {
        return this.saveInShutdownHookEnabled;
    }

    protected int getParameterCount(final Method method) {
        if (method != null) {
            return method.getParameterTypes().length;
        }
        return 0;
    }

    /**
     * @throws Throwable
     *
     */
    protected void parseInterface() throws Throwable {
        final HashMap<String, Method> keyGetterMap = new HashMap<String, Method>();
        final HashMap<String, Method> keySetterMap = new HashMap<String, Method>();
        String key;
        final HashMap<String, KeyHandler<?>> parseMap = new HashMap<String, KeyHandler<?>>();
        HashSet<Class<?>> dupes = new HashSet<Class<?>>();
        ArrayList<Class<?>> classes = findInterfaces(getConfigInterface(), dupes);
        for (Class<?> clazz : classes) {
            for (final Method m : clazz.getDeclaredMethods()) {
                final String methodName = m.getName().toLowerCase(Locale.ENGLISH);
                if (methodName.startsWith("get")) {
                    key = methodName.substring(3);
                    // we do not allow to setters/getters with the same name but
                    // different cases. this only confuses the user when editing
                    // the
                    // later config file
                    if (keyGetterMap.containsKey(key)) {
                        if (m.getName().equals(keyGetterMap.get(key).getName()) && getParameterCount(m) == getParameterCount(keyGetterMap.get(key))) {
                            // overridden method. that's ok
                            LogV3.info("Overridden Config Key found " + keyGetterMap.get(key) + "<-->" + m);
                            continue;
                        }
                        this.error(new InterfaceParseException("Key " + key + " Dupe found! " + keyGetterMap.get(key) + "<-->" + m));
                        continue;
                    }
                    keyGetterMap.put(key, m);
                    if (getParameterCount(m) > 0) {
                        this.error(new InterfaceParseException("Getter " + m + " has parameters."));
                        keyGetterMap.remove(key);
                        continue;
                    }
                    KeyHandler<?> kh = parseMap.get(key);
                    if (kh == null) {
                        kh = this.createKeyHandler(key, m.getGenericReturnType());
                        parseMap.put(key, kh);
                    }
                    kh.setGetMethod(m);
                    addKeyHandler(kh);
                } else if (methodName.startsWith("is")) {
                    key = methodName.substring(2);
                    // we do not allow to setters/getters with the same name but
                    // different cases. this only confuses the user when editing
                    // the
                    // later config file
                    if (keyGetterMap.containsKey(key)) {
                        if (m.getName().equals(keyGetterMap.get(key).getName()) && getParameterCount(m) == getParameterCount(keyGetterMap.get(key))) {
                            // overridden method. that's ok
                            LogV3.info("Overridden Config Key found " + keyGetterMap.get(key) + "<-->" + m);
                            continue;
                        }
                        this.error(new InterfaceParseException("Key " + key + " Dupe found! " + keyGetterMap.get(key) + "<-->" + m));
                        continue;
                    }
                    keyGetterMap.put(key, m);
                    if (getParameterCount(m) > 0) {
                        this.error(new InterfaceParseException("Getter " + m + " has parameters."));
                        keyGetterMap.remove(key);
                        continue;
                    }
                    KeyHandler<?> kh = parseMap.get(key);
                    if (kh == null) {
                        kh = this.createKeyHandler(key, m.getGenericReturnType());
                        parseMap.put(key, kh);
                    }
                    kh.setGetMethod(m);
                    addKeyHandler(kh);
                } else if (methodName.startsWith("set")) {
                    key = methodName.substring(3);
                    if (keySetterMap.containsKey(key)) {
                        Method other = keySetterMap.get(key);
                        if (m.getName().equals(keySetterMap.get(key).getName()) && getParameterCount(m) == getParameterCount(keySetterMap.get(key))) {
                            // overridden method. that's ok
                            LogV3.info("Overridden Config Key found " + keySetterMap.get(key) + "<-->" + m);
                            continue;
                        }
                        this.error(new InterfaceParseException("Key " + key + " Dupe found! " + keySetterMap.get(key) + "<-->" + m));
                        continue;
                    }
                    keySetterMap.put(key, m);
                    if (getParameterCount(m) != 1) {
                        this.error(new InterfaceParseException("Setter " + m + " has !=1 parameters."));
                        keySetterMap.remove(key);
                        continue;
                    }
                    if (m.getReturnType() != void.class) {
                        this.error(new InterfaceParseException("Setter " + m + " has a returntype != void"));
                        keySetterMap.remove(key);
                        continue;
                    }
                    KeyHandler<?> kh = parseMap.get(key);
                    if (kh == null) {
                        kh = this.createKeyHandler(key, m.getGenericParameterTypes()[0]);
                        parseMap.put(key, kh);
                    }
                    kh.setSetMethod(m);
                    addKeyHandler(kh);
                } else {
                    this.error(new InterfaceParseException("Only getter and setter allowed:" + m));
                    continue;
                }
            }
        }
        final ArrayList<KeyHandler<?>> keyHandlerToRemove = new ArrayList<KeyHandler<?>>();
        for (final KeyHandler<?> kh : key2KeyHandlerMap.values()) {
            try {
                kh.init();
            } catch (final Throwable e) {
                this.error(e);
                keyHandlerToRemove.add(kh);
            }
        }
        for (final KeyHandler<?> kh : keyHandlerToRemove) {
            removeKeyHandler(kh);
        }
    }

    /**
     * search all interfaces. avoid dupe loops and filter ConfigInterface and maybe other internal interfaces
     *
     * @param clazz
     * @param dupes
     * @return
     */
    protected ArrayList<Class<?>> findInterfaces(Class<?> clazz, HashSet<Class<?>> dupes) {
        final ArrayList<Class<?>> ret = new ArrayList<Class<?>>();
        if (clazz == ConfigInterface.class) {
            return ret;
        } else if (!dupes.add(clazz)) {
            return ret;
        } else {
            ret.add(clazz);
            final Class<?>[] interfaces = clazz.getInterfaces();
            if (interfaces != null) {
                for (Class<?> i : interfaces) {
                    ret.addAll(findInterfaces(i, dupes));
                }
            }
            return ret;
        }
    }

    public List<KeyHandler<?>> getKeyHandler() {
        return new ArrayList<KeyHandler<?>>(key2KeyHandlerMap.values());
    }

    private void removeKeyHandler(KeyHandler<?> keyHandler) {
        if (this.key2KeyHandlerMap.remove(keyHandler.getKey()) != null) {
            final Method getMethod = keyHandler.getGetMethod();
            if (getMethod != null) {
                this.method2KeyHandlerMap.remove(getMethod);
            }
            final Method setMethod = keyHandler.getSetMethod();
            if (setMethod != null) {
                this.method2KeyHandlerMap.remove(setMethod);
            }
        }
    }

    private void addKeyHandler(KeyHandler<?> keyHandler) {
        final Method getMethod = keyHandler.getGetMethod();
        if (getMethod != null) {
            this.method2KeyHandlerMap.put(getMethod, keyHandler);
        }
        final Method setMethod = keyHandler.getSetMethod();
        if (setMethod != null) {
            this.method2KeyHandlerMap.put(setMethod, keyHandler);
        }
        this.key2KeyHandlerMap.put(keyHandler.getKey(), keyHandler);
    }

    public void setObjectCacheEnabled(final boolean objectCacheEnabled) {
        this.objectCacheEnabled = objectCacheEnabled;
    }

    public void setSaveInShutdownHookEnabled(final boolean saveInShutdownHookEnabled) {
        this.saveInShutdownHookEnabled = saveInShutdownHookEnabled;
    }

    public void setWriteStrategy(final WriteStrategy writeStrategy) {
        this.writeStrategy = writeStrategy;
    }

    @Override
    public String toString() {
        final HashMap<String, Object> ret = new HashMap<String, Object>();
        for (final KeyHandler<?> h : key2KeyHandlerMap.values()) {
            try {
                ret.put(h.getKey(), this.invoke(null, h.getGetMethod(), new Object[] {}));
            } catch (final Throwable e) {
                e.printStackTrace();
                ret.put(h.getKey(), e.getMessage());
            }
        }
        return JSonStorage.toString(ret);
    }

    protected void validateKeys(final CryptedStorage crypted) {
    }

    /**
     * Writes the primitive storage
     */
    public void write() {
        final Storage storage = getPrimitiveStorage();
        if (storage != null) {
            storage.save();
        }
    }

    /**
     * @param key2
     */
    /**
     * @param b
     */
    public void setAllowWriteDefaultObjects(boolean b) {
        for (KeyHandler<?> kh : getKeyHandler()) {
            kh.setAllowWriteDefaultObjects(b);
        }
    }
}
