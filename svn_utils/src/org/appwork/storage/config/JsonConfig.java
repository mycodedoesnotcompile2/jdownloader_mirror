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
package org.appwork.storage.config;

import java.io.File;
import java.lang.reflect.Proxy;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicInteger;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.storage.config.annotations.CustomStorageName;
import org.appwork.storage.config.annotations.StorageHandlerFactoryAnnotation;
import org.appwork.storage.config.handler.StorageHandler;
import org.appwork.utils.Application;

/**
 * @author thomas
 *
 */
public class JsonConfig {
    private static class LockObject {
        private final String        id;
        private final AtomicInteger lock           = new AtomicInteger(0);
        private StorageHandler      storageHandler = null;

        private LockObject(final String id) {
            this.id = id;
        }

        public String getId() {
            return this.id;
        }

        public AtomicInteger getLock() {
            return this.lock;
        }

        public StorageHandler getStorageHandler() {
            return this.storageHandler;
        }

        public void setStorageHandler(final StorageHandler storageHandler) {
            this.storageHandler = storageHandler;
        }
    }

    private static final HashMap<String, ConfigInterface> CACHE = new HashMap<String, ConfigInterface>();
    private static final HashMap<String, LockObject>      LOCKS = new HashMap<String, LockObject>();

    public static <T extends ConfigInterface> String getStorageName(Class<T> configInterface) {
        String id = configInterface.getName();
        CustomStorageName anno = configInterface.getAnnotation(CustomStorageName.class);
        if (anno != null) {
            id = anno.value();
        }
        return id;
    }

    /**
     * @param <T>
     * @param class1
     * @return
     */
    @SuppressWarnings("unchecked")
    public static <T extends ConfigInterface> T create(final Class<T> configInterface) {
        String path = getStorageName(configInterface);
        synchronized (JsonConfig.CACHE) {
            final ConfigInterface ret = JsonConfig.CACHE.get(path);
            if (ret != null) {
                return (T) ret;
            }
        }
        final LockObject lock = JsonConfig.requestLock(path);
        synchronized (lock) {
            try {
                synchronized (JsonConfig.CACHE) {
                    final ConfigInterface ret = JsonConfig.CACHE.get(path);
                    if (ret != null) {
                        return (T) ret;
                    }
                }
                final ClassLoader cl = configInterface.getClassLoader();
                if (lock.getStorageHandler() == null) {
                    final StorageHandlerFactoryAnnotation factoryClass = configInterface.getAnnotation(StorageHandlerFactoryAnnotation.class);
                    final File f = Application.getResource("cfg/" + path);
                    if (factoryClass != null) {
                        lock.setStorageHandler(((StorageHandlerFactory<T>) factoryClass.value().newInstance()).create(f, configInterface));
                    } else {
                        lock.setStorageHandler(new StorageHandler<T>(f, configInterface));
                    }
                }
                final T ret = (T) Proxy.newProxyInstance(cl, new Class<?>[] { configInterface }, lock.getStorageHandler());
                synchronized (JsonConfig.CACHE) {
                    if (lock.getLock().get() == 1) {
                        JsonConfig.CACHE.put(path, ret);
                    }
                }
                return ret;
            } catch (final Throwable e) {
                LogV3.defaultLogger().log(e);
                if (e instanceof RuntimeException) {
                    throw (RuntimeException) e;
                } else {
                    throw new WTFException(e);
                }
            } finally {
                JsonConfig.unLock(lock);
            }
        }
    }

    public static <T extends ConfigInterface> T put(String path, final Class<T> configInterface, StorageHandler<T> storageHandler) {
        final LockObject lock = JsonConfig.requestLock(path);
        synchronized (lock) {
            try {
                final ClassLoader cl = configInterface.getClassLoader();
                lock.setStorageHandler(storageHandler);
                final T ret = (T) Proxy.newProxyInstance(cl, new Class<?>[] { configInterface }, storageHandler);
                synchronized (JsonConfig.CACHE) {
                    if (lock.getLock().get() == 1) {
                        if (CACHE.containsKey(path)) {
                            throw new IllegalStateException("Path is already initialized: " + path);
                        } else {
                            JsonConfig.CACHE.put(path, ret);
                        }
                    }
                }
                return ret;
            } catch (final Throwable e) {
                LogV3.defaultLogger().log(e);
                if (e instanceof RuntimeException) {
                    throw (RuntimeException) e;
                } else {
                    throw new WTFException(e);
                }
            } finally {
                JsonConfig.unLock(lock);
            }
        }
    }

    @SuppressWarnings("unchecked")
    public static <T extends ConfigInterface> T create(final File path, final Class<T> configInterface) {
        String id = path.getAbsolutePath() + getStorageName(configInterface);
        synchronized (JsonConfig.CACHE) {
            final ConfigInterface ret = JsonConfig.CACHE.get(id);
            if (ret != null) {
                return (T) ret;
            }
        }
        final LockObject lock = JsonConfig.requestLock(id);
        synchronized (lock) {
            try {
                synchronized (JsonConfig.CACHE) {
                    final ConfigInterface ret = JsonConfig.CACHE.get(id);
                    if (ret != null) {
                        return (T) ret;
                    }
                }
                final ClassLoader cl = configInterface.getClassLoader();
                if (lock.getStorageHandler() == null) {
                    StorageHandlerFactoryAnnotation factoryClass = configInterface.getAnnotation(StorageHandlerFactoryAnnotation.class);
                    if (factoryClass != null) {
                        lock.setStorageHandler(((StorageHandlerFactory<T>) factoryClass.value().newInstance()).create(path, configInterface));
                    } else {
                        lock.setStorageHandler(new StorageHandler<T>(path, configInterface));
                    }
                }
                final T ret = (T) Proxy.newProxyInstance(cl, new Class<?>[] { configInterface }, lock.getStorageHandler());
                synchronized (JsonConfig.CACHE) {
                    if (lock.getLock().get() == 1) {
                        JsonConfig.CACHE.put(id, ret);
                    }
                }
                return ret;
            } catch (final Throwable e) {
                LogV3.defaultLogger().log(e);
                if (e instanceof RuntimeException) {
                    throw (RuntimeException) e;
                } else {
                    throw new WTFException(e);
                }
            } finally {
                JsonConfig.unLock(lock);
            }
        }
    }

    @SuppressWarnings("unchecked")
    public static <T extends ConfigInterface> T create(final String urlPath, final Class<T> configInterface) {
        final String id = urlPath + getStorageName(configInterface);
        synchronized (JsonConfig.CACHE) {
            final ConfigInterface ret = JsonConfig.CACHE.get(id);
            if (ret != null) {
                return (T) ret;
            }
        }
        final LockObject lock = JsonConfig.requestLock(id);
        synchronized (lock) {
            try {
                synchronized (JsonConfig.CACHE) {
                    final ConfigInterface ret = JsonConfig.CACHE.get(id);
                    if (ret != null) {
                        return (T) ret;
                    }
                }
                final ClassLoader cl = configInterface.getClassLoader();
                if (lock.getStorageHandler() == null) {
                    StorageHandlerFactoryAnnotation factoryClass = configInterface.getAnnotation(StorageHandlerFactoryAnnotation.class);
                    if (factoryClass != null) {
                        lock.setStorageHandler(((StorageHandlerFactory<T>) factoryClass.value().newInstance()).create(urlPath, configInterface));
                    } else {
                        lock.setStorageHandler(new StorageHandler<T>(urlPath, configInterface));
                    }
                }
                final T ret = (T) Proxy.newProxyInstance(cl, new Class<?>[] { configInterface }, lock.getStorageHandler());
                synchronized (JsonConfig.CACHE) {
                    if (lock.getLock().get() == 1) {
                        JsonConfig.CACHE.put(id, ret);
                    }
                }
                return ret;
            } catch (final Throwable e) {
                LogV3.defaultLogger().log(e);
                if (e instanceof RuntimeException) {
                    throw (RuntimeException) e;
                } else {
                    throw new WTFException(e);
                }
            } finally {
                JsonConfig.unLock(lock);
            }
        }
    }

    public static HashMap<String, ConfigInterface> getCache() {
        return JsonConfig.CACHE;
    }

    private static synchronized LockObject requestLock(final String id) {
        LockObject lockObject = JsonConfig.LOCKS.get(id);
        if (lockObject == null) {
            lockObject = new LockObject(id);
            JsonConfig.LOCKS.put(id, lockObject);
        }
        lockObject.getLock().incrementAndGet();
        return lockObject;
    }

    private static synchronized void unLock(final LockObject lock) {
        final LockObject lockObject = JsonConfig.LOCKS.get(lock.getId());
        if (lockObject != null) {
            if (lockObject.getLock().decrementAndGet() == 0) {
                JsonConfig.LOCKS.remove(lock.getId());
            }
        }
    }

    /**
     * @param config
     */
    public static void clearFromCache(ConfigInterface config) {
        synchronized (JsonConfig.CACHE) {
            final HashSet<String> removedKeys = new HashSet<String>();
            for (Entry<String, ConfigInterface> c : CACHE.entrySet()) {
                if (c.getValue() == config) {
                    removedKeys.add(c.getKey());
                }
            }
            final StorageHandler<?> storageHandler = config._getStorageHandler();
            if (storageHandler != null) {
                StorageHandler.clearFromCache(storageHandler);
            }
            for (String key : removedKeys) {
                CACHE.remove(key);
            }
        }
    }
    /**
     * @param string
     * @param storageHandler
     */
}
