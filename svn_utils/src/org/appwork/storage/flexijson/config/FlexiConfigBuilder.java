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
package org.appwork.storage.flexijson.config;

import java.io.File;
import java.io.IOException;
import java.lang.ref.WeakReference;
import java.lang.reflect.Proxy;
import java.net.URL;
import java.util.concurrent.atomic.AtomicBoolean;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.scheduler.DelayedRunnable;
import org.appwork.shutdown.ShutdownController;
import org.appwork.shutdown.ShutdownEvent;
import org.appwork.shutdown.ShutdownRequest;
import org.appwork.storage.SimpleTypeRef;
import org.appwork.storage.flexijson.FlexiJSONParser;
import org.appwork.storage.flexijson.FlexiJSonArray;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.FlexiJSonObject;
import org.appwork.storage.flexijson.FlexiParserException;
import org.appwork.storage.flexijson.mapper.FlexiJSonMapper;
import org.appwork.storage.flexijson.mapper.FlexiMapperException;
import org.appwork.storage.flexijson.mapper.interfacestorage.FlexiStorableInterface;
import org.appwork.storage.flexijson.mapper.interfacestorage.InterfaceStorage;
import org.appwork.storage.flexijson.mapper.interfacestorage.InterfaceStorageListener;
import org.appwork.storage.flexijson.stringify.FlexiJSonPrettyStringify;
import org.appwork.utils.Application;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Files;
import org.appwork.utils.IO;
import org.appwork.utils.IO.SYNC;
import org.appwork.utils.event.basic.CoreDelegate;
import org.appwork.utils.event.basic.CoreEventSender;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.reflection.CompiledType;

/**
 * @author thomas
 * @date 10.03.2022
 *
 */
public class FlexiConfigBuilder<T> implements InterfaceStorageListener<Object> {
    /**
     * @author thomas
     * @date 28.07.2022
     *
     */
    protected Source source;

    public Source getSource() {
        return source;
    }

    public static final AtomicBoolean WRITE_ON_SHUTDOWN = new AtomicBoolean(true);

    /**
     * @author thomas
     * @date 28.07.2022
     *
     */
    public static class ShutdownHook extends ShutdownEvent {
        private WeakReference<FlexiConfigBuilder> builder;

        /**
         * @param targetPath
         * @param storage
         * @param handler
         */
        public ShutdownHook(FlexiConfigBuilder builder) {
            this.builder = new WeakReference<FlexiConfigBuilder>(builder);
        }

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
            if (!WRITE_ON_SHUTDOWN.get()) {
                return;
            }
            FlexiConfigBuilder actualStorage = builder.get();
            if (actualStorage != null) {
                actualStorage.onShutdown();
            }
        }

        @Override
        public String toString() {
            return "ShutdownEvent: Save FlexiConfig.";
        }
    }

    public static enum Source {
        /**
         * Read files from disc
         */
        DISK,
        // /**
        // * Use the Fallback list to read files from olds filds
        // */
        // DISK_FALLBACKS,
        /**
         * read file from the classpath root.
         */
        CLASSPATH_ROOT,
        /**
         * read files from the classpath, but relative to the interface location
         */
        CLASSPATH_RELATIVE
    }

    protected Source[] readSources = Source.values();

    public Source[] getReadSources() {
        return readSources;
    }

    public FlexiConfigBuilder<T> setReadSources(Source... readSources) {
        this.readSources = readSources;
        return this;
    }

    protected final Class<T> targetInterface;
    protected final File     targetPath;

    public File getTargetPath() {
        return targetPath;
    }

    /**
     * Fallback paths to rename storage location and name
     */
    protected File[] fallbacks;

    public FlexiConfigBuilder(Class<T> class1, File target, File... fallbacks) {
        if (!(FlexiStorableInterface.class.isAssignableFrom(class1))) {
            LogV3.warning("WARNING: " + class1.getName() + " does not extend from " + FlexiStorableInterface.class.getName() + ". This is recommended");
        }
        if (!target.getName().endsWith(".json")) {
            throw new IllegalArgumentException(target + " must end with .json");
        }
        if (fallbacks != null) {
            for (File file : fallbacks) {
                if (!file.getName().endsWith(".json")) {
                    throw new IllegalArgumentException(file + " fallback must end with .json");
                }
            }
        }
        this.targetInterface = class1;
        this.targetPath = target;
        this.fallbacks = fallbacks;
    }

    private int delayedWriteIfNoChangeFor = 2000;

    /**
     * if there are many changes on the object, do not write immediately, but wait until there are not changes for at least
     * #delayedWriteIfNoChangeFor ms. Write anyway if this takes longer than {@link #delayedWriteInAnyCaseAfter}. Set
     * {@link #delayedWriteInAnyCaseAfter} to 0 to write immediately.
     */
    public int getDelayedWriteIfNoChangeFor() {
        return delayedWriteIfNoChangeFor;
    }

    public FlexiConfigBuilder<T> setDelayedWriteIfNoChangeFor(int delayedWriteIfNoChangeFor) {
        this.delayedWriteIfNoChangeFor = delayedWriteIfNoChangeFor;
        return this;
    }

    /**
     * if there are many changes on the object, do not write immediately, but wait until there are not changes for at least
     * #delayedWriteIfNoChangeFor ms. Write anyway if this takes longer than {@link #delayedWriteInAnyCaseAfter}. Set
     * {@link #delayedWriteInAnyCaseAfter} to 0 to write immediately.
     */
    public int getDelayedWriteInAnyCaseAfter() {
        return delayedWriteInAnyCaseAfter;
    }

    public FlexiConfigBuilder<T> setDelayedWriteInAnyCaseAfter(int delayedWriteInAnyCaseAfter) {
        this.delayedWriteInAnyCaseAfter = delayedWriteInAnyCaseAfter;
        return this;
    }

    private int delayedWriteInAnyCaseAfter = 10000;

    /**
     *
     */
    public void onShutdown() {
        if (isSaveOnShutdownEnabled() && isAutoWriteEnabled()) {
            try {
                writeToDisk();
            } catch (FlexiMapperException e) {
                onAutoWriteException(e);
            } catch (IOException e) {
                onAutoWriteException(e);
            }
        }
    }

    private boolean autoWriteEnabled = true;

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.mapper.interfacestorage.InterfaceStorageListener#onInterfaceValueSet(java.lang.Object,
     * java.lang.String, java.lang.Object, java.lang.Object)
     */
    public boolean isAutoWriteEnabled() {
        return autoWriteEnabled;
    }

    public FlexiConfigBuilder<T> setAutoWriteEnabled(boolean autoWriteEnabled) {
        this.autoWriteEnabled = autoWriteEnabled;
        return this;
    }

    private CoreEventSender<FlexiConfigListener> eventSender;
    private volatile boolean                     writeRequired;

    protected boolean isWriteRequired() {
        // Changes detection is not 100% accurate( we will not get notified if an internal object changed) - since we always write to disk
        // if requested (e.g. after a real change or on shutdown - overwrite this method, if you want to write only if an actual change has
        // been detected
        return true;
    }

    public CoreEventSender<FlexiConfigListener> getEventSender() {
        synchronized (this) {
            if (eventSender == null) {
                eventSender = new CoreEventSender<FlexiConfigListener>();
            }
            return eventSender;
        }
    }

    public void onInterfaceValueSet(final Object storage, final String key, final Object oldValue, final Object newValue) {
        // this callback gets called if any property is changed.
        // it is called as well, if any SUB-property got changed, but only if the subproperty is a InterfaceStorage as well - don't rely on
        // this!
        if (oldValue != null) {
            if (Proxy.isProxyClass(oldValue.getClass())) {
                // remove listeners from old Object
                InterfaceStorage<Object> handler = InterfaceStorage.get(oldValue);
                if (handler != null) {
                    handler.getEventSender().removeListener(this);
                }
            }
        }
        if (newValue != null) {
            if (Proxy.isProxyClass(newValue.getClass())) {
                InterfaceStorage<Object> handler = InterfaceStorage.get(newValue);
                if (handler != null) {
                    handler.getEventSender().addListener(this);
                }
            }
        }
        if (eventSender != null) {
            eventSender.fireEvent(new CoreDelegate<FlexiConfigListener>() {
                @Override
                protected void fireTo(FlexiConfigListener listener) {
                    listener.onValueModified(FlexiConfigBuilder.this, storage, key, oldValue, newValue);
                }
            });
        }
        writeRequired = true;
        if (!isAutoWriteEnabled()) {
            return;
        }
        if (getDelayedWriteInAnyCaseAfter() <= 0) {
            try {
                writeToDisk();
            } catch (FlexiMapperException e) {
                onAutoWriteException(e);
            } catch (IOException e) {
                onAutoWriteException(e);
            }
            return;
        } else {
        }
        if (delayedSave == null || delayedSave.getMaximumDelay() != getDelayedWriteInAnyCaseAfter() || delayedSave.getMinimumDelay() != getDelayedWriteIfNoChangeFor()) {
            delayedSave = new DelayedRunnable(getDelayedWriteIfNoChangeFor(), getDelayedWriteInAnyCaseAfter()) {
                @Override
                public void delayedrun() {
                    try {
                        writeToDisk();
                    } catch (FlexiMapperException e) {
                        onAutoWriteException(e);
                    } catch (IOException e) {
                        onAutoWriteException(e);
                    }
                }
            };
        }
        delayedSave.resetAndStart();
    }

    /**
     * called if writing the storage to disk failed
     *
     * @param e
     */
    protected void onAutoWriteException(Exception e) {
    }

    /**
     * if true,fallbacks will be removed once the actual storage file has been written
     */
    private boolean deleteFallbacksOnWrite = true;

    public boolean isDeleteFallbacksOnWrite() {
        return deleteFallbacksOnWrite;
    }

    public void setDeleteFallbacksOnWrite(boolean deleteFallbacksOnWrite) {
        this.deleteFallbacksOnWrite = deleteFallbacksOnWrite;
    }

    /**
     * @throws FlexiMapperException
     * @throws IOException
     *
     */
    public synchronized void writeToDisk() throws FlexiMapperException, IOException {
        if (!isWriteRequired()) {
            return;
        }
        updateNode();
        String json = toJSONString();
        LogV3.info("Store FlexiConfig to disk:" + targetPath);
        targetPath.getParentFile().mkdirs();
        IO.secureWrite(targetPath, json, SYNC.META_AND_DATA);
        if (getSource() == Source.DISK) {
            if (isDeleteFallbacksOnWrite() && fallbacks != null) {
                for (File fb : fallbacks) {
                    if (fb.isFile()) {
                        LogV3.info("Delete Fallback file after writing to keep:" + targetPath + " delete:" + fb);
                        fb.delete();
                    }
                }
            }
        }
    }

    public void merge(final FlexiJSonNode a, final FlexiJSonNode b) {
        if (b instanceof FlexiJSonArray) {
            if (!(a instanceof FlexiJSonArray)) {
                return;
            }
            if (((FlexiJSonArray) b).size() != ((FlexiJSonArray) a).size()) {
                return;
            }
            for (int i = 0; i < ((FlexiJSonArray) a).size(); i++) {
                merge(((FlexiJSonArray) a).get(i), ((FlexiJSonArray) b).get(i));
            }
        } else if (b instanceof FlexiJSonObject) {
            if (!(a instanceof FlexiJSonObject)) {
                return;
            }
            for (final String key : ((FlexiJSonObject) b).getKeys()) {
                if (!((FlexiJSonObject) a).containsKey(key)) {
                    ((FlexiJSonObject) a).add(((FlexiJSonObject) b).getElement(key));
                } else {
                    merge(((FlexiJSonObject) a).getNode(key), ((FlexiJSonObject) b).getNode(key));
                }
            }
        }
    }

    protected String toJSONString() throws FlexiMapperException {
        return new FlexiJSonPrettyStringify().toJSONString(getBackendNode());
    }

    /**
     * @return
     */
    protected FlexiJSonNode getBackendNode() {
        return InterfaceStorage.get(storage).getBackendNode();
    }

    protected void updateNode() throws FlexiMapperException {
        FlexiJSonObject newNode = (FlexiJSonObject) mapper.objectToJsonNode(storage);
        keepUnknownProperties(newNode);
        // update backend
        InterfaceStorage.get(storage).setBackendNode(newNode);
    }

    protected void keepUnknownProperties(FlexiJSonNode newNode) {
        if (isKeepUnknownProperties()) {
            final InterfaceStorage<?> handler = InterfaceStorage.get(storage);
            // old data
            final FlexiJSonObject jsonData = handler.getBackendNode();
            merge(newNode, jsonData);
        }
    }

    private boolean keepUnknownProperties = true;

    public boolean isKeepUnknownProperties() {
        return keepUnknownProperties;
    }

    public void setKeepUnknownProperties(boolean keepUnknownProperties) {
        this.keepUnknownProperties = keepUnknownProperties;
    }

    private boolean           saveOnShutdownEnabled = true;
    protected T               storage;
    private DelayedRunnable   delayedSave;
    protected FlexiJSonMapper mapper;

    public boolean isSaveOnShutdownEnabled() {
        return saveOnShutdownEnabled;
    }

    public FlexiConfigBuilder<T> setSaveOnShutdownEnabled(boolean saveOnShutdown) {
        this.saveOnShutdownEnabled = saveOnShutdown;
        return this;
    }

    /**
     * @param object
     * @return
     */
    public T getStorageOrDefault(T defaultValue) {
        try {
            return getStorage();
        } catch (Exception e) {
            LogV3.log(e);
            if (e instanceof InterruptedException) {
                Thread.currentThread().interrupt();
                // restore interrupt flag
            }
        }
        return defaultValue;
    }

    public T getStorage() throws FlexiParserException, IOException, FlexiMapperException {
        if (storage != null) {
            return storage;
        }
        FlexiJSonObject node = getFlexiObject();
        if (node == null) {
            node = new FlexiJSonObject();
        }
        mapper = createMapper(node);
        storage = mapper.jsonToObject(node, new SimpleTypeRef<T>(targetInterface));
        ShutdownController.getInstance().addShutdownEvent(new ShutdownHook(this));
        return storage;
    }

    protected FlexiJSonObject getFlexiObject() throws FlexiParserException, IOException {
        File file = new File(targetPath.getAbsolutePath().replaceAll((CrossSystem.isWindows() ? "(?i)" : "") + "\\.json$", ""));
        for (Source source : getReadSources()) {
            this.source = source;
            switch (source) {
            case CLASSPATH_ROOT:
                String path = Files.getRelativePath(Application.getResource(""), file);
                URL url = targetInterface.getClassLoader().getResource(path + ".json");
                if (url != null) {
                    return resourceToObject(file, source);
                }
                if (fallbacks != null) {
                    for (File f : fallbacks) {
                        path = Files.getRelativePath(Application.getResource(""), f).replaceAll((CrossSystem.isWindows() ? "(?i)" : "") + "\\.json$", "");
                        url = targetInterface.getClassLoader().getResource(path + ".json");
                        if (url != null) {
                            return resourceToObject(file, source);
                        }
                    }
                }
                break;
            case CLASSPATH_RELATIVE:
                path = Files.getRelativePath(Application.getResource(""), file);
                url = targetInterface.getResource(path + ".json");
                if (url != null) {
                    return resourceToObject(file, source);
                }
                if (fallbacks != null) {
                    for (File f : fallbacks) {
                        path = Files.getRelativePath(Application.getResource(""), f).replaceAll((CrossSystem.isWindows() ? "(?i)" : "") + "\\.json$", "");
                        url = targetInterface.getResource(path + ".json");
                        if (url != null) {
                            return resourceToObject(file, source);
                        }
                    }
                }
                break;
            case DISK:
                if (new File(file.getAbsolutePath() + ".json").isFile() || fallbacks == null) {
                    return resourceToObject(file, source);
                }
                if (fallbacks != null) {
                    for (File f : fallbacks) {
                        File fallback = new File(f.getAbsolutePath().replaceAll((CrossSystem.isWindows() ? "(?i)" : "") + "\\.json$", ""));
                        if (new File(fallback.getAbsolutePath() + ".json").isFile()) {
                            return resourceToObject(fallback, source);
                        }
                    }
                }
                break;
            default:
                DebugMode.throwInIDEElse(new WTFException("All option should be handled here"));
            }
        }
        return null;
    }

    /**
     * @param file
     * @param source2
     * @return
     * @throws IOException
     * @throws FlexiParserException
     */
    protected FlexiJSonObject resourceToObject(File resource, Source source) throws IOException, FlexiParserException {
        switch (source) {
        case CLASSPATH_ROOT:
            String rel = Files.getRelativePath(Application.getResource(""), resource);
            String path;
            path = rel + ".json";
            URL url = targetInterface.getClassLoader().getResource(path);
            if (url != null) {
                final String json = IO.readURLToString(url);
                return (FlexiJSonObject) createParser(json).parse();
            }
        case CLASSPATH_RELATIVE:
            rel = Files.getRelativePath(Application.getResource(""), resource);
            path = rel + ".json";
            url = targetInterface.getResource(path);
            if (url != null) {
                final String json = IO.readURLToString(url);
                return (FlexiJSonObject) createParser(json).parse();
            }
            break;
        case DISK:
            File file = new File(resource.getAbsolutePath() + ".json");
            if (file.isFile()) {
                final String json = IO.readFileToString(file);
                return (FlexiJSonObject) createParser(json).parse();
            }
            break;
        default:
            DebugMode.throwInIDEElse(new WTFException("All option should be handled here"));
        }
        return null;
    }

    public FlexiJSONParser createParser(final String json) {
        final FlexiJSONParser ret = new FlexiJSONParser(json);
        ret.enableComments();
        ret.addIgnoreIssues(FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES);
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            ret.setDebug(new StringBuilder());
        }
        return ret;
    }

    /**
     * @param root
     * @return
     */
    protected FlexiJSonMapper createMapper(final FlexiJSonObject root) {
        return new FlexiJSonMapper() {
            /*
             * (non-Javadoc)
             *
             * @see org.appwork.storage.flexijson.mapper.FlexiJSonMapper#createProxy(java.lang.Class, java.lang.reflect.Type,
             * org.appwork.storage.flexijson.FlexiJSonObject)
             */
            @Override
            protected Object createProxy(CompiledType cType, FlexiJSonObject obj) throws IllegalArgumentException, SecurityException, NoSuchMethodException {
                Object proxy = super.createProxy(cType, obj);
                InterfaceStorage.get(proxy).getEventSender().addListener(FlexiConfigBuilder.this);
                return proxy;
            }

            /*
             * (non-Javadoc)
             *
             * @see org.appwork.storage.flexijson.mapper.FlexiJSonMapper#createInterfaceInvocationHandler(java.lang.Class,
             * java.lang.reflect.Type, org.appwork.storage.flexijson.FlexiJSonObject)
             */
            @Override
            protected InterfaceStorage<Object> createInterfaceInvocationHandler(CompiledType type, FlexiJSonObject obj) throws SecurityException, NoSuchMethodException {
                return super.createInterfaceInvocationHandler(type, obj);
            }
        };
    }

    /**
     * @param storage2
     * @return
     */
    public static FlexiConfigBuilder<?> get(FlexiStorableInterface storage) {
        InterfaceStorage handler = getStorage(storage);
        if (handler == null) {
            return null;
        }
        for (Object l : handler.getEventSender().getListener()) {
            if (l instanceof FlexiConfigBuilder) {
                return ((FlexiConfigBuilder) l);
            }
        }
        return null;
    }

    public static InterfaceStorage getStorage(FlexiStorableInterface storage) {
        return InterfaceStorage.get(storage);
    }
}
