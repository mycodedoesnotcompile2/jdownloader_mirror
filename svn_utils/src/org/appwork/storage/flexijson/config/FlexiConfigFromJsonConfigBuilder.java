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
import java.io.FileFilter;
import java.io.IOException;
import java.net.URL;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.FlexiJSonObject;
import org.appwork.storage.flexijson.FlexiParserException;
import org.appwork.storage.flexijson.KeyValueElement;
import org.appwork.storage.flexijson.mapper.FlexiJSonMapper;
import org.appwork.storage.flexijson.mapper.FlexiMapperException;
import org.appwork.storage.flexijson.mapper.interfacestorage.FlexiStorableInterface;
import org.appwork.storage.flexijson.mapper.interfacestorage.InterfaceStorage;
import org.appwork.storage.simplejson.mapper.ClassCache;
import org.appwork.storage.simplejson.mapper.Property;
import org.appwork.utils.Application;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Exceptions;
import org.appwork.utils.Files;
import org.appwork.utils.IO;
import org.appwork.utils.StringUtils;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.reflection.CompiledType;

/**
 * @author thomas
 * @date 10.03.2022
 *
 */
public class FlexiConfigFromJsonConfigBuilder<T> extends FlexiConfigBuilder<T> {
    public FlexiConfigFromJsonConfigBuilder(Class<T> class1, File target) {
        super(class1, target, new File[0]);
    }

    public FlexiConfigFromJsonConfigBuilder(Class<T> class1, File target, File... fallbacks) {
        super(class1, target, fallbacks);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.config.FlexiConfigBuilder#setAutoWriteEnabled(boolean)
     */
    @Override
    public FlexiConfigFromJsonConfigBuilder<T> setAutoWriteEnabled(boolean autoWriteEnabled) {
        return (FlexiConfigFromJsonConfigBuilder<T>) super.setAutoWriteEnabled(autoWriteEnabled);
    }

    public static class ConfigBridgeInvocationHandler extends InterfaceStorage {
        public ConfigBridgeInvocationHandler(FlexiJSonMapper mapper, CompiledType cType, FlexiJSonObject obj, FlexiConfigBuilder<?> flexiConfigBuilder) throws SecurityException, NoSuchMethodException {
            super(mapper, cType, obj);
        }

        @Override
        protected KeyValueElement getElementByKey(final String key) {
            final KeyValueElement ret = super.getElementByKey(key);
            if (ret == null) {
                for (final KeyValueElement e : getBackendNode().getElements()) {
                    if (StringUtils.equalsIgnoreCase(e.getKey(), key)) {
                        return e;
                    }
                }
            }
            return ret;
        }
    }

    public static FlexiConfigFromJsonConfigBuilder<?> get(FlexiStorableInterface storage) {
        InterfaceStorage handler = getStorage(storage);
        if (handler == null) {
            return null;
        }
        for (Object l : handler.getEventSender().getListener()) {
            if (l instanceof FlexiConfigFromJsonConfigBuilder) {
                return ((FlexiConfigFromJsonConfigBuilder) l);
            }
        }
        return null;
    }

    protected ConfigBridgeInvocationHandler createRootInterfaceStorageHandler(FlexiJSonMapper mapper, CompiledType cType, FlexiJSonObject obj) throws NoSuchMethodException {
        return new ConfigBridgeInvocationHandler(mapper, cType, obj, this);
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * org.appwork.storage.flexijson.config.FlexiConfigBuilder#setReadSources(org.appwork.storage.flexijson.config.FlexiConfigBuilder.Source
     * [])
     */
    @Override
    public FlexiConfigFromJsonConfigBuilder<T> setReadSources(Source... readSources) {
        return (FlexiConfigFromJsonConfigBuilder<T>) super.setReadSources(readSources);
    }

    /**
     * @param resource
     * @param source
     * @return
     * @throws IOException
     * @throws FlexiParserException
     */
    protected FlexiJSonObject resourceToObject(File resource, Source source) throws IOException, FlexiParserException {
        try {
            ClassCache classCache = ClassCache.getClassCache(targetInterface);
            Set<String> keys = classCache.getKeys();
            FlexiJSonObject node = (FlexiJSonObject) readSubNode(resource, null, source);
            if (node == null) {
                node = new FlexiJSonObject();
            }
            ;
            for (String key : keys) {
                Property p = classCache.getProperty(key);
                if (p != null && p.type.isContainer()) {
                    FlexiJSonNode subNode = readSubNode(resource, key, source);
                    if (subNode != null) {
                        node.add(new KeyValueElement(node, key, subNode));
                    }
                }
            }
            return node;
        } catch (SecurityException e1) {
            throw new WTFException(e1);
        } catch (NoSuchMethodException e1) {
            throw new WTFException(e1);
        }
    }

    /**
     * @param resource
     * @param key
     * @param source
     * @return
     * @throws IOException
     * @throws FlexiParserException
     */
    private FlexiJSonNode readSubNode(File resource, String key, Source source) throws IOException, FlexiParserException {
        switch (source) {
        case CLASSPATH_ROOT:
            String rel = Files.getRelativePath(Application.getResource(""), resource);
            String path;
            if (key == null) {
                path = rel + ".json";
            } else {
                path = rel + "." + key + ".json";
            }
            URL url = targetInterface.getClassLoader().getResource(path);
            if (url != null) {
                final String json = IO.readURLToString(url);
                return createParser(json).parse();
            }
        case CLASSPATH_RELATIVE:
            rel = Files.getRelativePath(Application.getResource(""), resource);
            if (key == null) {
                path = rel + ".json";
            } else {
                path = rel + "." + key + ".json";
            }
            url = targetInterface.getResource(path);
            if (url != null) {
                final String json = IO.readURLToString(url);
                return createParser(json).parse();
            }
            break;
        case DISK:
            File file = key == null ? new File(resource.getAbsolutePath() + ".json") : new File(resource.getAbsolutePath() + "." + key + ".json");
            if (file.isFile()) {
                try {
                    final String json = IO.readFileToString(file);
                    return createParser(json).parse();
                } catch (FlexiParserException e) {
                    throw Exceptions.addSuppressed(e, new Exception("Related File: " + file));
                }
            }
            break;
        default:
            DebugMode.throwInIDEElse(new WTFException("All option should be handled here"));
        }
        return null;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.config.FlexiConfigBuilder#writeToDisk()
     */
    @Override
    public void writeToDisk() throws FlexiMapperException, IOException {
        super.writeToDisk();
        if (getSource() == Source.DISK) {
            deleteByResource(targetPath);
            if (isDeleteFallbacksOnWrite() && fallbacks != null) {
                for (File targetPath : fallbacks) {
                    deleteByResource(targetPath);
                }
            }
        }
    }

    private void deleteByResource(File targetPath) {
        // clean up old JSONConfig files
        final File resource = new File(targetPath.getAbsolutePath().replaceAll((CrossSystem.isWindows() ? "(?i)" : "") + "\\.json$", ""));
        final File root = resource.getParentFile();
        final File[] files = root.listFiles(new FileFilter() {
            @Override
            public boolean accept(final File file) {
                final String rel = Files.getRelativePath(root, file);
                if (StringUtils.startsWithCaseInsensitive(rel, resource.getName() + ".")) {
                    return true;
                }
                return false;
            }
        });
        for (File file : files) {
            if (file.equals(this.targetPath)) {
                continue;
            }
            final Pattern pattern = Pattern.compile("^" + Pattern.quote(resource.getName()) + (CrossSystem.isWindows() ? "(?i)" : "") + "\\.(.*?)\\.?json$", Pattern.CASE_INSENSITIVE);
            final String rel = Files.getRelativePath(root, file);
            final Matcher matcher = pattern.matcher(rel);
            if (matcher.find()) {
                String key = matcher.group(1);
                LogV3.info("Delete deprecated/fallback config file: " + file);
                file.delete();
            }
        }
    }
}
