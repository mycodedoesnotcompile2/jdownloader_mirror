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
package org.appwork.storage.flexijson;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Proxy;
import java.util.HashSet;
import java.util.LinkedList;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.SimpleTypeRef;
import org.appwork.storage.TypeRef;
import org.appwork.storage.flexijson.mapper.FlexiJSonMapper;
import org.appwork.storage.flexijson.mapper.FlexiMapperException;
import org.appwork.storage.flexijson.mapper.interfacestorage.InterfaceStorage;
import org.appwork.storage.flexijson.mapper.mod.FlexiModifier;
import org.appwork.storage.flexijson.mapper.mod.JsonModification;
import org.appwork.storage.flexijson.mapper.mod.MergeException;
import org.appwork.storage.flexijson.stringify.FlexiJSonPrettyPrinterForConfig;
import org.appwork.storage.flexijson.stringify.FlexiJSonPrettyStringify;
import org.appwork.storage.flexijson.stringify.FlexiJSonStringBuilder;
import org.appwork.storage.flexijson.stringify.PropertyJSonPrettyStringify;
import org.appwork.utils.IO;
import org.appwork.utils.IO.SYNC;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.StringUtils;
import org.appwork.utils.reflection.Clazz;
import org.appwork.utils.reflection.CompiledType;

/**
 * @author thomas
 * @date 10.03.2022
 *
 */
public class FlexiUtils {
    /**
     *
     */
    /**
     * maps json to type, but ignores all "errors" if the parser is sure that the restored values are correct anyway. This method throws
     * exceptions anyway if there are severe syntax or mapping errors
     */
    public static <T> T jsonToObject(final String json, final TypeRef<T> type) throws FlexiParserException, FlexiMapperException {
        return jsonToObject(json, FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES, type);
    }

    public static <T> T jsonToObject(final String json, final HashSet<ParsingError> acceptableErrors, final TypeRef<T> type) throws FlexiParserException, FlexiMapperException {
        final FlexiJSONParser parser = new FlexiJSONParser(json);
        parser.setIgnoreIssues(acceptableErrors);
        return new FlexiJSonMapper().jsonToObject(parser.parse(), type);
    }

    /**
     * @param resource
     * @param ignoreListEnsureCorrectValues
     * @param class1
     * @return
     * @throws IOException
     * @throws FlexiMapperException
     * @throws FlexiParserException
     */
    public static <T> T jsonToObject(final File resource, final HashSet<ParsingError> ignoreListEnsureCorrectValues, final Class<T> class1) throws FlexiParserException, FlexiMapperException, IOException {
        return jsonToObject(resource, ignoreListEnsureCorrectValues, new SimpleTypeRef<T>(class1));
    }

    public static <T> T jsonToObject(final File resource, final HashSet<ParsingError> ignoreListEnsureCorrectValues, final TypeRef<T> type) throws FlexiParserException, FlexiMapperException, IOException {
        if (!resource.isFile()) {
            return null;
        }
        return new FlexiJSonMapper().jsonToObject(new FlexiJSONParser(IO.readFileToString(resource)).ignoreIssues(ignoreListEnsureCorrectValues).parse(), type);
    }

    /**
     * @param frontEndConfig
     * @return
     * @throws FlexiMapperException
     */
    public static String serializeToPrettyJson(final Object object) throws FlexiMapperException {
        final FlexiJSonPrettyStringify toString = new FlexiJSonPrettyStringify();
        if (object instanceof FlexiJSonNode) {
            return toString.toJSONString((FlexiJSonNode) object);
        }
        final FlexiJSonMapper mapper = new FlexiJSonMapper();
        mapper.setIgnoreDefaultValuesEnabled(false);
        final FlexiJSonNode node = mapper.objectToJsonNode(object);
        String newJson;
        newJson = toString.toJSONString(node);
        return newJson;
    }

    public static String serializeConfigStorable(final Object object) throws FlexiMapperException {
        return serializeConfigStorable(object, null);
    }

    /**
     * @param whitelist
     *            a startsWith whitelist for the path. . only whitelisted path will get serialized
     * @return
     * @throws FlexiMapperException
     */
    public static String serializeConfigStorable(final Object object, final NodeFilter filter) throws FlexiMapperException {
        final FlexiJSonPrettyStringify toString = new FlexiJSonPrettyPrinterForConfig(filter);
        final FlexiJSonMapper mapper = new FlexiJsonMapperForConfig();
        final FlexiJSonNode node = mapper.objectToJsonNode(object);
        String newJson;
        newJson = toString.toJSONString(node);
        return newJson;
    }

    /**
     * @param pretty
     *            TODO
     * @param metaInfo
     * @return
     * @throws FlexiMapperException
     */
    public static byte[] serializeToBytes(final Object data, boolean pretty) throws FlexiMapperException {
        FlexiJSonStringBuilder stringifier = pretty ? new FlexiJSonPrettyStringify() : new FlexiJSonStringBuilder();
        if (data instanceof FlexiJSonNode) {
            final ByteArrayOutputStream bout = new ByteArrayOutputStream();
            stringifier.toJSONString((FlexiJSonNode) data, bout, null);
            return bout.toByteArray();
        } else {
            final ByteArrayOutputStream bout = new ByteArrayOutputStream();
            stringifier.toJSONString(new FlexiJSonMapper().objectToJsonNode(data), bout, null);
            return bout.toByteArray();
        }
    }

    /**
     * @param json
     * @return
     * @throws InvalidPathException
     */
    public static String getPathString(final FlexiJSonNode json) throws InvalidPathException {
        return JSPath.fromFlexiNode(json).toPathString(true);
    }

    /**
     * Creates a deep cloned copy of the node.
     *
     * @param node
     * @return
     * @throws FlexiParserException
     */
    public static FlexiJSonNode copyOf(final FlexiJSonNode node) throws FlexiParserException {
        return new FlexiJSONParser(new FlexiJSonStringBuilder().toJSONString(node)).parse();
    }

    /**
     * @param inst1
     * @return
     * @throws FlexiMapperException
     */
    public static String serializeMinimized(final Object instance) throws FlexiMapperException {
        final FlexiJSonStringBuilder toString = new FlexiJSonStringBuilder();
        if (instance instanceof FlexiJSonNode) {
            return toString.toJSONString((FlexiJSonNode) instance);
        }
        final FlexiJSonMapper mapper = new FlexiJSonMapper();
        mapper.setIgnoreDefaultValuesEnabled(false);
        final FlexiJSonNode node = mapper.objectToJsonNode(instance);
        String newJson;
        newJson = toString.toJSONString(node);
        return newJson;
    }

    /**
     * @param value
     * @param t
     * @return
     * @throws FlexiMapperException
     * @throws FlexiParserException
     */
    public static Object jsonToObject(final String json, final CompiledType t) throws FlexiParserException, FlexiMapperException {
        return jsonToObject(json, FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES, t);
    }

    /**
     * @param json
     * @param ignoreListEnsureCorrectValues
     * @param t
     * @return
     * @throws FlexiMapperException
     * @throws FlexiParserException
     */
    private static Object jsonToObject(final String json, final HashSet<ParsingError> acceptableErrors, final CompiledType t) throws FlexiParserException, FlexiMapperException {
        final FlexiJSONParser parser = new FlexiJSONParser(json);
        parser.setIgnoreIssues(acceptableErrors);
        return new FlexiJSonMapper().jsonToObject(parser.parse(), t);
    }

    /**
     * @param target
     * @return
     */
    public static FlexiJSonNode getRoot(FlexiJSonNode target) {
        FlexiJSonNode pa = target.getParent();
        while (pa != null) {
            target = pa;
            pa = target.getParent();
        }
        return target;
    }

    /**
     * @param json
     * @return
     * @throws FlexiMapperException
     * @throws FlexiParserException
     */
    public static String prettyPrint(final String json) throws FlexiParserException, FlexiMapperException {
        return serializeToPrettyJson(jsonToObject(json, CompiledType.OBJECT));
    }

    /**
     * @param json
     * @param fromPathString
     * @return
     * @throws FlexiParserException
     */
    public static Object getValue(final String json, final JSPath path) throws FlexiParserException {
        return getValueNE(new FlexiJSONParser(json).parse(), path, Object.class);
    }

    public static <T> T getValue(final String json, final JSPath path, final Class<T> type) throws FlexiParserException {
        return getValueNE(new FlexiJSONParser(json).parse(), path, type);
    }

    /**
     * Returns a Key=Value List instead of json. One line per entry - except you define a custom deliminator
     *
     * @param location
     * @param string
     * @return
     */
    public static String serializeToProperties(final Object obj, final String deliminator) {
        String ret;
        try {
            ret = new PropertyJSonPrettyStringify().toJSONString(new FlexiJSonMapper().objectToJsonNode(obj));
            if (StringUtils.isNotEmpty(deliminator)) {
                ret = ret.replace("\r\n", deliminator);
            }
            return ret;
        } catch (final FlexiMapperException e) {
            throw new WTFException(e);
        }
    }

    /**
     * @param <T>
     * @param node
     * @param fromPathString
     * @return
     */
    public static <T> T getValueNE(final FlexiJSonNode node, final JSPath path, final Class<T> type) {
        return getValueNE(node, path, new SimpleTypeRef<T>(type));
    }

    public static <T> T getValueNE(final FlexiJSonNode node, final JSPath path, final TypeRef<T> type) {
        try {
            return getValue(node, path, type);
        } catch (final FlexiMapperException e) {
            throw new WTFException(e);
        }
    }

    /**
     * @param node
     * @param fromPathStringWithWTF
     * @param stringHashset
     * @throws FlexiMapperException
     */
    public static <T> T getValue(final FlexiJSonNode node, final JSPath path, final TypeRef<T> type) throws FlexiMapperException {
        if (path == null || path.isEmpty()) {
            return new FlexiJSonMapper().jsonToObject(node, type);
        }
        if (node instanceof FlexiJSonObject) {
            final FlexiJSonNode element = ((FlexiJSonObject) node).resolvePath(path);
            if (element == null) {
                // null>boolean
                return (T) ReflectionUtils.cast(null, type.getType());
            } else if (CompiledType.create(type).isInstanceOf(element.getClass())) {
                return (T) element;
            }
            return new FlexiJSonMapper().jsonToObject(element, type);
        } else if (node instanceof FlexiJSonArray) {
            final FlexiJSonNode element = ((FlexiJSonArray) node).resolvePath(path);
            if (element == null) {
                // null>boolean
                return (T) ReflectionUtils.cast(null, type.getType());
            } else if (CompiledType.create(type).isInstanceOf(element.getClass())) {
                return (T) element;
            }
            return new FlexiJSonMapper().jsonToObject(element, type);
        }
        return null;
    }

    /**
     * @param addressKey
     * @return
     */
    public static String serializeToPrettyJsonWithWTF(final Object o) {
        try {
            return serializeToPrettyJson(o);
        } catch (final FlexiMapperException e) {
            throw new WTFException(e);
        }
    }

    /**
     * @param <T>
     * @param object
     * @param class1
     * @return
     */
    @SuppressWarnings("unchecked")
    public static <T extends FlexiJSonNode> T objectToNodeWithWTF(final Object object, final Class<T> class1) {
        try {
            return (T) new FlexiJSonMapper().objectToJsonNode(object);
        } catch (final FlexiMapperException e) {
            throw new WTFException(e);
        }
    }

    /**
     * @param node
     * @param simpleTypeRef
     * @return
     */
    public static <T> T nodeToObjectWithWTF(final FlexiJSonNode node, final TypeRef<T> typRef) {
        try {
            return new FlexiJSonMapper().jsonToObject(node, typRef);
        } catch (final FlexiMapperException e) {
            throw new WTFException(e);
        }
    }

    /**
     * @param addressKey
     * @return
     */
    public static String serializeMinimizedWithWTF(final Object obj) {
        try {
            return serializeMinimized(obj);
        } catch (final FlexiMapperException e) {
            throw new WTFException(e);
        }
    }

    /**
     * @param <T>
     * @param objectToNodeWithWTF
     * @param flexiValueFilter
     */
    public static <T extends FlexiJSonNode> T filterValues(final T node, final FlexiValueFilter filter) {
        walk(node, new FlexiVisitor() {
            @Override
            public void onComment(final FlexiCommentJsonNode comment, final JSPath path) {
                if (!filter.keepComment(comment)) {
                    if (!comment.getParent().remove(comment)) {
                        throw new IllegalStateException();
                    }
                }
            }

            @Override
            public void onValue(final FlexiJSonValue value, final JSPath path) {
                if (!filter.keepValue(value)) {
                    if (!value.getParent().remove(value)) {
                        throw new IllegalStateException();
                    }
                }
            }

            @Override
            public void openObject(final FlexiJSonObject obj, final JSPath path) {
                if (!filter.keepObject(obj)) {
                    if (!obj.getParent().remove(obj)) {
                        throw new IllegalStateException();
                    }
                }
            }

            @Override
            public void closeObject(final FlexiJSonObject obj, final JSPath path) {
            }

            @Override
            public void openArray(final FlexiJSonArray array, final JSPath path) {
                if (!filter.keepArray(array)) {
                    if (!array.getParent().remove(array)) {
                        throw new IllegalStateException();
                    }
                }
            }

            @Override
            public void closeArray(final FlexiJSonArray array, final JSPath path) {
            }
        });
        return node;
    }

    /**
     * @param node
     * @param flexiVisitor
     */
    private static void walk(final FlexiJSonNode node, final FlexiVisitor flexiVisitor, final JSPath path) {
        if (node == null) {
            return;
        }
        if (node instanceof FlexiJSonObject) {
            final FlexiJSonObject obj = (FlexiJSonObject) node;
            flexiVisitor.openObject(obj, path);
            int comments = walkComments(obj.getCommentsBefore(), 0, flexiVisitor, path);
            comments = walkComments(obj.getCommentsInside(), comments, flexiVisitor, path);
            // new List to be Concurrent modification save. the loop might fail if we modify elements in the walk method
            for (final KeyValueElement e : new LinkedList<KeyValueElement>(obj.getElements())) {
                final JSPath newPath = path.derive(e.getKey());
                walk(e.getCommentsBeforeKey(), flexiVisitor, newPath);
                walk(e.getValue(), flexiVisitor, newPath);
                walk(e.getCommentsAfterKey(), flexiVisitor, newPath);
            }
            comments = walkComments(obj.getCommentsAfter(), comments, flexiVisitor, path);
            flexiVisitor.closeObject(obj, path);
        } else if (node instanceof FlexiJSonArray) {
            final FlexiJSonArray array = (FlexiJSonArray) node;
            flexiVisitor.openArray(array, path);
            int comments = walkComments(array.getCommentsBefore(), 0, flexiVisitor, path);
            comments = walkComments(array.getCommentsInside(), comments, flexiVisitor, path);
            // new List to be Concurrent modification save. the loop might fail if we modify elements in the walk method
            int i = 0;
            for (final FlexiJSonNode e : new LinkedList<FlexiJSonNode>(array)) {
                walk(e, flexiVisitor, path.derive(i++));
            }
            comments = walkComments(array.getCommentsAfter(), comments, flexiVisitor, path);
            flexiVisitor.closeArray(array, path);
        } else if (node instanceof FlexiJSonComments) {
            walkComments((FlexiJSonComments) node, 0, flexiVisitor, path);
        } else if (node instanceof FlexiCommentJsonNode) {
            flexiVisitor.onComment((FlexiCommentJsonNode) node, path);
        } else {
            final FlexiJSonValue value = (FlexiJSonValue) node;
            int comments = walkComments(value.getCommentsBefore(), 0, flexiVisitor, path);
            flexiVisitor.onValue(value, path);
            comments = walkComments(value.getCommentsAfter(), comments, flexiVisitor, path);
        }
    }

    /**
     * @param commentsBefore
     * @param i
     * @param flexiVisitor
     * @param path
     * @return
     */
    private static int walkComments(final FlexiJSonComments comments, int i, final FlexiVisitor flexiVisitor, final JSPath path) {
        if (comments == null || comments.size() == 0) {
            return i;
        }
        // new List to be Concurrent modification save. the loop might fail if we modify elements in the walk method
        for (final FlexiCommentJsonNode c : new LinkedList<FlexiCommentJsonNode>(comments)) {
            walk(c, flexiVisitor, path.derive(JSPath.COMMENT_PATH_PREFIX + i));
            i++;
        }
        return i;
    }

    public static void walk(final FlexiJSonNode node, final FlexiVisitor flexiVisitor) {
        walk(node, flexiVisitor, new JSPath());
    }

    /**
     * @param node
     * @param string
     * @param class1
     * @return
     */
    public static <T> T getValueNE(final FlexiJSonObject node, final String path, final Class<T> class1) {
        return getValueNE(node, JSPath.getNE(path), class1);
    }

    /**
     * @param latest
     * @param type
     * @return
     * @throws IOException
     */
    public static <T> T readObject(final File f, final TypeRef<T> type) throws FlexiParserException, FlexiMapperException, IOException {
        final InputStream is = IO.BOM.wrap(new BufferedInputStream(new FileInputStream(f)));
        try {
            return readObject(is, type);
        } finally {
            try {
                is.close();
            } catch (final IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }

    public static <T> T readObject(final InputStream is, final TypeRef<T> type) throws FlexiParserException, FlexiMapperException {
        final FlexiJSonNode node = new FlexiJSONParser(is).setDebug(new StringBuilder()).parse();
        if (type.getType() instanceof Class && Clazz.isInstanceof(node.getClass(), (Class<?>) type.getType())) {
            return (T) node;
        }
        return new FlexiJSonMapper().jsonToObject(node, type);
    }

    /**
     * @param <T>
     * @param configFile
     * @param class1
     * @return
     * @throws IOException
     * @throws FlexiMapperException
     * @throws FlexiParserException
     */
    public static <T> T readObject(final File f, final Class<T> type) throws FlexiParserException, FlexiMapperException, IOException {
        return readObject(f, new SimpleTypeRef<T>(type));
    }

    /**
     * @param configFile
     * @param class1
     * @return
     */
    public static <T> T readObjectNE(final File f, final Class<T> c) {
        try {
            return readObject(f, c);
        } catch (final FlexiParserException e) {
            throw new WTFException(e);
        } catch (final FlexiMapperException e) {
            throw new WTFException(e);
        } catch (final IOException e) {
            throw new WTFException(e);
        }
    }

    /**
     * @param file
     * @param hashmap
     * @return
     */
    public static <T> T readObjectNE(final File file, final TypeRef<T> type) {
        try {
            return readObject(file, type);
        } catch (final FlexiParserException e) {
            throw new WTFException(e);
        } catch (final FlexiMapperException e) {
            throw new WTFException(e);
        } catch (final IOException e) {
            throw new WTFException(e);
        }
    }

    /**
     * @param <T>
     * @param creates
     *            a clone of the input. The input may either be a storable/Interface or a FlexiNode. Only json content is cloned - no
     *            comments or other metadata.
     * @return
     * @throws FlexiMapperException
     * @throws FlexiParserException
     */
    public static <T> T clone(final T input) throws FlexiMapperException, FlexiParserException {
        final FlexiJSonMapper mapper = new FlexiJSonMapper();
        FlexiJSonNode node;
        if (input instanceof FlexiJSonNode) {
            return (T) new FlexiJSONParser(serializeMinimized(input)).parse();
        } else {
            node = mapper.objectToJsonNode(input);
            if (Proxy.isProxyClass(input.getClass())) {
                final InterfaceStorage<T> uis = InterfaceStorage.get(input);
                if (uis != null) {
                    return (T) mapper.jsonToObject(node, uis.cType);
                }
            }
            return (T) mapper.jsonToObject(node, CompiledType.create(input.getClass()));
        }
    }

    public static <T> T cloneNE(final T input) throws WTFException {
        try {
            return clone(input);
        } catch (final FlexiMapperException e) {
            throw new WTFException(e);
        } catch (final FlexiParserException e) {
            throw new WTFException(e);
        }
    }

    /**
     * Overwrites all fields in add or creates a new file
     *
     * @param file
     * @param add
     * @return
     * @throws IOException
     * @throws FlexiMapperException
     * @throws FlexiParserException
     * @throws MergeException
     */
    public static FlexiJSonObject overwrite(final File file, final FlexiJSonObject add) throws FlexiParserException, FlexiMapperException, IOException, MergeException {
        try {
            if (file.isFile()) {
                final FlexiJSonObject node = FlexiUtils.readObject(file, FlexiJSonObject.class);
                final JsonModification mod = new JsonModification();
                mod.setSet(add);
                new FlexiModifier(node).merge(mod);
                IO.secureWrite(file, serializeToPrettyJson(node), SYNC.META_AND_DATA);
                return node;
            } else {
                IO.secureWrite(file, serializeToPrettyJson(add), SYNC.META_AND_DATA);
                return add;
            }
        } catch (final IOException e) {
            e.printStackTrace();
            throw e;
        }
    }
}
