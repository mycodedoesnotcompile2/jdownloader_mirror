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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.appwork.serializer.Deser;
import org.appwork.serializer.SC;
import org.appwork.storage.TypeRef;
import org.appwork.storage.commonInterface.AbstractSerializer;
import org.appwork.storage.commonInterface.SerializerException;
import org.appwork.storage.commonInterface.SerializerInterface;
import org.appwork.storage.flexijson.mapper.FlexiJSonMapper;
import org.appwork.storage.flexijson.mapper.FlexiMapperException;
import org.appwork.storage.flexijson.stringify.FlexiJSonPrettyPrinterForConfig;
import org.appwork.storage.flexijson.stringify.FlexiJSonPrettyStringify;
import org.appwork.storage.flexijson.stringify.FlexiJSonStringBuilder;
import org.appwork.utils.DebugMode;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.reflection.Clazz;

/**
 * @author thomas
 * @date 15.09.2023
 *
 */
public class FlexiSerializer extends AbstractSerializer implements SerializerInterface {
    private final FlexiJSonMapper mapper;

    /**
     *
     */
    public FlexiSerializer() {
        mapper = new FlexiJSonMapper();
    }

    public FlexiJSonNode toNode(Object o) throws FlexiMapperException {
        try {
            FlexiJSonNode node;
            node = o instanceof FlexiJSonNode ? (FlexiJSonNode) o : mapper.objectToJsonNode(o);
            return node;
        } catch (Exception e) {
            throw SerializerException.wrap(e);
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.commonInterface.SerializerInterface#toString(java.lang.Object, boolean)
     */
    @Override
    public String toString(Object o, Object... context) throws SerializerException {
        DebugMode.breakIf(context == null || (context.length == 1 && context[0] == null), context);
        try {
            FlexiJSonNode node = null;
            if (o instanceof FlexiJSonNode) {
                node = (FlexiJSonNode) o;
            } else {
                if (contextContainsAll(context, SC.WITH_DOCUMENTATION)) {
                    final FlexiJSonPrettyStringify toString = new FlexiJSonPrettyPrinterForConfig(null);
                    final FlexiJSonMapper mapper = new FlexiJsonMapperForConfig();
                    node = mapper.objectToJsonNode(o);
                    return toString.toJSONString(node);
                }
                if (contextContainsAll(context, SC.IGNORE_DEFAULT_VALUES)) {
                    FlexiJSonMapper mapper = new FlexiJSonMapper();
                    mapper.setIgnoreDefaultValuesEnabled(contextContainsAll(context, SC.IGNORE_DEFAULT_VALUES));
                    node = mapper.objectToJsonNode(o);
                } else {
                    node = toNode(o);
                }
            }
            if (node == null) {
                node = toNode(o);
            }
            String ret = getStringifier(context).toJSONString(node);
            return ret;
        } catch (Exception e) {
            throw SerializerException.wrap(e);
        }
    }

    /**
     * @param pretty2
     * @return
     */
    private static final ThreadLocal<FlexiJSonPrettyStringify> PRETTIES  = new ThreadLocal<FlexiJSonPrettyStringify>();
    private static final ThreadLocal<FlexiJSonStringBuilder>   MINIFIEDS = new ThreadLocal<FlexiJSonStringBuilder>();

    private FlexiJSonStringBuilder getStringifier(Object... context) {
        boolean pretty = isContextPretty(context);
        if (pretty) {
            // no threadsafe - we cannot use a single instance
            FlexiJSonPrettyStringify inst = PRETTIES.get();
            if (inst == null) {
                inst = new FlexiJSonPrettyStringify();
                PRETTIES.set(inst);
            }
            return inst;
        } else {
            // no threadsafe - we cannot use a single instance
            FlexiJSonStringBuilder inst = MINIFIEDS.get();
            if (inst == null) {
                inst = new FlexiJSonStringBuilder();
                MINIFIEDS.set(inst);
            }
            return inst;
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.commonInterface.SerializerInterface#fromString(java.lang.String, org.appwork.storage.TypeRef)
     */
    @Override
    public <T> T fromString(String json, TypeRef<T> type, Object... context) throws SerializerException {
        FlexiJSonNode node;
        try {
            node = createParser(json, context).parse();
            return mapper.jsonToObject(node, type);
        } catch (Exception e) {
            throw SerializerException.wrap(e);
        }
    }

    public FlexiJSONParser createParser(String json, Object[] context) {
        FlexiJSONParser ret = new FlexiJSONParser(json);
        setIgnoreIssuesByContext(ret, context);
        return ret;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.commonInterface.SerializerInterface#fromStream(java.io.InputStream, org.appwork.storage.TypeRef)
     */
    @Override
    public <T> T fromStream(InputStream stream, TypeRef<T> type, Object... context) throws SerializerException {
        FlexiJSonNode node;
        try {
            node = createParser(stream, context).parse();
            return mapper.jsonToObject(node, type);
        } catch (Exception e) {
            throw SerializerException.wrap(e);
        }
    }

    /**
     * @param stream
     * @return
     */
    protected FlexiJSONParser createParser(InputStream stream, Object... context) {
        FlexiJSONParser ret = new FlexiJSONParser(stream);
        setIgnoreIssuesByContext(ret, context);
        return ret;
    }

    public void setIgnoreIssuesByContext(FlexiJSONParser ret, Object... context) {
        for (Object o : context) {
            if (o == SC.NON_STRICT) {
                ret.setIgnoreIssues(FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES);
            }
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.commonInterface.SerializerInterface#convert(java.lang.Object, boolean, java.lang.Class)
     */
    @Override
    public <T> T convert(Object o, TypeRef<T> type, Object... context) throws SerializerException {
        try {
            boolean ensureNewInstances = false;
            for (Object c : context) {
                if (c == SC.ENSURE_NEW_INSTANCES) {
                    ensureNewInstances = true;
                    break;
                }
            }
            if (!ensureNewInstances && Clazz.isInstanceof(o.getClass(), ReflectionUtils.getRaw(type.getType()))) {
                return (T) o;
            }
            return mapper.convert(o, type);
        } catch (Exception e) {
            throw SerializerException.wrap(e);
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.commonInterface.SerializerInterface#toStream(java.lang.Object, java.io.OutputStream, boolean, boolean)
     */
    @Override
    public void toStream(Object o, OutputStream os, boolean closeOutputStream, Object... context) throws SerializerException {
        DebugMode.breakIf(context == null || (context.length == 1 && context[0] == null), context);
        FlexiJSonNode node = null;
        try {
            if (o instanceof FlexiJSonNode) {
                node = (FlexiJSonNode) o;
            } else {
                if (contextContainsAll(context, SC.WITH_DOCUMENTATION)) {
                    final FlexiJSonPrettyStringify toString = new FlexiJSonPrettyPrinterForConfig(null);
                    final FlexiJSonMapper mapper = new FlexiJsonMapperForConfig();
                    mapper.setIgnoreDefaultValuesEnabled(contextContainsAll(context, SC.IGNORE_DEFAULT_VALUES));
                    node = mapper.objectToJsonNode(o);
                    toString.toJSONString(node, os, null);
                }
                if (contextContainsAll(context, SC.IGNORE_DEFAULT_VALUES)) {
                    FlexiJSonMapper mapper = new FlexiJSonMapper();
                    mapper.setIgnoreDefaultValuesEnabled(contextContainsAll(context, SC.IGNORE_DEFAULT_VALUES));
                    node = mapper.objectToJsonNode(o);
                }
                if (node == null) {
                    node = toNode(o);
                }
            }
            getStringifier(context).toJSONString(node, os, null);
        } catch (Exception e) {
            throw SerializerException.wrap(e);
        } finally {
            if (closeOutputStream) {
                try {
                    os.close();
                } catch (IOException e) {
                    throw SerializerException.wrap(e);
                }
            }
        }
    }

    /**
     *
     */
    public static void setAsDefault() {
        Deser.set(new FlexiSerializer());
    }
}
