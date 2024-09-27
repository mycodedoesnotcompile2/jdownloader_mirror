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
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

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
import org.appwork.utils.AutoCloseInputStream;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.DebugMode;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.reflection.Clazz;

/**
 * @author thomas
 * @date 15.09.2023
 *
 */
public class FlexiSerializer extends AbstractSerializer implements SerializerInterface {
    private FlexiJSonMapper defaultMapper = null;

    /**
     *
     */
    public FlexiSerializer() {
    }

    protected FlexiJSonNode toNode(Object o, Object... context) throws FlexiMapperException {
        try {
            if (o instanceof FlexiJSonNode) {
                return (FlexiJSonNode) o;
            } else {
                final FlexiJSonNode node = getMapper(context).objectToJsonNode(o);
                return node;
            }
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
        try {
            final FlexiJSonNode node = toNode(o, context);
            return getStringifier(context).toJSONString(node);
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

    public void cleanup() {
        PRETTIES.set(null);
        MINIFIEDS.set(null);
    }

    protected FlexiJSonMapper getMapper(Object... context) {
        FlexiJSonMapper ret = null;
        if (contextContainsAll(context, SC.WITH_DOCUMENTATION)) {
            ret = new FlexiJsonMapperForConfig();
        }
        if (contextContainsAll(context, SC.IGNORE_DEFAULT_VALUES)) {
            if (ret == null) {
                ret = new FlexiJSonMapper();
            }
            ret.setIgnoreDefaultValuesEnabled(true);
        }
        if (ret == null) {
            ret = defaultMapper;
            if (ret == null) {
                ret = new FlexiJSonMapper();
                defaultMapper = ret;
            }
        }
        return ret;
    }

    protected FlexiJSonStringBuilder getStringifier(Object... context) {
        if (contextContainsAll(context, SC.HASH_CONTENT)) {
            return new FlexiJSonStringBuilder() {
                @Override
                protected List<KeyValueElement> getObjectElementsList(FlexiJSonObject object) {
                    final List<KeyValueElement> elems = super.getObjectElementsList(object);
                    if (elems.size() <= 1) {
                        return elems;
                    } else {
                        // sort properties according to SC.HASH_CONTENT docs
                        final List<KeyValueElement> ret = new ArrayList<KeyValueElement>(elems);
                        Collections.sort(ret, new Comparator<KeyValueElement>() {
                            @Override
                            public int compare(KeyValueElement o1, KeyValueElement o2) {
                                return CompareUtils.compareComparable(o1.getKey(), o2.getKey());
                            }
                        });
                        return ret;
                    }
                }
            };
        } else if (contextContainsAll(context, SC.WITH_DOCUMENTATION)) {
            return new FlexiJSonPrettyPrinterForConfig(null);
        } else if (isContextPretty(context)) {
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
        try {
            final FlexiJSonNode node = createParser(json, context).parse();
            return getMapper(context).jsonToObject(node, type);
        } catch (Exception e) {
            throw SerializerException.wrap(e);
        }
    }

    protected FlexiJSONParser createParser(String json, Object[] context) {
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
        try {
            final FlexiJSonNode node = createParser(stream, context).parse();
            return getMapper(context).jsonToObject(node, type);
        } catch (Exception e) {
            throw SerializerException.wrap(e);
        } finally {
            if (stream instanceof AutoCloseInputStream) {
                try {
                    stream.close();
                } catch (IOException e) {
                    throw SerializerException.wrap(e);
                }
            }
        }
    }

    /**
     * @param stream
     * @return
     */
    protected FlexiJSONParser createParser(InputStream stream, Object... context) {
        FlexiJSONParser ret = new FlexiJSONParser(stream);
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            ret.setDebug(new StringBuilder());
        }
        setIgnoreIssuesByContext(ret, context);
        return ret;
    }

    public void setIgnoreIssuesByContext(FlexiJSONParser ret, Object... context) {
        for (Object o : context) {
            if (o == SC.NON_STRICT) {
                ret.addIgnoreIssues(FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES);
            } else if (o == SC.WITH_DOCUMENTATION) {
                ret.addIgnoreIssues(FlexiJSONParser.IGNORE_LIST_COMMENTS);
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
            final boolean ensureNewInstances = contextContainsAll(context, SC.ENSURE_NEW_INSTANCES);
            if (!ensureNewInstances && Clazz.isInstanceof(o.getClass(), ReflectionUtils.getRaw(type.getType()))) {
                return (T) o;
            } else {
                return getMapper(context).convert(o, type);
            }
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
        try {
            final FlexiJSonNode node = toNode(o, context);
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
