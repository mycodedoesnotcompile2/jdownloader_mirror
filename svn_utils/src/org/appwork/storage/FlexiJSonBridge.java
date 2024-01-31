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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.converter.ConverterInput;
import org.appwork.storage.converter.JsonConverterForDeserialization;
import org.appwork.storage.converter.JsonConverterForSerialization;
import org.appwork.storage.flexijson.FlexiJSONParser;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.FlexiParserException;
import org.appwork.storage.flexijson.mapper.FlexiJSonMapper;
import org.appwork.storage.flexijson.mapper.FlexiMapperException;
import org.appwork.storage.flexijson.mapper.DefaultObjectToJsonContext;
import org.appwork.storage.flexijson.stringify.FlexiJSonPrettyStringify;
import org.appwork.storage.flexijson.stringify.FlexiJSonStringBuilder;
import org.appwork.storage.mapper.AbstractJsonMapper;
import org.appwork.storage.simplejson.ParserException;
import org.appwork.storage.simplejson.mapper.Getter;
import org.appwork.storage.simplejson.mapper.Setter;
import org.appwork.utils.Exceptions;
import org.appwork.utils.reflection.CompiledType;

/**
 * @author thomas
 *
 */
public class FlexiJSonBridge extends AbstractJsonMapper {
    /**
     * @author thomas
     * @date 21.01.2022
     *
     */
    public class ExtFlexiJsonMapper extends FlexiJSonMapper {
        @Override
        protected Object handleMapperJsonNodeToObject(final FlexiJSonNode json, CompiledType type, Setter setter) throws FlexiMapperException {
            try {
                HashSet<Object> byp = bypass.get();
                if (byp == null || !byp.contains(json)) {
                    Class<?> raw = getRawTypeForDeserialization(type.type);
                    SimpleTypeRef<Object> typeRef = null;
                    {
                        List<JsonConverterForDeserialization> matches = findBestTypeMatch(raw, convertForDeserialization, JsonConverterForDeserialization.class);
                        if (matches != null && matches.size() > 0) {
                            ConverterInput input = new ConverterInput(json) {
                                @Override
                                protected Object lazyGenericRepresentationInit() {
                                    try {
                                        return ExtFlexiJsonMapper.this.jsonToObjectBypass(json, CompiledType.OBJECT);
                                    } catch (FlexiMapperException e) {
                                        throw new WTFException(e);
                                    }
                                }
                            };
                            if (typeRef == null) {
                                typeRef = new SimpleTypeRef<Object>(type.type);
                            }
                            Object ret = handleDeserializationConverterMatches(input, typeRef, matches);
                            if (ret != input) {
                                return ret;
                            }
                        }
                    }
                    {
                        List<JsonDeSerializer> matches = findBestTypeMatch(raw, deSerializer, JsonDeSerializer.class);
                        if (matches != null && matches.size() > 0) {
                            DeSerializerInput input = new DeSerializerInput(json) {
                                @Override
                                protected String lazyJSONRepresentationInit() {
                                    return new FlexiJSonStringBuilder().toJSONString(json);
                                }
                            };
                            if (typeRef == null) {
                                typeRef = new SimpleTypeRef<Object>(type.type);
                            }
                            Object ret = handleDeserializationMatches(input, typeRef, matches);
                            if (ret != input) {
                                return ret;
                            }
                        }
                    }
                }
                return super.handleMapperJsonNodeToObject(json, type, setter);
            } catch (WTFException e) {
                FlexiMapperException hidden = Exceptions.getInstanceof(e, FlexiMapperException.class);
                if (hidden != null) {
                    throw hidden;
                }
                throw e;
            }
        }

        /**
         * @param json
         * @param object
         * @return
         * @throws FlexiMapperException
         */
        protected Object jsonToObjectBypass(FlexiJSonNode json, CompiledType type) throws FlexiMapperException {
            // bypass the next call of handleMapperObjectToJsonNode
            HashSet<Object> set = bypass.get();
            if (set == null) {
                set = new HashSet<Object>();
                bypass.set(set);
            }
            set.add(json);
            try {
                return jsonToObject(json, type);
            } finally {
                set.remove(json);
            }
        }

        private ThreadLocal<HashSet<Object>> bypass = new ThreadLocal<HashSet<Object>>();

        @Override
        protected FlexiJSonNode handleMapperObjectToJsonNode(Getter reference, Object obj, DefaultObjectToJsonContext context) throws FlexiMapperException {
            boolean mapped = false;
            {
                List<JsonConverterForSerialization> matches = findBestTypeMatch(obj == null ? null : obj.getClass(), convertForSerialization, JsonConverterForSerialization.class);
                if (matches != null && matches.size() > 0) {
                    Object ret = handleSerializationConverterMatches(obj, matches);
                    if (ret != JsonConverterForSerialization.SKIP && ret != obj) {
                        obj = ret;
                        mapped = true;
                    }
                }
            }
            {
                List<JsonSerializer> matches = findBestTypeMatch(obj == null ? null : obj.getClass(), serializer, JsonSerializer.class);
                if (matches != null && matches.size() > 0) {
                    final String ret = handleSerializationMatches(obj, matches);
                    if (ret != JsonSerializer.SKIP) {
                        try {
                            return new FlexiJSONParser(ret).parse();
                        } catch (FlexiParserException e) {
                            throw new FlexiMapperException(null, CompiledType.create(obj.getClass()), e);
                        }
                    }
                }
            }
            if (mapped) {
                return mapper.objectToJsonNode(obj);
            }
            return super.handleMapperObjectToJsonNode(reference, obj, context);
        }
    }

    public final ExtFlexiJsonMapper mapper;

    public FlexiJSonBridge() {
        mapper = new ExtFlexiJsonMapper();
    }

    private boolean prettyPrintEnabled = true;

    public boolean isPrettyPrintEnabled() {
        return prettyPrintEnabled;
    }

    public void setPrettyPrintEnabled(final boolean prettyPrintEnabled) {
        this.prettyPrintEnabled = prettyPrintEnabled;
    }

    @Override
    public String objectToString(final Object value) throws JSonMapperException {
        try {
            FlexiJSonNode node = mapper.objectToJsonNode(value);
            if (isPrettyPrintEnabled()) {
                return new FlexiJSonPrettyStringify().toJSONString(node);
            } else {
                return new FlexiJSonStringBuilder().toJSONString(node);
            }
        } catch (FlexiMapperException e) {
            throw new JSonMapperException(e);
        }
    }

    @SuppressWarnings("unchecked")
    @Override
    public <T> T stringToObject(final String jsonString, final Class<T> clazz) throws JSonMapperException {
        try {
            FlexiJSonNode node = createParser(jsonString).parse();
            return (T) mapper.jsonToObject(node, CompiledType.create(clazz));
        } catch (final ParserException e) {
            throw new JSonMapperException(e);
        } catch (FlexiMapperException e) {
            throw new JSonMapperException(e);
        }
    }

    protected FlexiJSONParser createParser(final String jsonString) {
        return new FlexiJSONParser(jsonString);
    }

    @Override
    public <T> T stringToObject(final String jsonString, final TypeRef<T> type) throws JSonMapperException {
        try {
            FlexiJSonNode node = createParser(jsonString).parse();
            return mapper.jsonToObject(node, type);
        } catch (final ParserException e) {
            throw new JSonMapperException(e);
        } catch (FlexiMapperException e) {
            throw new JSonMapperException(e);
        }
    }

    @Override
    public <T> T convert(Object object, TypeRef<T> type) throws JSonMapperException {
        try {
            FlexiJSonNode node = mapper.objectToJsonNode(object);
            return mapper.jsonToObject(node, type);
        } catch (FlexiMapperException e) {
            throw new JSonMapperException(e);
        }
    }

    @Override
    public byte[] objectToByteArray(final Object value) throws JSonMapperException {
        try {
            FlexiJSonNode node = mapper.objectToJsonNode(value);
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            new FlexiJSonStringBuilder().toJSONString(node, out, new LinkedList<String>());
            return out.toByteArray();
        } catch (FlexiMapperException e) {
            throw new JSonMapperException(e);
        }
    }

    /**
     * closes outputStream
     */
    @Override
    public void writeObject(final OutputStream outputStream, final Object value) throws JSonMapperException {
        try {
            try {
                FlexiJSonNode node = mapper.objectToJsonNode(value);
                new FlexiJSonStringBuilder().toJSONString(node, outputStream, new LinkedList<String>());
            } finally {
                if (outputStream != null) {
                    try {
                        outputStream.close();
                    } catch (IOException e) {
                        throw new JSonMapperException(e);
                    }
                }
            }
        } catch (FlexiMapperException e) {
            throw new JSonMapperException(e);
        }
    }

    /**
     * closes inputStream
     *
     * @param inputStream
     * @param type
     * @return
     * @throws JSonMapperException
     */
    @Override
    public <T> T inputStreamToObject(final InputStream inputStream, final TypeRef<T> type) throws JSonMapperException {
        try {
            try {
                FlexiJSonNode node = new FlexiJSONParser(inputStream).parse();
                return mapper.jsonToObject(node, type);
            } finally {
                if (inputStream != null) {
                    inputStream.close();
                }
            }
        } catch (FlexiParserException e) {
            throw new JSonMapperException(e);
        } catch (IOException e) {
            throw new JSonMapperException(e);
        } catch (FlexiMapperException e) {
            throw new JSonMapperException(e);
        }
    }

    @Override
    public <T> T byteArrayToObject(final byte[] byteArray, final Class<T> clazz) throws JSonMapperException {
        try {
            FlexiJSonNode node = new FlexiJSONParser(new ByteArrayInputStream(byteArray)).parse();
            return (T) mapper.jsonToObject(node, CompiledType.create(clazz));
        } catch (final ParserException e) {
            throw new JSonMapperException(e);
        } catch (FlexiMapperException e) {
            throw new JSonMapperException(e);
        }
    }

    /**
     * closes inputStream
     *
     * @param inputStream
     * @param clazz
     * @return
     * @throws JSonMapperException
     */
    @Override
    public <T> T inputStreamToObject(final InputStream inputStream, Class<T> clazz) throws JSonMapperException {
        try {
            FlexiJSonNode node = new FlexiJSONParser(inputStream).parse();
            return (T) mapper.jsonToObject(node, CompiledType.create(clazz));
        } catch (final ParserException e) {
            throw new JSonMapperException(e);
        } catch (FlexiMapperException e) {
            throw new JSonMapperException(e);
        }
    }
}
