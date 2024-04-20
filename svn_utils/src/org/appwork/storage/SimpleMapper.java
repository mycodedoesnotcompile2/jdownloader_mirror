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

import java.lang.reflect.Type;
import java.util.List;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.converter.ConverterInput;
import org.appwork.storage.converter.JsonConverterForDeserialization;
import org.appwork.storage.converter.JsonConverterForSerialization;
import org.appwork.storage.mapper.AbstractJsonMapper;
import org.appwork.storage.simplejson.JSonFactory;
import org.appwork.storage.simplejson.JSonNode;
import org.appwork.storage.simplejson.ParserException;
import org.appwork.storage.simplejson.mapper.JSonMapper;
import org.appwork.storage.simplejson.mapper.MapperException;
import org.appwork.utils.Exceptions;

/**
 * @author thomas
 * @param <T>
 *
 */
public class SimpleMapper extends AbstractJsonMapper {
    /**
     * @author thomas
     * @date 16.04.2024
     *
     */
    public class InnerMapper extends JSonMapper {
        protected Object jsonToObjectByPassDeserializers(final JSonNode json, Type type) throws MapperException {
            return super.jsonToObject(json, type);
        }

        @Override
        public Object jsonToObject(final JSonNode json, Type type) throws MapperException {
            try {
                Class<?> raw = getRawTypeForDeserialization(type);
                SimpleTypeRef<Object> typeRef = null;
                {
                    List<JsonConverterForDeserialization> matches = findBestTypeMatch(raw, convertForDeserialization, JsonConverterForDeserialization.class);
                    if (matches != null && matches.size() > 0) {
                        ConverterInput input = new ConverterInput(json) {
                            @Override
                            protected Object lazyGenericRepresentationInit() {
                                try {
                                    return jsonToObjectByPassDeserializers(json, Object.class);
                                } catch (MapperException e) {
                                    throw new WTFException();
                                }
                            }
                        };
                        if (typeRef == null) {
                            typeRef = new SimpleTypeRef<Object>(type);
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
                                return json.toString();
                            }
                        };
                        if (typeRef == null) {
                            typeRef = new SimpleTypeRef<Object>(type);
                        }
                        Object ret = handleDeserializationMatches(input, typeRef, matches);
                        if (ret != input) {
                            return ret;
                        }
                    }
                }
                return jsonToObjectByPassDeserializers(json, type);
            } catch (WTFException e) {
                MapperException hiddenMapperException = Exceptions.getInstanceof(e, MapperException.class);
                if (hiddenMapperException != null) {
                    throw hiddenMapperException;
                }
                throw e;
            }
        }

        @Override
        public JSonNode create(Object obj) throws MapperException {
            {
                List<JsonConverterForSerialization> matches = findBestTypeMatch(obj == null ? null : obj.getClass(), convertForSerialization, JsonConverterForSerialization.class);
                if (matches != null && matches.size() > 0) {
                    Object ret = handleSerializationConverterMatches(obj, matches);
                    if (ret != JsonConverterForSerialization.SKIP && ret != obj) {
                        obj = ret;
                    }
                }
            }
            {
                List<JsonSerializer> matches = findBestTypeMatch(obj == null ? null : obj.getClass(), serializer, JsonSerializer.class);
                if (matches != null && matches.size() > 0) {
                    final String ret = handleSerializationMatches(obj, matches);
                    if (ret != JsonSerializer.SKIP) {
                        return new JSonNode() {
                            @Override
                            public String toString() {
                                return ret;
                            }

                            @Override
                            public String toPrettyString() {
                                return ret;
                            }
                        };
                    }
                }
            }
            return super.create(obj);
        }
    }

    protected final JSonMapper mapper;

    public SimpleMapper() {
        mapper = buildMapper();
    }

    protected JSonMapper buildMapper() {
        return new InnerMapper();
    }

    public JSonMapper getMapper() {
        return mapper;
    }

    private boolean prettyPrintEnabled = true;

    public boolean isPrettyPrintEnabled() {
        return prettyPrintEnabled;
    }

    protected JSonFactory newJsonFactory(String jsonString) {
        return new JSonFactory(jsonString);
    }

    public SimpleMapper setPrettyPrintEnabled(final boolean prettyPrintEnabled) {
        this.prettyPrintEnabled = prettyPrintEnabled;
        return this;
    }

    @Override
    public String objectToString(final Object value) throws JSonMapperException {
        try {
            if (isPrettyPrintEnabled()) {
                return mapper.create(value).toPrettyString();
            } else {
                return mapper.create(value).toString();
            }
        } catch (final MapperException e) {
            throw new JSonMapperException(e);
        }
    }

    @SuppressWarnings("unchecked")
    @Override
    public <T> T stringToObject(String jsonString, final Class<T> clazz) throws JSonMapperException {
        try {
            final JSonNode jsonFactory = newJsonFactory(jsonString).parse();
            // allow GC of jsonString
            jsonString = null;
            return (T) mapper.jsonToObject(jsonFactory, clazz);
        } catch (final ParserException e) {
            throw new JSonMapperException(e);
        } catch (final MapperException e) {
            throw new JSonMapperException(e);
        }
    }

    @Override
    public <T> T stringToObject(String jsonString, final TypeRef<T> type) throws JSonMapperException {
        try {
            final JSonNode jsonFactory = newJsonFactory(jsonString).parse();
            // allow GC of jsonString
            jsonString = null;
            return mapper.jsonToObject(jsonFactory, type);
        } catch (final ParserException e) {
            throw new JSonMapperException(e);
        } catch (final MapperException e) {
            throw new JSonMapperException(e);
        }
    }

    @Override
    public <T> T convert(Object object, TypeRef<T> type) throws JSonMapperException {
        try {
            return mapper.jsonToObject(mapper.create(object), type);
        } catch (MapperException e) {
            throw new JSonMapperException(e);
        }
    }
}
