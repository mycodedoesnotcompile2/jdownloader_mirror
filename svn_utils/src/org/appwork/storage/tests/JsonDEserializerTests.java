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
package org.appwork.storage.tests;

import java.io.UnsupportedEncodingException;
import java.lang.reflect.ParameterizedType;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.DeSerializerInput;
import org.appwork.storage.JSONMapper;
import org.appwork.storage.JsonDeSerializer;
import org.appwork.storage.JsonSerializer;
import org.appwork.storage.SelfJSONDeSerializer;
import org.appwork.storage.SelfJSONSerializer;
import org.appwork.storage.Storable;
import org.appwork.storage.TypeRef;
import org.appwork.storage.converter.ConverterInput;
import org.appwork.storage.converter.JsonConverterForDeserialization;
import org.appwork.storage.converter.JsonConverterForSerialization;
import org.appwork.storage.converter.SelfJsonConverterForDeserialization;
import org.appwork.storage.converter.SelfJsonConverterForSerialization;
import org.appwork.testframework.AWTest;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.formatter.HexFormatter;

/**
 * @author thomas
 * @date 14.01.2022
 *
 */
public abstract class JsonDEserializerTests extends AWTest {

    public static Object   CONTEXT;
    private static boolean FLAG;

    protected void testOnMapper(JSONMapper mapper, Object SC) throws Exception {

        CONTEXT = mapper;
        logInfoAnyway("Test mapper : " + mapper.getClass().getSimpleName());

        testSerializeDeserializeSelfSerializer(mapper);
        testSerializeDeserializeSelfConverter(mapper);
        testHiddenInterfaces(mapper);

        testAddingAConverter(mapper);

        testAddingASerializer(mapper);
        // do twice to see of multiple converters/serializers in th same mapper cause issues
        testAddingAConverter(mapper);

        testAddingASerializer(mapper);

        testConvertStringToHex(mapper);

        testConverterClassHirarchy(mapper);

        testNullSerializerAndObjectDeserializer(mapper);
    }

    @SuppressWarnings("deprecation")
    protected void testNullSerializerAndObjectDeserializer(JSONMapper mapper) throws Exception {
        mapper.putSerializer(null, new JsonSerializer() {

            @Override
            public String toJSonString(Object object, Object mapper) {
                // convert null to string "null"
                FLAG = false;
                return "\"null\"";
            }
        });

        mapper.putDeSerializer(Object.class, new JsonDeSerializer() {

            @Override
            public Object toObject(DeSerializerInput input, TypeRef<?> typeRef, Object mapper) {
                // if null, return null
                String json = input.getJSONRepresentation();
                if ("\"null\"".equals(json)) {
                    FLAG = false;
                    return null;
                }

                // else return the input -> Do not handle
                return input;
            }
        });
        FLAG = true;
        HashMap<String, String> org = new HashMap<String, String>();
        org.put("a", null);
        org.put("b", "null");
        String json = mapper.objectToString(org);
        assertFalse(FLAG);
        Object restoreTEst = mapper.stringToObject(json, TypeRef.OBJECT);
        assertType(restoreTEst, Map.class);
        FLAG = true;
        HashMap<String, String> restored = mapper.stringToObject(json, new TypeRef<HashMap<String, String>>() {
        });
        assertType(restored, Map.class);
        assertFalse(FLAG);
        assertEquals(restored.get("a"), null);
        // converted "null" to null;
        assertEquals(restored.get("b"), null);
    }

    protected void testConverterClassHirarchy(JSONMapper mapper) throws Exception {
        // we already have a converter for List.class that converts a list to a hashap
        // this mapper matches in ArrayList, and should get prefered if we have an actuall arraylist
        mapper.putConverterForSerialization(ArrayList.class, new JsonConverterForSerialization() {

            @Override
            public Object convertForSerialization(Object obj, Object mapper) {

                FLAG = false;
                return StringUtils.join(((ArrayList) obj), ",");
            }

        });

        mapper.putConverterFromDeserialization(ArrayList.class, new JsonConverterForDeserialization() {

            @Override
            public Object convertForDeserialization(ConverterInput input, TypeRef<?> typeRef, Object mapper) {
                Object object = input.getGenericRepresentation();
                return new ArrayList<String>(Arrays.asList(((String) object).split(",")));
            }
        });
        FLAG = true;
        List<String> org;
        String json = mapper.objectToString(org = new ArrayList<String>(Arrays.asList("1", "2", "3")));
        assertFalse(FLAG);
        Object restoreTEst = mapper.stringToObject(json, TypeRef.OBJECT);
        assertType(restoreTEst, String.class);
        List<String> restored = mapper.stringToObject(json, new TypeRef<ArrayList<String>>() {
        });
        assertType(restored, ArrayList.class);
        assertFalse(FLAG);
        assertEqualsDeep(org, restored);
    }

    protected void testConvertStringToHex(JSONMapper mapper) throws Exception {

        mapper.putConverterForSerialization(String.class, new JsonConverterForSerialization() {

            @Override
            public Object convertForSerialization(Object obj, Object mapper) {
                if (((String) obj).startsWith("___")) {
                    return obj;
                }
                FLAG = false;
                try {
                    return "___" + HexFormatter.byteArrayToHex(((String) obj).getBytes("UTF-8"));
                } catch (UnsupportedEncodingException e) {
                    throw new WTFException(e);
                }
            }

        });

        mapper.putConverterFromDeserialization(String.class, new JsonConverterForDeserialization() {

            @Override
            public Object convertForDeserialization(ConverterInput input, TypeRef<?> typeRef, Object mapper) {
                Object generic = input.getGenericRepresentation();
                if (generic == null) {
                    FLAG = false;
                    return input;
                }
                if (((String) generic).startsWith("___")) {
                    FLAG = false;
                    try {
                        return new String(HexFormatter.hexToByteArray(((String) generic).substring(3)), "UTF-8");
                    } catch (UnsupportedEncodingException e) {
                        throw new WTFException();
                    }
                }

                return input;
            }
        });
        FLAG = true;
        List<String> org;
        String json = mapper.objectToString(org = Arrays.asList("1", "2", "3"));
        assertFalse(FLAG);
        Object restoreTEst = mapper.stringToObject(json, TypeRef.OBJECT);
        assertType(restoreTEst, Map.class);
        List<String> restored = mapper.stringToObject(json, new TypeRef<List<String>>() {
        });
        assertType(restored, ArrayList.class);
        assertFalse(FLAG);
        assertEqualsDeep(org, restored);
        mapper.putConverterForSerialization(String.class, null);
        mapper.putConverterFromDeserialization(String.class, null);
    }

    // Adds a CONVERTER that converts each List to a map for serial and back to list in deserial.
    protected void testAddingAConverter(JSONMapper mapper) throws Exception {
        mapper.putConverterForSerialization(List.class, new JsonConverterForSerialization() {

            @Override
            public Object convertForSerialization(Object obj, Object mapper) {
                FLAG = false;
                HashMap<String, Object> map = new HashMap<String, Object>();
                for (int i = 0; i < ((List) obj).size(); i++) {
                    map.put("" + i, ((List) obj).get(i));
                }
                return map;
            }
        });

        mapper.putConverterFromDeserialization(List.class, new JsonConverterForDeserialization() {

            @Override
            public Object convertForDeserialization(ConverterInput input, TypeRef<?> typeRef, Object mapper) {
                Object object = input.getGenericRepresentation();
                FLAG = false;
                if (!(object instanceof Map)) {
                    // do not convert
                    return object;
                }
                HashMap<String, Object> map = (HashMap<String, Object>) object;
                int i = 0;
                ArrayList<Object> list = new ArrayList<Object>();
                while (true) {
                    if (map.containsKey(i + "")) {

                        Object entry = ((JSONMapper) mapper).convert(map.get(i + ""), new TypeRef<Object>(((ParameterizedType) typeRef.getType()).getActualTypeArguments()[0]) {

                        });
                        list.add(entry);
                        i++;
                    } else {
                        break;
                    }
                }
                return list;
            }
        });
        FLAG = true;
        List<String> org;
        String json = mapper.objectToString(org = Arrays.asList("1", "2", "3"));
        assertFalse(FLAG);
        Object restoreTEst = mapper.stringToObject(json, TypeRef.OBJECT);
        assertType(restoreTEst, Map.class);
        List<String> restored = mapper.stringToObject(json, new TypeRef<List<String>>() {
        });
        assertType(restored, ArrayList.class);
        assertFalse(FLAG);
        assertEqualsDeep(org, restored);
    }

    /**
     * Adds a serializer that stores Lists in MAP JSON String and back
     *
     * @param mapper
     * @throws Exception
     */
    @SuppressWarnings("deprecation")
    protected void testAddingASerializer(JSONMapper mapper) throws Exception {
        mapper.putSerializer(List.class, new JsonSerializer() {

            @Override
            public String toJSonString(Object object, Object mapper) {
                FLAG = false;
                HashMap<String, Object> map = new HashMap<String, Object>();
                for (int i = 0; i < ((List) object).size(); i++) {
                    map.put("" + i, ((List) object).get(i));
                }
                return ((JSONMapper) mapper).objectToString(map);

            }
        });

        mapper.putDeSerializer(List.class, new JsonDeSerializer() {

            @Override
            public Object toObject(DeSerializerInput input, TypeRef<?> typeRef, Object mapper) {
                FLAG = false;

                HashMap<String, String> map = ((JSONMapper) mapper).stringToObject(input.getJSONRepresentation(), TypeRef.HASHMAP_STRING);
                int i = 0;
                ArrayList<Object> list = new ArrayList<Object>();
                while (true) {
                    if (map.containsKey(i + "")) {
                        list.add(map.get(i + ""));
                        i++;
                    } else {
                        break;
                    }
                }
                return list;
            }
        });
        FLAG = true;
        List<String> org;
        String json = mapper.objectToString(org = Arrays.asList("1", "2", "3"));
        assertFalse(FLAG);
        Object restoreTEst = mapper.stringToObject(json, TypeRef.OBJECT);
        assertType(restoreTEst, Map.class);
        List<String> restored = mapper.stringToObject(json, new TypeRef<List<String>>() {
        });
        assertType(restored, ArrayList.class);
        assertFalse(FLAG);
        assertEqualsDeep(org, restored);
    }

    /**
     * Serializes and deserializes a class that implements the selfCOnverter INterface
     *
     * @param mapper
     * @throws Exception
     */
    protected void testSerializeDeserializeSelfConverter(JSONMapper mapper) throws Exception {
        {
            TestObjSelfConverter self = new TestObjSelfConverter();
            self.setTime(Time.timestamp());
            FLAG = true;
            String json = mapper.objectToString(self);
            assertFalse(FLAG);
            Object restoreTEst = mapper.stringToObject(json, TypeRef.OBJECT);
            assertType(restoreTEst, String.class);
            FLAG = true;
            TestObjSelfConverter restored = mapper.stringToObject(json, new TypeRef<TestObjSelfConverter<String>>() {
            });
            assertFalse(FLAG);
            if (!CompareUtils.equalsDeep(self, restored)) {
                throw new Exception("Self Converter failed");
            }
        }
    }

    /**
     * serializes and deserializes a class that implements the selfJSONSerializer interfaces
     *
     * @param mapper
     * @throws Exception
     */
    protected void testSerializeDeserializeSelfSerializer(JSONMapper mapper) throws Exception {
        {
            TestObjSelfSerializer self = new TestObjSelfSerializer();
            self.setTime(Time.timestamp());
            FLAG = true;
            String json = mapper.objectToString(self);
            assertFalse(FLAG);
            Object restoreTEst = mapper.stringToObject(json, TypeRef.OBJECT);
            assertType(restoreTEst, String.class);
            FLAG = true;
            TestObjSelfSerializer restored = mapper.stringToObject(json, new TypeRef<TestObjSelfSerializer<String>>() {
            });
            assertFalse(FLAG);
            if (!CompareUtils.equalsDeep(self, restored)) {
                throw new Exception("Self deSERIALIZER failed");
            }
        }
    }

    /**
     * Tests of the selfserializer interfaces throw an exception if not Implemented "directly"
     *
     * @param mapper
     */
    protected void testHiddenInterfaces(JSONMapper mapper) {
        {
            HiddenInterfaceTestObjSelfSerializer bad = new HiddenInterfaceTestObjSelfSerializer();
            bad.setTime(Time.timestamp());
            try {
                mapper.objectToString(bad);
                throw new Exception("This call must throw an exception, because the selfserializer interface is 'hidden'.");
            } catch (Exception e) {
                // e.printStackTrace();
            }
            TestObjSelfSerializer self = new TestObjSelfSerializer();
            self.setTime(Time.timestamp());
            try {
                mapper.stringToObject(mapper.objectToString(self), HiddenInterfaceTestObjSelfSerializer.TYPE);
                throw new Exception("This call must throw an exception, because the selfdeserializer interface is 'hidden'.");
            } catch (Exception e) {
                // e.printStackTrace();
            }
        }
    }

    public static class HiddenInterfaceTestObjSelfSerializer extends TestObjSelfSerializer {
        public static final org.appwork.storage.TypeRef<JsonDEserializerTests.HiddenInterfaceTestObjSelfSerializer> TYPE = new org.appwork.storage.TypeRef<JsonDEserializerTests.HiddenInterfaceTestObjSelfSerializer>(JsonDEserializerTests.HiddenInterfaceTestObjSelfSerializer.class) {
        };

        /**
         *
         */
        public HiddenInterfaceTestObjSelfSerializer() {
            // TODO Auto-generated constructor stub
        }
    }

    public static class TestObjSelfSerializer<T> implements Storable, SelfJSONSerializer, SelfJSONDeSerializer {

        public static final org.appwork.storage.TypeRef<JsonDEserializerTests.TestObjSelfSerializer> TYPE = new org.appwork.storage.TypeRef<JsonDEserializerTests.TestObjSelfSerializer>(JsonDEserializerTests.TestObjSelfSerializer.class) {
                                                                                                          };
        private T                                                                                    generic;

        public T getGeneric() {
            return generic;
        }

        public void setGeneric(T generic) {
            this.generic = generic;
        }

        /**
         *
         */
        public TestObjSelfSerializer() {
            // TODO Auto-generated constructor stub
        }

        /*
         * (non-Javadoc)
         *
         * @see org.appwork.storage.SelfJSONSerializer#toJSONStorable()
         */
        @Override
        public String serializeToJSON(Object mapper) {
            if (mapper == null) {
                throw new WTFException("Mapper Context is missing");
            }
            if (CONTEXT != mapper) {
                throw new WTFException("Mapper Context does not match Expected: " + CONTEXT + " IS: " + mapper);
            }
            FLAG = false;
            return ((JSONMapper) mapper).objectToString(new SimpleDateFormat("dd.MM.yyyy HH:mm:ss.SSS").format(new Date(getTime())));
        }

        private long time;

        public long getTime() {
            return time;
        }

        public void setTime(long time) {
            this.time = time;
        }

        /*
         * (non-Javadoc)
         *
         * @see org.appwork.storage.SelfJSONDeSerializer#deserializeFromJSON()
         */
        @Override
        public void deserializeFromJSON(DeSerializerInput input, Object mapper) {
            if (mapper == null) {
                throw new WTFException("Mapper Context is missing");
            }
            if (CONTEXT != mapper) {
                throw new WTFException("Mapper Context does not match Expected: " + CONTEXT + " IS: " + mapper);
            }
            try {

                setTime(new SimpleDateFormat("dd.MM.yyyy HH:mm:ss.SSS").parse(((JSONMapper) mapper).stringToObject(input.getJSONRepresentation(), TypeRef.STRING)).getTime());
                FLAG = false;
            } catch (ParseException e) {
                throw new WTFException(e);
            }

        }
    }

    public static class TestObjSelfConverter<T> implements Storable, SelfJsonConverterForSerialization, SelfJsonConverterForDeserialization {

        public static final org.appwork.storage.TypeRef<JsonDEserializerTests.TestObjSelfConverter> TYPE = new org.appwork.storage.TypeRef<JsonDEserializerTests.TestObjSelfConverter>(JsonDEserializerTests.TestObjSelfConverter.class) {
                                                                                                         };
        private T                                                                                   generic;

        public T getGeneric() {
            return generic;
        }

        public void setGeneric(T generic) {
            this.generic = generic;
        }

        /**
         *
         */
        public TestObjSelfConverter() {
            // TODO Auto-generated constructor stub
        }

        private long time;

        public long getTime() {
            return time;
        }

        public void setTime(long time) {
            this.time = time;
        }

        /*
         * (non-Javadoc)
         *
         * @see org.appwork.storage.SelfJsonConverterFromObjectToSerializable#convertToSerializable(java.lang.Object)
         */
        @Override
        public Object convertForSerialization(Object mapperContext) {
            if (mapperContext == null) {
                throw new WTFException("Mapper Context is missing");
            }
            if (CONTEXT != mapperContext) {
                throw new WTFException("Mapper Context does not match Expected: " + CONTEXT + " IS: " + mapperContext);
            }
            FLAG = false;
            return new SimpleDateFormat("dd.MM.yyyy HH:mm:ss.SSS").format(new Date(getTime()));
        }

        /*
         * (non-Javadoc)
         *
         * @see org.appwork.storage.SelfJsonConverterFromDeserializableToObject#restoreFromDeserializedObject(java.lang.Object,
         * java.lang.Object)
         */
        @Override
        public void restoreForDeserialization(ConverterInput input, Object mapperContext) {
            try {
                Object deserializedObject = input.getGenericRepresentation();
                assertNotNull(deserializedObject);

                assertType(deserializedObject, String.class);

                if (mapperContext == null) {
                    throw new WTFException("Mapper Context is missing");
                }
                if (CONTEXT != mapperContext) {
                    throw new WTFException("Mapper Context does not match Expected: " + CONTEXT + " IS: " + mapperContext);
                }
                try {
                    setTime(new SimpleDateFormat("dd.MM.yyyy HH:mm:ss.SSS").parse((String) deserializedObject).getTime());
                    FLAG = false;
                } catch (ParseException e) {
                    throw new WTFException(e);
                }

            } catch (Exception e1) {
                throw new WTFException(e1);
            }
        }
    }
}
