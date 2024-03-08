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
package org.appwork.storage.flexijson.mapper;

import java.lang.annotation.Annotation;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.remoteapi.annotations.ApiDoc;
import org.appwork.remoteapi.annotations.ApiDocExample;
import org.appwork.storage.StorableAvailableSince;
import org.appwork.storage.StorableDateFormat;
import org.appwork.storage.StorableDeprecatedSince;
import org.appwork.storage.StorableDoc;
import org.appwork.storage.StorableExample;
import org.appwork.storage.StorableHidden;
import org.appwork.storage.StorableLink;
import org.appwork.storage.StorableOrder;
import org.appwork.storage.StorableSee;
import org.appwork.storage.StorableTypeAlternatives;
import org.appwork.storage.StorableUnique;
import org.appwork.storage.StorableValidateMandatoryInJson;
import org.appwork.storage.StorableValidateNotNull;
import org.appwork.storage.TypeRef;
import org.appwork.storage.flexijson.FlexiComment;
import org.appwork.storage.flexijson.FlexiCommentJsonNode;
import org.appwork.storage.flexijson.FlexiJSonArray;
import org.appwork.storage.flexijson.FlexiJSonComments;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.FlexiJSonObject;
import org.appwork.storage.flexijson.FlexiJSonValue;
import org.appwork.storage.flexijson.FlexiUtils;
import org.appwork.storage.flexijson.KeyValueElement;
import org.appwork.storage.flexijson.mapper.interfacestorage.InterfaceStorage;
import org.appwork.storage.flexijson.mapper.typemapper.DateMapper;
import org.appwork.storage.flexijson.mapper.typemapper.LocaleMapMapper;
import org.appwork.storage.flexijson.mapper.typemapper.ReadableBytesMapper;
import org.appwork.storage.flexijson.mapper.typemapper.TimeSpanMapper;
import org.appwork.storage.flexijson.stringify.FlexiJSonStringBuilder;
import org.appwork.storage.simplejson.mapper.ClassCache;
import org.appwork.storage.simplejson.mapper.Getter;
import org.appwork.storage.simplejson.mapper.Setter;
import org.appwork.storage.validator.classvalidator.StorableClassValidator1;
import org.appwork.storage.validator.classvalidator.StorableClassValidator2;
import org.appwork.storage.validator.classvalidator.StorableClassValidator3;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.StringUtils;
import org.appwork.utils.reflection.Clazz;
import org.appwork.utils.reflection.CompiledType;

/**
 * @author thomas
 *
 */
public class FlexiJSonMapper {
    /**
     *
     */
    private static final String                     TYPE          = "Type: ";
    private static final ArrayList<FlexiTypeMapper> DEFAULTMAPPER = new ArrayList<FlexiTypeMapper>();
    static {
        DEFAULTMAPPER.add(new DateMapper());
        DEFAULTMAPPER.add(new TimeSpanMapper());
        DEFAULTMAPPER.add(new ReadableBytesMapper());
        DEFAULTMAPPER.add(new LocaleMapMapper());
    }

    public static void addDefaultMapper(FlexiTypeMapper mapper) {
        DEFAULTMAPPER.add(mapper);
    }

    public static final ThreadLocal<ArrayList<FlexiTypeMapper>> THREAD_MAPPERS = new ThreadLocal<ArrayList<FlexiTypeMapper>>();

    /**
     * @param urlMapper
     * @return
     */
    public static FlexiTypeMapper addThreadMapper(FlexiTypeMapper newMapper) {
        if (THREAD_MAPPERS.get() == null) {
            THREAD_MAPPERS.set(new ArrayList<FlexiTypeMapper>());
        }
        for (FlexiTypeMapper existing : THREAD_MAPPERS.get()) {
            if (existing.getClass() == newMapper.getClass()) {
                return null;
            }
        }
        THREAD_MAPPERS.get().add(newMapper);
        return newMapper;
    }

    public static boolean removeThreadMapper(FlexiTypeMapper newMapper) {
        if (newMapper == null || THREAD_MAPPERS.get() == null) {
            return false;
        }
        return THREAD_MAPPERS.get().remove(newMapper);
    }

    private boolean                            ignorePrimitiveNullMapping    = false;
    private boolean                            ignoreIllegalArgumentMappings = false;
    /**
     * @param value
     * @param type
     * @return
     */
    private boolean                            ignoreIllegalEnumMappings     = false;
    protected final ArrayList<FlexiTypeMapper> typeMapper;
    protected CompiledType                     autoMapFlexiJSonObjectClass   = CompiledType.create(new TypeRef<LinkedHashMap<String, Object>>() {
                                                                             });
    protected CompiledType                     autoMapFlexiJSonArrayclass    = CompiledType.create(new TypeRef<LinkedList<Object>>() {
                                                                             });
    protected CompiledType                     autoMapCollectionInterface    = autoMapFlexiJSonArrayclass;
    protected CompiledType                     autoMapMapInterface           = autoMapFlexiJSonObjectClass;
    protected CompiledType                     autoMapSetInterface           = CompiledType.create(new TypeRef<LinkedHashSet<Object>>() {
                                                                             });;
    private ArrayList<FlexiMapperException>    exceptions;
    private boolean                            ignoreDefaultValuesEnabled;
    private boolean                            tagDefaultValuesEnabled;

    public FlexiJSonMapper() {
        typeMapper = new ArrayList<FlexiTypeMapper>(DEFAULTMAPPER);
    }

    /**
     * Adds the new mapper with highest priority
     *
     * @param <T>
     * @param class1
     * @param fileMapper
     */
    public void addMapper(final FlexiTypeMapper mapper) {
        typeMapper.add(0, mapper);
    }

    /**
     * @param class1
     */
    public void removeMapperByType(Class<?> class1) {
        for (Iterator<FlexiTypeMapper> it = typeMapper.iterator(); it.hasNext();) {
            if (Clazz.isInstanceof(it.next().getClass(), class1)) {
                it.remove();
            }
        }
    }

    public void setTypeMapper(List<FlexiTypeMapper> customs) {
        typeMapper.clear();
        typeMapper.addAll(customs);
    }

    public List<FlexiTypeMapper> getTypeMapper() {
        return Collections.unmodifiableList(typeMapper);
    }

    /**
     * @param inst
     * @param typeRef
     * @return
     * @throws FlexiMapperException
     */
    public FlexiJSonNode objectToJsonNode(final Object obj, CompiledType type) throws FlexiMapperException {
        return objectToJsonNode(null, obj, createObjectToJsonContext(type));
    }

    public FlexiJSonNode objectToJsonNode(final Object obj) throws FlexiMapperException {
        return objectToJsonNode(null, obj, null);
    }

    /**
     * @param reference
     *            TODO
     * @param obj
     * @param context
     *            TODO
     * @return
     * @throws FlexiMapperException
     */
    @SuppressWarnings("unchecked")
    public FlexiJSonNode objectToJsonNode(Getter reference, Object obj, DefaultObjectToJsonContext context) throws FlexiMapperException {
        if (context == null) {
            context = createObjectToJsonContext(obj == null ? CompiledType.OBJECT : CompiledType.create(obj.getClass()));
        }
        if (obj instanceof FlexiJSonNode) {
            return (FlexiJSonNode) obj;
        }
        final FlexiJSonNode mapped = handleMapperObjectToJsonNode(reference, obj, context);
        if (mapped != null) {
            return mapped;
        }
        final CompiledType cType = getTargetClass(obj, context);
        if (cType.isPrimitiveWrapper()) {
            // if (obj == null) {
            // return createFlexiJSonValue((String) null);
            // }
            if (obj == null) {
                return createFlexiJSonValue();
            } else if (cType.isBoolean()) {
                return createFlexiJSonValue((Boolean) obj);
            } else if (cType.isCharacter()) {
                return createFlexiJSonValue(0 + ((Character) obj).charValue());
            } else if (cType.isNumber()) {
                return createFlexiJSonValue((Number) obj);
            } else {
                throw new WTFException("Unknown Primitive Type: " + cType);
            }
        } else if (cType.isPrimitive()) {
            // obj cannot be null
            if (cType.isBoolean()) {
                return createFlexiJSonValue(((Boolean) obj).booleanValue());
            } else if (cType.isCharacter()) {
                return createFlexiJSonValue(0 + ((Character) obj).charValue());
            } else if (cType.isNumber()) {
                return createFlexiJSonValue((Number) obj);
            } else {
                throw new WTFException("Unknown Primitive Type: " + cType);
            }
        } else if (cType.isString()) {
            return createFlexiJSonValue((String) obj);
        } else if (cType.isInstanceOf(CharSequence.class)) {
            return createFlexiJSonValue(obj == null ? (String) null : ((CharSequence) obj).toString());
        } else if (cType.isEnum(true)) {
            FlexiJSonValue ret = obj == null ? createFlexiJSonValue((String) null) : createFlexiJSonValue(String.valueOf(obj));
            FlexiJSonComments comments = addEnumCommentByAnnotations(null, obj, cType);
            ret.addCommentsAfter(comments);
            return ret;
        } else if (cType.isMap()) {
            final FlexiJSonNode node = obj == null ? createFlexiJSonValue() : createFlexiJSonObject();
            addClassHeaderCommentsByAnnotations(node, cType);
            CompiledType[] compTypes = cType.getComponentTypes(Map.class);
            if (compTypes.length == 2 && compTypes[1].isGenericsResolved()) {
                addClassHeaderCommentsByAnnotations(node, compTypes[1]);
            }
            if (obj == null) {
                return node;
            }
            final FlexiJSonObject ret = (FlexiJSonObject) node;
            Entry<Object, Object> next;
            for (final Iterator<Entry<Object, Object>> it = ((Map<Object, Object>) obj).entrySet().iterator(); it.hasNext();) {
                next = it.next();
                if (!(next.getKey() instanceof String)) {
                    returnFallbackOrThrowException(new FlexiMapperException(ret, cType, "Map keys have to be Strings: " + cType.type + " Keyclass:" + (next.getKey() == null ? "<null>" : next.getKey().getClass())));
                }
                CompiledType type = CompiledType.OBJECT;
                if (compTypes.length == 2 && compTypes[1].isGenericsResolved()) {
                    type = compTypes[1];
                }
                if (type.isObject() && next.getValue() != null) {
                    type = CompiledType.create(next.getValue().getClass());
                }
                try {
                    context.add(type, next.getKey());
                    ret.add(createKeyValueElement(ret, next.getKey().toString(), objectToJsonNode(reference, next.getValue(), context)));
                } finally {
                    context.removeLast();
                }
            }
            return ret;
        } else if (cType.isCollection()) {
            final FlexiJSonNode node = obj == null ? createFlexiJSonValue() : createFlexiJSonArray(((Collection<?>) obj).size());
            addClassHeaderCommentsByAnnotations(node, cType);
            CompiledType[] compTypes = cType.getComponentTypes(Collection.class);
            if (compTypes.length == 1 && compTypes[0].isGenericsResolved()) {
                addClassHeaderCommentsByAnnotations(node, compTypes[0]);
            }
            if (obj == null) {
                return node;
            }
            final FlexiJSonArray ret = (FlexiJSonArray) node;
            int i = 0;
            for (final Object o : (Collection<?>) obj) {
                CompiledType type = CompiledType.OBJECT;
                if (compTypes.length == 1 && compTypes[0].isGenericsResolved()) {
                    type = compTypes[0];
                }
                if (type.isObject() && o != null) {
                    type = CompiledType.create(o.getClass());
                }
                try {
                    context.add(type, i);
                    ret.add(objectToJsonNode(reference, o, context));
                } finally {
                    context.removeLast();
                    i++;
                }
            }
            return ret;
        } else if (cType.isArray()) {
            int length = obj == null ? 0 : Array.getLength(obj);
            final FlexiJSonNode node = obj == null ? createFlexiJSonValue() : createFlexiJSonArray(length);
            addClassHeaderCommentsByAnnotations(node, cType);
            CompiledType[] compTypes = cType.componentTypes;
            for (CompiledType c : compTypes) {
                addClassHeaderCommentsByAnnotations(node, c);
            }
            if (obj == null) {
                return node;
            }
            final FlexiJSonArray ret = (FlexiJSonArray) node;
            for (int i = 0; i < length; i++) {
                Object o = Array.get(obj, i);
                CompiledType type = CompiledType.OBJECT;
                if (compTypes.length == 1 && compTypes[0].isGenericsResolved()) {
                    type = compTypes[0];
                }
                if (type.isObject() && o != null) {
                    type = CompiledType.create(o.getClass());
                }
                try {
                    context.add(type, i);
                    ret.add(objectToJsonNode(reference, o, context));
                } finally {
                    context.removeLast();
                }
            }
            return ret;
        } else/* if (obj instanceof Storable) */ {
            InterfaceStorage<Object> is = null;
            if (obj instanceof Proxy && isIncludeInterfaceStorageBackendNode()) {
                is = InterfaceStorage.get(obj);
            }
            final FlexiJSonNode node = obj == null ? createFlexiJSonValue() : createFlexiJSonObject();
            if (is != null) {
                node.addCommentsAfter(is.backendNode.getCommentsAfter());
                node.addCommentsBefore(is.backendNode.getCommentsBefore());
                cleanUpComments(node.getCommentsAfter());
                cleanUpComments(node.getCommentsBefore());
            }
            addClassHeaderCommentsByAnnotations(node, cType);
            CompiledType[] compTypes = cType.componentTypes;
            for (CompiledType c : compTypes) {
                addClassHeaderCommentsByAnnotations(node, c);
            }
            if (obj == null) {
                return node;
            }
            final FlexiJSonObject ret = (FlexiJSonObject) node;
            if (is != null) {
                ret.addCommentsInside(is.backendNode.getCommentsInside());
                cleanUpComments(ret.getCommentsInside());
            }
            try {
                Object empty = null;
                ClassCache cc = cType.getClassCache();
                Collection<Getter> getters = cc.getGetterMap().values();
                HashMap<KeyValueElement, Integer> orderMap = null;
                for (final Getter g : getters) {
                    CompiledType t = CompiledType.create(g.getMethod().getGenericReturnType(), cType.type);
                    try {
                        context.add(t, g.getKey());
                        if (cType.getClassCache().getAnnotations(g.key, StorableHidden.class).size() > 0) {
                            continue;
                        }
                        if (isIgnoreProperty(obj, cType, context, g)) {
                            continue;
                        }
                        Object value = g.getValue(obj);
                        if (isIgnoreDefaultValuesEnabled(obj, cType, g)) {
                            if (empty == null) {
                                empty = createDefaultObject(obj, cType, ret, g, cType.getClassCache().getSetter(g.key), context);
                            }
                            if (empty != null) {
                                if (CompareUtils.equalsDeep(g.getValue(empty), value)) {
                                    continue;
                                }
                            }
                        }
                        FlexiJSonNode subNode = objectToJsonNode(g, value, context);
                        if (isTagDefaultValuesEnabled(obj, cType, g)) {
                            if (empty == null) {
                                empty = createDefaultObject(obj, cType, ret, g, cType.getClassCache().getSetter(g.key), context);
                            }
                            if (empty != null) {
                                if (CompareUtils.equalsDeep(g.getValue(empty), value)) {
                                    subNode.tag(FlexiMapperTags.DEFAULT_VALUE);
                                }
                            }
                        }
                        KeyValueElement element;
                        if (isAddDefaultValueCommentEnabled(obj, cType, g)) {
                            if (empty == null) {
                                empty = createDefaultObject(obj, cType, ret, g, cType.getClassCache().getSetter(g.key), context);
                            }
                            if (empty == null) {
                                element = (methodOrFieldAnnotationsToComments(g, context, createKeyValueElement(ret, g.getKey(), subNode), null, false));
                            } else {
                                element = (methodOrFieldAnnotationsToComments(g, context, createKeyValueElement(ret, g.getKey(), subNode), g.getValue(empty), true));
                            }
                        } else {
                            element = (methodOrFieldAnnotationsToComments(g, context, createKeyValueElement(ret, g.getKey(), subNode), null, false));
                        }
                        if (is != null) {
                            KeyValueElement backEndElement = is.backendNode.getElement(g.key);
                            if (backEndElement != null) {
                                element.addCommentsAfterKey(backEndElement.getCommentsAfterKey());
                                element.addCommentsBeforeKey(backEndElement.getCommentsBeforeKey(), true);
                                cleanUpComments(element.getCommentsBeforeKey());
                                cleanUpComments(element.getCommentsAfterKey());
                            }
                        }
                        List<StorableOrder> orders = cc.getAnnotations(g.key, StorableOrder.class);
                        if (orders.size() > 0) {
                            if (orderMap == null) {
                                orderMap = new HashMap<KeyValueElement, Integer>();
                            }
                            orderMap.put(element, orders.get(0).value());
                        }
                        ret.add(element);
                    } catch (IllegalArgumentException e) {
                        returnFallbackOrThrowException(new FlexiMapperException(ret, cType, e.getMessage(), e));
                    } catch (IllegalAccessException e) {
                        returnFallbackOrThrowException(new FlexiMapperException(ret, cType, e.getMessage(), e));
                    } catch (InvocationTargetException e) {
                        returnFallbackOrThrowException(new FlexiMapperException(ret, cType, e.getMessage(), e));
                    } finally {
                        context.removeLast();
                    }
                }
                if (is != null) {
                    // Convert backendNodes that have no getter
                    for (KeyValueElement el : is.backendNode.getElements()) {
                        if (el.getKey() == null) {
                            KeyValueElement emptyKeyValueElement = createKeyValueElement(ret, el.getKey(), el.getValue());
                            emptyKeyValueElement.addCommentsAfterKey(el.getCommentsAfterKey());
                            emptyKeyValueElement.addCommentsBeforeKey(el.getCommentsBeforeKey(), true);
                            cleanUpComments(emptyKeyValueElement.getCommentsBeforeKey());
                            cleanUpComments(emptyKeyValueElement.getCommentsAfterKey());
                            ret.add(emptyKeyValueElement);
                            continue;
                        }
                        if (!cc.getGetterMap().containsKey(el.getKey())) {
                            KeyValueElement element = createKeyValueElement(ret, el.getKey(), el.getValue());
                            element.addCommentsAfterKey(el.getCommentsAfterKey());
                            element.addCommentsBeforeKey(el.getCommentsBeforeKey(), true);
                            cleanUpComments(element.getCommentsBeforeKey());
                            cleanUpComments(element.getCommentsAfterKey());
                            ret.add(element);
                        }
                    }
                }
                if (orderMap != null) {
                    final HashMap<KeyValueElement, Integer> fOrderMap = orderMap;
                    ret.sort(new Comparator<KeyValueElement>() {
                        @Override
                        public int compare(KeyValueElement o1, KeyValueElement o2) {
                            Integer id1 = fOrderMap.get(o1);
                            Integer id2 = fOrderMap.get(o2);
                            if (id1 == null) {
                                id1 = 0;
                            }
                            if (id2 == null) {
                                id2 = 0;
                            }
                            int result = CompareUtils.compareComparable(id1, id2);
                            if (result == 1) {
                                return CompareUtils.compareComparable(o1.getKey(), o2.getKey());
                            } else {
                                return result;
                            }
                        }
                    });
                }
            } catch (SecurityException e) {
                returnFallbackOrThrowException(new FlexiMapperException(ret, cType, e.getMessage(), e));
            } catch (NoSuchMethodException e) {
                returnFallbackOrThrowException(new FlexiMapperException(ret, cType, e.getMessage(), e));
            }
            return ret;
        }
    }

    /**
     * @param compiledType
     * @return
     */
    protected DefaultObjectToJsonContext createObjectToJsonContext(CompiledType compiledType) {
        return new DefaultObjectToJsonContext(compiledType);
    }

    /**
     * @param obj
     * @param cType
     * @param typeHirarchy
     * @param g
     * @return
     */
    protected boolean isIgnoreProperty(Object obj, CompiledType cType, DefaultObjectToJsonContext context, Getter g) {
        // can be used to ignore properties during serialisation/obj -> Json mapping
        return false;
    }

    /**
     * @param commentsBeforeKey
     */
    private void cleanUpComments(FlexiJSonComments comments) {
        if (comments == null) {
            return;
        }
        comments.cleanUpDupes();
    }

    /**
     * @return
     */
    protected boolean isIncludeInterfaceStorageBackendNode() {
        return true;
    }

    /**
     * @param is
     * @param obj
     * @param cType
     * @param typeHirarchy
     * @return
     * @throws FlexiMapperException
     */
    protected FlexiJSonObject interfaceStorageToNode(InterfaceStorage<Object> is, Object obj, CompiledType cType, DefaultObjectToJsonContext context) throws FlexiMapperException {
        final FlexiJSonObject ret = createFlexiJSonObject();
        addClassHeaderCommentsByAnnotations(ret, cType);
        CompiledType[] compTypes = cType.componentTypes;
        for (CompiledType c : compTypes) {
            addClassHeaderCommentsByAnnotations(ret, c);
        }
        ret.addCommentsAfter(is.backendNode.getCommentsAfter());
        ret.addCommentsBefore(is.backendNode.getCommentsBefore());
        ret.addCommentsInside(is.backendNode.getCommentsInside());
        try {
            Object empty = null;
            Collection<Getter> getters = cType.getClassCache().getGetterMap().values();
            for (final Getter g : getters) {
                CompiledType t = CompiledType.create(g.getMethod().getGenericReturnType(), cType.type);
                try {
                    context.add(t, g.getKey());
                    if (cType.getClassCache().getAnnotations(g.key, StorableHidden.class).size() > 0) {
                        continue;
                    }
                    Object value = g.getValue(obj);
                    if (isIgnoreDefaultValuesEnabled(obj, cType, g)) {
                        if (empty == null) {
                            empty = createDefaultObject(obj, cType, ret, g, cType.getClassCache().getSetter(g.key), context);
                        }
                        if (empty != null) {
                            if (CompareUtils.equalsDeep(g.getValue(empty), value)) {
                                continue;
                            }
                        }
                    }
                    FlexiJSonNode subNode = objectToJsonNode(g, value, context);
                    if (isTagDefaultValuesEnabled(obj, cType, g)) {
                        if (empty == null) {
                            empty = createDefaultObject(obj, cType, ret, g, cType.getClassCache().getSetter(g.key), context);
                        }
                        if (empty != null) {
                            if (CompareUtils.equalsDeep(g.getValue(empty), value)) {
                                subNode.tag(FlexiMapperTags.DEFAULT_VALUE);
                            }
                        }
                    }
                    if (isAddDefaultValueCommentEnabled(obj, cType, g)) {
                        if (empty == null) {
                            empty = createDefaultObject(obj, cType, ret, g, cType.getClassCache().getSetter(g.key), context);
                        }
                        if (empty == null) {
                            ret.add(methodOrFieldAnnotationsToComments(g, context, createKeyValueElement(ret, g.getKey(), subNode), null, false));
                        } else {
                            ret.add(methodOrFieldAnnotationsToComments(g, context, createKeyValueElement(ret, g.getKey(), subNode), g.getValue(empty), true));
                        }
                    } else {
                        ret.add(methodOrFieldAnnotationsToComments(g, context, createKeyValueElement(ret, g.getKey(), subNode), null, false));
                    }
                } catch (IllegalArgumentException e) {
                    returnFallbackOrThrowException(new FlexiMapperException(ret, cType, e.getMessage(), e));
                } catch (IllegalAccessException e) {
                    returnFallbackOrThrowException(new FlexiMapperException(ret, cType, e.getMessage(), e));
                } catch (InvocationTargetException e) {
                    returnFallbackOrThrowException(new FlexiMapperException(ret, cType, e.getMessage(), e));
                } finally {
                    context.removeLast();
                }
            }
        } catch (SecurityException e) {
            returnFallbackOrThrowException(new FlexiMapperException(ret, cType, e.getMessage(), e));
        } catch (NoSuchMethodException e) {
            returnFallbackOrThrowException(new FlexiMapperException(ret, cType, e.getMessage(), e));
        }
        return ret;
    }

    public FlexiJSonArray createFlexiJSonArray(int length) {
        return new FlexiJSonArray(length);
    }

    /**
     * @param
     * @param obj
     * @param clazz
     * @param class1
     * @return
     * @throws FlexiMapperException
     */
    private FlexiJSonComments addEnumCommentByAnnotations(FlexiJSonComments comments, Object obj, CompiledType cType) throws FlexiMapperException {
        if (obj == null || !isAnnotationCommentsEnabled()) {
            return comments;
        }
        try {
            Field field = cType.raw.getField(((Enum) obj).name());
            for (Annotation an : field.getAnnotations()) {
                comments = addComment(comments, an, null);
            }
        } catch (NoSuchFieldException e) {
        } catch (SecurityException e) {
        }
        return comments;
    }

    /**
     * returns the class that should be used to serialize obj. By default, this is the objects class. Override to serialize as something
     * different.
     *
     * @param obj
     * @param context
     * @return
     */
    protected CompiledType getTargetClass(Object obj, DefaultObjectToJsonContext context) {
        if (obj == null) {
            return context.getLast();
        }
        if (obj instanceof Proxy) {
            InvocationHandler handler = Proxy.getInvocationHandler(obj);
            if (handler instanceof InterfaceStorage) {
                return ((InterfaceStorage) handler).cType;
            }
        }
        // not sure if we need the type hirarchy as context here. add typeHirarchy.getLast().type as second parameter if we get issues with
        // unreslvable Generics
        return CompiledType.create(obj.getClass());
    }

    /**
     * @param parent
     * @param string
     * @param objectToJsonNode
     * @return
     */
    protected KeyValueElement createKeyValueElement(FlexiJSonObject parent, String key, FlexiJSonNode node) {
        return new KeyValueElement(parent, key, node);
    }

    protected FlexiJSonNode handleMapperObjectToJsonNode(Getter getter, final Object obj, DefaultObjectToJsonContext context) throws FlexiMapperException {
        for (FlexiTypeMapper mapper : typeMapper) {
            if (mapper.canConvert2Json(obj, getter)) {
                return mapper.obj2JSon(this, obj, getter, context);
            }
        }
        ArrayList<FlexiTypeMapper> mappers = THREAD_MAPPERS.get();
        if (mappers != null) {
            for (FlexiTypeMapper mapper : mappers) {
                if (mapper.canConvert2Json(obj, getter)) {
                    return mapper.obj2JSon(this, obj, getter, context);
                }
            }
        }
        return null;
    }

    /**
     * overwrite if you want to add default values Tags in each node.
     *
     * @param obj
     * @param clazz
     * @param g
     * @return
     */
    protected boolean isTagDefaultValuesEnabled(Object obj, CompiledType cType, Getter g) {
        return tagDefaultValuesEnabled;
    }

    public boolean isTagDefaultValuesEnabled() {
        return tagDefaultValuesEnabled;
    }

    public void setTagDefaultValuesEnabled(boolean tagDefaultValuesEnabled) {
        this.tagDefaultValuesEnabled = tagDefaultValuesEnabled;
    }

    protected Object createDefaultObject(final Object obj, CompiledType cType, final FlexiJSonObject ret, Getter getter, Setter setter, DefaultObjectToJsonContext context) throws NoSuchMethodException, FlexiMapperException {
        if (obj instanceof Proxy) {
            InvocationHandler handler = Proxy.getInvocationHandler(obj);
            if (handler instanceof InterfaceStorage) {
                FlexiJSonObject dummy = new FlexiJSonObject();
                return initProxy(cType, dummy);
            }
        }
        /**
         * give the type mappers a try
         */
        for (FlexiTypeMapper mapper : typeMapper) {
            FlexiJSonNode tryNode = createFlexiJSonValue((String) null);
            if (mapper.canConvert2Object(tryNode, cType, setter)) {
                return mapper.json2Obj(this, tryNode, cType, setter);
            }
            tryNode = createFlexiJSonObject();
            if (mapper.canConvert2Object(tryNode, cType, setter)) {
                return mapper.json2Obj(this, tryNode, cType, setter);
            }
            tryNode = createFlexiJSonArray(0);
            if (mapper.canConvert2Object(tryNode, cType, setter)) {
                return mapper.json2Obj(this, tryNode, cType, setter);
            }
        }
        try {
            return cType.newInstance();
        } catch (InstantiationException e) {
            returnFallbackOrThrowException(new FlexiMapperException(ret, cType, e.getMessage(), e));
        } catch (IllegalAccessException e) {
            returnFallbackOrThrowException(new FlexiMapperException(ret, cType, e.getMessage(), e));
        } catch (IllegalArgumentException e) {
            returnFallbackOrThrowException(new FlexiMapperException(ret, cType, e.getMessage(), e));
        }
        return null;
    }

    private Object initProxy(CompiledType cType, FlexiJSonObject obj) throws NoSuchMethodException {
        Object proxy = createProxy(cType, obj);
        ((InterfaceStorage) Proxy.getInvocationHandler(proxy)).setStorage(proxy);
        return proxy;
    }

    /**
     * @param obj
     * @param clazz
     * @param g
     * @return
     */
    protected boolean isAddDefaultValueCommentEnabled(Object obj, CompiledType cType, Getter g) {
        return false;
    }

    /**
     * @param obj
     * @param clazz
     * @param g
     * @return
     */
    public boolean isIgnoreDefaultValuesEnabled(Object obj, CompiledType cType, Getter g) {
        return ignoreDefaultValuesEnabled;
    }

    /**
     * if true, the mapper will igore fields if their value is the default value (empty constructor) of the owner object
     *
     * @param ignoreDefaultValuesEnabled
     */
    public void setIgnoreDefaultValuesEnabled(boolean ignoreDefaultValuesEnabled) {
        this.ignoreDefaultValuesEnabled = ignoreDefaultValuesEnabled;
    }

    public FlexiJSonMapper ignoreDefaultValuesEnabled(boolean ignoreDefaultValuesEnabled) {
        this.ignoreDefaultValuesEnabled = ignoreDefaultValuesEnabled;
        return this;
    }

    /**
     * @return
     */
    protected boolean isTypeCommentsEnabled() {
        return false;
    }

    /**
     * @param g
     * @param context
     * @param create
     * @param defaultValue
     *            TODO
     * @param addDefaultValueAnnotation
     * @return
     * @throws FlexiMapperException
     */
    protected KeyValueElement methodOrFieldAnnotationsToComments(Getter g, DefaultObjectToJsonContext context, KeyValueElement create, Object defaultValue, boolean addDefaultValueAnnotation) throws FlexiMapperException {
        // / move comments to KeyValueElement
        FlexiJSonComments comments = create.getValue().getCommentsBefore();
        Class<?> cls = g.getMethod().getDeclaringClass();
        create.getValue().setCommentsBefore(null);
        if (isTypeCommentsEnabled()) {
            if (addDefaultValueAnnotation) {
                comments = addComment(comments, "Default: " + new FlexiJSonStringBuilder().toJSONString(new FlexiJSonMapper().objectToJsonNode(defaultValue)), FlexiMapperTags.DEFAULT_VALUE);
            }
            comments = addComment(comments, TYPE + typeToString(context.getCompiledType()), FlexiMapperTags.TYPE);
        } else if (addDefaultValueAnnotation) {
            comments = addComment(comments, "Default: " + defaultValue, FlexiMapperTags.DEFAULT_VALUE);
        }
        // if (isDefaultValueComment(obj, clazz, g)) {
        // comments = addComment(comments, typeToString(typeHirarchy, g.getMethod().getDeclaringClass()));
        // }
        if (!isAnnotationCommentsEnabled()) {
            return create;
        }
        comments = addCommentByAnnotations(g, comments, cls);
        comments = addEnumOptionsComments(comments, context.getLast());
        if (comments != null) {
            create.setCommentsBeforeKey(comments);
        }
        return create;
    }

    protected FlexiJSonComments addCommentByAnnotations(Getter g, FlexiJSonComments comments, Class<?> cls) throws FlexiMapperException {
        Class<?> targetClass = ReflectionUtils.getRaw(g.type);
        if (targetClass != null) {
            for (Annotation a : targetClass.getAnnotations()) {
                comments = addComment(comments, a, null);
            }
        }
        try {
            for (Type c : ClassCache.getClassCache(cls).getTypeHierarchy()) {
                CompiledType ct = CompiledType.create(c);
                try {
                    if (ct.raw != null) {
                        Method method = ct.raw.getDeclaredMethod(g.getMethod().getName(), g.getMethod().getParameterTypes());
                        for (Annotation a : method.getAnnotations()) {
                            comments = addComment(comments, a, null);
                        }
                        if (!ct.raw.isInterface()) {
                            Field field = ct.raw.getDeclaredField(g.getKey());
                            for (Annotation a : field.getAnnotations()) {
                                comments = addComment(comments, a, null);
                            }
                        }
                    }
                } catch (NoSuchMethodException e) {
                } catch (NoSuchFieldException e) {
                } catch (SecurityException e) {
                }
            }
        } catch (SecurityException e) {
        } catch (NoSuchMethodException e) {
        }
        return comments;
    }

    protected FlexiJSonComments addEnumOptionsComments(FlexiJSonComments comments, CompiledType cType) {
        // Skip anonymous enums
        while (cType != null && cType.raw == null) {
            cType = cType.superType;
        }
        if (cType.isEnum(false) && isEnumOptionsCommentsEnabled(cType)) {
            try {
                Object[] options = ReflectionUtils.getEnumValues((Class<? extends Enum>) cType.raw);
                try {
                    String str = "Options: ";
                    int max = 0;
                    for (Object o : options) {
                        max = Math.max(max, o.toString().length());
                    }
                    for (Object o : options) {
                        // Field field = ((Class<? extends Enum>) (type)).getDeclaredField(o.toString());
                        FlexiJSonComments enumComments = addEnumCommentByAnnotations(null, o, cType);
                        str += "\r\n   " + StringUtils.fillPre(o.toString(), " ", max);
                        boolean commentsep = false;
                        if (enumComments != null && enumComments.size() > 0) {
                            for (FlexiCommentJsonNode cs : enumComments) {
                                if (cs instanceof FlexiComment) {
                                    if (!commentsep) {
                                        str += ": ";
                                    } else {
                                        str += "; ";
                                    }
                                    commentsep = true;
                                    str += ((FlexiComment) cs).getText().replaceAll("[\r\n]{1,2}", " ");
                                }
                            }
                        }
                    }
                    comments = addComment(comments, str, FlexiMapperTags.OPTIONS);
                } catch (FlexiMapperException e) {
                    e.printStackTrace();
                }
            } catch (IllegalAccessException e) {
            } catch (IllegalArgumentException e) {
            } catch (InvocationTargetException e) {
            } catch (NoSuchMethodException e) {
            } catch (SecurityException e) {
            }
        }
        for (CompiledType c : cType.componentTypes) {
            addEnumOptionsComments(comments, c);
        }
        return comments;
    }

    /**
     * @param class1
     * @param enumClass
     * @return
     */
    protected boolean isEnumOptionsCommentsEnabled(CompiledType enumClass) {
        return false;
    }

    /**
     * @param genericReturnType
     * @return
     */
    private String typeToString(CompiledType com) {
        return com.toString(null);
    }

    /**
     * @param comments
     * @param tag
     * @param annoAPIDoc
     * @return
     * @throws FlexiMapperException
     */
    private FlexiJSonComments addComment(FlexiJSonComments comments, Object anno, FlexiMapperTags tag) throws FlexiMapperException {
        if (anno == null) {
            return comments;
        }
        if (anno instanceof String) {
            comments = pushComment(comments, (String) anno, tag);
        }
        if (anno instanceof ApiDoc) {
            comments = pushComment(comments, ((ApiDoc) anno).value(), FlexiMapperTags.DOCS);
            if (StringUtils.isNotEmpty(((ApiDoc) anno).authentication())) {
                comments = pushComment(comments, "Authentication: " + ((ApiDoc) anno).authentication(), FlexiMapperTags.AUTH);
            }
        }
        if (anno instanceof StorableSee) {
            Class<?>[] classes = ((StorableSee) anno).value();
            for (Class<?> cl : classes) {
                try {
                    if (cl.isEnum()) {
                        // for (Object e : cl.getEnumConstants()) {
                        // comments = pushComment(comments, cl.getSimpleName() + ": \r\n" + FlexiUtils.serializeConfigStorable(e),
                        // FlexiMapperTags.SEE);
                        // }
                        Object[] options = ReflectionUtils.getEnumValues((Class<? extends Enum>) (cl));
                        try {
                            String str = ((Class<? extends Enum>) (cl)).getSimpleName() + "-Options: ";
                            for (Object o : options) {
                                str += "\r\n   " + FlexiUtils.serializeConfigStorable(o);
                            }
                            comments = addComment(comments, str, FlexiMapperTags.SEE);
                        } catch (FlexiMapperException e) {
                            e.printStackTrace();
                        }
                    } else {
                        comments = pushComment(comments, cl.getSimpleName() + ": \r\n" + FlexiUtils.serializeConfigStorable(cl.newInstance()), FlexiMapperTags.SEE);
                    }
                } catch (InstantiationException e) {
                    throw new WTFException(e);
                } catch (IllegalAccessException e) {
                    throw new WTFException(e);
                } catch (IllegalArgumentException e1) {
                    e1.printStackTrace();
                } catch (InvocationTargetException e1) {
                    e1.printStackTrace();
                } catch (NoSuchMethodException e1) {
                    e1.printStackTrace();
                } catch (SecurityException e1) {
                    e1.printStackTrace();
                }
            }
        }
        if (anno instanceof StorableLink) {
            String[] hrefs = ((StorableLink) anno).hrefs();
            String[] lables = ((StorableLink) anno).labels();
            for (int i = 0; i < hrefs.length; i++) {
                comments = pushComment(comments, "\"" + lables[i] + "\":" + hrefs[i], FlexiMapperTags.HREF);
            }
        }
        if (anno instanceof StorableValidateNotNull) {
            comments = pushComment(comments, "Constraint: Must not be null", FlexiMapperTags.DOCS);
        }
        if (anno instanceof StorableClassValidator1) {
            try {
                comments = pushComment(comments, ((StorableClassValidator1) anno).cls().newInstance().getDocsDescription(((StorableClassValidator1) anno).parameter(), anno), FlexiMapperTags.DOCS);
            } catch (Exception e) {
                LogV3.log(e);
            }
        }
        if (anno instanceof StorableClassValidator2) {
            try {
                comments = pushComment(comments, ((StorableClassValidator2) anno).cls().newInstance().getDocsDescription(((StorableClassValidator2) anno).parameter(), anno), FlexiMapperTags.DOCS);
            } catch (Exception e) {
                LogV3.log(e);
            }
        }
        if (anno instanceof StorableClassValidator3) {
            try {
                comments = pushComment(comments, ((StorableClassValidator3) anno).cls().newInstance().getDocsDescription(((StorableClassValidator3) anno).parameter(), anno), FlexiMapperTags.DOCS);
            } catch (Exception e) {
                LogV3.log(e);
            }
        }
        if (anno instanceof StorableValidateMandatoryInJson) {
            comments = pushComment(comments, "Constraint: Mandatory! The json must contain this property!", FlexiMapperTags.DOCS);
        }
        if (anno instanceof ApiDocExample) {
            if (StringUtils.isNotEmpty(((ApiDocExample) anno).value())) {
                comments = pushComment(comments, "Example: " + ((ApiDocExample) anno).value(), FlexiMapperTags.EXAMPLE);
            }
        }
        if (anno instanceof StorableDoc) {
            comments = pushComment(comments, ((StorableDoc) anno).value(), FlexiMapperTags.DOCS);
        }
        if (anno instanceof StorableExample) {
            if (StringUtils.isNotEmpty(((StorableExample) anno).value())) {
                comments = pushComment(comments, "Example: " + ((StorableExample) anno).value(), FlexiMapperTags.EXAMPLE);
            }
        }
        if (anno instanceof StorableUnique) {
            comments = pushComment(comments, "Unique: " + ((StorableUnique) anno).value() + " - There may be only one entry with the same '" + ((StorableUnique) anno).value() + "' property", FlexiMapperTags.DOCS);
        }
        if (anno instanceof StorableTypeAlternatives) {
            if (((StorableTypeAlternatives) anno).value().length > 0) {
                for (Class<?> cl : ((StorableTypeAlternatives) anno).value()) {
                    comments = pushComment(comments, "Possible type: " + cl.getName(), FlexiMapperTags.TYPE);
                }
            }
        }
        if (anno instanceof StorableDeprecatedSince) {
            if (StringUtils.isNotEmpty(((StorableDeprecatedSince) anno).value())) {
                try {
                    if (StringUtils.isNotEmpty(((StorableDeprecatedSince) anno).message())) {
                        comments = pushComment(comments, "[WARNING]Deprecated since: " + ((StorableDeprecatedSince) anno).value() + "\r\n[WARNING]" + ((StorableDeprecatedSince) anno).message(), FlexiMapperTags.DEPRECATED);
                    } else {
                        comments = pushComment(comments, "[WARNING]Deprecated since: " + ((StorableDeprecatedSince) anno).value(), FlexiMapperTags.DEPRECATED);
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }
        if (anno instanceof StorableAvailableSince) {
            if (StringUtils.isNotEmpty(((StorableAvailableSince) anno).value())) {
                try {
                    if (StringUtils.isNotEmpty(((StorableAvailableSince) anno).message())) {
                        comments = pushComment(comments, "Available since: " + ((StorableAvailableSince) anno).value() + "\r\n" + ((StorableAvailableSince) anno).message(), FlexiMapperTags.AVAILABLE_SINCE);
                    } else {
                        comments = pushComment(comments, "Available since: " + ((StorableAvailableSince) anno).value(), FlexiMapperTags.AVAILABLE_SINCE);
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }
        if (anno instanceof StorableDateFormat) {
            if (StringUtils.isNotEmpty(((StorableDateFormat) anno).value())) {
                comments = pushComment(comments, "Date Format: " + ((StorableDateFormat) anno).value(), FlexiMapperTags.DATE_FORMAT);
            }
        }
        return comments;
    }

    /**
     * @param comments
     * @param value
     * @param tag
     *            TODO
     * @return
     */
    protected FlexiJSonComments pushComment(FlexiJSonComments comments, String value, FlexiMapperTags tag) {
        if (StringUtils.isEmpty(value)) {
            return comments;
        }
        if (comments == null) {
            comments = createFlexiJsonCommentsContainer();
        }
        for (FlexiCommentJsonNode c : comments) {
            if (c instanceof FlexiComment) {
                if (StringUtils.equalsIgnoreCase(((FlexiComment) c).getText(), value)) {
                    return comments;
                }
            }
        }
        comments.add(createFlexiJsonComment(value, tag, getCommentType(value)));
        return comments;
    }

    public FlexiComment createFlexiJsonComment(String value, FlexiMapperTags tag, org.appwork.storage.flexijson.FlexiComment.Type type) {
        return new FlexiComment(value, type, tag);
    }

    public FlexiJSonComments createFlexiJsonCommentsContainer() {
        return new FlexiJSonComments();
    }

    protected void addClassHeaderCommentsByAnnotations(final FlexiJSonNode ret, CompiledType cType) throws FlexiMapperException {
        if (!isAnnotationCommentsEnabled()) {
            return;
        }
        FlexiJSonComments comments = null;
        for (Annotation a : cType.raw.getAnnotations()) {
            comments = addComment(comments, a, null);
        }
        if (comments != null) {
            ret.setCommentsBefore(comments);
        }
    }

    /**
     * override to enable comments from annotations
     *
     * @return
     */
    protected boolean isAnnotationCommentsEnabled() {
        return false;
    }

    /**
     * @param value
     * @return
     */
    protected org.appwork.storage.flexijson.FlexiComment.Type getCommentType(String value) {
        if (value.contains("*/")) {
            return FlexiComment.Type.LINE;
        }
        return FlexiComment.Type.INLINE;
    }

    /**
     * @return
     */
    public FlexiJSonObject createFlexiJSonObject() {
        return new FlexiJSonObject();
    }

    public FlexiJSonValue createFlexiJSonValue() {
        return new FlexiJSonValue((String) null);
    }

    public FlexiJSonValue createFlexiJSonValue(Number longValue) {
        return new FlexiJSonValue(longValue);
    }

    public FlexiJSonValue createFlexiJSonValue(boolean value) {
        return new FlexiJSonValue(value);
    }

    /**
     * @param name
     * @return
     */
    public FlexiJSonValue createFlexiJSonValue(String value) {
        return new FlexiJSonValue(value);
    }

    public boolean isIgnoreIllegalArgumentMappings() {
        return ignoreIllegalArgumentMappings;
    }

    public boolean isIgnoreIllegalEnumMappings() {
        return ignoreIllegalEnumMappings;
    }

    /**
     * if json maps null to a primitive field
     *
     * @return
     */
    public boolean isIgnorePrimitiveNullMapping() {
        return ignorePrimitiveNullMapping;
    }

    public Object jsonToObject(final FlexiJSonNode json, CompiledType cType) throws FlexiMapperException {
        return jsonToObject(json, cType, null);
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    public Object jsonToObject(final FlexiJSonNode json, CompiledType cType, Setter setter) throws FlexiMapperException {
        try {
            cType = guessTypeForObject(json, cType);
            if (cType.isInstanceOf(FlexiJSonObject.class) && json instanceof FlexiJSonObject) {
                return json;
            } else if (cType.isInstanceOf(FlexiJSonArray.class) && json instanceof FlexiJSonArray) {
                return json;
            } else if (cType.isInstanceOf(FlexiJSonValue.class) && json instanceof FlexiJSonValue) {
                return json;
            }
            Object mapped = handleMapperJsonNodeToObject(json, cType, setter);
            if (mapped != json) {
                return mapped;
            }
            if (json instanceof FlexiJSonValue && ((FlexiJSonValue) json).getValue() == null) {
                try {
                    return cast(((FlexiJSonValue) json), null, cType);
                } catch (ClassCastException e) {
                    if (isIgnoreIllegalArgumentMappings()) {
                        return null;
                    } else {
                        return returnFallbackOrThrowException(new ClassCastFlexiMapperException(json, cType, null, e, null));
                    }
                }
            } else if (cType.isCollection()) {
                Collection<Object> inst;
                try {
                    inst = (Collection<Object>) mapClasses(cType).newInstance();
                } catch (Exception e) {
                    return returnFallbackOrThrowException(new FailedToCreateInstanceFlexiMapperException(json, cType, e));
                }
                final FlexiJSonArray obj = (FlexiJSonArray) json;
                CompiledType[] compTypes = cType.getComponentTypes(Collection.class);
                // if the Type is e.g. Simple "List" without any generic definitions, compTypeswill be empty here
                CompiledType componentType = compTypes.length == 0 ? CompiledType.OBJECT : compTypes[0];
                for (final FlexiJSonNode n : obj) {
                    inst.add(this.jsonToObject(n, componentType, setter));
                }
                return inst;
            } else if (cType.isMap()) {
                Map<String, Object> inst;
                try {
                    inst = (Map<String, Object>) mapClasses(cType).newInstance();
                } catch (Exception e) {
                    return returnFallbackOrThrowException(new FailedToCreateInstanceFlexiMapperException(json, cType, e));
                }
                final FlexiJSonObject obj = (FlexiJSonObject) json;
                CompiledType[] componentTypes = cType.getComponentTypes(Map.class);
                // may be empty if the type is a simple Map without generic definition
                CompiledType componentType = componentTypes.length < 1 ? CompiledType.OBJECT : componentTypes[1];
                for (KeyValueElement el : obj.getElements()) {
                    if (el.getKey() != null) {
                        inst.put(el.getKey(), this.jsonToObject(el.getValue(), mapTypeByKey(el.getKey(), componentType), setter));
                    }
                }
                return inst;
            } else if (cType.isArray()) {
                final FlexiJSonArray obj = (FlexiJSonArray) json;
                final Object arr = mapClasses(cType.componentTypes[0]).newArrayInstance(obj.size());
                for (int i = 0; i < obj.size(); i++) {
                    final Object v = this.jsonToObject(obj.get(i), cType.componentTypes[0], setter);
                    try {
                        Array.set(arr, i, v);
                    } catch (IllegalArgumentException e) {
                        returnFallbackOrThrowException(new FlexiMapperException(obj.get(i), mapClasses(cType.componentTypes[0]), "Cannot convert index [" + i + "] = " + obj.get(i) + " to type " + mapClasses(cType.componentTypes[0]), e));
                        Array.set(arr, i, null);
                    }
                }
                return arr;
            } else if (cType.isBoolean() || cType.isNumber() || cType.isString()) {
                try {
                    return cast(((FlexiJSonValue) json), ((FlexiJSonValue) json).getValue(), cType);
                } catch (ClassCastException e) {
                    if (isIgnoreIllegalArgumentMappings()) {
                        return null;
                    } else {
                        return returnFallbackOrThrowException(new ClassCastFlexiMapperException(json, cType, null, e, null));
                    }
                }
            } else if (cType.isAnyOf(CharSequence.class)) {
                try {
                    return cast(((FlexiJSonValue) json), ((FlexiJSonValue) json).getValue(), CompiledType.STRING);
                } catch (ClassCastException e) {
                    if (isIgnoreIllegalArgumentMappings()) {
                        return null;
                    } else {
                        return returnFallbackOrThrowException(new ClassCastFlexiMapperException(json, cType, null, e, null));
                    }
                }
            } else if (cType.isEnum(true)) {
                try {
                    return nodeToEnum(json, cType);
                } catch (final IllegalArgumentException e) {
                    if (isIgnoreIllegalArgumentMappings() || isIgnoreIllegalEnumMappings()) {
                        return null;
                    } else {
                        return returnFallbackOrThrowException(new ClassCastFlexiMapperException(json, cType, null, e, null));
                    }
                }
            } else {
                if (cType.isInterface()) {
                    try {
                        return initProxy(cType, (FlexiJSonObject) json);
                    } catch (Exception e1) {
                        return returnFallbackOrThrowException(new FailedToCreateInstanceFlexiMapperException(json, cType, e1));
                    }
                } else {
                    Object inst = null;
                    try {
                        inst = getInstance(cType);
                    } catch (Exception e1) {
                        return returnFallbackOrThrowException(new FailedToCreateInstanceFlexiMapperException(json, cType, e1));
                    }
                    writeNodeToObject(cType, (FlexiJSonObject) json, inst);
                    return inst;
                }
            }
        } catch (FlexiMapperException e) {
            // returnFallbackOrThrowException should be handled earlier.
            throw e;
        } catch (RuntimeException e) {
            return returnFallbackOrThrowException(new FlexiMapperException(json, cType, e));
        } finally {
        }
    }

    /**
     * @param key
     * @param componentType
     * @return
     */
    protected CompiledType mapTypeByKey(String key, CompiledType componentType) {
        return componentType;
    }

    public CompiledType guessTypeForObject(final FlexiJSonNode json, CompiledType cType) {
        if (cType.isObject() || cType.raw == null) {
            if (json instanceof FlexiJSonArray) {
                cType = autoMapFlexiJSonArrayclass;
            } else if (json instanceof FlexiJSonObject) {
                cType = autoMapFlexiJSonObjectClass;
            } else if (json instanceof FlexiJSonValue) {
                switch (((FlexiJSonValue) json).getType()) {
                case BOOLEAN:
                    cType = CompiledType.BOOLEAN_PRIMITIVE;
                    break;
                case DOUBLE:
                    cType = CompiledType.DOUBLE_PRIMITIVE;
                    break;
                case LONG:
                    cType = CompiledType.LONG_PRIMITIVE;
                    break;
                case NULL:
                case STRING:
                case UNDEFINED:
                    cType = CompiledType.STRING;
                    break;
                default:
                    throw new IllegalStateException("All cases should already have been handled");
                }
            }
        }
        return cType;
    }

    /**
     * @param type
     * @param obj
     * @return
     * @throws NoSuchMethodException
     * @throws SecurityException
     * @throws IllegalArgumentException
     */
    protected Object createProxy(CompiledType cType, FlexiJSonObject obj) throws IllegalArgumentException, SecurityException, NoSuchMethodException {
        Class<?> clazz = cType.raw;
        return Proxy.newProxyInstance(clazz.getClassLoader(), new Class[] { clazz }, createInterfaceInvocationHandler(cType, obj));
    }

    protected InterfaceStorage<Object> createInterfaceInvocationHandler(CompiledType cType, FlexiJSonObject obj) throws SecurityException, NoSuchMethodException {
        return new InterfaceStorage<Object>(this, cType, obj);
    }

    protected Object getInstance(final CompiledType cType) throws InstantiationException, IllegalAccessException, InvocationTargetException {
        try {
            return cType.newInstance();
        } catch (InstantiationException e) {
            LogV3.info(this, "Cannot create instance (InstantiationException) for %s", cType);
            throw e;
        } catch (IllegalAccessException e) {
            LogV3.info(this, "Cannot create instance (IllegalAccessException) for %s", cType);
            throw e;
        }
    }

    /**
     * @param value
     * @param object
     * @param type
     * @return
     */
    private Object cast(FlexiJSonValue node, Object value, CompiledType destType) {
        if (value == null) {
            if (destType.isNumber()) {
                if (!destType.isPrimitiveWrapper()) {
                    return convertNullToNumber(node, destType);
                } else {
                    return null;
                }
            } else if (destType.isBoolean()) {
                if (!destType.isPrimitiveWrapper()) {
                    return convertNullToBoolean(node);
                } else {
                    return null;
                }
            } else {
                return null;
            }
        } else if (destType == CompiledType.STRING) {
            if (!(value instanceof String)) {
                return convertToString(node);
            }
            return value;
        } else if (value instanceof CharSequence) {
            if (destType.isNumber()) {
                return convertStringToNumber(node, StringUtils.valueOfOrNull(value), destType);
            } else if (destType.isBoolean()) {
                return convertStringToBoolean(node, StringUtils.valueOfOrNull(value), destType);
            } else if (destType.isAnyOf(Number.class)) {
                // is exactly NUMBER.class ( public Number getNumber() )
                return convertStringToNumber(node, StringUtils.valueOfOrNull(value), destType);
            } else {
                throw new ClassCastException("Cannot cast " + value + "+ to " + destType);
            }
        }
        return ReflectionUtils.cast(value, destType.type);
    }

    /**
     * @param value
     * @param destType
     * @return
     */
    protected Object convertStringToBoolean(FlexiJSonValue node, String value, CompiledType destType) {
        if ("true".equals(value)) {
            return cast(node, Boolean.TRUE, destType);
        } else if ("false".equals(value)) {
            return cast(node, Boolean.FALSE, destType);
        } else if (destType == CompiledType.BOOLEAN_WRAPPER && value == null) {
            return null;
        }
        throw new ClassCastException("Cannot cast " + value + " to " + destType);
    }

    /**
     * @param value
     * @param destType
     * @return
     */
    protected Object convertStringToNumber(FlexiJSonValue node, String value, CompiledType destType) {
        if (destType.isFixedPointNumber()) {
            return cast(node, Long.valueOf(value), destType);
        } else if (destType.isFloatingPointNumber()) {
            return cast(node, Double.valueOf(value), destType);
        } else {
            if (value.matches("-?\\d+")) {
                return Long.parseLong(value);
            } else if (value.matches("-?\\d+\\.\\d+")) {
                return Double.valueOf(value).longValue();
            } else {
            }
        }
        throw new ClassCastException("Cannot cast " + value + " to " + destType);
    }

    /**
     * @param value
     * @return
     */
    protected Object convertToString(FlexiJSonValue node) {
        return StringUtils.valueOfOrNull(node.getValue());
    }

    /**
     * @param node
     * @return
     */
    protected Object convertNullToBoolean(FlexiJSonValue node) {
        return false;
    }

    /**
     * @param node
     * @param destType
     * @return
     */
    protected Object convertNullToNumber(FlexiJSonValue node, CompiledType destType) {
        return cast(node, 0, destType);
    }

    protected Enum nodeToEnum(final FlexiJSonNode json, CompiledType type) throws FlexiMapperException {
        if (((FlexiJSonValue) json).getValue() == null) {
            return null;
        }
        String string = StringUtils.valueOfOrNull(((FlexiJSonValue) json).getValue());
        try {
            return Enum.valueOf((Class<Enum>) type.raw, string);
        } catch (java.lang.IllegalArgumentException e) {
            // invalid enum check annotations to see if we have fallback values
            for (Field f : type.raw.getDeclaredFields()) {
                FlexiKeyLookup fallback = f.getAnnotation(FlexiKeyLookup.class);
                if (fallback != null) {
                    for (String fb : fallback.value()) {
                        if (fb.equals(string)) {
                            try {
                                return (Enum) f.get(null);
                            } catch (IllegalArgumentException e1) {
                                LogV3.log(e1);
                            } catch (IllegalAccessException e1) {
                                LogV3.log(e1);
                            }
                        }
                    }
                }
            }
            throw e;
        }
    }

    protected Object handleMapperJsonNodeToObject(final FlexiJSonNode json, CompiledType type, Setter setter) throws FlexiMapperException {
        boolean returnFallbackNull = false;
        for (FlexiTypeMapper mapper : typeMapper) {
            try {
                if (mapper.canConvert2Object(json, type, setter)) {
                    return mapper.json2Obj(this, json, type, setter);
                }
            } catch (FlexiMapperException e) {
                returnFallbackOrThrowException(e);
                returnFallbackNull = true;
            }
        }
        ArrayList<FlexiTypeMapper> mappers = THREAD_MAPPERS.get();
        if (mappers != null) {
            for (FlexiTypeMapper mapper : mappers) {
                try {
                    if (mapper.canConvert2Object(json, type, setter)) {
                        return mapper.json2Obj(this, json, type, setter);
                    }
                } catch (FlexiMapperException e) {
                    returnFallbackOrThrowException(e);
                    returnFallbackNull = true;
                }
            }
        }
        if (returnFallbackNull) {
            return null;
        }
        return json;
    }

    public void writeNodeToObject(CompiledType cType, final FlexiJSonObject obj, Object inst) throws FlexiMapperException {
        FlexiJSonNode value;
        Object v;
        for (KeyValueElement es : obj.getElements()) {
            String key = es.getKey();
            if (key == null) {
                // entry for comments without a key relation like {/*test*/}
                continue;
            }
            Setter s = getSetterByKey(cType, key);
            if (s == null) {
                onClassFieldMissing(inst, es.getKey(), es.getValue(), cType);
                continue;
            }
            value = es.getValue();
            //
            CompiledType fieldType = CompiledType.create(s.getType(), cType.type);
            if (!fieldType.isGenericsResolved()) {
                returnFallbackOrThrowException(new FlexiMapperException(value, cType, "Cannot resolve Type " + fieldType + " " + es.getKey() + "=" + value + " to actual type ", null));
            }
            try {
                v = this.jsonToObject(value, mapTypeByKey(key, fieldType), s);
            } catch (IllegalArgumentException e) {
                returnFallbackOrThrowException(new FlexiMapperException(value, cType, "Cannot convert " + es.getKey() + "=" + value + " to type " + fieldType, e));
                continue;
            }
            try {
                setValueToObject(es.getValue(), inst, cType, v, s);
            } catch (FlexiMapperException e) {
                throw e;
            } catch (final NullPointerException e) {
                if (isIgnoreIllegalArgumentMappings()) {
                    continue;
                } else if (v == null && isIgnorePrimitiveNullMapping()) {
                    continue;
                }
                returnFallbackOrThrowException(new ClassCastFlexiMapperException(value, fieldType, null, e, v));
            } catch (final IllegalArgumentException e) {
                if (isIgnoreIllegalArgumentMappings()) {
                    continue;
                } else if (v == null && isIgnorePrimitiveNullMapping()) {
                    continue;
                }
                returnFallbackOrThrowException(new ClassCastFlexiMapperException(value, fieldType, null, e, v));
            } catch (Exception e) {
                returnFallbackOrThrowException(new ClassCastFlexiMapperException(value, fieldType, null, e, v));
            }
        }
    }

    protected void setValueToObject(FlexiJSonNode node, Object inst, CompiledType instType, Object value, Setter setter) throws IllegalAccessException, InvocationTargetException, FlexiMapperException {
        setter.setValue(inst, value);
    }

    /**
     * return true if the exception should not be thrown
     *
     * @param ex
     * @return
     * @throws FlexiMapperException
     */
    protected Object returnFallbackOrThrowException(FlexiMapperException ex) throws FlexiMapperException {
        if (isCollectExceptions()) {
            if (exceptions == null) {
                exceptions = new ArrayList<FlexiMapperException>();
            }
            exceptions.add(ex);
            return null;
        } else {
            throw ex;
        }
    }

    public ArrayList<FlexiMapperException> getExceptions() {
        return exceptions;
    }

    protected Setter getSetterByKey(CompiledType cType, String key) {
        if (key == null) {
            return null;
        }
        Setter ret = cType.getClassCache().getSetter(key);
        if (ret == null) {
            if (Character.isUpperCase(key.charAt(0)) && isUpperCaseFirstKeyLetterIsEnabled()) {
                key = key.substring(0, 1).toLowerCase(Locale.ENGLISH) + key.substring(1);
                ret = cType.getClassCache().getSetter(key);
            }
        }
        if (ret == null && isGuessKeyNameCaseEnabled()) {
            for (Setter ss : cType.getClassCache().getSetter()) {
                if (ss.getKey().equalsIgnoreCase(key)) {
                    if (ret != null) {
                        // case insensitive key name mapping is ambigious
                        return null;
                    }
                    ret = ss;
                }
            }
        }
        return ret;
    }

    /**
     * @return
     */
    public boolean isGuessKeyNameCaseEnabled() {
        return true;
    }

    /**
     * if a key is something like "BasicV2",
     *
     * @return
     */
    public boolean isUpperCaseFirstKeyLetterIsEnabled() {
        return true;
    }

    /**
     * @param inst
     * @param key
     * @param value
     * @param cc
     *            TODO
     * @throws FlexiMapperException
     */
    protected void onClassFieldMissing(Object inst, String key, FlexiJSonNode value, CompiledType cType) throws FlexiMapperException {
        // LogV3.warning("Unexpected field in json: " + key + " in " + cType.toString(new JavaSyntax(true)));
    }

    /**
     * @param <T>
     * @param json
     * @param typeRef
     * @throws FlexiMapperException
     */
    @SuppressWarnings("unchecked")
    public <T> T jsonToObject(final FlexiJSonNode json, final TypeRef<T> type) throws FlexiMapperException {
        return (T) this.jsonToObject(json, CompiledType.create(type.getType()), null);
    }

    /**
     * @param class1
     * @return
     * @throws FlexiMapperException
     */
    protected CompiledType mapClasses(final CompiledType class1) throws FlexiMapperException {
        if (class1.isInterface()) {
            if (class1.isSet()) {
                return CompiledType.create(autoMapSetInterface.type, class1.getAssignableSuperClass(Set.class).type);
            } else if (class1.isCollection()) {
                // return a type of type autoMapCollectionInterface, but with the generic Collection component types of class1
                return CompiledType.create(autoMapCollectionInterface.type, class1.getAssignableSuperClass(Collection.class).type);
            } else if (class1.isMap()) {
                return CompiledType.create(autoMapMapInterface.type, class1.getAssignableSuperClass(Map.class).type);
            }
        }
        return class1;
    }

    private boolean collectExceptions = false;

    public boolean isCollectExceptions() {
        return collectExceptions;
    }

    public void setCollectExceptions(boolean collectExceptions) {
        this.collectExceptions = collectExceptions;
    }

    public void setIgnoreIllegalArgumentMappings(final boolean ignoreIllegalArgumentMappings) {
        this.ignoreIllegalArgumentMappings = ignoreIllegalArgumentMappings;
    }

    public void setIgnoreIllegalEnumMappings(final boolean ignoreIllegalEnumMappings) {
        this.ignoreIllegalEnumMappings = ignoreIllegalEnumMappings;
    }

    public void setIgnorePrimitiveNullMapping(final boolean ignoreIllegalNullArguments) {
        ignorePrimitiveNullMapping = ignoreIllegalNullArguments;
    }

    /**
     * @param example
     * @param simpleTypeRef
     * @return
     */
    public <T> T convert(final Object obj, final TypeRef<T> targetType) throws FlexiMapperException {
        return jsonToObject(objectToJsonNode(obj), targetType);
    }

    /**
     * @param inst
     * @param typeRef
     * @return
     * @throws FlexiMapperException
     */
    public FlexiJSonNode objectToJsonNode(final Object obj, final TypeRef<?> targetType) throws FlexiMapperException {
        return objectToJsonNode(obj, CompiledType.create(targetType.getType()));
    }

    /**
     * @return
     */
    public static ArrayList<FlexiTypeMapper> getThreadMappers() {
        return THREAD_MAPPERS.get();
    }
}
