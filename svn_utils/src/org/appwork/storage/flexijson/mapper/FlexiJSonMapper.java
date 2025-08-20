/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
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
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
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
import org.appwork.moncompare.BadFormatException;
import org.appwork.moncompare.ConditionException;
import org.appwork.moncompare.fromjson.FlexiCondition;
import org.appwork.moncompare.typehandler.FlexiTypeHandler;
import org.appwork.remoteapi.annotations.ApiDoc;
import org.appwork.remoteapi.annotations.ApiDocExample;
import org.appwork.storage.StorableAvailableSince;
import org.appwork.storage.StorableConditionalType;
import org.appwork.storage.StorableConditionalType2;
import org.appwork.storage.StorableConditionalType3;
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
import org.appwork.storage.flexijson.CannotResolvePathException;
import org.appwork.storage.flexijson.FlexiComment;
import org.appwork.storage.flexijson.FlexiCommentJsonNode;
import org.appwork.storage.flexijson.FlexiJSonArray;
import org.appwork.storage.flexijson.FlexiJSonComments;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.FlexiJSonObject;
import org.appwork.storage.flexijson.FlexiJSonValue;
import org.appwork.storage.flexijson.FlexiUtils;
import org.appwork.storage.flexijson.InvalidPathException;
import org.appwork.storage.flexijson.JSPath;
import org.appwork.storage.flexijson.JSPath.MetaElement;
import org.appwork.storage.flexijson.KeyValueElement;
import org.appwork.storage.flexijson.mapper.interfacestorage.FlexiStorableInterface;
import org.appwork.storage.flexijson.mapper.interfacestorage.FlexiVariableAccess;
import org.appwork.storage.flexijson.mapper.interfacestorage.InterfaceStorage;
import org.appwork.storage.flexijson.mapper.typemapper.DateMapper;
import org.appwork.storage.flexijson.mapper.typemapper.LocaleMapMapper;
import org.appwork.storage.flexijson.mapper.typemapper.ReadableBytesMapper;
import org.appwork.storage.flexijson.mapper.typemapper.TimeSpanMapper;
import org.appwork.storage.flexijson.stringify.FlexiJSonStringBuilder;
import org.appwork.storage.simplejson.ValueType;
import org.appwork.storage.simplejson.mapper.ClassCache;
import org.appwork.storage.simplejson.mapper.Getter;
import org.appwork.storage.simplejson.mapper.Setter;
import org.appwork.storage.validator.classvalidator.StorableClassValidator1;
import org.appwork.storage.validator.classvalidator.StorableClassValidator2;
import org.appwork.storage.validator.classvalidator.StorableClassValidator3;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Joiner;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.reflection.Clazz;
import org.appwork.utils.reflection.CompiledType;
import org.appwork.utils.reflection.TypeBuilder;
import org.appwork.utils.reflection.TypeParserException;

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

    public static void addDefaultMapper(final FlexiTypeMapper mapper) {
        DEFAULTMAPPER.add(mapper);
    }

    public static final ThreadLocal<ArrayList<FlexiTypeMapper>> THREAD_MAPPERS = new ThreadLocal<ArrayList<FlexiTypeMapper>>();

    /**
     * @param urlMapper
     * @return
     */
    public static FlexiTypeMapper addThreadMapper(final FlexiTypeMapper newMapper) {
        if (THREAD_MAPPERS.get() == null) {
            THREAD_MAPPERS.set(new ArrayList<FlexiTypeMapper>());
        }
        for (final FlexiTypeMapper existing : THREAD_MAPPERS.get()) {
            if (existing.getClass() == newMapper.getClass()) {
                return null;
            }
        }
        THREAD_MAPPERS.get().add(newMapper);
        return newMapper;
    }

    public static boolean removeThreadMapper(final FlexiTypeMapper newMapper) {
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
    protected CompiledType                     autoMapCollectionInterface    = this.autoMapFlexiJSonArrayclass;
    protected CompiledType                     autoMapMapInterface           = this.autoMapFlexiJSonObjectClass;
    protected CompiledType                     autoMapSetInterface           = CompiledType.create(new TypeRef<LinkedHashSet<Object>>() {
                                                                             });;
    private ArrayList<FlexiMapperException>    exceptions;
    private boolean                            ignoreDefaultValuesEnabled;
    private boolean                            tagDefaultValuesEnabled;

    public FlexiJSonMapper() {
        this.typeMapper = new ArrayList<FlexiTypeMapper>(DEFAULTMAPPER);
    }

    /**
     * Adds the new mapper with highest priority
     *
     * @param <T>
     * @param class1
     * @param fileMapper
     */
    public void addMapper(final FlexiTypeMapper mapper) {
        this.typeMapper.add(0, mapper);
    }

    /**
     * @param class1
     */
    public void removeMapperByType(final Class<?> class1) {
        for (final Iterator<FlexiTypeMapper> it = this.typeMapper.iterator(); it.hasNext();) {
            if (Clazz.isInstanceof(it.next().getClass(), class1)) {
                it.remove();
            }
        }
    }

    public void setTypeMapper(final List<FlexiTypeMapper> customs) {
        this.typeMapper.clear();
        this.typeMapper.addAll(customs);
    }

    public List<FlexiTypeMapper> getTypeMapper() {
        return Collections.unmodifiableList(this.typeMapper);
    }

    /**
     * @param inst
     * @param typeRef
     * @return
     * @throws FlexiMapperException
     */
    public FlexiJSonNode objectToJsonNode(final Object obj, final CompiledType type) throws FlexiMapperException {
        return this.objectToJsonNode(null, obj, this.createObjectToJsonContext(type));
    }

    public FlexiJSonNode objectToJsonNode(final Object obj) throws FlexiMapperException {
        return this.objectToJsonNode(null, obj, null);
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
    public FlexiJSonNode objectToJsonNode(final Getter reference, final Object obj, DefaultObjectToJsonContext context) throws FlexiMapperException {
        if (context == null) {
            context = this.createObjectToJsonContext(obj == null ? CompiledType.OBJECT : CompiledType.create(obj.getClass()));
        }
        if (obj instanceof FlexiJSonNode) {
            return (FlexiJSonNode) obj;
        }
        final FlexiJSonNode mapped = this.handleMapperObjectToJsonNode(reference, obj, context);
        if (mapped != null) {
            return mapped;
        }
        final CompiledType cType = this.getTargetClass(obj, context);
        if (cType.isPrimitiveWrapper()) {
            // if (obj == null) {
            // return createFlexiJSonValue((String) null);
            // }
            if (obj == null) {
                return this.createFlexiJSonValue();
            } else if (cType.isBoolean()) {
                return this.createFlexiJSonValue((Boolean) obj);
            } else if (cType.isCharacter()) {
                return this.createFlexiJSonValue(String.valueOf(obj));
            } else if (cType.isNumber()) {
                return this.createFlexiJSonValue((Number) obj);
            } else {
                throw new WTFException("Unknown Primitive Type: " + cType);
            }
        } else if (cType.isPrimitive()) {
            // obj cannot be null
            if (cType.isBoolean()) {
                return this.createFlexiJSonValue(((Boolean) obj).booleanValue());
            } else if (cType.isCharacter()) {
                return this.createFlexiJSonValue(String.valueOf(obj));
            } else if (cType.isNumber()) {
                return this.createFlexiJSonValue((Number) obj);
            } else {
                throw new WTFException("Unknown Primitive Type: " + cType);
            }
        } else if (cType.isString()) {
            return this.createFlexiJSonValue((String) obj);
        } else if (cType.isInstanceOf(CharSequence.class)) {
            return this.createFlexiJSonValue(obj == null ? (String) null : ((CharSequence) obj).toString());
        } else if (cType.isEnum(true)) {
            final FlexiJSonValue ret = obj == null ? this.createFlexiJSonValue((String) null) : this.createFlexiJSonValue(((Enum) obj).name());
            final FlexiJSonComments comments = this.addEnumCommentByAnnotations(null, obj, cType);
            ret.addCommentsAfter(comments);
            return ret;
        } else if (cType.isMap()) {
            final FlexiJSonNode node = obj == null ? this.createFlexiJSonValue() : this.createFlexiJSonObject();
            this.addClassHeaderCommentsByAnnotations(node, cType);
            final CompiledType[] compTypes = cType.getComponentTypes(Map.class);
            if (compTypes.length == 2 && compTypes[1].isGenericsResolved()) {
                this.addClassHeaderCommentsByAnnotations(node, compTypes[1]);
            }
            if (obj == null) {
                return node;
            }
            final FlexiJSonObject ret = (FlexiJSonObject) node;
            Entry<Object, Object> next;
            for (final Iterator<Entry<Object, Object>> it = ((Map<Object, Object>) obj).entrySet().iterator(); it.hasNext();) {
                next = it.next();
                if (!(next.getKey() instanceof String)) {
                    this.returnFallbackOrThrowException(new FlexiMapperException(ret, cType, "Map keys have to be Strings: " + cType.type + " Keyclass:" + (next.getKey() == null ? "<null>" : next.getKey().getClass())));
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
                    this.putObjectPropertyForMap(ret, this.createKeyValueElement(ret, next.getKey().toString(), this.objectToJsonNode(reference, next.getValue(), context)), type, context, obj, next.getValue());
                } finally {
                    context.removeLast();
                }
            }
            return ret;
        } else if (cType.isCollection()) {
            final FlexiJSonNode node = obj == null ? this.createFlexiJSonValue() : this.createFlexiJSonArray(((Collection<?>) obj).size());
            this.addClassHeaderCommentsByAnnotations(node, cType);
            final CompiledType[] compTypes = cType.getComponentTypes(Collection.class);
            if (compTypes.length == 1 && compTypes[0].isGenericsResolved()) {
                this.addClassHeaderCommentsByAnnotations(node, compTypes[0]);
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
                    ret.add(this.objectToJsonNode(reference, o, context));
                } finally {
                    context.removeLast();
                    i++;
                }
            }
            return ret;
        } else if (cType.isArray()) {
            final int length = obj == null ? 0 : Array.getLength(obj);
            final FlexiJSonNode node = obj == null ? this.createFlexiJSonValue() : this.createFlexiJSonArray(length);
            this.addClassHeaderCommentsByAnnotations(node, cType);
            final CompiledType[] compTypes = cType.componentTypes;
            for (final CompiledType c : compTypes) {
                this.addClassHeaderCommentsByAnnotations(node, c);
            }
            if (obj == null) {
                return node;
            }
            final FlexiJSonArray ret = (FlexiJSonArray) node;
            for (int i = 0; i < length; i++) {
                final Object o = Array.get(obj, i);
                CompiledType type = CompiledType.OBJECT;
                if (compTypes.length == 1 && compTypes[0].isGenericsResolved()) {
                    type = compTypes[0];
                }
                if (type.isObject() && o != null) {
                    type = CompiledType.create(o.getClass());
                }
                try {
                    context.add(type, i);
                    ret.add(this.objectToJsonNode(reference, o, context));
                } finally {
                    context.removeLast();
                }
            }
            return ret;
        } else/* if (obj instanceof Storable) */ {
            InterfaceStorage<Object> is = null;
            if (obj instanceof Proxy && this.isIncludeInterfaceStorageBackendNode()) {
                is = InterfaceStorage.get(obj);
            }
            final FlexiJSonNode node = obj == null ? this.createFlexiJSonValue() : this.createFlexiJSonObject();
            if (is != null) {
                node.addCommentsAfter(is.backendNode.getCommentsAfter());
                node.addCommentsBefore(is.backendNode.getCommentsBefore());
                this.cleanUpComments(node.getCommentsAfter());
                this.cleanUpComments(node.getCommentsBefore());
            }
            this.addClassHeaderCommentsByAnnotations(node, cType);
            final CompiledType[] compTypes = cType.componentTypes;
            for (final CompiledType c : compTypes) {
                this.addClassHeaderCommentsByAnnotations(node, c);
            }
            if (obj == null) {
                return node;
            }
            final FlexiJSonObject ret = (FlexiJSonObject) node;
            if (is != null) {
                ret.addCommentsInside(is.backendNode.getCommentsInside());
                this.cleanUpComments(ret.getCommentsInside());
            }
            try {
                Object empty = null;
                final ClassCache cc = cType.getClassCache();
                final Collection<Getter> getters = cc.getGetterMap().values();
                HashMap<KeyValueElement, Integer> orderMap = null;
                for (final Getter g : getters) {
                    final CompiledType t = CompiledType.create(g.getMethod().getGenericReturnType(), cType.type);
                    try {
                        context.add(t, g.getKey());
                        if (cType.getClassCache().getAnnotations(g.key, StorableHidden.class).size() > 0) {
                            continue;
                        }
                        if (this.isIgnoreProperty(obj, cType, context, g)) {
                            continue;
                        }
                        final Object value = g.getValue(obj);
                        if (this.isIgnoreDefaultValuesEnabled(obj, cType, g)) {
                            if (empty == null) {
                                empty = this.createDefaultObject(obj, cType, ret, g, cType.getClassCache().getSetter(g.key), context);
                            }
                            if (empty != null) {
                                if (cType.isAnonymousInterfaceImpl()) {
                                    // empty is a proxy of the interface
                                    try {
                                        final Method correctedGetter = cType.raw.getInterfaces()[0].getMethod(g.getMethod().getName(), g.getMethod().getParameterTypes());
                                        if (CompareUtils.equalsDeep(correctedGetter.invoke(empty, new Object[] {}), value)) {
                                            continue;
                                        }
                                    } catch (final NoSuchMethodException e) {
                                        // seems to be a Anonymous method only available in the impl.no way to get a default value
                                    }
                                } else if (cType.isAnonymousClass()) {
                                    // empty is a proxy of the interface
                                    try {
                                        final Method correctedGetter = cType.superType.raw.getMethod(g.getMethod().getName(), g.getMethod().getParameterTypes());
                                        if (CompareUtils.equalsDeep(correctedGetter.invoke(empty, new Object[] {}), value)) {
                                            continue;
                                        }
                                    } catch (final NoSuchMethodException e) {
                                        // seems to be a Anonymous method only available in the impl.no way to get a default value
                                    }
                                } else {
                                    if (CompareUtils.equalsDeep(g.getValue(empty), value)) {
                                        continue;
                                    }
                                }
                            }
                        }
                        final FlexiJSonNode subNode = this.objectToJsonNode(g, value, context);
                        if (this.isTagDefaultValuesEnabled(obj, cType, g)) {
                            if (empty == null) {
                                empty = this.createDefaultObject(obj, cType, ret, g, cType.getClassCache().getSetter(g.key), context);
                            }
                            if (empty != null) {
                                if (cType.isAnonymousInterfaceImpl()) {
                                    // empty is a proxy of the interface
                                    try {
                                        final Method correctedGetter = cType.raw.getInterfaces()[0].getMethod(g.getMethod().getName(), g.getMethod().getParameterTypes());
                                        if (CompareUtils.equalsDeep(correctedGetter.invoke(empty, new Object[] {}), value)) {
                                            subNode.tag(FlexiMapperTags.DEFAULT_VALUE);
                                        }
                                    } catch (final NoSuchMethodException e) {
                                        // seems to be a Anonymous method only available in the impl.no way to get a default value
                                    }
                                } else if (CompareUtils.equalsDeep(g.getValue(empty), value)) {
                                    subNode.tag(FlexiMapperTags.DEFAULT_VALUE);
                                }
                            }
                        }
                        KeyValueElement element;
                        if (this.isAddDefaultValueCommentEnabled(obj, cType, g)) {
                            if (empty == null) {
                                empty = this.createDefaultObject(obj, cType, ret, g, cType.getClassCache().getSetter(g.key), context);
                            }
                            if (empty == null) {
                                element = (this.methodOrFieldAnnotationsToComments(g, context, this.createKeyValueElement(ret, g.getKey(), subNode), null, false));
                            } else {
                                element = (this.methodOrFieldAnnotationsToComments(g, context, this.createKeyValueElement(ret, g.getKey(), subNode), g.getValue(empty), true));
                            }
                        } else {
                            element = (this.methodOrFieldAnnotationsToComments(g, context, this.createKeyValueElement(ret, g.getKey(), subNode), null, false));
                        }
                        if (is != null) {
                            final KeyValueElement backEndElement = is.backendNode.getElement(g.key);
                            if (backEndElement != null) {
                                element.addCommentsAfterKey(backEndElement.getCommentsAfterKey());
                                element.addCommentsBeforeKey(backEndElement.getCommentsBeforeKey(), true);
                                this.cleanUpComments(element.getCommentsBeforeKey());
                                this.cleanUpComments(element.getCommentsAfterKey());
                            }
                        }
                        final List<StorableOrder> orders = cc.getAnnotations(g.key, StorableOrder.class);
                        if (orders.size() > 0) {
                            if (orderMap == null) {
                                orderMap = new HashMap<KeyValueElement, Integer>();
                            }
                            orderMap.put(element, orders.get(0).value());
                        }
                        this.putObjectPropertyForStorableOrInterface(ret, element, cc, t, g, context, obj, value);
                    } catch (final IllegalArgumentException e) {
                        this.returnFallbackOrThrowException(new FlexiMapperException(ret, cType, e.getMessage(), e));
                    } catch (final IllegalAccessException e) {
                        this.returnFallbackOrThrowException(new FlexiMapperException(ret, cType, e.getMessage(), e));
                    } catch (final InvocationTargetException e) {
                        this.returnFallbackOrThrowException(new FlexiMapperException(ret, cType, e.getMessage(), e));
                    } finally {
                        context.removeLast();
                    }
                }
                if (is != null) {
                    // Convert backendNodes that have no getter
                    FlexiJSonObject backendNode = is.getBackendNode();
                    // clone backend map. do not use the backendNode directly here, because it would change the parent.
                    // TODO:faster clone
                    Map<String, Object> map = jsonToObject(backendNode, TypeRef.MAP);
                    backendNode = (FlexiJSonObject) objectToJsonNode(map);
                    if (backendNode == is.backendNode) {
                        throw new WTFException("Failed to clone!");
                    }
                    for (final KeyValueElement el : backendNode.getElements()) {
                        if (el.getKey() == null) {
                            final KeyValueElement emptyKeyValueElement = this.createKeyValueElement(ret, el.getKey(), el.getValue());
                            emptyKeyValueElement.addCommentsAfterKey(el.getCommentsAfterKey());
                            emptyKeyValueElement.addCommentsBeforeKey(el.getCommentsBeforeKey(), true);
                            this.cleanUpComments(emptyKeyValueElement.getCommentsBeforeKey());
                            this.cleanUpComments(emptyKeyValueElement.getCommentsAfterKey());
                            this.putObjectPropertyForInterfaceBackend(ret, emptyKeyValueElement, cc, context, obj);
                            continue;
                        }
                        if (!cc.getGetterMap().containsKey(el.getKey())) {
                            final KeyValueElement element = this.createKeyValueElement(ret, el.getKey(), el.getValue());
                            element.addCommentsAfterKey(el.getCommentsAfterKey());
                            element.addCommentsBeforeKey(el.getCommentsBeforeKey(), true);
                            this.cleanUpComments(element.getCommentsBeforeKey());
                            this.cleanUpComments(element.getCommentsAfterKey());
                            this.putObjectPropertyForInterfaceBackend(ret, element, cc, context, obj);
                        }
                    }
                }
                if (orderMap != null) {
                    final HashMap<KeyValueElement, Integer> fOrderMap = orderMap;
                    ret.sort(new Comparator<KeyValueElement>() {
                        @Override
                        public int compare(final KeyValueElement o1, final KeyValueElement o2) {
                            Integer id1 = fOrderMap.get(o1);
                            Integer id2 = fOrderMap.get(o2);
                            if (id1 == null) {
                                id1 = 0;
                            }
                            if (id2 == null) {
                                id2 = 0;
                            }
                            final int result = CompareUtils.compareComparable(id1, id2);
                            if (result == 1) {
                                return CompareUtils.compareComparable(o1.getKey(), o2.getKey());
                            } else {
                                return result;
                            }
                        }
                    });
                }
            } catch (final SecurityException e) {
                this.returnFallbackOrThrowException(new FlexiMapperException(ret, cType, e.getMessage(), e));
            } catch (final NoSuchMethodException e) {
                this.returnFallbackOrThrowException(new FlexiMapperException(ret, cType, e.getMessage(), e));
            }
            return ret;
        }
    }

    /**
     * Keep in mind, that cc, type, getter,parent may be null - depending on the object we try to create
     *
     * @param ret
     * @param element
     * @param cc
     * @param context
     *            TODO
     * @param parent
     *            TODO
     * @param value
     *            TODO
     * @param t
     * @param g
     */
    protected void putObjectPropertyForMap(final FlexiJSonObject ret, final KeyValueElement element, final CompiledType type, final DefaultObjectToJsonContext context, final Object parent, final Object value) {
        ret.add(element);
    }

    protected void putObjectPropertyForInterfaceBackend(final FlexiJSonObject ret, final KeyValueElement element, final ClassCache cc, final DefaultObjectToJsonContext context, final Object parent) {
        ret.add(element);
    }

    protected void putObjectPropertyForStorableOrInterface(final FlexiJSonObject ret, final KeyValueElement element, final ClassCache cc, final CompiledType type, final Getter getter, final DefaultObjectToJsonContext context, final Object parent, final Object value) throws IllegalArgumentException, IllegalAccessException, InvocationTargetException, FlexiMapperException {
        ret.add(element);
    }

    /**
     * @param compiledType
     * @return
     */
    protected DefaultObjectToJsonContext createObjectToJsonContext(final CompiledType compiledType) {
        return new DefaultObjectToJsonContext(compiledType);
    }

    /**
     * @param obj
     * @param cType
     * @param typeHirarchy
     * @param g
     * @return
     */
    protected boolean isIgnoreProperty(final Object obj, final CompiledType cType, final DefaultObjectToJsonContext context, final Getter g) {
        // can be used to ignore properties during serialisation/obj -> Json mapping
        return false;
    }

    /**
     * @param commentsBeforeKey
     */
    private void cleanUpComments(final FlexiJSonComments comments) {
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
    protected FlexiJSonObject interfaceStorageToNode(final InterfaceStorage<Object> is, final Object obj, final CompiledType cType, final DefaultObjectToJsonContext context) throws FlexiMapperException {
        final FlexiJSonObject ret = this.createFlexiJSonObject();
        this.addClassHeaderCommentsByAnnotations(ret, cType);
        final CompiledType[] compTypes = cType.componentTypes;
        for (final CompiledType c : compTypes) {
            this.addClassHeaderCommentsByAnnotations(ret, c);
        }
        ret.addCommentsAfter(is.backendNode.getCommentsAfter());
        ret.addCommentsBefore(is.backendNode.getCommentsBefore());
        ret.addCommentsInside(is.backendNode.getCommentsInside());
        try {
            Object empty = null;
            final Collection<Getter> getters = cType.getClassCache().getGetterMap().values();
            for (final Getter g : getters) {
                final CompiledType t = CompiledType.create(g.getMethod().getGenericReturnType(), cType.type);
                try {
                    context.add(t, g.getKey());
                    if (cType.getClassCache().getAnnotations(g.key, StorableHidden.class).size() > 0) {
                        continue;
                    }
                    final Object value = g.getValue(obj);
                    if (this.isIgnoreDefaultValuesEnabled(obj, cType, g)) {
                        if (empty == null) {
                            empty = this.createDefaultObject(obj, cType, ret, g, cType.getClassCache().getSetter(g.key), context);
                        }
                        if (empty != null) {
                            if (CompareUtils.equalsDeep(g.getValue(empty), value)) {
                                continue;
                            }
                        }
                    }
                    final FlexiJSonNode subNode = this.objectToJsonNode(g, value, context);
                    if (this.isTagDefaultValuesEnabled(obj, cType, g)) {
                        if (empty == null) {
                            empty = this.createDefaultObject(obj, cType, ret, g, cType.getClassCache().getSetter(g.key), context);
                        }
                        if (empty != null) {
                            if (CompareUtils.equalsDeep(g.getValue(empty), value)) {
                                subNode.tag(FlexiMapperTags.DEFAULT_VALUE);
                            }
                        }
                    }
                    if (this.isAddDefaultValueCommentEnabled(obj, cType, g)) {
                        if (empty == null) {
                            empty = this.createDefaultObject(obj, cType, ret, g, cType.getClassCache().getSetter(g.key), context);
                        }
                        if (empty == null) {
                            ret.add(this.methodOrFieldAnnotationsToComments(g, context, this.createKeyValueElement(ret, g.getKey(), subNode), null, false));
                        } else {
                            ret.add(this.methodOrFieldAnnotationsToComments(g, context, this.createKeyValueElement(ret, g.getKey(), subNode), g.getValue(empty), true));
                        }
                    } else {
                        ret.add(this.methodOrFieldAnnotationsToComments(g, context, this.createKeyValueElement(ret, g.getKey(), subNode), null, false));
                    }
                } catch (final IllegalArgumentException e) {
                    this.returnFallbackOrThrowException(new FlexiMapperException(ret, cType, e.getMessage(), e));
                } catch (final IllegalAccessException e) {
                    this.returnFallbackOrThrowException(new FlexiMapperException(ret, cType, e.getMessage(), e));
                } catch (final InvocationTargetException e) {
                    this.returnFallbackOrThrowException(new FlexiMapperException(ret, cType, e.getMessage(), e));
                } finally {
                    context.removeLast();
                }
            }
        } catch (final SecurityException e) {
            this.returnFallbackOrThrowException(new FlexiMapperException(ret, cType, e.getMessage(), e));
        } catch (final NoSuchMethodException e) {
            this.returnFallbackOrThrowException(new FlexiMapperException(ret, cType, e.getMessage(), e));
        }
        return ret;
    }

    public FlexiJSonArray createFlexiJSonArray(final int length) {
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
    private FlexiJSonComments addEnumCommentByAnnotations(FlexiJSonComments comments, final Object obj, final CompiledType cType) throws FlexiMapperException {
        if (obj == null || !this.isAnnotationCommentsEnabled()) {
            return comments;
        }
        try {
            final Field field = cType.raw.getField(((Enum) obj).name());
            for (final Annotation an : field.getAnnotations()) {
                comments = this.addComment(comments, an, null);
            }
        } catch (final NoSuchFieldException e) {
        } catch (final SecurityException e) {
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
    protected CompiledType getTargetClass(final Object obj, final DefaultObjectToJsonContext context) {
        if (obj == null) {
            return context.getLast();
        }
        if (obj instanceof Proxy) {
            final InvocationHandler handler = Proxy.getInvocationHandler(obj);
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
    protected KeyValueElement createKeyValueElement(final FlexiJSonObject parent, final String key, final FlexiJSonNode node) {
        return new KeyValueElement(parent, key, node);
    }

    protected FlexiJSonNode handleMapperObjectToJsonNode(final Getter getter, final Object obj, final DefaultObjectToJsonContext context) throws FlexiMapperException {
        for (final FlexiTypeMapper mapper : this.typeMapper) {
            if (mapper.canConvert2Json(obj, getter)) {
                return mapper.obj2JSon(this, obj, getter, context);
            }
        }
        final ArrayList<FlexiTypeMapper> mappers = THREAD_MAPPERS.get();
        if (mappers != null) {
            for (final FlexiTypeMapper mapper : mappers) {
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
    protected boolean isTagDefaultValuesEnabled(final Object obj, final CompiledType cType, final Getter g) {
        return this.tagDefaultValuesEnabled;
    }

    public boolean isTagDefaultValuesEnabled() {
        return this.tagDefaultValuesEnabled;
    }

    public void setTagDefaultValuesEnabled(final boolean tagDefaultValuesEnabled) {
        this.tagDefaultValuesEnabled = tagDefaultValuesEnabled;
    }

    protected Object createDefaultObject(final Object obj, final CompiledType cType, final FlexiJSonObject ret, final Getter getter, final Setter setter, final DefaultObjectToJsonContext context) throws NoSuchMethodException, FlexiMapperException {
        if (obj instanceof Proxy) {
            final InvocationHandler handler = Proxy.getInvocationHandler(obj);
            if (handler instanceof InterfaceStorage) {
                final FlexiJSonObject dummy = new FlexiJSonObject();
                return this.initProxy(cType, dummy);
            }
        }
        if (cType.isInstanceOf(FlexiStorableInterface.class) && cType.isAnonymousInterfaceImpl()) {
            final FlexiJSonObject dummy = new FlexiJSonObject();
            return this.initProxy(CompiledType.create(cType.raw.getInterfaces()[0]), dummy);
        }
        /**
         * give the type mappers a try
         */
        for (final FlexiTypeMapper mapper : this.typeMapper) {
            FlexiJSonNode tryNode = this.createFlexiJSonValue((String) null);
            if (mapper.canConvert2Object(tryNode, cType, setter)) {
                return mapper.json2Obj(this, tryNode, cType, setter);
            }
            tryNode = this.createFlexiJSonObject();
            if (mapper.canConvert2Object(tryNode, cType, setter)) {
                return mapper.json2Obj(this, tryNode, cType, setter);
            }
            tryNode = this.createFlexiJSonArray(0);
            if (mapper.canConvert2Object(tryNode, cType, setter)) {
                return mapper.json2Obj(this, tryNode, cType, setter);
            }
        }
        try {
            if (cType.isAnonymousClass()) {
                return cType.superType.newInstance();
            } else {
                return cType.newInstance();
            }
        } catch (final InstantiationException e) {
            this.returnFallbackOrThrowException(new FlexiMapperException(ret, cType, e.getMessage(), e));
        } catch (final IllegalAccessException e) {
            this.returnFallbackOrThrowException(new FlexiMapperException(ret, cType, e.getMessage(), e));
        } catch (final IllegalArgumentException e) {
            this.returnFallbackOrThrowException(new FlexiMapperException(ret, cType, e.getMessage(), e));
        }
        return null;
    }

    private Object initProxy(final CompiledType cType, final FlexiJSonObject obj) throws NoSuchMethodException {
        final Object proxy = this.createProxy(cType, obj);
        ((InterfaceStorage) Proxy.getInvocationHandler(proxy)).setStorage(proxy);
        return proxy;
    }

    /**
     * @param obj
     * @param clazz
     * @param g
     * @return
     */
    protected boolean isAddDefaultValueCommentEnabled(final Object obj, final CompiledType cType, final Getter g) {
        return false;
    }

    /**
     * @param obj
     * @param clazz
     * @param g
     * @return
     */
    public boolean isIgnoreDefaultValuesEnabled(final Object obj, final CompiledType cType, final Getter g) {
        return this.ignoreDefaultValuesEnabled;
    }

    /**
     * if true, the mapper will igore fields if their value is the default value (empty constructor) of the owner object
     *
     * @param ignoreDefaultValuesEnabled
     */
    public void setIgnoreDefaultValuesEnabled(final boolean ignoreDefaultValuesEnabled) {
        this.ignoreDefaultValuesEnabled = ignoreDefaultValuesEnabled;
    }

    public FlexiJSonMapper ignoreDefaultValuesEnabled(final boolean ignoreDefaultValuesEnabled) {
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
    protected KeyValueElement methodOrFieldAnnotationsToComments(final Getter g, final DefaultObjectToJsonContext context, final KeyValueElement create, final Object defaultValue, final boolean addDefaultValueAnnotation) throws FlexiMapperException {
        // / move comments to KeyValueElement
        FlexiJSonComments comments = create.getValue().getCommentsBefore();
        final Class<?> cls = g.getMethod().getDeclaringClass();
        create.getValue().setCommentsBefore(null);
        if (this.isTypeCommentsEnabled()) {
            if (addDefaultValueAnnotation) {
                comments = this.addComment(comments, "Default: " + new FlexiJSonStringBuilder().toJSONString(new FlexiJSonMapper().objectToJsonNode(defaultValue)), FlexiMapperTags.DEFAULT_VALUE);
            }
            comments = this.addComment(comments, TYPE + this.typeToString(context.getCompiledType()), FlexiMapperTags.TYPE);
        } else if (addDefaultValueAnnotation) {
            comments = this.addComment(comments, "Default: " + defaultValue, FlexiMapperTags.DEFAULT_VALUE);
        }
        // if (isDefaultValueComment(obj, clazz, g)) {
        // comments = addComment(comments, typeToString(typeHirarchy, g.getMethod().getDeclaringClass()));
        // }
        if (!this.isAnnotationCommentsEnabled()) {
            return create;
        }
        comments = this.addCommentByAnnotations(g, comments, cls);
        comments = this.addEnumOptionsComments(comments, context.getLast());
        if (comments != null) {
            create.setCommentsBeforeKey(comments);
        }
        return create;
    }

    protected FlexiJSonComments addCommentByAnnotations(final Getter g, FlexiJSonComments comments, final Class<?> cls) throws FlexiMapperException {
        final Class<?> targetClass = ReflectionUtils.getRaw(g.type);
        if (targetClass != null) {
            for (final Annotation a : targetClass.getAnnotations()) {
                comments = this.addComment(comments, a, null);
            }
        }
        try {
            for (final Type c : ClassCache.getClassCache(cls).getTypeHierarchy()) {
                final CompiledType ct = CompiledType.create(c);
                try {
                    if (ct.raw != null) {
                        final Method method = ct.raw.getDeclaredMethod(g.getMethod().getName(), g.getMethod().getParameterTypes());
                        for (final Annotation a : method.getAnnotations()) {
                            comments = this.addComment(comments, a, null);
                        }
                        if (!ct.raw.isInterface()) {
                            final Field field = ct.raw.getDeclaredField(g.getKey());
                            for (final Annotation a : field.getAnnotations()) {
                                comments = this.addComment(comments, a, null);
                            }
                        }
                    }
                } catch (final NoSuchMethodException e) {
                } catch (final NoSuchFieldException e) {
                } catch (final SecurityException e) {
                }
            }
        } catch (final SecurityException e) {
        } catch (final NoSuchMethodException e) {
        }
        return comments;
    }

    protected FlexiJSonComments addEnumOptionsComments(FlexiJSonComments comments, CompiledType cType) {
        // Skip anonymous enums
        while (cType != null && cType.raw == null) {
            cType = cType.superType;
        }
        if (cType == null) {
            // MyType<?> where ? is an enum
            return comments;
        }
        if (cType.isEnum(false) && this.isEnumOptionsCommentsEnabled(cType)) {
            try {
                final Object[] options = ReflectionUtils.getEnumValues((Class<? extends Enum>) cType.raw);
                try {
                    String str = "Options: ";
                    int max = 0;
                    for (final Object o : options) {
                        max = Math.max(max, o.toString().length());
                    }
                    for (final Object o : options) {
                        // Field field = ((Class<? extends Enum>) (type)).getDeclaredField(o.toString());
                        final FlexiJSonComments enumComments = this.addEnumCommentByAnnotations(null, o, cType);
                        str += "\r\n   " + StringUtils.fillPre(o.toString(), " ", max);
                        boolean commentsep = false;
                        if (enumComments != null && enumComments.size() > 0) {
                            for (final FlexiCommentJsonNode cs : enumComments) {
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
                    comments = this.addComment(comments, str, FlexiMapperTags.OPTIONS);
                } catch (final FlexiMapperException e) {
                    e.printStackTrace();
                }
            } catch (final IllegalAccessException e) {
            } catch (final IllegalArgumentException e) {
            } catch (final InvocationTargetException e) {
            } catch (final NoSuchMethodException e) {
            } catch (final SecurityException e) {
            }
        }
        for (final CompiledType c : cType.componentTypes) {
            this.addEnumOptionsComments(comments, c);
        }
        return comments;
    }

    /**
     * @param class1
     * @param enumClass
     * @return
     */
    protected boolean isEnumOptionsCommentsEnabled(final CompiledType enumClass) {
        return false;
    }

    /**
     * @param genericReturnType
     * @return
     */
    private String typeToString(final CompiledType com) {
        return com.toString(null);
    }

    /**
     * @param comments
     * @param tag
     * @param annoAPIDoc
     * @return
     * @throws FlexiMapperException
     */
    private FlexiJSonComments addComment(FlexiJSonComments comments, final Object anno, final FlexiMapperTags tag) throws FlexiMapperException {
        if (anno == null) {
            return comments;
        }
        if (anno instanceof String) {
            comments = this.pushComment(comments, (String) anno, tag);
        }
        if (anno instanceof ApiDoc) {
            comments = this.pushComment(comments, ((ApiDoc) anno).value(), FlexiMapperTags.DOCS);
            if (StringUtils.isNotEmpty(((ApiDoc) anno).authentication())) {
                comments = this.pushComment(comments, "Authentication: " + ((ApiDoc) anno).authentication(), FlexiMapperTags.AUTH);
            }
        }
        if (anno instanceof StorableSee) {
            final Class<?>[] classes = ((StorableSee) anno).value();
            for (final Class<?> cl : classes) {
                try {
                    if (cl.isEnum()) {
                        // for (Object e : cl.getEnumConstants()) {
                        // comments = pushComment(comments, cl.getSimpleName() + ": \r\n" + FlexiUtils.serializeConfigStorable(e),
                        // FlexiMapperTags.SEE);
                        // }
                        final Object[] options = ReflectionUtils.getEnumValues((Class<? extends Enum>) (cl));
                        try {
                            String str = ((Class<? extends Enum>) (cl)).getSimpleName() + "-Options: ";
                            for (final Object o : options) {
                                str += "\r\n   " + FlexiUtils.serializeConfigStorable(o);
                            }
                            comments = this.addComment(comments, str, FlexiMapperTags.SEE);
                        } catch (final FlexiMapperException e) {
                            e.printStackTrace();
                        }
                    } else {
                        comments = this.pushComment(comments, cl.getSimpleName() + ": \r\n" + FlexiUtils.serializeConfigStorable(cl.newInstance()), FlexiMapperTags.SEE);
                    }
                } catch (final InstantiationException e) {
                    throw new WTFException(e);
                } catch (final IllegalAccessException e) {
                    throw new WTFException(e);
                } catch (final IllegalArgumentException e1) {
                    e1.printStackTrace();
                } catch (final InvocationTargetException e1) {
                    e1.printStackTrace();
                } catch (final NoSuchMethodException e1) {
                    e1.printStackTrace();
                } catch (final SecurityException e1) {
                    e1.printStackTrace();
                }
            }
        }
        if (anno instanceof StorableLink) {
            final String[] hrefs = ((StorableLink) anno).hrefs();
            final String[] lables = ((StorableLink) anno).labels();
            for (int i = 0; i < hrefs.length; i++) {
                comments = this.pushComment(comments, "\"" + lables[i] + "\":" + hrefs[i], FlexiMapperTags.HREF);
            }
        }
        if (anno instanceof StorableValidateNotNull) {
            comments = this.pushComment(comments, "Constraint: Must not be null", FlexiMapperTags.DOCS);
        }
        if (anno instanceof StorableClassValidator1) {
            try {
                comments = this.pushComment(comments, ((StorableClassValidator1) anno).cls().newInstance().getDocsDescription(((StorableClassValidator1) anno).parameter(), anno), FlexiMapperTags.DOCS);
            } catch (final Exception e) {
                LogV3.log(e);
            }
        }
        if (anno instanceof StorableClassValidator2) {
            try {
                comments = this.pushComment(comments, ((StorableClassValidator2) anno).cls().newInstance().getDocsDescription(((StorableClassValidator2) anno).parameter(), anno), FlexiMapperTags.DOCS);
            } catch (final Exception e) {
                LogV3.log(e);
            }
        }
        if (anno instanceof StorableClassValidator3) {
            try {
                comments = this.pushComment(comments, ((StorableClassValidator3) anno).cls().newInstance().getDocsDescription(((StorableClassValidator3) anno).parameter(), anno), FlexiMapperTags.DOCS);
            } catch (final Exception e) {
                LogV3.log(e);
            }
        }
        if (anno instanceof StorableValidateMandatoryInJson) {
            comments = this.pushComment(comments, "Constraint: Mandatory! The json must contain this property!", FlexiMapperTags.DOCS);
        }
        if (anno instanceof ApiDocExample) {
            if (StringUtils.isNotEmpty(((ApiDocExample) anno).value())) {
                comments = this.pushComment(comments, "Example: " + ((ApiDocExample) anno).value(), FlexiMapperTags.EXAMPLE);
            }
        }
        if (anno instanceof StorableDoc) {
            comments = this.pushComment(comments, ((StorableDoc) anno).value(), FlexiMapperTags.DOCS);
        }
        if (anno instanceof StorableExample) {
            if (StringUtils.isNotEmpty(((StorableExample) anno).value())) {
                comments = this.pushComment(comments, "Example: " + ((StorableExample) anno).value(), FlexiMapperTags.EXAMPLE);
            }
        }
        if (anno instanceof StorableUnique) {
            comments = this.pushComment(comments, "Unique: " + ((StorableUnique) anno).value() + " - There may be only one entry with the same '" + ((StorableUnique) anno).value() + "' property", FlexiMapperTags.DOCS);
        }
        if (anno instanceof StorableTypeAlternatives) {
            if (((StorableTypeAlternatives) anno).value().length > 0) {
                for (final Class<?> cl : ((StorableTypeAlternatives) anno).value()) {
                    comments = this.pushComment(comments, "Possible type: " + cl.getName(), FlexiMapperTags.TYPE);
                }
            }
        }
        if (anno instanceof StorableDeprecatedSince) {
            if (StringUtils.isNotEmpty(((StorableDeprecatedSince) anno).value())) {
                try {
                    if (StringUtils.isNotEmpty(((StorableDeprecatedSince) anno).message())) {
                        comments = this.pushComment(comments, "[WARNING]Deprecated since: " + ((StorableDeprecatedSince) anno).value() + "\r\n[WARNING]" + ((StorableDeprecatedSince) anno).message(), FlexiMapperTags.DEPRECATED);
                    } else {
                        comments = this.pushComment(comments, "[WARNING]Deprecated since: " + ((StorableDeprecatedSince) anno).value(), FlexiMapperTags.DEPRECATED);
                    }
                } catch (final Exception e) {
                    e.printStackTrace();
                }
            }
        }
        if (anno instanceof StorableAvailableSince) {
            if (StringUtils.isNotEmpty(((StorableAvailableSince) anno).value())) {
                try {
                    if (StringUtils.isNotEmpty(((StorableAvailableSince) anno).message())) {
                        comments = this.pushComment(comments, "Available since: " + ((StorableAvailableSince) anno).value() + "\r\n" + ((StorableAvailableSince) anno).message(), FlexiMapperTags.AVAILABLE_SINCE);
                    } else {
                        comments = this.pushComment(comments, "Available since: " + ((StorableAvailableSince) anno).value(), FlexiMapperTags.AVAILABLE_SINCE);
                    }
                } catch (final Exception e) {
                    e.printStackTrace();
                }
            }
        }
        if (anno instanceof StorableDateFormat) {
            if (StringUtils.isNotEmpty(((StorableDateFormat) anno).value())) {
                comments = this.pushComment(comments, "Date Format: " + ((StorableDateFormat) anno).value(), FlexiMapperTags.DATE_FORMAT);
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
    protected FlexiJSonComments pushComment(FlexiJSonComments comments, final String value, final FlexiMapperTags tag) {
        if (StringUtils.isEmpty(value)) {
            return comments;
        }
        if (comments == null) {
            comments = this.createFlexiJsonCommentsContainer();
        }
        for (final FlexiCommentJsonNode c : comments) {
            if (c instanceof FlexiComment) {
                if (StringUtils.equalsIgnoreCase(((FlexiComment) c).getText(), value)) {
                    return comments;
                }
            }
        }
        comments.add(this.createFlexiJsonComment(value, tag, this.getCommentType(value)));
        return comments;
    }

    public FlexiComment createFlexiJsonComment(final String value, final FlexiMapperTags tag, final org.appwork.storage.flexijson.FlexiComment.Type type) {
        return new FlexiComment(value, type, tag);
    }

    public FlexiJSonComments createFlexiJsonCommentsContainer() {
        return new FlexiJSonComments();
    }

    protected void addClassHeaderCommentsByAnnotations(final FlexiJSonNode ret, final CompiledType cType) throws FlexiMapperException {
        if (!this.isAnnotationCommentsEnabled()) {
            return;
        }
        FlexiJSonComments comments = null;
        for (final Annotation a : cType.raw.getAnnotations()) {
            comments = this.addComment(comments, a, null);
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
    protected org.appwork.storage.flexijson.FlexiComment.Type getCommentType(final String value) {
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

    public FlexiJSonValue createFlexiJSonValue(final Number longValue) {
        return new FlexiJSonValue(longValue);
    }

    public FlexiJSonValue createFlexiJSonValue(final boolean value) {
        return new FlexiJSonValue(value);
    }

    /**
     * @param name
     * @return
     */
    public FlexiJSonValue createFlexiJSonValue(final String value) {
        return new FlexiJSonValue(value);
    }

    public boolean isIgnoreIllegalArgumentMappings() {
        return this.ignoreIllegalArgumentMappings;
    }

    public boolean isIgnoreIllegalEnumMappings() {
        return this.ignoreIllegalEnumMappings;
    }

    /**
     * if json maps null to a primitive field
     *
     * @return
     */
    public boolean isIgnorePrimitiveNullMapping() {
        return this.ignorePrimitiveNullMapping;
    }

    public Object jsonToObject(final FlexiJSonNode json, final CompiledType cType) throws FlexiMapperException {
        return this.jsonToObject(json, cType, null);
    }

    /**
     * @param cType
     * @return
     */
    private boolean initContext(CompiledType cType) {
        FlexiMapperContext c = context.get();
        if (c != null) {
            return false;
        }
        context.set(new FlexiMapperContext(cType));
        return true;
    }

    private boolean referencesEnabled = false;

    public boolean isReferencesEnabled() {
        return referencesEnabled;
    }

    public void setReferencesEnabled(boolean referencesEnabled) {
        this.referencesEnabled = referencesEnabled;
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    public Object jsonToObject(FlexiJSonNode json, CompiledType cType, final Setter setter) throws FlexiMapperException {
        boolean clearContext = initContext(cType);
        try {
            if (isReferencesEnabled()) {
                if (setter != null && json instanceof FlexiJSonValue && ((FlexiJSonValue) json).getType() == ValueType.STRING) {
                    List<String> access;
                    try {
                        access = getReferencesAccess(setter.getMethod().getDeclaringClass(), setter.key);
                        json = this.resolveValue(json, cType, access, null);
                    } catch (FlexiMapperException e) {
                        return this.returnFallbackOrThrowException(e);
                    } catch (final NoSuchMethodException e) {
                        return this.returnFallbackOrThrowException(new FlexiMapperException(json, cType, null, e));
                    } catch (CannotResolvePathException e) {
                        return this.returnFallbackOrThrowException(new FlexiMapperException(json, cType, null, e));
                    }
                }
            }
            cType = this.guessTypeForObject(json, cType, setter);
            if (cType.type == json.getClass()) {
                if (!cType.isInstanceOf(FlexiJSonNode.class)) {
                    DebugMode.debugger("Should never happen!");
                }
                // we tried to map to FlexiNodes Nodes
                return json;
            }
            if (cType.isInstanceOf(FlexiJSonNode.class)) {
                // extended Node elements like ExtFlexiJsonObject
                throw new WTFException("Not Supported!");
            }
            final Object mapped = this.handleMapperJsonNodeToObject(json, cType, setter);
            if (mapped != json) {
                return mapped;
            }
            if (json instanceof FlexiJSonValue && ((FlexiJSonValue) json).getValue() == null) {
                try {
                    return this.cast(((FlexiJSonValue) json), null, cType);
                } catch (final ClassCastException e) {
                    if (this.isIgnoreIllegalArgumentMappings()) {
                        return null;
                    } else {
                        return this.returnFallbackOrThrowException(new ClassCastFlexiMapperException(json, cType, null, e, null));
                    }
                }
            } else if (cType.isCollection()) {
                Collection<Object> inst;
                try {
                    inst = (Collection<Object>) this.mapClasses(cType).newInstance();
                } catch (final Exception e) {
                    return this.returnFallbackOrThrowException(new FailedToCreateInstanceFlexiMapperException(json, cType, e));
                }
                final FlexiJSonArray obj = (FlexiJSonArray) json;
                final CompiledType[] compTypes = cType.getComponentTypes(Collection.class);
                // if the Type is e.g. Simple "List" without any generic definitions, compTypeswill be empty here
                final CompiledType componentType = compTypes.length == 0 ? CompiledType.OBJECT : compTypes[0];
                for (final FlexiJSonNode n : obj) {
                    inst.add(this.jsonToObject(n, componentType, setter));
                }
                return inst;
            } else if (cType.isMap()) {
                Map<String, Object> inst;
                try {
                    inst = (Map<String, Object>) this.mapClasses(cType).newInstance();
                } catch (final Exception e) {
                    return this.returnFallbackOrThrowException(new FailedToCreateInstanceFlexiMapperException(json, cType, e));
                }
                final FlexiJSonObject obj = (FlexiJSonObject) json;
                final CompiledType[] componentTypes = cType.getComponentTypes(Map.class);
                // may be empty if the type is a simple Map without generic definition
                final CompiledType componentType = componentTypes.length < 1 ? CompiledType.OBJECT : componentTypes[1];
                for (final KeyValueElement el : obj.getElements()) {
                    if (el.getKey() != null) {
                        inst.put(el.getKey(), this.jsonToObject(el.getValue(), this.mapTypeByKey(el.getKey(), componentType), setter));
                    }
                }
                return inst;
            } else if (cType.isArray()) {
                final FlexiJSonArray obj = (FlexiJSonArray) json;
                final Object arr = this.mapClasses(cType.componentTypes[0]).newArrayInstance(obj.size());
                for (int i = 0; i < obj.size(); i++) {
                    final Object v = this.jsonToObject(obj.get(i), cType.componentTypes[0], setter);
                    try {
                        Array.set(arr, i, v);
                    } catch (final IllegalArgumentException e) {
                        this.returnFallbackOrThrowException(new FlexiMapperException(obj.get(i), this.mapClasses(cType.componentTypes[0]), "Cannot convert index [" + i + "] = " + obj.get(i) + " to type " + this.mapClasses(cType.componentTypes[0]), e));
                        Array.set(arr, i, null);
                    }
                }
                return arr;
            } else if (cType.isBoolean() || cType.isNumber() || cType.isString() || cType.isCharacter()) {
                try {
                    return this.cast(((FlexiJSonValue) json), ((FlexiJSonValue) json).getValue(), cType);
                } catch (final ClassCastException e) {
                    if (this.isIgnoreIllegalArgumentMappings()) {
                        return null;
                    } else {
                        return this.returnFallbackOrThrowException(new ClassCastFlexiMapperException(json, cType, null, e, null));
                    }
                }
            } else if (cType.isAnyOf(CharSequence.class)) {
                try {
                    return this.cast(((FlexiJSonValue) json), ((FlexiJSonValue) json).getValue(), CompiledType.STRING);
                } catch (final ClassCastException e) {
                    if (this.isIgnoreIllegalArgumentMappings()) {
                        return null;
                    } else {
                        return this.returnFallbackOrThrowException(new ClassCastFlexiMapperException(json, cType, null, e, null));
                    }
                }
            } else if (cType.isEnum(true)) {
                try {
                    return this.nodeToEnum(json, cType);
                } catch (final IllegalArgumentException e) {
                    if (this.isIgnoreIllegalArgumentMappings() || this.isIgnoreIllegalEnumMappings()) {
                        return null;
                    } else {
                        return this.returnFallbackOrThrowException(new ClassCastFlexiMapperException(json, cType, null, e, null));
                    }
                }
            } else {
                try {
                    if (cType.isInterface()) {
                        try {
                            return this.initProxy(cType, (FlexiJSonObject) json);
                        } catch (final Exception e1) {
                            return this.returnFallbackOrThrowException(new FailedToCreateInstanceFlexiMapperException(json, cType, e1));
                        }
                    } else {
                        Object inst = null;
                        try {
                            inst = this.getInstance(cType);
                        } catch (final Exception e1) {
                            return this.returnFallbackOrThrowException(new FailedToCreateInstanceFlexiMapperException(json, cType, e1));
                        }
                        this.writeNodeToObject(cType, (FlexiJSonObject) json, inst);
                        return inst;
                    }
                } catch (final ClassCastException e) {
                    // try to map a primitive to an object
                    return this.returnFallbackOrThrowException(new ClassCastFlexiMapperException(json, cType, "Cannot map " + json.getClass().getSimpleName() + " to type " + cType, e, null));
                }
            }
        } catch (final FlexiMapperException e) {
            // returnFallbackOrThrowException should be handled earlier.
            throw e;
        } catch (final RuntimeException e) {
            return this.returnFallbackOrThrowException(new FlexiMapperException(json, cType, e));
        } finally {
            if (clearContext) {
                context.set(null);
            }
        }
    }

    protected List<String> getReferencesAccess(final Type cls, String key) throws NoSuchMethodException {
        List<String> access = new ArrayList<String>();
        List<FlexiVariableAccess> a = ClassCache.getClassCache(cls).getAnnotations(key, FlexiVariableAccess.class);
        if (a != null) {
            for (FlexiVariableAccess aa : a) {
                access.add(aa.value());
            }
        }
        return access;
    }

    /**
     * @param key
     * @param componentType
     * @return
     */
    protected CompiledType mapTypeByKey(final String key, final CompiledType componentType) {
        return componentType;
    }

    public CompiledType guessTypeForObject(final FlexiJSonNode json, CompiledType cType, final Setter setter) {
        if (cType.isObject() || cType.raw == null) {
            if (json instanceof FlexiJSonArray) {
                cType = this.autoMapFlexiJSonArrayclass;
            } else if (json instanceof FlexiJSonObject) {
                cType = this.autoMapFlexiJSonObjectClass;
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
    protected Object createProxy(final CompiledType cType, final FlexiJSonObject obj) throws IllegalArgumentException, SecurityException, NoSuchMethodException {
        final Class<?> clazz = cType.raw;
        return Proxy.newProxyInstance(clazz.getClassLoader(), new Class[] { clazz }, this.createInterfaceInvocationHandler(cType, obj));
    }

    protected InterfaceStorage<Object> createInterfaceInvocationHandler(final CompiledType cType, final FlexiJSonObject obj) throws SecurityException, NoSuchMethodException {
        return new InterfaceStorage<Object>(this, cType, obj);
    }

    protected Object getInstance(final CompiledType cType) throws InstantiationException, IllegalAccessException, InvocationTargetException {
        try {
            return cType.newInstance();
        } catch (final InstantiationException e) {
            LogV3.info(this, "Cannot create instance (InstantiationException) for %s", cType);
            throw e;
        } catch (final IllegalAccessException e) {
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
    private Object cast(final FlexiJSonValue node, final Object value, final CompiledType destType) {
        if (value == null) {
            if (destType.isNumber()) {
                if (!destType.isPrimitiveWrapper()) {
                    return this.convertNullToNumber(node, destType);
                } else {
                    return null;
                }
            } else if (destType.isBoolean()) {
                if (!destType.isPrimitiveWrapper()) {
                    return this.convertNullToBoolean(node);
                } else {
                    return null;
                }
            } else {
                return null;
            }
        } else if (destType == CompiledType.STRING) {
            if (!(value instanceof String)) {
                return this.convertToString(node);
            }
            return value;
        } else if (value instanceof CharSequence) {
            if (destType.isNumber()) {
                return this.convertStringToNumber(node, StringUtils.valueOfOrNull(value), destType);
            } else if (destType.isBoolean()) {
                return this.convertStringToBoolean(node, StringUtils.valueOfOrNull(value), destType);
            } else if (destType.isAnyOf(Number.class)) {
                // is exactly NUMBER.class ( public Number getNumber() )
                return this.convertStringToNumber(node, StringUtils.valueOfOrNull(value), destType);
            } else if (destType.isCharacter()) {
                return ReflectionUtils.cast(value, destType.type);
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
    protected Object convertStringToBoolean(final FlexiJSonValue node, final String value, final CompiledType destType) {
        if ("true".equals(value)) {
            return this.cast(node, Boolean.TRUE, destType);
        } else if ("false".equals(value)) {
            return this.cast(node, Boolean.FALSE, destType);
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
    protected Object convertStringToNumber(final FlexiJSonValue node, final String value, final CompiledType destType) {
        if (destType.isFixedPointNumber()) {
            return this.cast(node, Long.valueOf(value), destType);
        } else if (destType.isFloatingPointNumber()) {
            return this.cast(node, Double.valueOf(value), destType);
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
    protected Object convertToString(final FlexiJSonValue node) {
        return StringUtils.valueOfOrNull(node.getValue());
    }

    /**
     * @param node
     * @return
     */
    protected Object convertNullToBoolean(final FlexiJSonValue node) {
        return false;
    }

    /**
     * @param node
     * @param destType
     * @return
     */
    protected Object convertNullToNumber(final FlexiJSonValue node, final CompiledType destType) {
        return this.cast(node, 0, destType);
    }

    protected Enum nodeToEnum(final FlexiJSonNode json, final CompiledType type) throws FlexiMapperException {
        if (((FlexiJSonValue) json).getValue() == null) {
            return null;
        }
        final String string = StringUtils.valueOfOrNull(((FlexiJSonValue) json).getValue());
        try {
            return Enum.valueOf((Class<Enum>) type.raw, string);
        } catch (final java.lang.IllegalArgumentException e) {
            // invalid enum check annotations to see if we have fallback values
            for (final Field f : type.raw.getDeclaredFields()) {
                final FlexiEnumFallback fallback = f.getAnnotation(FlexiEnumFallback.class);
                if (fallback != null) {
                    for (final String fb : fallback.value()) {
                        if (fb.equals(string)) {
                            try {
                                return (Enum) f.get(null);
                            } catch (final IllegalArgumentException e1) {
                                LogV3.log(e1);
                            } catch (final IllegalAccessException e1) {
                                LogV3.log(e1);
                            }
                        }
                    }
                }
            }
            throw e;
        }
    }

    protected Object handleMapperJsonNodeToObject(final FlexiJSonNode json, final CompiledType type, final Setter setter) throws FlexiMapperException {
        boolean returnFallbackNull = false;
        for (final FlexiTypeMapper mapper : this.typeMapper) {
            try {
                if (mapper.canConvert2Object(json, type, setter)) {
                    return mapper.json2Obj(this, json, type, setter);
                }
            } catch (final FlexiMapperException e) {
                this.returnFallbackOrThrowException(e);
                returnFallbackNull = true;
            }
        }
        final ArrayList<FlexiTypeMapper> mappers = THREAD_MAPPERS.get();
        if (mappers != null) {
            for (final FlexiTypeMapper mapper : mappers) {
                try {
                    if (mapper.canConvert2Object(json, type, setter)) {
                        return mapper.json2Obj(this, json, type, setter);
                    }
                } catch (final FlexiMapperException e) {
                    this.returnFallbackOrThrowException(e);
                    returnFallbackNull = true;
                }
            }
        }
        if (returnFallbackNull) {
            return null;
        }
        return json;
    }

    public void writeNodeToObject(final CompiledType cType, final FlexiJSonObject obj, final Object inst) throws FlexiMapperException {
        FlexiJSonNode value;
        Object v;
        for (final KeyValueElement es : obj.getElements()) {
            final String key = es.getKey();
            if (key == null) {
                // entry for comments without a key relation like {/*test*/}
                continue;
            }
            final Setter s = this.getSetterByKey(cType, key);
            if (s == null) {
                this.onClassFieldMissing(inst, es.getKey(), es.getValue(), cType);
                continue;
            }
            value = es.getValue();
            //
            CompiledType fieldType = CompiledType.create(s.getType(), cType.type);
            final CompiledType newType = this.handleConditionalTypeAnnotations(cType, obj, key);
            if (newType != null) {
                fieldType = newType;
            }
            // do not use isGenericsResolved because this method looks deeper in the type and retruns true if any generics are unresolved.
            // we need to throw only an exception if the type here is actually directly unresolved.
            if (fieldType.raw == null/* &&!fieldType.isGenericsResolved() */) {
                this.returnFallbackOrThrowException(new FlexiMapperException(value, cType, "Cannot resolve Type " + fieldType + " " + es.getKey() + "=" + value + " to actual type ", null));
            }
            try {
                v = this.jsonToObject(value, this.mapTypeByKey(key, fieldType), s);
            } catch (final IllegalArgumentException e) {
                this.returnFallbackOrThrowException(new FlexiMapperException(value, cType, "Cannot convert " + es.getKey() + "=" + value + " to type " + fieldType, e));
                continue;
            }
            try {
                this.setValueToObject(es.getValue(), inst, cType, v, s);
            } catch (final FlexiMapperException e) {
                throw e;
            } catch (final NullPointerException e) {
                if (this.isIgnoreIllegalArgumentMappings()) {
                    continue;
                } else if (v == null && this.isIgnorePrimitiveNullMapping()) {
                    continue;
                }
                this.returnFallbackOrThrowException(new ClassCastFlexiMapperException(value, fieldType, null, e, v));
            } catch (final IllegalArgumentException e) {
                if (this.isIgnoreIllegalArgumentMappings()) {
                    continue;
                } else if (v == null && this.isIgnorePrimitiveNullMapping()) {
                    continue;
                }
                this.returnFallbackOrThrowException(new ClassCastFlexiMapperException(value, fieldType, null, e, v));
            } catch (final Exception e) {
                this.returnFallbackOrThrowException(new ClassCastFlexiMapperException(value, fieldType, null, e, v));
            }
        }
    }

    protected FlexiJSonNode resolveValue(FlexiJSonNode value, final CompiledType fieldType, final List<String> access, LinkedHashSet<JSPath> loopCheck) throws FlexiMapperException, CannotResolvePathException, NoSuchMethodException {
        try {
            long a = Time.systemIndependentCurrentJVMTimeMillis();
            boolean isStepping = Time.systemIndependentCurrentJVMTimeMillis() - a > 10;
            ;
            if (access != null && access.size() > 0) {
                if (loopCheck == null) {
                    loopCheck = new LinkedHashSet<JSPath>();
                }
                JSPath valuePath = FlexiUtils.fromFlexiNode(value);
                if (!loopCheck.add(valuePath) && !isStepping) {
                    throw new ReferenceLoopException(value, fieldType, "Reference Loop detected:  Reference- Loop: " + new Joiner("->") {
                        /**
                         * @see org.appwork.utils.Joiner#elementToString(java.lang.Object)
                         */
                        @Override
                        protected String elementToString(Object s) {
                            if (s instanceof JSPath) {
                                return ((JSPath) s).toPathString(false);
                            } else {
                                return super.elementToString(s);
                            }
                        }
                    }.join(loopCheck) + "->" + valuePath.toPathString(false), null);
                }
                {
                    return resolveWithoutLoopCheck(value, fieldType, access, loopCheck, valuePath);
                }
            }
        } catch (final InvalidPathException e) {
            throw new FlexiMapperException(value, fieldType, "Illegal ${reference} path.", e);
        }
        return value;
    }

    protected FlexiJSonNode resolveWithoutLoopCheck(FlexiJSonNode value, final CompiledType fieldType, final List<String> access, LinkedHashSet<JSPath> loopCheck, JSPath valuePath) throws FlexiMapperException, InvalidPathException, NoSuchMethodException, CannotResolvePathException {
        String raw = ((FlexiJSonValue) value).getStringValue();
        final String orgRaw = raw;
        final HashSet<String> refs = new HashSet<String>();
        char[] chars = raw.toCharArray();
        boolean inVariable = false;
        List<int[]> toUnescape = new ArrayList<int[]>();
        StringBuilder newText = null;
        StringBuilder jsPathBuilder = new StringBuilder();
        int validatedUnescapes = -1;
        char marker = getRefMarker();
        int escapes = 0;
        NEXT_CHAR: for (int i = 0; i < chars.length; i++) {
            char c = chars[i];
            int remaining = chars.length - i - 1;
            try {
                if (inVariable) {
                    if (c == '}') {
                        if (escapes > 0) {
                            // collect escape sequences to replace later reverse to avoid index issues
                            toUnescape.add(0, new int[] { i - escapes, escapes });
                            if (escapes % 2 == 1) {
                                jsPathBuilder.append(c);
                                continue NEXT_CHAR;
                            }
                        }
                        String toResolve = jsPathBuilder.toString();
                        // refs.add(jsPathBuilder.toString());
                        FlexiJSonNode replacement = resolve(toResolve, value, fieldType, valuePath, loopCheck, access);
                        if (fieldType.isString() || fieldType.isObject()) {
                            if (replacement != null && replacement instanceof FlexiJSonValue) {
                                if (i == chars.length - 1 && newText.length() == 0) {
                                    if (((FlexiJSonValue) replacement).getType() == ValueType.STRING) {
                                        return replacement;
                                    } else {
                                        DebugMode.debugger();
                                    }
                                } else {
                                    newText.append(String.valueOf(((FlexiJSonValue) replacement).getValue()));
                                    jsPathBuilder.setLength(0);
                                    inVariable = false;
                                    continue NEXT_CHAR;
                                }
                            }
                        } else if (i == chars.length - 1) {
                            // ref only
                            return replacement;
                        } else {
                            throw new FlexiMapperException(value, fieldType, "Illegal Link - no replace allowed at this point - only direct links", null);
                        }
                    }
                    jsPathBuilder.append(c);
                } else {
                    if (newText != null) {
                        newText.append(c);
                    }
                    if (c == marker) {
                        if (remaining > 0 && chars[i + 1] == '{') {
                            // start tag reached
                            if (newText == null) {
                                newText = new StringBuilder(raw.length());
                                newText.append(raw, 0, i + 1);
                            }
                            i++;
                            if (escapes > 0) {
                                // collect escape sequences to replace later reverse to avoid index issues
                                newText.setLength(newText.length() - (escapes + 1) / 2);
                                if (escapes % 2 == 1) {
                                    newText.append('{');
                                    continue NEXT_CHAR;
                                }
                                DebugMode.breakIf(jsPathBuilder.length() > 0);
                            } else {
                                // remove $
                                newText.setLength(newText.length() - 1);
                            }
                            inVariable = true;
                        }
                    }
                }
            } finally {
                if (c == marker) {
                    escapes++;
                } else {
                    escapes = 0;
                }
            }
        }
        if (newText != null) {
            if (inVariable) {
                newText.append(marker).append('{').append(jsPathBuilder);
            }
            return createFlexiJSonValue(newText.toString());
        }
        // final Pattern pat = Pattern.compile("\\$\\{[\\w\\.\\d\\[\\]]+(#p\\d+)?}");
        // final Matcher matcher = pat.matcher(raw);
        // while (matcher.find()) {
        // final String group = matcher.group(0);
        // final String path = group.substring(2, group.length() - 1);
        // refs.add(path);
        // }
        return value;
    }

    /**
     * @return
     */
    private char getRefMarker() {
        return '~';
    }

    /**
     * @param value
     * @param fieldType
     * @param valuePath
     * @param loopCheck
     * @param toResolve
     * @param access
     * @return
     * @throws FlexiMapperException
     * @throws InvalidPathException
     * @throws NoSuchMethodException
     * @throws CannotResolvePathException
     */
    private FlexiJSonNode resolve(String path, FlexiJSonNode value, CompiledType fieldType, JSPath valuePath, LinkedHashSet<JSPath> loopCheck, List<String> access) throws FlexiMapperException, InvalidPathException, NoSuchMethodException, CannotResolvePathException {
        final HashSet<String> replaced = new HashSet<String>();
        FlexiJSonNode root = null;
        NEXT_ANNOTATION: for (final String regex : access) {
            if (path.matches(regex)) {
                FlexiJSonNode base = null;
                JSPath jsPath = JSPath.fromPathString(path);
                final Object last = jsPath.getLast();
                if (last instanceof MetaElement) {
                    jsPath = jsPath.getParent();
                    String meta = ((MetaElement) last).getString().substring(1);
                    base = value;
                    // supports #ppp or #p3
                    while (meta.startsWith("p")) {
                        base = base.getParent();
                        meta = meta.substring(1);
                    }
                    // this allows even a mixture like #pp2 ( -> #ppp)
                    String num = "";
                    while (meta.length() > 0 && Character.isDigit(meta.charAt(0))) {
                        num += meta.substring(0, 1);
                        meta = meta.substring(1);
                    }
                    for (int i = 1; i < Integer.parseInt(num); i++) {
                        base = base.getParent();
                    }
                    base = base.resolvePath(JSPath.fromPathString(meta));
                    if (base == null) {
                        throw new FlexiMapperException(value, fieldType, "Illegal #..base path definition");
                    }
                } else {
                    if (root == null) {
                        root = FlexiUtils.getRoot(value.getParent());
                    }
                    base = root;
                }
                JSPath backToWildcard = jsPath;
                int i = 0;
                int foundWildcardAt = -1;
                for (Object o : backToWildcard) {
                    if ("".equals(o)) {
                        foundWildcardAt = i;
                    }
                    i++;
                }
                JSPath refRoot = getReferenceRoot();
                JSPath me = valuePath;
                if (refRoot != null) {
                    me = refRoot.append(me);
                }
                ArrayList<Object> add = new ArrayList<Object>();
                if (foundWildcardAt >= 0) {
                    for (i = backToWildcard.size() - 1; i > foundWildcardAt; i--) {
                        add.add(0, backToWildcard.getLast());
                        backToWildcard = backToWildcard.getParent();
                    }
                }
                if (add.size() > 0) {
                    JSPath basePath = FlexiUtils.fromFlexiNode(base);
                    if (basePath.size() + backToWildcard.size() > me.size()) {
                        throw new FlexiMapperException(value, fieldType, "Cannot Resolve");
                    }
                    JSPath mustMatchWildCards = me.subPath(basePath.size(), basePath.size() + backToWildcard.size());
                    boolean matches = true;
                    for (int ii = 0; ii < mustMatchWildCards.size(); ii++) {
                        if ("".equals(backToWildcard.get(ii))) {
                            continue;
                        } else if (CompareUtils.equals(mustMatchWildCards.get(ii), backToWildcard.get(ii))) {
                            continue;
                        } else {
                            matches = false;
                            break;
                        }
                    }
                    if (!matches) {
                        throw new FlexiMapperException(value, fieldType, "Cannot resolve reference: Path not found: " + path, null);
                    }
                    if (refRoot != null) {
                        base = base.resolvePath(mustMatchWildCards.subPath(refRoot.size(), mustMatchWildCards.size()));
                    } else {
                        base = base.resolvePath(mustMatchWildCards);
                    }
                    jsPath = JSPath.fromPathElements(add);
                }
                if (base == null) {
                    throw new FlexiMapperException(value, fieldType, "Cannot resolve reference: Base not found: " + path, null);
                }
                FlexiJSonNode newValue = base.resolvePath(jsPath);
                if (newValue == null) {
                    throw new FlexiMapperException(value, fieldType, "Cannot resolve reference: Path not found: " + path, null);
                }
                JSPath pathFromRoot = FlexiUtils.fromFlexiNode(newValue);
                Object lastKey = pathFromRoot.getLast();
                JSPath parent = pathFromRoot.getParent();
                CompiledType linkedType = getContext().getRootType().resolve(parent);
                while (linkedType.isListContainer()) {
                    lastKey = parent.getLast();
                    parent = parent.getParent();
                    linkedType = getContext().getRootType().resolve(parent);
                }
                // ensure that the target is fully resolved
                List<String> subaccess = getReferencesAccess(linkedType.type, StringUtils.valueOfOrNull(lastKey));
                newValue = resolveValue(newValue, fieldType, subaccess, loopCheck);
                return newValue;
            } else {
                try {
                    LogV3.info("Forbidden Reference: " + path + " @ " + FlexiUtils.getPathString(value));
                } catch (final InvalidPathException e) {
                    LogV3.log(e);
                }
                return onUnresolvableReference(path);
            }
        }
        return null;
    }

    /**
     * @return
     */
    public JSPath getReferenceRoot() {
        return null;
    }

    protected FlexiJSonNode onUnresolvableReference(String path) {
        // TODO: Handling of forbidden references
        // DebugMode.debugger();
        return createFlexiJSonValue(getRefMarker() + "{" + path + "}");
    }

    private ThreadLocal<FlexiMapperContext> context = new ThreadLocal<FlexiMapperContext>();

    /**
     * @return
     */
    private FlexiMapperContext getContext() {
        FlexiMapperContext ret = context.get();
        DebugMode.breakIf(ret == null);
        return ret;
    }

    protected CompiledType handleConditionalTypeAnnotations(final CompiledType cType, final FlexiJSonObject obj, final String key) throws FlexiMapperException {
        try {
            final ClassCache cc = cType.getClassCache();
            if (cc != null) {
                {
                    final List<StorableConditionalType> annos = cc.getAnnotations(key, StorableConditionalType.class);
                    if (annos != null && annos.size() > 0) {
                        for (final StorableConditionalType a : annos) {
                            final org.appwork.moncompare.Condition condition = FlexiCondition.parse(a.condition());
                            condition.setTypeHandler(Arrays.asList(new FlexiTypeHandler()));
                            if (condition.matches(obj)) {
                                if (StringUtils.isNotEmpty(a.type())) {
                                    return CompiledType.create(new TypeBuilder().parse(a.type()), cType.type);
                                } else {
                                    return CompiledType.create(a.cls(), cType.type);
                                }
                            }
                        }
                    }
                }
                {
                    final List<StorableConditionalType2> annos = cc.getAnnotations(key, StorableConditionalType2.class);
                    if (annos != null && annos.size() > 0) {
                        for (final StorableConditionalType2 a : annos) {
                            final org.appwork.moncompare.Condition condition = FlexiCondition.parse(a.condition());
                            condition.setTypeHandler(Arrays.asList(new FlexiTypeHandler()));
                            if (condition.matches(obj)) {
                                if (StringUtils.isNotEmpty(a.type())) {
                                    return CompiledType.create(new TypeBuilder().parse(a.type()), cType.type);
                                } else {
                                    return CompiledType.create(a.cls(), cType.type);
                                }
                            }
                        }
                    }
                }
                {
                    final List<StorableConditionalType3> annos = cc.getAnnotations(key, StorableConditionalType3.class);
                    if (annos != null && annos.size() > 0) {
                        for (final StorableConditionalType3 a : annos) {
                            final org.appwork.moncompare.Condition condition = FlexiCondition.parse(a.condition());
                            condition.setTypeHandler(Arrays.asList(new FlexiTypeHandler()));
                            if (condition.matches(obj)) {
                                if (StringUtils.isNotEmpty(a.type())) {
                                    return CompiledType.create(new TypeBuilder().parse(a.type()), cType.type);
                                } else {
                                    return CompiledType.create(a.cls(), cType.type);
                                }
                            }
                        }
                    }
                }
            }
        } catch (final BadFormatException e) {
            throw new FlexiMapperException(obj, cType, e);
        } catch (final ConditionException e) {
            throw new FlexiMapperException(obj, cType, e);
        } catch (final TypeParserException e) {
            throw new FlexiMapperException(obj, cType, e);
        }
        return null;
    }

    protected void setValueToObject(final FlexiJSonNode node, final Object inst, final CompiledType instType, final Object value, final Setter setter) throws IllegalAccessException, InvocationTargetException, FlexiMapperException {
        setter.setValue(inst, value);
    }

    /**
     * return true if the exception should not be thrown
     *
     * @param ex
     * @return
     * @throws FlexiMapperException
     */
    protected Object returnFallbackOrThrowException(final FlexiMapperException ex) throws FlexiMapperException {
        if (this.isCollectExceptions()) {
            if (this.exceptions == null) {
                this.exceptions = new ArrayList<FlexiMapperException>();
            }
            this.exceptions.add(ex);
            return null;
        } else {
            throw ex;
        }
    }

    public ArrayList<FlexiMapperException> getExceptions() {
        return this.exceptions;
    }

    protected Setter getSetterByKey(final CompiledType cType, String key) {
        if (key == null) {
            return null;
        }
        Setter ret = cType.getClassCache().getSetter(key);
        if (ret == null) {
            if (Character.isUpperCase(key.charAt(0)) && this.isUpperCaseFirstKeyLetterIsEnabled()) {
                key = key.substring(0, 1).toLowerCase(Locale.ENGLISH) + key.substring(1);
                ret = cType.getClassCache().getSetter(key);
            }
        }
        if (ret == null && this.isGuessKeyNameCaseEnabled()) {
            for (final Setter ss : cType.getClassCache().getSetter()) {
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
    protected void onClassFieldMissing(final Object inst, final String key, final FlexiJSonNode value, final CompiledType cType) throws FlexiMapperException {
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
        CompiledType ct = CompiledType.create(type.getType());
        return (T) this.jsonToObject(json, ct);
    }

    /**
     * @param class1
     * @return
     * @throws FlexiMapperException
     */
    protected CompiledType mapClasses(final CompiledType class1) throws FlexiMapperException {
        if (class1.isInterface()) {
            if (class1.isSet()) {
                return CompiledType.create(this.autoMapSetInterface.type, class1.getAssignableSuperClass(Set.class).type);
            } else if (class1.isCollection()) {
                // return a type of type autoMapCollectionInterface, but with the generic Collection component types of class1
                return CompiledType.create(this.autoMapCollectionInterface.type, class1.getAssignableSuperClass(Collection.class).type);
            } else if (class1.isMap()) {
                return CompiledType.create(this.autoMapMapInterface.type, class1.getAssignableSuperClass(Map.class).type);
            }
        }
        return class1;
    }

    private boolean collectExceptions = false;

    public boolean isCollectExceptions() {
        return this.collectExceptions;
    }

    public void setCollectExceptions(final boolean collectExceptions) {
        this.collectExceptions = collectExceptions;
    }

    public void setIgnoreIllegalArgumentMappings(final boolean ignoreIllegalArgumentMappings) {
        this.ignoreIllegalArgumentMappings = ignoreIllegalArgumentMappings;
    }

    public void setIgnoreIllegalEnumMappings(final boolean ignoreIllegalEnumMappings) {
        this.ignoreIllegalEnumMappings = ignoreIllegalEnumMappings;
    }

    public void setIgnorePrimitiveNullMapping(final boolean ignoreIllegalNullArguments) {
        this.ignorePrimitiveNullMapping = ignoreIllegalNullArguments;
    }

    /**
     * @param example
     * @param simpleTypeRef
     * @return
     */
    public <T> T convert(final Object obj, final TypeRef<T> targetType) throws FlexiMapperException {
        return this.jsonToObject(this.objectToJsonNode(obj), targetType);
    }

    /**
     * @param inst
     * @param typeRef
     * @return
     * @throws FlexiMapperException
     */
    public FlexiJSonNode objectToJsonNode(final Object obj, final TypeRef<?> targetType) throws FlexiMapperException {
        return this.objectToJsonNode(obj, CompiledType.create(targetType.getType()));
    }

    /**
     * @return
     */
    public static ArrayList<FlexiTypeMapper> getThreadMappers() {
        return THREAD_MAPPERS.get();
    }
}
