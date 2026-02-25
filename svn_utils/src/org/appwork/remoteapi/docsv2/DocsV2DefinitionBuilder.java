package org.appwork.remoteapi.docsv2;

import java.lang.annotation.Annotation;
import java.lang.reflect.Array;
import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.lang.reflect.WildcardType;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.appwork.remoteapi.InterfaceHandler;
import org.appwork.remoteapi.RemoteAPIInterface;
import org.appwork.remoteapi.RemoteAPIRequest;
import org.appwork.remoteapi.RemoteAPIResponse;
import org.appwork.remoteapi.annotations.APIReturnTypes;
import org.appwork.remoteapi.annotations.APITag;
import org.appwork.remoteapi.annotations.APITagDefinition;
import org.appwork.remoteapi.annotations.APIParameterNames;
import org.appwork.remoteapi.annotations.APIParameterOptions;
import org.appwork.remoteapi.annotations.ApiDoc;
import org.appwork.remoteapi.annotations.ApiDocExample;
import org.appwork.remoteapi.annotations.HiddenForHelpDocs;
import org.appwork.remoteapi.docsv2.model.DocsV2Definition;
import org.appwork.remoteapi.docsv2.model.DocsV2Definition.EndpointDoc;
import org.appwork.remoteapi.docsv2.model.DocsV2Definition.EnumValueDoc;
import org.appwork.remoteapi.docsv2.model.DocsV2Definition.ExceptionDoc;
import org.appwork.remoteapi.docsv2.model.DocsV2Definition.FieldDoc;
import org.appwork.remoteapi.docsv2.model.DocsV2Definition.ParameterDoc;
import org.appwork.remoteapi.docsv2.model.DocsV2Definition.ReturnTypeDoc;
import org.appwork.remoteapi.docsv2.model.DocsV2Definition.TagDoc;
import org.appwork.remoteapi.docsv2.model.DocsV2Definition.TypeDoc;
import org.appwork.remoteapi.docsv2.model.DocsV2StorableDefinition;
import org.appwork.remoteapi.exceptions.BadParameterException;
import org.appwork.remoteapi.exceptions.BadRequestException;
import org.appwork.remoteapi.exceptions.BasicRemoteAPIException;
import org.appwork.remoteapi.exceptions.InternalApiException;
import org.appwork.serializer.Deser;
import org.appwork.serializer.SC;
import org.appwork.storage.StorableDoc;
import org.appwork.storage.StorableExample;
import org.appwork.storage.StorableAvailableSince;
import org.appwork.storage.StorableDeprecatedSince;
import org.appwork.storage.flexijson.FlexiJsonMapperForConfig;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.FlexiUtils;
import org.appwork.storage.flexijson.mapper.FlexiJSonMapper;
import org.appwork.storage.flexijson.stringify.FlexiJSonPrettyPrinterForConfig;
import org.appwork.storage.flexijson.stringify.FlexiJSonPrettyStringify;
import org.appwork.storage.simplejson.mapper.ClassCache;
import org.appwork.storage.simplejson.mapper.Getter;
import org.appwork.storage.simplejson.mapper.Setter;
import org.appwork.utils.StringUtils;
import org.appwork.utils.reflection.CompiledType;
import org.appwork.utils.reflection.JsonSyntax;

public class DocsV2DefinitionBuilder {
    private static final JsonSyntax JSON_SYNTAX = new JsonSyntax();

    public String createInteractiveDocsDefinitionJson(final Map<String, InterfaceHandler<RemoteAPIInterface>> handlers, final DocsV2ProjectDataProvider provider) {
        return Deser.toString(buildInteractiveDocsDefinition(handlers, provider), SC.NETWORK_TRANSFER);
    }

    public String createStorableDefinitionJson(final String javaType, final Map<String, InterfaceHandler<RemoteAPIInterface>> handlers, final DocsV2ProjectDataProvider provider) {
        final DocsV2Definition definition = buildInteractiveDocsDefinition(handlers, provider);
        final DocsV2StorableDefinition ret = new DocsV2StorableDefinition();
        ret.setGeneratedAt(definition.getGeneratedAt());
        ret.setJavaType(javaType);
        final List<TypeDoc> resultTypes = new ArrayList<TypeDoc>();
        if (definition.getTypes() != null && javaType != null) {
            for (final TypeDoc type : definition.getTypes()) {
                if (type == null) {
                    continue;
                }
                if (javaType.equals(type.getJavaType()) || javaType.equals(type.getName())) {
                    resultTypes.add(type);
                    break;
                }
            }
        }
        ret.setTypes(resultTypes);
        return Deser.toString(ret, SC.NETWORK_TRANSFER);
    }

    public String createExampleJson(final String javaType, final boolean includeDocumentation, final Map<String, InterfaceHandler<RemoteAPIInterface>> handlers, final DocsV2ProjectDataProvider provider) throws Exception {
        final Class<?> targetClass = resolveExampleTargetClass(javaType, handlers, provider);
        if (targetClass == null) {
            throw new IllegalArgumentException("Type not found: " + String.valueOf(javaType));
        }
        if (Throwable.class.isAssignableFrom(targetClass)) {
            return buildExceptionEnvelopeExampleJson(targetClass, includeDocumentation);
        }
        final String directExample = extractExample(targetClass);
        if (StringUtils.isNotEmpty(directExample)) {
            return renderAnnotationExample(directExample, targetClass, includeDocumentation);
        }
        Object exampleInstance = createExampleInstance(targetClass);
        if (exampleInstance != null) {
            applyAnnotatedExamplesToObject(exampleInstance, targetClass);
        }
        if (includeDocumentation) {
            if (exampleInstance != null) {
                return FlexiUtils.serializeConfigStorable(exampleInstance);
            }
            final FlexiJsonMapperForConfig mapper = new FlexiJsonMapperForConfig();
            final FlexiJSonNode node = mapper.objectToJsonNode(null, CompiledType.create(targetClass));
            return new FlexiJSonPrettyPrinterForConfig(null).toJSONString(node);
        } else {
            if (exampleInstance != null) {
                return FlexiUtils.serializeToPrettyJson(exampleInstance);
            }
            final FlexiJSonMapper mapper = new FlexiJSonMapper();
            final FlexiJSonNode node = mapper.objectToJsonNode(null, CompiledType.create(targetClass));
            return new FlexiJSonPrettyStringify().toJSONString(node);
        }
    }

    private String renderAnnotationExample(final String rawExample, final Class<?> targetClass, final boolean includeDocumentation) throws Exception {
        final String example = StringUtils.valueOrEmpty(rawExample).trim();
        if (StringUtils.isEmpty(example)) {
            return "";
        }
        if (includeDocumentation) {
            try {
                final Object typed = FlexiUtils.jsonToObject(example, CompiledType.create(targetClass));
                return FlexiUtils.serializeConfigStorable(typed);
            } catch (final Throwable ignore) {
            }
        }
        try {
            final Object parsed = FlexiUtils.jsonToObject(example, CompiledType.OBJECT);
            return FlexiUtils.serializeToPrettyJson(parsed);
        } catch (final Throwable ignore) {
        }
        return example;
    }

    private void applyAnnotatedExamplesToObject(final Object target, final Class<?> targetClass) {
        if (target == null || targetClass == null) {
            return;
        }
        try {
            final ClassCache cache = ClassCache.getClassCache(targetClass);
            for (final Getter getter : cache.getGetter()) {
                if (getter == null || StringUtils.isEmpty(getter.getKey())) {
                    continue;
                }
                final Setter setter = cache.getSetter(getter.getKey());
                if (setter == null) {
                    continue;
                }
                String example = getter.getMethod() == null ? null : extractExample(getter.getMethod());
                if (StringUtils.isEmpty(example)) {
                    final Field field = findFieldInHierarchy(targetClass, getter.getKey());
                    if (field != null) {
                        example = extractExample(field);
                    }
                }
                if (StringUtils.isEmpty(example)) {
                    continue;
                }
                try {
                    final CompiledType compiledType = CompiledType.create(setter.getType(), targetClass);
                    final Object value = FlexiUtils.jsonToObject(example, compiledType);
                    setter.setValue(target, value);
                } catch (final Throwable ignore) {
                }
            }
        } catch (final Throwable ignore) {
        }
    }

    private Field findFieldInHierarchy(final Class<?> startClass, final String fieldName) {
        Class<?> current = startClass;
        while (current != null && current != Object.class) {
            try {
                final Field field = current.getDeclaredField(fieldName);
                if (field != null) {
                    return field;
                }
            } catch (final Throwable ignore) {
            }
            current = current.getSuperclass();
        }
        return null;
    }

    public DocsV2Definition buildInteractiveDocsDefinition(final Map<String, InterfaceHandler<RemoteAPIInterface>> handlers, final DocsV2ProjectDataProvider provider) {
        final DocsV2Definition ret = new DocsV2Definition();
        ret.setGeneratedAt(System.currentTimeMillis());
        final List<EndpointDoc> endpoints = new ArrayList<EndpointDoc>();
        final Set<Class<?>> schemaClasses = new HashSet<Class<?>>();
        final List<String> namespaces = new ArrayList<String>(handlers.keySet());
        Collections.sort(namespaces);
        for (final String namespace : namespaces) {
            final InterfaceHandler<RemoteAPIInterface> handler = handlers.get(namespace);
            final Map<String, Method> methods = handler.getMethodsMap();
            final Set<String> seen = new HashSet<String>();
            for (final Map.Entry<String, Method> entry : methods.entrySet()) {
                final Method method = entry.getValue();
                if (method == null) {
                    continue;
                }
                final int parameterCount = handler.getParameterCount(method);
                final String methodName = exposedMethodName(entry.getKey(), method, parameterCount);
                final String dedupeKey = namespace + "/" + methodName + "#" + parameterCount;
                if (!seen.add(dedupeKey) || method.getAnnotation(HiddenForHelpDocs.class) != null) {
                    continue;
                }
                final EndpointDoc endpoint = new EndpointDoc();
                endpoint.setNamespace(namespace);
                endpoint.setMethodName(methodName);
                endpoint.setPath(namespace + "/" + methodName);
                final DocInfo endpointDocInfo = extractDocInfo(method, provider);
                endpoint.setDescription(endpointDocInfo.text);
                endpoint.setWikiLinks(endpointDocInfo.wikiLinks);
                final LifecycleInfo endpointLifecycle = extractLifecycleInfo(method);
                if (StringUtils.isEmpty(endpointLifecycle.availableSince) || StringUtils.isEmpty(endpointLifecycle.deprecatedSince)) {
                    final LifecycleInfo declaringTypeLifecycle = extractLifecycleInfo(method.getDeclaringClass());
                    if (StringUtils.isEmpty(endpointLifecycle.availableSince)) {
                        endpointLifecycle.availableSince = declaringTypeLifecycle.availableSince;
                        endpointLifecycle.availableSinceMessage = declaringTypeLifecycle.availableSinceMessage;
                    }
                    if (StringUtils.isEmpty(endpointLifecycle.deprecatedSince)) {
                        endpointLifecycle.deprecatedSince = declaringTypeLifecycle.deprecatedSince;
                        endpointLifecycle.deprecatedSinceMessage = declaringTypeLifecycle.deprecatedSinceMessage;
                    }
                }
                endpoint.setAvailableSince(endpointLifecycle.availableSince);
                endpoint.setAvailableSinceMessage(endpointLifecycle.availableSinceMessage);
                endpoint.setDeprecatedSince(endpointLifecycle.deprecatedSince);
                endpoint.setDeprecatedSinceMessage(endpointLifecycle.deprecatedSinceMessage);
                endpoint.setTags(extractTags(method));
                endpoint.setParameters(new ArrayList<ParameterDoc>());
                final APIParameterNames parameterNames = method.getAnnotation(APIParameterNames.class);
                final APIParameterOptions parameterOptions = method.getAnnotation(APIParameterOptions.class);
                final String[] explicitNames = parameterNames != null ? parameterNames.value() : null;
                final String[] explicitOptions = parameterOptions != null ? parameterOptions.value() : null;
                int visibleIndex = 0;
                for (int i = 0; i < method.getParameterTypes().length; i++) {
                    final Class<?> parameterClass = method.getParameterTypes()[i];
                    if (parameterClass == RemoteAPIRequest.class || parameterClass == RemoteAPIResponse.class) {
                        continue;
                    }
                    final ParameterDoc parameter = new ParameterDoc();
                    parameter.setName(explicitNames != null && i < explicitNames.length ? explicitNames[i] : ("arg" + visibleIndex));
                    parameter.setType(toSimpleTypeString(method.getGenericParameterTypes()[i]));
                    parameter.setJavaType(parameterClass.getName());
                    parameter.setMulti(parameterClass.isArray() || Collection.class.isAssignableFrom(parameterClass));
                    if (explicitOptions != null && i < explicitOptions.length && StringUtils.isNotEmpty(explicitOptions[i])) {
                        parameter.setOptionsEndpoint(explicitOptions[i]);
                    }
                    parameter.setOptions(extractInlineOptions(parameterClass));
                    if (parameter.getOptions() != null && parameter.getOptions().size() > 0 && StringUtils.isEmpty(parameter.getEditor())) {
                        parameter.setEditor(parameter.isMulti() ? "multiselect" : "select");
                    }
                    if (StringUtils.isNotEmpty(parameter.getOptionsEndpoint()) && StringUtils.isEmpty(parameter.getEditor())) {
                        parameter.setEditor(parameter.isMulti() ? "multiselect" : "select");
                    }
                    if (StringUtils.isEmpty(parameter.getEditor())) {
                        parameter.setEditor(inferEditor(method.getGenericParameterTypes()[i], parameterClass, parameter.isMulti()));
                    }
                    endpoint.getParameters().add(parameter);
                    collectClassesFromType(method.getGenericParameterTypes()[i], schemaClasses);
                    visibleIndex++;
                }
                endpoint.setReturnType(toSimpleTypeString(method.getGenericReturnType()));
                endpoint.setReturnJavaType(method.getReturnType() != null ? method.getReturnType().getName() : null);
                endpoint.setReturnTypes(createReturnTypes(method));
                if (method.getGenericReturnType() != void.class && method.getGenericReturnType() != Void.class) {
                    collectClassesFromType(method.getGenericReturnType(), schemaClasses);
                }
                final APIReturnTypes additionalReturnTypes = method.getAnnotation(APIReturnTypes.class);
                if (additionalReturnTypes != null && additionalReturnTypes.value() != null) {
                    for (final Class<?> cls : additionalReturnTypes.value()) {
                        if (cls != null && cls != void.class && cls != Void.class) {
                            collectClassesFromType(cls, schemaClasses);
                        }
                    }
                }
                endpoint.setExceptions(new ArrayList<ExceptionDoc>());
                for (final Class<?> exceptionClass : method.getExceptionTypes()) {
                    addExceptionDocIfMissing(endpoint.getExceptions(), exceptionClass, schemaClasses, provider);
                }
                addExceptionDocIfMissing(endpoint.getExceptions(), InternalApiException.class, schemaClasses, provider);
                addExceptionDocIfMissing(endpoint.getExceptions(), BadParameterException.class, schemaClasses, provider);
                addExceptionDocIfMissing(endpoint.getExceptions(), BadRequestException.class, schemaClasses, provider);
                endpoints.add(endpoint);
            }
        }
        Collections.sort(endpoints, new Comparator<EndpointDoc>() {
            @Override
            public int compare(final EndpointDoc o1, final EndpointDoc o2) {
                return StringUtils.valueOrEmpty(o1.getPath()).compareTo(StringUtils.valueOrEmpty(o2.getPath()));
            }
        });
        ret.setEndpoints(endpoints);
        ret.setTypes(buildTypeDocs(schemaClasses, provider));
        return ret;
    }

    private List<ReturnTypeDoc> createReturnTypes(final Method method) {
        final ArrayList<ReturnTypeDoc> ret = new ArrayList<ReturnTypeDoc>();
        final HashSet<String> dedupe = new HashSet<String>();
        final Class<?> declared = method == null ? null : method.getReturnType();
        if (declared != null && declared != void.class && declared != Void.class) {
            addReturnType(ret, dedupe, declared);
        }
        if (method != null) {
            final APIReturnTypes extra = method.getAnnotation(APIReturnTypes.class);
            if (extra != null && extra.value() != null) {
                for (final Class<?> cls : extra.value()) {
                    if (cls != null && cls != void.class && cls != Void.class) {
                        addReturnType(ret, dedupe, cls);
                    }
                }
            }
        }
        return ret;
    }

    private void addReturnType(final List<ReturnTypeDoc> sink, final Set<String> dedupe, final Class<?> cls) {
        if (sink == null || dedupe == null || cls == null) {
            return;
        }
        final String javaName = cls.getName();
        if (!dedupe.add(javaName)) {
            return;
        }
        final ReturnTypeDoc doc = new ReturnTypeDoc();
        doc.setJavaType(javaName);
        doc.setType(toSimpleTypeString(cls));
        sink.add(doc);
    }

    @SuppressWarnings("unchecked")
    private BasicRemoteAPIException instantiateRemoteAPIException(final Class<? extends BasicRemoteAPIException> exceptionClass) {
        try {
            return exceptionClass.getDeclaredConstructor().newInstance();
        } catch (final Throwable e) {
            try {
                return exceptionClass.getDeclaredConstructor(String.class).newInstance("");
            } catch (final Throwable ignored) {
                return null;
            }
        }
    }

    private void addExceptionDocIfMissing(final List<ExceptionDoc> target, final Class<?> exceptionClass, final Set<Class<?>> schemaClasses, final DocsV2ProjectDataProvider provider) {
        if (target == null || exceptionClass == null) {
            return;
        }
        final String typeName = exceptionClass.getName();
        for (final ExceptionDoc existing : target) {
            if (existing != null && typeName.equals(existing.getType())) {
                return;
            }
        }
        final ExceptionDoc exception = new ExceptionDoc();
        exception.setType(typeName);
        exception.setSimpleName(exceptionClass.getSimpleName());
        final DocInfo exceptionDocInfo = extractDocInfo(exceptionClass, provider);
        exception.setDescription(exceptionDocInfo.text);
        exception.setWikiLinks(exceptionDocInfo.wikiLinks);
        if (BasicRemoteAPIException.class.isAssignableFrom(exceptionClass)) {
            try {
                final BasicRemoteAPIException sample = instantiateRemoteAPIException(exceptionClass.asSubclass(BasicRemoteAPIException.class));
                if (sample != null) {
                    exception.setHttp(sample.getCode().getCode() + " " + sample.getCode().getDescription());
                    final Object data = sample.getData();
                    if (data != null) {
                        exception.setDataJavaType(data.getClass().getName());
                        exception.setDataType(toSimpleTypeString(data.getClass()));
                        collectClassesFromType(data.getClass(), schemaClasses);
                    }
                    if (StringUtils.isEmpty(exception.getDescription())) {
                        String description = String.valueOf(sample.getType());
                        if (data != null) {
                            description += " - " + String.valueOf(data);
                        }
                        exception.setDescription(description);
                    }
                }
            } catch (final Throwable ignore) {
            }
        }
        target.add(exception);
        collectClassesFromType(exceptionClass, schemaClasses);
    }

    private List<TagDoc> extractTags(final Method method) {
        final List<TagDoc> ret = new ArrayList<TagDoc>();
        if (method == null) {
            return ret;
        }
        final APITag tagAnnotation = method.getAnnotation(APITag.class);
        final HashSet<String> dedupe = new HashSet<String>();
        if (tagAnnotation != null && tagAnnotation.value() != null && tagAnnotation.value().length > 0) {
            for (final Class<? extends APITagDefinition> tagClass : tagAnnotation.value()) {
                if (tagClass == null) {
                    continue;
                }
                String name = null;
                String description = null;
                String icon = null;
                try {
                    final APITagDefinition tagDefinition = tagClass.getDeclaredConstructor().newInstance();
                    if (tagDefinition != null) {
                        name = StringUtils.valueOrEmpty(tagDefinition.getName()).trim();
                        description = StringUtils.valueOrEmpty(tagDefinition.getDescription()).trim();
                        icon = StringUtils.valueOrEmpty(tagDefinition.getIcon()).trim();
                    }
                } catch (final Throwable ignored) {
                }
                if (StringUtils.isEmpty(name)) {
                    name = tagClass.getSimpleName();
                }
                final String key = name + "|" + StringUtils.valueOrEmpty(description) + "|" + StringUtils.valueOrEmpty(icon);
                if (!dedupe.add(key)) {
                    continue;
                }
                final TagDoc doc = new TagDoc();
                doc.setName(name);
                doc.setDescription(description);
                doc.setIcon(icon);
                ret.add(doc);
            }
        }
        return ret;
    }

    private List<TypeDoc> buildTypeDocs(final Set<Class<?>> roots, final DocsV2ProjectDataProvider provider) {
        final LinkedHashMap<String, TypeDoc> docs = new LinkedHashMap<String, TypeDoc>();
        final ArrayDeque<Class<?>> queue = new ArrayDeque<Class<?>>();
        for (final Class<?> cls : roots) {
            if (cls != null) {
                queue.add(cls);
            }
        }
        while (!queue.isEmpty()) {
            final Class<?> cls = queue.poll();
            if (cls == null || shouldSkipSchemaClass(cls) || docs.containsKey(cls.getName())) {
                continue;
            }
            final TypeDoc typeDoc = new TypeDoc();
            typeDoc.setJavaType(cls.getName());
            typeDoc.setName(toSimpleTypeString(cls));
            final DocInfo typeDocInfo = extractDocInfo(cls, provider);
            typeDoc.setDescription(typeDocInfo.text);
            typeDoc.setWikiLinks(typeDocInfo.wikiLinks);
            final LifecycleInfo typeLifecycle = extractLifecycleInfo(cls);
            typeDoc.setAvailableSince(typeLifecycle.availableSince);
            typeDoc.setAvailableSinceMessage(typeLifecycle.availableSinceMessage);
            typeDoc.setDeprecatedSince(typeLifecycle.deprecatedSince);
            typeDoc.setDeprecatedSinceMessage(typeLifecycle.deprecatedSinceMessage);
            if (cls.isEnum()) {
                typeDoc.setKind("enum");
                final List<String> enumValues = new ArrayList<String>();
                final List<EnumValueDoc> enumValueDocs = new ArrayList<EnumValueDoc>();
                final Object[] values = cls.getEnumConstants();
                if (values != null) {
                    for (final Object value : values) {
                        final String enumName = String.valueOf(value);
                        enumValues.add(enumName);
                        final EnumValueDoc enumValueDoc = new EnumValueDoc();
                        enumValueDoc.setName(enumName);
                        try {
                            final Field enumField = cls.getField(enumName);
                            final DocInfo enumDocInfo = extractDocInfo(enumField, provider, false);
                            enumValueDoc.setDescription(enumDocInfo.text);
                            enumValueDoc.setWikiLinks(enumDocInfo.wikiLinks);
                        } catch (final Throwable ignore) {
                        }
                        enumValueDocs.add(enumValueDoc);
                    }
                }
                typeDoc.setEnumValues(enumValues);
                typeDoc.setEnumValueDocs(enumValueDocs);
            } else {
                typeDoc.setKind("object");
                final List<FieldDoc> fields = new ArrayList<FieldDoc>();
                typeDoc.setFields(fields);
                if (Map.class.isAssignableFrom(cls) || Collection.class.isAssignableFrom(cls)) {
                    // Maps/collections are serialized by their elements, not by bean getter properties.
                    docs.put(cls.getName(), typeDoc);
                    continue;
                }
                try {
                    final ClassCache cache = ClassCache.getClassCache(cls);
                    final List<Getter> getters = new ArrayList<Getter>(cache.getGetter());
                    Collections.sort(getters, new Comparator<Getter>() {
                        @Override
                        public int compare(final Getter o1, final Getter o2) {
                            return StringUtils.valueOrEmpty(o1.getKey()).compareTo(StringUtils.valueOrEmpty(o2.getKey()));
                        }
                    });
                    for (final Getter getter : getters) {
                        final FieldDoc field = new FieldDoc();
                        field.setName(getter.getKey());
                        field.setType(toSimpleTypeString(getter.type));
                        field.setJavaType(getter.type.toString());
                        final DocInfo getterDocInfo = getter.getMethod() != null ? extractDocInfo(getter.getMethod(), provider, false) : null;
                        field.setDescription(getterDocInfo != null ? getterDocInfo.text : null);
                        field.setWikiLinks(getterDocInfo != null ? getterDocInfo.wikiLinks : null);
                        if (getter.getMethod() != null) {
                            final LifecycleInfo getterLifecycle = extractLifecycleInfo(getter.getMethod());
                            field.setAvailableSince(getterLifecycle.availableSince);
                            field.setAvailableSinceMessage(getterLifecycle.availableSinceMessage);
                            field.setDeprecatedSince(getterLifecycle.deprecatedSince);
                            field.setDeprecatedSinceMessage(getterLifecycle.deprecatedSinceMessage);
                        }
                        if (StringUtils.isEmpty(field.getDescription())) {
                            try {
                                final Field reflected = cls.getDeclaredField(field.getName());
                                if (reflected != null) {
                                    if (StringUtils.isEmpty(field.getDescription())) {
                                        final DocInfo reflectedDocInfo = extractDocInfo(reflected, provider, false);
                                        field.setDescription(reflectedDocInfo.text);
                                        if (field.getWikiLinks() == null || field.getWikiLinks().size() == 0) {
                                            field.setWikiLinks(reflectedDocInfo.wikiLinks);
                                        }
                                    }
                                    if (StringUtils.isEmpty(field.getAvailableSince()) || StringUtils.isEmpty(field.getDeprecatedSince())) {
                                        final LifecycleInfo reflectedLifecycle = extractLifecycleInfo(reflected);
                                        if (StringUtils.isEmpty(field.getAvailableSince())) {
                                            field.setAvailableSince(reflectedLifecycle.availableSince);
                                            field.setAvailableSinceMessage(reflectedLifecycle.availableSinceMessage);
                                        }
                                        if (StringUtils.isEmpty(field.getDeprecatedSince())) {
                                            field.setDeprecatedSince(reflectedLifecycle.deprecatedSince);
                                            field.setDeprecatedSinceMessage(reflectedLifecycle.deprecatedSinceMessage);
                                        }
                                    }
                                }
                            } catch (final Throwable ignore) {
                            }
                        }
                        fields.add(field);
                        collectClassesFromType(getter.type, queue);
                    }
                } catch (final Throwable e) {
                    typeDoc.setDescription(appendFallbackError(typeDoc.getDescription(), e));
                }
            }
            docs.put(cls.getName(), typeDoc);
        }
        final List<TypeDoc> ret = new ArrayList<TypeDoc>(docs.values());
        Collections.sort(ret, new Comparator<TypeDoc>() {
            @Override
            public int compare(final TypeDoc o1, final TypeDoc o2) {
                return StringUtils.valueOrEmpty(o1.getJavaType()).compareTo(StringUtils.valueOrEmpty(o2.getJavaType()));
            }
        });
        return ret;
    }

    private void collectClassesFromType(final Type type, final Set<Class<?>> sink) {
        if (type == null || sink == null) {
            return;
        }
        try {
            collectClassesFromCompiledType(CompiledType.create(type), sink);
            return;
        } catch (final Throwable ignore) {
        }
        if (type instanceof Class<?>) {
            final Class<?> cls = (Class<?>) type;
            if (cls.isArray()) {
                collectClassesFromType(cls.getComponentType(), sink);
            } else {
                sink.add(cls);
            }
            return;
        }
        if (type instanceof ParameterizedType) {
            final ParameterizedType p = (ParameterizedType) type;
            collectClassesFromType(p.getRawType(), sink);
            for (final Type t : p.getActualTypeArguments()) {
                collectClassesFromType(t, sink);
            }
            return;
        }
        if (type instanceof TypeVariable<?>) {
            for (final Type t : ((TypeVariable<?>) type).getBounds()) {
                collectClassesFromType(t, sink);
            }
            return;
        }
        if (type instanceof WildcardType) {
            final WildcardType w = (WildcardType) type;
            for (final Type t : w.getUpperBounds()) {
                collectClassesFromType(t, sink);
            }
            for (final Type t : w.getLowerBounds()) {
                collectClassesFromType(t, sink);
            }
            return;
        }
        if (type instanceof GenericArrayType) {
            collectClassesFromType(((GenericArrayType) type).getGenericComponentType(), sink);
        }
    }

    private void collectClassesFromCompiledType(final CompiledType type, final Set<Class<?>> sink) {
        if (type == null || sink == null) {
            return;
        }
        if (type.raw != null) {
            if (!type.raw.isArray()) {
                sink.add(type.raw);
            }
        }
        final CompiledType[] componentTypes = type.getComponentTypes();
        if (componentTypes != null) {
            for (final CompiledType componentType : componentTypes) {
                collectClassesFromCompiledType(componentType, sink);
            }
        }
    }

    private void collectClassesFromType(final Type type, final ArrayDeque<Class<?>> sink) {
        final HashSet<Class<?>> classes = new HashSet<Class<?>>();
        collectClassesFromType(type, classes);
        sink.addAll(classes);
    }

    private List<String> extractInlineOptions(final Class<?> parameterClass) {
        if (parameterClass == null) {
            return null;
        }
        if (parameterClass == boolean.class || parameterClass == Boolean.class) {
            return Arrays.asList("true", "false");
        }
        if (parameterClass.isEnum()) {
            final ArrayList<String> values = new ArrayList<String>();
            final Object[] constants = parameterClass.getEnumConstants();
            if (constants != null) {
                for (final Object value : constants) {
                    values.add(String.valueOf(value));
                }
            }
            return values;
        }
        if (parameterClass.isArray() && parameterClass.getComponentType() != null && parameterClass.getComponentType().isEnum()) {
            final ArrayList<String> values = new ArrayList<String>();
            for (final Object value : parameterClass.getComponentType().getEnumConstants()) {
                values.add(String.valueOf(value));
            }
            return values;
        }
        return null;
    }

    private String inferEditor(final Type genericType, final Class<?> parameterClass, final boolean multi) {
        if (parameterClass == boolean.class || parameterClass == Boolean.class) {
            return "checkbox";
        }
        if (parameterClass.isEnum()) {
            return "select";
        }
        if (multi || Map.class.isAssignableFrom(parameterClass)) {
            return "json";
        }
        final String typeText = toSimpleTypeString(genericType);
        if (typeText.contains("Map<") || typeText.contains("{") || typeText.contains("[]")) {
            return "json";
        }
        return "text";
    }

    private boolean shouldSkipSchemaClass(final Class<?> cls) {
        if (cls == null || cls.isPrimitive() || cls == Object.class || cls == Class.class) {
            return true;
        }
        if (RemoteAPIRequest.class.isAssignableFrom(cls) || RemoteAPIResponse.class.isAssignableFrom(cls)) {
            return true;
        }
        if (cls == String.class || cls == Boolean.class || cls == Character.class) {
            return true;
        }
        if (Number.class.isAssignableFrom(cls) || Date.class.isAssignableFrom(cls)) {
            return true;
        }
        final String name = cls.getName();
        if (name.startsWith("java.") || name.startsWith("javax.") || name.startsWith("sun.")) {
            return !cls.isEnum();
        }
        return false;
    }

    private String appendFallbackError(final String base, final Throwable e) {
        final String msg = e == null ? null : e.getMessage();
        if (StringUtils.isEmpty(msg)) {
            return base;
        }
        if (StringUtils.isEmpty(base)) {
            return "Schema extraction fallback: " + msg;
        }
        return base + " | Schema extraction fallback: " + msg;
    }

    private DocInfo extractDocInfo(final AnnotatedElement element, final DocsV2ProjectDataProvider provider) {
        return extractDocInfo(element, provider, true);
    }

    private DocInfo extractDocInfo(final AnnotatedElement element, final DocsV2ProjectDataProvider provider, final boolean inferWikiLinks) {
        final DocInfo ret = new DocInfo();
        if (element == null) {
            return ret;
        }
        String storableText = null;
        final StorableDoc storableDoc = element.getAnnotation(StorableDoc.class);
        if (storableDoc != null) {
            if (StringUtils.isNotEmpty(storableDoc.value())) {
                storableText = storableDoc.value();
            }
            ret.wikiLinks = extractWikiLinks(storableDoc, provider);
        }
        String apiText = null;
        final ApiDoc apiDoc = element.getAnnotation(ApiDoc.class);
        if (apiDoc != null && StringUtils.isNotEmpty(apiDoc.value())) {
            apiText = apiDoc.value();
        }
        if (StringUtils.isNotEmpty(storableText) && StringUtils.isNotEmpty(apiText)) {
            ret.text = StringUtils.equals(storableText, apiText) ? storableText : storableText + "\n\n" + apiText;
        } else if (StringUtils.isNotEmpty(storableText)) {
            ret.text = storableText;
        } else if (StringUtils.isNotEmpty(apiText)) {
            ret.text = apiText;
        }
        if (inferWikiLinks && (ret.wikiLinks == null || ret.wikiLinks.size() == 0) && provider != null) {
            ret.wikiLinks = provider.inferWikiLinks(element);
        }
        if (ret.wikiLinks != null && ret.wikiLinks.size() == 0) {
            ret.wikiLinks = null;
        }
        return ret;
    }

    private String extractExample(final AnnotatedElement element) {
        if (element == null) {
            return null;
        }
        final StorableExample storableExample = element.getAnnotation(StorableExample.class);
        if (storableExample != null && StringUtils.isNotEmpty(storableExample.value())) {
            return storableExample.value();
        }
        final ApiDocExample apiDocExample = element.getAnnotation(ApiDocExample.class);
        if (apiDocExample != null && StringUtils.isNotEmpty(apiDocExample.value())) {
            return apiDocExample.value();
        }
        return null;
    }

    private List<String> extractWikiLinks(final StorableDoc storableDoc, final DocsV2ProjectDataProvider provider) {
        if (storableDoc == null || provider == null) {
            return null;
        }
        final ArrayList<String> ret = new ArrayList<String>();
        addWikiLink(ret, storableDoc.wiki(), provider);
        addWikiLink(ret, storableDoc.wiki2(), provider);
        addWikiLink(ret, storableDoc.wiki3(), provider);
        return ret.size() == 0 ? null : ret;
    }

    private void addWikiLink(final List<String> sink, final String wikiRef, final DocsV2ProjectDataProvider provider) {
        if (sink == null || provider == null || StringUtils.isEmpty(wikiRef)) {
            return;
        }
        final String wikiUrl = provider.toWikiUrl(wikiRef);
        if (StringUtils.isEmpty(wikiUrl) || sink.contains(wikiUrl)) {
            return;
        }
        sink.add(wikiUrl);
    }

    private String exposedMethodName(final String mapKey, final Method method, final int parameterCount) {
        if (mapKey == null) {
            return method.getName();
        }
        if ("help".equals(mapKey)) {
            return "help";
        }
        if (parameterCount >= 0) {
            final String suffix = String.valueOf(parameterCount);
            if (mapKey.endsWith(suffix) && mapKey.length() > suffix.length()) {
                return mapKey.substring(0, mapKey.length() - suffix.length());
            }
        }
        return mapKey;
    }

    private String toSimpleTypeString(final Type type) {
        if (type == null) {
            return "void";
        }
        if (type == void.class || type == Void.class) {
            return "void";
        }
        try {
            return CompiledType.create(type).toString(JSON_SYNTAX);
        } catch (final Throwable ignore) {
        }
        return String.valueOf(type);
    }

    private LifecycleInfo extractLifecycleInfo(final AnnotatedElement element) {
        final LifecycleInfo ret = new LifecycleInfo();
        if (element == null) {
            return ret;
        }
        final StorableAvailableSince available = element.getAnnotation(StorableAvailableSince.class);
        if (available != null && StringUtils.isNotEmpty(available.value())) {
            ret.availableSince = available.value();
            if (StringUtils.isNotEmpty(available.message())) {
                ret.availableSinceMessage = available.message();
            }
        }
        final StorableDeprecatedSince deprecated = element.getAnnotation(StorableDeprecatedSince.class);
        if (deprecated != null && StringUtils.isNotEmpty(deprecated.value())) {
            ret.deprecatedSince = deprecated.value();
            if (StringUtils.isNotEmpty(deprecated.message())) {
                ret.deprecatedSinceMessage = deprecated.message();
            }
        }
        return ret;
    }

    private Class<?> resolveExampleTargetClass(final String javaType, final Map<String, InterfaceHandler<RemoteAPIInterface>> handlers, final DocsV2ProjectDataProvider provider) {
        String type = StringUtils.valueOrEmpty(javaType).trim();
        if (StringUtils.isEmpty(type)) {
            return null;
        }
        if (type.startsWith("__exception__:")) {
            type = type.substring("__exception__:".length()).trim();
        }
        final int genericIndex = type.indexOf('<');
        if (genericIndex > 0) {
            type = type.substring(0, genericIndex).trim();
        }
        final Class<?> directClass = tryLoadClassByName(type);
        if (directClass != null) {
            return directClass;
        }
        final DocsV2Definition definition = buildInteractiveDocsDefinition(handlers, provider);
        if (definition.getTypes() != null) {
            for (final TypeDoc typeDoc : definition.getTypes()) {
                if (typeDoc == null) {
                    continue;
                }
                if (StringUtils.equals(type, typeDoc.getJavaType()) || StringUtils.equals(type, typeDoc.getName()) || StringUtils.equals(type, simpleTypeName(typeDoc.getJavaType()))) {
                    final Class<?> loaded = tryLoadClassByName(typeDoc.getJavaType());
                    if (loaded != null) {
                        return loaded;
                    }
                }
            }
        }
        if (definition.getEndpoints() != null) {
            for (final EndpointDoc endpoint : definition.getEndpoints()) {
                if (endpoint == null || endpoint.getExceptions() == null) {
                    continue;
                }
                for (final ExceptionDoc exceptionDoc : endpoint.getExceptions()) {
                    if (exceptionDoc == null) {
                        continue;
                    }
                    if (StringUtils.equals(type, exceptionDoc.getType()) || StringUtils.equals(type, exceptionDoc.getSimpleName())) {
                        final Class<?> loaded = tryLoadClassByName(exceptionDoc.getType());
                        if (loaded != null) {
                            return loaded;
                        }
                    }
                }
            }
        }
        return null;
    }

    private Class<?> tryLoadClassByName(final String javaType) {
        final String raw = StringUtils.valueOrEmpty(javaType).trim();
        if (StringUtils.isEmpty(raw)) {
            return null;
        }
        try {
            return Class.forName(raw);
        } catch (final Throwable ignore) {
        }
        return null;
    }

    private String buildExceptionEnvelopeExampleJson(final Class<?> exceptionClass, final boolean includeDocumentation) throws Exception {
        final ExampleExceptionEnvelope envelope = new ExampleExceptionEnvelope();
        envelope.setSrc("DEVICE");
        envelope.setType(exceptionClass == null ? "Exception" : exceptionClass.getSimpleName());
        if (exceptionClass != null && BasicRemoteAPIException.class.isAssignableFrom(exceptionClass)) {
            final BasicRemoteAPIException sample = instantiateRemoteAPIException(exceptionClass.asSubclass(BasicRemoteAPIException.class));
            if (sample != null) {
                envelope.setType(StringUtils.valueOrEmpty(sample.getType()));
                envelope.setData(sample.getData());
            }
        }
        if (includeDocumentation) {
            return FlexiUtils.serializeConfigStorable(envelope);
        }
        return FlexiUtils.serializeToPrettyJson(envelope);
    }

    private Object createExampleInstance(final Class<?> cls) {
        if (cls == null) {
            return null;
        }
        if (cls.isPrimitive() || String.class == cls || Number.class.isAssignableFrom(cls) || cls == Boolean.class || cls == Character.class) {
            return createSimpleDefaultValue(cls);
        }
        if (cls.isEnum()) {
            final Object[] values = cls.getEnumConstants();
            return values != null && values.length > 0 ? values[0] : null;
        }
        if (cls.isArray()) {
            return Array.newInstance(cls.getComponentType(), 0);
        }
        if (Map.class.isAssignableFrom(cls)) {
            return new LinkedHashMap<String, Object>();
        }
        if (Collection.class.isAssignableFrom(cls)) {
            return new ArrayList<Object>();
        }
        if (Modifier.isAbstract(cls.getModifiers()) || cls.isInterface()) {
            return null;
        }
        try {
            final Constructor<?> constructor = cls.getDeclaredConstructor();
            constructor.setAccessible(true);
            return constructor.newInstance();
        } catch (final Throwable ignore) {
        }
        return null;
    }

    private Object createSimpleDefaultValue(final Class<?> cls) {
        if (cls == null) {
            return null;
        }
        if (cls == String.class) {
            return "";
        }
        if (cls == boolean.class || cls == Boolean.class) {
            return Boolean.FALSE;
        }
        if (cls == char.class || cls == Character.class) {
            return Character.valueOf('\0');
        }
        if (cls == byte.class || cls == Byte.class) {
            return Byte.valueOf((byte) 0);
        }
        if (cls == short.class || cls == Short.class) {
            return Short.valueOf((short) 0);
        }
        if (cls == int.class || cls == Integer.class) {
            return Integer.valueOf(0);
        }
        if (cls == long.class || cls == Long.class) {
            return Long.valueOf(0L);
        }
        if (cls == float.class || cls == Float.class) {
            return Float.valueOf(0f);
        }
        if (cls == double.class || cls == Double.class) {
            return Double.valueOf(0d);
        }
        return null;
    }

    private String simpleTypeName(final String javaType) {
        final String raw = StringUtils.valueOrEmpty(javaType).trim();
        if (StringUtils.isEmpty(raw)) {
            return raw;
        }
        final int lastDot = raw.lastIndexOf('.');
        if (lastDot >= 0 && lastDot + 1 < raw.length()) {
            return raw.substring(lastDot + 1);
        }
        return raw;
    }

    private static class DocInfo {
        private String       text;
        private List<String> wikiLinks;
    }

    private static class LifecycleInfo {
        private String availableSince;
        private String availableSinceMessage;
        private String deprecatedSince;
        private String deprecatedSinceMessage;
    }

    public static class ExampleExceptionEnvelope {
        private String src;
        private String type;
        private Object data;

        public String getSrc() {
            return src;
        }

        public void setSrc(final String src) {
            this.src = src;
        }

        public String getType() {
            return type;
        }

        public void setType(final String type) {
            this.type = type;
        }

        public Object getData() {
            return data;
        }

        public void setData(final Object data) {
            this.data = data;
        }
    }
}
