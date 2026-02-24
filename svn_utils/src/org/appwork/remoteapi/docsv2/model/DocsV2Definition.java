package org.appwork.remoteapi.docsv2.model;

import java.util.List;

public class DocsV2Definition {
    private long              generatedAt;
    private List<EndpointDoc> endpoints;
    private List<TypeDoc>     types;

    public long getGeneratedAt() {
        return generatedAt;
    }

    public void setGeneratedAt(final long generatedAt) {
        this.generatedAt = generatedAt;
    }

    public List<EndpointDoc> getEndpoints() {
        return endpoints;
    }

    public void setEndpoints(final List<EndpointDoc> endpoints) {
        this.endpoints = endpoints;
    }

    public List<TypeDoc> getTypes() {
        return types;
    }

    public void setTypes(final List<TypeDoc> types) {
        this.types = types;
    }

    public static class EndpointDoc {
        private String             namespace;
        private String             methodName;
        private String             path;
        private String             description;
        private String             availableSince;
        private String             availableSinceMessage;
        private String             deprecatedSince;
        private String             deprecatedSinceMessage;
        private List<String>       wikiLinks;
        private String             returnType;
        private String             returnJavaType;
        private List<ReturnTypeDoc> returnTypes;
        private List<ParameterDoc> parameters;
        private List<ExceptionDoc> exceptions;
        private List<AuthDoc>      auth;
        private List<TagDoc>       tags;

        public String getNamespace() {
            return namespace;
        }

        public void setNamespace(final String namespace) {
            this.namespace = namespace;
        }

        public String getMethodName() {
            return methodName;
        }

        public void setMethodName(final String methodName) {
            this.methodName = methodName;
        }

        public String getPath() {
            return path;
        }

        public void setPath(final String path) {
            this.path = path;
        }

        public String getDescription() {
            return description;
        }

        public void setDescription(final String description) {
            this.description = description;
        }

        public String getAvailableSince() {
            return availableSince;
        }

        public void setAvailableSince(final String availableSince) {
            this.availableSince = availableSince;
        }

        public String getAvailableSinceMessage() {
            return availableSinceMessage;
        }

        public void setAvailableSinceMessage(final String availableSinceMessage) {
            this.availableSinceMessage = availableSinceMessage;
        }

        public String getDeprecatedSince() {
            return deprecatedSince;
        }

        public void setDeprecatedSince(final String deprecatedSince) {
            this.deprecatedSince = deprecatedSince;
        }

        public String getDeprecatedSinceMessage() {
            return deprecatedSinceMessage;
        }

        public void setDeprecatedSinceMessage(final String deprecatedSinceMessage) {
            this.deprecatedSinceMessage = deprecatedSinceMessage;
        }

        public List<String> getWikiLinks() {
            return wikiLinks;
        }

        public void setWikiLinks(final List<String> wikiLinks) {
            this.wikiLinks = wikiLinks;
        }

        public String getReturnType() {
            return returnType;
        }

        public void setReturnType(final String returnType) {
            this.returnType = returnType;
        }

        public String getReturnJavaType() {
            return returnJavaType;
        }

        public void setReturnJavaType(final String returnJavaType) {
            this.returnJavaType = returnJavaType;
        }

        public List<ReturnTypeDoc> getReturnTypes() {
            return returnTypes;
        }

        public void setReturnTypes(final List<ReturnTypeDoc> returnTypes) {
            this.returnTypes = returnTypes;
        }

        public List<ParameterDoc> getParameters() {
            return parameters;
        }

        public void setParameters(final List<ParameterDoc> parameters) {
            this.parameters = parameters;
        }

        public List<ExceptionDoc> getExceptions() {
            return exceptions;
        }

        public void setExceptions(final List<ExceptionDoc> exceptions) {
            this.exceptions = exceptions;
        }

        public List<AuthDoc> getAuth() {
            return auth;
        }

        public void setAuth(final List<AuthDoc> auth) {
            this.auth = auth;
        }

        public List<TagDoc> getTags() {
            return tags;
        }

        public void setTags(final List<TagDoc> tags) {
            this.tags = tags;
        }
    }

    public static class ReturnTypeDoc {
        private String type;
        private String javaType;

        public String getType() {
            return type;
        }

        public void setType(final String type) {
            this.type = type;
        }

        public String getJavaType() {
            return javaType;
        }

        public void setJavaType(final String javaType) {
            this.javaType = javaType;
        }
    }

    public static class ParameterDoc {
        private String       name;
        private String       type;
        private String       javaType;
        private String       editor;
        private boolean      multi;
        private String       optionsEndpoint;
        private List<String> options;

        public String getName() {
            return name;
        }

        public void setName(final String name) {
            this.name = name;
        }

        public String getType() {
            return type;
        }

        public void setType(final String type) {
            this.type = type;
        }

        public String getJavaType() {
            return javaType;
        }

        public void setJavaType(final String javaType) {
            this.javaType = javaType;
        }

        public String getEditor() {
            return editor;
        }

        public void setEditor(final String editor) {
            this.editor = editor;
        }

        public boolean isMulti() {
            return multi;
        }

        public void setMulti(final boolean multi) {
            this.multi = multi;
        }

        public String getOptionsEndpoint() {
            return optionsEndpoint;
        }

        public void setOptionsEndpoint(final String optionsEndpoint) {
            this.optionsEndpoint = optionsEndpoint;
        }

        public List<String> getOptions() {
            return options;
        }

        public void setOptions(final List<String> options) {
            this.options = options;
        }
    }

    public static class ExceptionDoc {
        private String       type;
        private String       simpleName;
        private String       http;
        private String       description;
        private List<String> wikiLinks;
        private String       dataType;
        private String       dataJavaType;

        public String getType() {
            return type;
        }

        public void setType(final String type) {
            this.type = type;
        }

        public String getSimpleName() {
            return simpleName;
        }

        public void setSimpleName(final String simpleName) {
            this.simpleName = simpleName;
        }

        public String getHttp() {
            return http;
        }

        public void setHttp(final String http) {
            this.http = http;
        }

        public String getDescription() {
            return description;
        }

        public void setDescription(final String description) {
            this.description = description;
        }

        public List<String> getWikiLinks() {
            return wikiLinks;
        }

        public void setWikiLinks(final List<String> wikiLinks) {
            this.wikiLinks = wikiLinks;
        }

        public String getDataType() {
            return dataType;
        }

        public void setDataType(final String dataType) {
            this.dataType = dataType;
        }

        public String getDataJavaType() {
            return dataJavaType;
        }

        public void setDataJavaType(final String dataJavaType) {
            this.dataJavaType = dataJavaType;
        }
    }

    public static class AuthDoc {
        private String name;
        private String doc;

        public String getName() {
            return name;
        }

        public void setName(final String name) {
            this.name = name;
        }

        public String getDoc() {
            return doc;
        }

        public void setDoc(final String doc) {
            this.doc = doc;
        }
    }

    public static class TagDoc {
        private String name;
        private String description;
        private String icon;

        public String getName() {
            return name;
        }

        public void setName(final String name) {
            this.name = name;
        }

        public String getDescription() {
            return description;
        }

        public void setDescription(final String description) {
            this.description = description;
        }

        public String getIcon() {
            return icon;
        }

        public void setIcon(final String icon) {
            this.icon = icon;
        }
    }

    public static class TypeDoc {
        private String         javaType;
        private String         name;
        private String         kind;
        private String         description;
        private String         availableSince;
        private String         availableSinceMessage;
        private String         deprecatedSince;
        private String         deprecatedSinceMessage;
        private List<String>   wikiLinks;
        private String         example;
        private List<FieldDoc> fields;
        private List<String>   enumValues;
        private List<EnumValueDoc> enumValueDocs;

        public String getJavaType() {
            return javaType;
        }

        public void setJavaType(final String javaType) {
            this.javaType = javaType;
        }

        public String getName() {
            return name;
        }

        public void setName(final String name) {
            this.name = name;
        }

        public String getKind() {
            return kind;
        }

        public void setKind(final String kind) {
            this.kind = kind;
        }

        public String getDescription() {
            return description;
        }

        public void setDescription(final String description) {
            this.description = description;
        }

        public String getAvailableSince() {
            return availableSince;
        }

        public void setAvailableSince(final String availableSince) {
            this.availableSince = availableSince;
        }

        public String getAvailableSinceMessage() {
            return availableSinceMessage;
        }

        public void setAvailableSinceMessage(final String availableSinceMessage) {
            this.availableSinceMessage = availableSinceMessage;
        }

        public String getDeprecatedSince() {
            return deprecatedSince;
        }

        public void setDeprecatedSince(final String deprecatedSince) {
            this.deprecatedSince = deprecatedSince;
        }

        public String getDeprecatedSinceMessage() {
            return deprecatedSinceMessage;
        }

        public void setDeprecatedSinceMessage(final String deprecatedSinceMessage) {
            this.deprecatedSinceMessage = deprecatedSinceMessage;
        }

        public List<String> getWikiLinks() {
            return wikiLinks;
        }

        public void setWikiLinks(final List<String> wikiLinks) {
            this.wikiLinks = wikiLinks;
        }

        public String getExample() {
            return example;
        }

        public void setExample(final String example) {
            this.example = example;
        }

        public List<FieldDoc> getFields() {
            return fields;
        }

        public void setFields(final List<FieldDoc> fields) {
            this.fields = fields;
        }

        public List<String> getEnumValues() {
            return enumValues;
        }

        public void setEnumValues(final List<String> enumValues) {
            this.enumValues = enumValues;
        }

        public List<EnumValueDoc> getEnumValueDocs() {
            return enumValueDocs;
        }

        public void setEnumValueDocs(final List<EnumValueDoc> enumValueDocs) {
            this.enumValueDocs = enumValueDocs;
        }
    }

    public static class EnumValueDoc {
        private String       name;
        private String       description;
        private List<String> wikiLinks;
        private String       example;

        public String getName() {
            return name;
        }

        public void setName(final String name) {
            this.name = name;
        }

        public String getDescription() {
            return description;
        }

        public void setDescription(final String description) {
            this.description = description;
        }

        public List<String> getWikiLinks() {
            return wikiLinks;
        }

        public void setWikiLinks(final List<String> wikiLinks) {
            this.wikiLinks = wikiLinks;
        }

        public String getExample() {
            return example;
        }

        public void setExample(final String example) {
            this.example = example;
        }
    }

    public static class FieldDoc {
        private String       name;
        private String       type;
        private String       javaType;
        private String       description;
        private String       availableSince;
        private String       availableSinceMessage;
        private String       deprecatedSince;
        private String       deprecatedSinceMessage;
        private List<String> wikiLinks;
        private String       example;

        public String getName() {
            return name;
        }

        public void setName(final String name) {
            this.name = name;
        }

        public String getType() {
            return type;
        }

        public void setType(final String type) {
            this.type = type;
        }

        public String getJavaType() {
            return javaType;
        }

        public void setJavaType(final String javaType) {
            this.javaType = javaType;
        }

        public String getDescription() {
            return description;
        }

        public void setDescription(final String description) {
            this.description = description;
        }

        public String getAvailableSince() {
            return availableSince;
        }

        public void setAvailableSince(final String availableSince) {
            this.availableSince = availableSince;
        }

        public String getAvailableSinceMessage() {
            return availableSinceMessage;
        }

        public void setAvailableSinceMessage(final String availableSinceMessage) {
            this.availableSinceMessage = availableSinceMessage;
        }

        public String getDeprecatedSince() {
            return deprecatedSince;
        }

        public void setDeprecatedSince(final String deprecatedSince) {
            this.deprecatedSince = deprecatedSince;
        }

        public String getDeprecatedSinceMessage() {
            return deprecatedSinceMessage;
        }

        public void setDeprecatedSinceMessage(final String deprecatedSinceMessage) {
            this.deprecatedSinceMessage = deprecatedSinceMessage;
        }

        public List<String> getWikiLinks() {
            return wikiLinks;
        }

        public void setWikiLinks(final List<String> wikiLinks) {
            this.wikiLinks = wikiLinks;
        }

        public String getExample() {
            return example;
        }

        public void setExample(final String example) {
            this.example = example;
        }
    }
}
