package org.appwork.remoteapi.docsv2.model;

import java.util.List;

import org.appwork.remoteapi.docsv2.model.DocsV2Definition.TypeDoc;

public class DocsV2StorableDefinition {
    private long          generatedAt;
    private String        javaType;
    private List<TypeDoc> types;

    public long getGeneratedAt() {
        return generatedAt;
    }

    public void setGeneratedAt(final long generatedAt) {
        this.generatedAt = generatedAt;
    }

    public String getJavaType() {
        return javaType;
    }

    public void setJavaType(final String javaType) {
        this.javaType = javaType;
    }

    public List<TypeDoc> getTypes() {
        return types;
    }

    public void setTypes(final List<TypeDoc> types) {
        this.types = types;
    }
}
