package org.appwork.remoteapi.docsv2;

/**
 * Supplies dynamic JSON payloads used by the DocsV2 endpoints.
 */
public interface DocsV2ContentProvider {
    String createDocsDefinitionJson() throws Exception;

    String createStorableDefinitionJson(String javaType) throws Exception;

    String createExampleJson(String javaType, boolean includeDocumentation) throws Exception;
}
