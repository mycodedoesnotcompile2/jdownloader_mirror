package org.appwork.remoteapi.docsv2;

import java.util.Map;

import org.appwork.remoteapi.InterfaceHandler;
import org.appwork.remoteapi.RemoteAPIInterface;

public class DocsV2DefinitionContentProvider implements DocsV2ContentProvider {
    private final Map<String, InterfaceHandler<RemoteAPIInterface>> handlers;
    private final DocsV2ProjectDataProvider                         projectDataProvider;
    private final DocsV2DefinitionBuilder                           definitionBuilder;

    public DocsV2DefinitionContentProvider(final Map<String, InterfaceHandler<RemoteAPIInterface>> handlers, final DocsV2ProjectDataProvider projectDataProvider) {
        this(handlers, projectDataProvider, new DocsV2DefinitionBuilder());
    }

    public DocsV2DefinitionContentProvider(final Map<String, InterfaceHandler<RemoteAPIInterface>> handlers, final DocsV2ProjectDataProvider projectDataProvider, final DocsV2DefinitionBuilder definitionBuilder) {
        this.handlers = handlers;
        this.projectDataProvider = projectDataProvider;
        this.definitionBuilder = definitionBuilder == null ? new DocsV2DefinitionBuilder() : definitionBuilder;
    }

    @Override
    public String createDocsDefinitionJson() throws Exception {
        return definitionBuilder.createInteractiveDocsDefinitionJson(handlers, projectDataProvider);
    }

    @Override
    public String createStorableDefinitionJson(final String javaType) throws Exception {
        return definitionBuilder.createStorableDefinitionJson(javaType, handlers, projectDataProvider);
    }

    @Override
    public String createExampleJson(final String javaType, final boolean includeDocumentation) throws Exception {
        return definitionBuilder.createExampleJson(javaType, includeDocumentation, handlers, projectDataProvider);
    }
}
