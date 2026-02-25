package org.appwork.remoteapi.docsv2;

import org.appwork.remoteapi.RemoteAPIInterface;
import org.appwork.remoteapi.RemoteAPIRequest;
import org.appwork.remoteapi.RemoteAPIResponse;
import org.appwork.remoteapi.annotations.APITag;
import org.appwork.remoteapi.annotations.APIParameterNames;
import org.appwork.remoteapi.annotations.ApiDoc;
import org.appwork.remoteapi.annotations.ApiNamespace;
import org.appwork.remoteapi.annotations.HiddenForHelpDocs;
import org.appwork.remoteapi.annotations.tags.ExperimentalAPITag;
import org.appwork.remoteapi.docsv2.model.DocsV2Definition;
import org.appwork.remoteapi.docsv2.model.DocsV2StorableDefinition;
import org.appwork.remoteapi.exceptions.InternalApiException;

@ApiNamespace("docsv2")
public interface DocsV2APIInterface extends RemoteAPIInterface {
    @APIParameterNames({ "request", "response" })
    @ApiDoc("Returns the interactive DocsV2 HTML page.")
    void docs(RemoteAPIRequest request, RemoteAPIResponse response) throws InternalApiException;

    @APIParameterNames({ "request", "response" })
    @HiddenForHelpDocs
    @ApiDoc("Returns the JavaScript bundle for DocsV2.")
    void docsJs(RemoteAPIRequest request, RemoteAPIResponse response) throws InternalApiException;

    @APIParameterNames({ "request" })
    @HiddenForHelpDocs
    @ApiDoc("Returns API endpoint/type metadata used by DocsV2.")
    DocsV2Definition docsDefinition(RemoteAPIRequest request) throws InternalApiException;

    @APIParameterNames({ "request", "javaType" })
    @HiddenForHelpDocs
    @APITag(ExperimentalAPITag.class)
    @ApiDoc("Returns storable/type metadata for one Java type.")
    DocsV2StorableDefinition getStorableDefinition(RemoteAPIRequest request, String javaType) throws InternalApiException;

    @APIParameterNames({ "request", "javaType", "includeDocumentation" })
    @HiddenForHelpDocs
    @APITag(ExperimentalAPITag.class)
    @ApiDoc("Creates an example JSON payload for one Java type. If includeDocumentation is true, inline documentation comments are included.")
    String createExampleJson(RemoteAPIRequest request, String javaType, boolean includeDocumentation) throws InternalApiException;
}
