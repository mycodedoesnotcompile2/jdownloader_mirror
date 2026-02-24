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

    @APIParameterNames({ "request", "response" })
    @HiddenForHelpDocs
    @ApiDoc("Returns API endpoint/type metadata used by DocsV2.")
    void docsDefinition(RemoteAPIRequest request, RemoteAPIResponse response) throws InternalApiException;

    @APIParameterNames({ "request", "response", "javaType" })
    @HiddenForHelpDocs
    @APITag(ExperimentalAPITag.class)
    @ApiDoc("Returns storable/type metadata for one Java type.")
    void getStorableDefinition(RemoteAPIRequest request, RemoteAPIResponse response, String javaType) throws InternalApiException;
}
