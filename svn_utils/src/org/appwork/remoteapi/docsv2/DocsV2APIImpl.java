package org.appwork.remoteapi.docsv2;

import java.io.IOException;
import java.io.InputStream;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.remoteapi.ContentSecurityHeader;
import org.appwork.remoteapi.RemoteAPIRequest;
import org.appwork.remoteapi.RemoteAPIResponse;
import org.appwork.remoteapi.exceptions.InternalApiException;
import org.appwork.utils.IO;
import org.appwork.utils.StringUtils;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.httpserver.requests.KeyValuePair;

public class DocsV2APIImpl implements DocsV2APIInterface {
    private static final String DEFAULT_HTML_RESOURCE = "/org/appwork/remoteapi/docsv2/docs.html";
    private static final String DEFAULT_JS_RESOURCE   = "/org/appwork/remoteapi/docsv2/docs.js";

    private final DocsV2ContentProvider provider;
    private final String                title;
    private final String                htmlResourcePath;
    private final String                jsResourcePath;

    public DocsV2APIImpl(final DocsV2ContentProvider provider) {
        this(provider, "Remote API Docs");
    }

    public DocsV2APIImpl(final DocsV2ContentProvider provider, final String title) {
        this(provider, title, DEFAULT_HTML_RESOURCE, DEFAULT_JS_RESOURCE);
    }

    public DocsV2APIImpl(final DocsV2ContentProvider provider, final String title, final String htmlResourcePath, final String jsResourcePath) {
        this.provider = provider;
        this.title = StringUtils.isEmpty(title) ? "Remote API Docs" : title;
        this.htmlResourcePath = htmlResourcePath;
        this.jsResourcePath = jsResourcePath;
    }

    @Override
    public void docs(final RemoteAPIRequest request, final RemoteAPIResponse response) throws InternalApiException {
        try {
            String html = new String(loadResourceBytes(this.htmlResourcePath), StandardCharsets.UTF_8);
            html = html.replace("__TOKEN_QUERY__", buildTokenQuerySuffix(request));
            html = html.replace("__DOCS_TITLE__", this.title);
            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "text/html; charset=utf-8"));
            final ContentSecurityHeader csp = new ContentSecurityHeader();
            csp.addDefaultSrc("'self'");
            csp.addScriptSrc("'self'");
            csp.addStyleSrc("'unsafe-inline'");
            csp.addConnectSrc("'self'");
            csp.addImgSrc("data:");
            csp.addImgSrc("blob:");
            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_SECURITY_POLICY, csp.toHeaderString()));
            response.setResponseCode(ResponseCode.SUCCESS_OK);
            response.sendBytes(request, html.getBytes(StandardCharsets.UTF_8));
        } catch (final IOException e) {
            throw new InternalApiException(e);
        }
    }

    @Override
    public void docsJs(final RemoteAPIRequest request, final RemoteAPIResponse response) throws InternalApiException {
        try {
            final byte[] js = loadResourceBytes(this.jsResourcePath);
            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "application/javascript; charset=utf-8"));
            response.setResponseCode(ResponseCode.SUCCESS_OK);
            response.sendBytes(request, js);
        } catch (final IOException e) {
            throw new InternalApiException(e);
        }
    }

    @Override
    public void docsDefinition(final RemoteAPIRequest request, final RemoteAPIResponse response) throws InternalApiException {
        if (this.provider == null) {
            throw new InternalApiException("No DocsV2ContentProvider configured");
        }
        try {
            final byte[] json = this.provider.createDocsDefinitionJson().getBytes(StandardCharsets.UTF_8);
            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "application/json; charset=utf-8"));
            response.setResponseCode(ResponseCode.SUCCESS_OK);
            response.sendBytes(request, json);
        } catch (final IOException e) {
            throw new InternalApiException(e);
        } catch (final Exception e) {
            throw new InternalApiException(e);
        }
    }

    @Override
    public void getStorableDefinition(final RemoteAPIRequest request, final RemoteAPIResponse response, final String javaType) throws InternalApiException {
        if (this.provider == null) {
            throw new InternalApiException("No DocsV2ContentProvider configured");
        }
        try {
            final byte[] json = this.provider.createStorableDefinitionJson(javaType).getBytes(StandardCharsets.UTF_8);
            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "application/json; charset=utf-8"));
            response.setResponseCode(ResponseCode.SUCCESS_OK);
            response.sendBytes(request, json);
        } catch (final IOException e) {
            throw new InternalApiException(e);
        } catch (final Exception e) {
            throw new InternalApiException(e);
        }
    }

    private byte[] loadResourceBytes(final String resourcePath) throws IOException {
        final InputStream is = DocsV2APIImpl.class.getResourceAsStream(resourcePath);
        if (is == null) {
            throw new IOException("Resource not found: " + resourcePath);
        }
        try {
            return IO.readStream(-1, is);
        } finally {
            try {
                is.close();
            } catch (final IOException ignore) {
            }
        }
    }

    private String buildTokenQuerySuffix(final RemoteAPIRequest request) {
        final String token = extractToken(request);
        if (StringUtils.isEmpty(token)) {
            return "";
        }
        try {
            return "?token=" + URLEncoder.encode(token, "UTF-8");
        } catch (final Throwable e) {
            return "";
        }
    }

    private String extractToken(final RemoteAPIRequest request) {
        try {
            if (request != null) {
                final String queryToken = request.getParameterbyKey("token");
                if (StringUtils.isNotEmpty(queryToken)) {
                    return queryToken;
                }
                final java.util.List<KeyValuePair> requestedURLParameters = request.getRequestedURLParameters();
                if (requestedURLParameters != null) {
                    for (final KeyValuePair kv : requestedURLParameters) {
                        if (kv != null && "token".equalsIgnoreCase(kv.key) && StringUtils.isNotEmpty(kv.value)) {
                            return kv.value;
                        }
                    }
                }
                final String requestedURL = request.getRequestedURL();
                if (StringUtils.isNotEmpty(requestedURL)) {
                    final int qIndex = requestedURL.indexOf('?');
                    if (qIndex >= 0 && qIndex + 1 < requestedURL.length()) {
                        final String[] pairs = requestedURL.substring(qIndex + 1).split("&");
                        for (final String pair : pairs) {
                            if (StringUtils.isEmpty(pair)) {
                                continue;
                            }
                            final int eq = pair.indexOf('=');
                            final String key = eq >= 0 ? pair.substring(0, eq) : pair;
                            final String value = eq >= 0 && eq + 1 < pair.length() ? pair.substring(eq + 1) : "";
                            if ("token".equalsIgnoreCase(key) && StringUtils.isNotEmpty(value)) {
                                return value;
                            }
                        }
                    }
                }
                final HTTPHeader xToken = request.getRequestHeaders().get("X-TOKEN");
                if (xToken != null && StringUtils.isNotEmpty(xToken.getValue())) {
                    return xToken.getValue();
                }
                final HTTPHeader xTokenLower = request.getRequestHeaders().get("x-token");
                if (xTokenLower != null && StringUtils.isNotEmpty(xTokenLower.getValue())) {
                    return xTokenLower.getValue();
                }
            }
        } catch (final Throwable ignore) {
        }
        return null;
    }
}
