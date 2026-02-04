/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.net.httpserver.tests;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.remoteapi.exceptions.BasicRemoteAPIException;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.httpserver.handler.HttpRequestHandler;
import org.appwork.utils.net.httpserver.requests.AbstractGetRequest;
import org.appwork.utils.net.httpserver.requests.AbstractPostRequest;
import org.appwork.utils.net.httpserver.requests.OptionsRequest;
import org.appwork.utils.net.httpserver.responses.HttpResponse;

/**
 * Dummy request handler for CORS test paths.
 * 
 * <p>
 * This handler provides simple 200 OK responses for paths used in CORS tests, allowing the tests to verify CORS functionality without needing actual API implementations.
 * </p>
 *
 * @author AppWork
 */
public class DummyCorsTestPathHandler implements HttpRequestHandler {
    
    /**
     * Set of paths that this handler will respond to with 200 OK.
     */
    private static final Set<String> SUPPORTED_PATHS = new HashSet<String>();
    
    static {
        // Paths used in path-aware CORS tests
        SUPPORTED_PATHS.add("/");
        SUPPORTED_PATHS.add("/connect/probe");
        SUPPORTED_PATHS.add("/api/v1");
        SUPPORTED_PATHS.add("/api/v2");
        SUPPORTED_PATHS.add("/api/v99");
        SUPPORTED_PATHS.add("/api/public");
        SUPPORTED_PATHS.add("/test");
        SUPPORTED_PATHS.add("/api");
        // Paths with wildcard patterns - we'll check if path starts with these
        // /api1/* paths
        // /connect/.* paths
    }
    
    /**
     * Checks if the given path is supported by this handler.
     * 
     * @param path The request path
     * @return true if the path is supported, false otherwise
     */
    private boolean isSupportedPath(final String path) {
        if (path == null) {
            return false;
        }
        // Check exact matches
        if (SUPPORTED_PATHS.contains(path)) {
            return true;
        }
        // Check if path starts with /api1/ (for wildcard pattern tests)
        if (path.startsWith("/api1/")) {
            return true;
        }
        // Check if path starts with /connect/ (for wildcard pattern tests)
        if (path.startsWith("/connect/")) {
            return true;
        }
        return false;
    }
    
    @Override
    public boolean onGetRequest(final AbstractGetRequest request, final HttpResponse response) throws BasicRemoteAPIException {
        final String path = request.getRequestedPath();
        if (this.isSupportedPath(path)) {
            try {
                // For OPTIONS requests, CORS handler will add headers, we just need to return 200/204
                if (request instanceof OptionsRequest) {
                    // OPTIONS requests are typically handled by CorsHandler, but if it reaches here,
                    // we'll return 204 No Content (standard for OPTIONS)
                    response.setResponseCode(HTTPConstants.ResponseCode.SUCCESS_NO_CONTENT);
                    return true;
                }
                // For GET requests, return 200 OK with body
                response.setResponseCode(HTTPConstants.ResponseCode.SUCCESS_OK);
                response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "text/plain; charset=UTF-8"));
                final String responseBody = "OK: " + path;
                final byte[] bodyBytes = responseBody.getBytes("UTF-8");
                response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, String.valueOf(bodyBytes.length)));
                response.getOutputStream(true).write(bodyBytes);
                response.getOutputStream(true).flush();
                return true;
            } catch (final IOException e) {
                throw new BasicRemoteAPIException(e);
            }
        }
        return false; // Let other handlers process this request
    }
    
    @Override
    public boolean onPostRequest(final AbstractPostRequest request, final HttpResponse response) throws BasicRemoteAPIException {
        final String path = request.getRequestedPath();
        if (this.isSupportedPath(path)) {
            try {
                response.setResponseCode(HTTPConstants.ResponseCode.SUCCESS_OK);
                response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "text/plain; charset=UTF-8"));
                final String responseBody = "OK: " + path;
                final byte[] bodyBytes = responseBody.getBytes("UTF-8");
                response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, String.valueOf(bodyBytes.length)));
                response.getOutputStream(true).write(bodyBytes);
                response.getOutputStream(true).flush();
                return true;
            } catch (final IOException e) {
                throw new BasicRemoteAPIException(e);
            }
        }
        return false; // Let other handlers process this request
    }
}
