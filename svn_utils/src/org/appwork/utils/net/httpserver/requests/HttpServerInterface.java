/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
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
package org.appwork.utils.net.httpserver.requests;

import java.util.List;
import java.util.Set;

import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpserver.ConnectionTimeouts;
import org.appwork.utils.net.httpserver.CorsHandler;
import org.appwork.utils.net.httpserver.HeaderValidationRules;
import org.appwork.utils.net.httpserver.RequestSizeLimits;
import org.appwork.utils.net.httpserver.ResponseSecurityHeaders;
import org.appwork.utils.net.httpserver.handler.HttpRequestHandler;
import org.appwork.utils.net.httpserver.responses.HttpResponse;

/**
 * @author Thomas
 * @date 28.03.2017
 *
 */
public interface HttpServerInterface {

    // /**
    // * @return
    // */
    // public int getMaxHeaderBytes();
    //
    // /**
    // * @return
    // */
    // public long getMaxPostBodyBytes();
    //
    // /**
    // * @return
    // */
    // public long getMaxPostProcessingBytes();

    /**
     * @param httpRequest
     * @param httpResponse
     * @return
     */
    public boolean isChunkedEncodedResponseAllowed(HttpRequest httpRequest, HttpResponse httpResponse);

    List<HttpRequestHandler> getHandler();

    /**
     * Returns the response security headers configuration.
     *
     * @return the response security headers configuration or null if security headers are disabled
     */
    ResponseSecurityHeaders getResponseSecurityHeaders();

    /**
     * Returns the set of allowed HTTP methods.
     *
     * @return the set of allowed methods or null if method validation is disabled
     */
    Set<RequestMethod> getAllowedMethods();

    /**
     * Returns the header validation rules configuration.
     *
     * @return the header validation rules or null if header validation is disabled
     */
    HeaderValidationRules getHeaderValidationRules();

    /**
     * Returns the request size limits configuration.
     *
     * @return the request size limits or null if size limits are disabled
     */
    RequestSizeLimits getRequestSizeLimits();

    /**
     * Returns the connection timeouts configuration.
     *
     * @return the connection timeouts configuration or null if timeouts are disabled
     */
    ConnectionTimeouts getConnectionTimeouts();

    /**
     * Returns the CORS configuration.
     *
     * @return the CORS configuration or null if CORS is disabled
     */
    CorsHandler getCorsHandler();

    /**
     * Returns the response Server header value.
     *
     * @return the Server header value, or null to disable the Server header
     */
    String getResponseServerHeader();

}
