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
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.remoteapi.exceptions;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.zip.GZIPOutputStream;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.storage.JSonStorage;
import org.appwork.utils.Exceptions;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.httpserver.HttpConnectionExceptionHandler;
import org.appwork.utils.net.httpserver.requests.HttpRequestInterface;
import org.appwork.utils.net.httpserver.responses.HttpResponse;
import org.appwork.utils.net.httpserver.responses.HttpResponseInterface;

/**
 * @author Thomas
 *
 */
public class BasicRemoteAPIException extends Exception implements HttpConnectionExceptionHandler {
    /**
     *
     */
    private static final long     serialVersionUID = 1L;
    private HttpRequestInterface  request;
    private HttpResponseInterface response;
    private final String          type;
    private final ResponseCode    code;
    private final Object          data;

    /**
     * @param e
     */
    public BasicRemoteAPIException(final IOException e) {
        this(e, "UNKNOWN", ResponseCode.SERVERERROR_INTERNAL, null);
    }

    public BasicRemoteAPIException(final ResponseCode code2) {
        this(null, code2.name(), code2, null);
    }

    /**
     * @param name
     * @param code2
     */
    public BasicRemoteAPIException(final String name, final ResponseCode code2) {
        this(null, name, code2, null);
    }

    /**
     * @param cause
     * @param name
     * @param code
     * @param data
     */
    public BasicRemoteAPIException(final Throwable cause, final String name, final ResponseCode code, final Object data) {
        super(name + "(" + code + ")", cause);
        this.data = data;
        this.type = name;
        this.code = code;
    }

    public ResponseCode getCode() {
        return this.code;
    }

    public Object getData() {
        return this.data;
    }

    public HttpRequestInterface getRequest() {
        return this.request;
    }

    public HttpResponseInterface getResponse() {
        return this.response;
    }

    public String getType() {
        return this.type;
    }

    /**
     * @param response
     * @throws IOException
     */
    public boolean handle(final HttpResponse response) throws IOException {
        return handle(response, false);
    }

    protected Object getErrorObject() {
        return new DeviceErrorResponse(this.getType(), getData());
    }

    public boolean handle(final HttpResponse response, boolean gzip) throws IOException {
        byte[] bytes;
        final String str = JSonStorage.serializeToJson(getErrorObject());
        bytes = str.getBytes("UTF-8");
        if (gzip) {
            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_ENCODING, "gzip"));
            ByteArrayOutputStream baos;
            GZIPOutputStream gzipout = new GZIPOutputStream(baos = new ByteArrayOutputStream());
            gzipout.write(bytes);
            gzipout.close();
            bytes = baos.toByteArray();
        }
        response.setResponseCode(this.getCode());
        /* needed for ajax/crossdomain */
        response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN, "*"));
        response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "application/json; charset=UTF-8"));
        response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, bytes.length + ""));
        response.getOutputStream(true).write(bytes);
        response.getOutputStream(true).flush();
        return true;
    }

    /**
     * @param request
     */
    public void setRequest(final HttpRequestInterface request) {
        this.request = request;
    }

    /**
     * @param response
     */
    public void setResponse(final HttpResponseInterface response) {
        this.response = response;
    }

    /**
     * @param e
     * @return
     */
    public static BasicRemoteAPIException wrap(Throwable e) {
        if (e instanceof BasicRemoteAPIException) {
            return (BasicRemoteAPIException) e;
        }
        return new BasicRemoteAPIException(e, e.getClass().getSimpleName(), ResponseCode.SERVERERROR_INTERNAL, null);
    }

    /**
     * @param defaultLogger
     */
    public void writeToLogger(LogInterface logger) {
        if (request == null) {
            logger.info("Exception in Request **\r\n" + Exceptions.getStackTrace(this) + "\r\ndata:" + data);
        } else {
            logger.info("Exception in Request " + request.getId() + "/" + request.getRequestedURL() + "\r\n" + Exceptions.getStackTrace(this) + "\r\ndata:" + data);
        }
    }
}
