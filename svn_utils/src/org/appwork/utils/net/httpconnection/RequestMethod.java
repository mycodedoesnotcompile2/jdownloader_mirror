/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
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
package org.appwork.utils.net.httpconnection;

import org.appwork.utils.ByteArrayUtils;

public enum RequestMethod {
    /**
     * UPnP NOTIFY method. Used to send event notifications. Has a request body (XML event data).
     */
    NOTIFY(true), // UPnP
    /**
     * UPnP M-SEARCH method. Used to discover UPnP devices and services. No request body.
     */
    MSEARCH("M-SEARCH", false), // UPnP
    /**
     * UPnP SUBSCRIBE method. Used to subscribe to event notifications. No request body (header-based).
     */
    SUBSCRIBE(false), // UPnP
    /**
     * UPnP UNSUBSCRIBE method. Used to unsubscribe from event notifications. No request body.
     */
    UNSUBSCRIBE(false), // UPnP
    /**
     * HTTP PUT method. Used to create or replace a resource. Requires a request body.
     */
    PUT(true), // HTTP / WebDAV
    /**
     * HTTP DELETE method. Used to delete a resource. No request body (body is not standard-compliant).
     */
    DELETE(false), // HTTP / WebDAV
    /**
     * HTTP OPTIONS method. Used to describe communication options. No request body.
     */
    OPTIONS(false), // HTTP
    /**
     * HTTP GET method. Used to retrieve a resource. No request body.
     */
    GET(false), // HTTP
    /**
     * HTTP POST method. Used to submit data to the server. Requires a request body.
     */
    POST(true), // HTTP
    /**
     * HTTP HEAD method. Same as GET but without a response body. No request body.
     */
    HEAD(false), // HTTP
    /**
     * WebDAV PROPFIND method. Used to retrieve properties of a resource. Requires a request body.
     */
    PROPFIND(true), // WebDAV
    /**
     * HTTP TRACE method. Used for diagnostic loop-back testing. No request body.
     */
    TRACE(false), // HTTP
    /**
     * HTTP CONNECT method. Used to establish a tunnel (e.g. for HTTPS). No HTTP request body.
     */
    CONNECT(false), // HTTP
    /**
     * HTTP PATCH method. Used to apply partial updates to a resource. Requires a request body.
     */
    PATCH(true), // HTTP
    /**
     * WebDAV PROPPATCH method. Used to modify resource properties. Requires a request body.
     */
    PROPPATCH(true), // WebDAV
    /**
     * WebDAV MKCOL method. Used to create a new collection (directory). No request body in standard usage.
     */
    MKCOL(false), // WebDAV
    /**
     * WebDAV COPY method. Used to copy a resource. Controlled by headers, not by a request body.
     */
    COPY(false), // WebDAV
    /**
     * WebDAV MOVE method. Used to move a resource. Controlled by headers, not by a request body.
     */
    MOVE(false), // WebDAV
    /**
     * WebDAV LOCK method. Used to lock a resource. Requires a request body with lock information.
     */
    LOCK(true), // WebDAV
    /**
     * WebDAV UNLOCK method. Used to remove a lock from a resource. No request body.
     */
    UNLOCK(false), // WebDAV
    /**
     * Unknown or unsupported HTTP method. No request body.
     */
    UNKNOWN(false);

    public final boolean mayHavePostBody;
    private final byte[] requestMethodBytes;

    private RequestMethod(final boolean requiresOutputStream) {
        this(null, requiresOutputStream);
    }

    /**
     *
     */
    private RequestMethod(final String methodName, final boolean requiresOutputStream) {
        this.mayHavePostBody = requiresOutputStream;
        final String methodNameString = methodName != null ? methodName : name();
        byte[] bytes = null;
        try {
            bytes = methodNameString.getBytes("ISO-8859-1");
        } catch (final Throwable e) {
            bytes = methodNameString.getBytes();
        }
        this.requestMethodBytes = bytes;
    }

    /**
     * Checks if the input byte array starts with this request method name.
     *
     * @param input
     *            The byte array to check
     * @return true if the input starts with this method name
     */
    private boolean isRequestMethod(final byte[] input) {
        if (input == null) {
            return false;
        }
        if (!ByteArrayUtils.contains(input, 0, requestMethodBytes)) {
            return false;
        }
        // Check that the method name is followed by a space, tab, or end of array
        if (input.length > this.requestMethodBytes.length) {
            final byte nextByte = input[this.requestMethodBytes.length];
            if (nextByte != ' ' && nextByte != '\t' && nextByte != '\r' && nextByte != '\n') {
                return false;
            }
        }
        return true;
    }

    /**
     * Parses the HTTP request method from a byte array containing the request line. The byte array should start with the method name,
     * followed by a space or end of array. This method is used to detect HTTP requests vs SSL handshakes (which start with different
     * bytes).
     *
     * @param guessProtocolBuffer
     *            The byte array containing the HTTP request line (e.g., "GET /path HTTP/1.1")
     * @return The matching RequestMethod enum value, or UNKNOWN if no match is found
     */
    public static RequestMethod get(byte[] guessProtocolBuffer) {
        if (guessProtocolBuffer == null || guessProtocolBuffer.length == 0) {
            return UNKNOWN;
        }
        for (RequestMethod method : values()) {
            if (method == UNKNOWN) {
                continue;
            } else if (method.isRequestMethod(guessProtocolBuffer)) {
                return method;
            }
        }
        return UNKNOWN;
    }
}