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
package org.appwork.utils.net.httpserver.requests;

import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.serializer.Deser;
import org.appwork.serializer.SC;
import org.appwork.storage.TypeRef;
import org.appwork.storage.commonInterface.SerializerInterface;
import org.appwork.storage.simplejson.JSonObject;
import org.appwork.utils.IO;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.net.ChunkedInputStream;
import org.appwork.utils.net.CountingInputStream;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.LimitedInputStream;
import org.appwork.utils.net.StreamValidEOF;
import org.appwork.utils.net.httpconnection.CountingGZIPInputStream;
import org.appwork.utils.net.httpconnection.CountingInflaterInputStream;
import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpserver.HttpServerConnection;
import org.appwork.utils.net.httpserver.LimitedInputStreamWithException;
import org.appwork.utils.net.httpserver.RawHttpConnectionInterface;
import org.appwork.utils.net.httpserver.RequestSizeLimits;

/**
 * HTTP POST request handler.
 *
 * <p>
 * POST is used to submit an entity to the specified resource.
 * </p>
 *
 * <p>
 * Note: This class extends {@link HttpRequest} directly and provides body handling functionality including input stream management, content
 * encoding (gzip, deflate), chunked transfer encoding, and parameter parsing (form-urlencoded and JSON). POST requests always have a
 * request body. According to the RequestMethod enum, POST has requiresOutputStream=true.
 * </p>
 *
 * <p>
 * This class serves as a base class for other request types that require body handling (e.g., PUT, PATCH, DELETE, WebDAV methods like
 * PROPFIND, PROPPATCH, COPY, MOVE, LOCK).
 * </p>
 *
 * @author daniel
 */
public abstract class AbstractPostRequest extends HttpRequest {
    public static enum CONTENT_TYPE {
        X_WWW_FORM_URLENCODED,
        JSON,
        UNKNOWN
    }

    protected class PostRequestInputStream extends CountingInputStream implements StreamValidEOF {
        protected PostRequestInputStream(InputStream in) {
            super(in);
        }

        protected volatile boolean closed = false;

        @Override
        public void close() throws IOException {
            this.closed = true;
            if (AbstractPostRequest.this.connection.closableStreams()) {
                super.close();
            }
        }

        @Override
        public int read() throws IOException {
            if (this.closed) {
                return -1;
            } else {
                return super.read();
            }
        }

        @Override
        public int read(final byte[] b) throws IOException {
            if (this.closed) {
                return -1;
            } else {
                return super.read(b);
            }
        }

        @Override
        public int read(final byte[] b, final int off, final int len) throws IOException {
            if (this.closed) {
                return -1;
            } else {
                return super.read(b, off, len);
            }
        }

        @Override
        public boolean isValidEOF() {
            if (in instanceof StreamValidEOF) {
                return ((StreamValidEOF) in).isValidEOF();
            } else {
                return false;
            }
        }
    }

    protected InputStream        inputStream         = null;
    protected boolean            postParameterParsed = false;
    protected List<KeyValuePair> postParameters      = null;

    /**
     * @param connection
     */
    public AbstractPostRequest(final RawHttpConnectionInterface connection) {
        super(connection);
    }

    /**
     * TODO: modify these to check if we need to wrap the inputstream again
     *
     * @return
     * @throws IOException
     */
    public synchronized InputStream getInputStream() throws IOException {
        if (this.inputStream == null) {
            final HTTPHeader transferEncoding = this.getRequestHeaders().get(HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING);
            InputStream inputStream;
            if (transferEncoding != null) {
                if ("chunked".equalsIgnoreCase(transferEncoding.getValue())) {
                    inputStream = new ChunkedInputStream(this.connection.getInputStream());
                } else {
                    throw new IOException("Unknown Transfer-Encoding " + transferEncoding.getValue());
                }
            } else {
                final HTTPHeader contentLength = this.getRequestHeaders().get(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH);
                if (contentLength == null || !contentLength.matches("^\\d+$")) {
                    inputStream = this.connection.getInputStream();
                } else {
                    inputStream = new LimitedInputStream(this.connection.getInputStream(), Long.parseLong(contentLength.getValue()));
                }
            }
            // Handle Content-Encoding (gzip, deflate)
            final HTTPHeader contentEncoding = this.getRequestHeaders().get(HTTPConstants.HEADER_REQUEST_CONTENT_ENCODING);
            if (contentEncoding != null) {
                final String encoding = contentEncoding.getValue();
                if (encoding != null) {
                    if ("gzip".equalsIgnoreCase(encoding) || "x-gzip".equalsIgnoreCase(encoding)) {
                        // GZIP encoding
                        inputStream = new CountingGZIPInputStream(inputStream);
                    } else if ("deflate".equalsIgnoreCase(encoding) || "x-deflate".equalsIgnoreCase(encoding)) {
                        // Deflate encoding
                        inputStream = new CountingInflaterInputStream(inputStream instanceof CountingInputStream ? (CountingInputStream) inputStream : new CountingInputStream(inputStream));
                    }
                    // Note: "identity" or "none" means no encoding, so we don't wrap
                }
            }
            // Apply post-processing limit AFTER decompression (if enabled)
            RequestSizeLimits limits = getServer().getRequestSizeLimits();
            if (limits != null) {
                long maxPostProcessing = limits.getMaxPostProcessedSize();
                if (maxPostProcessing > 0) {
                    inputStream = new LimitedInputStreamWithException(inputStream, maxPostProcessing);
                }
            }
            this.inputStream = new PostRequestInputStream(inputStream);
        }
        return this.inputStream;
    }

    public void setInputStream(InputStream inputStream) {
        this.inputStream = inputStream;
    }

    @Override
    public String getParameterbyKey(final String key) throws IOException {
        List<KeyValuePair> params = this.getRequestedURLParameters();
        if (params != null) {
            for (final KeyValuePair param : params) {
                if (key.equalsIgnoreCase(param.key)) {
                    return param.value;
                }
            }
        }
        params = this.getPostParameter();
        if (params != null) {
            for (final KeyValuePair param : params) {
                if (key.equalsIgnoreCase(param.key)) {
                    return param.value;
                }
            }
        }
        return null;
    }

    @Override
    public String[] getParametersbyKey(String key) throws IOException {
        final List<String> ret = new ArrayList<String>();
        List<KeyValuePair> params = this.getRequestedURLParameters();
        if (params != null) {
            for (final KeyValuePair param : params) {
                if (StringUtils.equalsIgnoreCase(key, param.key)) {
                    ret.add(param.value);
                }
            }
        }
        params = this.getPostParameter();
        if (params != null) {
            for (final KeyValuePair param : params) {
                if (StringUtils.equalsIgnoreCase(key, param.key)) {
                    ret.add(param.value);
                }
            }
        }
        if (ret.size() > 0) {
            return ret.toArray(new String[0]);
        } else {
            return null;
        }
    }

    /**
     * parse existing application/x-www-form-urlencoded PostParameters
     *
     * @return
     * @throws IOException
     */
    public synchronized List<KeyValuePair> getPostParameter() throws IOException {
        if (!this.postParameterParsed) {
            try {
                CONTENT_TYPE content_type = CONTENT_TYPE.UNKNOWN;
                String charSet = null;
                final String type = this.getRequestHeaders().getValue(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE);
                if (type != null) {
                    if (new Regex(type, "(application/x-www-form-urlencoded)").patternFind()) {
                        content_type = CONTENT_TYPE.X_WWW_FORM_URLENCODED;
                    } else if (new Regex(type, "(application/json)").patternFind()) {
                        content_type = CONTENT_TYPE.JSON;
                    } else {
                        content_type = CONTENT_TYPE.UNKNOWN;
                    }
                    charSet = new Regex(type, "charset=(.*?)($| )").getMatch(0);
                    if (charSet == null) {
                        charSet = "UTF-8";
                    }
                }
                postParameters = readPostParameters(content_type, charSet);
            } finally {
                this.postParameterParsed = true;
            }
        }
        return this.postParameters;
    }

    /**
     * @param updateClient
     * @return
     */
    public SerializerInterface getDeser(final Object context) {
        return Deser.get(context == null ? this : context);
    }

    protected List<KeyValuePair> readPostParameters(final CONTENT_TYPE content_type, final String charSet) throws IOException, UnsupportedEncodingException {
        if (content_type != null) {
            final InputStream inputStream = this.getInputStream();
            switch (content_type) {
            case JSON: {
                final byte[] jsonBytes = IO.readStream(-1, inputStream);
                final String jsonString = modifyByContentType(content_type, new String(jsonBytes, charSet));
                // try to parse JSonRequest object
                JSonRequest jsonRequest = null;
                if (jsonString.startsWith("[")) {
                    jsonRequest = new JSonRequest();
                    // List of Param Objects
                    jsonRequest.setParams(getDeser(this).fromString(jsonString, TypeRef.LIST).toArray(new Object[0]));
                } else {
                    jsonRequest = getDeser(this).fromString(jsonString, JSonRequest.TYPE_REF);
                    if (jsonRequest == null || jsonRequest.getParams() == null) {
                        // fallback to raw json object
                        final Object rawJSON = getDeser(this).fromString(jsonString, TypeRef.OBJECT);
                        if (rawJSON instanceof List) {
                            jsonRequest = new JSonRequest();
                            // List of Param Objects
                            jsonRequest.setParams(((List) rawJSON).toArray(new Object[0]));
                        } else if (rawJSON != null) {
                            jsonRequest = new JSonRequest();
                            // Single Param Object
                            jsonRequest.setParams(new Object[] { rawJSON });
                        }
                    }
                }
                if (jsonRequest != null && jsonRequest.getParams() != null) {
                    final LinkedList<KeyValuePair> ret = new LinkedList<KeyValuePair>();
                    for (final Object parameter : jsonRequest.getParams()) {
                        if (parameter instanceof JSonObject) {
                            /*
                             * JSonObject has customized .toString which converts Map to Json!
                             */
                            ret.add(new KeyValuePair(null, parameter.toString()));
                        } else {
                            final String jsonParameter = getDeser(this).toString(parameter, SC.NETWORK_TRANSFER);
                            ret.add(new KeyValuePair(null, jsonParameter));
                        }
                    }
                    return ret;
                }
            }
                break;
            case X_WWW_FORM_URLENCODED: {
                final byte[] formBytes = IO.readStream(-1, inputStream);
                final String formString = modifyByContentType(content_type, new String(formBytes, charSet));
                return HttpServerConnection.parseParameterList(formString);
            }
            case UNKNOWN:
            default:
                return null;
            }
        }
        return null;
    }

    /**
     * @param content_type
     * @param json
     * @return
     * @throws Exception
     */
    protected String modifyByContentType(CONTENT_TYPE content_type, String json) throws IOException {
        return json;
    }

    /**
     * @param params
     */
    public void setPostParameter(final List<KeyValuePair> params) {
        this.postParameterParsed = true;
        this.postParameters = params;
    }

    @Override
    public String toString() {
        try {
            final StringBuilder sb = new StringBuilder();
            sb.append("\r\n----------------Request " + getId() + "-------------------------\r\n");
            sb.append(getRequestMethod() + " ").append(this.getRequestedURL()).append(" HTTP/1.1\r\n");
            for (final HTTPHeader key : this.getRequestHeaders()) {
                sb.append(key.getKey());
                sb.append(": ");
                sb.append(key.getValue());
                sb.append("\r\n");
            }
            sb.append("\r\n");
            final List<KeyValuePair> postParams = postParameters;
            if (!postParameterParsed) {
                // do not read the stream in the toString method
                sb.append("Post Parameters unparsed");
            } else {
                if (postParams != null) {
                    for (final KeyValuePair s : postParams) {
                        sb.append(s.key);
                        sb.append(": ");
                        sb.append(s.value);
                        sb.append("\r\n");
                    }
                }
            }
            return sb.toString();
        } catch (final Throwable e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     * @return
     */
    public abstract RequestMethod getRequestMethod();
}
