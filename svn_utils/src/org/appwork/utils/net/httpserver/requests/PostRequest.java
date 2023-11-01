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
package org.appwork.utils.net.httpserver.requests;

import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.simplejson.JSonObject;
import org.appwork.utils.IO;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.net.ChunkedInputStream;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.LimitedInputStream;
import org.appwork.utils.net.StreamValidEOF;
import org.appwork.utils.net.httpconnection.HTTPConnection.RequestMethod;
import org.appwork.utils.net.httpserver.HttpConnection;
import org.appwork.utils.net.httpserver.HttpConnection.HttpConnectionType;
import org.appwork.utils.net.httpserver.RawHttpConnectionInterface;

/**
 * @author daniel
 *
 */
public class PostRequest extends HttpRequest {
    public static enum CONTENT_TYPE {
        X_WWW_FORM_URLENCODED,
        JSON,
        UNKNOWN
    }

    protected class PostRequestInputStream extends FilterInputStream implements StreamValidEOF {

        protected PostRequestInputStream(InputStream in) {
            super(in);
        }

        protected volatile boolean closed = false;

        @Override
        public void close() throws IOException {
            this.closed = true;
            if (PostRequest.this.connection.closableStreams()) {
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
    public PostRequest(final RawHttpConnectionInterface connection) {
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
            final InputStream inputStream;
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
                    if (new Regex(type, "(application/x-www-form-urlencoded)").matches()) {
                        content_type = CONTENT_TYPE.X_WWW_FORM_URLENCODED;
                    } else if (new Regex(type, "(application/json)").matches()) {
                        content_type = CONTENT_TYPE.JSON;
                    } else {
                        content_type = CONTENT_TYPE.UNKNOWN;
                    }
                    charSet = new Regex(type, "charset=(.*?)($| )").getMatch(0);
                    if (charSet == null) {
                        charSet = "UTF-8";
                    }
                }
                readPostParameters(content_type, charSet);
            } finally {
                this.postParameterParsed = true;
            }
        }
        return this.postParameters;
    }

    protected void readPostParameters(CONTENT_TYPE content_type, String charSet) throws IOException, UnsupportedEncodingException {
        JSonRequest jsonRequest = null;
        if (content_type != null) {
            switch (content_type) {
            case JSON: {
                final byte[] jsonBytes = IO.readStream(-1, this.getInputStream());
                String json = new String(jsonBytes, charSet);
                json = modifyByContentType(content_type, json);
                jsonRequest = JSonStorage.restoreFromString(json, JSonRequest.TYPE_REF);
            }
                break;
            case X_WWW_FORM_URLENCODED: {
                final byte[] jsonBytes = IO.readStream(-1, this.getInputStream());
                String params = new String(jsonBytes, charSet);
                params = modifyByContentType(content_type, params);
                this.postParameters = HttpConnection.parseParameterList(params);
            }
                break;
            case UNKNOWN:
            default:
                break;
            }
        }
        if (jsonRequest != null && jsonRequest.getParams() != null) {
            this.postParameters = new LinkedList<KeyValuePair>();
            for (final Object parameter : jsonRequest.getParams()) {
                if (parameter instanceof JSonObject) {
                    /*
                     * JSonObject has customized .toString which converts Map to Json!
                     */
                    this.postParameters.add(new KeyValuePair(null, parameter.toString()));
                } else {
                    final String jsonParameter = JSonStorage.serializeToJson(parameter);
                    this.postParameters.add(new KeyValuePair(null, jsonParameter));
                }
            }
        }
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
    protected RequestMethod getRequestMethod() {
        return RequestMethod.POST;
    }

    @Override
    public HttpConnectionType getHttpConnectionType() {
        return HttpConnectionType.POST;
    }
}
