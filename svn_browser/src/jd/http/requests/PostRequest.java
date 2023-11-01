//    jDownloader - Downloadmanager
//    Copyright (C) 2008  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.http.requests;

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map.Entry;

import jd.http.Request;
import jd.http.URLConnectionAdapter;

import org.appwork.utils.KeyValueEntry;
import org.appwork.utils.KeyValueStringEntry;
import org.appwork.utils.StringUtils;
import org.appwork.utils.net.CountingOutputStream;
import org.appwork.utils.net.NullOutputStream;
import org.appwork.utils.net.httpconnection.HTTPConnection.RequestMethod;
import org.appwork.utils.parser.UrlQuery;

public class PostRequest extends Request {
    private static enum SEND {
        VARIABLES,
        STRING,
        BYTES,
        NOTHING
    }

    public static java.util.List<KeyValueStringEntry> queryToVariableList(final UrlQuery post) {
        if (post == null) {
            return null;
        }
        final java.util.List<KeyValueStringEntry> ret = new ArrayList<KeyValueStringEntry>();
        for (final KeyValueEntry<String, String> entry : post.list()) {
            ret.add(new KeyValueStringEntry(entry.getKey(), entry.getValue()));
        }
        return ret;
    }

    private final java.util.List<KeyValueStringEntry> postVariables = new ArrayList<KeyValueStringEntry>();
    private String                                    postString    = null;
    private String                                    contentType   = null;
    private byte[]                                    postBytes     = null;
    private SEND                                      sendWHAT      = null;

    public PostRequest(final Request cloneRequest) {
        super(cloneRequest);
        if (cloneRequest instanceof PostRequest) {
            final PostRequest postRequest = (PostRequest) cloneRequest;
            this.addAll(postRequest.postVariables);
            this.sendWHAT = postRequest.sendWHAT;
            this.postString = postRequest.postString;
            this.setContentType(postRequest.getContentType());
            this.setPostBytes(postRequest.getPostBytes());
        }
    }

    public PostRequest(final String url) throws IOException {
        super(url);
    }

    public PostRequest(final URL url) throws IOException {
        super(url);
    }

    public void addAll(final HashMap<String, String> post) {
        for (final Entry<String, String> entry : post.entrySet()) {
            this.postVariables.add(new KeyValueStringEntry(entry));
        }
    }

    public void addAll(final java.util.List<KeyValueStringEntry> post) {
        this.postVariables.addAll(post);
    }

    public void addVariable(final String key, final String value) {
        this.postVariables.add(new KeyValueStringEntry(key, value));
    }

    /**
     * Changes the value of the first filed entry with the key, with new value. if no field exists, a new one is created.
     *
     * @param key
     * @param value
     */
    public void put(final String key, final String value) {
        final KeyValueStringEntry existing = this.getKeyValue(key);
        final KeyValueStringEntry newEntry = new KeyValueStringEntry(key, value);
        final int index = existing != null ? this.postVariables.indexOf(existing) : -1;
        if (index != -1) {
            this.postVariables.set(index, newEntry);
        } else {
            this.postVariables.add(newEntry);
        }
    }

    /**
     * Gets the first InputField with this key. REMEMBER. There can be more than one file with this key
     *
     * @param key
     * @return
     */
    public KeyValueStringEntry getKeyValue(final String key) {
        for (final KeyValueStringEntry input : this.postVariables) {
            if (input.getKey() != null && input.getKey().equalsIgnoreCase(key)) {
                return input;
            }
        }
        return null;
    }

    @Override
    public PostRequest cloneRequest() {
        final PostRequest ret = this.cloneRequestRaw();
        ret.addAll(this.postVariables);
        ret.sendWHAT = this.sendWHAT;
        ret.postString = this.postString;
        ret.setContentType(this.getContentType());
        ret.setPostBytes(this.getPostBytes());
        return ret;
    }

    protected PostRequest cloneRequestRaw() {
        return new PostRequest(this);
    }

    public String getPostDataString() {
        final SEND mode = this.getMode();
        if (mode != null) {
            switch (mode) {
            case STRING:
                return this.postString;
            case VARIABLES:
                final StringBuilder buffer = new StringBuilder();
                for (final KeyValueStringEntry rv : this.postVariables) {
                    if (rv.getKey() != null) {
                        buffer.append("&");
                        buffer.append(rv.getKey());
                        buffer.append("=");
                        final String value = rv.getValue();
                        if (value != null) {
                            buffer.append(value);
                        }
                    }
                }
                if (buffer.length() == 0) {
                    return "";
                } else {
                    return buffer.substring(1);
                }
            case BYTES:
            case NOTHING:
            default:
                return null;
            }
        }
        return null;
    }

    public String log() {
        if (this.sendWHAT == null) {
            return null;
        } else {
            switch (this.sendWHAT) {
            case NOTHING:
                return "zero content send";
            case BYTES:
                return this.postBytes.length + " raw-bytes send";
            case STRING:
            case VARIABLES:
                return this.getPostDataString();
            default:
                return "unknown postData send:" + this.sendWHAT;
            }
        }
    }

    protected long postContent(final URLConnectionAdapter httpConnection) throws IOException {
        if (this.sendWHAT == null) {
            throw new IOException("preRequest needs to be called first!");
        }
        final OutputStream os;
        if (httpConnection != null && httpConnection.getOutputStream() != null) {
            os = httpConnection.getOutputStream();
        } else {
            os = new CountingOutputStream(new NullOutputStream());
        }
        final CountingOutputStream output = new CountingOutputStream(os) {
            @Override
            public void close() throws IOException {
            }
        };
        try {
            switch (this.sendWHAT) {
            case NOTHING:
                return 0;
            case BYTES:
                output.write(this.postBytes);
                break;
            case STRING:
            case VARIABLES: {
                final String postString = this.getPostDataString();
                final OutputStreamWriter writer = new OutputStreamWriter(output, "UTF-8");
                writer.write(postString);
                writer.flush();
                writer.close();
            }
            break;
            default:
                throw new IOException("not implemented " + this.sendWHAT.name());
            }
            return output.transferedBytes();
        } finally {
            output.close();
        }
    }

    /**
     * send the postData of the Request. in case httpConnection is null, it outputs the data to a NullOutputStream
     */
    @Override
    public long postRequest() throws IOException {
        this.httpConnection.connect();
        return this.postContent(this.httpConnection);
    }

    @Override
    public void preRequest() throws IOException {
        this.httpConnection.setRequestMethod(this.getRequestMethod());
        if (this.contentType != null) {
            /* set Content Type */
            this.httpConnection.setRequestProperty("Content-Type", this.contentType);
        }
        /*
         * set Content-Length
         */
        if (this.postVariables != null && this.postVariables.size() > 0) {
            this.sendWHAT = SEND.VARIABLES;
            this.httpConnection.setRequestProperty("Content-Length", this.postContent(null) + "");
        } else if (!StringUtils.isEmpty(this.postString)) {
            this.sendWHAT = SEND.STRING;
            this.httpConnection.setRequestProperty("Content-Length", this.postContent(null) + "");
        } else if (this.postBytes != null) {
            this.sendWHAT = SEND.BYTES;
            this.httpConnection.setRequestProperty("Content-Length", this.postContent(null) + "");
        } else {
            this.sendWHAT = SEND.NOTHING;
            this.httpConnection.setRequestProperty("Content-Length", "0");
        }
    }

    public SEND getMode() {
        return this.sendWHAT;
    }

    @Override
    public RequestMethod getRequestMethod() {
        return RequestMethod.POST;
    }

    @Override
    protected boolean requireOutputStream() {
        return true;
    }

    public void setContentType(final String contentType) {
        this.contentType = contentType;
    }

    public String getContentType() {
        return this.contentType;
    }

    public void setPostBytes(final byte[] post) {
        this.postBytes = post;
    }

    public byte[] getPostBytes() {
        return this.postBytes;
    }

    public void setPostDataString(final String post) {
        this.postString = post;
    }
}
