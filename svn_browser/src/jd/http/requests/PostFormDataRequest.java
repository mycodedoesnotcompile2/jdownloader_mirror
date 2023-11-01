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

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import jd.http.Request;
import jd.http.URLConnectionAdapter;
import jd.http.requests.FormData.FormDataOutputStream;

import org.appwork.utils.StringUtils;
import org.appwork.utils.net.CountingOutputStream;
import org.appwork.utils.net.NullOutputStream;
import org.appwork.utils.net.httpconnection.HTTPConnection.RequestMethod;

/**
 * Extending the Request class, this class is able to to HTML Formdata Posts.
 *
 * @author coalado
 */
public class PostFormDataRequest extends Request {
    protected final String boundary;

    public String getBoundary() {
        return this.boundary;
    }

    protected final List<FormData> formDatas  = new ArrayList<FormData>();
    protected String               encodeType = "multipart/form-data";
    protected final static String  NEWLINE    = "\r\n";

    public PostFormDataRequest(final String url) throws IOException {
        super(url);
        this.boundary = this.generateBoundary();
    }

    public PostFormDataRequest(final URL url) throws IOException {
        super(url);
        this.boundary = this.generateBoundary();
    }

    protected PostFormDataRequest(PostFormDataRequest request) {
        super(request);
        this.boundary = this.generateBoundary();
    }

    public void addFormData(final FormData fd) throws IOException {
        if (fd != null) {
            switch (fd.getType()) {
            case DATA:
            case FILE:
            case STREAM:
            case VARIABLE:
                this.formDatas.add(fd);
                break;
            default:
                throw new IOException("Unsupported FormData type:" + fd.getType());
            }
        }
    }

    protected List<FormData> getFormData() {
        return this.formDatas;
    }

    protected String generateBoundary() {
        final long range = 999999999999999l - 100000000000000l;
        final long rand = (long) (Math.random() * range) + 100000000000000l;
        // https://www.ietf.org/rfc/rfc2046.txt
        // Boundary delimiters must not appear within the encapsulated material, and must be no longer than 70 characters, not counting the
        // two leading hyphens.
        return "---------------------" + System.currentTimeMillis() + "" + rand + "" + System.nanoTime();
    }

    public String getEncodeType() {
        return this.encodeType;
    }

    public String getPostDataString() {
        final StringBuffer sb = new StringBuffer();
        for (int i = 0; i < this.formDatas.size(); i++) {
            this.write(this.formDatas.get(i), sb);
        }
        sb.append(this.getBoundary());
        sb.append("--");
        sb.append(PostFormDataRequest.NEWLINE);
        return sb.toString();
    }

    private long postContent(final URLConnectionAdapter httpConnection) throws IOException {
        final CountingOutputStream output;
        if (httpConnection != null && httpConnection.getOutputStream() != null) {
            output = new CountingOutputStream(httpConnection.getOutputStream());
        } else {
            output = new CountingOutputStream(new NullOutputStream());
        }
        for (int i = 0; i < this.formDatas.size(); i++) {
            this.write(this.formDatas.get(i), output);
        }
        final OutputStreamWriter writer = new OutputStreamWriter(output, "UTF-8");
        writer.write(this.getBoundary());
        writer.write("--");
        writer.write(PostFormDataRequest.NEWLINE);
        writer.flush();
        output.flush();
        return output.transferedBytes();
    }

    @Override
    public Request cloneRequest() {
        final PostFormDataRequest ret = new PostFormDataRequest(this);
        ret.getFormData().addAll(this.getFormData());
        return ret;
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
    public RequestMethod getRequestMethod() {
        return RequestMethod.POST;
    }

    @Override
    public void preRequest() throws IOException {
        this.httpConnection.setRequestMethod(this.getRequestMethod());
        this.httpConnection.setRequestProperty("Content-Type", this.encodeType + "; boundary=" + this.getBoundary().substring(2));
        this.httpConnection.setRequestProperty("Content-Length", this.postContent(null) + "");
    }

    public void setEncodeType(final String encodeType) {
        this.encodeType = encodeType;
    }

    protected void write(final FormData formData, final OutputStream output) throws IOException {
        final OutputStreamWriter writer = new OutputStreamWriter(output);
        writer.write(this.getBoundary());
        writer.write(PostFormDataRequest.NEWLINE);
        final String value = StringUtils.valueOrEmpty(formData.getValue());
        switch (formData.getType()) {
        case VARIABLE:
            writer.write("Content-Disposition: form-data; name=\"" + formData.getName() + "\"");
            writer.write(PostFormDataRequest.NEWLINE);
            writer.write(PostFormDataRequest.NEWLINE);
            writer.write(value);
            writer.write(PostFormDataRequest.NEWLINE);
            break;
        case DATA:
            writer.write("Content-Disposition: form-data; name=\"" + formData.getName() + "\"; filename=\"" + value + "\"");
            final byte[] data = formData.getData();
            writer.write(PostFormDataRequest.NEWLINE);
            writer.write("Content-Type: " + formData.getDataType());
            writer.write(PostFormDataRequest.NEWLINE);
            writer.write("Content-Length: " + data.length);
            writer.write(PostFormDataRequest.NEWLINE);
            writer.write(PostFormDataRequest.NEWLINE);
            writer.flush();
            output.write(data);
            output.flush();
            writer.write(PostFormDataRequest.NEWLINE);
            writer.flush();
            break;
        case FILE:
            writer.write("Content-Disposition: form-data; name=\"" + formData.getName() + "\"; filename=\"" + value + "\"");
            writer.write(PostFormDataRequest.NEWLINE);
            writer.write("Content-Type: " + formData.getDataType());
            writer.write(PostFormDataRequest.NEWLINE);
            writer.write("Content-Length: " + formData.getFile().length());
            writer.write(PostFormDataRequest.NEWLINE);
            writer.write(PostFormDataRequest.NEWLINE);
            writer.flush();
            final InputStream in = new FileInputStream(formData.getFile());
            try {
                final byte[] buf = new byte[1024];
                int n = -1;
                while ((n = in.read(buf)) > -1) {
                    if (n > 0) {
                        output.write(buf, 0, n);
                    }
                }
            } finally {
                in.close();
            }
            output.flush();
            writer.write(PostFormDataRequest.NEWLINE);
            writer.flush();
            break;
        case STREAM:
            writer.write("Content-Disposition: form-data; name=\"" + formData.getName() + "\"; filename=\"" + value + "\"");
            writer.write(PostFormDataRequest.NEWLINE);
            writer.write("Content-Type: " + formData.getDataType());
            writer.write(PostFormDataRequest.NEWLINE);
            writer.write(PostFormDataRequest.NEWLINE);
            writer.flush();
            final FormDataOutputStream stream = formData.getStream();
            try {
                try {
                    stream.writeTo(output);
                } finally {
                    stream.onClosed();
                }
            } catch (IOException e) {
                stream.onIOException(e);
            }
            output.flush();
            writer.write(PostFormDataRequest.NEWLINE);
            writer.flush();
            break;
        }
        writer.flush();
        output.flush();
    }

    private void write(final FormData formData, final StringBuffer sb) {
        sb.append(this.getBoundary());
        sb.append(PostFormDataRequest.NEWLINE);
        final String value = StringUtils.valueOrEmpty(formData.getValue());
        switch (formData.getType()) {
        case VARIABLE:
            sb.append("Content-Disposition: form-data; name=\"").append(formData.getName()).append("\"");
            sb.append("\r\n\r\n");
            sb.append(value);
            sb.append("\r\n");
            break;
        case DATA:
            sb.append("Content-Disposition: form-data; name=\"").append(formData.getName()).append("\"; filename=\"").append(value).append("\"");
            final byte[] data = formData.getData();
            sb.append("\r\nContent-Type: " + formData.getDataType());
            sb.append("\r\nContent-Length: " + data.length);
            sb.append("\r\n\r\n[.....").append(data.length).append(" ByteArray DATA....]\r\n");
            break;
        case FILE:
            sb.append("Content-Disposition: form-data; name=\"").append(formData.getName()).append("\"; filename=\"").append(value).append("\"");
            sb.append("\r\nContent-Type: ").append(formData.getDataType());
            sb.append("\r\nContent-Length: " + formData.getFile().length());
            sb.append("\r\n\r\n[.....").append(formData.getFile().length()).append(" FileByte DATA....]\r\n");
            break;
        case STREAM:
            sb.append("Content-Disposition: form-data; name=\"").append(formData.getName()).append("\"; filename=\"").append(value).append("\"");
            sb.append("\r\nContent-Type: ").append(formData.getDataType());
            sb.append("\r\n\r\n[....").append("Stream DATA....]\r\n");
            break;
        }
    }
}
