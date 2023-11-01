//    jDownloader - Downloadmanager
//    Copyright (C) 2009  JD-Team support@jdownloader.org
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

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;

/**
 * In many cases, the purpose of an HTML Form is to send data to a server. The server will process the data and then send a response to the
 * user. Files are a special case with HTML forms. They are binary data—or considered as such—where all others data are text data. Because
 * HTTP is a text protocol, there are special requirements to handle binary data. This class is simple POJO holding {@link File} reference,
 * byte[] of data, a type and a name.
 */
public class FormData {
    public static interface FormDataOutputStream {
        public void writeTo(final OutputStream os) throws IOException;

        public void onIOException(IOException e) throws IOException;

        public void onClosed();
    }

    /**
     * enumeration of FormData types
     */
    public static enum Type {
        DATA,
        FILE,
        VARIABLE,
        STREAM
    }

    /** default MIME type: {@value #DEFAULT_MIME} */
    private static final String  DEFAULT_MIME = "application/octet-stream";
    /** data byte array */
    private byte[]               data;
    /** file */
    private File                 file;
    /** the Multipurpose Internet Mail Extensions */
    private String               mime;
    /** The name. */
    private final String         name;
    /** The type. */
    private final Type           type;
    /** The value. */
    private final String         value;
    private FormDataOutputStream stream;

    public FormDataOutputStream getStream() {
        return this.stream;
    }

    /**
     * constructor
     *
     * @param name
     *            the name
     * @param value
     *            the value
     */
    public FormData(final String name, final String value) {
        this.type = Type.VARIABLE;
        this.name = name;
        this.value = value;
    }

    public FormData(final String name, final String filename, final byte[] data) {
        this(name, filename, null, data);
    }

    public FormData(final String name, final String filename, final File file) {
        this(name, filename, null, file);
    }

    public FormData(final String name, final String filename, final String mime, final byte[] data) {
        this.mime = mime == null ? FormData.DEFAULT_MIME : mime;
        this.type = Type.DATA;
        this.name = name;
        this.value = filename;
        this.data = data;
    }

    public FormData(final String name, final String filename, final String mime, final File file) {
        this.mime = mime == null ? FormData.DEFAULT_MIME : mime;
        this.type = Type.FILE;
        this.name = name;
        this.value = filename;
        this.file = file;
    }

    public FormData(final String name, final String filename, final String mime, final FormDataOutputStream stream) {
        this.mime = mime == null ? FormData.DEFAULT_MIME : mime;
        this.type = Type.STREAM;
        this.name = name;
        this.value = filename;
        this.stream = stream;
    }

    /**
     * @return the data byte[]
     */
    public byte[] getData() {
        return this.data;
    }

    /**
     * @return the mime data type
     */
    public String getDataType() {
        final String ret = this.mime;
        if (ret != null) {
            return ret;
        } else {
            return FormData.DEFAULT_MIME;
        }
    }

    @Override
    public String toString() {
        switch (this.getType()) {
        case DATA:
        case STREAM:
            return this.getType() + ":" + this.getName();
        case FILE:
            return this.getType() + ":" + this.getName() + "=" + this.getFile();
        case VARIABLE:
        default:
            return this.getType() + ":" + this.getName() + "=" + this.getValue();
        }
    }

    /**
     * @return the file
     */
    public File getFile() {
        return this.file;
    }

    /**
     * @return the name
     */
    public String getName() {
        return this.name;
    }

    /**
     * @return the type
     */
    public Type getType() {
        return this.type;
    }

    /**
     * @return the value
     */
    public String getValue() {
        return this.value;
    }
}
