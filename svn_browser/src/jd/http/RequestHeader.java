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
package jd.http;

import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.HeaderCollection;

public class RequestHeader implements Iterable<HTTPHeader> {
    /**
     * For more header fields see
     *
     * @link(http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14).
     */
    private boolean                dominant = false;
    private final HeaderCollection headers  = new HeaderCollection();

    public RequestHeader() {
        this.headers.getAllowedDuplicatedKeys().clear();
    }

    public RequestHeader(final Map<String, String> headers) {
        this();
        this.putAll(headers);
    }

    public RequestHeader(final RequestHeader requestHeader) {
        this();
        if (requestHeader != null) {
            this.putAll(requestHeader);
            this.dominant = requestHeader.dominant;
        }
    }

    public void clear() {
        this.headers.clear();
    }

    @Override
    public RequestHeader clone() {
        return new RequestHeader(this);
    }

    public boolean contains(final String key) {
        return this.headers.get(key) != null;
    }

    public String getValue(final String key) {
        final HTTPHeader header = this.headers.get(key);
        if (header != null) {
            return header.getValue();
        } else {
            return null;
        }
    }

    @Deprecated
    public String get(final String key) {
        return this.getValue(key);
    }

    public HTTPHeader getHeader(final String key) {
        return this.headers.get(key);
    }

    public int indexOf(HTTPHeader header) {
        return this.headers.indexOf(header);
    }

    public boolean isDominant() {
        return this.dominant;
    }

    @Override
    public Iterator<HTTPHeader> iterator() {
        return this.headers.iterator();
    }

    public void put(final HTTPHeader header) {
        this.headers.add(header);
    }

    public void put(final String key, final String value) {
        this.put(new HTTPHeader(key, value));
    }

    public void putAll(final Map<String, String> properties) {
        for (final Entry<String, String> entry : properties.entrySet()) {
            this.put(entry.getKey(), entry.getValue());
        }
    }

    public void putAll(final RequestHeader headers) {
        for (final HTTPHeader header : headers) {
            this.put(header);
        }
    }

    public HTTPHeader remove(final HTTPHeader header) {
        return this.headers.remove(header);
    }

    public HTTPHeader remove(final String key) {
        return this.headers.remove(key);
    }

    /**
     * if a header is dominant, it will not get merged with existing headers. It will replace it completely
     *
     * @param dominant
     */
    public void setDominant(final boolean dominant) {
        this.dominant = dominant;
    }

    public int size() {
        return this.headers.size();
    }

    @Override
    public String toString() {
        return this.headers.toString();
    }
}
