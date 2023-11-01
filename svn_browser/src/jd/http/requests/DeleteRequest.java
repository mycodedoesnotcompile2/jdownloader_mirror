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
import java.net.MalformedURLException;
import java.net.URL;

import jd.http.Request;

import org.appwork.utils.net.httpconnection.HTTPConnection.RequestMethod;

/**
 * The HTTP/1.0 specification defined the GET, POST and HEAD methods and the HTTP/1.1 specification. The GET method requests a
 * representation of the specified resource. Requests using GET should only retrieve data and should have no other effect.
 */
public class DeleteRequest extends PostRequest {
    public DeleteRequest(final Request cloneRequest) {
        super(cloneRequest);
    }

    /**
     * constructor
     *
     * @param url
     *            the url - will be corrected
     * @see jd.http.Browser.correctURL(String)
     * @throws MalformedURLException
     *             in case URL was malformed
     */
    public DeleteRequest(final String url) throws IOException {
        super(url);
    }

    public DeleteRequest(final URL url) throws IOException {
        super(url);
    }

    @Override
    protected PostRequest cloneRequestRaw() {
        return new DeleteRequest(this);
    }

    @Override
    public RequestMethod getRequestMethod() {
        return RequestMethod.DELETE;
    }
}
