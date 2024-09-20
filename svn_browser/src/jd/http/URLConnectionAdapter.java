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

import java.io.IOException;
import java.io.InputStream;
import java.net.SocketAddress;

import org.appwork.utils.net.httpconnection.HTTPConnectionUtils.IPVERSION;

public interface URLConnectionAdapter extends org.appwork.utils.net.httpconnection.HTTPConnection {
    public static final String CRLF = "\r\n";

    long getContentLength();

    public InputStream getErrorStream();

    long getLongContentLength();

    public Request getRequest();

    void setRequest(Request request);

    public SocketAddress getEndPointSocketAddress();

    public InputStream setInputStream(InputStream is) throws IOException;

    public String getCipherSuite();

    public IPVERSION getIPVersion();

    public void setIPVersion(IPVERSION tcpVersion);

    public boolean isAllResponseCodesAllowed();

    public void setAllResponseCodesAllowed(boolean b);
}