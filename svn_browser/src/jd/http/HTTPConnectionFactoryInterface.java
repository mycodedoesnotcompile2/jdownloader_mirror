package jd.http;

import java.io.IOException;

import org.appwork.utils.net.httpconnection.HTTPProxy;

public interface HTTPConnectionFactoryInterface {
    URLConnectionAdapter createHTTPConnection(Request request, final HTTPProxy proxy) throws IOException;
}
