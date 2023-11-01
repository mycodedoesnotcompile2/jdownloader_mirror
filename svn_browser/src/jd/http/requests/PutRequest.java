package jd.http.requests;

import java.io.IOException;
import java.net.URL;

import jd.http.Request;

import org.appwork.utils.net.httpconnection.HTTPConnection.RequestMethod;

public class PutRequest extends PostRequest {
    public PutRequest(final Request cloneRequest) {
        super(cloneRequest);
    }

    public PutRequest(final String url) throws IOException {
        super(url);
    }

    public PutRequest(final URL url) throws IOException {
        super(url);
    }

    @Override
    public RequestMethod getRequestMethod() {
        return RequestMethod.PUT;
    }

    @Override
    protected PostRequest cloneRequestRaw() {
        return new PutRequest(this);
    }
}
