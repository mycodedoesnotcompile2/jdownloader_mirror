package jd.http;

import org.appwork.utils.net.httpconnection.HTTPProxy;

public class ClonedProxy extends HTTPProxy {

    private final HTTPProxy parent;

    public HTTPProxy getParent() {
        return this.parent;
    }

    public ClonedProxy(HTTPProxy parent) {
        super(parent);
        this.parent = parent;
    }
}