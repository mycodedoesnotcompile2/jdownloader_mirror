package jd.http;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.appwork.utils.net.httpconnection.HTTPProxy;

public class StaticProxySelector implements ProxySelectorInterface {
    private HTTPProxy       proxy;
    private List<HTTPProxy> lst;

    public StaticProxySelector(final HTTPProxy proxy) {
        this.proxy = proxy;
        ArrayList<HTTPProxy> lst = new ArrayList<HTTPProxy>();
        lst.add(proxy);
        this.lst = Collections.unmodifiableList(lst);
    }

    public HTTPProxy getProxy() {
        return this.proxy;
    }

    @Override
    public String toString() {
        return this.proxy.toString();
    }

    @Override
    public List<HTTPProxy> getProxiesByURL(final URL uri) {
        return this.lst;
    }

    @Override
    public boolean updateProxy(final Request request, final int retryCounter) {
        return false;
    }

    @Override
    public boolean reportConnectException(Request request, int retryCounter, IOException e) {
        return false;
    }
}
