package jd.http;

import java.io.IOException;
import java.net.URL;

import org.appwork.utils.StringUtils;
import org.appwork.utils.net.URLHelper;

public abstract class Authentication {
    protected final String username;

    public String getUsername() {
        return this.username;
    }

    public String getPassword() {
        return this.password;
    }

    public String getRealm() {
        return this.realm;
    }

    protected final String  password;
    protected final String  realm;
    protected final boolean isProxyAuthentication;
    protected final String  host;

    public String getHost() {
        return this.host;
    }

    public boolean isProxyAuthentication() {
        return this.isProxyAuthentication;
    }

    protected Authentication(final boolean isProxyAuthentication, final String host, final String username, final String password, final String realm) {
        this.isProxyAuthentication = isProxyAuthentication;
        this.host = host;
        this.username = username;
        this.password = password;
        this.realm = realm;
    }

    public abstract boolean authorize(Browser browser, Request request);

    public abstract boolean retry(Browser browser, Request request);

    public String getURLWithUserInfo(URL url) throws IOException {
        if (url != null) {
            return URLHelper.createURL(url.getProtocol(), StringUtils.valueOrEmpty(this.getUsername()) + ":" + StringUtils.valueOrEmpty(this.getPassword()), url.getHost(), url.getPort(), url.getPath(), url.getQuery(), url.getRef());
        } else {
            return null;
        }
    }
}
