package jd.http;

import java.io.IOException;
import java.net.URL;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.utils.StringUtils;
import org.appwork.utils.net.URLHelper;

public class BearerAuthentication extends Authentication {
    protected final String token;

    public String getToken() {
        return this.token;
    }

    // https://www.rfc-editor.org/rfc/rfc6750#section-2.1
    public BearerAuthentication(final String host, String token, String realm) {
        super(false, host, null, null, realm);
        this.token = token;
    }

    @Override
    public boolean authorize(final Browser browser, final Request request) {
        if (StringUtils.endsWithCaseInsensitive(request.getURL().getHost(), this.getHost()) && !this.isProxyAuthentication()) {
            request.getHeaders().put(HTTPConstants.HEADER_REQUEST_AUTHORIZATION, "Bearer " + this.getToken());
            return true;
        } else {
            return false;
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        } else if (obj == this) {
            return true;
        } else if (!(obj instanceof BearerAuthentication)) {
            return false;
        }
        final BearerAuthentication other = (BearerAuthentication) obj;
        if (!StringUtils.equals(this.getHost(), other.getHost())) {
            return false;
        }
        if (!StringUtils.equals(this.getToken(), other.getToken())) {
            return false;
        }
        if (!StringUtils.equals(this.getRealm(), other.getRealm())) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        return BearerAuthentication.class.hashCode();
    }

    @Override
    public boolean retry(Browser browser, Request request) {
        return false;
    }

    @Override
    public String getURLWithUserInfo(URL url) throws IOException {
        if (url != null) {
            return URLHelper.createURL(url.getProtocol(), null, url.getHost(), url.getPort(), url.getPath(), url.getQuery(), url.getRef());
        } else {
            return null;
        }
    }

    @Override
    public String toString() {
        return "Bearer Authorization[Host:" + this.getHost() + "|Realm:" + this.getRealm() + "|Token:" + this.getToken() + "]";
    }
}
