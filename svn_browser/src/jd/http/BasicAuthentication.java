package jd.http;

import jd.nutils.encoding.Encoding;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.utils.StringUtils;

public class BasicAuthentication extends Authentication {
    // https://tools.ietf.org/html/rfc7617#section-2.1
    public BasicAuthentication(final String host, String username, String password, String realm) {
        super(false, host, username, password, realm);
    }

    @Override
    public boolean authorize(final Browser browser, final Request request) {
        if (StringUtils.endsWithCaseInsensitive(request.getURL().getHost(), this.getHost()) && !this.isProxyAuthentication()) {
            final String userInfo = StringUtils.valueOrEmpty(this.getUsername()) + ":" + StringUtils.valueOrEmpty(this.getPassword());
            final String auth = Encoding.Base64Encode(userInfo);
            request.getHeaders().put(HTTPConstants.HEADER_REQUEST_AUTHORIZATION, "Basic " + auth);
            return true;
        } else {
            return false;
        }
    }

    @Override
    public boolean retry(Browser browser, Request request) {
        return false;
    }

    @Override
    public String toString() {
        return "Basic-Authorization[Host:" + this.getHost() + "|Realm:" + this.getRealm() + "|Username:" + this.getUsername() + "Password:" + this.getPassword() + "]";
    }
}
