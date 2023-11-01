package jd.http;

public class URLUserInfoAuthentication extends AbstractAuthenticationFactory {
    public URLUserInfoAuthentication() {
    }

    @Override
    protected Authentication buildBasicAuthentication(Browser browser, Request request, final String realm) {
        final String userInfo[] = this.getUserInfo(request);
        if (userInfo != null) {
            return new BasicAuthentication(request.getURL().getHost(), userInfo[0], userInfo[2], realm);
        } else {
            return null;
        }
    }

    @Override
    protected Authentication buildDigestAuthentication(Browser browser, Request request, final String realm) {
        final String userInfo[] = this.getUserInfo(request);
        if (userInfo != null) {
            return DigestAuthentication.build(browser, request, realm, userInfo[0], userInfo[2]);
        } else {
            return null;
        }
    }

    @Override
    public Authentication buildAuthentication(Browser browser, Request request) {
        if (this.getUserInfo(request) != null) {
            return super.buildAuthentication(browser, request);
        } else {
            return null;
        }
    }
}
