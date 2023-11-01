package jd.http;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;

public abstract class AbstractAuthenticationFactory implements AuthenticationFactory {
    protected boolean requiresAuthentication(Request request) {
        return request.getHttpConnection().getResponseCode() == 401 && this.getWWWAuthenticate(request) != null;
    }

    protected CopyOnWriteArrayList<Authentication> authentications = new CopyOnWriteArrayList<Authentication>();

    public boolean containsAuthentication(Authentication authentication) {
        return authentication != null && this.getAuthentications().contains(authentication);
    }

    public boolean addAuthentication(Authentication authentication) {
        return authentication != null && this.getAuthentications().addIfAbsent(authentication);
    }

    public boolean removeAuthentication(Authentication authentication) {
        return authentication != null && this.getAuthentications().remove(authentication);
    }

    public CopyOnWriteArrayList<Authentication> getAuthentications() {
        return this.authentications;
    }

    protected String[] getUserInfo(Request request) {
        final String[] userInfo = new Regex(request.getURL().getUserInfo(), "^(.*?)(:(.*?))?$").getRow(0);
        if (userInfo != null && (StringUtils.isNotEmpty(userInfo[0]) || StringUtils.isNotEmpty(userInfo[2]))) {
            return userInfo;
        } else {
            return null;
        }
    }

    protected abstract Authentication buildBasicAuthentication(Browser browser, Request request, final String realm);

    protected abstract Authentication buildDigestAuthentication(Browser browser, Request request, final String realm);

    @Override
    public boolean retry(Authentication authentication, Browser browser, Request request) {
        return authentication != null && this.containsAuthentication(authentication) && authentication.retry(browser, request);
    }

    protected String getRealm(Request request) {
        final List<String> wwwAuthenticateMethods = this.getWWWAuthenticate(request);
        if (wwwAuthenticateMethods != null) {
            for (final String wwwAuthenticateMethod : wwwAuthenticateMethods) {
                final String realm = new Regex(wwwAuthenticateMethod, "realm\\s*=\\s*\"(.*?)\"").getMatch(0);
                if (realm != null) {
                    return realm;
                }
            }
        }
        return null;
    }

    protected List<String> getWWWAuthenticate(Request request) {
        return request.getResponseHeaders(HTTPConstants.HEADER_RESPONSE_WWW_AUTHENTICATE);
    }

    @Override
    public Authentication buildAuthentication(Browser browser, Request request) {
        if (request.getAuthentication() == null && this.requiresAuthentication(request)) {
            final List<String> wwwAuthenticateMethods = this.getWWWAuthenticate(request);
            if (wwwAuthenticateMethods != null) {
                final String realm = this.getRealm(request);
                for (final String wwwAuthenticateMethod : wwwAuthenticateMethods) {
                    if (wwwAuthenticateMethod.matches("(?i)^\\s*Basic.*")) {
                        final Authentication ret = this.buildBasicAuthentication(browser, request, realm);
                        this.addAuthentication(ret);
                        return ret;
                    } else if (wwwAuthenticateMethod.matches("(?i)^\\s*Digest.*")) {
                        final Authentication ret = this.buildDigestAuthentication(browser, request, realm);
                        this.addAuthentication(ret);
                        return ret;
                    }
                }
            }
        }
        return null;
    }

    @Override
    public Authentication authorize(Browser browser, Request request) {
        for (final Authentication authentication : this.getAuthentications()) {
            if (authentication.authorize(browser, request)) {
                return authentication;
            }
        }
        return null;
    }
}
