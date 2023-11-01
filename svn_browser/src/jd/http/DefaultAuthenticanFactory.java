package jd.http;

import org.appwork.utils.StringUtils;

public class DefaultAuthenticanFactory extends AbstractAuthenticationFactory {
    protected final String host;
    protected final String realm;

    public String getRealm() {
        return this.realm;
    }

    public String getHost() {
        return this.host;
    }

    public String getUsername() {
        return this.username;
    }

    public String getPassword() {
        return this.password;
    }

    protected final String username;
    protected final String password;

    public DefaultAuthenticanFactory(final String host, final String realm, final String username, final String password) {
        this.host = host;
        this.realm = realm;
        this.username = username;
        this.password = password;
    }

    @Override
    protected Authentication buildBasicAuthentication(Browser browser, Request request, final String realm) {
        if (this.matches(browser, request, realm)) {
            return new BasicAuthentication(this.getHost(), this.getUsername(), this.getPassword(), realm);
        }
        return null;
    }

    protected boolean matchesHost(final Request request) {
        return StringUtils.endsWithCaseInsensitive(request.getURL().getHost(), this.getHost());
    }

    protected boolean matchesRealm(final String realm) {
        return this.getRealm() == null || StringUtils.equalsIgnoreCase(realm, this.getRealm()) || StringUtils.isEmpty(this.getRealm()) && StringUtils.isEmpty(realm);
    }

    protected boolean matches(Browser browser, Request request, final String realm) {
        return this.matchesHost(request) && this.matchesRealm(realm) && (StringUtils.isNotEmpty(this.getUsername()) || StringUtils.isNotEmpty(this.getPassword()));
    }

    @Override
    protected Authentication buildDigestAuthentication(Browser browser, Request request, final String realm) {
        if (this.matches(browser, request, realm)) {
            return DigestAuthentication.build(browser, request, realm, this.getUsername(), this.getPassword());
        } else {
            return null;
        }
    }

    @Override
    public Authentication buildAuthentication(Browser browser, Request request) {
        if (this.matches(browser, request, this.getRealm(request))) {
            return super.buildAuthentication(browser, request);
        } else {
            return null;
        }
    }
}
