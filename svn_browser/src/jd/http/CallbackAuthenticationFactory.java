package jd.http;

public abstract class CallbackAuthenticationFactory extends AbstractAuthenticationFactory {
    protected abstract Authentication askAuthentication(Browser browser, Request request, final String realm);

    @Override
    protected Authentication buildBasicAuthentication(Browser browser, Request request, String realm) {
        return this.askAuthentication(browser, request, realm);
    }

    @Override
    protected Authentication buildDigestAuthentication(Browser browser, Request request, String realm) {
        return this.askAuthentication(browser, request, realm);
    }
}
