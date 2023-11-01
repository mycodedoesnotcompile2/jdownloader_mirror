package jd.http;

public class NoneAuthenticationFactory implements AuthenticationFactory {
    @Override
    public Authentication authorize(Browser browser, Request request) {
        return null;
    }

    @Override
    public Authentication buildAuthentication(Browser browser, Request request) {
        return null;
    }

    @Override
    public boolean retry(Authentication authentication, Browser browser, Request request) {
        return false;
    }
}
