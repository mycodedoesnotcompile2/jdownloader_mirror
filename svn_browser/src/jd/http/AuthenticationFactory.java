package jd.http;

public interface AuthenticationFactory {
    public abstract Authentication authorize(Browser browser, Request request);

    public Authentication buildAuthentication(Browser browser, Request request);

    public boolean retry(Authentication authentication, Browser browser, Request request);
}
