package org.jdownloader.captcha.v2.challenge.areyouahuman;

import java.io.IOException;
import java.net.URL;

import jd.plugins.Plugin;

import org.appwork.exceptions.WTFException;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.remoteapi.exceptions.RemoteAPIException;
import org.appwork.utils.IO;
import org.appwork.utils.StringUtils;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.httpserver.requests.GetRequest;
import org.appwork.utils.net.httpserver.requests.HttpRequest;
import org.appwork.utils.net.httpserver.requests.PostRequest;
import org.appwork.utils.net.httpserver.responses.HttpResponse;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.solver.browser.AbstractBrowserChallenge;
import org.jdownloader.captcha.v2.solver.browser.BrowserReference;

public abstract class AreYouAHumanChallenge extends AbstractBrowserChallenge {
    private String siteKey;

    public String getSiteKey() {
        return siteKey;
    }

    public AreYouAHumanChallenge(String siteKey, Plugin pluginForHost) {
        super("areyouahuman", pluginForHost);
        this.siteKey = siteKey;
        if (siteKey == null || !siteKey.matches("^[a-f0-9]{40}$")) {
            throw new WTFException("Bad SiteKey");
        }
    }

    @Override
    public boolean onGetRequest(BrowserReference browserReference, GetRequest request, HttpResponse response) throws IOException, RemoteAPIException {
        String parameter = request.getParameterbyKey("response");
        if (StringUtils.isNotEmpty(parameter)) {
            browserReference.onResponse(parameter);
            response.setResponseCode(ResponseCode.SUCCESS_OK);
            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "text/html; charset=utf-8"));
            response.getOutputStream(true).write("Please Close the Browser now".getBytes("UTF-8"));
            return true;
        }
        return super.onGetRequest(browserReference, request, response);
    }

    @Override
    public boolean onPostRequest(BrowserReference browserReference, PostRequest request, HttpResponse response) throws IOException, RemoteAPIException {
        return false;
    }

    @Override
    public String getHTML(HttpRequest request, String id) {
        String html;
        try {
            URL url = AreYouAHumanChallenge.class.getResource("areyouahumanchallenge.html");
            html = IO.readURLToString(url);
            html = html.replace("%%%sitekey%%%", siteKey);
            return html;
        } catch (IOException e) {
            throw new WTFException(e);
        }
    }

    @Override
    protected String getCaptchaNameSpace() {
        return "areyouahuman";
    }

    @Override
    public boolean validateResponse(AbstractResponse<String> response) {
        return super.validateResponse(response) && isCaptchaResponseValid();
    }

    @Override
    public boolean isCaptchaResponseValid() {
        if (super.isCaptchaResponseValid() && getResult().getValue().matches("[\\w-]{30,}")) {
            return true;
        } else {
            return false;
        }
    }
}
