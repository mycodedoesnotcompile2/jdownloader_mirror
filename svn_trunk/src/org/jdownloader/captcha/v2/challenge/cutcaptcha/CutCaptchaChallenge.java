package org.jdownloader.captcha.v2.challenge.cutcaptcha;

import java.io.IOException;
import java.net.URL;
import java.util.regex.Matcher;

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
import org.jdownloader.captcha.v2.ValidationResult;
import org.jdownloader.captcha.v2.solver.browser.AbstractBrowserChallenge;
import org.jdownloader.captcha.v2.solver.browser.BrowserReference;

import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;

public abstract class CutCaptchaChallenge extends AbstractBrowserChallenge {
    private final String siteKey;
    private final String apiKey;

    public String getSiteKey() {
        return siteKey;
    }

    /**
     * Only available for the old CutCaptcha version (siteKey = 40 char "misery key"). Returns null for the new version (siteKey "cc_...").
     */
    public String getApiKey() {
        return apiKey;
    }

    /**
     * @return true if this is the new CutCaptcha version (api.cutcaptcha.net/v1/api.js) which only needs a siteKey ("cc_..."), false for
     *         the old version which needs siteKey (misery key) and apiKey.
     */
    public boolean isV2() {
        return looksLikeNewSiteKey(siteKey);
    }

    public CutCaptchaChallenge(final Plugin plugin, final String siteKey, final String apiKey) throws PluginException {
        super("cutcaptcha", plugin);
        if (looksLikeNewSiteKey(siteKey)) {
            /* New version: Only siteKey is needed. */
            this.apiKey = null;
        } else if (!looksLikeValidOldSiteKey(siteKey)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Bad SiteKey:" + siteKey);
        } else if (!looksLikeValidApiKey(apiKey)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Bad APIKey:" + apiKey);
        } else {
            this.apiKey = apiKey;
        }
        this.siteKey = siteKey;
    }

    /* Old version: 40 char hex "misery key". */
    private static boolean looksLikeValidOldSiteKey(final String siteKey) {
        return siteKey != null && siteKey.matches("^[a-f0-9]{40}$");
    }

    /* New version: e.g. "cc_zKZGmOrcOLMA6vMA4gJeFqrh" */
    public static boolean looksLikeNewSiteKey(final String siteKey) {
        return siteKey != null && siteKey.matches("^cc_[A-Za-z0-9]{10,}$");
    }

    private static boolean looksLikeValidApiKey(final String siteKey) {
        if (siteKey == null) {
            return false;
        } else if (siteKey.matches("^[\\w-]{5,}$")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public boolean onGetRequest(BrowserReference browserReference, GetRequest request, HttpResponse response) throws IOException, RemoteAPIException {
        final String parameter = request.getParameterbyKey("response");
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
            URL url = CutCaptchaChallenge.class.getResource("cutcaptcha.html");
            html = IO.readURLToString(url);
            if (isV2()) {
                /* V2: The template is written for V1, replace its script tag and its placeholder div with the V2 widget. */
                html = html.replace("<script src=\"https://cutcaptcha.com/captcha/%%%sitekey%%%.js\"></script>", "<script src=\"https://api.cutcaptcha.net/v1/api.js\" crossorigin=\"anonymous\" async defer></script>");
                html = html.replaceFirst("<div id=\"puzzle-captcha\"[^>]*>.*?</div>", Matcher.quoteReplacement("<div class=\"cutcaptcha\" data-callback=\"capResponseCallback\" data-sitekey=\"" + siteKey + "\" style=\"min-height:150px;width:100%;min-width:320px;display:flex;align-items:center;\"><strong>Please disable AdBlock (cutcaptcha.net)</strong></div>"));
            } else {
                /* V1: The script URL contains the apiKey, not the siteKey (misery key). */
                html = html.replace("%%%sitekey%%%", apiKey);
            }
            return html;
        } catch (IOException e) {
            throw new WTFException(e);
        }
    }

    @Override
    protected String getCaptchaNameSpace() {
        return "cut";
    }

    public static boolean looksLikeValidToken(final String str) {
        /* V2 tokens contain a dot, e.g. "cc1.mfaaa-FfFfFffffffffffffffffffffF" */
        return str != null && str.matches("[\\w.-]{10,}");
    }

    @Override
    public ValidationResult validateResponse(AbstractResponse<String> response) {
        final ValidationResult result = super.validateResponse(response);
        if (result != ValidationResult.VALID) {
            return result;
        }
        return looksLikeValidToken(response.getValue()) ? ValidationResult.VALID : ValidationResult.INVALID_TOKEN;
    }
}
