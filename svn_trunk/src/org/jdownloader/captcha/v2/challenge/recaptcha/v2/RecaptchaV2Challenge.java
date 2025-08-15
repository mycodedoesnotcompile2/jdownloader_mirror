package org.jdownloader.captcha.v2.challenge.recaptcha.v2;

import java.awt.Rectangle;
import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Pattern;

import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.nutils.encoding.HTMLEntities;
import jd.plugins.Plugin;

import org.appwork.exceptions.WTFException;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.remoteapi.exceptions.RemoteAPIException;
import org.appwork.storage.SimpleMapper;
import org.appwork.storage.Storable;
import org.appwork.utils.Application;
import org.appwork.utils.Hash;
import org.appwork.utils.IO;
import org.appwork.utils.KeyValueStringEntry;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.httpserver.requests.GetRequest;
import org.appwork.utils.net.httpserver.requests.HttpRequest;
import org.appwork.utils.net.httpserver.requests.PostRequest;
import org.appwork.utils.net.httpserver.responses.HttpResponse;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.ChallengeSolver;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.BasicCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.CaptchaResponse;
import org.jdownloader.captcha.v2.solver.browser.AbstractBrowserChallenge;
import org.jdownloader.captcha.v2.solver.browser.BrowserReference;
import org.jdownloader.captcha.v2.solver.browser.BrowserViewport;
import org.jdownloader.captcha.v2.solver.browser.BrowserWindow;
import org.jdownloader.captcha.v2.solver.service.BrowserSolverService;
import org.jdownloader.gui.translate._GUI;

public abstract class RecaptchaV2Challenge extends AbstractBrowserChallenge {
    public static final String             RAWTOKEN    = "rawtoken";
    public static final String             RECAPTCHAV2 = "recaptchav2";
    private final String                   siteKey;
    private volatile BasicCaptchaChallenge basicChallenge;
    private final String                   siteDomain;
    private final String                   secureToken;
    private final boolean                  localhost   = true;

    public String getSiteKey() {
        return siteKey;
    }

    public static class RecaptchaV2APIStorable implements Storable, AbstractBrowserChallengeAPIStorable {
        public RecaptchaV2APIStorable() {
        }

        private String  stoken;
        private String  type;
        private String  v3Action;
        private boolean enterprise = false;

        public boolean isEnterprise() {
            return enterprise;
        }

        public void setEnterprise(boolean enterprise) {
            this.enterprise = enterprise;
        }

        public String getV3Action() {
            return v3Action;
        }

        public void setV3Action(String v3Action) {
            this.v3Action = v3Action;
        }

        public String getType() {
            if (type == null) {
                return AbstractRecaptchaV2.TYPE.NORMAL.name();
            } else {
                return type;
            }
        }

        public void setType(String type) {
            this.type = type;
        }

        public String getStoken() {
            return stoken;
        }

        public void setStoken(String stoken) {
            this.stoken = stoken;
        }

        public String getSiteKey() {
            return siteKey;
        }

        public void setSiteKey(String siteKey) {
            this.siteKey = siteKey;
        }

        public String getContextUrl() {
            return contextUrl;
        }

        public void setContextUrl(String contextUrl) {
            this.contextUrl = contextUrl;
        }

        private String siteKey;
        private String contextUrl;
        private String siteUrl;

        public String getSiteUrl() {
            return siteUrl;
        }

        public void setSiteUrl(String siteUrl) {
            this.siteUrl = siteUrl;
        }

        public boolean isSameOrigin() {// required for webinterface checking on this field
            return true;
        }

        public void setSameOrigin(boolean sameOrigin) {
        }

        public boolean isBoundToDomain() {// required for webinterface checking on this field
            return true;
        }

        public void setBoundToDomain(boolean boundToDomain) {
        }
    }

    @Override
    public AbstractResponse<String> parseAPIAnswer(String result, String resultFormat, ChallengeSolver<?> solver) {
        if (RAWTOKEN.equals(resultFormat) || "extension".equals(resultFormat)) {
            return new CaptchaResponse(this, solver, result, 100);
        } else {
            if (hasBasicCaptchaChallenge()) {
                final BasicCaptchaChallenge basic = createBasicCaptchaChallenge(false);
                if (basic != null) {
                    return basic.parseAPIAnswer(result, resultFormat, solver);
                }
            }
            return super.parseAPIAnswer(result, resultFormat, solver);
        }
    }

    @Override
    public Object getAPIStorable(String format) throws Exception {
        if (RAWTOKEN.equals(format)) {
            final RecaptchaV2APIStorable ret = new RecaptchaV2APIStorable();
            ret.setSiteKey(getSiteKey());
            final String siteUrl = getSiteUrl();
            final String protocol;
            if (StringUtils.startsWithCaseInsensitive("https://", siteUrl)) {
                protocol = "https://";
            } else {
                protocol = "http://";
            }
            ret.setEnterprise(isEnterprise());
            ret.setContextUrl(protocol + getSiteDomain());
            ret.setSiteUrl(siteUrl);
            ret.setStoken(getSecureToken());
            final Map<String, Object> v3Action = getV3Action();
            if (v3Action != null) {
                ret.setV3Action(new SimpleMapper().setPrettyPrintEnabled(false).objectToString(v3Action));
            }
            ret.setType(getType());
            return ret;
        } else {
            final BasicCaptchaChallenge basic = createBasicCaptchaChallenge(true);
            if (basic != null) {
                return basic.getAPIStorable(format);
            } else {
                return super.getAPIStorable(format);
            }
        }
    }

    @Override
    public BrowserViewport getBrowserViewport(BrowserWindow screenResource, Rectangle elementBounds) {
        Rectangle rect = null;
        int sleep = 500;
        for (int i = 0; i < 3; i++) {
            try {
                Thread.sleep(sleep);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            rect = screenResource.getRectangleByColor(0xFF9900, 0, 0, 50, elementBounds.x, elementBounds.y);
            if (rect == null) {
                sleep *= 2;
                continue;
            }
            break;
        }
        if (rect != null) {
            return new Recaptcha2BrowserViewport(screenResource, rect, elementBounds) {
                @Override
                protected LogInterface getLogger() {
                    return RecaptchaV2Challenge.this.getLogger();
                }
            };
        } else {
            return null;
        }
    }

    @Override
    public void cleanup() {
        super.cleanup();
        final BasicCaptchaChallenge basicChallenge = this.basicChallenge;
        if (basicChallenge != null) {
            basicChallenge.cleanup();
        }
    }

    public RecaptchaV2Challenge(final String siteKey, final String secureToken, Plugin pluginForHost, Browser br, String siteDomain) {
        super(RECAPTCHAV2, pluginForHost, br);
        this.secureToken = secureToken;
        this.siteKey = siteKey;
        this.siteDomain = siteDomain;
        if (!AbstractRecaptchaV2.isValidSiteKey(siteKey)) {
            throw new WTFException("Bad SiteKey:" + siteKey);
        }
    }

    public boolean isV3() {
        return false;
    }

    public boolean isEnterprise() {
        return false;
    }

    public AbstractRecaptchaV2<?> getAbstractCaptchaHelperRecaptchaV2() {
        return null;
    }

    public boolean isInvisible() {
        return AbstractRecaptchaV2.TYPE.INVISIBLE.name().equals(getType());
    }

    public String getType() {
        return AbstractRecaptchaV2.TYPE.NORMAL.name();
    };

    public String getSiteUrl() {
        return null;
    }

    public String getSecureToken() {
        return secureToken;
    }

    public String getSiteDomain() {
        return siteDomain;
    }

    @Override
    public boolean onPostRequest(final BrowserReference brRef, final PostRequest request, final HttpResponse response) throws IOException, RemoteAPIException {
        synchronized (this) {
            String pDo = request.getParameterbyKey("do");
            // if (!"pDo".startsWith("http")) {
            // pDo = "https://www.google.com" + pDo;
            // }
            String refOrg = request.getRequestHeaders().get("Referer").getValue();
            String ref = UrlQuery.parse(refOrg).getDecoded("do");
            if (StringUtils.isEmpty(ref)) {
                ref = "http://" + getSiteDomain();
            }
            if (pDo.startsWith("/recaptcha/api2/reload?")) {
                ensureBrowser();
                String rawPost = IO.readInputStreamToString(request.getInputStream());
                br.setKeepResponseContentBytes(true);
                final jd.http.requests.PostRequest r = br.createPostRequest("https://www.google.com" + pDo, new ArrayList<KeyValueStringEntry>(), null);
                r.setPostDataString(rawPost);
                r.getHeaders().put("Referer", ref);
                forward(request, response, br, r, new Modifier() {
                    public byte[] onData(byte[] bytes) {
                        String str;
                        try {
                            str = new String(bytes, "UTF-8");
                            str = replace(brRef, str);
                            return str.getBytes("UTF-8");
                        } catch (UnsupportedEncodingException e) {
                            throw new WTFException(e);
                        }
                    }
                });
                return true;
            }
            if (pDo.startsWith("/recaptcha/api2/replaceimage?")) {
                ensureBrowser();
                String rawPost = IO.readInputStreamToString(request.getInputStream());
                br.setKeepResponseContentBytes(true);
                final jd.http.requests.PostRequest r = br.createPostRequest("https://www.google.com" + pDo, new ArrayList<KeyValueStringEntry>(), null);
                r.setPostDataString(rawPost);
                r.getHeaders().put("Referer", ref);
                forward(request, response, br, r, new Modifier() {
                    public byte[] onData(byte[] bytes) {
                        String str;
                        try {
                            str = new String(bytes, "UTF-8");
                            str = replace(brRef, str);
                            return str.getBytes("UTF-8");
                        } catch (UnsupportedEncodingException e) {
                            throw new WTFException(e);
                        }
                    }
                });
                return true;
            }
            if (pDo.startsWith("/recaptcha/api2/userverify?")) {
                ensureBrowser();
                String rawPost = IO.readInputStreamToString(request.getInputStream());
                br.setKeepResponseContentBytes(true);
                final jd.http.requests.PostRequest r = br.createPostRequest("https://www.google.com" + pDo, new ArrayList<KeyValueStringEntry>(), null);
                System.out.println("Verify: " + rawPost);
                r.setPostDataString(rawPost);
                r.getHeaders().put("Referer", ref);
                forward(request, response, br, r, new Modifier() {
                    public byte[] onData(byte[] bytes) {
                        String str;
                        try {
                            str = new String(bytes, "UTF-8");
                            System.out.println(str);
                            str = str.replaceAll(Pattern.quote(getSiteDomain()) + "(:\\d+)?", request.getRequestHeaders().getValue("Host"));
                            str = replace(brRef, str);
                            return str.getBytes("UTF-8");
                        } catch (UnsupportedEncodingException e) {
                            throw new WTFException(e);
                        }
                    }
                });
                return true;
            }
            return false;
        }
    }

    private interface Modifier {
        public byte[] onData(byte[] bytes);
    }

    @Override
    public boolean onGetRequest(final BrowserReference brRef, final GetRequest request, final HttpResponse response) throws IOException, RemoteAPIException {
        synchronized (this) {
            String pDo = request.getParameterbyKey("do");
            final HTTPHeader referer = request.getRequestHeaders().get("Referer");
            String ref = "http://" + getSiteDomain();
            if (referer != null) {
                final String refOrg = referer.getValue();
                ref = UrlQuery.parse(refOrg).getDecoded("do");
                if (StringUtils.isEmpty(ref)) {
                    ref = "http://" + getSiteDomain();
                }
            }
            if ("/recaptcha/api.js".equals(pDo)) {
                ensureBrowser();
                String url = "https://www.google.com" + pDo;
                for (int i = 2; i < request.getRequestedURLParameters().size(); i++) {
                    url += (i == 2) ? "?" : "&";
                    url += request.getRequestedURLParameters().get(i).key + "=" + Encoding.urlEncode(request.getRequestedURLParameters().get(i).value);
                }
                jd.http.requests.GetRequest r = new jd.http.requests.GetRequest(url);
                r.getHeaders().put("Referer", ref);
                forward(request, response, br, r, new Modifier() {
                    public byte[] onData(byte[] bytes) {
                        String str;
                        try {
                            str = new String(bytes, "UTF-8");
                            if (!localhost) {
                                str = replace(brRef, str);
                            }
                            return str.getBytes("UTF-8");
                        } catch (UnsupportedEncodingException e) {
                            throw new WTFException(e);
                        }
                    }
                });
                return true;
            }
            if (pDo.startsWith("https://www.google.com/recaptcha/api2/frame?")) {
                ensureBrowser();
                jd.http.requests.GetRequest r = new jd.http.requests.GetRequest(pDo);
                r.getHeaders().put("Referer", ref);
                forward(request, response, br, r, new Modifier() {
                    public byte[] onData(byte[] bytes) {
                        String str;
                        try {
                            str = new String(bytes, "UTF-8");
                            str = str.replaceAll(Pattern.quote(getSiteDomain()) + "(:\\d+)?", request.getRequestHeaders().getValue("Host"));
                            str = replace(brRef, str);
                            return str.getBytes("UTF-8");
                        } catch (UnsupportedEncodingException e) {
                            throw new WTFException(e);
                        }
                    }
                });
                return true;
            }
            if (pDo.startsWith("https://www.google.com/recaptcha/api2/payload?")) {
                ensureBrowser();
                jd.http.requests.GetRequest r = new jd.http.requests.GetRequest(pDo);
                r.getHeaders().put("Referer", ref);
                forwardHeader("User-Agent", r, request);
                forwardHeader("Accept-Language", r, request);
                forwardHeader("User-Agent", r, request);
                r.setKeepByteArray(true);
                br.setKeepResponseContentBytes(true);
                br.getPage(r);
                byte[] bytes = r.getResponseBytes();
                saveTile(bytes, pDo);
                response.setResponseCode(ResponseCode.SUCCESS_OK);
                forwardHeader("Expires", r, response);
                forwardHeader("Date", r, response);
                forwardHeader("Cache-Control", r, response);
                forwardHeader("Content-Type", r, response);
                forwardHeader("Server", r, response);
                response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, bytes.length + ""));
                response.getOutputStream(true).write(bytes);
                return true;
            }
            if (pDo.startsWith("https://www.google.com/recaptcha/api2/anchor?")) {
                ensureBrowser();
                jd.http.requests.GetRequest r = new jd.http.requests.GetRequest(pDo);
                r.getHeaders().put("Referer", ref);
                forward(request, response, br, r, new Modifier() {
                    public byte[] onData(byte[] bytes) {
                        String str;
                        try {
                            str = new String(bytes, "UTF-8");
                            str = str.replaceAll(Pattern.quote(getSiteDomain()) + "(:\\d+)?", request.getRequestHeaders().getValue("Host"));
                            str = replace(brRef, str);
                            String anchorJS = IO.readURLToString(RecaptchaV2Challenge.class.getResource("recaptchaAnchor.js"));
                            str = str.replace("</script></body></html>", anchorJS + "</script></body></html>");
                            return str.getBytes("UTF-8");
                        } catch (Throwable e) {
                            throw new WTFException(e);
                        }
                    }
                });
                return true;
            }
            if (pDo.startsWith("https://www.google.com/recaptcha/api2/webworker.js?")) {
                ensureBrowser();
                jd.http.requests.GetRequest r = new jd.http.requests.GetRequest(pDo);
                r.getHeaders().put("Referer", ref);
                forward(request, response, br, r, new Modifier() {
                    public byte[] onData(byte[] bytes) {
                        String str;
                        try {
                            str = new String(bytes, "UTF-8");
                            str = replace(brRef, str);
                            return str.getBytes("UTF-8");
                        } catch (UnsupportedEncodingException e) {
                            throw new WTFException(e);
                        }
                    }
                });
                return true;
            }
            if (Pattern.compile("https?\\:\\/\\/www\\.gstatic\\.com/recaptcha/api2/r(\\d+)/recaptcha__([a-zA-Z_]+).js", Pattern.CASE_INSENSITIVE).matcher(pDo).matches()) {
                // String js = IO.readURLToString(RecaptchaV2Challenge.class.getResource("recaptcha.js"));
                // js = replace(brRef, js);
                // js = js.replaceAll("var ([a-zA-z0-9]+)\\s*=\\s*new ([a-zA-z0-9]+)\\(window\\.location\\)\\;", "function
                // LWindow(url){this.url=url;}LWindow.prototype.toString=function(){return this.url;};var $1= new $2(new LWindow(\"" +
                // "http://" + getSiteDomain() + "\"));");
                // js = js.replaceAll("\\.init\\s*=\\s*function\\(\\)\\s*\\{var ([a-zA-z0-9]+)=([^;]+);", ".init=function(){var $1=\"" +
                // brRef.getBase() + "\";");
                // response.setResponseCode(ResponseCode.SUCCESS_OK);
                // response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "application/javascript;
                // charset=utf-8"));
                // response.getOutputStream(true).write(js.getBytes("UTF-8"));
                // if (true) {
                // return true;
                // }
                ensureBrowser();
                jd.http.requests.GetRequest r = new jd.http.requests.GetRequest(pDo);
                r.getHeaders().put("Referer", ref);
                forward(request, response, br, r, new Modifier() {
                    public byte[] onData(byte[] bytes) {
                        String str;
                        try {
                            str = new String(bytes, "UTF-8");
                            str = modifyRecaptchaJS(brRef, str);
                            str = replace(brRef, str);
                            return str.getBytes("UTF-8");
                        } catch (UnsupportedEncodingException e) {
                            throw new WTFException(e);
                        }
                    }
                });
                return true;
            }
            if (pDo.startsWith("https://www.google.com/recaptcha/api2/bframe?")) {
                ensureBrowser();
                jd.http.requests.GetRequest r = new jd.http.requests.GetRequest(pDo);
                r.getHeaders().put("Referer", ref);
                forward(request, response, br, r, new Modifier() {
                    public byte[] onData(byte[] bytes) {
                        String str;
                        try {
                            str = new String(bytes, "UTF-8");
                            str = str.replaceAll(Pattern.quote(getSiteDomain()) + "(:\\d+)?", request.getRequestHeaders().getValue("Host"));
                            str = modifyRecaptchaJS(brRef, str);
                            str = replace(brRef, str);
                            return str.getBytes("UTF-8");
                        } catch (UnsupportedEncodingException e) {
                            throw new WTFException(e);
                        }
                    }
                });
                return true;
            }
            if ("solve".equals(pDo)) {
                String responsetoken = request.getParameterbyKey("response");
                brRef.onResponse(responsetoken);
                response.setResponseCode(ResponseCode.SUCCESS_OK);
                response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "text/html; charset=utf-8"));
                response.getOutputStream(true).write("Please Close the Browser now".getBytes("UTF-8"));
                return true;
            }
            return false;
        }
    }

    protected String modifyRecaptchaJS(BrowserReference brRef, String js2) {
        try {
            check("(([a-zA-Z0-9]+)=([a-zA-Z0-9]+)\\[3\\]==([a-zA-Z0-9]+)\\[3\\]&&([a-zA-Z0-9]+)\\[1\\]==([a-zA-Z0-9]+)\\[1\\]&&([a-zA-Z0-9]+)\\[4\\]==([a-zA-Z0-9]+)\\[4\\])", js2);
            check("(var ([a-zA-Z0-9]+)=[a-zA-Z0-9]+\\(\"anchor\"\\);)([a-zA-Z0-9]+\\.[a-zA-Z0-9]+\\.anchor=)", js2);
            check("([a-zA-Z0-9]+\\.prototype\\.send=function\\([a-zA-Z0-9]+,[a-zA-Z0-9]+,[a-zA-Z0-9]+\\)\\{([a-zA-Z0-9]+)=this\\.[a-zA-Z0-9]+\\[[a-zA-Z0-9]+\\];)", js2);
            check("var ([a-zA-z0-9]+)\\s*=\\s*new ([a-zA-z0-9]+)\\(window\\.location\\)\\;", js2);
            check("([a-zA-z0-9]+\\.prototype\\.toString=function\\(\\)\\{var [a-zA-z0-9]+=\\[\\]\\,[a-zA-z0-9]+=this.[a-zA-z0-9]+\\;.*[a-zA-z0-9]+\\.push.*?[a-zA-z0-9]+\\.push.*?[a-zA-z0-9]+\\.push.*?[a-zA-z0-9]+\\.push.*?[a-zA-z0-9]+\\.push.*?[a-zA-z0-9]+\\.push.*?[a-zA-z0-9]+\\.push.*?[a-zA-z0-9]+\\.push.*?[a-zA-z0-9]+\\.push.*?;)return ([a-zA-z0-9]+)\\.join\\(\"\"\\)\\}", js2);
            String js = js2.replaceAll("(([a-zA-Z0-9]+)=([a-zA-Z0-9]+)\\[3\\]==([a-zA-Z0-9]+)\\[3\\]&&([a-zA-Z0-9]+)\\[1\\]==([a-zA-Z0-9]+)\\[1\\]&&([a-zA-Z0-9]+)\\[4\\]==([a-zA-Z0-9]+)\\[4\\])", "$2=$3;$1");
            js = js.replaceAll("(var ([a-zA-Z0-9]+)=[a-zA-Z0-9]+\\(\"anchor\"\\);)([a-zA-Z0-9]+\\.[a-zA-Z0-9]+\\.anchor=)", "$1$2=\"%%%baseUrl%%%/proxy\";$3");
            js = js.replaceAll("([a-zA-Z0-9]+\\.prototype\\.send=function\\([a-zA-Z0-9]+,[a-zA-Z0-9]+,[a-zA-Z0-9]+\\)\\{([a-zA-Z0-9]+)=this\\.[a-zA-Z0-9]+\\[[a-zA-Z0-9]+\\];)", "$0$2.path=\"*\";");
            js = js.replaceAll("var ([a-zA-z0-9]+)\\s*=\\s*new ([a-zA-z0-9]+)\\(window\\.location\\)\\;", "function LWindow(url){this.url=url;}LWindow.prototype.toString=function(){return this.url;};var $1= new $2(new LWindow(\"" + "http://" + getSiteDomain() + "\"));");
            js = js.replaceAll("([a-zA-z0-9]+\\.prototype\\.toString=function\\(\\)\\{var [a-zA-z0-9]+=\\[\\]\\,[a-zA-z0-9]+=this.[a-zA-z0-9]+\\;.*[a-zA-z0-9]+\\.push.*?[a-zA-z0-9]+\\.push.*?[a-zA-z0-9]+\\.push.*?[a-zA-z0-9]+\\.push.*?[a-zA-z0-9]+\\.push.*?[a-zA-z0-9]+\\.push.*?[a-zA-z0-9]+\\.push.*?[a-zA-z0-9]+\\.push.*?[a-zA-z0-9]+\\.push.*?;)return ([a-zA-z0-9]+)\\.join\\(\"\"\\)\\}", "$1 return \"%%%baseUrl%%%/?id=%%%session%%%&do=\"+encodeURIComponent($2.join(\"\"));}");
            return js;
        } catch (Throwable e) {
            e.printStackTrace();
        }
        return null;
    }

    private void check(String string, String js2) {
        if (js2.replaceAll(string, "AFFENKOPF").equals(js2)) {
            System.out.println("DOES NOT MATCH: " + string);
        }
    }

    protected void saveTile(byte[] bytes, String url) throws IOException {
        int i = 0;
        File file = null;
        do {
            file = Application.getResource("tmp/rc2/" + (UrlQuery.parse(url).get("id") == null ? "grid" : "tile") + "/" + i + "_" + Hash.getMD5(bytes) + ".png");
            i++;
        } while (file.exists());
        file.getParentFile().mkdirs();
        IO.writeToFile(file, bytes);
        // if (i > 1) {
        // Dialog.getInstance().showMessageDialog("DUPE: " + file);
        // }
    }

    private Browser br;

    private void ensureBrowser() {
        if (this.br == null) {
            this.br = new Browser();
            BrowserSolverService.fillCookies(br);
        }
    }

    private void forward(org.appwork.utils.net.httpserver.requests.HttpRequest request, HttpResponse response, Browser br, jd.http.Request r, Modifier modifier) throws IOException {
        forwardHeader("User-Agent", r, request);
        forwardHeader("Accept-Language", r, request);
        forwardHeader("User-Agent", r, request);
        r.setKeepByteArray(true);
        br.setKeepResponseContentBytes(true);
        br.getPage(r);
        byte[] bytes = r.getResponseBytes();
        if (modifier != null) {
            bytes = modifier.onData(bytes);
        }
        response.setResponseCode(ResponseCode.SUCCESS_OK);
        forwardHeader("Expires", r, response);
        forwardHeader("Date", r, response);
        forwardHeader("Cache-Control", r, response);
        forwardHeader("Content-Type", r, response);
        forwardHeader("Server", r, response);
        response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, bytes.length + ""));
        response.getOutputStream(true).write(bytes);
    }

    private void forwardHeader(String header, jd.http.Request r, org.appwork.utils.net.httpserver.requests.HttpRequest request) {
        final String h = request.getRequestHeaders().getValue(header);
        if (StringUtils.isNotEmpty(h)) {
            r.getHeaders().put(new HTTPHeader(header, h));
        }
    }

    private void forwardHeader(String header, jd.http.Request r, HttpResponse response) {
        final String h = r.getResponseHeader(header);
        if (StringUtils.isNotEmpty(h)) {
            response.getResponseHeaders().add(new HTTPHeader(header, h));
        }
    }

    private String replace(BrowserReference browserReference, String js) {
        String regex;
        String actualUrl = new Regex(js, regex = "(https\\:\\/\\/www\\.gstatic\\.com\\/recaptcha\\/api2\\/r\\d+/recaptcha__[a-zA-Z]+\\.js)").getMatch(0);
        js = js.replaceAll(regex, "?id=" + browserReference.getId().getID() + "&do=" + Encoding.urlEncode(actualUrl));
        js = js.replace("%%%session%%%", String.valueOf(browserReference.getId().getID()));
        js = js.replace("%%%siteDomain%%%", getSiteDomain());
        js = js.replace("%%%baseUrl%%%", browserReference.getBase());
        try {
            js = js.replace("%%%baseHost%%%", new URL(browserReference.getBase()).getHost());
        } catch (MalformedURLException e) {
            e.printStackTrace();
        }
        js = js.replace("%%%basePort%%%", browserReference.getBasePort() + "");
        return js;
    }

    protected String getChallengeType() {
        String ret = null;
        Class<?> cls = getClass();
        while (cls != null && StringUtils.isEmpty(ret)) {
            ret = cls.getSimpleName();
            cls = cls.getSuperclass();
        }
        // must return RecaptchaV2Challenge
        return ret;
    }

    @Override
    public String getHTML(HttpRequest request, String id) {
        try {
            final String userAgent = request.getRequestHeaders().getValue(HTTPConstants.HEADER_REQUEST_USER_AGENT);
            final boolean isSafari = userAgent != null && userAgent.toLowerCase(Locale.ENGLISH).matches("(?s).*^((?!chrome|android|crios|fxios).)*safari.*");
            final boolean isEdge = userAgent != null && userAgent.toLowerCase(Locale.ENGLISH).matches(".*edge\\/.*");
            final URL url = RecaptchaV2Challenge.class.getResource("recaptcha.html");
            String html = IO.readURLToString(url);
            html = html.replace("%%%challengeType%%%", getChallengeType());
            html = html.replace("%%%headTitle%%%", _GUI.T.recaptchav2_head_title());
            html = html.replace("%%%headDescription%%%", _GUI.T.recaptchav2_head_description());
            html = html.replace("%%%captchaHeader%%%", _GUI.T.recaptchav2_header());
            html = html.replace("%%%unsupportedBrowserHeader%%%", _GUI.T.extension_unsupported_browser_header());
            html = html.replace("%%%unsupportedBrowserDescription%%%", _GUI.T.extension_unsupported_browser_description());
            html = html.replace("%%%helpHeader%%%", _GUI.T.extension_help_header());
            html = html.replace("%%%helpDescription%%%", _GUI.T.extension_help_description());
            html = html.replace("%%%helpDescriptionLinkTitle%%%", _GUI.T.extension_help_description_link_title());
            html = html.replace("%%%extensionSupportHeader%%%", _GUI.T.extension_support_header());
            html = html.replace("%%%extensionSupportDescription%%%", _GUI.T.extension_support_description());
            html = html.replace("%%%extensionSupportLinkTitle%%%", _GUI.T.extension_support_link_title());
            html = html.replace("%%%siteUrl%%%", StringUtils.valueOrEmpty(getSiteUrl()));
            html = html.replace("%%%siteDomain%%%", getSiteDomain());
            html = html.replace("%%%sitekey%%%", getSiteKey());
            html = html.replace("%%%sitekeyType%%%", getType());
            html = html.replace("%%%enterprise%%%", String.valueOf(isEnterprise()));
            final Map<String, Object> v3Action = getV3Action();
            if (v3Action == null) {
                html = html.replace("%%%v3action%%%", "");
            } else {
                // pixiv, hier falsche antwort
                String v3ActionString = new SimpleMapper().setPrettyPrintEnabled(false).objectToString(v3Action);
                v3ActionString = HTMLEntities.htmlDoubleQuotes(v3ActionString);
                html = html.replace("%%%v3action%%%", v3ActionString);
            }
            html = html.replace("%%%unsupportedBrowser%%%", (isSafari || isEdge) ? "block" : "none");
            if (true) {
                html = html.replace("%%%display%%%", "none");
                html = html.replace("%%%noExtensionHeader%%%", _GUI.T.extension_required_header());
                html = html.replace("%%%noExtensionDescription%%%", _GUI.T.extension_required_description());
                html = html.replace("%%%noExtensionLinkTitle%%%", _GUI.T.extension_required_link_title());
            } else {
                html = html.replace("%%%display%%%", "block");
                html = html.replace("%%%noExtensionHeader%%%", _GUI.T.extension_required_header());
                html = html.replace("%%%noExtensionDescription%%%", _GUI.T.extension_required_description());
                html = html.replace("%%%noExtensionLinkTitle%%%", _GUI.T.extension_required_link_title());
            }
            html = html.replace("%%%installExtensionHeader%%%", _GUI.T.install_extension_header());
            html = html.replace("%%%installExtensionDone%%%", _GUI.T.install_extension_done());
            html = html.replace("%%%installExtensionUsingOtherBrowser%%%", _GUI.T.install_extension_using_other_browser());
            html = html.replace("%%%installExtensionShowAllBrowsers%%%", _GUI.T.install_extension_show_all_browsers_link());
            html = html.replace("%%%chromeExtensionHeader%%%", _GUI.T.install_chrome_extension_header());
            html = html.replace("%%%chromeExtensionDescription%%%", _GUI.T.install_chrome_extension_description());
            html = html.replace("%%%chromeExtensionLinkTitle%%%", _GUI.T.install_chrome_extension_link_title());
            html = html.replace("%%%firefoxExtensionHeader%%%", _GUI.T.install_firefox_extension_header());
            html = html.replace("%%%firefoxExtensionDescription%%%", _GUI.T.install_firefox_extension_description());
            html = html.replace("%%%firefoxExtensionLinkTitle%%%", _GUI.T.install_firefox_extension_link_title());
            html = html.replace("%%%operaExtensionHeader%%%", _GUI.T.install_opera_extension_header());
            html = html.replace("%%%operaExtensionDescription%%%", _GUI.T.install_opera_extension_description());
            html = html.replace("%%%operaExtensionLinkTitle%%%", _GUI.T.install_opera_extension_link_title());
            html = html.replace("%%%findMoreAppsLinkTitle%%%", _GUI.T.find_more_apps_link_title());
            html = html.replace("%%%findMoreAppsDescription%%%", _GUI.T.find_more_apps_description());
            html = html.replace("%%%session%%%", id);
            html = html.replace("%%%challengeId%%%", Long.toString(getId().getID()));
            html = html.replace("%%%namespace%%%", getHttpPath());
            html = html.replace("%%%boundToDomain%%%", String.valueOf(true));
            html = html.replace("%%%sameOrigin%%%", String.valueOf(true));
            final String stoken = getSecureToken();
            if (StringUtils.isNotEmpty(stoken)) {
                html = html.replace("%%%sToken%%%", stoken);
                html = html.replace("%%%optionals%%%", "data-stoken=\"" + stoken + "\"");
            } else {
                html = html.replace("%%%sToken%%%", "");
                html = html.replace("%%%optionals%%%", "");
            }
            return html;
        } catch (IOException e) {
            throw new WTFException(e);
        }
    }

    @Override
    protected String getHttpPath() {
        if (plugin != null) {
            return "captcha/" + getCaptchaNameSpace() + "/" + getSiteDomain();
        } else {
            return super.getHttpPath();
        }
    }

    @Override
    protected String getCaptchaNameSpace() {
        if (getV3Action() != null || isV3()) {
            return "recaptchav3";
        } else {
            return "recaptchav2";
        }
    }

    @Override
    public boolean isCaptchaResponseValid() {
        if (super.isCaptchaResponseValid() && isValidResponseToken(getResult().getValue())) {
            return true;
        } else {
            return false;
        }
    }

    protected boolean isValidResponseToken(String v) {
        return v != null && v.matches("^[\\w-_]{30,}");
    }

    @Override
    public boolean validateResponse(AbstractResponse<String> response) {
        return super.validateResponse(response) && isValidResponseToken(response.getValue());
    }

    @Override
    public void onHandled() {
        super.onHandled();
        final BasicCaptchaChallenge basicChallenge = this.basicChallenge;
        if (basicChallenge != null) {
            basicChallenge.onHandled();
        }
    }

    public synchronized boolean hasBasicCaptchaChallenge() {
        return basicChallenge != null;
    }

    public synchronized BasicCaptchaChallenge createBasicCaptchaChallenge(final boolean showInstallDialog) {
        return basicChallenge;
    }

    public Map<String, Object> getV3Action() {
        return null;
    }
}
