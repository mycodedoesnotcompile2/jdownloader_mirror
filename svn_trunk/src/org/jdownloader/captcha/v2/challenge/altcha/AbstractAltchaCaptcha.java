package org.jdownloader.captcha.v2.challenge.altcha;

import java.io.IOException;
import java.util.Map;

import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.Base64;
import org.appwork.utils.logging2.LogInterface;
import org.jdownloader.captcha.v2.solver.browser.BrowserViewport;
import org.jdownloader.captcha.v2.solver.browser.BrowserWindow;
import org.jdownloader.logging.LogController;

/**
 * ALTCHA: https://altcha.org/ <br>
 * Widget: '<altcha-widget ...>', the challenge source is given via one of these attributes (depending on the widget version): <br>
 * - 'challengeurl' (URL) or 'challengejson' (JSON) <br>
 * - 'challenge' (URL or JSON) <br>
 * - 'configuration' (JSON object with the field "challenge" (URL, JSON string or object) or "challengeurl") <br>
 * The solution ("payload") is a base64 encoded JSON which is usually submitted via the form field 'altcha'. <br>
 * Not supported: Sentinel/Cloud "code challenges" (image/audio code which has to be entered after the proof of work) and server side
 * verification ('verifyurl') which replaces the payload by a server signature.
 */
public abstract class AbstractAltchaCaptcha<T extends Plugin> {
    /** Default name of the form field which holds the solution. */
    public static final String   FORM_FIELD_NAME = "altcha";
    protected final T            plugin;
    protected final LogInterface logger;
    protected final Browser      br;
    protected String             challengeUrl;
    protected String             challengeJson;

    public static boolean containsAltchaClass(final Browser br) {
        return br != null && containsAltchaClass(br.toString());
    }

    public static boolean containsAltchaClass(final String string) {
        if (string == null) {
            return false;
        } else if (new Regex(string, "<altcha-widget").patternFind()) {
            return true;
        } else if (new Regex(string, "(?i)/altcha(@[\\w.\\-]+)?(/dist[^\"']*)?/altcha(\\.umd|\\.min)?(\\.c?js)").patternFind()) {
            return true;
        } else {
            return false;
        }
    }

    public static boolean containsAltchaClass(final Form form) {
        return form != null && containsAltchaClass(form.getHtmlCode());
    }

    public AbstractAltchaCaptcha(final T plugin, final Browser br) {
        this(plugin, br, null, null);
    }

    /**
     * @param challengeUrl
     *            Absolute or relative URL of the challenge. If null, it will be auto detected from the html code.
     * @param challengeJson
     *            Challenge JSON. If null, it will be auto detected from the html code or fetched from the challenge URL.
     */
    public AbstractAltchaCaptcha(final T plugin, final Browser br, final String challengeUrl, final String challengeJson) {
        this.plugin = plugin;
        if (br.getRequest() == null) {
            throw new IllegalStateException("Browser.getRequest() == null!");
        } else {
            this.br = br;
        }
        if (plugin.getLogger() == null) {
            logger = LogController.getInstance().getLogger(getClass().getSimpleName());
        } else {
            logger = plugin.getLogger();
        }
        this.challengeUrl = challengeUrl;
        this.challengeJson = challengeJson;
    }

    public T getPlugin() {
        return plugin;
    }

    /** Returns the absolute URL of the challenge JSON or null if none was found (e.g. challenge JSON is embedded in the html code). */
    public String getChallengeUrl() {
        if (challengeUrl == null) {
            challengeUrl = findChallengeSource(br.getRequest().getHtmlCode(), true);
            if (challengeUrl == null) {
                logger.info("No challengeUrl found!");
                return null;
            }
        }
        if (!looksLikeValidChallengeUrl(challengeUrl)) {
            try {
                challengeUrl = br.getURL(challengeUrl).toString();
            } catch (final IOException e) {
                logger.log(e);
                return null;
            }
        }
        return challengeUrl;
    }

    /**
     * Returns the challenge JSON: Either embedded in the html code or fetched from the challenge URL. Returns null if it could not be
     * obtained (solvers will then fetch it themselves via challenge URL).
     */
    public String getChallengeJson() {
        if (challengeJson != null) {
            return challengeJson;
        }
        challengeJson = findChallengeSource(br.getRequest().getHtmlCode(), false);
        if (challengeJson != null) {
            return challengeJson;
        }
        final String challengeUrl = getChallengeUrl();
        if (challengeUrl == null) {
            return null;
        }
        try {
            final Browser brc = br.cloneBrowser();
            brc.setFollowRedirects(true);
            brc.getHeaders().put("Accept", "application/json");
            final String json = brc.getPage(challengeUrl);
            if (looksLikeValidChallengeJson(json)) {
                challengeJson = json;
            } else {
                /* E.g. Sentinel requesting HIS data -> Let the solver handle the challenge URL. */
                logger.info("Unexpected challenge JSON:" + json);
            }
        } catch (final IOException e) {
            logger.log(e);
        }
        return challengeJson;
    }

    /**
     * Searches the challenge source within the html code.
     *
     * @param wantUrl
     *            true = return challenge URL, false = return challenge JSON
     */
    protected String findChallengeSource(final String source, final boolean wantUrl) {
        if (source == null) {
            return null;
        }
        final String[] widgets = new Regex(source, "<altcha-widget[^>]*>").getColumn(-1);
        if (widgets == null) {
            return null;
        }
        for (final String widget : widgets) {
            for (final String attribute : new String[] { "challengeurl", "challengejson", "challenge" }) {
                final String value = getAttributeValue(widget, attribute);
                if (value != null && isWantedChallengeSource(value, wantUrl)) {
                    return value;
                }
            }
            final String configuration = getAttributeValue(widget, "configuration");
            if (configuration == null) {
                continue;
            }
            final Map<String, Object> config = JSonStorage.restoreFromString(configuration, TypeRef.MAP, null);
            if (config == null) {
                continue;
            }
            for (final String key : new String[] { "challengeurl", "challengeUrl", "challenge" }) {
                final Object value = config.get(key);
                if (value instanceof Map) {
                    if (!wantUrl) {
                        return JSonStorage.serializeToJson(value);
                    }
                } else if (value instanceof String && isWantedChallengeSource((String) value, wantUrl)) {
                    return (String) value;
                }
            }
        }
        return null;
    }

    private static boolean isWantedChallengeSource(final String value, final boolean wantUrl) {
        if (StringUtils.isEmpty(value)) {
            return false;
        } else if (wantUrl) {
            return !value.trim().startsWith("{");
        } else {
            return looksLikeValidChallengeJson(value);
        }
    }

    private static String getAttributeValue(final String tag, final String attribute) {
        String value = new Regex(tag, "\\s" + attribute + "\\s*=\\s*\"([^\"]*)\"").getMatch(0);
        if (value == null) {
            value = new Regex(tag, "\\s" + attribute + "\\s*=\\s*'([^']*)'").getMatch(0);
        }
        if (value == null) {
            return null;
        }
        return Encoding.htmlOnlyDecode(value).trim();
    }

    public static boolean looksLikeValidChallengeUrl(final String str) {
        return str != null && str.matches("(?i)^https?://[^\\s]+$");
    }

    public static boolean looksLikeValidChallengeJson(final String str) {
        if (str == null || !str.trim().startsWith("{")) {
            return false;
        }
        final Map<String, Object> challenge = JSonStorage.restoreFromString(str, TypeRef.MAP, null);
        if (challenge == null) {
            return false;
        } else if (challenge.get("parameters") instanceof Map) {
            /* v2 */
            return true;
        } else if (challenge.containsKey("challenge") && challenge.containsKey("salt")) {
            /* v1 */
            return true;
        } else {
            return false;
        }
    }

    /** Returns the parsed challenge if the given JSON is a v1 challenge, null otherwise. */
    public static Map<String, Object> parseChallengeV1(final String challengeJson) {
        if (!looksLikeValidChallengeJson(challengeJson)) {
            return null;
        }
        final Map<String, Object> challenge = JSonStorage.restoreFromString(challengeJson, TypeRef.MAP, null);
        if (challenge.containsKey("parameters")) {
            return null;
        }
        return challenge;
    }

    /** The solution (payload) is a base64 encoded JSON object. */
    public static boolean looksLikeValidToken(final String str) {
        if (StringUtils.isEmpty(str)) {
            return false;
        }
        try {
            final String decoded = Base64.decodeToString(str);
            return decoded != null && decoded.trim().startsWith("{");
        } catch (final RuntimeException e) {
            return false;
        }
    }

    /** URL of the page which contains the captcha. */
    protected String getSiteUrl() {
        /* Remove anchor */
        return br.getURL().replaceAll("(#.+)", "");
    }

    protected Browser getBrowser() {
        return br;
    }

    public LogInterface getLogger() {
        return logger;
    }

    protected AltchaChallenge createChallenge() throws PluginException {
        final T plugin = getPlugin();
        if (plugin == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String challengeJson = getChallengeJson();
        final String challengeUrl = getChallengeUrl();
        if (challengeJson == null && challengeUrl == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return new AltchaChallenge(plugin, challengeUrl, challengeJson) {
            @Override
            public String getSiteUrl() {
                return AbstractAltchaCaptcha.this.getSiteUrl();
            }

            @Override
            public BrowserViewport getBrowserViewport(BrowserWindow screenResource, java.awt.Rectangle elementBounds) {
                return null;
            }

            @Override
            public String getHTML(org.appwork.utils.net.httpserver.requests.HttpRequest request, String id) {
                return null;
            }
        };
    }

    public int getSolutionTimeout() {
        return 5 * 60 * 1000;
    }
}
