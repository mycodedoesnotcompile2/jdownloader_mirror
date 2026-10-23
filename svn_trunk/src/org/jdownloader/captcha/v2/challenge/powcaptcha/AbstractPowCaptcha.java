package org.jdownloader.captcha.v2.challenge.powcaptcha;

import java.io.IOException;
import java.util.Map;

import jd.http.Browser;
import jd.parser.html.Form;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.logging2.LogInterface;
import org.jdownloader.captcha.v2.solver.browser.BrowserViewport;
import org.jdownloader.captcha.v2.solver.browser.BrowserWindow;
import org.jdownloader.logging.LogController;

/** Proof-of-work captcha used by filecrypt.cc (widget: '/js/pow_captcha.js', container: class 'pow-captcha'). */
public abstract class AbstractPowCaptcha<T extends Plugin> {
    protected final T            plugin;
    protected final LogInterface logger;
    protected final Browser      br;
    protected String             sessionUrl;
    protected String             proxyUrl = null;

    /**
     * Sets a proxy (e.g. 'socks5://user:password@host:port') which may be sent to the captcha solver service. Null = no proxy.
     */
    public void setProxyUrl(final String proxyUrl) {
        this.proxyUrl = proxyUrl;
    }

    public static boolean containsPowCaptchaClass(final Browser br) {
        return br != null && containsPowCaptchaClass(br.toString());
    }

    public static boolean containsPowCaptchaClass(final String string) {
        if (string == null) {
            return false;
        } else if (new Regex(string, "class=\"pow-captcha\"").patternFind()) {
            return true;
        } else if (new Regex(string, "/js/pow_captcha\\.js").patternFind() && new Regex(string, "name=\"pow_id\"").patternFind()) {
            return true;
        } else {
            return false;
        }
    }

    public static boolean containsPowCaptchaClass(final Form form) {
        return form != null && containsPowCaptchaClass(form.getHtmlCode());
    }

    public AbstractPowCaptcha(final T plugin, final Browser br) {
        this(plugin, br, null);
    }

    public AbstractPowCaptcha(final T plugin, final Browser br, final String sessionUrl) {
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
        this.sessionUrl = sessionUrl;
    }

    public T getPlugin() {
        return plugin;
    }

    /** Returns the absolute URL of the captcha session JSON. */
    public String getSessionUrl() {
        return getSessionUrl(br.getRequest().getHtmlCode());
    }

    protected String getSessionUrl(final String source) {
        if (sessionUrl != null) {
            return sessionUrl;
        } else if (source == null) {
            return null;
        }
        /* E.g. data-session="/captchasession/08726E6040.json" */
        final String path = new Regex(source, "data-session\\s*=\\s*(?:\"|')\\s*([^\"']+?)\\s*(?:\"|')").getMatch(0);
        if (path == null) {
            logger.info("No sessionUrl found!");
            return null;
        }
        try {
            sessionUrl = br.getURL(path).toString();
        } catch (final IOException e) {
            logger.log(e);
            return null;
        }
        return sessionUrl;
    }

    public static boolean looksLikeValidSessionUrl(final String str) {
        return str != null && str.matches("(?i)^https?://[^\\s]+$");
    }

    public static boolean looksLikeValidToken(final String str) {
        /* Valid solution: JSON object which contains at least the fields 'pow_id' and 'pow_data'. */
        if (StringUtils.isEmpty(str)) {
            return false;
        }
        final Map<String, Object> entries;
        try {
            entries = JSonStorage.restoreFromString(str, TypeRef.MAP);
        } catch (final Exception e) {
            return false;
        }
        return entries != null && entries.containsKey("pow_id") && entries.containsKey("pow_data");
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

    protected PowCaptchaChallenge createChallenge() throws PluginException {
        final T plugin = getPlugin();
        final String sessionUrl = getSessionUrl();
        if (plugin == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else if (sessionUrl == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else {
            final PowCaptchaChallenge challenge = new PowCaptchaChallenge(plugin, sessionUrl, br.getHeaders().get("User-Agent")) {
                @Override
                public String getSiteUrl() {
                    return AbstractPowCaptcha.this.getSiteUrl();
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
            challenge.setProxyUrl(proxyUrl);
            return challenge;
        }
    }

    public int getSolutionTimeout() {
        return 5 * 60 * 1000;
    }
}
