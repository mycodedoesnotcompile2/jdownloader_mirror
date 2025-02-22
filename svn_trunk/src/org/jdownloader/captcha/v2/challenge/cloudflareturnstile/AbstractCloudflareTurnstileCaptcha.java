package org.jdownloader.captcha.v2.challenge.cloudflareturnstile;

import org.appwork.utils.Regex;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.net.httpserver.requests.HttpRequest;
import org.jdownloader.captcha.v2.solver.browser.BrowserViewport;
import org.jdownloader.captcha.v2.solver.browser.BrowserWindow;
import org.jdownloader.logging.LogController;

import jd.http.Browser;
import jd.parser.html.Form;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;

/** https://www.cloudflare.com/products/turnstile/ */
public abstract class AbstractCloudflareTurnstileCaptcha<T extends Plugin> {
    public static boolean containsCloudflareTurnstileClass(final Browser br) {
        return br != null && containsCloudflareTurnstileClass(br.toString());
    }

    public static boolean containsCloudflareTurnstileClass(final String string) {
        if (string == null) {
            return false;
        } else if (new Regex(string, "challenges\\.cloudflare\\.com/turnstile/").patternFind()) {
            return true;
        } else if (new Regex(string, "class=\"cf-turnstile\"").patternFind()) {
            return true;
        } else {
            return false;
        }
    }

    public static boolean containsCloudflareTurnstileClass(final Form form) {
        return form != null && containsCloudflareTurnstileClass(form.getHtmlCode());
    }

    protected final T            plugin;
    protected final LogInterface logger;
    protected final Browser      br;
    protected String             siteKey;

    public AbstractCloudflareTurnstileCaptcha(T plugin, Browser br) {
        this(plugin, br, null, null);
    }

    @Deprecated
    public AbstractCloudflareTurnstileCaptcha(T plugin, Browser br, String siteKey) {
        this(plugin, br, siteKey, null);
    }

    public AbstractCloudflareTurnstileCaptcha(final T plugin, final Browser br, final String siteKey, final String apiKey) {
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
        this.siteKey = siteKey;
    }

    public T getPlugin() {
        return plugin;
    }

    /** Also referred to as "misery key". */
    public String getSiteKey() {
        return getSiteKey(br.getRequest().getHtmlCode());
    }

    public String getSiteKey(final String source) {
        if (siteKey != null) {
            return siteKey;
        } else {
            /* Auto find sitekey */
            final String autoSiteKey = new Regex(source, "class=\"cf-turnstile\"[^>]*data-sitekey=\"([^\"]+)\"").getMatch(0);
            if (autoSiteKey != null) {
                this.siteKey = autoSiteKey;
                return this.siteKey;
            } else {
                logger.info("AutoSiteKey: No siteKey found!");
                return null;
            }
        }
    }

    /** Returns URL of the page where the captcha is displayed at. */
    public String getPageURL() {
        return br.getURL();
    }

    protected Browser getBrowser() {
        return br;
    }

    public LogInterface getLogger() {
        return logger;
    }

    protected CloudflareTurnstileChallenge createChallenge() throws PluginException {
        final T plugin = getPlugin();
        final String siteKey = getSiteKey();
        if (plugin == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else if (siteKey == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else {
            return new CloudflareTurnstileChallenge(plugin, siteKey) {
                @Override
                public BrowserViewport getBrowserViewport(BrowserWindow screenResource, java.awt.Rectangle elementBounds) {
                    return null;
                }

                @Override
                public String getHTML(HttpRequest request, String id) {
                    return null;
                }
            };
        }
    }

    public int getSolutionTimeout() {
        return 5 * 60 * 1000;
    }
}
