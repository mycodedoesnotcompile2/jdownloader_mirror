package org.jdownloader.captcha.v2.challenge.cutcaptcha;

import java.util.regex.Pattern;

import jd.controlling.linkcrawler.CrawledLink;
import jd.http.Browser;
import jd.http.Request;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.PluginForHost;

import org.appwork.utils.StringUtils;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.net.httpconnection.HTTPConnection.RequestMethod;
import org.jdownloader.captcha.v2.solver.browser.BrowserViewport;
import org.jdownloader.captcha.v2.solver.browser.BrowserWindow;
import org.jdownloader.logging.LogController;

public abstract class AbstractCaptchaHelperCutCaptcha<T extends Plugin> {
    protected final T            plugin;
    protected final LogInterface logger;
    protected final Browser      br;
    protected String             siteKey;
    protected String             apiKey;
    protected final String       siteDomain;

    public AbstractCaptchaHelperCutCaptcha(T plugin, Browser br) {
        this(plugin, br, null, null);
    }

    @Deprecated
    public AbstractCaptchaHelperCutCaptcha(T plugin, Browser br, String siteKey) {
        this(plugin, br, siteKey, null);
    }

    public AbstractCaptchaHelperCutCaptcha(final T plugin, final Browser br, final String siteKey, final String apiKey) {
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
        this.apiKey = apiKey;
        this.siteDomain = Browser.getHost(br.getURL(), true);
    }

    public String getSiteDomain() {
        return siteDomain;
    }

    public T getPlugin() {
        return plugin;
    }

    /** Also referred to as "misery key". */
    public String getSiteKey() {
        return getSiteKey(br.getRequest().getHtmlCode());
    }

    /** Also referred to as "misery key". */
    public String getSiteKey(final String source) {
        if (siteKey != null) {
            return siteKey;
        } else {
            /* Auto find sitekey */
            final String autoSiteKey = new Regex(source, "CUTCAPTCHA_MISERY_KEY = \"([a-f0-9]{40})").getMatch(0);
            if (autoSiteKey != null) {
                this.siteKey = autoSiteKey;
                return this.siteKey;
            } else {
                logger.info("AutoSiteKey: No siteKey found!");
                return null;
            }
        }
    }

    public String getAPIKey() {
        return getAPIKey(br.getRequest().getHtmlCode());
    }

    public String getAPIKey(final String source) {
        if (apiKey != null) {
            return apiKey;
        } else {
            /* Auto find sitekey */
            final String autoApiKey = new Regex(source, "cutcaptcha\\.net/captcha/([A-Za-z0-9]+)\\.js").getMatch(0);
            if (autoApiKey != null) {
                this.apiKey = autoApiKey;
                return this.apiKey;
            } else {
                logger.info("AutoApiKey: No apiKey found!");
                return null;
            }
        }
    }

    protected String getSiteUrl() {
        final String siteDomain = getSiteDomain();
        String url = null;
        final Request request = br != null ? br.getRequest() : null;
        final boolean canUseRequestURL = request != null && request.getHttpConnection() != null && RequestMethod.GET.equals(request.getRequestMethod()) && StringUtils.containsIgnoreCase(request.getHttpConnection().getContentType(), "html");
        boolean rewriteHost = true;
        String defaultProtocol = "http://";
        if (plugin != null) {
            if (plugin.getMatcher().pattern().pattern().matches(".*(https?).*")) {
                defaultProtocol = "https://";
            }
            if (plugin instanceof PluginForHost) {
                final DownloadLink downloadLink = ((PluginForHost) plugin).getDownloadLink();
                if (downloadLink != null) {
                    url = downloadLink.getPluginPatternMatcher();
                }
            } else if (plugin instanceof PluginForDecrypt) {
                final CrawledLink crawledLink = ((PluginForDecrypt) plugin).getCurrentLink();
                if (crawledLink != null) {
                    url = crawledLink.getURL();
                }
            }
            if (url != null && request != null) {
                final String referer = request.getHeaders().getValue("Referer");
                if (referer != null && plugin.canHandle(referer) && canUseRequestURL) {
                    rewriteHost = false;
                    url = request.getUrl();
                } else {
                    url = url.replaceAll("^(?i)(https?://)", request.getURL().getProtocol() + "://");
                }
            }
            if (StringUtils.equals(url, siteDomain) || StringUtils.equals(url, plugin.getHost())) {
                if (request != null) {
                    url = request.getURL().getProtocol() + "://" + url;
                } else {
                    url = defaultProtocol + url;
                }
            }
        }
        if (url == null && request != null && canUseRequestURL) {
            url = request.getUrl();
        }
        if (url != null) {
            // remove anchor
            url = url.replaceAll("(#.+)", "");
            final String urlDomain = Browser.getHost(url, true);
            if (rewriteHost && !StringUtils.equalsIgnoreCase(urlDomain, siteDomain)) {
                url = url.replaceFirst(Pattern.quote(urlDomain), siteDomain);
            }
            return url;
        } else {
            if (request != null) {
                return request.getURL().getProtocol() + "://" + siteDomain;
            } else {
                return defaultProtocol + siteDomain;
            }
        }
    }

    protected Browser getBrowser() {
        return br;
    }

    public LogInterface getLogger() {
        return logger;
    }

    protected CutCaptchaChallenge createChallenge() throws PluginException {
        final T plugin = getPlugin();
        final String siteKey = getSiteKey();
        final String apiKey = getAPIKey();
        if (plugin == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else if (siteKey == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else if (apiKey == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else {
            return new CutCaptchaChallenge(plugin, siteKey, apiKey) {

                @Override
                public String getSiteUrl() {
                    return AbstractCaptchaHelperCutCaptcha.this.getSiteUrl();
                }

                @Override
                public BrowserViewport getBrowserViewport(BrowserWindow screenResource, java.awt.Rectangle elementBounds) {
                    return null;
                }
            };
        }
    }
}
