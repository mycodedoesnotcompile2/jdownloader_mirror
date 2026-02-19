package org.jdownloader.captcha.v2.challenge.cloudflareturnstile;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Pattern;

import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
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
    private final static Pattern PATTERN_VALID_RESPONSE_TOKEN = Pattern.compile("^0\\.[a-zA-Z0-9_\\-\\.]{60,}$");

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

    public final static String apiKeyRegex = "\\d+x[a-zA-Z0-9\\-_]{22}";

    public final static boolean isValidSiteKey(final String siteKey) {
        if (siteKey == null) {
            return false;
        } else if (siteKey.matches("^" + apiKeyRegex + "$")) {
            return true;
        } else {
            return false;
        }
    }

    protected static final HashMap<String, Set<String>> INVALID_SITE_KEYS = new HashMap<String, Set<String>>();

    protected String getSiteKey(final String source) {
        if (siteKey != null) {
            return siteKey;
        } else if (source == null) {
            return null;
        }
        String findNextSiteKeySource = source;
        final HashSet<String> siteKeys = new HashSet<String>();
        while (StringUtils.isNotEmpty(findNextSiteKeySource)) {
            final String siteKey = findNextSiteKey(findNextSiteKeySource);
            if (siteKey != null) {
                siteKeys.add(siteKey);
                findNextSiteKeySource = findNextSiteKeySource.replace(siteKey, "");
            } else {
                break;
            }
        }
        synchronized (INVALID_SITE_KEYS) {
            logger.info("Auto siteKeys unfiltered:" + siteKeys);
            final Set<String> invalidSiteKeys = INVALID_SITE_KEYS.get(getPlugin().getHost());
            if (invalidSiteKeys != null) {
                siteKeys.removeAll(invalidSiteKeys);
            }
            logger.info("Auto siteKeys filtered:" + siteKeys);
        }
        siteKey = findCorrectSiteKeys(source, br, siteKeys);
        return siteKey;
    }

    protected String findCorrectSiteKeys(final String source, final Browser br, Set<String> siteKeys) {
        if (siteKeys.size() == 0) {
            logger.info("No siteKey found!");
            return null;
        } else if (siteKeys.size() == 1) {
            final String siteKey = siteKeys.iterator().next();
            logger.info("Auto single siteKey:" + siteKey);
            return siteKey;
        } else {
            final String siteKey = siteKeys.iterator().next();
            logger.info("Auto multiple siteKeys:" + siteKeys + " -> " + siteKey);
            return siteKey;
        }
    }

    protected String[] getDIVs(String source) {
        return new Regex(source, "<\\s*(div|button)(?:[^>]*>.*?</\\1>|[^>]*\\s*/\\s*>)").getColumn(-1);
    }

    protected String findNextSiteKey(String source) {
        {
            // json values in script or json
            // with container, turnstile.render(container,parameters)
            String jsSource = new Regex(source, "turnstile\\.render\\s*\\(.*?,\\s*\\{(.*?)\\s*\\}\\s*\\)\\s*;").getMatch(0);
            String siteKey = new Regex(jsSource, "('|\"|)sitekey\\1\\s*:\\s*('|\"|)\\s*(" + apiKeyRegex + ")\\s*\\2").getMatch(2);
            if (siteKey != null) {
                return siteKey;
            }
            // without, turnstile.render(parameters)
            jsSource = new Regex(source, "turnstile\\.render\\s*\\(\\s*\\{(.*?)\\s*\\}\\s*\\)\\s*;").getMatch(0);
            siteKey = new Regex(jsSource, "('|\"|)sitekey\\1\\s*:\\s*('|\"|)\\s*(" + apiKeyRegex + ")\\s*\\2").getMatch(2);
            if (siteKey != null) {
                return siteKey;
            }
        }
        {
            // turnstile.execute(apiKey)
            final String siteKey = new Regex(source, "turnstile\\.execute\\s*\\(\\s*('|\")\\s*(" + apiKeyRegex + ")\\s*\\1").getMatch(1);
            if (siteKey != null) {
                return siteKey;
            }
        }
        {
            // within form
            final Form forms[] = Form.getForms(source);
            if (forms != null) {
                for (final Form form : forms) {
                    final String siteKey = new Regex(form.getHtmlCode(), "data-sitekey\\s*=\\s*('|\")?\\s*(" + apiKeyRegex + ")").getMatch(1);
                    if (siteKey != null) {
                        return siteKey;
                    }
                }
            }
        }
        return null;
    }

    public static boolean looksLikeValidToken(final String str) {
        /* E.g. 0.zTSnTXO0X0XwSjSCU8oyzbjEtD8p.d62306d4ee00c00dda690f959ebbd0bd90 */
        if (str == null) {
            return false;
        }
        return new Regex(str, PATTERN_VALID_RESPONSE_TOKEN).matches();
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
