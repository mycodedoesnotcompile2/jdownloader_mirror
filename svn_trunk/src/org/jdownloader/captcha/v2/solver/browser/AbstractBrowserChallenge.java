package org.jdownloader.captcha.v2.solver.browser;

import java.awt.Rectangle;
import java.io.IOException;

import jd.controlling.captcha.CaptchaSettings;
import jd.controlling.captcha.CaptchaSettings.INTERACTIVE_CAPTCHA_PRIVACY_LEVEL;
import jd.http.Browser;
import jd.plugins.Plugin;
import jd.plugins.PluginForDecrypt;
import jd.plugins.PluginForHost;

import org.appwork.remoteapi.exceptions.RemoteAPIException;
import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.Regex;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.net.httpserver.requests.GetRequest;
import org.appwork.utils.net.httpserver.requests.HttpRequest;
import org.appwork.utils.net.httpserver.requests.PostRequest;
import org.appwork.utils.net.httpserver.responses.HttpResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.solver.CESChallengeSolver;
import org.jdownloader.logging.LogController;
import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolver;

public abstract class AbstractBrowserChallenge extends Challenge<String> {
    protected final Plugin  plugin;
    protected final Browser pluginBrowser;

    public static interface AbstractBrowserChallengeAPIStorable {

        public String getSiteUrl();

        public String getType();

        public String getSiteKey();

        public String getContextUrl();
    }

    public Plugin getPlugin() {
        return plugin;
    }

    public Browser getPluginBrowser() {
        return pluginBrowser;
    }

    protected abstract String getSiteUrl();

    public String getSiteUrl(Object requestor) {
        final class CleanupUrl {
            /**
             * Wrapper function to be used when an interactive captcha challenge requests its siteURL. <br>
             * This function is to be used before such URLs are used anywhere in case they need to be modified for example due to the users'
             * privacy settings.
             */
            private String cleanup(String url) {
                if (INTERACTIVE_CAPTCHA_PRIVACY_LEVEL.STRICT.equals(JsonConfig.create(CaptchaSettings.class).getInteractiveCaptchaPrivacyLevel())) {
                    final String baseurl = new Regex(url, "(?i)(https?://[^/]+/?)").getMatch(0);
                    if (baseurl != null) {
                        url = baseurl;
                    }
                }
                return url;
            }
        }
        final String url = getSiteUrl();
        if (requestor instanceof CESChallengeSolver) {
            return new CleanupUrl().cleanup(url);
        } else if (requestor instanceof abstractPluginForCaptchaSolver) {
            return new CleanupUrl().cleanup(url);
        } else {
            return url;
        }
    }

    protected AbstractBrowserChallenge(final String method, final Plugin plugin, Browser pluginBrowser) {
        super(method, null);
        this.plugin = plugin;
        this.pluginBrowser = pluginBrowser;
    }

    public AbstractBrowserChallenge(final String method, final Plugin plugin) {
        super(method, null);
        if (plugin == null) {
            this.plugin = Plugin.getCurrentActivePlugin();
        } else {
            this.plugin = plugin;
        }
        if (this.plugin instanceof PluginForHost) {
            this.pluginBrowser = ((PluginForHost) this.plugin).getBrowser();
        } else if (this.plugin instanceof PluginForDecrypt) {
            this.pluginBrowser = ((PluginForDecrypt) this.plugin).getBrowser();
        } else {
            this.pluginBrowser = null;
        }
    }

    protected LogInterface getLogger() {
        LogInterface ret = null;
        if (plugin != null) {
            ret = plugin.getLogger();
            if (ret == null) {
                ret = LogController.CL();
            }
        }
        return ret;
    }

    abstract public String getHTML(HttpRequest request, String id);

    abstract public BrowserViewport getBrowserViewport(BrowserWindow screenResource, Rectangle elementBounds);

    public boolean onGetRequest(BrowserReference browserReference, GetRequest request, HttpResponse response) throws IOException, RemoteAPIException {
        return false;
    }

    public boolean onPostRequest(BrowserReference browserReference, PostRequest request, HttpResponse response) throws IOException, RemoteAPIException {
        return false;
    }

    public boolean onRawPostRequest(final BrowserReference browserRefefence, final PostRequest request, final HttpResponse response) throws IOException, RemoteAPIException {
        return false;
    }

    public boolean onRawGetRequest(final BrowserReference browserReference, final GetRequest request, final HttpResponse response) throws IOException, RemoteAPIException {
        return false;
    }

    abstract protected String getCaptchaNameSpace();

    protected String getHttpPath() {
        if (plugin != null) {
            return "captcha/" + getCaptchaNameSpace() + "/" + plugin.getHost();
        } else {
            return "captcha/" + getCaptchaNameSpace() + "/jd";
        }
    }
}
