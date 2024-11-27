package org.jdownloader.captcha.v2.challenge.sweetcaptcha;

import java.awt.Rectangle;

import jd.http.Browser;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

import org.jdownloader.captcha.v2.CaptchaHosterHelperInterface;
import org.jdownloader.captcha.v2.solver.browser.BrowserViewport;
import org.jdownloader.captcha.v2.solver.browser.BrowserWindow;

public class CaptchaHelperHostPluginSweetCaptcha extends AbstractCaptchaHelperSweetCaptcha<PluginForHost> implements CaptchaHosterHelperInterface {
    public CaptchaHelperHostPluginSweetCaptcha(final PluginForHost plugin, final Browser br, final String siteKey, final String appKey) {
        super(plugin, br, siteKey, appKey);
    }

    public CaptchaHelperHostPluginSweetCaptcha(final PluginForHost plugin, final Browser br) {
        this(plugin, br, null, null);
    }

    public String getToken() throws PluginException, InterruptedException {
        final PluginForHost plugin = this.getPlugin();
        final DownloadLink link = plugin.getDownloadLink();
        String appkey = appKey;
        if (appkey == null) {
            appkey = getSweetCaptchaAppKey();
            if (appkey == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "SweetCaptcha AppKey can not be found");
            }
        }
        String sitekey = siteKey;
        if (sitekey == null) {
            sitekey = getSweetCaptchaApiKey();
            if (sitekey == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "SweetCaptcha SiteKey can not be found");
            }
        }
        final SweetCaptchaChallenge challenge = new SweetCaptchaChallenge(sitekey, appkey, plugin) {
            @Override
            public BrowserViewport getBrowserViewport(BrowserWindow screenResource, Rectangle elementBounds) {
                return null;
            }
        };
        return plugin.handleCaptchaChallenge(link, challenge);
    }

}
