package org.jdownloader.captcha.v2.challenge.sweetcaptcha;

import java.awt.Rectangle;

import jd.http.Browser;
import jd.plugins.DecrypterException;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

import org.jdownloader.captcha.v2.CaptchaCrawlerHelperInterface;
import org.jdownloader.captcha.v2.solver.browser.BrowserViewport;
import org.jdownloader.captcha.v2.solver.browser.BrowserWindow;

public class CaptchaHelperCrawlerPluginSweetCaptcha extends AbstractCaptchaHelperSweetCaptcha<PluginForDecrypt> implements CaptchaCrawlerHelperInterface {
    public CaptchaHelperCrawlerPluginSweetCaptcha(final PluginForDecrypt plugin, final Browser br, final String siteKey, final String apiKey) {
        super(plugin, br, siteKey, apiKey);
    }

    public CaptchaHelperCrawlerPluginSweetCaptcha(final PluginForDecrypt plugin, final Browser br) {
        this(plugin, br, null, null);
    }

    public String getToken() throws PluginException, InterruptedException, DecrypterException {
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
        final PluginForDecrypt plugin = getPlugin();
        SweetCaptchaChallenge c = new SweetCaptchaChallenge(sitekey, appkey, plugin) {
            @Override
            public BrowserViewport getBrowserViewport(BrowserWindow screenResource, Rectangle elementBounds) {
                return null;
            }
        };
        return plugin.handleCaptchaChallenge(c);
    }

}
