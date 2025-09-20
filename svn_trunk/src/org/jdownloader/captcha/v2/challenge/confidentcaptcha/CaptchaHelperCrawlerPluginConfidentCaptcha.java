package org.jdownloader.captcha.v2.challenge.confidentcaptcha;

import java.awt.Rectangle;

import jd.http.Browser;
import jd.plugins.DecrypterException;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

import org.jdownloader.captcha.v2.CaptchaCrawlerHelperInterface;
import org.jdownloader.captcha.v2.solver.browser.BrowserViewport;
import org.jdownloader.captcha.v2.solver.browser.BrowserWindow;

public class CaptchaHelperCrawlerPluginConfidentCaptcha extends AbstractCaptchaHelperConfidentCaptcha<PluginForDecrypt> implements CaptchaCrawlerHelperInterface {
    public CaptchaHelperCrawlerPluginConfidentCaptcha(final PluginForDecrypt plugin, final Browser br, final String siteKey) {
        super(plugin, br, siteKey);
    }

    public CaptchaHelperCrawlerPluginConfidentCaptcha(final PluginForDecrypt plugin, final Browser br) {
        this(plugin, br, null);
    }

    public String getToken() throws PluginException, InterruptedException, DecrypterException {
        String sitekey = siteKey;
        if (sitekey == null) {
            sitekey = getConfidentCaptchaApiKey();
            if (sitekey == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "ConfidentCaptcha info can not be found");
            }
        }
        final PluginForDecrypt plugin = getPlugin();
        final ConfidentCaptchaChallenge c = new ConfidentCaptchaChallenge(plugin, sitekey) {
            @Override
            public BrowserViewport getBrowserViewport(BrowserWindow screenResource, Rectangle elementBounds) {
                return null;
            }
        };
        return plugin.handleCaptchaChallenge(c);
    }

    @Override
    public int getSolutionTimeout() {
        return -1;
    }
}
