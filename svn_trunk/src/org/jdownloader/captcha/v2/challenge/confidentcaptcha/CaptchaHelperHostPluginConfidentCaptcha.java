package org.jdownloader.captcha.v2.challenge.confidentcaptcha;

import java.awt.Rectangle;

import jd.http.Browser;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

import org.jdownloader.captcha.v2.CaptchaHosterHelperInterface;
import org.jdownloader.captcha.v2.solver.browser.BrowserViewport;
import org.jdownloader.captcha.v2.solver.browser.BrowserWindow;

public class CaptchaHelperHostPluginConfidentCaptcha extends AbstractCaptchaHelperConfidentCaptcha<PluginForHost> implements CaptchaHosterHelperInterface {
    public CaptchaHelperHostPluginConfidentCaptcha(final PluginForHost plugin, final Browser br, final String siteKey) {
        super(plugin, br, siteKey);
    }

    public CaptchaHelperHostPluginConfidentCaptcha(final PluginForHost plugin, final Browser br) {
        this(plugin, br, null);
    }

    public String getToken() throws PluginException, InterruptedException {
        final PluginForHost plugin = this.getPlugin();
        final DownloadLink link = plugin.getDownloadLink();
        String sitekey = siteKey;
        if (sitekey == null) {
            sitekey = getConfidentCaptchaApiKey();
            if (sitekey == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "ConfidentCaptcha info can not be found");
            }
        }
        final ConfidentCaptchaChallenge challenge = new ConfidentCaptchaChallenge(plugin, sitekey) {
            @Override
            public BrowserViewport getBrowserViewport(BrowserWindow screenResource, Rectangle elementBounds) {
                return null;
            }
        };
        return plugin.handleCaptchaChallenge(link, challenge);
    }

    @Override
    public int getSolutionTimeout() {
        return -1;
    }
}
