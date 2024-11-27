package org.jdownloader.captcha.v2.challenge.geetest;

import java.awt.Rectangle;

import jd.http.Browser;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

import org.jdownloader.captcha.v2.CaptchaHosterHelperInterface;
import org.jdownloader.captcha.v2.solver.browser.BrowserViewport;
import org.jdownloader.captcha.v2.solver.browser.BrowserWindow;

public class CaptchaHelperHostPluginGeeTest extends AbstractCaptchaHelperGeeTest<PluginForHost> implements CaptchaHosterHelperInterface {
    public CaptchaHelperHostPluginGeeTest(PluginForHost plugin, Browser br, String siteKey) {
        super(plugin, br, siteKey);
    }

    public CaptchaHelperHostPluginGeeTest(PluginForHost plugin, Browser br) {
        this(plugin, br, null);
    }

    public String getToken() throws PluginException, InterruptedException {
        String apiKey = siteKey;
        if (apiKey == null) {
            apiKey = getGeeTestApiKey();
            if (apiKey == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "GeeTest API Key can not be found");
            }
        }
        final PluginForHost plugin = getPlugin();
        final DownloadLink link = plugin.getDownloadLink();
        final GeeTestChallenge challenge = new GeeTestChallenge(apiKey, plugin) {
            @Override
            public BrowserViewport getBrowserViewport(BrowserWindow screenResource, Rectangle elementBounds) {
                return null;
            }
        };
        return plugin.handleCaptchaChallenge(link, challenge);
    }
}
