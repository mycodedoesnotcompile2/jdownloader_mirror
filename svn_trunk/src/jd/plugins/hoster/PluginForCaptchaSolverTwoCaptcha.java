package jd.plugins.hoster;

import java.util.ArrayList;
import java.util.List;

import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolverTwoCaptcha;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.plugins.HostPlugin;

/**
 * Plugin for 2Captcha captcha solving service (https://2captcha.com/).
 */
@HostPlugin(revision = "$Revision: 50808 $", interfaceVersion = 3, names = { "2captcha.com" }, urls = { "" })
public class PluginForCaptchaSolverTwoCaptcha extends abstractPluginForCaptchaSolverTwoCaptcha {
    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.CAPTCHA_SOLVER, LazyPlugin.FEATURE.BUBBLE_NOTIFICATION, LazyPlugin.FEATURE.API_KEY_LOGIN };
    }

    public PluginForCaptchaSolverTwoCaptcha(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public String getBuyPremiumUrl() {
        return "https://2captcha.com?from=15779444";
    }

    /**
     * Returns the list of captcha types supported by this solver.
     *
     * @return List of supported captcha types
     */
    @Override
    public List<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
        List<CAPTCHA_TYPE> types = new ArrayList<CAPTCHA_TYPE>();
        types.add(CAPTCHA_TYPE.IMAGE);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2_INVISIBLE);
        types.add(CAPTCHA_TYPE.HCAPTCHA);
        /* TODO: Add implementation for click captchas */
        types.add(CAPTCHA_TYPE.SINGLE_CLICK_CAPTCHA);
        types.add(CAPTCHA_TYPE.MULTI_CLICK_CAPTCHA);
        /* TODO: Add implementation for geetest captchas */
        types.add(CAPTCHA_TYPE.GEETEST_V1);
        types.add(CAPTCHA_TYPE.GEETEST_V4);
        return types;
    }

    protected String getApiBaseV2() {
        return "https://api." + getHost();
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/terms-of-service";
    }

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        if (str == null) {
            return false;
        }
        return str.matches("[a-f0-9]{32}");
    }

    @Override
    protected String getAPILoginHelpURL() {
        return "https://" + getHost() + "/enterpage";
    }
}