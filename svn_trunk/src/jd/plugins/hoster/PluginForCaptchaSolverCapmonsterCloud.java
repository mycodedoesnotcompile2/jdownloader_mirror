package jd.plugins.hoster;

import java.util.ArrayList;
import java.util.List;

import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolverTwoCaptchaAPIV2;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfigCapmonster;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.HostPlugin;

@HostPlugin(revision = "$Revision: 53490 $", interfaceVersion = 3, names = { "capmonster.cloud" }, urls = { "" })
public class PluginForCaptchaSolverCapmonsterCloud extends abstractPluginForCaptchaSolverTwoCaptchaAPIV2 {
    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.CAPTCHA_SOLVER, LazyPlugin.FEATURE.BUBBLE_NOTIFICATION, LazyPlugin.FEATURE.API_KEY_LOGIN };
    }

    public PluginForCaptchaSolverCapmonsterCloud(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public String getBuyPremiumUrl() {
        return "https://" + getHost() + "/#new-plans";
    }

    @Override
    public List<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
        final List<CAPTCHA_TYPE> types = new ArrayList<CAPTCHA_TYPE>();
        types.add(CAPTCHA_TYPE.IMAGE);
        types.add(CAPTCHA_TYPE.IMAGE_SINGLE_CLICK_CAPTCHA);
        types.add(CAPTCHA_TYPE.IMAGE_MULTI_CLICK_CAPTCHA);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2_ENTERPRISE);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2_INVISIBLE);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V3);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V3_ENTERPRISE);
        types.add(CAPTCHA_TYPE.CLOUDFLARE_TURNSTILE);
        types.add(CAPTCHA_TYPE.MT_CAPTCHA);
        return types;
    }

    protected String getApiBase() {
        return "https://api." + getHost();
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/en/terms-of-service/";
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
        return "https://dash.capmonster.cloud/";
    }

    @Override
    public Class<? extends CaptchaSolverPluginConfigCapmonster> getConfigInterface() {
        return CaptchaSolverPluginConfigCapmonster.class;
    }
}