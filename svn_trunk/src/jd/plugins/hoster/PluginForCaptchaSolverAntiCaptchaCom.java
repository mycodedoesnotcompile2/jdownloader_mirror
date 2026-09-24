package jd.plugins.hoster;

import java.util.ArrayList;
import java.util.List;

import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolverTwoCaptchaAPIV2;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfigAntiCaptchaCom;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.HostPlugin;

@HostPlugin(revision = "$Revision: 53503 $", interfaceVersion = 3, names = { "anti-captcha.com" }, urls = { "" })
public class PluginForCaptchaSolverAntiCaptchaCom extends abstractPluginForCaptchaSolverTwoCaptchaAPIV2 {
    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.CAPTCHA_SOLVER, LazyPlugin.FEATURE.BUBBLE_NOTIFICATION, LazyPlugin.FEATURE.API_KEY_LOGIN };
    }

    public PluginForCaptchaSolverAntiCaptchaCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public String getBuyPremiumUrl() {
        return "https://getcaptchasolution.com/pue5rd7req";
    }

    @Override
    public List<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
        /*
         * List of task types anti-captcha.com's API offers: https://anti-captcha.com/apidoc (confirmed 2026-09-24). Only types with a
         * matching solve() branch in abstractPluginForCaptchaSolverTwoCaptchaAPIV2 are declared here -- declaring one without a matching
         * branch would make solve() throw IllegalArgumentException for such a challenge.
         */
        final List<CAPTCHA_TYPE> types = new ArrayList<CAPTCHA_TYPE>();
        types.add(CAPTCHA_TYPE.IMAGE);
        types.add(CAPTCHA_TYPE.IMAGE_SINGLE_CLICK_CAPTCHA);
        types.add(CAPTCHA_TYPE.IMAGE_MULTI_CLICK_CAPTCHA);
        /*
         * 2026-09-24: anti-captcha.com's API does offer RecaptchaV3TaskProxyless, and solve() already builds a v3 task when a
         * RecaptchaV2Challenge reports isV3()/a v3 action -- but this was never live-tested for this service specifically, so it stays
         * disabled here until confirmed working. Re-enable once tested.
         */
        // types.add(CAPTCHA_TYPE.RECAPTCHA_V3);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2_ENTERPRISE);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2_INVISIBLE);
        types.add(CAPTCHA_TYPE.CLOUDFLARE_TURNSTILE);
        /*
         * 2026-09-24: anti-captcha.com's API does offer GeeTestTaskProxyless/FriendlyCaptchaTaskProxyless, but solve() has no request
         * handling for them yet, so they are NOT declared here. Re-add together with the corresponding solve() handling.
         */
        // types.add(CAPTCHA_TYPE.GEETEST_V1);
        // types.add(CAPTCHA_TYPE.GEETEST_V4);
        // types.add(CAPTCHA_TYPE.FRIENDLY_CAPTCHA);
        return types;
    }

    protected String getApiBase() {
        return "https://api." + getHost();
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/legal/tos";
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
        return "https://" + getHost() + "/tutorials";
    }

    @Override
    public Class<? extends CaptchaSolverPluginConfigAntiCaptchaCom> getConfigInterface() {
        return CaptchaSolverPluginConfigAntiCaptchaCom.class;
    }
}