package jd.plugins.hoster;

import java.util.ArrayList;
import java.util.List;

import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolverTwoCaptchaAPIV2;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfigCaptchasIo;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.HostPlugin;

@HostPlugin(revision = "$Revision: 52170 $", interfaceVersion = 3, names = { "captchas.io" }, urls = { "" })
public class PluginForCaptchaSolverCaptchasIo extends abstractPluginForCaptchaSolverTwoCaptchaAPIV2 {
    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.CAPTCHA_SOLVER, LazyPlugin.FEATURE.BUBBLE_NOTIFICATION, LazyPlugin.FEATURE.API_KEY_LOGIN };
    }

    public PluginForCaptchaSolverCaptchasIo(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public String getBuyPremiumUrl() {
        return "https://app." + getHost() + "/clients/v2/packages";
    }

    @Override
    public List<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
        // TODO: 2025-12-22: Re-check supported captcha types before this plugin can be used in prod
        final List<CAPTCHA_TYPE> types = new ArrayList<CAPTCHA_TYPE>();
        types.add(CAPTCHA_TYPE.IMAGE);
        types.add(CAPTCHA_TYPE.IMAGE_SINGLE_CLICK_CAPTCHA);
        types.add(CAPTCHA_TYPE.IMAGE_MULTI_CLICK_CAPTCHA);
        // types.add(CAPTCHA_TYPE.RECAPTCHA_V3);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2_ENTERPRISE);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2_INVISIBLE);
        /**
         * 2025-12-22: hCaptcha does not seem to be supported: <br>
         * {"errorId":1,"errorCode":"ERROR_WRONG_METHOD_VALUE","errorDescription":"You have set a wrong method value."}
         */
        // types.add(CAPTCHA_TYPE.HCAPTCHA);
        // types.add(CAPTCHA_TYPE.KEY_CAPTCHA);
        types.add(CAPTCHA_TYPE.CLOUDFLARE_TURNSTILE);
        // types.add(CAPTCHA_TYPE.MT_CAPTCHA);
        return types;
    }

    protected String getApiBase() {
        return "https://api." + getHost();
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/agreement";
    }

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        if (str == null) {
            return false;
        }
        return str.matches("[a-f0-9-.]{32}");
    }

    @Override
    protected String getAPILoginHelpURL() {
        return "https://app." + getHost() + "/clients/v2/index";
    }

    @Override
    public Class<? extends CaptchaSolverPluginConfigCaptchasIo> getConfigInterface() {
        return CaptchaSolverPluginConfigCaptchasIo.class;
    }
}