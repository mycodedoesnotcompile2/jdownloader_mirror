package jd.plugins.hoster;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.appwork.utils.Time;
import org.jdownloader.captcha.v2.challenge.hcaptcha.HCaptchaChallenge;
import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolverTwoCaptchaAPIV2;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfigTwoCaptcha;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.plugins.Account;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.HostPlugin;

@HostPlugin(revision = "$Revision: 52385 $", interfaceVersion = 3, names = { "2captcha.com" }, urls = { "" })
public class PluginForCaptchaSolverTwoCaptcha extends abstractPluginForCaptchaSolverTwoCaptchaAPIV2 {
    private final Map<Account, Long> hcaptcha_disabled_accounts = new HashMap<Account, Long>();

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.CAPTCHA_SOLVER, LazyPlugin.FEATURE.BUBBLE_NOTIFICATION, LazyPlugin.FEATURE.API_KEY_LOGIN };
    }

    public PluginForCaptchaSolverTwoCaptcha(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public String getBuyPremiumUrl() {
        // TODO: Remove this ref-link as it belongs into the server side ref link handling
        return "https://" + getHost() + "?from=15779444";
    }

    @Override
    public List<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
        final List<CAPTCHA_TYPE> types = new ArrayList<CAPTCHA_TYPE>();
        types.add(CAPTCHA_TYPE.IMAGE);
        types.add(CAPTCHA_TYPE.IMAGE_SINGLE_CLICK_CAPTCHA);
        types.add(CAPTCHA_TYPE.IMAGE_MULTI_CLICK_CAPTCHA);
        // types.add(CAPTCHA_TYPE.RECAPTCHA_V3);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2_ENTERPRISE);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2_INVISIBLE);
        /* 2025-12-22: hCaptcha is not supported anymore */
        // types.add(CAPTCHA_TYPE.HCAPTCHA);
        types.add(CAPTCHA_TYPE.CLOUDFLARE_TURNSTILE);
        types.add(CAPTCHA_TYPE.MT_CAPTCHA);
        types.add(CAPTCHA_TYPE.GEETEST_V1);
        types.add(CAPTCHA_TYPE.GEETEST_V4);
        return types;
    }

    @Override
    public List<CAPTCHA_TYPE> getSupportedCaptchaTypes(final Account account) {
        /* Get list of all captcha types supported by this service. */
        final List<CAPTCHA_TYPE> supported_captcha_types = this.getSupportedCaptchaTypes();
        if (hcaptcha_disabled_accounts.get(account) != null) {
            /* hCaptcha is not supported by this account. */
            supported_captcha_types.remove(CAPTCHA_TYPE.HCAPTCHA);
        }
        return supported_captcha_types;
    }

    protected String getApiBase() {
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

    @Override
    public Class<? extends CaptchaSolverPluginConfigTwoCaptcha> getConfigInterface() {
        return CaptchaSolverPluginConfigTwoCaptcha.class;
    }

    @Override
    protected void handleAPIErrors(final Map<String, Object> entries, final Account account) throws Exception {
        /* Handle special case where hCaptcha is not supported. */
        final int errorId = ((Number) entries.get("errorId")).intValue();
        if (errorId == 0) {
            /* No error */
            return;
        }
        final String errorCode = entries.get("errorCode").toString();
        if (this.getCurrentCaptchaChallenge() instanceof HCaptchaChallenge && errorId == 5 && "ERROR_METHOD_CALL".equalsIgnoreCase(errorCode)) {
            /* Special hCaptcha handling */
            /* Example response: {"errorId":5,"errorCode":"ERROR_METHOD_CALL","errorDescription":"Error"} */
            logger.info("hCaptcha is not supported by this 2captcha API key");
            hcaptcha_disabled_accounts.put(account, Time.systemIndependentCurrentJVMTimeMillis());
        }
        super.handleAPIErrors(entries, account);
    }
}