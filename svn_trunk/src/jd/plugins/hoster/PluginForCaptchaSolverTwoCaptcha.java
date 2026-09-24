package jd.plugins.hoster;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.plugins.Account;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.HostPlugin;

import org.appwork.utils.Time;
import org.jdownloader.captcha.v2.challenge.hcaptcha.HCaptchaChallenge;
import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolverTwoCaptchaAPIV2;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfigTwoCaptcha;
import org.jdownloader.plugins.controller.LazyPlugin;

@HostPlugin(revision = "$Revision: 53503 $", interfaceVersion = 3, names = { "2captcha.com" }, urls = { "" })
public class PluginForCaptchaSolverTwoCaptcha extends abstractPluginForCaptchaSolverTwoCaptchaAPIV2 {
    /*
     * hCaptcha support is cached per account via account properties (not an instance field): abstractPluginForCaptchaSolver creates a
     * fresh plugin instance for every single challenge (see getPluginChallengeSolver()), so an instance field would never actually persist
     * anything across solve attempts.
     */
    private static final String PROPERTY_HCAPTCHA_SUPPORTED               = "hcaptcha_supported";
    private static final String PROPERTY_HCAPTCHA_LAST_FAILURE_TIMESTAMP  = "hcaptcha_last_failure_timestamp";

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.CAPTCHA_SOLVER, LazyPlugin.FEATURE.BUBBLE_NOTIFICATION, LazyPlugin.FEATURE.API_KEY_LOGIN };
    }

    public PluginForCaptchaSolverTwoCaptcha(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public String getBuyPremiumUrl() {
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
        /*
         * 2025-12-22: hCaptcha is officially not supported anymore. Some accounts support it -> Add it if at least one account supports it.
         */
        final List<Account> accounts = AccountController.getInstance().getValidAccounts(this.getHost());
        if (accounts != null && accounts.size() > 0) {
            for (final Account account : accounts) {
                final Boolean hcStatus = this.supportsHcaptcha(account);
                if (hcStatus != Boolean.FALSE) {
                    /*
                     * If we got at least one account that has either never been tried for hCaptcha or even is confirmed working for
                     * hCaptcha, yes we can add hCaptcha to the list of supported captcha types.
                     */
                    types.add(CAPTCHA_TYPE.HCAPTCHA);
                    break;
                }
            }
        }
        types.add(CAPTCHA_TYPE.CLOUDFLARE_TURNSTILE);
        /*
         * 2026-09-24: 2captcha's API does offer MtCaptchaTaskProxyless/GeeTestTaskProxyless, but solve() (in
         * abstractPluginForCaptchaSolverTwoCaptchaAPIV2) has no request handling for them yet, so they are NOT declared here. Declaring
         * them without a matching solve() branch would make solve() throw for such a challenge. Re-add together with the corresponding
         * solve() handling.
         */
        // types.add(CAPTCHA_TYPE.MT_CAPTCHA);
        // types.add(CAPTCHA_TYPE.GEETEST_V1);
        // types.add(CAPTCHA_TYPE.GEETEST_V4);
        return types;
    }

    @Override
    public List<CAPTCHA_TYPE> getSupportedCaptchaTypes(final Account account) {
        /* Get list of all captcha types supported by this service. */
        final List<CAPTCHA_TYPE> supported_captcha_types = this.getSupportedCaptchaTypes();
        if (Boolean.FALSE.equals(supportsHcaptcha(account))) {
            /* hCaptcha is not supported by this account -> Remove from list of supported captcha types. */
            supported_captcha_types.remove(CAPTCHA_TYPE.HCAPTCHA);
        }
        return supported_captcha_types;
    }

    /* Returns true if account supports hCaptcha. */
    private Boolean supportsHcaptcha(final Account account) {
        if (account.getBooleanProperty(PROPERTY_HCAPTCHA_SUPPORTED, false)) {
            /* Account supports hCaptcha */
            return Boolean.TRUE;
        }
        if (account.hasProperty(PROPERTY_HCAPTCHA_LAST_FAILURE_TIMESTAMP)) {
            /* Last failure timestamp must be given -> We know that this account doesn't support hCaptcha */
            return Boolean.FALSE;
        }
        /* hCaptcha status hasn't been evaluated yet */
        return null;
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
            if (this.getCurrentCaptchaChallenge() instanceof HCaptchaChallenge) {
                account.setProperty(PROPERTY_HCAPTCHA_SUPPORTED, true);
            }
            return;
        }
        final String errorCode = entries.get("errorCode").toString();
        if (this.getCurrentCaptchaChallenge() instanceof HCaptchaChallenge && errorId == 5 && "ERROR_METHOD_CALL".equalsIgnoreCase(errorCode)) {
            /* Special hCaptcha handling. This should only happen once per session. */
            /* Example response: {"errorId":5,"errorCode":"ERROR_METHOD_CALL","errorDescription":"Error"} */
            logger.info("hCaptcha is not supported by this 2captcha API key");
            account.setProperty(PROPERTY_HCAPTCHA_LAST_FAILURE_TIMESTAMP, Time.systemIndependentCurrentJVMTimeMillis());
        }
        super.handleAPIErrors(entries, account);
    }
}