package jd.plugins.hoster;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.appwork.utils.DebugMode;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.ChallengeSolver.FeedbackType;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CloudflareTurnstileChallenge;
import org.jdownloader.captcha.v2.challenge.multiclickcaptcha.MultiClickCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.ImageCaptchaChallenge;
import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolverTwoCaptchaAPIV2;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfigCapmonster;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.requests.PostRequest;
import jd.plugins.Account;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.HostPlugin;

@HostPlugin(revision = "$Revision: 53597 $", interfaceVersion = 3, names = { "capmonster.cloud" }, urls = { "" })
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
        /* 2026-10-07: Works according to them but failed for me. */
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            types.add(CAPTCHA_TYPE.HCAPTCHA);
        }
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2_ENTERPRISE);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2_INVISIBLE);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V3);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V3_ENTERPRISE);
        types.add(CAPTCHA_TYPE.CLOUDFLARE_TURNSTILE);
        /*
         * 2026-09-24: capmonster.cloud's API does offer MtCaptchaTaskProxyless, but solve() (in
         * abstractPluginForCaptchaSolverTwoCaptchaAPIV2) has no request handling for it yet, so it is NOT declared here. Declaring it
         * without a matching solve() branch would make solve() throw for such a challenge. Re-add together with the corresponding solve()
         * handling.
         */
        // types.add(CAPTCHA_TYPE.MT_CAPTCHA);
        return types;
    }

    @Override
    public List<FeedbackType> getSupportedFeedbackTypes() {
        /*
         * Only negative feedback is supported by the API. There is no "report correct" and no "abort task" method. See
         * sendCaptchaFeedback() for details.
         */
        final List<FeedbackType> types = new ArrayList<FeedbackType>();
        types.add(FeedbackType.REPORT_INVALID_CAPTCHAS);
        return types;
    }

    @Override
    public boolean setInvalid(final AbstractResponse<?> response, final Account account) {
        return sendCaptchaFeedback(response, account);
    }

    @Override
    public boolean setValid(final AbstractResponse<?> response, final Account account) {
        /* Not supported by the API. */
        return false;
    }

    /**
     * Docs: https://docs.capmonster.cloud/docs/api/bad-tokens/ <br>
     * /reportIncorrectImageCaptcha: image captchas <br>
     * /reportIncorrectTokenCaptcha: reCaptcha (2, 3, Enterprise), GeeTest, Turnstile <br>
     * All other captcha types do not support feedback.
     */
    private final boolean sendCaptchaFeedback(final AbstractResponse<?> response, final Account account) {
        final Challenge<?> challenge = response.getChallenge();
        final String method;
        if (challenge instanceof RecaptchaV2Challenge || challenge instanceof CloudflareTurnstileChallenge) {
            method = "/reportIncorrectTokenCaptcha";
        } else if (challenge instanceof ImageCaptchaChallenge || challenge instanceof MultiClickCaptchaChallenge) {
            method = "/reportIncorrectImageCaptcha";
        } else {
            /* Not supported by the API. */
            return false;
        }
        try {
            final Map<String, Object> postdata = new HashMap<String, Object>();
            postdata.put("clientKey", account.getPass());
            postdata.put("taskId", Long.parseLong(response.getCaptchaSolverTaskID()));
            final PostRequest req = br.createJSonPostRequest(this.getApiBase() + method, postdata);
            br.getPage(req);
            final Map<String, Object> entries = this.handleAPIErrors(br, account);
            return "success".equalsIgnoreCase(entries.get("status").toString());
        } catch (final Throwable e) {
            e.printStackTrace();
            return false;
        }
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

    @Override
    public long getServerSideMaxPollingTimeoutMillis() {
        /* capmonster.cloud gives up on a submitted task server-side after 5 minutes. */
        return 5 * 60 * 1000L;
    }
}