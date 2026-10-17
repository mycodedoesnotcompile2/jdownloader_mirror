package jd.plugins.hoster;

import java.util.Currency;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.ReflectionUtils;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.ChallengeSolver.FeedbackType;
import org.jdownloader.captcha.v2.SolverStatus;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CloudflareTurnstileChallenge;
import org.jdownloader.captcha.v2.challenge.hcaptcha.AbstractHCaptcha;
import org.jdownloader.captcha.v2.challenge.hcaptcha.HCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.CaptchaResponse;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.ImageCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.TokenCaptchaResponse;
import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolver;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfigCapmonster;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.requests.PostRequest;
import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountUnavailableException;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

/**
 * Captcha solver plugin for capmonster.cloud: https://docs.capmonster.cloud/ <br>
 * The API is similar to the one of anti-captcha.com/2captcha.com (createTask/getTaskResult) but has its own task types and feedback
 * methods.
 */
@HostPlugin(revision = "$Revision: 53673 $", interfaceVersion = 3, names = { "capmonster.cloud" }, urls = { "" })
public class PluginForCaptchaSolverCapmonsterCloud extends abstractPluginForCaptchaSolver {
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
    public Set<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
        final Set<CAPTCHA_TYPE> types = EnumSet.noneOf(CAPTCHA_TYPE.class);
        types.add(CAPTCHA_TYPE.IMAGE);
        /**
         * 2026-10-13: Click captchas are solved via ComplexImageTask module "shein" (the only coordinate module, see
         * https://docs.capmonster.cloud/docs/captchas/compleximage/coordinate/shein/). capmonster.cloud has no generic "click on the image"
         * task, so this only works for captchas that module was trained for. Images are limited to 50 KB (ERROR_TOO_BIG_CAPTCHA_FILESIZE)
         * and must be sent as base64 (no URL/upload). <br>
         * 2026-10-15: Disabled all click captcha challenges since they only support very specific click captchas called "Shein".
         */
        // types.add(CAPTCHA_TYPE.IMAGE_SINGLE_CLICK_CAPTCHA);
        // types.add(CAPTCHA_TYPE.IMAGE_MULTI_CLICK_CAPTCHA);
        /* 2026-10-16: hCaptcha is not supported. */
        types.add(CAPTCHA_TYPE.HCAPTCHA);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2_ENTERPRISE);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2_INVISIBLE);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V3);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V3_ENTERPRISE);
        types.add(CAPTCHA_TYPE.CLOUDFLARE_TURNSTILE);
        /*
         * 2026-09-24: capmonster.cloud's API does offer MtCaptchaTaskProxyless, but solve() has no request handling for it yet, so it is
         * NOT declared here. Declaring it without a matching solve() branch would make solve() throw for such a challenge. Re-add together
         * with the corresponding solve() handling.
         */
        // types.add(CAPTCHA_TYPE.MT_CAPTCHA);
        return types;
    }

    @Override
    public Set<FeedbackType> getSupportedFeedbackTypes() {
        /*
         * Only negative feedback is supported by the API. There is no "report correct" and no "abort task" method. See
         * sendCaptchaFeedback() for details.
         */
        return EnumSet.of(FeedbackType.REPORT_INVALID_CAPTCHAS);
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final Map<String, Object> postdata = new HashMap<String, Object>();
        postdata.put("clientKey", account.getPass());
        final PostRequest req = br.createJSonPostRequest(this.getApiBase() + "/getBalance", postdata);
        br.getPage(req);
        final Map<String, Object> entries = this.handleAPIErrors(br, account);
        final double balance = ((Number) ReflectionUtils.cast(entries.get("balance"), Double.class)).doubleValue();
        final AccountInfo ai = new AccountInfo();
        ai.setAccountBalance(balance, Currency.getInstance("USD"));
        return ai;
    }

    @Override
    public void solve(final CESSolverJob<?> job, final Account account) throws Exception {
        final Challenge<?> captchachallenge = job.getChallenge();
        job.setStatus(SolverStatus.UPLOADING);
        final Map<String, Object> postdata = new HashMap<String, Object>();
        final String apikey = account.getPass();
        postdata.put("clientKey", apikey);
        final Map<String, Object> task = new HashMap<String, Object>();
        if (captchachallenge instanceof RecaptchaV2Challenge) {
            final RecaptchaV2Challenge challenge = (RecaptchaV2Challenge) captchachallenge;
            task.put("type", "RecaptchaV2TaskProxyless");
            task.put("websiteKey", challenge.getSiteKey());
            task.put("websiteURL", challenge.getSiteUrl(this));
            final Map<String, Object> action = challenge.getV3Action();
            if (challenge.isV3() || action != null) {
                /* https://docs.capmonster.cloud/docs/captchas/recaptcha-v3-task/ (isEnterprise is supported by this task type) */
                task.put("type", "RecaptchaV3TaskProxyless");
                task.put("isEnterprise", challenge.isEnterprise());
                final Double minScore = challenge.getMinScore();
                if (minScore != null) {
                    task.put("minScore", minScore);
                }
                if (action != null) {
                    task.put("pageAction", action.get("action"));
                }
            } else {
                if (challenge.isEnterprise()) {
                    /* https://docs.capmonster.cloud/docs/captchas/recaptcha-v2-enterprise-task/ */
                    task.put("type", "RecaptchaV2EnterpriseTaskProxyless");
                }
                task.put("isInvisible", challenge.isInvisible());
            }
        } else if (captchachallenge instanceof HCaptchaChallenge) {
            final HCaptchaChallenge challenge = (HCaptchaChallenge) captchachallenge;
            task.put("type", "HCaptchaTaskProxyless");
            task.put("websiteURL", challenge.getSiteUrl(this));
            task.put("websiteKey", challenge.getSiteKey());
            final AbstractHCaptcha<?> hCaptcha = challenge.getAbstractCaptchaHelperHCaptcha();
            if (hCaptcha != null && AbstractHCaptcha.TYPE.INVISIBLE.equals(hCaptcha.getType())) {
                task.put("isInvisible", true);
            }
        } else if (captchachallenge instanceof CloudflareTurnstileChallenge) {
            /* https://docs.capmonster.cloud/docs/captchas/turnstile-task/ */
            final CloudflareTurnstileChallenge challenge = (CloudflareTurnstileChallenge) captchachallenge;
            task.put("type", "TurnstileTaskProxyless");
            task.put("websiteURL", challenge.getSiteUrl(this));
            task.put("websiteKey", challenge.getSiteKey());
        } else if (captchachallenge instanceof ImageCaptchaChallenge) {
            final ImageCaptchaChallenge<?> challenge = (ImageCaptchaChallenge<?>) captchachallenge;
            /* https://docs.capmonster.cloud/docs/captchas/ImageToText/image-to-text/ */
            task.put("type", "ImageToTextTask");
            task.put("body", challenge.getBase64ImageFile());
            final Boolean caseSensitive = challenge.getCaseSensitive();
            if (caseSensitive != null) {
                task.put("case", caseSensitive.booleanValue());
            }
            final Boolean numeric = challenge.getNumeric();
            if (numeric != null) {
                /* 0 = may contain other characters, 1 = digits only */
                task.put("numeric", numeric.booleanValue() ? 1 : 0);
            }
        } else {
            throw new IllegalArgumentException("Unexpected captcha challenge type");
        }
        postdata.put("task", task);
        /* Submit captcha */
        final PostRequest req_createTask = br.createJSonPostRequest(this.getApiBase() + "/createTask", postdata);
        br.getPage(req_createTask);
        Map<String, Object> entries = this.handleAPIErrors(br, account);
        final long taskID = ((Number) entries.get("taskId")).longValue();
        final Map<String, Object> postdata_getTaskResult = new HashMap<String, Object>();
        postdata_getTaskResult.put("clientKey", apikey);
        postdata_getTaskResult.put("taskId", taskID);
        /* Wait for captcha answer */
        job.setStatus(SolverStatus.SOLVING);
        while (job.getJob().isAlive() && !job.getJob().isSolved()) {
            waitDuringPolling(job.getChallenge(), account);
            final PostRequest req_getTaskResult = br.createJSonPostRequest(this.getApiBase() + "/getTaskResult", postdata_getTaskResult);
            br.getPage(req_getTaskResult);
            entries = this.handleAPIErrors(br, account);
            logger.info(br.getRequest().getHtmlCode());
            final String status = entries.get("status").toString();
            if (status.equalsIgnoreCase("processing")) {
                /* Not yet ready */
                continue;
            }
            if (!status.equalsIgnoreCase("ready")) {
                /* Something must've gone wrong */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final Map<String, Object> solutionmap = (Map<String, Object>) entries.get("solution");
            final AbstractResponse<?> resp;
            if (captchachallenge instanceof ImageCaptchaChallenge) {
                /* ImageToTextTask: {"solution":{"text":"answer"},"status":"ready",...} */
                resp = new CaptchaResponse((Challenge<String>) captchachallenge, job.getSolver(), solutionmap.get("text").toString());
            } else {
                /* Token captchas: Turnstile returns "token", the others return "gRecaptchaResponse". */
                String token = (String) solutionmap.get("token");
                if (token == null) {
                    token = (String) solutionmap.get("gRecaptchaResponse");
                }
                resp = new TokenCaptchaResponse((Challenge<String>) captchachallenge, job.getSolver(), token);
            }
            resp.setCaptchaSolverTaskID(Long.toString(taskID));
            job.setAnswer(resp);
            return;
        }
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
        } else if (challenge instanceof ImageCaptchaChallenge) {
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

    /** See docs: https://docs.capmonster.cloud/docs/api/api-errors/ */
    protected Map<String, Object> handleAPIErrors(final Browser br, final Account account) throws Exception {
        final Map<String, Object> entries;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException ignore) {
            /* This should never happen. */
            throw new AccountUnavailableException("Invalid API response", 1 * 60 * 1000);
        }
        handleAPIErrors(entries, account);
        return entries;
    }

    protected void handleAPIErrors(final Map<String, Object> entries, final Account account) throws Exception {
        final HashSet<String> accountErrorsPermament = new HashSet<String>();
        accountErrorsPermament.add("ERROR_KEY_DOES_NOT_EXIST");
        accountErrorsPermament.add("ERROR_ZERO_BALANCE");
        final HashSet<String> accountErrorsTemp = new HashSet<String>();
        accountErrorsTemp.add("ERROR_IP_NOT_ALLOWED");
        accountErrorsTemp.add("ERROR_IP_BANNED");
        accountErrorsTemp.add("ERROR_IP_BLOCKED");
        accountErrorsTemp.add("ERROR_TOO_MUCH_REQUESTS");
        final int errorId = ((Number) entries.get("errorId")).intValue();
        if (errorId == 0) {
            /* No error */
            return;
        }
        final String errorCode = entries.get("errorCode").toString();
        final String errorDescription = entries.get("errorDescription").toString();
        if (accountErrorsPermament.contains(errorCode)) {
            throw new AccountInvalidException(errorDescription);
        } else if (accountErrorsTemp.contains(errorCode)) {
            throw new AccountUnavailableException(errorDescription, 5 * 60 * 1000);
        } else {
            /*
             * Everything else (bad captcha data/parameters, image too big/small, unsolvable captcha, unsupported task type, ...) is a
             * per-request/captcha problem, not an account problem.
             */
            throw new PluginException(LinkStatus.ERROR_CAPTCHA, errorDescription);
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
    public long getServerSideMinPollingIntervalMillis() {
        /*
         * Docs: https://docs.capmonster.cloud/docs/api/api-errors/ -> getTaskResult must not be called more often than once every 2
         * seconds. Otherwise the API returns ERROR_TOO_MUCH_REQUESTS and blocks the API key for 10 minutes.
         */
        return 2 * 1000L;
    }

    @Override
    public long getServerSideMaxPollingTimeoutMillis() {
        /* capmonster.cloud gives up on a submitted task server-side after 5 minutes. */
        return 5 * 60 * 1000L;
    }
}
