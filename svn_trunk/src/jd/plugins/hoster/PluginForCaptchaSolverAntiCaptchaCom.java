package jd.plugins.hoster;

import java.util.EnumSet;
import java.util.Set;
import java.util.ArrayList;
import java.util.Currency;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.ReflectionUtils;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.ChallengeSolver.FeedbackType;
import org.jdownloader.captcha.v2.SolverStatus;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CloudflareTurnstileChallenge;
import org.jdownloader.captcha.v2.challenge.multiclickcaptcha.MultiClickCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.multiclickcaptcha.MultiClickedPoint;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.CaptchaResponse;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.ImageCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.TokenCaptchaResponse;
import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolver;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfigAntiCaptchaCom;
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
 * Captcha solver plugin for anti-captcha.com: https://anti-captcha.com/apidoc
 */
@HostPlugin(revision = "$Revision: 53649 $", interfaceVersion = 3, names = { "anti-captcha.com" }, urls = { "" })
public class PluginForCaptchaSolverAntiCaptchaCom extends abstractPluginForCaptchaSolver {
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
    public Set<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
        /*
         * List of task types anti-captcha.com's API offers: https://anti-captcha.com/apidoc (confirmed 2026-09-24). Only types with a
         * matching solve() branch are declared here -- declaring one without a matching branch would make solve() throw
         * IllegalArgumentException for such a challenge.
         */
        final Set<CAPTCHA_TYPE> types = EnumSet.noneOf(CAPTCHA_TYPE.class);
        types.add(CAPTCHA_TYPE.IMAGE);
        /*
         * 2026-10-12: Click captchas are solved via "ImageToCoordinatesTask" (2captcha calls this "CoordinatesTask", which anti-captcha
         * rejects with ERROR_TASK_NOT_SUPPORTED).
         */
        types.add(CAPTCHA_TYPE.IMAGE_SINGLE_CLICK_CAPTCHA);
        types.add(CAPTCHA_TYPE.IMAGE_MULTI_CLICK_CAPTCHA);
        /*
         * 2026-09-24: anti-captcha.com's API does offer RecaptchaV3TaskProxyless, and solve() already builds a v3 task when a
         * RecaptchaV2Challenge reports isV3()/a v3 action -- but this was never live-tested for this service specifically, so it stays
         * disabled here until confirmed working. Re-enable once tested.
         */
        types.add(CAPTCHA_TYPE.RECAPTCHA_V3);
        /* 2026-10-13: Looks to be not working but API also doesn't return an error. */
        types.add(CAPTCHA_TYPE.RECAPTCHA_V3_ENTERPRISE);
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

    @Override
    public Set<FeedbackType> getSupportedFeedbackTypes() {
        /*
         * Invalid: image captchas + reCaptcha. Valid: reCaptcha only. Aborting captchas is not supported by the API. See
         * sendCaptchaFeedback() for details.
         */
        return EnumSet.of(FeedbackType.REPORT_INVALID_CAPTCHAS, FeedbackType.REPORT_VALID_CAPTCHAS);
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
    public void solve(CESSolverJob<?> job, Account account) throws Exception {
        final Challenge<?> captchachallenge = job.getChallenge();
        job.setStatus(SolverStatus.UPLOADING);
        final Map<String, Object> postdata = new HashMap<String, Object>();
        final String apikey = account.getPass();
        postdata.put("clientKey", apikey);
        final Map<String, Object> task = new HashMap<String, Object>();
        if (captchachallenge instanceof RecaptchaV2Challenge) {
            /* https://anti-captcha.com/apidoc/task-types/RecaptchaV2TaskProxyless */
            final RecaptchaV2Challenge challenge = (RecaptchaV2Challenge) captchachallenge;
            task.put("type", "RecaptchaV2TaskProxyless");
            task.put("websiteKey", challenge.getSiteKey());
            task.put("websiteURL", challenge.getSiteUrl(this));
            final Map<String, Object> action = challenge.getV3Action();
            if (challenge.isV3() || action != null) {
                /* https://anti-captcha.com/apidoc/task-types/RecaptchaV3TaskProxyless */
                task.put("type", "RecaptchaV3TaskProxyless");
                task.put("isEnterprise", challenge.isEnterprise());
                /*
                 * minScore is required and only 0.3, 0.7 and 0.9 are allowed (selects the worker queue). Use the next allowed value that is
                 * >= the requested score, default to 0.3.
                 */
                final Double minScore = challenge.getMinScore();
                if (minScore == null || minScore <= 0.3) {
                    task.put("minScore", 0.3);
                } else if (minScore <= 0.7) {
                    task.put("minScore", 0.7);
                } else {
                    task.put("minScore", 0.9);
                }
                if (action != null) {
                    task.put("pageAction", action.get("action"));
                }
            } else {
                if (challenge.isEnterprise()) {
                    /* https://anti-captcha.com/apidoc/task-types/RecaptchaV2EnterpriseTaskProxyless */
                    task.put("type", "RecaptchaV2EnterpriseTaskProxyless");
                }
                task.put("isInvisible", challenge.isInvisible());
            }
        } else if (captchachallenge instanceof CloudflareTurnstileChallenge) {
            /* https://anti-captcha.com/apidoc/task-types/TurnstileTaskProxyless */
            final CloudflareTurnstileChallenge challenge = (CloudflareTurnstileChallenge) captchachallenge;
            task.put("type", "TurnstileTaskProxyless");
            task.put("websiteURL", challenge.getSiteUrl(this));
            task.put("websiteKey", challenge.getSiteKey());
        } else if (captchachallenge instanceof MultiClickCaptchaChallenge) {
            /*
             * https://anti-captcha.com/apidoc/task-types/ImageToCoordinatesTask <br> No min/maxClicks supported, the "comment" is the only
             * hint for the workers.
             */
            final MultiClickCaptchaChallenge challenge = (MultiClickCaptchaChallenge) captchachallenge;
            task.put("type", "ImageToCoordinatesTask");
            task.put("body", challenge.getBase64ImageFile());
            task.put("mode", "points");
            if (captchachallenge.getExplain() != null) {
                task.put("comment", captchachallenge.getExplain());
            }
        } else if (captchachallenge instanceof ImageCaptchaChallenge) {
            /* https://anti-captcha.com/apidoc/task-types/ImageToTextTask */
            final ImageCaptchaChallenge<?> challenge = (ImageCaptchaChallenge<?>) captchachallenge;
            task.put("type", "ImageToTextTask");
            task.put("body", challenge.getBase64ImageFile());
            final Boolean caseSensitive = challenge.getCaseSensitive();
            if (caseSensitive != null) {
                task.put("case", caseSensitive.booleanValue());
            }
            final Boolean numeric = challenge.getNumeric();
            if (numeric != null) {
                /* 0 = no requirements, 1 = digits only */
                task.put("numeric", numeric.booleanValue() ? 1 : 0);
            }
            if (captchachallenge.getExplain() != null) {
                task.put("comment", captchachallenge.getExplain());
            }
        } else {
            throw new IllegalArgumentException("Unexpected captcha challenge type");
        }
        postdata.put("task", task);
        /* Submit captcha */
        final PostRequest req_createTask = br.createJSonPostRequest(this.getApiBase() + "/createTask", postdata);
        br.getPage(req_createTask);
        Map<String, Object> entries = this.handleAPIErrors(br, account);
        final String id = entries.get("taskId").toString();
        final Map<String, Object> postdata_getTaskResult = new HashMap<String, Object>();
        postdata_getTaskResult.put("clientKey", apikey);
        postdata_getTaskResult.put("taskId", ((Number) entries.get("taskId")).longValue());
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
            if (captchachallenge instanceof RecaptchaV2Challenge) {
                resp = new TokenCaptchaResponse((Challenge<String>) captchachallenge, job.getSolver(), (String) solutionmap.get("gRecaptchaResponse"));
            } else if (captchachallenge instanceof CloudflareTurnstileChallenge) {
                resp = new TokenCaptchaResponse((Challenge<String>) captchachallenge, job.getSolver(), (String) solutionmap.get("token"));
            } else if (captchachallenge instanceof MultiClickCaptchaChallenge) {
                /* Format: "coordinates":[[x,y],[x,y]] */
                final List<List<Number>> clicklist = (List<List<Number>>) solutionmap.get("coordinates");
                final int[] x = new int[clicklist.size()];
                final int[] y = new int[clicklist.size()];
                int i = 0;
                for (final List<Number> click : clicklist) {
                    x[i] = click.get(0).intValue();
                    y[i] = click.get(1).intValue();
                    i++;
                }
                resp = new AbstractResponse<MultiClickedPoint>((MultiClickCaptchaChallenge) captchachallenge, job.getSolver(), new MultiClickedPoint(x, y));
            } else {
                resp = new CaptchaResponse((Challenge<String>) captchachallenge, job.getSolver(), solutionmap.get("text").toString());
            }
            resp.setCaptchaSolverTaskID(id);
            job.setAnswer(resp);
            return;
        }
    }

    @Override
    public boolean setInvalid(final AbstractResponse<?> response, final Account account) {
        return sendCaptchaFeedback(response, account, false);
    }

    @Override
    public boolean setValid(final AbstractResponse<?> response, final Account account) {
        return sendCaptchaFeedback(response, account, true);
    }

    /**
     * Docs: <br>
     * https://anti-captcha.com/apidoc/methods/reportIncorrectImageCaptcha (image captchas only, negative feedback only) <br>
     * https://anti-captcha.com/apidoc/methods/reportCorrectRecaptcha <br>
     * https://anti-captcha.com/apidoc/methods/reportIncorrectRecaptcha <br>
     * All other captcha types do not support feedback.
     */
    private final boolean sendCaptchaFeedback(final AbstractResponse<?> response, final Account account, final boolean positiveFeedback) {
        final Challenge<?> challenge = response.getChallenge();
        final String method;
        if (challenge instanceof RecaptchaV2Challenge) {
            method = positiveFeedback ? "/reportCorrectRecaptcha" : "/reportIncorrectRecaptcha";
        } else if (challenge instanceof MultiClickCaptchaChallenge || !(challenge instanceof ImageCaptchaChallenge)) {
            /* Not supported by the API. */
            return false;
        } else if (positiveFeedback) {
            /* Only negative feedback is supported for image captchas. */
            return false;
        } else {
            method = "/reportIncorrectImageCaptcha";
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

    /** See docs: https://anti-captcha.com/apidoc/errors */
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
        accountErrorsPermament.add("ERROR_ACCOUNT_SUSPENDED");
        final HashSet<String> accountErrorsTemp = new HashSet<String>();
        accountErrorsTemp.add("ERROR_NO_SLOT_AVAILABLE");
        accountErrorsTemp.add("ERROR_IP_NOT_ALLOWED");
        accountErrorsTemp.add("ERROR_IP_BLOCKED");
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
            /* Everything else is a per-request/captcha problem, not an account problem. */
            throw new PluginException(LinkStatus.ERROR_CAPTCHA, errorDescription);
        }
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
