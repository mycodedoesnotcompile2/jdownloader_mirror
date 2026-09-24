package org.jdownloader.plugins.components.captchasolver;

import java.util.Currency;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.StringUtils;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.SolverStatus;
import org.jdownloader.captcha.v2.challenge.clickcaptcha.ClickCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.clickcaptcha.ClickedPoint;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CloudflareTurnstileChallenge;
import org.jdownloader.captcha.v2.challenge.cutcaptcha.CutCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.hcaptcha.AbstractHCaptcha;
import org.jdownloader.captcha.v2.challenge.hcaptcha.HCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.multiclickcaptcha.MultiClickCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.multiclickcaptcha.MultiClickedPoint;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.AbstractRecaptchaV2.TYPE;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.CaptchaResponse;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.ClickCaptchaResponse;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.ImageCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.MultiClickCaptchaResponse;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.TokenCaptchaResponse;
import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.requests.PostRequest;
import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountUnavailableException;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

/**
 * Base plugin class for captcha solving via 2captcha.com APIv2: https://2captcha.com/api-docs
 */
@HostPlugin(revision = "$Revision: 50799 $", interfaceVersion = 3, names = { "2captcha.com" }, urls = { "" })
public abstract class abstractPluginForCaptchaSolverTwoCaptchaAPIV2 extends abstractPluginForCaptchaSolver {
    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.CAPTCHA_SOLVER, LazyPlugin.FEATURE.BUBBLE_NOTIFICATION, LazyPlugin.FEATURE.API_KEY_LOGIN };
    }

    public abstractPluginForCaptchaSolverTwoCaptchaAPIV2(PluginWrapper wrapper) {
        super(wrapper);
    }

    public abstract String getBuyPremiumUrl();

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final Map<String, Object> postdata = new HashMap<String, Object>();
        postdata.put("clientKey", account.getPass());
        final PostRequest req = br.createJSonPostRequest(this.getApiBase() + "/getBalance", postdata);
        br.getPage(req);
        final Map<String, Object> entries = this.handleAPIErrors(br, account);
        final double balance = ((Number) ReflectionUtils.cast(entries.get("balance"), Double.class)).doubleValue();
        final AccountInfo ai = new AccountInfo();
        ai.setAccountBalance(balance, Currency.getInstance("EUR"));
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
                final RecaptchaV2Challenge challenge = (RecaptchaV2Challenge) job.getChallenge();
                task.put("type", "RecaptchaV2TaskProxyless");
                task.put("websiteKey", challenge.getSiteKey());
                task.put("websiteURL", challenge.getSiteUrl(this));
                final Map<String, Object> action = challenge.getV3Action();
                if (challenge.isV3() || action != null) {
                    task.put("type", "RecaptchaV3TaskProxyless");
                    task.put("isEnterprise", challenge.isEnterprise());
                } else if (challenge.isEnterprise()) {
                    task.put("type", "RecaptchaV2EnterpriseTaskProxyless");
                }
                if (action != null) {
                    task.put("pageAction", action.get("action"));
                }
                task.put("isInvisible", TYPE.INVISIBLE.equals(challenge.getType()));
                final Double minScore = challenge.getMinScore();
                if (minScore != null) {
                    task.put("minScore", minScore);
                }
                if (account.getHoster().equals("2captcha.com") && challenge.isEnterprise() && StringUtils.containsIgnoreCase(challenge.getSiteUrl(this), "filer.net")) {
                    /**
                     * Special workaround for API bug, this should be RecaptchaV3TaskProxyless but if we use it we will get wrong results.
                     * <br>
                     * Is: https://2captcha.com/api-docs/recaptcha-v2-enterprise#recaptcha-v2-enterprise <br>
                     * Should be: https://2captcha.com/api-docs/recaptcha-v3
                     */
                    /**
                     * undocumented: RecaptchaV2EnterpriseTaskProxyless also supports pageAction(v3)
                     */
                    task.put("type", "RecaptchaV2EnterpriseTaskProxyless");
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
            } else if (captchachallenge instanceof CutCaptchaChallenge) {
                /* CutCaptcha: https://2captcha.com/api-docs/cutcaptcha */
                final CutCaptchaChallenge challenge = (CutCaptchaChallenge) captchachallenge;
                task.put("type", "CutCaptchaTaskProxyless");
                task.put("miseryKey", challenge.getSiteKey());
                task.put("apiKey", challenge.getApiKey());
                task.put("websiteURL", challenge.getSiteUrl(this));
            } else if (captchachallenge instanceof CloudflareTurnstileChallenge) {
                /* Cloudflare turnstile: https://2captcha.com/api-docs/cloudflare-turnstile */
                final CloudflareTurnstileChallenge challenge = (CloudflareTurnstileChallenge) captchachallenge;
                task.put("type", "TurnstileTaskProxyless");
                task.put("websiteURL", challenge.getSiteUrl(this));
                task.put("websiteKey", challenge.getSiteKey());
            } else if (captchachallenge instanceof ClickCaptchaChallenge) {
                /* Coordinates task: https://2captcha.com/api-docs/coordinates */
                final ClickCaptchaChallenge challenge = (ClickCaptchaChallenge) captchachallenge;
                task.put("type", "CoordinatesTask");
                task.put("body", challenge.getBase64ImageFile());
                task.put("minClicks", 1);
                task.put("maxClicks", 1);
            } else if (captchachallenge instanceof MultiClickCaptchaChallenge) {
                /* Coordinates task: https://2captcha.com/api-docs/coordinates */
                final MultiClickCaptchaChallenge challenge = (MultiClickCaptchaChallenge) captchachallenge;
                task.put("type", "CoordinatesTask");
                task.put("body", challenge.getBase64ImageFile());
                task.put("minClicks", 1);
                if (challenge.getMaxClicks() != -1) {
                    task.put("maxClicks", challenge.getMaxClicks());
                }
            } else if (captchachallenge instanceof ImageCaptchaChallenge) {
                /* Image captcha: https://2captcha.com/api-docs/normal-captcha */
                final ImageCaptchaChallenge challenge = (ImageCaptchaChallenge<String>) job.getChallenge();
                task.put("type", "ImageToTextTask");
                task.put("body", challenge.getBase64ImageFile());
            } else {
                throw new IllegalArgumentException("Unexpected captcha challenge type");
            }
            if (captchachallenge.getExplain() != null) {
                task.put("comment", captchachallenge.getExplain());
            }
            postdata.put("task", task);
            /* Submit captcha */
            final PostRequest req_createTask = br.createJSonPostRequest(this.getApiBase() + "/createTask", postdata);
            br.getPage(req_createTask);
            Map<String, Object> entries = this.handleAPIErrors(br, account);
            final String id = entries.get("taskId").toString();
            final Map<String, Object> postdata_getTaskResult = new HashMap<String, Object>();
            postdata_getTaskResult.put("clientKey", apikey);
            postdata_getTaskResult.put("taskId", id);
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
                /* Answer for interactive browser captchas is given both in field "gRecaptchaResponse" and "solution". */
                String token = (String) solutionmap.get("token");
                if (token == null) {
                    /* 2026-09-22: e.g. capmonster.cloud returns result only via this field. */
                    token = (String) solutionmap.get("gRecaptchaResponse");
                }
                AbstractResponse resp = null;
                if (captchachallenge instanceof RecaptchaV2Challenge || captchachallenge instanceof HCaptchaChallenge || captchachallenge instanceof CloudflareTurnstileChallenge || captchachallenge instanceof CutCaptchaChallenge) {
                    resp = new TokenCaptchaResponse((Challenge<String>) captchachallenge, job.getSolver(), token);
                } else if (captchachallenge instanceof MultiClickCaptchaChallenge || captchachallenge instanceof ClickCaptchaChallenge) {
                    // TODO: Test this
                    final List<Map<String, Object>> clicklist = (List<Map<String, Object>>) solutionmap.get("coordinates");
                    final int[] x = new int[clicklist.size()];
                    final int[] y = new int[clicklist.size()];
                    int i = 0;
                    for (final Map<String, Object> clicks : clicklist) {
                        x[i] = ((Number) clicks.get("x")).intValue();
                        y[i] = ((Number) clicks.get("y")).intValue();
                        i++;
                    }
                    /* Coordinates task: https://2captcha.com/api-docs/coordinates */
                    if (captchachallenge instanceof MultiClickCaptchaChallenge) {
                        final MultiClickedPoint mcp = new MultiClickedPoint(x, y);
                        final MultiClickCaptchaChallenge challenge = (MultiClickCaptchaChallenge) captchachallenge;
                        resp = new MultiClickCaptchaResponse(challenge, job.getSolver(), mcp);
                    } else {
                        final ClickCaptchaChallenge challenge = (ClickCaptchaChallenge) captchachallenge;
                        final ClickedPoint cp = new ClickedPoint(x[0], y[0]);
                        resp = new ClickCaptchaResponse(challenge, job.getSolver(), cp);
                    }
                } else {
                    resp = new CaptchaResponse((Challenge<String>) captchachallenge, job.getSolver(), solutionmap.get("text").toString());
                }
                resp.setCaptchaSolverTaskID(id);
                job.setAnswer(resp);
                return;
            }
    }

    @Override
    public boolean setInvalid(AbstractResponse<?> response, Account account) {
        return sendCaptchaFeedback(response, account, false);
    }

    @Override
    public boolean setValid(AbstractResponse<?> response, Account account) {
        return sendCaptchaFeedback(response, account, true);
    }

    private final boolean sendCaptchaFeedback(final AbstractResponse<?> response, Account account, final boolean positiveFeedback) {
        /* The 2captcha task id was stored on the response when the captcha was solved (see solve()). */
        final String captchaID = response.getCaptchaSolverTaskID();
        try {
            final String url;
            if (positiveFeedback) {
                /* https://2captcha.com/api-docs/report-correct */
                url = "/reportCorrect";
            } else {
                /* https://2captcha.com/api-docs/report-incorrect */
                url = "/reportIncorrect";
            }
            final Map<String, Object> postdata = new HashMap<String, Object>();
            postdata.put("clientKey", account.getPass());
            postdata.put("taskId", captchaID);
            final PostRequest req = br.createJSonPostRequest(this.getApiBase() + url, postdata);
            br.getPage(req);
            final Map<String, Object> entries = this.handleAPIErrors(br, account);
            final String status = entries.get("status").toString();
            if (status.equalsIgnoreCase("success")) {
                return true;
            } else {
                return false;
            }
        } catch (final Throwable e) {
            e.printStackTrace();
            return false;
        }
    }

    /** See docs: https://2captcha.com/api-docs/error-codes */
    protected Map<String, Object> handleAPIErrors(final Browser br, final Account account) throws Exception {
        Map<String, Object> entries = null;
        try {
            /* 2024-11-21: Hotfix for API returning invalid json: "1{"val" (string starts with "1" and not with "{". */
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException ignore) {
            /* This should never happen. */
            final String msg = "Invalid API response";
            final long wait = 1 * 60 * 1000;
            throw new AccountUnavailableException(msg, wait);
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
        /*
         * This should only happen if the user uses an IP black- or whitelist and when tries to solve captchas using a blocked IP ->
         * User-induced problem!
         */
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
            /*
             * Everything else (bad captcha data/parameters, unsolvable captcha, unsupported/unknown task type, missing method, ...) is a
             * per-request/captcha problem, not an account problem, see https://2captcha.com/api-docs/error-codes
             */
            throw new PluginException(LinkStatus.ERROR_CAPTCHA, errorDescription);
        }
    }

    protected String getApiBase() {
        return "https://api." + getHost();
    }

    @Override
    public abstract String getAGBLink();

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        if (str == null) {
            return false;
        }
        return str.matches("[a-f0-9]{32}");
    }

    @Override
    protected abstract String getAPILoginHelpURL();
}