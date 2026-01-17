package org.jdownloader.plugins.components.captchasolver;

import java.io.IOException;
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
import org.jdownloader.captcha.v2.solver.jac.SolverException;
import org.jdownloader.captcha.v2.solver.twocaptcha.TwoCaptchaResponse;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.controlling.captcha.SkipException;
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
        ai.setAccountBalance(balance, Currency.getInstance("USD"));
        return ai;
    }

    @Override
    public void solve(CESSolverJob<?> job, Account account) throws InterruptedException, SolverException, SkipException {
        final Challenge<?> captchachallenge = job.getChallenge();
        job.setStatus(SolverStatus.UPLOADING);
        try {
            final Map<String, Object> postdata = new HashMap<String, Object>();
            final String apikey = account.getPass();
            postdata.put("clientKey", apikey);
            final Map<String, Object> task = new HashMap<String, Object>(); // APIv2
            if (captchachallenge instanceof RecaptchaV2Challenge) {
                final RecaptchaV2Challenge challenge = (RecaptchaV2Challenge) job.getChallenge();
                task.put("type", "RecaptchaV2TaskProxyless");
                task.put("websiteKey", challenge.getSiteKey());
                task.put("websiteURL", challenge.getSiteUrl());
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
                if (account.getHoster().equals("2captcha.com") && challenge.isEnterprise() && StringUtils.containsIgnoreCase(challenge.getSiteUrl(), "filer.net")) {
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
                task.put("websiteURL", challenge.getSiteUrl());
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
                task.put("websiteURL", challenge.getSiteUrl());
            } else if (captchachallenge instanceof CloudflareTurnstileChallenge) {
                /* Cloudflare turnstile: https://2captcha.com/api-docs/cloudflare-turnstile */
                final CloudflareTurnstileChallenge challenge = (CloudflareTurnstileChallenge) captchachallenge;
                task.put("type", "TurnstileTaskProxyless");
                task.put("websiteURL", challenge.getSiteUrl());
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
            Map<String, Object> entries = this.handleAPIErrors(br, getCurrentAccount());
            final String id = entries.get("taskId").toString();
            final Map<String, Object> postdata_getTaskResult = new HashMap<String, Object>();
            postdata_getTaskResult.put("clientKey", apikey);
            postdata_getTaskResult.put("taskId", id);
            /* Wait for captcha answer */
            job.setStatus(SolverStatus.SOLVING);
            while (job.getJob().isAlive() && !job.getJob().isSolved()) {
                checkInterruption();
                final PostRequest req_getTaskResult = br.createJSonPostRequest(this.getApiBase() + "/getTaskResult", postdata_getTaskResult);
                br.getPage(req_getTaskResult);
                entries = this.handleAPIErrors(br, getCurrentAccount());
                logger.info(br.getRequest().getHtmlCode());
                final String status = entries.get("status").toString();
                if (status.equalsIgnoreCase("processing")) {
                    /* Not yet ready */
                    // TODO: Use other, more responsive sleep method
                    Thread.sleep(getPollingIntervalMillis(account));
                    continue;
                }
                if (!status.equalsIgnoreCase("ready")) {
                    /* Something must've gone wrong */
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                final Map<String, Object> solutionmap = (Map<String, Object>) entries.get("solution");
                final List<Map<String, Object>> clicklist = (List<Map<String, Object>>) solutionmap.get("coordinates");
                /* Answer for interactive browser captchas is given both in field "gRecaptchaResponse" and "solution". */
                // final String gRecaptchaResponse = (String) solutionmap.get("gRecaptchaResponse");
                final String token = (String) solutionmap.get("token");
                AbstractResponse resp = null;
                if (captchachallenge instanceof RecaptchaV2Challenge || captchachallenge instanceof HCaptchaChallenge || captchachallenge instanceof CloudflareTurnstileChallenge || captchachallenge instanceof CutCaptchaChallenge) {
                    resp = new TokenCaptchaResponse((Challenge<String>) captchachallenge, this, token, 0);
                } else if (captchachallenge instanceof MultiClickCaptchaChallenge || captchachallenge instanceof ClickCaptchaChallenge) {
                    // TODO: Test this
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
                        resp = new MultiClickCaptchaResponse(challenge, this, mcp, 0);
                    } else {
                        final ClickCaptchaChallenge challenge = (ClickCaptchaChallenge) captchachallenge;
                        final ClickedPoint cp = new ClickedPoint(x[0], y[0]);
                        resp = new ClickCaptchaResponse(challenge, this, cp, 0);
                    }
                } else {
                    resp = new CaptchaResponse((Challenge<String>) captchachallenge, this, solutionmap.get("text").toString(), 0);
                }
                /**
                 * TODO: Correct answer leads to exception in PluginForHost -> Line 676 check why this happens <br>
                 * if (!c.isSolved()) { throw new PluginException(LinkStatus.ERROR_CAPTCHA);
                 */
                resp.setCaptchaSolverTaskID(id);
                job.setAnswer(resp);
                return;
            }
        } catch (IOException e) {
            job.getLogger().log(e);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            System.out.println(1);
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
        final TwoCaptchaResponse twocaptcharesponse = (TwoCaptchaResponse) response;
        final String captchaID = twocaptcharesponse.getCaptchaID();
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
    private Map<String, Object> handleAPIErrors(final Browser br, final Account account) throws Exception {
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
        final HashSet<String> accountErrorsPermament = new HashSet<String>();
        accountErrorsPermament.add("ERROR_KEY_DOES_NOT_EXIST");
        accountErrorsPermament.add("ERROR_ZERO_BALANCE");
        accountErrorsPermament.add("ERROR_NO_SUCH_METHOD");
        accountErrorsPermament.add("ERROR_ACCOUNT_SUSPENDED");
        final HashSet<String> accountErrorsTemp = new HashSet<String>();
        accountErrorsTemp.add("ERROR_NO_SLOT_AVAILABLE");
        accountErrorsTemp.add("ERROR_IP_NOT_ALLOWED");
        accountErrorsTemp.add("ERROR_IP_BLOCKED");
        final HashSet<String> captchaErrors = new HashSet<String>();
        captchaErrors.add("ERROR_ZERO_CAPTCHA_FILESIZE");
        captchaErrors.add("ERROR_TOO_BIG_CAPTCHA_FILESIZE");
        captchaErrors.add("ERROR_CAPTCHA_UNSOLVABLE");
        captchaErrors.add("ERROR_BAD_DUPLICATES");
        captchaErrors.add("ERROR_IMAGE_TYPE_NOT_SUPPORTED");
        /* Errors that should never happen */
        captchaErrors.add("ERROR_NO_SUCH_CAPCHA_ID");
        captchaErrors.add("ERROR_TASK_ABSENT");
        captchaErrors.add("ERROR_TASK_NOT_SUPPORTED");
        captchaErrors.add("ERROR_RECAPTCHA_INVALID_SITEKEY");
        captchaErrors.add("ERROR_BAD_PROXY");
        captchaErrors.add("ERROR_BAD_PARAMETERS");
        captchaErrors.add("ERROR_BAD_IMGINSTRUCTIONS");
        final int errorId = ((Number) entries.get("errorId")).intValue();
        if (errorId == 0) {
            /* No error */
            return entries;
        }
        final String errorCode = entries.get("errorCode").toString();
        final String errorDescription = entries.get("errorDescription").toString();
        if (accountErrorsPermament.contains(errorCode)) {
            throw new AccountInvalidException(errorDescription);
        } else if (accountErrorsTemp.contains(errorCode)) {
            throw new AccountUnavailableException(errorDescription, 5 * 60 * 1000);
        } else {
            // TODO: Check this
            throw new PluginException(LinkStatus.ERROR_CAPTCHA);
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