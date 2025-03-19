package org.jdownloader.plugins.components.captchasolver;

import java.io.IOException;
import java.util.Currency;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

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

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.IO;
import org.appwork.utils.encoding.Base64;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.SolverStatus;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CloudflareTurnstileChallenge;
import org.jdownloader.captcha.v2.challenge.cutcaptcha.CutCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.hcaptcha.AbstractHCaptcha;
import org.jdownloader.captcha.v2.challenge.hcaptcha.HCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.AbstractRecaptchaV2;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.AbstractRecaptchaV2.TYPE;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.ImageCaptchaChallenge;
import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.captcha.v2.solver.twocaptcha.TwoCaptchaResponse;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;
import org.jdownloader.plugins.controller.LazyPlugin;

/**
 * Plugin for 2Captcha captcha solving service (https://2captcha.com/).
 */
@HostPlugin(revision = "$Revision: 50799 $", interfaceVersion = 3, names = { "2captcha.com" }, urls = { "" })
public abstract class abstractPluginForCaptchaSolverTwoCaptcha extends abstractPluginForCaptchaSolver {
    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.CAPTCHA_SOLVER, LazyPlugin.FEATURE.BUBBLE_NOTIFICATION, LazyPlugin.FEATURE.API_KEY_LOGIN };
    }

    public abstractPluginForCaptchaSolverTwoCaptcha(PluginWrapper wrapper) {
        super(wrapper);
    }

    public abstract String getBuyPremiumUrl();

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final Map<String, Object> postdata = new HashMap<String, Object>();
        postdata.put("clientKey", account.getPass());
        final PostRequest req = br.createJSonPostRequest(this.getApiBaseV2() + "/getBalance", postdata);
        br.getPage(req);
        final Map<String, Object> entries = this.handleAPIErrors(br, account);
        final double balance = ((Number) entries.get("balance")).doubleValue();
        final AccountInfo ai = new AccountInfo();
        ai.setAccountBalance(balance);
        ai.setCurrency(Currency.getInstance("USD"));
        return ai;
    }

    @Override
    protected void solveCES(CESSolverJob<?> job, Account account) throws Exception {
        final Challenge<String> captchaChallenge = (Challenge<String>) job.getChallenge();
        try {
            final Map<String, Object> postdata = new HashMap<String, Object>();
            final String apikey = this.getCurrentAccount().getPass();
            postdata.put("clientKey", apikey);
            final Map<String, Object> task = new HashMap<String, Object>(); // APIv2
            if (captchaChallenge instanceof RecaptchaV2Challenge) {
                final RecaptchaV2Challenge challenge = (RecaptchaV2Challenge) job.getChallenge();
                task.put("type", "RecaptchaV2TaskProxyless");
                task.put("websiteKey", challenge.getSiteKey());
                task.put("websiteURL", challenge.getSiteUrl());
                final AbstractRecaptchaV2<?> recaptchaChallenge = challenge.getAbstractCaptchaHelperRecaptchaV2();
                if (recaptchaChallenge != null) {
                    if (challenge.isEnterprise()) {
                        task.put("isEnterprise", true);
                    }
                    final Map<String, Object> action = challenge.getV3Action();
                    if (action != null && action.containsKey("action")) {
                        task.put("type", "RecaptchaV3TaskProxyless");
                        task.put("pageAction", String.valueOf(action.get("action")));
                    } else if (TYPE.INVISIBLE.equals(recaptchaChallenge.getType())) {
                        task.put("isInvisible", true);
                    }
                }
            } else if (captchaChallenge instanceof HCaptchaChallenge) {
                final HCaptchaChallenge challenge = (HCaptchaChallenge) job.getChallenge();
                task.put("type", "HCaptchaTaskProxyless");
                task.put("websiteURL", challenge.getSiteUrl());
                task.put("websiteKey", challenge.getSiteKey());
                final AbstractHCaptcha<?> hCaptcha = challenge.getAbstractCaptchaHelperHCaptcha();
                if (hCaptcha != null && AbstractHCaptcha.TYPE.INVISIBLE.equals(hCaptcha.getType())) {
                    task.put("isInvisible", true);
                }
            } else if (captchaChallenge instanceof CutCaptchaChallenge) {
                /* CutCaptcha: https://2captcha.com/api-docs/cutcaptcha */
                final CutCaptchaChallenge challenge = (CutCaptchaChallenge) job.getChallenge();
                task.put("type", "CutCaptchaTaskProxyless");
                task.put("miseryKey", challenge.getSiteKey());
                task.put("apiKey", challenge.getApiKey());
                task.put("websiteURL", challenge.getSiteUrl());
            } else if (captchaChallenge instanceof CloudflareTurnstileChallenge) {
                /* Cloudflare turnstile: https://2captcha.com/api-docs/cloudflare-turnstile */
                final CloudflareTurnstileChallenge challenge = (CloudflareTurnstileChallenge) job.getChallenge();
                task.put("type", "TurnstileTaskProxyless");
                task.put("websiteURL", challenge.getSiteUrl());
                task.put("websiteKey", challenge.getSiteKey());
            } else {
                /* Image captcha: https://2captcha.com/api-docs/normal-captcha */
                final ImageCaptchaChallenge challenge = (ImageCaptchaChallenge<String>) job.getChallenge();
                final byte[] data = IO.readFile(challenge.getImageFile());
                task.put("type", "ImageToTextTask");
                task.put("body", Base64.encodeToString(data, false));
                task.put("comment", challenge.getExplain());
            }
            boolean clickCaptcha = false;
            if (clickCaptcha) {
                task.put("type", "CoordinatesTask");
                // v2task.put("body", Base64.encodeToString(data, false));
                // v2task.put("comment", challenge.getExplain());
                /* We want a single set of coordinates -> One click */
                task.put("maxClicks", 1);
            }
            postdata.put("task", task);
            // TODO
            // job.showBubble(this);
            checkInterruption();
            job.setStatus(SolverStatus.SOLVING);
            /* Submit captcha */
            final PostRequest req_createTask = br.createJSonPostRequest(this.getApiBaseV2() + "/createTask", postdata);
            br.getPage(req_createTask);
            Map<String, Object> entries = this.handleAPIErrors(br, getCurrentAccount());
            final String id = entries.get("taskId").toString();
            final Map<String, Object> postdata_getTaskResult = new HashMap<String, Object>();
            postdata_getTaskResult.put("clientKey", apikey);
            postdata_getTaskResult.put("taskId", id);
            job.setStatus(new SolverStatus(_GUI.T.DeathByCaptchaSolver_solveBasicCaptchaChallenge_solving(), NewTheme.I().getIcon(IconKey.ICON_WAIT, 10)));
            while (job.getJob().isAlive() && !job.getJob().isSolved()) {
                checkInterruption();
                final PostRequest req_getTaskResult = br.createJSonPostRequest(this.getApiBaseV2() + "/getTaskResult", postdata_getTaskResult);
                br.getPage(req_getTaskResult);
                entries = this.handleAPIErrors(br, getCurrentAccount());
                logger.info(br.getRequest().getHtmlCode());
                final String status = entries.get("status").toString();
                if (status.equals("processing")) {
                    Thread.sleep(5000);
                    continue;
                }
                final Map<String, Object> solutionmap = (Map<String, Object>) entries.get("solution");
                final List<Map<String, Object>> clicklist = (List<Map<String, Object>>) solutionmap.get("corrdinates");
                final String resultText;
                final String gRecaptchaResponse = (String) solutionmap.get("gRecaptchaResponse");
                final String token = (String) solutionmap.get("token");
                if (gRecaptchaResponse != null) {
                    resultText = gRecaptchaResponse;
                } else if (token != null) {
                    /* For example reCaptchaV2, CloudflareTurnstile */
                    resultText = token;
                } else if (clicklist != null) {
                    // TODO
                    resultText = "TODO_IMPLEMENT_CLICK_CAPTCHA";
                } else {
                    resultText = solutionmap.get("text").toString();
                }
                // TODO
                // job.setAnswer(new TwoCaptchaResponse(captchaChallenge, this, id, resultText));
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
    protected boolean setInvalid(AbstractResponse<?> response) {
        return sendCaptchaFeedback(response, false);
    }

    @Override
    protected boolean setValid(AbstractResponse<?> response) {
        return sendCaptchaFeedback(response, true);
    }

    private final boolean sendCaptchaFeedback(final AbstractResponse<?> response, final boolean positiveFeedback) {
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
            postdata.put("clientKey", this.getCurrentAccount().getPass());
            postdata.put("taskId", captchaID);
            final PostRequest req = br.createJSonPostRequest(this.getApiBaseV2() + url, postdata);
            br.getPage(req);
            final Map<String, Object> entries = this.handleAPIErrors(br, this.getCurrentAccount());
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

    protected String getApiBaseV2() {
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