package jd.plugins.hoster;

import java.util.Currency;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.StringUtils;
import org.appwork.utils.net.httpconnection.HTTPProxy;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.ChallengeSolver.SolverFeature;
import org.jdownloader.captcha.v2.SolverStatus;
import org.jdownloader.captcha.v2.challenge.altcha.AltchaChallenge;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CloudflareTurnstileChallenge;
import org.jdownloader.captcha.v2.challenge.powcaptcha.PowCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.CaptchaResponse;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.ImageCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.TokenCaptchaResponse;
import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolver;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfigAzcaptcha;
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
 * Captcha solver plugin for azcaptcha.com: https://azcaptcha.com/docs <br>
 * Its JSON API (V2: createTask/getTaskResult/getBalance) is similar to the one of 2captcha.com but NOT compatible with
 * abstractPluginForCaptchaSolverTwoCaptchaAPIV2: <br>
 * - API base is https://azcaptcha.com (no "api." subdomain) <br>
 * - reCAPTCHA v2 uses the task type "NoCaptchaTaskProxyless" and reCAPTCHA v3 Enterprise has its own task type <br>
 * - Negative feedback is only available via the classic API (res.php?action=reportbad), there is no "report correct" method.
 */
@HostPlugin(revision = "$Revision: 53779 $", interfaceVersion = 3, names = { "azcaptcha.com" }, urls = { "" })
public class PluginForCaptchaSolverAzcaptcha extends abstractPluginForCaptchaSolver {
    /**
     * Task types with proxy of the JSON API, see https://azcaptcha.com/docs: RecaptchaV2Task, RecaptchaV2EnterpriseTask, RecaptchaV3Task,
     * RecaptchaV3EnterpriseTask, TurnstileTask and FilecryptPowTask. The other task types (e.g. Altcha) have no proxy variant documented.
     */
    private static final Map<String, String> PROXY_TASK_TYPES = new HashMap<String, String>();
    static {
        PROXY_TASK_TYPES.put("NoCaptchaTaskProxyless", "RecaptchaV2Task");
        PROXY_TASK_TYPES.put("RecaptchaV2EnterpriseTaskProxyless", "RecaptchaV2EnterpriseTask");
        PROXY_TASK_TYPES.put("RecaptchaV3TaskProxyless", "RecaptchaV3Task");
        PROXY_TASK_TYPES.put("RecaptchaV3EnterpriseTaskProxyless", "RecaptchaV3EnterpriseTask");
        PROXY_TASK_TYPES.put("TurnstileTaskProxyless", "TurnstileTask");
        /* FilecryptPowTask always needs a proxy and has no separate "Proxyless" variant. */
        PROXY_TASK_TYPES.put("FilecryptPowTask", "FilecryptPowTask");
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.CAPTCHA_SOLVER, LazyPlugin.FEATURE.BUBBLE_NOTIFICATION, LazyPlugin.FEATURE.API_KEY_LOGIN };
    }

    public PluginForCaptchaSolverAzcaptcha(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public String getBuyPremiumUrl() {
        return "https://" + getHost() + "/pricing";
    }

    @Override
    public Set<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
        final Set<CAPTCHA_TYPE> types = EnumSet.noneOf(CAPTCHA_TYPE.class);
        types.add(CAPTCHA_TYPE.IMAGE);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2);
        /* 2026-10-16: v2Enterprise and v2Invisible did not work for me. Issues were reported to azcaptcha support. */
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2_ENTERPRISE);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2_INVISIBLE);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V3);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V3_ENTERPRISE);
        types.add(CAPTCHA_TYPE.CLOUDFLARE_TURNSTILE);
        /* 2026-10-22: https://azcaptcha.com/docs#altcha */
        types.add(CAPTCHA_TYPE.ALTCHA);
        /* 2026-10-27: Added after azcaptcha support confirmed it (task FilecryptPowTask, proxy required). */
        types.add(CAPTCHA_TYPE.POW_CAPTCHA);
        /*
         * hCaptcha is only mentioned in the queue statistics and the classic API (method=hcaptcha) but has no documented V2 task, so it is
         * not declared. Same for click captchas (only "RecaptchaV2GridTask" exists), MTCaptcha, FriendlyCaptcha, ...
         */
        return types;
    }

    @Override
    public Set<SolverFeature> getSolverFeatures() {
        /* Only reporting incorrectly solved captchas is possible, see sendCaptchaFeedbackInvalid(). */
        return EnumSet.of(SolverFeature.REPORT_INVALID_CAPTCHAS, SolverFeature.PROXY);
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

    /**
     * Sends the proxy of the challenge in the format of the AZCaptcha JSON API (task types see PROXY_TASK_TYPES).
     */
    private void applyProxy(final Map<String, Object> task, final HTTPProxy proxy) {
        if (proxy == null) {
            return;
        }
        final String proxyTaskType = PROXY_TASK_TYPES.get(task.get("type"));
        if (proxyTaskType == null) {
            logger.info("The service has no task type with proxy for: " + task.get("type"));
            return;
        }
        task.put("type", proxyTaskType);
        final String proxyType;
        switch (proxy.getType()) {
        case SOCKS5:
            proxyType = "socks5";
            break;
        case SOCKS4:
        case SOCKS4A:
            proxyType = "socks4";
            break;
        case HTTP:
        case HTTPS:
        default:
            proxyType = "http";
            break;
        }
        task.put("proxyType", proxyType);
        task.put("proxyAddress", proxy.getHost());
        task.put("proxyPort", proxy.getPort());
        if (StringUtils.isNotEmpty(proxy.getUser())) {
            task.put("proxyLogin", proxy.getUser());
            task.put("proxyPassword", proxy.getPass() != null ? proxy.getPass() : "");
        }
    }

    @Override
    public void solve(final CESSolverJob<?> job, final Account account) throws Exception {
        final Challenge<?> captchachallenge = job.getChallenge();
        job.setStatus(SolverStatus.UPLOADING);
        final Map<String, Object> postdata = new HashMap<String, Object>();
        final String apikey = account.getPass();
        postdata.put("clientKey", apikey);
        final Map<String, Object> task = new HashMap<String, Object>();
        /* Use the proxy of the challenge (if there is one and if the task type supports it). */
        HTTPProxy proxy = captchachallenge.hasProxy() ? captchachallenge.getProxy() : null;
        if (captchachallenge instanceof PowCaptchaChallenge) {
            /*
             * powCAPTCHA (filecrypt.cc): The token is bound to the IP address which solved it, so a proxy is REQUIRED (the service returns
             * ERROR_PROXY_REQUIRED without one). The proxy is added by applyProxy().
             */
            if (proxy == null) {
                throw new PluginException(LinkStatus.ERROR_CAPTCHA, "ERROR_PROXY_REQUIRED: powCAPTCHA can only be solved with a proxy");
            }
            task.put("type", "FilecryptPowTask");
            task.put("websiteURL", ((PowCaptchaChallenge) captchachallenge).getSiteUrl(this));
        } else if (captchachallenge instanceof RecaptchaV2Challenge) {
            final RecaptchaV2Challenge challenge = (RecaptchaV2Challenge) captchachallenge;
            task.put("websiteKey", challenge.getSiteKey());
            task.put("websiteURL", challenge.getSiteUrl(this));
            final Map<String, Object> action = challenge.getV3Action();
            if (challenge.isV3() || action != null) {
                /* https://azcaptcha.com/docs -> reCAPTCHA v3 / reCAPTCHA v3 Enterprise */
                task.put("type", challenge.isEnterprise() ? "RecaptchaV3EnterpriseTaskProxyless" : "RecaptchaV3TaskProxyless");
                final Double minScore = challenge.getMinScore();
                if (minScore != null) {
                    task.put("minScore", minScore);
                }
                if (action != null) {
                    task.put("pageAction", action.get("action"));
                }
            } else if (challenge.isEnterprise()) {
                task.put("type", "RecaptchaV2EnterpriseTaskProxyless");
            } else {
                task.put("type", "NoCaptchaTaskProxyless");
                task.put("isInvisible", challenge.isInvisible());
            }
        } else if (captchachallenge instanceof CloudflareTurnstileChallenge) {
            final CloudflareTurnstileChallenge challenge = (CloudflareTurnstileChallenge) captchachallenge;
            task.put("type", "TurnstileTaskProxyless");
            task.put("websiteURL", challenge.getSiteUrl(this));
            task.put("websiteKey", challenge.getSiteKey());
        } else if (captchachallenge instanceof AltchaChallenge) {
            /*
             * https://azcaptcha.com/docs#altcha -> Either challengeURL or challengeJSON. Supports v1 (SHA) and v2 (PBKDF2, Argon2id,
             * Scrypt) challenges.
             */
            final AltchaChallenge challenge = (AltchaChallenge) captchachallenge;
            task.put("type", "AltchaTaskProxyless");
            task.put("websiteURL", challenge.getSiteUrl(this));
            if (challenge.getChallengeJson() != null) {
                task.put("challengeJSON", challenge.getChallengeJson());
            } else {
                task.put("challengeURL", challenge.getChallengeUrl());
            }
        } else if (captchachallenge instanceof ImageCaptchaChallenge) {
            final ImageCaptchaChallenge<?> challenge = (ImageCaptchaChallenge<?>) captchachallenge;
            task.put("type", "ImageToTextTask");
            task.put("body", challenge.getBase64ImageFile());
            final Boolean caseSensitive = challenge.getCaseSensitive();
            if (caseSensitive != null) {
                task.put("case", caseSensitive.booleanValue());
            }
            final Boolean numeric = challenge.getNumeric();
            if (numeric != null) {
                /* 0 = any characters, 1 = numbers only */
                task.put("numeric", numeric.booleanValue() ? 1 : 0);
            }
            if (challenge.getExplain() != null) {
                task.put("comment", challenge.getExplain());
            }
        } else {
            throw new IllegalArgumentException("Unexpected captcha challenge type");
        }
        applyProxy(task, proxy);
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
            if (captchachallenge instanceof PowCaptchaChallenge) {
                /* The website expects exactly these six fields. */
                final Map<String, Object> token = new LinkedHashMap<String, Object>();
                token.put("pow_id", solutionmap.get("pow_id"));
                token.put("pow_nonce", solutionmap.get("pow_nonce"));
                token.put("pow_elapsed", solutionmap.get("pow_elapsed"));
                token.put("pow_pauses", solutionmap.get("pow_pauses"));
                token.put("pow_data", solutionmap.get("pow_data"));
                token.put("pow_x", solutionmap.get("pow_x"));
                resp = new TokenCaptchaResponse((Challenge<String>) captchachallenge, job.getSolver(), JSonStorage.serializeToJson(token));
            } else if (captchachallenge instanceof ImageCaptchaChallenge) {
                resp = new CaptchaResponse((Challenge<String>) captchachallenge, job.getSolver(), solutionmap.get("text").toString());
            } else {
                /* reCAPTCHA returns "gRecaptchaResponse", other token captchas (e.g. Turnstile) are expected to return "token". */
                String token = (String) solutionmap.get("gRecaptchaResponse");
                if (token == null) {
                    token = (String) solutionmap.get("token");
                }
                resp = new TokenCaptchaResponse((Challenge<String>) captchachallenge, job.getSolver(), token);
            }
            resp.setCaptchaSolverTaskID(id);
            job.setAnswer(resp);
            return;
        }
    }

    @Override
    public boolean setInvalid(final AbstractResponse<?> response, final Account account) {
        return sendCaptchaFeedbackInvalid(response, account);
    }

    @Override
    public boolean setValid(final AbstractResponse<?> response, final Account account) {
        /* Not supported by the API. */
        return false;
    }

    /**
     * Docs: https://azcaptcha.com/docs -> res.php (API V1), action=reportbad. There is no equivalent in the JSON API. <br>
     * The response format of this action is not documented, so success is detected by a response starting with "OK".
     */
    private final boolean sendCaptchaFeedbackInvalid(final AbstractResponse<?> response, final Account account) {
        try {
            final Browser brc = br.cloneBrowser();
            brc.getPage(this.getApiBase() + "/res.php?key=" + account.getPass() + "&action=reportbad&id=" + response.getCaptchaSolverTaskID());
            /* e.g. OK_REPORT_RECORDED */
            return brc.getRequest().getHtmlCode().trim().startsWith("OK");
        } catch (final Throwable e) {
            e.printStackTrace();
            return false;
        }
    }

    /** See docs: https://azcaptcha.com/docs -> Error Codes */
    protected Map<String, Object> handleAPIErrors(final Browser br, final Account account) throws Exception {
        final Map<String, Object> entries;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException ignore) {
            /* This should never happen. */
            throw new AccountUnavailableException("Invalid API response", TimeUnit.MINUTES.toMillis(1));
        }
        handleAPIErrors(entries, account);
        return entries;
    }

    protected void handleAPIErrors(final Map<String, Object> entries, final Account account) throws Exception {
        final HashSet<String> accountErrorsPermament = new HashSet<String>();
        accountErrorsPermament.add("ERROR_KEY_DOES_NOT_EXIST");
        accountErrorsPermament.add("ERROR_ZERO_BALANCE");
        final HashSet<String> accountErrorsTemp = new HashSet<String>();
        accountErrorsTemp.add("ERROR_NO_SLOT_AVAILABLE");
        accountErrorsTemp.add("ERROR_IP_NOT_ALLOWED");
        final int errorId = ((Number) entries.get("errorId")).intValue();
        if (errorId == 0) {
            /* No error */
            return;
        }
        final String errorCode = entries.get("errorCode").toString();
        /* errorDescription can be null. */
        final Object errorDescriptionO = entries.get("errorDescription");
        final String errorDescription;
        if (errorDescriptionO != null) {
            errorDescription = errorCode + ": " + errorDescriptionO.toString();
        } else {
            errorDescription = errorCode;
        }
        if (accountErrorsPermament.contains(errorCode)) {
            throw new AccountInvalidException(errorDescription);
        } else if (accountErrorsTemp.contains(errorCode)) {
            throw new AccountUnavailableException(errorDescription, TimeUnit.MINUTES.toMillis(5));
        } else {
            /* Everything else (bad parameters, unsolvable captcha, unsupported task, timeout, ...) is a per-captcha problem. */
            throw new PluginException(LinkStatus.ERROR_CAPTCHA, errorDescription);
        }
    }

    protected String getApiBase() {
        return "https://" + getHost();
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/terms";
    }

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        if (str == null) {
            return false;
        }
        return str.matches("[a-f0-9]{40}");
    }

    @Override
    protected String getAPILoginHelpURL() {
        return "https://" + getHost() + "/dashboard";
    }

    @Override
    public Class<? extends CaptchaSolverPluginConfigAzcaptcha> getConfigInterface() {
        return CaptchaSolverPluginConfigAzcaptcha.class;
    }

    @Override
    public long getServerSideMinPollingIntervalMillis() {
        /* Docs: "Call every 3-5 seconds". */
        return TimeUnit.SECONDS.toMillis(3);
    }
}
