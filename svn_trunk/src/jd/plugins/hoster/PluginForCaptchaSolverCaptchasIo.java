package jd.plugins.hoster;

import java.util.ArrayList;
import java.util.Currency;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.SolverStatus;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CloudflareTurnstileChallenge;
import org.jdownloader.captcha.v2.challenge.hcaptcha.HCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.CaptchaResponse;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.ImageCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.TokenCaptchaResponse;
import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolver;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfigCaptchasIo;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountUnavailableException;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

/**
 * Base plugin class for captcha solving via captchas.io. <br>
 * Despite its name suggesting a 2captcha-compatible APIv2 (JSON, createTask/getTaskResult), captchas.io actually emulates the legacy
 * 2captcha/rucaptcha/anti-captcha API (in.php/res.php, status/request fields), see: https://api.captchas.io/document/#/endpoints and
 * https://api.captchas.io/document/#/migration <br>
 * Docs: https://api.captchas.io/document/#/methods <br>
 * Formerly known as captchasolutions.com
 */
@HostPlugin(revision = "$Revision: 53502 $", interfaceVersion = 3, names = { "captchas.io" }, urls = { "" })
public class PluginForCaptchaSolverCaptchasIo extends abstractPluginForCaptchaSolver {
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
        final List<CAPTCHA_TYPE> types = new ArrayList<CAPTCHA_TYPE>();
        types.add(CAPTCHA_TYPE.IMAGE);
        /**
         * 2026-09-23: Removed IMAGE_SINGLE_CLICK_CAPTCHA/IMAGE_MULTI_CLICK_CAPTCHA: captchas.io's API does not offer a
         * coordinates/click-captcha method, see: https://api.captchas.io/document/#/methods
         */
        types.add(CAPTCHA_TYPE.RECAPTCHA_V3);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2_ENTERPRISE);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2_INVISIBLE);
        /**
         * 2026-09-23: hCaptcha is only possible for special accounts, see: <br>
         * https://api.captchas.io/document/#/specials?id=hcaptcha
         */
        // types.add(CAPTCHA_TYPE.HCAPTCHA);
        types.add(CAPTCHA_TYPE.CLOUDFLARE_TURNSTILE);
        // Supported by them but not by JDownloader
        /* types.add(CAPTCHA_TYPE.MT_CAPTCHA); */
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

    @Override
    public int getServerSideMaxSimultaneousCaptchaThreadsLimit(final Account account) {
        /**
         * captchas.io limits every account to 5 simultaneous captchas server-side for "basic" accounts. <br>
         * At this moment it is not possible to obtain the users' account-type via API so we'll just use "5" for all added captchas.io
         * accounts.
         */
        return 5;
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        /* Undocumented but present in every rucaptcha/2captcha-legacy-compatible API that captchas.io emulates. */
        final UrlQuery query = new UrlQuery();
        query.appendEncoded("key", account.getPass());
        query.appendEncoded("action", "getbalance");
        query.appendEncoded("json", "1");
        br.getPage(this.getApiBase() + "/res.php?" + query.toString());
        final Map<String, Object> entries = this.handleAPIErrors(br);
        final double balance = Double.parseDouble(entries.get("request").toString());
        final AccountInfo ai = new AccountInfo();
        ai.setAccountBalance(balance, Currency.getInstance("USD"));
        return ai;
    }

    @Override
    public void solve(final CESSolverJob<?> job, final Account account) throws Exception {
        final Challenge<?> captchachallenge = job.getChallenge();
        job.setStatus(SolverStatus.UPLOADING);
        final String apikey = account.getPass();
            final UrlQuery query = new UrlQuery();
            query.appendEncoded("key", apikey);
            query.appendEncoded("json", "1");
            if (captchachallenge instanceof RecaptchaV2Challenge) {
                /* https://api.captchas.io/document/#/methods?id=recaptcha-v2 + ?id=recaptcha-v3 + ?id=recaptcha-enterprise */
                final RecaptchaV2Challenge challenge = (RecaptchaV2Challenge) captchachallenge;
                query.appendEncoded("method", "userrecaptcha");
                query.appendEncoded("googlekey", challenge.getSiteKey());
                query.appendEncoded("pageurl", challenge.getSiteUrl(this));
                final Map<String, Object> action = challenge.getV3Action();
                if (challenge.isEnterprise()) {
                    query.appendEncoded("enterprise", "1");
                    query.appendEncoded("version", "v2");
                } else if (challenge.isV3() || action != null) {
                    query.appendEncoded("version", "v3");
                    query.appendEncoded("invisible", challenge.isInvisible() ? "1" : "0");
                } else {
                    query.appendEncoded("version", "v2");
                }
                if (action != null) {
                    query.appendEncoded("action", (String) action.get("action"));
                }
                final Double minScore = challenge.getMinScore();
                if (minScore != null) {
                    query.appendEncoded("min_score", minScore.toString());
                }
            } else if (captchachallenge instanceof HCaptchaChallenge) {
                /* https://api.captchas.io/document/#/specials?id=hcaptcha */
                final HCaptchaChallenge challenge = (HCaptchaChallenge) captchachallenge;
                query.appendEncoded("method", "hcaptcha");
                query.appendEncoded("sitekey", challenge.getSiteKey());
                query.appendEncoded("pageurl", challenge.getSiteUrl(this));
            } else if (captchachallenge instanceof CloudflareTurnstileChallenge) {
                /* https://api.captchas.io/document/#/methods?id=turnstile */
                final CloudflareTurnstileChallenge challenge = (CloudflareTurnstileChallenge) captchachallenge;
                query.appendEncoded("method", "turnstile");
                query.appendEncoded("sitekey", challenge.getSiteKey());
                query.appendEncoded("pageurl", challenge.getSiteUrl(this));
            } else if (captchachallenge instanceof ImageCaptchaChallenge) {
                /* https://api.captchas.io/document/#/endpoints?id=_1nbspinphp-send-or-upload */
                final ImageCaptchaChallenge<String> challenge = (ImageCaptchaChallenge<String>) captchachallenge;
                query.appendEncoded("method", "base64");
                query.appendEncoded("body", challenge.getBase64ImageFile());
            } else {
                throw new IllegalArgumentException("Unexpected captcha challenge type");
            }
            /* Submit captcha */
            br.postPage(this.getApiBase() + "/in.php", query);
            Map<String, Object> entries = this.handleAPIErrors(br);
            final String id = entries.get("request").toString();
            /* Wait for captcha answer */
            job.setStatus(SolverStatus.SOLVING);
            final UrlQuery pollQuery = new UrlQuery();
            pollQuery.appendEncoded("key", apikey);
            pollQuery.appendEncoded("action", "get");
            pollQuery.appendEncoded("json", "1");
            pollQuery.appendEncoded("id", id);
            final String pollUrl = this.getApiBase() + "/res.php?" + pollQuery.toString();
            while (job.getJob().isAlive() && !job.getJob().isSolved()) {
                waitDuringPolling(job.getChallenge(), account);
                br.getPage(pollUrl);
                entries = this.handleAPIErrors(br);
                final String answer = entries.get("request").toString();
                if (answer.equals("CAPCHA_NOT_READY")) {
                    /* Not yet ready */
                    continue;
                }
                final AbstractResponse resp;
                if (captchachallenge instanceof ImageCaptchaChallenge) {
                    resp = new CaptchaResponse((Challenge<String>) captchachallenge, job.getSolver(), answer);
                } else {
                    resp = new TokenCaptchaResponse((Challenge<String>) captchachallenge, job.getSolver(), answer);
                }
                resp.setCaptchaSolverTaskID(id);
                job.setAnswer(resp);
                return;
            }
    }

    @Override
    public boolean setInvalid(final AbstractResponse<?> response, final Account account) {
        /* captchas.io does not offer any captcha feedback (report correct/incorrect) API method. */
        return false;
    }

    @Override
    public boolean setValid(final AbstractResponse<?> response, final Account account) {
        /* captchas.io does not offer any captcha feedback (report correct/incorrect) API method. */
        return false;
    }

    /** See docs: https://api.captchas.io/document/#/errors */
    protected Map<String, Object> handleAPIErrors(final Browser br) throws Exception {
        final Map<String, Object> entries;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException ignore) {
            /* This should never happen. */
            final String msg = "Invalid API response";
            final long wait = 1 * 60 * 1000;
            throw new AccountUnavailableException(msg, wait);
        }
        final int status = ((Number) entries.get("status")).intValue();
        if (status == 1) {
            /* No error */
            return entries;
        }
        final String request = entries.get("request").toString();
        if (request.equals("CAPCHA_NOT_READY")) {
            /* Not an error, just not ready yet -> caller will keep polling. */
            return entries;
        }
        final HashSet<String> accountErrorsPermanent = new HashSet<String>();
        accountErrorsPermanent.add("ERROR_API_KEY_NOT_FOUND");
        accountErrorsPermanent.add("ERROR_ACCESS_DENIED");
        final HashSet<String> accountErrorsTemp = new HashSet<String>();
        accountErrorsTemp.add("ERROR_DAILY_SOLVES_LIMIT_REACHED");
        accountErrorsTemp.add("ERROR_NO_AVAILABLE_THREADS");
        final String errorMessage = getHumanReadableErrorMessage(request);
        if (accountErrorsPermanent.contains(request)) {
            throw new AccountInvalidException(errorMessage);
        } else if (accountErrorsTemp.contains(request)) {
            throw new AccountUnavailableException(errorMessage, 5 * 60 * 1000);
        } else {
            throw new PluginException(LinkStatus.ERROR_CAPTCHA, errorMessage);
        }
    }

    /**
     * captchas.io's legacy-style API only returns an enum-like error key (no separate human-readable description field, unlike the
     * 2captcha APIv2 family), so this hardcodes the descriptions from their docs: https://api.captchas.io/document/#/errors
     */
    private static String getHumanReadableErrorMessage(final String errorCode) {
        if ("ERROR_DAILY_SOLVES_LIMIT_REACHED".equals(errorCode)) {
            return "You have reached your account's allowable daily solves limit. Wait or upgrade your package plan.";
        } else if ("ERROR_NO_AVAILABLE_THREADS".equals(errorCode)) {
            return "You have no more available threads in your account's subscribed plan. Wait for a free thread or upgrade your package plan.";
        } else if ("ERROR_CAPTCHA_UNSOLVABLE".equals(errorCode)) {
            return "The captcha was found to be unsolvable.";
        } else if ("ERROR_API_KEY_NOT_FOUND".equals(errorCode)) {
            return "The API key was not found.";
        } else if ("ERROR_SITEKEY_NOT_FOUND".equals(errorCode)) {
            return "The captcha sitekey was not found or is empty.";
        } else if ("ERROR_ACCESS_DENIED".equals(errorCode)) {
            return "Access to the API was denied.";
        } else {
            return errorCode;
        }
    }
}
