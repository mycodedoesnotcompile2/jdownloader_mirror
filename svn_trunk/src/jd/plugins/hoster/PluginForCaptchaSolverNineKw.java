package jd.plugins.hoster;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.PluginChallengeSolver;
import org.jdownloader.captcha.v2.ChallengeSolver.FeedbackType;
import org.jdownloader.captcha.v2.SolverStatus;
import org.jdownloader.captcha.v2.challenge.clickcaptcha.ClickCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.clickcaptcha.ClickedPoint;
import org.jdownloader.captcha.v2.challenge.hcaptcha.HCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.multiclickcaptcha.MultiClickCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.multiclickcaptcha.MultiClickedPoint;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.CaptchaResponse;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.ClickCaptchaResponse;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.ImageCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.MultiClickCaptchaResponse;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.TokenCaptchaResponse;
import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolver;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfigNinekw;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountUnavailableException;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

/**
 * Plugin for 9kw captcha solving service (https://9kw.eu/).
 */
@HostPlugin(revision = "$Revision: 53549 $", interfaceVersion = 3, names = { "9kw.eu" }, urls = { "" })
public class PluginForCaptchaSolverNineKw extends abstractPluginForCaptchaSolver {
    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.CAPTCHA_SOLVER, LazyPlugin.FEATURE.BUBBLE_NOTIFICATION, LazyPlugin.FEATURE.API_KEY_LOGIN };
    }

    public PluginForCaptchaSolverNineKw(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public String getBuyPremiumUrl() {
        return "https://www." + getHost() + "/register.html";
    }

    @Override
    public List<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
        List<CAPTCHA_TYPE> types = new ArrayList<CAPTCHA_TYPE>();
        types.add(CAPTCHA_TYPE.IMAGE);
        types.add(CAPTCHA_TYPE.IMAGE_SINGLE_CLICK_CAPTCHA);
        types.add(CAPTCHA_TYPE.IMAGE_MULTI_CLICK_CAPTCHA);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2_INVISIBLE);
        types.add(CAPTCHA_TYPE.HCAPTCHA);
        // types.add(CAPTCHA_TYPE.GEETEST_V1);
        // types.add(CAPTCHA_TYPE.GEETEST_V4);
        return types;
    }

    @Override
    public List<FeedbackType> getSupportedFeedbackTypes() {
        final List<FeedbackType> types = new ArrayList<FeedbackType>();
        types.add(FeedbackType.REPORT_INVALID_CAPTCHAS);
        types.add(FeedbackType.REPORT_VALID_CAPTCHAS);
        types.add(FeedbackType.ABORT_CAPTCHAS);
        return types;
    }

    protected String getApiBaseV2() {
        return "https://api." + getHost();
    }

    @Override
    public String getAGBLink() {
        return getBaseURL() + "/userapi.html";
    }

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        if (str == null) {
            return false;
        }
        return str.matches("[a-zA-Z0-9]{10,}");
    }

    @Override
    protected String getAPILoginHelpURL() {
        return getBaseURL() + "/enterpage";
    }

    private String getBaseURL() {
        return "https://www." + getHost();
    }

    @Override
    public boolean setInvalid(AbstractResponse<?> response, Account account) {
        return sendCaptchaFeedback(response, account, 2);
    }

    @Override
    public boolean setValid(AbstractResponse<?> response, Account account) {
        return sendCaptchaFeedback(response, account, 1);
    }

    @Override
    public boolean setUnused(AbstractResponse<?> response, Account account) {
        return sendCaptchaFeedback(response, account, 3);
    }

    private boolean sendCaptchaFeedback(AbstractResponse<?> response, Account account, final int correct_value) {
        final UrlQuery query = new UrlQuery();
        query.appendEncoded("action", "usercaptchacorrectback");
        /* 1 = correct, 2 = incorrect, 3 = unused */
        query.appendEncoded("correct", String.valueOf(correct_value));
        query.appendEncoded("id", response.getCaptchaSolverTaskID());
        try {
            final Map<String, Object> resp = this.callAPI(query, account);
            final Map<String, Object> status = (Map<String, Object>) resp.get("status");
            return ((Boolean) status.get("success")).booleanValue();
        } catch (final Exception e) {
            e.printStackTrace();
            return false;
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(Account account) throws Exception {
        final UrlQuery query = new UrlQuery();
        query.appendEncoded("action", "usercaptchaguthaben");
        final Map<String, Object> entries = this.callAPI(query, account);
        final Double credits = ((Number) entries.get("credits")).doubleValue();
        final AccountInfo ai = new AccountInfo();
        ai.setAccountBalance(credits);
        return ai;
    }

    /**
     * Docs: https://www.9kw.eu/api.html#apigeneral-tab
     *
     * @throws PluginException
     */
    private Map<String, Object> callAPI(final UrlQuery query, final Account account) throws IOException, PluginException {
        query.appendEncoded("json", "1");
        query.appendEncoded("apikey", account.getPass());
        /* Potentially unneeded params */
        /**
         * 2026-03-17: Do not add jd=2 parameter as this will make API return non-json responses for some cases but we want json whenever
         * possible. </br>
         * Known effects when this parameter is sent: <br>
         * - Sometimes non-json responses <br>
         * - "captcha_id" field instead of "captchaid" <br>
         */
        // query.appendEncoded("jd", "2");
        query.appendEncoded("source", "jd2");
        query.appendEncoded("captchaSource", "jdPlugin");
        query.appendEncoded("version", "1.2");
        br.getPage(getBaseURL() + "/index.cgi?" + query.toString());
        /* Check for non-json response. This is the best workaround I found in order to "keep things pretty". */
        /* See list of possible errors here: https://www.9kw.eu/api.html#apigeneral-tab */
        final Regex non_json_error_regex = br.getRegex("(\\d{4}) (.+)");
        if (non_json_error_regex.patternFind()) {
            final int error_code = Integer.parseInt(non_json_error_regex.getMatch(0));
            final String error_msg = non_json_error_regex.getMatch(1);
            throwForErrorCode(error_code, error_msg);
        }
        final Regex captcha_upload_success = br.getRegex("OK-(\\d+)");
        if (captcha_upload_success.patternFind()) {
            final Map<String, Object> resp = new HashMap<String, Object>();
            resp.put("captcha_id", captcha_upload_success.getMatch(0));
            return resp;
        }
        final Regex captcha_response_success = br.getRegex("OK-answered-(.+)");
        if (captcha_response_success.patternFind()) {
            final Map<String, Object> resp = new HashMap<String, Object>();
            resp.put("answer", captcha_response_success.getMatch(0));
            return resp;
        }
        if (br.getRequest().getHtmlCode().equalsIgnoreCase("OK")) {
            // TODO: Check if this case still exists
            return null;
        }
        /* Expect json response */
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> status = (Map<String, Object>) entries.get("status");
        if (Boolean.TRUE.equals(status.get("success"))) {
            /* No error */
            return entries;
        }
        final String error = (String) entries.get("error");
        final String errorNumber = new Regex(error, "^(\\d{1,4})").getMatch(0);
        if (errorNumber == null) {
            throw new AccountInvalidException(error);
        }
        throwForErrorCode(Integer.parseInt(errorNumber), error);
        /* Unreachable: throwForErrorCode always throws. */
        throw new WTFException();
    }

    /**
     * Classifies a 9kw error code (see https://www.9kw.eu/api.html#apigeneral-tab) as a permanent account error, a temporary account
     * error, or a plain captcha error, and throws the matching exception. Always throws.
     */
    private void throwForErrorCode(final int errorcode, final String message) throws PluginException {
        switch (errorcode) {
        case 1:
        case 2:
        case 3:
        case 4:
        case 5:
        case 11:
        case 24:
        case 26:
        case 30:
        case 32:
        case 34:
        case 35:
        case 36:
        case 55:
            /*
             * 1-5: no/inactive/deactivated API key or no matching account found. 11 & 24: insufficient balance (two separate error codes
             * for the same underlying problem). 26: terms of service not accepted. 30: user/account not found. 32: account temporarily or
             * permanently restricted by the operator. 34/35: https/http requests are not allowed by the account settings. 36: source not
             * allowed. 55: IP not allowed. All of these need a manual change by the user (or support) -> permanent.
             */
            throw new AccountInvalidException(message);
        case 31:
            /* Account is not yet 24h old -> resolves itself, purely temporary. */
            throw new AccountUnavailableException(message, 24 * 60 * 60 * 1000L);
        case 33:
            /* This plugin/API version is not accepted anymore. Only a JDownloader update helps -> check again later. */
            throw new AccountUnavailableException(message, 60 * 60 * 1000L);
        case 56:
            /* A limit (e.g. captchas per hour/minute of the 9kw account) is reached -> resolves itself. */
            throw new AccountUnavailableException(message, 15 * 60 * 1000L);
        case 15:
            /* Captchas were submitted too quickly (min. 250ms between two requests) -> only a short pause is needed. */
            throw new AccountUnavailableException(message, 10 * 1000L);
        default:
            /* Captcha error */
            throw new PluginException(LinkStatus.ERROR_CAPTCHA, message);
        }
    }

    /**
     * The 9kw API docs recommend the first request for a solution after 5-10 seconds and further requests every few seconds, so do not
     * poll faster than every 5 seconds, no matter what the user configured.
     */
    @Override
    public long getServerSideMinPollingIntervalMillis() {
        return 5000;
    }

    @Override
    public void solve(CESSolverJob<?> job, Account account) throws Exception {
        job.setStatus(SolverStatus.UPLOADING);
        final UrlQuery upload_query = new UrlQuery();
        upload_query.appendEncoded("action", "usercaptchaupload");
        final Challenge<?> captchachallenge = job.getChallenge();
        if (captchachallenge instanceof RecaptchaV2Challenge) {
            final RecaptchaV2Challenge challenge = (RecaptchaV2Challenge) captchachallenge;
            upload_query.appendEncoded("data-sitekey", challenge.getSiteKey());
            upload_query.appendEncoded("isInvisible", challenge.isInvisible() == true ? "1" : "0");
            /* Parameter "captchachoice" is not in the docs (anymore), "oldsource" below is enough. */
            final Map<String, Object> v3action = challenge.getV3Action();
            upload_query.appendEncoded("pageurl", challenge.getSiteUrl(this));
            if (v3action != null) {
                upload_query.appendEncoded("actionname", (String) v3action.get("action"));
                upload_query.appendEncoded("min_score", "0.3");// minimal score
            }
            upload_query.appendEncoded("interactive", "1");
            upload_query.appendEncoded("securetoken", challenge.getSecureToken());
            if (v3action != null || challenge.isV3()) {
                upload_query.appendEncoded("oldsource", "recaptchav3");
            } else {
                upload_query.appendEncoded("oldsource", "recaptchav2");
            }
        } else if (captchachallenge instanceof HCaptchaChallenge) {
            final HCaptchaChallenge challenge = (HCaptchaChallenge) captchachallenge;
            upload_query.appendEncoded("data-sitekey", challenge.getSiteKey());
            upload_query.appendEncoded("pageurl", challenge.getSiteUrl(this));
            upload_query.appendEncoded("oldsource", "hcaptcha");
            upload_query.appendEncoded("interactive", "1");
        } else if (captchachallenge instanceof ClickCaptchaChallenge) {
            /* Coordinates task: https://2captcha.com/api-docs/coordinates */
            final ClickCaptchaChallenge challenge = (ClickCaptchaChallenge) captchachallenge;
            upload_query.appendEncoded("mouse", "1");
            upload_query.appendEncoded("base64", "1");
            upload_query.appendEncoded("file-upload-01", challenge.getBase64ImageFile());
        } else if (captchachallenge instanceof MultiClickCaptchaChallenge) {
            /* Coordinates task: https://2captcha.com/api-docs/coordinates */
            final MultiClickCaptchaChallenge challenge = (MultiClickCaptchaChallenge) captchachallenge;
            upload_query.appendEncoded("multimouse", "1");
            upload_query.appendEncoded("base64", "1");
            upload_query.appendEncoded("file-upload-01", challenge.getBase64ImageFile());
        } else if (captchachallenge instanceof ImageCaptchaChallenge) {
            /* Image captcha: https://2captcha.com/api-docs/normal-captcha */
            final ImageCaptchaChallenge challenge = (ImageCaptchaChallenge<String>) job.getChallenge();
            upload_query.appendEncoded("base64", "1");
            upload_query.appendEncoded("file-upload-01", challenge.getBase64ImageFile());
        } else {
            throw new IllegalArgumentException("Unexpected captcha challenge type");
        }
        /*
         * maxtimeout is in SECONDS. Use the time JDownloader itself waits for this solver (see ChallengeSolver#getFinalTimeoutMillis, what
         * JobRunnable enforces), so 9kw does not give up on the captcha before we do. Limits of the 9kw API: the old JDownloader 9kw
         * setting allowed 75 - 3999 seconds, so keep the value within that range. Before, this was Math.min(60, Challenge#getTimeout()),
         * which is in milliseconds and therefore always resulted in 60 (or -1 for challenges without timeout).
         */
        final long timeoutSeconds = new PluginChallengeSolver<Object>(this, account).getFinalTimeoutMillis() / 1000;
        final long minTimeoutSeconds = 75;
        final long maxTimeoutSeconds = 3999;
        upload_query.appendEncoded("maxtimeout", Math.max(minTimeoutSeconds, Math.min(maxTimeoutSeconds, timeoutSeconds)) + "");
        if (captchachallenge.getExplain() != null) {
            upload_query.appendEncoded("textinstructions", captchachallenge.getExplain());
        }
        final CaptchaSolverPluginConfigNinekw cfg = PluginJsonConfig.get(this.getConfigInterface());
        upload_query.appendEncoded("prio", cfg.getPrio() + "");
        upload_query.appendEncoded("selfsolve", cfg.isSelfsolve() + "");
        upload_query.appendEncoded("confirm", cfg.isConfirm() + "");
        final Map<String, Object> uploadresp = this.callAPI(upload_query, account);
        final String captcha_id = uploadresp.get("captchaid").toString();
        final UrlQuery polling_query = new UrlQuery();
        polling_query.appendEncoded("action", "usercaptchacorrectdata");
        polling_query.appendEncoded("id", captcha_id);
        /* Wait for captcha answer */
        job.setStatus(SolverStatus.SOLVING);
        while (job.getJob().isAlive() && !job.getJob().isSolved()) {
            waitDuringPolling(job.getChallenge(), account);
            final Map<String, Object> pollingresp = this.callAPI(polling_query, account);
            final Number credits = (Number) pollingresp.get("credits");
            if (credits != null) {
                try {
                    account.getAccountInfo().setAccountBalance(credits.doubleValue());
                } catch (final Exception e) {
                }
            }
            final Number try_again = (Number) pollingresp.get("try_again");
            if (try_again != null && try_again.shortValue() == 1) {
                /* {"credits":40628,"message":"OK","try_again":1,"answer":"","status":{"https":1,"success":true}} */
                logger.info("No response yet -> Retry");
                continue;
            }
            final String answer = (String) pollingresp.get("answer");
            if (StringUtils.isEmpty(answer)) {
                /* No error && no retry allowed && no answer -> Unsolved for unknown reasons */
                logger.info("No answer and no retry allowed anymore -> Stopping polling");
                job.setStatus(SolverStatus.UNSOLVED);
                return;
            }
            final AbstractResponse resp;
            if (captchachallenge instanceof RecaptchaV2Challenge || captchachallenge instanceof HCaptchaChallenge) {
                resp = new TokenCaptchaResponse((Challenge<String>) captchachallenge, job.getSolver(), answer);
            } else if (captchachallenge instanceof ClickCaptchaChallenge) {
                // TODO: Test this
                final String[] splitResult = answer.split("x");
                final ClickCaptchaChallenge challenge = (ClickCaptchaChallenge) captchachallenge;
                final ClickedPoint cp = new ClickedPoint(Integer.parseInt(splitResult[0]), Integer.parseInt(splitResult[1]));
                resp = new ClickCaptchaResponse(challenge, job.getSolver(), cp);
            } else if (captchachallenge instanceof MultiClickCaptchaChallenge) {
                // TODO: Test this
                final String[] pairs = answer.split(";"); // e.g. "68x149;81x192"
                final int[] x = new int[pairs.length];
                final int[] y = new int[pairs.length];
                for (int i = 0; i < pairs.length; i++) {
                    final String[] xy = pairs[i].split("x");
                    x[i] = Integer.parseInt(xy[0]);
                    y[i] = Integer.parseInt(xy[1]);
                }
                final MultiClickCaptchaChallenge challenge = (MultiClickCaptchaChallenge) captchachallenge;
                resp = new MultiClickCaptchaResponse(challenge, job.getSolver(), new MultiClickedPoint(x, y));
            } else {
                resp = new CaptchaResponse((Challenge<String>) captchachallenge, job.getSolver(), answer);
            }
            resp.setCaptchaSolverTaskID(captcha_id);
            job.setAnswer(resp);
            return;
        }
    }

    @Override
    public Class<? extends CaptchaSolverPluginConfigNinekw> getConfigInterface() {
        return CaptchaSolverPluginConfigNinekw.class;
    }
}