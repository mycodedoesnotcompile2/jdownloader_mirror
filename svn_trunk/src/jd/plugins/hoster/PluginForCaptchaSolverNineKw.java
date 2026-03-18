package jd.plugins.hoster;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.ChallengeSolver.FeedbackType;
import org.jdownloader.captcha.v2.SolverStatus;
import org.jdownloader.captcha.v2.challenge.clickcaptcha.ClickCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.clickcaptcha.ClickedPoint;
import org.jdownloader.captcha.v2.challenge.cutcaptcha.CutCaptchaChallenge;
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
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

/**
 * Plugin for 9kw captcha solving service (https://9kw.eu/).
 */
@HostPlugin(revision = "$Revision: 52511 $", interfaceVersion = 3, names = { "9kw.eu" }, urls = { "" })
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
        query.appendEncoded("correct", "");
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
            // TODO: Check for account related problems too
            final String error_code = non_json_error_regex.getMatch(0);
            final String error_msg = non_json_error_regex.getMatch(1);
            throw new PluginException(LinkStatus.ERROR_CAPTCHA, error_msg);
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
        final int errorcode = Integer.parseInt(errorNumber);
        if (errorcode == 1 || errorcode == 2 || errorcode == 3 || errorcode == 4 || errorcode == 5 || errorcode == 11 || errorcode == 26 || errorcode == 30 || errorcode == 31 || errorcode == 32) {
            throw new AccountInvalidException(error);
        } else {
            /* Captcha error */
            throw new PluginException(LinkStatus.ERROR_CAPTCHA, error);
        }
    }

    @Override
    public void solve(CESSolverJob<?> job, Account account) throws Exception {
        final UrlQuery upload_query = new UrlQuery();
        upload_query.appendEncoded("action", "usercaptchaupload");
        final Challenge<?> captchachallenge = job.getChallenge();
        final Map<String, Object> task = new HashMap<String, Object>(); // APIv2
        if (captchachallenge instanceof RecaptchaV2Challenge) {
            final RecaptchaV2Challenge challenge = (RecaptchaV2Challenge) captchachallenge;
            upload_query.appendEncoded("data-sitekey", challenge.getSiteKey());
            upload_query.appendEncoded("isInvisible", challenge.isInvisible() == true ? "1" : "0");
            /** TODO: Remove parameter "captchachoice": it is not in the docs anymore and parameter "oldsource" should be enough. */
            final Map<String, Object> v3action = challenge.getV3Action();
            if (v3action != null) {
                upload_query.appendEncoded("pageurl", challenge.getSiteUrl(this));
                upload_query.appendEncoded("captchachoice", "recaptchav3");
                upload_query.appendEncoded("actionname", (String) v3action.get("action"));
                upload_query.appendEncoded("min_score", "0.3");// minimal score
            } else {
                // if (options.isSiteDomain()) {
                // query.appendEncoded("pageurl", rcChallenge.getSiteDomain());
                // } else {
                // query.appendEncoded("pageurl", rcChallenge.getSiteUrl());
                // }
                upload_query.appendEncoded("pageurl", challenge.getSiteUrl(this));
                upload_query.appendEncoded("captchachoice", "recaptchav2");
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
        } else if (captchachallenge instanceof CutCaptchaChallenge) {
            /* CutCaptcha: https://2captcha.com/api-docs/cutcaptcha */
            final CutCaptchaChallenge challenge = (CutCaptchaChallenge) captchachallenge;
            task.put("type", "CutCaptchaTaskProxyless");
            task.put("miseryKey", challenge.getSiteKey());
            task.put("apiKey", challenge.getApiKey());
            task.put("websiteURL", challenge.getSiteUrl(this));
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
        upload_query.appendEncoded("maxtimeout", Math.min(60, captchachallenge.getTimeout()) + "");
        if (captchachallenge.getExplain() != null) {
            upload_query.appendEncoded("textinstructions", captchachallenge.getExplain());
        }
        final CaptchaSolverPluginConfigNinekw cfg = PluginJsonConfig.get(this.getConfigInterface());
        upload_query.appendEncoded("captchaperhour", cfg.getHour() + "");
        upload_query.appendEncoded("captchapermin", cfg.getMinute() + "");
        upload_query.appendEncoded("prio", cfg.getPrio() + "");
        upload_query.appendEncoded("selfsolve", cfg.isSelfsolve() + "");
        upload_query.appendEncoded("confirm", cfg.isConfirm() + "");
        final Map<String, Object> uploadresp = this.callAPI(upload_query, account);
        final String captcha_id = uploadresp.get("captchaid").toString();
        final UrlQuery polling_query = new UrlQuery();
        polling_query.appendEncoded("action", "usercaptchacorrectdata");
        polling_query.appendEncoded("id", captcha_id);
        // q.appendEncoded("maxtimeout", cfg.tt + "");
        /* Wait for captcha answer */
        job.setStatus(SolverStatus.SOLVING);
        while (job.getJob().isAlive() && !job.getJob().isSolved()) {
            checkInterruption();
            Thread.sleep(getPollingIntervalMillis(account));
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
            if (captchachallenge instanceof RecaptchaV2Challenge || captchachallenge instanceof HCaptchaChallenge || captchachallenge instanceof CutCaptchaChallenge) {
                resp = new TokenCaptchaResponse((Challenge<String>) captchachallenge, this, answer);
            } else if (captchachallenge instanceof ClickCaptchaChallenge) {
                // TODO: Test this
                final String[] splitResult = answer.split("x");
                final ClickCaptchaChallenge challenge = (ClickCaptchaChallenge) captchachallenge;
                final ClickedPoint cp = new ClickedPoint(Integer.parseInt(splitResult[0]), Integer.parseInt(splitResult[1]));
                resp = new ClickCaptchaResponse(challenge, this, cp);
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
                resp = new MultiClickCaptchaResponse(challenge, this, new MultiClickedPoint(x, y));
            } else {
                resp = new CaptchaResponse((Challenge<String>) captchachallenge, this, answer);
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