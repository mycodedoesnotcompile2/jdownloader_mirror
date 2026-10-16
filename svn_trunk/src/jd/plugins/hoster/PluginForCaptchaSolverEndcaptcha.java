package jd.plugins.hoster;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.EnumSet;
import java.util.Set;
import java.util.ArrayList;
import java.util.Currency;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.appwork.storage.JSonStorage;
import org.appwork.utils.ImageProvider.ImageProvider;
import org.appwork.utils.images.IconIO;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.ChallengeSolver.FeedbackType;
import org.jdownloader.captcha.v2.SolverStatus;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.CaptchaResponse;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.ImageCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.TokenCaptchaResponse;
import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.captcha.v2.solver.jac.SolverException;
import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolver;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfigEndcaptcha;

import jd.PluginWrapper;
import jd.http.Request;
import jd.http.requests.FormData;
import jd.http.requests.PostFormDataRequest;
import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.HostPlugin;
import jd.plugins.PluginException;

@HostPlugin(revision = "$Revision: 53649 $", interfaceVersion = 3, names = { "endcaptcha.com" }, urls = { "" })
public class PluginForCaptchaSolverEndcaptcha extends abstractPluginForCaptchaSolver {
    public PluginForCaptchaSolverEndcaptcha(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public String getBuyPremiumUrl() {
        return "https://" + getHost() + "/";
    }

    @Override
    public Set<FeedbackType> getSupportedFeedbackTypes() {
        return EnumSet.of(FeedbackType.REPORT_INVALID_CAPTCHAS);
    }

    @Override
    public Set<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
        final Set<CAPTCHA_TYPE> types = EnumSet.noneOf(CAPTCHA_TYPE.class);
        types.add(CAPTCHA_TYPE.IMAGE);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V3);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2);
        /**
         * 2026-01-22: reCaptcha enterprise doesn't look to be supported according to their API docs
         */
        // types.add(CAPTCHA_TYPE.RECAPTCHA_V2_ENTERPRISE);
        /** 2026-10-14: Looks to be broken: API replies with "?" (plaintext questionmark) during polling (wtf!) */
        // types.add(CAPTCHA_TYPE.RECAPTCHA_V2_INVISIBLE);
        return types;
    }

    private String getBaseURL() {
        return "https://" + getHost();
    }

    protected String getApiBase() {
        /* 2026-01-22: API doesn't support https. */
        return "http://api." + getHost();
    }

    @Override
    public String getAGBLink() {
        return getBaseURL() + "/";
    }

    @Override
    public AccountInfo fetchAccountInfo(Account account) throws Exception {
        final UrlQuery query = new UrlQuery(true);
        query.append("username", account.getUser(), true);
        query.append("password", account.getPass(), true);
        final Request req = br.createPostRequest(this.getApiBase() + "/balance", query);
        this.callAPI(req);
        final Double creditsInDollarCent = Double.parseDouble(br.getRequest().getHtmlCode());
        final Double creditsInDollar = creditsInDollarCent / 100;
        final AccountInfo ai = new AccountInfo();
        ai.setAccountBalance(creditsInDollar, Currency.getInstance("USD"));
        return ai;
    }

    @Override
    public void solve(CESSolverJob<?> job, Account account) throws Exception {
        final Challenge<?> challenge = job.getChallenge();
        challenge.sendStatsSolving(job.getSolver());
        try {
            job.setStatus(SolverStatus.UPLOADING);
            final PostFormDataRequest r = new PostFormDataRequest(getApiBase() + "/upload");
            r.addFormData(new FormData("username", account.getUser()));
            r.addFormData(new FormData("password", account.getPass()));
            final String type;
            if (challenge instanceof RecaptchaV2Challenge) {
                final RecaptchaV2Challenge rc_challenge = (RecaptchaV2Challenge) challenge;
                final Map<String, Object> token_param = new HashMap<String, Object>();
                token_param.put("googlekey", rc_challenge.getSiteKey());
                token_param.put("pageurl", rc_challenge.getSiteUrl(this));
                final Map<String, Object> v3action = rc_challenge.getV3Action();
                if (v3action != null) {
                    type = "RecaptchaV3";
                    r.addFormData(new FormData("type", "5"));
                    token_param.put("action", v3action.get("action"));
                    token_param.put("min_score", getMinScoreOrDefault(rc_challenge));
                } else {
                    if (rc_challenge.isV3()) {
                        type = "RecaptchaV3";
                        r.addFormData(new FormData("type", "5"));
                        token_param.put("min_score", getMinScoreOrDefault(rc_challenge));
                    } else if (rc_challenge.isInvisible()) {
                        type = "RecaptchaV2 invisible";
                        r.addFormData(new FormData("type", "4"));
                    } else {
                        type = "RecaptchaV2";
                        r.addFormData(new FormData("type", "4"));
                    }
                }
                r.addFormData(new FormData("token_params", JSonStorage.serializeToJson(token_param)));
            } else if (challenge instanceof ImageCaptchaChallenge) {
                type = "Image";
                final ImageCaptchaChallenge bcc = (ImageCaptchaChallenge) challenge;
                final BufferedImage image = ImageProvider.read(bcc.getImageFile());
                final byte[] bytes = IconIO.toJpgBytes(image);
                final String base64 = new String(org.appwork.utils.encoding.Base64.encodeToByte(bytes, false));
                r.addFormData(new FormData("image", "base64:" + base64));
            } else {
                throw new IllegalArgumentException("Unexpected captcha challenge type");
            }
            this.callAPI(r);
            /*
             * The upload endpoint either returns the solution right away (captcha was solved while waiting for the server response) or
             * "UNSOLVED_YET:/poll/<id>". In the first case there is no captcha ID.
             */
            String captchaID = this.findCaptchaID();
            if (captchaID != null) {
                job.setStatus(SolverStatus.SOLVING);
                /*
                 * The poll endpoint returns plain text (NOT JSON): "UNSOLVED_YET:/poll/<id>" while the captcha is still being solved, or
                 * the plain solution text once it is done. Completion is therefore detected by the absence of the "UNSOLVED_YET" marker.
                 * Polling interval is limited by getServerSideMinPollingIntervalMillis().
                 */
                while (true) {
                    waitDuringPolling(job.getChallenge(), account);
                    this.callAPI(br.createGetRequest(this.getApiBase() + "/poll/" + captchaID));
                    if (this.findCaptchaID() == null) {
                        break;
                    }
                }
            }
            final String solution = br.getRequest().getHtmlCode().trim();
            job.getLogger().info("CAPTCHA(" + type + ") solved: " + solution);
            final AbstractResponse resp;
            if (challenge instanceof RecaptchaV2Challenge) {
                resp = new TokenCaptchaResponse((Challenge<String>) challenge, job.getSolver(), solution);
            } else {
                resp = new CaptchaResponse((Challenge<String>) challenge, job.getSolver(), solution);
            }
            if (captchaID != null) {
                resp.setCaptchaSolverTaskID(captchaID);
            }
            job.setAnswer(resp);
            return;
        } catch (Exception e) {
            challenge.sendStatsError(job.getSolver(), e);
            throw e;
        }
    }

    private String findCaptchaID() {
        return br.getRegex("UNSOLVED_YET:/poll/(\\d+)").getMatch(0);
    }

    /** Docs: min_score is "usually 0.3". */
    private Double getMinScoreOrDefault(final RecaptchaV2Challenge challenge) {
        final Double minScore = challenge.getMinScore();
        if (minScore != null) {
            return minScore;
        }
        return 0.3d;
    }

    @Override
    public long getServerSideMinPollingIntervalMillis() {
        /* API docs: "Please don't poll for a CAPTCHA status more than once every 3 seconds." */
        return 3000;
    }

    @Override
    public boolean setInvalid(AbstractResponse<?> response, Account account) {
        /*
         * API docs: https://endcaptcha.com/api-specs (section 4). Refund only possible if the captcha was uploaded less than an hour ago.
         */
        final String captchaID = response.getCaptchaSolverTaskID();
        if (captchaID == null) {
            /* Captcha was solved directly during upload -> no captcha ID available to report it. */
            return false;
        }
        final UrlQuery query = new UrlQuery(true);
        query.append("username", account.getUser(), true);
        query.append("password", account.getPass(), true);
        query.append("captcha_id", captchaID, true);
        try {
            final Request req = br.createPostRequest(this.getApiBase() + "/report", query);
            this.callAPI(req);
            return br.getRequest().getHtmlCode().trim().equalsIgnoreCase("OK");
        } catch (Exception e) {
            /* E.g. ERROR:ALREADY REPORTED, ERROR:INCORRECT CAPTCHA ID, ERROR:SOLVING, CANT REPORT */
            e.printStackTrace();
            return false;
        }
    }

    private void callAPI(final Request req) throws IOException, PluginException, SolverException {
        br.getPage(req);
        final String error = br.getRegex("^ERROR:(.+)").getMatch(0);
        if (error == null) {
            /* No error */
            return;
        }
        /* Check if error is related to login or captcha solving. Docs: https://endcaptcha.com/api-specs */
        if (this.getPluginEnvironment() == PluginEnvironment.ACCOUNT_CHECK) {
            throw new AccountInvalidException(error);
        } else if (error.equalsIgnoreCase("NOT AUTHENTICATED") || error.equalsIgnoreCase("NOT ENOUGH BALANCE") || error.equalsIgnoreCase("INVALD CREDENTIALS") || error.equalsIgnoreCase("INVALID CREDENTIALS")) {
            /* Note: "INVALD CREDENTIALS" (sic) is the spelling used in the API docs for /report. */
            throw new AccountInvalidException(error);
        }
        throw new SolverException(error);
    }

    @Override
    public Class<? extends CaptchaSolverPluginConfigEndcaptcha> getConfigInterface() {
        return CaptchaSolverPluginConfigEndcaptcha.class;
    }
}