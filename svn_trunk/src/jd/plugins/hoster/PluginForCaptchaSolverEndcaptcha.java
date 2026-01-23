package jd.plugins.hoster;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Currency;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.ImageProvider.ImageProvider;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.images.IconIO;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.ChallengeSolver.FeedbackType;
import org.jdownloader.captcha.v2.SolverStatus;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CloudflareTurnstileChallenge;
import org.jdownloader.captcha.v2.challenge.cutcaptcha.CutCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.hcaptcha.HCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.CaptchaResponse;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.ImageCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.TokenCaptchaResponse;
import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.captcha.v2.solver.jac.SolverException;
import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolver;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfig;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Request;
import jd.http.requests.FormData;
import jd.http.requests.PostFormDataRequest;
import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

@HostPlugin(revision = "$Revision: 52165 $", interfaceVersion = 3, names = { "endcaptcha.com" }, urls = { "" })
public class PluginForCaptchaSolverEndcaptcha extends abstractPluginForCaptchaSolver {
    public PluginForCaptchaSolverEndcaptcha(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getBuyPremiumUrl() {
        return "https://" + getHost() + "/";
    }

    @Override
    public List<FeedbackType> getSupportedFeedbackTypes() {
        final List<FeedbackType> types = new ArrayList<FeedbackType>();
        types.add(FeedbackType.REPORT_INVALID_CAPTCHAS);
        return types;
    }

    @Override
    public List<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
        final List<CAPTCHA_TYPE> types = new ArrayList<CAPTCHA_TYPE>();
        types.add(CAPTCHA_TYPE.IMAGE);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V3);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2);
        /**
         * 2026-01-22: reCaptcha enterprise doesn't look to be supported according to their API docs
         */
        // types.add(CAPTCHA_TYPE.RECAPTCHA_V2_ENTERPRISE);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2_INVISIBLE);
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
        final UrlQuery query = new UrlQuery();
        query.addAndReplace("username", URLEncode.encodeRFC2396(account.getUser()));
        query.addAndReplace("password", URLEncode.encodeRFC2396(account.getPass()));
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
        // TODO
        // job.showBubble(this);
        try {
            // TODO
            // challenge.sendStatsSolving(this);
            job.setStatus(SolverStatus.UPLOADING);
            final PostFormDataRequest r = new PostFormDataRequest(getApiBase() + "/upload");
            r.addFormData(new FormData("username", account.getUser()));
            r.addFormData(new FormData("password", account.getPass()));
            final String type;
            if (challenge instanceof RecaptchaV2Challenge) {
                final RecaptchaV2Challenge rc_challenge = (RecaptchaV2Challenge) challenge;
                final Map<String, Object> token_param = new HashMap<String, Object>();
                token_param.put("googlekey", rc_challenge.getSiteKey());
                token_param.put("pageurl", rc_challenge.getSiteUrl());
                final Map<String, Object> v3action = rc_challenge.getV3Action();
                if (v3action != null) {
                    type = "RecaptchaV3";
                    r.addFormData(new FormData("type", "5"));
                    token_param.put("action", v3action.get("action"));
                    final Double minScore = rc_challenge.getMinScore();
                    if (minScore != null) {
                        token_param.put("min_score", minScore);
                    }
                } else {
                    if (rc_challenge.isV3()) {
                        type = "RecaptchaV3";
                        r.addFormData(new FormData("type", "5"));
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
            final String captchaID = this.findCaptchaID();
            if (captchaID == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            checkInterruption();
            job.setStatus(SolverStatus.SOLVING);
            Map<String, Object> pollresp = null;
            while (true) {
                this.sleep(this.getPollingIntervalMillis(account), null);
                this.callAPI(br.createGetRequest(this.getApiBase() + "/poll/" + captchaID));
                pollresp = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                final int status = ((Number) pollresp.get("status")).intValue();
                final boolean is_correct = ((Boolean) pollresp.get("is_correct")).booleanValue();
                if (this.findCaptchaID() == null) {
                    break;
                }
            }
            final String solution = br.getRequest().getHtmlCode();
            job.getLogger().info("CAPTCHA(" + type + ") solved: " + solution);
            final AbstractResponse resp;
            if (challenge instanceof RecaptchaV2Challenge || challenge instanceof HCaptchaChallenge || challenge instanceof CloudflareTurnstileChallenge || challenge instanceof CutCaptchaChallenge) {
                resp = new TokenCaptchaResponse((Challenge<String>) challenge, this, solution);
            } else {
                resp = new CaptchaResponse((Challenge<String>) challenge, this, solution);
            }
            resp.setCaptchaSolverTaskID(captchaID);
            job.setAnswer(resp);
            return;
        } catch (Exception e) {
            // TODO
            // challenge.sendStatsError(this, e);
            throw e;
        }
    }

    private String findCaptchaID() {
        return br.getRegex("UNSOLVED_YET:/poll/(\\d+)").getMatch(0);
    }

    @Override
    public boolean setValid(AbstractResponse<?> response, Account account) {
        /* API has no call to report valid captchas, only invalid. */
        return false;
    }

    @Override
    public boolean setInvalid(AbstractResponse<?> response, Account account) {
        /* API docs: https://deathbycaptcha.com/api#api_details_report */
        final UrlQuery query = new UrlQuery();
        query.addAndReplace("username", URLEncode.encodeRFC2396(account.getUser()));
        query.addAndReplace("password", URLEncode.encodeRFC2396(account.getPass()));
        query.addAndReplace("captcha_id", response.getCaptchaSolverTaskID());
        try {
            final Request req = br.createPostRequest(this.getApiBase() + "/report", query);
            this.callAPI(req);
            if (br.getRequest().getHtmlCode().equalsIgnoreCase("OK")) {
                return true;
            } else {
                return false;
            }
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }

    private Map<String, Object> callAPI(final Request req) throws IOException, PluginException, SolverException {
        br.getPage(req);
        final String error = br.getRegex("^ERROR:(.+)").getMatch(0);
        if (error == null) {
            /* No error */
            return null;
        }
        /* Check if error is related to login or captcha solving */
        if (this.getPluginEnvironment() == PluginEnvironment.ACCOUNT_CHECK) {
            throw new AccountInvalidException(error);
        } else if (error.equalsIgnoreCase("NOT AUTHENTICATED") || error.equalsIgnoreCase("NOT AUTHENTICATED")) {
            throw new AccountInvalidException(error);
        }
        throw new SolverException(error);
    }

    @Override
    public Class<? extends CaptchaSolverPluginConfig> getConfigInterface() {
        // TODO: Replace this by custom config for deathbycaptcha, this is just for testing!
        return CaptchaSolverPluginConfig.class;
    }
}