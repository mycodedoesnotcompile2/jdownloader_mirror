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
import org.jdownloader.captcha.v2.SolverStatus;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CloudflareTurnstileChallenge;
import org.jdownloader.captcha.v2.challenge.cutcaptcha.CutCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.hcaptcha.HCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.CaptchaResponse;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.ImageCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.TokenCaptchaResponse;
import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolver;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfigCheapcaptchaCom;
import org.jdownloader.plugins.controller.LazyPlugin;

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

@HostPlugin(revision = "$Revision: 52194 $", interfaceVersion = 3, names = { "cheapcaptcha.com" }, urls = { "" })
public class PluginForCaptchaSolverCheapcaptchaCom extends abstractPluginForCaptchaSolver {
    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.CAPTCHA_SOLVER, LazyPlugin.FEATURE.BUBBLE_NOTIFICATION };
    }

    public PluginForCaptchaSolverCheapcaptchaCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.getHeaders().put("Accept", "application/json");
        br.getHeaders().put("User-Agent", "JDownloader");
        br.setAllowedResponseCodes(200, 400);
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getBuyPremiumUrl() {
        return this.getBaseURL();
    }

    @Override
    public List<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
        final List<CAPTCHA_TYPE> types = new ArrayList<CAPTCHA_TYPE>();
        types.add(CAPTCHA_TYPE.IMAGE);
        // types.add(CAPTCHA_TYPE.RECAPTCHA_V3);
        // types.add(CAPTCHA_TYPE.RECAPTCHA_V2);
        // types.add(CAPTCHA_TYPE.RECAPTCHA_V2_ENTERPRISE);
        // types.add(CAPTCHA_TYPE.RECAPTCHA_V2_INVISIBLE);
        // types.add(CAPTCHA_TYPE.CLOUDFLARE_TURNSTILE);
        // types.add(CAPTCHA_TYPE.CUTCAPTCHA);
        // types.add(CAPTCHA_TYPE.HCAPTCHA);
        // types.add(CAPTCHA_TYPE.MT_CAPTCHA);
        // types.add(CAPTCHA_TYPE.GEETEST_V1);
        // types.add(CAPTCHA_TYPE.GEETEST_V4);
        // types.add(CAPTCHA_TYPE.FRIENDLY_CAPTCHA);
        return types;
    }

    private String getBaseURL() {
        return "https://" + getHost();
    }

    protected String getApiBase() {
        return "http://api." + getHost() + "/api";
    }

    @Override
    public String getAGBLink() {
        return getBaseURL() + "/terms";
    }

    @Override
    public AccountInfo fetchAccountInfo(Account account) throws Exception {
        final String username = account.getUser();
        final String password = account.getPass();
        final UrlQuery query = new UrlQuery();
        query.addAndReplace("username", URLEncode.encodeRFC2396(username));
        query.addAndReplace("password", URLEncode.encodeRFC2396(password));
        final Request req = br.createPostRequest(this.getApiBase() + "/user", query);
        final Map<String, Object> entries = this.callAPI(req);
        final Double creditsInDollarCent = ((Number) entries.get("balance")).doubleValue();
        final Double creditsInDollar = creditsInDollarCent / 100;
        final AccountInfo ai = new AccountInfo();
        ai.setAccountBalance(creditsInDollar, Currency.getInstance("USD"));
        ai.setStatus("Balance: " + ai.getAccountBalanceFormatted() + " | Rate: " + entries.get("rate"));
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
            final PostFormDataRequest r = new PostFormDataRequest(getApiBase() + "/captcha");
            final String username = account.getUser();
            final String password = account.getPass();
            r.addFormData(new FormData("username", username));
            r.addFormData(new FormData("password", password));
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
            } else if (challenge instanceof CutCaptchaChallenge) {
                type = "CutCaptcha";
                final CutCaptchaChallenge cc = (CutCaptchaChallenge) challenge;
                r.addFormData(new FormData("type", "19"));
                final Map<String, Object> cutcaptcha_params = new HashMap<String, Object>();
                cutcaptcha_params.put("apikey", cc.getApiKey());
                cutcaptcha_params.put("miserykey", cc.getSiteKey());
                cutcaptcha_params.put("pageurl", cc.getSiteUrl());
                r.addFormData(new FormData("cutcaptcha_params", JSonStorage.serializeToJson(cutcaptcha_params)));
            } else if (challenge instanceof CloudflareTurnstileChallenge) {
                type = "CloudflareTurnstileCaptcha";
                final CloudflareTurnstileChallenge cc = (CloudflareTurnstileChallenge) challenge;
                r.addFormData(new FormData("type", "12"));
                final Map<String, Object> turnstile_params = new HashMap<String, Object>();
                turnstile_params.put("sitekey", cc.getSiteKey());
                turnstile_params.put("pageurl", cc.getSiteUrl());
                r.addFormData(new FormData("turnstile_params", JSonStorage.serializeToJson(turnstile_params)));
            } else if (challenge instanceof ImageCaptchaChallenge) {
                type = "Image";
                final ImageCaptchaChallenge bcc = (ImageCaptchaChallenge) challenge;
                final BufferedImage image = ImageProvider.read(bcc.getImageFile());
                final byte[] bytes = IconIO.toJpgBytes(image);
                r.addFormData(new FormData("captchafile", "captcha", "application/octet-stream", bytes));
            } else {
                throw new IllegalArgumentException("Unexpected captcha challenge type");
            }
            br.getPage(r);
            final Map<String, Object> uploadresp = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final int captchaID = ((Number) uploadresp.get("captcha")).intValue();
            if (captchaID <= 0) {
                throw new PluginException(LinkStatus.ERROR_CAPTCHA, "Failed to upload captcha");
            }
            job.setStatus(SolverStatus.SOLVING);
            long startTime = System.currentTimeMillis();
            Map<String, Object> pollresp = null;
            while (true) {
                this.sleep(this.getPollingIntervalMillis(account), null);
                br.getPage(getApiBase() + "/captcha/" + captchaID);
                pollresp = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                final int status = ((Number) pollresp.get("status")).intValue();
                final boolean is_correct = ((Boolean) pollresp.get("is_correct")).booleanValue();
                if (is_correct) {
                    break;
                }
                if (status == 255) {
                    throw new PluginException(LinkStatus.ERROR_CAPTCHA, "Captcha solution incorrect");
                } else if (status != 0) {
                    throw new PluginException(LinkStatus.ERROR_CAPTCHA, "Captcha solve error: status " + status);
                } else if (System.currentTimeMillis() - startTime > 60 * 60 * 1000) {
                    throw new PluginException(LinkStatus.ERROR_CAPTCHA, "Captcha solve timeout");
                }
            }
            final String solution = (String) pollresp.get("text");
            job.getLogger().info("CAPTCHA(" + type + ") solved: " + solution);
            AbstractResponse resp = null;
            if (challenge instanceof RecaptchaV2Challenge || challenge instanceof HCaptchaChallenge || challenge instanceof CloudflareTurnstileChallenge || challenge instanceof CutCaptchaChallenge) {
                resp = new TokenCaptchaResponse((Challenge<String>) challenge, this, solution);
            } else {
                resp = new CaptchaResponse((Challenge<String>) challenge, this, solution);
            }
            resp.setCaptchaSolverTaskID(Integer.toString(captchaID));
            job.setAnswer(resp);
            return;
        } catch (Exception e) {
            // TODO
            // challenge.sendStatsError(this, e);
            throw e;
        }
    }

    @Override
    public boolean setInvalid(AbstractResponse<?> response, Account account) {
        UrlQuery query = new UrlQuery();
        query.addAndReplace("password", URLEncode.encodeRFC2396(account.getPass())).addAndReplace("username", URLEncode.encodeRFC2396(account.getUser()));
        try {
            final Request req = br.createPostRequest(this.getApiBase() + "/captcha/" + response.getCaptchaSolverTaskID() + "/report", query);
            this.callAPI(req);
            return true;
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }

    private Map<String, Object> callAPI(final Request req) throws IOException, PluginException {
        br.getPage(req);
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        if (Boolean.TRUE.equals(entries.get("is_banned"))) {
            throw new AccountInvalidException("Account is banned");
        }
        final String error = (String) entries.get("error");
        final int status = ((Number) entries.get("status")).intValue();
        if (status == 0) {
            /* No error */
            return entries;
        }
        if (error == null) {
            return entries;
        }
        throw new AccountInvalidException(error);
    }

    @Override
    public Class<? extends CaptchaSolverPluginConfigCheapcaptchaCom> getConfigInterface() {
        return CaptchaSolverPluginConfigCheapcaptchaCom.class;
    }
}