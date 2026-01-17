package jd.plugins.hoster;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.challenge.clickcaptcha.ClickCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.cutcaptcha.CutCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.hcaptcha.HCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.multiclickcaptcha.MultiClickCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.ImageCaptchaChallenge;
import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolver;
import org.jdownloader.plugins.components.config.CaptchaSolverNinekwConfig;
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
@HostPlugin(revision = "$Revision: 52110 $", interfaceVersion = 3, names = { "9kw.eu" }, urls = { "" })
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

    /**
     * Returns the list of captcha types supported by this solver.
     *
     * @return List of supported captcha types
     */
    @Override
    public List<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
        List<CAPTCHA_TYPE> types = new ArrayList<CAPTCHA_TYPE>();
        types.add(CAPTCHA_TYPE.IMAGE);
        types.add(CAPTCHA_TYPE.IMAGE_SINGLE_CLICK_CAPTCHA);
        types.add(CAPTCHA_TYPE.IMAGE_MULTI_CLICK_CAPTCHA);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2_INVISIBLE);
        types.add(CAPTCHA_TYPE.HCAPTCHA);
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
        return false;
    }

    @Override
    public boolean setValid(AbstractResponse<?> response, Account account) {
        return false;
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
        query.appendEncoded("jd", "2");
        query.appendEncoded("source", "jd2");
        query.appendEncoded("captchaSource", "jdPlugin");
        query.appendEncoded("version", "1.2");
        br.getPage(getBaseURL() + "/index.cgi?" + query.toString());
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
        final UrlQuery q = new UrlQuery();
        q.appendEncoded("action", "usercaptchaupload");
        final Challenge<?> captchachallenge = job.getChallenge();
        final Map<String, Object> task = new HashMap<String, Object>(); // APIv2
        if (captchachallenge instanceof RecaptchaV2Challenge) {
            final RecaptchaV2Challenge challenge = (RecaptchaV2Challenge) captchachallenge;
            q.appendEncoded("data-sitekey", challenge.getSiteKey());
            q.appendEncoded("oldsource", challenge.getTypeID() + "");
            q.appendEncoded("isInvisible", challenge.isInvisible() == true ? "1" : "0");
            final Map<String, Object> v3action = challenge.getV3Action();
            if (v3action != null) {
                q.appendEncoded("pageurl", challenge.getSiteUrl());
                q.appendEncoded("captchachoice", "recaptchav3");
                q.appendEncoded("actionname", (String) v3action.get("action"));
                q.appendEncoded("min_score", "0.3");// minimal score
            } else {
                // if (options.isSiteDomain()) {
                // query.appendEncoded("pageurl", rcChallenge.getSiteDomain());
                // } else {
                // query.appendEncoded("pageurl", rcChallenge.getSiteUrl());
                // }
                q.appendEncoded("pageurl", challenge.getSiteUrl());
                q.appendEncoded("captchachoice", "recaptchav2");
            }
            q.appendEncoded("securetoken", challenge.getSecureToken());
            q.appendEncoded("interactive", "1");
        } else if (captchachallenge instanceof HCaptchaChallenge) {
            final HCaptchaChallenge challenge = (HCaptchaChallenge) captchachallenge;
            q.appendEncoded("data-sitekey", challenge.getSiteKey());
            q.appendEncoded("pageurl", challenge.getSiteUrl());
            q.appendEncoded("oldsource", "hcaptcha");
            q.appendEncoded("captchachoice", "hcaptcha");
            q.appendEncoded("interactive", "1");
        } else if (captchachallenge instanceof CutCaptchaChallenge) {
            /* CutCaptcha: https://2captcha.com/api-docs/cutcaptcha */
            final CutCaptchaChallenge challenge = (CutCaptchaChallenge) captchachallenge;
            task.put("type", "CutCaptchaTaskProxyless");
            task.put("miseryKey", challenge.getSiteKey());
            task.put("apiKey", challenge.getApiKey());
            task.put("websiteURL", challenge.getSiteUrl());
        } else if (captchachallenge instanceof ClickCaptchaChallenge) {
            /* Coordinates task: https://2captcha.com/api-docs/coordinates */
            final ClickCaptchaChallenge challenge = (ClickCaptchaChallenge) captchachallenge;
            q.appendEncoded("mouse", "1");
            q.appendEncoded("base64", "1");
            q.appendEncoded("file-upload-01", challenge.getBase64ImageFile());
        } else if (captchachallenge instanceof MultiClickCaptchaChallenge) {
            /* Coordinates task: https://2captcha.com/api-docs/coordinates */
            final MultiClickCaptchaChallenge challenge = (MultiClickCaptchaChallenge) captchachallenge;
            q.appendEncoded("multimouse", "1");
            q.appendEncoded("base64", "1");
            q.appendEncoded("file-upload-01", challenge.getBase64ImageFile());
        } else if (captchachallenge instanceof ImageCaptchaChallenge) {
            /* Image captcha: https://2captcha.com/api-docs/normal-captcha */
            final ImageCaptchaChallenge challenge = (ImageCaptchaChallenge<String>) job.getChallenge();
            q.appendEncoded("base64", "1");
            q.appendEncoded("file-upload-01", challenge.getBase64ImageFile());
        } else {
            throw new IllegalArgumentException("Unexpected captcha challenge type");
        }
        q.appendEncoded("maxtimeout", Math.min(60, captchachallenge.getTimeout()) + "");
        if (captchachallenge.getExplain() != null) {
            q.appendEncoded("textinstructions", captchachallenge.getExplain());
        }
        try {
            final Map<String, Object> uploadresp = this.callAPI(q, account);
            // TODO: Add captcha check loop
        } catch (IOException e) {
            e.printStackTrace();
        }
        final CaptchaSolverNinekwConfig cfg = PluginJsonConfig.get(CaptchaSolverNinekwConfig.class);
        q.appendEncoded("captchaperhour", cfg.getHour() + "");
        q.appendEncoded("captchapermin", cfg.getMinute() + "");
        q.appendEncoded("prio", cfg.getPrio() + "");
        q.appendEncoded("selfsolve", cfg.isSelfsolve() + "");
        q.appendEncoded("confirm", cfg.isConfirm() + "");
        // q.appendEncoded("maxtimeout", cfg.tt + "");
    }

    @Override
    public Class<? extends CaptchaSolverNinekwConfig> getConfigInterface() {
        return CaptchaSolverNinekwConfig.class;
    }
}