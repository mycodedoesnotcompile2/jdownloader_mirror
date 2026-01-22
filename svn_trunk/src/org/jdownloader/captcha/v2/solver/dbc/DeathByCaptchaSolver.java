package org.jdownloader.captcha.v2.solver.dbc;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.ImageProvider.ImageProvider;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.images.IconIO;
import org.appwork.utils.logging2.LogSource;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.SolverStatus;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CloudflareTurnstileChallenge;
import org.jdownloader.captcha.v2.challenge.cutcaptcha.CutCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.hcaptcha.HCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.BasicCaptchaChallenge;
import org.jdownloader.captcha.v2.solver.CESChallengeSolver;
import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.captcha.v2.solver.jac.SolverException;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.images.NewTheme;
import org.jdownloader.logging.LogController;
import org.jdownloader.settings.staticreferences.CFG_CAPTCHA;
import org.jdownloader.settings.staticreferences.CFG_DBC;

import jd.http.Browser;
import jd.http.requests.FormData;
import jd.http.requests.PostFormDataRequest;

public class DeathByCaptchaSolver extends CESChallengeSolver<String> {
    private DeathByCaptchaSettings            config;
    private static final DeathByCaptchaSolver INSTANCE   = new DeathByCaptchaSolver();
    private ThreadPoolExecutor                threadPool = new ThreadPoolExecutor(0, 1, 30000, TimeUnit.MILLISECONDS, new LinkedBlockingDeque<Runnable>(), Executors.defaultThreadFactory());
    private LogSource                         logger;
    private static final String               API_BASE   = "http://api.dbcapi.me/api";

    public static DeathByCaptchaSolver getInstance() {
        return INSTANCE;
    }

    @Override
    public Class<String> getResultType() {
        return String.class;
    }

    @Override
    public DeathByCaptchaSolverService getService() {
        return (DeathByCaptchaSolverService) super.getService();
    }

    private DeathByCaptchaSolver() {
        super(new DeathByCaptchaSolverService(), Math.max(1, Math.min(25, JsonConfig.create(DeathByCaptchaSettings.class).getThreadpoolSize())));
        getService().setSolver(this);
        config = JsonConfig.create(DeathByCaptchaSettings.class);
        logger = LogController.getInstance().getLogger(DeathByCaptchaSolver.class.getName());
        threadPool.allowCoreThreadTimeOut(true);
    }

    @Override
    protected LogSource getLogger() {
        return logger;
    }

    @Override
    public ChallengeVetoReason getChallengeVetoReason(Challenge<?> c) {
        if (c instanceof BasicCaptchaChallenge) {
            return null;
        } else if (c instanceof RecaptchaV2Challenge) {
            return null;
        } else if (c instanceof CutCaptchaChallenge) {
            return null;
        } else if (c instanceof CloudflareTurnstileChallenge) {
            return null;
        } else {
            return ChallengeVetoReason.UNSUPPORTED_BY_SOLVER;
        }
    }

    @Override
    protected void solveCES(final CESSolverJob<String> job) throws InterruptedException, SolverException {
        final Challenge<?> challenge = job.getChallenge();
        job.showBubble(this, getBubbleTimeout(challenge));
        checkInterruption();
        try {
            challenge.sendStatsSolving(this);
            job.setStatus(SolverStatus.UPLOADING);
            final Browser br = createNewBrowserInstance(challenge);
            final PostFormDataRequest r = new PostFormDataRequest(API_BASE + "/captcha");
            final String username = config.getUserName();
            final String password = config.getPassword();
            if (StringUtils.isEmpty(username)) {
                r.addFormData(new FormData("authtoken", password));
            } else {
                r.addFormData(new FormData("username", username));
                r.addFormData(new FormData("password", password));
            }
            final String type;
            if (challenge instanceof HCaptchaChallenge) {
                /* 2025-02-24: Not supported anymore, see isChallengeSupported and: https://deathbycaptcha.com/api#supported_captchas */
                type = "HCaptcha";
                final HCaptchaChallenge hc = (HCaptchaChallenge) challenge;
                r.addFormData(new FormData("type", "7"));
                final Map<String, Object> hcaptcha_params = new HashMap<String, Object>();
                hcaptcha_params.put("pageurl", hc.getSiteUrl());
                hcaptcha_params.put("sitekey", hc.getSiteKey());
                r.addFormData(new FormData("hcaptcha_params", JSonStorage.serializeToJson(hcaptcha_params)));
            } else if (challenge instanceof RecaptchaV2Challenge) {
                /* https://deathbycaptcha.com/api/newtokenrecaptcha */
                final RecaptchaV2Challenge rc_challenge = (RecaptchaV2Challenge) challenge;
                final Map<String, Object> token_param = new HashMap<String, Object>();
                token_param.put("googlekey", rc_challenge.getSiteKey());
                token_param.put("pageurl", rc_challenge.getSiteUrl());
                final Map<String, Object> v3action = rc_challenge.getV3Action();
                if (v3action != null) {
                    // recaptchav3
                    type = "RecaptchaV3";
                    r.addFormData(new FormData("type", "5"));
                    // required parameters,https://deathbycaptcha.com/user/api/newtokenrecaptcha#reCAPTCHAv3
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
                    // required parameters
                    // token_param.put("google_stoken", rv2c.getSecureToken());
                }
                // TODO invisible captcha oder falsche domain /pageurl hier
                r.addFormData(new FormData("token_params", JSonStorage.serializeToJson(token_param)));
            } else if (challenge instanceof CutCaptchaChallenge) {
                /* See: https://deathbycaptcha.com/api/cutcaptcha */
                type = "CutCaptcha";
                final CutCaptchaChallenge cc = (CutCaptchaChallenge) challenge;
                r.addFormData(new FormData("type", "19"));
                final Map<String, Object> cutcaptcha_params = new HashMap<String, Object>();
                cutcaptcha_params.put("apikey", cc.getApiKey());
                cutcaptcha_params.put("miserykey", cc.getSiteKey());
                cutcaptcha_params.put("pageurl", cc.getSiteUrl());
                r.addFormData(new FormData("cutcaptcha_params", JSonStorage.serializeToJson(cutcaptcha_params)));
            } else if (challenge instanceof CloudflareTurnstileChallenge) {
                /* See: https://deathbycaptcha.com/api/turnstile */
                type = "CloudflareTurnstileCaptcha";
                final CloudflareTurnstileChallenge cc = (CloudflareTurnstileChallenge) challenge;
                r.addFormData(new FormData("type", "12"));
                final Map<String, Object> turnstile_params = new HashMap<String, Object>();
                turnstile_params.put("sitekey", cc.getSiteKey());
                turnstile_params.put("pageurl", cc.getSiteUrl());
                r.addFormData(new FormData("turnstile_params", JSonStorage.serializeToJson(turnstile_params)));
            } else if (challenge instanceof BasicCaptchaChallenge) {
                type = "Image";
                final BasicCaptchaChallenge bcc = (BasicCaptchaChallenge) challenge;
                final BufferedImage image = ImageProvider.read(bcc.getImageFile());
                final byte[] bytes = IconIO.toJpgBytes(image);
                r.addFormData(new FormData("swid", "0"));
                r.addFormData(new FormData("challenge", ""));
                r.addFormData(new FormData("captchafile", "captcha", "application/octet-stream", bytes));
            } else {
                /* This should never happen */
                throw new IllegalArgumentException("Got unexpected captcha type!");
            }
            br.setAllowedResponseCodes(200, 400);
            br.getPage(r);
            final DBCUploadResponse uploadStatus = JSonStorage.restoreFromString(br.getRequest().getHtmlCode(), DBCUploadResponse.TYPE);
            DBCUploadResponse status = uploadStatus;
            if (status != null && status.getCaptcha() > 0) {
                job.setStatus(new SolverStatus(_GUI.T.DeathByCaptchaSolver_solveBasicCaptchaChallenge_solving(), NewTheme.I().getIcon(IconKey.ICON_WAIT, 20)));
                job.getLogger().info("CAPTCHA(" + type + ")uploaded: " + status.getCaptcha());
                long startTime = Time.systemIndependentCurrentJVMTimeMillis();
                while (true) {
                    checkInterruption();
                    Thread.sleep(5000);
                    job.getLogger().info("deathbycaptcha.com NO answer after " + ((Time.systemIndependentCurrentJVMTimeMillis() - startTime) / 1000) + "s ");
                    br.getPage(API_BASE + "/captcha/" + uploadStatus.getCaptcha());
                    status = JSonStorage.restoreFromString(br.getRequest().getHtmlCode(), DBCUploadResponse.TYPE);
                    if (status.isSolved()) {
                        job.getLogger().info("Stopping because: isSolved = true");
                        break;
                    } else if (!status.isIs_correct()) {
                        job.getLogger().info("Stopping because: isCorrect = false");
                        break;
                    } else if (Time.systemIndependentCurrentJVMTimeMillis() - startTime > 5 * 60 * 60 * 1000l) {
                        throw new SolverException("Failed: Timeout");
                    } else {
                        continue;
                    }
                }
                if (status == null || !status.isSolved()) {
                    job.getLogger().info("Failed solving CAPTCHA(" + type + ")");
                    throw new SolverException("Failed:" + JSonStorage.serializeToJson(status));
                }
                job.getLogger().info("CAPTCHA(" + type + ")uploaded: " + status.getCaptcha() + "|solved: " + status.getText());
                final DeathByCaptchaResponse response;
                if (challenge instanceof HCaptchaChallenge) {
                    final HCaptchaChallenge hc = (HCaptchaChallenge) challenge;
                    response = new DeathByCaptchaResponse(hc, this, status, status.getText());
                } else if (challenge instanceof RecaptchaV2Challenge) {
                    final RecaptchaV2Challenge rv2c = (RecaptchaV2Challenge) challenge;
                    response = new DeathByCaptchaResponse(rv2c, this, status, status.getText());
                } else if (challenge instanceof CutCaptchaChallenge) {
                    final CutCaptchaChallenge cc = (CutCaptchaChallenge) challenge;
                    response = new DeathByCaptchaResponse(cc, this, status, status.getText());
                } else if (challenge instanceof CloudflareTurnstileChallenge) {
                    final CloudflareTurnstileChallenge cc = (CloudflareTurnstileChallenge) challenge;
                    response = new DeathByCaptchaResponse(cc, this, status, status.getText());
                } else {
                    final BasicCaptchaChallenge bcc = (BasicCaptchaChallenge) challenge;
                    final AbstractResponse<String> answer = bcc.parseAPIAnswer(status.getText().replace("[", "").replace("]", ""), null, this);
                    response = new DeathByCaptchaResponse(bcc, this, status, answer.getValue());
                }
                job.setAnswer(response);
            }
        } catch (final Exception e) {
            job.setStatus(getErrorByException(e), new AbstractIcon(IconKey.ICON_ERROR, 20));
            job.getLogger().log(e);
            challenge.sendStatsError(this, e);
        } finally {
            System.out.println("DBC DONe");
        }
    }

    protected void solveBasicCaptchaChallenge(CESSolverJob<String> job, BasicCaptchaChallenge challenge) throws InterruptedException, SolverException {
        throw new WTFException();
    }

    private int getBubbleTimeout(final Challenge<?> challenge) {
        final HashMap<String, Integer> map = config.getBubbleTimeoutByHostMap();
        Integer ret = map.get(challenge.getHost().toLowerCase(Locale.ENGLISH));
        if (ret == null || ret < 0) {
            ret = CFG_CAPTCHA.CFG.getCaptchaExchangeChanceToSkipBubbleTimeout();
        }
        return ret;
    }

    @Override
    protected boolean validateLogins() {
        if (!CFG_DBC.ENABLED.isEnabled()) {
            return false;
        } else if (StringUtils.isAllNotEmpty(CFG_DBC.USER_NAME.getValue(), CFG_DBC.PASSWORD.getValue())) {
            // username/password
            return true;
        } else if (StringUtils.isNotEmpty(CFG_DBC.PASSWORD.getValue())) {
            // authtoken
            return true;
        } else {
            return false;
        }
    }

    @Override
    public boolean setUnused(AbstractResponse<?> response) {
        return false;
    }

    @Override
    public boolean setInvalid(final AbstractResponse<?> response) {
        if (!config.isFeedBackSendingEnabled()) {
            return false;
        } else if (!(response instanceof DeathByCaptchaResponse)) {
            return false;
        }
        /* API docs: https://deathbycaptcha.com/api#api_details_report */
        threadPool.execute(new Runnable() {
            @Override
            public void run() {
                try {
                    final DBCUploadResponse captcha = ((DeathByCaptchaResponse) response).getCaptcha();
                    // Report incorrectly solved CAPTCHA if neccessary.
                    // Make sure you've checked if the CAPTCHA was in fact
                    // incorrectly solved, or else you might get banned as
                    // abuser.
                    Challenge<?> challenge = response.getChallenge();
                    if (challenge instanceof BasicCaptchaChallenge) {
                        final String username = config.getUserName();
                        final String password = config.getPassword();
                        UrlQuery query = new UrlQuery();
                        if (StringUtils.isEmpty(username)) {
                            query = query.addAndReplace("authtoken", URLEncode.encodeRFC2396(password));
                        } else {
                            query = query.addAndReplace("password", URLEncode.encodeRFC2396(password)).addAndReplace("username", URLEncode.encodeRFC2396(username));
                        }
                        createNewBrowserInstance(null).postPage(API_BASE + "/captcha/" + captcha.getCaptcha() + "/report", query);
                    }
                } catch (final Throwable e) {
                    logger.log(e);
                }
            }
        });
        return true;
    }

    public DBCAccount loadAccount() {
        DBCAccount ret = new DBCAccount();
        try {
            final DBCGetUserResponse user = getUserData();
            ret.setBalance(user.getBalance());
            ret.setBanned(user.isIs_banned());
            ret.setId(user.getUser());
            ret.setRate(user.getRate());
        } catch (Exception e) {
            logger.log(e);
            ret.setError(getErrorByException(e));
        }
        return ret;
    }

    private String getErrorByException(Exception e) {
        Throwable ee = e;
        String ret = null;
        while (ee != null && StringUtils.isEmpty(ee.getMessage())) {
            ee = ee.getCause();
        }
        if (ee != null) {
            ret = ee.getMessage();
        } else {
            ret = e.getMessage();
        }
        if (StringUtils.isEmpty(ret)) {
            ret = (_GUI.T.DBC_UNKNOWN_ERROR(e.getClass().getSimpleName()));
        }
        return ret;
    }

    private DBCGetUserResponse getUserData() throws UnsupportedEncodingException, IOException {
        final String username = config.getUserName();
        final String password = config.getPassword();
        UrlQuery query = new UrlQuery();
        if (StringUtils.isEmpty(username)) {
            query = query.addAndReplace("authtoken", URLEncode.encodeRFC2396(password));
        } else {
            query = query.addAndReplace("password", URLEncode.encodeRFC2396(password)).addAndReplace("username", URLEncode.encodeRFC2396(username));
        }
        final String json = createNewBrowserInstance(null).postPage(API_BASE + "/user", query);
        if (StringUtils.containsIgnoreCase(json, "<htm")) {
            throw new IOException("Invalid server response");
        }
        final Map<String, Object> map = JSonStorage.restoreFromString(json, TypeRef.MAP);
        if (((Number) (map.get("status"))).intValue() == 255) {
            throw new IOException(String.valueOf(map.get("error")));
        }
        return JSonStorage.restoreFromString(json, DBCGetUserResponse.TYPE);
    }

    @Override
    protected Browser createNewBrowserInstance(Challenge<?> challenge) {
        final Browser br = super.createNewBrowserInstance(challenge);
        br.setLogger(logger);
        br.setDebug(true);
        br.getHeaders().put("Accept", "application/json");
        br.getHeaders().put("User-Agent", "JDownloader");
        return br;
    }
}
