package org.jdownloader.captcha.v2.solver.captchasolutions;

import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.config.JsonConfig;
import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.utils.IO;
import org.appwork.utils.StringUtils;
import org.appwork.utils.logging2.LogSource;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.SolverStatus;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.BasicCaptchaChallenge;
import org.jdownloader.captcha.v2.solver.CESChallengeSolver;
import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.captcha.v2.solver.jac.SolverException;
import org.jdownloader.logging.LogController;
import org.jdownloader.settings.staticreferences.CFG_CAPTCHA_SOLUTIONS;

import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.http.requests.FormData;
import jd.http.requests.PostFormDataRequest;
import jd.nutils.encoding.Encoding;

public class CaptchaSolutionsSolver extends CESChallengeSolver<String> implements GenericConfigEventListener<String> {
    private final CaptchaSolutionsConfigInterface config;
    private static final CaptchaSolutionsSolver   INSTANCE   = new CaptchaSolutionsSolver();
    private final ThreadPoolExecutor              threadPool = new ThreadPoolExecutor(0, 1, 30000, TimeUnit.MILLISECONDS, new LinkedBlockingDeque<Runnable>(), Executors.defaultThreadFactory());
    private final LogSource                       logger;

    public static CaptchaSolutionsSolver getInstance() {
        return INSTANCE;
    }

    @Override
    public Class<String> getResultType() {
        return String.class;
    }

    @Override
    public CaptchaSolutionsSolverService getService() {
        return (CaptchaSolutionsSolverService) super.getService();
    }

    private CaptchaSolutionsSolver() {
        super(new CaptchaSolutionsSolverService(), Math.max(1, Math.min(25, JsonConfig.create(CaptchaSolutionsConfigInterface.class).getThreadpoolSize())));
        getService().setSolver(this);
        config = JsonConfig.create(CaptchaSolutionsConfigInterface.class);
        logger = LogController.getInstance().getLogger(CaptchaSolutionsSolver.class.getName());
        threadPool.allowCoreThreadTimeOut(true);
        CFG_CAPTCHA_SOLUTIONS.USER_NAME.getEventSender().addListener(this);
        CFG_CAPTCHA_SOLUTIONS.PASSWORD.getEventSender().addListener(this);
    }

    @Override
    protected boolean isChallengeSupported(Challenge<?> c) {
        return c instanceof RecaptchaV2Challenge || c instanceof BasicCaptchaChallenge;
    }

    protected <T> Challenge<T> getChallenge(CESSolverJob<T> job) throws SolverException {
        final Challenge<?> challenge = job.getChallenge();
        return (Challenge<T>) challenge;
    }

    @Override
    protected void solveCES(CESSolverJob<String> job) throws InterruptedException, SolverException {
        Challenge<String> challenge = getChallenge(job);
        if (challenge instanceof RecaptchaV2Challenge) {
            RecaptchaV2Challenge rc2 = ((RecaptchaV2Challenge) challenge);
            Browser br = new Browser();
            try {
                br.setReadTimeout(5 * 60000);
                // Put your CAPTCHA image file, file object, input stream,
                // or vector of bytes here:
                job.setStatus(SolverStatus.SOLVING);
                long startTime = System.currentTimeMillis();
                PostFormDataRequest r = new PostFormDataRequest("http://api.captchasolutions.com/solve");
                final String[] credentials = ensureAPIKey();
                r.addFormData(new FormData("p", "nocaptcha"));
                r.addFormData(new FormData("googlekey", Encoding.urlEncode(rc2.getSiteKey())));
                r.addFormData(new FormData("key", Encoding.urlEncode(credentials[0])));
                r.addFormData(new FormData("secret", Encoding.urlEncode(credentials[0])));
                r.addFormData(new FormData("pageurl", Encoding.urlEncode("http://" + rc2.getSiteDomain() + "/")));
                br.getPage(r);
                String token = br.getRegex("<decaptcha>\\s*(\\S+)\\s*</decaptcha>").getMatch(0);
                if (StringUtils.isNotEmpty(token)) {
                    job.setAnswer(new CaptchaSolutionsResponse(rc2, this, null, token, 100));
                    return;
                } else {
                    throw new WTFException("RC2 Failed");
                }
            } catch (Exception e) {
                job.getChallenge().sendStatsError(this, e);
                job.getLogger().log(e);
                throw new SolverException(e);
            }
        }
        super.solveCES(job);
    }

    protected void solveBasicCaptchaChallenge(CESSolverJob<String> job, BasicCaptchaChallenge challenge) throws InterruptedException, SolverException {
        job.showBubble(this);
        checkInterruption();
        job.getChallenge().sendStatsSolving(this);
        try {
            Browser br = new Browser();
            br.setReadTimeout(5 * 60000);
            // Put your CAPTCHA image file, file object, input stream,
            // or vector of bytes here:
            job.setStatus(SolverStatus.SOLVING);
            final String[] credentials = ensureAPIKey();
            PostFormDataRequest r = new PostFormDataRequest("http://api.captchasolutions.com/solve");
            r.addFormData(new FormData("p", "upload"));
            r.addFormData(new FormData("key", Encoding.urlEncode(credentials[0])));
            r.addFormData(new FormData("secret", Encoding.urlEncode(credentials[0])));
            // byte[] bytes = challenge.getAnnotatedImageBytes();
            final byte[] bytes = IO.readFile(challenge.getImageFile());
            r.addFormData(new FormData("captcha", "image.jpg", "image/jpg", bytes));
            final URLConnectionAdapter con = br.openRequestConnection(r);
            try {
                br.loadConnection(con);
            } finally {
                con.disconnect();
            }
            String decaptcha = br.getRegex("<decaptcha>(.*?)</decaptcha>").getMatch(0);
            if (StringUtils.isEmpty(decaptcha)) {
                throw new SolverException("API Error");
            }
            try {
                decaptcha = Encoding.urlDecode(decaptcha, false);
            } catch (Throwable e) {
            }
            if (decaptcha.trim().startsWith("Error:")) {
                // do poll
                String error = decaptcha.trim().substring(6);
                throw new SolverException(error);
            }
            job.getLogger().info("CAPTCHA " + challenge.getImageFile() + " solved: " + decaptcha.trim());
            AbstractResponse<String> answer = challenge.parseAPIAnswer(decaptcha.trim(), null, this);
            job.getLogger().info("CAPTCHA " + challenge.getImageFile() + " solved: " + br.toString());
            job.setAnswer(new CaptchaSolutionsResponse(challenge, this, null, answer.getValue(), answer.getPriority()));
            return;
        } catch (Exception e) {
            job.getChallenge().sendStatsError(this, e);
            job.getLogger().log(e);
        }
    }

    @Override
    protected boolean validateLogins() {
        if (!CFG_CAPTCHA_SOLUTIONS.ENABLED.isEnabled()) {
            return false;
        } else if (StringUtils.isAllNotEmpty(config.getAPIKey(), config.getAPISecret())) {
            return true;
        } else if (StringUtils.isEmpty(CFG_CAPTCHA_SOLUTIONS.USER_NAME.getValue())) {
            return false;
        } else if (StringUtils.isEmpty(CFG_CAPTCHA_SOLUTIONS.PASSWORD.getValue())) {
            return false;
        } else {
            return true;
        }
    }

    public CaptchaSolutionsAccount loadAccount() {
        CaptchaSolutionsAccount ret = new CaptchaSolutionsAccount();
        final Browser br = new Browser();
        br.setFollowRedirects(false);
        try {
            final String[] credentials = ensureAPIKey();
            br.getPage("http://api.captchasolutions.com/solve?p=balance&key=" + Encoding.urlEncode(credentials[0]) + "&secret=" + Encoding.urlEncode(credentials[1]));
            final String tokens = br.getRegex("<tokens>\\s*(\\d+)\\s*</tokens>").getMatch(0);
            if (tokens == null) {
                throw new Exception("API Error!?");
            }
            ret.setTokens(Integer.parseInt(tokens));
            ret.setUserName(config.getUserName());
        } catch (Exception e) {
            logger.log(e);
            ret.setError(e.getMessage());
        }
        return ret;
    }

    private synchronized boolean verifyAPIKey(String[] credentials) {
        final Browser br = new Browser();
        br.setFollowRedirects(false);
        try {
            br.getPage("http://api.captchasolutions.com/solve?p=balance&key=" + Encoding.urlEncode(credentials[0]) + "&secret=" + Encoding.urlEncode(credentials[1]));
            final String tokens = br.getRegex("<tokens>\\s*(\\d+)\\s*</tokens>").getMatch(0);
            return tokens != null;
        } catch (Exception e) {
            logger.log(e);
            return false;
        }
    }

    private synchronized String[] ensureAPIKey() throws IOException {
        String key = config.getAPIKey();
        String secret = config.getAPISecret();
        if (StringUtils.isAllNotEmpty(key, secret)) {
            final String[] credentials = new String[] { key, secret };
            if (verifyAPIKey(credentials)) {
                return credentials;
            }
        }
        final Browser br = new Browser();
        br.setFollowRedirects(false);
        final LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
        map.put("email", Encoding.urlEncode(StringUtils.trim(config.getUserName())));
        map.put("password", Encoding.urlEncode(StringUtils.trim(config.getPassword())));
        map.put("submit", "submit");
        //
        final URLConnectionAdapter conn = br.openPostConnection("https://www.captchasolutions.com/clients/login/", map);
        try {
            br.loadConnection(conn);
        } finally {
            conn.disconnect();
        }
        if (br.getRequest().getHttpConnection().getResponseCode() != 302) {
            throw new WTFException(StringUtils.trim(br.getRegex("<strong>Oh snap!</strong>(.*?)<").getMatch(0)));
        } else {
            br.setFollowRedirects(true);
            br.followRedirect();
        }
        br.getPage("https://www.captchasolutions.com/clients/home/generatekeys/");
        key = br.getRegex("<strong>API KEY</strong>.*?<p>(.*?)</p>").getMatch(0);
        secret = br.getRegex("<strong>API SECRET</strong>.*?<p>(.*?)</p>").getMatch(0);
        if (StringUtils.isAllNotEmpty(key, secret)) {
            config.setAPIKey(key);
            config.setAPISecret(secret);
            return new String[] { key, secret };
        } else {
            throw new WTFException("Login failed!");
        }
    }

    @Override
    public void onConfigValidatorError(KeyHandler<String> keyHandler, String invalidValue, ValidationException validateException) {
    }

    @Override
    public void onConfigValueModified(KeyHandler<String> keyHandler, String newValue) {
        synchronized (this) {
            config.setAPIKey(null);
            config.setAPISecret(null);
        }
    }
}
