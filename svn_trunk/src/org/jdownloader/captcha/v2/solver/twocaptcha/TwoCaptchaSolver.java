package org.jdownloader.captcha.v2.solver.twocaptcha;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import jd.http.Browser;
import jd.http.requests.PostRequest;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.Storable;
import org.appwork.storage.TypeRef;
import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.IO;
import org.appwork.utils.encoding.Base64;
import org.appwork.utils.logging2.LogSource;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.SolverStatus;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CloudflareTurnstileChallenge;
import org.jdownloader.captcha.v2.challenge.cutcaptcha.CutCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.hcaptcha.AbstractHCaptcha;
import org.jdownloader.captcha.v2.challenge.hcaptcha.HCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.AbstractRecaptchaV2;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.AbstractRecaptchaV2.TYPE;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.BasicCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.ImageCaptchaChallenge;
import org.jdownloader.captcha.v2.solver.CESChallengeSolver;
import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.captcha.v2.solver.jac.SolverException;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;
import org.jdownloader.logging.LogController;
import org.jdownloader.settings.staticreferences.CFG_TWO_CAPTCHA;

public class TwoCaptchaSolver extends CESChallengeSolver<String> {
    private static final TwoCaptchaSolver     INSTANCE           = new TwoCaptchaSolver();
    private String                            accountStatusString;
    protected final TwoCaptchaConfigInterface config;
    AtomicInteger                             counter            = new AtomicInteger();
    AtomicInteger                             counterInterrupted = new AtomicInteger();
    AtomicInteger                             counterNotOK       = new AtomicInteger();
    AtomicInteger                             counterOK          = new AtomicInteger();
    AtomicInteger                             counterSend        = new AtomicInteger();
    AtomicInteger                             counterSendError   = new AtomicInteger();
    AtomicInteger                             counterSolved      = new AtomicInteger();
    AtomicInteger                             counterUnused      = new AtomicInteger();
    protected final LogSource                 logger;

    public static TwoCaptchaSolver getInstance() {
        return INSTANCE;
    }

    @Override
    public Class<String> getResultType() {
        return String.class;
    }

    @Override
    public TwoCaptchaSolverService getService() {
        return (TwoCaptchaSolverService) super.getService();
    }

    private TwoCaptchaSolver() {
        super(new TwoCaptchaSolverService(), Math.max(1, Math.min(25, JsonConfig.create(TwoCaptchaConfigInterface.class).getThreadpoolSize())));
        config = JsonConfig.create(TwoCaptchaConfigInterface.class);
        logger = LogController.getInstance().getLogger(TwoCaptchaSolver.class.getName());
        threadPool.allowCoreThreadTimeOut(true);
        getService().setSolver(this);
    }

    @Override
    public String getAccountStatusString() {
        return accountStatusString;
    }

    @Override
    protected void solveBasicCaptchaChallenge(CESSolverJob<String> job, BasicCaptchaChallenge challenge) throws SolverException {
        // not used. solveCES is overwritten
    }

    protected String getApiBaseV2() {
        return "https://api.2captcha.com";
    }

    @Override
    protected boolean isChallengeSupported(final Challenge<?> c) {
        if (c instanceof RecaptchaV2Challenge || c instanceof HCaptchaChallenge || c instanceof BasicCaptchaChallenge) {
            return true;
        } else if (c instanceof CutCaptchaChallenge) {
            return true;
        } else if (c instanceof CloudflareTurnstileChallenge) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    protected void solveCES(CESSolverJob<String> job) throws InterruptedException, SolverException {
        final Challenge<String> captchaChallenge = job.getChallenge();
        try {
            final Map<String, Object> postdata = new HashMap<String, Object>(); // APIv2
            postdata.put("clientKey", config.getApiKey());
            final Map<String, Object> task = new HashMap<String, Object>(); // APIv2
            if (captchaChallenge instanceof RecaptchaV2Challenge) {
                final RecaptchaV2Challenge challenge = (RecaptchaV2Challenge) job.getChallenge();
                task.put("type", "RecaptchaV2TaskProxyless");
                task.put("websiteKey", challenge.getSiteKey());
                task.put("websiteURL", challenge.getSiteUrl());
                final AbstractRecaptchaV2<?> recaptchaChallenge = challenge.getAbstractCaptchaHelperRecaptchaV2();
                if (recaptchaChallenge != null) {
                    if (challenge.isEnterprise()) {
                        task.put("isEnterprise", true);
                    }
                    final Map<String, Object> action = challenge.getV3Action();
                    if (action != null && action.containsKey("action")) {
                        task.put("type", "RecaptchaV3TaskProxyless");
                        task.put("pageAction", String.valueOf(action.get("action")));
                    } else if (challenge.isV3()) {
                        task.put("type", "RecaptchaV3TaskProxyless");
                    } else if (TYPE.INVISIBLE.equals(recaptchaChallenge.getType())) {
                        task.put("isInvisible", true);
                    }
                }
            } else if (captchaChallenge instanceof HCaptchaChallenge) {
                final HCaptchaChallenge challenge = (HCaptchaChallenge) job.getChallenge();
                task.put("type", "HCaptchaTaskProxyless");
                task.put("websiteURL", challenge.getSiteUrl());
                task.put("websiteKey", challenge.getSiteKey());
                final AbstractHCaptcha<?> hCaptcha = challenge.getAbstractCaptchaHelperHCaptcha();
                if (hCaptcha != null && AbstractHCaptcha.TYPE.INVISIBLE.equals(hCaptcha.getType())) {
                    task.put("isInvisible", true);
                }
            } else if (captchaChallenge instanceof CutCaptchaChallenge) {
                /* CutCaptcha: https://2captcha.com/api-docs/cutcaptcha */
                final CutCaptchaChallenge challenge = (CutCaptchaChallenge) job.getChallenge();
                challenge.sendStatsSolving(this);
                task.put("type", "CutCaptchaTaskProxyless");
                task.put("miseryKey", challenge.getSiteKey());
                task.put("apiKey", challenge.getApiKey());
                task.put("websiteURL", challenge.getSiteUrl());
            } else if (captchaChallenge instanceof CloudflareTurnstileChallenge) {
                /* Cloudflare turnstile: https://2captcha.com/api-docs/cloudflare-turnstile */
                final CloudflareTurnstileChallenge challenge = (CloudflareTurnstileChallenge) job.getChallenge();
                challenge.sendStatsSolving(this);
                task.put("type", "TurnstileTaskProxyless");
                task.put("websiteURL", challenge.getSiteUrl());
                task.put("websiteKey", challenge.getSiteKey());
            } else {
                /* Image captcha: https://2captcha.com/api-docs/normal-captcha */
                final ImageCaptchaChallenge challenge = (ImageCaptchaChallenge<String>) job.getChallenge();
                final byte[] data = IO.readFile(challenge.getImageFile());
                task.put("type", "ImageToTextTask");
                task.put("body", Base64.encodeToString(data, false));
                task.put("comment", challenge.getExplain());
            }
            boolean clickCaptcha = false;
            if (clickCaptcha) {
                task.put("type", "CoordinatesTask");
                // v2task.put("body", Base64.encodeToString(data, false));
                // v2task.put("comment", challenge.getExplain());
                /* We want a single set of coordinates -> One click */
                task.put("maxClicks", 1);
            }
            postdata.put("task", task);
            job.showBubble(this);
            checkInterruption();
            job.getChallenge().sendStatsSolving(this);
            job.setStatus(SolverStatus.SOLVING);
            /* Submit captcha */
            final Browser br = this.createNewBrowserInstance();
            final PostRequest req_createTask = br.createJSonPostRequest(this.getApiBaseV2() + "/createTask", postdata);
            br.getPage(req_createTask);
            final BalanceResponse resp_createTask = JSonStorage.restoreFromString(br.getRequest().getHtmlCode(), new TypeRef<BalanceResponse>() {
            });
            if (resp_createTask.getErrorId() != 0) {
                throw new IOException("Captcha image upload failure, status: " + resp_createTask.getStatus());
            }
            final String id = resp_createTask.getTaskId();
            final Map<String, Object> postdata_getTaskResult = new HashMap<String, Object>();
            postdata_getTaskResult.put("clientKey", this.config.getApiKey());
            postdata_getTaskResult.put("taskId", id);
            job.setStatus(new SolverStatus(_GUI.T.DeathByCaptchaSolver_solveBasicCaptchaChallenge_solving(), NewTheme.I().getIcon(IconKey.ICON_WAIT, 10)));
            while (job.getJob().isAlive() && !job.getJob().isSolved()) {
                checkInterruption();
                final PostRequest req_getTaskResult = br.createJSonPostRequest(this.getApiBaseV2() + "/getTaskResult", postdata_getTaskResult);
                br.getPage(req_getTaskResult);
                final BalanceResponse resp_getTaskResult = JSonStorage.restoreFromString(br.getRequest().getHtmlCode(), new TypeRef<BalanceResponse>() {
                });
                logger.info(br.getRequest().getHtmlCode());
                final String status = resp_getTaskResult.getStatus();
                if (status.equals("processing")) {
                    Thread.sleep(5000);
                    continue;
                } else if (status.equals("ready")) {
                    final Solution solution = resp_getTaskResult.getSolution();
                    final String resultText;
                    if (solution.getgRecaptchaResponse() != null) {
                        resultText = solution.getgRecaptchaResponse();
                    } else if (solution.getToken() != null) {
                        /* For example reCaptchaV2, CloudflareTurnstile */
                        resultText = solution.getToken();
                    } else if (solution.getCorrdinates() != null && solution.getCorrdinates().size() > 0) {
                        // TODO
                        resultText = "TODO_IMPLEMENT_CLICK_CAPTCHA";
                    } else {
                        resultText = solution.getText();
                    }
                    job.setAnswer(new TwoCaptchaResponse(captchaChallenge, this, id, resultText));
                    return;
                } else {
                    throw new IOException("Captcha task polling failure, status: " + status);
                }
            }
        } catch (IOException e) {
            job.getChallenge().sendStatsError(this, e);
            job.getLogger().log(e);
        } finally {
            System.out.println(1);
        }
    }

    @Override
    protected boolean validateLogins() {
        if (!CFG_TWO_CAPTCHA.ENABLED.isEnabled()) {
            return false;
        } else if (!looksLikeValidAPIKey(CFG_TWO_CAPTCHA.API_KEY.getValue())) {
            return false;
        } else {
            return true;
        }
    }

    protected boolean looksLikeValidAPIKey(final String str) {
        if (str == null) {
            return false;
        }
        return str.matches("[a-f0-9]{32}");
    }

    @Override
    public boolean setInvalid(final AbstractResponse<?> response) {
        return sendCaptchaFeedback(response, false);
    }

    @Override
    public boolean setValid(final AbstractResponse<?> response) {
        return sendCaptchaFeedback(response, true);
    }

    private final boolean sendCaptchaFeedback(final AbstractResponse<?> response, final boolean positiveFeedback) {
        if (!this.config.isFeedBackSendingEnabled()) {
            /* User has disabled feedback sending */
            return false;
        }
        final TwoCaptchaResponse twocaptcharesponse = (TwoCaptchaResponse) response;
        final String captchaID = twocaptcharesponse.getCaptchaID();
        final Browser br = this.createNewBrowserInstance();
        try {
            final String url;
            if (positiveFeedback) {
                /* https://2captcha.com/api-docs/report-correct */
                url = "/reportCorrect";
            } else {
                /* https://2captcha.com/api-docs/report-incorrect */
                url = "/reportIncorrect";
            }
            final Map<String, Object> postdata = new HashMap<String, Object>();
            postdata.put("clientKey", this.config.getApiKey());
            postdata.put("taskId", captchaID);
            final PostRequest req = br.createJSonPostRequest(this.getApiBaseV2() + url, postdata);
            br.getPage(req);
            final BalanceResponse resp = JSonStorage.restoreFromString(br.getRequest().getHtmlCode(), new TypeRef<BalanceResponse>() {
            });
            if ("success".equalsIgnoreCase(resp.getStatus())) {
                return true;
            } else {
                return false;
            }
        } catch (final Throwable e) {
            e.printStackTrace();
            return false;
        }
    }

    public static class BalanceResponse implements Storable {
        public BalanceResponse() {
        }

        private int      errorId;
        private String   taskId;
        private Double   balance;
        private String   cost;
        private String   status;
        private Solution solution;
        private String   errorCode;
        private String   errorDescription;

        public int getErrorId() {
            return errorId;
        }

        public void setErrorId(int errorId) {
            this.errorId = errorId;
        }

        public Double getBalance() {
            return balance;
        }

        public void setBalance(Double balance) {
            this.balance = balance;
        }

        public String getStatus() {
            return status;
        }

        public void setStatus(String status) {
            this.status = status;
        }

        public String getCost() {
            return cost;
        }

        public void setCost(String cost) {
            this.cost = cost;
        }

        public Solution getSolution() {
            return solution;
        }

        public void setSolution(Solution solution) {
            this.solution = solution;
        }

        public String getTaskId() {
            return taskId;
        }

        public void setTaskId(String taskId) {
            this.taskId = taskId;
        }

        public String getErrorCode() {
            return errorCode;
        }

        public void setErrorCode(String errorCode) {
            this.errorCode = errorCode;
        }

        public String getErrorDescription() {
            return errorDescription;
        }

        public void setErrorDescription(String errorDescription) {
            this.errorDescription = errorDescription;
        }
    }

    public static class Solution implements Storable {
        public Solution() {
        }

        /* Same for reCaptcha and hCaptcha */
        private String            gRecaptchaResponse;
        private String            token;
        private String            text;
        private List<Coordinates> corrdinates;

        public String getgRecaptchaResponse() {
            return gRecaptchaResponse;
        }

        public void setgRecaptchaResponse(String gRecaptchaResponse) {
            this.gRecaptchaResponse = gRecaptchaResponse;
        }

        public String getToken() {
            return token;
        }

        public void setToken(String token) {
            this.token = token;
        }

        public String getText() {
            return text;
        }

        public void setText(String text) {
            this.text = text;
        }

        public List<Coordinates> getCorrdinates() {
            return corrdinates;
        }

        public void setCorrdinates(List<Coordinates> corrdinates) {
            this.corrdinates = corrdinates;
        }
    }

    public static class Coordinates implements Storable {
        public Coordinates() {
        }

        private int x;
        private int y;

        public int getX() {
            return x;
        }

        public void setX(int x) {
            this.x = x;
        }

        public int getY() {
            return y;
        }

        public void setY(int y) {
            this.y = y;
        }
    }

    public TwoCaptchaAccount loadAccount() {
        final TwoCaptchaAccount ret = new TwoCaptchaAccount();
        try {
            final Browser br = this.createNewBrowserInstance();
            final Map<String, Object> postdata = new HashMap<String, Object>();
            postdata.put("clientKey", this.config.getApiKey());
            final PostRequest req = br.createJSonPostRequest(this.getApiBaseV2() + "/getBalance", postdata);
            final String json = br.getPage(req);
            final BalanceResponse response = JSonStorage.restoreFromString(json, new TypeRef<BalanceResponse>() {
            });
            if (response.getErrorId() != 0) {
                ret.setError("Error code " + response.getErrorId() + ": " + response.getErrorDescription());
                return ret;
            }
            ret.setBalance(response.getBalance());
            ret.setUserName(config.getApiKey());
        } catch (final Exception e) {
            logger.log(e);
            ret.setError(e.getMessage());
        }
        return ret;
    }
}