package org.jdownloader.captcha.v2.solver.twocaptcha;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.Storable;
import org.appwork.storage.TypeRef;
import org.appwork.utils.IO;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.Base64;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.SolverStatus;
import org.jdownloader.captcha.v2.challenge.cutcaptcha.CutCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.hcaptcha.AbstractHCaptcha;
import org.jdownloader.captcha.v2.challenge.hcaptcha.HCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.AbstractRecaptchaV2;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.AbstractRecaptchaV2.TYPE;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.BasicCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.ImageCaptchaChallenge;
import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.captcha.v2.solver.jac.SolverException;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;
import org.jdownloader.settings.staticreferences.CFG_TWO_CAPTCHA;

import jd.http.Browser;
import jd.http.requests.PostRequest;

public class TwoCaptchaSolver extends AbstractTwoCaptchaSolver<String> {
    private static final TwoCaptchaSolver INSTANCE = new TwoCaptchaSolver();

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
        super();
        getService().setSolver(this);
    }

    @Override
    protected boolean isChallengeSupported(final Challenge<?> c) {
        if (c instanceof RecaptchaV2Challenge || c instanceof HCaptchaChallenge || c instanceof BasicCaptchaChallenge) {
            return true;
        } else if (c instanceof CutCaptchaChallenge) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    protected void solveCES(CESSolverJob<String> job) throws InterruptedException, SolverException {
        final Challenge<String> captchaChallenge = job.getChallenge();
        try {
            final Map<String, Object> v2postdata = new HashMap<String, Object>(); // APIv2
            v2postdata.put("clientKey", config.getApiKey());
            final Map<String, Object> v2task = new HashMap<String, Object>(); // APIv2
            final UrlQuery q = new UrlQuery();
            q.appendEncoded("key", config.getApiKey());
            q.appendEncoded("json", "1");
            q.appendEncoded("soft_id", getSoftID());
            if (captchaChallenge instanceof RecaptchaV2Challenge) {
                final RecaptchaV2Challenge challenge = (RecaptchaV2Challenge) job.getChallenge();
                v2task.put("type", "RecaptchaV2TaskProxyless");
                v2task.put("websiteKey", challenge.getSiteKey());
                v2task.put("websiteURL", challenge.getSiteUrl());
                q.appendEncoded("googlekey", challenge.getSiteKey());
                q.appendEncoded("pageurl", challenge.getSiteUrl());
                final AbstractRecaptchaV2<?> recaptchaChallenge = challenge.getAbstractCaptchaHelperRecaptchaV2();
                if (recaptchaChallenge != null) {
                    if (challenge.isEnterprise()) {
                        v2task.put("isEnterprise", true);
                        q.appendEncoded("enterprise", "1");
                    }
                    final Map<String, Object> action = challenge.getV3Action();
                    if (action != null && action.containsKey("action")) {
                        v2task.put("type", "RecaptchaV3TaskProxyless");
                        v2task.put("pageAction", String.valueOf(action.get("action")));
                        q.appendEncoded("version", "v3");
                        q.appendEncoded("action", String.valueOf(action.get("action")));
                    } else if (TYPE.INVISIBLE.equals(recaptchaChallenge.getType())) {
                        q.appendEncoded("invisible", "1");
                        v2task.put("isInvisible", true);
                    }
                }
            } else if (captchaChallenge instanceof HCaptchaChallenge) {
                final HCaptchaChallenge challenge = (HCaptchaChallenge) job.getChallenge();
                v2task.put("type", "HCaptchaTaskProxyless");
                v2task.put("websiteURL", challenge.getSiteUrl());
                v2task.put("websiteKey", challenge.getSiteKey());
                q.appendEncoded("method", "hcaptcha");
                q.appendEncoded("sitekey", challenge.getSiteKey());
                q.appendEncoded("pageurl", challenge.getSiteUrl());
                final AbstractHCaptcha<?> hCaptcha = challenge.getAbstractCaptchaHelperHCaptcha();
                if (hCaptcha != null && AbstractHCaptcha.TYPE.INVISIBLE.equals(hCaptcha.getType())) {
                    v2task.put("isInvisible", true);
                    q.appendEncoded("invisible", "1");
                }
            } else if (captchaChallenge instanceof CutCaptchaChallenge) {
                /* CutCaptcha: https://2captcha.com/api-docs/cutcaptcha */
                final CutCaptchaChallenge challenge = (CutCaptchaChallenge) job.getChallenge();
                challenge.sendStatsSolving(this);
                q.appendEncoded("method", "cutcaptcha");
                q.appendEncoded("misery_key", challenge.getSiteKey());
                q.appendEncoded("api_key", challenge.getApiKey());
                q.appendEncoded("pageurl", challenge.getSiteUrl());
                v2task.put("type", "CutCaptchaTaskProxyless");
                v2task.put("miseryKey", challenge.getSiteKey());
                v2task.put("apiKey", challenge.getApiKey());
                v2task.put("websiteURL", challenge.getSiteUrl());
            } else {
                /* Image captcha: https://2captcha.com/api-docs/normal-captcha */
                final ImageCaptchaChallenge challenge = (ImageCaptchaChallenge<String>) job.getChallenge();
                final byte[] data = IO.readFile(challenge.getImageFile());
                q.appendEncoded("method", "base64");
                q.appendEncoded("body", Base64.encodeToString(data, false));
                if (challenge.getExplain() != null) {
                    q.appendEncoded("comment", challenge.getExplain());
                }
                v2task.put("type", "ImageToTextTask");
                v2task.put("body", Base64.encodeToString(data, false));
                v2task.put("comment", challenge.getExplain());
            }
            boolean clickCaptcha = false;
            if (clickCaptcha) {
                v2task.put("type", "CoordinatesTask");
                // v2task.put("body", Base64.encodeToString(data, false));
                // v2task.put("comment", challenge.getExplain());
                /* We want a single set of coordinates -> One click */
                v2task.put("maxClicks", 1);
            }
            v2postdata.put("task", v2task);
            job.showBubble(this);
            checkInterruption();
            job.getChallenge().sendStatsSolving(this);
            job.setStatus(SolverStatus.SOLVING);
            /* Submit captcha */
            final Browser br = this.createNewBrowserInstance();
            final PostRequest req_createTask = br.createJSonPostRequest(this.getApiBaseV2() + "/createTask", v2postdata);
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
                        resultText = solution.getToken();
                    } else if (solution.getCorrdinates() != null && solution.getCorrdinates().size() > 0) {
                        // TODO
                        resultText = "TODO";
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

    protected boolean validateLogins() {
        if (!CFG_TWO_CAPTCHA.ENABLED.isEnabled()) {
            return false;
        } else if (StringUtils.isEmpty(CFG_TWO_CAPTCHA.API_KEY.getValue())) {
            return false;
        } else {
            return true;
        }
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
                ret.setError("Bad Login: " + json);
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
