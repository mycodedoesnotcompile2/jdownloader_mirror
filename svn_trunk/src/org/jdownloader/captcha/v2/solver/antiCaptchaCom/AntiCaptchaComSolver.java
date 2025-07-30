package org.jdownloader.captcha.v2.solver.antiCaptchaCom;

import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import jd.http.Browser;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.Storable;
import org.appwork.storage.TypeRef;
import org.appwork.utils.IO;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.Base64;
import org.appwork.utils.net.URLHelper;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.SolverStatus;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CloudflareTurnstileChallenge;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.BasicCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.ImageCaptchaChallenge;
import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.captcha.v2.solver.jac.SolverException;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;
import org.jdownloader.settings.staticreferences.CFG_ANTICAPTCHA_COM;

public class AntiCaptchaComSolver extends AbstractAntiCaptchaComSolver<String> {
    private static final AntiCaptchaComSolver INSTANCE = new AntiCaptchaComSolver();

    public static AntiCaptchaComSolver getInstance() {
        return INSTANCE;
    }

    @Override
    public Class<String> getResultType() {
        return String.class;
    }

    @Override
    public AntiCaptchaComSolverService getService() {
        return (AntiCaptchaComSolverService) super.getService();
    }

    private AntiCaptchaComSolver() {
        super();
        getService().setSolver(this);
    }

    @Override
    protected boolean isChallengeSupported(Challenge<?> challenge) {
        if (challenge instanceof RecaptchaV2Challenge) {
            return true;
        } else if (challenge instanceof BasicCaptchaChallenge) {
            return true;
        } else if (challenge instanceof CloudflareTurnstileChallenge) {
            return true;
        } else {
            return false;
        }
    }

    private void errorHandling(AntiCaptchaComAccount account, Map<String, Object> response) throws Exception {
        // https://anticaptcha.atlassian.net/wiki/display/API/Errors
        final Number errorID = (Number) response.get("errorId");
        switch (errorID.intValue()) {
        case 0:
            return;
        case 1:
            // ERROR_KEY_DOES_NOT_EXIST
            if (account != null) {
                account.setError(String.valueOf(response.get("errorDescription")));
            }
            CFG_ANTICAPTCHA_COM.API_KEY.setValue(null);
            throw new SolverException("ErrorID:" + errorID + "|Error:" + response.get("errorDescription"));
        case 10:
            // ERROR_ZERO_BALANCE
            if (account != null) {
                account.setError(String.valueOf(response.get("errorDescription")));
            }
            throw new SolverException("ErrorID:" + errorID + "|Error:" + response.get("errorDescription"));
        default:
            throw new SolverException("ErrorID:" + errorID + "|Error:" + response.get("errorDescription"));
        }
    }

    @Override
    protected void solveCES(CESSolverJob<String> job) throws InterruptedException, SolverException {
        final Challenge<String> challenge = job.getChallenge();
        if (challenge instanceof RecaptchaV2Challenge) {
            handleRecaptchaV2(job);
        } else if (challenge instanceof ImageCaptchaChallenge) {
            handleImageCaptcha(job);
        } else if (challenge instanceof CloudflareTurnstileChallenge) {
            handleTurnstileCaptcha(job);
        }
    }

    /**
     * https://anti-captcha.com/de/apidoc/task-types/ImageToTextTask
     *
     * @param job
     * @throws InterruptedException
     */
    private void handleImageCaptcha(CESSolverJob<String> job) throws InterruptedException {
        final Challenge<String> challenge = job.getChallenge();
        job.showBubble(this);
        checkInterruption();
        try {
            job.getChallenge().sendStatsSolving(this);
            final Browser br = createNewBrowserInstance();
            br.setReadTimeout(5 * 60000);
            // Put your CAPTCHA image file, file object, input stream,
            // or vector of bytes here:
            job.setStatus(SolverStatus.SOLVING);
            final HashMap<String, Object> task = new HashMap<String, Object>();
            task.put("type", "ImageToTextTask");
            task.put("body", Base64.encodeToString(IO.readFile(((ImageCaptchaChallenge) challenge).getImageFile()), false));
            task.put("phrase", false);
            task.put("case", true);
            task.put("numeric", false);
            task.put("math", false);
            task.put("minLength", 0);
            task.put("maxLength", 0);
            HashMap<String, Object> dataMap = new HashMap<String, Object>();
            dataMap.put("clientKey", config.getApiKey());
            dataMap.put("task", task);
            dataMap.put("softId", 832);
            String json = br.postPageRaw(URLHelper.parseLocation(new URL(config.getApiBase()), "/createTask"), JSonStorage.serializeToJson(dataMap));
            HashMap<String, Object> response = JSonStorage.restoreFromString(json, TypeRef.HASHMAP);
            errorHandling(null, response);
            final int taskID = ((Number) response.get("taskId")).intValue();
            job.setStatus(new SolverStatus(_GUI.T.DeathByCaptchaSolver_solveBasicCaptchaChallenge_solving(), NewTheme.I().getIcon(IconKey.ICON_WAIT, 20)));
            while (true) {
                dataMap = new HashMap<String, Object>();
                dataMap.put("clientKey", config.getApiKey());
                dataMap.put("taskId", taskID);
                json = br.postPageRaw(URLHelper.parseLocation(new URL(config.getApiBase()), "/getTaskResult"), JSonStorage.serializeToJson(dataMap));
                response = JSonStorage.restoreFromString(json, TypeRef.HASHMAP);
                errorHandling(null, response);
                logger.info(json);
                if ("ready".equals(response.get("status"))) {
                    final Map<String, Object> solution = ((Map<String, Object>) response.get("solution"));
                    job.setAnswer(new AntiCaptchaComResponse(challenge, this, taskID, String.valueOf(solution.get("text"))));
                } else {
                    Thread.sleep(1000);
                    continue;
                }
                return;
            }
        } catch (Exception e) {
            job.getChallenge().sendStatsError(this, e);
            job.getLogger().log(e);
        }
    }

    /**
     * https://anti-captcha.com/de/apidoc/task-types/TurnstileTaskProxyless
     *
     * @param job
     * @throws InterruptedException
     */
    private void handleTurnstileCaptcha(CESSolverJob<String> job) throws InterruptedException {
        final CloudflareTurnstileChallenge challenge = (CloudflareTurnstileChallenge) job.getChallenge();
        job.showBubble(this);
        checkInterruption();
        try {
            job.getChallenge().sendStatsSolving(this);
            final Browser br = createNewBrowserInstance();
            br.setReadTimeout(5 * 60000);
            job.setStatus(SolverStatus.SOLVING);
            final HashMap<String, Object> task = new HashMap<String, Object>();
            task.put("type", "TurnstileTaskProxyless");
            task.put("websiteURL", challenge.getSiteUrl());
            task.put("websiteKey", challenge.getSiteKey());
            HashMap<String, Object> dataMap = new HashMap<String, Object>();
            dataMap.put("clientKey", config.getApiKey());
            dataMap.put("task", task);
            dataMap.put("softId", 832);
            String json = br.postPageRaw(URLHelper.parseLocation(new URL(config.getApiBase()), "/createTask"), JSonStorage.serializeToJson(dataMap));
            HashMap<String, Object> response = JSonStorage.restoreFromString(json, TypeRef.HASHMAP);
            errorHandling(null, response);
            final int taskID = ((Number) response.get("taskId")).intValue();
            job.setStatus(new SolverStatus(_GUI.T.DeathByCaptchaSolver_solveBasicCaptchaChallenge_solving(), NewTheme.I().getIcon(IconKey.ICON_WAIT, 20)));
            while (true) {
                dataMap = new HashMap<String, Object>();
                dataMap.put("clientKey", config.getApiKey());
                dataMap.put("taskId", taskID);
                json = br.postPageRaw(URLHelper.parseLocation(new URL(config.getApiBase()), "/getTaskResult"), JSonStorage.serializeToJson(dataMap));
                response = JSonStorage.restoreFromString(json, TypeRef.HASHMAP);
                errorHandling(null, response);
                logger.info(json);
                if ("ready".equals(response.get("status"))) {
                    final Map<String, Object> solution = ((Map<String, Object>) response.get("solution"));
                    job.setAnswer(new AntiCaptchaComResponse(challenge, this, taskID, String.valueOf(solution.get("token"))));
                } else {
                    Thread.sleep(1000);
                    continue;
                }
                return;
            }
        } catch (Exception e) {
            job.getChallenge().sendStatsError(this, e);
            job.getLogger().log(e);
        }
    }

    private void handleRecaptchaV2(CESSolverJob<String> job) throws InterruptedException {
        RecaptchaV2Challenge challenge = (RecaptchaV2Challenge) job.getChallenge();
        job.showBubble(this);
        checkInterruption();
        try {
            job.getChallenge().sendStatsSolving(this);
            final Browser br = createNewBrowserInstance();
            br.setReadTimeout(5 * 60000);
            // Put your CAPTCHA image file, file object, input stream,
            // or vector of bytes here:
            job.setStatus(SolverStatus.SOLVING);
            HashMap<String, Object> task = new HashMap<String, Object>();
            task.put("websiteURL", challenge.getSiteUrl());
            task.put("websiteKey", challenge.getSiteKey());
            if (challenge.getV3Action() != null) {
                // v3
                // https://anti-captcha.com/de/apidoc/task-types/RecaptchaV3TaskProxyless
                task.put("type", "RecaptchaV3TaskProxyless");
                if (challenge.isEnterprise()) {
                    // https://anti-captcha.com/de/apidoc/task-types/RecaptchaV3Enterprise
                    task.put("isEnterprise", Boolean.TRUE);
                }
                final String action = (String) challenge.getV3Action().get("action");
                if (action != null) {
                    task.put("pageAction", action);
                }
                task.put("minScore", 0.3d);
            } else {
                // v2
                if (challenge.isEnterprise()) {
                    // https://anti-captcha.com/de/apidoc/task-types/RecaptchaV2EnterpriseTaskProxyless
                    task.put("type", "RecaptchaV2EnterpriseTaskProxyless");
                } else {
                    // https://anti-captcha.com/de/apidoc/task-types/RecaptchaV2TaskProxyless
                    task.put("type", "RecaptchaV2TaskProxyless");
                }
                if (challenge.isInvisible()) {
                    task.put("isInvisible", Boolean.TRUE);
                }
            }
            if (StringUtils.isNotEmpty(challenge.getSecureToken())) {
                task.put("websiteSToken", challenge.getSecureToken());
            }
            HashMap<String, Object> dataMap = new HashMap<String, Object>();
            dataMap.put("clientKey", config.getApiKey());
            dataMap.put("task", task);
            dataMap.put("softId", 832);
            String json = br.postPageRaw(URLHelper.parseLocation(new URL(config.getApiBase()), "/createTask"), JSonStorage.serializeToJson(dataMap));
            HashMap<String, Object> response = JSonStorage.restoreFromString(json, TypeRef.HASHMAP);
            errorHandling(null, response);
            final int taskID = ((Number) response.get("taskId")).intValue();
            job.setStatus(new SolverStatus(_GUI.T.DeathByCaptchaSolver_solveBasicCaptchaChallenge_solving(), NewTheme.I().getIcon(IconKey.ICON_WAIT, 20)));
            while (true) {
                dataMap = new HashMap<String, Object>();
                dataMap.put("clientKey", config.getApiKey());
                dataMap.put("taskId", taskID);
                json = br.postPageRaw(URLHelper.parseLocation(new URL(config.getApiBase()), "/getTaskResult"), JSonStorage.serializeToJson(dataMap));
                response = JSonStorage.restoreFromString(json, TypeRef.HASHMAP);
                errorHandling(null, response);
                logger.info(json);
                if ("ready".equals(response.get("status"))) {
                    Map<String, Object> solution = ((Map<String, Object>) response.get("solution"));
                    job.setAnswer(new AntiCaptchaComResponse(challenge, this, taskID, String.valueOf(solution.get("gRecaptchaResponse"))));
                } else {
                    Thread.sleep(1000);
                    continue;
                }
                return;
            }
        } catch (Exception e) {
            job.getChallenge().sendStatsError(this, e);
            job.getLogger().log(e);
        }
    }

    @Override
    protected boolean validateLogins() {
        if (!CFG_ANTICAPTCHA_COM.ENABLED.isEnabled()) {
            return false;
        } else if (StringUtils.isEmpty(CFG_ANTICAPTCHA_COM.API_KEY.getValue())) {
            return false;
        } else {
            return true;
        }
    }

    @Override
    public boolean setInvalid(final AbstractResponse<?> response) {
        return false;
    }

    public static class BalanceResponse implements Storable {
        public BalanceResponse() {
        }

        private int status;

        public int getStatus() {
            return status;
        }

        public void setStatus(int status) {
            this.status = status;
        }

        public String getRequest() {
            return request;
        }

        public void setRequest(String request) {
            this.request = request;
        }

        private String request;
    }

    public AntiCaptchaComAccount loadAccount() {
        final AntiCaptchaComAccount ret = new AntiCaptchaComAccount();
        try {
            final Browser br = createNewBrowserInstance();
            final HashMap<String, Object> dataMap = new HashMap<String, Object>();
            dataMap.put("clientKey", config.getApiKey());
            final String json = br.postPageRaw(URLHelper.parseLocation(new URL(config.getApiBase()), "/getBalance"), JSonStorage.serializeToJson(dataMap));
            final Map<String, Object> response = JSonStorage.restoreFromString(json, TypeRef.MAP);
            errorHandling(null, response);
            ret.setBalance(((Number) response.get("balance")).doubleValue());
        } catch (Exception e) {
            logger.log(e);
            ret.setError(e.getMessage());
        }
        return ret;
    }
}
