package org.jdownloader.captcha.v2;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.WeakHashMap;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

import org.appwork.exceptions.WTFException;
import org.appwork.utils.StringUtils;
import org.jdownloader.captcha.v2.solver.jac.SolverException;
import org.jdownloader.captcha.v2.solverjob.SolverJob;

import jd.controlling.captcha.SkipException;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.Plugin;

public abstract class ChallengeSolver<T> {
    public static final ChallengeSolver EXTERN = new ChallengeSolver<Object>() {
        @Override
        public void solve(SolverJob<Object> solverJob) throws InterruptedException, SolverException, SkipException {
            throw new WTFException("Not Implemented");
        }

        @Override
        public SolverType getSolverType() {
            return SolverType.EXTERNAL;
        }
    };

    public enum FeedbackType {
        REPORT_INVALID_CAPTCHAS,
        REPORT_VALID_CAPTCHAS,
        ABORT_CAPTCHAS
    }

    public enum SolverType {
        JD_LOCAL,
        JD_LOCAL_BROWSER,
        JD_REMOTE_API,
        EXTERNAL
    }

    public enum ChallengeVetoReason {
        SOLVER_DISABLED,
        UNSUPPORTED_BY_SOLVER,
        UNSUPPORTED_BY_SOLVER_ACCOUNT,
        UNSUPPORTED_FOR_INTERNAL_SPECIAL_REASONS,
        UNSUPPORTED_BROWSER_NO_URL_OPEN,
        UNSUITABLE_FOR_SOLVER,
        CHALLENGE_BLACKLISTED,
        CAPTCHA_TYPE_DISABLED_BY_USER,
        ACCOUNT_DISABLED,
        ACCOUNT_IN_ERROR_STATE,
        ACCOUNT_NOT_ENOUGH_CREDITS,
        /** The user globally disabled external (plugin based) captcha solver accounts, see GeneralSettings#isUseAvailableCaptchaSolverAccounts. */
        EXTERNAL_SOLVER_ACCOUNTS_DISABLED
    }

    protected ChallengeSolver() {
    }

    public List<FeedbackType> getSupportedFeedbackTypes() {
        // TODO: Make this abstract
        return null;
    }

    /**
     * Returns the list of captcha types supported by this solver. <br>
     * Important: If a solver supports all reCaptcha captcha types, return RECAPTCHA_V2, RECAPTCHA_V2_ENTERPRISE AND RECAPTCHA_V2_INVISIBLE
     * !
     *
     * @return List of supported captcha types
     */
    public List<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
        // TODO: Make this abstract
        return null;
    }

    /* Returns type of solver e.g. browser solver, local image solver or external solver. */
    public abstract SolverType getSolverType();

    /**
     * Return list of user disabled captcha types if any are disabled. <br>
     * Returns null or empty list of user has not disabled any captcha types.
     */
    public List<CAPTCHA_TYPE> getUserDisabledCaptchaTypes() {
        return null;
    }

    public boolean setInvalid(AbstractResponse<?> response) {
        return false;
    }

    public boolean setUnused(AbstractResponse<?> response) {
        return false;
    }

    public boolean setValid(AbstractResponse<?> response) {
        return false;
    }

    protected ThreadPoolExecutor threadPool;
    private Class<T>             resultType;
    protected SolverService      service;

    /**
     *
     * @param i
     *            size of the threadpool. if i<=0 there will be no threadpool. each challenge will get a new thread in this case
     */
    @SuppressWarnings("unchecked")
    public ChallengeSolver(SolverService service, int i) {
        this.service = service;
        if (service == null) {
            this.service = (SolverService) this;
        }
        initThreadPool(i);
        Class<?> cls = this.getClass();
        while (true) {
            Type superClass = cls.getGenericSuperclass();
            if (superClass == null) {
                throw new IllegalArgumentException("Wrong Construct");
            }
            if (superClass instanceof Class) {
                cls = (Class<?>) superClass;
            } else if (superClass instanceof ParameterizedType) {
                resultType = (Class<T>) ((ParameterizedType) superClass).getActualTypeArguments()[0];
                break;
            } else {
                throw new IllegalArgumentException("Wrong Construct");
            }
        }
    }

    public ChallengeSolver(int i) {
        this(null, i);
    }

    protected final WeakHashMap<SolverJob<T>, JobRunnable<T>> map = new WeakHashMap<SolverJob<T>, JobRunnable<T>>();

    public SolverService getService() {
        return service;
    }

    public boolean isEnabled() {
        return getService().isEnabled();
    }

    public List<SolverJob<T>> listJobs() {
        synchronized (map) {
            return new ArrayList<SolverJob<T>>(map.keySet());
        }
    }

    public boolean hasJobs() {
        synchronized (map) {
            return map.size() > 0;
        }
    }

    public boolean isJobDone(SolverJob<?> job) {
        synchronized (map) {
            return !map.containsKey(job);
        }
    }

    public void enqueue(SolverJob<T> job) {
        final JobRunnable<T> jr = new JobRunnable<T>(this, job);
        synchronized (map) {
            map.put(job, jr);
            if (threadPool == null) {
                new Thread(jr, "ChallengeSolverThread").start();
            } else {
                threadPool.execute(jr);
            }
        }
    }

    protected static void checkInterruption() throws InterruptedException {
        if (Thread.interrupted()) {
            throw new InterruptedException();
        }
    }

    public void kill(SolverJob<T> job) {
        if (job == null) {
            return;
        }
        synchronized (map) {
            final JobRunnable<T> jr = map.remove(job);
            if (jr != null) {
                job.getLogger().info("Cancel " + jr);
                jr.cancel();
            } else {
                job.getLogger().info("Could not kill " + job + " in " + this);
            }
        }
    }

    private void initThreadPool(int i) {
        if (i <= 0) {
            return;
        }
        threadPool = new ThreadPoolExecutor(i, i, 5000, TimeUnit.MILLISECONDS, new LinkedBlockingDeque<Runnable>(), new ThreadFactory() {
            public Thread newThread(final Runnable r) {
                return new Thread(r, "SolverThread:" + ChallengeSolver.this.toString());
            }
        }, new ThreadPoolExecutor.AbortPolicy());
        threadPool.allowCoreThreadTimeOut(true);
    }

    public abstract void solve(SolverJob<T> solverJob) throws InterruptedException, SolverException, SkipException;

    public Class<T> getResultType() {
        return resultType;
    }

    /**
     * Returns null if challenge can be handled by ChallengeSolver. <br>
     * Returns nun null value if challenge cannot be solved by this ChallengeSolver.
     */
    public ChallengeVetoReason getChallengeVetoReason(final Challenge<?> c) {
        if (!this.isEnabled()) {
            return ChallengeVetoReason.SOLVER_DISABLED;
        }
        final List<CAPTCHA_TYPE> supported_types = this.getSupportedCaptchaTypes();
        final CAPTCHA_TYPE ctype = CAPTCHA_TYPE.getCaptchaTypeForChallenge(c);
        if (supported_types != null && ctype != null) {
            final List<CAPTCHA_TYPE> disabled_types = this.getUserDisabledCaptchaTypes();
            if (disabled_types != null && supported_types.contains(ctype) && disabled_types.contains(ctype)) {
                /* Captcha type is supported by plugin but user has disabled this captcha type for this account. */
                return ChallengeVetoReason.CAPTCHA_TYPE_DISABLED_BY_USER;
            }
            if (!supported_types.contains(ctype)) {
                /* Challenge is not supported by solver */
                return ChallengeVetoReason.UNSUPPORTED_BY_SOLVER;
            }
        }
        if (getResultType() != null && !getResultType().isAssignableFrom(c.getResultType())) {
            // TODO: fix possible NPE in above condition, getResultType should never return null?
            // This should never happen?!
            return ChallengeVetoReason.UNSUITABLE_FOR_SOLVER;
        }
        final CaptchaChallengeFilterResult filterResult = validateChallengeFilters(c);
        switch (filterResult) {
        case FILTERED_BLACKLIST:
            return ChallengeVetoReason.CHALLENGE_BLACKLISTED;
        case FILTERED_WHITELIST:
        case NOT_FILTERED:
        default:
            return null;
        }
    }

    /**
     * Evaluates the challenge against the central {@link CaptchaChallengeFilterController}, using this solver's id (
     * {@link SolverService#getID()}) as solver id.
     *
     * @param c
     *            Challenge to validate
     * @return the filter result: NOT_FILTERED, FILTERED_BLACKLIST or FILTERED_WHITELIST
     */
    protected CaptchaChallengeFilterResult validateChallengeFilters(final Challenge<?> c) {
        return CaptchaChallengeFilterController.getInstance().getFilterResult(c, getService().getID());
    }

    public long getTimeout() {
        return -1;
    }

    public int getWaitForByID(String solverID) {
        Integer obj = getWaitForMap().get(solverID);
        return obj == null ? 0 : obj.intValue();
    }

    private Map<String, Integer> waitForMap = null;

    public synchronized Map<String, Integer> getWaitForMap() {
        if (waitForMap != null) {
            return waitForMap;
        }
        /* Wait-for persistence is disabled for now; use the static default map exposed by the service. */
        waitForMap = Collections.synchronizedMap(getService().getWaitForMapCopy());
        return waitForMap;
    }

    public String toString() {
        return getClass().getSimpleName();
    }
}