package org.jdownloader.captcha.v2;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;
import java.util.WeakHashMap;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.config.annotations.LabelInterface;
import org.jdownloader.captcha.v2.solver.jac.SolverException;
import org.jdownloader.captcha.v2.solverjob.SolverJob;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.settings.staticreferences.CFG_CAPTCHA;

import jd.controlling.captcha.SkipException;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;

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

        @Override
        public List<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
            /* Placeholder solver that never receives challenges. */
            return new ArrayList<CAPTCHA_TYPE>();
        }
    };

    public enum FeedbackType {
        REPORT_INVALID_CAPTCHAS,
        REPORT_VALID_CAPTCHAS,
        ABORT_CAPTCHAS
    }

    public enum SolverType implements LabelInterface {
        /** Local automatic solver (no user interaction), e.g. JAC. */
        JD_LOCAL {
            @Override
            public String getLabel() {
                return _GUI.T.JACSolver_getName_();
            }
        },
        /** Local dialog solver: a human solves the captcha in a JDownloader dialog. */
        JD_LOCAL_DIALOG {
            @Override
            public String getLabel() {
                return _GUI.T.DialogBasicCaptchaSolver_getName();
            }
        },
        JD_LOCAL_BROWSER {
            @Override
            public String getLabel() {
                return _GUI.T.BrowserSolverService_getName();
            }
        },
        JD_REMOTE_API {
            @Override
            public String getLabel() {
                return _GUI.T.CaptchaAPISolver_getName();
            }
        },
        EXTERNAL {
            @Override
            public String getLabel() {
                return _GUI.T.CaptchaSolverService_type();
            }
        }
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
        /**
         * The user globally disabled external (plugin based) captcha solver accounts, see
         * GeneralSettings#isUseAvailableCaptchaSolverAccounts.
         */
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
     * Every solver that solves captchas must return a non-empty list: a null or empty list means "supports no captcha type at all" and is
     * treated as a bug, see {@link #getChallengeVetoReason(Challenge)} which vetoes every captcha challenge for such a solver. The only
     * solvers which legitimately return an empty list are those that never receive challenges with a {@link CAPTCHA_TYPE}, e.g. the OAuth
     * solvers. <br>
     * Important: If a solver supports all reCaptcha captcha types, return RECAPTCHA_V2, RECAPTCHA_V2_ENTERPRISE AND RECAPTCHA_V2_INVISIBLE
     * !
     *
     * @return List of supported captcha types
     */
    public abstract List<CAPTCHA_TYPE> getSupportedCaptchaTypes();

    /* Returns type of solver e.g. browser solver, local image solver or external solver. Defined by the solver's service. */
    public SolverType getSolverType() {
        return getService().getType();
    }

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
        return getService().getConfigV3().isEnabled();
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
        if (ctype != null) {
            if (supported_types == null || supported_types.isEmpty()) {
                /* Solver does not declare any supported captcha type (bug in solver implementation) -> Cannot solve any captcha. */
                return ChallengeVetoReason.UNSUPPORTED_BY_SOLVER;
            }
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
        /* Check for filter list entry. */
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

    /**
     * Returns the max time in milliseconds this solver has to solve a challenge before {@link JobRunnable} kills it (see
     * {@link JobRunnable#run()}, which only arms this timeout for values &gt; 0). Default: -1 (no solver-specific timeout). <br>
     * Do not call this directly to decide whether/when to kill a solver -- use {@link #getFinalTimeoutMillis()}, which additionally
     * applies {@link jd.controlling.captcha.CaptchaSettings#getDefaultMaxSolverChallengePollingTimeoutMillis()} as a global fallback/cap.
     */
    public long getTimeoutMillis() {
        return -1;
    }

    /**
     * Combines this solver's own {@link #getTimeoutMillis()} with the global
     * {@link jd.controlling.captcha.CaptchaSettings#getDefaultMaxSolverChallengePollingTimeoutMillis()} default: the global default
     * applies unless the solver itself specifies a lower (positive) value, e.g. a server-side polling timeout. This is what
     * {@link JobRunnable} actually uses.
     */
    public final long getFinalTimeoutMillis() {
        final long ownTimeoutMillis = getTimeoutMillis();
        final long defaultMaxMillis = CFG_CAPTCHA.CFG.getDefaultMaxSolverChallengePollingTimeoutMillis();
        if (ownTimeoutMillis <= 0) {
            return defaultMaxMillis;
        }
        return Math.min(ownTimeoutMillis, defaultMaxMillis);
    }

    /**
     * Returns the actual, safe maximum number of captchas this solver may work on at the same time. Default: unlimited (no local or
     * server-side limit applies to this solver type). See {@link PluginChallengeSolver#getFinalMaxCaptchaThreads()} for the
     * plugin-/account-based override.
     */
    public int getFinalMaxCaptchaThreads() {
        return Integer.MAX_VALUE;
    }

    public String toString() {
        return getClass().getSimpleName();
    }
}