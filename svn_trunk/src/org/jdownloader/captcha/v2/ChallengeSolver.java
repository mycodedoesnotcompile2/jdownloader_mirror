package org.jdownloader.captcha.v2;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.EnumSet;
import java.util.Set;
import java.util.ArrayList;
import java.util.List;
import java.util.WeakHashMap;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import javax.swing.Icon;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.config.annotations.LabelInterface;
import org.jdownloader.captcha.v2.solver.jac.SolverException;
import org.jdownloader.captcha.v2.solver.service.AbstractSolverService;
import org.jdownloader.captcha.v2.solverjob.ChallengeSolverJobListener;
import org.jdownloader.captcha.v2.solverjob.SolverJob;
import org.jdownloader.captcha.v2.solverjob.SolverJob.SolverJobStatus;
import org.jdownloader.captcha.v2.solverjob.SolverJob.SolverOutcome;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.images.NewTheme;
import org.jdownloader.settings.staticreferences.CFG_CAPTCHA;

import jd.controlling.captcha.SkipException;
import jd.controlling.captcha.SkipRequest;
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
        public Set<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
            /* Placeholder solver that never receives challenges. */
            return EnumSet.noneOf(CAPTCHA_TYPE.class);
        }

    };

    public enum SolverFeature implements LabelInterface {
        REPORT_INVALID_CAPTCHAS {
            @Override
            public String getLabel() {
                return _GUI.T.SolverFeature_REPORT_INVALID_CAPTCHAS();
            }
        },
        REPORT_VALID_CAPTCHAS {
            @Override
            public String getLabel() {
                return _GUI.T.SolverFeature_REPORT_VALID_CAPTCHAS();
            }
        },
        /* Solver delivered an answer but JDownloader did not use it, see setUnused. */
        REPORT_UNUSED_CAPTCHAS {
            @Override
            public String getLabel() {
                return _GUI.T.SolverFeature_REPORT_UNUSED_CAPTCHAS();
            }
        },
        /* Challenge without answer can be aborted on the server side, see abstractPluginForCaptchaSolver#abortCaptcha. */
        ABORT_CAPTCHAS {
            @Override
            public String getLabel() {
                return _GUI.T.SolverFeature_ABORT_CAPTCHAS();
            }
        },
        /**
         * The solver can send the proxy of a challenge (see Challenge#getProxy) to its service, so the captcha is solved through the
         * proxy and with the IP address of the proxy. Only needed for solvers which work from another IP address (external solvers).
         */
        PROXY {
            @Override
            public String getLabel() {
                return _GUI.T.SolverFeature_PROXY();
            }
        }
    }

    public enum SolverType implements LabelInterface {
        /** Local automatic solver (no user interaction), e.g. JAC. */
        JD_LOCAL(SolverType.DEFAULT_START_NEXT_AFTER_SECONDS_JD_LOCAL) {
            @Override
            public String getLabel() {
                return _GUI.T.JACSolver_getName_();
            }

            @Override
            public Icon getIcon(final int size) {
                return NewTheme.I().getIcon(IconKey.ICON_OCR, size);
            }
        },
        /** Local dialog solver: a human solves the captcha in a JDownloader dialog. */
        JD_LOCAL_DIALOG(SolverType.DEFAULT_START_NEXT_AFTER_SECONDS_JD_LOCAL_DIALOG) {
            @Override
            public String getLabel() {
                return _GUI.T.DialogBasicCaptchaSolver_getName();
            }

            @Override
            public Icon getIcon(final int size) {
                return NewTheme.I().getIcon(IconKey.ICON_EDIT, size);
            }
        },
        JD_LOCAL_BROWSER(SolverType.DEFAULT_START_NEXT_AFTER_SECONDS_JD_LOCAL_BROWSER) {
            @Override
            public String getLabel() {
                return _GUI.T.BrowserSolverService_getName();
            }

            @Override
            public Icon getIcon(final int size) {
                return NewTheme.I().getIcon(IconKey.ICON_BROWSE, size);
            }
        },
        JD_REMOTE_MYJD(SolverType.DEFAULT_START_NEXT_AFTER_SECONDS_JD_REMOTE_MYJD) {
            @Override
            public String getLabel() {
                return _GUI.T.SolverType_JD_REMOTE_MYJD();
            }

            @Override
            public Icon getIcon(final int size) {
                return new AbstractIcon(IconKey.ICON_LOGO_MYJDOWNLOADER, size);
            }
        },
        EXTERNAL(SolverType.DEFAULT_START_NEXT_AFTER_SECONDS_EXTERNAL) {
            @Override
            public String getLabel() {
                return _GUI.T.CaptchaSolverService_type();
            }

            @Override
            public Icon getIcon(final int size) {
                return NewTheme.I().getIcon(IconKey.ICON_PREMIUM, size);
            }
        };

        /*
         * The defaults (in seconds) of "Start the next solver after" (see CaptchaSolverConfigV3#getStartNextSolverAfterSeconds()). This is
         * the one central place for these numbers: The enum constants above use them, and the config interfaces of the solvers use them as
         * the default of their config entry (an annotation can only use constants).
         */
        /** Default of every solver config which does not override it. */
        public static final int DEFAULT_START_NEXT_AFTER_SECONDS_DEFAULT          = 60;
        /** JAntiCaptcha: local and fast, the next solver is started soon if it has no solution. */
        public static final int DEFAULT_START_NEXT_AFTER_SECONDS_JD_LOCAL         = 2;
        /** Dialog in JDownloader: gives the user some time to answer. */
        public static final int DEFAULT_START_NEXT_AFTER_SECONDS_JD_LOCAL_DIALOG  = 30;
        /** Dialog in the browser: opening the browser and answering takes longer. */
        public static final int DEFAULT_START_NEXT_AFTER_SECONDS_JD_LOCAL_BROWSER = 45;
        /** Remote solver via MyJDownloader: a user answers on another device. */
        public static final int DEFAULT_START_NEXT_AFTER_SECONDS_JD_REMOTE_MYJD   = 30;
        /** All external (paid) solvers: gives the solver time to answer before the next paid solver is started. */
        public static final int DEFAULT_START_NEXT_AFTER_SECONDS_EXTERNAL         = 60;
        /** Highest value in seconds of "Start the next solver after", used by the config entries and the timing dialog. */
        public static final int DEFAULT_START_NEXT_AFTER_SECONDS_MAX                      = 3600;
        private final int       defaultStartNextSolverAfterSeconds;

        private SolverType(final int defaultStartNextSolverAfterSeconds) {
            this.defaultStartNextSolverAfterSeconds = defaultStartNextSolverAfterSeconds;
        }

        /**
         * The default of the "start the next solver after" time in seconds for solvers of this type: how long a solver of this type may
         * work on a captcha without a solution before the next solver of the job is started in parallel. The numbers are the
         * DEFAULT_START_NEXT_AFTER_SECONDS_* constants of this enum, which the config interfaces of the solvers use as the default of their config
         * entry. The value the running solver really has as default is the one of its config (see
         * JobRunnable#getDefaultStartNextSolverAfterSeconds).
         */
        public int getDefaultStartNextSolverAfterSeconds() {
            return defaultStartNextSolverAfterSeconds;
        }

        /** Icon of this solver type, e.g. for the solver type selection of the solver comparison table. */
        public abstract Icon getIcon(int size);
    }

    public enum ChallengeVetoReason {
        SOLVER_DISABLED,
        UNSUPPORTED_BY_SOLVER,
        UNSUPPORTED_BY_SOLVER_ACCOUNT,
        UNSUPPORTED_FOR_INTERNAL_SPECIAL_REASONS,
        UNSUPPORTED_BROWSER_NO_URL_OPEN,
        UNSUPPORTED_BROWSER_NOT_FOUND,
        UNSUITABLE_FOR_SOLVER,
        CHALLENGE_BLACKLISTED,
        CAPTCHA_TYPE_DISABLED_BY_USER,
        ACCOUNT_DISABLED,
        ACCOUNT_IN_ERROR_STATE,
        ACCOUNT_NOT_ENOUGH_BALANCE,
        /** One of the solver's enabled custom limit rules (max captchas per time interval) is currently used up. */
        CUSTOM_LIMIT_REACHED,
        /** The challenge requires the IP address of the user (see Challenge#isSameIPRequired) but this external solver cannot use proxies. */
        SAME_IP_REQUIRED_PROXY_UNSUPPORTED,
        /** The challenge requires the IP address of the user and this external solver needs a proxy for that but the challenge has none. */
        SAME_IP_REQUIRED_NO_PROXY,
        /**
         * The user globally disabled external (plugin based) captcha solver accounts, see
         * GeneralSettings#isUseAvailableCaptchaSolverAccounts.
         */
        EXTERNAL_SOLVER_ACCOUNTS_DISABLED,
        /**
         * The silent mode is active and set so that captchas must not be solved by the user ("Skip the Captcha" or "Do not show a dialog,
         * but try auto solver"): Solvers which need the user (dialog, browser) are not used. Depends on the state of the silent mode, so it
         * is a temporary reason and not an "unsupported" one, see {@link ChallengeSolver#getUnsupportedReason(Challenge)}.
         */
        SILENT_MODE
    }

    protected ChallengeSolver() {
    }

    /**
     * Returns the captcha feedback types this solver supports (see {@link #setValid(AbstractResponse)},
     * {@link #setInvalid(AbstractResponse)} and {@link #setUnused(AbstractResponse)}). Default: none, because the default implementations of
     * those methods do nothing (e.g. the BrowserSolver and the manual dialog solvers cannot receive any feedback). Solvers that really
     * handle feedback have to override this method. Null or an empty set means that no feedback is supported.
     */
    public Set<SolverFeature> getSolverFeatures() {
        return EnumSet.noneOf(SolverFeature.class);
    }

    /**
     * Returns the list of captcha types supported by this solver. <br>
     * Every solver that solves captchas must return a non-empty list: a null or empty list means "supports no captcha type at all" and is
     * treated as a bug, see {@link #getUnsupportedReason(Challenge)} which vetoes every challenge for such a solver. The only solvers
     * which legitimately return an empty list are those that never receive challenges with a {@link CAPTCHA_TYPE}, e.g. the OAuth
     * solvers: They have to override {@link #getUnsupportedReason(Challenge)} without calling super. <br>
     * Important: If a solver supports all reCaptcha captcha types, return RECAPTCHA_V2, RECAPTCHA_V2_ENTERPRISE AND RECAPTCHA_V2_INVISIBLE
     * !
     *
     * @return Set of supported captcha types
     */
    public abstract Set<CAPTCHA_TYPE> getSupportedCaptchaTypes();

    /* Returns type of solver e.g. browser solver, local image solver or external solver. Defined by the solver's service. */
    public SolverType getSolverType() {
        return getService().getType();
    }

    /**
     * Return list of user disabled captcha types if any are disabled. <br>
     * Returns null or empty list of user has not disabled any captcha types.
     */
    public Set<CAPTCHA_TYPE> getUserDisabledCaptchaTypes() {
        return null;
    }

    /** True if this solver supports the given feature, see {@link #getSolverFeatures()}. */
    public boolean supportsFeature(final SolverFeature feature) {
        final Set<SolverFeature> features = getSolverFeatures();
        return features != null && features.contains(feature);
    }

    /**
     * Reports to the solver that its answer was not correct. Only calls {@link #onInvalid(AbstractResponse)} if the solver declares
     * {@link SolverFeature#REPORT_INVALID_CAPTCHAS}.
     *
     * @return true if the feedback was delivered
     */
    public final boolean setInvalid(final AbstractResponse<?> response) {
        return supportsFeature(SolverFeature.REPORT_INVALID_CAPTCHAS) && onInvalid(response);
    }

    /**
     * Tells the solver that its answer was not used. Only calls {@link #onUnused(AbstractResponse)} if the solver declares
     * {@link SolverFeature#REPORT_UNUSED_CAPTCHAS}.
     *
     * @return true if the feedback was delivered
     */
    public final boolean setUnused(final AbstractResponse<?> response) {
        return supportsFeature(SolverFeature.REPORT_UNUSED_CAPTCHAS) && onUnused(response);
    }

    /**
     * Reports to the solver that its answer was correct. Only calls {@link #onValid(AbstractResponse)} if the solver declares
     * {@link SolverFeature#REPORT_VALID_CAPTCHAS}.
     *
     * @return true if the feedback was delivered
     */
    public final boolean setValid(final AbstractResponse<?> response) {
        return supportsFeature(SolverFeature.REPORT_VALID_CAPTCHAS) && onValid(response);
    }

    /** Override to handle "incorrect" feedback. Only called if the solver supports {@link SolverFeature#REPORT_INVALID_CAPTCHAS}. */
    protected boolean onInvalid(final AbstractResponse<?> response) {
        return false;
    }

    /** Override to handle "unused" feedback. Only called if the solver supports {@link SolverFeature#REPORT_UNUSED_CAPTCHAS}. */
    protected boolean onUnused(final AbstractResponse<?> response) {
        return false;
    }

    /** Override to handle "correct" feedback. Only called if the solver supports {@link SolverFeature#REPORT_VALID_CAPTCHAS}. */
    protected boolean onValid(final AbstractResponse<?> response) {
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

    /**
     * For solvers which need the user to solve the challenge on this computer (dialog, browser): Has to be called before the user is
     * asked, because the silent mode may have been activated after the job was created (a solver is not used for a job created during such
     * a silent mode at all, see {@link ChallengeVetoReason#SILENT_MODE}). <br>
     * If the silent mode does not allow to ask the user, only this solver is skipped, it must end without asking: The job is not aborted,
     * the other solvers can still solve the challenge. If none of them does, all captchas are skipped (BLOCK_ALL_CAPTCHAS) as soon as all solvers
     * are done.
     *
     * @return true if the solver must not ask the user
     */
    protected final boolean isAskingUserBlockedBySilentMode(final SolverJob<T> job) throws InterruptedException {
        if (!AbstractSolverService.isManualSolvingBlockedBySilentMode()) {
            checkInterruption();
            return false;
        }
        endBecauseOfSilentMode(job, "The silent mode does not allow to ask the user");
        return true;
    }

    /**
     * Marks this solver as ended because of the silent mode (the user was not asked): The solver must end without an answer afterwards.
     * The job is not aborted, the other solvers can still solve the challenge. If none of them does, all captchas are skipped
     * (BLOCK_ALL_CAPTCHAS) as soon as all solvers are done (if all of them ended because of the silent mode, the job status is {@link SolverJobStatus#SILENT_MODE}).
     */
    protected final void endBecauseOfSilentMode(final SolverJob<T> job, final String reason) {
        job.setSolverResult(this, SolverOutcome.SILENT_MODE, reason);
        job.getLogger().info("Silent mode: " + this + " does not ask the user, the other solvers can still solve the challenge");
        job.getEventSender().addListener(new ChallengeSolverJobListener() {
            @Override
            public void onSolverTimedOut(final ChallengeSolver<?> parameter) {
            }

            @Override
            public void onSolverStarts(final ChallengeSolver<?> parameter) {
            }

            @Override
            public void onSolverJobReceivedNewResponse(final AbstractResponse<?> response) {
            }

            @Override
            public void onSolverDone(final ChallengeSolver<?> solver) {
                if (job.isDone()) {
                    if (!job.isSolved()) {
                        /* BLOCK_ALL_CAPTCHAS like in ChallengeResponseController#handle: The next captchas run into the same situation. */
                        ChallengeResponseController.getInstance().setSkipRequest(SkipRequest.BLOCK_ALL_CAPTCHAS, ChallengeSolver.this, job.getChallenge());
                    }
                    job.getEventSender().removeListener(this);
                }
            }
        });
    }

    /**
     * For solvers which showed a dialog: The user chose an external solver in it ("use external solver instead"), so the dialog was closed
     * without an answer and without a skip. Records that for this solver: Without a result a solver that ended without an answer looks
     * like it is still working until the whole job is done, e.g. in the captcha test panel. The job goes on with the chosen solver.
     */
    protected final void endBecauseUserChoseExternalSolver(final SolverJob<T> job, final ChallengeSolver<?> externalSolver) {
        job.setSolverResult(this, SolverOutcome.ABORTED_USER_CHOSE_OTHER_SOLVER, "The user chose the external solver " + externalSolver.getService().getName() + " in the dialog instead");
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
     * Returns nun null value if challenge cannot be solved by this ChallengeSolver. <br>
     * Checks the user's settings first (solver enabled, captcha type disabled), then {@link #getUnsupportedReason(Challenge)}, then the
     * captcha rules. <br>
     * Subclasses that add checks independent of the user's settings must override {@link #getUnsupportedReason(Challenge)} instead.
     * Subclasses that add checks depending on state or settings (e.g. account state) have to call super first, then check the solver
     * specific things and return null (= can be handled) if none of them vetoes.
     */
    public ChallengeVetoReason getChallengeVetoReason(final Challenge<?> c) {
        if (!this.isEnabled()) {
            return ChallengeVetoReason.SOLVER_DISABLED;
        }
        final Set<CAPTCHA_TYPE> supported_types = this.getSupportedCaptchaTypes();
        final CAPTCHA_TYPE ctype = CAPTCHA_TYPE.getCaptchaTypeForChallenge(c);
        if (ctype != null && supported_types != null) {
            final Set<CAPTCHA_TYPE> disabled_types = this.getUserDisabledCaptchaTypes();
            if (disabled_types != null && supported_types.contains(ctype) && disabled_types.contains(ctype)) {
                /* Captcha type is supported by plugin but user has disabled this captcha type for this account. */
                return ChallengeVetoReason.CAPTCHA_TYPE_DISABLED_BY_USER;
            }
        }
        final ChallengeVetoReason unsupportedReason = getUnsupportedReason(c);
        if (unsupportedReason != null) {
            return unsupportedReason;
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
     * Returns null if this solver is technically able to solve the given challenge, no matter whether the user enabled it or not.
     * Returns a non-null value if it can never solve it, e.g. because it does not support the captcha type. <br>
     * Only reasons which do not depend on the user's settings (solver enabled, captcha type disabled, captcha rules, ...) may be returned
     * here. This allows to find out that "the only thing in the way is the user's own configuration", see
     * {@link #getChallengeVetoReason(Challenge)} which includes this check. <br>
     * Subclasses that add their own checks have to call super first, then check the solver specific things and return null if none of them
     * vetoes. Subclasses must not override {@link #getChallengeVetoReason(Challenge)} for such checks.
     */
    public ChallengeVetoReason getUnsupportedReason(final Challenge<?> c) {
        final Set<CAPTCHA_TYPE> supported_types = this.getSupportedCaptchaTypes();
        final CAPTCHA_TYPE ctype = CAPTCHA_TYPE.getCaptchaTypeForChallenge(c);
        if (supported_types == null || supported_types.isEmpty()) {
            /*
             * Solver does not declare any supported captcha type: An empty list never means "supports everything" -> Cannot solve anything.
             * Solvers which legitimately support no captcha type (e.g. OAuth) have to override this method without calling super.
             */
            return ChallengeVetoReason.UNSUPPORTED_BY_SOLVER;
        }
        if (ctype != null && !supported_types.contains(ctype)) {
            /* Challenge is not supported by solver */
            return ChallengeVetoReason.UNSUPPORTED_BY_SOLVER;
        }
        if (getResultType() != null && !getResultType().isAssignableFrom(c.getResultType())) {
            // TODO: fix possible NPE in above condition, getResultType should never return null?
            // This should never happen?!
            return ChallengeVetoReason.UNSUITABLE_FOR_SOLVER;
        }
        if (c.isSameIPRequired() && getSolverType() == SolverType.EXTERNAL) {
            /*
             * External solvers work from another IP address than the user: They can only solve such a challenge through the proxy that was
             * used for the requests of the challenge.
             */
            if (!supportsFeature(SolverFeature.PROXY)) {
                return ChallengeVetoReason.SAME_IP_REQUIRED_PROXY_UNSUPPORTED;
            } else if (!c.hasProxy()) {
                return ChallengeVetoReason.SAME_IP_REQUIRED_NO_PROXY;
            }
        }
        return null;
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
     * Returns the maximum number of captchas this solver (i.e. its {@link SolverService}, summed over all of its accounts, if any) may
     * work on at the same time. Default: unlimited (no local limit applies to this solver type). See
     * {@link PluginChallengeSolver#getFinalMaxCaptchaThreads()} for the plugin-based override (the user's own limit). <br>
     * Not to be confused with {@link #getFinalMaxCaptchaThreadsPerAccount()}, which is a separate limit that applies to every single
     * account on its own.
     */
    public int getFinalMaxCaptchaThreads() {
        return Integer.MAX_VALUE;
    }

    /**
     * Returns the maximum number of captchas this solver may work on at the same time with the one account it is bound to, independent of
     * how many other accounts the same solver service has. Only meaningful for account based solvers; default: unlimited. See
     * {@link PluginChallengeSolver#getFinalMaxCaptchaThreadsPerAccount()} for the plugin-based override (the service's server-side limit).
     * Enforced by {@link ChallengeResponseController#reserveCaptchaSlot(ChallengeSolver)} next to
     * {@link #getFinalMaxCaptchaThreads()}.
     */
    public int getFinalMaxCaptchaThreadsPerAccount() {
        return Integer.MAX_VALUE;
    }

    /**
     * Max number of captchas that can be worked on at the same time when only this solver (= one account) is used, i.e. the smaller one of
     * {@link #getFinalMaxCaptchaThreads()} and {@link #getFinalMaxCaptchaThreadsPerAccount()}, at least 1. <br>
     * Do not use this for the limit enforcement in {@link ChallengeResponseController}: there the service-wide limit must not include the
     * per-account limit, because a service with several accounts may use more captchas than a single account allows.
     */
    public final int getFinalMaxCaptchaThreadsSingleAccount() {
        return Math.max(1, Math.min(getFinalMaxCaptchaThreads(), getFinalMaxCaptchaThreadsPerAccount()));
    }

    /**
     * Returns the custom limit rules (max captchas per time interval) that currently apply to this solver, i.e. only the enabled ones, and
     * only while the solver's custom limits are switched on. Default: null (no custom limits). The returned list is a snapshot and may be
     * freely iterated. It is evaluated by {@link ChallengeResponseController}: a solver with a used up rule is vetoed for new challenges (see
     * {@link ChallengeVetoReason#CUSTOM_LIMIT_REACHED}) and every started captcha counts against all rules of the solver. See
     * {@link PluginChallengeSolver#getCustomLimitRules()} for the plugin-based override.
     */
    public List<CaptchaSolverLimitRule> getCustomLimitRules() {
        return null;
    }

    public String toString() {
        return getClass().getSimpleName();
    }
}