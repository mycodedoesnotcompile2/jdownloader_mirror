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
        UNSUPPORTED_FOR_INTERNAL_SPECIAL_REASONS,
        UNSUPPORTED_BROWSER_NO_URL_OPEN,
        UNSUITABLE_FOR_SOLVER,
        CHALLENGE_BLACKLISTED,
        CAPTCHA_TYPE_DISABLED_BY_USER,
        ACCOUNT_DISABLED,
        ACCOUNT_IN_ERROR_STATE,
        ACCOUNT_NOT_ENOUGH_CREDITS
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
    public SolverType getSolverType() {
        // TODO: Make this abstract
        return null;
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

    /* If logins are required for a solver, return false if they are invalid (e.g. empty API key field or invalid API key format). */
    protected boolean validateLogins() {
        return true;
    }

    public boolean isEnabled() {
        return getService().getConfig().isEnabled();
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
        if (!this.validateLogins()) {
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
        } else if (!validateChallengeFilters(c)) {
            return ChallengeVetoReason.CHALLENGE_BLACKLISTED;
        } else {
            return null;
        }
    }

    public boolean isFilterListEnabled() {
        // TODO: Make this abstract
        // TODO: Migrate to new captcha solver config system
        return getService().getConfig().isBlackWhiteListingEnabled();
    }

    public List<CaptchaChallengeFilter> getCaptchaChallengeFilterList() {
        // TODO: Make this abstract
        return null;
    }

    /**
     * Validates challenge against CaptchaChallengeFilter list. Returns true if challenge is allowed, false if blocked.
     *
     * @param c
     *            Challenge to validate
     * @return true if allowed, false if blocked
     */
    protected boolean validateChallengeFilters(final Challenge<?> c) {
        if (!this.isFilterListEnabled()) {
            /* Filter list disabled by user -> No need to check */
            return true;
        }
        final List<CaptchaChallengeFilter> filters = this.getCaptchaChallengeFilterList();
        if (filters == null || filters.isEmpty()) {
            /* No filters configured -> Allow */
            return true;
        }
        /* Check each filter in order (sorted by position) */
        for (int i = 0; i < filters.size(); i++) {
            final CaptchaChallengeFilter filter = filters.get(i);
            if (!filter._isValid()) {
                /* Skip invalid filters */
                continue;
            }
            final CompiledCaptchaChallengeFilter compiledFilter = new CompiledCaptchaChallengeFilter(filter);
            if (!compiledFilter.isValid()) {
                /* Skip invalid/disabled/broken filters */
                continue;
            }
            if (!compiledFilter.matches(c)) {
                /* Filter doesn't match this challenge */
                continue;
            }
            /* Filter matches - apply action */
            if (compiledFilter.getFilterType() == CaptchaChallengeFilter.CaptchaFilterType.BLACKLIST) {
                return false;
            } else {
                /* WHITELIST */
                return true;
            }
        }
        /* No filter matched -> default behavior: allow */
        return true;
    }

    public boolean isDomainBlacklistEnabled() {
        // TODO: Migrate to new captcha solver config system
        return getService().getConfig().isBlackWhiteListingEnabled();
    }

    public List<String> getBlacklistedDomains() {
        return getService().getConfig().getBlacklistEntries();
    }

    public boolean isDomainWhitelistEnabled() {
        // TODO: Migrate to new captcha solver config system
        return getService().getConfig().isBlackWhiteListingEnabled();
    }

    public List<String> getWhitelistedDomains() {
        return getService().getConfig().getWhitelistEntries();
    }

    /**
     * returns true for whitelisted and false for blacklisted
     *
     * @param c
     * @return
     */
    @Deprecated
    protected final boolean validateBlackWhite(final Challenge<?> c) {
        if (!this.isDomainWhitelistEnabled() && !this.isDomainBlacklistEnabled()) {
            /* Black/Whitelist disabled by user -> No need to check */
            return true;
        }
        final Set<String> hosts = new HashSet<String>();
        hosts.add(c.getHost());
        final Plugin plugin = c.getPlugin();
        final String[] siteSupportedNames = plugin.siteSupportedNames();
        if (siteSupportedNames != null) {
            hosts.addAll(Arrays.asList(siteSupportedNames));
        }
        if (this.isDomainWhitelistEnabled()) {
            final List<String> whiteListEntries = getWhitelistedDomains();
            whiteListHandling: if (whiteListEntries != null && whiteListEntries.size() > 0) {
                for (final String whiteListEntry : whiteListEntries) {
                    try {
                        final Pattern whiteListPattern = Pattern.compile(whiteListEntry, Pattern.CASE_INSENSITIVE);
                        for (final String host : hosts) {
                            final Boolean matches = match(c, host, whiteListPattern);
                            if (Boolean.TRUE.equals(matches)) {
                                plugin.getLogger().info(c + " is whitelisted for " + this);
                                return true;
                            }
                        }
                    } catch (Throwable e) {
                        c.getPlugin().getLogger().log(e);
                    }
                }
            }
        }
        if (this.isDomainBlacklistEnabled()) {
            final List<String> blackListEntries = getBlacklistedDomains();
            if (blackListEntries != null && blackListEntries.size() > 0) {
                blackListHandling: for (final String blackListEntry : blackListEntries) {
                    try {
                        final Pattern blackListPattern = Pattern.compile(blackListEntry, Pattern.CASE_INSENSITIVE);
                        for (final String host : hosts) {
                            final Boolean matches = match(c, host, blackListPattern);
                            if (Boolean.TRUE.equals(matches)) {
                                plugin.getLogger().info(c + " is blacklisted for " + this);
                                return false;
                            }
                        }
                    } catch (Throwable e) {
                        c.getPlugin().getLogger().log(e);
                    }
                }
            }
        }
        return true;
    }

    @Deprecated
    private Boolean match(final Challenge<?> c, final String host, final Pattern pattern) {
        if (!StringUtils.equalsIgnoreCase(host, c.getTypeID())) {
            if (pattern.matcher(host + "-" + c.getTypeID()).matches()) {
                return true;
            }
            if (pattern.matcher(host).matches()) {
                return true;
            }
        }
        if (pattern.matcher(c.getTypeID()).matches()) {
            return true;
        }
        return null;
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
        Map<String, Integer> map = getService().getConfig().getWaitForMap();
        if (map == null || map.size() == 0) {
            map = getService().getWaitForOthersDefaultMap();
            getService().getConfig().setWaitForMap(map);
        }
        waitForMap = Collections.synchronizedMap(map);
        return waitForMap;
    }

    public String toString() {
        return getClass().getSimpleName();
    }
}