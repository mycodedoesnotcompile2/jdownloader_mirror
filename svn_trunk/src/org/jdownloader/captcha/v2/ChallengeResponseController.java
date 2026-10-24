package org.jdownloader.captcha.v2;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import org.appwork.storage.config.JsonConfig;
import org.appwork.timetracker.TimeTracker;
import org.appwork.timetracker.TimeTrackerController;
import org.appwork.timetracker.TrackerRule;
import org.appwork.uio.CloseReason;
import org.appwork.uio.ConfirmDialogInterface;
import org.appwork.uio.UIOManager;
import org.appwork.utils.Application;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Time;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.logging2.LogSource;
import org.jdownloader.api.captcha.CaptchaAPISolver;
import org.jdownloader.captcha.blacklist.BlacklistEntry;
import org.jdownloader.captcha.blacklist.CaptchaBlackList;
import org.jdownloader.captcha.event.ChallengeResponseEvent;
import org.jdownloader.captcha.event.ChallengeResponseEventSender;
import org.jdownloader.captcha.v2.ChallengeSolver.ChallengeVetoReason;
import org.jdownloader.captcha.v2.ChallengeSolver.SolverType;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CloudflareTurnstileChallenge;
import org.jdownloader.captcha.v2.challenge.hcaptcha.HCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.oauth.AccountOAuthSolver;
import org.jdownloader.captcha.v2.challenge.oauth.OAuthDialogSolver;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.solver.browser.AbstractBrowserChallenge;
import org.jdownloader.captcha.v2.solver.browser.BrowserSolver;
import org.jdownloader.captcha.v2.solver.gui.DialogBasicCaptchaSolver;
import org.jdownloader.captcha.v2.solver.gui.DialogMultiClickCaptchaSolver;
import org.jdownloader.captcha.v2.solver.jac.JACSolver;
import org.jdownloader.captcha.v2.solverjob.ResponseList;
import org.jdownloader.captcha.v2.solverjob.SolverJob;
import org.jdownloader.controlling.UniqueAlltimeID;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.logging.LogController;
import org.jdownloader.plugins.components.captchasolver.PluginForCaptchaSolverSolverService;
import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolver;
import org.jdownloader.plugins.controller.LazyPlugin.FEATURE;
import org.jdownloader.plugins.controller.PluginClassLoader;
import org.jdownloader.plugins.controller.PluginClassLoader.PluginClassLoaderChild;
import org.jdownloader.plugins.controller.host.HostPluginController;
import org.jdownloader.plugins.controller.host.LazyHostPlugin;
import org.jdownloader.plugins.controller.host.LazyHostPluginFilter;
import org.jdownloader.settings.staticreferences.CFG_CAPTCHA;
import org.jdownloader.settings.staticreferences.CFG_GENERAL;
import org.jdownloader.updatev2.UpdateController;

import jd.controlling.AccountController;
import jd.controlling.AccountFilter;
import jd.controlling.captcha.CaptchaSettings;
import jd.controlling.captcha.SkipException;
import jd.controlling.captcha.SkipRequest;
import jd.gui.swing.jdgui.views.settings.panels.anticaptcha.CaptchaConfigPanel;
import jd.plugins.Account;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import java.awt.Cursor;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.dialog.ConfirmDialog;
import org.appwork.utils.swing.dialog.Dialog;
import net.miginfocom.swing.MigLayout;

public class ChallengeResponseController {
    private static final ChallengeResponseController INSTANCE         = new ChallengeResponseController();
    protected final static CaptchaSettings           CAPTCHA_SETTINGS = JsonConfig.create(CaptchaSettings.class);

    /**
     * get the only existing instance of ChallengeResponseController. This is a singleton
     *
     * @return
     */
    public static ChallengeResponseController getInstance() {
        return ChallengeResponseController.INSTANCE;
    }

    private ChallengeResponseEventSender eventSender;

    public ChallengeResponseEventSender getEventSender() {
        return eventSender;
    }

    private LogSource             logger;
    private TimeTrackerController trackerCache;

    /**
     * Create a new instance of ChallengeResponseController. This is a singleton class. Access the only existing instance by using
     * {@link #getInstance()}.
     */
    private ChallengeResponseController() {
        logger = LogController.getInstance().getLogger(getClass().getName());
        eventSender = new ChallengeResponseEventSender(logger);
        trackerCache = new TimeTrackerController();
        HashMap<String, ArrayList<CaptchaQualityEnsuranceRule>> rules = CFG_CAPTCHA.CFG.getQualityEnsuranceRules();
        if (rules == null) {
            rules = new HashMap<String, ArrayList<CaptchaQualityEnsuranceRule>>();
        }
        boolean save = false;
        save = addDefaultRules(rules, RecaptchaV2Challenge.RECAPTCHAV2, new CaptchaQualityEnsuranceRule(60, 10 * 60 * 1000), new CaptchaQualityEnsuranceRule(6, 60 * 1000), new CaptchaQualityEnsuranceRule(3, 30 * 1000), new CaptchaQualityEnsuranceRule(2, 10 * 1000)) || save;
        save = addDefaultRules(rules, HCaptchaChallenge.getChallengeType(), new CaptchaQualityEnsuranceRule(60, 10 * 60 * 1000), new CaptchaQualityEnsuranceRule(6, 60 * 1000), new CaptchaQualityEnsuranceRule(3, 30 * 1000), new CaptchaQualityEnsuranceRule(2, 10 * 1000)) || save;
        if (save) {
            CFG_CAPTCHA.CFG.setQualityEnsuranceRules(rules);
        }
        for (Entry<String, ArrayList<CaptchaQualityEnsuranceRule>> es : rules.entrySet()) {
            ArrayList<CaptchaQualityEnsuranceRule> rc = es.getValue();
            final TimeTracker tracker = trackerCache.getTracker(es.getKey());
            for (CaptchaQualityEnsuranceRule r : rc) {
                logger.info("Add Captcha Limit Rule for " + es.getKey() + " " + r.getLimit() + " Reqs in " + TimeFormatter.formatMilliSeconds(r.getInterval(), 0));
                tracker.addRule(new TrackerRule(r.getLimit(), r.getInterval()));
            }
        }
    }

    private boolean addDefaultRules(HashMap<String, ArrayList<CaptchaQualityEnsuranceRule>> rules, String key, CaptchaQualityEnsuranceRule... defList) {
        ArrayList<CaptchaQualityEnsuranceRule> rc = rules.get(key);
        if (rc == null || rc.size() == 0) {
            rc = new ArrayList<CaptchaQualityEnsuranceRule>();
            for (CaptchaQualityEnsuranceRule r : defList) {
                rc.add(r);
            }
            rules.put(key, rc);
            return true;
        }
        return false;
    }

    private final AtomicBoolean init = new AtomicBoolean(false);

    public void init() {
        if (!init.compareAndSet(false, true)) {
            return;
        }
        addSolver(JACSolver.getInstance());
        /* Legacy paid captcha solver engines were removed; they are now provided as captcha solver plugins. */
        if (!Application.isHeadless()) {
            addSolver(DialogBasicCaptchaSolver.getInstance());
            addSolver(DialogMultiClickCaptchaSolver.getInstance());
            addSolver(BrowserSolver.getInstance());
            addSolver(OAuthDialogSolver.getInstance());
        }
        addSolver(AccountOAuthSolver.getInstance());
        addSolver(CaptchaAPISolver.getInstance());
        /* Add plugin captcha solver services */
        final List<LazyHostPlugin> captchaSolverPlugins = HostPluginController.getInstance().list(LazyHostPluginFilter.ALL_CAPTCHA_SOLVERS);
        for (final LazyHostPlugin plg : captchaSolverPlugins) {
            final PluginClassLoaderChild pluginClassLoaderChild = PluginClassLoader.getThreadPluginClassLoaderChild();
            abstractPluginForCaptchaSolver plugin;
            try {
                plugin = Plugin.getNewPluginInstance(null, plg, pluginClassLoaderChild);
            } catch (PluginException e) {
                e.printStackTrace();
                continue;
            }
            final PluginForCaptchaSolverSolverService solverservice = new PluginForCaptchaSolverSolverService(plugin);
            addSolverService(solverservice);
        }
    }

    public List<ChallengeSolver<?>> listSolvers() {
        return new ArrayList<ChallengeSolver<?>>(solverList);
    }

    private final HashMap<String, SolverService> solverMap   = new HashMap<String, SolverService>();
    private final List<SolverService>            serviceList = new CopyOnWriteArrayList<SolverService>();

    private synchronized boolean addSolver(ChallengeSolver<?> solver) {
        addSolverService(solver.getService());
        return solverList.add(solver);
    }

    public synchronized boolean addSolverService(SolverService service) {
        if (solverMap.put(service.getID(), service) == null) {
            serviceList.add(service);
            return true;
        }
        return false;
    }

    public <E> void fireNewAnswerEvent(SolverJob<E> job, AbstractResponse<E> abstractResponse) {
        eventSender.fireEvent(new ChallengeResponseEvent(this, ChallengeResponseEvent.Type.JOB_ANSWER, abstractResponse, job));
    }

    public List<SolverJob<?>> listJobs() {
        synchronized (activeJobs) {
            return new ArrayList<SolverJob<?>>(activeJobs);
        }
    }

    public boolean hasPendingJobs() {
        synchronized (activeJobs) {
            return activeJobs.size() > 0;
        }
    }

    public void fireBeforeSolveEvent(SolverJob<?> job, ChallengeSolver<?> solver) {
        eventSender.fireEvent(new ChallengeResponseEvent(this, ChallengeResponseEvent.Type.SOLVER_START, solver, job));
    }

    public void fireAfterSolveEvent(SolverJob<?> job, ChallengeSolver<?> solver) {
        job._notifyAll();
        eventSender.fireEvent(new ChallengeResponseEvent(this, ChallengeResponseEvent.Type.SOLVER_END, solver, job));
    }

    private void fireNewJobEvent(SolverJob<?> job) {
        eventSender.fireEvent(new ChallengeResponseEvent(this, ChallengeResponseEvent.Type.NEW_JOB, job));
    }

    private void fireJobDone(SolverJob<?> job) {
        eventSender.fireEvent(new ChallengeResponseEvent(this, ChallengeResponseEvent.Type.JOB_DONE, job));
    }

    private final List<ChallengeSolver<?>>                          solverList                       = new CopyOnWriteArrayList<ChallengeSolver<?>>();
    private final List<SolverJob<?>>                                activeJobs                       = new ArrayList<SolverJob<?>>();
    private final HashMap<UniqueAlltimeID, SolverJob<?>>            challengeIDToJobMap              = new HashMap<UniqueAlltimeID, SolverJob<?>>();
    /**
     * Tracks how many solve attempts are currently in-flight per solver service (key: {@link SolverService#getID()}), i.e. summed over all
     * accounts of that service. Counted against {@link ChallengeSolver#getFinalMaxCaptchaThreads()}. Used by {@link #createList(Challenge)}
     * to push a solver that is already at/over a limit to the end of the candidate list instead of excluding it outright, so it still gets
     * used as a last resort if no solver with free capacity is available, and by {@link #reserveCaptchaSlot(ChallengeSolver)}/
     * {@link #releaseCaptchaSlot(ChallengeSolver)}, which {@link JobRunnable} uses to actually enforce the limits around a solve attempt.
     */
    private final ConcurrentHashMap<String, AtomicInteger>          activeCaptchasByServiceID        = new ConcurrentHashMap<String, AtomicInteger>();
    /**
     * Same as {@link #activeCaptchasByServiceID}, but per account (key: {@link Account#getId()}), only for account based solvers. Counted
     * against {@link ChallengeSolver#getFinalMaxCaptchaThreadsPerAccount()}, which applies to every account on its own.
     */
    private final ConcurrentHashMap<UniqueAlltimeID, AtomicInteger> activeCaptchasByAccountID        = new ConcurrentHashMap<UniqueAlltimeID, AtomicInteger>();
    /**
     * Guards reserving/releasing slots and is what waiting solvers wait on. A single lock for both counters makes it possible to check and
     * increment the service and the account counter together atomically.
     */
    private final Object                                            CAPTCHA_SLOT_LOCK                = new Object();
    /**
     * Start times (ms, ascending) of the captchas recently started per solver service (key: {@link SolverService#getID()}): the basis of
     * the custom limit rules ({@link ChallengeSolver#getCustomLimitRules()}). Only kept in memory, so a restart of JDownloader resets them,
     * and only filled for solvers that have enabled custom limit rules. Guarded by {@link #CAPTCHA_SLOT_LOCK}.
     */
    private final HashMap<String, ArrayList<Long>>                  captchaStartTimesByServiceID     = new HashMap<String, ArrayList<Long>>();
    /** Max time to wait for a solver's max-simultaneous-captchas limit to free up before giving up on using that solver for a job. */
    private static final long                                       CAPTCHA_SLOT_WAIT_TIMEOUT_MILLIS = 60 * 1000L;

    /** Outcome of {@link #reserveCaptchaSlot(ChallengeSolver)}. */
    public enum CaptchaSlotResult {
        /** A slot was reserved (and the captcha counted against the custom limits): the caller must release it afterwards. */
        RESERVED,
        /** One of the solver's enabled custom limit rules is used up -> the caller must not run this solver. */
        CUSTOM_LIMIT_REACHED,
        /** A max-simultaneous-captchas limit was still reached after waiting -> the caller must not run this solver. */
        NO_FREE_SLOT
    }

    private static <K> int getActiveCaptchas(final ConcurrentHashMap<K, AtomicInteger> counters, final K key) {
        final AtomicInteger counter = counters.get(key);
        return counter == null ? 0 : counter.get();
    }

    private static <K> AtomicInteger getOrCreateActiveCaptchasCounter(final ConcurrentHashMap<K, AtomicInteger> counters, final K key) {
        final AtomicInteger existing = counters.get(key);
        if (existing != null) {
            return existing;
        }
        final AtomicInteger created = new AtomicInteger(0);
        final AtomicInteger race = counters.putIfAbsent(key, created);
        return race != null ? race : created;
    }

    /** Returns the account a solver is bound to (the key of the per-account limit), or null for solvers without an account. */
    private static UniqueAlltimeID getSlotAccountID(final ChallengeSolver<?> solver) {
        if (solver instanceof PluginChallengeSolver) {
            return ((PluginChallengeSolver<?>) solver).getAccount().getId();
        }
        return null;
    }

    /** True if the account the given solver is bound to is already at/over its own per-account limit (not the solver-wide limit). */
    private boolean isAtAccountLimit(final ChallengeSolver<?> solver) {
        final UniqueAlltimeID accountID = getSlotAccountID(solver);
        return accountID != null && getActiveCaptchas(activeCaptchasByAccountID, accountID) >= solver.getFinalMaxCaptchaThreadsPerAccount();
    }

    /** True if the solver is at/over the solver-wide limit or the limit of its own account. */
    private boolean isOverCaptchaThreadsLimit(final ChallengeSolver<?> solver) {
        return getActiveCaptchas(activeCaptchasByServiceID, solver.getService().getID()) >= solver.getFinalMaxCaptchaThreads() || isAtAccountLimit(solver);
    }

    /**
     * Returns a description of the first used up rule, or null if every rule still has room for one more captcha. Also forgets the start
     * times that are older than the longest rule interval. The caller must hold {@link #CAPTCHA_SLOT_LOCK}.
     */
    private String getExhaustedCustomLimit(final String serviceID, final List<CaptchaSolverLimitRule> rules, final long now) {
        final ArrayList<Long> startTimes = captchaStartTimesByServiceID.get(serviceID);
        if (startTimes == null) {
            return null;
        }
        long longestIntervalMillis = 0;
        for (final CaptchaSolverLimitRule rule : rules) {
            longestIntervalMillis = Math.max(longestIntervalMillis, rule._getIntervalMillis());
        }
        while (!startTimes.isEmpty() && startTimes.get(0).longValue() < now - longestIntervalMillis) {
            startTimes.remove(0);
        }
        for (final CaptchaSolverLimitRule rule : rules) {
            final long intervalMillis = rule._getIntervalMillis();
            int startedInInterval = 0;
            for (final Long startTime : startTimes) {
                if (startTime.longValue() >= now - intervalMillis) {
                    startedInInterval++;
                }
            }
            if (startedInInterval < rule.getMaxCaptchas()) {
                continue;
            }
            /* The rule is usable again as soon as enough of the oldest captchas in the interval have left it. */
            final long usableAgainAt = startTimes.get(startTimes.size() - rule.getMaxCaptchas()).longValue() + intervalMillis;
            final String name = rule.getName() != null && rule.getName().length() > 0 ? " \"" + rule.getName() + "\"" : "";
            return "rule" + name + ": " + startedInInterval + "/" + rule.getMaxCaptchas() + " captchas in the last " + rule.getInterval() + " " + rule.getUnit().name().toLowerCase(Locale.ENGLISH) + ", usable again in " + TimeFormatter.formatMilliSeconds(Math.max(0, usableAgainAt - now), 0);
        }
        return null;
    }

    /**
     * True if one of the solver's enabled custom limit rules (see {@link ChallengeSolver#getCustomLimitRules()}) is currently used up, so
     * the solver must not get another captcha. Used to veto the solver for new challenges; logs which rule is used up.
     */
    public boolean isCustomLimitReached(final ChallengeSolver<?> solver) {
        final List<CaptchaSolverLimitRule> rules = solver.getCustomLimitRules();
        if (rules == null || rules.isEmpty()) {
            return false;
        }
        final String exhausted;
        synchronized (CAPTCHA_SLOT_LOCK) {
            exhausted = getExhaustedCustomLimit(solver.getService().getID(), rules, System.currentTimeMillis());
        }
        if (exhausted == null) {
            return false;
        }
        logger.info(solver + " is not used because of a custom limit " + exhausted);
        return true;
    }

    /**
     * Reserves a "slot" for the given solver against both its {@link ChallengeSolver#getFinalMaxCaptchaThreads()} limit (all accounts of
     * the service together) and, for account based solvers, its {@link ChallengeSolver#getFinalMaxCaptchaThreadsPerAccount()} limit (its
     * own account only). If either limit is currently reached, this waits up to {@link #CAPTCHA_SLOT_WAIT_TIMEOUT_MILLIS} for a slot to
     * free up. <br>
     * The solver's enabled custom limit rules ({@link ChallengeSolver#getCustomLimitRules()}) are checked at the same time, atomically with
     * counting the new captcha, so parallel jobs cannot both slip through the last free place of a rule. A used up rule is not waited for
     * (it may stay used up for hours); the solver is skipped instead. <br>
     * Every successful reservation must be paired with a matching {@link #releaseCaptchaSlot(ChallengeSolver)} once the solve attempt
     * (whether it succeeded, failed, or was skipped) is finished. The captcha stays counted against the custom limits afterwards.
     */
    public CaptchaSlotResult reserveCaptchaSlot(final ChallengeSolver<?> solver) throws InterruptedException {
        final int maxThreads = solver.getFinalMaxCaptchaThreads();
        final int maxThreadsPerAccount = solver.getFinalMaxCaptchaThreadsPerAccount();
        final String serviceID = solver.getService().getID();
        final List<CaptchaSolverLimitRule> limitRules = solver.getCustomLimitRules();
        final AtomicInteger serviceCounter = getOrCreateActiveCaptchasCounter(activeCaptchasByServiceID, serviceID);
        final UniqueAlltimeID accountID = getSlotAccountID(solver);
        final AtomicInteger accountCounter = accountID != null ? getOrCreateActiveCaptchasCounter(activeCaptchasByAccountID, accountID) : null;
        final long deadline = System.currentTimeMillis() + CAPTCHA_SLOT_WAIT_TIMEOUT_MILLIS;
        synchronized (CAPTCHA_SLOT_LOCK) {
            while (true) {
                if (limitRules != null) {
                    final String exhausted = getExhaustedCustomLimit(serviceID, limitRules, System.currentTimeMillis());
                    if (exhausted != null) {
                        logger.info(solver + " is skipped because of a custom limit " + exhausted);
                        return CaptchaSlotResult.CUSTOM_LIMIT_REACHED;
                    }
                }
                if (serviceCounter.get() < maxThreads && (accountCounter == null || accountCounter.get() < maxThreadsPerAccount)) {
                    serviceCounter.incrementAndGet();
                    if (accountCounter != null) {
                        accountCounter.incrementAndGet();
                    }
                    if (limitRules != null) {
                        ArrayList<Long> startTimes = captchaStartTimesByServiceID.get(serviceID);
                        if (startTimes == null) {
                            startTimes = new ArrayList<Long>();
                            captchaStartTimesByServiceID.put(serviceID, startTimes);
                        }
                        startTimes.add(Long.valueOf(System.currentTimeMillis()));
                    }
                    return CaptchaSlotResult.RESERVED;
                }
                final long remaining = deadline - System.currentTimeMillis();
                if (remaining <= 0) {
                    return CaptchaSlotResult.NO_FREE_SLOT;
                }
                CAPTCHA_SLOT_LOCK.wait(remaining);
            }
        }
    }

    /** Releases a slot previously reserved via {@link #reserveCaptchaSlot(ChallengeSolver)} for the given solver. */
    public void releaseCaptchaSlot(final ChallengeSolver<?> solver) {
        final AtomicInteger serviceCounter = activeCaptchasByServiceID.get(solver.getService().getID());
        final UniqueAlltimeID accountID = getSlotAccountID(solver);
        final AtomicInteger accountCounter = accountID != null ? activeCaptchasByAccountID.get(accountID) : null;
        synchronized (CAPTCHA_SLOT_LOCK) {
            if (serviceCounter != null) {
                serviceCounter.decrementAndGet();
            }
            if (accountCounter != null) {
                accountCounter.decrementAndGet();
            }
            CAPTCHA_SLOT_LOCK.notifyAll();
        }
    }

    /**
     * When one job gets a skiprequest, we have to check all pending jobs if this skiprequest affects them as well. if so, we have to skip
     * them as well.
     *
     * @param skipRequest
     * @param solver
     * @param challenge
     */
    public void setSkipRequest(SkipRequest skipRequest, ChallengeSolver<?> solver, Challenge<?> sourceChallenge) {
        synchronized (activeJobs) {
            for (SolverJob<?> job : activeJobs) {
                if (job.getChallenge() == sourceChallenge) {
                    job.setSkipRequest(skipRequest);
                } else if (job.getChallenge().canBeSkippedBy(skipRequest, solver, sourceChallenge)) {
                    job.setSkipRequest(skipRequest);
                }
            }
        }
    }

    public void keepAlivePendingChallenges(Challenge<?> c) {
        for (final SolverJob<?> job : activeJobs) {
            job.getChallenge().keepAlive();
        }
    }

    public <T> SolverJob<T> handle(final Challenge<T> c) throws InterruptedException, SkipException {
        final List<ChallengeSolver<T>> solvers = createList(c);
        applyLoginCaptchaPolicy(c, solvers);
        reSortByOverThreadsLimit(c, solvers);
        return handleInternal(c, solvers, true);
    }

    /**
     * Same as {@link #handle(Challenge)} but only solvers of the given service are used (captcha test panel). All other veto checks
     * (disabled solver, unsupported/disabled captcha type, ...) still apply, but there are no login captcha special handling and no "no
     * solver available" info dialogs because the user restricts the solvers on purpose.
     */
    public <T> SolverJob<T> handleTest(final Challenge<T> c, final SolverService service) throws InterruptedException, SkipException {
        final List<ChallengeSolver<T>> solvers = createList(c);
        for (final Iterator<ChallengeSolver<T>> it = solvers.iterator(); it.hasNext();) {
            if (!service.getID().equals(it.next().getService().getID())) {
                it.remove();
            }
        }
        reSortByOverThreadsLimit(c, solvers);
        return handleInternal(c, solvers, false);
    }

    private final <T> SolverJob<T> handleInternal(final Challenge<T> c, final List<ChallengeSolver<T>> solvers, final boolean showNoSolverDialogs) throws InterruptedException, SkipException {
        if (c instanceof AbstractBrowserChallenge) {
            if (c instanceof RecaptchaV2Challenge) {
                UpdateController.getInstance().addFeedback("rc");
            } else if (c instanceof HCaptchaChallenge) {
                UpdateController.getInstance().addFeedback("hc");
            } else if (c instanceof CloudflareTurnstileChallenge) {
                UpdateController.getInstance().addFeedback("tc");
            }
        }
        CaptchaHistoryManager.getInstance().addEntry(c);
        LogSource logger = LogController.getInstance().getPreviousThreadLogSource();
        if (logger == null) {
            logger = this.logger;
        }
        logger.info("Log to " + logger.getName());
        logger.info("Handle Challenge: " + c);
        logger.info("Solver: " + solvers);
        if (solvers == null || solvers.size() == 0) {
            logger.info("No solver available!");
            if (showNoSolverDialogs) {
                showNoSolverInfoDialog(c);
            }
            /* See: https://support.jdownloader.org/knowledgebase/article/error-skipped-captcha-is-required */
            throw new SkipException(c, SkipRequest.BLOCK_HOSTER, "No solver available!");
        }
        final SolverJob<T> job = new SolverJob<T>(this, c, solvers);
        job.setLogger(logger);
        c.initController(job);
        final UniqueAlltimeID challengeID = c.getId();
        synchronized (activeJobs) {
            activeJobs.add(job);
            challengeIDToJobMap.put(challengeID, job);
        }
        try {
            for (final ChallengeSolver<T> cs : solvers) {
                logger.info("Send to solver: " + cs + " " + job);
                cs.enqueue(job);
            }
            logger.info("Fire New Job Event");
            fireNewJobEvent(job);
            logger.info("Wait");
            boolean timeout = false;
            while (!job.isSolved() && !job.isDone()) {
                final BlacklistEntry<?> blackListEntry = CaptchaBlackList.getInstance().matches(c);
                final Challenge<T> challenge = job.getChallenge();
                challenge.poll(job);
                if (!job.isSolved() && !job.isDone()) {
                    final long validUntil = challenge.getValidUntil();
                    if (validUntil != -1 && System.currentTimeMillis() > validUntil) {
                        timeout = true;
                        break;
                    }
                    job._wait(1000);
                } else {
                    break;
                }
                if (blackListEntry != null && job.setSkipRequest(SkipRequest.SINGLE)) {
                    break;
                }
            }
            if (timeout && job.setSkipRequest(SkipRequest.TIMEOUT)) {
                final Challenge<T> challenge = job.getChallenge();
                final long expired = System.currentTimeMillis() - challenge.getCreated();
                final int jobTimeout = challenge.getTimeout();
                logger.info("Challenge Timeout detected|Job:" + job + "|Expired:" + expired + "|Timeout:" + jobTimeout);
            }
            if (!SkipRequest.TIMEOUT.equals(job.getSkipRequest())) {
                keepAlivePendingChallenges(c);
            }
            if (job.getSkipRequest() != null) {
                throw new SkipException(c, job.getSkipRequest());
            }
            final ResponseList<T> response = job.getResponseAndKill();
            logger.info("Solving Done. Result: " + response + " | All responses: " + job.getResponses());
            return job;
        } catch (InterruptedException e) { // for example downloads have been stopped
            job.setInterrupted();
            job.kill();
            throw e;
        } finally {
            logger.info("Job finished with status " + job.getStatus() + " | " + job.getStatusDetail() + " | " + job);
            try {
                synchronized (activeJobs) {
                    activeJobs.remove(job);
                    challengeIDToJobMap.remove(challengeID);
                }
            } finally {
                fireJobDone(job);
            }
            c.onHandled();
        }
    }

    /**
     * True if the given interactive (browser based) challenge cannot be solved by the local {@link BrowserSolver} because it is not
     * supported by it, which means that the user can only solve it with an external captcha solver. Reasons caused by the user's own
     * settings (solver/captcha type disabled, blacklisted by captcha rules) do not count, see
     * {@link ChallengeSolver#getUnsupportedReason(Challenge)}.
     */
    private boolean isExternalSolverRequired(final Challenge<?> c) {
        if (!(c instanceof AbstractBrowserChallenge)) {
            /* Not an interactive captcha (e.g. image captcha) -> No BrowserSolver required */
            return false;
        }
        return BrowserSolver.getInstance().getUnsupportedReason(c) != null;
    }

    protected final static Map<String, Long> TIMESTAMP_NO_BROWSER_SOLVER_AVAILABLE_DIALOG_LAST_DISPLAYED = new HashMap<String, Long>();

    /** Returns the name of the captcha type of the given challenge for display purposes. */
    private String getCaptchaTypeDisplayName(final Challenge<?> c) {
        final CAPTCHA_TYPE captcha_type = CAPTCHA_TYPE.getCaptchaTypeForChallenge(c);
        if (captcha_type != null) {
            return captcha_type.getDisplayName();
        }
        logger.warning("captcha_type is null");
        return c.getTypeID();
    }

    /**
     * Returns true (and remembers the current time) if the dialog with the given key may be displayed now, false if it has been displayed
     * just recently.
     */
    private boolean allowDialogNow(final String key) {
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            return true;
        }
        synchronized (TIMESTAMP_NO_BROWSER_SOLVER_AVAILABLE_DIALOG_LAST_DISPLAYED) {
            final Long lastDisplay = TIMESTAMP_NO_BROWSER_SOLVER_AVAILABLE_DIALOG_LAST_DISPLAYED.get(key);
            if (lastDisplay != null && Time.systemIndependentCurrentJVMTimeMillis() - lastDisplay.longValue() < TimeUnit.HOURS.toMillis(1)) {
                /* Dialog has been shown already just recently -> Do not display now. */
                return false;
            }
            TIMESTAMP_NO_BROWSER_SOLVER_AVAILABLE_DIALOG_LAST_DISPLAYED.put(key, Time.systemIndependentCurrentJVMTimeMillis());
            return true;
        }
    }

    /*
     * Indexes of the texts of the "no solver available" dialog, see NO_SOLVER_DIALOG_TEXTS. "{type}" is a placeholder for the captcha type,
     * "{disabled}" and "{notReady}" are placeholders for the number of disabled / enabled but not ready solvers. "<br>" is a line break.
     */
    private static final int                      TEXT_TITLE_EXTERNAL_REQUIRED = 0;
    private static final int                      TEXT_TITLE_NO_ENABLED_SOLVER = 1;
    private static final int                      TEXT_INTRO_EXTERNAL_REQUIRED = 2;
    private static final int                      TEXT_INTRO_NO_ENABLED_SOLVER = 3;
    private static final int                      TEXT_ACCOUNTS_DISABLED       = 4;
    private static final int                      TEXT_SOLVER_PROMPT           = 5;
    private static final int                      TEXT_HELP_PROMPT             = 6;
    private static final int                      TEXT_SOLVER_COUNTS_DISABLED  = 7;
    private static final int                      TEXT_SETTINGS_BUTTON         = 8;
    private static final int                      TEXT_SOLVER_COUNTS_NOT_READY = 9;    private static final Map<String, String[]>    NO_SOLVER_DIALOG_TEXTS       = new HashMap<String, String[]>();
    static {
        NO_SOLVER_DIALOG_TEXTS.put("en", new String[] { "External solver required for this captcha challenge", "No enabled captcha solver available", "The interactive type of captcha challenge '{type}' cannot currently be solved locally in your browser.<br><br>" + "Therefore, in JD your only option is to use a captcha solving service that may require payment and is capable of handling this type of captcha.<br><br>", "No solver is currently available for the captcha challenge '{type}', although there are solvers that could solve this captcha type.<br><br>", "The usage of captcha solver accounts is globally disabled in the settings.<br><br>", "Ensure that at least one of the following {count} solvers is configured and enabled:", "For more detailed information, please read the help article below:<br>", "Disabled captcha solvers: {disabled}", "Go to captcha settings", "Enabled but not ready captcha solvers: {notReady}" });
        NO_SOLVER_DIALOG_TEXTS.put("de", new String[] { "Externer Solver erforderlich für diese Captcha-Challenge", "Kein aktivierter Captcha-Solver verfügbar", "Die interaktive Art der Captcha-Herausforderung '{type}', die gelöst werden muss, kann derzeit nicht lokal in deinem Browser gelöst werden.<br><br>" + "Daher bleibt dir in JD nur die Möglichkeit, einen möglicherweise kostenpflichtigen Captcha-Lösungsdienst zu nutzen, der diesen Captcha-Typ verarbeiten kann.<br><br>", "Für die Captcha-Challenge '{type}' steht aktuell kein Solver zur Verfügung, obwohl es Solver gibt, die diesen Captcha-Typ lösen könnten.<br><br>", "Die Nutzung von Captcha-Solver-Accounts ist in den Einstellungen global deaktiviert.<br><br>", "Stelle sicher, dass mindestens einer der folgenden {count} Solver konfiguriert und aktiviert ist:", "Für detailliertere Informationen lies bitte den folgenden Hilfe-Artikel:<br>", "Deaktivierte Captcha Solver: {disabled}", "Zu den Captcha-Einstellungen", "Aktivierte, aber nicht bereite Captcha Solver: {notReady}" });
        NO_SOLVER_DIALOG_TEXTS.put("es", new String[] { "Se requiere un solucionador externo para este desafío de CAPTCHA", "No hay ningún solucionador de CAPTCHA activado", "El tipo interactivo de desafío CAPTCHA '{type}' no puede resolverse actualmente de forma local en tu navegador.<br><br>" + "Por lo tanto, en JD solo tienes la opción de utilizar un servicio de resolución de captchas posiblemente de pago que pueda manejar este tipo de CAPTCHA.<br><br>", "Actualmente no hay ningún solucionador disponible para el desafío CAPTCHA '{type}', aunque existen solucionadores que podrían resolver este tipo de CAPTCHA.<br><br>", "El uso de cuentas de solucionadores de CAPTCHA está desactivado globalmente en los ajustes.<br><br>", "Asegúrate de que al menos uno de los siguientes {count} solucionadores esté configurado y activado:", "Para obtener información más detallada, lee el siguiente artículo de ayuda:<br>", "Solucionadores de CAPTCHA desactivados: {disabled}", "Ir a la configuración de CAPTCHA", "Solucionadores de CAPTCHA activados pero no listos: {notReady}" });
        NO_SOLVER_DIALOG_TEXTS.put("fr", new String[] { "Solveur externe requis pour ce défi CAPTCHA", "Aucun solveur CAPTCHA activé disponible", "Le type interactif de défi CAPTCHA '{type}' ne peut actuellement pas être résolu localement dans votre navigateur.<br><br>" + "Par conséquent, dans JD, votre seule option est d’utiliser un service de résolution de CAPTCHA potentiellement payant capable de traiter ce type de CAPTCHA.<br><br>", "Aucun solveur n’est actuellement disponible pour le défi CAPTCHA '{type}', bien que des solveurs puissent résoudre ce type de CAPTCHA.<br><br>", "L’utilisation des comptes de solveurs CAPTCHA est désactivée globalement dans les paramètres.<br><br>", "Assurez-vous qu’au moins l’un des {count} solveurs suivants est configuré et activé :", "Pour plus d’informations détaillées, veuillez consulter l’article d’aide ci-dessous :<br>", "Solveurs CAPTCHA désactivés : {disabled}", "Aller aux paramètres CAPTCHA", "Solveurs CAPTCHA activés mais pas prêts : {notReady}" });
        NO_SOLVER_DIALOG_TEXTS.put("hi", new String[] { "इस CAPTCHA चुनौती के लिए बाहरी सॉल्वर आवश्यक है", "कोई सक्षम CAPTCHA सॉल्वर उपलब्ध नहीं", "CAPTCHA चुनौती '{type}' की इंटरएक्टिव प्रकृति को आपके ब्राउज़र में वर्तमान में स्थानीय रूप से हल नहीं किया जा सकता।<br><br>" + "इसलिए JD में आपका एकमात्र विकल्प एक संभावित सशुल्क CAPTCHA समाधान सेवा का उपयोग करना है जो इस प्रकार के CAPTCHA को संभाल सके।<br><br>", "CAPTCHA चुनौती '{type}' के लिए वर्तमान में कोई सॉल्वर उपलब्ध नहीं है, जबकि ऐसे सॉल्वर मौजूद हैं जो इस प्रकार के CAPTCHA को हल कर सकते हैं।<br><br>", "सेटिंग्स में CAPTCHA सॉल्वर खातों का उपयोग वैश्विक रूप से अक्षम है।<br><br>", "सुनिश्चित करें कि निम्न {count} सॉल्वर में से कम से कम एक कॉन्फ़िगर और सक्षम है:", "अधिक विस्तृत जानकारी के लिए कृपया नीचे दिया गया सहायता लेख पढ़ें:<br>", "अक्षम CAPTCHA सॉल्वर: {disabled}", "CAPTCHA सेटिंग्स पर जाएँ", "सक्षम लेकिन तैयार नहीं CAPTCHA सॉल्वर: {notReady}" });
        NO_SOLVER_DIALOG_TEXTS.put("zh", new String[] { "此验证码挑战需要外部解答服务", "没有可用的已启用验证码解答服务", "交互式验证码挑战 '{type}' 当前无法在您的浏览器中本地完成。<br><br>" + "因此，在 JD 中，您只能使用能够处理此类验证码的可能需要付费的验证码解答服务。<br><br>", "当前没有可用于验证码挑战 '{type}' 的解答服务，但存在可以解决此类验证码的服务。<br><br>", "验证码解答服务账户的使用已在设置中被全局禁用。<br><br>", "请确保至少配置并启用以下 {count} 个服务中的一个：", "有关更详细的信息，请阅读以下帮助文章：<br>", "已禁用的验证码解答服务：{disabled}", "前往验证码设置", "已启用但未就绪的验证码解答服务：{notReady}" });
    }

    /** Returns the texts of the "no solver available" dialog in the language of the user (English as fallback). */
    private String[] getNoSolverDialogTexts() {
        final String[] texts = NO_SOLVER_DIALOG_TEXTS.get(System.getProperty("user.language").toLowerCase(Locale.ENGLISH));
        return texts != null ? texts : NO_SOLVER_DIALOG_TEXTS.get("en");
    }

    /**
     * Called if no solver at all is available for the given challenge. Informs the user why (if it is something the user can change), lists
     * the solvers which could solve the challenge and offers a button to the captcha settings.
     */
    private <T> void showNoSolverInfoDialog(final Challenge<T> c) {
        final CAPTCHA_TYPE captcha_type = CAPTCHA_TYPE.getCaptchaTypeForChallenge(c);
        final boolean externalSolverRequired = isExternalSolverRequired(c);
        /* Solvers which could solve this captcha type if they were configured and enabled. */
        final List<SolverService> candidates = new ArrayList<SolverService>();
        boolean readyAndEnabledExternalSolverExists = false;
        if (captcha_type != null) {
            for (final SolverService service : listServices()) {
                final Set<CAPTCHA_TYPE> supportedTypes = service.getSupportedCaptchaTypes();
                if (supportedTypes == null || !supportedTypes.contains(captcha_type)) {
                    continue;
                }
                final boolean external = service.getType() == SolverType.EXTERNAL;
                if (external && service.isReady() && service.getConfigV3().isEnabled()) {
                    readyAndEnabledExternalSolverExists = true;
                }
                if (externalSolverRequired && !external) {
                    /* The local solvers are no option for this challenge. */
                    continue;
                }
                candidates.add(service);
            }
        }
        /*
         * Case 1: The challenge cannot be solved locally and the user has no usable external solver -> Explain that one is needed. Case 2:
         * There are solvers for this captcha type but all of them are disabled/unconfigured/vetoed.
         */
        final boolean explainExternalSolverRequired = externalSolverRequired && !readyAndEnabledExternalSolverExists;
        if (!explainExternalSolverRequired && candidates.size() == 0) {
            logger.info("No solver supports this captcha type at all: " + captcha_type);
            return;
        }
        /*
         * The keys of the "external solver required" case are the same as before, so the "do not show again" choice of users stays valid.
         */
        final String captchaType = getCaptchaTypeDisplayName(c);
        final String dialogKey = explainExternalSolverRequired ? "showNoBrowserSolverInfoDialog." + captchaType : "showNoEnabledSolverInfoDialog." + captchaType;
        if (!allowDialogNow(dialogKey)) {
            return;
        }
        final String[] texts = getNoSolverDialogTexts();
        final String title = (explainExternalSolverRequired ? texts[TEXT_TITLE_EXTERNAL_REQUIRED] : texts[TEXT_TITLE_NO_ENABLED_SOLVER]) + " (" + captchaType + ")";
        String intro = (explainExternalSolverRequired ? texts[TEXT_INTRO_EXTERNAL_REQUIRED] : texts[TEXT_INTRO_NO_ENABLED_SOLVER]).replace("{type}", captchaType);
        if (!CFG_GENERAL.CFG.isUseAvailableCaptchaSolverAccounts()) {
            intro += texts[TEXT_ACCOUNTS_DISABLED];
        }
        int disabledCount = 0;
        int enabledNotReadyCount = 0;
        for (final SolverService service : candidates) {
            if (!service.getConfigV3().isEnabled()) {
                disabledCount++;
            } else if (!service.isReady()) {
                enabledNotReadyCount++;
            }
        }
        /* Only mention the counts that are at least 1. */
        String summary = null;
        if (disabledCount >= 1) {
            summary = texts[TEXT_SOLVER_COUNTS_DISABLED].replace("{disabled}", String.valueOf(disabledCount));
        }
        if (enabledNotReadyCount >= 1) {
            final String notReady = texts[TEXT_SOLVER_COUNTS_NOT_READY].replace("{notReady}", String.valueOf(enabledNotReadyCount));
            summary = summary == null ? notReady : summary + " | " + notReady;
        }
        final String helpURL = explainExternalSolverRequired ? "https://support.jdownloader.org/knowledgebase/article/error-skipped-captcha-is-required" : null;
        /* The settings are only available in the GUI. */
        final boolean gui = !Application.isHeadless();
        final NoSolverAvailableDialog dialog = new NoSolverAvailableDialog(ChallengeResponseController.class.getSimpleName() + "." + dialogKey, title, intro, texts[TEXT_SOLVER_PROMPT].replace("{count}", String.valueOf(candidates.size())), candidates, summary, texts[TEXT_HELP_PROMPT], helpURL, gui ? texts[TEXT_SETTINGS_BUTTON] : null, _GUI.T.lit_close());
        dialog.setTimeout((int) TimeUnit.MINUTES.toMillis(3));
        final Thread thread = new Thread() {
            public void run() {
                try {
                    if (UIOManager.I().show(ConfirmDialogInterface.class, dialog).getCloseReason() == CloseReason.OK && gui) {
                        CaptchaConfigPanel.showCaptchaSettings();
                    }
                } catch (final Throwable e) {
                    logger.log(e);
                }
            };
        };
        thread.setDaemon(true);
        thread.start();
    }

    /**
     * Info dialog "no (enabled) captcha solver available" with the list of solvers which could solve the challenge. <br>
     * The same dialog object is shown in the GUI and offered to MyJDownloader (see RemoteAPIIOHandlerWrapper), so it has two variants with
     * the same texts: MyJDownloader only gets title and plain text message ({@link #getMessage()}), the GUI shows
     * {@link #layoutDialogContent()}, which also displays the icons of the solvers.
     */
    private static class NoSolverAvailableDialog extends ConfirmDialog {
        private final String              dontShowAgainKey;
        private final String              introHtml;
        private final String              solverPrompt;
        private final List<SolverService> solvers;
        private final String              summary;
        private final String              helpPrompt;
        private final String              helpURL;

        /**
         * @param introHtml
         *            Intro text, "&lt;br&gt;" is used for line breaks.
         * @param solvers
         *            Solvers to list.
         * @param summary
         *            Text below the solver list, may be null.
         * @param helpPrompt
         *            Text in front of the help article link, "&lt;br&gt;" is used for line breaks. Only used if helpURL is not null.
         * @param helpURL
         *            URL of the help article, may be null.
         * @param settingsButtonText
         *            Text of the button which leads to the captcha settings, null for a dialog with just a close button (headless).
         */
        public NoSolverAvailableDialog(final String dontShowAgainKey, final String title, final String introHtml, final String solverPrompt, final List<SolverService> solvers, final String summary, final String helpPrompt, final String helpURL, final String settingsButtonText, final String closeButtonText) {
            super(Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN | (settingsButtonText == null ? UIOManager.BUTTONS_HIDE_CANCEL : 0), title, createPlainMessage(introHtml, solverPrompt, solvers, summary, helpPrompt, helpURL), null, settingsButtonText != null ? settingsButtonText : closeButtonText, closeButtonText);
            this.dontShowAgainKey = dontShowAgainKey;
            this.introHtml = introHtml;
            this.solverPrompt = solverPrompt;
            this.solvers = solvers;
            this.summary = summary;
            this.helpPrompt = helpPrompt;
            this.helpURL = helpURL;
        }

        /** Variant for MyJDownloader: plain text without HTML and without icons. */
        private static String createPlainMessage(final String introHtml, final String solverPrompt, final List<SolverService> solvers, final String summary, final String helpPrompt, final String helpURL) {
            final StringBuilder sb = new StringBuilder(toPlain(introHtml));
            if (solvers.size() > 0) {
                sb.append(solverPrompt);
                for (final SolverService solver : solvers) {
                    sb.append("\n- ").append(solver.getName());
                }
                if (summary != null) {
                    sb.append("\n").append(summary);
                }
            }
            if (helpURL != null) {
                sb.append("\n\n").append(toPlain(helpPrompt)).append(helpURL);
            }
            return sb.toString();
        }

        private static String toPlain(final String html) {
            return html.replace("<br>", "\n");
        }

        @Override
        public String getDontShowAgainKey() {
            return dontShowAgainKey;
        }

        /** Variant for the GUI: Same texts as {@link #getMessage()}, but the solvers are listed with their icons. */
        @Override
        public JComponent layoutDialogContent() {
            final JPanel ret = new JPanel(new MigLayout("ins 0, wrap 1", "[grow,fill]", "[]"));
            ret.add(createTextLabel(introHtml));
            if (solvers.size() > 0) {
                ret.add(createTextLabel(solverPrompt));
                for (final SolverService solver : solvers) {
                    /* Own panel per row: The cells of the parent layout are "fill", which spreads the labels instead of left aligning them. */
                final JPanel row = new JPanel(new MigLayout("ins 0, gapx 5", "[][]", "[]"));
                row.add(new JLabel("•"));
                row.add(new JLabel(solver.getName(), solver.getIcon(20), SwingConstants.LEADING));
                ret.add(row, "gapleft 10, alignx left");
                }
                if (summary != null) {
                    ret.add(new JLabel(summary), "gaptop 5");
                }
            }
            if (helpURL != null) {
                ret.add(createTextLabel(helpPrompt), "gaptop 10");
                final JLabel link = new JLabel("<html><a href=\"" + helpURL + "\">" + helpURL + "</a></html>");
                link.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
                link.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mouseClicked(final MouseEvent e) {
                        CrossSystem.openURL(helpURL);
                    }
                });
                ret.add(link);
            }
            return ret;
        }

        /** Label with fixed width, so that long texts get wrapped. */
        private JLabel createTextLabel(final String html) {
            return new JLabel("<html><body style=\"width: 450px\">" + html + "</body></html>");
        }
    }

    @SuppressWarnings("unchecked")
    private <T> List<ChallengeSolver<T>> createList(final Challenge<T> c) {
        final List<ChallengeSolver<T>> ret = new ArrayList<ChallengeSolver<T>>();
        final HashSet<ChallengeVetoReason> vetoReasons = new HashSet<ChallengeVetoReason>();
        /* Process our own default/local solvers */
        for (final ChallengeSolver<?> solver : solverList) {
            try {
                final ChallengeVetoReason veto = solver.getChallengeVetoReason(c);
                if (veto != null) {
                    vetoReasons.add(veto);
                    continue;
                }
                ret.add((ChallengeSolver<T>) solver);
            } catch (final Throwable e) {
                logger.log(e);
            }
        }
        add_solver_accounts: {
            if (!CFG_GENERAL.CFG.isUseAvailableCaptchaSolverAccounts()) {
                /* User has globally disabled usage of solver accounts -> Don't even fetch accounts */
                logger.info("User has globally disabled usage of solver accounts");
                vetoReasons.add(ChallengeVetoReason.EXTERNAL_SOLVER_ACCOUNTS_DISABLED);
                break add_solver_accounts;
            }
            final AccountFilter af = new AccountFilter().setFeature(FEATURE.CAPTCHA_SOLVER);
            /* Map to collect solver accounts by domain */
            final Map<String, Account> bestAccountsByDomain = new HashMap<String, Account>();
            /* Whether the account stored in bestAccountsByDomain is already at its own per-account captcha limit (same keys). */
            final Map<String, Boolean> bestAccountAtLimitByDomain = new HashMap<String, Boolean>();
            /* Collect unavailable solver domains and their veto reasons for logging purposes only */
            final HashSet<String> unavailableSolverDomains = new HashSet<String>();
            final Map<String, ChallengeVetoReason> unavailableSolverDomainVetoReasons = new HashMap<String, ChallengeVetoReason>();
            final List<Account> solverAccounts = AccountController.getInstance().listAccounts(af);
            for (final Account solverAccount : solverAccounts) {
                if (solverAccount.getAccountInfo() == null) {
                    /* Skip accounts without AccountInfo. */
                    continue;
                }
                boolean success = false;
                try {
                    final abstractPluginForCaptchaSolver plugin = (abstractPluginForCaptchaSolver) solverAccount.getPlugin();
                    final ChallengeSolver<T> solver = plugin.getPluginChallengeSolver(c, solverAccount);
                    if (solver == null) {
                        /* E.g. solver cannot handle challenge it gets presented */
                        continue;
                    }
                    final SolverService service = solver.getService();
                    if (!service.getConfigV3().isEnabled()) {
                        // TODO: Move this into getChallengeVetoReason
                        vetoReasons.add(ChallengeVetoReason.SOLVER_DISABLED);
                        unavailableSolverDomainVetoReasons.put(solverAccount.getHoster(), ChallengeVetoReason.SOLVER_DISABLED);
                        continue;
                    }
                    final ChallengeVetoReason veto = solver.getChallengeVetoReason(c);
                    if (veto != null) {
                        vetoReasons.add(veto);
                        unavailableSolverDomainVetoReasons.put(solverAccount.getHoster(), veto);
                        continue;
                    }
                    /*
                     * Collect account by domain, keeping only ONE: preferably the one with lowest balance, but an account that is already
                     * at its own per-account captcha limit loses against one that still has free capacity. If every account of the domain
                     * is at its limit, the one with the lowest balance is kept anyway (it then waits for a free slot, see
                     * reserveCaptchaSlot). Only the per-account limit counts here; the user's solver-wide limit is the same for all
                     * accounts of the domain, so switching the account would not help.
                     */
                    final String domain = solverAccount.getHoster();
                    final double currentBalance = solverAccount.getAccountInfo().getAccountBalance();
                    final boolean atAccountLimit = isAtAccountLimit(solver);
                    final Account existingAccount = bestAccountsByDomain.get(domain);
                    final boolean existingAtAccountLimit = existingAccount != null && Boolean.TRUE.equals(bestAccountAtLimitByDomain.get(domain));
                    final boolean betterThanExisting;
                    if (existingAccount == null) {
                        betterThanExisting = true;
                    } else if (atAccountLimit != existingAtAccountLimit) {
                        /* Exactly one of the two is at its limit: the one with free capacity wins regardless of the balance. */
                        betterThanExisting = existingAtAccountLimit;
                    } else {
                        betterThanExisting = currentBalance < existingAccount.getAccountInfo().getAccountBalance();
                    }
                    if (betterThanExisting) {
                        bestAccountsByDomain.put(domain, solverAccount);
                        bestAccountAtLimitByDomain.put(domain, Boolean.valueOf(atAccountLimit));
                    }
                    success = true;
                } catch (final Throwable e) {
                    logger.log(e);
                    logger.warning("Exception happened during collecting fitting solver plugins");
                } finally {
                    if (success) {
                        unavailableSolverDomains.remove(solverAccount.getHoster());
                        unavailableSolverDomainVetoReasons.remove(solverAccount.getHoster());
                    } else {
                        unavailableSolverDomains.add(solverAccount.getHoster());
                    }
                }
            }
            /* Add solvers from best accounts to ret list -> Only one account per solver, only the account with the lowest balance. */
            for (final Account bestAccount : bestAccountsByDomain.values()) {
                try {
                    final abstractPluginForCaptchaSolver plugin = (abstractPluginForCaptchaSolver) bestAccount.getPlugin();
                    final ChallengeSolver<T> solver = plugin.getPluginChallengeSolver(c, bestAccount);
                    if (solver != null) {
                        ret.add(solver);
                    }
                } catch (final Throwable e) {
                    logger.log(e);
                }
            }
            if (DebugMode.TRUE_IN_IDE_ELSE_FALSE && unavailableSolverDomains.size() > 0) {
                /* Debug logger */
                logger.info("Existing solver accounts that cannot be used for this challenge: " + unavailableSolverDomains + "|vetoReasons=" + unavailableSolverDomainVetoReasons);
            }
        }
        return ret;
    }

    /**
     * Special handling for login captchas: Removes the external (auto) solvers from the list if wished and if at least one other solver is
     * left.
     */
    private <T> void applyLoginCaptchaPolicy(final Challenge<T> c, final List<ChallengeSolver<T>> ret) {
        int avoidedAutoSolversForLoginCaptcha = 0;
        avoidAutoSolver: if (c.isAccountLogin() && CAPTCHA_SETTINGS.isAvoidAutoSolverForLoginCaptchas()) {
            /*
             * Special handling for login captchas: Solve them locally if possible and wished in order to solve them faster since account
             * logins in JDownloader are supposed to happen fast.
             */
            final List<ChallengeSolver<T>> manualSolvers = new ArrayList<ChallengeSolver<T>>();
            int numberofExternalSolvers = 0;
            for (final ChallengeSolver<T> solver : ret) {
                if (solver.getSolverType() == SolverType.EXTERNAL) {
                    numberofExternalSolvers++;
                    continue;
                }
                manualSolvers.add(solver);
            }
            if (manualSolvers.size() == 0) {
                logger.info("Tried to avoid auto solvers for login challenge -> Impossible because there are no manual solvers available!");
                break avoidAutoSolver;
            }
            /* Clear all external solvers and only allow local solvers. */
            avoidedAutoSolversForLoginCaptcha = numberofExternalSolvers;
            ret.clear();
            ret.addAll(manualSolvers);
        }
        logger.info("avoidedAutoSolversForLoginCaptcha=" + avoidedAutoSolversForLoginCaptcha + " for challenge " + c.getId());
    }

    /**
     * Soft-limit handling for ChallengeSolver#getFinalMaxCaptchaThreads() and #getFinalMaxCaptchaThreadsPerAccount(): a solver already
     * at/over one of its max-simultaneous-captchas limits is not removed from the list, just moved to the end, so it is only used if no
     * solver with free capacity is available.
     */
    private <T> void reSortByOverThreadsLimit(final Challenge<T> c, final List<ChallengeSolver<T>> ret) {
        final List<ChallengeSolver<T>> withinCaptchaThreadsLimit = new ArrayList<ChallengeSolver<T>>(ret.size());
        final List<ChallengeSolver<T>> overCaptchaThreadsLimit = new ArrayList<ChallengeSolver<T>>();
        for (final ChallengeSolver<T> solver : ret) {
            if (isOverCaptchaThreadsLimit(solver)) {
                overCaptchaThreadsLimit.add(solver);
            } else {
                withinCaptchaThreadsLimit.add(solver);
            }
        }
        if (overCaptchaThreadsLimit.size() > 0) {
            logger.info("Solvers currently over their max-simultaneous-captchas limit for challenge " + c.getId() + ": " + overCaptchaThreadsLimit);
            ret.clear();
            ret.addAll(withinCaptchaThreadsLimit);
            ret.addAll(overCaptchaThreadsLimit);
        }
        logger.info("Eligible solvers for challenge " + c.getId() + ": " + ret);
    }

    public SolverJob<?> getJobByChallengeId(long id) {
        synchronized (challengeIDToJobMap) {
            return challengeIDToJobMap.get(new UniqueAlltimeID(id));
        }
    }

    public List<SolverService> listServices() {
        return new ArrayList<SolverService>(serviceList);
    }

    public SolverService getServiceByID(String key) {
        for (final SolverService service : serviceList) {
            if (service.getID().equals(key)) {
                return service;
            }
        }
        return null;
    }

    public void resetTiming() {
        final HashSet<Object> dupe = new HashSet<Object>();
        for (final ChallengeSolver<?> s : solverList) {
            if (dupe.add(s.getService())) {
                /* Removes the user's wait times, so the defaults apply again. */
                JobRunnable.resetWaitFor(s.getService());
            }
        }
    }

    public TimeTracker getTracker(String method) {
        return trackerCache.getTracker(method);
    }
}
