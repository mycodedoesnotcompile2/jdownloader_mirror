package org.jdownloader.captcha.v2;

import java.awt.Cursor;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;

import jd.controlling.AccountController;
import jd.controlling.AccountFilter;
import jd.controlling.captcha.CaptchaSettings;
import jd.controlling.captcha.SkipException;
import jd.controlling.captcha.SkipRequest;
import jd.controlling.downloadcontroller.FileNameField;
import jd.controlling.downloadcontroller.PackageNameField;
import jd.controlling.linkcrawler.CrawledLink;
import jd.gui.swing.jdgui.views.settings.panels.anticaptcha.CaptchaConfigPanel;
import jd.plugins.Account;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import net.miginfocom.swing.MigLayout;

import org.appwork.storage.config.JsonConfig;
import org.appwork.uio.CloseReason;
import org.appwork.uio.ConfirmDialogInterface;
import org.appwork.uio.UIOManager;
import org.appwork.utils.Application;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Time;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.logging2.LogSource;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.dialog.ConfirmDialog;
import org.appwork.utils.swing.dialog.Dialog;
import org.jdownloader.DomainInfo;
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
import org.jdownloader.captcha.v2.solver.service.AbstractSolverService;
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
import org.jdownloader.settings.staticreferences.CFG_GENERAL;
import org.jdownloader.updatev2.UpdateController;

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

    private LogSource logger;

    /**
     * Create a new instance of ChallengeResponseController. This is a singleton class. Access the only existing instance by using
     * {@link #getInstance()}.
     */
    private ChallengeResponseController() {
        logger = LogController.getInstance().getLogger(getClass().getName());
        eventSender = new ChallengeResponseEventSender(logger);
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

    /**
     * Number of captchas that the solvers of the given solver service are working on right now (summed over all accounts of the service). A
     * solver counts as soon as it got its captcha slot and stops counting when it is done, no matter how it ended.
     */
    public int getActiveCaptchaCount(final String serviceID) {
        return getActiveCaptchas(activeCaptchasByServiceID, serviceID);
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
        final ExhaustedCustomLimit exhausted = findExhaustedCustomLimit(serviceID, rules, now);
        return exhausted == null ? null : exhausted.rule + ", usable again in " + exhausted.usableAgainIn;
    }

    /**
     * A used up custom limit rule: what it is (e.g. "rule "x": 3/3 captchas in the last 1 hours") and how long until it is usable again.
     */
    public static final class ExhaustedCustomLimit {
        public final String rule;
        public final String usableAgainIn;

        private ExhaustedCustomLimit(final String rule, final String usableAgainIn) {
            this.rule = rule;
            this.usableAgainIn = usableAgainIn;
        }
    }

    private ExhaustedCustomLimit findExhaustedCustomLimit(final String serviceID, final List<CaptchaSolverLimitRule> rules, final long now) {
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
            return new ExhaustedCustomLimit("rule" + name + ": " + startedInInterval + "/" + rule.getMaxCaptchas() + " captchas in the last " + rule.getInterval() + " " + rule.getUnit().name().toLowerCase(Locale.ENGLISH), TimeFormatter.formatMilliSeconds(Math.max(0, usableAgainAt - now), 0));
        }
        return null;
    }

    /**
     * Returns the first used up custom limit rule of the given solver service, or null if there is none. Unlike
     * {@link #isCustomLimitReached(ChallengeSolver)} this does not log, it is meant for status displays.
     */
    public ExhaustedCustomLimit getExhaustedCustomLimit(final String serviceID, final List<CaptchaSolverLimitRule> rules) {
        if (rules == null || rules.isEmpty()) {
            return null;
        }
        synchronized (CAPTCHA_SLOT_LOCK) {
            return findExhaustedCustomLimit(serviceID, rules, Time.systemIndependentCurrentJVMTimeMillis());
        }
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
            exhausted = getExhaustedCustomLimit(solver.getService().getID(), rules, Time.systemIndependentCurrentJVMTimeMillis());
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
        final long deadline = Time.systemIndependentCurrentJVMTimeMillis() + CAPTCHA_SLOT_WAIT_TIMEOUT_MILLIS;
        synchronized (CAPTCHA_SLOT_LOCK) {
            while (true) {
                if (limitRules != null) {
                    final String exhausted = getExhaustedCustomLimit(serviceID, limitRules, Time.systemIndependentCurrentJVMTimeMillis());
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
                        startTimes.add(Long.valueOf(Time.systemIndependentCurrentJVMTimeMillis()));
                    }
                    return CaptchaSlotResult.RESERVED;
                }
                final long remaining = deadline - Time.systemIndependentCurrentJVMTimeMillis();
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
        final Set<ChallengeVetoReason> vetoReasons = new HashSet<ChallengeVetoReason>();
        final List<ChallengeSolver<T>> solvers = createList(c, vetoReasons);
        sortByServiceOrder(solvers);
        applyLoginCaptchaPolicy(c, solvers);
        reSortByOverThreadsLimit(c, solvers);
        reSortBySilentMode(c, solvers);
        final SolverJob<T> job = handleInternal(c, solvers, vetoReasons, true);
        if (vetoReasons.contains(ChallengeVetoReason.SILENT_MODE) && job.getResponse() == null) {
            /*
             * The silent mode kept the manual solvers away and the others did not solve the challenge: The link is skipped, as the user set
             * up the silent mode ("Skip the Captcha" / "Do not show a dialog, but try auto solver").
             */
            logger.info("Challenge not solved and the silent mode kept the manual solvers away -> Skip all captchas: " + c.getId());
            /**
             * 2026-11-04: TODO: I chose BLOCK_ALL_CAPTCHAS here because we assume that all other links from this hoster will run into the
             * same situation. <br>
             * We could also check if other solvers are available and then only do a BLOCK_HOSTER here. <br>
             * While this should be reviewed, this is an edge case anyways.
             */
            showNoSolverInfoDialog(c, true);
            throw new SkipException(c, SkipRequest.BLOCK_ALL_CAPTCHAS, "Silent mode: Not solved by the solvers which work without the user");
        }
        return job;
    }

    /**
     * Returns the solvers which can be used for the given challenge in the captcha test panel, in the order in which they are used. All
     * veto checks (disabled solver, unsupported/disabled captcha type, ...) still apply. If a service is given, only solvers of this
     * service are returned, without service (single test without selected solver) all solvers that passed the veto checks. Pass the result
     * to {@link #handleTest(Challenge, List)}.
     */
    public <T> List<ChallengeSolver<T>> getTestSolvers(final Challenge<T> c, final SolverService service) {
        return getTestSolvers(c, service, new HashSet<ChallengeVetoReason>(), new HashMap<String, ChallengeVetoReason>());
    }

    /**
     * Like {@link #getTestSolvers(Challenge, SolverService)}, but tells why solvers were not returned.
     *
     * @param vetoReasons
     *            receives the reasons why solvers were not used (of all solvers), pass it to {@link #handleTest(Challenge, List, Set)}
     * @param reasonsByServiceID
     *            receives the reason why the solvers of a solver service (key: {@link SolverService#getID()}) were not used. A service
     *            without a solver and without a reason has no usable account (external solver) or does not take part for another reason.
     */
    public <T> List<ChallengeSolver<T>> getTestSolvers(final Challenge<T> c, final SolverService service, final Set<ChallengeVetoReason> vetoReasons, final Map<String, ChallengeVetoReason> reasonsByServiceID) {
        final List<ChallengeSolver<T>> solvers = createList(c, vetoReasons, reasonsByServiceID);
        for (final Iterator<ChallengeSolver<T>> it = solvers.iterator(); service != null && it.hasNext();) {
            final SolverService solverService = it.next().getService();
            if (solverService == null || !service.getID().equals(solverService.getID())) {
                it.remove();
            }
        }
        sortByServiceOrder(solvers);
        reSortByOverThreadsLimit(c, solvers);
        return solvers;
    }

    /**
     * Same as {@link #handle(Challenge)} but with the solvers of {@link #getTestSolvers(Challenge, SolverService)} (captcha test panel).
     * There are no login captcha special handling and no "no solver available" info dialogs because the user restricts the solvers on
     * purpose.
     */
    public <T> SolverJob<T> handleTest(final Challenge<T> c, final List<ChallengeSolver<T>> solvers) throws InterruptedException, SkipException {
        return handleTest(c, solvers, new HashSet<ChallengeVetoReason>());
    }

    /**
     * @param vetoReasons
     *            the reasons why solvers were not used, see {@link #getTestSolvers(Challenge, SolverService, Set, Map)}: Without a solver
     *            the test is skipped with the matching reason (e.g. the silent mode)
     */
    public <T> SolverJob<T> handleTest(final Challenge<T> c, final List<ChallengeSolver<T>> solvers, final Set<ChallengeVetoReason> vetoReasons) throws InterruptedException, SkipException {
        return handleInternal(c, solvers, vetoReasons, false);
    }

    /**
     * @param vetoReasons
     *            the reasons why solvers were not used for this challenge, see {@link #createList(Challenge, Set)}
     */
    private final <T> SolverJob<T> handleInternal(final Challenge<T> c, final List<ChallengeSolver<T>> solvers, final Set<ChallengeVetoReason> vetoReasons, final boolean showNoSolverDialogs) throws InterruptedException, SkipException {
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
            if (vetoReasons.contains(ChallengeVetoReason.SILENT_MODE)) {
                /*
                 * The silent mode kept the only possible solvers (the manual ones) away. The user is told with a dialog of its own (it is
                 * shown as soon as the silent mode is over, the dialog handler holds it back until then). BLOCK_ALL_CAPTCHAS: See the
                 * comment in handle().
                 */
                logger.info("No solver available because of the silent mode -> Skip all captchas");
                if (showNoSolverDialogs) {
                    showNoSolverInfoDialog(c, true);
                }
                throw new SkipException(c, SkipRequest.BLOCK_ALL_CAPTCHAS, "Silent mode: No solver left which works without the user");
            }
            if (showNoSolverDialogs) {
                if (AbstractSolverService.isSilentModeActive()) {
                    /* No dialog during the silent mode, it would show up in front of the user's other work. */
                    logger.info("No info dialog about the missing solver because the silent mode is active");
                } else {
                    showNoSolverInfoDialog(c, false);
                }
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
                    if (validUntil != -1 && Time.systemIndependentCurrentJVMTimeMillis() > validUntil) {
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
                final long expired = Time.systemIndependentCurrentJVMTimeMillis() - challenge.getCreated();
                final int jobTimeout = challenge.getTimeout();
                logger.info("Challenge Timeout detected|Job:" + job + "|Expired:" + expired + "|Timeout:" + jobTimeout);
            }
            if (!SkipRequest.TIMEOUT.equals(job.getSkipRequest())) {
                keepAlivePendingChallenges(c);
            }
            if (job.getSkipRequest() != null) {
                /* The silent mode is not the user's decision for this challenge: Tell it (the job status says why, see SolverJobStatus). */
                final String reason;
                switch (job.getStatus()) {
                case SILENT_MODE:
                    reason = job.getStatusDetail();
                    break;
                default:
                    reason = null;
                    break;
                }
                throw new SkipException(c, job.getSkipRequest(), reason);
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
        final boolean debugDisplayAlways = true;
        if (debugDisplayAlways && DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
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
    private static final int                   TEXT_TITLE_EXTERNAL_REQUIRED = 0;
    private static final int                   TEXT_TITLE_NO_ENABLED_SOLVER = 1;
    private static final int                   TEXT_INTRO_EXTERNAL_REQUIRED = 2;
    private static final int                   TEXT_INTRO_NO_ENABLED_SOLVER = 3;
    private static final int                   TEXT_ACCOUNTS_DISABLED       = 4;
    private static final int                   TEXT_SOLVER_PROMPT           = 5;
    private static final int                   TEXT_HELP_PROMPT             = 6;
    private static final int                   TEXT_SOLVER_COUNTS_DISABLED  = 7;
    private static final int                   TEXT_SETTINGS_BUTTON         = 8;
    private static final int                   TEXT_SOLVER_COUNTS_NOT_READY = 9;
    /* Origin of the captcha ("{plugin}" / "{link}" are placeholders) and the help links for the different origins. */
    private static final int                   TEXT_ORIGIN_DOWNLOAD         = 10;
    private static final int                   TEXT_ORIGIN_ACCOUNT_CHECK    = 11;
    private static final int                   TEXT_ORIGIN_CRAWLER          = 12;
    private static final int                   TEXT_ORIGIN_PLUGIN           = 13;
    private static final int                   TEXT_ORIGIN_LINK             = 14;
    private static final int                   TEXT_HELP_ACCOUNT_CHECK      = 15;
    private static final int                   TEXT_HELP_PREMIUM            = 16;
    private static final int                   TEXT_HELP_FREE               = 17;
    private static final int                   TEXT_REASONS_HEADING         = 18;
    private static final int                   TEXT_ORIGIN_FILENAME         = 19;
    private static final int                   TEXT_ORIGIN_PACKAGE          = 20;
    /* The captcha was skipped because of the silent mode. */
    private static final int                   TEXT_TITLE_SILENT_MODE       = 21;
    private static final int                   TEXT_INTRO_SILENT_MODE       = 22;
    private static final String                HELP_URL_PREMIUM_MODE        = "https://support.jdownloader.org/knowledgebase/article/captchas-in-premium-mode-and-during-account-login";
    private static final String                HELP_URL_DOWNLOADS_NOT_START = "https://support.jdownloader.org/knowledgebase/article/warum-starten-meine-downloads-nicht";
    private static final Map<String, String[]> NO_SOLVER_DIALOG_TEXTS       = new HashMap<String, String[]>();
    static {
        NO_SOLVER_DIALOG_TEXTS.put("en", new String[] { "External solver required for this captcha challenge", "No enabled captcha solver available", "The interactive type of captcha challenge '{type}' cannot currently be solved locally in your browser.<br><br>" + "Therefore, in JD your only option is to use a captcha solving service that may require payment and is capable of handling this type of captcha.<br><br>", "No solver is currently available for the captcha challenge '{type}', although there are solvers that could solve this captcha type.<br><br>", "The usage of captcha solver accounts is globally disabled in the settings.<br><br>", "Ensure that at least one of the following {count} solvers is configured and enabled:", "For more detailed information, please read the help article below:<br>", "Disabled captcha solvers: {disabled}", "Go to captcha settings",
                "Enabled but not ready captcha solvers: {notReady}", "Origin: This captcha was triggered by a download process.", "Origin: This captcha was triggered by an account check (adding or checking an account).", "Origin: This captcha was triggered by a crawler plugin.", "Plugin: {plugin}", "Link: {link}", "Read more about captchas during account add/check or in premium mode:", "If you are a premium user, read this:", "Otherwise, read this:", "This captcha cannot be solved at the moment for the following reasons:", "Filename:", "Package:", "Captcha skipped because of the silent mode", "The silent mode is active and keeps the solvers away which need you in front of the computer (captcha dialog and browser). The captcha challenge '{type}' could not be solved in another way and was skipped. Disable the silent mode or use a captcha solving service which works without you.<br><br>" });
        NO_SOLVER_DIALOG_TEXTS.put("de", new String[] { "Externer Solver erforderlich für diese Captcha-Challenge", "Kein aktivierter Captcha-Solver verfügbar", "Die interaktive Art der Captcha-Herausforderung '{type}', die gelöst werden muss, kann derzeit nicht lokal in deinem Browser gelöst werden.<br><br>" + "Daher bleibt dir in JD nur die Möglichkeit, einen möglicherweise kostenpflichtigen Captcha-Lösungsdienst zu nutzen, der diesen Captcha-Typ verarbeiten kann.<br><br>", "Für die Captcha-Challenge '{type}' steht aktuell kein Solver zur Verfügung, obwohl es Solver gibt, die diesen Captcha-Typ lösen könnten.<br><br>", "Die Nutzung von Captcha-Solver-Accounts ist in den Einstellungen global deaktiviert.<br><br>", "Stelle sicher, dass mindestens einer der folgenden {count} Solver konfiguriert und aktiviert ist:",
                "Für detailliertere Informationen lies bitte den folgenden Hilfe-Artikel:<br>", "Deaktivierte Captcha Solver: {disabled}", "Zu den Captcha-Einstellungen", "Aktivierte, aber nicht bereite Captcha Solver: {notReady}", "Herkunft: Dieses Captcha wurde durch einen Download-Vorgang ausgelöst.", "Herkunft: Dieses Captcha wurde durch eine Account-Prüfung ausgelöst (Account hinzufügen oder prüfen).", "Herkunft: Dieses Captcha wurde durch ein Crawler-Plugin ausgelöst.", "Plugin: {plugin}", "Link: {link}", "Mehr zu Captchas beim Hinzufügen/Prüfen von Accounts oder im Premium-Modus:", "Wenn du Premium-Nutzer bist, lies dies:", "Ansonsten lies dies:", "Dieses Captcha ist momentan aus folgenden Gründen nicht lösbar:", "Dateiname:", "Paket:", "Captcha wegen des Silent Modes übersprungen",
        "Der Silent Mode ist aktiv und hält die Solver fern, die dich am Rechner brauchen (Captcha-Dialog und Browser). Die Captcha-Challenge '{type}' konnte auf anderem Weg nicht gelöst werden und wurde übersprungen. Deaktiviere den Silent Mode oder nutze einen Captcha-Lösungsdienst, der ohne dich auskommt.<br><br>" });
        NO_SOLVER_DIALOG_TEXTS.put("es", new String[] { "Se requiere un solucionador externo para este desafío de CAPTCHA", "No hay ningún solucionador de CAPTCHA activado", "El tipo interactivo de desafío CAPTCHA '{type}' no puede resolverse actualmente de forma local en tu navegador.<br><br>" + "Por lo tanto, en JD solo tienes la opción de utilizar un servicio de resolución de captchas posiblemente de pago que pueda manejar este tipo de CAPTCHA.<br><br>", "Actualmente no hay ningún solucionador disponible para el desafío CAPTCHA '{type}', aunque existen solucionadores que podrían resolver este tipo de CAPTCHA.<br><br>", "El uso de cuentas de solucionadores de CAPTCHA está desactivado globalmente en los ajustes.<br><br>", "Asegúrate de que al menos uno de los siguientes {count} solucionadores esté configurado y activado:",
                "Para obtener información más detallada, lee el siguiente artículo de ayuda:<br>", "Solucionadores de CAPTCHA desactivados: {disabled}", "Ir a la configuración de CAPTCHA", "Solucionadores de CAPTCHA activados pero no listos: {notReady}", "Origen: Este CAPTCHA fue activado por un proceso de descarga.", "Origen: Este CAPTCHA fue activado por una comprobación de cuenta (añadir o comprobar una cuenta).", "Origen: Este CAPTCHA fue activado por un plugin de crawler.", "Plugin: {plugin}", "Enlace: {link}", "Más información sobre los CAPTCHAs al añadir/comprobar cuentas o en el modo premium:", "Si eres usuario premium, lee esto:", "De lo contrario, lee esto:", "Este CAPTCHA no se puede resolver en este momento por los siguientes motivos:", "Nombre de archivo:", "Paquete:", "CAPTCHA omitido por el modo silencioso",
        "El modo silencioso está activo y mantiene alejados a los solucionadores que necesitan que estés frente al ordenador (diálogo de CAPTCHA y navegador). El desafío CAPTCHA '{type}' no pudo resolverse de otra forma y se omitió. Desactiva el modo silencioso o utiliza un servicio de resolución de CAPTCHAs que funcione sin ti.<br><br>" });
        NO_SOLVER_DIALOG_TEXTS.put("fr", new String[] { "Solveur externe requis pour ce défi CAPTCHA", "Aucun solveur CAPTCHA activé disponible", "Le type interactif de défi CAPTCHA '{type}' ne peut actuellement pas être résolu localement dans votre navigateur.<br><br>" + "Par conséquent, dans JD, votre seule option est d’utiliser un service de résolution de CAPTCHA potentiellement payant capable de traiter ce type de CAPTCHA.<br><br>", "Aucun solveur n’est actuellement disponible pour le défi CAPTCHA '{type}', bien que des solveurs puissent résoudre ce type de CAPTCHA.<br><br>", "L’utilisation des comptes de solveurs CAPTCHA est désactivée globalement dans les paramètres.<br><br>", "Assurez-vous qu’au moins l’un des {count} solveurs suivants est configuré et activé :", "Pour plus d’informations détaillées, veuillez consulter l’article d’aide ci-dessous :<br>",
                "Solveurs CAPTCHA désactivés : {disabled}", "Aller aux paramètres CAPTCHA", "Solveurs CAPTCHA activés mais pas prêts : {notReady}", "Origine : ce CAPTCHA a été déclenché par un processus de téléchargement.", "Origine : ce CAPTCHA a été déclenché par une vérification de compte (ajout ou vérification d’un compte).", "Origine : ce CAPTCHA a été déclenché par un plugin de crawler.", "Plugin : {plugin}", "Lien : {link}", "Pour en savoir plus sur les CAPTCHA lors de l’ajout/vérification de comptes ou en mode premium :", "Si vous êtes utilisateur premium, lisez ceci :", "Sinon, lisez ceci :", "Ce CAPTCHA ne peut pas être résolu pour le moment pour les raisons suivantes :", "Nom du fichier :", "Paquet :", "CAPTCHA ignoré à cause du mode silencieux",
        "Le mode silencieux est actif et écarte les solveurs qui nécessitent votre présence devant l’ordinateur (boîte de dialogue CAPTCHA et navigateur). Le défi CAPTCHA '{type}' n’a pas pu être résolu autrement et a été ignoré. Désactivez le mode silencieux ou utilisez un service de résolution de CAPTCHA qui fonctionne sans vous.<br><br>" });
        NO_SOLVER_DIALOG_TEXTS.put("hi", new String[] { "इस CAPTCHA चुनौती के लिए बाहरी सॉल्वर आवश्यक है", "कोई सक्षम CAPTCHA सॉल्वर उपलब्ध नहीं", "CAPTCHA चुनौती '{type}' की इंटरएक्टिव प्रकृति को आपके ब्राउज़र में वर्तमान में स्थानीय रूप से हल नहीं किया जा सकता।<br><br>" + "इसलिए JD में आपका एकमात्र विकल्प एक संभावित सशुल्क CAPTCHA समाधान सेवा का उपयोग करना है जो इस प्रकार के CAPTCHA को संभाल सके।<br><br>", "CAPTCHA चुनौती '{type}' के लिए वर्तमान में कोई सॉल्वर उपलब्ध नहीं है, जबकि ऐसे सॉल्वर मौजूद हैं जो इस प्रकार के CAPTCHA को हल कर सकते हैं।<br><br>", "सेटिंग्स में CAPTCHA सॉल्वर खातों का उपयोग वैश्विक रूप से अक्षम है।<br><br>", "सुनिश्चित करें कि निम्न {count} सॉल्वर में से कम से कम एक कॉन्फ़िगर और सक्षम है:", "अधिक विस्तृत जानकारी के लिए कृपया नीचे दिया गया सहायता लेख पढ़ें:<br>", "अक्षम CAPTCHA सॉल्वर: {disabled}", "CAPTCHA सेटिंग्स पर जाएँ",
                "सक्षम लेकिन तैयार नहीं CAPTCHA सॉल्वर: {notReady}", "मूल: यह CAPTCHA एक डाउनलोड प्रक्रिया द्वारा ट्रिगर किया गया था।", "मूल: यह CAPTCHA एक खाता जाँच (खाता जोड़ना या जाँचना) द्वारा ट्रिगर किया गया था।", "मूल: यह CAPTCHA एक क्रॉलर प्लगइन द्वारा ट्रिगर किया गया था।", "प्लगइन: {plugin}", "लिंक: {link}", "खाता जोड़ने/जाँचने या प्रीमियम मोड के दौरान CAPTCHA के बारे में अधिक पढ़ें:", "यदि आप प्रीमियम उपयोगकर्ता हैं, तो यह पढ़ें:", "अन्यथा, यह पढ़ें:", "इस CAPTCHA को फिलहाल निम्नलिखित कारणों से हल नहीं किया जा सकता:", "फ़ाइल नाम:", "पैकेज:", "साइलेंट मोड के कारण CAPTCHA छोड़ा गया",
        "साइलेंट मोड सक्रिय है और उन सॉल्वरों को दूर रखता है जिन्हें आपके कंप्यूटर के सामने होने की आवश्यकता है (CAPTCHA डायलॉग और ब्राउज़र)। CAPTCHA चुनौती '{type}' को किसी अन्य तरीके से हल नहीं किया जा सका और छोड़ दिया गया। साइलेंट मोड बंद करें या ऐसी CAPTCHA समाधान सेवा का उपयोग करें जो आपके बिना काम करती है।<br><br>" });
        NO_SOLVER_DIALOG_TEXTS.put("zh", new String[] { "此验证码挑战需要外部解答服务", "没有可用的已启用验证码解答服务", "交互式验证码挑战 '{type}' 当前无法在您的浏览器中本地完成。<br><br>" + "因此，在 JD 中，您只能使用能够处理此类验证码的可能需要付费的验证码解答服务。<br><br>", "当前没有可用于验证码挑战 '{type}' 的解答服务，但存在可以解决此类验证码的服务。<br><br>", "验证码解答服务账户的使用已在设置中被全局禁用。<br><br>", "请确保至少配置并启用以下 {count} 个服务中的一个：", "有关更详细的信息，请阅读以下帮助文章：<br>", "已禁用的验证码解答服务：{disabled}", "前往验证码设置", "已启用但未就绪的验证码解答服务：{notReady}", "来源：此验证码由下载过程触发。", "来源：此验证码由账户检查（添加或检查账户）触发。", "来源：此验证码由爬虫插件触发。", "插件：{plugin}", "链接：{link}", "有关添加/检查账户或高级模式期间验证码的更多信息：", "如果您是高级用户，请阅读此文：", "否则，请阅读此文：", "目前由于以下原因无法解决此验证码：", "文件名：", "包：", "因静默模式跳过了验证码", "静默模式已启用，因此不会使用需要您在电脑前操作的解答方式（验证码对话框和浏览器）。验证码挑战 '{type}' 无法通过其他方式解决，已被跳过。请关闭静默模式，或使用无需您参与的验证码解答服务。<br><br>" });
    }

    /** The intro texts consist of several sentences separated by line breaks: one list entry has no trailing line break. */
    private static String toReasonText(final String html) {
        String ret = html;
        while (ret.endsWith("<br>")) {
            ret = ret.substring(0, ret.length() - "<br>".length());
        }
        return ret.replace("<br><br>", " ").replace("<br>", " ");
    }

    /** Returns the texts of the "no solver available" dialog in the language of the user (English as fallback). */
    private String[] getNoSolverDialogTexts() {
        final String[] texts = NO_SOLVER_DIALOG_TEXTS.get(System.getProperty("user.language").toLowerCase(Locale.ENGLISH));
        return texts != null ? texts : NO_SOLVER_DIALOG_TEXTS.get("en");
    }

    /** True for the solver types which need the user in front of the computer (dialog, browser). */
    private static boolean isManualSolverType(final SolverType type) {
        switch (type) {
        case JD_LOCAL_DIALOG:
        case JD_LOCAL_BROWSER:
            return true;
        default:
            return false;
        }
    }

    /**
     * Called if no solver at all is available for the given challenge. Informs the user why (if it is something the user can change), lists
     * the solvers which could solve the challenge and offers a button to the captcha settings.
     *
     * @param silentMode
     *            true if the captcha was skipped because the silent mode kept the solvers away which need the user (dialog, browser): The
     *            dialog then explains that instead of a missing solver setup, and lists the solvers which work without the user.
     */
    private <T> void showNoSolverInfoDialog(final Challenge<T> c, final boolean silentMode) {
        final CAPTCHA_TYPE captcha_type = CAPTCHA_TYPE.getCaptchaTypeForChallenge(c);
        final boolean externalSolverRequired = isExternalSolverRequired(c);
        /* Solvers which could solve this captcha type if they were configured and enabled. */
        final List<SolverService> candidates = new ArrayList<SolverService>();
        /* Silent mode only: All solvers of the captcha type, if all of them need the user. */
        final List<SolverService> manualCandidates = new ArrayList<SolverService>();
        boolean readyAndEnabledExternalSolverExists = false;
        if (captcha_type != null) {
            /* The solvers are listed in the order the user set in the solver table. */
            for (final SolverService service : getOrderedServices()) {
                final Set<CAPTCHA_TYPE> supportedTypes = service.getSupportedCaptchaTypes();
                if (supportedTypes == null || !supportedTypes.contains(captcha_type)) {
                    continue;
                }
                final boolean external = service.getType() == SolverType.EXTERNAL;
                if (external && service.isUsable()) {
                    readyAndEnabledExternalSolverExists = true;
                }
                if (silentMode && isManualSolverType(service.getType())) {
                    /* The solvers which need the user are no option during the silent mode. */
                    manualCandidates.add(service);
                    continue;
                }
                if (externalSolverRequired && !external) {
                    /* The local solvers are no option for this challenge. */
                    continue;
                }
                candidates.add(service);
            }
        }
        if (silentMode && candidates.size() == 0) {
            /* Only solvers which need the user can solve this captcha type: They are listed, the user has to leave the silent mode. */
            candidates.addAll(manualCandidates);
        }
        /*
         * Case 1: The challenge cannot be solved locally and the user has no usable external solver -> Explain that one is needed. Case 2:
         * There are solvers for this captcha type but all of them are disabled/unconfigured/vetoed. Case 3 (silent mode): The captcha was
         * skipped because of the silent mode, see the parameter.
         */
        final boolean explainExternalSolverRequired = !silentMode && externalSolverRequired && !readyAndEnabledExternalSolverExists;
        if (!explainExternalSolverRequired && candidates.size() == 0) {
            logger.info("No solver supports this captcha type at all: " + captcha_type);
            return;
        }
        /*
         * The keys of the "external solver required" case are the same as before, so the "do not show again" choice of users stays valid.
         */
        final String captchaType = getCaptchaTypeDisplayName(c);
        final Challenge.CaptchaRequestType requestType = c.getCaptchaRequestType();
        /* The texts depend on the origin of the captcha, so a "do not show again" choice only counts for the same origin. */
        final String dialogKeyPrefix;
        if (silentMode) {
            dialogKeyPrefix = "showNoSolverSilentModeInfoDialog.";
        } else if (explainExternalSolverRequired) {
            dialogKeyPrefix = "showNoBrowserSolverInfoDialog.";
        } else {
            dialogKeyPrefix = "showNoEnabledSolverInfoDialog.";
        }
        final String dialogKey = dialogKeyPrefix + captchaType + "." + (requestType != null ? requestType.name() : "UNKNOWN");
        if (!allowDialogNow(dialogKey)) {
            return;
        }
        final String[] texts = getNoSolverDialogTexts();
        final String titleText;
        final String introText;
        if (silentMode) {
            titleText = texts[TEXT_TITLE_SILENT_MODE];
            introText = texts[TEXT_INTRO_SILENT_MODE];
        } else if (explainExternalSolverRequired) {
            titleText = texts[TEXT_TITLE_EXTERNAL_REQUIRED];
            introText = texts[TEXT_INTRO_EXTERNAL_REQUIRED];
        } else {
            titleText = texts[TEXT_TITLE_NO_ENABLED_SOLVER];
            introText = texts[TEXT_INTRO_NO_ENABLED_SOLVER];
        }
        final String title = titleText + " (" + captchaType + ")";
        /* The reasons why the captcha cannot be solved right now, shown as a list. */
        final List<String> reasons = new ArrayList<String>();
        reasons.add(toReasonText(introText.replace("{type}", captchaType)));
        if (!CFG_GENERAL.CFG.isUseAvailableCaptchaSolverAccounts()) {
            reasons.add(toReasonText(texts[TEXT_ACCOUNTS_DISABLED]));
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
        if (disabledCount >= 1) {
            reasons.add(texts[TEXT_SOLVER_COUNTS_DISABLED].replace("{disabled}", String.valueOf(disabledCount)));
        }
        if (enabledNotReadyCount >= 1) {
            reasons.add(texts[TEXT_SOLVER_COUNTS_NOT_READY].replace("{notReady}", String.valueOf(enabledNotReadyCount)));
        }
        /* Where does the captcha come from? Helps the user to understand why a captcha is asked at all. */
        final List<OriginLine> originLines = new ArrayList<OriginLine>();
        final List<String[]> helpLinks = new ArrayList<String[]>();
        if (requestType != null) {
            switch (requestType) {
            case HOSTER:
                originLines.add(new OriginLine(texts[TEXT_ORIGIN_DOWNLOAD], null));
                break;
            case HOSTER_LOGIN:
                originLines.add(new OriginLine(texts[TEXT_ORIGIN_ACCOUNT_CHECK], null));
                break;
            case DECRYPTER:
                originLines.add(new OriginLine(texts[TEXT_ORIGIN_CRAWLER], null));
                break;
            default:
                break;
            }
        }
        final Plugin plugin = c.getPlugin();
        if (plugin != null && plugin.getHost() != null) {
            originLines.add(new OriginLine(texts[TEXT_ORIGIN_PLUGIN].replace("{plugin}", plugin.getHost()), DomainInfo.getInstance(plugin.getHost()).getIcon(18)));
        }
        if (requestType == Challenge.CaptchaRequestType.HOSTER) {
            final DownloadLink downloadLink = c.getDownloadLink();
            if (downloadLink != null) {
                originLines.add(new OriginLine(texts[TEXT_ORIGIN_FILENAME], OriginLine.ValueType.FILENAME, downloadLink.getName()));
                final FilePackage filePackage = downloadLink.getFilePackage();
                if (filePackage != null && !FilePackage.isDefaultFilePackage(filePackage)) {
                    originLines.add(new OriginLine(texts[TEXT_ORIGIN_PACKAGE], OriginLine.ValueType.PACKAGENAME, filePackage.getName()));
                }
            }
        } else if (requestType == Challenge.CaptchaRequestType.DECRYPTER && plugin != null) {
            final CrawledLink crawledLink = plugin.getCurrentLink();
            if (crawledLink != null && crawledLink.getURL() != null) {
                originLines.add(new OriginLine(texts[TEXT_ORIGIN_LINK].replace("{link}", crawledLink.getURL()), null));
            }
        }
        if (requestType != null) {
            switch (requestType) {
            case HOSTER:
                helpLinks.add(new String[] { texts[TEXT_HELP_PREMIUM], HELP_URL_DOWNLOADS_NOT_START });
                helpLinks.add(new String[] { texts[TEXT_HELP_FREE], HELP_URL_DOWNLOADS_NOT_START });
                break;
            case HOSTER_LOGIN:
                helpLinks.add(new String[] { texts[TEXT_HELP_ACCOUNT_CHECK], HELP_URL_PREMIUM_MODE });
                helpLinks.add(new String[] { null, HELP_URL_DOWNLOADS_NOT_START });
                break;
            case DECRYPTER:
            default:
                break;
            }
        }
        if (explainExternalSolverRequired) {
            helpLinks.add(new String[] { texts[TEXT_HELP_PROMPT], "https://support.jdownloader.org/knowledgebase/article/error-skipped-captcha-is-required" });
        }
        /* The settings are only available in the GUI. */
        final boolean gui = !Application.isHeadless();
        final NoSolverAvailableDialog dialog = new NoSolverAvailableDialog(ChallengeResponseController.class.getSimpleName() + "." + dialogKey, title, originLines, texts[TEXT_REASONS_HEADING], reasons, texts[TEXT_SOLVER_PROMPT].replace("{count}", String.valueOf(candidates.size())), candidates, helpLinks, gui ? texts[TEXT_SETTINGS_BUTTON] : null, _GUI.T.lit_close());
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
    /**
     * One line of the origin information of the "no solver available" dialog, with an optional icon (e.g. the favicon of the plugin) or,
     * instead of the icon, a file name: the text is then only the label in front of the file name, which is shown with the reusable file
     * name component.
     */
    private static class OriginLine {
        /** How the value of a line is shown. */
        private enum ValueType {
            FILENAME,
            PACKAGENAME
        }

        private final String    text;
        private final Icon      icon;
        private final ValueType valueType;
        private final String    value;

        private OriginLine(final String text, final Icon icon) {
            this.text = text;
            this.icon = icon;
            this.valueType = null;
            this.value = null;
        }

        /** Line with a label (the text) and a file name / package name, which are shown with the reusable GUI components. */
        private OriginLine(final String label, final ValueType valueType, final String value) {
            this.text = label;
            this.icon = null;
            this.valueType = valueType;
            this.value = value;
        }

        /** The line as plain text (MyJDownloader). */
        private String toPlainText() {
            return valueType == null ? text : text + " " + value;
        }

        /** The component for the value of the line, or null if the line is just a text. */
        private JComponent createValueComponent() {
            if (valueType == null) {
                return null;
            }
            switch (valueType) {
            case PACKAGENAME:
                return new PackageNameField(value);
            case FILENAME:
            default:
                return new FileNameField(value, false);
            }
        }
    }

    private static class NoSolverAvailableDialog extends ConfirmDialog {
        private final String              dontShowAgainKey;
        private final List<OriginLine>    originLines;
        private final String              reasonsHeading;
        private final List<String>        reasons;
        private final String              solverPrompt;
        private final List<SolverService> solvers;
        private final List<String[]>      helpLinks;

        /**
         * @param originLines
         *            Plain text lines (with an optional icon) which tell where the captcha comes from (origin, plugin, link), may be empty.
         * @param reasonsHeading
         *            Text in front of the list of reasons why the captcha cannot be solved.
         * @param reasons
         *            The reasons, shown as a list.
         * @param solvers
         *            Solvers to list.
         * @param helpLinks
         *            Help articles: {prompt, url}. The prompt ("&lt;br&gt;" is used for line breaks) is shown in front of the link and may
         *            be null (then only the link is shown).
         * @param settingsButtonText
         *            Text of the button which leads to the captcha settings, null for a dialog with just a close button (headless).
         */
        public NoSolverAvailableDialog(final String dontShowAgainKey, final String title, final List<OriginLine> originLines, final String reasonsHeading, final List<String> reasons, final String solverPrompt, final List<SolverService> solvers, final List<String[]> helpLinks, final String settingsButtonText, final String closeButtonText) {
            super(Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN | (settingsButtonText == null ? UIOManager.BUTTONS_HIDE_CANCEL : 0), title, createPlainMessage(originLines, reasonsHeading, reasons, solverPrompt, solvers, helpLinks), null, settingsButtonText != null ? settingsButtonText : closeButtonText, closeButtonText);
            this.dontShowAgainKey = dontShowAgainKey;
            this.originLines = originLines;
            this.reasonsHeading = reasonsHeading;
            this.reasons = reasons;
            this.solverPrompt = solverPrompt;
            this.solvers = solvers;
            this.helpLinks = helpLinks;
        }

        /** Variant for MyJDownloader: plain text without HTML and without icons. */
        private static String createPlainMessage(final List<OriginLine> originLines, final String reasonsHeading, final List<String> reasons, final String solverPrompt, final List<SolverService> solvers, final List<String[]> helpLinks) {
            final StringBuilder sb = new StringBuilder();
            if (originLines.size() > 0) {
                for (final OriginLine line : originLines) {
                    sb.append(line.toPlainText()).append("\n");
                }
                sb.append("\n");
            }
            sb.append(reasonsHeading);
            for (final String reason : reasons) {
                sb.append("\n* ").append(reason);
            }
            if (solvers.size() > 0) {
                sb.append("\n\n").append(solverPrompt);
                for (final SolverService solver : solvers) {
                    sb.append("\n- ").append(solver.getName());
                }
            }
            for (final String[] helpLink : helpLinks) {
                sb.append("\n\n");
                if (helpLink[0] != null) {
                    sb.append(toPlain(helpLink[0]));
                    if (!helpLink[0].endsWith("<br>")) {
                        sb.append("\n");
                    }
                }
                sb.append(helpLink[1]);
            }
            return sb.toString();
        }

        private static String toPlain(final String html) {
            return html.replace("<br>", "\n");
        }

        private static String escapeHtml(final String text) {
            return text.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;");
        }

        @Override
        public String getDontShowAgainKey() {
            return dontShowAgainKey;
        }

        /** Variant for the GUI: Same texts as {@link #getMessage()}, but the solvers are listed with their icons. */
        @Override
        public JComponent layoutDialogContent() {
            final JPanel ret = new JPanel(new MigLayout("ins 0, wrap 1", "[grow,fill]", "[]"));
            if (originLines.size() > 0) {
                for (final OriginLine line : originLines) {
                    final JComponent valueComponent = line.createValueComponent();
                    if (valueComponent != null) {
                        final JPanel valueRow = new JPanel(new MigLayout("ins 0, gapx 5", "[][grow,fill]", "[]"));
                        valueRow.add(new JLabel(line.text));
                        valueRow.add(valueComponent);
                        ret.add(valueRow, "growx");
                    } else if (line.icon == null) {
                        ret.add(createTextLabel(escapeHtml(line.text)));
                    } else {
                        final JPanel originRow = new JPanel(new MigLayout("ins 0, gapx 5", "[][]", "[]"));
                        originRow.add(new JLabel(line.icon));
                        originRow.add(createTextLabel(escapeHtml(line.text)));
                        ret.add(originRow, "alignx left");
                    }
                }
                ret.add(new JLabel(" "), "gaptop 0");
            }
            ret.add(createTextLabel(reasonsHeading));
            for (final String reason : reasons) {
                /* Own panel per row, see the solver rows below. */
                final JPanel reasonRow = new JPanel(new MigLayout("ins 0, gapx 5", "[][]", "[]"));
                reasonRow.add(new JLabel("•"), "aligny top");
                reasonRow.add(new JLabel("<html><body style=\"width: 420px\">" + reason + "</body></html>"));
                ret.add(reasonRow, "gapleft 10, alignx left");
            }
            if (solvers.size() > 0) {
                ret.add(createTextLabel(solverPrompt), "gaptop 10");
                for (final SolverService solver : solvers) {
                    /*
                     * Own panel per row: The cells of the parent layout are "fill", which spreads the labels instead of left aligning them.
                     */
                    final JPanel row = new JPanel(new MigLayout("ins 0, gapx 5", "[][]", "[]"));
                    row.add(new JLabel("•"));
                    row.add(new JLabel(solver.getName(), solver.getIcon(20), SwingConstants.LEADING));
                    ret.add(row, "gapleft 10, alignx left");
                }
            }
            for (final String[] helpLink : helpLinks) {
                final String helpURL = helpLink[1];
                if (helpLink[0] != null) {
                    ret.add(createTextLabel(helpLink[0]), "gaptop 10");
                }
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

    private <T> List<ChallengeSolver<T>> createList(final Challenge<T> c) {
        return createList(c, new HashSet<ChallengeVetoReason>());
    }

    private <T> List<ChallengeSolver<T>> createList(final Challenge<T> c, final Set<ChallengeVetoReason> vetoReasons) {
        return createList(c, vetoReasons, null);
    }

    /**
     * @param vetoReasons
     *            receives the reasons why solvers were not added to the list (all of them, not only the ones of the last solver)
     * @param reasonsByServiceID
     *            optional, receives the reason why the solvers of a solver service (key: {@link SolverService#getID()}) were not added to
     *            the list, see {@link #getTestSolvers(Challenge, SolverService, Map)}
     */
    @SuppressWarnings("unchecked")
    private <T> List<ChallengeSolver<T>> createList(final Challenge<T> c, final Set<ChallengeVetoReason> vetoReasons, final Map<String, ChallengeVetoReason> reasonsByServiceID) {
        final List<ChallengeSolver<T>> ret = new ArrayList<ChallengeSolver<T>>();
        /* Process our own default/local solvers */
        for (final ChallengeSolver<?> solver : solverList) {
            try {
                final ChallengeVetoReason veto = solver.getChallengeVetoReason(c);
                if (veto != null) {
                    vetoReasons.add(veto);
                    if (reasonsByServiceID != null) {
                        reasonsByServiceID.put(solver.getService().getID(), veto);
                    }
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
                if (reasonsByServiceID != null) {
                    for (final SolverService service : serviceList) {
                        if (service.getType() == SolverType.EXTERNAL) {
                            reasonsByServiceID.put(service.getID(), ChallengeVetoReason.EXTERNAL_SOLVER_ACCOUNTS_DISABLED);
                        }
                    }
                }
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
                        if (reasonsByServiceID != null) {
                            reasonsByServiceID.put(service.getID(), ChallengeVetoReason.SOLVER_DISABLED);
                        }
                        unavailableSolverDomainVetoReasons.put(solverAccount.getHoster(), ChallengeVetoReason.SOLVER_DISABLED);
                        continue;
                    }
                    final ChallengeVetoReason veto = solver.getChallengeVetoReason(c);
                    if (veto != null) {
                        vetoReasons.add(veto);
                        if (reasonsByServiceID != null) {
                            reasonsByServiceID.put(service.getID(), veto);
                        }
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
        if (!c.getPlugin().isAccountLoginCaptchaChallenge(c)) {
            /* Not a login captcha challenge */
            return;
        }
        if (!CAPTCHA_SETTINGS.isAvoidAutoSolverForLoginCaptchas()) {
            /* User has deactivated this special behavior for login captchas. */
            return;
        }
        if (AbstractSolverService.isSilentModeActive()) {
            /*
             * Silent mode active -> Beats login captcha settings -> User does not want popups or most likely is afk anyways.
             */
            return;
        }
        int avoidedAutoSolversForLoginCaptcha = 0;
        avoidAutoSolver: {
            /*
             * Special handling for login captchas: Solve them locally if possible and wished in order to solve them faster since account
             * logins in JDownloader are supposed to happen fast.
             */
            final List<ChallengeSolver<T>> manualSolvers = new ArrayList<ChallengeSolver<T>>();
            int numberOfAvoidedSolvers = 0;
            for (final ChallengeSolver<T> solver : ret) {
                /*
                 * Explicitly collect the solver types which are wanted for login captchas. Everything else (external solvers and any solver
                 * type added in the future) is avoided.
                 */
                switch (solver.getSolverType()) {
                case JD_LOCAL:
                case JD_LOCAL_DIALOG:
                case JD_LOCAL_BROWSER:
                case JD_REMOTE_MYJD:
                    manualSolvers.add(solver);
                    break;
                case EXTERNAL:
                default:
                    numberOfAvoidedSolvers++;
                    break;
                }
            }
            if (manualSolvers.size() == 0) {
                logger.info("Tried to avoid auto solvers for login challenge -> Impossible because there are no manual solvers available!");
                break avoidAutoSolver;
            }
            /* Clear all external solvers and only allow local solvers. */
            avoidedAutoSolversForLoginCaptcha = numberOfAvoidedSolvers;
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

    /**
     * Silent mode with "Wait in Background until window gets focus or timeout is reached": The manual solvers (dialog, browser) have to
     * wait for the user anyway, so the other solvers (external, local auto solvers) are moved in front of them. The order is otherwise
     * kept.
     */
    private <T> void reSortBySilentMode(final Challenge<T> c, final List<ChallengeSolver<T>> ret) {
        if (!AbstractSolverService.isManualSolvingDelayedBySilentMode()) {
            return;
        }
        final List<ChallengeSolver<T>> others = new ArrayList<ChallengeSolver<T>>(ret.size());
        final List<ChallengeSolver<T>> manual = new ArrayList<ChallengeSolver<T>>();
        for (final ChallengeSolver<T> solver : ret) {
            switch (solver.getSolverType()) {
            case JD_LOCAL_DIALOG:
            case JD_LOCAL_BROWSER:
                manual.add(solver);
                break;
            default:
                others.add(solver);
                break;
            }
        }
        if (manual.size() > 0 && others.size() > 0) {
            logger.info("Silent mode: The manual solvers are moved to the end for challenge " + c.getId() + ": " + manual);
            ret.clear();
            ret.addAll(others);
            ret.addAll(manual);
        }
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

    /**
     * Rank of a solver type, defining the default order of the solvers: JD_LOCAL, then JD_LOCAL_DIALOG, then JD_LOCAL_BROWSER, then
     * JD_REMOTE_MYJD, and EXTERNAL last.
     */
    private static int getDefaultOrderRank(final SolverType type) {
        switch (type) {
        case JD_LOCAL:
            return 0;
        case JD_LOCAL_DIALOG:
            return 1;
        case JD_LOCAL_BROWSER:
            return 2;
        case JD_REMOTE_MYJD:
            return 3;
        case EXTERNAL:
        default:
            return 4;
        }
    }

    /**
     * Returns the solver services in the order in which their solvers are started for a captcha: The first solver starts right away, the
     * next one is started in parallel when the solver before it did not find a solution within its "start the next solver after" time (see
     * {@link JobRunnable#getStartNextSolverAfterMillis(SolverService)}). <br>
     * This is the order the user set in the solver table (see {@link #setServiceOrder(List)}). Services the user has not placed yet (e.g.
     * new ones) come last, in the default order (by type of the solver, see {@link #getDefaultOrderRank(SolverType)}). Without any order
     * set by the user, the default order is used for all services.
     */
    public List<SolverService> getOrderedServices() {
        final List<SolverService> remaining = new ArrayList<SolverService>(serviceList);
        /* Stable sort: The services of the same type keep the order in which they were added. */
        Collections.sort(remaining, new Comparator<SolverService>() {
            @Override
            public int compare(final SolverService a, final SolverService b) {
                return getDefaultOrderRank(a.getType()) - getDefaultOrderRank(b.getType());
            }
        });
        final List<String> userOrder = CAPTCHA_SETTINGS.getSolverOrder();
        if (userOrder == null || userOrder.isEmpty()) {
            return remaining;
        }
        final List<SolverService> ret = new ArrayList<SolverService>(remaining.size());
        for (final String id : userOrder) {
            for (final Iterator<SolverService> it = remaining.iterator(); it.hasNext();) {
                final SolverService service = it.next();
                if (service.getID().equals(id)) {
                    ret.add(service);
                    it.remove();
                    break;
                }
            }
        }
        ret.addAll(remaining);
        return ret;
    }

    /** Stores the order of the solver services, see {@link #getOrderedServices()}. */
    public void setServiceOrder(final List<SolverService> services) {
        final ArrayList<String> ids = new ArrayList<String>(services.size());
        for (final SolverService service : services) {
            ids.add(service.getID());
        }
        CAPTCHA_SETTINGS.setSolverOrder(ids);
    }

    /**
     * Sorts the given solvers by the order of their services, see {@link #getOrderedServices()}. Solvers of the same service keep their
     * order.
     */
    private <T> void sortByServiceOrder(final List<ChallengeSolver<T>> solvers) {
        final Map<String, Integer> indexByServiceID = new HashMap<String, Integer>();
        for (final SolverService service : getOrderedServices()) {
            indexByServiceID.put(service.getID(), Integer.valueOf(indexByServiceID.size()));
        }
        Collections.sort(solvers, new Comparator<ChallengeSolver<T>>() {
            @Override
            public int compare(final ChallengeSolver<T> a, final ChallengeSolver<T> b) {
                return getIndex(a) - getIndex(b);
            }

            private int getIndex(final ChallengeSolver<T> solver) {
                final Integer index = indexByServiceID.get(solver.getService().getID());
                return index == null ? Integer.MAX_VALUE : index.intValue();
            }
        });
    }
}
