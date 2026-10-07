package org.jdownloader.captcha.v2;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.concurrent.ScheduledExecutorService;

import org.appwork.exceptions.WTFException;
import org.appwork.scheduler.DelayedRunnable;
import org.appwork.storage.JSonStorage;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.captcha.v2.solverjob.SolverJob;

import jd.controlling.captcha.SkipException;

public class JobRunnable<T> implements Runnable {
    private SolverJob<T>                         job;
    private ChallengeSolver<T>                   solver;
    private boolean                              canceled;
    public final static ScheduledExecutorService TIMINGQUEUE = DelayedRunnable.getNewScheduledExecutorService();

    public JobRunnable(ChallengeSolver<T> challengeSolver, SolverJob<T> job) {
        this.job = job;
        this.solver = challengeSolver;
    }

    public SolverJob<T> getJob() {
        return job;
    }

    public void fireTimeoutEvent() {
        if (job.isDone(solver)) {
            return;
        }
        solver.kill(job);
        job.fireTimeoutEvent(solver);
    }

    /**
     * Marks this runnable active (names the thread, logs the start) and returns true, unless it was already {@link #cancel()}ed before
     * {@link #run()} got to execute, in which case it returns false and does nothing else.
     */
    private synchronized boolean activate() {
        if (canceled) {
            return false;
        }
        getJob().getLogger().info(solver + " is Active.");
        thread = Thread.currentThread();
        thread.setName(solver + "-Thread");
        return true;
    }

    @Override
    public void run() {
        try {
            if (!activate()) {
                return;
            }
            fireBeforeSolveEvent();
            DelayedRunnable timeout = null;
            // final Thread thread = Thread.currentThread();
            final long finalTimeoutMillis = solver.getFinalTimeoutMillis();
            if (finalTimeoutMillis > 0) {
                timeout = new DelayedRunnable(TIMINGQUEUE, finalTimeoutMillis) {
                    @Override
                    public void delayedrun() {
                        System.out.println("Timeout!");
                        fireTimeoutEvent();
                    }
                };
            }
            try {
                if (timeout != null) {
                    timeout.resetAndStart();
                }
                long startedWaiting = System.currentTimeMillis();
                for (ChallengeSolver<?> s : job.getSolverList()) {
                    if (s == solver) {
                        continue;
                    }
                    int waitForThisSolver = getWaitFor(solver.getService(), s.getService(), job);
                    if (waitForThisSolver > 1000) {
                        job.getLogger().info(solver + " will wait up to " + TimeFormatter.formatMilliSeconds(waitForThisSolver, 0) + " for " + s);
                    }
                }
                System.out.println("Logged");
                for (ChallengeSolver<?> s : job.getSolverList()) {
                    if (s == solver) {
                        continue;
                    }
                    int waitForThisSolver = getWaitFor(solver.getService(), s.getService(), job);
                    waitForThisSolver -= (System.currentTimeMillis() - startedWaiting);
                    if (waitForThisSolver <= 0) {
                        continue;
                    }
                    /*
                     * Guard against wait loops before actually waiting. The defaults alone are always loop-free (rank order + strict random
                     * tie-break), but the user's own override wait times may combine with each other or with a random default into a loop;
                     * the same job is passed so this check sees the exact same wait directions as the waiting below. On a detected loop this
                     * solver does not wait for s (logged below) instead of blocking.
                     */
                    ArrayList<SolverService> waitLoop = validateWaittimeQueue(solver.getService(), s.getService(), job);
                    if (waitLoop == null) {
                        if (waitForThisSolver > 1000) {
                            long t = System.currentTimeMillis();
                            job.getLogger().info(solver + " now waits up to " + TimeFormatter.formatMilliSeconds(waitForThisSolver, 0) + " for " + s);
                            job.waitFor(waitForThisSolver, s);
                            job.getLogger().info(solver + " actually waited " + TimeFormatter.formatMilliSeconds(System.currentTimeMillis() - t, 0) + " for " + s);
                        }
                        continue;
                    }
                    job.getLogger().info(solver + " wait VALIDATION FAILED!" + TimeFormatter.formatMilliSeconds(waitForThisSolver, 0) + " for " + s);
                    job.getLogger().info("Wait Loop- >" + waitLoop + "");
                    try {
                        SolverService lastService = null;
                        for (SolverService le : waitLoop) {
                            if (lastService != null) {
                                job.getLogger().info("Wait Loop- " + le.getName() + " waits " + getWaitFor(le, lastService, job) + " for " + lastService.getName() + "");
                            }
                            lastService = le;
                        }
                        HashSet<Object> service = new HashSet<Object>();
                        for (ChallengeSolver<?> ss : job.getSolverList()) {
                            if (!service.add(ss.getService())) {
                                continue;
                            }
                            job.getLogger().info("Debug " + ss.getService().getName());
                            job.getLogger().info(ss.getService().getConfigV3() + "");
                            job.getLogger().info(JSonStorage.serializeToJson(ss.getService().getConfigV3().getWaitForOthers()));
                        }
                    } catch (Throwable e) {
                        job.getLogger().log(e);
                    }
                }
                if (ChallengeResponseController.getInstance().reserveCaptchaSlot(solver)) {
                    try {
                        job.getLogger().info("Solver Start: " + solver);
                        solver.solve(job);
                    } finally {
                        ChallengeResponseController.getInstance().releaseCaptchaSlot(solver);
                    }
                } else {
                    job.getLogger().info(solver + " gave up waiting for a free max-simultaneous-captchas slot -> Skipping this solver for this job.");
                }
            } catch (SkipException e) {
                ChallengeResponseController.getInstance().setSkipRequest(e.getSkipRequest(), solver, job.getChallenge());
            } catch (Throwable e) {
                getJob().getLogger().log(e);
            } finally {
                if (timeout != null) {
                    timeout.stop();
                }
                fireDoneAndAfterSolveEvents();
            }
        } finally {
            thread = null;
        }
    }

    public void fireDoneAndAfterSolveEvents() {
        // order is important. listeners should have a chance to validate which solvers are done
        job.setSolverDone(solver);
        job.fireAfterSolveEvent(solver);
    }

    public void fireBeforeSolveEvent() {
        job.fireBeforeSolveEvent(solver);
    }

    private Thread thread;

    public Thread getThread() {
        return thread;
    }

    /*
     * Wait-for timings: how long a solver waits for another solver before it starts itself (e.g. the manual dialog solver waits for the
     * automatic solvers). The user's own values are stored per solver in CaptchaSolverConfigV3#getWaitForOthers (other solver id -> ms,
     * overrides only). For every pair without such an override, a default derived from the SolverType of both solvers applies, so new
     * solvers (e.g. created from accounts) need no timing configuration at all.
     */
    /** Default wait time of any non-local-automatic solver for the local automatic solver (JAC) to solve first. */
    private static final int    DEFAULT_WAIT_FOR_LOCAL_AUTO          = 10000;
    /**
     * Default wait time of the external (paid) solver for the local automatic solver (JAC) to solve first - kept short since JAC is fast.
     */
    private static final int    DEFAULT_WAIT_EXTERNAL_FOR_LOCAL_AUTO = 5000;
    /** Default wait time of a lower-priority local/manual solver for a higher-priority local/manual solver to solve first. */
    private static final int    DEFAULT_WAIT_FOR_LOCAL_MANUAL        = 10000;
    /**
     * Default wait time of the external (paid) solver for any local/manual solver to solve first, so paid solvers are only used as a last
     * resort.
     */
    private static final int    DEFAULT_WAIT_EXTERNAL_FOR_MANUAL     = 30000;
    /**
     * Default wait time of one external (paid) solver for another external solver. External solvers all share the same rank, so the rank
     * rule alone would let them all start at once; this makes them try one after another instead (see tie-breaker in
     * {@link #getDefaultWaitFor(SolverService, SolverService)}).
     */
    private static final int    DEFAULT_WAIT_EXTERNAL_FOR_EXTERNAL   = 60000;
    private static final Object WAIT_FOR_LOCK                        = new Object();

    /**
     * Rank of a solver type, defining the default auto-handling order: JD_LOCAL, then JD_LOCAL_DIALOG, then JD_LOCAL_BROWSER, then
     * JD_REMOTE_API, and EXTERNAL last. A solver waits by default only for solvers with a lower rank, so the default timings can never form
     * a wait loop.
     */
    private static int getRank(final ChallengeSolver.SolverType type) {
        switch (type) {
        case JD_LOCAL:
            return 0;
        case JD_LOCAL_DIALOG:
            return 1;
        case JD_LOCAL_BROWSER:
            return 2;
        case JD_REMOTE_API:
            return 3;
        case EXTERNAL:
            return 4;
        default:
            throw new WTFException();
        }
    }

    /**
     * Default wait time in ms of "owner" for "other" if the user did not configure anything, outside of an actual solve (no job context,
     * e.g. the timing config dialog). Between two equally ranked external solvers there is no fixed default (the direction is only decided
     * randomly at solve time, see {@link #getDefaultWaitFor(SolverService, SolverService, SolverJob)}), so 0 is returned here.
     */
    public static int getDefaultWaitFor(final SolverService owner, final SolverService other) {
        return getDefaultWaitFor(owner, other, null);
    }

    /**
     * Default wait time in ms of "owner" for "other" if the user did not configure anything.
     *
     * @param job
     *            the captcha job currently being solved, or null when there is no solve in progress (e.g. the timing config dialog). It
     *            provides the per-job random tie-break between two equally ranked external solvers (see
     *            {@link SolverJob#getExternalTieBreakOrder(String)}); without it, no default wait applies between two external solvers.
     */
    public static int getDefaultWaitFor(final SolverService owner, final SolverService other, final SolverJob<?> job) {
        final ChallengeSolver.SolverType ownerType = owner.getType();
        final ChallengeSolver.SolverType otherType = other.getType();
        if (ownerType == ChallengeSolver.SolverType.EXTERNAL && otherType == ChallengeSolver.SolverType.EXTERNAL) {
            /*
             * Two external (paid/remote) solvers share the same rank, so without a tie-breaker either both would start at once (paying
             * twice for one captcha) or a symmetric wait would block both. There is no real priority between external solvers yet, so for
             * each captcha one of them is picked to go first at random: the job draws a random ordinal per external solver (once, kept
             * stable for that job) and the solver with the greater ordinal waits for the one with the smaller ordinal. This stays a strict
             * total order, so it can never form a wait loop, and it is re-drawn for the next captcha. Without a job (config dialog) there is
             * no such live decision, so no default wait applies.
             */
            if (job == null) {
                return 0;
            }
            final int ownerOrder = job.getExternalTieBreakOrder(owner.getID());
            final int otherOrder = job.getExternalTieBreakOrder(other.getID());
            if (ownerOrder != otherOrder) {
                return ownerOrder > otherOrder ? DEFAULT_WAIT_EXTERNAL_FOR_EXTERNAL : 0;
            }
            /* Astronomically unlikely random collision: fall back to the stable id order so exactly one direction still waits. */
            return owner.getID().compareTo(other.getID()) > 0 ? DEFAULT_WAIT_EXTERNAL_FOR_EXTERNAL : 0;
        }
        if (getRank(otherType) >= getRank(ownerType)) {
            return 0;
        }
        final boolean ownerIsExternal = ownerType == ChallengeSolver.SolverType.EXTERNAL;
        switch (otherType) {
        case JD_LOCAL:
            return ownerIsExternal ? DEFAULT_WAIT_EXTERNAL_FOR_LOCAL_AUTO : DEFAULT_WAIT_FOR_LOCAL_AUTO;
        case JD_LOCAL_DIALOG:
        case JD_LOCAL_BROWSER:
        case JD_REMOTE_API:
            return ownerIsExternal ? DEFAULT_WAIT_EXTERNAL_FOR_MANUAL : DEFAULT_WAIT_FOR_LOCAL_MANUAL;
        case EXTERNAL:
        default:
            /* Unreachable: EXTERNAL has the highest rank (lowest priority), so it can never be a lower-ranked "other" here. */
            throw new WTFException();
        }
    }

    /**
     * Returns how long "owner" waits for "other" in ms: the user's value if set, else the default. Variant without a job context (e.g. the
     * timing config dialog); see {@link #getWaitFor(SolverService, SolverService, SolverJob)} for the solve-time variant.
     */
    public static int getWaitFor(final SolverService owner, final SolverService other) {
        return getWaitFor(owner, other, null);
    }

    /**
     * Returns how long "owner" waits for "other" in ms: the user's value if set, else the default.
     *
     * @param job
     *            the captcha job currently being solved, or null (see {@link #getDefaultWaitFor(SolverService, SolverService, SolverJob)}).
     */
    public static int getWaitFor(final SolverService owner, final SolverService other, final SolverJob<?> job) {
        synchronized (WAIT_FOR_LOCK) {
            /*
             * A wait time the user configured for this exact pair (owner -> other) always wins and is returned as-is: it takes precedence
             * over every default, including the per-job random tie-break between two external solvers below. The override is per direction,
             * so only pairs the user did not set fall through to getDefaultWaitFor. A user override in one direction combined with a random
             * default in the other could in theory form a wait loop; that is caught at solve time by validateWaittimeQueue (same job), so no
             * deadlock results.
             */
            final Map<String, Integer> overrides = owner.getConfigV3().getWaitForOthers();
            if (overrides != null) {
                final Integer value = overrides.get(other.getID());
                if (value != null) {
                    return Math.max(0, value.intValue());
                }
            }
        }
        return getDefaultWaitFor(owner, other, job);
    }

    /** Stores the wait time in ms "owner" waits for the solver with the given id. 0 is stored, too (= explicitly do not wait). */
    public static void setWaitFor(final SolverService owner, final String otherID, final int waitFor) {
        synchronized (WAIT_FOR_LOCK) {
            final Map<String, Integer> map = new HashMap<String, Integer>();
            final Map<String, Integer> overrides = owner.getConfigV3().getWaitForOthers();
            if (overrides != null) {
                map.putAll(overrides);
            }
            map.put(otherID, Math.max(0, waitFor));
            owner.getConfigV3().setWaitForOthers(map);
        }
    }

    /** Removes all user values of "owner", so the defaults apply again. */
    public static void resetWaitFor(final SolverService owner) {
        synchronized (WAIT_FOR_LOCK) {
            owner.getConfigV3().setWaitForOthers(new HashMap<String, Integer>());
        }
    }

    /** Returns the number of wait time values the user configured for "owner". */
    public static int getWaitForOverrideCount(final SolverService owner) {
        synchronized (WAIT_FOR_LOCK) {
            final Map<String, Integer> overrides = owner.getConfigV3().getWaitForOthers();
            return overrides == null ? 0 : overrides.size();
        }
    }

    /**
     * Checks whether "start" waiting for "check" would result in a wait loop (A waits for B, B waits for C, C waits for A).
     *
     * @return the chain of solvers forming the loop, or null if there is none
     */
    public static ArrayList<SolverService> validateWaittimeQueue(final SolverService start, final SolverService check) {
        return validateWaittimeQueue(start, check, null);
    }

    /**
     * @param job
     *            the captcha job currently being solved, or null (see {@link #getDefaultWaitFor(SolverService, SolverService, SolverJob)}).
     *            It must be the same job that drives the actual waiting, so the loop check sees the exact same (per-job random) wait
     *            directions as the waiting itself.
     */
    public static ArrayList<SolverService> validateWaittimeQueue(final SolverService start, final SolverService check, final SolverJob<?> job) {
        if (start == null || check == null) {
            return null;
        } else {
            return validateWaittimeQueue(start, check, new ArrayList<SolverService>(), new HashSet<SolverService>(), job);
        }
    }

    private static ArrayList<SolverService> validateWaittimeQueue(final SolverService start, final SolverService check, ArrayList<SolverService> chain, HashSet<SolverService> dupe, final SolverJob<?> job) {
        if (chain.size() == 0) {
            chain.add(start);
            dupe.add(start);
        }
        chain.add(check);
        if (!dupe.add(check)) {
            return chain;
        }
        for (final SolverService service : ChallengeResponseController.getInstance().listServices()) {
            if (service != check && service.getConfigV3().isEnabled() && getWaitFor(check, service, job) > 0) {
                final ArrayList<SolverService> ret = validateWaittimeQueue(start, service, new ArrayList<SolverService>(chain), new HashSet<SolverService>(dupe), job);
                if (ret != null) {
                    return ret;
                }
            }
        }
        return null;
    }

    public void cancel() {
        synchronized (this) {
            this.canceled = true;
            Thread locThread = thread;
            if (locThread == Thread.currentThread()) {
                return;
            }
            if (locThread != null) {
                getJob().getLogger().warning("Interrupt: " + solver + " : " + locThread);
                locThread.interrupt();
            } else {
                getJob().getLogger().warning("Could Not Interrupt: " + solver + " : " + locThread);
            }
        }
    }
}
