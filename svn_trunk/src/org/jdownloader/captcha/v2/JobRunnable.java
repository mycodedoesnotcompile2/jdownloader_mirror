package org.jdownloader.captcha.v2;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.concurrent.ScheduledExecutorService;

import jd.controlling.captcha.SkipException;

import org.appwork.scheduler.DelayedRunnable;
import org.appwork.storage.JSonStorage;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.captcha.v2.solverjob.SolverJob;

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

    @Override
    public void run() {

        try {
            synchronized (this) {
                if (canceled) {
                    return;
                }

                getJob().getLogger().info(solver + " is Active.");
                thread = Thread.currentThread();
                thread.setName(solver + "-Thread");
            }
            fireBeforeSolveEvent();

            DelayedRunnable timeout = null;
            // final Thread thread = Thread.currentThread();
            if (solver.getTimeout() > 0) {
                timeout = new DelayedRunnable(TIMINGQUEUE, solver.getTimeout()) {
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
                // int waitTimeout = solver.getWaitForOthersTimeout();
                // ChallengeSolver<?>[] waitInstances = ChallengeResponseController.getInstance().getWaitForOtherSolversList(solver);
                // getJob().getLogger().info("Solver " + solver + " Waits " + TimeFormatter.formatMilliSeconds(waitTimeout, 0) + " for " +
                // Arrays.toString(waitInstances));
                // if (waitTimeout > 0 && waitInstances != null && waitInstances.length > 0) {
                // job.waitFor(waitTimeout, waitInstances);
                //
                // }
                // getJob().getLogger().info("Solver " + solver + " Waiting Done... run now.");
                long startedWaiting = System.currentTimeMillis();
                for (ChallengeSolver<?> s : job.getSolverList()) {
                    if (s == solver) {
                        continue;
                    }
                    int waitForThisSolver = getWaitFor(solver.getService(), s.getService());
                    if (waitForThisSolver > 1000) {
                        job.getLogger().info(solver + " will wait up to " + TimeFormatter.formatMilliSeconds(waitForThisSolver, 0) + " for " + s);
                    }
                }
                System.out.println("Logged");

                for (ChallengeSolver<?> s : job.getSolverList()) {
                    if (s == solver) {
                        continue;
                    }
                    int waitForThisSolver = getWaitFor(solver.getService(), s.getService());
                    waitForThisSolver -= (System.currentTimeMillis() - startedWaiting);
                    if (waitForThisSolver <= 0) {
                        continue;
                    }
                    ArrayList<SolverService> waitLoop = validateWaittimeQueue(solver.getService(), s.getService());
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
                                job.getLogger().info("Wait Loop- " + le.getName() + " waits " + getWaitFor(le, lastService) + " for " + lastService.getName() + "");
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
                job.getLogger().info("Solver Start: " + solver);
                solver.solve(job);

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
    /** Default wait time of external solvers for local automatic solvers (JAC). */
    private static final int  DEFAULT_WAIT_EXTERNAL_FOR_LOCAL = 5000;
    /** Default wait time of manual (human) solvers for local automatic solvers (JAC). */
    private static final int  DEFAULT_WAIT_MANUAL_FOR_LOCAL   = 10000;
    /** Default wait time of manual (human) solvers for external solvers. */
    private static final int  DEFAULT_WAIT_MANUAL_FOR_EXTERNAL = 30000;
    private static final Object WAIT_FOR_LOCK                 = new Object();

    /**
     * Rank of a solver type: 0 = local automatic solver, 1 = external solver (paid service), 2 = manual solver (a human has to act). A solver
     * waits by default only for solvers with a lower rank, so the default timings can never form a wait loop.
     */
    private static int getRank(final ChallengeSolver.SolverType type) {
        switch (type) {
        case JD_LOCAL:
            return 0;
        case EXTERNAL:
            return 1;
        case JD_LOCAL_DIALOG:
        case JD_LOCAL_BROWSER:
        case JD_REMOTE_API:
        default:
            return 2;
        }
    }

    /** Default wait time in ms of "owner" for "other" if the user did not configure anything. */
    public static int getDefaultWaitFor(final SolverService owner, final SolverService other) {
        final int ownerRank = getRank(owner.getType());
        final int otherRank = getRank(other.getType());
        if (otherRank >= ownerRank) {
            return 0;
        }
        switch (ownerRank) {
        case 1:
            return DEFAULT_WAIT_EXTERNAL_FOR_LOCAL;
        case 2:
            return otherRank == 0 ? DEFAULT_WAIT_MANUAL_FOR_LOCAL : DEFAULT_WAIT_MANUAL_FOR_EXTERNAL;
        default:
            return 0;
        }
    }

    /** Returns how long "owner" waits for "other" in ms: the user's value if set, else the default. */
    public static int getWaitFor(final SolverService owner, final SolverService other) {
        synchronized (WAIT_FOR_LOCK) {
            final Map<String, Integer> overrides = owner.getConfigV3().getWaitForOthers();
            if (overrides != null) {
                final Integer value = overrides.get(other.getID());
                if (value != null) {
                    return Math.max(0, value.intValue());
                }
            }
        }
        return getDefaultWaitFor(owner, other);
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
        if (start == null || check == null) {
            return null;
        } else {
            return validateWaittimeQueue(start, check, new ArrayList<SolverService>(), new HashSet<SolverService>());
        }
    }

    private static ArrayList<SolverService> validateWaittimeQueue(final SolverService start, final SolverService check, ArrayList<SolverService> chain, HashSet<SolverService> dupe) {
        if (chain.size() == 0) {
            chain.add(start);
            dupe.add(start);
        }
        chain.add(check);
        if (!dupe.add(check)) {
            return chain;
        }
        for (final SolverService service : ChallengeResponseController.getInstance().listServices()) {
            if (service != check && service.getConfigV3().isEnabled() && getWaitFor(check, service) > 0) {
                final ArrayList<SolverService> ret = validateWaittimeQueue(start, service, new ArrayList<SolverService>(chain), new HashSet<SolverService>(dupe));
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
