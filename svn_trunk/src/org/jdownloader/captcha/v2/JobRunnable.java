package org.jdownloader.captcha.v2;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.appwork.scheduler.DelayedRunnable;
import org.appwork.storage.config.handler.IntegerKeyHandler;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.Time;
import org.jdownloader.captcha.v2.solverjob.SolverJob;
import org.jdownloader.captcha.v2.solverjob.SolverJob.SolverOutcome;

import jd.controlling.captcha.SkipException;
import jd.controlling.captcha.SkipRequest;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;

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
        /* Mark first: kill() interrupts the solver, which has to be able to tell a timeout from any other abort. */
        job.setSolverTimedOut(solver);
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
                /*
                 * The solvers are started one after another in the order of the solver list (see ChallengeResponseController): This solver
                 * waits until the solver before it has been working on the captcha for the time set for that solver
                 * (getStartNextSolverAfterMillis) without a solution. It does not stop that solver, they work in parallel then. If that
                 * solver is done without a solution before, this solver starts right away. The user can also force the start (e.g. "use
                 * external solver" in the captcha dialog).
                 */
                final ChallengeSolver<T> previous = job.getPreviousSolver(solver);
                if (previous != null) {
                    final long delayMillis = getStartNextSolverAfterMillis(previous.getService(), job.getCaptchaType());
                    job.getLogger().info(solver + " starts " + TimeFormatter.formatMilliSeconds(delayMillis, 0) + " after " + previous + " started (or as soon as it is done)");
                    final long t = Time.systemIndependentCurrentJVMTimeMillis();
                    job.waitForPreviousSolver(solver, delayMillis);
                    job.getLogger().info(solver + " actually waited " + TimeFormatter.formatMilliSeconds(Time.systemIndependentCurrentJVMTimeMillis() - t, 0) + " for " + previous);
                }
                switch (ChallengeResponseController.getInstance().reserveCaptchaSlot(solver)) {
                case RESERVED:
                    try {
                        job.setSolverSolving(solver);
                        job.getLogger().info("Solver Start: " + solver);
                        solver.solve(job);
                    } finally {
                        ChallengeResponseController.getInstance().releaseCaptchaSlot(solver);
                    }
                    break;
                case CUSTOM_LIMIT_REACHED:
                    /* The controller already logged which custom limit rule is used up. */
                    job.getLogger().info(solver + " has reached one of its custom limits -> Skipping this solver for this job.");
                    job.logSolverStatus(solver, "NOT STARTED (custom limit reached)");
                    job.setSolverResult(solver, SolverOutcome.NOT_STARTED, "Custom limit reached");
                    break;
                case NO_FREE_SLOT:
                default:
                    job.getLogger().info(solver + " gave up waiting for a free max-simultaneous-captchas slot -> Skipping this solver for this job.");
                    job.logSolverStatus(solver, "NOT STARTED (no free captcha slot)");
                    job.setSolverResult(solver, SolverOutcome.NOT_STARTED, "No free captcha slot");
                    break;
                }
            } catch (SkipException e) {
                if (SkipRequest.TIMEOUT.equals(e.getSkipRequest())) {
                    job.logSolverStatus(solver, "TIMED OUT (solver ran into its timeout, e.g. dialog countdown expired)");
                } else {
                    job.logSolverStatus(solver, "SKIPPED (solver requested skip: " + e.getSkipRequest() + ")");
                }
                ChallengeResponseController.getInstance().setSkipRequest(e.getSkipRequest(), solver, job.getChallenge());
            } catch (InterruptedException e) {
                if (job.isSolverTimedOut(solver)) {
                    job.logSolverStatus(solver, "TIMED OUT (solver or job timeout, solver was interrupted)");
                } else {
                    job.logSolverStatus(solver, "INTERRUPTED/CANCELED (" + e.getMessage() + ")");
                }
                getJob().getLogger().log(e);
            } catch (Throwable e) {
                job.logSolverStatus(solver, "FAILED (" + e + ")");
                job.setSolverResult(solver, SolverOutcome.SOLVER_ERROR, String.valueOf(e));
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
     * "Start the next solver after" timings: How long a solver may work on a captcha without a solution before the next solver in the
     * order of the solver list is started in parallel to it. The user's value is stored in the config of each solver (see
     * CaptchaSolverConfigV3#getStartNextSolverAfterSeconds). The default is set per solver type in the config interface of the solver (see
     * SolverType), so new solvers (e.g. created from accounts) need no configuration at all.
     */
    private static final Object START_NEXT_LOCK = new Object();

    /**
     * The default of the given solver service: It is the default of the config entry in the config interface of the solver (see
     * {@link ChallengeSolver.SolverType}, where all the numbers are defined).
     */
    public static int getDefaultStartNextSolverAfterSeconds(final SolverService service) {
        final IntegerKeyHandler key = service.getConfigV3()._getStorageHandler().getKeyHandler("startnextsolverafterseconds", IntegerKeyHandler.class);
        return key.getDefaultValue().intValue();
    }

    /**
     * Returns the time in seconds after which the given solver service starts the next solver of the job in parallel (0 = immediately).
     * The value the user set for the given captcha type wins. Without such a value (or without a captcha type) the value of the solver is
     * used (its default depends on the solver, see {@link ChallengeSolver.SolverType}).
     *
     * @param type
     *            the captcha type of the challenge, or null for the value of the solver without a captcha type
     */
    public static int getStartNextSolverAfterSeconds(final SolverService service, final CAPTCHA_TYPE type) {
        synchronized (START_NEXT_LOCK) {
            if (type != null) {
                final Map<String, Integer> byType = service.getConfigV3().getStartNextSolverAfterSecondsByCaptchaType();
                final Integer seconds = byType == null ? null : byType.get(type.name());
                if (seconds != null && seconds.intValue() >= 0) {
                    return seconds.intValue();
                }
            }
            return Math.max(0, service.getConfigV3().getStartNextSolverAfterSeconds());
        }
    }

    /** Same as {@link #getStartNextSolverAfterSeconds(SolverService, CAPTCHA_TYPE)} in ms. */
    public static long getStartNextSolverAfterMillis(final SolverService service, final CAPTCHA_TYPE type) {
        return TimeUnit.SECONDS.toMillis(getStartNextSolverAfterSeconds(service, type));
    }

    /**
     * Stores the timings of the given solver in its config (used by the timing dialog when the user saves).
     *
     * @param seconds
     *            the value of the solver (all captcha types), -1 = the default of the solver type
     * @param byCaptchaType
     *            the values for single captcha types, key: name of the captcha type
     */
    public static void storeStartNextSolverAfterSeconds(final SolverService service, final int seconds, final Map<String, Integer> byCaptchaType) {
        synchronized (START_NEXT_LOCK) {
            service.getConfigV3().setStartNextSolverAfterSeconds(seconds);
            service.getConfigV3().setStartNextSolverAfterSecondsByCaptchaType(new HashMap<String, Integer>(byCaptchaType));
        }
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
