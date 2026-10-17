package org.jdownloader.captcha.v2.solverjob;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.storage.config.JsonConfig;
import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.utils.Exceptions;
import org.appwork.utils.Time;
import org.appwork.utils.logging2.LogSource;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.ChallengeResponseController;
import org.jdownloader.captcha.v2.ChallengeSolver;
import org.jdownloader.captcha.v2.ValidationResult;
import org.jdownloader.gui.translate._GUI;

import jd.controlling.captcha.CaptchaSettings;
import jd.controlling.captcha.SkipRequest;

public class SolverJob<T> {
    /**
     * Status of a {@link SolverJob} as a whole, see {@link SolverJob#getStatus()}. It says nothing about what happened inside the
     * individual solvers: one solver can fail while another one solves the job.
     */
    public enum SolverJobStatus implements LabelInterface {
        /** The job is still being processed. */
        RUNNING {
            @Override
            public String getLabel() {
                return _GUI.T.SolverJobStatus_RUNNING();
            }
        },
        /** A solver delivered an answer that the job accepted. */
        SOLVED {
            @Override
            public String getLabel() {
                return _GUI.T.SolverJobStatus_SOLVED();
            }
        },
        /** The job ran into its timeout (see Challenge#getTimeout) or every solver ran into its own timeout before the job was solved. */
        TIMEOUT {
            @Override
            public String getLabel() {
                return _GUI.T.SolverJobStatus_TIMEOUT();
            }
        },
        /** The job was skipped/aborted (single skip, block hoster/package/all captchas, stop current action, ...). */
        ABORTED_BY_USER {
            @Override
            public String getLabel() {
                return _GUI.T.SolverJobStatus_ABORTED_BY_USER();
            }
        },
        /** The captcha was refreshed, so this job was ended in order to get a new challenge. */
        REFRESHED {
            @Override
            public String getLabel() {
                return _GUI.T.SolverJobStatus_REFRESHED();
            }
        },
        /** All solvers are done but none delivered an usable answer, and there was neither a timeout nor a skip. */
        UNSOLVED {
            @Override
            public String getLabel() {
                return _GUI.T.SolverJobStatus_UNSOLVED();
            }
        },
        /** The thread waiting for the job was interrupted, e.g. because downloads were stopped. */
        INTERRUPTED {
            @Override
            public String getLabel() {
                return _GUI.T.SolverJobStatus_INTERRUPTED();
            }
        }
    }

    private final Challenge<T>                                   challenge;
    private final CaptchaSettings                                config;
    private final ChallengeResponseController                    controller;
    private volatile ArrayList<ResponseList<T>>                  cumulatedList;
    private final HashSet<ChallengeSolver<T>>                    doneList    = new HashSet<ChallengeSolver<T>>();
    private final AtomicReference<ChallengeSolverJobEventSender> eventSender = new AtomicReference<ChallengeSolverJobEventSender>();
    private final List<AbstractResponse<T>>                      responses   = new ArrayList<AbstractResponse<T>>();
    private final CopyOnWriteArraySet<ChallengeSolver<T>>        solverList;
    private LogSource                                            logger;
    private volatile SkipRequest                                 skipRequest;
    private volatile boolean                                     interrupted;
    private final CopyOnWriteArraySet<ChallengeSolver<?>>        solvingSet     = new CopyOnWriteArraySet<ChallengeSolver<?>>();
    private final CopyOnWriteArraySet<ChallengeSolver<?>>        timedOutSet    = new CopyOnWriteArraySet<ChallengeSolver<?>>();
    private final CopyOnWriteArraySet<ChallengeSolver<?>>        forcedStartSet = new CopyOnWriteArraySet<ChallengeSolver<?>>();
    private final AtomicInteger                                  LOCK        = new AtomicInteger();
    private final AtomicBoolean                                  alive       = new AtomicBoolean(true);
    private long                                                 created;
    /**
     * Random tie-break ordinal per external solver id, drawn once per job (see {@link #getExternalTieBreakOrder(String)}). Used by
     * {@link org.jdownloader.captcha.v2.JobRunnable#getDefaultWaitFor} to pick, at random for this one captcha, which of several equally
     * ranked external solvers is tried first.
     */
    private final HashMap<String, Integer>                       externalTieBreakOrder = new HashMap<String, Integer>();
    private static final Random                                  TIEBREAK_RANDOM       = new Random();

    public String toString() {
        return "CaptchaJob: " + new Date(created) + " " + challenge + " Solver: " + solverList;
    }

    public SolverJob(ChallengeResponseController controller, Challenge<T> c, List<ChallengeSolver<T>> solver) {
        this.challenge = c;
        created = System.currentTimeMillis();
        this.controller = controller;
        this.solverList = new CopyOnWriteArraySet<ChallengeSolver<T>>(solver);
        config = JsonConfig.create(CaptchaSettings.class);
    }

    /**
     * Logs a status change of a solver within this job in a uniform format, so the life cycle of every solver (started, waiting, solving,
     * skipped, failed, done, ...) can be followed in the job log.
     */
    public void logSolverStatus(final ChallengeSolver<?> solver, final String status) {
        log("Solver status: " + solver + " [" + solver.getSolverType() + "] -> " + status);
    }

    protected void log(String txt) {
        final LogSource lLogger = getLogger();
        if (lLogger != null) {
            lLogger.info(txt);
        }
    }

    public boolean addAnswer(AbstractResponse<T> abstractResponse) {
        if (!abstractResponse.getChallenge().validateResponse(abstractResponse)) {
            abstractResponse.setValidation(ValidationResult.INVALID);
            return false;
        }
        boolean kill = false;
        boolean isAlive;
        if (abstractResponse.getSolver() instanceof ChallengeSolver) {
            logSolverStatus((ChallengeSolver<?>) abstractResponse.getSolver(), "ANSWER received (job alive: " + alive.get() + ")");
        }
        synchronized (LOCK) {
            isAlive = alive.get();
            if (isAlive) {
                responses.add(abstractResponse);
                challenge.setResult(cumulate());
                if (isSolved()) {
                    alive.set(false);
                    log("Is Solved - kill rest");
                    kill = true;
                }
            }
        }
        if (kill) {
            kill();
        }
        if (!isAlive) {
            abstractResponse.setValidation(ValidationResult.UNUSED);
            return false;
        }
        fireNewAnswerEvent(abstractResponse);
        return true;
    }

    public ArrayList<ResponseList<T>> getResponses() {
        return cumulatedList;
    }

    public ResponseList<T> getResponseAndKill() {
        synchronized (LOCK) {
            alive.set(false);
            final ArrayList<ResponseList<T>> lst = cumulatedList;
            if (lst == null || lst.size() == 0) {
                return null;
            }
            return lst.get(0);
        }
    }

    public ResponseList<T> getResponse() {
        synchronized (LOCK) {
            final ArrayList<ResponseList<T>> lst = cumulatedList;
            if (lst == null || lst.size() == 0) {
                return null;
            }
            return lst.get(0);
        }
    }

    public boolean areDone(ChallengeSolver<?>... instances) {
        synchronized (LOCK) {
            for (ChallengeSolver<?> cs : instances) {
                if (solverList.contains(cs) && !doneList.contains(cs)) {
                    return false;
                }
            }
            log("All: " + solverList);
            log("Done: " + doneList);
        }
        return true;
    }

    private ResponseList<T> cumulate() {
        final HashMap<Object, ResponseList<T>> map = new HashMap<Object, ResponseList<T>>();
        final ArrayList<ResponseList<T>> list = new ArrayList<ResponseList<T>>();
        synchronized (LOCK) {
            for (AbstractResponse<T> a : responses) {
                ResponseList<T> cache = map.get(a.getValue());
                if (cache == null) {
                    cache = new ResponseList<T>();
                    list.add(cache);
                    map.put(a.getValue(), cache);
                }
                cache.add(a);
            }
        }
        Collections.sort(list);
        this.cumulatedList = list;
        return list.size() > 0 ? list.get(0) : null;
    }

    public void fireAfterSolveEvent(ChallengeSolver<T> solver) {
        controller.fireAfterSolveEvent(this, solver);
    }

    public void setSolverDone(ChallengeSolver<T> solver) {
        try {
            synchronized (LOCK) {
                if (!solverList.contains(solver)) {
                    throw new IllegalStateException("This Job does not contain this solver");
                }
                if (!doneList.add(solver)) {
                    return;
                }
            }
            logSolverStatus(solver, "DONE (" + doneList.size() + "/" + solverList.size() + " solvers done)");
            final ChallengeSolverJobEventSender eventSender = getEventSender(false);
            if (eventSender != null) {
                eventSender.fireEvent(new ChallengeSolverJobEvent(this, ChallengeSolverJobEvent.Type.SOLVER_DONE, solver));
            }
        } finally {
            _notifyAll();
        }
    }

    public boolean _wait(final int timeout) throws InterruptedException {
        final int notifyCnt = LOCK.get();
        synchronized (LOCK) {
            if (notifyCnt != LOCK.get()) {
                return false;
            }
            LOCK.wait(timeout);
            return true;
        }
    }

    public void _notifyAll() {
        synchronized (LOCK) {
            LOCK.incrementAndGet();
            LOCK.notifyAll();
        }
    }

    public void fireBeforeSolveEvent(ChallengeSolver<T> solver) {
        synchronized (LOCK) {
            if (!solverList.contains(solver)) {
                throw new IllegalStateException("This Job does not contain this solver");
            }
        }
        logSolverStatus(solver, "ACTIVE (waits for other solvers first, if configured)");
        final ChallengeSolverJobEventSender eventSender = getEventSender(false);
        if (eventSender != null) {
            eventSender.fireEvent(new ChallengeSolverJobEvent(this, ChallengeSolverJobEvent.Type.SOLVER_START, solver));
        }
        controller.fireBeforeSolveEvent(this, solver);
    }

    private void fireNewAnswerEvent(AbstractResponse<T> abstractResponse) {
        controller.fireNewAnswerEvent(this, abstractResponse);
        final ChallengeSolverJobEventSender eventSender = getEventSender(false);
        if (eventSender != null) {
            eventSender.fireEvent(new ChallengeSolverJobEvent(this, ChallengeSolverJobEvent.Type.NEW_ANSWER, abstractResponse));
        }
    }

    public void fireTimeoutEvent(ChallengeSolver<T> solver) {
        synchronized (LOCK) {
            if (!solverList.contains(solver)) {
                throw new IllegalStateException("This Job does not contain this solver");
            }
        }
        logSolverStatus(solver, "TIMEOUT (solver took too long)");
        final ChallengeSolverJobEventSender eventSender = getEventSender(false);
        if (eventSender != null) {
            eventSender.fireEvent(new ChallengeSolverJobEvent(this, ChallengeSolverJobEvent.Type.SOLVER_TIMEOUT, solver));
        }
    }

    public long getCreated() {
        return created;
    }

    public Challenge<T> getChallenge() {
        return challenge;
    }

    /**
     * Returns the random tie-break ordinal for the given external solver id. The ordinal is drawn once (randomly) the first time it is
     * needed within this job and then kept stable for the rest of this job, so the resulting "who waits for whom" direction is consistent
     * across all solver threads of this job, its wait-loop validation and its logging. A fresh job (i.e. the next captcha) starts with an
     * empty map and therefore draws fresh ordinals, which is what re-randomizes the external solver that goes first for every captcha that
     * gets solved.
     */
    public int getExternalTieBreakOrder(final String solverID) {
        synchronized (externalTieBreakOrder) {
            final Integer order = externalTieBreakOrder.get(solverID);
            if (order != null) {
                return order.intValue();
            }
            final int newOrder = TIEBREAK_RANDOM.nextInt();
            externalTieBreakOrder.put(solverID, Integer.valueOf(newOrder));
            return newOrder;
        }
    }

    public ChallengeSolverJobEventSender getEventSender() {
        return getEventSender(true);
    }

    protected ChallengeSolverJobEventSender getEventSender(final boolean mustInit) {
        while (true) {
            final ChallengeSolverJobEventSender eventSender = this.eventSender.get();
            if (eventSender != null || !mustInit) {
                return eventSender;
            }
            this.eventSender.compareAndSet(null, new ChallengeSolverJobEventSender());
        }
    }

    public boolean isDone() {
        synchronized (LOCK) {
            for (ChallengeSolver<T> sj : solverList) {
                if (!doneList.contains(sj) && sj.isJobDone(this)) {
                    setSolverDone(sj);
                }
            }
            return solverList.size() == doneList.size();
        }
    }

    public boolean isDone(ChallengeSolver<T> instance) {
        synchronized (LOCK) {
            if (instance.isJobDone(this)) {
                setSolverDone(instance);
            }
            return !solverList.contains(instance) || doneList.contains(instance);
        }
    }

    public boolean isSolved() {
        final ResponseList<T> response = getResponse();
        if (response == null) {
            return false;
        }
        if (response.getTrustlevelPercentageSum() != null && response.getTrustlevelPercentageSum() < config.getAutoCaptchaPriorityThreshold()) {
            /* Response quality is not good enough for us */
            return false;
        }
        return true;
    }

    public boolean isAlive() {
        return alive.get();
    }

    public void kill() {
        final ArrayList<ChallengeSolver<T>> killList = new ArrayList<ChallengeSolver<T>>();
        synchronized (LOCK) {
            alive.set(false);
            for (ChallengeSolver<T> s : solverList) {
                if (!doneList.contains(s)) {
                    if (SkipRequest.TIMEOUT.equals(skipRequest)) {
                        logSolverStatus(s, "KILLED (job timed out)");
                    } else {
                        logSolverStatus(s, "KILLED (job is solved, skipped or aborted)");
                    }
                    killList.add(s);
                }
            }
        }
        for (ChallengeSolver<T> s : killList) {
            s.kill(this);
        }
    }

    /**
     * Marks the given solver as actually working on this job (it passed all its wait times and got its captcha slot). Solvers that are only
     * waiting for other solvers are not marked.
     */
    public void setSolverSolving(final ChallengeSolver<?> solver) {
        if (solvingSet.add(solver)) {
            logSolverStatus(solver, "SOLVING (wait times over, captcha slot reserved)");
        }
        _notifyAll();
    }

    /** Marks the solver as stopped because of its own timeout (see JobRunnable#fireTimeoutEvent). Call before the solver is killed. */
    public void setSolverTimedOut(final ChallengeSolver<?> solver) {
        timedOutSet.add(solver);
    }

    /**
     * True if the solver ran into its own timeout or the whole job timed out (skip request TIMEOUT). Used to tell a timeout from other
     * reasons why a solver was interrupted.
     */
    public boolean isSolverTimedOut(final ChallengeSolver<?> solver) {
        return timedOutSet.contains(solver) || SkipRequest.TIMEOUT.equals(skipRequest);
    }

    public boolean isSolverSolving(final ChallengeSolver<?> solver) {
        return solvingSet.contains(solver);
    }

    /**
     * Makes the given solver skip all its remaining wait times for other solvers, so it starts working on this job right now. Wakes up the
     * solver if it is currently waiting.
     */
    public void forceStart(final ChallengeSolver<?> solver) {
        if (forcedStartSet.add(solver)) {
            logSolverStatus(solver, "FORCE START requested (skips all remaining wait times)");
        }
        _notifyAll();
    }

    public boolean isForcedStart(final ChallengeSolver<?> solver) {
        return forcedStartSet.contains(solver);
    }

    /**
     * Returns all external solvers of this job which are neither done nor working on it yet, i.e. which would still be used later if the
     * captcha is not solved by the currently running (manual) solvers.
     */
    public List<ChallengeSolver<?>> getPendingExternalSolvers() {
        final List<ChallengeSolver<?>> ret = new ArrayList<ChallengeSolver<?>>();
        for (final ChallengeSolver<T> solver : getSolverList()) {
            if (solver.getSolverType() == ChallengeSolver.SolverType.EXTERNAL && !isDone(solver) && !isSolverSolving(solver)) {
                ret.add(solver);
            }
        }
        return ret;
    }

    public void waitFor(int timeout, ChallengeSolver<?>... instances) throws InterruptedException {
        waitFor(null, timeout, instances);
    }

    /**
     * @param waiter
     *            the solver that waits (may be null). If {@link #forceStart(ChallengeSolver)} is called for it, the wait ends immediately.
     */
    public void waitFor(final ChallengeSolver<?> waiter, int timeout, ChallengeSolver<?>... instances) throws InterruptedException {
        long endTime = -1;
        if (timeout > 0) {
            endTime = Time.systemIndependentCurrentJVMTimeMillis() + timeout;
            log(this + " Wait max" + timeout + " ms for " + instances);
        } else {
            log(this + " Wait infinite for " + instances);
        }
        try {
            while (!areDone(instances)) {
                if (waiter != null && isForcedStart(waiter)) {
                    log(waiter + " wait aborted by force start");
                    return;
                }
                if (isSolved()) {
                    throw new InterruptedException(this + " is Solved");
                } else if (Thread.interrupted()) {
                    throw new InterruptedException(this + " got interrupted");
                }
                if (!areDone(instances)) {
                    if (endTime > 0) {
                        long timeToWait = endTime - Time.systemIndependentCurrentJVMTimeMillis();
                        if (timeToWait > 0) {
                            log(this + " Wait " + timeToWait);
                            _wait(timeout);
                        } else {
                            log(this + " Timed Out! ");
                            return;
                        }
                    } else {
                        log(this + " Wait infinite");
                        _wait(0);
                    }
                    log(this + " Wokeup");
                }
            }
            if (isSolved()) {
                throw new InterruptedException(this + " is Solved");
            } else if (Thread.interrupted()) {
                throw new InterruptedException(this + " got interrupted");
            }
            log("Exit " + this + " by done: " + areDone(instances));
        } catch (InterruptedException e) {
            log(Exceptions.getStackTrace(e));
            log(this + " exit by interrupt");
            throw e;
        }
    }

    public void setLogger(LogSource logSource) {
        this.logger = logSource;
    }

    public LogSource getLogger() {
        return logger;
    }

    public SkipRequest getSkipRequest() {
        return skipRequest;
    }

    /** Marks the job as ended because the thread waiting for it was interrupted. Call before the job is killed. */
    public void setInterrupted() {
        interrupted = true;
    }

    /**
     * Returns the current status of this job as a whole. It is derived from the existing state on every call, so it changes while the job
     * proceeds (RUNNING -> SOLVED/...). Once the job is finished the result is stable.
     */
    public SolverJobStatus getStatus() {
        final SkipRequest skip = skipRequest;
        if (skip != null) {
            switch (skip) {
            case TIMEOUT:
                return SolverJobStatus.TIMEOUT;
            case REFRESH:
                return SolverJobStatus.REFRESHED;
            default:
                return SolverJobStatus.ABORTED_BY_USER;
            }
        }
        if (isSolved()) {
            return SolverJobStatus.SOLVED;
        }
        if (interrupted) {
            return SolverJobStatus.INTERRUPTED;
        }
        if (isDone()) {
            /*
             * Every solver ran into its own timeout: The job itself did not get a skip request (its own timeout was not reached or the
             * solvers were faster), but from the outside this is still a timeout.
             */
            if (solverList.size() > 0 && timedOutSet.containsAll(solverList)) {
                return SolverJobStatus.TIMEOUT;
            }
            return SolverJobStatus.UNSOLVED;
        }
        return SolverJobStatus.RUNNING;
    }

    public boolean setSkipRequest(SkipRequest skipRequest) {
        boolean kill = false;
        synchronized (LOCK) {
            if (alive.compareAndSet(true, false)) {
                this.skipRequest = skipRequest;
                if (skipRequest != null) {
                    log("Got Skip Request:" + skipRequest);
                }
                kill = true;
            }
        }
        if (kill) {
            kill();
        }
        return kill;
    }

    public Collection<ChallengeSolver<T>> getSolverList() {
        synchronized (LOCK) {
            return Collections.unmodifiableCollection(solverList);
        }
    }

    /**
     * call to tell the job, that the result has been correct
     */
    public void validate() {
        final ArrayList<ResponseList<T>> responsesLists = this.getResponses();
        if (responsesLists == null) {
            return;
        }
        for (int i = 0; i < responsesLists.size(); i++) {
            final ResponseList<T> responseList = responsesLists.get(i);
            for (final AbstractResponse<T> response : responseList) {
                if (i == 0) {
                    // used response
                    response.setValidation(ValidationResult.VALID);
                } else {
                    // unused responses
                    // maybe send invalid instead?
                    response.setValidation(ValidationResult.UNUSED);
                }
            }
        }
    }

    /**
     * call to tell the job, that the result has been INCORRECT
     */
    public void invalidate() {
        final ArrayList<ResponseList<T>> responsesLists = this.getResponses();
        if (responsesLists == null) {
            return;
        }
        for (int i = 0; i < responsesLists.size(); i++) {
            final ResponseList<T> responseList = responsesLists.get(i);
            for (final AbstractResponse<T> response : responseList) {
                if (i == 0) {
                    // used response
                    response.setValidation(ValidationResult.INVALID);
                } else {
                    // unused responses
                    response.setValidation(ValidationResult.UNUSED);
                }
            }
        }
    }
}
