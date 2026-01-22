package org.jdownloader.captcha.v2;

import org.appwork.utils.logging2.extmanager.LoggerFactory;
import org.jdownloader.controlling.UniqueAlltimeID;

public class AbstractResponse<T> {
    private final UniqueAlltimeID id                  = new UniqueAlltimeID();
    private Integer               trustlevel          = null;
    private ValidationResult      validation          = null;
    private String                captchaSolverTaskID = null;

    public ValidationResult getValidation() {
        return validation;
    }

    public boolean setValidation(final ValidationResult validation) {
        try {
            if (this.validation != null) {
                /* Already validated */
                return false;
            }
            this.validation = validation;
            final Object solver = getSolver();
            if (solver != null && solver instanceof ChallengeSolver) {
                final Challenge<T> c = getChallenge();
                switch (validation) {
                case INVALID:
                    if (c != null) {
                        c.sendStatsValidation(((ChallengeSolver) solver), "false");
                    }
                    return ((ChallengeSolver<?>) solver).setInvalid(this);
                case UNUSED:
                    if (c != null) {
                        c.sendStatsValidation(((ChallengeSolver) solver), "unused");
                    }
                    return ((ChallengeSolver<?>) solver).setUnused(this);
                case VALID:
                    if (c != null) {
                        c.sendStatsValidation(((ChallengeSolver) solver), "true");
                    }
                    return ((ChallengeSolver<?>) solver).setValid(this);
                }
            }
            return true;
        } catch (final Throwable e) {
            this.validation = null;
            LoggerFactory.getDefaultLogger().log(e);
            return false;
        }
    }

    public Integer getTrustLevel() {
        return trustlevel;
    }

    public UniqueAlltimeID getId() {
        return id;
    }

    /**
     * Set level of trust of captcha response in percent. Mostly used for old automatic captcha solvers which sometimes were not 100% sure
     * if their answer was correct. <br>
     */
    public void setTrustLevel(Integer num) {
        this.trustlevel = num;
    }

    public T getValue() {
        return value;
    }

    public void setValue(T value) {
        this.value = value;
    }

    private T                  value;
    private final Object       solver;
    private final Challenge<T> challenge;

    public Object getSolver() {
        return solver;
    }

    public AbstractResponse(Challenge<T> challenge, Object solver, T responseData) {
        this.solver = solver;
        this.value = responseData;
        this.challenge = challenge;
    }

    public String toString() {
        return getClass().getSimpleName() + ": Value:" + value + " Priority: " + trustlevel + " Solved By: " + solver;
    }

    public Challenge<T> getChallenge() {
        return challenge;
    }

    public String getCaptchaSolverTaskID() {
        return captchaSolverTaskID;
    }

    public void setCaptchaSolverTaskID(String captchaSolverTaskID) {
        this.captchaSolverTaskID = captchaSolverTaskID;
    }
}
