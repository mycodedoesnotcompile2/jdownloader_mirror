package org.jdownloader.extensions.schedulerV2.actions;

public class CaptchaServiceActionConfig implements IScheduleActionConfig {
    public CaptchaServiceActionConfig(/* Storable */) {
    }

    /**
     * Legacy field. Older saved actions stored the selected captcha service as a CAPTCHA_SERVICE enum name here (e.g. "NINEKWEU"). It is
     * kept only so those actions can be migrated to the new free-text {@link #solverIDs} list. Null for actions created with the new format.
     */
    private String service = null;

    public String getService() {
        return service;
    }

    public void setService(String service) {
        this.service = service;
    }

    /**
     * Comma separated list of captcha solver ids (= plugin hosts / solver ids) to toggle when the action runs. An empty list or a list
     * containing "*" means "all solvers". Null when the action has not been configured yet (then it is migrated from {@link #service}).
     */
    private String solverIDs = null;

    public String getSolverIDs() {
        return solverIDs;
    }

    public void setSolverIDs(String solverIDs) {
        this.solverIDs = solverIDs;
    }
}
