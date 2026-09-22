package org.jdownloader.captcha.v2.solver.service;

import jd.SecondLevelLaunch;
import jd.gui.swing.jdgui.components.premiumbar.ServicePanel;

import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.jdownloader.captcha.v2.SolverService;

public abstract class AbstractSolverService implements SolverService {
    public AbstractSolverService() {
    }

    @Override
    public String getDescription() {
        return null;
    }

    @Override
    public Double getBalance() {
        return null;
    }

    @Override
    public java.util.Currency getBalanceCurrency() {
        return null;
    }

    @Override
    public String getHelpArticleURL() {
        return null;
    }

    @Override
    public String getBuyURL() {
        return null;
    }

    @Override
    public String getStatusText() {
        return "Ready";
    }

    @Override
    public String getStatusActionName() {
        return null;
    }

    @Override
    public void onStatusAction() {
    }

    @Override
    public boolean isStatusActionWarning() {
        return false;
    }

    /**
     * Default: a solver is ready when it does not require a status action (e.g. "Add Account"/"Configure"). Overridable.
     */
    @Override
    public boolean isReady() {
        return getStatusActionName() == null;
    }

    protected void initServicePanel(final KeyHandler... handlers) {
        if (org.appwork.utils.Application.isHeadless()) {
            return;
        }
        SecondLevelLaunch.GUI_COMPLETE.executeWhenReached(new Runnable() {
            @SuppressWarnings("unchecked")
            public void run() {
                for (KeyHandler k : handlers) {
                    k.getEventSender().addListener(new GenericConfigEventListener<Object>() {
                        @Override
                        public void onConfigValidatorError(KeyHandler<Object> keyHandler, Object invalidValue, ValidationException validateException) {
                        }

                        @Override
                        public void onConfigValueModified(KeyHandler<Object> keyHandler, Object newValue) {
                            ServicePanel.getInstance().requestUpdate(true);
                        }
                    });
                }
            }
        });
    }
}
