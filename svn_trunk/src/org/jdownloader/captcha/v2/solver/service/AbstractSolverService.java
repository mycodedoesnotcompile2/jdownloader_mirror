package org.jdownloader.captcha.v2.solver.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import jd.plugins.CaptchaType.CAPTCHA_TYPE;

import org.jdownloader.gui.settings.AbstractConfigPanel;

import jd.SecondLevelLaunch;
import jd.gui.swing.jdgui.components.premiumbar.ServicePanel;
import jd.plugins.PluginConfigPanelNG;

import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.jdownloader.captcha.v2.CaptchaSolverConfigV3;
import org.jdownloader.captcha.v2.ChallengeResponseController;
import org.jdownloader.captcha.v2.SolverService;

public abstract class AbstractSolverService implements SolverService {
    public AbstractSolverService() {
    }

    @Override
    public String getDescription() {
        return null;
    }

    @Override
    public List<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
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

    @Override
    public AbstractConfigPanel getConfigComponent() {
        final CaptchaSolverConfigV3 cfg = getConfigV3();
        /* Build a config panel generically from the solver's V3 config interface, just like plugin config panels. */
        final PluginConfigPanelNG panel = new PluginConfigPanelNG() {
            @Override
            public void updateContents() {
            }

            @Override
            public void save() {
            }
        };
        panel.build(cfg);
        return panel;
    }

    /*
     * Wait-for timings are stored (per solver) in the solver's own CaptchaSolverConfigV3 (getWaitForOthers). The map is empty by default,
     * which means "automatic / no wait". It is hidden in the GUI for now.
     */
    @Override
    public synchronized int getWaitForByID(String solverID) {
        final Map<String, Integer> map = getConfigV3().getWaitForOthers();
        if (map != null) {
            final Integer obj = map.get(solverID);
            return obj == null ? 0 : Math.max(0, obj.intValue());
        } else {
            return 0;
        }
    }

    @Override
    public synchronized void setWaitFor(String id, Integer waitFor) {
        final CaptchaSolverConfigV3 cfg = getConfigV3();
        Map<String, Integer> map = cfg.getWaitForOthers();
        if (map == null) {
            map = new HashMap<String, Integer>();
        }
        if (id == null || waitFor == null || waitFor.intValue() <= 0) {
            if (id != null) {
                map.remove(id);
            }
        } else {
            map.put(id, waitFor);
        }
        cfg.setWaitForOthers(map);
    }

    @Override
    public synchronized Map<String, Integer> getWaitForMapCopy() {
        final Map<String, Integer> ret = new HashMap<String, Integer>();
        final Map<String, Integer> map = getConfigV3().getWaitForOthers();
        if (map != null) {
            ret.putAll(map);
        }
        return ret;
    }

    @Override
    public boolean isEnabled() {
        return getConfigV3().isEnabled();
    }

    @Override
    public void setEnabled(boolean b) {
        getConfigV3().setEnabled(b);
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

    public static ArrayList<SolverService> validateWaittimeQueue(SolverService start, SolverService check) {
        if (start == null || check == null) {
            return null;
        } else {
            return validateWaittimeQueue(start, check, new ArrayList<SolverService>(), new HashSet<SolverService>());
        }
    }

    private static final Object LOCK = new Object();

    private static ArrayList<SolverService> validateWaittimeQueue(SolverService start, SolverService check, ArrayList<SolverService> arrayList, HashSet<SolverService> dupe) {
        synchronized (LOCK) {
            if (arrayList == null) {
                arrayList = new ArrayList<SolverService>();
            }
            if (dupe == null) {
                dupe = new HashSet<SolverService>();
            }
            //
            // System.out.println("Start: " + start.getName());
            // System.out.println("Check: " + check.getName());
            if (arrayList.size() == 0) {
                // System.out.println("Added " + start.getName());
                arrayList.add(start);
                dupe.add(start);
            }
            // System.out.println("Added " + check.getName());
            arrayList.add(check);
            if (!dupe.add(check)) {
                // System.out.println("Dupe found " + check.getName());
                return arrayList;
            }
            // System.out.println(check.getName() + ": " + JSonStorage.serializeToJson(check.getWaitForMapCopy()));
            for (final Entry<String, Integer> es : check.getWaitForMapCopy().entrySet()) {
                final SolverService service = ChallengeResponseController.getInstance().getServiceByID(es.getKey());
                if (service != null && service.isEnabled() && es.getValue() != null && es.getValue().intValue() > 0) {
                    // System.out.println(check.getName() + " waits for " + service.getName() + " : " + es.getValue().intValue());
                    final ArrayList<SolverService> ret = validateWaittimeQueue(start, service, new ArrayList<SolverService>(arrayList), new HashSet<SolverService>(dupe));
                    if (ret != null) {
                        return ret;
                    }
                }
            }
            return null;
        }
    }
}
