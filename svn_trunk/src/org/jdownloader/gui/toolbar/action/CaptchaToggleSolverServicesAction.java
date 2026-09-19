package org.jdownloader.gui.toolbar.action;

import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.List;

import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.BooleanKeyHandler;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.utils.StringUtils;
import org.appwork.utils.swing.EDTRunner;
import org.jdownloader.captcha.v2.CaptchaSolverConfigV3;
import org.jdownloader.captcha.v2.ChallengeResponseController;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.controlling.contextmenu.Customizer;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;

/**
 * Generic toolbar toggle action for captcha solver services. The user configures a comma separated list of solver ids (see
 * {@link SolverService#getID()}) via the action setting; a single click toggles all matching solvers on or off at once. An empty list or a
 * list containing "*" targets all solvers, which is also the default. This single action replaces the former per-solver toggle actions
 * (dialog/browser/jac/myjdremote). The toggle state is kept in sync with external enable/disable changes via a config listener.
 */
public class CaptchaToggleSolverServicesAction extends AbstractToolBarAction implements GenericConfigEventListener<Boolean> {
    /* Default "" = target no solver, so a freshly added action does nothing until the user explicitly configures which solvers to toggle. */
    private String solverIDs = "";

    public CaptchaToggleSolverServicesAction() {
        setIconKey(IconKey.ICON_OCR);
        setName(_GUI.T.CaptchaToggleSolverServicesAction_name());
        /* Listen to the "enabled" state of every solver so the toggle button reflects changes made elsewhere. */
        final List<SolverService> all = ChallengeResponseController.getInstance().listServices();
        for (final SolverService service : all) {
            final BooleanKeyHandler kh = getEnabledKeyHandler(service);
            if (kh != null) {
                kh.getEventSender().addListener(this, true);
            }
        }
        updateSelectedState();
        updateEnabledState();
    }

    @Override
    protected String createTooltip() {
        return _GUI.T.CaptchaToggleSolverServicesAction_tooltip();
    }

    private static BooleanKeyHandler getEnabledKeyHandler(final SolverService service) {
        final CaptchaSolverConfigV3 cfg = service.getConfigV3();
        return cfg._getStorageHandler().getKeyHandler("enabled", BooleanKeyHandler.class);
    }

    public static String getTranslationForSolverIDs() {
        /* Build the hint text as an HTML list (one id per line) so the action edit dialog does not become extremely wide. */
        final StringBuilder sb = new StringBuilder();
        sb.append("<html>").append(_GUI.T.CaptchaToggleSolverServicesAction_solverIDs_hint());
        final List<SolverService> all = ChallengeResponseController.getInstance().listServices();
        for (final SolverService service : all) {
            sb.append("<br>&nbsp;&nbsp;- ").append(service.getID());
        }
        sb.append("</html>");
        return sb.toString();
    }

    @Customizer(link = "#getTranslationForSolverIDs")
    public String getSolverIDs() {
        return solverIDs;
    }

    public void setSolverIDs(final String solverIDs) {
        this.solverIDs = solverIDs;
        updateSelectedState();
        updateEnabledState();
    }

    /**
     * Resolves the configured ids to solver services (case-insensitive, unknown ids are ignored). A "*" entry resolves to all available
     * solvers. An empty list resolves to nothing (the action is then greyed out), so it does NOT mean "all".
     */
    private List<SolverService> resolveServices() {
        final List<SolverService> all = ChallengeResponseController.getInstance().listServices();
        if (solverIDs != null && solverIDs.contains("*")) {
            return new ArrayList<SolverService>(all);
        }
        final List<SolverService> ret = new ArrayList<SolverService>();
        if (StringUtils.isEmpty(solverIDs)) {
            return ret;
        }
        final String[] ids = solverIDs.split(",");
        for (int i = 0; i < ids.length; i++) {
            final String wanted = ids[i].trim();
            if (wanted.length() == 0) {
                continue;
            }
            for (final SolverService service : all) {
                if (StringUtils.equalsIgnoreCase(service.getID(), wanted) && !ret.contains(service)) {
                    ret.add(service);
                    break;
                }
            }
        }
        return ret;
    }

    private boolean areAllEnabled(final List<SolverService> services) {
        if (services.isEmpty()) {
            return false;
        }
        for (final SolverService service : services) {
            if (!service.isEnabled()) {
                return false;
            }
        }
        return true;
    }

    private void updateSelectedState() {
        final boolean selected = areAllEnabled(resolveServices());
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                setSelected(selected);
            }
        };
    }

    /** Greys the action out when no valid solver is configured (empty list or only unknown ids). */
    private void updateEnabledState() {
        final boolean enabled = !resolveServices().isEmpty();
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                setEnabled(enabled);
            }
        };
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        final List<SolverService> services = resolveServices();
        if (services.isEmpty()) {
            return;
        }
        /* Toggle: if all are currently enabled, disable all; otherwise enable all. */
        final boolean newState = !areAllEnabled(services);
        for (final SolverService service : services) {
            service.setEnabled(newState);
        }
        updateSelectedState();
    }

    @Override
    public void onConfigValueModified(final KeyHandler<Boolean> keyHandler, final Boolean newValue) {
        updateSelectedState();
    }

    @Override
    public void onConfigValidatorError(final KeyHandler<Boolean> keyHandler, final Boolean invalidValue, final ValidationException validateException) {
    }
}
