package org.jdownloader.gui.toolbar.action;

import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.AbstractButton;
import javax.swing.Action;
import javax.swing.Icon;

import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.BooleanKeyHandler;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.swing.components.ExtJToggleButton;
import org.appwork.utils.StringUtils;
import org.appwork.utils.swing.EDTRunner;
import org.jdownloader.captcha.v2.CaptchaSolverConfigV3;
import org.jdownloader.captcha.v2.ChallengeResponseController;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.controlling.contextmenu.Customizer;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;

/**
 * Generic toolbar toggle action for captcha solver services. The user configures a comma separated list of solver ids (see
 * {@link SolverService#getID()}) via the action setting; a single click toggles all matching solvers on or off at once. An empty list
 * targets no solver, which is also the default. There is deliberately no wildcard to target all solvers, as all captcha solvers can already
 * be toggled by a separate action. This single action replaces the former per-solver toggle actions (dialog/browser/jac/myjdremote). The
 * toggle state is kept in sync with external enable/disable changes via a config listener.
 */
public class CaptchaToggleSolverServicesAction extends AbstractToolBarAction implements GenericConfigEventListener<Boolean> {
    /* Default "" = target no solver, so a freshly added action does nothing until the user explicitly configures which solvers to toggle. */
    private String           solverIDs = "";
    /* Icon state: the single configured solver (its icon) or the number of configured solvers (badge on the default icon). */
    private SolverService    iconService;
    private int              iconCount;
    private ExtJToggleButton toolbarButton;

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
        final List<SolverService> all = ChallengeResponseController.getInstance().getOrderedServices();
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
        updateIcon();
    }

    /**
     * Single solver: show its icon. Multiple solvers: show the default icon with the number of solvers. Empty: default icon.
     */
    private void updateIcon() {
        SolverService newService = null;
        int newCount = 0;
        if (!StringUtils.isEmpty(solverIDs)) {
            final List<SolverService> services = resolveServices();
            newCount = services.size();
            if (newCount == 1) {
                newService = services.get(0);
            }
        }
        if (newCount == iconCount && newService == iconService) {
            return;
        }
        iconCount = newCount;
        iconService = newService;
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                firePropertyChange(Action.SMALL_ICON, new Object(), new Object());
                firePropertyChange(Action.LARGE_ICON_KEY, new Object(), new Object());
                updateButtonIcons();
            }
        };
    }

    private Icon createIcon(final int size) {
        final SolverService service = iconService;
        if (service != null) {
            final Icon icon = service.getIcon(size);
            if (icon != null) {
                return icon;
            }
        }
        final Icon base = NewTheme.I().getIcon(getIconKey(), size);
        if (service == null && iconCount > 1) {
            return new CountBadgeIcon(base, iconCount);
        }
        return base;
    }

    @Override
    protected Icon getSmallIconForToolbar() {
        return createIcon(18);
    }

    @Override
    protected Icon getLargeIconForToolbar() {
        return createIcon(24);
    }

    /** The toolbar sets the icons of toggle buttons explicitly instead of reading them from the action, so the button is updated manually. */
    private void updateButtonIcons() {
        final ExtJToggleButton button = toolbarButton;
        if (button == null) {
            return;
        }
        final Icon base = createIcon(24);
        final Icon normal = CountBadgeIcon.createToolbarCheckBoxIcon(base, false);
        final Icon selected = CountBadgeIcon.createToolbarCheckBoxIcon(base, true);
        button.setIcon(normal);
        button.setRolloverIcon(normal);
        button.setSelectedIcon(selected);
        button.setRolloverSelectedIcon(selected);
    }

    @Override
    public AbstractButton createButton() {
        if (toolbarButton == null) {
            final ExtJToggleButton button = new ExtJToggleButton(this);
            button.setHideActionText(true);
            toolbarButton = button;
            updateButtonIcons();
        }
        return toolbarButton;
    }

    /**
     * Resolves the configured ids to solver services (case-insensitive, unknown ids are ignored). An empty list resolves to nothing (the
     * action is then greyed out), so it does NOT mean "all". There is deliberately no wildcard to target all solvers.
     */
    private List<SolverService> resolveServices() {
        final List<SolverService> all = ChallengeResponseController.getInstance().listServices();
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
            if (!service.getConfigV3().isEnabled()) {
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
            service.getConfigV3().setEnabled(newState);
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
