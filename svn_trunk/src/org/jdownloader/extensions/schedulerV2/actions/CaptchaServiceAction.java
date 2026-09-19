package org.jdownloader.extensions.schedulerV2.actions;

import java.util.ArrayList;
import java.util.List;

import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import org.appwork.utils.StringUtils;
import org.appwork.utils.logging2.LogInterface;
import org.jdownloader.captcha.v2.ChallengeResponseController;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.extensions.schedulerV2.translate.T;

/**
 * Toggles a user-defined set of captcha solver services. The user configures a comma separated list of solver ids (see
 * {@link SolverService#getID()}); an empty list or a list containing "*" toggles all solvers. Each run flips the state: if all configured
 * solvers are currently enabled they are disabled, otherwise they are all enabled.
 */
@ScheduleActionIDAnnotation("SET_CAPTCHASERVICE")
public class CaptchaServiceAction extends AbstractScheduleAction<CaptchaServiceActionConfig> {
    public CaptchaServiceAction(String configJson) {
        super(configJson);
    }

    @Override
    public String getReadableName() {
        return T.T.action_setCaptchaService();
    }

    @Override
    public void execute(LogInterface logger) {
        final List<SolverService> services = resolveServices();
        if (services.isEmpty()) {
            return;
        }
        /* Toggle: if all configured solvers are currently enabled, disable them all; otherwise enable them all. */
        boolean allEnabled = true;
        for (final SolverService service : services) {
            if (!service.isEnabled()) {
                allEnabled = false;
                break;
            }
        }
        final boolean newState = !allEnabled;
        for (final SolverService service : services) {
            service.setEnabled(newState);
        }
    }

    /**
     * Returns the configured solver-id list. Migrates a legacy action (that still stored the old CAPTCHA_SERVICE enum) to the new format on
     * first access and persists the migrated value.
     */
    private String getEffectiveSolverIDs() {
        final String ids = getConfig().getSolverIDs();
        if (ids != null) {
            return ids;
        }
        final String legacy = getConfig().getService();
        final String migrated;
        if ("NINEKWEU".equals(legacy)) {
            migrated = "9kw.eu";
        } else if ("DEATHBYCAPTCHA".equals(legacy)) {
            migrated = "deathbycaptcha.com";
        } else {
            /* No (or NONE) legacy value: default to an empty list. */
            migrated = "";
        }
        getConfig().setSolverIDs(migrated);
        return migrated;
    }

    /** Resolves the configured ids to solver services. Empty list or a "*" entry resolves to all available solvers. */
    private List<SolverService> resolveServices() {
        final List<SolverService> all = ChallengeResponseController.getInstance().listServices();
        final String raw = getEffectiveSolverIDs();
        if (StringUtils.isEmpty(raw) || raw.contains("*")) {
            return new ArrayList<SolverService>(all);
        }
        final List<SolverService> ret = new ArrayList<SolverService>();
        final String[] ids = raw.split(",");
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

    /** Builds the hint text listing the solver ids that currently exist, so the user knows the valid values. */
    private String getSolverIDsHint() {
        final StringBuilder sb = new StringBuilder();
        sb.append("Comma separated solver ids (empty or \"*\" = all). Available: ");
        final List<SolverService> all = ChallengeResponseController.getInstance().listServices();
        boolean first = true;
        for (final SolverService service : all) {
            if (!first) {
                sb.append(", ");
            }
            sb.append(service.getID());
            first = false;
        }
        return sb.toString();
    }

    @Override
    protected void createPanel() {
        panel.put(new JLabel(T.T.action_captcha_service() + ":"), "gapleft 10,");
        final JTextField txtSolverIDs = new JTextField();
        txtSolverIDs.setToolTipText(getSolverIDsHint());
        final String current = getConfig().getSolverIDs();
        txtSolverIDs.setText(current != null ? current : "");
        txtSolverIDs.getDocument().addDocumentListener(new DocumentListener() {
            private void save() {
                getConfig().setSolverIDs(txtSolverIDs.getText());
            }

            @Override
            public void insertUpdate(DocumentEvent e) {
                save();
            }

            @Override
            public void removeUpdate(DocumentEvent e) {
                save();
            }

            @Override
            public void changedUpdate(DocumentEvent e) {
                save();
            }
        });
        panel.put(txtSolverIDs, "width 250!");
    };

    @Override
    public String getReadableParameter() {
        final String ids = getConfig().getSolverIDs();
        if (StringUtils.isEmpty(ids)) {
            return "*";
        }
        return ids;
    }
}
