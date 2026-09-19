package org.jdownloader.captcha.v2;

import java.util.Currency;
import java.util.List;
import java.util.Map;

import javax.swing.Icon;

import org.jdownloader.gui.settings.AbstractConfigPanel;

import jd.plugins.CaptchaType.CAPTCHA_TYPE;

public interface SolverService {
    public abstract String getName();

    /** Short human readable description of what this solver does. May be null. */
    public abstract String getDescription();

    /** Captcha types supported by this solver, or null if unknown (treated as "supports all"). */
    public abstract List<CAPTCHA_TYPE> getSupportedCaptchaTypes();

    /** Total account balance of this solver across its valid accounts, or null if this solver has no balance. */
    public abstract Double getBalance();

    /** Currency of {@link #getBalance()}, or null when unknown / not applicable. */
    public abstract Currency getBalanceCurrency();

    /** URL of a help/knowledgebase article for this solver, or null when there is none (then no help icon is shown). */
    public abstract String getHelpArticleURL();

    /**
     * Status text shown in the solver overview (e.g. "Ready", "Ready | Balance: ..."). Returns null when instead an action button (see
     * {@link #getStatusActionName()}) should be shown.
     */
    public abstract String getStatusText();

    /** Label of the status action button (e.g. "Configure", "Add Account"), or null when {@link #getStatusText()} should be shown. */
    public abstract String getStatusActionName();

    /** Performs the status action (only called when {@link #getStatusActionName()} is non-null). */
    public abstract void onStatusAction();

    /**
     * Returns true when {@link #getStatusActionName()} represents a warning that needs the user's attention (e.g. "Enable solver
     * accounts" because external captcha solver accounts are globally disabled), as opposed to a routine action like "Add Account". Used
     * by the solver overview to pick a warning icon instead of the regular "add" icon for the status action button. Ignored when
     * {@link #getStatusActionName()} is null.
     */
    public abstract boolean isStatusActionWarning();

    /**
     * Returns true if this solver is ready to be used right now (e.g. has a valid account / is connected). Not-ready solvers are sorted to
     * the bottom of the solver overview.
     */
    public abstract boolean isReady();

    /**
     * Returns the captcha solver config (V3) of this solver. For plugin solvers this is the plugin's own config; for local solvers it is
     * their dedicated {@link CaptchaSolverConfigV3} subclass. Every solver has a config, so this never returns null.
     */
    public abstract CaptchaSolverConfigV3 getConfigV3();

    /**
     * Returns the config panel to show below the supported-captcha-types overview for this solver. Built generically from
     * {@link #getConfigV3()}. May be null.
     */
    public abstract AbstractConfigPanel getConfigComponent();

    public abstract boolean hasConfigPanel();

    public abstract Icon getIcon(int size);

    public abstract String getType();

    public int getWaitForByID(String solverID);

    public abstract String getID();

    public abstract boolean isEnabled();

    public abstract void setEnabled(boolean b);

    public abstract void setWaitFor(String id, Integer waitFor);

    public abstract Map<String, Integer> getWaitForMapCopy();
}
