package org.jdownloader.gui.toolbar.action;

import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

import javax.swing.AbstractButton;
import javax.swing.Action;
import javax.swing.Icon;

import jd.controlling.AccountController;
import jd.controlling.AccountControllerEvent;
import jd.controlling.AccountControllerListener;
import jd.plugins.Account;

import org.appwork.swing.components.ExtJToggleButton;
import org.appwork.utils.StringUtils;
import org.appwork.utils.images.IconIO;
import org.appwork.utils.swing.EDTRunner;
import org.jdownloader.DomainInfo;
import org.jdownloader.controlling.contextmenu.Customizer;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;
import org.jdownloader.plugins.controller.host.HostPluginController;
import org.jdownloader.plugins.controller.host.LazyHostPlugin;

/**
 * Generic toolbar toggle action for download accounts of ALL account types (free, premium, lifetime, multihoster, unknown, valid, invalid,
 * expired, ...). There is deliberately no filter on the account type. The user configures a comma separated list of hosts/domains via the
 * action setting; a single click enables or disables all matching accounts at once. An empty list targets none. There is deliberately no
 * wildcard to target all accounts.
 * <p>
 * A plugin can have multiple domains and the user may enter several of them. Therefore the matching is done per account (every account is
 * visited exactly once and added to the result at most once) instead of per configured host. This guarantees that an account is toggled
 * only once per run, no matter how many of its domains are configured.
 * <p>
 * The toolbar icon shows the favicon if exactly one hoster is configured, otherwise the default icon with the number of hosters.
 */
public class ToggleAllAccountTypesAction extends AbstractToolBarAction implements AccountControllerListener {
    /* Default "" = target no account, so a freshly added action does nothing until the user explicitly configures which hosts to toggle. */
    private String           hosts = "";
    /* Icon state: the single configured hoster (favicon) or the number of configured hosters (badge on the default icon). */
    private String           iconHost;
    private int              iconCount;
    private ExtJToggleButton toolbarButton;

    public ToggleAllAccountTypesAction() {
        setIconKey(IconKey.ICON_PREMIUM);
        setName(_GUI.T.ToggleAllAccountTypesAction_name());
        AccountController.getInstance().getEventSender().addListener(this, true);
        updateSelectedState();
        updateEnabledState();
    }

    @Override
    protected String createTooltip() {
        return _GUI.T.ToggleAllAccountTypesAction_tooltip();
    }

    @Customizer(link = "#getTranslationForHosts")
    public String getHosts() {
        return hosts;
    }

    public static String getTranslationForHosts() {
        return "<html>" + _GUI.T.ToggleAllAccountTypesAction_hosts_hint() + "</html>";
    }

    public void setHosts(final String hosts) {
        this.hosts = hosts;
        updateSelectedState();
        updateEnabledState();
        updateIcon();
    }

    /** Returns the lower case host/domain names the user configured. Names of known plugins are added with their main host as well. */
    private Set<String> getWantedHosts() {
        final Set<String> ret = new HashSet<String>();
        if (StringUtils.isEmpty(hosts)) {
            return ret;
        }
        final String[] entries = hosts.split(",");
        for (int i = 0; i < entries.length; i++) {
            final String wanted = entries[i].trim().toLowerCase(Locale.ENGLISH);
            if (wanted.length() == 0) {
                continue;
            }
            ret.add(wanted);
            /* A configured domain may be an alternative domain of a plugin whose accounts are stored under its main host. */
            final LazyHostPlugin lazy = HostPluginController.getInstance().get(wanted);
            if (lazy != null && lazy.getHost() != null) {
                ret.add(lazy.getHost().toLowerCase(Locale.ENGLISH));
            }
        }
        return ret;
    }

    /**
     * Returns the distinct hosters of the configured entries. Several domains of the same plugin are counted as one hoster, so the result
     * can be smaller than the number of entries.
     */
    private Set<String> listHosters() {
        final Set<String> ret = new LinkedHashSet<String>();
        if (StringUtils.isEmpty(hosts)) {
            return ret;
        }
        final String[] entries = hosts.split(",");
        for (int i = 0; i < entries.length; i++) {
            final String wanted = entries[i].trim().toLowerCase(Locale.ENGLISH);
            if (wanted.length() == 0) {
                continue;
            }
            final LazyHostPlugin lazy = HostPluginController.getInstance().get(wanted);
            if (lazy != null && lazy.getHost() != null) {
                ret.add(lazy.getHost().toLowerCase(Locale.ENGLISH));
            } else {
                ret.add(wanted);
            }
        }
        return ret;
    }

    /**
     * Single hoster: show its favicon. Multiple hosters: show the default icon with the number of hosters. Empty: default icon.
     */
    private void updateIcon() {
        String newHost = null;
        int newCount = 0;
        if (!StringUtils.isEmpty(hosts)) {
            final Set<String> hosters = listHosters();
            newCount = hosters.size();
            if (newCount == 1) {
                newHost = hosters.iterator().next();
            }
        }
        if (newCount == iconCount && StringUtils.equals(newHost, iconHost)) {
            return;
        }
        iconCount = newCount;
        iconHost = newHost;
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
        final String host = iconHost;
        if (host != null) {
            final DomainInfo domainInfo = DomainInfo.getInstance(host);
            final Icon icon = domainInfo.getIcon(size);
            if (icon != null) {
                return icon;
            }
            return IconIO.getScaledInstance(domainInfo.getFavIcon(), size, size);
        }
        final Icon base = NewTheme.I().getIcon(getIconKey(), size);
        if (iconCount > 1) {
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
     * Resolves the configured hosts to accounts. Every account is checked once and added at most once, so it can never be toggled twice
     * even if several of its plugin's domains are configured.
     */
    private List<Account> resolveAccounts() {
        final List<Account> ret = new ArrayList<Account>();
        if (StringUtils.isEmpty(hosts)) {
            return ret;
        }
        final Set<String> wanted = getWantedHosts();
        /* Account.equals() compares hoster/user/password, so remember the unique ids to be safe against distinct but "equal" accounts. */
        final Set<Long> handled = new HashSet<Long>();
        /* No AccountFilter on purpose: accounts of all types are included. */
        for (final Account account : AccountController.getInstance().list()) {
            if (!handled.add(account.getId().getID())) {
                continue;
            }
            if (matches(account, wanted)) {
                ret.add(account);
            }
        }
        return ret;
    }

    private boolean matches(final Account account, final Set<String> wanted) {
        final String hoster = StringUtils.toLowerCaseOrNull(account.getHoster());
        if (hoster != null && wanted.contains(hoster)) {
            return true;
        }
        final String byPlugin = StringUtils.toLowerCaseOrNull(account.getHosterByPlugin());
        return byPlugin != null && wanted.contains(byPlugin);
    }

    private boolean areAllEnabled(final List<Account> accounts) {
        if (accounts.isEmpty()) {
            return false;
        }
        for (final Account account : accounts) {
            if (!account.isEnabled()) {
                return false;
            }
        }
        return true;
    }

    private void updateSelectedState() {
        final boolean selected = areAllEnabled(resolveAccounts());
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                setSelected(selected);
            }
        };
    }

    /** Greys the action out when no account matches the configured hosts. */
    private void updateEnabledState() {
        final boolean enabled = !resolveAccounts().isEmpty();
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                setEnabled(enabled);
            }
        };
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        final List<Account> accounts = resolveAccounts();
        if (accounts.isEmpty()) {
            return;
        }
        /* Toggle: if all are currently enabled, disable all; otherwise enable all. */
        final boolean newState = !areAllEnabled(accounts);
        for (final Account account : accounts) {
            account.setEnabled(newState);
        }
        updateSelectedState();
    }

    @Override
    public void onAccountControllerEvent(final AccountControllerEvent event) {
        updateSelectedState();
        updateEnabledState();
    }
}
