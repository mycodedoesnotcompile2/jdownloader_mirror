package jd.gui.swing.jdgui.views.settings.panels.accountmanager.orderpanel;

import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;

import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;
import org.jdownloader.DomainInfo;
import org.jdownloader.controlling.hosterrule.AccountUsageRule;
import org.jdownloader.controlling.hosterrule.HosterRuleController;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.components.AbstractAddAction;
import org.jdownloader.plugins.controller.LazyPlugin.FEATURE;
import org.jdownloader.plugins.controller.host.HostPluginController;
import org.jdownloader.plugins.controller.host.LazyHostPlugin;

import jd.controlling.AccountController;
import jd.controlling.AccountFilter;
import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.MultiHostHost;

public class NewRuleAction extends AbstractAddAction {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    public NewRuleAction() {
        super();
    }

    public void actionPerformed(final ActionEvent e) {
        final ArrayList<DomainInfo> list = getAvailableDomainInfoList();
        final HosterRuleController hrc = HosterRuleController.getInstance();
        final ChooseHosterDialog d = new ChooseHosterDialog(_GUI.T.NewRuleAction_actionPerformed_choose_hoster_message(), list.toArray(new DomainInfo[] {}));
        final DomainInfo di;
        try {
            Dialog.getInstance().showDialog(d);
            di = d.getSelectedItem();
            if (di == null) {
                return;
            }
        } catch (DialogClosedException e1) {
            e1.printStackTrace();
            return;
        } catch (DialogCanceledException e1) {
            e1.printStackTrace();
            return;
        }
        /* Add rule for selected item [unless user cancels edit dialog]. */
        final String domain = di.getTld();
        final AccountUsageRule rule = new AccountUsageRule(domain);
        rule.setEnabled(true);
        if (!HosterRuleController.getInstance().validateRule(rule)) {
            /* Rule is invalid -> Do not add it */
            return;
        }
        if (!HosterRuleController.getInstance().showEditPanel(rule)) {
            /* User canceled editing the new rule -> Do not add it */
            return;
        }
        /* Remove already existing rule(s) for same domain as we only want one rule per domain */
        hrc.removeRulesByDomain(domain);
        hrc.add(rule);
    }

    /** Returns list of possible domains which an AccountUsageRule can be added for. */
    protected ArrayList<DomainInfo> getAvailableDomainInfoList() {
        final HashSet<DomainInfo> domains = new HashSet<DomainInfo>();
        /* List of all hosts supported by all multi hoster accounts the user owns */
        final List<MultiHostHost> all_mhosts = new ArrayList<MultiHostHost>();
        /* Collect domains of all multihoster accounts which the user currently has. */
        final List<Account> multihoster_accounts = AccountController.getInstance().listAccounts(new AccountFilter().setFeature(FEATURE.MULTIHOST));
        for (final Account acc : multihoster_accounts) {
            final AccountInfo ai = acc.getAccountInfo();
            if (ai == null) {
                /* Multihost without any AccountInfo -> Shouldn't happen. */
                continue;
            }
            final List<MultiHostHost> this_mhosts = ai.getMultiHostSupportV2();
            if (this_mhosts == null || this_mhosts.isEmpty()) {
                /* Multihost without any supported hosts -> Shouldn't happen. */
                continue;
            }
            all_mhosts.addAll(this_mhosts);
        }
        final HashSet<String> multihosterSupportedDomains = new HashSet<String>();
        for (final MultiHostHost mhost : all_mhosts) {
            multihosterSupportedDomains.addAll(mhost.getDomains());
        }
        final Collection<LazyHostPlugin> plugins = HostPluginController.getInstance().list();
        /* Collect all domains which the user is allowed to create usage rules for. */
        for (final LazyHostPlugin plg : plugins) {
            if (plg.hasFeature(FEATURE.MULTIHOST)) {
                /* Do not allow user to add rules for multihoster domains. */
                continue;
            } else if (plg.isOfflinePlugin()) {
                /* Do not allow users to create account usage rules for domains known to be permanently offline. */
                continue;
            } else if (plg.hasFeature(FEATURE.USENET) && !plg.getHost().equals("usenet")) {
                /* Do not allow usage rule creation for pure usenet plugins. */
                continue;
            } else if (plg.hasFeature(FEATURE.INTERNAL)) {
                /* Do not allow users to create account usage rules for internal plugins. */
                continue;
            } else if (!plg.isPremium() && !multihosterSupportedDomains.contains(plg.getHost())) {
                /* Plugin has no account support and no multihost has support for it -> Do not allow the user to create a rule for it. */
                continue;
            }
            domains.add(plg.getDomainInfo());
        }
        /* Sort results. */
        final ArrayList<DomainInfo> lst = new ArrayList<DomainInfo>(domains);
        Collections.sort(lst, new Comparator<DomainInfo>() {
            @Override
            public int compare(DomainInfo o1, DomainInfo o2) {
                return o1.getTld().compareTo(o2.getTld());
            }
        });
        return lst;
    }

    @Override
    public String getTooltipText() {
        return _GUI.T.NewRuleAction_getTooltipText_tt_();
    }
}
