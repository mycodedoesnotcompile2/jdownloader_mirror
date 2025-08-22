package org.jdownloader.controlling.filter;

import org.appwork.storage.Storable;
import org.appwork.storage.StorableAllowPrivateAccessModifier;

import jd.controlling.AccountController;
import jd.controlling.AccountFilter;
import jd.controlling.linkcrawler.CrawledLink;
import jd.gui.swing.jdgui.views.settings.panels.linkgrabberfilter.editdialog.PluginStatusFilter;
import jd.plugins.Account.AccountType;

public class CompiledPluginStatusFilter extends PluginStatusFilter implements Storable {
    @StorableAllowPrivateAccessModifier
    private CompiledPluginStatusFilter() {
    }

    public CompiledPluginStatusFilter(PluginStatusFilter pluginStatusFilter) {
        super(pluginStatusFilter.getMatchType(), pluginStatusFilter.isEnabled(), pluginStatusFilter.getPluginStatus());
    }

    public boolean matches(CrawledLink link) {
        switch (getMatchType()) {
        case IS:
            switch (getPluginStatus()) {
            case PREMIUM:
                return VerifyPremium(link);
            case ACCOUNT:
                return VerifyAccount(link);
            case AUTOCAPTCHA:
                return link.hasAutoCaptcha() || !link.hasCaptcha(null);
            case NO_DIRECT_HTTP:
                return !link.isDirectHTTP() && !link.isFTP();
            }
            break;
        case ISNOT:
            switch (getPluginStatus()) {
            case PREMIUM:
                return !VerifyPremium(link);
            case ACCOUNT:
                return !VerifyAccount(link);
            case AUTOCAPTCHA:
                return !link.hasAutoCaptcha() && link.hasCaptcha(null);
            case NO_DIRECT_HTTP:
                return link.isDirectHTTP() || link.isFTP();
            }
            break;
        }
        return false;
    }

    private boolean VerifyAccount(CrawledLink link) {
        if (link.isDirectHTTP() || link.isFTP()) {
            return true;
        }
        if (AccountController.getInstance().listAccounts(new AccountFilter().setMaxResultsNum(1).setEnabled(true).setValid(true).setTemporarilyDisabled(false).setHosts(link.getHost())).size() > 0) {
            return true;
        }
        if (AccountController.getInstance().listAccounts(new AccountFilter().setMaxResultsNum(1).setEnabled(true).setValid(true).setTemporarilyDisabled(false).setMultiHostSupported(link.getHost())).size() > 0) {
            return true;
        }
        return false;
    }

    /**
     * Verify if there is at least one premium account valid
     *
     * @param link
     *            Link that we want to werify
     * @return true if a premium account is associated, false otherwise.
     */
    private boolean VerifyPremium(CrawledLink link) {
        if (link.isDirectHTTP() || link.isFTP()) {
            return true;
        }
        if (AccountController.getInstance().listAccounts(new AccountFilter().setMaxResultsNum(1).setEnabled(true).setAccountTypes(AccountType.PREMIUM, AccountType.LIFETIME).setValid(true).setExpired(false).setTemporarilyDisabled(false).setHosts(link.getHost())).size() > 0) {
            return true;
        }
        if (AccountController.getInstance().listAccounts(new AccountFilter().setMaxResultsNum(1).setEnabled(true).setAccountTypes(AccountType.PREMIUM, AccountType.LIFETIME).setValid(true).setExpired(false).setTemporarilyDisabled(false).setMultiHostSupported(link.getHost())).size() > 0) {
            return true;
        }
        return false;
    }
}
