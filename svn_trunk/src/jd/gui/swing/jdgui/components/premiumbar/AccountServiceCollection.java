package jd.gui.swing.jdgui.components.premiumbar;

import java.util.HashSet;

import javax.swing.Icon;
import javax.swing.JComponent;

import org.appwork.swing.components.ExtMergedIcon;
import org.appwork.swing.components.tooltips.ExtTooltip;
import org.jdownloader.DomainInfo;
import org.jdownloader.gui.IconKey;
import org.jdownloader.images.AbstractIcon;

import jd.plugins.Account;

public class AccountServiceCollection extends ServiceCollection<Account> {
    /**
     *
     */
    private static final long serialVersionUID            = -6958497120849521678L;
    private DomainInfo        domainInfo;
    private boolean           enabled;
    private HashSet<Account>  hashSet;
    private Boolean           account_type_multi          = null;
    private Boolean           account_type_captcha_solver = null;
    private long              lastValidTimeStamp          = -1;
    private int               invalid;
    private boolean           inuse;
    private volatile Icon     icon                        = null;

    public DomainInfo getDomainInfo() {
        return domainInfo;
    }

    protected boolean isInUse() {
        return inuse;
    }

    public AccountServiceCollection(DomainInfo domainInfo) {
        this.domainInfo = domainInfo;
        enabled = false;
        inuse = false;
        invalid = 0;
        hashSet = new HashSet<Account>();
    }

    public JComponent createIconComponent(ServicePanel servicePanel) {
        return new TinyProgressBar(servicePanel, this);
    }

    public boolean isEnabled() {
        return enabled && org.jdownloader.settings.staticreferences.CFG_GENERAL.USE_AVAILABLE_ACCOUNTS.isEnabled();
    }

    @Override
    public boolean add(Account acc) {
        if (!hashSet.add(acc)) {
            return false;
        }
        if (account_type_multi == null && acc.isMultiHost()) {
            account_type_multi = acc.isMultiHost();
        } else if (account_type_captcha_solver == null && acc.isCaptchaSolverPlugin()) {
            account_type_captcha_solver = true;
        }
        if (acc.isEnabled()) {
            enabled = true;
        }
        if (!acc.isValid()) {
            invalid++;
        } else if (acc.isEnabled()) {
            inuse = true;
        }
        lastValidTimeStamp = Math.max(acc.getLastValidTimestamp(), lastValidTimeStamp);
        final boolean ret = super.add(acc);
        icon = null;
        return ret;
    }

    public boolean isMulti() {
        if (size() == 1) {
            return get(0).isMultiHost();
        }
        return Boolean.TRUE.equals(account_type_multi);
    }

    public boolean isCaptchaSolver() {
        if (size() == 1) {
            return get(0).isCaptchaSolverPlugin();
        }
        return Boolean.TRUE.equals(account_type_captcha_solver);
    }

    protected void disableMulti() {
        this.account_type_multi = false;
    }

    @Override
    public Icon getIcon() {
        if (icon == null) {
            if (!inuse && invalid > 0) {
                icon = new ExtMergedIcon(domainInfo, 0, 0).add(new AbstractIcon(IconKey.ICON_ERROR, 12), 6, 6);
            } else if (invalid > 0) {
                icon = new ExtMergedIcon(domainInfo, 0, 0).add(new AbstractIcon(IconKey.ICON_WARNING, 12), 6, 6);
            } else {
                icon = domainInfo.getFavIcon();
            }
        }
        return icon;
    }

    @Override
    protected long getLastActiveTimestamp() {
        return lastValidTimeStamp;
    }

    @Override
    protected String getName() {
        return domainInfo.getTld();
    }

    @Override
    public ExtTooltip createTooltip(ServicePanel owner) {
        return new AccountTooltip(owner, this);
    }

    public int getInvalidCount() {
        return invalid;
    }
}
