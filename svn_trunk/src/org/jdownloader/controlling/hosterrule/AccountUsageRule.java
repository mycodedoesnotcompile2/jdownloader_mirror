package org.jdownloader.controlling.hosterrule;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import org.jdownloader.DomainInfo;

public class AccountUsageRule {

    private boolean              enabled;
    private String               hoster;
    private List<AccountGroup>   accounts;
    private HosterRuleController owner;
    private DomainInfo           domainInfo = null;

    public AccountUsageRule(String tld) {
        setHoster(tld);
        accounts = new CopyOnWriteArrayList<AccountGroup>();
    }

    public void setHoster(String hoster) {
        this.hoster = hoster;
        this.domainInfo = null;
    }

    public DomainInfo getDomainInfo() {
        DomainInfo ret = this.domainInfo;
        if (ret != null) {
            return ret;
        }
        return this.domainInfo = ret = DomainInfo.getInstance(getHoster());
    }

    public void setEnabled(boolean enabled) {
        if (this.enabled == enabled) {
            return;
        }
        this.enabled = enabled;
        final HosterRuleController lowner = owner;
        if (lowner != null) {
            lowner.fireUpdate(this);
        }
    }

    public boolean isEnabled() {
        return enabled;
    }

    public String getHoster() {
        return hoster;
    }

    public List<AccountGroup> getAccounts() {
        return accounts;
    }

    public void setOwner(HosterRuleController controller) {
        owner = controller;
    }

    public void set(boolean enabledState, List<AccountGroup> list) {
        if (list == null || list.isEmpty()) {
            list = new ArrayList<AccountGroup>(1);
            enabledState = false;
        }
        this.enabled = enabledState;
        this.accounts = new CopyOnWriteArrayList<AccountGroup>(list);
        final HosterRuleController lowner = owner;
        if (lowner != null) {
            lowner.fireUpdate(this);
        }
    }
}
