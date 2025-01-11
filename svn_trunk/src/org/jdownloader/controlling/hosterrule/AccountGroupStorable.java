package org.jdownloader.controlling.hosterrule;

import java.util.ArrayList;
import java.util.List;

import org.appwork.storage.Storable;
import org.appwork.storage.StorableAllowPrivateAccessModifier;
import org.jdownloader.controlling.hosterrule.AccountGroup.Rules;

import jd.plugins.Account;

public class AccountGroupStorable implements Storable {
    private ArrayList<AccountReferenceStorable> children;
    private Rules                               rule = Rules.RANDOM;
    private String                              name;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getRule() {
        return rule.name();
    }

    public void setRule(String rule) {
        try {
            this.rule = Rules.valueOf(rule);
        } catch (final Throwable e) {
            this.rule = Rules.ORDER;
        }
    }

    @StorableAllowPrivateAccessModifier
    private AccountGroupStorable(/* Storable */) {
    }

    public AccountGroupStorable(AccountGroup ag) {
        rule = ag.getRule();
        children = new ArrayList<AccountReferenceStorable>();
        name = ag.getName();
        for (AccountReference acc : ag.getChildren()) {
            children.add(new AccountReferenceStorable(acc));
        }
    }

    public ArrayList<AccountReferenceStorable> getChildren() {
        return children;
    }

    public void setChildren(ArrayList<AccountReferenceStorable> children) {
        this.children = children;
    }

    public AccountGroup restore(String hoster, List<Account> availableAccounts) {
        if (availableAccounts == null) {
            return null;
        }
        final ArrayList<AccountReference> childsP = new ArrayList<AccountReference>(children.size());
        nextChild: for (AccountReferenceStorable ars : children) {
            final AccountReference restored = ars.restore(hoster, availableAccounts);
            if (restored == null) {
                continue;
            }
            for (AccountReference child : childsP) {
                if (child.getID() == restored.getID()) {
                    /* Avoid duplicates. */
                    continue nextChild;
                }
            }
            childsP.add(restored);
        }
        if (childsP.size() == 0) {
            return null;
        }
        AccountGroup ret = new AccountGroup(childsP);
        ret.setRule(rule);
        ret.setName(getName());
        return ret;
    }
}
