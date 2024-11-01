package org.jdownloader.controlling.hosterrule;

import java.util.Date;

import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.AccountTrafficView;

import org.appwork.exceptions.WTFException;
import org.jdownloader.DomainInfo;

public class FreeAccountReference extends AccountReference {
    static final int     FREE_ID = 0;
    private final String hoster;

    public FreeAccountReference(String hoster) {
        this.hoster = hoster;
    }

    @Override
    public Account getAccount() {
        throw new WTFException("Not implemented");
    }

    @Override
    public AccountTrafficView getAccountTrafficView() {
        return null;
    }

    @Override
    protected String getRef(long refID) {
        return null;
    }

    public Date getExpireDate() {
        return null;
    }

    public DomainInfo getDomainInfo() {
        return DomainInfo.getInstance(getHoster());
    }

    public boolean isAvailable() {
        return true;
    }

    public AccountInfo getAccountInfo() {
        return null;
    }

    public boolean isValid() {
        return true;
    }

    public boolean isTempDisabled() {
        return false;
    }

    public long getTmpDisabledTimeout() {
        return -1;
    }

    @Override
    public long getID() {
        return FREE_ID;
    }

    @Override
    public String getHoster() {
        return hoster;
    }

    @Override
    public String getUser() {
        return "";
    }

    public String toString() {
        return "Account: " + getHoster();
    }

    public static boolean isFreeAccount(AccountReference ar) {
        if (ar == null) {
            return false;
        }
        if (ar instanceof FreeAccountReference) {
            return true;
        }
        if (ar.getAccount() != null && ar.getID() == FREE_ID) {
            return true;
        }
        return false;
    }
}
