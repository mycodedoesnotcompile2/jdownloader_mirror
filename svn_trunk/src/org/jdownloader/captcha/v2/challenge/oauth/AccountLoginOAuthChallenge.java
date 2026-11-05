package org.jdownloader.captcha.v2.challenge.oauth;

import org.jdownloader.captcha.v2.solverjob.SolverJob;
import org.jdownloader.translate._JDT;

import jd.plugins.Account;

public abstract class AccountLoginOAuthChallenge extends OAuthChallenge {
    private Account account;

    public Account getAccount() {
        return account;
    }

    public void setAccount(Account account) {
        this.account = account;
    }

    public AccountLoginOAuthChallenge(String method, String explain, Account account, String url) {
        super(method, url, explain == null ? _JDT.T.OAUTH_CHALLENGE_EXPLAIN() : explain);
        this.account = account;
    }

    public abstract boolean autoSolveChallenge(SolverJob<Boolean> job);
}
