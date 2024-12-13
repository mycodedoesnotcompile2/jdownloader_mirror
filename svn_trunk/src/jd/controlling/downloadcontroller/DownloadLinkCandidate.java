package jd.controlling.downloadcontroller;

import java.lang.ref.WeakReference;

import org.jdownloader.controlling.domainrules.DomainRuleController;
import org.jdownloader.controlling.domainrules.DomainRuleSet;

import jd.controlling.downloadcontroller.AccountCache.CachedAccount;
import jd.controlling.proxy.AbstractProxySelectorImpl;
import jd.plugins.DownloadLink;

public class DownloadLinkCandidate {
    private final boolean                     forced;
    private final WeakReference<DownloadLink> link;
    private final CachedAccount               cachedAccount;
    private final AbstractProxySelectorImpl   proxySelector;
    private final boolean                     customizedAccount;
    private DomainRuleSet                     domainRuleSet = null;

    public AbstractProxySelectorImpl getProxySelector() {
        return proxySelector;
    }

    public CachedAccount getCachedAccount() {
        return cachedAccount;
    }

    public DownloadLink getLink() {
        return link == null ? null : link.get();
    }

    public boolean isForced() {
        return forced;
    }

    @Override
    public String toString() {
        final DownloadLink link = getLink();
        return "DownloadCandidate:" + link + "|Host " + (link == null ? null : link.getHost()) + "|Account:" + cachedAccount + "|Proxy:" + proxySelector;
    }

    public DownloadLinkCandidate(DownloadLink link, boolean forced) {
        this(link, forced, null, null, false);
    }

    public DownloadLinkCandidate(DownloadLinkCandidate candidate, CachedAccount cachedAccount, boolean customizedAccount) {
        this(candidate.getLink(), candidate.isForced(), cachedAccount, null, customizedAccount);
    }

    public DownloadLinkCandidate(DownloadLink link, boolean forced, CachedAccount cachedAccount, AbstractProxySelectorImpl proxy, boolean customizedAccount) {
        this.link = new WeakReference<DownloadLink>(link);
        this.forced = forced;
        this.cachedAccount = cachedAccount;
        this.proxySelector = proxy;
        this.customizedAccount = customizedAccount;
    }

    public DownloadLinkCandidate(DownloadLinkCandidate candidate, AbstractProxySelectorImpl proxy) {
        this(candidate.getLink(), candidate.isForced(), candidate.getCachedAccount(), proxy, candidate.isCustomizedAccount());
    }

    public boolean isCustomizedAccount() {
        return customizedAccount;
    }

    public DomainRuleSet getDomainRuleSet() {
        if (domainRuleSet != null) {
            return domainRuleSet;
        }
        final DownloadLink link = getLink();
        final String downloadDomain = link.getDomainInfo().getTld();
        final CachedAccount cachedAccount = getCachedAccount();
        final String pluginDomain = cachedAccount.getHost();
        final String fileName = link.getName();
        domainRuleSet = DomainRuleController.getInstance().createRuleSet(cachedAccount.getAccount(), downloadDomain, pluginDomain, fileName);
        return domainRuleSet;
    }
}