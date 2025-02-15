package jd.plugins.components;

import jd.config.Property;
import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.MultiHostHost;
import jd.plugins.Plugin;
import jd.plugins.PluginException;

import org.appwork.exceptions.WTFException;

/**
 * Instead of duplication we create a class
 *
 * This class will handle disabling of a host at 3 levels 1) multihoster wide, 2) AccountType 3) Account
 *
 * @author raztoki
 *
 */
public class MultiHosterManagement {
    private final String host;

    public MultiHosterManagement(final String host) {
        this.host = host;
    }

    protected String getErrorProperty() {
        return getHost().replaceAll("https?://|\\.|\\-", "") + "_failedtimes_";
    }

    private String getHost() {
        final Plugin plugin = Plugin.getCurrentActivePlugin();
        final String host = plugin != null ? plugin.getHost() : this.host;
        if (host == null) {
            throw new WTFException();
        } else {
            return host;
        }
    }

    public void putError(final Object account, final DownloadLink downloadLink, final Long timeout, final String reason) throws PluginException {
        setLimitOnAccount: if (account != null && account instanceof Account) {
            /* Set limit on account so it is aware of current state of that host. */
            final Account acc = (Account) account;
            final AccountInfo ai = acc.getAccountInfo();
            if (ai == null) {
                break setLimitOnAccount;
            }
            final MultiHostHost mhost = ai.getMultihostSupportedHost(downloadLink.getHost());
            if (mhost == null) {
                /* Host might have been removed from list of supported hosts in the meantime. */
                break setLimitOnAccount;
            }
            mhost.setErrorStatus(reason, timeout);
            ai.updateMultihostSupportedHost(mhost);
        }
        throw new PluginException(LinkStatus.ERROR_RETRY, reason);
    }

    /**
     * pushes into {@link #handleErrorGeneric(Account, DownloadLink, String, int, long)}, with default long of 1 hour.
     *
     * @param account
     * @param downloadLink
     * @param error
     * @param maxRetries
     * @throws PluginException
     * @throws InterruptedException
     */
    public void handleErrorGeneric(final Account account, final DownloadLink downloadLink, final String error, final int maxRetries) throws PluginException, InterruptedException {
        this.handleErrorGeneric(account, downloadLink, error, maxRetries, 1 * 60 * 60 * 1000l);
    }

    /**
     * Intended to handle errors which warrant a series of retries before 'putError' called.
     *
     * @param downloadLink
     *            : DownloadLink
     * @param error
     *            : name of the error
     * @param maxRetries
     *            : Max retries
     * @throws InterruptedException
     */
    public void handleErrorGeneric(final Account account, final DownloadLink downloadLink, final String error, final int maxRetries, final long errorWait) throws PluginException, InterruptedException {
        if (downloadLink == null) {
            /* 2019-07-23: Just set account to invalid if called without DownloadLink. */
            throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_TEMP_DISABLE);
        }
        final String errorID = getErrorProperty() + error;
        int timesFailed = downloadLink.getIntegerProperty(errorID, 0);
        if (timesFailed < maxRetries) {
            timesFailed++;
            downloadLink.setProperty(errorID, timesFailed);
            // we will apply some grace period here since JD2 core does not apply the wait time parameter with the retry exception.
            long waitPeriod = 5 * timesFailed;
            do {
                downloadLink.getLinkStatus().setStatusText("Small wait: " + waitPeriod + " secs");
                Thread.sleep(1000l);
            } while (--waitPeriod > 0);
            downloadLink.getLinkStatus().setStatusText("");
            throw new PluginException(LinkStatus.ERROR_RETRY, "Retry " + timesFailed + "/" + maxRetries + ": " + error);
        } else {
            downloadLink.setProperty(errorID, Property.NULL);
            final String errorPrefix;
            if (maxRetries <= 1) {
                errorPrefix = "";
            } else {
                errorPrefix = "Too many failed attempts: >= " + maxRetries + ": ";
            }
            this.putError(account, downloadLink, errorWait, errorPrefix + error);
        }
    }

}
