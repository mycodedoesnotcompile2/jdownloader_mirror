package org.jdownloader.plugins;

import javax.swing.Icon;

import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.jdownloader.gui.IconKey;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.translate._JDT;

import jd.controlling.packagecontroller.AbstractNode;
import jd.nutils.Formatter;
import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.DownloadLink;
import jd.plugins.MultiHostHost;

public class WaitingSkipReasonMultihostHostUnavailable implements ConditionalSkipReason, IgnorableConditionalSkipReason, TimeOutCondition {
    private final Account account;
    private final String  host;
    private final String  unavailableReason;
    private final long    unavailableTimestamp;
    private final Icon    icon;

    public Icon getIcon() {
        return icon;
    }

    public WaitingSkipReasonMultihostHostUnavailable(final Account account, final String host, final String unavailableReason, final long unavailableTimestamp) {
        this.account = account;
        this.host = host;
        this.unavailableReason = unavailableReason;
        this.unavailableTimestamp = unavailableTimestamp;
        icon = new AbstractIcon(IconKey.ICON_WAIT, 16);
    }

    public Account getAccount() {
        return account;
    }

    @Override
    public boolean canIgnore() {
        return false;
    }

    /** Returns true if the timeout which was initially coming from a {@link MultiHostHost} item is gone. */
    private final boolean isTimeoutGone() {
        final AccountInfo ai = getAccount().getAccountInfo();
        if (ai == null) {
            return true;
        }
        final MultiHostHost mhost = ai.getMultihostSupportedHost(this.host);
        if (mhost == null) {
            /* Host is not supported by multihost anymore */
            return true;
        }
        final long unavailableTimestamp = mhost.getUnavailableUntilTimestamp();
        if (unavailableTimestamp == this.unavailableTimestamp && StringUtils.equals(mhost.getUnavailableStatusText(), this.unavailableReason)) {
            /* Host is still unavailable for the same reason and with the same unavailable timestamp */
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "WaitForAccountMultiHostHostTrafficSkipReason(Account:" + getAccount() + "|" + getMessage(this, null) + ")";
    }

    @Override
    public boolean isConditionReached() {
        return getAccount().isEnabled() == false || getAccount().isValid() == false || getAccount().getAccountController() == null || getTimeOutLeft() == 0 || isTimeoutGone();
    }

    @Override
    public String getMessage(Object requestor, AbstractNode node) {
        if (requestor instanceof CustomConditionalSkipReasonMessageIcon) {
            return ((CustomConditionalSkipReasonMessageIcon) requestor).getMessage(this, node);
        } else {
            return _JDT.T.gui_download_waittime_notenoughtraffic_multihost_host_temporarily_unavailable(this.getAccount().getHoster(), Formatter.formatSeconds(getTimeOutLeft() / 1000), this.unavailableReason);
        }
    }

    @Override
    public Icon getIcon(Object requestor, AbstractNode node) {
        if (requestor instanceof CustomConditionalSkipReasonMessageIcon) {
            return ((CustomConditionalSkipReasonMessageIcon) requestor).getIcon(this, node);
        } else {
            return getIcon();
        }
    }

    @Override
    public void finalize(DownloadLink link) {
    }

    @Override
    public long getTimeOutTimeStamp() {
        return this.unavailableTimestamp;
    }

    public long getTimeOutLeft() {
        return Math.max(0, getTimeOutTimeStamp() - Time.systemIndependentCurrentJVMTimeMillis());
    }
}
