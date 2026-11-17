package org.jdownloader.plugins;

import javax.swing.Icon;

import org.appwork.utils.StringUtils;
import org.jdownloader.api.downloads.ChannelCollector;
import org.jdownloader.api.downloads.DownloadControllerEventPublisher;
import org.jdownloader.api.downloads.v2.DownloadsAPIV2Impl;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.views.downloads.columns.ETAColumn;
import org.jdownloader.gui.views.downloads.columns.TaskColumn;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.translate._JDT;

import jd.controlling.downloadcontroller.HistoryEntry;
import jd.controlling.packagecontroller.AbstractNode;
import jd.nutils.Formatter;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackageView;

public class WaitForAccountSkipReason implements ConditionalSkipReason, IgnorableConditionalSkipReason, TimeOutCondition {
    private final Account account;
    private final Icon    icon;

    public Icon getIcon() {
        return icon;
    }

    public WaitForAccountSkipReason(Account account) {
        this.account = account;
        icon = new AbstractIcon(IconKey.ICON_WAIT, 16);
    }

    public Account getAccount() {
        return account;
    }

    @Override
    public long getTimeOutTimeStamp() {
        return getAccount().getTmpDisabledTimeout();
    }

    @Override
    public long getTimeOutLeft() {
        return Math.max(0, getTimeOutTimeStamp() - System.currentTimeMillis());
    }

    @Override
    public boolean canIgnore() {
        return true;
    }

    @Override
    public String toString() {
        return "WaitForAccountSkipReason(Account:" + getAccount() + "|" + getMessage(this, null) + ")";
    }

    @Override
    public boolean isConditionReached() {
        return getAccount().isEnabled() == false || getAccount().isValid() == false || getAccount().getAccountController() == null || getAccount().isTempDisabled() == false || getTimeOutLeft() == 0;
    }

    @Override
    public String getMessage(Object requestor, AbstractNode node) {
        if (requestor instanceof CustomConditionalSkipReasonMessageIcon) {
            return ((CustomConditionalSkipReasonMessageIcon) requestor).getMessage(this, node);
        }
        /* Same requestor handling as WaitingSkipReason: message in the task column, countdown only in the ETA column. */
        final long left = getTimeOutLeft();
        if (left <= 0) {
            return null;
        } else if (requestor == this || requestor instanceof HistoryEntry || requestor instanceof TaskColumn || requestor instanceof DownloadControllerEventPublisher || requestor instanceof ChannelCollector || requestor instanceof DownloadsAPIV2Impl || requestor instanceof FilePackageView) {
            final String accountError = getAccount().getErrorString();
            if (StringUtils.isEmpty(accountError)) {
                return _JDT.T.gui_download_waittime_status2(Formatter.formatSeconds(left / 1000));
            } else {
                return _JDT.T.gui_download_account_problem(accountError);
            }
        } else if (requestor instanceof ETAColumn) {
            return Formatter.formatSeconds(left / 1000);
        } else {
            return null;
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
}
