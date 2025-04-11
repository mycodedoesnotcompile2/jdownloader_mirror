package org.jdownloader.plugins;

import javax.swing.Icon;

import org.jdownloader.gui.IconKey;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.translate._JDT;

import jd.controlling.packagecontroller.AbstractNode;
import jd.nutils.Formatter;
import jd.plugins.Account;
import jd.plugins.DownloadLink;

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
        } else {
            final long left = getTimeOutLeft();
            return _JDT.T.gui_download_waittime_status2(Formatter.formatSeconds(Math.max(0, left / 1000)));
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
