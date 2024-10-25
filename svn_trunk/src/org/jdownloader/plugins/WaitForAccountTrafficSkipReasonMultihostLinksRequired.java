package org.jdownloader.plugins;

import javax.swing.Icon;

import org.jdownloader.gui.IconKey;
import org.jdownloader.images.AbstractIcon;

import jd.controlling.packagecontroller.AbstractNode;
import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.DownloadLink;
import jd.plugins.MultiHostHost;

public class WaitForAccountTrafficSkipReasonMultihostLinksRequired implements ConditionalSkipReason, IgnorableConditionalSkipReason {
    private final Account account;
    private final String  host;
    private int           linksMax;
    private final Icon    icon;

    public Icon getIcon() {
        return icon;
    }

    public WaitForAccountTrafficSkipReasonMultihostLinksRequired(final Account account, final String host, final int linksMax) {
        this.account = account;
        this.host = host;
        this.linksMax = linksMax;
        icon = new AbstractIcon(IconKey.ICON_WAIT, 16);
    }

    public Account getAccount() {
        return account;
    }

    @Override
    public boolean canIgnore() {
        return true;
    }

    private final boolean hasEnoughTraffic() {
        final AccountInfo ai = getAccount().getAccountInfo();
        if (ai == null) {
            return true;
        }
        final MultiHostHost mhost = ai.getMultihostSupportedHost(this.host);
        if (mhost == null) {
            /* Host is not supported anymore */
            return true;
        }
        if (!mhost.isUnlimitedLinks() && mhost.getLinksLeft() <= 0) {
            /* Max limits link is reached -> Cannot download */
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
        return getAccount().isEnabled() == false || getAccount().isValid() == false || getAccount().getAccountController() == null || hasEnoughTraffic();
    }

    @Override
    public String getMessage(Object requestor, AbstractNode node) {
        if (requestor instanceof CustomConditionalSkipReasonMessageIcon) {
            return ((CustomConditionalSkipReasonMessageIcon) requestor).getMessage(this, node);
        } else {
            return "Not enough links left: 0/" + this.linksMax;
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
