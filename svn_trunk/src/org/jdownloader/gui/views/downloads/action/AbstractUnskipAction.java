package org.jdownloader.gui.views.downloads.action;

import java.util.HashSet;
import java.util.Set;

import org.jdownloader.captcha.blacklist.BlacklistEntry;
import org.jdownloader.captcha.blacklist.BlockDownloadCaptchasByHost;
import org.jdownloader.captcha.blacklist.CaptchaBlackList;
import org.jdownloader.controlling.contextmenu.CustomizableTableContextAppAction;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;
import org.jdownloader.plugins.SkipReason;

import jd.controlling.downloadcontroller.DownloadWatchDog;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

/**
 * Base of the actions in the Unskip menu of the download list: The action is always shown for a selection, but only enabled if it can do
 * something for that selection (see {@link #isAvailable(SelectionInfo)}), so the menu looks the same whether the user clicked a package, a
 * link or several of them. While the downloads are not running (or paused), all of them are disabled without any further check.
 */
public abstract class AbstractUnskipAction extends CustomizableTableContextAppAction<FilePackage, DownloadLink> {
    private static final long serialVersionUID = 1L;

    /** True if the action can do something for the selection. */
    protected abstract boolean isAvailable(SelectionInfo<FilePackage, DownloadLink> selection);

    @Override
    protected void onRequestUpdateSelection(final Object requestor, final SelectionType selectionType, final SelectionInfo<FilePackage, DownloadLink> selectionInfo) {
        super.onRequestUpdateSelection(requestor, selectionType, selectionInfo);
        if (!isEnabled()) {
            return;
        }
        /* The skipped captchas are only handled while the downloads are running, so there is nothing to calculate otherwise. */
        if (!DownloadWatchDog.getInstance().isRunningAndNotPaused() || SelectionInfo.isEmpty(selectionInfo)) {
            setEnabled(false);
            return;
        }
        setEnabled(isAvailable(selectionInfo));
    }

    protected static boolean isCaptchaSkipped(final DownloadLink link) {
        return SkipReason.CAPTCHA.equals(link.getSkipReason());
    }

    /** The hosts whose captchas are blocked as a whole. */
    protected static Set<String> getBlockedHosts() {
        final Set<String> ret = new HashSet<String>();
        for (final BlacklistEntry entry : CaptchaBlackList.getInstance().listEntries()) {
            if (entry instanceof BlockDownloadCaptchasByHost) {
                final String host = ((BlockDownloadCaptchasByHost) entry).getBlockedHost();
                if (host != null) {
                    ret.add(host);
                }
            }
        }
        return ret;
    }

    /** The hosts of the selected links, also the ones of selected packages. */
    protected static Set<String> getSelectedHosts(final SelectionInfo<FilePackage, DownloadLink> selection) {
        final Set<String> ret = new HashSet<String>();
        for (final DownloadLink link : selection.getChildren()) {
            ret.add(link.getHost());
        }
        return ret;
    }
}
