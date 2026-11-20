package org.jdownloader.gui.views.downloads.action;

import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.List;

import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

import org.jdownloader.captcha.blacklist.BlacklistEntry;
import org.jdownloader.captcha.blacklist.BlockDownloadCaptchasByPackage;
import org.jdownloader.captcha.blacklist.CaptchaBlackList;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.toolbar.UnskipLinksAction;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;
import org.jdownloader.gui.views.SelectionInfo.PackageView;

/**
 * Unskip menu: The captchas of the packages of the selection (selected packages and packages of selected links) are not skipped any more.
 * Only enabled if one of the packages is blocked or has a link that is skipped because of a captcha. Removes the blocks of the packages and of their links, resets all links of the packages that are skipped
 * because of a captcha (also the ones skipped one by one) and removes the blocks of the hosts of these links.
 */
public class UnskipCaptchasPackageAction extends AbstractUnskipAction {
    private static final long serialVersionUID = 1L;

    public UnskipCaptchasPackageAction() {
        setIconKey(IconKey.ICON_UNDO);
        setName(_GUI.T.UnskipCaptchasPackageAction());
    }

    private static List<FilePackage> getPackages(final SelectionInfo<FilePackage, DownloadLink> selection) {
        final List<FilePackage> ret = new ArrayList<FilePackage>();
        for (final PackageView<FilePackage, DownloadLink> view : selection.getPackageViews()) {
            ret.add(view.getPackage());
        }
        return ret;
    }

    @Override
    protected boolean isAvailable(final SelectionInfo<FilePackage, DownloadLink> selection) {
        final List<FilePackage> packages = getPackages(selection);
        for (final BlacklistEntry entry : CaptchaBlackList.getInstance().listEntries()) {
            if (entry instanceof BlockDownloadCaptchasByPackage && packages.contains(((BlockDownloadCaptchasByPackage) entry).getFilePackage())) {
                return true;
            }
        }
        for (final FilePackage pkg : packages) {
            /* All links of the package, not only the selected ones. */
            final boolean readL = pkg.getModifyLock().readLock();
            try {
                for (final DownloadLink link : pkg.getChildren()) {
                    if (isCaptchaSkipped(link)) {
                        return true;
                    }
                }
            } finally {
                pkg.getModifyLock().readUnlock(readL);
            }
        }
        return false;
    }

    @Override
    protected void onActionPerformed(final ActionEvent e, final SelectionType selectionType, final SelectionInfo<FilePackage, DownloadLink> selectionInfo) {
        if (!isEnabled()) {
            return;
        }
        UnskipLinksAction.unSkipCaptchaPackages(getPackages(selectionInfo));
    }
}
