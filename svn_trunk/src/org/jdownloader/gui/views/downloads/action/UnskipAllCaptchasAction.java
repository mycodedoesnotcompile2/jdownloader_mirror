package org.jdownloader.gui.views.downloads.action;

import java.awt.event.ActionEvent;

import jd.controlling.downloadcontroller.DownloadWatchDog;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

import org.jdownloader.captcha.blacklist.BlacklistEntry;
import org.jdownloader.captcha.blacklist.CaptchaBlackList;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.toolbar.UnskipLinksAction;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;

/**
 * Unskip menu: All captchas are not skipped any more, independent of the selection: Removes all captcha blocks of the downloads and resets all
 * links skipped because of a captcha. Only enabled if captchas are blocked or links are skipped.
 */
public class UnskipAllCaptchasAction extends AbstractUnskipAction {
    private static final long serialVersionUID = 1L;

    public UnskipAllCaptchasAction() {
        setIconKey(IconKey.ICON_UNDO);
        setName(_GUI.T.UnskipAllCaptchasAction());
    }

    @Override
    protected boolean isAvailable(final SelectionInfo<FilePackage, DownloadLink> selection) {
        for (final BlacklistEntry entry : CaptchaBlackList.getInstance().listEntries()) {
            if (UnskipLinksAction.isDownloadEntry(entry)) {
                return true;
            }
        }
        return DownloadWatchDog.getInstance().getSession().getSkipCounter() > 0;
    }

    @Override
    protected void onActionPerformed(final ActionEvent e, final SelectionType selectionType, final SelectionInfo<FilePackage, DownloadLink> selectionInfo) {
        if (!isEnabled()) {
            return;
        }
        UnskipLinksAction.unSkipCaptchas();
    }
}
