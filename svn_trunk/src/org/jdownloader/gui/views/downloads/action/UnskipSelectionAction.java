package org.jdownloader.gui.views.downloads.action;

import java.awt.event.ActionEvent;

import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.toolbar.UnskipLinksAction;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;

/**
 * Unskip menu: Unskips the skipped links of the selection, whatever the reason is, and removes the captcha blocks that apply to them (of the
 * link, its package and its host). Enabled if a selected link is skipped or blocked. See {@link UnskipLinksAction#unSkipLinks}.
 */
public class UnskipSelectionAction extends AbstractUnskipAction {
    private static final long serialVersionUID = 1L;

    public UnskipSelectionAction() {
        setIconKey(IconKey.ICON_SKIPPED);
        setName(_GUI.T.UnskipSelectionAction());
    }

    @Override
    protected boolean isAvailable(final SelectionInfo<FilePackage, DownloadLink> selection) {
        for (final DownloadLink link : selection.getChildren()) {
            if (link.isSkipped()) {
                return true;
            }
        }
        return UnskipLinksAction.isBlocked(selection.getChildren());
    }

    @Override
    protected void onActionPerformed(final ActionEvent e, final SelectionType selectionType, final SelectionInfo<FilePackage, DownloadLink> selectionInfo) {
        if (!isEnabled()) {
            return;
        }
        UnskipLinksAction.unSkipLinks(selectionInfo.getChildren());
    }
}
