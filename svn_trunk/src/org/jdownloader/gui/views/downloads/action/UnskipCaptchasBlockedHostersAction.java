package org.jdownloader.gui.views.downloads.action;

import java.awt.event.ActionEvent;
import java.util.Set;

import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.toolbar.UnskipLinksAction;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;

import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

/**
 * Unskip menu: The captchas of all hosters of the selection whose captchas are blocked as a whole are not skipped any more, in the whole
 * download list. Enabled if the selection contains at least one of those hosters.
 */
public class UnskipCaptchasBlockedHostersAction extends AbstractUnskipAction {
    private static final long serialVersionUID = 1L;

    public UnskipCaptchasBlockedHostersAction() {
        setIconKey(IconKey.ICON_UNDO);
        setName(_GUI.T.UnskipCaptchasBlockedHostersAction());
    }

    private static Set<String> getSelectedBlockedHosts(final SelectionInfo<FilePackage, DownloadLink> selection) {
        final Set<String> hosts = getSelectedHosts(selection);
        hosts.retainAll(getBlockedHosts());
        return hosts;
    }

    @Override
    protected boolean isAvailable(final SelectionInfo<FilePackage, DownloadLink> selection) {
        return getSelectedBlockedHosts(selection).size() > 0;
    }

    @Override
    protected void onActionPerformed(final ActionEvent e, final SelectionType selectionType, final SelectionInfo<FilePackage, DownloadLink> selectionInfo) {
        if (!isEnabled()) {
            return;
        }
        final Set<String> hosts = getSelectedBlockedHosts(selectionInfo);
        if (hosts.isEmpty()) {
            return;
        }
        UnskipLinksAction.unSkipCaptchaHosts(hosts);
    }
}
