package org.jdownloader.gui.views.downloads.action;

import java.awt.event.ActionEvent;
import java.util.List;

import org.jdownloader.controlling.FileCreationManager.DeleteOption;
import org.jdownloader.controlling.contextmenu.ActionContext;
import org.jdownloader.controlling.contextmenu.CustomizableTableContextAppAction;
import org.jdownloader.controlling.contextmenu.Customizer;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.downloads.action.ResetSettings.ResetDownloadlinkActionConfirmDialogMode;
import org.jdownloader.plugins.config.Order;

import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

public class ResetAction extends CustomizableTableContextAppAction<FilePackage, DownloadLink> implements ActionContext {
    private static final long                        serialVersionUID = -5583373118359478729L;
    private final static String                      NAME             = _GUI.T.gui_table_contextmenu_reset();
    private static final ResetSettings                DEFAULT_SETTINGS = new ResetSettings();
    private DeleteOption                              deleteMode           = DEFAULT_SETTINGS.getDeleteMode();
    private boolean                                   includeDisabledLinks = DEFAULT_SETTINGS.isIncludeDisabledLinks();
    private ResetDownloadlinkActionConfirmDialogMode  confirmDialogMode    = DEFAULT_SETTINGS.getConfirmDialogMode();

    public ResetAction() {
        setIconKey(IconKey.ICON_UNDO);
        setName(NAME);
    }

    public static String getTranslationForDeleteMode() {
        return "Delete mode";
    }

    public static String getTranslationForIncludeDisabledLinks() {
        return "Include disabled links";
    }

    public static String getTranslationForConfirmDialogMode() {
        return "Show confirmation dialog";
    }

    @Customizer(link = "#getTranslationForDeleteMode")
    @Order(10)
    public DeleteOption getDeleteMode() {
        return deleteMode;
    }

    public void setDeleteMode(DeleteOption mode) {
        this.deleteMode = mode;
    }

    @Customizer(link = "#getTranslationForIncludeDisabledLinks")
    @Order(20)
    public boolean isIncludeDisabledLinks() {
        return includeDisabledLinks;
    }

    public void setIncludeDisabledLinks(boolean includeDisabledLinks) {
        this.includeDisabledLinks = includeDisabledLinks;
    }

    @Customizer(link = "#getTranslationForConfirmDialogMode")
    @Order(30)
    public ResetDownloadlinkActionConfirmDialogMode getConfirmDialogMode() {
        return confirmDialogMode;
    }

    public void setConfirmDialogMode(ResetDownloadlinkActionConfirmDialogMode mode) {
        this.confirmDialogMode = mode;
    }

    @Override
    public boolean isEnabled() {
        if (!super.isEnabled()) {
            return false;
        }
        if (isIncludeDisabledLinks()) {
            return true;
        }
        return DownloadTabActionUtils.containsEnabledLink(getSelection().getChildren());
    }

    public void actionPerformed(ActionEvent e) {
        if (!isEnabled()) {
            return;
        }
        final SelectionInfo<FilePackage, DownloadLink> rawSelection = getSelection();
        final List<DownloadLink> selection = rawSelection.getChildren();
        final ResetSettings settings = new ResetSettings();
        settings.setDeleteMode(getDeleteMode());
        settings.setIncludeDisabledLinks(isIncludeDisabledLinks());
        settings.setConfirmDialogMode(getConfirmDialogMode());
        DownloadTabActionUtils.resetLinksRequest(selection, settings);
    }
}
