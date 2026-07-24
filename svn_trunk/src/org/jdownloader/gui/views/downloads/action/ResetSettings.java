package org.jdownloader.gui.views.downloads.action;

import org.appwork.storage.config.annotations.LabelInterface;
import org.jdownloader.controlling.FileCreationManager.DeleteOption;

public class ResetSettings {
    private DeleteOption                             deleteMode           = DeleteOption.NULL;
    private boolean                                  includeDisabledLinks = true;
    private ResetDownloadlinkActionConfirmDialogMode confirmDialogMode    = ResetDownloadlinkActionConfirmDialogMode.ALWAYS;

    public static enum ResetDownloadlinkActionConfirmDialogMode implements LabelInterface {
        ALWAYS {
            @Override
            public String getLabel() {
                return "Always";
            }
        },
        IF_FILES_ARE_DELETED {
            @Override
            public String getLabel() {
                return "If at least one file will be deleted";
            }
        },
        NEVER {
            @Override
            public String getLabel() {
                return "Never";
            }
        };
    }

    public ResetSettings setDeleteMode(DeleteOption deleteMode) {
        this.deleteMode = deleteMode;
        return this;
    }

    public ResetSettings setIncludeDisabledLinks(boolean includeDisabledLinks) {
        this.includeDisabledLinks = includeDisabledLinks;
        return this;
    }

    public ResetSettings setConfirmDialogMode(ResetDownloadlinkActionConfirmDialogMode confirmDialogMode) {
        this.confirmDialogMode = confirmDialogMode;
        return this;
    }

    public DeleteOption getDeleteMode() {
        return deleteMode;
    }

    public boolean isIncludeDisabledLinks() {
        return includeDisabledLinks;
    }

    public ResetDownloadlinkActionConfirmDialogMode getConfirmDialogMode() {
        return confirmDialogMode;
    }
}
