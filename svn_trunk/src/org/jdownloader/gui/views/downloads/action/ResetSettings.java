package org.jdownloader.gui.views.downloads.action;

import org.jdownloader.controlling.FileCreationManager.DeleteOption;

public class ResetSettings {
    private DeleteOption deleteMode           = DeleteOption.NULL;
    private boolean      includeDisabledLinks = true;

    public ResetSettings setDeleteMode(DeleteOption deleteMode) {
        this.deleteMode = deleteMode;
        return this;
    }

    public ResetSettings setIncludeDisabledLinks(boolean includeDisabledLinks) {
        this.includeDisabledLinks = includeDisabledLinks;
        return this;
    }

    public DeleteOption getDeleteMode() {
        return deleteMode;
    }

    public boolean isIncludeDisabledLinks() {
        return includeDisabledLinks;
    }
}
