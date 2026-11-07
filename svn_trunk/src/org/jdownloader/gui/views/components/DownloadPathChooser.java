package org.jdownloader.gui.views.components;

import javax.swing.Icon;

import org.appwork.swing.components.pathchooser.PathChooser;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.packagehistorycontroller.DownloadPathHistoryManager;
import org.jdownloader.images.NewTheme;

/**
 * {@link PathChooser} whose quick selection is based on the {@link DownloadPathHistoryManager}: Entries of the download destination history
 * can be removed from the popup (remove icon on the highlighted entry, DEL key).
 */
public class DownloadPathChooser extends PathChooser {
    private static final long serialVersionUID = 1L;

    public DownloadPathChooser(final String id, final boolean useQuickList) {
        super(id, useQuickList);
    }

    @Override
    protected boolean isQuickSelectionEntryRemovable(final String path) {
        /* Quick selection may also contain e.g. the default download folder which is not part of the history */
        return DownloadPathHistoryManager.getInstance().contains(path);
    }

    @Override
    protected void onQuickSelectionEntryRemoved(final String path) {
        DownloadPathHistoryManager.getInstance().remove(path);
    }

    @Override
    protected Icon getQuickSelectionIcon(final String path) {
        /* Clock icon marks the entries as history */
        return NewTheme.I().getIcon(IconKey.ICON_WAIT, 14);
    }

    @Override
    protected Icon getQuickSelectionRemoveIcon(final Icon defaultIcon) {
        return NewTheme.I().getIcon(IconKey.ICON_CLOSE, 10);
    }
}
