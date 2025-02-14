package org.jdownloader.gui.views.linkgrabber.bottombar;

import java.awt.event.ActionEvent;
import java.io.File;

import org.jdownloader.controlling.contextmenu.CustomizableAppAction;
import org.jdownloader.controlling.packagizer.PackagizerController;
import org.jdownloader.gui.IconKey;
import org.jdownloader.settings.GeneralSettings;

public class CleanupDownloadFolderAction extends CustomizableAppAction {
    public CleanupDownloadFolderAction() {
        super();
        setName("Cleanup download folder: Delete empty folders");
        setIconKey(IconKey.ICON_TRASH);
        /* Hide by default */
        setVisible(false);
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        final Thread thread = new Thread("CleanupDownloadFolderAction") {

            @Override
            public void run() {
                final String path = PackagizerController.replaceDynamicTags(org.appwork.storage.config.JsonConfig.create(GeneralSettings.class).getDefaultDownloadFolder(), null, null);
                final File folder = new File(path);
                if (folder == null || !folder.isDirectory()) {
                    return;
                }
                final File[] files = folder.listFiles();
                if (files == null) {
                    /* Download folder is empty -> Nothing to delete. */
                    return;
                }
                for (final File file : files) {
                    final String[] list;
                    if (file.isDirectory() && (list = file.list()) != null && list.length == 0) {
                        file.delete();
                    }
                }
            }
        };
        thread.setName("CleanupDownloadFolderAction");
        thread.setDaemon(true);
        thread.start();
    }
}
