package org.jdownloader.extensions.extraction.contextmenu.downloadlist.action;

import java.awt.event.ActionEvent;
import java.io.File;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import jd.controlling.packagecontroller.AbstractNode;

import org.appwork.utils.StringUtils;
import org.appwork.utils.os.CrossSystem;
import org.jdownloader.extensions.extraction.Archive;
import org.jdownloader.extensions.extraction.ExtractionExtension;
import org.jdownloader.extensions.extraction.ExtractionInfo;
import org.jdownloader.extensions.extraction.contextmenu.downloadlist.AbstractExtractionContextAction;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.ExtractToResolver;
import org.jdownloader.gui.views.components.packagetable.LinkTreeUtils;

/**
 * Opens the extraction directory of the context node, like "open download directory": It always refers to the last selected package or
 * link (not to the whole selection).<br>
 * If the archive was already extracted, the directory it was really extracted to is used, otherwise the directory it will be extracted to
 * (without the subfolder, which depends on the archive content). If the archives of a selected package are extracted to different
 * directories, the download directory of the package is opened.
 */
public class OpenExtractionDirectoryAction extends AbstractExtractionContextAction {
    public OpenExtractionDirectoryAction() {
        super();
        setName(org.jdownloader.extensions.extraction.translate.T.T.contextmenu_open_extraction_directory());
        setIconKey(IconKey.ICON_FOLDER_OPEN);
    }

    @Override
    protected void onAsyncInitDone(final List<Archive> archives, final SelectionInfo<?, ?> selectionInfo) {
        super.onAsyncInitDone(archives, selectionInfo);
        if (!CrossSystem.isOpenFileSupported()) {
            setEnabled(false);
        }
    }

    /**
     * @return the last selected package or link, or null if there is no selection
     */
    private static AbstractNode getContextNode(final SelectionInfo<?, ?> selectionInfo) {
        if (selectionInfo == null || selectionInfo.isEmpty()) {
            return null;
        }
        final AbstractNode context = selectionInfo.getRawContext();
        if (context != null) {
            return context;
        }
        return selectionInfo.getFirstPackage();
    }

    @Override
    protected void onActionPerformed(final ActionEvent e, final List<Archive> archives, final SelectionInfo<?, ?> selectionInfo) {
        final AbstractNode context = getContextNode(selectionInfo);
        if (context == null) {
            return;
        }
        /* Building the archives and checking the file system must not happen in the EDT */
        final Thread thread = new Thread() {
            @Override
            public void run() {
                final ExtractionExtension extension = _getExtension();
                final Set<File> directories = new LinkedHashSet<File>();
                for (final Archive archive : ExtractToResolver.getArchives(extension, context)) {
                    final File directory = getDirectory(extension, archive);
                    if (directory != null) {
                        directories.add(directory);
                    }
                }
                final File directory;
                if (directories.size() > 1) {
                    /*
                     * TODO: Reconsider opening several destinations. Currently only ONE directory is opened: if the archives of the package
                     * are extracted to different directories, the download directory of the package is opened (as "open download
                     * directory" would do). An alternative would be to open every distinct destination (maybe capped, e.g. at 5, to avoid
                     * a flood of file manager windows). It was decided against this for now, because the package directory is predictable.
                     * Review this once there is feedback from users who have packages with archives with different destinations.
                     */
                    directory = LinkTreeUtils.getDownloadDirectory(context);
                    /* Log it, so we can see how often this happens (see TODO above) */
                    extension.getLogger().info("Open extraction directory: " + context + " has archives with " + directories.size() + " different directories: " + directories + " -> opening download directory of the package: " + directory);
                } else if (directories.size() == 1) {
                    directory = directories.iterator().next();
                } else {
                    directory = null;
                }
                if (directory != null) {
                    CrossSystem.openFile(directory);
                }
            }
        };
        thread.setName("Extract Context: open extraction directory");
        thread.setDaemon(true);
        thread.start();
    }

    /**
     * The existence of the directory is not checked.
     *
     * @return the directory the archive was extracted to or will be extracted to, or null
     */
    private static File getDirectory(final ExtractionExtension extension, final Archive archive) {
        final ExtractionInfo extractionInfo = archive.getSettings().getExtractionInfo();
        if (extractionInfo != null && !StringUtils.isEmpty(extractionInfo.getExtractToFolder())) {
            return new File(extractionInfo.getExtractToFolder());
        }
        final File directory = extension.getFinalExtractToFolder(archive, false, false);
        if (directory != null) {
            return directory;
        }
        final String defaultFolder = archive.getFactory().createDefaultExtractToPath(archive);
        if (StringUtils.isEmpty(defaultFolder)) {
            return null;
        }
        return new File(defaultFolder);
    }
}
