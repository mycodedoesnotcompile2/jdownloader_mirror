package org.jdownloader.extensions.extraction.contextmenu.downloadlist.action;

import java.awt.event.ActionEvent;
import java.io.File;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.appwork.utils.StringUtils;
import org.appwork.utils.os.CrossSystem;
import org.jdownloader.extensions.extraction.Archive;
import org.jdownloader.extensions.extraction.ExtractionExtension;
import org.jdownloader.extensions.extraction.ExtractionInfo;
import org.jdownloader.extensions.extraction.contextmenu.downloadlist.AbstractExtractionContextAction;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.views.SelectionInfo;

/**
 * Opens the directory the selected archives are extracted to. If an archive was already extracted, the directory it was really extracted
 * to is used, otherwise the directory it will be extracted to (without the subfolder, which depends on the archive content).
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

    @Override
    protected void onActionPerformed(final ActionEvent e, final List<Archive> archives, final SelectionInfo<?, ?> selectionInfo) {
        /* Checking the file system must not happen in the EDT */
        final Thread thread = new Thread() {
            @Override
            public void run() {
                final Set<File> directories = new LinkedHashSet<File>();
                for (final Archive archive : archives) {
                    final File directory = getDirectory(_getExtension(), archive);
                    if (directory != null) {
                        directories.add(directory);
                    }
                }
                /*
                 * TODO: Reconsider opening several destinations. Currently only ONE directory is opened: if the selected archives are
                 * extracted to different directories, their deepest common parent is opened. An alternative would be to open every
                 * distinct destination (maybe capped, e.g. at 5, to avoid a flood of file manager windows). It was decided against this for
                 * now, because the common parent is predictable and behaves like the action applied to the package. Review this once there
                 * is feedback from users who select archives with different destinations.
                 */
                final File directory = getCommonDirectory(directories);
                if (directories.size() > 1) {
                    /* Log it, so we can see how often this happens (see TODO above) */
                    _getExtension().getLogger().info("Open extraction directory: selection contains " + archives.size() + " archive(s) with " + directories.size() + " different directories: " + directories + " -> opening common directory: " + directory);
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
     * Only one directory is opened. If the archives are extracted to different directories, this is the deepest directory that contains all
     * of them.
     *
     * @return the directory to open or null if there is none (e.g. directories on different drives have no common directory)
     */
    private static File getCommonDirectory(final Set<File> directories) {
        if (directories.isEmpty()) {
            return null;
        }
        File common = null;
        for (final File directory : directories) {
            if (common == null) {
                common = directory.getAbsoluteFile();
                continue;
            }
            final File other = directory.getAbsoluteFile();
            /* Walk up from the current candidate until it contains the other directory */
            while (common != null && !isSameOrParentOf(common, other)) {
                common = common.getParentFile();
            }
            if (common == null) {
                return null;
            }
        }
        return common;
    }

    private static boolean isSameOrParentOf(final File parent, File child) {
        while (child != null) {
            if (parent.equals(child)) {
                return true;
            }
            child = child.getParentFile();
        }
        return false;
    }

    /**
     * @return the existing directory to open or null. If the extraction directory does not exist (yet), its parent is used.
     */
    private static File getDirectory(final ExtractionExtension extension, final Archive archive) {
        final ExtractionInfo extractionInfo = archive.getSettings().getExtractionInfo();
        if (extractionInfo != null && !StringUtils.isEmpty(extractionInfo.getExtractToFolder())) {
            final File extracted = new File(extractionInfo.getExtractToFolder());
            if (extracted.exists()) {
                return extracted;
            }
        }
        File directory = extension.getFinalExtractToFolder(archive, false, false);
        if (directory == null) {
            final String defaultFolder = archive.getFactory().createDefaultExtractToPath(archive);
            if (StringUtils.isEmpty(defaultFolder)) {
                return null;
            }
            directory = new File(defaultFolder);
        }
        if (directory.exists()) {
            return directory;
        }
        directory = directory.getParentFile();
        if (directory != null && directory.exists()) {
            return directory;
        }
        return null;
    }
}
