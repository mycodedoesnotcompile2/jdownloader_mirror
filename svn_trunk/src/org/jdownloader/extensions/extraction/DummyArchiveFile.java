package org.jdownloader.extensions.extraction;

import java.io.File;

import jd.plugins.DownloadLink.AvailableStatus;

import org.jdownloader.extensions.extraction.bindings.crawledlink.CrawledLinkArchiveFile;
import org.jdownloader.extensions.extraction.bindings.downloadlink.DownloadLinkArchiveFile;
import org.jdownloader.extensions.extraction.bindings.file.FileArchiveFile;

public class DummyArchiveFile {
    private final String      name;
    private final ArchiveFile archiveFile;

    public ArchiveFile getArchiveFile() {
        return archiveFile;
    }

    public boolean exists() {
        final ArchiveFile archiveFile = getArchiveFile();
        return archiveFile != null && archiveFile.exists();
    }

    public Boolean isIncomplete() {
        final ArchiveFile archiveFile = getArchiveFile();
        if (archiveFile == null) {
            return Boolean.TRUE;
        } else {
            final Boolean complete = archiveFile.isComplete();
            if (complete == null) {
                return null;
            }
            return complete.booleanValue() ? Boolean.FALSE : Boolean.TRUE;
        }
    }

    public boolean isMissing() {
        final ArchiveFile archiveFile = getArchiveFile();
        return archiveFile == null || archiveFile instanceof MissingArchiveFile;
    }

    public DummyArchiveFile(String miss, File folder) {
        name = miss;
        this.archiveFile = null;
    }

    public String toString() {
        final ArchiveFile archiveFile = getArchiveFile();
        if (archiveFile != null) {
            return archiveFile.toString();
        } else {
            return name;
        }
    }

    public DummyArchiveFile(ArchiveFile af) {
        name = af.getName();
        archiveFile = af;
    }

    public String getName() {
        return name;
    }

    public AvailableStatus getOnlineStatus() {
        final ArchiveFile archiveFile = getArchiveFile();
        if (archiveFile != null) {
            if (archiveFile instanceof CrawledLinkArchiveFile) {
                return ((CrawledLinkArchiveFile) archiveFile).getAvailableStatus();
            } else if (archiveFile instanceof DownloadLinkArchiveFile) {
                return ((DownloadLinkArchiveFile) archiveFile).getAvailableStatus();
            } else if (archiveFile instanceof FileArchiveFile) {
                if (((FileArchiveFile) archiveFile).exists()) {
                    return AvailableStatus.TRUE;
                } else {
                    return AvailableStatus.FALSE;
                }
            }
        }
        return AvailableStatus.UNCHECKED;
    }

    public boolean isLocalFileAvailable() {
        final ArchiveFile archiveFile = getArchiveFile();
        return archiveFile != null && archiveFile.exists();
    }
}
