package org.jdownloader.extensions.eventscripter.sandboxobjects;

import org.jdownloader.extensions.extraction.ArchiveFile;
import org.jdownloader.extensions.extraction.ExtractionStatus;
import org.jdownloader.extensions.extraction.MissingArchiveFile;
import org.jdownloader.extensions.extraction.bindings.crawledlink.CrawledLinkArchiveFile;
import org.jdownloader.extensions.extraction.bindings.downloadlink.DownloadLinkArchiveFile;

public class ArchiveFileSandbox {
    private final ArchiveFile archiveFile;

    public ArchiveFileSandbox(ArchiveFile archiveFile) {
        this.archiveFile = archiveFile;
    }

    public Boolean isComplete() {
        if (archiveFile != null) {
            return archiveFile.isComplete();
        } else {
            return null;
        }
    }

    public String getFilePath() {
        if (archiveFile != null) {
            return archiveFile.getFilePath();
        } else {
            return null;
        }
    }

    public FilePathSandbox getPath() {
        final String filePath = getFilePath();
        if (filePath != null) {
            return ScriptEnvironment.getPath(filePath);
        } else {
            return null;
        }
    }

    public boolean isMissingArchiveFile() {
        return archiveFile instanceof MissingArchiveFile;
    }

    public long getFileSize() {
        if (archiveFile != null) {
            return archiveFile.getFileSize();
        } else {
            return 0;
        }
    }

    public CrawledLinkSandbox[] getCrawledLinks() {
        if (archiveFile instanceof CrawledLinkArchiveFile) {
            final CrawledLinkSandbox[] ret = CrawledLinkSandbox.wrapSandBox(((CrawledLinkArchiveFile) archiveFile).getLinks());
            if (ret.length > 0) {
                return ret;
            }
        }
        return null;
    }

    public DownloadLinkSandBox[] getDownloadLinks() {
        if (archiveFile instanceof DownloadLinkArchiveFile) {
            final DownloadLinkSandBox[] ret = DownloadLinkSandBox.wrapSandBox(((DownloadLinkArchiveFile) archiveFile).getDownloadLinks());
            if (ret.length > 0) {
                return ret;
            }
        }
        return null;
    }

    public boolean exists() {
        return archiveFile != null && archiveFile.exists();
    }

    public String getExtractionStatus() {
        if (archiveFile != null) {
            return archiveFile.getStatus().name();
        }
        return ExtractionStatus.NA.name();
    }

    public boolean exists(boolean ignoreCache) {
        return archiveFile != null && archiveFile.exists(ignoreCache);
    }

    public void invalidateExists() {
        if (archiveFile != null) {
            archiveFile.invalidateExists();
        }
    }

    public String getName() {
        if (archiveFile != null) {
            return archiveFile.getName();
        } else {
            return null;
        }
    }

    public Boolean isPartOfAnArchive() {
        if (archiveFile != null) {
            return archiveFile.isPartOfAnArchive();
        } else {
            return null;
        }
    }

    public String getArchiveID() {
        if (archiveFile != null) {
            return archiveFile.getArchiveID();
        } else {
            return null;
        }
    }
}
