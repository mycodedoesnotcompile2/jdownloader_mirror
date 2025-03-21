package org.jdownloader.extensions.extraction;

import java.awt.Color;

import jd.plugins.LinkInfo;

import org.appwork.utils.StringUtils;
import org.jdownloader.controlling.FileCreationManager.DeleteOption;

public class MissingArchiveFile implements ArchiveFile {
    private final String  name;
    private final String  filePath;
    private String        archiveID = null;
    private final Archive missingOrIncompleteArchive;

    public Archive getMissingOrIncompleteArchive() {
        return missingOrIncompleteArchive;
    }

    @Override
    public Boolean isComplete() {
        return Boolean.FALSE;
    }

    @Override
    public String getFilePath() {
        return filePath;
    }

    @Override
    public long getFileSize() {
        return -1;
    }

    public MissingArchiveFile(final String name, final String filePath) {
        this.name = name;
        this.filePath = filePath;
        this.missingOrIncompleteArchive = null;
    }

    public MissingArchiveFile(Archive missingArchive, final String filePath) {
        this.name = missingArchive.getName();
        this.missingOrIncompleteArchive = missingArchive;
        this.filePath = filePath;
    }

    @Override
    public LinkInfo getLinkInfo() {
        return LinkInfo.getLinkInfo(getName());
    }

    @Override
    public void deleteFile(DeleteOption option) {
    }

    @Override
    public boolean contains(Object obj) {
        if (obj == this) {
            return true;
        } else if (obj instanceof String) {
            return getFilePath().equals(obj);
        } else if (obj instanceof ArchiveFile && StringUtils.equals(getFilePath(), ((ArchiveFile) obj).getFilePath())) {
            return true;
        } else if (obj instanceof MissingArchiveFile) {
            return StringUtils.equals(getName(), ((MissingArchiveFile) obj).getName());
        } else {
            return false;
        }
    }

    @Override
    public String toString() {
        if (missingOrIncompleteArchive != null) {
            return "MissingArchiveFile:" + missingOrIncompleteArchive + "|" + missingOrIncompleteArchive.getArchiveFiles();
        } else {
            return "MissingArchiveFile:" + getName();
        }
    }

    @Override
    public boolean exists() {
        return false;
    }

    @Override
    public boolean exists(boolean ignoreCache) {
        return false;
    }

    @Override
    public String getName() {
        return name;
    }

    private volatile ExtractionStatus status = null;

    @Override
    public void setStatus(ExtractionController controller, ExtractionStatus status) {
        this.status = status;
    }

    public ExtractionStatus getStatus() {
        final ExtractionStatus status = this.status;
        if (status != null) {
            return status;
        }
        return ExtractionStatus.NA;
    }

    @Override
    public void setMessage(ExtractionController controller, String plugins_optional_extraction_status_notenoughspace) {
    }

    @Override
    public void setProgress(ExtractionController controller, long value, long max, Color color) {
    }

    @Override
    public void removePluginProgress(ExtractionController controller) {
    }

    @Override
    public void onCleanedUp(ExtractionController controller) {
    }

    @Override
    public void setArchive(Archive archive) {
        this.archiveID = archive.getArchiveID();
    }

    @Override
    public void notifyChanges(Object type) {
    }

    @Override
    public void invalidateExists() {
    }

    @Override
    public void setPartOfAnArchive(Boolean b) {
    }

    @Override
    public Boolean isPartOfAnArchive() {
        return null;
    }

    @Override
    public String getArchiveID() {
        return archiveID;
    }
}
