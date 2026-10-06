package org.jdownloader.extensions.extraction;

import java.util.ArrayList;
import java.util.List;

import org.jdownloader.extensions.extraction.multi.ArchiveType;
import org.jdownloader.extensions.extraction.split.SplitType;

public class DummyArchive {
    private final String      name;
    private final ArchiveType archiveType;

    public ArchiveType getArchiveType() {
        return archiveType;
    }

    public SplitType getSplitType() {
        return splitType;
    }

    private final SplitType splitType;

    public String getType() {
        if (archiveType != null) {
            return archiveType.name();
        } else if (splitType != null) {
            return splitType.name();
        } else {
            return null;
        }
    }

    public int getIncompleteCount() {
        int ret = 0;
        for (final DummyArchiveFile dummyArchiveFile : getList()) {
            if (Boolean.TRUE.equals(dummyArchiveFile.isIncomplete())) {
                ret++;
            }
        }
        return ret;
    }

    public int getMissingCount() {
        int ret = 0;
        for (final DummyArchiveFile dummyArchiveFile : getList()) {
            if (dummyArchiveFile.isMissing()) {
                ret++;
            }
        }
        return ret;
    }

    private final List<DummyArchiveFile> dummyArchiveFiles;

    public List<DummyArchiveFile> getList() {
        return dummyArchiveFiles;
    }

    public DummyArchive(Archive archive, ArchiveType archiveType) {
        name = archive.getName();
        this.archiveType = archiveType;
        this.splitType = null;
        dummyArchiveFiles = new ArrayList<DummyArchiveFile>();
    }

    public DummyArchive(Archive archive, SplitType splitType) {
        name = archive.getName();
        this.splitType = splitType;
        this.archiveType = null;
        dummyArchiveFiles = new ArrayList<DummyArchiveFile>();
    }

    public void add(DummyArchiveFile archiveFile) {
        dummyArchiveFiles.add(archiveFile);
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append("Archive:");
        sb.append(getName());
        sb.append("\r\n");
        sb.append("Type:");
        sb.append(getType());
        for (final DummyArchiveFile dummyArchiveFile : getList()) {
            sb.append("\r\n");
            if (dummyArchiveFile.isMissing()) {
                sb.append("Missing:");
            } else {
                sb.append("Existing:");
            }
            sb.append(dummyArchiveFile.toString());
        }
        sb.append("\r\n");
        sb.append("Complete:");
        sb.append(isComplete());
        return sb.toString();
    }

    public boolean isComplete() {
        return isComplete(false);
    }

    public boolean isComplete(boolean checkExists) {
        if (getSize() == 0 || getMissingCount() > 0 || getIncompleteCount() > 0) {
            return false;
        }
        if (!checkExists) {
            return true;
        }
        for (final DummyArchiveFile dummyArchiveFile : getList()) {
            if (!dummyArchiveFile.exists()) {
                return false;
            }
        }
        return true;
    }

    public int getSize() {
        return getList().size();
    }

    public String getName() {
        return name;
    }
}
