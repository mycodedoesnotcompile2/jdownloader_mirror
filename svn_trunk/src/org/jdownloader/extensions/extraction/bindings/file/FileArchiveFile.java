package org.jdownloader.extensions.extraction.bindings.file;

import java.awt.Color;
import java.io.File;
import java.util.concurrent.atomic.AtomicReference;

import jd.plugins.LinkInfo;

import org.appwork.utils.StringUtils;
import org.jdownloader.controlling.FileCreationManager;
import org.jdownloader.controlling.FileStateManager;
import org.jdownloader.controlling.FileStateManager.FILESTATE;
import org.jdownloader.extensions.extraction.Archive;
import org.jdownloader.extensions.extraction.ArchiveFile;
import org.jdownloader.extensions.extraction.ExtractionController;
import org.jdownloader.extensions.extraction.ExtractionStatus;

public class FileArchiveFile implements ArchiveFile {
    private final File                     file;
    private final String                   name;
    private final String                   filePath;
    private final int                      hashCode;
    private final AtomicReference<Boolean> exists    = new AtomicReference<Boolean>(null);
    private String                         archiveID = null;

    protected FileArchiveFile(File f) {
        this.file = f;
        exists.set(Boolean.TRUE);
        name = file.getName();
        filePath = file.getAbsolutePath();
        hashCode = (getClass() + name).hashCode();
    }

    public File getFile() {
        return file;
    }

    public Boolean isComplete() {
        return exists();
    }

    public String getFilePath() {
        return filePath;
    }

    @Override
    public int hashCode() {
        return hashCode;
    }

    @Override
    public LinkInfo getLinkInfo() {
        return LinkInfo.getLinkInfo(getName());
    }

    @Override
    public boolean contains(Object obj) {
        if (obj == this) {
            return true;
        } else if (obj != null) {
            if (obj instanceof String) {
                return getFilePath().equals(obj);
            } else if (obj instanceof ArchiveFile && StringUtils.equals(getFilePath(), ((ArchiveFile) obj).getFilePath())) {
                return true;
            } else if (obj instanceof FileArchiveFile) {
                return getFile().equals(((FileArchiveFile) obj).getFile());
            } else if (obj instanceof File) {
                return getFile().equals((obj));
            }
        }
        return false;
    }

    public void deleteFile(FileCreationManager.DeleteOption option) {
        FileCreationManager.getInstance().delete(getFile(), option);
        invalidateExists();
    }

    public String toString() {
        return "File:" + filePath + "|Complete:" + isComplete();
    }

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

    public void setMessage(ExtractionController controller, String plugins_optional_extraction_status_notenoughspace) {
    }

    public void setProgress(ExtractionController controller, long value, long max, Color color) {
    }

    @Override
    public long getFileSize() {
        return getFile().length();
    }

    @Override
    public void onCleanedUp(ExtractionController controller) {
    }

    @Override
    public void setArchive(Archive archive) {
        archiveID = archive.getArchiveID();
    }

    @Override
    public boolean exists() {
        return exists(false);
    };

    @Override
    public boolean exists(boolean ignoreCache) {
        if (FileStateManager.getInstance().hasFileState(getFile(), FILESTATE.WRITE_EXCLUSIVE)) {
            return false;
        }
        Boolean ret = ignoreCache ? null : exists.get();
        if (ret == null) {
            ret = getFile().isFile();
            exists.compareAndSet(null, ret);
        }
        return ret;
    }

    @Override
    public void notifyChanges(Object type) {
    }

    @Override
    public void removePluginProgress(ExtractionController controller) {
    }

    @Override
    public void invalidateExists() {
        exists.set(null);
    }

    @Override
    public void setPartOfAnArchive(Boolean b) {
    }

    public Boolean isPartOfAnArchive() {
        return null;
    }

    @Override
    public String getArchiveID() {
        return archiveID;
    };
}
