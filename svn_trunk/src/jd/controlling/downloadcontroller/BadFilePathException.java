package jd.controlling.downloadcontroller;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.appwork.storage.config.annotations.LabelInterface;
import org.jdownloader.translate._JDT;

public class BadFilePathException extends IOException {
    public static enum PathFailureReason implements LabelInterface {
        PATH_SEGMENT_TOO_LONG {
            @Override
            public String getLabel() {
                return _JDT.T.BadFilePathException_PathFailureReason_PATH_SEGMENT_TOO_LONG();
            }
        },
        PATH_TOO_LONG {
            @Override
            public String getLabel() {
                return _JDT.T.BadFilePathException_PathFailureReason_PATH_TOO_LONG();
            }
        },
        PERMISSION_PROBLEM_FILE {
            @Override
            public String getLabel() {
                return _JDT.T.BadFilePathException_PathFailureReason_PERMISSION_PROBLEM_FILE();
            }
        },
        PERMISSION_PROBLEM_FOLDER {
            @Override
            public String getLabel() {
                return _JDT.T.BadFilePathException_PathFailureReason_PERMISSION_PROBLEM_FOLDER();
            }
        },
        FILE_ALREADY_EXISTS {
            @Override
            public String getLabel() {
                return _JDT.T.BadFilePathException_PathFailureReason_FILE_ALREADY_EXISTS();
            }
        },
        FILE_ALREADY_EXISTS_AS_FOLDER {
            @Override
            public String getLabel() {
                return _JDT.T.BadFilePathException_PathFailureReason_FILE_ALREADY_EXISTS_AS_FOLDER();
            }
        },
        INVALID_DESTINATION {
            @Override
            public String getLabel() {
                return _JDT.T.BadFilePathException_PathFailureReason_INVALID_DESTINATION();
            }
        },
        WRITE_TEST_DELETION_FAILURE {
            @Override
            public String getLabel() {
                return _JDT.T.BadFilePathException_PathFailureReason_WRITE_TEST_DELETION_FAILURE();
            }
        };
    }

    private File              file;
    private PathFailureReason reason;
    private int               index;

    public BadFilePathException(final File file) {
        init(file, PathFailureReason.INVALID_DESTINATION, -1);
    }

    public BadFilePathException(final File file, final PathFailureReason reason) {
        init(file, reason, -1);
    }

    public BadFilePathException(final File file, final PathFailureReason reason, final int index) {
        this.init(file, reason, index);
    }

    private void init(File file, final PathFailureReason reason, final int index) {
        this.file = file;
        this.reason = reason;
        this.index = index;
    }

    public File getFile() {
        return this.file;
    }

    public PathFailureReason getReason() {
        return this.reason;
    }

    public int getIndex() {
        return index;
    }

    /**
     * Returns path segment which caused the file/folder creation to fail. </br>
     * If the failure was not caused by a specific segment (e.g. full path too long), this returns the given file item. </br>
     * This will never return null.
     */
    public File getProblematicPathSegment() {
        if (this.index == -1) {
            return this.file;
        }
        final List<File> pathList = new ArrayList<File>();
        File next = this.getFile();
        while (true) {
            pathList.add(0, next);
            next = next.getParentFile();
            if (next == null) {
                /* We've reached the end. */
                break;
            }
        }
        return pathList.get(index);
    }
}
