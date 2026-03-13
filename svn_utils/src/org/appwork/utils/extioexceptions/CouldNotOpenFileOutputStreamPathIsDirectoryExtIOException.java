package org.appwork.utils.extioexceptions;

import java.io.File;

/**
 * Thrown when a FileOutputStream cannot be opened because the path exists and is a directory (not a file).
 */
public class CouldNotOpenFileOutputStreamPathIsDirectoryExtIOException extends AbstractLocalExtIOException {
    public CouldNotOpenFileOutputStreamPathIsDirectoryExtIOException(String message, Throwable cause, File file) {
        super(message == null ? ("Path is a directory, cannot open for writing: " + file) : message, cause, file);
    }
}
