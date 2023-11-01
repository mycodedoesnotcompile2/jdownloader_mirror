package org.appwork.utils.extioexceptions;

import java.io.File;

public class CouldNotWriteFileExtIOException extends AbstractLocalExtIOException {
    /**
     * @param cause
     *            TODO
     * @param src
     * @param dest
     * @param string
     */
    public CouldNotWriteFileExtIOException(String message, Throwable cause, File dest) {
        super(message == null ? ("Could not write to file " + dest) : message, cause, dest);
    }
}
