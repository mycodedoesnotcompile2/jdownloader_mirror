package org.appwork.utils.extioexceptions;

import java.io.File;

public class CouldNotOpenFileInputStreamExtIOException extends AbstractLocalExtIOException {
    /**
     * @param cause
     *            TODO
     * @param src
     * @param dest
     * @param string
     */
    public CouldNotOpenFileInputStreamExtIOException(String message, Throwable cause, File dest) {
        super(message == null ? ("Could not open(read) file " + dest) : message, cause, dest);
    }
}
