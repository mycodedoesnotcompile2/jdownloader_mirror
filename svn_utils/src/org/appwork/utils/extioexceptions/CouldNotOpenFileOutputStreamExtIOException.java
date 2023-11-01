package org.appwork.utils.extioexceptions;

import java.io.File;

public class CouldNotOpenFileOutputStreamExtIOException extends AbstractLocalExtIOException {
    /**
     * @param cause
     *            TODO
     * @param src
     * @param dest
     * @param string
     */
    public CouldNotOpenFileOutputStreamExtIOException(String message, Throwable cause, File dest) {
        super(message == null ? ("Could not open(write) file " + dest) : message, cause, dest);
    }
}
