package org.appwork.utils.extioexceptions;

import java.io.File;

public class CouldNotCreateFileExtIOException extends AbstractLocalExtIOException {
    /**
     * @param cause
     *            TODO
     * @param src
     * @param dest
     * @param string
     */
    public CouldNotCreateFileExtIOException(String message, Throwable cause, File dest) {
        super(message == null ? ("Could not create file " + dest) : message, cause, dest);
    }
}
