package org.appwork.utils.extioexceptions;

import java.io.File;

public class CouldNotCreateDirectoryExtIOException extends AbstractLocalExtIOException {
    /**
     * @param cause
     *            TODO
     * @param src
     * @param dest
     * @param string
     */
    public CouldNotCreateDirectoryExtIOException(String message, Throwable cause, File dest) {
        super(message == null ? ("Could not create file " + dest) : message, cause, dest);
    }
}
