package org.appwork.utils.extioexceptions;

import java.io.File;

public class CouldNotReadFileExtIOException extends AbstractLocalExtIOException {
    /**
     * @param cause
     *            TODO
     * @param src
     * @param dest
     * @param string
     */
    public CouldNotReadFileExtIOException(String message, Throwable cause, File dest) {
        super(message, cause, dest);
    }
}
