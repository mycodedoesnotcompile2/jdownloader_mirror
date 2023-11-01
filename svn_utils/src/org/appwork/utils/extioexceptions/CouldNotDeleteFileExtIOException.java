package org.appwork.utils.extioexceptions;

import java.io.File;

public class CouldNotDeleteFileExtIOException extends AbstractLocalExtIOException {
    /**
     * @param message
     *            TODO
     * @param file
     * @param e
     */
    //
    public CouldNotDeleteFileExtIOException(String message, Throwable cause, File file) {
        super(message == null ? "Could not delete file" : message, cause, file);
    }
}
