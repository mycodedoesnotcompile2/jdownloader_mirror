package org.appwork.utils.extioexceptions;

import java.io.File;

public class CouldNotAccessPathExtIOException extends AbstractLocalExtIOException {
    /**
     * @param cause
     *            TODO
     * @param src
     * @param dest
     * @param string
     */
    public CouldNotAccessPathExtIOException(String message, Throwable cause, File dest) {
        super(message == null ? ("Could not access path" + dest) : message, cause, dest);
    }
}
