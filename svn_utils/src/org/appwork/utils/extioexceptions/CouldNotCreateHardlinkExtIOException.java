package org.appwork.utils.extioexceptions;

import java.io.File;

public class CouldNotCreateHardlinkExtIOException extends AbstractLocalExtIOException {
    public final File srcFile;

    /**
     * @param cause
     *            TODO
     * @param src
     * @param dest
     * @param string
     */
    public CouldNotCreateHardlinkExtIOException(String message, Throwable cause, File src, File dest) {
        super(message == null ? ("Could not hardlink " + src + "->" + dest) : message, cause, dest);
        this.srcFile = src;
    }
}
