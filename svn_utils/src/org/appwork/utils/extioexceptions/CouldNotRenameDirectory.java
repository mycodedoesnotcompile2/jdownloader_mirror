package org.appwork.utils.extioexceptions;

import java.io.File;

public class CouldNotRenameDirectory extends AbstractLocalExtIOException {
    public final File srcFile;

    /**
     * @param cause
     *            TODO
     * @param src
     * @param dest
     * @param string
     */
    public CouldNotRenameDirectory(String message, Throwable cause, File src, File dest) {
        super(message == null ? ("Could not rename directory " + src + "->" + dest) : message, cause, dest);
        this.srcFile = src;
    }
    /*
     *
     * if (src.isDirectory()) { throw new CouldNotRenameFileOrDirectory(UpdTrans.I().getErrorCannotRenameDirectoryToFILE(dest),); } else {
     * throw new ExtIOException(UpdTrans.I().getErrorCannotRenameFileToFILE(dest), ExtIOException.IOExceptionType.LOCAL,
     * src.getAbsolutePath()); }
     *
     */
}
