package org.appwork.utils.extioexceptions;

import java.io.File;

public class CouldNotRenameFile extends AbstractLocalExtIOException {
    public final File srcFile;

    /**
     * @param cause
     *            TODO
     * @param src
     * @param dest
     * @param string
     */
    public CouldNotRenameFile(String message, Throwable cause, File src, File dest) {
        super(message == null ? ("Could not rename file " + src + "->" + dest) : message, cause, dest);
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
