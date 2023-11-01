package org.appwork.utils.extioexceptions;

import java.io.File;

public class FileNotFoundExtIOException extends AbstractLocalExtIOException {
    /**
     * @param e
     * @param file
     */
    public FileNotFoundExtIOException(String translation, Throwable cause, File file) {
        super(translation == null ? ("File/Folder not found: " + file) : translation, cause, file);
    }
    // UpdTrans.I().getErrorCannotMoveNonExistingFile()
}
