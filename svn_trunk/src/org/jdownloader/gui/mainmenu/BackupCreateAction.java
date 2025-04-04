//    jDownloader - Downloadmanager
//    Copyright (C) 2009  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package org.jdownloader.gui.mainmenu;

import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.zip.ZipEntry;

import javax.swing.filechooser.FileFilter;

import org.appwork.loggingv3.LogV3;
import org.appwork.shutdown.ShutdownController;
import org.appwork.shutdown.ShutdownEvent;
import org.appwork.shutdown.ShutdownRequest;
import org.appwork.utils.Application;
import org.appwork.utils.StringUtils;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;
import org.appwork.utils.swing.dialog.ExtFileChooserDialog;
import org.appwork.utils.swing.dialog.FileChooserSelectionMode;
import org.appwork.utils.swing.dialog.FileChooserType;
import org.appwork.utils.zip.ZipIOException;
import org.appwork.utils.zip.ZipIOWriter;
import org.jdownloader.controlling.contextmenu.CustomizableAppAction;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.updatev2.ForcedRestartRequest;
import org.jdownloader.updatev2.RestartController;

public class BackupCreateAction extends CustomizableAppAction {
    public static final String HIDE_ON_MAC = "HideOnMac";

    public BackupCreateAction() {
        setIconKey(IconKey.ICON_SAVE);
        setName(_GUI.T.BackupCreateAction_BackupCreateAction());
        setTooltipText(_GUI.T.BackupCreateAction_BackupCreateAction_tt());
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        new Thread("Create Backup") {
            @Override
            public void run() {
                ExtFileChooserDialog d = new ExtFileChooserDialog(0, _GUI.T.BackupCreateAction_actionPerformed_filechooser_title(), _GUI.T.lit_save(), null);
                d.setFileFilter(new FileFilter() {
                    @Override
                    public String getDescription() {
                        return "*.jd2backup";
                    }

                    @Override
                    public boolean accept(File f) {
                        return f.isDirectory() || f.getName().endsWith(".jd2backup");
                    }
                });
                d.setFileSelectionMode(FileChooserSelectionMode.FILES_ONLY);
                d.setMultiSelection(false);
                d.setStorageID("jd2backup");
                d.setType(FileChooserType.SAVE_DIALOG);
                try {
                    Dialog.getInstance().showConfirmDialog(0, _GUI.T.lit_restart(), _GUI.T.BackupCreateAction_run_restart_ask(), null, _GUI.T.lit_continue(), null);
                    Dialog.getInstance().showDialog(d);
                    File file = d.getSelectedFile();
                    if (file == null) {
                        return;
                    }
                    /* Correct filename if needed. */
                    if (!file.getName().endsWith(".jd2backup")) {
                        file = new File(file.getAbsolutePath() + ".jd2backup");
                    }
                    if (file.exists()) {
                        /* File already exists -> Ask user to confirm deletion of that file. */
                        Dialog.getInstance().showConfirmDialog(0, _GUI.T.lit_overwrite(), _GUI.T.file_exists_want_to_overwrite_question(file.getName()));
                        file.delete();
                    }
                    /**
                     * Try to write the file. <br>
                     * If we can't write it, it doesn't make sense to initiate an application restart.
                     */
                    final FileOutputStream backupOutputStream;
                    try {
                        if (!file.getParentFile().exists()) {
                            file.getParentFile().mkdirs();
                        }
                        backupOutputStream = new FileOutputStream(file);
                    } catch (final IOException e) {
                        // TODO: Add translation
                        final String titleAndMsg = "Failed to write file " + file.getName();
                        Dialog.getInstance().showExceptionDialog(titleAndMsg, titleAndMsg, e);
                        return;
                    }
                    final File backupFile = file;
                    ShutdownController.getInstance().addShutdownEvent(new ShutdownEvent() {
                        {
                            setHookPriority(Integer.MIN_VALUE);
                        }

                        @Override
                        public String toString() {
                            return "ShutdownHook: Create Backup";
                        }

                        @Override
                        public long getMaxDuration() {
                            return 0;
                        }

                        @Override
                        public void onShutdown(ShutdownRequest shutdownRequest) {
                            try {
                                create(backupOutputStream);
                            } catch (Throwable e) {
                                try {
                                    try {
                                        backupOutputStream.close();
                                    } catch (IOException ignore) {
                                        LogV3.defaultLogger().log(e);
                                    }
                                    LogV3.defaultLogger().log(e);
                                } finally {
                                    backupFile.delete();
                                }
                            }
                        }
                    });
                    RestartController.getInstance().directRestart(new ForcedRestartRequest());
                } catch (DialogClosedException e1) {
                    e1.printStackTrace();
                } catch (DialogCanceledException e1) {
                    e1.printStackTrace();
                }
            }
        }.start();
    }

    protected static void create(final File file) throws IOException {
        if (!file.getParentFile().exists()) {
            file.getParentFile().mkdirs();
        }
        final FileOutputStream fos = new FileOutputStream(file);
        try {
            create(fos);
        } catch (IOException e) {
            fos.close();
            file.delete();
            throw e;
        } finally {
            fos.close();
        }
    }

    protected static void create(final OutputStream fos) throws IOException {
        ZipIOWriter zipper = null;
        ZipIOException exception = null;
        try {
            final StringBuilder backupErrors = new StringBuilder();
            zipper = new ZipIOWriter(fos) {
                @Override
                protected boolean throwExceptionOnFileGone(File file) {
                    return false;
                }

                public boolean isFiltered(File e) {
                    final String name = new File(e.getName()).getName();
                    if (StringUtils.startsWithCaseInsensitive(name, "RememberRelativeLocator-")) {
                        return true;
                    } else if (StringUtils.startsWithCaseInsensitive(name, "RememberLastDimensor-")) {
                        return true;
                    } else if (StringUtils.startsWithCaseInsensitive(name, "RememberAbsoluteLocator-")) {
                        return true;
                    } else if (StringUtils.startsWithCaseInsensitive(name, "CaptchaDialogDimensions_")) {
                        return true;
                    } else {
                        return false;
                    }
                }

                final byte[] buf = new byte[32 * 1024];

                @Override
                protected void addFile(File addFile, boolean compress, String fullPath) throws ZipIOException {
                    if (addFile == null) {
                        throw new ZipIOException("addFile invalid:null");
                    } else if (isFiltered(addFile)) {
                        return;
                    }
                    ZipEntry entry = null;
                    ZipIOException exception = null;
                    FileInputStream fis = null;
                    try {
                        fis = new FileInputStream(addFile);
                        try {
                            long remaining = addFile.length();
                            if (remaining == 0) {
                                entry = new ZipEntry(fullPath);
                                entry.setMethod(ZipEntry.DEFLATED);
                                this.zipStream.putNextEntry(entry);
                            } else {
                                long written = 0;
                                while (remaining > 0) {
                                    final int read;
                                    try {
                                        read = fis.read(buf);
                                        // if (DebugMode.TRUE_IN_IDE_ELSE_FALSE && new Random().nextInt(100) > 20) {
                                        // throw new IOException("Random");
                                        // }
                                    } catch (IOException e) {
                                        LogV3.defaultLogger().log(e);
                                        break;
                                    }
                                    if (read == -1) {
                                        break;
                                    } else if (read > 0) {
                                        if (entry == null) {
                                            entry = new ZipEntry(fullPath);
                                            entry.setMethod(ZipEntry.DEFLATED);
                                            this.zipStream.putNextEntry(entry);
                                        }
                                        this.zipStream.write(buf, 0, read);
                                        written += read;
                                        remaining -= read;
                                        notify(entry, read, written);
                                    }
                                }
                                if (written != addFile.length()) {
                                    if (written == 0) {
                                        backupErrors.append("Missing(Gone):" + addFile.getAbsolutePath()).append("\r\n");
                                    } else {
                                        backupErrors.append("Incomplete:" + addFile.getAbsolutePath()).append("\r\n");
                                    }
                                }
                            }
                        } finally {
                            if (entry != null) {
                                this.zipStream.closeEntry();
                            }
                        }
                    } catch (FileNotFoundException e) {
                        backupErrors.append("Missing(Not Found):" + addFile.getAbsolutePath()).append("\r\n");
                        LogV3.defaultLogger().log(e);
                        if (addFile.exists() == false) {
                            if (throwExceptionOnFileGone(addFile)) {
                                exception = ZipIOException.wrapOrAddSurpressed(exception, e, entry);
                            } else {
                                return;
                            }
                        } else {
                            exception = ZipIOException.wrapOrAddSurpressed(exception, e, entry);
                        }
                    } catch (IOException e) {
                        exception = ZipIOException.wrapOrAddSurpressed(exception, e, entry);
                    } finally {
                        try {
                            if (fis != null) {
                                fis.close();
                            }
                        } catch (IOException e) {
                            exception = ZipIOException.wrapOrAddSurpressed(exception, e, entry);
                        }
                        if (exception != null) {
                            throw exception;
                        }
                    }
                }
            };
            zipper.add(Application.getResource("cfg"), false, "");
            if (backupErrors.length() > 0) {
                zipper.add(backupErrors.toString().getBytes("UTF-8"), true, "cfg", "BackupErrors.txt");
            }
        } catch (IOException e) {
            exception = ZipIOException.wrapOrAddSurpressed(exception, e, null);
        } finally {
            try {
                if (zipper != null) {
                    zipper.close();
                }
            } catch (Throwable e) {
                exception = ZipIOException.wrapOrAddSurpressed(exception, e, null);
            }
            if (exception != null) {
                throw exception;
            }
        }
    }
}
