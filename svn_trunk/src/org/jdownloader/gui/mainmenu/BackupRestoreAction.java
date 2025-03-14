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
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

import javax.swing.filechooser.FileFilter;

import org.appwork.loggingv3.LogV3;
import org.appwork.shutdown.ShutdownController;
import org.appwork.shutdown.ShutdownEvent;
import org.appwork.shutdown.ShutdownRequest;
import org.appwork.utils.Application;
import org.appwork.utils.Files;
import org.appwork.utils.Regex;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;
import org.appwork.utils.swing.dialog.ExtFileChooserDialog;
import org.appwork.utils.swing.dialog.FileChooserSelectionMode;
import org.appwork.utils.swing.dialog.FileChooserType;
import org.appwork.utils.zip.ZipIOReader;
import org.jdownloader.controlling.contextmenu.ActionContext;
import org.jdownloader.controlling.contextmenu.CustomizableAppAction;
import org.jdownloader.controlling.contextmenu.Customizer;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.translate._JDT;
import org.jdownloader.updatev2.ForcedRestartRequest;
import org.jdownloader.updatev2.RestartController;

public class BackupRestoreAction extends CustomizableAppAction implements ActionContext {
    public BackupRestoreAction() {
        super();
        setIconKey(IconKey.ICON_LOAD);
        setName(_GUI.T.BackupRestoreAction_BackupRestoreAction());
        setTooltipText(_GUI.T.BackupRestoreAction_BackupRestoreAction_tt());
    }

    private int maxAutoBackupFiles = -1;

    public static String getTranslationForMaxAutoBackupFiles() {
        return _JDT.T.BackupRestoreAction_getTranslationForMaxAutoBackupFiles();
    }

    public static String getTranslationForMaxCFGBackupFolders() {
        return _JDT.T.BackupRestoreAction_getTranslationForMaxCFGBackupFolders();
    }

    @Customizer(link = "#getTranslationForMaxAutoBackupFiles")
    public int getMaxAutoBackupFiles() {
        return maxAutoBackupFiles;
    }

    public void setMaxAutoBackupFiles(int maxAutoBackupFiles) {
        this.maxAutoBackupFiles = maxAutoBackupFiles;
    }

    private int maxCFGBackupFolders = -1;

    @Customizer(link = "#getTranslationForMaxCFGBackupFolders")
    public int getMaxCFGBackupFolders() {
        return maxCFGBackupFolders;
    }

    public void setMaxCFGBackupFolders(int maxCFGBackupFolders) {
        this.maxCFGBackupFolders = maxCFGBackupFolders;
    }

    private void cleanupCFGBackFolders(int maxCFGBackupFolders) {
        if (maxCFGBackupFolders < 1) {
            return;
        }
        File[] files = Application.getResource("autobackup").getParentFile().listFiles(new FilenameFilter() {
            @Override
            public boolean accept(File dir, String name) {
                return name.matches("^cfg_backup_\\d+$") && new File(dir, name).isDirectory();
            }
        });
        if (files != null && files.length > maxCFGBackupFolders) {
            ArrayList<File> list = new ArrayList<File>();
            for (File f : files) {
                list.add(f);
            }
            Collections.sort(list, new Comparator<File>() {
                @Override
                public int compare(File o1, File o2) {
                    long i1 = Long.parseLong(new Regex(o1.getName(), "cfg_backup_(\\d+)").getMatch(0));
                    long i2 = Long.parseLong(new Regex(o2.getName(), "cfg_backup_(\\d+)").getMatch(0));
                    return Long.compare(i2, i1);
                }
            });
            for (int i = maxCFGBackupFolders; i < list.size(); i++) {
                try {
                    Files.deleteRecursiv(list.get(i));
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    private void cleanupAutoBackups(int maxAutoBackupFiles) {
        if (maxAutoBackupFiles < 0) {
            return;
        }
        File[] files = Application.getResource("autobackup").listFiles(new FilenameFilter() {
            @Override
            public boolean accept(File dir, String name) {
                return name.matches("^backup_\\d+\\.jd2backup$");
            }
        });
        if (files != null && files.length > maxAutoBackupFiles) {
            ArrayList<File> list = new ArrayList<File>();
            for (File f : files) {
                list.add(f);
            }
            Collections.sort(list, new Comparator<File>() {
                @Override
                public int compare(File o1, File o2) {
                    int i1 = Integer.parseInt(new Regex(o1.getName(), "backup_(\\d+)\\.jd2backup").getMatch(0));
                    int i2 = Integer.parseInt(new Regex(o2.getName(), "backup_(\\d+)\\.jd2backup").getMatch(0));
                    return Integer.compare(i2, i1);
                }
            });
            for (int i = maxAutoBackupFiles; i < list.size(); i++) {
                list.get(i).delete();
            }
        }
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        new Thread("Restore Backup") {
            @Override
            public void run() {
                cleanupCFGBackFolders(3);
                ExtFileChooserDialog d = new ExtFileChooserDialog(0, _GUI.T.BackupCreateAction_actionPerformed_filechooser_title(), _GUI.T.lit_open(), null);
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
                d.setType(FileChooserType.OPEN_DIALOG);
                try {
                    int i = 1;
                    File auto = Application.getResource("autobackup/backup_" + i + ".jd2backup");
                    while (auto.exists()) {
                        i++;
                        auto = Application.getResource("autobackup/backup_" + i + ".jd2backup");
                    }
                    final File fauto = auto;
                    Dialog.getInstance().showConfirmDialog(0, _GUI.T.lit_restart(), _GUI.T.BackupRestoreAction_run_restart_ask(auto.getAbsolutePath()), null, _GUI.T.lit_continue(), null);
                    Dialog.getInstance().showDialog(d);
                    final File file = d.getSelectedFile();
                    if (file == null || !file.exists()) {
                        return;
                    }
                    ShutdownController.getInstance().addShutdownEvent(new ShutdownEvent() {
                        {
                            setHookPriority(Integer.MIN_VALUE);
                        }

                        @Override
                        public String toString() {
                            return "ShutdownHook: Restore Backup";
                        }

                        @Override
                        public long getMaxDuration() {
                            return 0;
                        }

                        @Override
                        public void onShutdown(ShutdownRequest shutdownRequest) {
                            try {
                                if (getMaxAutoBackupFiles() > 0) {
                                    BackupCreateAction.create(fauto);
                                }
                                if (getMaxAutoBackupFiles() >= 0) {
                                    cleanupAutoBackups(getMaxAutoBackupFiles());
                                }
                                File tmp = Application.getTempResource("restorebackup_" + System.currentTimeMillis());
                                while (tmp.exists()) {
                                    tmp = Application.getTempResource("restorebackup_" + System.currentTimeMillis());
                                }
                                File backup = Application.getResource("cfg_backup_" + System.currentTimeMillis());
                                while (backup.exists()) {
                                    backup = Application.getResource("cfg_backup_" + System.currentTimeMillis());
                                }
                                ZipIOReader.extractTo(file, tmp);
                                Application.getResource("cfg").renameTo(backup);
                                if (getMaxCFGBackupFolders() >= 0) {
                                    cleanupCFGBackFolders(getMaxCFGBackupFolders());
                                }
                                if (Application.getResource("cfg").exists()) {
                                    throw new Exception("Could not delete " + Application.getResource("cfg"));
                                } else {
                                    new File(tmp, "cfg").renameTo(Application.getResource("cfg"));
                                }
                            } catch (Exception e) {
                                LogV3.defaultLogger().log(e);
                                Dialog.getInstance().showExceptionDialog(_GUI.T.lit_error_occured(), e.getMessage() + "\r\nPlease try to close JDownloader, and extract the file\r\n" + file.getAbsolutePath() + "\r\nto " + Application.getResource("cfg").getParent() + "\r\nusing an application like WinZip, 7Zip or Winrar.\r\nIf this does not work, feel free to contact our support.", e);
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
}
