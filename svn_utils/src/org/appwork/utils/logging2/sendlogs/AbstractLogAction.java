/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.logging2.sendlogs;

import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import org.appwork.exceptions.WTFException;
import org.appwork.swing.action.BasicAction;
import org.appwork.uio.UIOManager;
import org.appwork.utils.Application;
import org.appwork.utils.Files;
import org.appwork.utils.IO;
import org.appwork.utils.Regex;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;
import org.appwork.utils.swing.dialog.ProgressDialog;
import org.appwork.utils.swing.dialog.ProgressDialog.ProgressGetter;
import org.appwork.utils.zip.ZipIOException;
import org.appwork.utils.zip.ZipIOWriter;

/**
 * @author Thomas
 *
 */
public abstract class AbstractLogAction extends BasicAction {
    protected int total;
    protected int current;

    public AbstractLogAction() {
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        final ProgressDialog p = new ProgressDialog(new ProgressGetter() {
            @Override
            public String getLabelString() {
                return null;
            }

            @Override
            public int getProgress() {
                return -1;
            }

            @Override
            public String getString() {
                return T.T.LogAction_getString_uploading_();
            }

            @Override
            public void run() throws Exception {
                AbstractLogAction.this.create();
            }
        }, UIOManager.BUTTONS_HIDE_OK, T.T.LogAction_actionPerformed_zip_title_(), T.T.LogAction_actionPerformed_wait_(), null, null, null);
        try {
            Dialog.getInstance().showDialog(p);
        } catch (final Throwable e1) {
        }
    }

    protected SendLogDialog getSendLogDialog(List<LogFolder> folders) {
        return new SendLogDialog(folders);
    }

    protected void create() throws DialogClosedException, DialogCanceledException {
        final java.util.List<LogFolder> folders = getLogFolders();
        LogFolder currentLog = null;
        LogFolder latestLog = null;
        for (final LogFolder folder : folders) {
            if (this.isCurrentLogFolder(folder.getCreated())) {
                /*
                 * this is our current logfolder, flush it before we can upload it
                 */
                folder.setNeedsFlush(true);
                currentLog = folder;
            }
            if (latestLog == null || folder.getCreated() > latestLog.getCreated()) {
                latestLog = folder;
            }
        }
        if (currentLog != null) {
            currentLog.setSelected(true);
            currentLog.setCurrent(true);
        } else {
            if (latestLog != null) {
                latestLog.setSelected(true);
            }
        }
        final SendLogDialog d = getSendLogDialog(folders);
        Dialog.getInstance().showDialog(d);
        final java.util.List<LogFolder> selection = d.getSelectedFolders();
        if (selection.size() == 0) {
            return;
        }
        this.total = selection.size();
        this.current = 0;
        final ProgressDialog p = new ProgressDialog(new ProgressGetter() {
            @Override
            public String getLabelString() {
                return null;
            }

            @Override
            public int getProgress() {
                if (AbstractLogAction.this.current == 0) {
                    return -1;
                }
                return Math.min(99, AbstractLogAction.this.current * 100 / AbstractLogAction.this.total);
            }

            @Override
            public String getString() {
                return T.T.LogAction_getString_uploading_();
            }

            @Override
            public void run() throws Exception {
                try {
                    AbstractLogAction.this.createPackage(selection);
                } catch (final WTFException e) {
                    throw new InterruptedException();
                }
            }
        }, UIOManager.BUTTONS_HIDE_OK, T.T.LogAction_actionPerformed_zip_title_(), T.T.LogAction_actionPerformed_wait_(), null, null, null);
        Dialog.getInstance().showDialog(p);
    }

    /**
     * @return
     */
    public static ArrayList<LogFolder> getLogFolders() {
        final File[] logs = Application.getResource("logs").listFiles();
        final ArrayList<LogFolder> folders = new ArrayList<LogFolder>();
        if (logs != null) {
            for (final File f : logs) {
                final String timestampString = new Regex(f.getName(), "(\\d{7,})_").getMatch(0);
                if (timestampString != null) {
                    final LogFolder logFolder = new LogFolder(f, Long.parseLong(timestampString));
                    folders.add(logFolder);
                }
            }
            Collections.sort(folders, new Comparator<LogFolder>() {
                private final int compare(long x, long y) {
                    return (x < y) ? -1 : ((x == y) ? 0 : 1);
                }

                @Override
                public int compare(LogFolder o1, LogFolder o2) {
                    return compare(o2.getCreated(), o1.getCreated());
                }
            });
        }
        return folders;
    }

    /**
     * @param selection
     *
     */
    protected void createPackage(final List<LogFolder> selection) throws Exception {
        for (final LogFolder lf : selection) {
            final File zip = Application.getTempResource("logs/logPackage.zip");
            zip.delete();
            zip.getParentFile().mkdirs();
            ZipIOWriter writer = null;
            final String name = lf.getFolder().getName() + "-" + this.format(lf.getCreated()) + " to " + this.format(lf.getLastModified());
            final File folder = Application.getTempResource("logs/" + name);
            try {
                if (lf.isNeedsFlush()) {
                    this.flushLogs();
                }
                writer = new ZipIOWriter(zip) {
                    @Override
                    public void addFile(final File addFile, final boolean compress, final String fullPath) throws FileNotFoundException, ZipIOException, IOException {
                        if (addFile.getName().endsWith(".lck") || addFile.isFile() && addFile.length() == 0) {
                            return;
                        }
                        if (Thread.currentThread().isInterrupted()) {
                            throw new WTFException("INterrupted");
                        }
                        super.addFile(addFile, compress, fullPath);
                    }
                };
                if (folder.exists()) {
                    Files.deleteRecursiv(folder);
                }
                IO.copyFolderRecursive(lf.getFolder(), folder, true);
                writer.addDirectory(folder, true, null);
            } finally {
                try {
                    writer.close();
                } catch (final Throwable e) {
                }
            }
            this.onNewPackage(zip, this.format(lf.getCreated()) + "-" + this.format(lf.getLastModified()));
            this.current++;
        }
    }

    /**
     *
     */
    abstract protected void flushLogs();

    /**
     * @param created
     * @return
     */
    protected String format(final long created) {
        final Date date = new Date(created);
        return new SimpleDateFormat("dd.MM.yy HH.mm.ss", Locale.GERMANY).format(date);
    }

    /**
     * @param timestamp
     * @return
     */
    abstract protected boolean isCurrentLogFolder(long timestamp);

    /**
     * @param zip
     * @param string
     * @throws IOException
     */
    abstract protected void onNewPackage(File zip, String string) throws IOException;
}
