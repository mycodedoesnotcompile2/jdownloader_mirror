package org.jdownloader.gui.views.downloads.action;

import java.awt.event.ActionEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

import javax.swing.JProgressBar;

import org.appwork.utils.StringUtils;
import org.appwork.utils.locale._AWU;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogNoAnswerException;
import org.appwork.utils.swing.dialog.ProgressDialog;
import org.appwork.utils.swing.dialog.ProgressDialog.ProgressGetter;
import org.jdownloader.controlling.contextmenu.CustomizableTableContextAppAction;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;
import org.jdownloader.plugins.FinalLinkState;

import jd.controlling.downloadcontroller.DownloadSession;
import jd.controlling.downloadcontroller.DownloadWatchDog;
import jd.controlling.downloadcontroller.DownloadWatchDogJob;
import jd.controlling.downloadcontroller.SingleDownloadController;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

/**
 * "Mark as finished if file exists on disk" context menu action.
 *
 * Walks all selected {@link DownloadLink}s and marks each one as finished whose expected output file already exists on disk. A cache
 * of already-checked absolute paths keeps the number of {@link File#exists()} calls to a minimum, which also covers the case where the
 * same file (same path) is referenced by two different packages. The whole scan runs inside a cancelable {@link ProgressDialog}.
 */
public class MarkExistingOnDiskAsFinishedAction extends CustomizableTableContextAppAction<FilePackage, DownloadLink> {
    private static final long   serialVersionUID = 8087143123808363306L;
    private final static String NAME             = _GUI.T.gui_table_contextmenu_markexistingondiskasfinished();

    public MarkExistingOnDiskAsFinishedAction() {
        setIconKey(IconKey.ICON_TRUE);
        setName(NAME);
    }

    private void setFinished(final DownloadLink downloadlink) {
        downloadlink.setFinalLinkState(FinalLinkState.FINISHED);
        final long knownSize = downloadlink.getKnownDownloadSize();
        if (knownSize >= 0) {
            downloadlink.setDownloadCurrent(knownSize);
        }
    }

    /**
     * Enqueues the state change through the {@link DownloadWatchDog}. If the link is currently being downloaded, the change is deferred
     * until the download controller detaches, otherwise it is applied right away. Mirrors {@link MarkDownloadFinishedAction}.
     */
    private void markFinished(final DownloadLink link) {
        DownloadWatchDog.getInstance().enqueueJob(new DownloadWatchDogJob() {
            @Override
            public boolean isHighPriority() {
                return false;
            }

            @Override
            public void interrupt() {
            }

            @Override
            public void execute(DownloadSession currentSession) {
                final DownloadWatchDogJob setFinishedJob = new DownloadWatchDogJob() {
                    @Override
                    public boolean isHighPriority() {
                        return false;
                    }

                    @Override
                    public void interrupt() {
                    }

                    @Override
                    public void execute(DownloadSession currentSession) {
                        setFinished(link);
                    }
                };
                final SingleDownloadController con = link.getDownloadLinkController();
                if (con == null || !con.isAlive()) {
                    setFinishedJob.execute(currentSession);
                } else {
                    con.getJobsAfterDetach().add(setFinishedJob);
                }
            }
        });
    }

    public void actionPerformed(ActionEvent e) {
        final List<DownloadLink> selection = getSelection().getChildren();
        if (selection.size() == 0) {
            return;
        }
        new Thread("MarkExistingOnDiskAsFinishedAction") {
            public void run() {
                /* Holds the text currently shown above the progress bar (e.g. "Working on: <filename>"). */
                final AtomicReference<String> workingOn = new AtomicReference<String>();
                final ProgressGetter getter = new ProgressGetter() {
                    private final int    total             = selection.size();
                    private volatile int current           = 0;
                    private volatile int marked            = 0;
                    private volatile int skipped           = 0;
                    private volatile int errors            = 0;
                    /* Phase flag: true while scanning, false during the auto-close countdown that follows a completed scan. */
                    private volatile boolean scanning      = true;
                    /* System time (ms) at which the dialog auto-closes; only meaningful once scanning == false. */
                    private volatile long    closeDeadline = 0;
                    private final long       AUTO_CLOSE_TIMEOUT = 30000;

                    @Override
                    public void run() throws Exception {
                        /* Cache of absolute file path -> existence, to minimize File.exists calls for duplicate paths. */
                        final HashMap<String, Boolean> existsCache = new HashMap<String, Boolean>();
                        /*
                         * Cache of parent directory path -> "may contain existing files". A single dir.list() call determines both
                         * existence and emptiness of the directory: null means the directory does not exist, an empty array means it is
                         * empty. In both cases none of the files inside can exist, so we can skip the per-file File.exists() call
                         * entirely. This is correct on all platforms (no case-sensitivity assumptions).
                         */
                        final HashMap<String, Boolean> dirUsableCache = new HashMap<String, Boolean>();
                        final List<DownloadLink> toMark = new ArrayList<DownloadLink>();
                        boolean canceled = false;
                        try {
                            for (final DownloadLink link : selection) {
                                if (Thread.currentThread().isInterrupted()) {
                                    /* User closed the dialog: stop scanning but still apply what we found so far. */
                                    canceled = true;
                                    break;
                                }
                                current++;
                                workingOn.set(_GUI.T.MarkExistingOnDiskAsFinishedAction_working_on(link.getName()));
                                if (FinalLinkState.CheckFinished(link.getFinalLinkState())) {
                                    /* Already marked as finished. */
                                    skipped++;
                                    continue;
                                }
                                try {
                                    final String fileOutput = link.getFileOutput();
                                    if (StringUtils.isEmpty(fileOutput)) {
                                        errors++;
                                        continue;
                                    }
                                    if (fileExists(fileOutput, existsCache, dirUsableCache)) {
                                        toMark.add(link);
                                        marked++;
                                    }
                                } catch (final Throwable t) {
                                    t.printStackTrace();
                                    errors++;
                                }
                            }
                        } finally {
                            for (final DownloadLink link : toMark) {
                                markFinished(link);
                            }
                        }
                        if (canceled || Thread.currentThread().isInterrupted()) {
                            /* User already closed the dialog during the scan: do not start the countdown. */
                            return;
                        }
                        /*
                         * Scan finished: switch to the auto-close countdown. The dialog stays open (progress is kept below 100% so the
                         * ProgressDialog does not dispose itself) until the timeout elapses or the user clicks the Close button. Set the
                         * deadline before flipping the phase flag so a concurrent reader never sees closeDeadline == 0.
                         */
                        workingOn.set(_GUI.T.MarkExistingOnDiskAsFinishedAction_working_done());
                        closeDeadline = System.currentTimeMillis() + AUTO_CLOSE_TIMEOUT;
                        scanning = false;
                        while (!Thread.currentThread().isInterrupted()) {
                            final long remaining = closeDeadline - System.currentTimeMillis();
                            if (remaining <= 0) {
                                break;
                            }
                            try {
                                Thread.sleep(Math.min(200, remaining));
                            } catch (final InterruptedException e) {
                                /* User clicked Close during the countdown. */
                                Thread.currentThread().interrupt();
                                break;
                            }
                        }
                        /* Returning from run() lets the ProgressDialog dispose itself. */
                    }

                    /**
                     * Checks whether the given file path exists, using both a per-file existence cache and a per-directory cache. When
                     * the parent directory is missing or empty, the result is derived from the (cached) directory listing without an
                     * extra {@link File#exists()} call.
                     */
                    private boolean fileExists(final String fileOutput, final HashMap<String, Boolean> existsCache, final HashMap<String, Boolean> dirUsableCache) {
                        final Boolean cachedExists = existsCache.get(fileOutput);
                        if (cachedExists != null) {
                            return cachedExists.booleanValue();
                        }
                        final File file = new File(fileOutput);
                        final File parent = file.getParentFile();
                        boolean exists;
                        if (parent == null) {
                            exists = file.exists();
                        } else {
                            final String parentPath = parent.getAbsolutePath();
                            Boolean dirUsable = dirUsableCache.get(parentPath);
                            if (dirUsable == null) {
                                final String[] entries = parent.list();
                                dirUsable = Boolean.valueOf(entries != null && entries.length > 0);
                                dirUsableCache.put(parentPath, dirUsable);
                            }
                            if (dirUsable.booleanValue()) {
                                exists = file.exists();
                            } else {
                                /* Parent directory is missing or empty -> the file cannot exist. */
                                exists = false;
                            }
                        }
                        existsCache.put(fileOutput, Boolean.valueOf(exists));
                        return exists;
                    }

                    @Override
                    public String getString() {
                        if (scanning) {
                            return _GUI.T.MarkExistingOnDiskAsFinishedAction_progress_status(current, total, marked, skipped, errors);
                        } else {
                            final long remaining = closeDeadline - System.currentTimeMillis();
                            final int seconds = (int) Math.max(0, (remaining + 999) / 1000);
                            return _GUI.T.MarkExistingOnDiskAsFinishedAction_progress_status_done(marked, skipped, errors, seconds);
                        }
                    }

                    @Override
                    public int getProgress() {
                        if (!scanning) {
                            /*
                             * Countdown phase: drain the bar from the remaining time towards 0. Never return >= 100 here, otherwise the
                             * ProgressDialog would dispose itself immediately instead of waiting for the timeout / Close button.
                             */
                            final long remaining = closeDeadline - System.currentTimeMillis();
                            if (remaining <= 0) {
                                return 0;
                            }
                            return (int) Math.min(99, (remaining * 99) / AUTO_CLOSE_TIMEOUT);
                        }
                        if (total == 0) {
                            return -1;
                        }
                        /* Cap at 99% so a completed scan does not auto-close the dialog before the countdown starts. */
                        return Math.min(99, (current * 100) / total);
                    }

                    @Override
                    public String getLabelString() {
                        return null;
                    }
                };
                final ProgressDialog pg = new ProgressDialog(getter, 0, _GUI.T.MarkExistingOnDiskAsFinishedAction_progress_title(), _GUI.T.MarkExistingOnDiskAsFinishedAction_progress_msg(selection.size()), NewTheme.I().getIcon(IconKey.ICON_TRUE, 32), null, _AWU.T.lit_close()) {
                    @Override
                    protected void updateText(final JProgressBar bar, final ProgressGetter getter) {
                        super.updateText(bar, getter);
                        /* Show the file we are currently working on above the progress bar (updated live). */
                        final String working = workingOn.get();
                        if (working != null && textField != null && !working.equals(textField.getText())) {
                            textField.setText(working);
                        }
                    }
                };
                try {
                    Dialog.getInstance().showDialog(pg);
                } catch (DialogNoAnswerException e) {
                    e.printStackTrace();
                }
            }
        }.start();
    }
}
