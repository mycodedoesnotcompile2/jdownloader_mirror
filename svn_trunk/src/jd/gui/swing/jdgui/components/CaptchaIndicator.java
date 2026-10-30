package jd.gui.swing.jdgui.components;

import java.awt.Color;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.Timer;

import jd.controlling.captcha.SkipRequest;
import jd.controlling.downloadcontroller.DownloadWatchDog;
import jd.controlling.downloadcontroller.FileNameField;
import jd.controlling.downloadcontroller.PackageNameField;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.Plugin;
import jd.plugins.PluginForHost;

import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.swing.MigPanel;
import org.appwork.swing.components.ExtMergedIcon;
import org.appwork.swing.components.tooltips.ExtTooltip;
import org.appwork.swing.components.tooltips.PanelToolTip;
import org.appwork.swing.components.tooltips.TooltipPanel;
import org.appwork.utils.DebugMode;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.SwingUtils;
import org.jdownloader.DomainInfo;
import org.jdownloader.actions.AppAction;
import org.jdownloader.captcha.event.ChallengeResponseListener;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.ChallengeResponseController;
import org.jdownloader.captcha.v2.ChallengeSolver;
import org.jdownloader.captcha.v2.JobRunnable;
import org.jdownloader.captcha.v2.solverjob.SolverJob;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.images.BadgeIcon;
import org.jdownloader.images.NewTheme;
import org.jdownloader.updatev2.gui.LAFOptions;

public class CaptchaIndicator extends IconedProcessIndicator implements ChallengeResponseListener {
    /**
     *
     */
    private static final long         serialVersionUID = -7267364376253248300L;
    private final SolverJob<?>        job;
    private final StatusBarImpl       statusBar;
    private final Timer               timer;
    private final static AbstractIcon ocr              = new AbstractIcon(IconKey.ICON_OCR, 16);

    public CaptchaIndicator(final StatusBarImpl statusBar, SolverJob<?> job) {
        super(ocr);
        this.job = job;
        this.statusBar = statusBar;
        setTitle(_GUI.T.StatusBarImpl_initGUI_captcha());
        final Icon icon = new ExtMergedIcon(getDomainInfo()).add(ocr, 6, 6);
        updatePainter(icon, Color.WHITE, Color.GRAY, Color.WHITE, Color.GREEN, Color.LIGHT_GRAY, Color.GREEN);
        setEnabled(true);
        ChallengeResponseController.getInstance().getEventSender().addListener(this, true);
        timer = new Timer(500, new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                updateLiveRows();
            }
        });
        if (job.isAlive()) {
            /* Endless animation: shows the user that something is going on */
            setIndeterminate(true);
            statusBar.addProcessIndicator(this);
            timer.start();
        }
    }

    /**
     * Returns the point in time when the job is going to run into a timeout, or -1 if there is no timeout. This is the smaller of the
     * challenge timeout and the latest solver timeout: the job ends as soon as one of them is reached. Solvers are assumed to start their
     * timeout together with the job.
     */
    private long getEndTime() {
        long end = -1;
        final long challengeEnd = job.getChallenge().getValidUntil();
        if (challengeEnd > 0) {
            end = challengeEnd;
        }
        final List<? extends ChallengeSolver<?>> solvers = job.getUnfinishedSolvers();
        long solverEnd = -1;
        boolean solverWithoutTimeout = solvers.isEmpty();
        for (final ChallengeSolver<?> solver : solvers) {
            final long solverTimeout = solver.getFinalTimeoutMillis();
            if (solverTimeout <= 0) {
                solverWithoutTimeout = true;
                break;
            }
            solverEnd = Math.max(solverEnd, job.getCreated() + solverTimeout);
        }
        if (!solverWithoutTimeout && solverEnd > 0 && (end < 0 || solverEnd < end)) {
            end = solverEnd;
        }
        return end;
    }

    private int getPercent() {
        final long end = getEndTime();
        final long total = end - job.getCreated();
        if (total <= 0) {
            return -1;
        }
        final long elapsed = Math.max(0, System.currentTimeMillis() - job.getCreated());
        return (int) Math.min(100, elapsed * 100 / total);
    }

    private static String formatTime(final long millis) {
        final long seconds = Math.max(0, millis) / 1000;
        final long rest = seconds % 60;
        return (seconds / 60) + ":" + (rest < 10 ? "0" : "") + rest;
    }

    /** Provides the current text of a tooltip row. */
    private interface TextProvider {
        String getText();
    }

    /** A tooltip row whose text is refreshed while the tooltip is visible. */
    private static final class LiveRow {
        private final JLabel       label;
        private final TextProvider provider;

        private LiveRow(final JLabel label, final TextProvider provider) {
            this.label = label;
            this.provider = provider;
        }

        private void update() {
            final String text = provider.getText();
            if (!text.equals(label.getText())) {
                label.setText(text);
            }
        }
    }

    /* Rows of the most recently created tooltip, refreshed by the timer. Only accessed in the EDT. */
    private List<LiveRow> liveRows = new ArrayList<LiveRow>();

    /** Refreshes the texts of the tooltip. Runs in the EDT. */
    private void updateLiveRows() {
        if (!job.isAlive()) {
            /* Solved, skipped or timed out: do not wait for the job to be finished completely, hide the indicator now */
            removeIndicator();
            return;
        }
        for (final LiveRow row : liveRows) {
            row.update();
        }
        if (solverReorder != null) {
            solverReorder.run();
        }
    }

    private JLabel createTooltipLabel(final String text, final Icon icon) {
        final JLabel label = new JLabel(text);
        label.setForeground(LAFOptions.getInstance().getColorForTooltipForeground());
        if (icon != null) {
            label.setIcon(icon);
        }
        return label;
    }

    private JLabel addTooltipRow(final JPanel panel, final String text, final Icon icon, final boolean indent) {
        final JLabel label = createTooltipLabel(text, icon);
        panel.add(label, indent ? "gapleft 12" : "");
        return label;
    }

    /** Adds a row with a caption followed by a value, which gets the given icon in front of it. */
    private void addIconValueRow(final JPanel panel, final String caption, final String value, final Icon icon) {
        final MigPanel row = new MigPanel("ins 0", "[][]", "[]");
        row.setOpaque(false);
        row.add(createTooltipLabel(caption, null));
        row.add(createTooltipLabel(value, icon));
        panel.add(row);
    }

    /**
     * Adds a labeled name field (file or package name). The width is limited so very long names are cut off by the text field instead of
     * enlarging the tooltip.
     */
    private void addNameFieldRow(final JPanel panel, final String labelText, final JComponent nameField) {
        final MigPanel row = new MigPanel("ins 0", "[][grow,fill]", "[]");
        row.setOpaque(false);
        final JLabel label = new JLabel(labelText);
        label.setForeground(LAFOptions.getInstance().getColorForTooltipForeground());
        row.add(label);
        row.add(nameField, "growx, wmax 320");
        panel.add(row, "wmax 420");
    }

    private void addLiveRow(final JPanel panel, final List<LiveRow> rows, final Icon icon, final boolean indent, final TextProvider provider) {
        rows.add(new LiveRow(addTooltipRow(panel, provider.getText(), icon, indent), provider));
    }

    /**
     * Returns how long the given solver still waits for the other solvers before it starts, mirroring the wait logic in
     * {@link JobRunnable}: it waits for every other unfinished solver in turn, but all waits count from the same start, so the longest one
     * decides. Solvers that are already done are not waited for any more. The start of the waiting is approximated by the creation of the
     * job. Returns a value &lt;= 0 if there is no (known) remaining wait.
     */
    private <T> long getRemainingWaitMillis(final SolverJob<T> job, final ChallengeSolver<T> solver) {
        if (job.isForcedStart(solver)) {
            return 0;
        }
        long wait = 0;
        for (final ChallengeSolver<T> other : job.getSolverList()) {
            if (other == solver || job.isDone(other)) {
                continue;
            }
            final int waitForOther = JobRunnable.getWaitFor(solver.getService(), other.getService(), job);
            /* JobRunnable ignores waits of one second or less */
            if (waitForOther > 1000) {
                wait = Math.max(wait, waitForOther);
            }
        }
        return wait - (System.currentTimeMillis() - job.getCreated());
    }

    /** A solver together with its position in the sorted solver list. */
    private static final class SolverSortEntry<T> {
        private final ChallengeSolver<T> solver;
        private final long              key;

        private SolverSortEntry(final ChallengeSolver<T> solver, final long key) {
            this.solver = solver;
            this.key = key;
        }
    }

    /**
     * Returns the solvers in display order: the solvers that are solving first, then the waiting ones with the least remaining wait time
     * first, finished solvers last. Solvers with an equal position keep their original order.
     */
    private <T> List<ChallengeSolver<T>> getSortedSolvers(final SolverJob<T> job) {
        final List<SolverSortEntry<T>> entries = new ArrayList<SolverSortEntry<T>>();
        for (final ChallengeSolver<T> solver : job.getSolverList()) {
            final long key;
            if (job.isDone(solver)) {
                key = Long.MAX_VALUE;
            } else if (job.isSolverSolving(solver)) {
                key = -1;
            } else {
                key = Math.max(0, getRemainingWaitMillis(job, solver));
            }
            entries.add(new SolverSortEntry<T>(solver, key));
        }
        Collections.sort(entries, new Comparator<SolverSortEntry<T>>() {
            @Override
            public int compare(final SolverSortEntry<T> o1, final SolverSortEntry<T> o2) {
                return o1.key < o2.key ? -1 : (o1.key == o2.key ? 0 : 1);
            }
        });
        final List<ChallengeSolver<T>> ret = new ArrayList<ChallengeSolver<T>>();
        for (final SolverSortEntry<T> entry : entries) {
            ret.add(entry.solver);
        }
        return ret;
    }

    /* Re-sorts the solver rows of the most recently created tooltip. Only accessed in the EDT. */
    private Runnable solverReorder;

    private <T> void addSolverRows(final JPanel panel, final List<LiveRow> rows, final SolverJob<T> job) {
        final MigPanel solverPanel = new MigPanel("ins 0,wrap 1", "[grow,fill]", "[]");
        solverPanel.setOpaque(false);
        final Map<ChallengeSolver<T>, LiveRow> solverRows = new HashMap<ChallengeSolver<T>, LiveRow>();
        for (final ChallengeSolver<T> solver : job.getSolverList()) {
            final String name = solver.getService().getName();
            Icon solverIcon = null;
            try {
                solverIcon = solver.getService().getIcon(16);
            } catch (final Throwable e) {
                /* Icon is optional */
            }
            final TextProvider provider = new TextProvider() {
                @Override
                public String getText() {
                    if (job.isSolverTimedOut(solver) && job.isDone(solver)) {
                        return _GUI.T.CaptchaIndicator_solver_timedout(name);
                    } else if (job.isDone(solver)) {
                        return _GUI.T.CaptchaIndicator_solver_done(name);
                    } else if (job.isSolverSolving(solver)) {
                        return _GUI.T.CaptchaIndicator_solver_solving(name);
                    } else {
                        final long remaining = getRemainingWaitMillis(job, solver);
                        if (remaining > 0) {
                            /* Round up so the countdown does not show 0:00 while the solver still waits */
                            return _GUI.T.CaptchaIndicator_solver_waiting_countdown(name, formatTime(remaining + 999));
                        }
                        return _GUI.T.CaptchaIndicator_solver_waiting(name);
                    }
                }
            };
            final LiveRow row = new LiveRow(createTooltipLabel(provider.getText(), solverIcon), provider);
            rows.add(row);
            solverRows.put(solver, row);
        }
        final List<ChallengeSolver<T>> currentOrder = new ArrayList<ChallengeSolver<T>>();
        final List<Integer> lastStates = new ArrayList<Integer>();
        solverReorder = new Runnable() {
            @Override
            public void run() {
                /*
                 * Only sort again if the state of a solver changed (waiting -> solving -> done). The countdowns of the waiting solvers change
                 * all the time and would make the rows jump around.
                 */
                final List<Integer> states = new ArrayList<Integer>();
                for (final ChallengeSolver<T> solver : job.getSolverList()) {
                    if (job.isDone(solver)) {
                        states.add(2);
                    } else if (job.isSolverSolving(solver)) {
                        states.add(0);
                    } else {
                        states.add(1);
                    }
                }
                if (!currentOrder.isEmpty() && states.equals(lastStates)) {
                    return;
                }
                lastStates.clear();
                lastStates.addAll(states);
                final List<ChallengeSolver<T>> sorted = getSortedSolvers(job);
                if (sorted.equals(currentOrder)) {
                    return;
                }
                currentOrder.clear();
                currentOrder.addAll(sorted);
                solverPanel.removeAll();
                for (final ChallengeSolver<T> solver : sorted) {
                    solverPanel.add(solverRows.get(solver).label, "gapleft 12");
                }
                solverPanel.revalidate();
                solverPanel.repaint();
            }
        };
        solverReorder.run();
        panel.add(solverPanel);
    }

    @Override
    public ExtTooltip createExtTooltip(final Point mousePosition) {
        final TooltipPanel rows = new TooltipPanel("ins 0,wrap 1", "[grow,fill]", "[]");
        final JLabel title = new JLabel(getTitle());
        title.setForeground(LAFOptions.getInstance().getColorForTooltipForeground());
        SwingUtils.toBold(title);
        rows.add(title);
        final String host = getHost();
        if (host != null) {
            Icon hostIcon = null;
            try {
                hostIcon = getDomainInfo().getIcon(16);
            } catch (final Throwable e) {
                /* Icon is optional */
            }
            addIconValueRow(rows, _GUI.T.CaptchaIndicator_plugin(), host, hostIcon);
        }
        final Challenge<?> challenge = job.getChallenge();
        final CAPTCHA_TYPE type = CAPTCHA_TYPE.getCaptchaTypeForChallenge(challenge);
        final String typeName;
        Icon typeIcon = null;
        if (type != null) {
            typeName = type.getDisplayName();
            try {
                typeIcon = type.getIcon();
            } catch (final Throwable e) {
                /* Icon is optional */
            }
        } else if (challenge.getTypeID() != null) {
            typeName = challenge.getTypeID();
        } else {
            typeName = challenge.getClass().getSimpleName();
        }
        addIconValueRow(rows, _GUI.T.CaptchaIndicator_type(), typeName, typeIcon);
        addTooltipRow(rows, _GUI.T.CaptchaIndicator_origin(getContext().getLabel()), null, false);
        final String fileName = getFileName();
        switch (getContext()) {
        case ACCOUNT_CHECK:
            /* The "file name" of an account check is the account, which has neither an extension nor a package. */
            if (fileName != null) {
                addIconValueRow(rows, _GUI.T.CaptchaIndicator_account(), fileName, null);
            }
            break;
        case CRAWLER:
        case DOWNLOAD:
        default:
            if (fileName != null) {
                addNameFieldRow(rows, _GUI.T.CaptchaIndicator_filename(), new FileNameField(fileName, false));
            }
            final String packageName = getPackageName();
            if (packageName != null) {
                addNameFieldRow(rows, _GUI.T.CaptchaIndicator_package(), new PackageNameField(packageName));
            }
            break;
        }
        final List<LiveRow> newLiveRows = new ArrayList<LiveRow>();
        addLiveRow(rows, newLiveRows, null, false, new TextProvider() {
            @Override
            public String getText() {
                final long elapsed = System.currentTimeMillis() - job.getCreated();
                final int percent = getPercent();
                if (percent < 0) {
                    return _GUI.T.CaptchaIndicator_time_unlimited(formatTime(elapsed));
                }
                final long total = getEndTime() - job.getCreated();
                return _GUI.T.CaptchaIndicator_time(formatTime(elapsed), formatTime(total), percent + "%");
            }
        });
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            addLiveRow(rows, newLiveRows, null, false, new TextProvider() {
                @Override
                public String getText() {
                    return "Debug: Open captcha jobs: " + ChallengeResponseController.getInstance().listJobs().size();
                }
            });
        }
        addTooltipRow(rows, _GUI.T.CaptchaIndicator_solvers(), null, false);
        addSolverRows(rows, newLiveRows, job);
        liveRows = newLiveRows;
        return new PanelToolTip(rows);
    }

    protected String getHost() {
        return job.getChallenge().getHost();
    }

    protected DomainInfo getDomainInfo() {
        return job.getChallenge().getDomainInfo();
    }

    protected String getFileName() {
        final DownloadLink link = job.getChallenge().getDownloadLink();
        return link != null ? link.getName() : null;
    }

    protected String getPackageName() {
        final Plugin plugin = job.getChallenge().getPlugin();
        if (plugin instanceof PluginForHost) {
            final DownloadLink link = ((PluginForHost) plugin).getDownloadLink();
            if (link != null && !FilePackage.isDefaultFilePackage(link.getFilePackage())) {
                return link.getFilePackage().getName();
            }
        }
        return null;
    }

    /** The kind of action that asked for the captcha. It decides which skip actions make sense. */
    private enum CaptchaContext implements LabelInterface {
        DOWNLOAD {
            @Override
            public String getLabel() {
                return _GUI.T.CaptchaIndicator_origin_download();
            }
        },
        CRAWLER {
            @Override
            public String getLabel() {
                return _GUI.T.CaptchaIndicator_origin_crawler();
            }
        },
        ACCOUNT_CHECK {
            @Override
            public String getLabel() {
                return _GUI.T.CaptchaIndicator_origin_accountcheck();
            }
        }
    }

    private CaptchaContext getContext() {
        final Challenge<?> challenge = job.getChallenge();
        if (challenge.isCreatedInsideAccountChecker()) {
            return CaptchaContext.ACCOUNT_CHECK;
        }
        if (challenge.getPlugin() instanceof PluginForHost) {
            return CaptchaContext.DOWNLOAD;
        }
        return CaptchaContext.CRAWLER;
    }

    private void addSkipAction(final JPopupMenu popup, final String name, final Icon icon, final SkipRequest skipRequest) {
        popup.add(new AppAction() {
            private static final long serialVersionUID = 1L;
            {
                setName(name);
                setSmallIcon(icon);
            }

            @Override
            public void actionPerformed(final ActionEvent e) {
                skip(skipRequest);
            }
        });
    }

    private void addSkipHostAction(final JPopupMenu popup) {
        Icon icon;
        try {
            icon = getDomainInfo().getIcon(16);
        } catch (final Throwable e) {
            icon = new AbstractIcon(IconKey.ICON_CANCEL, 16);
        }
        addSkipAction(popup, _GUI.T.AbstractCaptchaDialog_createPopup_skip_and_disable_all_downloads_from(getHost()), icon, SkipRequest.BLOCK_HOSTER);
    }

    protected JPopupMenu createPopup() {
        final JPopupMenu popup = new JPopupMenu();
        switch (getContext()) {
        case ACCOUNT_CHECK:
            /* Skipping the captcha already aborts the account check, so there is nothing else to offer. */
            addSkipAction(popup, _GUI.T.AbstractCaptchaDialog_createPopup_skip_account_check(), new AbstractIcon(IconKey.ICON_SKIPPED, 16), SkipRequest.SINGLE);
            break;
        case CRAWLER:
            addSkipAction(popup, _GUI.T.AbstractCaptchaDialog_AbstractCaptchaDialog_cancel(), new AbstractIcon(IconKey.ICON_SKIPPED, 16), SkipRequest.SINGLE);
            addSkipHostAction(popup);
            /* There are no packages while crawling: BLOCK_PACKAGE affects all captchas of the same crawl job (origin link). */
            addSkipAction(popup, _GUI.T.AbstractCaptchaDialog_createPopup_skip_crawl_job(), new BadgeIcon(IconKey.ICON_LINKGRABBER, IconKey.ICON_SKIPPED, 16), SkipRequest.BLOCK_PACKAGE);
            addSkipAction(popup, _GUI.T.AbstractCaptchaDialog_createPopup_cancel_linkgrabbing(), new AbstractIcon(IconKey.ICON_STOP, 16), SkipRequest.STOP_CURRENT_ACTION);
            break;
        case DOWNLOAD:
        default:
            addSkipAction(popup, _GUI.T.AbstractCaptchaDialog_AbstractCaptchaDialog_cancel(), new AbstractIcon(IconKey.ICON_SKIPPED, 16), SkipRequest.SINGLE);
            addSkipHostAction(popup);
            final String packageName = getPackageName();
            if (packageName != null) {
                addSkipAction(popup, _GUI.T.AbstractCaptchaDialog_createPopup_skip_and_disable_package(packageName), new BadgeIcon(IconKey.ICON_PACKAGE_OPEN, IconKey.ICON_SKIPPED, 16), SkipRequest.BLOCK_PACKAGE);
            }
            addSkipAction(popup, _GUI.T.AbstractCaptchaDialog_createPopup_skip_and_hide_all_captchas_download(), NewTheme.I().getIcon(IconKey.ICON_CLEAR, 16), SkipRequest.BLOCK_ALL_CAPTCHAS);
            if (DownloadWatchDog.getInstance().isRunning()) {
                addSkipAction(popup, _GUI.T.AbstractCaptchaDialog_createPopup_skip_and_stop_all_downloads(), new AbstractIcon(IconKey.ICON_STOP, 16), SkipRequest.STOP_CURRENT_ACTION);
            }
            break;
        }
        return popup;
    }

    @Override
    public void mouseReleased(MouseEvent e) {
        if (e.isPopupTrigger() || e.getButton() == MouseEvent.BUTTON3) {
            final JPopupMenu popup = createPopup();
            popup.show(CaptchaIndicator.this, e.getPoint().x, 0 - popup.getPreferredSize().height);
        }
    }

    @Override
    public void onNewJobAnswer(SolverJob<?> job, AbstractResponse<?> response) {
    }

    @Override
    public void onJobDone(final SolverJob<?> job) {
        if (job == this.job) {
            new EDTRunner() {
                @Override
                protected void runInEDT() {
                    removeIndicator();
                }
            };
        }
    }

    /** Removes this indicator from the status bar right away. Runs in the EDT. */
    private void removeIndicator() {
        timer.stop();
        setIndeterminate(false);
        statusBar.removeProcessIndicator(this);
    }

    /**
     * Skips the job and hides the indicator immediately instead of waiting for the job to be finished by the
     * {@link ChallengeResponseController}.
     */
    private void skip(final SkipRequest skipRequest) {
        job.setSkipRequest(skipRequest);
        removeIndicator();
    }

    @Override
    public void onNewJob(SolverJob<?> job) {
    }

    @Override
    public void onJobSolverEnd(ChallengeSolver<?> solver, SolverJob<?> job) {
    }

    @Override
    public void onJobSolverStart(ChallengeSolver<?> solver, SolverJob<?> job) {
    }
}
