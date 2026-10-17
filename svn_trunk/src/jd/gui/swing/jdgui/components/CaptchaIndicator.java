package jd.gui.swing.jdgui.components;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.Timer;

import jd.controlling.captcha.SkipRequest;
import jd.controlling.downloadcontroller.FileNameField;
import jd.controlling.downloadcontroller.PackageNameField;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.DownloadLink;
import jd.plugins.Plugin;
import jd.plugins.PluginForHost;

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
        setDescription(_GUI.T.gui_captchaWindow_waitForInput(job.getChallenge().getHost()));
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
     * Returns the solvers of the given job which are not done yet. Generic helper because {@link SolverJob#isDone(ChallengeSolver)} needs
     * the type parameter of the job.
     */
    private static <T> List<ChallengeSolver<?>> getUnfinishedSolvers(final SolverJob<T> job) {
        final List<ChallengeSolver<?>> ret = new ArrayList<ChallengeSolver<?>>();
        for (final ChallengeSolver<T> solver : job.getSolverList()) {
            if (!job.isDone(solver)) {
                ret.add(solver);
            }
        }
        return ret;
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
        final List<ChallengeSolver<?>> solvers = getUnfinishedSolvers(job);
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
            timer.stop();
            return;
        }
        for (final LiveRow row : liveRows) {
            row.update();
        }
    }

    private JLabel addTooltipRow(final JPanel panel, final String text, final Icon icon, final boolean indent) {
        final JLabel label = new JLabel(text);
        label.setForeground(LAFOptions.getInstance().getColorForTooltipForeground());
        if (icon != null) {
            label.setIcon(icon);
        }
        panel.add(label, indent ? "gapleft 12" : "");
        return label;
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

    private <T> void addSolverRows(final JPanel panel, final List<LiveRow> rows, final SolverJob<T> job) {
        for (final ChallengeSolver<T> solver : job.getSolverList()) {
            final String name = solver.getService().getName();
            Icon solverIcon = null;
            try {
                solverIcon = solver.getService().getIcon(16);
            } catch (final Throwable e) {
                /* Icon is optional */
            }
            addLiveRow(panel, rows, solverIcon, true, new TextProvider() {
                @Override
                public String getText() {
                    if (job.isSolverTimedOut(solver) && job.isDone(solver)) {
                        return _GUI.T.CaptchaIndicator_solver_timedout(name);
                    } else if (job.isDone(solver)) {
                        return _GUI.T.CaptchaIndicator_solver_done(name);
                    } else if (job.isSolverSolving(solver)) {
                        return _GUI.T.CaptchaIndicator_solver_solving(name);
                    } else {
                        return _GUI.T.CaptchaIndicator_solver_waiting(name);
                    }
                }
            });
        }
    }

    @Override
    public ExtTooltip createExtTooltip(final Point mousePosition) {
        final IconedProcessIndicator comp = new IconedProcessIndicator();
        comp.valuePainter = valuePainter;
        comp.nonValuePainter = nonValuePainter;
        comp.activeValuePainter = activeValuePainter;
        comp.activeNonValuePainter = activeNonValuePainter;
        comp.setEnabled(isEnabled());
        comp.setIndeterminate(isIndeterminate());
        comp.setActive(isActive());
        comp.setPreferredSize(new Dimension(32, 32));
        comp.setOpaque(false);
        final TooltipPanel panel = new TooltipPanel("ins 0", "[][grow,fill]", "[grow,fill]");
        final TooltipPanel rows = new TooltipPanel("ins 0,wrap 1", "[grow,fill]", "[]");
        final JLabel title = new JLabel(getTitle());
        title.setForeground(LAFOptions.getInstance().getColorForTooltipForeground());
        SwingUtils.toBold(title);
        rows.add(title);
        addTooltipRow(rows, getDescription(), null, false);
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
        addTooltipRow(rows, _GUI.T.CaptchaIndicator_type(typeName), typeIcon, false);
        final String fileName = getFileName();
        if (fileName != null) {
            addNameFieldRow(rows, _GUI.T.CaptchaIndicator_filename(), new FileNameField(fileName, false));
        }
        final String packageName = getPackageName();
        if (packageName != null) {
            addNameFieldRow(rows, _GUI.T.CaptchaIndicator_package(), new PackageNameField(packageName));
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
        panel.add(comp, "aligny top");
        panel.add(rows);
        return new PanelToolTip(panel);
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
            if (link != null) {
                return link.getFilePackage().getName();
            }
        }
        return null;
    }

    protected JPopupMenu createPopup() {
        final JPopupMenu popup = new JPopupMenu();
        popup.add(new AppAction() {
            private static final long serialVersionUID = -968768342263254431L;
            {
                this.setIconKey(IconKey.ICON_CANCEL);
                this.setName(_GUI.T.AbstractCaptchaDialog_AbstractCaptchaDialog_cancel());
            }

            public void actionPerformed(ActionEvent e) {
                job.setSkipRequest(SkipRequest.SINGLE);
            }
        });
        final Plugin plugin = job.getChallenge().getPlugin();
        if (plugin instanceof PluginForHost) {
            popup.add(new AppAction() {
                {
                    setName(_GUI.T.AbstractCaptchaDialog_createPopup_skip_and_disable_all_downloads_from(getHost()));
                    try {
                        setSmallIcon(getDomainInfo().getIcon(16));
                    } catch (final Throwable e) {
                        this.setIconKey(IconKey.ICON_CANCEL);
                    }
                }

                @Override
                public void actionPerformed(ActionEvent e) {
                    job.setSkipRequest(SkipRequest.BLOCK_HOSTER);
                }
            });
            popup.add(new AppAction() {
                {
                    setName(_GUI.T.AbstractCaptchaDialog_createPopup_skip_and_disable_package(getPackageName()));
                    setSmallIcon(new BadgeIcon(IconKey.ICON_PACKAGE_OPEN, IconKey.ICON_SKIPPED, 16));
                }

                @Override
                public void actionPerformed(ActionEvent e) {
                    job.setSkipRequest(SkipRequest.BLOCK_PACKAGE);
                }
            });
            popup.add(new AppAction() {
                {
                    setName(_GUI.T.AbstractCaptchaDialog_createPopup_skip_and_hide_all_captchas_download());
                    setSmallIcon(NewTheme.I().getIcon(IconKey.ICON_CLEAR, 16));
                }

                @Override
                public void actionPerformed(ActionEvent e) {
                    job.setSkipRequest(SkipRequest.BLOCK_ALL_CAPTCHAS);
                }
            });
            popup.add(new AppAction() {
                {
                    setName(_GUI.T.AbstractCaptchaDialog_createPopup_skip_and_stop_all_downloads());
                    setSmallIcon(new AbstractIcon(IconKey.ICON_STOP, 16));
                }

                @Override
                public void actionPerformed(ActionEvent e) {
                    job.setSkipRequest(SkipRequest.STOP_CURRENT_ACTION);
                }
            });
        } else {
            popup.add(new AppAction() {
                {
                    setName(_GUI.T.AbstractCaptchaDialog_createPopup_cancel_linkgrabbing());
                    setSmallIcon(new AbstractIcon(IconKey.ICON_STOP, 16));
                }

                @Override
                public void actionPerformed(ActionEvent e) {
                    job.setSkipRequest(SkipRequest.STOP_CURRENT_ACTION);
                }
            });
            popup.add(new AppAction() {
                {
                    setName(_GUI.T.AbstractCaptchaDialog_createPopup_cancel_stop_showing_crawlercaptchs());
                    setSmallIcon(new AbstractIcon(IconKey.ICON_FIND, 16));
                }

                @Override
                public void actionPerformed(ActionEvent e) {
                    job.setSkipRequest(SkipRequest.BLOCK_ALL_CAPTCHAS);
                }
            });
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
                    timer.stop();
                    setIndeterminate(false);
                    statusBar.removeProcessIndicator(CaptchaIndicator.this);
                }
            };
        }
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
