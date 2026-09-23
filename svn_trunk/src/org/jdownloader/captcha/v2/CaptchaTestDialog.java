package org.jdownloader.captcha.v2;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.Timer;

import org.appwork.swing.MigPanel;
import org.appwork.uio.UIOManager;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.dialog.AbstractDialog;
import org.appwork.utils.swing.dialog.Dialog;
import org.jdownloader.captcha.v2.solverjob.ResponseList;
import org.jdownloader.captcha.v2.solverjob.SolverJob;
import org.jdownloader.logging.LogController;

/**
 * Cancelable progress dialog for the "Test Captcha solver" column (IDE-only, see {@code CaptchaSolverCaptchaTypesSettingsPanelBuilder}):
 * runs a throwaway test {@link Challenge} (see {@link CaptchaTestChallengeFactory}) through the normal solving pipeline
 * ({@link ChallengeResponseController#handle(Challenge)}) on a background thread, shows the result once available, and lets the user tell
 * the winning solver whether the answer was actually correct.
 */
public class CaptchaTestDialog<T> extends AbstractDialog<Void> {
    /** Builds a fresh, single-use test challenge, e.g. for a "Retry" action. May return null if no test data is available (anymore). */
    public interface ChallengeFactory<T> {
        Challenge<T> newChallenge();
    }

    private final ChallengeFactory<T> challengeFactory;
    private volatile Challenge<T>     challenge;
    private volatile long             challengeId;
    private volatile long             startTime = System.currentTimeMillis();
    private volatile SolverJob<T>     finishedJob;
    private volatile Thread           solverThread;
    private JProgressBar              progress;
    private JLabel                    statusLabel;
    private JLabel                    elapsedLabel;
    private JTextArea                 resultArea;
    private JButton                   correctButton;
    private JButton                   incorrectButton;
    private JButton                   abortButton;
    private JButton                   retryButton;
    private Timer                     elapsedTimer;

    /**
     * Starts and shows a test dialog, building the first challenge via {@code challengeFactory.newChallenge()}. Does nothing if the factory
     * is null or its first challenge is null (no test data available for the type).
     */
    public static void showFor(final ChallengeFactory<?> challengeFactory, final String title) {
        if (challengeFactory == null) {
            return;
        }
        showForCaptured(challengeFactory, title);
    }

    /** Wildcard-capture helper: needed to tie the factory/challenge/dialog/job to a single, consistent generic type T. */
    private static <T> void showForCaptured(final ChallengeFactory<T> challengeFactory, final String title) {
        final Challenge<T> challenge = challengeFactory.newChallenge();
        if (challenge == null) {
            return;
        }
        try {
            Dialog.getInstance().showDialog(new CaptchaTestDialog<T>(challengeFactory, challenge, title));
        } catch (final Throwable e) {
            LogController.CL().log(e);
        }
    }

    private CaptchaTestDialog(final ChallengeFactory<T> challengeFactory, final Challenge<T> challenge, final String title) {
        super(UIOManager.BUTTONS_HIDE_OK | UIOManager.BUTTONS_HIDE_CANCEL, title, null, null, null);
        this.challengeFactory = challengeFactory;
        this.challenge = challenge;
        this.challengeId = challenge.getId().getID();
    }

    @Override
    protected Void createReturnValue() {
        return null;
    }

    @Override
    public JComponent layoutDialogContent() {
        final MigPanel panel = new MigPanel("ins 10, wrap 1", "[grow,fill]", "[][][][grow,fill][]");
        progress = new JProgressBar();
        progress.setIndeterminate(true);
        panel.add(progress, "growx");
        statusLabel = new JLabel("Waiting for a solver...");
        panel.add(statusLabel);
        elapsedLabel = new JLabel("Elapsed: 0.0s");
        panel.add(elapsedLabel);
        resultArea = new JTextArea();
        resultArea.setEditable(false);
        resultArea.setLineWrap(true);
        resultArea.setWrapStyleWord(true);
        resultArea.setForeground(Color.RED);
        panel.add(new JScrollPane(resultArea), "height 80:80:200");
        final MigPanel buttonBar = new MigPanel("ins 0", "[][][grow,fill][]", "[]");
        correctButton = new JButton("Correct");
        correctButton.setEnabled(false);
        correctButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                final SolverJob<T> job = finishedJob;
                if (job != null) {
                    job.validate();
                }
                getDialog().dispose();
            }
        });
        incorrectButton = new JButton("Incorrect");
        incorrectButton.setEnabled(false);
        incorrectButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                final SolverJob<T> job = finishedJob;
                if (job != null) {
                    job.invalidate();
                }
                getDialog().dispose();
            }
        });
        abortButton = new JButton("Abort");
        abortButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                getDialog().dispose();
            }
        });
        retryButton = new JButton("Retry");
        retryButton.setEnabled(false);
        retryButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                retry();
            }
        });
        buttonBar.add(correctButton, "sg 1");
        buttonBar.add(incorrectButton, "sg 1");
        buttonBar.add(Box.createHorizontalGlue());
        buttonBar.add(retryButton);
        buttonBar.add(abortButton);
        panel.add(buttonBar, "growx");
        registerEscape(panel);
        startElapsedTimer();
        startSolving();
        return panel;
    }

    /** Builds a fresh challenge via the factory and re-runs the solving pipeline for it, reusing this same dialog. */
    private void retry() {
        final Challenge<T> nextChallenge = challengeFactory.newChallenge();
        if (nextChallenge == null) {
            return;
        }
        challenge = nextChallenge;
        challengeId = nextChallenge.getId().getID();
        finishedJob = null;
        startTime = System.currentTimeMillis();
        retryButton.setEnabled(false);
        correctButton.setEnabled(false);
        incorrectButton.setEnabled(false);
        abortButton.setEnabled(true);
        resultArea.setForeground(Color.RED);
        resultArea.setText("");
        statusLabel.setText("Waiting for a solver...");
        progress.setIndeterminate(true);
        startElapsedTimer();
        startSolving();
    }

    private String formatElapsed() {
        return "Elapsed: " + ((System.currentTimeMillis() - startTime) / 100) / 10d + "s";
    }

    /** Ticks the elapsed-time label while waiting; stopped once solving finishes (see {@link #stopElapsedTimer()}). */
    private void startElapsedTimer() {
        elapsedTimer = new Timer(100, new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                elapsedLabel.setText(formatElapsed());
            }
        });
        elapsedTimer.setRepeats(true);
        elapsedTimer.start();
    }

    private void stopElapsedTimer() {
        if (elapsedTimer != null) {
            elapsedTimer.stop();
        }
        elapsedLabel.setText(formatElapsed());
    }

    private void startSolving() {
        solverThread = new Thread("CaptchaTestDialog") {
            @Override
            public void run() {
                try {
                    final SolverJob<T> job = ChallengeResponseController.getInstance().handle(challenge);
                    finishedJob = job;
                    onSolved();
                } catch (final Throwable e) {
                    onFailed(e);
                }
            }
        };
        solverThread.setDaemon(true);
        solverThread.start();
    }

    private void onSolved() {
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                progress.setIndeterminate(false);
                progress.setValue(100);
                stopElapsedTimer();
                /* Nothing left to abort: either a result already arrived, or the job ran into its timeout/was skipped/killed. */
                abortButton.setEnabled(false);
                retryButton.setEnabled(true);
                final ResponseList<T> result = challenge.getResult();
                if (result != null && result.getValue() != null) {
                    final Object solver = result.get(0).getSolver();
                    statusLabel.setText("Solved by: " + solver);
                    resultArea.setForeground(Color.BLACK);
                    resultArea.setText(String.valueOf(result.getValue()));
                    correctButton.setEnabled(true);
                    incorrectButton.setEnabled(true);
                } else {
                    statusLabel.setText("No answer received (skipped/timed out/killed).");
                    resultArea.setForeground(Color.RED);
                    resultArea.setText("No answer received (skipped/timed out/killed).");
                }
            }
        };
    }

    private void onFailed(final Throwable e) {
        LogController.CL().log(e);
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                progress.setIndeterminate(false);
                stopElapsedTimer();
                abortButton.setEnabled(false);
                retryButton.setEnabled(true);
                statusLabel.setText("Failed: " + e.getMessage());
                resultArea.setForeground(Color.RED);
                resultArea.setText("Failed: " + e.getMessage());
            }
        };
    }

    @Override
    public void dispose() {
        try {
            if (elapsedTimer != null) {
                elapsedTimer.stop();
            }
            final SolverJob<?> live = ChallengeResponseController.getInstance().getJobByChallengeId(challengeId);
            if (live != null) {
                live.kill();
            }
            final Thread thread = solverThread;
            if (thread != null && thread.isAlive()) {
                thread.interrupt();
            }
        } finally {
            super.dispose();
        }
    }
}
