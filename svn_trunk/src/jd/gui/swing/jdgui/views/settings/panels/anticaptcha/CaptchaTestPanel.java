package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.Color;
import java.awt.BasicStroke;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.List;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;

import javax.swing.Box;
import javax.swing.DefaultListCellRenderer;
import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.Timer;
import javax.swing.filechooser.FileNameExtensionFilter;

import org.appwork.swing.MigPanel;
import org.appwork.utils.StringUtils;
import org.appwork.utils.swing.EDTRunner;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.Challenge.CaptchaRequestType;
import org.jdownloader.captcha.v2.ChallengeResponseController;
import org.jdownloader.captcha.v2.ChallengeSolver;
import org.jdownloader.captcha.v2.challenge.multiclickcaptcha.MultiClickedPoint;
import org.jdownloader.captcha.v2.solverjob.ResponseList;
import org.jdownloader.captcha.v2.solverjob.SolverJob;
import org.jdownloader.captcha.v2.test.CaptchaTestChallengeFactory;
import org.jdownloader.captcha.v2.test.CaptchaTestParameters;
import org.jdownloader.logging.LogController;

import jd.plugins.CaptchaType.CAPTCHA_TYPE;

/**
 * IDE-only "Test &amp; Debug" tab of the captcha settings: lets the developer pick the captcha source (download/login/crawler captcha) and
 * the captcha type to test, edit the site URL/site key (pre-filled with the known test values, see {@link CaptchaTestParameters}) and run a
 * real throwaway test {@link Challenge} (see {@link CaptchaTestChallengeFactory}) through the normal solving pipeline
 * ({@link ChallengeResponseController#handle(Challenge)}) on a background thread. The result is shown below the form; if an expected result
 * is given, the solver's answer is compared against it and the matching feedback button is disabled (a correct answer cannot be reported as
 * incorrect and vice versa).
 */
public class CaptchaTestPanel extends MigPanel {
    private static final long          serialVersionUID = 1L;
    private final JComboBox            sourceBox;
    private final JComboBox            typeBox;
    private final JLabel               siteUrlLabel;
    private final JTextField           siteUrlField;
    private final JLabel               siteKeyLabel;
    private final JTextField           siteKeyField;
    private final JLabel               imageLabel;
    private final MigPanel             imagePanel;
    private final JLabel               imagePreview;
    /* Original captcha image of the preview, the clicked positions of the result and whether it is shown in full size. */
    private BufferedImage              previewSource;
    private final List<Point>          resultMarkers    = new ArrayList<Point>();
    private boolean                    previewExpanded  = false;
    /* All captcha images are scaled into a box of this size unless they are expanded by a click. */
    private static final int           PREVIEW_WIDTH    = 320;
    private static final int           PREVIEW_HEIGHT   = 200;
    private File                       imageFile;
    private final JButton              startButton;
    private final JLabel               actionLabel;
    private final JTextField           actionField;
    private final JLabel               minScoreLabel;
    private final JCheckBox            minScoreCheckBox;
    private final JSpinner             minScoreSpinner;
    private final JLabel               minClicksLabel;
    private final JSpinner             minClicksSpinner;
    private final JTextField           expectedResultField;
    private final MigPanel             runPanel;
    private final JProgressBar         progress;
    private final JLabel               statusLabel;
    private final JLabel               elapsedLabel;
    private final JTextArea            resultArea;
    private final JButton              correctButton;
    private final JButton              incorrectButton;
    private final JButton              abortButton;
    private final JButton              retryButton;
    private Timer                      elapsedTimer;
    /* Snapshot of the form values of the currently running/last test, so that "Retry" is not affected by later form edits. */
    private CAPTCHA_TYPE               runType;
    private CaptchaRequestType         runSource;
    private CaptchaTestParameters      runParameters;
    private volatile Challenge<Object> challenge;
    private volatile SolverJob<Object> finishedJob;
    private volatile Thread            solverThread;
    private volatile long              startTime        = System.currentTimeMillis();

    public CaptchaTestPanel() {
        super("ins 10, wrap 2", "[][grow,fill]", "[]");
        sourceBox = new JComboBox(CaptchaRequestType.values());
        sourceBox.setRenderer(new DefaultListCellRenderer() {
            private static final long serialVersionUID = 1L;

            @Override
            public Component getListCellRendererComponent(final JList list, final Object value, final int index, final boolean isSelected, final boolean cellHasFocus) {
                return super.getListCellRendererComponent(list, ((CaptchaRequestType) value).getLabel(), index, isSelected, cellHasFocus);
            }
        });
        typeBox = new JComboBox();
        for (final CAPTCHA_TYPE type : CAPTCHA_TYPE.values()) {
            if (type.hasTestChallenges()) {
                typeBox.addItem(type);
            }
        }
        typeBox.setRenderer(new DefaultListCellRenderer() {
            private static final long serialVersionUID = 1L;

            @Override
            public Component getListCellRendererComponent(final JList list, final Object value, final int index, final boolean isSelected, final boolean cellHasFocus) {
                return super.getListCellRendererComponent(list, ((CAPTCHA_TYPE) value).getDisplayName(), index, isSelected, cellHasFocus);
            }
        });
        siteUrlField = new JTextField();
        siteKeyField = new JTextField();
        actionField = new JTextField();
        expectedResultField = new JTextField();
        typeBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                applyDefaults();
            }
        });
        add(new JLabel("Captcha source"));
        add(sourceBox, "growx");
        add(new JLabel("Captcha type"));
        add(typeBox, "growx");
        siteUrlLabel = new JLabel("Site URL");
        add(siteUrlLabel);
        add(siteUrlField, "growx");
        siteKeyLabel = new JLabel("Site key");
        add(siteKeyLabel);
        add(siteKeyField, "growx");
        /* Image captcha types: the captcha image can be replaced by an own one. */
        imageLabel = new JLabel("Captcha image");
        imagePanel = new MigPanel("ins 0, wrap 1", "[grow,fill]", "[]");
        final JButton chooseImageButton = new JButton("Choose own image...");
        chooseImageButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                chooseImage();
            }
        });
        imagePreview = new JLabel();
        imagePreview.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        imagePreview.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(final MouseEvent e) {
                /* Click: full size, next click: back to the fixed size. */
                previewExpanded = !previewExpanded;
                renderPreview();
            }
        });
        imagePanel.add(chooseImageButton, "alignx left");
        imagePanel.add(imagePreview);
        add(imageLabel, "aligny top");
        add(imagePanel, "growx");
        actionLabel = new JLabel("Action (reCAPTCHA v3)");
        add(actionLabel);
        add(actionField, "growx");
        minScoreLabel = new JLabel("Min score (reCAPTCHA v3)");
        minScoreCheckBox = new JCheckBox("Set");
        /* Default: no min score is requested. */
        minScoreSpinner = new JSpinner(new SpinnerNumberModel(0.5d, 0.1d, 0.9d, 0.1d));
        minScoreSpinner.setEnabled(false);
        minScoreCheckBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                minScoreSpinner.setEnabled(minScoreCheckBox.isSelected());
            }
        });
        final MigPanel minScorePanel = new MigPanel("ins 0", "[][]", "[]");
        minScorePanel.add(minScoreCheckBox);
        minScorePanel.add(minScoreSpinner);
        add(minScoreLabel);
        add(minScorePanel, "growx");
        minClicksLabel = new JLabel("Min clicks (multi click captcha)");
        minClicksSpinner = new JSpinner(new SpinnerNumberModel(8, 1, 100, 1));
        add(minClicksLabel);
        add(minClicksSpinner, "growx");
        add(new JLabel("Expected result"));
        add(expectedResultField, "growx");
        startButton = new JButton("Start Test");
        startButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                startTest();
            }
        });
        add(startButton, "skip 1, alignx left");
        /* Run area (the former test dialog): hidden until the first test was started. */
        runPanel = new MigPanel("ins 0, wrap 1", "[grow,fill]", "[][][][grow,fill][]");
        progress = new JProgressBar();
        progress.setIndeterminate(true);
        runPanel.add(progress, "growx");
        statusLabel = new JLabel();
        runPanel.add(statusLabel);
        elapsedLabel = new JLabel();
        runPanel.add(elapsedLabel);
        resultArea = new JTextArea();
        resultArea.setEditable(false);
        resultArea.setLineWrap(true);
        resultArea.setWrapStyleWord(true);
        runPanel.add(new JScrollPane(resultArea), "height 80:80:200");
        correctButton = new JButton("Correct");
        correctButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                final SolverJob<Object> job = finishedJob;
                if (job != null) {
                    job.validate();
                }
                correctButton.setEnabled(false);
                incorrectButton.setEnabled(false);
            }
        });
        incorrectButton = new JButton("Incorrect");
        incorrectButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                final SolverJob<Object> job = finishedJob;
                if (job != null) {
                    job.invalidate();
                }
                correctButton.setEnabled(false);
                incorrectButton.setEnabled(false);
            }
        });
        abortButton = new JButton("Abort");
        abortButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                abort();
            }
        });
        retryButton = new JButton("Retry");
        retryButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                runTest();
            }
        });
        final MigPanel buttonBar = new MigPanel("ins 0", "[][][grow,fill][]", "[]");
        buttonBar.add(correctButton, "sg 1");
        buttonBar.add(incorrectButton, "sg 1");
        buttonBar.add(Box.createHorizontalGlue());
        buttonBar.add(retryButton);
        buttonBar.add(abortButton);
        runPanel.add(buttonBar, "growx");
        runPanel.setVisible(false);
        add(runPanel, "span 2, growx, pushy, aligny top");
        applyDefaults();
    }

    /** Fills the site URL/site key/action/expected result fields with the pre-filled test values of the selected captcha type. */
    private void applyDefaults() {
        final CAPTCHA_TYPE type = (CAPTCHA_TYPE) typeBox.getSelectedItem();
        final CaptchaTestParameters defaults = CaptchaTestParameters.getDefaults(type);
        siteUrlField.setText(defaults.getSiteUrl() == null ? "" : defaults.getSiteUrl());
        siteKeyField.setText(defaults.getSiteKey() == null ? "" : defaults.getSiteKey());
        final boolean usesImage = CaptchaTestParameters.usesImage(type);
        siteUrlLabel.setEnabled(!usesImage);
        siteUrlField.setEnabled(!usesImage);
        siteKeyLabel.setEnabled(!usesImage);
        siteKeyField.setEnabled(!usesImage);
        imageLabel.setVisible(usesImage);
        imagePanel.setVisible(usesImage);
        setImageFile(defaults.getImageFile());
        actionField.setText(defaults.getAction() == null ? "" : defaults.getAction());
        expectedResultField.setText(defaults.getExpectedResult() == null ? "" : defaults.getExpectedResult());
        final boolean usesAction = CaptchaTestParameters.usesAction(type);
        actionLabel.setEnabled(usesAction);
        actionField.setEnabled(usesAction);
        final boolean usesMinScore = CaptchaTestParameters.usesMinScore(type);
        minScoreLabel.setEnabled(usesMinScore);
        minScoreCheckBox.setSelected(false);
        minScoreCheckBox.setEnabled(usesMinScore);
        minScoreSpinner.setEnabled(false);
        final boolean usesMinClicks = CaptchaTestParameters.usesMinClicks(type);
        minClicksLabel.setEnabled(usesMinClicks);
        minClicksSpinner.setEnabled(usesMinClicks);
        minClicksSpinner.setValue(defaults.getMinClicks());
    }

    /** Image captcha types cannot be tested without a readable image -> "Start Test" is disabled in that case. */
    private void updateStartButtonEnabled() {
        final CAPTCHA_TYPE type = (CAPTCHA_TYPE) typeBox.getSelectedItem();
        startButton.setEnabled(!CaptchaTestParameters.usesImage(type) || (imageFile != null && imageFile.isFile()));
    }

    /** Shows the given captcha image (or an error text if it cannot be read) in the preview. */
    private void setImageFile(final File file) {
        imageFile = file;
        previewSource = null;
        previewExpanded = false;
        resultMarkers.clear();
        updateStartButtonEnabled();
        imagePreview.setIcon(null);
        imagePreview.setText(null);
        if (file == null || !file.isFile()) {
            imagePreview.setText(file == null ? "No image" : "Image not found: " + file);
            return;
        }
        try {
            final BufferedImage image = ImageIO.read(file);
            if (image == null) {
                imagePreview.setText("Not a readable image: " + file);
            } else {
                previewSource = image;
                renderPreview();
            }
        } catch (final IOException e) {
            imagePreview.setText("Cannot read image: " + e.getMessage());
        }
    }

    /**
     * Shows the captcha image in the preview: scaled into the fixed box (see PREVIEW_WIDTH/PREVIEW_HEIGHT) or in full size if expanded. The
     * clicked positions of the result (if any) are marked with a big red X, in the coordinates of the original image.
     */
    private void renderPreview() {
        final BufferedImage source = previewSource;
        if (source == null) {
            return;
        }
        BufferedImage image = source;
        if (!resultMarkers.isEmpty()) {
            image = new BufferedImage(source.getWidth(), source.getHeight(), BufferedImage.TYPE_INT_ARGB);
            final Graphics2D g = image.createGraphics();
            try {
                g.drawImage(source, 0, 0, null);
                g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                final int size = Math.max(12, Math.min(source.getWidth(), source.getHeight()) / 15);
                g.setStroke(new BasicStroke(Math.max(3, size / 4), BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
                g.setColor(Color.RED);
                for (final Point p : resultMarkers) {
                    g.drawLine(p.x - size, p.y - size, p.x + size, p.y + size);
                    g.drawLine(p.x - size, p.y + size, p.x + size, p.y - size);
                }
            } finally {
                g.dispose();
            }
        }
        if (!previewExpanded) {
            final double scale = Math.min((double) PREVIEW_WIDTH / source.getWidth(), (double) PREVIEW_HEIGHT / source.getHeight());
            final int width = Math.max(1, (int) Math.round(source.getWidth() * scale));
            final int height = Math.max(1, (int) Math.round(source.getHeight() * scale));
            final BufferedImage scaled = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
            final Graphics2D g = scaled.createGraphics();
            try {
                g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);
                g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
                g.drawImage(image, 0, 0, width, height, null);
            } finally {
                g.dispose();
            }
            image = scaled;
        }
        imagePreview.setIcon(new ImageIcon(image));
        imagePreview.setToolTipText(imageFile != null ? imageFile.getAbsolutePath() + " (click to " + (previewExpanded ? "shrink" : "enlarge") + ")" : null);
        imagePanel.revalidate();
        revalidate();
        repaint();
    }

    /** Lets the user pick an own captcha image. The pre-filled expected result belongs to the default image, so it is cleared. */
    private void chooseImage() {
        final JFileChooser chooser = new JFileChooser(imageFile != null ? imageFile.getParentFile() : null);
        chooser.setFileFilter(new FileNameExtensionFilter("Images (png, jpg, gif)", "png", "jpg", "jpeg", "gif"));
        if (chooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
            setImageFile(chooser.getSelectedFile());
            expectedResultField.setText("");
        }
    }

    private void startTest() {
        runType = (CAPTCHA_TYPE) typeBox.getSelectedItem();
        runSource = (CaptchaRequestType) sourceBox.getSelectedItem();
        runParameters = new CaptchaTestParameters(siteKeyField.getText().trim(), siteUrlField.getText().trim(), actionField.getText().trim(), expectedResultField.getText());
        runParameters.setImageFile(imageFile);
        runParameters.setMinClicks(((Number) minClicksSpinner.getValue()).intValue());
        if (CaptchaTestParameters.usesMinScore(runType) && minScoreCheckBox.isSelected()) {
            /* Round to one decimal: the spinner steps in 0.1 and floating point noise must not end up in the request. */
            runParameters.setMinScore(Math.round(((Number) minScoreSpinner.getValue()).doubleValue() * 10d) / 10d);
        }
        runTest();
    }

    /** Builds a fresh challenge from the form values snapshot taken in {@link #startTest()} and runs it through the solving pipeline. */
    private void runTest() {
        stopRunning();
        /* Remove the markers of a previous result. */
        resultMarkers.clear();
        renderPreview();
        /* The answer type depends on the captcha type (token, text, click positions, ...). */
        @SuppressWarnings("unchecked")
        final Challenge<Object> newChallenge = (Challenge<Object>) CaptchaTestChallengeFactory.newChallenge(runType, runSource, runParameters);
        runPanel.setVisible(true);
        resultArea.setForeground(Color.RED);
        if (newChallenge == null) {
            progress.setIndeterminate(false);
            statusLabel.setText("Could not build a test challenge for " + runType.getDisplayName() + " (missing test data/image?)");
            elapsedLabel.setText("");
            resultArea.setText("");
            correctButton.setEnabled(false);
            incorrectButton.setEnabled(false);
            abortButton.setEnabled(false);
            retryButton.setEnabled(true);
            revalidate();
            return;
        }
        challenge = newChallenge;
        finishedJob = null;
        startTime = System.currentTimeMillis();
        correctButton.setEnabled(false);
        incorrectButton.setEnabled(false);
        retryButton.setEnabled(false);
        abortButton.setEnabled(true);
        resultArea.setText("");
        statusLabel.setText("Waiting for a solver...");
        elapsedLabel.setText(formatElapsed());
        progress.setIndeterminate(true);
        startElapsedTimer();
        startSolving(newChallenge);
        revalidate();
    }

    private String formatElapsed() {
        return "Elapsed: " + ((System.currentTimeMillis() - startTime) / 100) / 10d + "s";
    }

    /** Ticks the elapsed-time label and the "currently solving" status while waiting; stopped once solving finishes. */
    private void startElapsedTimer() {
        elapsedTimer = new Timer(100, new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                elapsedLabel.setText(formatElapsed());
                updateActiveSolversStatus();
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

    /** Shows which solver(s) are still working on the challenge and their {@link ChallengeSolver#getFinalTimeoutMillis()}, while waiting. */
    private void updateActiveSolversStatus() {
        final Challenge<Object> current = challenge;
        if (finishedJob != null || current == null) {
            /* Solving already finished -> onSolved()/onFailed() own the status label from here on. */
            return;
        }
        final SolverJob<?> job = ChallengeResponseController.getInstance().getJobByChallengeId(current.getId().getID());
        if (job != null) {
            updateActiveSolversStatus(job);
        }
    }

    private <X> void updateActiveSolversStatus(final SolverJob<X> job) {
        final StringBuilder sb = new StringBuilder();
        for (final ChallengeSolver<X> solver : job.getSolverList()) {
            if (job.isDone(solver)) {
                continue;
            }
            if (sb.length() > 0) {
                sb.append(", ");
            }
            sb.append(solver).append(" (timeout: ").append(formatTimeout(solver.getFinalTimeoutMillis())).append(")");
        }
        if (sb.length() > 0) {
            statusLabel.setText("Solving with: " + sb);
        }
    }

    /** {@link ChallengeSolver#getFinalTimeoutMillis()} returns -1 for "no timeout", and a millisecond delay otherwise. */
    private static String formatTimeout(final long timeoutMillis) {
        return timeoutMillis <= 0 ? "unlimited" : (timeoutMillis / 1000L) + "s";
    }

    private void startSolving(final Challenge<Object> currentChallenge) {
        final Thread thread = new Thread("CaptchaTestPanel") {
            @Override
            public void run() {
                try {
                    final SolverJob<Object> job = ChallengeResponseController.getInstance().handle(currentChallenge);
                    if (currentChallenge != challenge) {
                        /* Aborted/superseded by a newer test in the meantime. */
                        return;
                    }
                    finishedJob = job;
                    onSolved(currentChallenge);
                } catch (final Throwable e) {
                    if (currentChallenge == challenge) {
                        onFailed(e);
                    }
                }
            }
        };
        thread.setDaemon(true);
        solverThread = thread;
        thread.start();
    }

    private void onSolved(final Challenge<Object> solvedChallenge) {
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                progress.setIndeterminate(false);
                progress.setValue(100);
                stopElapsedTimer();
                /* Nothing left to abort: either a result already arrived, or the job ran into its timeout/was skipped/killed. */
                abortButton.setEnabled(false);
                retryButton.setEnabled(true);
                final ResponseList<Object> result = solvedChallenge.getResult();
                if (result != null && result.getValue() != null) {
                    final String answer = formatAnswer(result.getValue());
                    final String expected = runParameters.getExpectedResult();
                    statusLabel.setText("Solved by: " + result.get(0).getSolver());
                    if (result.getValue() instanceof MultiClickedPoint) {
                        /* Mark all clicked positions in the image. */
                        final MultiClickedPoint points = (MultiClickedPoint) result.getValue();
                        resultMarkers.clear();
                        for (int i = 0; i < points.getX().length; i++) {
                            resultMarkers.add(new Point(points.getX()[i], points.getY()[i]));
                        }
                        renderPreview();
                    }
                    resultArea.setText(answer);
                    if (StringUtils.isEmpty(expected)) {
                        resultArea.setForeground(Color.BLACK);
                        correctButton.setEnabled(true);
                        incorrectButton.setEnabled(true);
                    } else if (expected.equals(answer)) {
                        /* A correct answer cannot be reported as incorrect. */
                        statusLabel.setText(statusLabel.getText() + " | matches the expected result");
                        resultArea.setForeground(new Color(0, 128, 0));
                        correctButton.setEnabled(true);
                        incorrectButton.setEnabled(false);
                    } else {
                        /* A wrong answer cannot be reported as correct. */
                        statusLabel.setText(statusLabel.getText() + " | does NOT match the expected result");
                        resultArea.setForeground(Color.RED);
                        resultArea.setText(answer + "\r\n\r\nExpected: " + expected);
                        correctButton.setEnabled(false);
                        incorrectButton.setEnabled(true);
                    }
                } else {
                    statusLabel.setText("No answer received (skipped/timed out/killed).");
                    resultArea.setForeground(Color.RED);
                    resultArea.setText("No answer received (skipped/timed out/killed).");
                }
            }
        };
    }

    /** Click captchas answer with positions, everything else with a string. */
    private static String formatAnswer(final Object value) {
        if (value instanceof MultiClickedPoint) {
            final MultiClickedPoint points = (MultiClickedPoint) value;
            final StringBuilder sb = new StringBuilder();
            for (int i = 0; i < points.getX().length; i++) {
                if (sb.length() > 0) {
                    sb.append("; ");
                }
                sb.append(points.getX()[i]).append("x").append(points.getY()[i]);
            }
            return sb.toString();
        }
        return String.valueOf(value);
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

    /** Kills a still running test job (if any) and interrupts its solver thread. */
    private void stopRunning() {
        if (elapsedTimer != null) {
            elapsedTimer.stop();
        }
        final Challenge<Object> current = challenge;
        challenge = null;
        if (current != null) {
            final SolverJob<?> live = ChallengeResponseController.getInstance().getJobByChallengeId(current.getId().getID());
            if (live != null) {
                live.kill();
            }
        }
        final Thread thread = solverThread;
        if (thread != null && thread.isAlive()) {
            thread.interrupt();
        }
    }

    private void abort() {
        stopRunning();
        progress.setIndeterminate(false);
        stopElapsedTimer();
        statusLabel.setText("Aborted.");
        abortButton.setEnabled(false);
        correctButton.setEnabled(false);
        incorrectButton.setEnabled(false);
        retryButton.setEnabled(true);
    }
}
