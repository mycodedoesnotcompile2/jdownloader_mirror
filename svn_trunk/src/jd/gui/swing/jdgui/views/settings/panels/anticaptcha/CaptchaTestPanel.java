package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.imageio.ImageIO;
import javax.swing.Box;
import javax.swing.DefaultListCellRenderer;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JList;
import javax.swing.JPopupMenu;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingConstants;
import javax.swing.Timer;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellRenderer;

import org.appwork.swing.MigPanel;
import org.appwork.utils.StringUtils;
import org.appwork.utils.swing.EDTRunner;
import org.jdownloader.captcha.event.ChallengeResponseListener;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.Challenge.CaptchaRequestType;
import org.jdownloader.captcha.v2.ChallengeResponseController;
import org.jdownloader.captcha.v2.ChallengeSolver;
import org.jdownloader.captcha.v2.ChallengeSolver.FeedbackType;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.captcha.v2.challenge.multiclickcaptcha.MultiClickedPoint;
import org.jdownloader.captcha.v2.solverjob.ResponseList;
import org.jdownloader.captcha.v2.solverjob.SolverJob;
import org.jdownloader.captcha.v2.test.CaptchaTestChallengeFactory;
import org.jdownloader.captcha.v2.test.CaptchaTestParameters;
import org.jdownloader.gui.IconKey;
import org.jdownloader.images.NewTheme;
import org.jdownloader.logging.LogController;

import jd.controlling.captcha.SkipException;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;

/**
 * IDE-only "Test &amp; Debug" tab of the captcha settings: lets the developer pick the captcha source (download/login/crawler captcha) and
 * the captcha type to test, edit the site URL/site key (pre-filled with the known test values, see {@link CaptchaTestParameters}) and run
 * real throwaway test {@link Challenge}s (see {@link CaptchaTestChallengeFactory}) through the normal solving pipeline
 * ({@link ChallengeResponseController#handle(Challenge)}) on a background thread. <br>
 * Every test (a single test is just a table with one row, a multi test runs all selected captcha types of one solver one after another)
 * is shown as a row of the result table. Clicking a row shows its details below the table: progress, status, elapsed time, result and the
 * feedback/abort/retry buttons. If an expected result is given, the solver's answer is compared against it and the matching feedback
 * button is disabled (a correct answer cannot be reported as incorrect and vice versa).
 */
public class CaptchaTestPanel extends MigPanel {
    private static final long           serialVersionUID  = 1L;
    private static final Color          COLOR_ORANGE      = new Color(230, 126, 0);
    private final JComboBox             sourceBox;
    private final JComboBox             typeBox;
    private final JLabel                siteUrlLabel;
    private final JTextField            siteUrlField;
    private final JLabel                siteKeyLabel;
    private final JTextField            siteKeyField;
    private final JLabel                imageLabel;
    private final MigPanel              imagePanel;
    private final JLabel                imagePreview;
    private final JLabel                imageResolutionLabel;
    /* Original captcha image of the preview, the clicked positions of the result and whether it is shown in full size. */
    private BufferedImage               previewSource;
    private final List<Point>           resultMarkers     = new ArrayList<Point>();
    private boolean                     previewExpanded   = false;
    /* All captcha images are scaled into a box of this size unless they are expanded by a click. */
    private static final int            PREVIEW_WIDTH     = 320;
    private static final int            PREVIEW_HEIGHT    = 200;
    private File                        imageFile;
    private final JButton               startButton;
    private final JLabel                descriptionLabel;
    private final JTextField            descriptionField;
    private final JLabel                caseSensitiveLabel;
    private final JComboBox             caseSensitiveBox;
    private final JLabel                actionLabel;
    private final JTextField            actionField;
    private final JLabel                minScoreLabel;
    private final JCheckBox             minScoreCheckBox;
    private final JSpinner              minScoreSpinner;
    private final JLabel                minClicksLabel;
    private final JSpinner              minClicksSpinner;
    private final JTextField            expectedResultField;
    private final JComboBox             solverBox;
    private final JButton               typesButton;
    private final JPopupMenu            typesPopup;
    /* First entry of the popup: selects/deselects all captcha types. */
    private final JCheckBox             selectAllBox      = new JCheckBox("Select/Deselect all");
    private final List<JCheckBox>       typeCheckBoxes    = new ArrayList<JCheckBox>();
    private final List<CAPTCHA_TYPE>    typeCheckBoxTypes = new ArrayList<CAPTCHA_TYPE>();
    private final JButton               multiStartButton;
    private final JButton               multiCancelButton;
    /* Result table: one row per test. */
    private final RunTableModel         runModel          = new RunTableModel();
    private final JLabel                batchLabel        = new JLabel();
    private volatile List<TestRun>      batchRuns         = new ArrayList<TestRun>();
    private volatile long               batchStartTime;
    private final JTable                runTable;
    private final JScrollPane           runScroll;
    /* Details of the test selected in the table. */
    private final MigPanel              runPanel;
    private final JProgressBar          progress;
    private final JLabel                statusLabel;
    private final JLabel                elapsedLabel;
    private final JTextArea             resultArea;
    private final JButton               correctButton;
    private final JButton               incorrectButton;
    private final JButton               abortButton;
    private final JButton               retryButton;
    private Timer                       refreshTimer;
    private final Color                 defaultButtonForeground;
    private volatile TestRun            selectedRun;
    /* The tests of the current/last batch are executed one after another by this thread. */
    private volatile Thread             batchThread;
    private volatile boolean            batchRunning      = false;
    private volatile boolean            batchCancelled    = false;
    /*
     * Reads the timeout of a test as soon as its job exists (before the periodic refresh could), so that even very fast tests have a
     * progress percentage.
     */
    private final ChallengeResponseListener jobListener = new ChallengeResponseListener() {
        @Override
        public void onNewJob(final SolverJob<?> job) {
            for (final TestRun run : runModel.getRuns()) {
                if (run.challenge != null && run.challenge == job.getChallenge()) {
                    run.maxTimeoutMillis = getMaxTimeoutMillis(job);
                }
            }
        }

        @Override
        public void onNewJobAnswer(final SolverJob<?> job, final AbstractResponse<?> response) {
        }

        @Override
        public void onJobDone(final SolverJob<?> job) {
        }

        @Override
        public void onJobSolverEnd(final ChallengeSolver<?> solver, final SolverJob<?> job) {
        }

        @Override
        public void onJobSolverStart(final ChallengeSolver<?> solver, final SolverJob<?> job) {
        }
    };

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
        imageResolutionLabel = new JLabel();
        imagePanel.add(imageResolutionLabel, "alignx left");
        add(imageLabel, "aligny top");
        add(imagePanel, "growx");
        descriptionLabel = new JLabel("Challenge Description");
        descriptionField = new JTextField();
        add(descriptionLabel);
        add(descriptionField, "growx");
        caseSensitiveLabel = new JLabel("Case sensitive (image captcha)");
        caseSensitiveBox = new JComboBox(new String[] { "Unset", "true", "false" });
        add(caseSensitiveLabel);
        add(caseSensitiveBox, "growx");
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
        startButton = new JButton("Start Single Test");
        startButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                startSingleTest();
            }
        });
        add(startButton, "skip 1, alignx left");
        /* Multi test: run all selected captcha types of one solver one after another. */
        /* Solvers that are not ready (no account, disabled, ...) are listed greyed out and cannot be selected. */
        solverBox = new JComboBox() {
            private static final long serialVersionUID = 1L;

            @Override
            public void setSelectedItem(final Object anObject) {
                if (anObject instanceof SolverService && !isSelectable((SolverService) anObject)) {
                    return;
                }
                super.setSelectedItem(anObject);
            }
        };
        SolverService firstReadyService = null;
        for (final SolverService service : ChallengeResponseController.getInstance().listServices()) {
            solverBox.addItem(service);
            if (firstReadyService == null && isSelectable(service)) {
                firstReadyService = service;
            }
        }
        if (firstReadyService != null) {
            solverBox.setSelectedItem(firstReadyService);
        }
        solverBox.setRenderer(new DefaultListCellRenderer() {
            private static final long serialVersionUID = 1L;

            @Override
            public Component getListCellRendererComponent(final JList list, final Object value, final int index, final boolean isSelected, final boolean cellHasFocus) {
                final SolverService service = (SolverService) value;
                final boolean ready = service == null || isSelectable(service);
                final Component comp = super.getListCellRendererComponent(list, service == null ? "" : service.getName(), index, isSelected && ready, cellHasFocus);
                if (!ready) {
                    comp.setEnabled(false);
                    comp.setForeground(Color.GRAY);
                }
                return comp;
            }
        });
        typesPopup = new JPopupMenu();
        selectAllBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                for (final JCheckBox box : typeCheckBoxes) {
                    box.setSelected(selectAllBox.isSelected());
                }
                updateTypesButtonText();
            }
        });
        typesButton = new JButton();
        typesButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                typesPopup.show(typesButton, 0, typesButton.getHeight());
            }
        });
        multiStartButton = new JButton("Start Multi Test");
        multiStartButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                startMultiTest();
            }
        });
        multiCancelButton = new JButton("Cancel all");
        multiCancelButton.setEnabled(false);
        multiCancelButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                cancelBatch();
            }
        });
        solverBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                rebuildTypeList();
            }
        });
        add(new JLabel("<html>Choose solver to test. You can only choose solvers with status \"Ready\", they must be activated in the \"Solver overview\" tab and if it is a paid solver, an active and enabled account must be present.</html>"), "span 2, growx, wmin 0");
        add(new JLabel("Multi test: Solver"));
        add(solverBox, "growx");
        add(new JLabel("Multi test: Captcha types"));
        add(typesButton, "growx");
        final MigPanel multiButtons = new MigPanel("ins 0", "[][]", "[]");
        multiButtons.add(multiStartButton);
        multiButtons.add(multiCancelButton);
        add(multiButtons, "skip 1, alignx left");
        /* Result table: hidden until the first test was started. */
        runTable = new JTable(runModel);
        runTable.setRowHeight(22);
        runTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        runTable.getColumnModel().getColumn(0).setPreferredWidth(200);
        runTable.getColumnModel().getColumn(1).setPreferredWidth(100);
        runTable.getColumnModel().getColumn(2).setPreferredWidth(80);
        runTable.getColumnModel().getColumn(3).setPreferredWidth(400);
        runTable.setDefaultRenderer(CAPTCHA_TYPE.class, new TableCellRenderer() {
            private final JLabel label = new JLabel();

            @Override
            public Component getTableCellRendererComponent(final JTable table, final Object value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                final CAPTCHA_TYPE type = (CAPTCHA_TYPE) value;
                label.setOpaque(true);
                label.setBackground(isSelected ? table.getSelectionBackground() : table.getBackground());
                label.setForeground(isSelected ? table.getSelectionForeground() : table.getForeground());
                label.setHorizontalAlignment(SwingConstants.LEFT);
                label.setIcon(type.getIcon());
                label.setText(type.getDisplayName());
                return label;
            }
        });
        runTable.setDefaultRenderer(RunState.class, new TableCellRenderer() {
            private final JLabel label = new JLabel();

            @Override
            public Component getTableCellRendererComponent(final JTable table, final Object value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                final RunState state = (RunState) value;
                label.setOpaque(true);
                label.setBackground(isSelected ? table.getSelectionBackground() : table.getBackground());
                label.setHorizontalAlignment(SwingConstants.LEFT);
                label.setIcon(state.getIcon());
                label.setText(state.getText());
                label.setForeground(state.getColor());
                return label;
            }
        });
        /* Progress column: elapsed time in percent of the solvers' timeout (100% = timeout fully elapsed). */
        runTable.getColumnModel().getColumn(2).setCellRenderer(new TableCellRenderer() {
            private final JProgressBar bar = new JProgressBar(0, 100);

            @Override
            public Component getTableCellRendererComponent(final JTable table, final Object value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                final TestRun run = (TestRun) value;
                final long duration = getDurationMillis(run);
                final int percent = getProgressPercent(run);
                bar.setValue(Math.max(percent, 0));
                bar.setStringPainted(duration >= 0);
                bar.setString(duration < 0 ? "" : (percent >= 0 ? percent + "% (" : "(") + (duration / 100 / 10d) + "s)");
                return bar;
            }
        });
        /* Answer column: a "Cancel" button while the test is still waiting/running, the answer text afterwards. */
        runTable.getColumnModel().getColumn(3).setCellRenderer(new TableCellRenderer() {
            private final JLabel  label  = new JLabel();
            private final JButton button = new JButton("Cancel");

            @Override
            public Component getTableCellRendererComponent(final JTable table, final Object value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                final TestRun run = runModel.getRun(row);
                if (isCancellable(run)) {
                    return button;
                }
                label.setOpaque(true);
                label.setBackground(isSelected ? table.getSelectionBackground() : table.getBackground());
                label.setForeground(isSelected ? table.getSelectionForeground() : table.getForeground());
                label.setText(value == null ? "" : value.toString());
                return label;
            }
        });
        runTable.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(final MouseEvent e) {
                if (e.getButton() != MouseEvent.BUTTON1) {
                    return;
                }
                final int row = runTable.rowAtPoint(e.getPoint());
                if (row >= 0 && runTable.columnAtPoint(e.getPoint()) == 3) {
                    final TestRun run = runModel.getRun(row);
                    if (isCancellable(run)) {
                        abortRun(run, false);
                    }
                }
            }

            @Override
            public void mousePressed(final MouseEvent e) {
                showContextMenu(e);
            }

            @Override
            public void mouseReleased(final MouseEvent e) {
                showContextMenu(e);
            }
        });
        runTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
            @Override
            public void valueChanged(final ListSelectionEvent e) {
                if (!e.getValueIsAdjusting()) {
                    final int row = runTable.getSelectedRow();
                    selectedRun = row < 0 ? null : runModel.getRun(row);
                    syncMarkers();
                    refreshDetails();
                }
            }
        });
        runScroll = new JScrollPane(runTable);
        runScroll.setPreferredSize(new Dimension(400, 150));
        runScroll.setVisible(false);
        batchLabel.setVisible(false);
        add(batchLabel, "span 2");
        add(runScroll, "span 2, growx");
        /* Details of the selected test (the former test dialog): hidden until a test is selected. */
        runPanel = new MigPanel("ins 0, wrap 1", "[grow,fill]", "[][][][grow,fill][][]");
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
                sendFeedback(true);
            }
        });
        incorrectButton = new JButton("Incorrect");
        incorrectButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                sendFeedback(false);
            }
        });
        abortButton = new JButton("Abort");
        abortButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                final TestRun run = selectedRun;
                if (run != null) {
                    abortRun(run, false);
                }
            }
        });
        retryButton = new JButton("Retry");
        retryButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                final TestRun run = selectedRun;
                if (run != null) {
                    run.reset();
                    final List<TestRun> runs = new ArrayList<TestRun>();
                    runs.add(run);
                    startBatch(runs, false);
                }
            }
        });
        final MigPanel buttonBar = new MigPanel("ins 0", "[][][grow,fill][]", "[]");
        buttonBar.add(correctButton, "sg 1");
        buttonBar.add(incorrectButton, "sg 1");
        buttonBar.add(Box.createHorizontalGlue());
        buttonBar.add(retryButton);
        buttonBar.add(abortButton);
        defaultButtonForeground = correctButton.getForeground();
        runPanel.add(new JLabel("Send Captcha Feedback"));
        runPanel.add(buttonBar, "growx");
        runPanel.setVisible(false);
        add(runPanel, "span 2, growx, pushy, aligny top");
        rebuildTypeList();
        applyDefaults();
        /* Solvers can be (de)activated in the other tabs meanwhile -> check the selection whenever this panel is shown again. */
        addHierarchyListener(new HierarchyListener() {
            @Override
            public void hierarchyChanged(final HierarchyEvent e) {
                if ((e.getChangeFlags() & HierarchyEvent.SHOWING_CHANGED) != 0 && isShowing()) {
                    refreshSolverSelection();
                }
            }
        });
    }

    /**
     * Makes sure the selected solver can still be tested: if it was deactivated/is no longer ready, the first usable solver is selected, or
     * nothing if there is none (then no captcha types are listed and "Start Multi Test" is disabled).
     *
     * @return true if the selection is usable
     */
    private boolean refreshSolverSelection() {
        if (batchRunning) {
            return true;
        }
        final SolverService selected = (SolverService) solverBox.getSelectedItem();
        if (selected != null && isSelectable(selected)) {
            return true;
        }
        SolverService replacement = null;
        for (int i = 0; i < solverBox.getItemCount(); i++) {
            final SolverService service = (SolverService) solverBox.getItemAt(i);
            if (isSelectable(service)) {
                replacement = service;
                break;
            }
        }
        /* Fires the combo box listener, which rebuilds the captcha type list. */
        solverBox.setSelectedItem(replacement);
        rebuildTypeList();
        solverBox.repaint();
        return replacement != null;
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
        descriptionLabel.setEnabled(usesImage);
        descriptionField.setEnabled(usesImage);
        descriptionField.setText(defaults.getChallengeDescription() == null ? "" : defaults.getChallengeDescription());
        final boolean usesCaseSensitive = CaptchaTestParameters.usesCaseSensitive(type);
        caseSensitiveLabel.setEnabled(usesCaseSensitive);
        caseSensitiveBox.setEnabled(usesCaseSensitive);
        final Boolean caseSensitive = defaults.getCaseSensitive();
        caseSensitiveBox.setSelectedIndex(caseSensitive == null ? 0 : (caseSensitive.booleanValue() ? 1 : 2));
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
        syncMarkers();
    }

    /** Image captcha types cannot be tested without a readable image -> "Start Single Test" is disabled in that case. */
    private void updateStartButtonEnabled() {
        final CAPTCHA_TYPE type = (CAPTCHA_TYPE) typeBox.getSelectedItem();
        startButton.setEnabled(!batchRunning && (!CaptchaTestParameters.usesImage(type) || (imageFile != null && imageFile.isFile())));
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
        imageResolutionLabel.setText(null);
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
                imageResolutionLabel.setText("Resolution: " + image.getWidth() + " x " + image.getHeight() + " px");
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

    /**
     * Marks the clicked positions of the selected test in the image preview. The preview shows the image of the form, so markers are only
     * shown for a test of the captcha type selected in the form.
     */
    private void syncMarkers() {
        resultMarkers.clear();
        final TestRun run = selectedRun;
        if (run != null && run.type == typeBox.getSelectedItem()) {
            resultMarkers.addAll(run.markers);
        }
        renderPreview();
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

    /** Builds the test parameters from the current form values for the given captcha type. */
    private CaptchaTestParameters buildFormParameters(final CAPTCHA_TYPE type) {
        final CaptchaTestParameters parameters = new CaptchaTestParameters(siteKeyField.getText().trim(), siteUrlField.getText().trim(), actionField.getText().trim(), expectedResultField.getText());
        parameters.setImageFile(imageFile);
        parameters.setChallengeDescription(descriptionField.getText());
        parameters.setMinClicks(((Number) minClicksSpinner.getValue()).intValue());
        switch (caseSensitiveBox.getSelectedIndex()) {
        case 1:
            parameters.setCaseSensitive(Boolean.TRUE);
            break;
        case 2:
            parameters.setCaseSensitive(Boolean.FALSE);
            break;
        default:
            parameters.setCaseSensitive(null);
            break;
        }
        if (CaptchaTestParameters.usesMinScore(type) && minScoreCheckBox.isSelected()) {
            /* Round to one decimal: the spinner steps in 0.1 and floating point noise must not end up in the request. */
            parameters.setMinScore(Math.round(((Number) minScoreSpinner.getValue()).doubleValue() * 10d) / 10d);
        }
        return parameters;
    }

    /** A solver can only be tested if it is ready and activated in the "Solver overview" tab. */
    private static boolean isSelectable(final SolverService service) {
        return service.isReady() && service.getConfigV3().isEnabled();
    }

    /** Rebuilds the captcha type check boxes (all selected) from the types the selected solver supports and has test challenges for. */
    private void rebuildTypeList() {
        typesPopup.removeAll();
        typeCheckBoxes.clear();
        typeCheckBoxTypes.clear();
        typesPopup.add(selectAllBox);
        typesPopup.addSeparator();
        final SolverService service = (SolverService) solverBox.getSelectedItem();
        final List<CAPTCHA_TYPE> supported = service == null || !isSelectable(service) ? null : service.getSupportedCaptchaTypes();
        if (supported != null) {
            for (final CAPTCHA_TYPE type : CAPTCHA_TYPE.values()) {
                if (type.hasTestChallenges() && supported.contains(type)) {
                    final JCheckBox box = new JCheckBox(type.getDisplayName(), true);
                    box.addActionListener(new ActionListener() {
                        @Override
                        public void actionPerformed(final ActionEvent e) {
                            updateTypesButtonText();
                        }
                    });
                    typeCheckBoxes.add(box);
                    typeCheckBoxTypes.add(type);
                    typesPopup.add(box);
                }
            }
        }
        updateTypesButtonText();
    }

    private void updateTypesButtonText() {
        int selected = 0;
        for (final JCheckBox box : typeCheckBoxes) {
            if (box.isSelected()) {
                selected++;
            }
        }
        typesButton.setText(selected + " of " + typeCheckBoxes.size() + " captcha types selected");
        multiStartButton.setEnabled(selected > 0 && !batchRunning);
        /* Checked only if every type is selected. */
        selectAllBox.setSelected(selected > 0 && selected == typeCheckBoxes.size());
        selectAllBox.setEnabled(!typeCheckBoxes.isEmpty());
    }

    private void startSingleTest() {
        final CAPTCHA_TYPE type = (CAPTCHA_TYPE) typeBox.getSelectedItem();
        final List<TestRun> runs = new ArrayList<TestRun>();
        runs.add(new TestRun(type, (CaptchaRequestType) sourceBox.getSelectedItem(), buildFormParameters(type), null));
        startBatch(runs, true);
    }

    /**
     * Runs all selected captcha types of the selected solver one after another. The captcha type selected in the form uses the form values
     * (which are the defaults unless the user changed them), all other types use their default test values. The captcha source always
     * comes from the form.
     */
    private void startMultiTest() {
        final SolverService service = (SolverService) solverBox.getSelectedItem();
        if (service == null || !isSelectable(service)) {
            /* Deactivated meanwhile: update the selection but do not silently test a different solver than the user picked. */
            refreshSolverSelection();
            return;
        }
        final CAPTCHA_TYPE formType = (CAPTCHA_TYPE) typeBox.getSelectedItem();
        final CaptchaRequestType source = (CaptchaRequestType) sourceBox.getSelectedItem();
        final List<TestRun> runs = new ArrayList<TestRun>();
        for (int i = 0; i < typeCheckBoxes.size(); i++) {
            if (typeCheckBoxes.get(i).isSelected()) {
                final CAPTCHA_TYPE type = typeCheckBoxTypes.get(i);
                runs.add(new TestRun(type, source, type == formType ? buildFormParameters(type) : CaptchaTestParameters.getDefaults(type), service));
            }
        }
        if (service == null || runs.isEmpty()) {
            return;
        }
        startBatch(runs, true);
    }

    /**
     * Executes the given tests one after another on a background thread.
     *
     * @param replaceTable
     *            true: the table shows exactly these tests (new single/multi test), false: the tests are already rows of the table (retry)
     */
    private void startBatch(final List<TestRun> runs, final boolean replaceTable) {
        if (replaceTable) {
            runModel.setRuns(runs);
        } else {
            runModel.fireRunsChanged();
        }
        runScroll.setVisible(true);
        batchCancelled = false;
        batchRunning = true;
        batchRuns = runs;
        batchStartTime = System.currentTimeMillis();
        updateBatchLabel();
        setBatchRunningState(true);
        startRefreshTimer();
        ChallengeResponseController.getInstance().getEventSender().addListener(jobListener);
        final Thread thread = new Thread("CaptchaTestPanelBatch") {
            @Override
            public void run() {
                try {
                    for (final TestRun run : runs) {
                        if (run.state == RunState.FAILED && run.aborted) {
                            /* Aborted while it was still waiting for its turn. */
                            continue;
                        }
                        if (batchCancelled) {
                            finishRun(run, RunState.FAILED, "Cancelled", null);
                            continue;
                        }
                        executeRun(run);
                    }
                } finally {
                    batchRunning = false;
                    ChallengeResponseController.getInstance().getEventSender().removeListener(jobListener);
                    new EDTRunner() {
                        @Override
                        protected void runInEDT() {
                            stopRefreshTimer();
                            setBatchRunningState(false);
                            runModel.fireRunsChanged();
                            updateBatchLabel();
                            refreshDetails();
                        }
                    };
                }
            }
        };
        thread.setDaemon(true);
        batchThread = thread;
        if (replaceTable) {
            runTable.setRowSelectionInterval(0, 0);
        }
        refreshDetails();
        thread.start();
    }

    private void setBatchRunningState(final boolean running) {
        solverBox.setEnabled(!running);
        typesButton.setEnabled(!running);
        multiCancelButton.setEnabled(running);
        if (running) {
            multiStartButton.setEnabled(false);
        } else {
            updateTypesButtonText();
        }
        updateStartButtonEnabled();
    }

    /** Builds a fresh challenge from the snapshot of the test and runs it through the solving pipeline. Runs on the batch thread. */
    private void executeRun(final TestRun run) {
        /* Clear an interrupt flag that was meant for the previous test. */
        Thread.interrupted();
        @SuppressWarnings("unchecked")
        final Challenge<Object> newChallenge = (Challenge<Object>) CaptchaTestChallengeFactory.newChallenge(run.type, run.source, run.parameters);
        if (newChallenge == null) {
            run.startTime = System.currentTimeMillis();
            finishRun(run, RunState.FAILED, "Could not build a test challenge for " + run.type.getDisplayName() + " (missing test data/image?)", null);
            return;
        }
        run.challenge = newChallenge;
        run.maxTimeoutMillis = 0;
        run.statusText = "Waiting for a solver...";
        run.startTime = System.currentTimeMillis();
        run.state = RunState.RUNNING;
        runChanged(run, false);
        try {
            final SolverJob<Object> job = ChallengeResponseController.getInstance().handle(newChallenge, run.service);
            run.finishedJob = job;
            final ResponseList<Object> result = newChallenge.getResult();
            if (batchCancelled) {
                finishRun(run, RunState.FAILED, "Cancelled", null);
            } else if (run.aborted) {
                finishRun(run, RunState.FAILED, "Aborted", null);
            } else if (result != null && result.getValue() != null) {
                final String answer = formatAnswer(result.getValue());
                final String expected = run.parameters.getExpectedResult();
                final String solvedBy = "Solved by: " + result.get(0).getSolver();
                /* The solver that answered decides whether it accepts feedback. */
                final Object solver = result.get(0).getSolver();
                if (solver instanceof ChallengeSolver) {
                    final List<FeedbackType> feedbackTypes = ((ChallengeSolver<?>) solver).getSupportedFeedbackTypes();
                    /* null = no restriction declared (same interpretation as PluginChallengeSolver#setValid/setInvalid). */
                    run.correctUnsupported = feedbackTypes != null && !feedbackTypes.contains(FeedbackType.REPORT_VALID_CAPTCHAS);
                    run.incorrectUnsupported = feedbackTypes != null && !feedbackTypes.contains(FeedbackType.REPORT_INVALID_CAPTCHAS);
                }
                if (result.getValue() instanceof MultiClickedPoint) {
                    /* Remember all clicked positions to mark them in the image. */
                    final MultiClickedPoint points = (MultiClickedPoint) result.getValue();
                    for (int i = 0; i < points.getX().length; i++) {
                        run.markers.add(new Point(points.getX()[i], points.getY()[i]));
                    }
                }
                if (StringUtils.isEmpty(expected)) {
                    run.canCorrect = true;
                    run.canIncorrect = true;
                    finishRun(run, RunState.PASSED, answer, solvedBy);
                } else if (expected.equals(answer)) {
                    /* A correct answer cannot be reported as incorrect. */
                    run.canCorrect = true;
                    finishRun(run, RunState.PASSED, answer, solvedBy + " | matches the expected result");
                } else {
                    /* A wrong answer cannot be reported as correct. */
                    run.canIncorrect = true;
                    run.expectedText = expected;
                    finishRun(run, RunState.PASSED_UNEXPECTED_ANSWER, answer, solvedBy + " | does NOT match the expected result");
                }
            } else {
                finishRun(run, RunState.FAILED, "No answer received (skipped/timed out/killed).", null);
            }
        } catch (final InterruptedException e) {
            finishRun(run, RunState.FAILED, batchCancelled ? "Cancelled" : "Aborted", null);
        } catch (final SkipException e) {
            finishRun(run, RunState.FAILED, batchCancelled ? "Cancelled" : (run.aborted ? "Aborted" : "Skipped: " + e.getMessage()), null);
        } catch (final Throwable e) {
            LogController.CL().log(e);
            finishRun(run, RunState.FAILED, batchCancelled ? "Cancelled" : "Failed: " + e.getMessage(), null);
        } finally {
            run.challenge = null;
        }
    }

    /** Stores the final result of a test and updates the GUI. */
    private void finishRun(final TestRun run, final RunState state, final String answer, final String statusText) {
        run.endTime = System.currentTimeMillis();
        run.answer = answer;
        run.statusText = statusText != null ? statusText : (state == RunState.FAILED ? answer : "");
        run.state = state;
        runChanged(run, true);
    }

    /** Updates the table row of the given test and, if it is the selected one, the details. */
    private void runChanged(final TestRun run, final boolean finished) {
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                runModel.fireRunChanged(run);
                if (run == selectedRun) {
                    if (finished) {
                        syncMarkers();
                    }
                    refreshDetails();
                }
            }
        };
    }

    private void startRefreshTimer() {
        stopRefreshTimer();
        refreshTimer = new Timer(100, new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                for (final TestRun run : runModel.getRuns()) {
                    if (run.state == RunState.RUNNING) {
                        updateActiveSolversStatus(run);
                        runModel.fireRunChanged(run);
                    }
                }
                updateBatchLabel();
                refreshDetails();
            }
        });
        refreshTimer.setRepeats(true);
        refreshTimer.start();
    }

    private void stopRefreshTimer() {
        if (refreshTimer != null) {
            refreshTimer.stop();
        }
    }

    /**
     * Elapsed time in percent of the timeout of the solvers (max 100), or -1 if the timeout is unknown/unlimited or the test did not start.
     */
    private static int getProgressPercent(final TestRun run) {
        final long duration = getDurationMillis(run);
        final long timeout = run.maxTimeoutMillis;
        if (duration < 0 || timeout <= 0) {
            return -1;
        }
        return (int) Math.min(100, duration * 100 / timeout);
    }

    /** Progress line above the table of a multi test: "Running test 1/X" while running, the total time when done. */
    private void updateBatchLabel() {
        final List<TestRun> runs = batchRuns;
        batchLabel.setVisible(runModel.getRowCount() > 1);
        if (batchRunning) {
            int started = 0;
            for (final TestRun run : runs) {
                if (run.state != RunState.PENDING) {
                    started++;
                }
            }
            batchLabel.setText("Running test " + Math.max(1, Math.min(started, runs.size())) + "/" + runs.size());
        } else {
            final long elapsed = System.currentTimeMillis() - batchStartTime;
            batchLabel.setText((batchCancelled ? "Tests cancelled" : "Tests completed") + " | Time: " + (elapsed / 100 / 10d) + "s");
        }
    }

    /** True while a test is waiting for its turn or running, i.e. it can still be cancelled. */
    private static boolean isCancellable(final TestRun run) {
        return run.state == RunState.PENDING || run.state == RunState.RUNNING;
    }

    /** Right click on a test: selects it and offers "Cancel". */
    private void showContextMenu(final MouseEvent e) {
        if (!e.isPopupTrigger()) {
            return;
        }
        final int row = runTable.rowAtPoint(e.getPoint());
        if (row < 0) {
            return;
        }
        runTable.setRowSelectionInterval(row, row);
        final TestRun run = runModel.getRun(row);
        final JPopupMenu menu = new JPopupMenu();
        final JMenuItem cancel = new JMenuItem("Cancel");
        cancel.setEnabled(isCancellable(run));
        cancel.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent event) {
                abortRun(run, false);
            }
        });
        menu.add(cancel);
        menu.show(runTable, e.getX(), e.getY());
    }

    /** Shows the state of the selected test (or hides the details if none is selected). */
    private void refreshDetails() {
        final TestRun run = selectedRun;
        runPanel.setVisible(run != null);
        if (run == null) {
            return;
        }
        final RunState state = run.state;
        progress.setIndeterminate(state == RunState.RUNNING);
        progress.setValue(state == RunState.PENDING || state == RunState.RUNNING ? 0 : 100);
        statusLabel.setText(run.statusText);
        elapsedLabel.setText(formatElapsed(run));
        switch (state) {
        case PASSED:
            resultArea.setForeground(Color.BLACK);
            resultArea.setText(run.answer);
            break;
        case PASSED_UNEXPECTED_ANSWER:
            resultArea.setForeground(COLOR_ORANGE);
            resultArea.setText(run.answer + "\r\n\r\nExpected: " + run.expectedText);
            break;
        case FAILED:
            resultArea.setForeground(Color.RED);
            resultArea.setText(run.answer);
            break;
        case PENDING:
        case RUNNING:
        default:
            resultArea.setForeground(Color.BLACK);
            resultArea.setText("");
            break;
        }
        correctButton.setEnabled(run.canCorrect && !run.correctUnsupported);
        incorrectButton.setEnabled(run.canIncorrect && !run.incorrectUnsupported);
        /* Feedback not accepted by the solver: shown disabled with red text. */
        correctButton.setText(run.correctUnsupported ? "Correct (unsupported)" : "Correct");
        correctButton.setForeground(run.correctUnsupported ? Color.RED : defaultButtonForeground);
        incorrectButton.setText(run.incorrectUnsupported ? "Incorrect (unsupported)" : "Incorrect");
        incorrectButton.setForeground(run.incorrectUnsupported ? Color.RED : defaultButtonForeground);
        abortButton.setEnabled(state == RunState.PENDING || state == RunState.RUNNING);
        retryButton.setEnabled(!batchRunning && state != RunState.PENDING && state != RunState.RUNNING);
        runPanel.revalidate();
        revalidate();
    }

    private void sendFeedback(final boolean correct) {
        final TestRun run = selectedRun;
        if (run == null) {
            return;
        }
        final SolverJob<Object> job = run.finishedJob;
        if (job != null) {
            if (correct) {
                job.validate();
            } else {
                job.invalidate();
            }
        }
        run.canCorrect = false;
        run.canIncorrect = false;
        refreshDetails();
    }

    private static String formatElapsed(final TestRun run) {
        if (run.startTime <= 0) {
            return "";
        }
        final long elapsedMillis = getDurationMillis(run);
        final StringBuilder sb = new StringBuilder("Elapsed: ").append(elapsedMillis / 100 / 10d).append("s");
        final long maxMillis = run.maxTimeoutMillis;
        if (maxMillis > 0 && run.state == RunState.RUNNING) {
            sb.append(" (").append(elapsedMillis * 100 / maxMillis).append("% of ").append(maxMillis / 1000L).append("s)");
        }
        return sb.toString();
    }

    /** Duration of a test: until its end, or until now while it is running. */
    private static long getDurationMillis(final TestRun run) {
        final long start = run.startTime;
        if (start <= 0) {
            return -1;
        }
        final long end = run.endTime;
        return (end > 0 ? end : System.currentTimeMillis()) - start;
    }

    /**
     * Longest timeout of all solvers of the job (done or not), or -1 as soon as one of them has no timeout (-> no percentage shown).
     */
    private static <X> long getMaxTimeoutMillis(final SolverJob<X> job) {
        long maxTimeout = 0;
        for (final ChallengeSolver<X> solver : job.getSolverList()) {
            final long solverTimeout = solver.getFinalTimeoutMillis();
            if (solverTimeout <= 0) {
                return -1;
            }
            maxTimeout = Math.max(maxTimeout, solverTimeout);
        }
        return maxTimeout;
    }

    /** Shows which solver(s) are still working on the challenge and their {@link ChallengeSolver#getFinalTimeoutMillis()}, while waiting. */
    private void updateActiveSolversStatus(final TestRun run) {
        final Challenge<Object> current = run.challenge;
        if (run.finishedJob != null || current == null) {
            /* Solving already finished -> finishRun() owns the status from here on. */
            return;
        }
        final SolverJob<?> job = ChallengeResponseController.getInstance().getJobByChallengeId(current.getId().getID());
        if (job != null) {
            updateActiveSolversStatus(run, job);
        }
    }

    private <X> void updateActiveSolversStatus(final TestRun run, final SolverJob<X> job) {
        final StringBuilder sb = new StringBuilder();
        /* Longest timeout of all active solvers; -1 as soon as one of them has no timeout (-> no percentage shown). */
        long maxTimeout = 0;
        for (final ChallengeSolver<X> solver : job.getSolverList()) {
            if (job.isDone(solver)) {
                continue;
            }
            final long solverTimeout = solver.getFinalTimeoutMillis();
            if (solverTimeout <= 0) {
                maxTimeout = -1;
            } else if (maxTimeout >= 0) {
                maxTimeout = Math.max(maxTimeout, solverTimeout);
            }
            if (sb.length() > 0) {
                sb.append(", ");
            }
            sb.append(solver).append(" (timeout: ").append(formatTimeout(solver.getFinalTimeoutMillis())).append(")");
        }
        if (sb.length() > 0) {
            run.maxTimeoutMillis = maxTimeout;
            run.statusText = "Solving with: " + sb;
        }
    }

    /** {@link ChallengeSolver#getFinalTimeoutMillis()} returns -1 for "no timeout", and a millisecond delay otherwise. */
    private static String formatTimeout(final long timeoutMillis) {
        return timeoutMillis <= 0 ? "unlimited" : (timeoutMillis / 1000L) + "s";
    }

    /** Click captchas answer with positions ("[2] 5x2, 5x6"), everything else with a string. */
    private static String formatAnswer(final Object value) {
        if (value instanceof MultiClickedPoint) {
            final MultiClickedPoint points = (MultiClickedPoint) value;
            final StringBuilder sb = new StringBuilder();
            sb.append("[").append(points.getX().length).append("] ");
            for (int i = 0; i < points.getX().length; i++) {
                if (i > 0) {
                    sb.append(", ");
                }
                sb.append(points.getX()[i]).append("x").append(points.getY()[i]);
            }
            return sb.toString();
        }
        return String.valueOf(value);
    }

    /** Kills the live solver job of a running test (if any). */
    private static void killLiveJob(final TestRun run) {
        final Challenge<Object> current = run.challenge;
        if (current != null) {
            final SolverJob<?> live = ChallengeResponseController.getInstance().getJobByChallengeId(current.getId().getID());
            if (live != null) {
                live.kill();
            }
        }
    }

    /**
     * Aborts a single test: a running one is killed (the batch continues with the next test), one that did not start yet is marked as
     * failed right away.
     */
    private void abortRun(final TestRun run, final boolean cancelled) {
        run.aborted = true;
        switch (run.state) {
        case RUNNING:
            killLiveJob(run);
            final Thread thread = batchThread;
            if (thread != null && thread.isAlive()) {
                thread.interrupt();
            }
            break;
        case PENDING:
            finishRun(run, RunState.FAILED, cancelled ? "Cancelled" : "Aborted", null);
            break;
        default:
            break;
        }
    }

    /** Cancels the running test and all tests that did not start yet. */
    private void cancelBatch() {
        batchCancelled = true;
        for (final TestRun run : runModel.getRuns()) {
            abortRun(run, true);
        }
    }

    /** State of one test. */
    private enum RunState {
        PENDING,
        RUNNING,
        PASSED,
        PASSED_UNEXPECTED_ANSWER,
        FAILED;
        String getText() {
            switch (this) {
            case RUNNING:
                return "Running...";
            case PASSED:
            case PASSED_UNEXPECTED_ANSWER:
                return "Passed";
            case FAILED:
                return "Failed";
            case PENDING:
            default:
                return "Pending";
            }
        }

        Color getColor() {
            switch (this) {
            case PASSED_UNEXPECTED_ANSWER:
                return COLOR_ORANGE;
            case FAILED:
                return Color.RED;
            case PASSED:
            case RUNNING:
            case PENDING:
            default:
                return Color.BLACK;
            }
        }

        /** Green check for every received answer, red cross for failed, none while pending/running. */
        javax.swing.Icon getIcon() {
            switch (this) {
            case PASSED:
            case PASSED_UNEXPECTED_ANSWER:
                return NewTheme.I().getIcon(IconKey.ICON_TRUE, 16);
            case FAILED:
                return NewTheme.I().getIcon(IconKey.ICON_FALSE, 16);
            case RUNNING:
            case PENDING:
            default:
                return null;
            }
        }
    }

    /** One test (= one table row): its input snapshot, state and result. */
    private static class TestRun {
        private final CAPTCHA_TYPE          type;
        private final CaptchaRequestType    source;
        private final CaptchaTestParameters parameters;
        /* null = all solvers, otherwise only this one. */
        private final SolverService         service;
        private volatile RunState           state;
        private volatile long               startTime;
        private volatile long               endTime;
        private volatile String             answer;
        private volatile String             statusText;
        private volatile String             expectedText;
        private volatile boolean            canCorrect;
        private volatile boolean            canIncorrect;
        private volatile boolean            correctUnsupported;
        private volatile boolean            incorrectUnsupported;
        private volatile boolean            aborted;
        private volatile long               maxTimeoutMillis;
        private volatile Challenge<Object>  challenge;
        private volatile SolverJob<Object>  finishedJob;
        private final List<Point>           markers = new ArrayList<Point>();

        private TestRun(final CAPTCHA_TYPE type, final CaptchaRequestType source, final CaptchaTestParameters parameters, final SolverService service) {
            this.type = type;
            this.source = source;
            this.parameters = parameters;
            this.service = service;
            reset();
        }

        /** Back to the state before the test was started. */
        private void reset() {
            state = RunState.PENDING;
            startTime = 0;
            endTime = 0;
            answer = "";
            statusText = "";
            expectedText = null;
            canCorrect = false;
            canIncorrect = false;
            correctUnsupported = false;
            incorrectUnsupported = false;
            aborted = false;
            maxTimeoutMillis = 0;
            challenge = null;
            finishedJob = null;
            markers.clear();
        }
    }

    /** Table model of the tests: one row per test. */
    private static class RunTableModel extends AbstractTableModel {
        private static final long   serialVersionUID = 1L;
        private final List<TestRun> runs             = new ArrayList<TestRun>();
        private final String[]      columns          = { "Captcha type", "Passed", "Progress", "Answer" };

        void setRuns(final List<TestRun> newRuns) {
            runs.clear();
            runs.addAll(newRuns);
            fireTableDataChanged();
        }

        List<TestRun> getRuns() {
            return new ArrayList<TestRun>(runs);
        }

        TestRun getRun(final int row) {
            return runs.get(row);
        }

        void fireRunChanged(final TestRun run) {
            final int row = runs.indexOf(run);
            if (row >= 0) {
                fireTableRowsUpdated(row, row);
            }
        }

        void fireRunsChanged() {
            if (!runs.isEmpty()) {
                fireTableRowsUpdated(0, runs.size() - 1);
            }
        }

        @Override
        public int getRowCount() {
            return runs.size();
        }

        @Override
        public int getColumnCount() {
            return columns.length;
        }

        @Override
        public String getColumnName(final int column) {
            return columns[column];
        }

        @Override
        public Class<?> getColumnClass(final int column) {
            switch (column) {
            case 0:
                return CAPTCHA_TYPE.class;
            case 1:
                return RunState.class;
            case 2:
                return TestRun.class;
            default:
                return String.class;
            }
        }

        @Override
        public Object getValueAt(final int row, final int column) {
            final TestRun run = runs.get(row);
            switch (column) {
            case 0:
                return run.type;
            case 1:
                return run.state;
            case 2:
                return run;
            default:
                return run.answer;
            }
        }
    }
}
