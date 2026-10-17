package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.datatransfer.StringSelection;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Set;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;

import javax.imageio.ImageIO;
import javax.swing.Box;
import javax.swing.DefaultListCellRenderer;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenuItem;
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

import org.appwork.swing.MigPanel;
import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtDefaultRowSorter;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.swing.exttable.columns.ExtComponentColumn;
import org.appwork.swing.exttable.columns.ExtProgressColumn;
import org.appwork.swing.exttable.columns.ExtTextColumn;
import org.appwork.utils.swing.renderer.RendererMigPanel;
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
import org.jdownloader.plugins.components.captchasolver.PluginForCaptchaSolverSolverService;
import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolver;

import jd.controlling.AccountController;
import jd.gui.swing.jdgui.BasicJDTable;
import jd.controlling.captcha.SkipException;
import jd.plugins.Account;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;

/**
 * IDE-only "Test &amp; Debug" tab of the captcha settings: lets the developer pick the captcha source (download/login/crawler captcha) and
 * the captcha type to test, edit the site URL/site key (pre-filled with the known test values, see {@link CaptchaTestParameters}) and run
 * real throwaway test {@link Challenge}s (see {@link CaptchaTestChallengeFactory}) through the normal solving pipeline
 * ({@link ChallengeResponseController#handle(Challenge)}) on a background thread. <br>
 * Every test (a single test is just a table with one row, a multi test runs all selected captcha types of one solver one after another) is
 * shown as a row of the result table. Clicking a row shows its details below the table: progress, status, elapsed time, result and the
 * feedback/abort/retry buttons. If an expected result is given, the solver's answer is compared against it and the matching feedback button
 * is disabled (a correct answer cannot be reported as incorrect and vice versa).
 */
public class CaptchaTestPanel extends WidthTrackingPanel {
    private static final long               serialVersionUID   = 1L;
    private static final Color              COLOR_ORANGE       = new Color(230, 126, 0);
    private final JComboBox                 sourceBox;
    private final JComboBox                 typeBox;
    private final JLabel                    siteUrlLabel;
    private final JTextField                siteUrlField;
    private final JLabel                    siteKeyLabel;
    private final JTextField                siteKeyField;
    private final JLabel                    imageLabel;
    private final MigPanel                  imagePanel;
    private final JLabel                    imagePreview;
    private final JLabel                    imageResolutionLabel;
    /* Original captcha image of the preview, the clicked positions of the result and whether it is shown in full size. */
    private BufferedImage                   previewSource;
    private final List<Point>               resultMarkers      = new ArrayList<Point>();
    private boolean                         previewExpanded    = false;
    /* All captcha images are scaled into a box of this size unless they are expanded by a click. */
    /* Upper limit of the max simultaneous tests spinner, the limit of the solver itself may be lower. */
    private static final int                MAX_PARALLEL_TESTS = 20;
    private static final int                PREVIEW_WIDTH      = 320;
    private static final int                PREVIEW_HEIGHT     = 200;
    private File                            imageFile;
    private final JButton                   startButton;
    private final JLabel                    descriptionLabel;
    private final JTextField                descriptionField;
    private final JLabel                    caseSensitiveLabel;
    private final JComboBox                 caseSensitiveBox;
    private final JLabel                    actionLabel;
    private final JTextField                actionField;
    private final JLabel                    minScoreLabel;
    private final JCheckBox                 minScoreCheckBox;
    private final JSpinner                  minScoreSpinner;
    private final JLabel                    minClicksLabel;
    private final JSpinner                  minClicksSpinner;
    private final JTextField                expectedResultField;
    private final JComboBox                 solverBox;
    /* Red warning, shown if there is no solver that could be selected. */
    private final JLabel                noSolverWarning = new JLabel();
    /* Red warning, shown if captcha types are selectable but the user deselected all of them. */
    private final JLabel                noTypeWarning   = new JLabel();
    private final JButton                   typesButton;
    private final JPopupMenu                typesPopup;
    /* First entry of the popup: selects/deselects all captcha types. */
    private final JCheckBox                 selectAllBox       = new JCheckBox("Select/Deselect all");
    private final List<JCheckBox>           typeCheckBoxes     = new ArrayList<JCheckBox>();
    private final List<CAPTCHA_TYPE>        typeCheckBoxTypes  = new ArrayList<CAPTCHA_TYPE>();
    private final JButton                   multiStartButton;
    private final JButton                   multiCancelButton;
    /* Result table: one row per test. */
    private final RunTableModel             runModel           = new RunTableModel();
    /* Overall progress of the batch (finished tests) and, while at least one test is still running, an animated activity bar. */
    private final JProgressBar                batchBar           = new JProgressBar(0, 1);
    private final JProgressBar                activityBar        = new JProgressBar();
    private volatile List<TestRun>          batchRuns          = new ArrayList<TestRun>();
    private volatile long                   batchStartTime;
    private final RunTable                 runTable;
    private final JScrollPane               runScroll;
    /* Details of the test selected in the table. */
    private final MigPanel                  runPanel;
    private final JProgressBar              progress;
    private final JLabel                    statusLabel;
    private final JLabel                    elapsedLabel;
    private final JTextArea                 resultArea;
    private final JButton                   correctButton;
    private final JButton                   incorrectButton;
    private final JButton                   abortButton;
    private final JButton                   retryButton;
    private Timer                           refreshTimer;
    private final Color                     defaultButtonForeground;
    /* True once the user clicked a column header: the automatic sorting is off until the next test is started. */
    private boolean                     userSorted         = false;
    /* True while the sort order is changed by the panel itself (not by a click of the user on a column header). */
    private boolean                     internalSortChange = false;
    private volatile TestRun                selectedRun;
    /* The tests of the current/last batch are executed one after another by this thread. */
    private final JSpinner                  parallelSpinner;
    private final JLabel                    parallelInfoLabel  = new JLabel();
    private volatile boolean                batchRunning       = false;
    private volatile boolean                batchCancelled     = false;
    /*
     * Reads the timeout of a test as soon as its job exists (before the periodic refresh could), so that even very fast tests have a
     * progress percentage.
     */
    private final ChallengeResponseListener jobListener        = new ChallengeResponseListener() {
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
                if (service != null && comp instanceof JLabel) {
                    /* Same solver icon as in the other solver lists (see SolverOrderTableModel). */
                    ((JLabel) comp).setIcon(service.getIcon(18));
                }
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
        multiCancelButton = new JButton("Cancel all", NewTheme.I().getIcon(IconKey.ICON_FALSE, 16));
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
        add(new JLabel("<html>Choose solver to test. You can only choose solvers with status \"Ready\", they must be enabled in the \"Solver overview\" tab and if it is a paid solver, an active and enabled account must be present.</html>"), "span 2, growx, wmin 0");
        noSolverWarning.setIcon(NewTheme.I().getIcon(IconKey.ICON_WARNING_RED, 18));
        noSolverWarning.setText("!! All Solvers are disabled or not properly configured !!");
        noSolverWarning.setForeground(Color.RED);
        noSolverWarning.setVisible(false);
        add(noSolverWarning, "span 2, growx, wmin 0");
        add(new JLabel("Multi test: Solver"));
        add(solverBox, "growx");
        add(new JLabel("Multi test: Captcha types"));
        add(typesButton, "growx");
        noTypeWarning.setIcon(NewTheme.I().getIcon(IconKey.ICON_WARNING_RED, 18));
        noTypeWarning.setText("No captcha type selected: select at least one captcha type to test");
        noTypeWarning.setForeground(Color.RED);
        noTypeWarning.setVisible(false);
        add(noTypeWarning, "skip 1, growx, wmin 0");
        /* Max simultaneous tests: up to MAX_PARALLEL_TESTS, but never more than the selected solver accepts at the same time. */
        parallelSpinner = new JSpinner(new SpinnerNumberModel(1, 1, MAX_PARALLEL_TESTS, 1));
        final MigPanel parallelPanel = new MigPanel("ins 0", "[][grow,fill]", "[]");
        parallelPanel.add(parallelSpinner);
        parallelPanel.add(parallelInfoLabel);
        add(new JLabel("Multi test: Max simultaneous tests"));
        add(parallelPanel, "growx");
        final MigPanel multiButtons = new MigPanel("ins 0", "[][]", "[]");
        multiButtons.add(multiStartButton);
        multiButtons.add(multiCancelButton);
        add(multiButtons, "skip 1, alignx left");
        /* Result table: hidden until the first test was started. */
        /* Based on BasicJDTable like the other tables: column menu in the header (show/hide columns), sortable columns, theme. */
        runTable = new RunTable(runModel);
        runTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
            @Override
            public void valueChanged(final ListSelectionEvent e) {
                if (!e.getValueIsAdjusting()) {
                    final int row = runTable.getSelectedRow();
                    selectedRun = row < 0 ? null : runModel.getObjectbyRow(row);
                    syncMarkers();
                    refreshDetails();
                }
            }
        });
        runScroll = new JScrollPane(runTable);
        /* Always shown at full size (all rows), so it never scrolls itself; the mouse wheel scrolls the surrounding panel instead. */
        runScroll.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
        runScroll.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        SolverOrderContainer.passMouseWheelToParent(runScroll);
        runScroll.setVisible(false);
        batchBar.setStringPainted(true);
        batchBar.setVisible(false);
        activityBar.setIndeterminate(true);
        activityBar.setPreferredSize(new Dimension(100, 6));
        activityBar.setVisible(false);
        add(batchBar, "span 2, growx");
        add(activityBar, "span 2, growx, gaptop 2");
        add(runScroll, "span 2, growx, wmin 0");
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
        retryButton = new JButton("Retry", NewTheme.I().getIcon(IconKey.ICON_REFRESH, 16));
        retryButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                final TestRun run = selectedRun;
                if (run != null) {
                    retryRun(run);
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
        updateNoSolverWarning();
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
        updateNoSolverWarning();
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

    /** Selects the given solver for the multi test (only if it can be selected, see isSelectable). */
    public void selectMultiTestSolver(final SolverService solver) {
        updateNoSolverWarning();
        if (!batchRunning && solver != null && isSelectable(solver)) {
            solverBox.setSelectedItem(solver);
        }
    }

    /** Shows the warning if not a single solver can be selected. */
    private void updateNoSolverWarning() {
        boolean anySelectable = false;
        for (int i = 0; i < solverBox.getItemCount(); i++) {
            if (isSelectable((SolverService) solverBox.getItemAt(i))) {
                anySelectable = true;
                break;
            }
        }
        noSolverWarning.setVisible(!anySelectable);
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
        /* Default: no min score is requested, unless the captcha type has a default min score. */
        final Double defaultMinScore = defaults.getMinScore();
        minScoreCheckBox.setSelected(usesMinScore && defaultMinScore != null);
        minScoreCheckBox.setEnabled(usesMinScore);
        minScoreSpinner.setValue(defaultMinScore != null ? defaultMinScore : Double.valueOf(0.5d));
        minScoreSpinner.setEnabled(usesMinScore && defaultMinScore != null);
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

    /**
     * Max number of captchas the given solver accepts at the same time, as enforced by the controller: the solver's
     * {@link ChallengeSolver#getFinalMaxCaptchaThreadsSingleAccount()} (user limit and server-side limit of one account). Never more than
     * MAX_PARALLEL_TESTS.
     */
    private static int getSolverParallelLimit(final SolverService service) {
        int limit = MAX_PARALLEL_TESTS;
        if (service == null) {
            return limit;
        }
        /* Local solvers exist once. */
        for (final ChallengeSolver<?> solver : ChallengeResponseController.getInstance().listSolvers()) {
            if (service.getID().equals(solver.getService().getID())) {
                limit = Math.min(limit, getSolverLimit(solver));
            }
        }
        if (service instanceof PluginForCaptchaSolverSolverService) {
            /* External solvers are created per account (the controller uses one account per solver service). */
            final List<Account> accounts = AccountController.getInstance().getValidAccounts(service.getID());
            if (accounts != null && !accounts.isEmpty()) {
                final Account account = accounts.get(0);
                try {
                    limit = Math.min(limit, getSolverLimit(((abstractPluginForCaptchaSolver) account.getPlugin()).getPluginChallengeSolver(null, account)));
                } catch (final Throwable e) {
                    LogController.CL().log(e);
                }
            }
        }
        return limit;
    }

    private static int getSolverLimit(final ChallengeSolver<?> solver) {
        return solver.getFinalMaxCaptchaThreadsSingleAccount();
    }

    /** Applies the limit of the selected solver to the max simultaneous tests spinner. */
    private void updateParallelLimit() {
        final int limit = getSolverParallelLimit((SolverService) solverBox.getSelectedItem());
        final SpinnerNumberModel model = (SpinnerNumberModel) parallelSpinner.getModel();
        model.setMaximum(Integer.valueOf(limit));
        if (((Number) model.getValue()).intValue() > limit) {
            model.setValue(Integer.valueOf(limit));
        }
        parallelInfoLabel.setText(limit < MAX_PARALLEL_TESTS ? "(limited to " + limit + " by the selected solver)" : "(max " + MAX_PARALLEL_TESTS + ")");
    }

    /** A solver can only be tested if it is ready and activated in the "Solver overview" tab. */
    static boolean isSelectable(final SolverService service) {
        return service.isReady() && service.getConfigV3().isEnabled();
    }

    /** Rebuilds the captcha type check boxes (all selected) from the types the selected solver supports and has test challenges for. */
    private void rebuildTypeList() {
        typesPopup.removeAll();
        typeCheckBoxes.clear();
        typeCheckBoxTypes.clear();
        updateParallelLimit();
        typesPopup.add(selectAllBox);
        typesPopup.addSeparator();
        final SolverService service = (SolverService) solverBox.getSelectedItem();
        final Set<CAPTCHA_TYPE> supported = service == null || !isSelectable(service) ? null : service.getSupportedCaptchaTypes();
        if (supported != null) {
            for (final CAPTCHA_TYPE type : CAPTCHA_TYPE.values()) {
                if (type.hasTestChallenges() && supported.contains(type)) {
                    /* The check box itself has no text: the captcha type icon and name are a separate label (click toggles the check box). */
                    final JCheckBox box = new JCheckBox("", true);
                    final JLabel typeLabel = new JLabel(type.getDisplayName(), type.getIcon(), SwingConstants.LEADING);
                    typeLabel.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
                    typeLabel.addMouseListener(new MouseAdapter() {
                        @Override
                        public void mouseClicked(final MouseEvent e) {
                            box.doClick();
                        }
                    });
                    final MigPanel typeRow = new MigPanel("ins 0", "[][grow,fill]", "[]");
                    typeRow.add(box);
                    typeRow.add(typeLabel);
                    box.addActionListener(new ActionListener() {
                        @Override
                        public void actionPerformed(final ActionEvent e) {
                            updateTypesButtonText();
                        }
                    });
                    typeCheckBoxes.add(box);
                    typeCheckBoxTypes.add(type);
                    typesPopup.add(typeRow);
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
        noTypeWarning.setVisible(!typeCheckBoxes.isEmpty() && selected == 0);
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
     * (which are the defaults unless the user changed them), all other types use their default test values. The captcha source always comes
     * from the form.
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
            /* A new test starts: the automatic sorting is active again. */
            enableAutoSort();
            runModel.setRuns(runs);
        } else {
            runModel.fireRunsChanged();
        }
        updateRunScrollHeight();
        runScroll.setVisible(true);
        batchCancelled = false;
        batchRunning = true;
        batchRuns = runs;
        batchStartTime = System.currentTimeMillis();
        updateBatchLabel();
        setBatchRunningState(true);
        startRefreshTimer();
        ChallengeResponseController.getInstance().getEventSender().addListener(jobListener);
        /* Single tests and retries run alone; a multi test runs as many tests at the same time as chosen (and allowed by the solver). */
        final int parallel = Math.max(1, Math.min(runs.size(), Math.min(((Number) parallelSpinner.getValue()).intValue(), getSolverParallelLimit(runs.get(0).service))));
        final Thread thread = new Thread("CaptchaTestPanelBatch") {
            @Override
            public void run() {
                final ExecutorService pool = Executors.newFixedThreadPool(parallel, new ThreadFactory() {
                    @Override
                    public Thread newThread(final Runnable r) {
                        final Thread t = new Thread(r, "CaptchaTestPanelTest");
                        t.setDaemon(true);
                        return t;
                    }
                });
                try {
                    for (final TestRun run : runs) {
                        pool.execute(new Runnable() {
                            @Override
                            public void run() {
                                if (run.state == RunState.FAILED && run.aborted) {
                                    /* Aborted while it was still waiting for its turn. */
                                    return;
                                }
                                if (batchCancelled) {
                                    finishRun(run, RunState.FAILED, "Cancelled", null);
                                    return;
                                }
                                executeRun(run);
                            }
                        });
                    }
                    pool.shutdown();
                    while (!pool.awaitTermination(1, TimeUnit.HOURS)) {
                        /* Wait until all tests are done (each one has its own solver timeout). */
                    }
                } catch (final InterruptedException e) {
                    pool.shutdownNow();
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
        /* Clear an interrupt flag that was meant for the previous test of this pool thread. */
        Thread.interrupted();
        run.thread = Thread.currentThread();
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
                    final Set<FeedbackType> feedbackTypes = ((ChallengeSolver<?>) solver).getSupportedFeedbackTypes();
                    /* Null or empty = the solver does not support any feedback. */
                    run.correctUnsupported = feedbackTypes == null || !feedbackTypes.contains(FeedbackType.REPORT_VALID_CAPTCHAS);
                    run.incorrectUnsupported = feedbackTypes == null || !feedbackTypes.contains(FeedbackType.REPORT_INVALID_CAPTCHAS);
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
                finishRun(run, RunState.FAILED, "No answer received (job status: " + job.getStatus() + ")", null);
            }
        } catch (final InterruptedException e) {
            finishRun(run, RunState.FAILED, batchCancelled ? "Cancelled" : "Aborted", null);
        } catch (final SkipException e) {
            finishRun(run, RunState.FAILED, batchCancelled ? "Cancelled" : (run.aborted ? "Aborted" : "Skipped: " + e.getSkipRequest()), null);
        } catch (final Throwable e) {
            LogController.CL().log(e);
            finishRun(run, RunState.FAILED, batchCancelled ? "Cancelled" : "Failed: " + e.getClass().getSimpleName() + ": " + e.getMessage(), null);
        } finally {
            run.thread = null;
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
                if (finished && !userSorted) {
                    /* Passed first, then failed, running, pending. */
                    runModel.refreshSort();
                }
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

    /**
     * Sizes the result table so that the header and all rows are visible (see SolverOrderContainer#tableFullHeight for the extra pixels,
     * without them the last row is partly cut and a scrollbar would be needed).
     */
    private void updateRunScrollHeight() {
        final int height = SolverOrderContainer.tableFullHeight(runTable);
        /*
         * Only the height is fixed. The width must not follow the table's preferred width (sum of the column widths): it only grows when the
         * window gets bigger and would then force a horizontal scroll bar on the surrounding panel when the window gets smaller again.
         */
        runScroll.setPreferredSize(new Dimension(100, height));
        runScroll.revalidate();
        revalidate();
    }

    /** Progress line above the table of a multi test: "Running test 1/X" while running, the total time when done. */
    private void updateBatchLabel() {
        final List<TestRun> runs = batchRuns;
        final boolean visible = runModel.getRowCount() > 1;
        batchBar.setVisible(visible);
        /* The animation shows that at least one test is still running. */
        activityBar.setVisible(visible && batchRunning);
        int started = 0;
        int finished = 0;
        for (final TestRun run : runs) {
            switch (run.state) {
            case PENDING:
                break;
            case RUNNING:
                started++;
                break;
            case PASSED:
            case PASSED_UNEXPECTED_ANSWER:
            case FAILED:
            default:
                started++;
                finished++;
                break;
            }
        }
        batchBar.setMaximum(Math.max(1, runs.size()));
        batchBar.setValue(finished);
        final long elapsed = System.currentTimeMillis() - batchStartTime;
        if (batchRunning) {
            batchBar.setString("Finished tests " + finished + "/" + runs.size() + " | Time: " + (elapsed / 100 / 10d) + "s");
        } else {
            batchBar.setString((batchCancelled ? "Tests cancelled" : "Tests completed") + " | Time: " + (elapsed / 100 / 10d) + "s");
        }
    }

    /** Automatic sorting: passed, failed, running, pending, applied again after every finished test (until the user sorts by himself). */
    private void enableAutoSort() {
        internalSortChange = true;
        try {
            runModel.passedColumn.setSortOrderIdentifier(ExtColumn.SORT_ASC);
            runModel.setSortColumn(runModel.passedColumn);
        } finally {
            internalSortChange = false;
        }
        userSorted = false;
    }

    /** A finished test can be run again, but not while another test is running. */
    private boolean isRetryable(final TestRun run) {
        return !batchRunning && !isCancellable(run);
    }

    /** Runs the given (finished) test again with the same input. */
    private void retryRun(final TestRun run) {
        run.reset();
        final List<TestRun> runs = new ArrayList<TestRun>();
        runs.add(run);
        startBatch(runs, false);
    }

    /** True while a test is waiting for its turn or running, i.e. it can still be cancelled. */
    private static boolean isCancellable(final TestRun run) {
        return run.state == RunState.PENDING || run.state == RunState.RUNNING;
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
        retryButton.setEnabled(isRetryable(run));
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

    /**
     * Sort key of the duration column: the duration of finished tests only. Running tests (their duration changes all the time) and
     * pending tests have a fixed key and are sorted behind the finished ones, so the order does not change live.
     */
    private static long getSortDuration(final TestRun run) {
        switch (run.state) {
        case PASSED:
        case PASSED_UNEXPECTED_ANSWER:
        case FAILED:
            return Math.max(0, getDurationMillis(run));
        case RUNNING:
            return Long.MAX_VALUE - 1;
        case PENDING:
        default:
            return Long.MAX_VALUE;
        }
    }

    /** Example: "10.3s (34% of 30.00s)", or only "10.3s" if the timeout is unknown, empty if the test did not start yet. */
    private static String formatDurationText(final TestRun value) {
        final long duration = getDurationMillis(value);
        final int percent = getProgressPercent(value);
        if (duration < 0) {
            return "";
        } else if (percent >= 0) {
            return (duration / 100 / 10d) + "s (" + percent + "% of " + String.format(java.util.Locale.ENGLISH, "%.2f", value.maxTimeoutMillis / 1000d) + "s)";
        } else {
            return (duration / 100 / 10d) + "s";
        }
    }

    private static void copyToClipboard(final String text) {
        Toolkit.getDefaultToolkit().getSystemClipboard().setContents(new StringSelection(text), null);
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

    /**
     * Shows which solver(s) are still working on the challenge and their {@link ChallengeSolver#getFinalTimeoutMillis()}, while waiting.
     */
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
            final Thread thread = run.thread;
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

        /** Default table order: passed, failed, running, pending. */
        int getSortRank() {
            switch (this) {
            case PASSED:
            case PASSED_UNEXPECTED_ANSWER:
                return 0;
            case FAILED:
                return 1;
            case RUNNING:
                return 2;
            case PENDING:
            default:
                return 3;
            }
        }

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
        /* Pool thread that currently executes this test. */
        private volatile Thread             thread;
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

    /** Table of the tests: right click on a test offers "Cancel", the header offers the show/hide column menu. */
    private class RunTable extends BasicJDTable<TestRun> {
        private static final long serialVersionUID = 1L;

        private RunTable(final RunTableModel model) {
            super(model);
            setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
            /* Like the comparison table, if the columns fit: all columns follow the width of the table (shrink and grow together). */
            setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
        }

        /*
         * The table is always shown in full height without an own scroll bar. ExtTable's column button (upper right corner, the right click
         * menu of the header stays available) would force a permanent vertical scroll bar.
         */
        @Override
        public boolean isColumnButtonVisible() {
            return false;
        }

        @Override
        protected JPopupMenu onContextMenu(final JPopupMenu popup, final TestRun contextObject, final List<TestRun> selection, final ExtColumn<TestRun> column, final MouseEvent ev) {
            if (contextObject != null) {
                /* All selected tests (the clicked one if the selection is empty) that are still waiting or running can be cancelled. */
                final List<TestRun> cancellable = new ArrayList<TestRun>();
                final List<TestRun> targets = selection != null && !selection.isEmpty() ? selection : Collections.singletonList(contextObject);
                for (final TestRun run : targets) {
                    if (isCancellable(run)) {
                        cancellable.add(run);
                    }
                }
                final JMenuItem cancel = new JMenuItem(cancellable.size() > 1 ? "Cancel (" + cancellable.size() + " tests)" : "Cancel", NewTheme.I().getIcon(IconKey.ICON_FALSE, 16));
                cancel.setEnabled(!cancellable.isEmpty());
                cancel.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(final ActionEvent event) {
                        for (final TestRun run : cancellable) {
                            abortRun(run, false);
                        }
                    }
                });
                popup.add(cancel);
                /* Only for finished tests. */
                final JMenuItem retry = new JMenuItem("Retry", NewTheme.I().getIcon(IconKey.ICON_REFRESH, 16));
                retry.setEnabled(isRetryable(contextObject));
                retry.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(final ActionEvent event) {
                        retryRun(contextObject);
                    }
                });
                popup.add(retry);
                popup.addSeparator();
                /* Copies the line of the test the right click was made on (all columns, separated by tabs). */
                final JMenuItem copyRow = new JMenuItem("Copy row");
                copyRow.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(final ActionEvent event) {
                        copyToClipboard(contextObject.type.getDisplayName() + "	" + contextObject.state.getText() + "	" + formatDurationText(contextObject) + "	" + contextObject.answer);
                    }
                });
                popup.add(copyRow);
                final JMenuItem copyAnswer = new JMenuItem("Copy answer");
                copyAnswer.setEnabled(!StringUtils.isEmpty(contextObject.answer));
                copyAnswer.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(final ActionEvent event) {
                        copyToClipboard(contextObject.answer);
                    }
                });
                popup.add(copyAnswer);
            }
            return popup;
        }
    }

    /** Table model of the tests: one row per test. All columns are sortable. */
    private class RunTableModel extends ExtTableModel<TestRun> {
        private static final long serialVersionUID = 1L;
        /* Assigned in initColumns() (called by the super constructor), so it must not have an initializer. */
        private ExtColumn<TestRun> passedColumn;

        private RunTableModel() {
            super("CaptchaTestPanelRunTable");
        }

        @Override
        public void setSortColumn(final ExtColumn<TestRun> e) {
            if (!internalSortChange) {
                /* Click of the user on a column header: automatic sorting off until the next test is started. */
                userSorted = true;
            }
            super.setSortColumn(e);
        }

        void setRuns(final List<TestRun> newRuns) {
            _fireTableStructureChanged(new ArrayList<TestRun>(newRuns), true);
        }

        List<TestRun> getRuns() {
            return new ArrayList<TestRun>(getTableData());
        }

        void fireRunChanged(final TestRun run) {
            final int row = getRowforObject(run);
            if (row >= 0) {
                fireTableRowsUpdated(row, row);
            }
        }

        void fireRunsChanged() {
            if (getRowCount() > 0) {
                fireTableRowsUpdated(0, getRowCount() - 1);
            }
        }

        @Override
        protected void initColumns() {
            addColumn(new ExtTextColumn<TestRun>("Captcha type") {
                private static final long serialVersionUID = 1L;

                @Override
                public String getStringValue(final TestRun value) {
                    return value.type.getDisplayName();
                }

                @Override
                protected javax.swing.Icon getIcon(final TestRun value) {
                    return value.type.getIcon();
                }

                @Override
                public int getDefaultWidth() {
                    return 200;
                }
            });
            passedColumn = new ExtTextColumn<TestRun>("Passed") {
                private static final long serialVersionUID = 1L;

                {
                    /* Passed, failed, running, pending (descending: the other way round). */
                    setRowSorter(new ExtDefaultRowSorter<TestRun>() {
                        @Override
                        public int compare(final TestRun o1, final TestRun o2) {
                            final int diff = o1.state.getSortRank() - o2.state.getSortRank();
                            return ExtColumn.SORT_ASC.equals(getSortOrderIdentifier()) ? diff : -diff;
                        }
                    });
                }

                @Override
                public String getStringValue(final TestRun value) {
                    return value.state.getText();
                }

                @Override
                protected javax.swing.Icon getIcon(final TestRun value) {
                    return value.state.getIcon();
                }

                @Override
                public void configureRendererComponent(final TestRun value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                    super.configureRendererComponent(value, isSelected, hasFocus, row, column);
                    /* Normal text, orange if the answer is not the expected one, red if failed. */
                    final Color color = value.state.getColor();
                    rendererField.setForeground(color == Color.BLACK ? null : color);
                }

                @Override
                public int getDefaultWidth() {
                    return 100;
                }
            };
            addColumn(passedColumn);
            addColumn(new ExtProgressColumn<TestRun>("Duration") {
                private static final long serialVersionUID = 1L;

                {
                    /* Sorted by the duration of finished tests only, see getSortDuration. */
                    setRowSorter(new ExtDefaultRowSorter<TestRun>() {
                        @Override
                        public int compare(final TestRun o1, final TestRun o2) {
                            final long d1 = getSortDuration(o1);
                            final long d2 = getSortDuration(o2);
                            final int diff = d1 < d2 ? -1 : (d1 == d2 ? 0 : 1);
                            return ExtColumn.SORT_ASC.equals(getSortOrderIdentifier()) ? diff : -diff;
                        }
                    });
                }

                @Override
                protected long getValue(final TestRun value) {
                    /* Elapsed time in percent of the timeout (100% = timeout fully elapsed). */
                    return Math.max(0, getProgressPercent(value));
                }

                @Override
                protected boolean isIndeterminated(final TestRun value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                    return false;
                }

                @Override
                protected String getString(final TestRun value, final long current, final long total) {
                    return formatDurationText(value);
                }

                @Override
                public int getDefaultWidth() {
                    return 160;
                }
            });
            addColumn(new AnswerColumn());
        }
    }

    /** Answer of the test; while the test is still waiting/running a "Cancel" button is shown instead. Sortable by the answer text. */
    private class AnswerColumn extends ExtComponentColumn<TestRun> {
        private static final long serialVersionUID = 1L;
        private final MigPanel    renderer;
        private final JLabel      rendererLabel = new JLabel();
        private final JButton     rendererButton;
        private final MigPanel    editor;
        private final JLabel      editorLabel   = new JLabel();
        private final JButton     editorButton;
        private volatile TestRun  editorRun;

        private AnswerColumn() {
            super("Answer");
            renderer = new RendererMigPanel("ins 2", "[grow,fill]", "[]");
            editor = new RendererMigPanel("ins 2", "[grow,fill]", "[]");
            rendererButton = createCancelButton();
            editorButton = createCancelButton();
            editorButton.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(final ActionEvent e) {
                    final TestRun run = editorRun;
                    if (run != null && isCancellable(run)) {
                        abortRun(run, false);
                    }
                    stopCellEditing();
                }
            });
            renderer.add(rendererLabel, "hidemode 3, growx, wmin 0");
            renderer.add(rendererButton, "hidemode 3, alignx left");
            editor.add(editorLabel, "hidemode 3, growx, wmin 0");
            editor.add(editorButton, "hidemode 3, alignx left");
            setRowSorter(new ExtDefaultRowSorter<TestRun>() {
                @Override
                public int compare(final TestRun o1, final TestRun o2) {
                    final int diff = o1.answer.compareToIgnoreCase(o2.answer);
                    return ExtColumn.SORT_ASC.equals(getSortOrderIdentifier()) ? diff : -diff;
                }
            });
        }

        private JButton createCancelButton() {
            final JButton button = new JButton("Cancel", NewTheme.I().getIcon(IconKey.ICON_FALSE, 16));
            button.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
            return button;
        }

        @Override
        public boolean isSortable(final TestRun obj) {
            return true;
        }

        /** Default width: about 20 characters. Longer answers are clipped, the user can still resize the column. */
        @Override
        public int getDefaultWidth() {
            return rendererLabel.getFontMetrics(rendererLabel.getFont()).stringWidth("01234567890123456789") + 20;
        }

        @Override
        public int getMinWidth() {
            return 100;
        }

        private void configure(final TestRun value, final JLabel label, final JButton button) {
            final boolean cancellable = isCancellable(value);
            button.setVisible(cancellable);
            label.setVisible(!cancellable);
            label.setText(value.answer);
        }

        @Override
        public void configureRendererComponent(final TestRun value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
            configure(value, rendererLabel, rendererButton);
        }

        @Override
        public void configureEditorComponent(final TestRun value, final boolean isSelected, final int row, final int column) {
            editorRun = value;
            configure(value, editorLabel, editorButton);
        }

        @Override
        protected JComponent getInternalRendererComponent(final TestRun value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
            return renderer;
        }

        @Override
        protected JComponent getInternalEditorComponent(final TestRun value, final boolean isSelected, final int row, final int column) {
            return editor;
        }

        @Override
        public void resetEditor() {
            editor.setBackground(null);
            editor.setOpaque(false);
        }

        @Override
        public void resetRenderer() {
            renderer.setBackground(null);
            renderer.setOpaque(false);
        }
    }

}
