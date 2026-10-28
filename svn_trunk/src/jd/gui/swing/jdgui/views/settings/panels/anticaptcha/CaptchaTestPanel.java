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
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;
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
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.EDTRunner;
import org.jdownloader.captcha.event.ChallengeResponseListener;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.Challenge.CaptchaRequestType;
import org.jdownloader.captcha.v2.ChallengeResponseController;
import org.jdownloader.captcha.v2.ChallengeSolver;
import org.jdownloader.captcha.v2.ChallengeSolver.SolverFeature;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.captcha.v2.challenge.multiclickcaptcha.MultiClickedPoint;
import org.jdownloader.captcha.v2.solverjob.ResponseList;
import org.jdownloader.captcha.v2.solverjob.SolverJob;
import org.jdownloader.captcha.v2.tester.CaptchaTestChallengeFactory;
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
 * the captcha type to test, edit the site URL/site key (pre-filled with the known test values, see {@link CaptchaTestChallengeFactory}) and run
 * real throwaway test {@link Challenge}s (see {@link CaptchaTestChallengeFactory}) through the normal solving pipeline
 * ({@link ChallengeResponseController#handle(Challenge)}) on a background thread. <br>
 * Every test (a single test, or one test per selected captcha type of a multi test) is shown in the result table with one row per solver
 * that can be used for the test: Each row shows the answer, state and working time of its solver (solvers that were not used are listed
 * too). Clicking a row shows its details below the table: progress, status, elapsed time, result and the
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
    private final JLabel                    challengeUrlLabel;
    private final JTextField                challengeUrlField;
    private final JLabel                    proxyLabel;
    private final JTextField                proxyField;
    private final JLabel                    imageLabel;
    private final MigPanel                  imagePanel;
    private final JLabel                    imagePreview;
    private final JLabel                    imageResolutionLabel;
    private final JButton                   openFolderButton;
    /* Original captcha image of the preview, the clicked positions of the result and whether it is shown in full size. */
    private BufferedImage                   previewSource;
    private final List<Point>               resultMarkers      = new ArrayList<Point>();
    private boolean                         previewExpanded    = false;
    /* All captcha images are scaled into a box of this size unless they are expanded by a click. */
    /* Upper limit of the max simultaneous tests spinner, the limit of the solver itself may be lower. */
    private static final int                MAX_PARALLEL_TESTS = 50;
    private static final int                PREVIEW_WIDTH      = 320;
    private static final int                PREVIEW_HEIGHT     = 200;
    /* The captcha image: a URL (bundled test image) or a File (own image). */
    private Object                          imageSource;
    private final JButton                   startButton;
    private final JLabel                    descriptionLabel;
    private final JTextField                descriptionField;
    private final JLabel                    caseSensitiveLabel;
    private final JComboBox                 caseSensitiveBox;
    private final JLabel                    actionLabel;
    private final JTextField                actionField;
    private final JLabel                    minScoreLabel;
    private final MigPanel                  minScorePanel;
    private final JCheckBox                 minScoreCheckBox;
    private final JSpinner                  minScoreSpinner;
    private final JLabel                    minClicksLabel;
    private final JSpinner                  minClicksSpinner;
    private final JTextField                expectedResultField;
    /* Multi test: the solvers to test. Only enabled and ready solvers are listed. */
    private final JButton                   solversButton;
    private final JPopupMenu                solversPopup;
    /* First entry of the solver popup: selects/deselects all solvers. */
    private final JCheckBox                 solverSelectAllBox     = new JCheckBox("Select/Deselect all");
    private final List<JCheckBox>           solverCheckBoxes       = new ArrayList<JCheckBox>();
    private final List<SolverService>       solverCheckBoxServices = new ArrayList<SolverService>();
    /* Red warning, shown if solvers are selectable but the user deselected all of them. */
    private final JLabel                    noSolverSelectedWarning = new JLabel();
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
    /* The tests (not the rows) of the table, in table order. Every test owns its rows, see TestRun#rows. */
    private volatile List<TestRun>          tests              = new ArrayList<TestRun>();
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
     * Reads the timeouts of the solvers as soon as the job of a test exists (before the periodic refresh could), so that even very fast
     * tests have a progress percentage.
     */
    private final ChallengeResponseListener jobListener        = new ChallengeResponseListener() {
                                                                   @Override
                                                                   public void onNewJob(final SolverJob<?> job) {
                                                                       for (final TestRun test : tests) {
                                                                           if (test.challenge != null && test.challenge == job.getChallenge()) {
                                                                               test.liveJob = job;
                                                                               syncSolverRows(test, job, false);
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
        challengeUrlField = new JTextField();
        proxyField = new JTextField();
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
        siteKeyLabel = new JLabel("Site key");
        add(siteKeyLabel, "hidemode 3");
        add(siteKeyField, "growx, hidemode 3");
        siteUrlLabel = new JLabel("Site URL");
        add(siteUrlLabel, "hidemode 3");
        add(siteUrlField, "growx, hidemode 3");
        challengeUrlLabel = new JLabel("Challenge URL");
        add(challengeUrlLabel, "hidemode 3");
        add(challengeUrlField, "growx, hidemode 3");
        proxyLabel = new JLabel("Proxy");
        proxyField.setToolTipText("Optional, e.g. socks5://user:password@host:port or http://host:port. Sent to the solver if it supports proxies (challenges which require the same IP address need one).");
        add(proxyLabel, "hidemode 3");
        add(proxyField, "growx, hidemode 3");
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
        /* Opens the folder that contains the image, disabled if there is no valid folder (see updateOpenFolderButton). */
        openFolderButton = new JButton("Open folder", NewTheme.I().getIcon(IconKey.ICON_FOLDER_OPEN, 16));
        openFolderButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                if (getImageFolder() != null) {
                    /* Same as the other places that reveal a file: opens its folder and selects the file (or just opens the folder if the file does not exist). */
                    CrossSystem.showInExplorer(getImageAsFile());
                }
            }
        });
        imagePanel.add(openFolderButton, "alignx left");
        add(imageLabel, "aligny top, hidemode 3");
        add(imagePanel, "growx, hidemode 3");
        descriptionLabel = new JLabel("Challenge Description");
        descriptionField = new JTextField();
        add(descriptionLabel, "hidemode 3");
        add(descriptionField, "growx, hidemode 3");
        caseSensitiveLabel = new JLabel("Case sensitive (image captcha)");
        caseSensitiveBox = new JComboBox(new String[] { "Unset", "true", "false" });
        add(caseSensitiveLabel, "hidemode 3");
        add(caseSensitiveBox, "growx, hidemode 3");
        actionLabel = new JLabel("Action (reCAPTCHA v3)");
        add(actionLabel, "hidemode 3");
        add(actionField, "growx, hidemode 3");
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
        minScorePanel = new MigPanel("ins 0", "[][]", "[]");
        minScorePanel.add(minScoreCheckBox);
        minScorePanel.add(minScoreSpinner);
        add(minScoreLabel, "hidemode 3");
        add(minScorePanel, "growx, hidemode 3");
        minClicksLabel = new JLabel("Min clicks (multi click captcha)");
        minClicksSpinner = new JSpinner(new SpinnerNumberModel(8, 1, 100, 1));
        add(minClicksLabel, "hidemode 3");
        add(minClicksSpinner, "growx, hidemode 3");
        add(new JLabel("Expected result"));
        add(expectedResultField, "growx");
        startButton = new JButton("Start Single Challenge Auto Test");
        startButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                startSingleTest();
            }
        });
        add(startButton, "skip 1, alignx left");
        /* Multi test: run all selected captcha types with all selected solvers. Solvers that are not enabled/ready cannot be selected. */
        solversPopup = new JPopupMenu();
        solverSelectAllBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                for (final JCheckBox box : solverCheckBoxes) {
                    box.setSelected(solverSelectAllBox.isSelected());
                }
                onSolverSelectionChanged();
            }
        });
        solversButton = new JButton();
        solversButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                solversPopup.show(solversButton, 0, solversButton.getHeight());
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
        add(new JLabel("<html>Choose the solvers to test. You can only choose solvers with status \"Ready\", they must be enabled in the \"Solver overview\" tab and if it is a paid solver, an active and enabled account must be present.</html>"), "span 2, growx, wmin 0");
        noSolverWarning.setIcon(NewTheme.I().getIcon(IconKey.ICON_WARNING_RED, 18));
        noSolverWarning.setText("!! All Solvers are disabled or not properly configured !!");
        noSolverWarning.setForeground(Color.RED);
        noSolverWarning.setVisible(false);
        add(noSolverWarning, "span 2, growx, wmin 0");
        add(new JLabel("Multi test: Solvers"));
        add(solversButton, "growx");
        noSolverSelectedWarning.setIcon(NewTheme.I().getIcon(IconKey.ICON_WARNING_RED, 18));
        noSolverSelectedWarning.setText("No solver selected: select at least one solver to test");
        noSolverSelectedWarning.setForeground(Color.RED);
        noSolverSelectedWarning.setVisible(false);
        add(noSolverSelectedWarning, "skip 1, growx, wmin 0");
        add(new JLabel("Multi test: Captcha types"));
        add(typesButton, "growx");
        noTypeWarning.setIcon(NewTheme.I().getIcon(IconKey.ICON_WARNING_RED, 18));
        noTypeWarning.setText("No captcha type selected: select at least one captcha type to test");
        noTypeWarning.setForeground(Color.RED);
        noTypeWarning.setVisible(false);
        add(noTypeWarning, "skip 1, growx, wmin 0");
        /* Max simultaneous tests: up to MAX_PARALLEL_TESTS, but never more than the selected solver accepts at the same time. */
        parallelSpinner = new JSpinner(new SpinnerNumberModel(5, 1, MAX_PARALLEL_TESTS, 1));
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
        /* Always shown at full size (all rows), so it never scrolls itself; the mouse wheel scrolls the surrounding panel instead. */
        runScroll = runTable.createScrollPane();
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
        rebuildSolverList(false);
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
     * Makes sure the selected solvers can still be tested: solvers which were deactivated/are no longer ready are removed from the
     * selection and solvers which became usable are listed (not selected). If none of the selected solvers is usable any more, the first
     * usable solver is selected.
     *
     * @return true if there is a solver that can be selected
     */
    private boolean refreshSolverSelection() {
        updateNoSolverWarning();
        if (batchRunning) {
            return true;
        }
        rebuildSolverList(true);
        return !solverCheckBoxes.isEmpty();
    }

    /** The solvers which are selected for the multi test. */
    private List<SolverService> getSelectedSolvers() {
        final List<SolverService> ret = new ArrayList<SolverService>();
        for (int i = 0; i < solverCheckBoxes.size(); i++) {
            if (solverCheckBoxes.get(i).isSelected()) {
                ret.add(solverCheckBoxServices.get(i));
            }
        }
        return ret;
    }

    /**
     * Rebuilds the solver check boxes from all solvers which are enabled and ready.
     *
     * @param keepSelection
     *            true: solvers which are still usable stay selected (if none of them is usable any more, the first solver is selected),
     *            false: only the first usable solver is selected
     */
    private void rebuildSolverList(final boolean keepSelection) {
        final List<SolverService> previous = keepSelection ? getSelectedSolvers() : new ArrayList<SolverService>();
        solversPopup.removeAll();
        solverCheckBoxes.clear();
        solverCheckBoxServices.clear();
        solversPopup.add(solverSelectAllBox);
        solversPopup.addSeparator();
        for (final SolverService service : ChallengeResponseController.getInstance().listServices()) {
            if (!isSelectable(service)) {
                continue;
            }
            /* The check box itself has no text: the solver icon and name are a separate label (click toggles the check box). */
            final JCheckBox box = new JCheckBox("", previous.contains(service));
            final JLabel solverLabel = new JLabel(service.getName(), service.getIcon(18), SwingConstants.LEADING);
            solverLabel.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
            solverLabel.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseClicked(final MouseEvent e) {
                    box.doClick();
                }
            });
            final MigPanel solverRow = new MigPanel("ins 0", "[][grow,fill]", "[]");
            solverRow.add(box);
            solverRow.add(solverLabel);
            box.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(final ActionEvent e) {
                    onSolverSelectionChanged();
                }
            });
            solverCheckBoxes.add(box);
            solverCheckBoxServices.add(service);
            solversPopup.add(solverRow);
        }
        if (!solverCheckBoxes.isEmpty() && (!keepSelection || (!previous.isEmpty() && getSelectedSolvers().isEmpty()))) {
            solverCheckBoxes.get(0).setSelected(true);
        }
        updateSolversButtonText();
        rebuildTypeList(keepSelection);
    }

    /** The user (de)selected a solver: the selectable captcha types are the ones of all selected solvers. */
    private void onSolverSelectionChanged() {
        updateSolversButtonText();
        rebuildTypeList(true);
    }

    private void updateSolversButtonText() {
        final int selected = getSelectedSolvers().size();
        solversButton.setText(selected + " of " + solverCheckBoxes.size() + " solvers selected");
        noSolverSelectedWarning.setVisible(!solverCheckBoxes.isEmpty() && selected == 0);
        /* Checked only if every solver is selected. */
        solverSelectAllBox.setSelected(selected > 0 && selected == solverCheckBoxes.size());
        solverSelectAllBox.setEnabled(!solverCheckBoxes.isEmpty());
    }

    /** Selects the given solver for the multi test (only if it can be selected, see isSelectable), all other solvers are deselected. */
    public void selectMultiTestSolver(final SolverService solver) {
        selectMultiTestSolvers(Collections.singletonList(solver), null);
    }

    /**
     * Selects the given solver for the multi test and the given captcha types: the first one (which has a test) is selected in the
     * captcha type box at the top, all given types in the multi selection (all other types are deselected).
     */
    public void selectMultiTestSolver(final SolverService solver, final java.util.Collection<CAPTCHA_TYPE> types) {
        selectMultiTestSolvers(Collections.singletonList(solver), types);
    }

    /**
     * Selects the given solvers for the multi test (only the ones which can be selected, see isSelectable, all others are deselected)
     * and, if not null, the given captcha types: the first one (which has a test) is selected in the captcha type box at the top, all given
     * types in the multi selection (all other types are deselected). If none of the solvers can be selected, nothing is changed.
     */
    public void selectMultiTestSolvers(final java.util.Collection<SolverService> solvers, final java.util.Collection<CAPTCHA_TYPE> types) {
        updateNoSolverWarning();
        if (batchRunning || solvers == null) {
            return;
        }
        /* Solvers can be (de)activated in the other tabs meanwhile -> list the usable solvers first. */
        rebuildSolverList(true);
        boolean found = false;
        for (final SolverService service : solverCheckBoxServices) {
            found |= solvers.contains(service);
        }
        if (!found) {
            return;
        }
        for (int i = 0; i < solverCheckBoxes.size(); i++) {
            solverCheckBoxes.get(i).setSelected(solvers.contains(solverCheckBoxServices.get(i)));
        }
        updateSolversButtonText();
        /* Fresh list (all types selected), then deselect the types which were not chosen. */
        rebuildTypeList(false);
        if (types == null) {
            return;
        }
        for (int i = 0; i < typeCheckBoxes.size(); i++) {
            typeCheckBoxes.get(i).setSelected(types.contains(typeCheckBoxTypes.get(i)));
        }
        updateTypesButtonText();
        for (final CAPTCHA_TYPE type : types) {
            if (type.hasTestChallenges()) {
                typeBox.setSelectedItem(type);
                break;
            }
        }
    }

    /** Shows the warning if not a single solver can be selected. */
    private void updateNoSolverWarning() {
        boolean anySelectable = false;
        for (final SolverService service : ChallengeResponseController.getInstance().listServices()) {
            if (isSelectable(service)) {
                anySelectable = true;
                break;
            }
        }
        noSolverWarning.setVisible(!anySelectable);
    }

    /** Fills the site URL/site key/action/expected result fields with the pre-filled test values of the selected captcha type. */
    private void applyDefaults() {
        final CAPTCHA_TYPE type = (CAPTCHA_TYPE) typeBox.getSelectedItem();
        /* A key in the defaults is a value this captcha type uses: only these fields are enabled. */
        final Map<String, Object> defaults = CaptchaTestChallengeFactory.getDefaults(type);
        final boolean usesSiteUrl = defaults.containsKey(CaptchaTestChallengeFactory.KEY_SITE_URL);
        siteUrlField.setText(text(defaults.get(CaptchaTestChallengeFactory.KEY_SITE_URL)));
        siteUrlLabel.setVisible(usesSiteUrl);
        siteUrlField.setVisible(usesSiteUrl);
        final boolean usesSiteKey = defaults.containsKey(CaptchaTestChallengeFactory.KEY_SITE_KEY);
        siteKeyField.setText(text(defaults.get(CaptchaTestChallengeFactory.KEY_SITE_KEY)));
        siteKeyLabel.setVisible(usesSiteKey);
        siteKeyField.setVisible(usesSiteKey);
        final boolean usesChallengeUrl = defaults.containsKey(CaptchaTestChallengeFactory.KEY_CHALLENGE_URL);
        challengeUrlField.setText(text(defaults.get(CaptchaTestChallengeFactory.KEY_CHALLENGE_URL)));
        challengeUrlLabel.setVisible(usesChallengeUrl);
        challengeUrlField.setVisible(usesChallengeUrl);
        /* The proxy is not reset when the captcha type changes: it is the same for all interactive captcha types. */
        final boolean usesProxy = defaults.containsKey(CaptchaTestChallengeFactory.KEY_PROXY);
        proxyLabel.setVisible(usesProxy);
        proxyField.setVisible(usesProxy);
        final boolean usesImage = defaults.containsKey(CaptchaTestChallengeFactory.KEY_IMAGE_FILE);
        imageLabel.setVisible(usesImage);
        imagePanel.setVisible(usesImage);
        setImageSource(defaults.get(CaptchaTestChallengeFactory.KEY_IMAGE_FILE));
        final boolean usesDescription = defaults.containsKey(CaptchaTestChallengeFactory.KEY_DESCRIPTION);
        descriptionLabel.setVisible(usesDescription);
        descriptionField.setVisible(usesDescription);
        descriptionField.setText(text(defaults.get(CaptchaTestChallengeFactory.KEY_DESCRIPTION)));
        final boolean usesCaseSensitive = defaults.containsKey(CaptchaTestChallengeFactory.KEY_CASE_SENSITIVE);
        caseSensitiveLabel.setVisible(usesCaseSensitive);
        caseSensitiveBox.setVisible(usesCaseSensitive);
        final Boolean caseSensitive = (Boolean) defaults.get(CaptchaTestChallengeFactory.KEY_CASE_SENSITIVE);
        caseSensitiveBox.setSelectedIndex(caseSensitive == null ? 0 : (caseSensitive.booleanValue() ? 1 : 2));
        actionField.setText(text(defaults.get(CaptchaTestChallengeFactory.KEY_ACTION)));
        expectedResultField.setText(text(defaults.get(CaptchaTestChallengeFactory.KEY_EXPECTED_RESULT)));
        final boolean usesAction = defaults.containsKey(CaptchaTestChallengeFactory.KEY_ACTION);
        actionLabel.setVisible(usesAction);
        actionField.setVisible(usesAction);
        final boolean usesMinScore = defaults.containsKey(CaptchaTestChallengeFactory.KEY_MIN_SCORE);
        minScoreLabel.setVisible(usesMinScore);
        minScorePanel.setVisible(usesMinScore);
        /* Default: no min score is requested, unless the captcha type has a default min score. */
        final Double defaultMinScore = (Double) defaults.get(CaptchaTestChallengeFactory.KEY_MIN_SCORE);
        minScoreCheckBox.setSelected(usesMinScore && defaultMinScore != null);
        minScoreSpinner.setValue(defaultMinScore != null ? defaultMinScore : Double.valueOf(0.5d));
        minScoreSpinner.setEnabled(defaultMinScore != null);
        final boolean usesMinClicks = defaults.containsKey(CaptchaTestChallengeFactory.KEY_MIN_CLICKS);
        minClicksLabel.setVisible(usesMinClicks);
        minClicksSpinner.setVisible(usesMinClicks);
        final Integer defaultMinClicks = (Integer) defaults.get(CaptchaTestChallengeFactory.KEY_MIN_CLICKS);
        minClicksSpinner.setValue(defaultMinClicks != null ? defaultMinClicks : Integer.valueOf(1));
        revalidate();
        repaint();
        syncMarkers();
    }

    /** Text of a test value for a text field, empty if there is none. */
    private static String text(final Object value) {
        return value == null ? "" : value.toString();
    }

    /** Image captchas cannot be tested without a readable image. */
    private static boolean isTestDataValid(final Map<String, Object> data) {
        if (!data.containsKey(CaptchaTestChallengeFactory.KEY_IMAGE_FILE)) {
            return true;
        }
        final Object image = data.get(CaptchaTestChallengeFactory.KEY_IMAGE_FILE);
        return image instanceof java.net.URL || (image instanceof File && ((File) image).isFile());
    }

    /** Test data which is not valid (e.g. image captchas without a readable image) cannot be tested -> "Start Single Challenge Auto Test" is disabled. */
    private void updateStartButtonEnabled() {
        final CAPTCHA_TYPE type = (CAPTCHA_TYPE) typeBox.getSelectedItem();
        startButton.setEnabled(!batchRunning && isTestDataValid(buildFormData(type)));
    }

    /** The folder that contains the captcha image, or null if there is no image path or the folder does not exist. */
    private File getImageFolder() {
        final File file = getImageAsFile();
        final File folder = file == null ? null : file.getParentFile();
        return folder != null && folder.isDirectory() ? folder : null;
    }

    /** The image as file, or null if there is none or if it lies in a jar (a bundled test image of a standalone version). */
    private File getImageAsFile() {
        if (imageSource instanceof File) {
            return (File) imageSource;
        } else if (imageSource instanceof java.net.URL && "file".equals(((java.net.URL) imageSource).getProtocol())) {
            try {
                return new File(((java.net.URL) imageSource).toURI());
            } catch (final Exception e) {
                return null;
            }
        }
        return null;
    }

    private void updateOpenFolderButton() {
        openFolderButton.setEnabled(getImageFolder() != null);
    }

    /** Shows the given captcha image (or an error text if it cannot be read) in the preview. */
    private void setImageSource(final Object source) {
        imageSource = source;
        previewSource = null;
        previewExpanded = false;
        resultMarkers.clear();
        updateStartButtonEnabled();
        updateOpenFolderButton();
        imagePreview.setIcon(null);
        imagePreview.setText(null);
        imageResolutionLabel.setText(null);
        if (source == null || (source instanceof File && !((File) source).isFile())) {
            imagePreview.setText(source == null ? "No image" : "Image not found: " + source);
            return;
        }
        try {
            final BufferedImage image = source instanceof java.net.URL ? ImageIO.read((java.net.URL) source) : ImageIO.read((File) source);
            if (image == null) {
                imagePreview.setText("Not a readable image: " + source);
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
        imagePreview.setToolTipText(imageSource != null ? (imageSource instanceof File ? ((File) imageSource).getAbsolutePath() : String.valueOf(imageSource)) + " (click to " + (previewExpanded ? "shrink" : "enlarge") + ")" : null);
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
        final File currentFile = getImageAsFile();
        final JFileChooser chooser = new JFileChooser(currentFile != null ? currentFile.getParentFile() : null);
        chooser.setFileFilter(new FileNameExtensionFilter("Images (png, jpg, gif)", "png", "jpg", "jpeg", "gif"));
        if (chooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
            setImageSource(chooser.getSelectedFile());
            expectedResultField.setText("");
        }
    }

    /** The defaults of the captcha type, but with the proxy of the form (it is used for all interactive captcha types). */
    private Map<String, Object> newDefaultsWithProxy(final CAPTCHA_TYPE type) {
        final Map<String, Object> data = CaptchaTestChallengeFactory.getDefaults(type);
        putIfUsed(data, CaptchaTestChallengeFactory.KEY_PROXY, proxyField.getText().trim());
        return data;
    }

    /** Puts the value into the test data, but only if the captcha type uses it (= the key is part of its defaults). */
    private static void putIfUsed(final Map<String, Object> data, final String key, final Object value) {
        if (data.containsKey(key)) {
            data.put(key, value);
        }
    }

    /** Builds the test data from the current form values for the given captcha type: the defaults of the type with the form values. */
    private Map<String, Object> buildFormData(final CAPTCHA_TYPE type) {
        final Map<String, Object> data = CaptchaTestChallengeFactory.getDefaults(type);
        putIfUsed(data, CaptchaTestChallengeFactory.KEY_SITE_KEY, siteKeyField.getText().trim());
        putIfUsed(data, CaptchaTestChallengeFactory.KEY_SITE_URL, siteUrlField.getText().trim());
        putIfUsed(data, CaptchaTestChallengeFactory.KEY_CHALLENGE_URL, challengeUrlField.getText().trim());
        putIfUsed(data, CaptchaTestChallengeFactory.KEY_PROXY, proxyField.getText().trim());
        putIfUsed(data, CaptchaTestChallengeFactory.KEY_ACTION, actionField.getText().trim());
        putIfUsed(data, CaptchaTestChallengeFactory.KEY_EXPECTED_RESULT, expectedResultField.getText());
        putIfUsed(data, CaptchaTestChallengeFactory.KEY_IMAGE_FILE, imageSource);
        putIfUsed(data, CaptchaTestChallengeFactory.KEY_DESCRIPTION, descriptionField.getText());
        putIfUsed(data, CaptchaTestChallengeFactory.KEY_MIN_CLICKS, Integer.valueOf(((Number) minClicksSpinner.getValue()).intValue()));
        switch (caseSensitiveBox.getSelectedIndex()) {
        case 1:
            putIfUsed(data, CaptchaTestChallengeFactory.KEY_CASE_SENSITIVE, Boolean.TRUE);
            break;
        case 2:
            putIfUsed(data, CaptchaTestChallengeFactory.KEY_CASE_SENSITIVE, Boolean.FALSE);
            break;
        default:
            putIfUsed(data, CaptchaTestChallengeFactory.KEY_CASE_SENSITIVE, null);
            break;
        }
        /* Round to one decimal: the spinner steps in 0.1 and floating point noise must not end up in the request. */
        putIfUsed(data, CaptchaTestChallengeFactory.KEY_MIN_SCORE, minScoreCheckBox.isSelected() ? Double.valueOf(Math.round(((Number) minScoreSpinner.getValue()).doubleValue() * 10d) / 10d) : null);
        return data;
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

    /** Applies the limit of the selected solvers (the smallest one) to the max simultaneous tests spinner. */
    private void updateParallelLimit() {
        int limit = MAX_PARALLEL_TESTS;
        for (final SolverService service : getSelectedSolvers()) {
            limit = Math.min(limit, getSolverParallelLimit(service));
        }
        final SpinnerNumberModel model = (SpinnerNumberModel) parallelSpinner.getModel();
        model.setMaximum(Integer.valueOf(limit));
        if (((Number) model.getValue()).intValue() > limit) {
            model.setValue(Integer.valueOf(limit));
        }
        parallelInfoLabel.setText(limit < MAX_PARALLEL_TESTS ? "(limited to " + limit + " by the selected solvers)" : "(max " + MAX_PARALLEL_TESTS + ")");
    }

    /** A solver can only be tested if it is ready and activated in the "Solver overview" tab. */
    public static boolean isSelectable(final SolverService service) {
        return service.isReady() && service.getConfigV3().isEnabled();
    }

    /**
     * Rebuilds the captcha type check boxes from the types which at least one of the selected solvers supports and which have test
     * challenges.
     *
     * @param keepSelection
     *            true: types which were deselected stay deselected (new types are selected), false: all types are selected
     */
    private void rebuildTypeList(final boolean keepSelection) {
        final Map<CAPTCHA_TYPE, Boolean> previous = new java.util.EnumMap<CAPTCHA_TYPE, Boolean>(CAPTCHA_TYPE.class);
        for (int i = 0; i < typeCheckBoxes.size(); i++) {
            previous.put(typeCheckBoxTypes.get(i), Boolean.valueOf(typeCheckBoxes.get(i).isSelected()));
        }
        typesPopup.removeAll();
        typeCheckBoxes.clear();
        typeCheckBoxTypes.clear();
        updateParallelLimit();
        typesPopup.add(selectAllBox);
        typesPopup.addSeparator();
        /* The sum of the captcha types of all selected solvers. */
        final Set<CAPTCHA_TYPE> supported = new java.util.HashSet<CAPTCHA_TYPE>();
        for (final SolverService service : getSelectedSolvers()) {
            if (isSelectable(service)) {
                supported.addAll(service.getSupportedCaptchaTypes());
            }
        }
        {
            for (final CAPTCHA_TYPE type : CAPTCHA_TYPE.values()) {
                if (type.hasTestChallenges() && supported.contains(type)) {
                    /* The check box itself has no text: the captcha type icon and name are a separate label (click toggles the check box). */
                    final Boolean wasSelected = previous.get(type);
                    final JCheckBox box = new JCheckBox("", !keepSelection || wasSelected == null || wasSelected.booleanValue());
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
        runs.add(new TestRun(type, (CaptchaRequestType) sourceBox.getSelectedItem(), buildFormData(type), null));
        startBatch(runs, true);
    }

    /**
     * Runs every selected captcha type with every selected solver which supports it (one test per solver and captcha type). The captcha
     * type selected in the form uses the form values (which are the defaults unless the user changed them), all other types use their
     * default test values. The captcha source always comes from the form.
     */
    private void startMultiTest() {
        final List<SolverService> services = getSelectedSolvers();
        for (final SolverService service : services) {
            if (!isSelectable(service)) {
                /* Deactivated meanwhile: update the selection but do not silently test different solvers than the user picked. */
                refreshSolverSelection();
                return;
            }
        }
        final CAPTCHA_TYPE formType = (CAPTCHA_TYPE) typeBox.getSelectedItem();
        final CaptchaRequestType source = (CaptchaRequestType) sourceBox.getSelectedItem();
        final List<TestRun> runs = new ArrayList<TestRun>();
        for (final SolverService service : services) {
            final Set<CAPTCHA_TYPE> supported = service.getSupportedCaptchaTypes();
            for (int i = 0; i < typeCheckBoxes.size(); i++) {
                final CAPTCHA_TYPE type = typeCheckBoxTypes.get(i);
                if (typeCheckBoxes.get(i).isSelected() && supported.contains(type)) {
                    runs.add(new TestRun(type, source, type == formType ? buildFormData(type) : newDefaultsWithProxy(type), service));
                }
            }
        }
        if (runs.isEmpty()) {
            return;
        }
        startBatch(runs, true);
    }

    /** Max simultaneous tests for the solvers of the given tests: the smallest limit of them. */
    private static int getParallelLimit(final List<TestRun> runs) {
        int limit = MAX_PARALLEL_TESTS;
        final Set<SolverService> services = new java.util.HashSet<SolverService>();
        for (final TestRun run : runs) {
            if (run.service != null && services.add(run.service)) {
                limit = Math.min(limit, getSolverParallelLimit(run.service));
            }
        }
        return limit;
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
            tests = new ArrayList<TestRun>(runs);
            runModel.setRuns(getAllRows());
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
        final int parallel = Math.max(1, Math.min(runs.size(), Math.min(((Number) parallelSpinner.getValue()).intValue(), getParallelLimit(runs))));
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
                                if (run.finished && run.aborted) {
                                    /* Aborted while it was still waiting for its turn. */
                                    return;
                                }
                                if (batchCancelled) {
                                    finishTest(run, RunState.FAILED, "Cancelled", null);
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
        solversButton.setEnabled(!running);
        typesButton.setEnabled(!running);
        multiCancelButton.setEnabled(running);
        if (running) {
            multiStartButton.setEnabled(false);
        } else {
            updateTypesButtonText();
        }
        updateStartButtonEnabled();
    }

    /**
     * Builds a fresh challenge from the snapshot of the test and runs it through the solving pipeline. Runs on the batch thread. Before the
     * challenge is handled, one row per usable solver is added to the table, see {@link #createSolverRows(TestRun, List)}.
     */
    private void executeRun(final TestRun run) {
        /* Clear an interrupt flag that was meant for the previous test of this pool thread. */
        Thread.interrupted();
        run.thread = Thread.currentThread();
        run.started = true;
        run.maxTimeoutMillis = 0;
        run.statusText = "Preparing test data...";
        run.startTime = System.currentTimeMillis();
        run.state = RunState.RUNNING;
        runChanged(run, false);
        /* Prepared right now (not when the test was queued), e.g. ALTCHA needs a fresh challenge. */
        final Challenge<Object> newChallenge;
        try {
            @SuppressWarnings("unchecked")
            final Challenge<Object> built = (Challenge<Object>) CaptchaTestChallengeFactory.newChallenge(run.source, run.type, run.data);
            newChallenge = built;
        } catch (final Throwable e) {
            LogController.CL().log(e);
            run.thread = null;
            finishTest(run, RunState.FAILED, batchCancelled ? "Cancelled" : (run.aborted ? "Aborted" : "Failed: " + e.getMessage()), null);
            return;
        }
        run.challenge = newChallenge;
        run.statusText = "Waiting for a solver...";
        try {
            final List<ChallengeSolver<Object>> solvers = ChallengeResponseController.getInstance().getTestSolvers(newChallenge, run.service);
            createSolverRows(run, solvers);
            final SolverJob<Object> job = ChallengeResponseController.getInstance().handleTest(newChallenge, solvers);
            run.finishedJob = job;
            if (batchCancelled) {
                finishTest(run, RunState.FAILED, "Cancelled", null);
            } else if (run.aborted) {
                finishTest(run, RunState.FAILED, "Aborted", null);
            } else {
                /* Final state of every solver row (answer, error, not used, ...). */
                syncSolverRows(run, job, true);
                finishTest(run, RunState.FAILED, "No answer received (" + job.getStatusDetail() + ")", null);
            }
        } catch (final InterruptedException e) {
            finishTest(run, RunState.FAILED, batchCancelled ? "Cancelled" : "Aborted", null);
        } catch (final SkipException e) {
            syncLiveJobBeforeFinish(run);
            finishTest(run, RunState.FAILED, batchCancelled ? "Cancelled" : (run.aborted ? "Aborted" : "Skipped: " + e.getSkipRequest()), null);
        } catch (final Throwable e) {
            LogController.CL().log(e);
            finishTest(run, RunState.FAILED, batchCancelled ? "Cancelled" : "Failed: " + e.getClass().getSimpleName() + ": " + e.getMessage(), null);
        } finally {
            run.thread = null;
            run.challenge = null;
            run.liveJob = null;
        }
    }

    /** A job that ended by itself (e.g. timeout) without an exception of the user: the solver rows get the results of their solvers. */
    private void syncLiveJobBeforeFinish(final TestRun run) {
        final SolverJob<?> job = run.liveJob;
        if (job != null && !batchCancelled && !run.aborted) {
            syncSolverRows(run, job, true);
        }
    }

    /**
     * Ends the test: All rows of the test which are still waiting/running get the given state, answer and status text (rows that already
     * have their result keep it), then the GUI is updated.
     */
    private void finishTest(final TestRun run, final RunState state, final String answer, final String statusText) {
        synchronized (run) {
            final long now = System.currentTimeMillis();
            final boolean userEnded = run.aborted || batchCancelled;
            for (final TestRun row : run.rows) {
                /* Rows with an answer keep it. If the user ended the test, "not used" rows are cancelled like all others. */
                if (row.state == RunState.PENDING || row.state == RunState.RUNNING || (userEnded && row.state == RunState.NOT_USED)) {
                    row.endTime = now;
                    row.answer = answer;
                    row.statusText = statusText != null ? statusText : (state == RunState.FAILED ? answer : "");
                    row.state = state;
                }
            }
            run.endTime = run.endTime > 0 ? run.endTime : now;
            run.finished = true;
        }
        runChanged(run, true);
    }

    /**
     * Adds one row for every solver that can be used for the test (the first one is the row of the test itself). The rows exist from the
     * start of the test, so solvers which are not used in the end are listed too.
     */
    private <X> void createSolverRows(final TestRun run, final List<ChallengeSolver<X>> solvers) {
        if (solvers.isEmpty()) {
            /* The test fails with "no solver" -> stays one row. */
            return;
        }
        synchronized (run) {
            run.rows.clear();
            for (int i = 0; i < solvers.size(); i++) {
                final TestRun row = i == 0 ? run : new TestRun(run);
                row.solver = solvers.get(i);
                row.solverService = row.solver.getService();
                row.startTime = run.startTime;
                row.state = RunState.RUNNING;
                row.statusText = "Waiting for a solver...";
                row.maxTimeoutMillis = row.solver.getFinalTimeoutMillis();
                run.rows.add(row);
            }
        }
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                rebuildTable();
            }
        };
    }

    /** All rows of all tests in table order. */
    private List<TestRun> getAllRows() {
        final List<TestRun> rows = new ArrayList<TestRun>();
        for (final TestRun test : tests) {
            rows.addAll(test.rows);
        }
        return rows;
    }

    /** Applies the current rows of all tests to the table (new solver rows, or only the one row of a test after a retry). Runs on the EDT. */
    private void rebuildTable() {
        final TestRun selected = selectedRun;
        runModel.setRuns(getAllRows());
        updateRunScrollHeight();
        TestRun target = selected;
        if (target != null && runModel.getRowforObject(target) < 0) {
            /* The selected row is gone (retry): select the row of the test. */
            target = target.lead;
        }
        final int index = target == null ? -1 : runModel.getRowforObject(target);
        if (index >= 0) {
            runTable.setRowSelectionInterval(index, index);
        }
    }

    /**
     * Updates all solver rows of the test from the job: answers are shown as soon as a solver delivered one, solvers that are still working
     * or waiting are running rows, all others show what happened to them. With finalSync (the job is over) no row stays in a running
     * state.
     */
    private <X> void syncSolverRows(final TestRun run, final SolverJob<X> job, final boolean finalSync) {
        if (run.aborted || batchCancelled) {
            /*
             * The user ended the test: finishTest marks all unfinished rows the same way. Without this, the results which the killed solvers
             * report ("aborted") would turn some rows into "Not used", depending on timing.
             */
            return;
        }
        synchronized (run) {
            final ResponseList<X> winning = job.getResponse();
            for (final TestRun row : run.rows) {
                final ChallengeSolver<?> solver = row.solver;
                if (solver == null) {
                    continue;
                }
                row.durationMillis = job.getSolverWorkingMillis(solver);
                row.maxTimeoutMillis = solver.getFinalTimeoutMillis();
                final AbstractResponse<X> response = findResponse(job, solver);
                final SolverJob.SolverResult result = job.getSolverResult(solver);
                if (response != null) {
                    applyAnswer(run, row, response, finalSync, winning != null && winning.contains(response));
                } else if (result != null) {
                    applyOutcome(row, result);
                } else if (finalSync) {
                    if (job.isSolved()) {
                        setRowFinal(row, RunState.NOT_USED, "Aborted (solved by another solver)");
                    } else {
                        setRowFinal(row, RunState.FAILED, job.getStatusDetail());
                    }
                } else if (job.isSolverSolving(solver)) {
                    row.state = RunState.RUNNING;
                    row.statusText = "Solving (timeout: " + formatTimeout(row.maxTimeoutMillis) + ")";
                } else {
                    row.state = RunState.RUNNING;
                    row.statusText = "Waiting for other solvers...";
                }
            }
        }
    }

    /** The response which the given solver delivered to the job, or null. */
    private static <X> AbstractResponse<X> findResponse(final SolverJob<X> job, final ChallengeSolver<?> solver) {
        final List<ResponseList<X>> lists = job.getResponses();
        if (lists != null) {
            for (final ResponseList<X> list : lists) {
                for (final AbstractResponse<X> response : list) {
                    if (response.getSolver() == solver) {
                        return response;
                    }
                }
            }
        }
        return null;
    }

    private static void setRowFinal(final TestRun row, final RunState state, final String text) {
        if (row.endTime <= 0) {
            row.endTime = System.currentTimeMillis();
        }
        row.answer = text;
        row.statusText = text;
        row.state = state;
    }

    /** The solver delivered an answer: compared with the expected result (if any). Feedback is only possible for the answer that is used. */
    private <X> void applyAnswer(final TestRun run, final TestRun row, final AbstractResponse<X> response, final boolean finalSync, final boolean winning) {
        final X value = response.getValue();
        if (value == null) {
            setRowFinal(row, RunState.FAILED, "No answer received");
            return;
        }
        final String answer = formatAnswer(value);
        final String expected = (String) run.data.get(CaptchaTestChallengeFactory.KEY_EXPECTED_RESULT);
        final String solvedBy = "Solved by: " + response.getSolver();
        if (value instanceof MultiClickedPoint && row.markers.isEmpty()) {
            /* Remember all clicked positions to mark them in the image. */
            final MultiClickedPoint points = (MultiClickedPoint) value;
            for (int i = 0; i < points.getX().length; i++) {
                row.markers.add(new Point(points.getX()[i], points.getY()[i]));
            }
        }
        if (row.endTime <= 0) {
            row.endTime = System.currentTimeMillis();
        }
        row.answer = answer;
        final boolean hasExpected = !StringUtils.isEmpty(expected);
        final boolean matches = hasExpected && expected.equals(answer);
        final boolean unexpected = hasExpected && !matches;
        if (unexpected) {
            row.expectedText = expected;
            row.statusText = solvedBy + " | does NOT match the expected result";
            row.state = RunState.PASSED_UNEXPECTED_ANSWER;
        } else {
            row.statusText = matches ? solvedBy + " | matches the expected result" : solvedBy;
            row.state = RunState.PASSED;
        }
        if (finalSync && winning) {
            /* A correct answer cannot be reported as incorrect and vice versa. */
            row.canCorrect = !unexpected;
            row.canIncorrect = !matches;
            /* The solver that answered decides whether it accepts feedback. */
            final Object solver = response.getSolver();
            if (solver instanceof ChallengeSolver) {
                final Set<SolverFeature> feedbackTypes = ((ChallengeSolver<?>) solver).getSolverFeatures();
                /* Null or empty = the solver does not support any feedback. */
                row.correctUnsupported = feedbackTypes == null || !feedbackTypes.contains(SolverFeature.REPORT_VALID_CAPTCHAS);
                row.incorrectUnsupported = feedbackTypes == null || !feedbackTypes.contains(SolverFeature.REPORT_INVALID_CAPTCHAS);
            }
        }
    }

    /** The solver did not deliver an answer: shows why. Solvers which were not used are no failure. */
    private static void applyOutcome(final TestRun row, final SolverJob.SolverResult result) {
        final String message = result.getMessage();
        String text;
        RunState state = RunState.FAILED;
        switch (result.getOutcome()) {
        case ABORTED:
            state = RunState.NOT_USED;
            text = "Aborted";
            break;
        case NOT_STARTED:
            state = RunState.NOT_USED;
            text = "Not started";
            break;
        case TIMED_OUT:
            text = "Timeout";
            break;
        case ANSWER_REJECTED:
            text = "Answer rejected";
            break;
        case UNSOLVABLE:
            text = "Unsolvable";
            break;
        case SOLVER_ERROR_ACCOUNT:
            text = "Account error";
            break;
        case ANSWERED:
        case SOLVER_ERROR:
        default:
            text = "Error";
            break;
        }
        setRowFinal(row, state, message != null ? text + ": " + message : text);
    }

    /** Updates the table rows of the given test and, if one of them is the selected one, the details. */
    private void runChanged(final TestRun run, final boolean finished) {
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                for (final TestRun row : run.rows) {
                    runModel.fireRunChanged(row);
                }
                if (finished && !userSorted) {
                    /* Passed first, then failed, running, pending. */
                    runModel.refreshSort();
                }
                final TestRun selected = selectedRun;
                if (selected != null && selected.lead == run) {
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
                for (final TestRun test : tests) {
                    if (test.started && !test.finished) {
                        final Challenge<Object> current = test.challenge;
                        if (current != null) {
                            final SolverJob<?> job = ChallengeResponseController.getInstance().getJobByChallengeId(current.getId().getID());
                            if (job != null) {
                                syncSolverRows(test, job, false);
                            }
                        }
                    }
                }
                /*
                 * Only a repaint, no table model event: A row update every 100 ms cancels the cell editor of the answer column (the cancel
                 * button) again and again, which makes the column flicker. The values are read from the rows when painting.
                 */
                runTable.repaint();
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
        final boolean visible = tests.size() > 1;
        batchBar.setVisible(visible);
        /* The animation shows that at least one test is still running. */
        activityBar.setVisible(visible && batchRunning);
        int finished = 0;
        for (final TestRun run : runs) {
            if (run.finished) {
                finished++;
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
    private void retryRun(final TestRun row) {
        /* The whole test is run again: it is one row again until its solvers are known. */
        final TestRun run = row.lead;
        run.reset();
        rebuildTable();
        final List<TestRun> runs = new ArrayList<TestRun>();
        runs.add(run);
        startBatch(runs, false);
    }

    /** True while a test is waiting for its turn or running, i.e. it can still be cancelled. Cancelling a row cancels the whole test. */
    private static boolean isCancellable(final TestRun run) {
        return !run.lead.finished && (run.state == RunState.PENDING || run.state == RunState.RUNNING);
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
        case NOT_USED:
            resultArea.setForeground(Color.GRAY);
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
        abortButton.setEnabled(isCancellable(run));
        retryButton.setEnabled(isRetryable(run));
        runPanel.revalidate();
        revalidate();
    }

    private void sendFeedback(final boolean correct) {
        final TestRun run = selectedRun;
        if (run == null) {
            return;
        }
        final SolverJob<Object> job = run.lead.finishedJob;
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
        if (elapsedMillis < 0) {
            /* The solver of the row did not start working yet. */
            return "";
        }
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
        case NOT_USED:
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

    /** Duration of a test (row): until its end, or until now while it is running. */
    private static long getDurationMillis(final TestRun run) {
        if (run.solver != null) {
            /* Row of a solver: the time that this solver worked (-1 = did not start). */
            return run.durationMillis;
        }
        final long start = run.startTime;
        if (start <= 0) {
            return -1;
        }
        final long end = run.endTime;
        return (end > 0 ? end : System.currentTimeMillis()) - start;
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
     * Aborts a test (a row stands for its whole test): a running one is killed (the batch continues with the next test), one that did not
     * start yet is marked as failed right away.
     */
    private void abortRun(final TestRun row, final boolean cancelled) {
        final TestRun run = row.lead;
        run.aborted = true;
        if (run.finished) {
            return;
        }
        if (run.started) {
            killLiveJob(run);
            final Thread thread = run.thread;
            if (thread != null && thread.isAlive()) {
                thread.interrupt();
            }
        } else {
            finishTest(run, RunState.FAILED, cancelled ? "Cancelled" : "Aborted", null);
        }
    }

    /** Cancels the running test and all tests that did not start yet. */
    private void cancelBatch() {
        batchCancelled = true;
        for (final TestRun run : tests) {
            abortRun(run, true);
        }
    }

    /** State of one test. */
    private enum RunState {
        PENDING,
        RUNNING,
        PASSED,
        PASSED_UNEXPECTED_ANSWER,
        FAILED,
        /* The solver of the row did not work on the test (e.g. aborted because another solver was faster). */
        NOT_USED;

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
                return 3;
            case NOT_USED:
            default:
                return 4;
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
            case NOT_USED:
                return "Not used";
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
            case NOT_USED:
                return Color.GRAY;
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
            case NOT_USED:
            default:
                return null;
            }
        }
    }

    /**
     * One table row: a test (captcha type) and one solver of it. A test owns one row per solver that can be used (lead = the first row,
     * which also executes the test): The state, answer, times and markers belong to the row, everything that belongs to the test
     * (thread, challenge, job, abort) is only used on the lead.
     */
    private static class TestRun {
        private final CAPTCHA_TYPE                type;
        private final CaptchaRequestType          source;
        private final Map<String, Object>         data;
        /* null = all solvers, otherwise only this one. */
        private final SolverService               service;
        /* The row which executes the test (this for the first row). */
        private final TestRun                     lead;
        /* All rows of the test, the lead is the first (only used on the lead). */
        private final List<TestRun>               rows    = new CopyOnWriteArrayList<TestRun>();
        /* The solver of this row, null as long as the solvers of the test are not known. */
        private volatile ChallengeSolver<?>       solver;
        /* The service shown in the solver column: the chosen one until the solver is known. */
        private volatile SolverService            solverService;
        /* How long the solver worked on the test, -1 if it did not start. */
        private volatile long                     durationMillis;
        private volatile boolean                  started;
        private volatile boolean                  finished;
        private volatile RunState                 state;
        private volatile long                     startTime;
        private volatile long                     endTime;
        private volatile String                   answer;
        private volatile String                   statusText;
        private volatile String                   expectedText;
        private volatile boolean                  canCorrect;
        private volatile boolean                  canIncorrect;
        private volatile boolean                  correctUnsupported;
        private volatile boolean                  incorrectUnsupported;
        private volatile boolean                  aborted;
        private volatile long                     maxTimeoutMillis;
        private volatile Challenge<Object>        challenge;
        /* Pool thread that currently executes this test. */
        private volatile Thread                   thread;
        private volatile SolverJob<Object>        finishedJob;
        /* The job while the test is handled. */
        private volatile SolverJob<?>             liveJob;
        private final List<Point>                 markers = new CopyOnWriteArrayList<Point>();

        private TestRun(final CAPTCHA_TYPE type, final CaptchaRequestType source, final Map<String, Object> data, final SolverService service) {
            this.type = type;
            this.source = source;
            this.data = data;
            this.service = service;
            this.lead = this;
            reset();
        }

        /** Additional row of the test of the given lead row (one more solver). */
        private TestRun(final TestRun lead) {
            this.type = lead.type;
            this.source = lead.source;
            this.data = lead.data;
            this.service = lead.service;
            this.lead = lead;
            resetRow();
        }

        /** Back to the state before the test was started: one row (this one) without solver. Only for the lead. */
        private void reset() {
            resetRow();
            aborted = false;
            started = false;
            finished = false;
            challenge = null;
            finishedJob = null;
            liveJob = null;
            solver = null;
            solverService = service;
            rows.clear();
            rows.add(this);
        }

        private void resetRow() {
            state = RunState.PENDING;
            startTime = 0;
            endTime = 0;
            durationMillis = -1;
            answer = "";
            statusText = "";
            expectedText = null;
            canCorrect = false;
            canIncorrect = false;
            correctUnsupported = false;
            incorrectUnsupported = false;
            maxTimeoutMillis = 0;
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
        protected boolean isDisplayFullsizeAlways() {
            return true;
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
                final JMenuItem copyRow = new JMenuItem("Copy row", NewTheme.I().getIcon(IconKey.ICON_COPY, 16));
                copyRow.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(final ActionEvent event) {
                        copyToClipboard(contextObject.type.getDisplayName() + "\t" + getSolverName(contextObject) + "\t" + contextObject.state.getText() + "\t" + formatDurationText(contextObject) + "\t" + contextObject.answer);
                    }
                });
                popup.add(copyRow);
                final JMenuItem copyAnswer = new JMenuItem("Copy answer", NewTheme.I().getIcon(IconKey.ICON_COPY, 16));
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
    private static String getSolverName(final TestRun row) {
        final SolverService service = row.solverService;
        return service != null ? service.getName() : "";
    }

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
            /* The solver of the row; as long as it is not known the chosen solver (empty if there is none). */
            addColumn(new ExtTextColumn<TestRun>("Solver") {
                private static final long serialVersionUID = 1L;

                @Override
                public String getStringValue(final TestRun value) {
                    return getSolverName(value);
                }

                @Override
                protected javax.swing.Icon getIcon(final TestRun value) {
                    final SolverService service = value.solverService;
                    return service != null ? service.getIcon(16) : null;
                }

                @Override
                public int getDefaultWidth() {
                    return 160;
                }
            });
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
