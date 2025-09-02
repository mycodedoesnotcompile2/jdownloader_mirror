package org.jdownloader.gui.views.linkgrabber.addlinksdialog;

import java.awt.Component;
import java.awt.Dialog.ModalityType;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPopupMenu;
import javax.swing.JRootPane;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ListCellRenderer;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import org.appwork.scheduler.DelayedRunnable;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.storage.config.JsonConfig;
import org.appwork.swing.MigPanel;
import org.appwork.swing.components.ExtButton;
import org.appwork.swing.components.ExtCheckBox;
import org.appwork.swing.components.ExtMergedIcon;
import org.appwork.swing.components.ExtTextArea;
import org.appwork.swing.components.ExtTextField;
import org.appwork.swing.components.pathchooser.PathChooser;
import org.appwork.swing.components.searchcombo.SearchComboBox;
import org.appwork.uio.UIOManager;
import org.appwork.utils.StringUtils;
import org.appwork.utils.swing.EDTHelper;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.dialog.AbstractDialog;
import org.appwork.utils.swing.dialog.DefaultButtonPanel;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;
import org.appwork.utils.swing.dialog.dimensor.RememberLastDialogDimension;
import org.appwork.utils.swing.dialog.locator.RememberRelativeDialogLocator;
import org.jdownloader.actions.AppAction;
import org.jdownloader.controlling.PasswordUtils;
import org.jdownloader.controlling.Priority;
import org.jdownloader.controlling.packagizer.PackagizerController;
import org.jdownloader.extensions.extraction.BooleanStatus;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.helpdialogs.HelpDialog;
import org.jdownloader.gui.packagehistorycontroller.DownloadPathHistoryManager;
import org.jdownloader.gui.packagehistorycontroller.PackageHistoryEntry;
import org.jdownloader.gui.packagehistorycontroller.PackageHistoryManager;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.DownloadFolderChooserDialog;
import org.jdownloader.gui.views.linkgrabber.actions.AddLinksAction.EnableDisableUnchanged;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.images.BadgeIcon;
import org.jdownloader.logging.LogController;
import org.jdownloader.settings.GeneralSettings;
import org.jdownloader.settings.staticreferences.CFG_GUI;
import org.jdownloader.settings.staticreferences.CFG_LINKGRABBER;
import org.jdownloader.updatev2.gui.LAFOptions;

import jd.controlling.ClipboardMonitoring;
import jd.controlling.ClipboardMonitoring.ClipboardContent;
import jd.controlling.linkcollector.LinkCollectingJob;
import jd.controlling.linkcollector.LinkOrigin;
import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.CrawledLinkModifier;
import jd.controlling.linkcrawler.CrawledLinkModifiers;
import jd.controlling.linkcrawler.LinkCrawler;
import jd.controlling.linkcrawler.modifier.CommentModifier;
import jd.controlling.linkcrawler.modifier.DownloadFolderModifier;
import jd.controlling.linkcrawler.modifier.PackageNameModifier;
import jd.gui.swing.jdgui.JDGui;
import jd.gui.swing.jdgui.views.settings.panels.packagizer.VariableAction;
import jd.parser.html.HTMLParser;
import jd.parser.html.HTMLParser.HtmlParserCharSequence;
import jd.parser.html.HTMLParser.HtmlParserResultSet;
import jd.plugins.DownloadLink;

public class AddLinksDialog extends AbstractDialog<LinkCollectingJob> {
    private ExtTextArea                         input;
    private PathChooser                         destination;
    private SearchComboBox<PackageHistoryEntry> packagename;
    private JScrollPane                         sp;
    private LinkgrabberSettings                 config;
    private ExtTextField                        password;
    private ExtCheckBox                         extractToggle;
    private EnableDisableUnchanged              autoStart = EnableDisableUnchanged.UNCHANGED;

    /**
     * @return the autoStart
     */
    public EnableDisableUnchanged getAutoStart() {
        return autoStart;
    }

    /**
     * @param autoStart
     *            the autoStart to set
     */
    public void setAutoStart(EnableDisableUnchanged autoStart) {
        this.autoStart = autoStart;
    }

    /**
     * @return the autoConfirm
     */
    public EnableDisableUnchanged getAutoConfirm() {
        return autoConfirm;
    }

    /**
     * @param autoConfirm
     *            the autoConfirm to set
     */
    public void setAutoConfirm(EnableDisableUnchanged autoConfirm) {
        this.autoConfirm = autoConfirm;
    }

    private EnableDisableUnchanged autoConfirm                              = EnableDisableUnchanged.UNCHANGED;
    private JButton                confirmOptions;
    private boolean                deepAnalyse                              = false;
    private DelayedRunnable        delayedValidate;
    private ExtTextField           downloadPassword;
    private JComboBox              priority;
    private final HashSet<String>  autoPasswords                            = new HashSet<String>();
    private ExtTextField           comment;
    private JCheckBox              overwritePackagizer;
    private boolean                hasUserClickedOverwritePackagizerButton = false;
    /*
     * Dummy value for a possible future setting to allow user to enable/disable "overwrite Packagizer rule" checkbox based on other fields'
     * user inputs in AddLinksDialog.
     */
    private final boolean          packagizerAutoModeEnabled                = true;

    public boolean isDeepAnalyse() {
        return deepAnalyse;
    }

    public void setDeepAnalyse(boolean deepAnalyse) {
        this.deepAnalyse = deepAnalyse;
    }

    public AddLinksDialog() {
        super(UIOManager.BUTTONS_HIDE_OK, _GUI.T.AddLinksDialog_AddLinksDialog_(), null, _GUI.T.AddLinksDialog_AddLinksDialog_confirm(), null);
        config = JsonConfig.create(LinkgrabberSettings.class);
        final String dialog_id = "AddLinksDialog";
        delayedValidate = new DelayedRunnable(500l, 10000l) {
            @Override
            public String getID() {
                return dialog_id;
            }

            @Override
            public void delayedrun() {
                if (config.getAddLinksDialogCheck()) {
                    validateForm();
                }
            }
        };
        setPreferredWidth(700);
        setPreferredHeight(300);
        setLocator(new RememberRelativeDialogLocator(dialog_id, JDGui.getInstance().getMainFrame()));
        setDimensor(new RememberLastDialogDimension(dialog_id));
    }

    @Override
    public ModalityType getModalityType() {
        return ModalityType.MODELESS;
    }

    @Override
    protected MigPanel createBottomPanel() {
        MigPanel ret = new MigPanel("ins 0", "[][][][]20[grow,fill][]", "[]");
        JLabel lbl = new JLabel(_GUI.T.AddLinksDialog_getDefaultButtonPanel_overwrite_packagizer());
        overwritePackagizer = new JCheckBox();
        overwritePackagizer.setSelected(CFG_LINKGRABBER.CFG.isAddLinksDialogOverwritesPackagizerRulesEnabled());
        overwritePackagizer.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                hasUserClickedOverwritePackagizerButton = true;
            }
        });
        ret.add(new JLabel(new AbstractIcon(IconKey.ICON_UPLOAD, 22)), "gapleft 5");
        ret.add(overwritePackagizer, "gapleft 0");
        ret.add(lbl);
        return ret;
    }

    public boolean isOverwritePackagizerEnabled() {
        return overwritePackagizer.isSelected();
    }

    @Override
    protected void setReturnmask(boolean b) {
        super.setReturnmask(b);
        if (b && hasUserClickedOverwritePackagizerButton) {
            /* Update config so next time the dialog is opened, the value is remembered. */
            CFG_LINKGRABBER.CFG.setAddLinksDialogOverwritesPackagizerRulesEnabled(overwritePackagizer.isSelected());
        }
    }

    @Override
    protected DefaultButtonPanel getDefaultButtonPanel() {
        final DefaultButtonPanel ret = new DefaultButtonPanel("ins 0 0 0 0", "[grow,fill]0[][]", "0[fill]0");
        confirmOptions = new JButton(new ConfirmOptionsAction(okButton, this)) {
            public void setBounds(int x, int y, int width, int height) {
                super.setBounds(x - 1, y, width + 1, height);
            }
        };
        // Set OK as defaultbutton
        this.getDialog().getRootPane().setDefaultButton(this.okButton);
        this.okButton.addHierarchyListener(new HierarchyListener() {
            public void hierarchyChanged(final HierarchyEvent e) {
                if ((e.getChangeFlags() & HierarchyEvent.PARENT_CHANGED) != 0) {
                    final JButton defaultButton = (JButton) e.getComponent();
                    final JRootPane root = SwingUtilities.getRootPane(defaultButton);
                    if (root != null) {
                        root.setDefaultButton(defaultButton);
                    }
                }
            }
        });
        ret.add(this.okButton, "alignx right,sizegroup confirms,growx,pushx");
        ret.add(confirmOptions, "width 12!");
        return ret;
    }

    @Override
    public void dispose() {
        try {
            if (isInitialized() && destination != null) {
                final String finalDestination = destination.getFile() != null ? destination.getFile().getAbsolutePath() : null;
                if (StringUtils.isNotEmpty(finalDestination)) {
                    DownloadPathHistoryManager.getInstance().add(finalDestination);
                }
            }
        } finally {
            super.dispose();
        }
    }

    @Override
    protected LinkCollectingJob createReturnValue() {
        final LinkCollectingJob job = new LinkCollectingJob(LinkOrigin.ADD_LINKS_DIALOG.getLinkOriginDetails(), input.getText());
        job.setDeepAnalyse(isDeepAnalyse());
        final List<CrawledLinkModifier> modifiers = new ArrayList<CrawledLinkModifier>();
        final List<CrawledLinkModifier> requiredPreModifiers = new ArrayList<CrawledLinkModifier>();
        final boolean overwritePackagizerRules = isOverwritePackagizerEnabled();
        final String finalPackageName = getPackageName();
        if (StringUtils.isNotEmpty(finalPackageName)) {
            PackageHistoryManager.getInstance().add(finalPackageName);
            final PackageNameModifier mod = new PackageNameModifier(finalPackageName, overwritePackagizerRules);
            modifiers.add(mod);
            requiredPreModifiers.add(mod);
        }
        final String finalComment = getComment();
        if (StringUtils.isNotEmpty(finalComment)) {
            modifiers.add(new CommentModifier(finalComment));
        }
        final String finalDestination = getDestination();
        if (StringUtils.isNotEmpty(finalDestination)) {
            modifiers.add(new DownloadFolderModifier(finalDestination, overwritePackagizerRules));
        }
        final String finalDownloadPassword = getDownloadPassword();
        if (StringUtils.isNotEmpty(finalDownloadPassword)) {
            job.setCrawlerPassword(finalDownloadPassword);
            modifiers.add(new CrawledLinkModifier() {
                @Override
                public boolean modifyCrawledLink(CrawledLink link) {
                    final DownloadLink dlLink = link.getDownloadLink();
                    if (dlLink != null) {
                        if (overwritePackagizerRules || StringUtils.isEmpty(dlLink.getDownloadPassword())) {
                            dlLink.setDownloadPassword(finalDownloadPassword);
                            return true;
                        }
                    }
                    return false;
                }
            });
        }
        final Priority finalPriority = getPriority();
        if (finalPriority != null && !Priority.DEFAULT.equals(finalPriority)) {
            modifiers.add(new CrawledLinkModifier() {
                @Override
                public boolean modifyCrawledLink(CrawledLink link) {
                    link.setPriority(finalPriority);
                    return true;
                }
            });
        }
        // there is no GUI for this - set externally
        final EnableDisableUnchanged ac = getAutoConfirm();
        if (ac != null && ac != EnableDisableUnchanged.UNCHANGED) {
            modifiers.add(new CrawledLinkModifier() {
                @Override
                public boolean modifyCrawledLink(CrawledLink link) {
                    link.setAutoConfirmEnabled(ac == EnableDisableUnchanged.ENABLED);
                    return true;
                }
            });
        }
        // there is no GUI for this - set externally
        final EnableDisableUnchanged as = getAutoStart();
        if (as != null && as != EnableDisableUnchanged.UNCHANGED) {
            modifiers.add(new CrawledLinkModifier() {
                @Override
                public boolean modifyCrawledLink(CrawledLink link) {
                    link.setAutoStartEnabled(as == EnableDisableUnchanged.ENABLED);
                    return true;
                }
            });
        }
        final String passwordTxt = password.getText();
        if (StringUtils.isNotEmpty(passwordTxt)) {
            HashSet<String> passwords = JSonStorage.restoreFromString(passwordTxt, new TypeRef<HashSet<String>>() {
            }, new HashSet<String>());
            if (passwords == null || passwords.size() == 0) {
                passwords = new HashSet<String>();
                passwords.add(passwordTxt.trim());
            }
            final HashSet<String> finalPasswords = passwords;
            job.setArchivPasswords(new ArrayList<String>(passwords));
            modifiers.add(new CrawledLinkModifier() {
                @Override
                public boolean modifyCrawledLink(CrawledLink link) {
                    link.getArchiveInfo().getExtractionPasswords().addAll(finalPasswords);
                    return true;
                }
            });
        }
        final BooleanStatus extractAfterDownload = this.extractToggle.isSelected() ? BooleanStatus.TRUE : BooleanStatus.FALSE;
        if (BooleanStatus.isSet(extractAfterDownload)) {
            modifiers.add(new CrawledLinkModifier() {
                @Override
                public boolean modifyCrawledLink(CrawledLink link) {
                    link.getArchiveInfo().setAutoExtract(extractAfterDownload);
                    return true;
                }
            });
        }
        if (modifiers.size() > 0) {
            if (overwritePackagizerRules) {
                job.addPrePackagizerModifier(new CrawledLinkModifiers(requiredPreModifiers));
                job.addPostPackagizerModifier(new CrawledLinkModifiers(modifiers));
            } else {
                job.addPrePackagizerModifier(new CrawledLinkModifiers(modifiers));
            }
        }
        return job;
    }

    public void actionPerformed(final ActionEvent e) {
        super.actionPerformed(e);
    }

    public String getPackageName() {
        return this.packagename.getText().trim();
    }

    public String getComment() {
        return new EDTHelper<String>() {
            @Override
            public String edtRun() {
                return comment.getText();
            }
        }.getReturnValue().trim();
    }

    public String getDestination() {
        return destination.getFile() != null ? destination.getFile().getAbsolutePath() : null;
    }

    public String getDownloadPassword() {
        return downloadPassword.getText();
    }

    public Priority getPriority() {
        return (Priority) priority.getSelectedItem();
    }

    @Override
    public JComponent layoutDialogContent() {
        destination = new PathChooser("ADDLinks", true) {
            {
                txt.setDragEnabled(true);
            }

            protected void onChanged(ExtTextField txt2) {
                delayedValidate.run();
                final File file = this.getFile();
                if (file == null) {
                    return;
                }
                final String result = file.getAbsolutePath();
                final String defaultDownloadDestinationFieldValue = getDownloadDestinationFieldDefaultValue();
                if (StringUtils.isEmpty(result)) {
                    return;
                }
                if (result.equals(defaultDownloadDestinationFieldValue)) {
                    autoDisablePackagizerCheckbox();
                } else {
                    autoEnablePackagizerCheckbox();
                }
            }

            @Override
            public JPopupMenu getPopupMenu(ExtTextField txt, AbstractAction cutAction, AbstractAction copyAction, AbstractAction pasteAction, AbstractAction deleteAction, AbstractAction selectAction) {
                JPopupMenu menu = new JPopupMenu();
                menu.add(new VariableAction(txt, _GUI.T.PackagizerFilterRuleDialog_createVariablesMenu_date(), "<jd:" + PackagizerController.SIMPLEDATE + ":dd.MM.yyyy>"));
                menu.add(new VariableAction(txt, _GUI.T.PackagizerFilterRuleDialog_createVariablesMenu_packagename(), "<jd:" + PackagizerController.PACKAGENAME + ">"));
                return menu;
            }

            public File doFileChooser() {
                try {
                    return DownloadFolderChooserDialog.open(getFile(), true, getDialogTitle());
                } catch (DialogClosedException e) {
                    e.printStackTrace();
                } catch (DialogCanceledException e) {
                    e.printStackTrace();
                }
                return null;
            }

            @Override
            protected String getProtoType(SearchComboBox<String> comboBox, List<String> model) {
                return "";
            }

            protected String getHelpText() {
                return _GUI.T.AddLinksDialog_layoutDialogContent_help_destination();
            }
        };
        packagename = new SearchComboBox<PackageHistoryEntry>() {
            @Override
            protected boolean isSearchCaseSensitive() {
                return true;
            }

            @Override
            protected Icon getIconForValue(PackageHistoryEntry value) {
                return null;
            }

            @Override
            public PackageHistoryEntry getProtoType(List<PackageHistoryEntry> model) {
                final PackageHistoryEntry ret = new PackageHistoryEntry("");
                return ret;
            }

            @Override
            public JTextField createTextField() {
                final JTextField ret = super.createTextField();
                ret.setDragEnabled(true);
                return ret;
            }

            @Override
            protected String getTextForValue(PackageHistoryEntry value) {
                return value == null ? null : value.getName();
            }

            @Override
            public void onChanged() {
                if (!StringUtils.isEmpty(this.getText())) {
                    /* Non default value */
                    autoEnablePackagizerCheckbox();
                } else {
                    /* Default value */
                    autoDisablePackagizerCheckbox();
                }
            }
        };
        packagename.setBadColor(null);
        packagename.setList(PackageHistoryManager.getInstance().list());
        packagename.setUnkownTextInputAllowed(true);
        packagename.setHelpText(_GUI.T.AddLinksDialog_layoutDialogContent_packagename_help());
        packagename.setSelectedItem(null);
        comment = new ExtTextField() {
            @Override
            public void onChanged() {
                if (!StringUtils.isEmpty(this.getText())) {
                    /* Non default value */
                    autoEnablePackagizerCheckbox();
                } else {
                    /* Default value */
                    autoDisablePackagizerCheckbox();
                }
            }
        };
        comment.setDragEnabled(true);
        comment.setHelpText(_GUI.T.AddLinksDialog_layoutDialogContent_comment_help());
        comment.setBorder(BorderFactory.createCompoundBorder(comment.getBorder(), BorderFactory.createEmptyBorder(2, 6, 1, 6)));
        final String defaultFolder = org.appwork.storage.config.JsonConfig.create(GeneralSettings.class).getDefaultDownloadFolder();
        destination.setQuickSelectionList(DownloadPathHistoryManager.getInstance().listPaths(defaultFolder));
        destination.setFile(new File(getDownloadDestinationFieldDefaultValue()));
        input = new ExtTextArea() {
            {
                final ExtTextArea textArea = this;
                addKeyListener(new KeyListener() {
                    @Override
                    public void keyTyped(KeyEvent e) {
                    }

                    @Override
                    public void keyPressed(KeyEvent e) {
                        if (e.getKeyChar() == '\t') {
                            if (e.isShiftDown()) {
                                textArea.transferFocusBackward();
                            } else {
                                textArea.transferFocus();
                            }
                            e.consume();
                        }
                    }

                    @Override
                    public void keyReleased(KeyEvent e) {
                    }
                });
            }

            @Override
            public void onChanged() {
                delayedValidate.run();
            }
        };
        // input.setLineWrap(true);
        input.setWrapStyleWord(true);
        input.setHelpText(_GUI.T.AddLinksDialog_layoutDialogContent_input_help());
        input.setDragEnabled(true);
        sp = new JScrollPane(input);
        sp.setViewportBorder(BorderFactory.createEmptyBorder(2, 6, 1, 6));
        password = new ExtTextField() {
            @Override
            public void onChanged() {
                if (!StringUtils.isEmpty(this.getText())) {
                    /* Non default value */
                    autoEnablePackagizerCheckbox();
                } else {
                    /* Default value */
                    autoDisablePackagizerCheckbox();
                }
            }
        };
        password.setDragEnabled(true);
        password.setHelpText(_GUI.T.AddLinksDialog_createExtracOptionsPanel_password());
        password.setBorder(BorderFactory.createCompoundBorder(password.getBorder(), BorderFactory.createEmptyBorder(2, 6, 1, 6)));
        priority = new JComboBox(Priority.values());
        final ListCellRenderer org = priority.getRenderer();
        priority.setRenderer(new ListCellRenderer() {
            public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
                JLabel r = (JLabel) org.getListCellRendererComponent(list, ((Priority) value).T(), index, isSelected, cellHasFocus);
                r.setIcon(((Priority) value).loadIcon(20));
                return r;
            }
        });
        priority.setSelectedItem(getPriorityFieldDefaultValue());
        priority.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                JComboBox source = (JComboBox) e.getSource();
                Priority priority = (Priority) source.getSelectedItem();
                if (priority == getPriorityFieldDefaultValue()) {
                    /* Default value */
                    autoDisablePackagizerCheckbox();
                } else {
                    /* Non default value */
                    autoEnablePackagizerCheckbox();
                }
            }
        });
        downloadPassword = new ExtTextField();
        downloadPassword.setDragEnabled(true);
        downloadPassword.setHelpText(_GUI.T.AddLinksDialog_createExtracOptionsPanel_downloadpassword());
        downloadPassword.setBorder(BorderFactory.createCompoundBorder(downloadPassword.getBorder(), BorderFactory.createEmptyBorder(2, 6, 1, 6)));
        extractToggle = new ExtCheckBox();
        extractToggle.setSelected(getAutoExtractionFieldDefaultValue());
        // extractToggle.setBorderPainted(false);
        extractToggle.setToolTipText(_GUI.T.AddLinksDialog_layoutDialogContent_autoextract_tooltip());
        extractToggle.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                JCheckBox source = (JCheckBox) e.getSource();
                boolean isSelected = source.isSelected();
                if (isSelected == getAutoExtractionFieldDefaultValue()) {
                    /* Default value */
                    autoDisablePackagizerCheckbox();
                } else {
                    /* Non default value */
                    autoEnablePackagizerCheckbox();
                }
            }
        });
        int height = Math.max(24, (int) (comment.getPreferredSize().height * 0.9));
        MigPanel p = new MigPanel("ins 0 0 3 0,wrap 3", "[][grow,fill][]", "[fill,grow][grow," + height + "!][grow," + height + "!][grow," + height + "!][grow," + height + "!]");
        p.add(new JLabel(new AbstractIcon(IconKey.ICON_LINKGRABBER, 32)), "aligny top,height 32!,width 32!");
        p.add(sp, "height 30:100:n,spanx");
        p.add(createIconLabel(IconKey.ICON_SAVE, _GUI.T.AddLinksDialog_layoutDialogContent_save_tt()), "aligny center,width 32!,height " + height + "!");
        p.add(destination.getDestination(), "height " + height + "!");
        p.add(destination.getButton(), "sg right,height " + height + "!");
        p.add(createIconLabel(IconKey.ICON_PACKAGE_OPEN, _GUI.T.AddLinksDialog_layoutDialogContent_package_tt()), "aligny center,width 32!");
        p.add(packagename, "spanx,height " + height + "!");
        p.add(createIconLabel(IconKey.ICON_DOCUMENT, _GUI.T.AddLinksDialog_layoutDialogContent_comment_tt()), "aligny center,width 32!");
        p.add(comment, "spanx,height " + height + "!");
        p.add(createIconLabel(new ExtMergedIcon(new AbstractIcon(IconKey.ICON_EXTRACT, 24)).add(new AbstractIcon(IconKey.ICON_LOCK, 18), 6, 6), _GUI.T.AddLinksDialog_layoutDialogContent_downloadpassword_tt()), "aligny center,height " + height + "!,width 32!");
        p.add(password, "pushx,growx,height " + height + "!");
        MigPanel subpanel = new MigPanel("ins 0", "[grow,fill][]", "[" + height + "!,grow]");
        p.add(subpanel, "sg right");
        JLabel lbl;
        subpanel.add(lbl = new JLabel(_GUI.T.AddLinksDialog_layoutDialogContent_autoextract_lbl()));
        lbl.setHorizontalAlignment(SwingConstants.RIGHT);
        subpanel.add(extractToggle, "aligny center");
        p.add(createIconLabel(new BadgeIcon(IconKey.ICON_PASSWORD, IconKey.ICON_DOWNLOAD, 24), _GUI.T.AddLinksDialog_layoutDialogContent_downloadpassword_tt()), "aligny center,width 32!");
        p.add(downloadPassword);
        p.add(priority, "sg right");
        input.setTransferHandler(new DragAndDropDelegater(input));
        this.getDialog().addWindowListener(new WindowListener() {
            public void windowOpened(WindowEvent e) {
                final Thread thread = new Thread() {
                    {
                        setDaemon(true);
                        setName(getClass().getName());
                    }

                    @Override
                    public void run() {
                        inform();
                        String textAuto = config.getPresetDebugLinks();
                        if (textAuto != null && textAuto.startsWith("\"") && textAuto.endsWith("\"")) {
                            try {
                                final String result = JSonStorage.restoreFromString(textAuto, TypeRef.STRING);
                                if (result != null) {
                                    textAuto = result;
                                }
                            } catch (Exception e) {
                                LogController.CL().log(e);
                            }
                        }
                        ClipboardContent clipboardContent = null;
                        if (config.isAutoFillAddLinksDialogWithClipboardContentEnabled() && StringUtils.isEmpty(textAuto)) {
                            final ClipboardMonitoring clp = ClipboardMonitoring.getINSTANCE();
                            clipboardContent = clp.getCurrentContent();
                            if (clipboardContent != null) {
                                textAuto = clipboardContent.getContent();
                            }
                        }
                        new EDTRunner() {
                            @Override
                            protected void runInEDT() {
                                input.setEditable(false);
                                input.setText(_GUI.T.AddLinksDialog_ParsingClipboard());
                            };
                        };
                        asyncAnalyse(textAuto, clipboardContent);
                    }
                };
                thread.start();
                getDialog().removeWindowListener(this);
            }

            public void windowIconified(WindowEvent e) {
            }

            public void windowDeiconified(WindowEvent e) {
            }

            public void windowDeactivated(WindowEvent e) {
            }

            public void windowClosing(WindowEvent e) {
            }

            public void windowClosed(WindowEvent e) {
            }

            public void windowActivated(WindowEvent e) {
            }
        });
        return p;
    }

    private String getDownloadDestinationFieldDefaultValue() {
        final String defaultFolder = org.appwork.storage.config.JsonConfig.create(GeneralSettings.class).getDefaultDownloadFolder();
        final String latest = config.getLatestDownloadDestinationFolder();
        if (config.isUseLastDownloadDestinationAsDefault() && !StringUtils.isEmpty(latest)) {
            return latest;
        } else {
            return defaultFolder;
        }
    }

    private boolean getAutoExtractionFieldDefaultValue() {
        return config.isAutoExtractionEnabled();
    }

    private Priority getPriorityFieldDefaultValue() {
        return Priority.DEFAULT;
    }

    private Component createIconLabel(Icon add, String tooltip) {
        JLabel ret = new JLabel(add);
        ret.setToolTipText(tooltip);
        return ret;
    }

    protected JButton createOkButton() {
        return new ExtButton(new AppAction() {
            {
                setName(okButtonText);
            }

            @Override
            public void actionPerformed(ActionEvent e) {
            }
        }) {
            @Override
            public int getTooltipDelay(Point mousePositionOnScreen) {
                return 500;
            }

            @Override
            public boolean isTooltipWithoutFocusEnabled() {
                return super.isTooltipWithoutFocusEnabled();
            }
        };
    }

    protected void validateForm() {
        if (input == null) {
            return;
        }
        final String text = new EDTHelper<String>() {
            @Override
            public String edtRun() {
                return input.getText();
            }
        }.getReturnValue();
        final AtomicBoolean fastContainsLinksFlag = new AtomicBoolean(false);
        try {
            jd.parser.html.HTMLParser.getHttpLinks(text, null, new HtmlParserResultSet() {
                @Override
                public boolean add(HtmlParserCharSequence e) {
                    if (e != null) {
                        fastContainsLinksFlag.set(true);
                        throw new RuntimeException("abort");
                    }
                    return false;
                }
            });
        } catch (Throwable ignore) {
            if (!fastContainsLinksFlag.get()) {
                LogController.CL().log(ignore);
            }
        }
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                if (fastContainsLinksFlag.get()) {
                    okButton.setToolTipText("");
                    input.setToolTipText(null);
                    okButton.setEnabled(true);
                    confirmOptions.setEnabled(true);
                } else {
                    okButton.setToolTipText(_GUI.T.AddLinksDialog_validateForm_input_missing());
                    input.setToolTipText(_GUI.T.AddLinksDialog_validateForm_input_missing());
                    okButton.setEnabled(false);
                    confirmOptions.setEnabled(false);
                }
                final File dest = destination.getFile();
                if (dest == null || !validateFolder(dest.getAbsolutePath())) {
                    final String toolTip = okButton.getToolTipText();
                    if (toolTip == null || toolTip.length() == 0) {
                        okButton.setToolTipText(_GUI.T.AddLinksDialog_validateForm_folder_invalid_missing());
                    }
                    okButton.setEnabled(false);
                    destination.setToolTipText(_GUI.T.AddLinksDialog_validateForm_folder_invalid_missing());
                    confirmOptions.setEnabled(false);
                    destination.getTxt().setForeground((LAFOptions.getInstance().getColorForErrorForeground()));
                } else {
                    destination.setToolTipText(null);
                    destination.getTxt().setForeground(null);
                }
                if (okButton.isEnabled() && cancelButton.hasFocus()) {
                    okButton.requestFocus();
                }
            }
        };
    }

    private boolean validateFolder(String text) {
        if (text == null) {
            return false;
        }
        File file = new File(text);
        if (file.isDirectory() && file.exists()) {
            return true;
        }
        int threshold = config.getMissingParentDirectoryThreshold();
        while (threshold > 0) {
            final File parent = file.getParentFile();
            if (parent == null) {
                return false;
            } else if (parent.exists()) {
                return true;
            } else {
                file = parent;
                threshold--;
            }
        }
        return false;
    }

    private void inform() {
        new Thread() {
            {
                setDaemon(true);
                setName(getClass().getName());
            }

            public void run() {
                try {
                    Thread.sleep(2000);
                } catch (InterruptedException e1) {
                    e1.printStackTrace();
                }
                new EDTRunner() {
                    @Override
                    protected void runInEDT() {
                        if (input.isShowing() && CFG_GUI.HELP_DIALOGS_ENABLED.isEnabled()) {
                            HelpDialog.show(Boolean.FALSE, Boolean.TRUE, new Point(input.getLocationOnScreen().x + input.getWidth() / 2, input.getLocationOnScreen().y + 10), null, Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN, _GUI.T.AddLinksDialog_AddLinksDialog_(), _GUI.T.AddLinksDialog_layoutDialogContent_description(), new AbstractIcon(IconKey.ICON_LINKGRABBER, 32));
                        }
                    }
                };
            }
        }.start();
    }

    public void pack() {
        this.getDialog().pack();
    }

    protected boolean isResizable() {
        return true;
    }

    private final AtomicReference<Thread> asyncImportThread = new AtomicReference<Thread>();

    protected void asyncAnalyse(final String textAuto, final ClipboardContent clipboardContent) {
        final Thread thread = new Thread() {
            private Thread thisThread = null;
            {
                setDaemon(true);
                setName(getClass().getName());
            }

            public void run() {
                try {
                    thisThread = Thread.currentThread();
                    final String resultText = processTextAuto(textAuto, clipboardContent);
                    updateUIWithResult(resultText);
                } catch (final Throwable e) {
                    if (thisThread == asyncImportThread.get()) {
                        LogController.CL().log(e);
                    }
                } finally {
                    asyncImportThread.compareAndSet(Thread.currentThread(), null);
                }
            }

            /** Process text: find links and possible extraction passwords. */
            private String processTextAuto(String textAuto, final ClipboardContent clipboardContent) {
                if (textAuto == null) {
                    /* Nothing to process */
                    return "";
                }
                /* replace lonely carrier return with CRLF */
                textAuto = textAuto.replaceAll("\r(!\n)", "\r\n");
                if (config.isAddLinksPreParserAutoExtractionPasswordSearchEnabled()) {
                    processPasswordExtraction(textAuto, clipboardContent);
                }
                if (!config.isAddLinksPreParserEnabled()) {
                    /* Links pre parser is disabled in config. */
                    return textAuto;
                }
                return extractLinksFromText(textAuto, clipboardContent);
            }

            private void processPasswordExtraction(final String textAuto, final ClipboardContent clipboardContent) {
                HashSet<String> passwords = PasswordUtils.getPasswords(clipboardContent != null ? clipboardContent.getContentText() : null);
                if (passwords != null && !passwords.isEmpty()) {
                    // Passwords found in clipboard content
                } else {
                    // Look for passwords in auto generated text / html
                    passwords = PasswordUtils.getPasswords(textAuto);
                }
                if (passwords == null || passwords.size() == 0) {
                    return;
                }
                synchronized (autoPasswords) {
                    autoPasswords.addAll(passwords);
                }
                updatePasswordUI();
            }

            private void updatePasswordUI() {
                new EDTRunner() {
                    protected void runInEDT() {
                        synchronized (autoPasswords) {
                            if (autoPasswords.size() > 1) {
                                password.setText(JSonStorage.serializeToJson(autoPasswords));
                            } else if (autoPasswords.size() > 0) {
                                password.setText(autoPasswords.toArray(new String[] {})[0]);
                            }
                        }
                    }
                };
            }

            private String extractLinksFromText(final String textAuto, final ClipboardContent clipboardContent) {
                final String parseTextAuto = preprocessFind(textAuto);
                final String base = clipboardContent != null ? clipboardContent.getBrowserURL() : null;
                // Try original text
                String[] result = tryExtractLinks(parseTextAuto, base);
                if (result.length > 0) {
                    return StringUtils.join(result, "\r\n");
                }
                // Try with www. replaced
                result = tryExtractLinks(parseTextAuto.replace("www.", "http://www."), base);
                if (result.length > 0) {
                    return StringUtils.join(result, "\r\n");
                }
                // Try with http:// prefix
                result = tryExtractLinks("http://" + parseTextAuto, base);
                return StringUtils.join(result, "\r\n");
            }

            private String[] tryExtractLinks(final String text, final String base) {
                return HTMLParser.getHttpLinks(text, base, new HtmlParserResultSet() {
                    public boolean add(HtmlParserCharSequence e) {
                        if (thisThread != asyncImportThread.get()) {
                            throw new RuntimeException("abort");
                        }
                        return super.add(e);
                    }
                });
            }

            private void updateUIWithResult(final String resultText) {
                new EDTRunner() {
                    protected void runInEDT() {
                        if (thisThread == asyncImportThread.get()) {
                            input.setText(resultText);
                            input.setEditable(true);
                        }
                    }
                }.waitForEDT();
            }
        };
        asyncImportThread.set(thread);
        thread.start();
    }

    private void autoEnablePackagizerCheckbox() {
        if (this.overwritePackagizer == null) {
            return;
        } else if (!packagizerAutoModeEnabled) {
            return;
        } else if (hasUserClickedOverwritePackagizerButton) {
            /* User has already altered this checkbox -> Do not touch */
            return;
        }
        this.overwritePackagizer.setSelected(true);
        /* ChangeListener on overwritePackagizer will set this to true so we'll correct that here. */
        hasUserClickedOverwritePackagizerButton = false;
    }

    private void autoDisablePackagizerCheckbox() {
        if (this.overwritePackagizer == null) {
            return;
        } else if (!packagizerAutoModeEnabled) {
            return;
        } else if (hasUserClickedOverwritePackagizerButton) {
            /* User has already altered this checkbox -> Do not touch */
            return;
        }
        /* Check for non-default values for all relevant fields. If any field is non default, do not touch Packagizer checkbox. */
        final String finalPackageName = getPackageName();
        if (!StringUtils.isEmpty(finalPackageName)) {
            return;
        }
        final String finalComment = getComment();
        if (!StringUtils.isEmpty(finalComment)) {
            return;
        }
        final String finalDownloadPassword = getDownloadPassword();
        if (!StringUtils.isEmpty(finalDownloadPassword)) {
            return;
        }
        final String finalDestination = getDestination();
        if (finalDestination != null && !finalDestination.equals(this.getDownloadDestinationFieldDefaultValue())) {
            return;
        }
        final Priority priority = this.getPriority();
        if (priority != this.getPriorityFieldDefaultValue()) {
            return;
        }
        /* ChangeListener on overwritePackagizer will set this to true so we'll correct that here. */
        this.overwritePackagizer.setSelected(false);
    }

    private Component createIconLabel(String iconKey, String tooltip) {
        JLabel ret = new JLabel(new AbstractIcon(iconKey, 24));
        ret.setToolTipText(tooltip);
        return ret;
    }

    protected String preprocessFind(String text) {
        if (text == null) {
            return null;
        }
        return new LinkCrawler(false, false).preprocessFind(text, null, false);
    }
}
