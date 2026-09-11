package jd.controlling.downloadcontroller;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dialog.ModalityType;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.File;
import java.text.NumberFormat;

import javax.swing.ButtonGroup;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JRadioButton;
import javax.swing.JSeparator;
import javax.swing.JTextField;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DocumentFilter;

import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.swing.MigPanel;
import org.appwork.swing.components.ExtTextArea;
import org.appwork.uio.UIOManager;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.SwingUtils;
import org.appwork.utils.swing.dialog.AbstractDialog;
import org.appwork.utils.swing.dialog.Dialog;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.downloads.table.DownloadsTableModel;
import org.jdownloader.settings.GeneralSettings.OnSkipDueToAlreadyExistsAction;
import org.jdownloader.settings.IfFileExistsAction;
import org.jdownloader.settings.staticreferences.CFG_GENERAL;
import org.jdownloader.translate._JDT;

import jd.controlling.linkcollector.LinknameCleaner;
import jd.plugins.DownloadLink;
import jd.plugins.ParsedFilename;

public class IfFileExistsDialog extends AbstractDialog<IfFileExistsAction> implements IfFileExistsDialogInterface, FocusListener {
    private final String       path;
    private IfFileExistsAction result;
    private final String       packagename;

    @Override
    public boolean isRemoteAPIEnabled() {
        return true;
    }

    public String getPackagename() {
        return packagename;
    }

    public String getPackageID() {
        return packageID;
    }

    private JRadioButton         skip;
    private JRadioButton         overwrite;
    private JRadioButton         useAutoRename;
    private JRadioButton         useCustom;
    /** Dropdown next to the "skip" option to pick the "on skip due to already exists" action; only enabled while skip is selected. */
    private JComboBox            onSkipActionComboBox;
    private final String         packageID;
    private final DownloadLink   downloadLink;
    private final DownloadLink   downloadLinkInProgress;
    /** Name of the already existing file. */
    private final String         originalFilename;
    /** Pre-generated auto-rename suggestion (e.g. "file_2.ext"); null when no free name could be pre-computed. */
    private final String         autoRenamedFilename;
    /** Parsed base filename (suggestion or original) used to reattach the extension to a custom name on return. */
    private final ParsedFilename parsedBaseFilename;
    private FileNameField        customFilenameInput;
    private JTextField           textfieldCustomName;
    /** Non-blocking hint shown when the currently chosen new filename already exists in the download folder. */
    private final JLabel         filenameExistsWarning = new JLabel("");
    /** "Characters left" hint for the custom filename; only shown/updated when a length limit applies (maxFilenameLength > 0). */
    private final JLabel         charactersLeftLabel   = new JLabel("");
    /** Fully transparent color: used to "hide" the always-present warning text without changing the reserved layout space. */
    private static final Color   TRANSPARENT_COLOR     = new Color(0, 0, 0, 0);
    /** Download folder the new file would be written into; used to check whether the chosen filename already exists. */
    private final File           targetFolder;
    /** Per-package "Remember selection for this Package" checkbox (the framework checkbox is the global "Do not ask again" default). */
    private JCheckBox            rememberForPackage;
    /** "Don't ask again during this session" checkbox: remembers the decision session-wide (stored in the DownloadSession). */
    private JCheckBox            dontAskAgainThisSession;
    private final boolean        singleItemPackage;
    /** Maximum allowed filename length; <= 0 means "no restriction". > 0 when routed here from the too-long handling. */
    private final int            maxFilenameLength;
    /**
     * The actual already existing file this dialog is about (name + size shown); null to derive it from the download link's output path.
     * Non-null e.g. when routed here from the too-long handling, where the collision is on the shortened name rather than the link's name.
     */
    private final File           alreadyExistingFile;

    public IfFileExistsDialog(final DownloadLink downloadLink, final DownloadLink downloadLinkInProgress) {
        this(downloadLink, downloadLinkInProgress, null, -1, null);
    }

    /**
     * @param autoRenameSuggestion
     *            A pre-generated auto-rename suggestion (e.g. "file_2.ext") to show and pre-fill, or null when none was generated (e.g.
     *            when "skip" is the pre-selected option). The suggestion is generated by the caller so it is only computed when actually
     *            needed.
     * @param maxFilenameLength
     *            Maximum allowed filename length (including extension); <= 0 means "no restriction". > 0 when routed here from the too-long
     *            handling, so a custom filename entered here cannot exceed the filesystem limit again.
     * @param alreadyExistingFile
     *            The actual existing file this dialog is about (its name and size are shown); null to derive it from the download link's
     *            output path. Non-null e.g. from the too-long handling, where the collision is on the shortened name.
     */
    public IfFileExistsDialog(final DownloadLink downloadLink, final DownloadLink downloadLinkInProgress, final String autoRenameSuggestion, final int maxFilenameLength, final File alreadyExistingFile) {
        super(Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN | UIOManager.LOGIC_COUNTDOWN, _JDT.T.jd_controlling_SingleDownloadController_askexists_title(), null, null, null);
        this.packagename = downloadLink.getFilePackage().getName();
        this.singleItemPackage = downloadLink.getFilePackage().size() == 1;
        this.packageID = downloadLink.getFilePackage().getName() + "_" + downloadLink.getFilePackage().getCreated();
        this.path = downloadLink.getFileOutput();
        this.downloadLink = downloadLink;
        this.downloadLinkInProgress = downloadLinkInProgress;
        this.maxFilenameLength = maxFilenameLength;
        this.alreadyExistingFile = alreadyExistingFile;
        final File existingFile = alreadyExistingFile != null ? alreadyExistingFile : new File(this.path);
        this.targetFolder = existingFile.getParentFile();
        this.originalFilename = existingFile.getName();
        this.autoRenamedFilename = autoRenameSuggestion;
        this.parsedBaseFilename = new ParsedFilename(autoRenameSuggestion != null ? autoRenameSuggestion : this.originalFilename);
        setTimeout(60000);
    }

    @Override
    public ModalityType getModalityType() {
        return ModalityType.MODELESS;
    }

    @Override
    public String getDontShowAgainKey() {
        // returning null causes the dialog to show a checkbox, but the dialog itself does not handle the results
        return null;
    }

    @Override
    protected IfFileExistsAction createReturnValue() {
        if (okButton != null) {
            okButton.removeFocusListener(this);
            if (!okButton.isEnabled()) {
                /* OK greyed out (custom filename collides with an existing file) -> do not proceed with it; treat as skip. */
                return IfFileExistsAction.SKIP_FILE;
            }
        }
        /* Remember the last choice for pre-selection next time. */
        if (result != null) {
            org.jdownloader.settings.staticreferences.CFG_GUI.CFG.setLastIfFileExists(result);
        }
        return result;
    }

    protected String getDontShowAgainLabelText() {
        /* The framework's built-in "do not display again" checkbox is used here as the global "Do not ask again" default option. */
        return _GUI.T.IfFileExistsDialog_do_not_ask_again();
    }

    /**
     * A custom (per-file) filename must never be stored as the global default. When the user chose to enter a custom filename the "Do not
     * ask again" checkbox is greyed out (disabled), which already makes the framework implementation return false; this override states the
     * rule explicitly so it does not depend on the checkbox's enabled state alone.
     */
    @Override
    public boolean isDontShowAgainSelected() {
        if (this.useCustom != null && this.useCustom.isSelected()) {
            return false;
        }
        return super.isDontShowAgainSelected();
    }

    @Override
    public boolean isRememberForPackageSelected() {
        /*
         * isEnabled() ensures a custom (per-file) filename is never remembered for the whole package (checkbox is disabled in that case).
         */
        return this.rememberForPackage != null && this.rememberForPackage.isSelected() && this.rememberForPackage.isEnabled();
    }

    @Override
    public boolean isDontAskAgainThisSessionSelected() {
        /* isEnabled() ensures this returns false when a custom filename or the global "do not show again" option is selected. */
        return this.dontAskAgainThisSession != null && this.dontAskAgainThisSession.isSelected() && this.dontAskAgainThisSession.isEnabled();
    }

    @Override
    public OnSkipDueToAlreadyExistsAction getOnSkipDueToAlreadyExistsAction() {
        /* Only meaningful when the skip option is selected; otherwise null so the caller falls back to the global config value. */
        if (this.skip != null && this.skip.isSelected() && this.onSkipActionComboBox != null) {
            return (OnSkipDueToAlreadyExistsAction) this.onSkipActionComboBox.getSelectedItem();
        }
        return null;
    }

    /**
     * The framework creates the "Do not ask again" checkbox only here (after {@link #layoutDialogContent()}), so re-apply the
     * enabled/checked state of all checkboxes as soon as it exists. Toggling the framework checkbox also has to refresh the session
     * checkbox, so hook a listener onto it.
     */
    @Override
    protected void initDoNotShowAgainCheckbox(final MigPanel bottom) {
        super.initDoNotShowAgainCheckbox(bottom);
        if (this.dontshowagain != null) {
            this.dontshowagain.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    updateDontShowAgainVisibility();
                    stopTimer();
                }
            });
        }
        updateDontShowAgainVisibility();
    }

    /**
     * Keeps the "Remember selection for this Package", "Don't ask again during this session" and framework "Do not ask again" checkboxes in
     * a consistent state: a custom (per-file) filename must never be remembered anywhere, so the remember/session checkboxes are disabled
     * (and the per-package one unchecked) while a custom filename is selected. The session checkbox is additionally disabled while the
     * global "do not show again" checkbox is selected (the global default already covers it). For a single-item package the per-package
     * checkbox is meaningless, so it is shown deselected and disabled.
     */
    private void updateDontShowAgainVisibility() {
        final boolean customSelected = this.useCustom != null && this.useCustom.isSelected();
        final boolean dontShowAgainSelected = this.dontshowagain != null && this.dontshowagain.isSelected();
        if (this.rememberForPackage != null) {
            if (singleItemPackage) {
                this.rememberForPackage.setSelected(false);
                this.rememberForPackage.setEnabled(false);
            } else {
                if (customSelected) {
                    this.rememberForPackage.setSelected(false);
                }
                this.rememberForPackage.setEnabled(!customSelected);
            }
            refreshCheckboxParent(this.rememberForPackage);
        }
        if (this.dontAskAgainThisSession != null) {
            /* Disabled (and thus reporting false) while a custom filename or the global "do not show again" option is selected. */
            this.dontAskAgainThisSession.setEnabled(!customSelected && !dontShowAgainSelected);
            refreshCheckboxParent(this.dontAskAgainThisSession);
        }
        if (this.dontshowagain != null) {
            this.dontshowagain.setEnabled(!customSelected);
            refreshCheckboxParent(this.dontshowagain);
        }
    }

    private static void refreshCheckboxParent(final java.awt.Component component) {
        final java.awt.Container parent = component.getParent();
        if (parent != null) {
            parent.revalidate();
            parent.repaint();
        }
    }

    @Override
    public JComponent layoutDialogContent() {
        final String textfieldConstraints = "growx, pushx, wmin 100";
        final MigPanel p = new MigPanel("ins 0,wrap 1, wmax 500", "", "");
        final ExtTextArea txt = new ExtTextArea();
        txt.setLabelMode(true);
        txt.setLineWrap(true);
        txt.setWrapStyleWord(true);
        txt.setToolTipText(path);
        txt.setText(_JDT.T.jd_controlling_SingleDownloadController_askexists3());
        p.add(txt, textfieldConstraints);
        /* Existing file. */
        p.add(SwingUtils.toBold(new JLabel(_GUI.T.IfFileExistsDialog_layoutDialogContent_filename())), "split 2,sg 1");
        p.add(new FileNameField(this.originalFilename, false), textfieldConstraints);
        /* Existing file's size first, then the new file's size (order intentionally existing -> new). */
        final File existingFile = getExistingFile();
        final long existingSize;
        if (downloadLinkInProgress != null) {
            existingSize = Math.max(downloadLinkInProgress.getKnownDownloadSize(), existingFile.length());
        } else {
            existingSize = existingFile.length();
        }
        p.add(SwingUtils.toBold(new JLabel(_GUI.T.IfFileExistsDialog_layoutDialogContent_filesize_existing())), "split 2,sg 1");
        p.add(new JLabel(SizeFormatter.formatBytes(NumberFormat.getInstance(), existingSize)));
        p.add(SwingUtils.toBold(new JLabel(_GUI.T.IfFileExistsDialog_layoutDialogContent_filesize2())), "split 2,sg 1");
        p.add(new JLabel(SizeFormatter.formatBytes(NumberFormat.getInstance(), downloadLink.getView().getBytesTotalEstimated())));
        /* Pre-generated auto-rename suggestion (read-only). */
        p.add(SwingUtils.toBold(new JLabel(_GUI.T.IfFileExistsDialog_auto_renamed_filename())), "split 2,sg 1");
        p.add(new FileNameField(this.autoRenamedFilename != null ? this.autoRenamedFilename : this.originalFilename, false), textfieldConstraints);
        /* Editable custom filename, pre-filled with the suggestion. */
        p.add(SwingUtils.toBold(new JLabel(_GUI.T.IfFileExistsDialog_custom_filename())), "split 2,sg 1");
        this.customFilenameInput = new FileNameField(this.autoRenamedFilename != null ? this.autoRenamedFilename : this.originalFilename, true);
        this.textfieldCustomName = this.customFilenameInput.getNameField();
        if (this.maxFilenameLength > 0) {
            /* Routed here from the too-long handling -> limit the custom name so it cannot exceed the filesystem limit again. */
            setTextFieldLimit(this.textfieldCustomName);
        }
        p.add(this.customFilenameInput, textfieldConstraints);
        if (this.maxFilenameLength > 0) {
            /* Only meaningful when a length limit applies. */
            p.add(SwingUtils.toBold(charactersLeftLabel));
        }
        /*
         * Keep the warning text present at all times so its space (width and height) is reserved permanently; updateFilenameExistsWarning()
         * only toggles the color (red vs. transparent), so showing/hiding the warning never changes the dialog dimensions.
         */
        filenameExistsWarning.setText(_GUI.T.IfFilenameTooLongDialog_filename_already_exists());
        filenameExistsWarning.setForeground(TRANSPARENT_COLOR);
        p.add(SwingUtils.toBold(filenameExistsWarning));
        /* Package and hoster. */
        p.add(SwingUtils.toBold(new JLabel(_GUI.T.IfFileExistsDialog_layoutDialogContent_package())), "split 2,sg 1");
        p.add(new PackageNameField(packagename), textfieldConstraints);
        p.add(SwingUtils.toBold(new JLabel(_GUI.T.IfFileExistsDialog_layoutDialogContent_hoster())), "split 2,sg 1");
        p.add(new JLabel(downloadLink.getDomainInfo().getTld()));
        /* Options. */
        skip = new JRadioButton(IfFileExistsAction.SKIP_FILE.getLabel());
        skip.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                result = IfFileExistsAction.SKIP_FILE;
                onSelectionChanged();
            }
        });
        overwrite = new JRadioButton(IfFileExistsAction.OVERWRITE_FILE.getLabel());
        overwrite.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                result = IfFileExistsAction.OVERWRITE_FILE;
                onSelectionChanged();
            }
        });
        useAutoRename = new JRadioButton(IfFileExistsAction.AUTO_RENAME.getLabel());
        useAutoRename.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                result = IfFileExistsAction.AUTO_RENAME;
                onSelectionChanged();
            }
        });
        useCustom = new JRadioButton(_GUI.T.IfFilenameTooLongDialog_use_custom_filename());
        useCustom.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                result = IfFileExistsAction.AUTO_RENAME;
                onSelectionChanged();
            }
        });
        final ButtonGroup group = new ButtonGroup();
        group.add(skip);
        group.add(overwrite);
        group.add(useAutoRename);
        group.add(useCustom);
        p.add(new JSeparator(), "pushx,growx");
        // Skip option with a dropdown next to it choosing what "skip" means (skip / mark successful / mark as successful mirror).
        onSkipActionComboBox = new JComboBox(OnSkipDueToAlreadyExistsAction.values());
        onSkipActionComboBox.setSelectedItem(CFG_GENERAL.CFG.getOnSkipDueToAlreadyExistsAction());
        /* Disabled until the skip option is selected. */
        onSkipActionComboBox.setEnabled(false);
        onSkipActionComboBox.setRenderer(new DefaultListCellRenderer() {
            @Override
            public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
                final Component c = super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
                if (value instanceof LabelInterface) {
                    setText(((LabelInterface) value).getLabel());
                }
                return c;
            }
        });
        onSkipActionComboBox.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                stopTimer();
            }
        });
        p.add(skip, "split 2, gapleft 10");
        p.add(onSkipActionComboBox);
        p.add(overwrite, "gapleft 10");
        p.add(useAutoRename, "gapleft 10");
        p.add(useCustom, "gapleft 10");
        // "Remember selection for this Package" applies the chosen action to all remaining items of this package (dedicated checkbox).
        rememberForPackage = new JCheckBox(_GUI.T.IfFileExistsDialog_getDontShowAgainLabelText_());
        rememberForPackage.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                stopTimer();
            }
        });
        p.add(rememberForPackage, "gapleft 10");
        // "Don't ask again during this session" remembers the chosen action session-wide (stored in the DownloadSession).
        dontAskAgainThisSession = new JCheckBox(_GUI.T.dialog_dont_ask_again_this_session());
        dontAskAgainThisSession.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                stopTimer();
            }
        });
        p.add(dontAskAgainThisSession, "gapleft 10");
        /* Editing the custom name auto-selects the custom option. */
        this.textfieldCustomName.getDocument().addDocumentListener(new DocumentListener() {
            @Override
            public void insertUpdate(DocumentEvent e) {
                onCustomNameChanged();
            }

            @Override
            public void removeUpdate(DocumentEvent e) {
                onCustomNameChanged();
            }

            @Override
            public void changedUpdate(DocumentEvent e) {
                onCustomNameChanged();
            }
        });
        /* Default selection: last used action. ASK_FOR_EACH_FILE must never be pre-selected as a result -> fall back to skip. */
        IfFileExistsAction def = org.jdownloader.settings.staticreferences.CFG_GUI.CFG.getLastIfFileExists();
        if (def == null || def == IfFileExistsAction.ASK_FOR_EACH_FILE) {
            def = IfFileExistsAction.SKIP_FILE;
        }
        switch (def) {
        case AUTO_RENAME:
            useAutoRename.setSelected(true);
            break;
        case OVERWRITE_FILE:
            overwrite.setSelected(true);
            break;
        case ASK_FOR_EACH_FILE:
        case SKIP_FILE:
        default:
            skip.setSelected(true);
            def = IfFileExistsAction.SKIP_FILE;
            break;
        }
        result = def;
        /*
         * Stop the auto-confirm countdown as soon as the user makes a choice. Added after the default selection above so it does not fire
         * for the initial (programmatic) default (same principle as IfFilenameTooLongDialog).
         */
        final ItemListener userSelectionListener = new ItemListener() {
            @Override
            public void itemStateChanged(final ItemEvent e) {
                if (e.getStateChange() != ItemEvent.SELECTED) {
                    return;
                }
                onSelectionChanged();
                stopTimer();
            }
        };
        skip.addItemListener(userSelectionListener);
        overwrite.addItemListener(userSelectionListener);
        useAutoRename.addItemListener(userSelectionListener);
        useCustom.addItemListener(userSelectionListener);
        if (okButton != null) {
            okButton.addFocusListener(this);
        }
        onSelectionChanged();
        return p;
    }

    /** Returns the existing file on disk; falls back to the ".part" file when the final file does not exist yet. */
    private File getExistingFile() {
        if (this.alreadyExistingFile != null) {
            /* Caller told us exactly which file exists (e.g. the shortened too-long name). */
            return this.alreadyExistingFile;
        }
        File localFile = new File(path);
        if (!localFile.exists()) {
            localFile = new File(path + ".part");
        }
        return localFile;
    }

    private void onCustomNameChanged() {
        /* Any manual edit of the custom name selects the custom option. */
        if (this.useCustom != null && !this.useCustom.isSelected()) {
            this.useCustom.setSelected(true);
            this.result = IfFileExistsAction.AUTO_RENAME;
        }
        onSelectionChanged();
        stopTimer();
    }

    private void onSelectionChanged() {
        updateFilenameExistsWarning();
        updateCharactersLeft();
        updateDontShowAgainVisibility();
        /* The "on skip" dropdown can only be changed while the skip option is selected. */
        if (this.onSkipActionComboBox != null) {
            this.onSkipActionComboBox.setEnabled(this.skip != null && this.skip.isSelected());
        }
    }

    /** Updates the "characters left" hint for the custom filename. Only relevant when a length limit applies (maxFilenameLength > 0). */
    private void updateCharactersLeft() {
        if (this.maxFilenameLength <= 0 || this.textfieldCustomName == null) {
            return;
        }
        final int maxCharacters = getEffectiveMaxCustomNameLength();
        final int charactersLeft = maxCharacters - this.textfieldCustomName.getText().length();
        if (this.useCustom != null && this.useCustom.isSelected() && charactersLeft <= 0) {
            this.charactersLeftLabel.setForeground(Color.RED);
        } else {
            this.charactersLeftLabel.setForeground(Color.BLACK);
        }
        this.charactersLeftLabel.setText(_GUI.T.IfFilenameTooLongDialog_characters_left(String.valueOf(charactersLeft), String.valueOf(maxCharacters)));
    }

    /**
     * Shows a red warning when the currently chosen new filename already exists in the download folder. For the auto-rename suggestion this
     * is only a hint (the caller resolves it to a free name). For a user-entered custom filename it additionally greys out the OK button,
     * so a colliding custom name cannot be confirmed.
     */
    private void updateFilenameExistsWarning() {
        if (this.filenameExistsWarning == null) {
            return;
        }
        boolean exists = false;
        if (this.result == IfFileExistsAction.AUTO_RENAME && this.targetFolder != null) {
            final String newFilename = getNewFilename();
            if (newFilename != null && new File(this.targetFolder, newFilename).exists()) {
                exists = true;
            }
        }
        /* The text stays set at all times (space reserved); only the color toggles so the dialog dimensions never change. */
        this.filenameExistsWarning.setForeground(exists ? Color.RED : TRANSPARENT_COLOR);
        /*
         * Grey out OK when the user entered a custom filename that already exists on disk, so a colliding name cannot be confirmed. The
         * auto-rename suggestion is resolved to a free name by the caller, so only the custom case blocks confirmation.
         */
        if (this.okButton != null) {
            final boolean customFilenameExists = exists && this.useCustom != null && this.useCustom.isSelected();
            this.okButton.setEnabled(!customFilenameExists);
        }
    }

    public IfFileExistsAction getAction() {
        return result;
    }

    /**
     * Maximum allowed length of the (extension-less) custom name part, so that name + extension stays within {@link #maxFilenameLength}.
     * Returns {@link Integer#MAX_VALUE} when no length restriction applies.
     */
    private int getEffectiveMaxCustomNameLength() {
        if (this.maxFilenameLength <= 0) {
            return Integer.MAX_VALUE;
        }
        final String ext = this.parsedBaseFilename.getExtensionAdvanced();
        final int extLength = ext != null ? ext.length() : 0;
        final int max = this.maxFilenameLength - extLength;
        return max > 0 ? max : 1;
    }

    /** Cleans and length-limits input in the given textfield on every change (typing, deletion and paste). */
    private void setTextFieldLimit(final JTextField textField) {
        ((AbstractDocument) textField.getDocument()).setDocumentFilter(new DocumentFilter() {
            @Override
            public void insertString(FilterBypass fb, int offset, String string, AttributeSet attr) throws BadLocationException {
                applyChange(fb, offset, 0, string, attr);
            }

            @Override
            public void remove(FilterBypass fb, int offset, int length) throws BadLocationException {
                applyChange(fb, offset, length, "", null);
            }

            @Override
            public void replace(FilterBypass fb, int offset, int length, String string, AttributeSet attr) throws BadLocationException {
                applyChange(fb, offset, length, string, attr);
            }

            /**
             * Builds the text that would result from the requested edit, runs it through {@link #cleanKeepTrailingSpaces(String)} - so
             * every change (typing, deletion and paste) is cleaned directly in the field - and applies it. The edit is rejected with a
             * warning when the cleaned result would be empty or longer than the allowed maximum.
             */
            private void applyChange(final FilterBypass fb, final int offset, final int length, final String inserted, final AttributeSet attr) throws BadLocationException {
                final String insertedString = inserted == null ? "" : inserted;
                final String current = fb.getDocument().getText(0, fb.getDocument().getLength());
                final String proposed = current.substring(0, offset) + insertedString + current.substring(offset + length);
                final String cleaned = cleanKeepTrailingSpaces(proposed);
                if (cleaned.length() < 1 || cleaned.length() > getEffectiveMaxCustomNameLength()) {
                    /* Empty name or too long -> reject the edit and keep the previous content. */
                    triggerWarning(textField);
                    return;
                }
                /* FilterBypass edits bypass this filter, so replacing the whole content does not recurse. */
                fb.replace(0, current.length(), cleaned, attr);
                textField.setCaretPosition(Math.min(offset + insertedString.length(), cleaned.length()));
            }
        });
    }

    /**
     * Runs the given text through {@link LinknameCleaner#cleanFilename(String)} but preserves whitespace at the very end. cleanFilename
     * trims trailing whitespace; doing that on every keystroke would make it impossible to type a space inside the name (each space is
     * momentarily trailing). The preserved trailing spaces are trimmed for good by cleanFilename in {@link #getNewFilename()} on return.
     */
    private static String cleanKeepTrailingSpaces(final String input) {
        if (input == null) {
            return "";
        }
        int end = input.length();
        while (end > 0 && input.charAt(end - 1) == ' ') {
            end--;
        }
        if (end == 0) {
            /* Only spaces entered so far -> keep as-is; getNewFilename() cleans it on return. */
            return input;
        }
        return LinknameCleaner.cleanFilename(input.substring(0, end)) + input.substring(end);
    }

    private void triggerWarning(final JTextField textField) {
        CrossSystem.playErrorSound();
        textField.setForeground(Color.RED);
        new Thread() {
            public void run() {
                try {
                    Thread.sleep(500);
                } catch (InterruptedException ignored) {
                }
                new EDTRunner() {
                    @Override
                    protected void runInEDT() {
                        textField.setForeground(Color.BLACK);
                    }
                };
            };
        }.start();
    }

    @Override
    public String getNewFilename() {
        if (this.result != IfFileExistsAction.AUTO_RENAME) {
            /* Skip/overwrite -> no explicit new filename. */
            return null;
        }
        if (this.useCustom != null && this.useCustom.isSelected()) {
            String name = this.textfieldCustomName.getText();
            if (name == null) {
                return null;
            }
            name = LinknameCleaner.cleanFilename(name);
            final String ext = this.parsedBaseFilename.getExtensionAdvanced();
            if (ext != null && !StringUtils.endsWithCaseInsensitive(name, ext)) {
                name += ext;
            }
            if (name.length() == 0) {
                return null;
            }
            return name;
        }
        /* Auto-rename selected -> the pre-generated suggestion (may be null, then the caller auto-generates a name itself). */
        return this.autoRenamedFilename;
    }

    public String getFilePath() {
        return path;
    }

    public IfFileExistsDialogInterface show() {
        return UIOManager.I().show(IfFileExistsDialogInterface.class, this);
    }

    @Override
    public void focusGained(FocusEvent e) {
        DownloadsTableModel.getInstance().setSelectedObject(downloadLink);
    }

    @Override
    public void focusLost(FocusEvent e) {
    }

    @Override
    public String getHost() {
        return downloadLink.getHost();
    }
}
