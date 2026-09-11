package jd.controlling.downloadcontroller;

import java.awt.Color;
import java.awt.Dialog.ModalityType;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.File;

import javax.swing.ButtonGroup;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
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
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.SwingUtils;
import org.appwork.utils.swing.dialog.AbstractDialog;
import org.appwork.utils.swing.dialog.Dialog;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.downloads.table.DownloadsTableModel;
import org.jdownloader.settings.GraphicalUserInterfaceSettings.SIZEUNIT;
import org.jdownloader.settings.staticreferences.CFG_GUI;

import jd.controlling.linkcollector.LinknameCleaner;
import jd.plugins.DownloadLink;
import jd.plugins.ParsedFilename;

public class IfFilenameTooLongDialog extends AbstractDialog<IfFilenameTooLongDialog.IfFilenameTooLongAction> implements IfFilenameTooLongDialogInterface, FocusListener {
    /** Defines what happens when a filename is too long to be saved on the filesystem. */
    public static enum IfFilenameTooLongAction implements LabelInterface {
        SKIP_FILE() {
            @Override
            public String getLabel() {
                return _GUI.T.skip_file();
            }
        },
        AUTO_RENAME() {
            @Override
            public String getLabel() {
                return _GUI.T.too_long_filename_use_shortened_filename();
            }
        },
        ASK_FOR_EACH_FILE() {
            @Override
            public String getLabel() {
                return _GUI.T.ask();
            }
        }
    }

    /**
     * Builds the dialog flags. STYLE_SHOW_DO_NOT_DISPLAY_AGAIN adds the framework checkbox, which this dialog uses as the global "Do not
     * ask again" option (see {@link #getDontShowAgainLabelText()}). The separate per-package "Remember selection for this Package" checkbox
     * is a dedicated component added in {@link #layoutDialogContent()}.
     */
    private static int getDialogFlags(final DownloadLink link) {
        return UIOManager.LOGIC_COUNTDOWN | Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN;
    }

    @Override
    public boolean isRemoteAPIEnabled() {
        return true;
    }

    @Override
    public String getPackagename() {
        return packagename;
    }

    @Override
    public String getPackageID() {
        return packageID;
    }

    private final String            path;
    private IfFilenameTooLongAction result;
    private final String            packagename;
    private final boolean           singleItemPackage;
    private JRadioButton            skip;
    private JRadioButton            useAutoShortened;
    private JRadioButton            useCustom;
    private JCheckBox               rememberForPackage;
    /** "Don't ask again during this session" checkbox: remembers the decision session-wide (stored in the DownloadSession). */
    private JCheckBox               dontAskAgainThisSession;
    private JTextField              textfieldFilenameNew;
    private FileNameField           customFilenameInput;
    private final JLabel            newFilenameCharactersLeft = new JLabel("");
    /** Fully transparent color: used to "hide" the always-present warning text without changing the reserved layout space. */
    private static final Color      TRANSPARENT_COLOR         = new Color(0, 0, 0, 0);
    /** Non-blocking hint shown when the currently chosen (shortened) filename already exists in the download folder. */
    private final JLabel            filenameExistsWarning     = new JLabel("");
    /** Download folder the chosen filename would be written into; used to check whether that filename already exists. */
    private final File              targetFolder;
    private final String            packageID;
    private final DownloadLink      downloadLink;
    private final String            autoShortenedFilename;
    private final String            autoShortenedFilenameWithoutExt;
    private final ParsedFilename    parsedOriginalFilename;

    public IfFilenameTooLongDialog(final DownloadLink link, final ParsedFilename originalFilenameParsed, final String autoShortenedFilenameSuggestion) {
        super(getDialogFlags(link), _GUI.T.IfFilenameTooLongDialog_title(), null, null, null);
        this.packagename = link.getFilePackage().getName();
        this.singleItemPackage = link.getFilePackage().size() == 1;
        this.packageID = link.getFilePackage().getPackageKey();
        this.path = link.getFileOutput();
        this.targetFolder = new File(this.path).getParentFile();
        this.downloadLink = link;
        this.parsedOriginalFilename = originalFilenameParsed;
        autoShortenedFilename = autoShortenedFilenameSuggestion;
        final String ext = originalFilenameParsed.getExtensionAdvanced();
        if (ext != null) {
            this.autoShortenedFilenameWithoutExt = autoShortenedFilenameSuggestion.substring(0, autoShortenedFilenameSuggestion.length() - ext.length());
        } else {
            this.autoShortenedFilenameWithoutExt = autoShortenedFilenameSuggestion;
        }
        setTimeout(60000);
    }

    ItemListener userSelectionListener = new ItemListener() {
        @Override
        public void itemStateChanged(ItemEvent e) {
            if (e.getStateChange() != ItemEvent.SELECTED) {
                return;
            }
            updateNewFilenameCharactersLeftTextAndColor();
            updateDontShowAgainVisibility();
            stopTimer();
        }
    };

    /**
     * A custom filename is a per-file decision, so neither "Remember selection for this Package" (dedicated {@link #rememberForPackage}
     * checkbox) nor "Do not ask again" (framework {@link #dontshowagain} checkbox) may be applied when the user chose to enter a custom
     * filename. In that case both checkboxes are greyed out (disabled), which makes {@link #isRememberForPackageSelected()} and
     * {@link #isDontShowAgainSelected()} return false regardless of their checked state. The "Do not ask again" checkbox keeps its checked
     * state (it is only disabled), while the per-package checkbox is additionally unchecked. For all other options the checkboxes stay
     * enabled.</br>
     * Exception: for a single-item package the "Remember selection for this Package" checkbox is meaningless, so it is shown deselected and
     * greyed out (disabled), regardless of the selected option.
     */
    private void updateDontShowAgainVisibility() {
        final boolean customSelected = this.useCustom != null && this.useCustom.isSelected();
        final boolean dontShowAgainSelected = this.dontshowagain != null && this.dontshowagain.isSelected();
        if (this.rememberForPackage != null) {
            if (singleItemPackage) {
                /*
                 * Single-item package: "Remember selection for this Package" is meaningless to toggle -> show it deselected and disabled.
                 */
                this.rememberForPackage.setSelected(false);
                this.rememberForPackage.setEnabled(false);
            } else {
                if (customSelected) {
                    /* Uncheck it too, so a custom (per-file) filename is never remembered for the whole package. */
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
            /*
             * A custom (per-file) filename must never be stored as the global default. We only grey out "Do not ask again" here (keeping
             * its checked state) instead of unchecking it: isDontShowAgainSelected() requires the checkbox to be enabled, so a disabled box
             * already reports false and thus prevents storing auto-rename as the global default while a custom filename is selected.
             */
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
    protected IfFilenameTooLongAction createReturnValue() {
        if (okButton != null) {
            okButton.removeFocusListener(this);
            if (!okButton.isEnabled()) {
                /* Validation not passed aka user has entered bad "shortened" filename -> do not remember this as the last choice. */
                return IfFilenameTooLongAction.SKIP_FILE;
            }
        }
        /* Remember the last choice for pre-selection next time. */
        if (result != null) {
            CFG_GUI.CFG.setLastIfFilenameTooLong(result);
        }
        return result;
    }

    protected String getDontShowAgainLabelText() {
        /* The framework's built-in "do not display again" checkbox is used here as the global "Do not ask again" default option. */
        return _GUI.T.IfFilenameTooLongDialog_do_not_ask_again();
    }

    /**
     * A custom (per-file) filename must never be stored as the global default. While the checkbox is also greyed out (disabled) in that
     * case - which would already make the framework implementation return false - this override states the rule explicitly so the "false
     * for a custom filename" case does not depend on the checkbox's enabled state alone.
     */
    @Override
    public boolean isDontShowAgainSelected() {
        if (this.useCustom != null && this.useCustom.isSelected()) {
            return false;
        }
        return super.isDontShowAgainSelected();
    }

    @Override
    public JComponent layoutDialogContent() {
        final String textfieldConstraints = "growx, pushx, wmin 100";
        final MigPanel p = new MigPanel("ins 0,wrap 1, wmax 500", "", "");
        final ExtTextArea txt = new ExtTextArea();
        txt.setLabelMode(true);
        txt.setLineWrap(true);
        txt.setWrapStyleWord(true);
        txt.setText(_GUI.T.IfFilenameTooLongDialog_message());
        p.add(txt, textfieldConstraints);
        /* Hint that advanced users can configure their own filename cleanup regexes. */
        final ExtTextArea regexHint = new ExtTextArea();
        regexHint.setLabelMode(true);
        regexHint.setLineWrap(true);
        regexHint.setWrapStyleWord(true);
        regexHint.setText(_GUI.T.IfFilenameTooLongDialog_regex_replace_hint());
        p.add(regexHint, textfieldConstraints);
        p.add(SwingUtils.toBold(new JLabel(_GUI.T.IfFilenameTooLongDialog_current_filename())), "split 2,sg 1");
        p.add(new FileNameField(this.parsedOriginalFilename, false), textfieldConstraints);
        p.add(SwingUtils.toBold(new JLabel(_GUI.T.IfFilenameTooLongDialog_auto_shortened_filename())), "split 2,sg 1");
        p.add(new FileNameField(autoShortenedFilename, false), textfieldConstraints);
        /* The custom filename is the only editable one; keep a reference to its (editable) name field for validation and reassembly. */
        p.add(SwingUtils.toBold(new JLabel(_GUI.T.IfFilenameTooLongDialog_custom_shortened_filename())), "split 2,sg 1");
        this.customFilenameInput = new FileNameField(autoShortenedFilename, true);
        this.textfieldFilenameNew = this.customFilenameInput.getNameField();
        p.add(this.customFilenameInput, textfieldConstraints);
        p.add(SwingUtils.toBold(newFilenameCharactersLeft));
        /*
         * Keep the warning text present at all times so its space (width and height) is reserved permanently; updateFilenameExistsWarning()
         * only toggles the color (red vs. transparent), so showing/hiding the warning never changes the dialog dimensions.
         */
        filenameExistsWarning.setText(_GUI.T.IfFilenameTooLongDialog_filename_already_exists());
        filenameExistsWarning.setForeground(TRANSPARENT_COLOR);
        p.add(SwingUtils.toBold(filenameExistsWarning));
        updateNewFilenameCharactersLeftTextAndColor();
        p.add(SwingUtils.toBold(new JLabel(_GUI.T.IfFilenameTooLongDialog_filesize())), "split 2,sg 1");
        final SIZEUNIT maxSizeUnit = CFG_GUI.MAX_SIZE_UNIT.getValue();
        final long bytesTotal = this.downloadLink.getView().getBytesTotal();
        /* A negative value means the filesize is unknown and must be displayed as "~", not as an absolute byte value. */
        p.add(new JLabel(SIZEUNIT.formatValue(maxSizeUnit.toNonNegativeUnit(), bytesTotal)));
        p.add(SwingUtils.toBold(new JLabel(_GUI.T.IfFileExistsDialog_layoutDialogContent_package())), "split 2,sg 1");
        p.add(new PackageNameField(packagename), textfieldConstraints);
        p.add(SwingUtils.toBold(new JLabel(_GUI.T.IfFileExistsDialog_layoutDialogContent_hoster())), "split 2,sg 1");
        p.add(new JLabel(downloadLink.getDomainInfo().getTld()));
        // Group the radio buttons.
        final ButtonGroup group = new ButtonGroup();
        skip = new JRadioButton(IfFilenameTooLongAction.SKIP_FILE.getLabel());
        skip.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                result = IfFilenameTooLongAction.SKIP_FILE;
                updateNewFilenameCharactersLeftTextAndColor();
            }
        });
        useAutoShortened = new JRadioButton(IfFilenameTooLongAction.AUTO_RENAME.getLabel());
        useAutoShortened.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                result = IfFilenameTooLongAction.AUTO_RENAME;
                updateNewFilenameCharactersLeftTextAndColor();
            }
        });
        useCustom = new JRadioButton(_GUI.T.IfFilenameTooLongDialog_use_custom_filename());
        useCustom.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                result = IfFilenameTooLongAction.AUTO_RENAME;
                updateNewFilenameCharactersLeftTextAndColor();
            }
        });
        group.add(skip);
        group.add(useAutoShortened);
        group.add(useCustom);
        p.add(new JSeparator(), "pushx,growx");
        p.add(skip, "gapleft 10");
        p.add(useAutoShortened, "gapleft 10");
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
        /*
         * Default selection: the last action the user chose. A custom filename is per-file and is never remembered, so AUTO_RENAME maps to
         * "use auto shortened filename". ASK_FOR_EACH_FILE must never be pre-selected as a result -> fall back to auto shortened.
         */
        IfFilenameTooLongAction lastAction = CFG_GUI.CFG.getLastIfFilenameTooLong();
        if (lastAction == null || lastAction == IfFilenameTooLongAction.ASK_FOR_EACH_FILE) {
            lastAction = IfFilenameTooLongAction.AUTO_RENAME;
        }
        switch (lastAction) {
        case SKIP_FILE:
            skip.setSelected(true);
            result = IfFilenameTooLongAction.SKIP_FILE;
            break;
        case ASK_FOR_EACH_FILE:
        case AUTO_RENAME:
        default:
            useAutoShortened.setSelected(true);
            result = IfFilenameTooLongAction.AUTO_RENAME;
            break;
        }
        skip.addItemListener(userSelectionListener);
        useAutoShortened.addItemListener(userSelectionListener);
        useCustom.addItemListener(userSelectionListener);
        if (okButton != null) {
            okButton.addFocusListener(this);
        }
        this.textfieldFilenameNew.getDocument().addDocumentListener(new DocumentListener() {
            @Override
            public void insertUpdate(DocumentEvent e) {
                onFilenameChanged();
            }

            @Override
            public void removeUpdate(DocumentEvent e) {
                onFilenameChanged();
            }

            @Override
            public void changedUpdate(DocumentEvent e) {
                onFilenameChanged();
            }
        });
        setTextFieldLimit(this.textfieldFilenameNew);
        /*
         * The default selection above (useAutoShortened) is set before the item listeners are attached, so it does not trigger an update.
         * Refresh once here so the initial state (characters left and the "file already exists" hint) reflects the default selection.
         */
        updateNewFilenameCharactersLeftTextAndColor();
        return p;
    }

    private int getEffectiveMaxNewFilenameLength() {
        return this.autoShortenedFilenameWithoutExt.length();
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
                if (cleaned.length() < 1 || cleaned.length() > getEffectiveMaxNewFilenameLength()) {
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

    private void onFilenameChanged() {
        autoSelectCustomIfAllowed();
        updateNewFilenameCharactersLeftTextAndColor();
        stopTimer();
    }

    private void autoSelectCustomIfAllowed() {
        if (!filenameHasChanged()) {
            /* File name hasn't changed -> Do nothing. */
            return;
        }
        useCustom.setSelected(true);
        result = IfFilenameTooLongAction.AUTO_RENAME;
    }

    private void updateNewFilenameCharactersLeftTextAndColor() {
        final int maxCharacters = getEffectiveMaxNewFilenameLength();
        final int charactersLeft = maxCharacters - this.textfieldFilenameNew.getText().length();
        if (this.useCustom != null && this.useCustom.isSelected() && charactersLeft <= 0) {
            newFilenameCharactersLeft.setForeground(Color.RED);
        } else {
            newFilenameCharactersLeft.setForeground(Color.BLACK);
        }
        newFilenameCharactersLeft.setText(_GUI.T.IfFilenameTooLongDialog_characters_left(String.valueOf(charactersLeft), String.valueOf(maxCharacters)));
        updateFilenameExistsWarning();
    }

    /**
     * Shows a red warning when the currently chosen (shortened) filename already exists in the download folder. For the auto-shortened
     * suggestion this is only a hint (the "file already exists" handling downstream applies the user's policy). For a user-entered custom
     * filename it additionally greys out the OK button, so a colliding custom name cannot be confirmed.
     */
    private void updateFilenameExistsWarning() {
        if (this.filenameExistsWarning == null) {
            /* Called before the label exists (should not happen). */
            return;
        }
        boolean exists = false;
        if (this.result == IfFilenameTooLongAction.AUTO_RENAME && this.targetFolder != null) {
            /* Only relevant when we are actually going to write a (shortened) file; irrelevant when the file is skipped. */
            final String newFilename = getNewFilename();
            if (newFilename != null && new File(this.targetFolder, newFilename).exists()) {
                exists = true;
            }
        }
        /* The text stays set at all times (space reserved); only the color toggles so the dialog dimensions never change. */
        this.filenameExistsWarning.setForeground(exists ? Color.RED : TRANSPARENT_COLOR);
        /*
         * Grey out OK when the user entered a custom filename that already exists on disk, so a colliding name cannot be confirmed. The
         * auto-shortened suggestion is handled by the downstream policy, so only the custom case blocks confirmation.
         */
        if (this.okButton != null) {
            final boolean customFilenameExists = exists && this.useCustom != null && this.useCustom.isSelected();
            this.okButton.setEnabled(!customFilenameExists);
        }
    }

    /** Returns true if user defined filename differs from the initially suggested auto shortened filename. */
    private boolean filenameHasChanged() {
        if (!StringUtils.equals(this.autoShortenedFilenameWithoutExt, this.textfieldFilenameNew.getText())) {
            return true;
        } else {
            return false;
        }
    }

    public IfFilenameTooLongAction getAction() {
        return result;
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

    public String getFilePath() {
        return path;
    }

    public IfFilenameTooLongDialogInterface show() {
        return UIOManager.I().show(IfFilenameTooLongDialogInterface.class, this);
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

    @Override
    public String getNewFilename() {
        if (useCustom == null || !useCustom.isSelected()) {
            /* "Use auto shortened filename" is selected -> Return the auto shortened filename (already includes the extension). */
            return autoShortenedFilename;
        }
        /*
         * Custom filename selected -> Run the (manually entered or pasted) name part through the same cleanup used for crawled filenames
         * before reattaching the unchanged extension.
         */
        String name = this.textfieldFilenameNew.getText();
        if (name == null) {
            return null;
        }
        name = LinknameCleaner.cleanFilename(name);
        final String ext = this.parsedOriginalFilename.getExtensionAdvanced();
        if (ext != null && !StringUtils.endsWithCaseInsensitive(name, ext)) {
            name += ext;
        }
        return name;
    }
}
