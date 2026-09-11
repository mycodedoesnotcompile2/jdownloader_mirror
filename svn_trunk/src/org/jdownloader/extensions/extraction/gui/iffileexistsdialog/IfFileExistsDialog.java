package org.jdownloader.extensions.extraction.gui.iffileexistsdialog;

import java.awt.Color;
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
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JRadioButton;
import javax.swing.JSeparator;
import javax.swing.JTextField;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import org.appwork.swing.MigPanel;
import org.appwork.swing.components.ExtTextArea;
import org.appwork.uio.UIOManager;
import org.appwork.utils.Files;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.swing.SwingUtils;
import org.appwork.utils.swing.dialog.AbstractDialog;
import org.appwork.utils.swing.dialog.Dialog;
import org.jdownloader.extensions.extraction.Archive;
import org.jdownloader.extensions.extraction.ArchiveFile;
import org.jdownloader.extensions.extraction.CFG_EXTRACTION;
import org.jdownloader.extensions.extraction.Item;
import org.jdownloader.extensions.extraction.bindings.downloadlink.DownloadLinkArchiveFile;
import org.jdownloader.extensions.extraction.translate.T;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.downloads.table.DownloadsTableModel;
import org.jdownloader.settings.IfFileExistsAction;
import org.jdownloader.translate._JDT;

import jd.controlling.downloadcontroller.FileNameField;
import jd.controlling.downloadcontroller.IfFileExistsDialogInterface;
import jd.controlling.downloadcontroller.PackageNameField;
import jd.controlling.linkcollector.LinknameCleaner;
import jd.plugins.DownloadLink;
import jd.plugins.ParsedFilename;

public class IfFileExistsDialog extends AbstractDialog<IfFileExistsAction> implements IfFileExistsDialogInterface, FocusListener {
    private final String       path;
    private IfFileExistsAction result;
    private String             packagename;

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
    private JRadioButton         rename;
    private JRadioButton         useCustom;
    private String               packageID;
    private DownloadLink         downloadLink;
    private final Archive        archive;
    private final Item           item;
    /** Pre-generated auto-rename suggestion (e.g. "file_2.ext"); shown read-only and pre-fills the custom filename input. */
    private final String         newNameString;
    /** Parsed base filename (the auto-rename suggestion) used to reattach the extension to a custom name on return. */
    private final ParsedFilename parsedBaseFilename;
    private FileNameField        customFilenameInput;
    private JTextField           textfieldCustomName;
    /** Non-blocking hint shown when the currently chosen new filename already exists in the target folder. */
    private final JLabel         filenameExistsWarning = new JLabel("");
    /** Fully transparent color: used to "hide" the always-present warning text without changing the reserved layout space. */
    private static final Color   TRANSPARENT_COLOR     = new Color(0, 0, 0, 0);
    /** Target folder the new file would be written into; used to check whether the chosen filename already exists. */
    private final File           targetFolder;

    @Override
    protected String getDontShowAgainLabelText() {
        return T.T.if_file_exists_dont_show_again();
    }

    public IfFileExistsDialog(File extractTo, Item item, Archive archive) {
        super(Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN | UIOManager.LOGIC_COUNTDOWN, _JDT.T.jd_controlling_SingleDownloadController_askexists_title(), null, null, null);
        this.archive = archive;
        for (ArchiveFile af : archive.getArchiveFiles()) {
            if (af instanceof DownloadLinkArchiveFile) {
                downloadLink = ((DownloadLinkArchiveFile) af).getDownloadLinks().get(0);
                this.packagename = downloadLink.getFilePackage().getName();
                this.packageID = downloadLink.getFilePackage().getName() + "_" + downloadLink.getFilePackage().getCreated();
                break;
            }
        }
        this.item = item;
        this.path = extractTo.getAbsolutePath();
        this.targetFolder = new File(this.path).getParentFile();
        String extension = Files.getExtension(extractTo.getName());
        String name = StringUtils.isEmpty(extension) ? extractTo.getName() : extractTo.getName().substring(0, extractTo.getName().length() - extension.length() - 1);
        int i = 1;
        while (extractTo.exists()) {
            if (StringUtils.isEmpty(extension)) {
                extractTo = new File(extractTo.getParentFile(), name + "_" + i);
            } else {
                extractTo = new File(extractTo.getParentFile(), name + "_" + i + "." + extension);
            }
            i++;
        }
        newNameString = extractTo.getName();
        this.parsedBaseFilename = new ParsedFilename(newNameString);
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

    /**
     * A custom (per-file) filename must never be stored as the global default. When the user chose to enter a custom filename the "Do not ask
     * again" checkbox is greyed out (disabled), which already makes the framework implementation return false; this override states the rule
     * explicitly so it does not depend on the checkbox's enabled state alone.
     */
    @Override
    public boolean isDontShowAgainSelected() {
        if (this.useCustom != null && this.useCustom.isSelected()) {
            return false;
        }
        return super.isDontShowAgainSelected();
    }

    /**
     * The framework creates the "Do not ask again" checkbox only after {@link #layoutDialogContent()}, so hook onto it here: toggling the
     * checkbox counts as user interaction (stops the auto-confirm timer) and its enabled state has to follow the custom-filename selection (a
     * per-file custom name must never become the global default).
     */
    @Override
    protected void initDoNotShowAgainCheckbox(final MigPanel bottom) {
        super.initDoNotShowAgainCheckbox(bottom);
        if (this.dontshowagain != null) {
            this.dontshowagain.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    stopTimer();
                }
            });
        }
        updateDontShowAgainState();
    }

    /** A custom (per-file) filename must never be stored as the global default, so disable the framework checkbox while it is selected. */
    private void updateDontShowAgainState() {
        if (this.dontshowagain != null) {
            final boolean customSelected = this.useCustom != null && this.useCustom.isSelected();
            this.dontshowagain.setEnabled(!customSelected);
            final java.awt.Container parent = this.dontshowagain.getParent();
            if (parent != null) {
                parent.revalidate();
                parent.repaint();
            }
        }
    }

    @Override
    public void dispose() {
        super.dispose();
        if (okButton != null) {
            okButton.removeFocusListener(this);
        }
        if (result != null) {
            CFG_EXTRACTION.CFG.setLatestIfFileExistsAction(result);
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
        txt.setText(T.T.file_exists_message());
        p.add(txt, textfieldConstraints);
        final File localFile = new File(path);
        /* Existing file. */
        p.add(SwingUtils.toBold(new JLabel(_GUI.T.IfFileExistsDialog_layoutDialogContent_filename())), "split 2,sg 1");
        p.add(new FileNameField(localFile.getName(), false), textfieldConstraints);
        /* Existing file's size first, then the new file's size (order intentionally existing -> new). */
        p.add(SwingUtils.toBold(new JLabel(_GUI.T.IfFileExistsDialog_layoutDialogContent_filesize_existing())), "split 2,sg 1");
        p.add(new JLabel(SizeFormatter.formatBytes(NumberFormat.getInstance(), localFile.length())));
        if (item != null) {
            p.add(SwingUtils.toBold(new JLabel(_GUI.T.IfFileExistsDialog_layoutDialogContent_filesize2())), "split 2,sg 1");
            if (item.getSize() >= 0) {
                p.add(new JLabel(SizeFormatter.formatBytes(NumberFormat.getInstance(), item.getSize())));
            } else {
                p.add(new JLabel(_GUI.T.OriginFilter_toString_nothing()));
            }
        }
        /* Pre-generated auto-rename suggestion (read-only). */
        p.add(SwingUtils.toBold(new JLabel(_GUI.T.IfFileExistsDialog_auto_renamed_filename())), "split 2,sg 1");
        p.add(new FileNameField(newNameString, false), textfieldConstraints);
        /* Editable custom filename, pre-filled with the suggestion. */
        p.add(SwingUtils.toBold(new JLabel(_GUI.T.IfFileExistsDialog_custom_filename())), "split 2,sg 1");
        this.customFilenameInput = new FileNameField(newNameString, true);
        this.textfieldCustomName = this.customFilenameInput.getNameField();
        p.add(this.customFilenameInput, textfieldConstraints);
        /*
         * Keep the warning text present at all times so its space (width and height) is reserved permanently; updateFilenameExistsWarning()
         * only toggles the color (red vs. transparent), so showing/hiding the warning never changes the dialog dimensions.
         */
        filenameExistsWarning.setText(_GUI.T.IfFilenameTooLongDialog_filename_already_exists());
        filenameExistsWarning.setForeground(TRANSPARENT_COLOR);
        p.add(SwingUtils.toBold(filenameExistsWarning));
        /* Package. */
        if (packagename != null) {
            p.add(SwingUtils.toBold(new JLabel(_GUI.T.IfFileExistsDialog_layoutDialogContent_package())), "split 2,sg 1");
            p.add(new PackageNameField(packagename), textfieldConstraints);
        }
        /* Archive (extraction-specific; no hoster shown here). */
        p.add(SwingUtils.toBold(new JLabel(T.T.IfFileExistsDialog_layoutDialogContent_archive())), "split 2,sg 1");
        p.add(new JLabel(archive.getName()));
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
        rename = new JRadioButton(IfFileExistsAction.AUTO_RENAME.getLabel());
        rename.addActionListener(new ActionListener() {
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
        // Group the radio buttons.
        final ButtonGroup group = new ButtonGroup();
        group.add(skip);
        group.add(overwrite);
        group.add(rename);
        group.add(useCustom);
        p.add(new JSeparator(), "pushx,growx");
        p.add(skip, "gapleft 10");
        p.add(overwrite, "gapleft 10");
        p.add(rename, "gapleft 10");
        p.add(useCustom, "gapleft 10");
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
        IfFileExistsAction def = CFG_EXTRACTION.CFG.getLatestIfFileExistsAction();
        if (def == null) {
            def = IfFileExistsAction.SKIP_FILE;
        }
        switch (def) {
        case AUTO_RENAME:
            rename.setSelected(true);
            break;
        case OVERWRITE_FILE:
            overwrite.setSelected(true);
            break;
        default:
            skip.setSelected(true);
            def = IfFileExistsAction.SKIP_FILE;
            break;
        }
        result = def;
        /*
         * Stop the auto-confirm countdown as soon as the user makes a choice. Added after the default selection above so it does not fire for
         * the initial (programmatic) default (same principle as the download IfFileExistsDialog).
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
        rename.addItemListener(userSelectionListener);
        useCustom.addItemListener(userSelectionListener);
        if (okButton != null) {
            okButton.addFocusListener(this);
        }
        onSelectionChanged();
        return p;
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
        updateDontShowAgainState();
    }

    /**
     * Shows a red warning when the currently chosen new filename already exists in the target folder. For the auto-rename suggestion this is
     * only a hint (the suggestion was pre-computed to be free). For a user-entered custom filename it additionally greys out the OK button,
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
         * auto-rename suggestion is pre-computed to be free, so only the custom case blocks confirmation.
         */
        if (this.okButton != null) {
            final boolean customFilenameExists = exists && this.useCustom != null && this.useCustom.isSelected();
            this.okButton.setEnabled(!customFilenameExists);
        }
    }

    public String getNewName() {
        return getNewFilename();
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
        /* Auto-rename selected -> the pre-generated suggestion. */
        return newNameString;
    }

    @Override
    public boolean isRememberForPackageSelected() {
        /* No per-package "remember" option in the extraction dialog. */
        return false;
    }

    @Override
    public boolean isDontAskAgainThisSessionSelected() {
        /* No "don't ask again during this session" option in the extraction dialog. */
        return false;
    }

    @Override
    public org.jdownloader.settings.GeneralSettings.OnSkipDueToAlreadyExistsAction getOnSkipDueToAlreadyExistsAction() {
        /* No "on skip due to already exists" dropdown in the extraction dialog -> fall back to the global config value. */
        return null;
    }

    public IfFileExistsAction getAction() {
        return result;
    }

    public String getFilePath() {
        return path;
    }

    public IfFileExistsDialogInterface show() {
        return UIOManager.I().show(IfFileExistsDialogInterface.class, this);
    }

    @Override
    public void focusGained(FocusEvent e) {
        if (downloadLink != null) {
            DownloadsTableModel.getInstance().setSelectedObject(downloadLink);
        }
    }

    @Override
    public void focusLost(FocusEvent e) {
    }

    @Override
    protected IfFileExistsAction createReturnValue() {
        if (okButton != null && !okButton.isEnabled()) {
            /* OK greyed out (custom filename collides with an existing file) -> do not proceed with it; treat as skip. */
            return IfFileExistsAction.SKIP_FILE;
        }
        return null;
    }

    @Override
    public String getHost() {
        return downloadLink.getHost();
    }
}
