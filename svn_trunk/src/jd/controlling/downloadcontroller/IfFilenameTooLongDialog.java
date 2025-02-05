package jd.controlling.downloadcontroller;

import java.awt.Color;
import java.awt.Dialog.ModalityType;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.concurrent.atomic.AtomicBoolean;

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
import org.appwork.utils.StringUtils;
import org.appwork.utils.swing.SwingUtils;
import org.appwork.utils.swing.dialog.AbstractDialog;
import org.appwork.utils.swing.dialog.Dialog;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.downloads.table.DownloadsTableModel;
import org.jdownloader.settings.GraphicalUserInterfaceSettings.SIZEUNIT;
import org.jdownloader.settings.IfFilenameTooLongAction;
import org.jdownloader.settings.staticreferences.CFG_GUI;

import jd.plugins.DownloadLink;
import jd.plugins.ParsedFilename;

public class IfFilenameTooLongDialog extends AbstractDialog<IfFilenameTooLongAction> implements IfFilenameTooLongDialogInterface, FocusListener {
    private final String            path;
    private IfFilenameTooLongAction result;
    private final String            packagename;

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
    private JRadioButton         rename;
    final JTextField             textfieldFilenameNew;
    private final String         packageID;
    private final DownloadLink   downloadLink;
    private final String         autoShortenedFilenameSuggestion;
    private final ParsedFilename parsedOriginalFilename;
    final AtomicBoolean          userChangedSelection               = new AtomicBoolean(false);
    private final JLabel         warningLabel1_is_empty_filename    = new JLabel("Filename can't be empty!");
    private final JLabel         warningLabel2_is_filename_too_long = new JLabel("Filename is too long!");

    public IfFilenameTooLongDialog(final DownloadLink link, final ParsedFilename parsedOriginalFilename, final String autoShortenedFilenameSuggestion) {
        super(Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN | UIOManager.LOGIC_COUNTDOWN, "Filename is too long", null, null, null);
        this.packagename = link.getFilePackage().getName();
        this.packageID = link.getFilePackage().getName() + "_" + link.getFilePackage().getCreated();
        this.path = link.getFileOutput();
        this.downloadLink = link;
        this.parsedOriginalFilename = parsedOriginalFilename;
        this.autoShortenedFilenameSuggestion = autoShortenedFilenameSuggestion;
        this.textfieldFilenameNew = new JTextField(this.autoShortenedFilenameSuggestion);
        setTimeout(60000);
    }

    ItemListener userSelectionListener = new ItemListener() {
        @Override
        public void itemStateChanged(ItemEvent e) {
            if (e.getStateChange() == ItemEvent.SELECTED) {
                userChangedSelection.set(true);
            }
            validateInput();
            stopTimer();
        }
    };

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
                /* Validation not passed aka user has entered bad "shortened" filename. */
                return IfFilenameTooLongAction.SKIP_FILE;
            }
        }
        // TODO: Set last selected value so that "do not ask again" dialog will work fine
        // if (result != null) {
        // org.jdownloader.settings.staticreferences.CFG_GUI.CFG.setLastIfFileExists(result);
        // }
        return result;
    }

    protected String getDontShowAgainLabelText() {
        return _GUI.T.IfFileExistsDialog_getDontShowAgainLabelText_();
    }

    @Override
    public JComponent layoutDialogContent() {
        final MigPanel p = new MigPanel("ins 0,wrap 1", "", "");
        final ExtTextArea txt = new ExtTextArea();
        txt.setLabelMode(true);
        txt.setToolTipText(path);
        // TODO: Maybe only display the 'subfolder by package name' hint if we know that that setting is enabled.
        // TODO: Add hyperlink to "subfolder by package name" support article.
        txt.setText("The name of this file is too long to write it to your filesystem.\r\nWhat should we do about it?\r\nHint: If you are running into this problem frequently, turn off 'subfolder by package name'.\nFor Windows users: You can remove the path length limitation via Registry though be aware that this can have unwanted side effects!");
        p.add(txt);
        final JTextField textfieldFilenameOld = new JTextField(this.downloadLink.getName());
        textfieldFilenameOld.setEditable(false);
        p.add(textfieldFilenameOld);
        p.add(SwingUtils.toBold(new JLabel("Current filename:")), "split 2,sg 1");
        // p.add(new JLabel(this.downloadLink.getName()));
        p.add(SwingUtils.toBold(new JLabel("File extension:")), "split 2,sg 1");
        p.add(new JTextField(this.parsedOriginalFilename.getExtensionAdvanced()));
        p.add(SwingUtils.toBold(new JLabel("Auto shortened filename:")), "split 2,sg 1");
        p.add(new JLabel(this.autoShortenedFilenameSuggestion));
        p.add(textfieldFilenameNew);
        warningLabel1_is_empty_filename.setForeground(Color.RED);
        warningLabel1_is_empty_filename.setVisible(false); // Hide for now
        p.add(warningLabel1_is_empty_filename);
        warningLabel2_is_filename_too_long.setForeground(Color.RED);
        warningLabel2_is_filename_too_long.setVisible(false); // Hide for now
        p.add(warningLabel2_is_filename_too_long);
        p.add(SwingUtils.toBold(new JLabel("Filesize:")), "split 2,sg 1");
        final SIZEUNIT maxSizeUnit = (SIZEUNIT) CFG_GUI.MAX_SIZE_UNIT.getValue();
        p.add(new JLabel(SIZEUNIT.formatValue(maxSizeUnit, this.downloadLink.getDownloadSize())));
        p.add(SwingUtils.toBold(new JLabel(_GUI.T.IfFileExistsDialog_layoutDialogContent_package())), "split 2,sg 1");
        p.add(new JLabel(packagename));
        final JTextField textfieldPackagename = new JTextField(packagename);
        textfieldPackagename.setEditable(false);
        p.add(textfieldPackagename);
        p.add(SwingUtils.toBold(new JLabel(_GUI.T.IfFileExistsDialog_layoutDialogContent_hoster())), "split 2,sg 1");
        p.add(new JLabel(downloadLink.getDomainInfo().getTld()));
        skip = new JRadioButton(_GUI.T.IfFileExistsDialog_layoutDialogContent_skip_());
        skip.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                result = IfFilenameTooLongAction.SKIP_FILE;
                validateInput();
            }
        });
        rename = new JRadioButton("Use shortened filename");
        rename.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                result = IfFilenameTooLongAction.RENAME_FILE;
                validateInput();
            }
        });
        // Group the radio buttons.
        final ButtonGroup group = new ButtonGroup();
        group.add(skip);
        group.add(rename);
        p.add(new JSeparator(), "pushx,growx");
        p.add(skip, "gapleft 10");
        p.add(rename, "gapleft 10");
        // TODO: Update this so last selection is correctly used as current default
        // IfFileExistsAction def = org.jdownloader.settings.staticreferences.CFG_GUI.CFG.getLastIfFileExists();
        IfFilenameTooLongAction def = null;
        switch (def) {
        case RENAME_FILE:
            rename.setSelected(true);
            break;
        default:
            skip.setSelected(true);
        }
        skip.addItemListener(userSelectionListener);
        rename.addItemListener(userSelectionListener);
        if (okButton != null) {
            okButton.addFocusListener(this);
        }
        this.textfieldFilenameNew.getDocument().addDocumentListener(new DocumentListener() {
            @Override
            public void insertUpdate(DocumentEvent e) {
                autoSelectRenameIfAllowed();
                validateInput();
                stopTimer();
            }

            @Override
            public void removeUpdate(DocumentEvent e) {
                autoSelectRenameIfAllowed();
                validateInput();
                stopTimer();
            }

            @Override
            public void changedUpdate(DocumentEvent e) {
                autoSelectRenameIfAllowed();
                validateInput();
                stopTimer();
            }
        });
        result = def;
        return p;
    }

    private void autoSelectRenameIfAllowed() {
        if (userChangedSelection.get() == false && filenameHasChanged()) {
            rename.setSelected(true);
            /* Will be set to true by now -> Set to false because we've changed the field, not the user. */
            userChangedSelection.set(false);
            result = IfFilenameTooLongAction.RENAME_FILE;
        }
    }

    /** Returns true if user defined filename differs from the initially suggested auto shortened filename. */
    private boolean filenameHasChanged() {
        if (!StringUtils.equals(this.autoShortenedFilenameSuggestion, this.textfieldFilenameNew.getText())) {
            return true;
        } else {
            return false;
        }
    }

    private boolean validateInput() {
        if (result == IfFilenameTooLongAction.RENAME_FILE) {
            final boolean isFilenameEmpty = StringUtils.isEmpty(textfieldFilenameNew.getText());
            final boolean isFilenameTooLong = textfieldFilenameNew.getText().length() > this.autoShortenedFilenameSuggestion.length();
            if (isFilenameEmpty || isFilenameTooLong) {
                warningLabel1_is_empty_filename.setVisible(isFilenameEmpty);
                warningLabel2_is_filename_too_long.setVisible(isFilenameTooLong);
                okButton.setEnabled(false);
                return false;
            }
        }
        warningLabel1_is_empty_filename.setVisible(false);
        warningLabel2_is_filename_too_long.setVisible(false);
        okButton.setEnabled(true);
        return true;
    }

    public IfFilenameTooLongAction getAction() {
        return result;
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
        String newFilename = textfieldFilenameNew.getText();
        if (newFilename == null) {
            return null;
        }
        final String ext = this.parsedOriginalFilename.getExtensionAdvanced();
        if (ext != null && !StringUtils.endsWithCaseInsensitive(newFilename, ext)) {
            newFilename += ext;
        }
        return newFilename;
    }
}
