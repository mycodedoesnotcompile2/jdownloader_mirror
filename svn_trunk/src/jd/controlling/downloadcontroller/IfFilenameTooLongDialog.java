package jd.controlling.downloadcontroller;

import java.awt.Color;
import java.awt.Dialog.ModalityType;
import java.awt.Toolkit;
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
import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DocumentFilter;

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
    private JRadioButton            skip;
    private JRadioButton            rename;
    final JTextField                textfieldFilenameNew;
    private final JLabel            newFilenameCharactersLeft = new JLabel("");
    private final String            packageID;
    private final DownloadLink      downloadLink;
    private final String            autoShortenedFilename;
    private final String            autoShortenedFilenameWithoutExt;
    private final ParsedFilename    parsedOriginalFilename;
    final AtomicBoolean             userChangedSelection      = new AtomicBoolean(false);

    public IfFilenameTooLongDialog(final DownloadLink link, final ParsedFilename originalFilenameParsed, final String autoShortenedFilenameSuggestion) {
        super(Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN | UIOManager.LOGIC_COUNTDOWN, "Filename is too long", null, null, null);
        this.packagename = link.getFilePackage().getName();
        this.packageID = link.getFilePackage().getPackageKey();
        this.path = link.getFileOutput();
        this.downloadLink = link;
        this.parsedOriginalFilename = originalFilenameParsed;
        autoShortenedFilename = autoShortenedFilenameSuggestion;
        final String ext = originalFilenameParsed.getExtensionAdvanced();
        if (ext != null) {
            this.autoShortenedFilenameWithoutExt = autoShortenedFilenameSuggestion.substring(0, autoShortenedFilenameSuggestion.length() - ext.length());
        } else {
            this.autoShortenedFilenameWithoutExt = autoShortenedFilenameSuggestion;
        }
        this.textfieldFilenameNew = new JTextField(this.autoShortenedFilenameWithoutExt);
        setTimeout(60000);
    }

    ItemListener userSelectionListener = new ItemListener() {
        @Override
        public void itemStateChanged(ItemEvent e) {
            if (e.getStateChange() != ItemEvent.SELECTED) {
                return;
            }
            userChangedSelection.set(true);
            updateNewFilenameCharactersLeftTextAndColor();
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
        final String textfieldConstraints = "growx, pushx, wmin 100";
        final MigPanel p = new MigPanel("ins 0,wrap 1, wmax 500", "", "");
        final ExtTextArea txt = new ExtTextArea();
        txt.setLabelMode(true);
        txt.setLineWrap(true);
        txt.setWrapStyleWord(true);
        txt.setText("The filename is too long to be saved on your filesystem.\r\nHow would you like to proceed?");
        p.add(txt, textfieldConstraints);
        p.add(SwingUtils.toBold(new JLabel("Current filename:")), "split 2,sg 1");
        final JTextField textfieldFilenameCurrent = new JTextField(this.downloadLink.getName());
        textfieldFilenameCurrent.setEditable(false);
        p.add(textfieldFilenameCurrent, textfieldConstraints);
        p.add(SwingUtils.toBold(new JLabel("Auto shortened filename:")), "split 2,sg 1");
        final JTextField textfieldFilenameAutoShortened = new JTextField(autoShortenedFilename);
        textfieldFilenameAutoShortened.setEditable(false);
        p.add(textfieldFilenameAutoShortened, textfieldConstraints);
        final String ext = this.parsedOriginalFilename.getExtensionAdvanced();
        p.add(SwingUtils.toBold(new JLabel("Custom shortened filename:")), "split 3,sg 1");
        p.add(textfieldFilenameNew, textfieldConstraints);
        final JTextField extField = new JTextField(ext != null ? ext : "NO_EXT");
        extField.setEditable(false);
        p.add(extField, textfieldConstraints);
        p.add(SwingUtils.toBold(newFilenameCharactersLeft));
        updateNewFilenameCharactersLeftTextAndColor();
        p.add(SwingUtils.toBold(new JLabel("Filesize:")), "split 2,sg 1");
        final SIZEUNIT maxSizeUnit = (SIZEUNIT) CFG_GUI.MAX_SIZE_UNIT.getValue();
        p.add(new JLabel(SIZEUNIT.formatValue(maxSizeUnit, this.downloadLink.getView().getBytesTotal())));
        p.add(SwingUtils.toBold(new JLabel(_GUI.T.IfFileExistsDialog_layoutDialogContent_package())), "split 2,sg 1");
        final JTextField textfieldPackagename = new JTextField(packagename);
        textfieldPackagename.setEditable(false);
        p.add(textfieldPackagename, textfieldConstraints);
        p.add(SwingUtils.toBold(new JLabel(_GUI.T.IfFileExistsDialog_layoutDialogContent_hoster())), "split 2,sg 1");
        p.add(new JLabel(downloadLink.getDomainInfo().getTld()));
        // Group the radio buttons.
        final ButtonGroup group = new ButtonGroup();
        skip = new JRadioButton(_GUI.T.IfFileExistsDialog_layoutDialogContent_skip_());
        skip.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                result = IfFilenameTooLongAction.SKIP_FILE;
                updateNewFilenameCharactersLeftTextAndColor();
            }
        });
        rename = new JRadioButton("Use shortened filename");
        rename.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                result = IfFilenameTooLongAction.RENAME_FILE;
                updateNewFilenameCharactersLeftTextAndColor();
            }
        });
        group.add(skip);
        group.add(rename);
        p.add(new JSeparator(), "pushx,growx");
        p.add(skip, "gapleft 10");
        p.add(rename, "gapleft 10");
        // TODO: Update this so last selection is correctly used as current default
        // IfFileExistsAction def = org.jdownloader.settings.staticreferences.CFG_GUI.CFG.getLastIfFileExists();
        IfFilenameTooLongAction def = IfFilenameTooLongAction.SKIP_FILE;
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
        result = def;
        return p;
    }

    private int getEffectiveMaxNewFilenameLength() {
        return this.autoShortenedFilenameWithoutExt.length();
    }

    /** This logic limits input in given textfield. */
    private void setTextFieldLimit(final JTextField textField) {
        ((AbstractDocument) textField.getDocument()).setDocumentFilter(new DocumentFilter() {
            @Override
            public void insertString(FilterBypass fb, int offset, String string, AttributeSet attr) throws BadLocationException {
                if (!isValidLength(fb, string, 0)) {
                    triggerWarning(textField);
                    return;
                }
                super.insertString(fb, offset, string, attr);
            }

            @Override
            public void remove(FilterBypass fb, int offset, int length) throws BadLocationException {
                if (fb.getDocument().getLength() - length < 1) {
                    /* User is trying to remove last character -> Don't allow him to */
                    triggerWarning(textField);
                    return;
                }
                super.remove(fb, offset, length);
            }

            @Override
            public void replace(FilterBypass fb, int offset, int length, String string, AttributeSet attr) throws BadLocationException {
                if (!isValidLength(fb, string, length)) {
                    triggerWarning(textField);
                    return;
                }
                super.replace(fb, offset, length, string, attr);
            }

            private boolean isValidLength(FilterBypass fb, String string, int lengthToRemove) {
                if (string == null) {
                    return false;
                }
                final int maxlen = getEffectiveMaxNewFilenameLength();
                final int newlen = fb.getDocument().getLength() - lengthToRemove + string.length();
                if (newlen > maxlen) {
                    return false;
                } else {
                    return true;
                }
            }
        });
    }

    private void triggerWarning(final JTextField textField) {
        Toolkit.getDefaultToolkit().beep();
        textField.setForeground(Color.RED);
        new Thread(() -> {
            try {
                Thread.sleep(500);
            } catch (InterruptedException ignored) {
            }
            textField.setForeground(Color.BLACK);
        }).start();
    }

    private void onFilenameChanged() {
        autoSelectRenameIfAllowed();
        updateNewFilenameCharactersLeftTextAndColor();
        stopTimer();
    }

    private void autoSelectRenameIfAllowed() {
        if (userChangedSelection.get() == true) {
            /* User has manually changed radio buttons -> Do not auto change selection anymore. */
            return;
        } else if (!filenameHasChanged()) {
            /* File name hasn't changed -> Do nothing. */
            return;
        }
        rename.setSelected(true);
        /* Will be set to true by now -> Set to false because we've changed the field, not the user. */
        userChangedSelection.set(false);
        result = IfFilenameTooLongAction.RENAME_FILE;
    }

    private void updateNewFilenameCharactersLeftTextAndColor() {
        final int charactersLeft = getEffectiveMaxNewFilenameLength() - this.textfieldFilenameNew.getText().length();
        if (this.result == IfFilenameTooLongAction.RENAME_FILE && this.userChangedSelection.get() == true && charactersLeft <= 0) {
            newFilenameCharactersLeft.setForeground(Color.RED);
        } else {
            newFilenameCharactersLeft.setForeground(Color.BLACK);
        }
        newFilenameCharactersLeft.setText(String.format("Characters left: %d", charactersLeft));
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
