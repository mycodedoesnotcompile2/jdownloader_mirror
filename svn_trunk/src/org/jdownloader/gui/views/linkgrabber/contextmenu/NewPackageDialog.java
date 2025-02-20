package org.jdownloader.gui.views.linkgrabber.contextmenu;

import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.io.File;

import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;

import org.appwork.swing.MigPanel;
import org.appwork.swing.components.ExtTextField;
import org.appwork.utils.StringUtils;
import org.appwork.utils.swing.dialog.AbstractDialog;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.LinkTreeUtils;
import org.jdownloader.translate._JDT;

import jd.gui.swing.jdgui.views.settings.components.FolderChooser;

public class NewPackageDialog extends AbstractDialog<Object> {
    private SelectionInfo<?, ?> selection;
    private ExtTextField        packageNameField;
    private FolderChooser       downloadFolderFolderChooserField;
    private JCheckBox           mergeCheckbox                             = null;
    private JCheckBox           expandCheckbox                            = null;
    private String              preSetDownloadFolder                      = null;
    private boolean             mergeCheckboxDefaultValue                 = false;
    private boolean             displayCheckboxMergeWithSameNamedPackages = true;
    private boolean             expandCheckboxDefaultValue                = false;

    public NewPackageDialog(SelectionInfo<?, ?> selection) {
        super(0, _GUI.T.NewPackageDialog_NewPackageDialog_(), null, null, null);
        this.selection = selection;
    }

    protected int getPreferredWidth() {
        return Math.min(Math.max(packageNameField.getPreferredSize().width, downloadFolderFolderChooserField.getPreferredSize().width) * 2, getDialog().getParent().getWidth());
    }

    private String getNewName() {
        String defValue = _GUI.T.MergeToPackageAction_actionPerformed_newpackage_();
        try {
            defValue = selection.getFirstPackage().getName();
        } catch (Throwable e2) {
            // too many unsafe casts. catch problems - just to be sure
            org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().log(e2);
        }
        return defValue;
    }

    @Override
    protected void initFocus(JComponent focus) {
        super.initFocus(packageNameField);
    }

    @Override
    protected Object createReturnValue() {
        return null;
    }

    @Override
    public JComponent layoutDialogContent() {
        MigPanel p = new MigPanel("ins 0,wrap 2", "[][grow,fill]", "[]");
        p.add(new JLabel(_GUI.T.NewPackageDialog_layoutDialogContent_newname_()));
        packageNameField = new ExtTextField();
        packageNameField.setText(getNewName());
        p.add(packageNameField);
        p.add(new JLabel(_GUI.T.NewPackageDialog_layoutDialogContent_saveto()));
        downloadFolderFolderChooserField = new FolderChooser();
        File path = null;
        if (StringUtils.isNotEmpty(preSetDownloadFolder)) {
            downloadFolderFolderChooserField.setText(preSetDownloadFolder);
        } else {
            path = LinkTreeUtils.getRawDownloadDirectory(selection.getFirstPackage());
            if (path != null) {
                downloadFolderFolderChooserField.setText(path.getAbsolutePath());
            }
        }
        p.add(downloadFolderFolderChooserField, "pushx,growx");
        if (displayCheckboxMergeWithSameNamedPackages) {
            mergeCheckbox = new JCheckBox(_GUI.T.MergeSameNamedPackagesAction_());
            mergeCheckbox.setSelected(mergeCheckboxDefaultValue);
            p.add(mergeCheckbox, "span 2");
        }
        expandCheckbox = new JCheckBox(_JDT.T.MergeToPackageAction_getTranslationForExpandNewPackage());
        expandCheckbox.setSelected(expandCheckboxDefaultValue);
        p.add(expandCheckbox, "span 2");
        return p;
    }

    @Override
    protected void packed() {
        packageNameField.addFocusListener(new FocusListener() {
            @Override
            public void focusLost(FocusEvent e) {
            }

            @Override
            public void focusGained(FocusEvent e) {
                packageNameField.selectAll();
            }
        });
        this.packageNameField.requestFocusInWindow();
        this.packageNameField.selectAll();
    }

    public String getName() {
        return packageNameField.getText();
    }

    public void setDownloadFolder(String path) {
        preSetDownloadFolder = path;
    }

    public String getDownloadFolder() {
        return downloadFolderFolderChooserField.getText();
    }

    public boolean isMergeWithSameNamedPackages() {
        if (mergeCheckbox != null) {
            return mergeCheckbox.isSelected();
        } else {
            return this.mergeCheckboxDefaultValue;
        }
    }

    public void setMergeCheckboxDefaultValue(boolean b) {
        this.mergeCheckboxDefaultValue = b;
        if (mergeCheckbox != null) {
            mergeCheckbox.setSelected(b);
        }
    }

    public boolean isExpandPackage() {
        if (expandCheckbox != null) {
            return expandCheckbox.isSelected();
        } else {
            return this.expandCheckboxDefaultValue;
        }
    }

    public void setExpandPackage(boolean b) {
        this.expandCheckboxDefaultValue = b;
        if (expandCheckbox != null) {
            expandCheckbox.setSelected(b);
        }
    }

    public void setDisplayCheckboxMergeWithSameNamedPackages(boolean display) {
        this.displayCheckboxMergeWithSameNamedPackages = display;
    }
}
