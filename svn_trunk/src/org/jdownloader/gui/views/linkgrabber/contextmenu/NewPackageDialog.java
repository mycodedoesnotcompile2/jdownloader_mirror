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
import org.jdownloader.controlling.packagizer.PackagizerController;
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
    private JCheckBox           subfolderByPackageCheckbox                = null;
    private String              preSetDownloadFolder                      = null;
    private String              preSetPackageName                         = null;
    private boolean             mergeCheckboxDefaultValue                 = false;
    private boolean             displayCheckboxMergeWithSameNamedPackages = true;
    private boolean             expandCheckboxDefaultValue                = false;
    private boolean             subfolderByPackageDefaultValue            = false;

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
    protected void initFocus(final JComponent focus) {
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
        if (preSetPackageName != null) {
            packageNameField.setText(preSetPackageName);
        } else {
            packageNameField.setText(getNewName());
        }
        p.add(packageNameField);
        p.add(new JLabel(_GUI.T.NewPackageDialog_layoutDialogContent_saveto()));
        downloadFolderFolderChooserField = new FolderChooser();
        File path = null;
        if (StringUtils.isNotEmpty(preSetDownloadFolder)) {
            downloadFolderFolderChooserField.setText(stripTrailingSubfolderTag(preSetDownloadFolder));
        } else {
            path = LinkTreeUtils.getRawDownloadDirectory(selection.getFirstPackage());
            if (path != null) {
                /* getPath(): no absolutization needed here, the raw directory is only used for display. */
                downloadFolderFolderChooserField.setText(stripTrailingSubfolderTag(path.getPath()));
            }
        }
        p.add(downloadFolderFolderChooserField, "pushx,growx");
        /* Subfolder-by-packagename checkbox must be the topmost checkbox, directly below the download path. */
        subfolderByPackageCheckbox = new JCheckBox(_JDT.T.PackagizerSettings_folderbypackage_rule_name());
        subfolderByPackageCheckbox.setSelected(subfolderByPackageDefaultValue);
        p.add(subfolderByPackageCheckbox, "span 2");
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
        final String folder = downloadFolderFolderChooserField.getText();
        if (subfolderByPackageCheckbox != null) {
            return applySubfolderByPackage(folder, subfolderByPackageCheckbox.isSelected());
        }
        return folder;
    }

    public void setSubfolderByPackage(boolean b) {
        this.subfolderByPackageDefaultValue = b;
    }

    public boolean isSubfolderByPackage() {
        if (subfolderByPackageCheckbox != null) {
            return subfolderByPackageCheckbox.isSelected();
        } else {
            return this.subfolderByPackageDefaultValue;
        }
    }

    /**
     * Removes a trailing packagename subfolder tag ({@link PackagizerController#PACKAGETAG}) from the given folder, if present.
     *
     * Detection is done on the last path segment ({@link File#getName()}) rather than {@link String#endsWith(String)}, so a trailing
     * separator (e.g. "...\<jd:packagename>\") does not hide the tag. Results are returned via {@link File#getPath()} instead of
     * {@link File#getAbsolutePath()} to avoid its potential native IO call and to leave a relative path relative.
     */
    public static String stripTrailingSubfolderTag(final String folder) {
        if (StringUtils.isEmpty(folder)) {
            return folder;
        }
        final File f = new File(folder.trim());
        if (PackagizerController.PACKAGETAG.equals(f.getName())) {
            final File parent = f.getParentFile();
            if (parent != null) {
                return parent.getPath();
            }
        }
        return folder;
    }

    /**
     * Appends or removes the packagename subfolder tag depending on the given flag. The tag is stripped first so it can never be added
     * twice, even if the given folder already ends with it. Like {@link #stripTrailingSubfolderTag(String)}, the result is built via
     * {@link File#getPath()} to avoid the potential native IO call behind {@link File#getAbsolutePath()}.
     */
    public static String applySubfolderByPackage(final String folder, final boolean subfolderByPackage) {
        final String cleaned = stripTrailingSubfolderTag(folder);
        if (subfolderByPackage) {
            if (StringUtils.isEmpty(cleaned)) {
                return cleaned;
            }
            return new File(cleaned, PackagizerController.PACKAGETAG).getPath();
        } else {
            return cleaned;
        }
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
    }

    public void setDisplayCheckboxMergeWithSameNamedPackages(boolean display) {
        this.displayCheckboxMergeWithSameNamedPackages = display;
    }

    public String getPreSetPackageName() {
        return preSetPackageName;
    }

    public void setPreSetPackageName(String preSetPackageName) {
        this.preSetPackageName = preSetPackageName;
    }
}
