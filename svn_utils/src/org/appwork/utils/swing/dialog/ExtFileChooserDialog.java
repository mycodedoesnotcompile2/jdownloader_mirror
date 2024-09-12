/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.swing.dialog;

import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicReference;

import javax.swing.AbstractAction;
import javax.swing.DefaultListSelectionModel;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JRootPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.KeyStroke;
import javax.swing.ListCellRenderer;
import javax.swing.ListModel;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.filechooser.FileFilter;
import javax.swing.plaf.FileChooserUI;
import javax.swing.plaf.basic.BasicDirectoryModel;
import javax.swing.plaf.basic.BasicFileChooserUI;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

import org.appwork.resources.AWUTheme;
import org.appwork.storage.config.JsonConfig;
import org.appwork.sunwrapper.sun.awt.shell.ShellFolderWrapper;
import org.appwork.swing.components.searchcombo.SearchComboBox;
import org.appwork.utils.Application;
import org.appwork.utils.Files;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.StringUtils;
import org.appwork.utils.locale._AWU;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.SwingUtils;
import org.appwork.utils.swing.dialog.dimensor.RememberLastDialogDimension;

public class ExtFileChooserDialog extends AbstractDialog<File[]> {
    /**
     * @author DESKTOP-7KP0VLD$
     * @date 09.12.2020
     *
     */
    private final class FileChooserFileSystemView extends ModdedJFileChooser {
        private Insets  nullInsets;
        private boolean initComplete = false;
        {
            initComplete = true;
            nullInsets = new Insets(0, 0, 0, 0);
        }

        /**
         * @param extFileSystemView
         */
        private FileChooserFileSystemView(ExtFileSystemView extFileSystemView) {
            super(extFileSystemView);
        }

        /*
         * (non-Javadoc)
         *
         * @see javax.swing.JFileChooser#getCurrentDirectory()
         */
        @Override
        public File getCurrentDirectory() {
            final File file = super.getCurrentDirectory();
            if (file == null) {
                return null;
            } else {
                return getShellFolder(file);
            }
        }

        private File getShellFolder(File file) {
            try {
                return file != null ? ShellFolderWrapper.getShellFolder(file) : null;
            } catch (FileNotFoundException e) {
                return file;
            }
        }

        @Override
        public void addPropertyChangeListener(PropertyChangeListener listener) {
            if (listener instanceof BasicDirectoryModel && directoryModel == null) {
                // this is a workaround to avoid multiple init scans of the
                // filedirectory during the filechooser setup.
                directoryModel = listener;
                return;
            }
            super.addPropertyChangeListener(listener);
        }

        @Override
        public Icon getIcon(final File f) {
            Icon ret = ExtFileChooserDialog.this.getDirectoryIcon(f);
            if (ret == null) {
                ret = super.getIcon(f);
            }
            return ret;
        }

        @Override
        public Insets getInsets() {
            return nullInsets;
        }

        private void setCurrentDirectoryInternal(final File dir) {
            final boolean oldSelecting = selecting;
            selecting = true;
            try {
                if (dir == fileSystemView.getNetworkFolder()) {
                    okButton.setEnabled(false);
                } else {
                    okButton.setEnabled(true);
                }
                setSelectedFile(null);
                super.setCurrentDirectory(getShellFolder(dir));
            } finally {
                selecting = oldSelecting;
            }
        }

        @Override
        public void setCurrentDirectory(final File dir) {
            if (!initComplete) {
                return;
            }
            if (duringInit) {
                // synch during init. else preselection will fail
                setCurrentDirectoryInternal(dir);
            } else {
                // if we select a new directory in the combobox, this action
                // will call this method. We should do this asynchron in
                // order to give the combobox a chance to close itself.
                // else the combobox may stay open until this call finished.
                // This call my take it's time especially for network
                // folders
                SwingUtilities.invokeLater(new Runnable() {
                    public void run() {
                        setCurrentDirectoryInternal(dir);
                    }
                });
            }
        }

        @Override
        public void setSelectedFile(final File file) {
            final boolean oldSelecting = selecting;
            selecting = true;
            try {
                super.setSelectedFile(getShellFolder(file));
            } finally {
                selecting = oldSelecting;
            }
        }

        @Override
        public void updateUI() {
            // UIManager.put("FileChooser.lookInLabelText",
            // _AWU.T.DIALOG_FILECHOOSER_lookInLabelText());
            // UIManager.put("FileChooser.saveInLabelText",
            // _AWU.T.DIALOG_FILECHOOSER_saveInLabelText());
            putClientProperty("FileChooser.useShellFolder", Boolean.FALSE);
            super.updateUI();
            final FileChooserUI myUi = getUI();
            if (myUi instanceof BasicFileChooserUI) {
                ((BasicFileChooserUI) myUi).getModel().addPropertyChangeListener(new PropertyChangeListener() {
                    @Override
                    public void propertyChange(final PropertyChangeEvent evt) {
                        if ("busy".equals(evt.getPropertyName())) {
                            setBusy(((Boolean) evt.getNewValue()).booleanValue());
                        }
                    }
                });
            }
        }
    }

    static {
        UIManager.put("FileChooser.lookInLabelText", _AWU.T.DIALOG_FILECHOOSER_lookInLabelText());
        UIManager.put("FileChooser.saveInLabelText", _AWU.T.DIALOG_FILECHOOSER_saveInLabelText());
        UIManager.put("FileChooser.fileNameLabelText", _AWU.T.DIALOG_FILECHOOSER_fileNameLabelText());
        UIManager.put("FileChooser.folderNameLabelText", _AWU.T.DIALOG_FILECHOOSER_folderNameLabelText());
        UIManager.put("FileChooser.filesOfTypeLabelText", _AWU.T.DIALOG_FILECHOOSER_filesOfTypeLabelText());
        UIManager.put("FileChooser.upFolderToolTipText", _AWU.T.DIALOG_FILECHOOSER_TOOLTIP_UPFOLDER());
        UIManager.put("FileChooser.upFolderAccessibleName", _AWU.T.DIALOG_FILECHOOSER_upFolderAccessibleName());
        UIManager.put("FileChooser.homeFolderToolTipText", _AWU.T.DIALOG_FILECHOOSER_TOOLTIP_HOMEFOLDER());
        UIManager.put("FileChooser.homeFolderAccessibleName", _AWU.T.DIALOG_FILECHOOSER_homeFolderAccessibleName());
        UIManager.put("FileChooser.newFolderToolTipText", _AWU.T.DIALOG_FILECHOOSER_TOOLTIP_NEWFOLDER());
        UIManager.put("FileChooser.newFolderAccessibleName", _AWU.T.DIALOG_FILECHOOSER_newFolderAccessibleName());
        UIManager.put("FileChooser.listViewButtonToolTipText", _AWU.T.DIALOG_FILECHOOSER_TOOLTIP_LIST());
        UIManager.put("FileChooser.listViewButtonAccessibleName", _AWU.T.DIALOG_FILECHOOSER_listViewButtonAccessibleName());
        UIManager.put("FileChooser.detailsViewButtonToolTipText", _AWU.T.DIALOG_FILECHOOSER_TOOLTIP_DETAILS());
        UIManager.put("FileChooser.detailsViewButtonAccessibleName", _AWU.T.DIALOG_FILECHOOSER_detailsViewButtonAccessibleName());
        UIManager.put("FileChooser.newFolderErrorText", _AWU.T.DIALOG_FILECHOOSER_newFolderErrorText());
        UIManager.put("FileChooser.newFolderErrorSeparator", _AWU.T.DIALOG_FILECHOOSER_newFolderErrorSeparator());
        UIManager.put("FileChooser.newFolderParentDoesntExistTitleText", _AWU.T.DIALOG_FILECHOOSER_newFolderParentDoesntExistTitleText());
        UIManager.put("FileChooser.newFolderParentDoesntExistText", _AWU.T.DIALOG_FILECHOOSER_newFolderParentDoesntExistText());
        UIManager.put("FileChooser.fileDescriptionText", _AWU.T.DIALOG_FILECHOOSER_fileDescriptionText());
        UIManager.put("FileChooser.directoryDescriptionText", _AWU.T.DIALOG_FILECHOOSER_directoryDescriptionText());
        UIManager.put("FileChooser.saveButtonText", _AWU.T.DIALOG_FILECHOOSER_saveButtonText());
        UIManager.put("FileChooser.openButtonText", _AWU.T.DIALOG_FILECHOOSER_openButtonText());
        UIManager.put("FileChooser.saveDialogTitleText", _AWU.T.DIALOG_FILECHOOSER_saveDialogTitleText());
        UIManager.put("FileChooser.openDialogTitleText", _AWU.T.DIALOG_FILECHOOSER_openDialogTitleText());
        UIManager.put("FileChooser.cancelButtonText", _AWU.T.DIALOG_FILECHOOSER_cancelButtonText());
        UIManager.put("FileChooser.updateButtonText", _AWU.T.DIALOG_FILECHOOSER_updateButtonText());
        UIManager.put("FileChooser.helpButtonText", _AWU.T.DIALOG_FILECHOOSER_helpButtonText());
        UIManager.put("FileChooser.directoryOpenButtonText", _AWU.T.DIALOG_FILECHOOSER_directoryOpenButtonText());
        UIManager.put("FileChooser.saveButtonToolTipText", _AWU.T.DIALOG_FILECHOOSER_saveButtonToolTipText());
        UIManager.put("FileChooser.openButtonToolTipText", _AWU.T.DIALOG_FILECHOOSER_openButtonToolTipText());
        UIManager.put("FileChooser.cancelButtonToolTipText", _AWU.T.DIALOG_FILECHOOSER_cancelButtonToolTipText());
        UIManager.put("FileChooser.updateButtonToolTipText", _AWU.T.DIALOG_FILECHOOSER_updateButtonToolTipText());
        UIManager.put("FileChooser.helpButtonToolTipText", _AWU.T.DIALOG_FILECHOOSER_helpButtonToolTipText());
        UIManager.put("FileChooser.directoryOpenButtonToolTipText", _AWU.T.DIALOG_FILECHOOSER_directoryOpenButtonToolTipText());
    }
    /**
     *
     */
    public static final String       LASTSELECTION          = "LASTSELECTION_";
    private final static Cursor      BUSY_CURSOR            = Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR);
    private final static Cursor      DEFAULT_CURSOR         = Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR);
    /**
     *
     */
    public static final String       FILECHOOSER            = "FILECHOOSER";
    public static String             FILE_SYSTEM_VIEW_CLASS = "FILECHOOSER_FILE_SYSTEM_VIEW_CLASS";
    private FileChooserSelectionMode fileSelectionMode      = FileChooserSelectionMode.FILES_AND_DIRECTORIES;
    private FileFilter[]             fileFilter;
    private boolean                  multiSelection         = false;
    private File                     preSelection;
    protected ModdedJFileChooser     fc;
    private BasicFileChooserUI       fcUI;
    private java.util.List<String>   quickSelectionList;
    protected boolean                selecting;
    private SearchComboBox<String>   destination;
    private ExtFileSystemView        fileSystemView;
    private Component                parentGlassPane;
    protected View                   view                   = View.DETAILS;
    private FileChooserType          type                   = FileChooserType.SAVE_DIALOG;
    private String                   storageID;
    private boolean                  duringInit;
    protected PropertyChangeListener directoryModel;
    private File[]                   selection              = null;
    private boolean                  busy;

    /**
     * @param flag
     * @param title
     * @param icon
     * @param okOption
     * @param cancelOption
     */
    public ExtFileChooserDialog(final int flag, final String title, final String okOption, final String cancelOption) {
        super(flag | Dialog.STYLE_HIDE_ICON, title, null, okOption, cancelOption);
        setDimensor(new RememberLastDialogDimension("ExtFileChooserDialog"));
        // TODO Auto-generated constructor stub
    }

    @Override
    protected void _init() {
        super._init();
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        if (e.getSource() == okButton) {
            getLogger().fine("Answer: Button<OK:" + okButton.getText() + ">");
            if (fcUI != null) {
                fcUI.getApproveSelectionAction().actionPerformed(e);
            } else {
                setReturnmask(true);
            }
        } else if (e.getSource() == cancelButton) {
            getLogger().fine("Answer: Button<CANCEL:" + cancelButton.getText() + ">");
            setReturnmask(false);
        }
        dispose();
    }

    @Override
    public void dispose() {
        previousTableColumnModelReference.set(null);
        super.dispose();
    }

    /**
     * @param oldTextField
     *
     */
    protected void auto(final JTextField oldTextField) {
        final String txt = oldTextField.getText();
        final int selstart = oldTextField.getSelectionStart();
        final int selend = oldTextField.getSelectionEnd();
        if (selend != txt.length()) {
            return;
        }
        final String sel = txt.substring(selstart, selend);
        final String bef = txt.substring(0, selstart);
        final String name = bef.endsWith("/") || bef.endsWith("\\") ? "" : new File(bef).getName();
        final String findName = txt.endsWith("/") || txt.endsWith("\\") ? "" : new File(txt).getName();
        boolean found = sel.length() == 0;
        for (final File f : fc.getCurrentDirectory().listFiles()) {
            if (fc.getFileFilter() != null && !fc.getFileFilter().accept(f)) {
                continue;
            }
            if (fc.getFileSelectionMode() == JFileChooser.FILES_ONLY && f.isDirectory()) {
                continue;
            }
            if (fc.getFileSelectionMode() == JFileChooser.DIRECTORIES_ONLY && !f.isDirectory()) {
                continue;
            }
            if (f.isHidden() && fc.isFileHidingEnabled()) {
                continue;
            }
            if (this.equals(f.getName(), findName)) {
                found = true;
                continue;
            }
            if (found && startsWith(f.getName(), name)) {
                final boolean oldSelecting = selecting;
                selecting = true;
                oldTextField.setText(f.getAbsolutePath());
                oldTextField.setSelectionStart(selstart);
                oldTextField.setSelectionEnd(oldTextField.getText().length());
                selecting = oldSelecting;
                return;
            }
        }
        final boolean oldSelecting = selecting;
        selecting = true;
        oldTextField.setText(bef);
        selecting = oldSelecting;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.dialog.AbstractDialog#createReturnValue()
     */
    @Override
    protected File[] createReturnValue() {
        try {
            if (isMultiSelection()) {
                final File[] files = fc.getSelectedFiles();
                return files;
            } else {
                File f = fc.getSelectedFile();
                if (f == null) {
                    final String path = getText();
                    if (path != null && isAllowedPath(path)) {
                        f = new File(path);
                    } else {
                        switch (getFileSelectionMode()) {
                        case DIRECTORIES_ONLY:
                        case FILES_AND_DIRECTORIES:
                            f = fc.getCurrentDirectory();
                            if (f != null) {
                                return new File[] { f };
                            }
                        }
                        return null;
                    }
                }
                return new File[] { f };
            }
        } finally {
            // try {
            // getIDConfig().setLastSelection(fc.getCurrentDirectory().getAbsolutePath());
            // } catch (final Exception e) {
            // // may throw nullpointers
            // }
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.dialog.AbstractDialog#setDisposed(boolean)
     */
    @Override
    protected void setDisposed(final boolean b) {
        if (b) {
            try {
                final File[] files = createReturnValue();
                if (files != null && files.length > 0) {
                    File file = files[0];
                    if (file.isFile()) {
                        file = file.getParentFile();
                    }
                    getIDConfig().setLastSelection(file.getAbsolutePath());
                }
            } catch (final Exception e) {
                // may throw nullpointers
            }
        }
        super.setDisposed(b);
    }

    /**
     * @param path
     * @return
     */
    protected boolean isAllowedPath(final String path) {
        if (path.equals(ExtFileSystemView.VIRTUAL_NETWORKFOLDER_XP)) {
            return false;
        } else if (path.equals(ExtFileSystemView.VIRTUAL_NETWORKFOLDER)) {
            return false;
        } else if (path.equals(_AWU.T.DIALOG_FILECHOOSER_networkfolder())) {
            return false;
        } else {
            return true;
        }
    }

    /**
     * @param name
     * @param findName
     * @return
     */
    private boolean equals(final String name, final String findName) {
        if (CrossSystem.isWindows()) {
            return name.equalsIgnoreCase(findName);
        } else {
            return name.equals(findName);
        }
    }

    protected boolean exists(final File f) {
        if (f.exists()) {
            return true;
        } else if (isSambaFolder(f)) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * @param ret
     * @param f
     * @return
     */
    protected Icon getDirectoryIcon(File f) {
        if (isFile(f)) {
            try {
                final String ext = Files.getExtension(f.getName(), true);
                if (ext == null) {
                    return null;
                }
                return CrossSystem.getMime().getFileIcon(ext, 16, 16);
            } catch (final Exception e) {
                return null;
            }
        }
        String key = null;
        if (f instanceof NetWorkFolder) {
            // NetworkFolder in Map->very very bad idea :)will trigger network calls
            key = ExtFileChooserDialogIcon.FILECHOOSER_NETWORK.path();
        } else {
            f = fileSystemView.mapSpecialFolders(f);
            key = ExtFileChooserDialogIcon.FILECHOOSER_FOLDER.path();
            if (f.getName().equals("Desktop")) {
                key = ExtFileChooserDialogIcon.FILECHOOSER_DESKTOP.path();
            } else if (f.getPath().equals(ExtFileSystemView.VIRTUAL_NETWORKFOLDER_XP) || f.getPath().equals(ExtFileSystemView.VIRTUAL_NETWORKFOLDER) || f.getPath().startsWith("\\") && f.getPath().indexOf("\\", 2) < 0) {
                key = ExtFileChooserDialogIcon.FILECHOOSER_NETWORK.path();
            } else if (f.getPath().length() == 3 && f.getPath().charAt(1) == ':' && (f.getPath().charAt(0) + "").matches("[a-zA-Z]{1}")) {
                key = ExtFileChooserDialogIcon.FILECHOOSER_HARDDRIVE.path();
            } else if (f instanceof HomeFolder) {
                key = ((HomeFolder) f).getIconKey();
            } else if (f instanceof VirtualRoot) {
                final Icon ret = ((VirtualRoot) f).getIcon(18);
                if (ret != null) {
                    return ret;
                }
            }
        }
        if (AWUTheme.I().hasIcon(key)) {
            return AWUTheme.I().getIcon(key, 18);
        } else {
            return null;
        }
    }

    /**
     * @param f
     * @return
     */
    private boolean isFile(final File f) {
        if (CrossSystem.isWindows()) {
            // let's try to speed up this. windows may take a log of time for
            // f.isFile if the file is a network folder
            if (f instanceof NetWorkFolder) {
                return false;
            }
            // example c:\
            if (StringUtils.isEmpty(f.getName())) {
                return false;
            }
        }
        return f.isFile();
    }

    public FileFilter[] getFileFilter() {
        return fileFilter;
    }

    protected boolean useAcceptAllFileFilter = true;

    public boolean isAcceptAllFileFilterUsed() {
        return useAcceptAllFileFilter;
    }

    public void setAcceptAllFileFilterUsed(boolean b) {
        this.useAcceptAllFileFilter = b;
    }

    public FileChooserSelectionMode getFileSelectionMode() {
        return fileSelectionMode;
    }

    private ExtFileChooserIdConfig configCache = null;

    /**
     * @return
     */
    protected ExtFileChooserIdConfig getIDConfig() {
        final File path = Application.getResource("cfg/FileChooser/" + getStorageID());
        ExtFileChooserIdConfig configCache = this.configCache;
        if (configCache != null && path.equals(configCache._getStorageHandler().getPath())) {
            return configCache;
        }
        path.getParentFile().mkdirs();
        configCache = JsonConfig.create(path, ExtFileChooserIdConfig.class);
        this.configCache = configCache;
        return configCache;
    }

    public File getPreSelection() {
        return preSelection;
    }

    public java.util.List<String> getQuickSelectionList() {
        return quickSelectionList;
    }

    /**
     * @return
     */
    public File getSelectedFile() {
        if (isMultiSelection()) {
            throw new IllegalStateException("Not available if multiselection is active. use #getSelection() instead");
        }
        final File[] sel = getSelection();
        return sel == null || sel.length == 0 ? null : sel[0];
    }

    /**
     * @return
     */
    public File[] getSelection() {
        if (selection != null) {
            return selection;
        }
        return createReturnValue();
    }

    public String getStorageID() {
        return storageID;
    }

    public String getText() {
        try {
            return destination != null ? destination.getText() : fc.getSelectedFile().getAbsolutePath();
        } catch (final Throwable e) {
            return null;
        }
    }

    /**
     * @return
     */
    public FileChooserType getType() {
        return type;
    }

    /**
     * @return
     */
    public View getView() {
        return view;
    }

    /**
     * @return
     */
    protected boolean isFilePreviewEnabled() {
        return getFileSelectionMode() != FileChooserSelectionMode.DIRECTORIES_ONLY;
    }

    public boolean isMultiSelection() {
        return multiSelection;
    }

    private void setBusy(final boolean newValue) {
        if (busy == newValue) {
            return;
        }
        if (newValue) {
            // System.out.println("Busy TRUE");
            if (parentGlassPane != null) {
                parentGlassPane.setCursor(BUSY_CURSOR);
                parentGlassPane.setVisible(true);
            }
        } else {
            // System.out.println("Busy FALSE");
            if (parentGlassPane != null) {
                parentGlassPane.setCursor(null);
                parentGlassPane.setVisible(false);
            }
        }
        busy = newValue;
    }

    /**
     * @param f
     * @return
     */
    protected boolean isSambaFolder(final File f) {
        if (fileSystemView.getNetworkFolder() != null) {
            return fileSystemView.getNetworkFolder().get(f.getAbsolutePath()) != null;
        }
        return false;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.dialog.AbstractDialog#layoutDialogContent()
     */
    @Override
    public JComponent layoutDialogContent() {
        final long t = System.currentTimeMillis();
        System.out.println("Duration 0  " + (System.currentTimeMillis() - t));
        if (SwingUtilities.getRootPane(getDialog().getParent()) != null) {
            final JRootPane root2 = SwingUtilities.getRootPane(getDialog());
            parentGlassPane = root2.getGlassPane();
        }
        if (parentGlassPane != null) {
            parentGlassPane.setCursor(ExtFileChooserDialog.BUSY_CURSOR);
            parentGlassPane.setVisible(true);
        }
        // forwardPropertyChanges=false;
        duringInit = true;
        putIcons();
        System.out.println("Duration 1  " + (System.currentTimeMillis() - t));
        fc = new FileChooserFileSystemView(fileSystemView = createFileSystemView());
        cleanupIcons();
        fc.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                if (JFileChooser.CANCEL_SELECTION.equals(e.getActionCommand())) {
                    getLogger().fine("Answer: FC CANCEL>");
                    // this is called indirectly through #setReturnmask(false).
                    // so we use super here to avoid a loop
                    ExtFileChooserDialog.super.setReturnmask(false);
                    //
                    ExtFileChooserDialog.this.dispose();
                } else if (JFileChooser.APPROVE_SELECTION.equals(e.getActionCommand())) {
                    getLogger().fine("Answer: FC APPROVE>");
                    ExtFileChooserDialog.this.setReturnmask(true);
                    ExtFileChooserDialog.this.dispose();
                }
            }
        });
        try {
            fcUI = (BasicFileChooserUI) fc.getUI();
        } catch (final Throwable e) {
            getLogger().log(e);
        }
        if (isFilePreviewEnabled()) {
            fc.setAccessory(new FilePreview(fc));
        }
        fc.setControlButtonsAreShown(false);
        fc.setDialogType(getType().getId());
        if (fileSelectionMode != null) {
            fc.setFileSelectionMode(fileSelectionMode.getId());
        }
        if (fileFilter != null) {
            final boolean isAcceptAllFileFilterUsed = isAcceptAllFileFilterUsed();
            fc.setAcceptAllFileFilterUsed(isAcceptAllFileFilterUsed);
            final FileFilter allFileFilter = fc.getAcceptAllFileFilter();
            if (allFileFilter != null) {
                fc.removeChoosableFileFilter(allFileFilter);
            }
            if (fileFilter.length == 1) {
                fc.setFileFilter(fileFilter[0]);
            } else {
                for (FileFilter filter : fileFilter) {
                    if (filter != null) {
                        fc.addChoosableFileFilter(filter);
                    }
                }
            }
            if (isAcceptAllFileFilterUsed && allFileFilter != null) {
                fc.addChoosableFileFilter(allFileFilter);
            }
        }
        if (multiSelection) {
            fc.setMultiSelectionEnabled(true);
        } else {
            fc.setMultiSelectionEnabled(false);
        }
        /* preSelection */
        File presel = preSelection;
        if (presel != null && StringUtils.isEmpty(presel.getName())) {
            // find and eliminate file.filePath=null file objects.
            presel = null;
        }
        getLogger().info("Given presel: " + presel);
        if (presel == null) {
            final String path = getIDConfig().getLastSelection();
            presel = StringUtils.isEmpty(path) ? null : new File(path);
        } else if (presel.getParentFile() == null) {
            // mix stoerage with new filename
            final String path = getIDConfig().getLastSelection();
            File preselFromStorage = StringUtils.isEmpty(path) ? null : new File(path);
            if (preselFromStorage != null) {
                presel = new File(preselFromStorage.getParentFile(), presel.getName());
            }
        }
        if (presel == null) {
            presel = new File(System.getProperty("user.home"), "documents");
        }
        final File orgPresel = presel;
        while (presel != null) {
            if (!presel.exists()) {
                /* file does not exist, try ParentFile */
                presel = presel.getParentFile();
            } else {
                if (presel.isDirectory()) {
                    fc.setCurrentDirectory(presel);
                    /*
                     * we have to setSelectedFile here too, so the folder is preselected
                     */
                } else {
                    fc.setCurrentDirectory(presel.getParentFile());
                    /* only preselect file in savedialog */
                    if (fileSelectionMode != null) {
                        if (fileSelectionMode.getId() == FileChooserSelectionMode.DIRECTORIES_ONLY.getId()) {
                            fc.setSelectedFile(presel.getParentFile());
                        } else {
                            fc.setSelectedFile(presel);
                        }
                    }
                }
                break;
            }
        }
        if (orgPresel != null && !orgPresel.isDirectory()) {
            if (fc.getSelectedFile() == null || !fc.getSelectedFile().equals(orgPresel)) {
                fc.setSelectedFile(new File(fc.getCurrentDirectory(), orgPresel.getName()));
            }
        }
        updateView();
        try {
            final JToggleButton detailsButton = (JToggleButton) SwingUtils.getParent(fc, 0, 0, 7);
            detailsButton.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(final ActionEvent e) {
                    view = View.DETAILS;
                }
            });
        } catch (final ArrayIndexOutOfBoundsException ignore) {
            // might throw exceptions, because the path, and the whole
            // detailsview thingy is part of the ui/LAF
        } catch (final Throwable e) {
            getLogger().log(e);
        }
        try {
            final JToggleButton listButton = (JToggleButton) SwingUtils.getParent(fc, 0, 0, 6);
            listButton.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(final ActionEvent e) {
                    view = View.LIST;
                }
            });
        } catch (final ArrayIndexOutOfBoundsException ignore) {
            // might throw exceptions, because the path, and the whole
            // detailsview thingy is part of the ui/LAF
        } catch (final Throwable e) {
            getLogger().log(e);
        }
        final JComponent fcContainer = getFileChooserContainer(fc);
        if (fcContainer != null) {
            if (fileSelectionMode.getId() == FileChooserSelectionMode.DIRECTORIES_ONLY.getId() && fcContainer.getComponentCount() >= 4) {
                ((JComponent) fcContainer.getComponent(3)).getComponent(1).setVisible(false);
                ((JComponent) fcContainer.getComponent(3)).getComponent(2).setVisible(false);
            }
            if (quickSelectionList != null && multiSelection == false && fcContainer.getComponentCount() >= 4) {
                try {
                    // wraps the textfield to enter a path in a SearchCombobox
                    // FilePane filepane = (sun.swing.FilePane)fc.getComponent(2);
                    final JPanel namePanel = (JPanel) ((JComponent) fcContainer.getComponent(3)).getComponent(0);
                    final JTextField oldTextField = (JTextField) namePanel.getComponent(1);
                    namePanel.remove(1);
                    final String text = oldTextField.getText();
                    destination = new SearchComboBox<String>() {
                        @Override
                        public JTextField createTextField() {
                            return oldTextField;
                        }

                        @Override
                        protected Icon getIconForValue(final String value) {
                            return null;
                        }

                        @Override
                        protected String getTextForValue(final String value) {
                            return value;
                        }

                        @Override
                        public boolean isAutoCompletionEnabled() {
                            return false;
                        }

                        @Override
                        public void onChanged() {
                            if (selecting) {
                                SwingUtilities.invokeLater(new Runnable() {
                                    @Override
                                    public void run() {
                                        final String txt = getText();
                                        updateButtonsAndText(txt);
                                    }
                                });
                                return;
                            }
                            final String txt = getText();
                            if (CrossSystem.getOSFamily() == org.appwork.utils.os.CrossSystem.OSFamily.WINDOWS && txt.length() <= 2 && !txt.startsWith("\\")) {
                                // c:
                                // g: but not c:\ and not \\nas
                                // problem: under windows, the filesystem converts
                                // c: to <current_process_dir> if the root of
                                // current process dir is c.
                                // result: if the current dir is c:\ and the user
                                // presses backspace, the new dir becomes for
                                // example c:\program files\JDownloader instead of
                                // c:
                                // solution: break the auto completion for this
                                // special case
                                return;
                            }
                            SwingUtilities.invokeLater(new Runnable() {
                                private File getFile(final String txt) {
                                    if (fileSystemView.getNetworkFolder() != null && "\\".equals(txt)) {
                                        return fileSystemView.getNetworkFolder();
                                    }
                                    File ret = null;
                                    if (fileSystemView.getNetworkFolder() != null) {
                                        ret = fileSystemView.getNetworkFolder().get(new File(txt).getAbsolutePath());
                                    }
                                    return ret != null ? ret : new File(txt);
                                }

                                @Override
                                public void run() {
                                    final String orgText = getText();
                                    String curText = orgText;
                                    try {
                                        File f = getFile(curText);
                                        boolean parent = false;
                                        while (f != null && f.getParentFile() != f) {
                                            curText = getText();
                                            if (ExtFileChooserDialog.this.exists(f)) {
                                                final String finalTxt = curText;
                                                final PropertyChangeListener listener = new PropertyChangeListener() {
                                                    @Override
                                                    public void propertyChange(PropertyChangeEvent evt) {
                                                        if (JFileChooser.DIRECTORY_CHANGED_PROPERTY.equals(evt.getPropertyName()) || JFileChooser.SELECTED_FILE_CHANGED_PROPERTY.equals(evt.getPropertyName())) {
                                                            final boolean setText;
                                                            if (JFileChooser.DIRECTORY_CHANGED_PROPERTY.equals(evt.getPropertyName())) {
                                                                setText = evt.getOldValue() == null || (evt.getNewValue().toString().length() < evt.getOldValue().toString().length());
                                                            } else {
                                                                setText = !StringUtils.equals(orgText, finalTxt);
                                                            }
                                                            if (setText) {
                                                                SwingUtilities.invokeLater(new Runnable() {
                                                                    @Override
                                                                    public void run() {
                                                                        final boolean oldSelecting = selecting;
                                                                        selecting = true;
                                                                        setText(finalTxt);
                                                                        selecting = oldSelecting;
                                                                    }
                                                                });
                                                            }
                                                        }
                                                    }
                                                };
                                                try {
                                                    fc.addPropertyChangeListener(listener);
                                                    if (f.isFile()) {
                                                        fc.setSelectedFile(f);
                                                    } else if (f.getParentFile() == null || !f.getParentFile().exists() || parent || f.isDirectory()) {
                                                        fc.setCurrentDirectory(f);
                                                        fc.setSelectedFile(null);
                                                    }
                                                } finally {
                                                    SwingUtilities.invokeLater(new Runnable() {
                                                        @Override
                                                        public void run() {
                                                            fc.removePropertyChangeListener(listener);
                                                        }
                                                    });
                                                }
                                                return;
                                            } else {
                                                parent = true;
                                                f = f.getParentFile();
                                            }
                                        }
                                    } finally {
                                        updateButtonsAndText(curText);
                                    }
                                }
                            });
                        }

                        protected void updateButtonsAndText(final String txt) {
                            if (txt.equals(ExtFileSystemView.VIRTUAL_NETWORKFOLDER_XP) || txt.equals(ExtFileSystemView.VIRTUAL_NETWORKFOLDER)) {
                                destination.getTextField().setText(_AWU.T.DIALOG_FILECHOOSER_networkfolder());
                                ExtFileChooserDialog.this.okButton.setEnabled(false);
                                return;
                            }
                            final File[] ret = ExtFileChooserDialog.this.createReturnValue();
                            if (ret == null || ret.length == 0) {
                                ExtFileChooserDialog.this.okButton.setEnabled(false);
                            } else {
                                ExtFileChooserDialog.this.okButton.setEnabled(true);
                            }
                        }
                    };
                    // this code makes enter leave the dialog.
                    destination.getTextField().getInputMap().put(KeyStroke.getKeyStroke("ENTER"), "approveSelection");
                    destination.getTextField().getInputMap().put(KeyStroke.getKeyStroke("pressed TAB"), "auto");
                    destination.getTextField().setFocusTraversalKeysEnabled(false);
                    destination.setActionMap(fc.getActionMap());
                    destination.getTextField().getActionMap().put("auto", new AbstractAction() {
                        @Override
                        public void actionPerformed(final ActionEvent e) {
                            ExtFileChooserDialog.this.auto(oldTextField);
                        }
                    });
                    destination.setUnkownTextInputAllowed(true);
                    destination.setBadColor(null);
                    destination.setSelectedItem(null);
                    destination.setList(quickSelectionList);
                    destination.setText(text);
                    if (orgPresel != null && orgPresel.exists()) {
                        if (orgPresel.isDirectory()) {
                            if (orgPresel.getAbsolutePath().endsWith(File.separatorChar + "")) {
                                destination.setText(orgPresel.getAbsolutePath());
                            } else {
                                destination.setText(orgPresel.getAbsolutePath() + File.separatorChar);
                            }
                        } else {
                            destination.setText(orgPresel.getAbsolutePath());
                        }
                    }
                    namePanel.add(destination);
                    modifiyNamePanel(namePanel);
                    // SwingUtils.printComponentTree(fc);
                    // [2][0][0][0][0]
                    JComponent c = (JComponent) fcContainer.getComponent(2);
                    c = (JComponent) c.getComponent(0);
                    c = (JComponent) c.getComponent(0);
                    c = (JComponent) c.getComponent(0);
                    // sun.swing.FilePane
                    // this is only a list in list view. else a jtable or something
                    // else
                    c = (JComponent) c.getComponent(0);
                    if (c instanceof JList) {
                        final JList list = (JList) c;
                        list.addMouseListener(new MouseAdapter() {
                            // mouselistener sets directory back if we click in
                            // empty
                            // list spaces
                            public int loc2IndexFileList(final JList jlist, final Point point) {
                                int i = jlist.locationToIndex(point);
                                if (i != -1) {
                                    if (!pointIsInActualBounds(jlist, i, point)) {
                                        i = -1;
                                    }
                                }
                                return i;
                            }

                            @Override
                            public void mouseClicked(final MouseEvent e) {
                                final int index = loc2IndexFileList(list, e.getPoint());
                                if (index < 0) {
                                    final File dir = fc.getSelectedFile();
                                    if (dir != null) {
                                        destination.setText(dir.getParent() + File.separator);
                                        final ListSelectionModel listSelectionModel = list.getSelectionModel();
                                        if (listSelectionModel != null) {
                                            listSelectionModel.clearSelection();
                                            ((DefaultListSelectionModel) listSelectionModel).moveLeadSelectionIndex(0);
                                            listSelectionModel.setAnchorSelectionIndex(0);
                                        }
                                    }
                                }
                            }

                            private boolean pointIsInActualBounds(final JList jlist, final int i, final Point point) {
                                final ListCellRenderer listcellrenderer = jlist.getCellRenderer();
                                final ListModel listmodel = jlist.getModel();
                                final Object obj = listmodel.getElementAt(i);
                                final Component component = listcellrenderer.getListCellRendererComponent(jlist, obj, i, false, false);
                                final Dimension dimension = component.getPreferredSize();
                                final Rectangle rectangle = jlist.getCellBounds(i, i);
                                if (!component.getComponentOrientation().isLeftToRight()) {
                                    rectangle.x += rectangle.width - dimension.width;
                                }
                                rectangle.width = dimension.width;
                                return rectangle.contains(point);
                            }
                        });
                    }
                } catch (final Throwable e) {
                    getLogger().log(e);
                }
            }
        }
        if (directoryModel != null) {
            // this is a workaround to avoid multiple init scans of the
            // filedirectory during the filechooser setup.
            fc.addPropertyChangeListener(directoryModel);
            directoryModel.propertyChange(new PropertyChangeEvent(this, JFileChooser.FILE_FILTER_CHANGED_PROPERTY, null, null));
        }
        duringInit = false;
        return fc;
    }

    /**
     * @param fc2
     * @return
     */
    protected JComponent getFileChooserContainer(JComponent fc) {
        for (int index = 0; index < fc.getComponentCount(); index++) {
            final Component c = fc.getComponent(index);
            if (ReflectionUtils.isInstanceOf("sun.swing.FilePane", c) && index == 2) {
                // FilePane at 3rd position (Synthethica, FlatLAT, Metal...)
                return fc;
            }
        }
        for (int index = 0; index < fc.getComponentCount(); index++) {
            final Component c = fc.getComponent(index);
            if (c instanceof JComponent && ((JComponent) c).getComponentCount() > 0) {
                final JComponent ret = getFileChooserContainer((JComponent) c);
                if (ret != null) {
                    return ret;
                }
            }
        }
        return null;
    }

    protected ExtFileSystemView createFileSystemView() {
        try {
            if (StringUtils.isEmpty(FILE_SYSTEM_VIEW_CLASS)) {
                return new ExtFileSystemView();
            } else {
                final Class<?> clazz = Class.forName(FILE_SYSTEM_VIEW_CLASS);
                return (ExtFileSystemView) clazz.newInstance();
            }
        } catch (Exception e) {
            return new ExtFileSystemView();
        }
    }

    /**
     *
     */
    private void putIcons() {
        putIcon("FileView.directoryIcon", ExtFileChooserDialogIcon.FILECHOOSER_FOLDER.path());
        putIcon("FileView.fileIcon", ExtFileChooserDialogIcon.FILECHOOSER_FILE.path());
        putIcon("FileView.computerIcon", ExtFileChooserDialogIcon.FILECHOOSER_COMPUTER.path());
        putIcon("FileView.hardDriveIcon", ExtFileChooserDialogIcon.FILECHOOSER_HARDDRIVE.path());
        putIcon("FileView.floppyDriveIcon", ExtFileChooserDialogIcon.FILECHOOSER_FLOPPY.path());
        //
        putIcon("FileChooser.newFolderIcon", ExtFileChooserDialogIcon.FILECHOOSER_NEW_FOLDER.path());
        putIcon("FileChooser.upFolderIcon", ExtFileChooserDialogIcon.FILECHOOSER_PARENT.path());
        putIcon("FileChooser.homeFolderIcon", ExtFileChooserDialogIcon.FILECHOOSER_DESKTOP.path());
        putIcon("FileChooser.detailsViewIcon", ExtFileChooserDialogIcon.FILECHOOSER_DETAILS_VIEW.path());
        putIcon("FileChooser.listViewIcon", ExtFileChooserDialogIcon.FILECHOOSER_LIST_VIEW.path());
        putIcon("FileChooser.viewMenuIcon", ExtFileChooserDialogIcon.FILECHOOSER_VIEW.path());
    }

    /**
     * @param string
     * @param string2
     */
    private void putIcon(final String key, final String iconKey) {
        if (AWUTheme.I().hasIcon(iconKey)) {
            UIManager.put(key, AWUTheme.I().getIcon(iconKey, 18));
        }
    }

    /**
     *
     */
    private void cleanupIcons() {
        cleanupIcon("FileView.directoryIcon", ExtFileChooserDialogIcon.FILECHOOSER_FOLDER.path());
        cleanupIcon("FileView.fileIcon", ExtFileChooserDialogIcon.FILECHOOSER_FILE.path());
        cleanupIcon("FileView.computerIcon", ExtFileChooserDialogIcon.FILECHOOSER_COMPUTER.path());
        cleanupIcon("FileView.hardDriveIcon", ExtFileChooserDialogIcon.FILECHOOSER_COMPUTER.path());
        cleanupIcon("FileView.floppyDriveIcon", ExtFileChooserDialogIcon.FILECHOOSER_FLOPPY.path());
        //
        cleanupIcon("FileChooser.newFolderIcon", ExtFileChooserDialogIcon.FILECHOOSER_NEW_FOLDER.path());
        cleanupIcon("FileChooser.upFolderIcon", ExtFileChooserDialogIcon.FILECHOOSER_PARENT.path());
        cleanupIcon("FileChooser.homeFolderIcon", ExtFileChooserDialogIcon.FILECHOOSER_DESKTOP.path());
        cleanupIcon("FileChooser.detailsViewIcon", ExtFileChooserDialogIcon.FILECHOOSER_DETAILS_VIEW.path());
        cleanupIcon("FileChooser.listViewIcon", ExtFileChooserDialogIcon.FILECHOOSER_LIST_VIEW.path());
        cleanupIcon("FileChooser.viewMenuIcon", ExtFileChooserDialogIcon.FILECHOOSER_VIEW.path());
    }

    /**
     * @param string
     * @param string2
     */
    private void cleanupIcon(final String key, final String iconKey) {
        if (AWUTheme.I().hasIcon(iconKey)) {
            UIManager.put(key, null);
        }
    }

    /**
     * @param namePanel
     */
    protected void modifiyNamePanel(final JPanel namePanel) {
    }

    @Override
    protected void packed() {
        super.packed();
        if (parentGlassPane != null) {
            parentGlassPane.setCursor(null);
            parentGlassPane.setVisible(false);
        }
    }

    public void setFileFilter(final FileFilter... fileFilter) {
        this.fileFilter = fileFilter;
    }

    public void setFileSelectionMode(final FileChooserSelectionMode fileSelectionMode) {
        this.fileSelectionMode = fileSelectionMode;
    }

    public void setMultiSelection(final boolean multiSelection) {
        this.multiSelection = multiSelection;
    }

    public void setPreSelection(final File preSelection) {
        this.preSelection = preSelection;
    }

    public void setQuickSelectionList(final java.util.List<String> quickSelectionList) {
        this.quickSelectionList = quickSelectionList;
    }

    @Override
    protected void setReturnmask(final boolean b) {
        if (b) {
            // fc.approveSelection();
        } else {
            fc.cancelSelection();
        }
        super.setReturnmask(b);
    }

    public void setSelection(final File[] selection) {
        this.selection = selection;
    }

    /**
     * @param id
     */
    public void setStorageID(final String id) {
        storageID = id;
    }

    public void setType(final FileChooserType type) {
        this.type = type;
    }

    public void setView(View view) {
        if (view == null) {
            view = View.DETAILS;
        }
        this.view = view;
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                ExtFileChooserDialog.this.updateView();
            }
        };
    }

    protected synchronized Map<String, Object> getStorage() {
        final ExtFileChooserIdConfig config = getIDConfig();
        ConcurrentHashMap<String, Object> storage = config.getStorage();
        if (storage == null) {
            storage = new ConcurrentHashMap<String, Object>();
            config.setStorage(storage);
        }
        return storage;
    }

    /**
     * @param name
     * @param name2
     * @return
     */
    private boolean startsWith(final String name, final String name2) {
        if (CrossSystem.isWindows()) {//
            return name.toLowerCase(Locale.ENGLISH).startsWith(name2.toLowerCase(Locale.ENGLISH));
        } else {
            return name.startsWith(name2);
        }
    }

    /**
     *
     */
    private void updateView() {
        if (fc != null) {
            switch (getView()) {
            case DETAILS:
                try {
                    final JToggleButton detailsButton = (JToggleButton) SwingUtils.getParent(fc, 0, 0, 7);
                    detailsButton.doClick();
                } catch (final ArrayIndexOutOfBoundsException ignore) {
                    // might throw exceptions, because the path, and the whole
                    // detailsview thingy is part of the ui/LAF
                } catch (final Throwable t) {
                    getLogger().log(t);
                }
                break;
            case LIST:
                try {
                    final JToggleButton detailsButton = (JToggleButton) SwingUtils.getParent(fc, 0, 0, 6);
                    detailsButton.doClick();
                } catch (final ArrayIndexOutOfBoundsException ignore) {
                    // might throw exceptions, because the path, and the whole
                    // detailsview thingy is part of the ui/LAF
                } catch (final Throwable t) {
                    getLogger().log(t);
                }
                break;
            }
            final Map<JTable, List<Integer>> jtables = SwingUtils.find(fc, null, JTable.class);
            if (jtables != null && jtables.size() == 1) {
                final JTable jtable = jtables.keySet().iterator().next();
                jtable.getModel().addTableModelListener(new TableModelListener() {
                    @Override
                    public void tableChanged(TableModelEvent e) {
                        if (e.getType() == TableModelEvent.UPDATE && e.getColumn() == TableModelEvent.ALL_COLUMNS) {
                            restoreTableColumnModel(jtable);
                        }
                    }
                });
                restoreTableColumnModel(jtable);
            }
        }
    }

    final AtomicReference<TableColumnModel> previousTableColumnModelReference = new AtomicReference<TableColumnModel>();

    protected void restoreTableColumnModel(final JTable jtable) {
        final Map<String, Object> storage = getStorage();
        final TableColumnModel currentTableColumnModel = jtable.getColumnModel();
        final TableColumnModel previousTableColumnModel = previousTableColumnModelReference.getAndSet(currentTableColumnModel);
        if (previousTableColumnModel != null) {
            for (int c = 0; c < previousTableColumnModel.getColumnCount(); c++) {
                final TableColumn col = previousTableColumnModel.getColumn(c);
                storage.put(col.getIdentifier() + "_width", col.getWidth());
                storage.put(col.getIdentifier() + "_pos", c);
            }
        }
        for (int c = 0; c < currentTableColumnModel.getColumnCount(); c++) {
            final TableColumn col = currentTableColumnModel.getColumn(c);
            final Object width = storage.get(col.getIdentifier() + "_width");
            if (width instanceof Number) {
                final int w = ((Number) width).intValue();
                if (w > 0) {
                    col.setWidth(w);
                    col.setPreferredWidth(w);
                }
            }
        }
        for (int c = 0; c < currentTableColumnModel.getColumnCount(); c++) {
            final TableColumn col = currentTableColumnModel.getColumn(c);
            final Object pos = storage.get(col.getIdentifier() + "_pos");
            if (pos instanceof Number) {
                final int p = ((Number) pos).intValue();
                if (p >= 0) {
                    currentTableColumnModel.moveColumn(c, p);
                }
            }
        }
        jtable.setAutoResizeMode(JTable.AUTO_RESIZE_LAST_COLUMN);
    }
}
