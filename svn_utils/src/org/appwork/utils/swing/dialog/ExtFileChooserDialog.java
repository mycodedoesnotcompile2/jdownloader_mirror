/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
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
import java.awt.KeyboardFocusManager;
import java.awt.MouseInfo;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ContainerAdapter;
import java.awt.event.ContainerEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicReference;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.DefaultListSelectionModel;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JList;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
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
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.filechooser.FileFilter;
import javax.swing.plaf.FileChooserUI;
import javax.swing.plaf.basic.BasicDirectoryModel;
import javax.swing.plaf.basic.BasicFileChooserUI;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

import org.appwork.loggingv3.LogV3;
import org.appwork.resources.AWUTheme;
import org.appwork.storage.config.JsonConfig;
import org.appwork.swing.components.searchcombo.SearchComboBox;
import org.appwork.utils.Application;
import org.appwork.utils.Files;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
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
                return fileSystemView.mapSpecialFolders(ExtFileSystemView.getShellFolder(file));
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
                super.setCurrentDirectory(ExtFileSystemView.getShellFolder(dir));
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
                super.setSelectedFile(ExtFileSystemView.getShellFolder(file));
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
            /* required for our ExtFileSystemView.getRoots is getting used, else it is */
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
    // TODO: The default is NOT a class name. createFileSystemView() does Class.forName(FILE_SYSTEM_VIEW_CLASS) for every dialog, which
    // always fails with this value. The exception is swallowed and the fallback "new ExtFileSystemView()" is used. It works by accident,
    // but
    // hides misconfiguration. Probably this should be null/empty (= default) or a real class name.
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
            }
            if (fc.getReturnValueExt() != JFileChooser.ERROR_OPTION) {
                setReturnmask(true);
                dispose();
            } else {
                getLogger().fine("No approval - Filechooser internal state change only");
                // the filechooser got not approved, but change an internal state - like switched the current folder..
            }
        } else if (e.getSource() == cancelButton) {
            getLogger().fine("Answer: Button<CANCEL:" + cancelButton.getText() + ">");
            setReturnmask(false);
            dispose();
        }
    }

    @Override
    public void dispose() {
        // store the column widths/positions the user ended up with. restoreTableColumnModel only stores the PREVIOUS model when the table
        // changes, so the last state would get lost otherwise. Has to happen before super.dispose(), which saves the config.
        final TableColumnModel lastTableColumnModel = previousTableColumnModelReference.getAndSet(null);
        if (lastTableColumnModel != null) {
            try {
                storeTableColumnModel(lastTableColumnModel, getStorage());
            } catch (final Throwable e) {
                getLogger().log(e);
            }
        }
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
        final File currentDirectory = fc.getCurrentDirectory();
        // listFiles() returns null if the directory is not readable or is not a real directory (e.g. network folder)
        final File[] files = currentDirectory == null ? null : currentDirectory.listFiles();
        if (files == null) {
            return;
        }
        for (final File f : files) {
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
                        f = ExtFileSystemView.getShellFolder(new File(path));
                    } else {
                        final FileChooserSelectionMode mode = getFileSelectionMode();
                        if (mode == FileChooserSelectionMode.DIRECTORIES_ONLY || mode == FileChooserSelectionMode.FILES_AND_DIRECTORIES) {
                            f = ExtFileSystemView.getShellFolder(fc.getCurrentDirectory());
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
        // remember the last selection only if the user approved the dialog. Cancel/close must not change it.
        if (b && (getReturnmask() & Dialog.RETURN_OK) != 0) {
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

    protected String getLastSelection() {
        return getIDConfig().getLastSelection();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.dialog.AbstractDialog#layoutDialogContent()
     */
    @Override
    public JComponent layoutDialogContent() {
        boolean success = false;
        try {
            final JComponent ret = layoutDialogContentInternal();
            success = true;
            return ret;
        } finally {
            if (!success) {
                // do not leave the busy cursor/glasspane and the init mode behind if the layout failed
                duringInit = false;
                if (parentGlassPane != null) {
                    parentGlassPane.setCursor(null);
                    parentGlassPane.setVisible(false);
                }
            }
        }
    }

    private JComponent layoutDialogContentInternal() {
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
                } else if (JFileChooser.APPROVE_SELECTION.equals(e.getActionCommand()) || /* FilePane.ACTION_APPROVE_SELECTION */"approveSelection".equals(e.getActionCommand())) {
                    getLogger().fine("Answer: FC APPROVE>");
                    ExtFileChooserDialog.this.setReturnmask(true);
                    ExtFileChooserDialog.this.dispose();
                } else {
                    LogV3.info("Unknown Action Command: " + e.getActionCommand());
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
        File presel = getPreSelection();
        if (presel != null && StringUtils.isEmpty(presel.getName())) {
            // find and eliminate file.filePath=null file objects.
            presel = null;
        }
        getLogger().info("Given presel: " + presel);
        if (presel == null) {
            final String path = getLastSelection();
            presel = StringUtils.isEmpty(path) ? null : new File(path);
        } else if (presel.getParentFile() == null) {
            // mix stoerage with new filename
            final String path = getLastSelection();
            File preselFromStorage = StringUtils.isEmpty(path) ? null : new File(path);
            if (preselFromStorage != null) {
                presel = new File(preselFromStorage.getParentFile(), presel.getName());
            }
        }
        if (presel == null) {
            presel = new File(System.getProperty("user.home"), "documents");
        }
        final File orgPresel = ExtFileSystemView.getShellFolder(presel);
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
                fc.setSelectedFile(ExtFileSystemView.getShellFolder(new File(ExtFileSystemView.getShellFolder(fc.getCurrentDirectory()), orgPresel.getName())));
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
            try {
                installDeleteContextMenu(fcContainer);
                installDeleteHotkey();
                installNewFolderHotkey(fcContainer);
                installViewTypeListener(fcContainer);
            } catch (final Throwable e) {
                // internal structure of the FilePane depends on the LAF
                getLogger().log(e);
            }
            if (fileSelectionMode != null && fileSelectionMode.getId() == FileChooserSelectionMode.DIRECTORIES_ONLY.getId() && fcContainer.getComponentCount() >= 4) {
                // TODO:please doc... what does this hide?
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
                        protected boolean isEntryRemovable(final String value) {
                            return ExtFileChooserDialog.this.isQuickSelectionEntryRemovable(value);
                        }

                        @Override
                        protected void onEntryRemoved(final String value) {
                            ExtFileChooserDialog.this.onQuickSelectionEntryRemoved(value);
                        }

                        @Override
                        protected Icon getRemoveEntryIcon() {
                            return ExtFileChooserDialog.this.getQuickSelectionRemoveIcon(super.getRemoveEntryIcon());
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
                                        ret = fileSystemView.getNetworkFolder().get(ExtFileSystemView.getShellFolder(new File(txt)).getAbsolutePath());
                                    }
                                    return ret != null ? ret : ExtFileSystemView.getShellFolder(new File(txt));
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
                                                                // new/old directory may be null
                                                                setText = evt.getOldValue() == null || evt.getNewValue() == null || (evt.getNewValue().toString().length() < evt.getOldValue().toString().length());
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
                                        // TODO: dir.getParent() is null if dir is a root (e.g. "C:\"). This results in the text "null\".
                                        // Needs a
                                        // decision: omit the parent for roots, or use the path of dir itself.
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
        cleanupIcon("FileView.hardDriveIcon", ExtFileChooserDialogIcon.FILECHOOSER_HARDDRIVE.path());
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

    /**
     * Override to allow removing entries from the quick selection popup (remove icon on the highlighted entry, DEL key).
     *
     * @see SearchComboBox#isEntryRemovable(Object)
     */
    protected boolean isQuickSelectionEntryRemovable(final String path) {
        return false;
    }

    /**
     * Gets called after the user removed an entry from the quick selection popup, e.g. to remove it from a persistent history.
     */
    protected void onQuickSelectionEntryRemoved(final String path) {
    }

    /**
     * @param defaultIcon
     *            default remove icon of the {@link SearchComboBox}
     * @return icon shown at the right side of the highlighted removable quick selection entry
     */
    protected Icon getQuickSelectionRemoveIcon(final Icon defaultIcon) {
        return defaultIcon;
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
            hookViews();
        }
    }

    /**
     * Hooks the table (details view) and the list (list view). The FilePane creates them when they are shown the first time, so this is
     * also called whenever the view type changes. Every hook is installed only once per component.
     */
    private void hookViews() {
        if (fc != null) {
            final Map<JTable, List<Integer>> jtables = SwingUtils.find(fc, null, JTable.class);
            if (jtables != null && jtables.size() == 1) {
                final JTable jtable = jtables.keySet().iterator().next();
                if (jtable.getClientProperty(COLUMN_RESTORE_LISTENER_INSTALLED) == null) {
                    // updateView() may be called multiple times. Install the listeners only once per table.
                    jtable.putClientProperty(COLUMN_RESTORE_LISTENER_INSTALLED, Boolean.TRUE);
                    jtable.getModel().addTableModelListener(new TableModelListener() {
                        @Override
                        public void tableChanged(TableModelEvent e) {
                            if (e.getType() == TableModelEvent.UPDATE && e.getColumn() == TableModelEvent.ALL_COLUMNS) {
                                restoreTableColumnModel(jtable);
                            }
                            /*
                             * The reload of the folder is reported with any kind of event. Mostly as removed/inserted rows, not as complete
                             * update. So check the pending selection (after delete/rename) on every event.
                             */
                            selectPendingFileLater(jtable);
                        }
                    });
                    // the FilePane replaces the whole TableColumnModel if the columns change (e.g. other folder type). This fires no table
                    // model event, so we have to restore the layout for the new column model here.
                    jtable.addPropertyChangeListener("columnModel", new PropertyChangeListener() {
                        @Override
                        public void propertyChange(final PropertyChangeEvent evt) {
                            restoreTableColumnModel(jtable);
                        }
                    });
                }
                restoreTableColumnModel(jtable);
                installEditorFocusHandler(jtable);
            }
            final JComponent fcContainer = getFileChooserContainer(fc);
            final JComponent filePane = fcContainer == null ? null : findFilePane(fcContainer);
            if (filePane != null) {
                final Map<JList, List<Integer>> jlists = SwingUtils.find(filePane, null, JList.class);
                if (jlists != null && jlists.size() == 1) {
                    installListEditHandler(jlists.keySet().iterator().next());
                }
            }
        }
    }

    private static final String VIEW_TYPE_LISTENER_INSTALLED = ExtFileChooserDialog.class.getName() + ".viewTypeListener";

    /**
     * The FilePane fires "viewType" when the user switches between list and details view. The component of the new view may not exist
     * before, so hook it now.
     */
    private void installViewTypeListener(final JComponent fcContainer) {
        final JComponent filePane = findFilePane(fcContainer);
        if (filePane == null || filePane.getClientProperty(VIEW_TYPE_LISTENER_INSTALLED) != null) {
            return;
        }
        filePane.putClientProperty(VIEW_TYPE_LISTENER_INSTALLED, Boolean.TRUE);
        filePane.addPropertyChangeListener("viewType", new PropertyChangeListener() {
            @Override
            public void propertyChange(final PropertyChangeEvent evt) {
                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        hookViews();
                    }
                });
            }
        });
    }

    private JComponent findFilePane(final JComponent fcContainer) {
        for (int index = 0; index < fcContainer.getComponentCount(); index++) {
            final Component c = fcContainer.getComponent(index);
            if (c instanceof JComponent && ReflectionUtils.isInstanceOf("sun.swing.FilePane", c)) {
                return (JComponent) c;
            }
        }
        return null;
    }

    private static final String NEW_FOLDER_HOTKEY_ACTION = ExtFileChooserDialog.class.getName() + ".newFolder";

    /**
     * Default hotkey to create a new folder: Ctrl+Shift+N (Windows Explorer, Nautilus), Cmd+Shift+N on macOS (Finder). Triggers the same
     * action as the "New Folder" button/menu entry, so the new folder gets the inline rename like usual.
     */
    protected void installNewFolderHotkey(final JComponent fcContainer) {
        final JComponent filePane = findFilePane(fcContainer);
        if (filePane == null) {
            return;
        }
        final Action newFolderAction = filePane.getActionMap().get("New Folder");
        if (newFolderAction == null) {
            return;
        }
        final int modifiers = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask() | InputEvent.SHIFT_MASK;
        /*
         * Register it on the root pane of the dialog for the whole window. Directly after opening, the focus is on a button of the dialog,
         * which is no child of the file chooser. So a binding on the file chooser would not work until the user clicks into it.
         */
        final JRootPane rootPane = getDialog().getRootPane();
        rootPane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_N, modifiers), NEW_FOLDER_HOTKEY_ACTION);
        rootPane.getActionMap().put(NEW_FOLDER_HOTKEY_ACTION, new AbstractAction() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                /* the action is disabled if the current folder is not writable */
                if (newFolderAction.isEnabled()) {
                    newFolderAction.actionPerformed(e);
                }
            }
        });
    }

    /**
     * Adds "Delete file/folder" to the right click menu of the file list (like in the Explorer). The popup menu of the FilePane is created
     * once and used by the list view and the details view. We update the entry every time the menu opens, based on the item under the
     * mouse.
     */
    protected void installDeleteContextMenu(final JComponent fcContainer) {
        final JComponent filePane = findFilePane(fcContainer);
        if (filePane == null) {
            return;
        }
        final JPopupMenu menu = filePane.getComponentPopupMenu();
        if (menu == null) {
            return;
        }
        final JMenuItem deleteItem = new JMenuItem();
        final AtomicReference<File> target = new AtomicReference<File>();
        final AtomicReference<Component> targetView = new AtomicReference<Component>();
        deleteItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                deleteFromContextMenu(target.get(), targetView.get());
            }
        });
        menu.addSeparator();
        menu.add(deleteItem);
        menu.addPopupMenuListener(new PopupMenuListener() {
            @Override
            public void popupMenuWillBecomeVisible(final PopupMenuEvent e) {
                final File file = getFileUnderMouse(menu.getInvoker());
                target.set(file);
                targetView.set(menu.getInvoker());
                final boolean deletable = isDeletable(file);
                deleteItem.setEnabled(deletable);
                deleteItem.setText(file != null && file.isDirectory() ? _AWU.T.DIALOG_FILECHOOSER_deleteFolder() : _AWU.T.DIALOG_FILECHOOSER_deleteFile());
            }

            @Override
            public void popupMenuWillBecomeInvisible(final PopupMenuEvent e) {
            }

            @Override
            public void popupMenuCanceled(final PopupMenuEvent e) {
            }
        });
    }

    private static final String DELETE_HOTKEY_ACTION = ExtFileChooserDialog.class.getName() + ".delete";

    /**
     * DEL deletes the selected file/folder of the list/details view. Only if the list or table has the focus. Text fields (file name field,
     * rename editor) handle DEL themselves.
     */
    protected void installDeleteHotkey() {
        fc.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0), DELETE_HOTKEY_ACTION);
        fc.getActionMap().put(DELETE_HOTKEY_ACTION, new AbstractAction() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                final Component owner = KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();
                final File file = getSelectedFileOfView(owner);
                if (isDeletable(file)) {
                    deleteFromContextMenu(file, owner);
                }
            }
        });
    }

    /**
     * @return the selected file of the list/table. null if nothing or more than one file is selected
     */
    private File getSelectedFileOfView(final Component view) {
        Object value = null;
        if (view instanceof JTable) {
            final JTable table = (JTable) view;
            if (table.getSelectedRowCount() == 1) {
                value = getTableFile(table, table.getSelectedRow());
            }
        } else if (view instanceof JList) {
            final JList list = (JList) view;
            if (list.getSelectedIndices().length == 1) {
                value = list.getSelectedValue();
            }
        }
        return value instanceof File ? (File) value : null;
    }

    /**
     * Returns the file (list entry or table row) below the mouse pointer, and selects it, like the Explorer does on a right click.
     */
    private File getFileUnderMouse(final Component invoker) {
        try {
            final Point p = MouseInfo.getPointerInfo().getLocation();
            SwingUtilities.convertPointFromScreen(p, invoker);
            Object value = null;
            if (invoker instanceof JTable) {
                final JTable table = (JTable) invoker;
                final int row = table.rowAtPoint(p);
                if (row >= 0) {
                    value = getTableFile(table, row);
                    if (!table.isRowSelected(row)) {
                        table.setRowSelectionInterval(row, row);
                    }
                }
            } else if (invoker instanceof JList) {
                final JList list = (JList) invoker;
                final int index = list.locationToIndex(p);
                final Rectangle bounds = index < 0 ? null : list.getCellBounds(index, index);
                if (bounds != null && bounds.contains(p)) {
                    value = list.getModel().getElementAt(index);
                    if (!list.isSelectedIndex(index)) {
                        list.setSelectedIndex(index);
                    }
                }
            }
            return value instanceof File ? (File) value : null;
        } catch (final Throwable e) {
            getLogger().log(e);
            return null;
        }
    }

    /**
     * Virtual entries (network, home folders, roots/drives) must never be deleted.
     */
    protected boolean isDeletable(final File file) {
        if (file == null || file instanceof NetWorkFolder || file instanceof VirtualRoot || file instanceof HomeFolder) {
            return false;
        } else if (file.getParentFile() == null) {
            return false;
        } else {
            return file.exists();
        }
    }

    /**
     * Deletes a file or folder (context menu and DEL hotkey). Files and folders with content have to be confirmed. Empty folders are
     * deleted right away.
     */
    private void deleteFromContextMenu(final File file, final Component view) {
        if (!isDeletable(file)) {
            return;
        }
        // the entry that gets selected after the deletion
        final File neighbor = getNeighborFile(view, file);
        final boolean directory = file.isDirectory();
        final String title = directory ? _AWU.T.DIALOG_FILECHOOSER_deleteFolder() : _AWU.T.DIALOG_FILECHOOSER_deleteFile();
        boolean confirm = true;
        if (directory) {
            // list() returns null if the content is unknown (e.g. no access). Ask in this case, too.
            final String[] content = file.list();
            confirm = content == null || content.length > 0;
        }
        if (confirm) {
            final String question = directory ? _AWU.T.DIALOG_FILECHOOSER_deleteFolderConfirm(file.getAbsolutePath()) : _AWU.T.DIALOG_FILECHOOSER_deleteFileConfirm(file.getAbsolutePath());
            if (JOptionPane.showConfirmDialog(fc, question, title, JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE) != JOptionPane.YES_OPTION) {
                return;
            }
        }
        setBusy(true);
        final Thread thread = new Thread("ExtFileChooserDialog:delete") {
            @Override
            public void run() {
                boolean success = false;
                try {
                    if (directory && !isSymbolicLink(file)) {
                        Files.deleteRecursive(file);
                    } else {
                        // files and links: delete only the entry itself. Never follow a link into its target.
                        file.delete();
                    }
                    success = !file.exists();
                } catch (final Throwable e) {
                    getLogger().log(e);
                    success = !file.exists();
                }
                final boolean deleted = success;
                new EDTRunner() {
                    @Override
                    protected void runInEDT() {
                        setBusy(false);
                        if (deleted) {
                            // select the next entry (else the previous one, null if the list is empty now). The FilePane applies the
                            // selection to the list/table after the rescan, so keyboard navigation continues from there.
                            fc.setSelectedFile(neighbor);
                            /*
                             * The reload of the list clears the selection of the table/list including lead and anchor (keyboard navigation
                             * then starts at the top). So select the neighbor again after the reload.
                             */
                            if (neighbor != null && view != null) {
                                pendingSelectionAfterRename.set(neighbor);
                                pendingDeletedFile.set(file);
                                pendingSelectionDeadline = Time.systemIndependentCurrentJVMTimeMillis() + PENDING_SELECTION_TIMEOUT;
                            }
                        }
                        fc.rescanCurrentDirectory();
                        if (view != null) {
                            selectPendingFileLater(view);
                        }
                        if (view != null) {
                            view.requestFocusInWindow();
                        }
                        if (!deleted) {
                            JOptionPane.showMessageDialog(fc, _AWU.T.DIALOG_FILECHOOSER_deleteError(file.getAbsolutePath()), title, JOptionPane.ERROR_MESSAGE);
                        }
                    }
                };
            }
        };
        thread.setDaemon(true);
        thread.start();
    }

    /**
     * @return the entry below the given file in the list/table (in the order the user sees it), else the entry above it. null if there is
     *         none.
     */
    private File getNeighborFile(final Component view, final File file) {
        final List<File> entries = new ArrayList<File>();
        if (view instanceof JTable) {
            final JTable table = (JTable) view;
            for (int row = 0; row < table.getRowCount(); row++) {
                entries.add(getTableFile(table, row));
            }
        } else if (view instanceof JList) {
            final ListModel model = ((JList) view).getModel();
            for (int index = 0; index < model.getSize(); index++) {
                final Object value = model.getElementAt(index);
                entries.add(value instanceof File ? (File) value : null);
            }
        }
        final int index = entries.indexOf(file);
        if (index < 0) {
            return null;
        }
        if (index + 1 < entries.size()) {
            return entries.get(index + 1);
        } else if (index > 0) {
            return entries.get(index - 1);
        } else {
            return null;
        }
    }

    private boolean isSymbolicLink(File file) {
        try {
            file = ExtFileSystemView.getShellFolder(file);
            final File parent = file.getAbsoluteFile().getParentFile();
            final File canonical = parent == null ? file.getCanonicalFile() : new File(parent.getCanonicalFile(), file.getName());
            return !canonical.getCanonicalFile().equals(canonical.getAbsoluteFile());
        } catch (final Throwable e) {
            // unknown: handle it as link, which means we do not delete recursively
            return true;
        }
    }

    private static final String COLUMN_RESTORE_LISTENER_INSTALLED = ExtFileChooserDialog.class.getName() + ".columnRestoreListener";
    private static final String EDITOR_FOCUS_HANDLER_INSTALLED    = ExtFileChooserDialog.class.getName() + ".editorFocusHandler";

    /**
     * JTable#editCellAt does not request the focus for the editor component. After "New Folder", the FilePane starts the inline rename via
     * editCellAt, so the new name is neither focused nor selected. Behave like the Windows Explorer: focus the editor and select the whole
     * (auto generated) name, so the user can type a name right away.
     */
    protected void installEditorFocusHandler(final JTable jtable) {
        if (jtable.getClientProperty(EDITOR_FOCUS_HANDLER_INSTALLED) != null) {
            return;
        }
        jtable.putClientProperty(EDITOR_FOCUS_HANDLER_INSTALLED, Boolean.TRUE);
        final AtomicReference<File> editedFile = new AtomicReference<File>();
        final AtomicReference<JTextField> editedField = new AtomicReference<JTextField>();
        jtable.addPropertyChangeListener("tableCellEditor", new PropertyChangeListener() {
            @Override
            public void propertyChange(final PropertyChangeEvent evt) {
                if (evt.getNewValue() == null) {
                    /*
                     * The editor was removed. If the file was renamed (JTable#editingStopped calls setValueAt, which renames, before it
                     * removes the editor), the renamed file has to be selected afterwards.
                     */
                    onRenameEditorClosed(jtable, editedFile.getAndSet(null), editedField.getAndSet(null));
                    return;
                }
                // the editor component is added to the table after the property change -> wait for it
                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        if (!jtable.isEditing()) {
                            return;
                        }
                        final Component editor = jtable.getEditorComponent();
                        if (editor == null) {
                            return;
                        }
                        final int row = jtable.getEditingRow();
                        editedFile.set(row < 0 ? null : getTableFile(jtable, row));
                        editedField.set(editor instanceof JTextField ? (JTextField) editor : null);
                        /*
                         * The edited entry (e.g. a new folder) has to be the only selected entry. Else the old selection stays visible
                         * until the name is confirmed. Changing the selection may make the FileChooser apply and close the editor (it does
                         * so if the selected file changes). So start the editor again in this case. The new editor starts this handler
                         * again.
                         */
                        if (row >= 0 && (jtable.getSelectedRowCount() != 1 || !jtable.isRowSelected(row))) {
                            final int column = jtable.getEditingColumn();
                            jtable.setRowSelectionInterval(row, row);
                            if (!jtable.isEditing() && column >= 0) {
                                jtable.editCellAt(row, column);
                                return;
                            }
                        }
                        editor.requestFocusInWindow();
                        if (editor instanceof JTextField) {
                            ((JTextField) editor).selectAll();
                        }
                    }
                });
            }
        });
    }

    private final AtomicReference<File> pendingSelectionAfterRename = new AtomicReference<File>();
    /** only used after deleting: the selection is applied after this file is gone from the list */
    private final AtomicReference<File> pendingDeletedFile          = new AtomicReference<File>();
    private static final long           PENDING_SELECTION_TIMEOUT   = 5000;
    /** the pending selection is dropped after this timestamp (Time.systemIndependentCurrentJVMTimeMillis) */
    private volatile long               pendingSelectionDeadline    = 0;

    /**
     * @return the file of the given row (view index). The file is always in the first column of the table MODEL, but the user may have
     *         moved the columns, so the first column of the view is not reliable.
     */
    private File getTableFile(final JTable table, final int viewRow) {
        final Object value = table.getModel().getValueAt(table.convertRowIndexToModel(viewRow), 0);
        return value instanceof File ? (File) value : null;
    }

    private static final String LIST_EDIT_HANDLER_INSTALLED = ExtFileChooserDialog.class.getName() + ".listEditHandler";

    /**
     * Same as {@link #installEditorFocusHandler(JTable)} for the list view. The FilePane does the inline rename there with a text field,
     * which it adds to the list while editing, and removes afterwards.
     */
    protected void installListEditHandler(final JList list) {
        if (list.getClientProperty(LIST_EDIT_HANDLER_INSTALLED) != null) {
            return;
        }
        list.putClientProperty(LIST_EDIT_HANDLER_INSTALLED, Boolean.TRUE);
        final AtomicReference<File> editedFile = new AtomicReference<File>();
        final AtomicReference<JTextField> editedField = new AtomicReference<JTextField>();
        list.addContainerListener(new ContainerAdapter() {
            @Override
            public void componentAdded(final ContainerEvent e) {
                if (!(e.getChild() instanceof JTextField)) {
                    return;
                }
                final JTextField field = (JTextField) e.getChild();
                /* the inline rename starts for the selected entry (new folders are selected before) */
                final Object selected = list.getSelectedValue();
                editedFile.set(selected instanceof File ? (File) selected : null);
                editedField.set(field);
                /* the FilePane sets text and bounds after adding the field */
                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        if (field.isShowing()) {
                            /* the entry below the editor is the edited one. Make it the only selected entry (old selection disappears). */
                            final int index = list.locationToIndex(new Point(field.getX() + 1, field.getY() + field.getHeight() / 2));
                            if (index >= 0) {
                                final Object edited = list.getModel().getElementAt(index);
                                editedFile.set(edited instanceof File ? (File) edited : null);
                                if (list.getSelectedIndices().length != 1 || !list.isSelectedIndex(index)) {
                                    list.setSelectedIndex(index);
                                    /*
                                     * The FileChooser may close the editor when the selection changes. Start it again with the action of
                                     * the FilePane (it edits the selected entry). The new field starts this handler again.
                                     */
                                    if (field.getParent() == null) {
                                        Component parent = list.getParent();
                                        while (parent != null && !ReflectionUtils.isInstanceOf("sun.swing.FilePane", parent)) {
                                            parent = parent.getParent();
                                        }
                                        final Action editAction = parent instanceof JComponent ? ((JComponent) parent).getActionMap().get("editFileName") : null;
                                        if (editAction != null && editAction.isEnabled()) {
                                            editAction.actionPerformed(new ActionEvent(list, ActionEvent.ACTION_PERFORMED, "editFileName"));
                                        }
                                        return;
                                    }
                                }
                            }
                            field.requestFocusInWindow();
                            field.selectAll();
                        }
                    }
                });
            }

            @Override
            public void componentRemoved(final ContainerEvent e) {
                if (e.getChild() == editedField.get()) {
                    onRenameEditorClosed(list, editedFile.getAndSet(null), editedField.getAndSet(null));
                }
            }
        });
        /* the list is updated asynchronously after a rename */
        list.getModel().addListDataListener(new ListDataListener() {
            @Override
            public void intervalAdded(final ListDataEvent e) {
                selectPendingFileLater(list);
            }

            @Override
            public void intervalRemoved(final ListDataEvent e) {
                selectPendingFileLater(list);
            }

            @Override
            public void contentsChanged(final ListDataEvent e) {
                selectPendingFileLater(list);
            }
        });
    }

    /**
     * Called when the inline rename editor (table or list) was closed. If the file was renamed, the renamed file has to be selected as soon
     * as it is in the list.
     */
    private void onRenameEditorClosed(final Component view, final File oldFile, final JTextField field) {
        if (oldFile == null || field == null || oldFile.getParentFile() == null) {
            return;
        }
        final String name = field.getText().trim();
        if (name.length() == 0) {
            return;
        }
        final File renamed = ExtFileSystemView.getShellFolder(new File(oldFile.getParentFile(), name));
        /* the rename is already done at this point. If not (cancelled, error), there is nothing to select */
        if (!oldFile.exists() && renamed.exists()) {
            pendingSelectionAfterRename.set(renamed);
            pendingDeletedFile.set(null);
            pendingSelectionDeadline = Time.systemIndependentCurrentJVMTimeMillis() + PENDING_SELECTION_TIMEOUT;
            selectPendingFileLater(view);
        }
    }

    /**
     * Later, because the row sorter/list has to process the update first.
     */
    private void selectPendingFileLater(final Component view) {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                selectPendingFile(view);
            }
        });
    }

    /**
     * Renaming is asynchronous: the new name is not in the list yet when the rename is done. We select the renamed file as soon as it is in
     * the list. Called after the rename, and after every following update of the list/table data.
     */
    private void selectPendingFile(final Component view) {
        final File target = pendingSelectionAfterRename.get();
        if (target == null) {
            return;
        }
        /*
         * after a delete: wait until the list was reloaded, which means the deleted file is gone. Else the selection would be lost again.
         */
        final File deleted = pendingDeletedFile.get();
        final int index = deleted != null && getIndexInView(view, deleted) >= 0 ? -1 : getIndexInView(view, target);
        if (index >= 0) {
            pendingSelectionAfterRename.set(null);
            pendingDeletedFile.set(null);
            fc.setSelectedFile(target);
            /* this also sets lead and anchor, so the keyboard navigation continues from this entry */
            if (view instanceof JTable) {
                final JTable jtable = (JTable) view;
                jtable.setRowSelectionInterval(index, index);
                jtable.scrollRectToVisible(jtable.getCellRect(index, 0, true));
            } else if (view instanceof JList) {
                final JList list = (JList) view;
                list.setSelectedIndex(index);
                list.ensureIndexIsVisible(index);
            }
            return;
        }
        /* give up if the file does not show up (e.g. other folder opened in the meantime) */
        if (Time.systemIndependentCurrentJVMTimeMillis() > pendingSelectionDeadline) {
            pendingSelectionAfterRename.set(null);
            pendingDeletedFile.set(null);
        }
    }

    /**
     * @return the index (view) of the file in the table/list, or -1
     */
    private int getIndexInView(final Component view, final File file) {
        if (view instanceof JTable) {
            final JTable jtable = (JTable) view;
            for (int row = 0; row < jtable.getRowCount(); row++) {
                if (file.equals(getTableFile(jtable, row))) {
                    return row;
                }
            }
        } else if (view instanceof JList) {
            final ListModel model = ((JList) view).getModel();
            for (int index = 0; index < model.getSize(); index++) {
                if (file.equals(model.getElementAt(index))) {
                    return index;
                }
            }
        }
        return -1;
    }

    final AtomicReference<TableColumnModel> previousTableColumnModelReference = new AtomicReference<TableColumnModel>();

    protected void storeTableColumnModel(final TableColumnModel tableColumnModel, final Map<String, Object> storage) {
        for (int c = 0; c < tableColumnModel.getColumnCount(); c++) {
            final TableColumn col = tableColumnModel.getColumn(c);
            final String key = getColumnStorageKey(col);
            storage.put(key + "_width", col.getWidth());
            storage.put(key + "_pos", c);
        }
    }

    /**
     * Fixed, language independent storage keys of the columns of the FilePane details view (in the order of their model index). The current
     * directory of the dialog is always a ShellFolder (see FileChooserFileSystemView#getCurrentDirectory), so the columns are the ones of
     * the shell: name, size, type, modified (and more for special folders, which use the key "column&lt;index&gt;"). Plain Files would have
     * the default columns (name, size, modified) only.
     */
    private static final String[] COLUMN_STORAGE_KEYS = new String[] { "name", "size", "type", "modified" };

    /**
     * The FilePane does not set a column identifier, so TableColumn#getIdentifier() falls back to the (localized) header title. Storing by
     * title loses the layout when the language changes. So we use fixed keys, based on the model index of the column.
     */
    protected String getColumnStorageKey(final TableColumn col) {
        final int index = col.getModelIndex();
        if (index >= 0 && index < COLUMN_STORAGE_KEYS.length) {
            return COLUMN_STORAGE_KEYS[index];
        }
        return "column" + index;
    }

    protected void restoreTableColumnModel(final JTable jtable) {
        if (jtable.isEditing()) {
            // changing width/position of columns makes JTable stop the cell editor (columnMarginChanged/columnMoved). This would abort the
            // inline rename (e.g. after "New Folder") right after it started.
            return;
        }
        final Map<String, Object> storage = getStorage();
        final TableColumnModel currentTableColumnModel = jtable.getColumnModel();
        final TableColumnModel previousTableColumnModel = previousTableColumnModelReference.getAndSet(currentTableColumnModel);
        if (previousTableColumnModel != null) {
            storeTableColumnModel(previousTableColumnModel, storage);
        }
        for (int c = 0; c < currentTableColumnModel.getColumnCount(); c++) {
            final TableColumn col = currentTableColumnModel.getColumn(c);
            final Object width = storage.get(getColumnStorageKey(col) + "_width");
            if (width instanceof Number) {
                final int w = ((Number) width).intValue();
                if (w > 0) {
                    col.setWidth(w);
                    col.setPreferredWidth(w);
                }
            }
        }
        // moveColumn shifts the indices of the other columns. So we cannot move while iterating over the current indices. Sort the columns
        // by
        // their stored position and move them one after another, beginning with the lowest position.
        final java.util.List<TableColumn> ordered = new ArrayList<TableColumn>();
        final Map<TableColumn, Integer> positions = new HashMap<TableColumn, Integer>();
        for (int c = 0; c < currentTableColumnModel.getColumnCount(); c++) {
            final TableColumn col = currentTableColumnModel.getColumn(c);
            final Object pos = storage.get(getColumnStorageKey(col) + "_pos");
            if (pos instanceof Number && ((Number) pos).intValue() >= 0) {
                ordered.add(col);
                positions.put(col, ((Number) pos).intValue());
            }
        }
        Collections.sort(ordered, new Comparator<TableColumn>() {
            @Override
            public int compare(final TableColumn o1, final TableColumn o2) {
                return positions.get(o1).compareTo(positions.get(o2));
            }
        });
        for (final TableColumn col : ordered) {
            final int target = Math.min(positions.get(col).intValue(), currentTableColumnModel.getColumnCount() - 1);
            for (int c = 0; c < currentTableColumnModel.getColumnCount(); c++) {
                if (currentTableColumnModel.getColumn(c) == col) {
                    if (c != target) {
                        currentTableColumnModel.moveColumn(c, target);
                    }
                    break;
                }
            }
        }
        jtable.setAutoResizeMode(JTable.AUTO_RESIZE_LAST_COLUMN);
    }
}
