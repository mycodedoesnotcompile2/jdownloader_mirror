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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import javax.swing.Icon;
import javax.swing.filechooser.FileSystemView;

import org.appwork.loggingv3.LogV3;
import org.appwork.storage.config.JsonConfig;
import org.appwork.sunwrapper.sun.awt.shell.ShellFolderWrapper;
import org.appwork.utils.Time;
import org.appwork.utils.locale._AWU;
import org.appwork.utils.os.CrossSystem;

/**
 *
 *
 * This ExtFileSystemView is a workaround. The standard FileSystemView maps all contents of the Windows Desktop into the save in combobox.
 * The virtual WIndows Folders are a pain in the a%/&
 *
 * @author Thomas
 *
 */
public class ExtFileSystemView extends FileSystemView {

    public static File getShellFolder(File file) {
        try {
            return file != null ? ShellFolderWrapper.getShellFolderIfAvailable(file) : file;
        } catch (FileNotFoundException e) {
            return file;
        }
    }

    private final FileSystemView  org;
    private File[]                roots;
    private NetWorkFolder         networkFolder;
    private HashMap<Object, File> specialsMap;
    private final boolean         useFileIcons;
    /**
     *
     */
    public static final String    VIRTUAL_NETWORKFOLDER    = "::{F02C1A0D-BE21-4350-88B0-7367FC96EF3C}";
    public static final String    VIRTUAL_NETWORKFOLDER_XP = "::{208D2C60-3AEA-1069-A2D7-08002B30309D}";
    public static final String    VIRTUAL_GALLERY          = "::{E88865EA-0E1C-4E20-9AA6-EDCD0212C87C}";
    public static final String    VIRTUAL_LIBRARY          = "::{031E4825-7B94-4DC3-B131-E946B44C8DD5}";

    /**
     */
    public ExtFileSystemView() {
        this.org = FileSystemView.getFileSystemView();
        this.useFileIcons = JsonConfig.create(ExtFileSystemViewSettings.class).isUseSystemIcons();
    }

    @Override
    public File createFileObject(final File dir, final String filename) {
        return this.org.createFileObject(dir, filename);
    }

    @Override
    public File createFileObject(final String path) {
        return this.org.createFileObject(path);
    }

    /*
     * (non-Javadoc)
     *
     * @see javax.swing.filechooser.FileSystemView#createNewFolder(java.io.File)
     */
    @Override
    public File createNewFolder(final File containingDir) throws IOException {
        return this.org.createNewFolder(containingDir);
    }

    @Override
    public File getChild(final File parent, final String fileName) {
        return this.org.getChild(parent, fileName);
    }

    @Override
    public File getDefaultDirectory() {
        return this.org.getDefaultDirectory();
    }

    @Override
    public File[] getFiles(final File dir, final boolean useFileHiding) {
        final long t = Time.systemIndependentCurrentJVMTimeMillis();
        try {
            final File[] ret;
            if (dir == this.networkFolder) {
                LogV3.I().getDefaultLogger().info("getFilesShellfolder");
                ret = this.getFilesShellfolder((NetWorkFolder) dir, useFileHiding);
            } else {
                LogV3.I().getDefaultLogger().info("org.getFiles(dir, useFileHiding);");
                ret = this.org.getFiles(dir, useFileHiding);
            }
            LogV3.I().getDefaultLogger().info("getFiles: ms:" + (Time.systemIndependentCurrentJVMTimeMillis() - t) + " " + dir + "(" + dir.getName() + ")" + "|" + ret.length);
            final java.util.List<File> filtered = new ArrayList<File>();
            for (final File f : ret) {
                if (f.getName().equals(ExtFileSystemView.VIRTUAL_NETWORKFOLDER)) {
                    filtered.add(new NetWorkFolder(f));
                    continue;
                } else if (f.getName().equals(ExtFileSystemView.VIRTUAL_NETWORKFOLDER_XP)) {
                    filtered.add(new NetWorkFolder(f));
                    continue;
                } else if (f.getName().startsWith("::{")) {
                    continue;
                }
                filtered.add(f);
            }
            LogV3.I().getDefaultLogger().info("Return Files for " + dir + "(" + useFileHiding + "): " + filtered.size());
            return filtered.toArray(new File[] {});
        } finally {
            LogV3.I().getDefaultLogger().info("getFiles(end): ms:" + (Time.systemIndependentCurrentJVMTimeMillis() - t) + " " + dir + "(" + dir.getName() + ")");
        }
    }

    public File[] getFilesShellfolder(final NetWorkFolder network, final boolean useFileHiding) {
        final List<File> files = new ArrayList<File>();
        final File[] names = network.listFiles(useFileHiding);
        if (names == null) {
            return new File[0];
        }
        for (File f : names) {
            if (Thread.currentThread().isInterrupted()) {
                break;
            }
            if (!ShellFolderWrapper.isInstanceof(f)) {
                if (this.isFileSystemRoot(f)) {
                    f = this.createFileSystemRoot(f);
                }
                try {
                    f = ShellFolderWrapper.getShellFolderIfAvailable(f);
                } catch (final FileNotFoundException e) {
                    // Not a valid file (wouldn't show in native file
                    // chooser)
                    // Example: C:\pagefile.sys
                    continue;
                } catch (final InternalError e) {
                    // Not a valid file (wouldn't show in native file
                    // chooser)
                    // Example C:\Winnt\Profiles\joe\history\History.IE5
                    continue;
                }
            }
            if (!useFileHiding || !this.isHiddenFile(f)) {
                files.add(f);
            }
        }
        return files.toArray(new File[files.size()]);
    }

    @Override
    public File getHomeDirectory() {
        return this.org.getHomeDirectory();
    }

    /**
     * @return
     */
    public NetWorkFolder getNetworkFolder() {
        return this.networkFolder;
    }

    @Override
    public File getParentDirectory(final File dir) {
        final File ret = this.org.getParentDirectory(dir);
        return mapSpecialFolders(ret);
    }

    protected File toShellFolder(File file) {
        return ExtFileSystemView.getShellFolder(file);
    }

    @Override
    public File[] getRoots() {
        final long t = Time.systemIndependentCurrentJVMTimeMillis();
        LogV3.I().getDefaultLogger().info("Get Roots");
        if (this.roots != null) {
            return this.roots;
        }
        try {
            // this may take a long time on some systems.
            final File[] baseFolders = getBaseFolders();
            LogV3.I().getDefaultLogger().info("Listed Base folders " + (Time.systemIndependentCurrentJVMTimeMillis() - t) + "|" + (baseFolders != null ? baseFolders.length : -1));
            final Set<File> unique = new LinkedHashSet<File>();
            File desktopPath = new File(System.getProperty("user.home") + "/Desktop");
            final HashMap<Object, File> nspecialsMap = new HashMap<Object, File>();
            if (desktopPath.isDirectory()) {
                /* we prefer/want this Desktop folder over special Win32ShellFolder2 ones */
                desktopPath = toShellFolder(desktopPath);
                nspecialsMap.put(desktopPath, desktopPath);
                nspecialsMap.put(desktopPath.getPath(), desktopPath);
                unique.add(desktopPath);
            }
            this.mount(new File("/Volumes"), unique);
            this.mount(new File("/media"), unique);
            final String userName = System.getProperty("user.name");
            if (userName != null) {
                this.mount(new File("/run/media/" + userName), unique);
            }
            final HomeFolder[] homeFolders = new HomeFolder[] { new HomeFolder(HomeFolder.HOME_ROOT, ExtFileChooserDialogIcon.FILECHOOSER_HOME.path()), new HomeFolder(HomeFolder.DOCUMENTS, ExtFileChooserDialogIcon.FILECHOOSER_DOCUMENTS.path()), new HomeFolder(HomeFolder.DROPBOX, ExtFileChooserDialogIcon.FILECHOOSER_BOX.path()), new HomeFolder(HomeFolder.PICTURES, ExtFileChooserDialogIcon.FILECHOOSER_IMAGES.path()), new HomeFolder(HomeFolder.VIDEOS, ExtFileChooserDialogIcon.FILECHOOSER_VIDEO.path()), new HomeFolder(HomeFolder.DOWNLOADS, ExtFileChooserDialogIcon.FILECHOOSER_DOWNLOADS.path()), new HomeFolder(HomeFolder.MUSIC, ExtFileChooserDialogIcon.FILECHOOSER_MUSIC.path()) };
            for (final HomeFolder homeFolder : homeFolders) {
                if (homeFolder.isDirectory()) {
                    unique.add(homeFolder);
                }
            }
            if (baseFolders != null && this.networkFolder == null) {
                for (final File baseFolder : baseFolders) {
                    final String baseFolderName = baseFolder.getName();
                    if (baseFolderName.equals("Recent")) {
                        continue;
                    }
                    if (baseFolderName.equals(ExtFileSystemView.VIRTUAL_NETWORKFOLDER)) {
                        unique.add(this.networkFolder = new NetWorkFolder(baseFolder));
                        break;
                    } else if (baseFolderName.equals(ExtFileSystemView.VIRTUAL_NETWORKFOLDER_XP)) {
                        unique.add(this.networkFolder = new NetWorkFolder(baseFolder));
                        break;
                    }
                }
            }
            if (baseFolders != null) {
                final Set<String> skip = new HashSet<String>();
                if (CrossSystem.isWindows()) {
                    /* skip special Win32ShellFolder2 folder */
                    skip.add(VIRTUAL_LIBRARY);
                    skip.add(VIRTUAL_GALLERY);
                    skip.add("Recent");
                    skip.add("Desktop");
                }
                for (final File baseFolder : baseFolders) {
                    if (skip.contains(baseFolder.getName())) {
                        continue;
                    }
                    if (unique.contains(baseFolder)) {
                        /* skip already added HomeFolder or NetWorkFolder */
                        continue;
                    }
                    final File parentFile = baseFolder.getParentFile();
                    if (parentFile == null) {
                        if (desktopPath != null && desktopPath.getPath().equals(baseFolder.getPath())) {
                            /* skip special (Win32ShellFolder2)Desktop folder, has no parent */
                            continue;
                        }
                        unique.add(baseFolder);
                    } else {
                        if (baseFolder.getPath().equals(parentFile.getPath())) {
                            /* skip special (Win32ShellFolder2)Desktop folder, path and parent are the same */
                            continue;
                        }
                        if (desktopPath != null) {
                            if (baseFolder.getPath().equals(desktopPath.getPath())) {
                                continue;
                            }
                            if (baseFolder.getPath().startsWith(desktopPath.getPath())) {
                                /* skip all folders on (Win32ShellFolder2)Desktop folder */
                                continue;
                            }
                        }
                        unique.add(baseFolder);
                    }
                    LogV3.I().getDefaultLogger().info("Basefolder: " + baseFolder.getName() + " - " + baseFolder + " - " + CrossSystem.getOSString());
                }
            }
            final File[] roots = unique.toArray(new File[] {});
            for (final File nroot : roots) {
                if (nspecialsMap.containsKey(nroot) || nspecialsMap.containsKey(nroot.getPath())) {
                    continue;
                }
                nspecialsMap.put(nroot, nroot);
                nspecialsMap.put(nroot.getPath(), nroot);
            }
            this.specialsMap = nspecialsMap;
            return this.roots = roots;
        } finally {
            LogV3.I().getDefaultLogger().info("Roots: " + (Time.systemIndependentCurrentJVMTimeMillis() - t));
        }
    }

    private File[] getBaseFolders() {
        final File[] fileChooserComboBoxFolders = AccessController.doPrivileged(new PrivilegedAction<File[]>() {
            public File[] run() {
                try {
                    return (File[]) ShellFolderWrapper.get("fileChooserComboBoxFolders");
                } catch (final Throwable e) {
                    LogV3.I().getDefaultLogger().log(e);
                    return null;
                }
            }
        });
        return fileChooserComboBoxFolders;
    }

    @Override
    public String getSystemDisplayName(final File f) {
        if (f == this.networkFolder) {
            return _AWU.T.DIALOG_FILECHOOSER_networkfolder();
        } else if (f instanceof VirtualRoot) {
            return f.getName();
        } else {
            final File shellFolder = ShellFolderWrapper.getInstanceShellFolder(f);
            if (shellFolder != null) {
                return this.org.getSystemDisplayName(shellFolder);
            } else {
                return this.org.getSystemDisplayName(f);
            }
        }
    }

    @Override
    public Icon getSystemIcon(final File f) {
        try {
            if (f instanceof VirtualRoot) {
                return ((VirtualRoot) f).getIcon(18);
            } else if (useFileIcons) {
                final File shellFolder = ShellFolderWrapper.getInstanceShellFolder(f);
                if (shellFolder != null) {
                    return this.org.getSystemIcon(shellFolder);
                } else {
                    return this.org.getSystemIcon(f);
                }
            } else {
                if (f.isDirectory()) {
                    return ExtFileChooserDialogIcon.EXTFILESYSTEM_folder.get(18);
                } else {
                    return ExtFileChooserDialogIcon.EXTFILESYSTEM_fileIcon.get(18);
                }
            }
        } catch (final Exception e) {
            // seems like getSystemIcon can throw a FileNotFoundException or a
            // Nullpointerfor
            // 1.6 java
            return null;
        }
    }

    @Override
    public String getSystemTypeDescription(final File f) {
        return this.org.getSystemTypeDescription(f);
    }

    @Override
    public boolean isComputerNode(final File dir) {
        return this.org.isComputerNode(dir);
    }

    @Override
    public boolean isDrive(final File dir) {
        return this.org.isDrive(dir);
    }

    @Override
    public boolean isFileSystem(final File f) {
        return this.org.isFileSystem(f);
    }

    @Override
    public boolean isFileSystemRoot(final File dir) {
        return this.org.isFileSystemRoot(dir);
    }

    @Override
    public boolean isFloppyDrive(final File dir) {
        return this.org.isFloppyDrive(dir);
    }

    @Override
    public boolean isHiddenFile(final File f) {
        return this.org.isHiddenFile(f);
    }

    @Override
    public boolean isParent(final File folder, final File file) {
        boolean ret = this.org.isParent(folder, file);
        return ret;
    }

    @Override
    public boolean isRoot(final File f) {
        return this.org.isRoot(f);
    }

    @Override
    public Boolean isTraversable(final File f) {
        return this.org.isTraversable(f);
    }

    /**
     * @param f
     * @return
     */
    public File mapSpecialFolders(final File f) {
        if (specialsMap == null) {
            // if extended, this may not be initialized yet
            return f;
        }
        /* mapping via File.equals */
        File ret = this.specialsMap.get(f);
        if (ret == null) {
            /* mapping via File.getPath */
            ret = this.specialsMap.get(f.getPath());
        }
        return ret != null ? ret : f;
    }

    private void mount(File path, final Set<File> files) {
        if (path.exists() && path.isDirectory()) {
            path = ExtFileSystemView.getShellFolder(path);
            final File[] content = path.listFiles();
            if (content != null) {
                for (final File f : content) {
                    if (f.isHidden()) {
                        continue;
                    }
                    final VirtualRoot vFile = new VirtualRoot(f, f.getName());
                    files.add(vFile);
                }
            }
        }
    }
}
