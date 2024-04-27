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
import java.util.LinkedHashSet;
import java.util.List;

import javax.swing.Icon;
import javax.swing.filechooser.FileSystemView;

import org.appwork.loggingv3.LogV3;
import org.appwork.storage.config.JsonConfig;
import org.appwork.sunwrapper.sun.awt.shell.ShellFolderWrapper;
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
    public static boolean SAMBA_SCANNED = false;

    public static void runSambaScanner() {
        if (ExtFileSystemView.SAMBA_SCANNED) {
            return;
        }
        ExtFileSystemView.SAMBA_SCANNED = true;
        // final long tt = System.currentTimeMillis();
        // new Thread("Networkfolder Loader") {
        // @Override
        // public void run() {
        // final ExtFileSystemView view = new ExtFileSystemView();
        // view.getRoots();
        // try {
        // if (view.networkFolder != null) {
        // view.networkFolder.listFiles();
        // LogV3.I().getDefaultLogger().info("List Networkfolder done " +
        // (System.currentTimeMillis() -
        // tt));
        // }
        // } catch (final Exception e) {
        // e.printStackTrace();
        // }
        // }
        // }.start();
    }

    private final FileSystemView org;
    private File[]               roots;
    private NetWorkFolder        networkFolder;
    private HashMap<File, File>  specialsMap;
    private final boolean        useFileIcons;
    /**
     *
     */
    public static final String   VIRTUAL_NETWORKFOLDER    = "::{F02C1A0D-BE21-4350-88B0-7367FC96EF3C}";
    public static final String   VIRTUAL_NETWORKFOLDER_XP = "::{208D2C60-3AEA-1069-A2D7-08002B30309D}";

    /**
     */
    public ExtFileSystemView() {
        this.org = FileSystemView.getFileSystemView();
        if (!ExtFileSystemView.SAMBA_SCANNED) {
            new Exception("run ExtFileSystemView.runSambaScanner() as early as possible in your app!");
            ExtFileSystemView.runSambaScanner();
        }
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
        final long t = System.currentTimeMillis();
        try {
            final File[] ret;
            if (dir == this.networkFolder) {
                LogV3.I().getDefaultLogger().info("getFilesShellfolder");
                ret = this.getFilesShellfolder((NetWorkFolder) dir, useFileHiding);
            } else {
                LogV3.I().getDefaultLogger().info("org.getFiles(dir, useFileHiding);");
                ret = this.org.getFiles(dir, useFileHiding);
            }
            LogV3.I().getDefaultLogger().info("getFiles: ms:" + (System.currentTimeMillis() - t) + " " + dir + "|" + ret.length);
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
            LogV3.I().getDefaultLogger().info("getFiles(end): ms:" + (System.currentTimeMillis() - t) + " " + dir);
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
        return this.org.getParentDirectory(dir);
    }

    @Override
    public File[] getRoots() {
        final long t = System.currentTimeMillis();
        LogV3.I().getDefaultLogger().info("Get Roots");
        if (this.roots != null) {
            return this.roots;
        }
        try {
            // this may take a long time on some systems.
            final File[] baseFolders = getBaseFolders();
            LogV3.I().getDefaultLogger().info("Listed Base folders " + (System.currentTimeMillis() - t) + "|" + (baseFolders != null ? baseFolders.length : -1));
            final LinkedHashSet<File> unique = new LinkedHashSet<File>() {
                /*
                 * (non-Javadoc)
                 *
                 * @see java.util.HashSet#add(java.lang.Object)
                 */
                @Override
                public boolean add(final File e) {
                    if (this.contains(e)) {
                        return false;
                    }
                    return super.add(e);
                }
            };
            final File desktopPath = new File(System.getProperty("user.home") + "/Desktop");
            if (desktopPath.exists() && desktopPath.isDirectory()) {
                unique.add(desktopPath);
            }
            this.mount(new File("/Volumes"), unique);
            this.mount(new File("/media"), unique);
            String userName = System.getProperty("user.name");
            if (userName != null) {
                this.mount(new File("/run/media/" + userName), unique);
            }
            final HomeFolder[] homeFolders = new HomeFolder[] { new HomeFolder(HomeFolder.HOME_ROOT, ExtFileChooserDialogIcon.FILECHOOSER_HOME.path()), new HomeFolder(HomeFolder.DOCUMENTS, ExtFileChooserDialogIcon.FILECHOOSER_DOCUMENTS.path()), new HomeFolder(HomeFolder.DROPBOX, ExtFileChooserDialogIcon.FILECHOOSER_BOX.path()), new HomeFolder(HomeFolder.PICTURES, ExtFileChooserDialogIcon.FILECHOOSER_IMAGES.path()), new HomeFolder(HomeFolder.VIDEOS, ExtFileChooserDialogIcon.FILECHOOSER_VIDEO.path()), new HomeFolder(HomeFolder.DOWNLOADS, ExtFileChooserDialogIcon.FILECHOOSER_DOWNLOADS.path()), new HomeFolder(HomeFolder.MUSIC, ExtFileChooserDialogIcon.FILECHOOSER_MUSIC.path()) };
            for (final HomeFolder hf : homeFolders) {
                if (hf.exists()) {
                    unique.add(hf);
                }
            }
            if (baseFolders != null) {
                for (final File f : baseFolders) {
                    // Win32ShellFolder2.class
                    if (f.getName().equals("Recent")) {
                        continue;
                    }
                    if (f.getName().equals(ExtFileSystemView.VIRTUAL_NETWORKFOLDER)) {
                        this.networkFolder = new NetWorkFolder(f);
                        break;
                    } else if (f.getName().equals(ExtFileSystemView.VIRTUAL_NETWORKFOLDER_XP)) {
                        this.networkFolder = new NetWorkFolder(f);
                        break;
                    }
                }
            }
            if (this.networkFolder != null) {
                unique.add(this.networkFolder);
            }
            final File home = this.getHomeDirectory();
            if (baseFolders != null) {
                for (final File f : baseFolders) {
                    // Win32ShellFolder2.class
                    if (f.getName().equals("Recent")) {
                        continue;
                    }
                    if (f.getParentFile() == null || !f.getParentFile().equals(home)) {
                        unique.add(f);
                    }
                    LogV3.I().getDefaultLogger().info("Basefolder: " + f.getName() + " - " + f + " - " + CrossSystem.getOSString());
                }
            }
            final File[] nroots = unique.toArray(new File[] {});
            final HashMap<File, File> nspecialsMap = new HashMap<File, File>();
            for (final File f : nroots) {
                nspecialsMap.put(f, f);
            }
            this.specialsMap = nspecialsMap;
            this.roots = nroots;
            return this.roots;
        } finally {
            LogV3.I().getDefaultLogger().info("Roots: " + (System.currentTimeMillis() - t));
        }
    }

    private File[] getBaseFolders() {
        return AccessController.doPrivileged(new PrivilegedAction<File[]>() {
            public File[] run() {
                try {
                    return (File[]) ShellFolderWrapper.get("fileChooserComboBoxFolders");
                } catch (final Throwable e) {
                    LogV3.I().getDefaultLogger().log(e);
                    return null;
                }
            }
        });
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
        final File ret = this.specialsMap.get(f);
        return ret != null ? ret : f;
    }

    private void mount(final File path, final LinkedHashSet<File> files) {
        if (path.exists() && path.isDirectory()) {
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
