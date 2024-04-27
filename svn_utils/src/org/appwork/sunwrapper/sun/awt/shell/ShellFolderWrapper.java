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
package org.appwork.sunwrapper.sun.awt.shell;

import java.awt.Image;
import java.io.File;
import java.io.FileNotFoundException;
import java.lang.reflect.InvocationTargetException;

import javax.swing.Icon;
import javax.swing.filechooser.FileSystemView;

import org.appwork.loggingv3.LogV3;
import org.appwork.sunwrapper.WrapperNotAvailableException;
import org.appwork.utils.DebugMode;
import org.appwork.utils.JVMVersion;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.images.IconIO;

/**
 * @author Thomas
 *
 */
public class ShellFolderWrapper {
    private static final FileSystemView view                     = FileSystemView.getFileSystemView();
    private static boolean              shellFolderGetIconAccess = true;

    public static interface ShellFolderSource {
        public File getShellFolder();
    }

    /**
     * @param file
     * @throws WrapperNotAvailableException
     * @throws FileNotFoundException
     */
    public static Image getIcon(File file) throws WrapperNotAvailableException, FileNotFoundException {
        try {
            if (file == null) {
                return null;
            } else if (!file.exists()) {
                throw new FileNotFoundException(file.getAbsolutePath());
            } else if (shellFolderGetIconAccess) {
                final File shellFolder = getShellFolder(file);
                if (isInstanceof(shellFolder)) {
                    return ReflectionUtils.invoke(shellFolder.getClass(), "getIcon", shellFolder, Image.class, true);
                }
            }
        } catch (FileNotFoundException e) {
            throw e;
        } catch (InvocationTargetException e) {
            shellFolderGetIconAccess = false;
            DebugMode.logInIDEOnly(LogV3.I().getLogger(ShellFolderWrapper.class.getName()), e);
        }
        final Icon icon = view.getSystemIcon(file);
        if (icon != null) {
            return IconIO.toImage(icon);
        } else {
            return null;
        }
    }

    /**
     * @param f
     * @return
     * @throws FileNotFoundException
     */
    public static File getShellFolderIfAvailable(File file) throws FileNotFoundException {
        return getShellFolder(file);
    }

    /**
     * @param f
     * @return
     * @throws InvocationTargetException
     */
    public static boolean isInstanceof(File f) {
        return ReflectionUtils.isInstanceOf("sun.awt.shell.ShellFolder", f);
    }

    private static boolean shellFolderGetAccess = true;

    /**
     * @param string
     * @return
     */
    public static Object get(String key) {
        try {
            if ("roots".equals(key)) {
                return view.getRoots();
            } else if ("fileChooserDefaultFolder".equals(key)) {
                return view.getDefaultDirectory();
            } else if (JVMVersion.isMinimum(JVMVersion.JAVA_9) && "fileChooserComboBoxFolders".equals(key)) {
                return ReflectionUtils.invoke(view.getClass(), "getChooserComboBoxFiles", view, File[].class);
            } else if (JVMVersion.isMinimum(JVMVersion.JAVA_12) && "fileChooserShortcutPanelFolders".equals(key)) {
                return ReflectionUtils.invoke(view.getClass(), "getChooserShortcutPanelFiles", view, File[].class);
            } else if (shellFolderGetAccess) {
                return ReflectionUtils.invoke("sun.awt.shell.ShellFolder", "get", null, String.class, key);
            }
        } catch (InvocationTargetException e) {
            shellFolderGetAccess = false;
            DebugMode.logInIDEOnly(LogV3.I().getLogger(ShellFolderWrapper.class.getName()), e);
        }
        return null;
    }

    public static File getShellFolderViaFileSystem(File file) throws FileNotFoundException {
        if (isInstanceof(file)) {
            return file;
        } else if (!file.exists()) {
            throw new FileNotFoundException(file.getAbsolutePath());
        } else {
            final File parent = view.getParentDirectory(file);
            if (parent != null) {
                boolean initialShellFolderInstanceCheck = true;
                final File[] children = view.getFiles(parent, true);
                if (children != null && children.length > 0) {
                    for (final File child : children) {
                        if (initialShellFolderInstanceCheck) {
                            // initial check if childrend are instanceof ShellFolder
                            if (!isInstanceof(child)) {
                                return null;
                            } else {
                                initialShellFolderInstanceCheck = false;
                            }
                        }
                        if (child.equals(file)) {
                            if (isInstanceof(child)) {
                                return child;
                            } else {
                                return null;
                            }
                        }
                    }
                }
            }
            return null;
        }
    }

    private static boolean shellFolderGetShellFolderAccess = true;

    public static File getInstanceShellFolder(File file) {
        if (file == null) {
            return null;
        } else if (isInstanceof(file)) {
            return file;
        } else if (file instanceof ShellFolderSource) {
            final File source = ((ShellFolderSource) file).getShellFolder();
            if (source != null) {
                return getInstanceShellFolder(source);
            }
        }
        return null;
    }

    /**
     * @param file
     * @return @throws FileNotFoundException @throws
     */
    public static File getShellFolder(File file) throws FileNotFoundException {
        try {
            final File shellFolder = getInstanceShellFolder(file);
            if (shellFolder != null) {
                return shellFolder;
            } else if (!file.exists()) {
                throw new FileNotFoundException(file.getAbsolutePath());
            } else if (shellFolderGetShellFolderAccess) {
                return ReflectionUtils.invoke(FileSystemView.class, "getShellFolder", view, File.class, file);
            }
        } catch (InvocationTargetException e) {
            shellFolderGetShellFolderAccess = false;
            DebugMode.logInIDEOnly(LogV3.I().getLogger(ShellFolderWrapper.class.getName()), e);
        }
        final File shellFolderViaFileSystem = getShellFolderViaFileSystem(file);
        if (shellFolderViaFileSystem != null) {
            return shellFolderViaFileSystem;
        } else {
            return file;
        }
    }
}
