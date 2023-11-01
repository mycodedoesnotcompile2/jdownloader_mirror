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
package org.appwork.utils.os.mime;

import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.filechooser.FileSystemView;

import org.appwork.loggingv3.LogV3;
import org.appwork.sunwrapper.WrapperNotAvailableException;
import org.appwork.sunwrapper.sun.awt.shell.ShellFolderWrapper;
import org.appwork.utils.Application;
import org.appwork.utils.IO;
import org.appwork.utils.IO.SYNC;
import org.appwork.utils.JVMVersion;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.ImageProvider.ImageProvider;
import org.appwork.utils.images.IconIO;

public class MimeWindows extends MimeDefault {
    @Override
    public Icon getFileIcon(final String extension, final int width, final int height) throws IOException {
        final String iconKey = "osFileIcon_" + super.getIconKey(extension, width, height);
        Icon ret = super.getCacheIcon(iconKey);
        if (ret != null) {
            return ret;
        }
        final Boolean registryContainsFileIcon = registryContainsFileIcon(extension);
        if (Boolean.FALSE.equals(registryContainsFileIcon)) {
            ret = super.getFileIcon(extension, width, height);
        } else {
            final File path = Application.getTempResource("images/" + extension + ".png");
            if (path.getParentFile().isDirectory()) {
                // woraround a bug we had until 24.06.2013.. created folders
                // instead of files
                path.getParentFile().delete();
            }
            if (!path.getParentFile().exists()) {
                path.getParentFile().mkdirs();
            }
            try {
                if (path.isFile()) {
                    try {
                        final BufferedImage image = ImageProvider.read(path);
                        ret = image != null ? new ImageIcon(image) : null;
                    } catch (IOException e) {
                        LogV3.I().getDefaultLogger().exception("Image:" + path, e);
                    }
                }
                if (ret == null) {
                    File tempFile = null;
                    try {
                        tempFile = Application.getTempResource("icon" + System.nanoTime() + "." + extension);
                        if (!tempFile.exists()) {
                            IO.writeToFile(tempFile, new byte[0], SYNC.NONE);
                        }
                        if (JVMVersion.isMinimum(JVMVersion.JAVA_17)) {
                            try {
                                final Icon icon = ReflectionUtils.invoke(FileSystemView.class, "getSystemIcon", FileSystemView.getFileSystemView(), Icon.class, new Object[] { tempFile, 32, 32 });
                                ret = icon != null ? ImageProvider.toImageIcon(icon) : null;
                            } catch (InvocationTargetException e) {
                                LogV3.log(e);
                            }
                        }
                        if (ret == null) {
                            try {
                                try {
                                    final Image image = ShellFolderWrapper.getIcon(tempFile);
                                    if (image != null) {
                                        ret = new ImageIcon(image);
                                        if (Boolean.TRUE.equals(registryContainsFileIcon)) {
                                            final FileOutputStream fos = new FileOutputStream(path);
                                            try {
                                                ImageProvider.writeImage(IconIO.toBufferedImage(image), "png", fos);
                                            } finally {
                                                fos.close();
                                            }
                                        }
                                    }
                                } catch (WrapperNotAvailableException e) {
                                    LogV3.log(e);
                                }
                            } catch (final Throwable e) {
                                LogV3.I().getDefaultLogger().exception("TempFile:" + tempFile, e);
                            }
                        }
                        if (ret == null) {
                            final Icon icon = FileSystemView.getFileSystemView().getSystemIcon(tempFile);
                            ret = icon != null ? ImageProvider.toImageIcon(icon) : null;
                        }
                    } finally {
                        if (tempFile != null && tempFile.exists() && !tempFile.delete()) {
                            tempFile.deleteOnExit();
                        }
                    }
                }
            } catch (final Throwable e) {
                LogV3.log(e);
            }
        }
        if (ret == null) {
            ret = super.getFileIcon(extension, width, height);
        }
        if (ret != null && (ret.getIconWidth() != width || ret.getIconHeight() != height)) {
            ret = IconIO.getScaledInstance(ret, width, height);
        }
        super.saveIconCache(iconKey, ret);
        return ret;
    }

    protected Boolean registryContainsFileIcon(final String extension) {
        return null;
    }

}