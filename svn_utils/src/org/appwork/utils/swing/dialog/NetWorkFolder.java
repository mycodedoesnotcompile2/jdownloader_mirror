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
import java.io.FileFilter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.nio.file.Path;
import java.util.HashMap;

import javax.swing.filechooser.FileSystemView;

import org.appwork.sunwrapper.sun.awt.shell.ShellFolderWrapper;
import org.appwork.sunwrapper.sun.awt.shell.ShellFolderWrapper.ShellFolderSource;

/**
 * @author Thomas
 *
 */
public class NetWorkFolder extends File implements ShellFolderSource {
    private final File            networkFolder;
    private File[]                fileList;
    private HashMap<String, File> map;

    /**
     * @param networkFolder
     */
    public NetWorkFolder(final File networkFolder) {
        super(networkFolder, "");
        this.networkFolder = networkFolder;
    }

    @Override
    public boolean canExecute() {
        return this.networkFolder.canExecute();
    }

    @Override
    public boolean canRead() {
        return this.networkFolder.canRead();
    }

    @Override
    public boolean canWrite() {
        return this.networkFolder.canWrite();
    }

    @Override
    public int compareTo(final File pathname) {
        return this.networkFolder.compareTo(pathname);
    }

    @Override
    public boolean createNewFile() throws IOException {
        return this.networkFolder.createNewFile();
    }

    @Override
    public boolean delete() {
        return this.networkFolder.delete();
    }

    @Override
    public void deleteOnExit() {
        this.networkFolder.deleteOnExit();
    }

    @Override
    public boolean equals(final Object obj) {
        return this.networkFolder.equals(obj);
    }

    @Override
    public boolean exists() {
        return this.networkFolder.exists();
    }

    /**
     * @param absolutePath
     * @return
     */
    public File get(final String absolutePath) {
        if (this.map != null) {
            return this.map.get(absolutePath);
        } else {
            return null;
        }
    }

    @Override
    public File getAbsoluteFile() {
        return this.networkFolder.getAbsoluteFile();
    }

    @Override
    public String getAbsolutePath() {
        return this.networkFolder.getAbsolutePath();
    }

    @Override
    public File getCanonicalFile() throws IOException {
        return this.networkFolder.getCanonicalFile();
    }

    @Override
    public String getCanonicalPath() throws IOException {
        return this.networkFolder.getCanonicalPath();
    }

    @Override
    public long getFreeSpace() {
        return this.networkFolder.getFreeSpace();
    }

    @Override
    public String getName() {
        return this.networkFolder.getName();
    }

    @Override
    public String getParent() {
        return this.networkFolder.getParent();
    }

    @Override
    public File getParentFile() {
        return this.networkFolder.getParentFile();
    }

    @Override
    public String getPath() {
        return this.networkFolder.getPath();
    }

    @Override
    public long getTotalSpace() {
        return this.networkFolder.getTotalSpace();
    }

    @Override
    public long getUsableSpace() {
        return this.networkFolder.getUsableSpace();
    }

    @Override
    public int hashCode() {
        return this.networkFolder.hashCode();
    }

    @Override
    public boolean isAbsolute() {
        return this.networkFolder.isAbsolute();
    }

    @Override
    public boolean isDirectory() {
        return this.networkFolder.isDirectory();
    }

    @Override
    public boolean isFile() {
        return this.networkFolder.isFile();
    }

    @Override
    public boolean isHidden() {
        return this.networkFolder.isHidden();
    }

    @Override
    public long lastModified() {
        return this.networkFolder.lastModified();
    }

    @Override
    public long length() {
        return this.networkFolder.length();
    }

    @Override
    public String[] list() {
        return this.networkFolder.list();
    }

    @Override
    public String[] list(final FilenameFilter filter) {
        return this.networkFolder.list(filter);
    }

    @Override
    public File[] listFiles() {
        return this.listFiles(true);
    }

    /**
     * @param useFileHidingsvn
     *
     * @return
     */
    public File[] listFiles(final boolean useFileHiding) {
        final File[] fileList = FileSystemView.getFileSystemView().getFiles(networkFolder, useFileHiding);
        if (fileList != null) {
            final HashMap<String, File> map = new HashMap<String, File>();
            for (final File f : fileList) {
                map.put(f.getAbsolutePath(), f);
            }
            this.map = map;
        }
        this.fileList = fileList;
        return fileList;
    }

    @Override
    public File[] listFiles(final FileFilter filter) {
        return this.networkFolder.listFiles(filter);
    }

    @Override
    public File[] listFiles(final FilenameFilter filter) {
        return this.networkFolder.listFiles(filter);
    }

    /**
     * @return
     */
    public File[] listFilesAsynch() {
        return this.fileList;
    }

    @Override
    public boolean mkdir() {
        return this.networkFolder.mkdir();
    }

    @Override
    public boolean mkdirs() {
        return this.networkFolder.mkdirs();
    }

    @Override
    public boolean renameTo(final File dest) {
        return this.networkFolder.renameTo(dest);
    }

    @Override
    public boolean setExecutable(final boolean executable) {
        return this.networkFolder.setExecutable(executable);
    }

    @Override
    public boolean setExecutable(final boolean executable, final boolean ownerOnly) {
        return this.networkFolder.setExecutable(executable, ownerOnly);
    }

    @Override
    public boolean setLastModified(final long time) {
        return this.networkFolder.setLastModified(time);
    }

    @Override
    public boolean setReadable(final boolean readable) {
        return this.networkFolder.setReadable(readable);
    }

    @Override
    public boolean setReadable(final boolean readable, final boolean ownerOnly) {
        return this.networkFolder.setReadable(readable, ownerOnly);
    }

    @Override
    public boolean setReadOnly() {
        return this.networkFolder.setReadOnly();
    }

    @Override
    public boolean setWritable(final boolean writable) {
        return this.networkFolder.setWritable(writable);
    }

    @Override
    public boolean setWritable(final boolean writable, final boolean ownerOnly) {
        return this.networkFolder.setWritable(writable, ownerOnly);
    }

    @Override
    public Path toPath() {
        return this.networkFolder.toPath();
    }

    @Override
    public String toString() {
        return this.networkFolder.toString();
    }

    @Override
    public URI toURI() {
        return this.networkFolder.toURI();
    }

    @Override
    public URL toURL() throws MalformedURLException {
        return this.networkFolder.toURI().toURL();
    }

    @Override
    public File getShellFolder() {
        if (ShellFolderWrapper.isInstanceof(networkFolder)) {
            return networkFolder;
        } else {
            return null;
        }
    }
}
