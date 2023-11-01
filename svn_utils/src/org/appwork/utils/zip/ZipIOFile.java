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
package org.appwork.utils.zip;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.zip.ZipEntry;

/**
 * @author daniel
 * 
 */
public class ZipIOFile {

    private final String name;
    private final ZipEntry file;
    private final ZipIOReader zipFile;
    private final ZipIOFile parent;
    private final boolean isFile;
    private ArrayList<ZipIOFile> files = new ArrayList<ZipIOFile>();

    /**
     * constructor for ZipIOFile
     * 
     * @param name
     *            name of the ZipIOFile node
     * @param file
     *            ZipEntry that represents the given File
     * @param zipFile
     *            ZipIOReader for this ZipIOFile
     * @param parent
     *            ZipIOFile that represents the parent of this node or null if
     *            no parent available
     */
    protected ZipIOFile(String name, ZipEntry file, ZipIOReader zipFile, ZipIOFile parent) {
        this.name = name;
        this.zipFile = zipFile;
        this.file = file;
        this.parent = parent;
        if (file == null) {
            this.isFile = false;
        } else {
            this.isFile = !file.isDirectory();
        }
    }

    /**
     * returns ZipIOFile list for all the filesif this ZipIOFile represents a
     * directory(internal use for ZipIOReader)
     * 
     * @return
     */
    protected final ArrayList<ZipIOFile> getFilesInternal() {
        return files;
    }

    /**
     * returns ZipIOFile list for all the filesif this ZipIOFile represents a
     * directory
     * 
     * @return
     */
    public final ZipIOFile[] getFiles() {
        return files.toArray(new ZipIOFile[files.size()]);
    }

    /**
     * returns the name of this ZipIOFile
     * 
     * @return
     */
    public final String getName() {
        return name;
    }

    /**
     * is this ZipIOFile a directory
     * 
     * @return
     */
    public final boolean isDirectory() {
        return !isFile;
    }

    /**
     * returns parent ZipIOFile
     * 
     * @return
     */
    public final ZipIOFile getParent() {
        return parent;
    }

    /**
     * is this ZipIOFile a file
     * 
     * @return
     */
    public final boolean isFile() {
        return isFile;
    }

    /**
     * returns the absolutepath of this ZipIOFile
     * 
     * @return
     */
    public final String getAbsolutePath() {
        if (file == null) return (parent != null ? parent.getAbsolutePath() : "") + name + "/";
        return file.getName();
    }

    /**
     * returns filesize for this ZipIOFile
     * 
     * @return filesize or 0 if this ZipIOFile is a directory
     */
    public final long getSize() {
        if (!isFile) return 0;
        return file.getSize();
    }

    /**
     * returns CRC32 for this ZipIOFile
     * 
     * @return CRC32 or 0 if this ZipIOFile is a directory
     */
    public final long getCRC32() {
        if (!isFile) return 0;
        return file.getCrc();
    }

    /**
     * returns InputStream for this ZipIOFile
     * 
     * @return InputStream or null if this ZipIOFile is a directory
     */
    public final InputStream getInputStream() throws IOException, ZipIOException {
        if (!isFile) return null;
        return zipFile.getInputStream(file);
    }

    @Override
    public String toString() {
        return getAbsolutePath();
    }

    /**
     * extracts this ZipIOFile to given output File
     * 
     * @param output
     *            File to extract this ZipIOFile to
     * @throws ZipIOException
     * @throws IOException
     */
    public void extract(File output) throws ZipIOException, IOException {
        if (isFile) {
            zipFile.extract(file, output);
        } else {
            throw new ZipIOException("Cannot extract a directory", file);
        }
    }

}
