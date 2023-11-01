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

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.HashSet;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.appwork.utils.Hash;

/**
 * @author daniel
 *
 */
public class ZipIOWriter {
    protected ZipOutputStream zipStream  = null;
    protected OutputStream    fileStream = null;
    protected File            zipFile    = null;
    protected final byte[]    buf        = new byte[16384];

    public ZipIOWriter(final ByteArrayOutputStream stream) throws FileNotFoundException, ZipIOException {
        this.fileStream = stream;
        this.zipStream = new ZipOutputStream(this.fileStream);
    }

    /**
     * constructor for ZipIOWriter
     *
     * @param zipFile
     *            zipFile we want create (does not overwrite existing files!)
     * @throws FileNotFoundException
     * @throws ZipIOException
     */
    public ZipIOWriter(final File zipFile) throws FileNotFoundException, ZipIOException {
        this.zipFile = zipFile;
        this.openZip(false);
    }

    /**
     * constructor for ZipIOWriter
     *
     * @param zipFile
     *            zipFile we want create
     * @param overwrite
     *            overwrite existing ziFiles?
     * @throws FileNotFoundException
     * @throws ZipIOException
     */
    public ZipIOWriter(final File zipFile, final boolean overwrite) throws FileNotFoundException, ZipIOException {
        this.zipFile = zipFile;
        this.openZip(overwrite);
    }

    public ZipIOWriter(final OutputStream stream) throws FileNotFoundException, ZipIOException {
        this.fileStream = stream;
        this.zipStream = new ZipOutputStream(this.fileStream);
    }

    /**
     * add given File (File or Directory) to this ZipFile
     *
     * @param add
     *            File to add
     * @param compress
     *            compress or store
     * @param path
     *            customized path
     * @throws ZipIOException
     * @throws IOException
     */
    public synchronized void add(final File add, final boolean compress, final String path) throws ZipIOException, IOException {
        if (add == null || !add.exists()) {
            throw new ZipIOException("add " + add.getAbsolutePath() + " invalid");
        }
        if (add.isFile()) {
            this.addFileInternal(add, compress, path);
        } else if (add.isDirectory()) {
            this.addDirectoryInternal(add, compress, path);
        } else {
            throw new ZipIOException("add " + add.getAbsolutePath() + " invalid");
        }
    }

    public synchronized void addByteArry(final byte[] data, final boolean compress, final String path, final String name) throws IOException, ZipIOException {
        boolean zipEntryAdded = false;
        try {
            if (data == null) {
                throw new ZipIOException("data array is invalid");
            }
            final String fullPath = (path != null && path.trim().length() > 0 ? path + "/" : "") + name;
            if (!dupes.add(fullPath)) {
                throw new IOException("Cannot add the same path multiple times...");
            }
            final ZipEntry zipAdd = new ZipEntry(fullPath);
            zipAdd.setSize(data.length);
            if (compress) {
                zipAdd.setMethod(ZipEntry.DEFLATED);
            } else {
                zipAdd.setMethod(ZipEntry.STORED);
                zipAdd.setCompressedSize(data.length);
                /* STORED must have a CRC32! */
                zipAdd.setCrc(Hash.getCRC32(data));
            }
            this.zipStream.putNextEntry(zipAdd);
            zipEntryAdded = true;
            this.zipStream.write(data, 0, data.length);
            notify(zipAdd, data.length, data.length);
        } catch (IOException e) {
            throw new IOException("Path:" + path + "|Name:" + name, e);
        } finally {
            if (zipEntryAdded) {
                this.zipStream.closeEntry();
            }
        }
    }

    public ZipOutputStream getZipStream() {
        return zipStream;
    }

    /**
     * add given Directory to this ZipFile
     *
     * @param addDirectory
     *            Directory to add
     * @param compress
     *            compress or store
     * @param path
     *            customized path
     * @throws ZipIOException
     * @throws IOException
     */
    public synchronized void addDirectory(final File addDirectory, final boolean compress, final String path) throws ZipIOException, IOException {
        this.addDirectoryInternal(addDirectory, compress, path);
    }

    protected void addDirectoryInternal(final File addDirectory, final boolean compress, final String path) throws ZipIOException, IOException {
        if (addDirectory == null) {
            throw new ZipIOException("addDirectory invalid: null");
        }
        if (!addDirectory.exists() && throwExceptionOnFileGone(addDirectory)) {
            throw new ZipIOException("addDirectory " + addDirectory.getAbsolutePath() + " invalid: does not exist");
        }
        final File[] list = addDirectory.listFiles();
        if (list != null) {
            for (final File add : list) {
                if (add.isFile()) {
                    this.addFileInternal(add, compress, (path != null && path.trim().length() > 0 ? path + "/" : "") + addDirectory.getName());
                } else if (add.isDirectory()) {
                    this.addDirectoryInternal(add, compress, (path != null && path.trim().length() > 0 ? path + "/" : "") + addDirectory.getName());
                } else if (!add.exists() && throwExceptionOnFileGone(add)) {
                    throw new ZipIOException("addDirectory: " + add.getAbsolutePath() + "(File:" + add.isFile() + "|Directory:" + add.isDirectory() + ")");
                }
            }
        }
    }

    protected boolean throwExceptionOnFileGone(File file) {
        return true;
    }

    protected void notify(final ZipEntry entry, final long bytesWrite, final long bytesProcessed) {
    }

    /**
     * Add file
     *
     * @param addFile
     * @param compress
     * @param fullPath
     *            full path incl. filename
     * @throws ZipIOException
     * @throws IOException
     * @throws FileNotFoundException
     */
    public synchronized void addFile(final File addFile, final boolean compress, final String fullPath) throws ZipIOException, IOException, FileNotFoundException {
        FileInputStream fin = null;
        boolean zipEntryAdded = false;
        try {
            if (addFile == null) {
                throw new ZipIOException("addFile invalid:null");
            }
            if (!dupes.add(fullPath)) {
                throw new IOException("Cannot add the same path multiple times...");
            }
            fin = new FileInputStream(addFile);
            final ZipEntry zipAdd = new ZipEntry(fullPath);
            final long size = addFile.length();
            zipAdd.setSize(size);
            if (compress) {
                zipAdd.setMethod(ZipEntry.DEFLATED);
            } else {
                zipAdd.setMethod(ZipEntry.STORED);
                zipAdd.setCompressedSize(addFile.length());
                /* STORED must have a CRC32! */
                zipAdd.setCrc(Hash.getCRC32(addFile));
            }
            long total = 0;
            this.zipStream.putNextEntry(zipAdd);
            int len;
            while ((len = fin.read(this.buf)) > 0) {
                this.zipStream.write(this.buf, 0, len);
                total += len;
                notify(zipAdd, len, total);
            }
            zipEntryAdded = true;
        } catch (FileNotFoundException e) {
            if (addFile.exists() == false) {
                if (throwExceptionOnFileGone(addFile)) {
                    throw e;
                }
                return;
            }
            throw e;
        } catch (IOException e) {
            throw new IOException("File:" + addFile + "|Path:" + fullPath, e);
        } finally {
            try {
                if (fin != null) {
                    fin.close();
                }
            } catch (final Throwable e) {
            }
            if (zipEntryAdded) {
                this.zipStream.closeEntry();
            }
        }
    }

    private void addFileInternal(final File addFile, final boolean compress, final String path) throws ZipIOException, IOException {
        final String fullPath = (path != null && path.trim().length() > 0 ? path + "/" : "") + addFile.getName();
        this.addFile(addFile, compress, fullPath);
    }

    /**
     * add given File to this ZipFile
     *
     * @param addFile
     *            File to add
     * @param compress
     *            compress or store
     * @param path
     *            customized path without filename!
     * @throws ZipIOException
     * @throws IOException
     */
    public synchronized void addFileToPath(final File addFile, final boolean compress, final String path) throws ZipIOException, IOException {
        this.addFileInternal(addFile, compress, path);
    }

    private HashSet<String> dupes = new HashSet<String>();

    public synchronized void addFolder(String fullPath) throws IOException {
        if (!fullPath.endsWith("/")) {
            fullPath = fullPath + "/";
        }
        if (!dupes.add(fullPath)) {
            return;
        }
        boolean zipEntryAdded = false;
        try {
            final ZipEntry zipAdd = new ZipEntry(fullPath);
            this.zipStream.putNextEntry(zipAdd);
            notify(zipAdd, -1, -1);
            zipEntryAdded = true;
        } finally {
            if (zipEntryAdded) {
                this.zipStream.closeEntry();
            }
        }
    }

    /**
     * closes the ZipFile
     *
     * @throws Throwable
     */
    public synchronized void close() throws IOException {
        try {
            try {
                this.zipStream.flush();
            } catch (final Throwable e2) {
            }
            try {
                this.fileStream.flush();
            } catch (final Throwable e2) {
            }
            try {
                this.zipStream.close();
            } catch (final Throwable e2) {
            }
            try {
                if (this.fileStream instanceof FileOutputStream) {
                    ((FileOutputStream) this.fileStream).getChannel().force(true);
                }
            } catch (final Throwable e2) {
            }
            try {
                this.fileStream.close();
            } catch (final Throwable e2) {
            }
        } finally {
            this.zipStream = null;
            this.fileStream = null;
        }
    }

    /**
     * opens the zipFile for further use
     *
     * @param overwrite
     *            overwrite existing zipFiles?
     * @throws ZipIOException
     * @throws FileNotFoundException
     */
    private void openZip(final boolean overwrite) throws ZipIOException, FileNotFoundException {
        if (this.fileStream != null && this.zipStream != null) {
            return;
        }
        if (this.zipFile == null || this.zipFile.isDirectory()) {
            throw new ZipIOException("invalid zipFile");
        }
        if (this.zipFile.exists() && !overwrite) {
            throw new ZipIOException("zipFile already exists");
        }
        this.fileStream = new FileOutputStream(this.zipFile);
        this.zipStream = new ZipOutputStream(this.fileStream);
    }
}
