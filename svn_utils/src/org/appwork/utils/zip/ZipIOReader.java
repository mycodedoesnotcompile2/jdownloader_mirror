/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@apppwork.org
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

import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.RandomAccessFile;
import java.io.UnsupportedEncodingException;
import java.security.GeneralSecurityException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.PublicKey;
import java.security.Signature;
import java.security.SignatureException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Map;
import java.util.zip.CRC32;
import java.util.zip.CheckedInputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;

import org.appwork.serializer.Deser;
import org.appwork.storage.TypeRef;
import org.appwork.storage.commonInterface.SerializerException;
import org.appwork.utils.Application;
import org.appwork.utils.Files;
import org.appwork.utils.IO;
import org.appwork.utils.JVMVersion;
import org.appwork.utils.JavaVersion;
import org.appwork.utils.encoding.Base64;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.net.NullOutputStream;

public class ZipIOReader {
    private File       zipFile               = null;
    private ZipFile    zip                   = null;
    private ZipIOFile  rootFS                = null;
    private boolean    autoCreateExtractPath = true;
    private boolean    overwrite             = false;
    private boolean    autoCreateSubDirs     = true;
    private byte[]     byteArray             = null;
    private int        zipEntriesSize        = -1;
    private ZipEntry[] zipEntries            = null;
    private boolean    breakOnError          = true;
    private Signature  signature;
    private PublicKey  publicKey;
    private byte[]     salt;
    private File       tmpZipFile;

    /**
     * * @deprecated
     */
    @Deprecated
    public ZipIOReader(final byte[] byteArray) {
        this.byteArray = byteArray;
    }

    /**
     * open the zipFile for this ZipIOReader
     *
     * @param zipFile
     *            the zipFile we want to open
     * @throws ZipIOException
     * @throws ZipException
     * @throws IOException
     */
    public ZipIOReader(final File zipFile) throws ZipIOException {
        this.zipFile = zipFile;
        this.openZip();
    }

    /**
     * closes the ZipFile
     *
     * @throws IOException
     */
    public synchronized void close() throws IOException {
        try {
            if (this.zip != null) {
                this.zip.close();
            }
            publicKey = null;
            signature = null;
            if (tmpZipFile != null) {
                tmpZipFile.delete();
                tmpZipFile = null;
            }
        } finally {
            this.byteArray = null;
            this.zip = null;
        }
    }

    /**
     * extract given ZipEntry to output File
     *
     * @param entry
     *            ZipEntry to extract
     * @param output
     *            File to extract to
     * @return
     * @throws ZipIOException
     * @throws IOException
     */
    public synchronized java.util.List<File> extract(final ZipEntry entry, final File output) throws ZipIOException {
        final java.util.List<File> ret = new ArrayList<File>();
        if (output.exists() && output.isDirectory()) {
            if (this.isOverwrite()) {
                try {
                    Files.deleteRecursive(output);
                } catch (IOException e) {
                    throw new ZipIOException(e);
                }
                if (output.exists()) {
                    if (this.isBreakOnError()) {
                        throw new ZipIOException("Cannot extract File to Directory " + output);
                    } else {
                        org.appwork.loggingv3.LogV3.severe("Cannot extract File to Directory " + output);
                    }
                }
            }
            if (output.exists() && output.isDirectory()) {
                org.appwork.loggingv3.LogV3.finer("Skipped extraction: directory exists: " + output);
                return ret;
            }
        }
        if (output.exists()) {
            if (this.isOverwrite()) {
                output.delete();
                if (output.exists()) {
                    if (this.isBreakOnError()) {
                        throw new ZipIOException("Cannot overwrite File " + output);
                    } else {
                        org.appwork.loggingv3.LogV3.severe("Cannot overwrite File " + output);
                    }
                }
            }
            if (output.exists()) {
                org.appwork.loggingv3.LogV3.finer("Skipped extraction: file exists: " + output);
                return ret;
            }
        }
        if (!output.getParentFile().exists()) {
            if (this.isAutoCreateSubDirs()) {
                output.getParentFile().mkdirs();
                ret.add(output.getParentFile());
                if (!output.getParentFile().exists()) {
                    if (this.isBreakOnError()) {
                        throw new ZipIOException("Cannot create folder for File " + output);
                    } else {
                        org.appwork.loggingv3.LogV3.severe("Cannot create folder for File " + output);
                    }
                }
            }
            if (!output.getParentFile().exists()) {
                org.appwork.loggingv3.LogV3.finer("Skipped extraction: cannot create dir: " + output);
                return ret;
            }
        }
        OutputStream os = null;
        ZipIOException exception = null;
        try {
            os = new BufferedOutputStream(new FileOutputStream(output));
            this.extract(entry, os);
        } catch (FileNotFoundException e) {
            exception = ZipIOException.wrapOrAddSurpressed(exception, e, entry);
        } finally {
            if (os != null) {
                try {
                    os.close();
                } catch (IOException e) {
                    exception = ZipIOException.wrapOrAddSurpressed(exception, e, entry);
                }
            }
            if (exception != null) {
                throw exception;
            }
        }
        ret.add(output);
        return ret;
    }

    /**
     * @param entry
     * @param fileOutputStream
     * @throws ZipIOException
     * @throws IOException
     */
    public void extract(final ZipEntry entry, final OutputStream stream) throws ZipIOException {
        if (entry.isDirectory()) {
            if (this.isBreakOnError()) {
                throw new ZipIOException("Cannot extract a directory", entry);
            } else {
                org.appwork.loggingv3.LogV3.severe("Cannot extract a directory " + entry.getName());
            }
        }
        if (logger != null) {
            logger.info("Extract " + entry.getName() + " -> " + stream.getClass().getSimpleName());
        }
        CheckedInputStream in = null;
        ZipIOException exception = null;
        try {
            final InputStream is = this.getInputStream(entry);
            in = new CheckedInputStream(is, new CRC32());
            final byte[] buffer = new byte[32767];
            int len = 0;
            long total = 0;
            while ((len = in.read(buffer)) != -1) {
                if (len == 0) {
                    continue;
                }
                stream.write(buffer, 0, len);
                total += len;
                notify(entry, len, total);
            }
            crcCheck(entry, in);
        } catch (IOException e) {
            exception = ZipIOException.wrapOrAddSurpressed(exception, e, entry);
        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException e) {
                    exception = ZipIOException.wrapOrAddSurpressed(exception, e, entry);
                }
            }
            if (stream != null) {
                try {
                    stream.close();
                } catch (IOException e) {
                    exception = ZipIOException.wrapOrAddSurpressed(exception, e, entry);
                }
            }
            if (exception != null) {
                throw exception;
            }
        }
    }

    protected void crcCheck(final ZipEntry entry, CheckedInputStream in) throws ZipIOException {
        if (entry.getCrc() != -1 && entry.getCrc() != in.getChecksum().getValue()) {
            if (this.isBreakOnError()) {
                throw new ZipIOException("CRC32 Failed", entry);
            } else {
                org.appwork.loggingv3.LogV3.severe("CRC32 Failed " + entry);
            }
        }
    }

    protected void notify(final ZipEntry entry, final long bytesWrite, final long bytesProcessed) {
    }

    public synchronized java.util.List<File> extractTo(final File outputDirectory) throws ZipIOException {
        if (outputDirectory.exists() && outputDirectory.isFile()) {
            if (this.isBreakOnError()) {
                throw new ZipIOException("cannot extract to a file " + outputDirectory);
            } else {
                org.appwork.loggingv3.LogV3.severe("cannot extract to a file " + outputDirectory);
            }
        }
        if (!outputDirectory.exists() && !(this.autoCreateExtractPath && outputDirectory.mkdirs())) {
            if (this.isBreakOnError()) {
                throw new ZipIOException("could not create outputDirectory " + outputDirectory);
            } else {
                org.appwork.loggingv3.LogV3.severe("could not create outputDirectory " + outputDirectory);
            }
        }
        final java.util.List<File> ret = new ArrayList<File>();
        for (final ZipEntry entry : this.getZipFiles()) {
            final File out = new File(outputDirectory, entry.getName());
            if (entry.isDirectory()) {
                if (!out.exists()) {
                    if (this.isAutoCreateSubDirs()) {
                        if (!out.mkdir()) {
                            if (this.isBreakOnError()) {
                                throw new ZipIOException("could not create outputDirectory " + out);
                            } else {
                                org.appwork.loggingv3.LogV3.severe("could not create outputDirectory " + out);
                            }
                        }
                        ret.add(out);
                    } else {
                        org.appwork.loggingv3.LogV3.finer("SKipped creatzion of: " + out);
                    }
                }
            } else {
                ret.addAll(this.extract(entry, out));
            }
        }
        return ret;
    }

    /**
     * find ZipIOFile that represents the Folder with given path
     *
     * @param path
     *            the path we search a ZipIOFile for
     * @param currentRoot
     *            currentRoot for the search
     * @return ZipIOFile if path is found, else null
     */
    private ZipIOFile getFolder(final String path, final ZipIOFile currentRoot) {
        if (path == null || currentRoot == null || !currentRoot.isDirectory()) {
            return null;
        }
        if (currentRoot.getAbsolutePath().equalsIgnoreCase(path)) {
            return currentRoot;
        }
        for (final ZipIOFile tmp : currentRoot.getFiles()) {
            if (tmp.isDirectory() && tmp.getAbsolutePath().equalsIgnoreCase(path)) {
                return tmp;
            } else if (tmp.isDirectory()) {
                final ZipIOFile ret = this.getFolder(path, tmp);
                if (ret != null) {
                    return ret;
                }
            }
        }
        return null;
    }

    /**
     * returns an InputStream for given ZipEntry
     *
     * @param entry
     *            ZipEntry we want an InputStream
     * @return InputStream for given ZipEntry
     * @throws ZipIOException
     * @throws IOException
     */
    public synchronized InputStream getInputStream(final ZipEntry entry) throws ZipIOException {
        if (entry == null) {
            throw new ZipIOException("invalid zipEntry");
        }
        ensureIndexIsVerified();
        verifyPathSignature(entry);
        // if (zip == null && signature != null) {
        // ensureZipFile();
        // }
        InputStream ret = null;
        try {
            if (this.zip != null) {
                ret = this.zip.getInputStream(entry);
            } else {
                ZipInputStream zis = null;
                boolean close = true;
                ZipIOException exception = null;
                try {
                    zis = new ZipInputStream(new ByteArrayInputStream(this.byteArray));
                    ZipEntry ze = null;
                    while ((ze = zis.getNextEntry()) != null) {
                        /* find the entry that matches */
                        final String name = ze.getName();
                        if (name.equals(entry.getName())) {
                            final ZipInputStream zis2 = zis;
                            close = false;
                            ret = new InputStream() {
                                @Override
                                public int available() throws IOException {
                                    return zis2.available();
                                }

                                @Override
                                public void close() throws IOException {
                                    zis2.close();
                                }

                                @Override
                                public synchronized void mark(final int readlimit) {
                                    zis2.mark(readlimit);
                                }

                                @Override
                                public boolean markSupported() {
                                    return zis2.markSupported();
                                }

                                @Override
                                public int read() throws IOException {
                                    return zis2.read();
                                }

                                @Override
                                public int read(final byte b[]) throws IOException {
                                    return zis2.read(b);
                                }

                                @Override
                                public int read(final byte b[], final int off, final int len) throws IOException {
                                    return zis2.read(b, off, len);
                                }

                                @Override
                                public synchronized void reset() throws IOException {
                                    zis2.reset();
                                }

                                @Override
                                public long skip(final long n) throws IOException {
                                    return zis2.skip(n);
                                }
                            };
                            break;
                        }
                    }
                } catch (final IOException e) {
                    exception = ZipIOException.wrapOrAddSurpressed(exception, e, entry);
                } finally {
                    if (close) {
                        if (zis != null) {
                            try {
                                zis.close();
                            } catch (IOException e) {
                                exception = ZipIOException.wrapOrAddSurpressed(exception, e, entry);
                            }
                        }
                    }
                }
                if (exception != null) {
                    throw exception;
                }
            }
            if (signature != null) {
                try {
                    String comment = entry.getComment();
                    signature.initVerify(publicKey);
                    Map<String, Object> cont = Deser.get().fromString(comment, TypeRef.MAP);
                    final byte[] contentSignatur = Base64.decode((String) cont.get(ZipIOWriter.C_SIG));
                    byte[] cSalt = Base64.decode((String) cont.get(ZipIOWriter.SIG_SALT));
                    signature.update(cSalt);
                    signature.update(salt);
                    ret = new FilterInputStream(ret) {
                        private boolean sigOk = false;

                        /**
                         * @see java.io.FilterInputStream#read()
                         */
                        @Override
                        public int read() throws IOException {
                            int ret = super.read();
                            if (ret < 0) {
                                // oef
                                finishSignature();
                            } else {
                                try {
                                    signature.update((byte) ret);
                                } catch (SignatureException e) {
                                    throw new IOException(e);
                                }
                            }
                            return ret;
                        }

                        private void finishSignature() throws IOException {
                            try {
                                if (sigOk) {
                                    return;
                                }
                                if (logger != null) {
                                    logger.info("Verify Signature " + entry.getName());
                                }
                                if (!signature.verify(contentSignatur)) {
                                    throw new SignatureException("Failed on " + entry.getName());
                                }
                                sigOk = true;
                            } catch (SignatureException e) {
                                throw new IOException(e);
                            }
                        }

                        /**
                         * @see java.io.FilterInputStream#read(byte[], int, int)
                         */
                        @Override
                        public int read(byte[] b, int off, int len) throws IOException {
                            int ret = super.read(b, off, len);
                            if (ret > 0) {
                                try {
                                    signature.update(b, off, ret);
                                } catch (SignatureException e) {
                                    throw new IOException(e);
                                }
                            } else {
                                finishSignature();
                            }
                            return ret;
                        }

                        /**
                         * @see java.io.FilterInputStream#close()
                         */
                        @Override
                        public void close() throws IOException {
                            try {
                                finishSignature();
                            } finally {
                                super.close();
                            }
                        }
                    };
                } catch (GeneralSecurityException e) {
                    throw ZipIOException.wrapOrAddSurpressed(null, e, entry);
                }
            }
        } catch (IOException e) {
            throw ZipIOException.wrapOrAddSurpressed(null, e, entry);
        }
        return ret;
    }

    protected void verifyPathSignature(final ZipEntry entry) throws ZipIOException {
        if (signature != null) {
            String comment = entry.getComment();
            Map<String, Object> cont = Deser.get().fromString(comment, TypeRef.MAP);
            byte[] pathSignature = Base64.decode((String) cont.get(ZipIOWriter.P_SIG));
            byte[] cSalt = Base64.decode((String) cont.get(ZipIOWriter.SIG_SALT));
            try {
                signature.initVerify(publicKey);
                signature.update(cSalt);
                signature.update(salt);
                signature.update(entry.getName().getBytes("UTF-8"));
                signature.verify(pathSignature);
            } catch (GeneralSecurityException e) {
                throw new ZipIOException("Signature Failed ", e);
            } catch (UnsupportedEncodingException e) {
                throw new ZipIOException("Signature Failed ", e);
            }
        }
    }

    /**
     * returns the ZipEntry for the given name
     *
     * @param fileName
     *            Filename we want a ZipEntry for
     * @return ZipEntry if filename is found or null if not found
     * @throws ZipIOException
     */
    public synchronized ZipEntry getZipFile(final String fileName) throws ZipIOException {
        if (fileName == null) {
            throw new ZipIOException("invalid fileName");
        }
        if (this.zip != null) {
            return this.zip.getEntry(fileName);
        } else {
            ZipInputStream zis = null;
            ZipIOException exception = null;
            ZipEntry ze = null;
            try {
                zis = new ZipInputStream(new ByteArrayInputStream(this.byteArray));
                while ((ze = zis.getNextEntry()) != null) {
                    if (ze.getName().equals(fileName)) {
                        return ze;
                    }
                }
            } catch (final IOException e) {
                exception = ZipIOException.wrapOrAddSurpressed(exception, e, ze);
            } finally {
                try {
                    if (zis != null) {
                        zis.close();
                    }
                } catch (final Throwable e) {
                    exception = ZipIOException.wrapOrAddSurpressed(exception, e, ze);
                }
                if (exception != null) {
                    throw exception;
                }
            }
        }
        return null;
    }

    /**
     * returns a list of all ZipEntries in this ZipFile
     *
     * @return ZipEntry[] of all files in the ZipFile
     * @throws ZipIOException
     */
    public synchronized ZipEntry[] getZipFiles() throws ZipIOException {
        ensureIndexIsVerified();
        return getZipEntriesInternal();
    }

    protected ZipEntry[] getZipEntriesInternal() throws ZipIOException {
        if (this.zipEntries != null) {
            return this.zipEntries;
        }
        final java.util.List<ZipEntry> ret = new ArrayList<ZipEntry>();
        ensureZipFile();
        if (this.zip != null) {
            final Enumeration<? extends ZipEntry> zipIter = this.zip.entries();
            while (zipIter.hasMoreElements()) {
                ret.add(zipIter.nextElement());
            }
        } else {
            ZipInputStream zis = null;
            ZipIOException exception = null;
            try {
                zis = new ZipInputStream(new ByteArrayInputStream(this.byteArray));
                ZipEntry ze = null;
                while ((ze = zis.getNextEntry()) != null) {
                    ret.add(ze);
                }
            } catch (final IOException e) {
                exception = ZipIOException.wrapOrAddSurpressed(exception, e, null);
            } finally {
                if (zis != null) {
                    try {
                        zis.close();
                    } catch (IOException e) {
                        exception = ZipIOException.wrapOrAddSurpressed(exception, e, null);
                    }
                }
                if (exception != null) {
                    throw exception;
                }
            }
        }
        this.zipEntries = ret.toArray(new ZipEntry[ret.size()]);
        return this.zipEntries;
    }

    protected void ensureZipFile() throws ZipIOException {
        if (signature != null && zip == null) {
            tmpZipFile = Application.getTempUniqueResource(".zip");
            tmpZipFile.deleteOnExit();
            try {
                IO.secureWrite(tmpZipFile, byteArray);
                zip = new ZipFile(tmpZipFile);
            } catch (IOException e) {
                throw ZipIOException.wrapOrAddSurpressed(null, e, null);
            }
        }
    }

    /**
     * returns a ZipIOFile Filesystem for this ZipFile
     *
     * @return ZipIOFile that represents ROOT of the Filesystem
     * @throws ZipIOException
     * @deprecated
     */
    @Deprecated
    public synchronized ZipIOFile getZipIOFileSystem() throws ZipIOException {
        if (this.rootFS != null) {
            return this.rootFS;
        }
        final ZipEntry[] content = this.getZipFiles();
        final java.util.List<ZipIOFile> root = new ArrayList<ZipIOFile>();
        for (final ZipEntry file : content) {
            if (!file.isDirectory() && !file.getName().contains("/")) {
                /* file is in root */
                final ZipIOFile tmp = new ZipIOFile(file.getName(), file, this, null);
                root.add(tmp);
            } else if (!file.isDirectory()) {
                /* file is not in root */
                final String parts[] = file.getName().split("/");
                /* we begin at root */
                ZipIOFile currentParent = null;
                String path = "";
                for (int i = 0; i < parts.length; i++) {
                    if (i == parts.length - 1) {
                        /* the file */
                        final ZipIOFile tmp = new ZipIOFile(parts[i], file, this, currentParent);
                        currentParent.getFilesInternal().add(tmp);
                    } else {
                        path = path + parts[i] + "/";
                        ZipIOFile found = null;
                        for (final ZipIOFile tmp : root) {
                            found = this.getFolder(path, tmp);
                            if (found != null) {
                                break;
                            }
                        }
                        if (found != null) {
                            currentParent = found;
                        } else {
                            final ZipIOFile newFolder = new ZipIOFile(parts[i], null, this, currentParent);
                            if (currentParent != null) {
                                currentParent.getFilesInternal().add(newFolder);
                            } else {
                                root.add(newFolder);
                            }
                            currentParent = newFolder;
                        }
                    }
                }
            }
        }
        this.rootFS = new ZipIOFile("", null, this, null);
        this.rootFS.getFilesInternal().addAll(root);
        this.rootFS.getFilesInternal().trimToSize();
        this.trimZipIOFiles(this.rootFS);
        return this.rootFS;
    }

    public boolean isAutoCreateExtractPath() {
        return this.autoCreateExtractPath;
    }

    /**
     * @return
     */
    protected boolean isAutoCreateSubDirs() {
        return this.autoCreateSubDirs;
    }

    public boolean isBreakOnError() {
        return this.breakOnError;
    }

    /**
     * @return
     */
    protected boolean isOverwrite() {
        return this.overwrite;
    }

    /**
     * opens the ZipFile for further use
     *
     * @throws ZipIOException
     * @throws ZipException
     * @throws IOException
     */
    private synchronized void openZip() throws ZipIOException {
        if (this.zip != null) {
            return;
        }
        if (this.zipFile == null || this.zipFile.isDirectory() || !this.zipFile.exists()) {
            throw new ZipIOException("invalid zipFile");
        }
        try {
            this.zip = new ZipFile(this.zipFile);
        } catch (ZipException e) {
            throw new ZipIOException(e);
        } catch (IOException e) {
            throw new ZipIOException(e);
        }
    }

    public void setAutoCreateExtractPath(final boolean autoCreateExtractPath) {
        this.autoCreateExtractPath = autoCreateExtractPath;
    }

    public void setAutoCreateSubDirs(final boolean autoCreateSubDirs) {
        this.autoCreateSubDirs = autoCreateSubDirs;
    }

    /**
     * Set to true of you want to extract as many files as possible. if false, the first error throws an exception and interrupts the
     * process
     *
     * @param breakOnError
     */
    public void setBreakOnError(final boolean breakOnError) {
        this.breakOnError = breakOnError;
    }

    public void setOverwrite(final boolean overwrite) {
        this.overwrite = overwrite;
    }

    /**
     * how many ZipEntries does this ZipFile have
     *
     * @return
     * @throws ZipIOException
     * @throws IOException
     */
    public synchronized int size() throws ZipIOException {
        if (this.zipEntriesSize != -1) {
            return this.zipEntriesSize;
        }
        if (this.zip != null) {
            this.zipEntriesSize = this.zip.size();
        } else {
            this.zipEntriesSize = getZipFiles().length;
        }
        return this.zipEntriesSize;
    }

    /**
     * trims the ZipIOFiles(reduces memory)
     *
     * @param root
     *            ZipIOFile we want to start
     */
    private void trimZipIOFiles(final ZipIOFile root) {
        if (root == null) {
            return;
        }
        for (final ZipIOFile tmp : root.getFiles()) {
            if (tmp.isDirectory()) {
                this.trimZipIOFiles(tmp);
            }
        }
        root.getFilesInternal().trimToSize();
    }

    /**
     * @param file
     * @param extracted
     * @throws IOException
     * @throws ZipException
     * @throws ZipIOException
     */
    public static void extractTo(File zipFile, File targetFolder) throws ZipIOException {
        ZipIOReader ziper = new ZipIOReader(zipFile);
        ZipIOException exception = null;
        try {
            if (!targetFolder.exists()) {
                targetFolder.mkdirs();
            }
            ziper.extractTo(targetFolder);
        } catch (ZipIOException e) {
            exception = ZipIOException.wrapOrAddSurpressed(exception, e, null);
        } finally {
            try {
                ziper.close();
            } catch (IOException e) {
                exception = ZipIOException.wrapOrAddSurpressed(exception, e, null);
            }
            if (exception != null) {
                throw exception;
            }
        }
    }

    /**
     * @return
     */
    public String getComment() {
        return this.zip.getComment();
    }

    private LogInterface logger;
    private byte[]       indexSignature;
    private String       signatureType;
    private Boolean      indexVerifyResult = null;

    public LogInterface getLogger() {
        return logger;
    }

    public void setLogger(LogInterface logger) {
        this.logger = logger;
    }

    /**
     * @param public1
     * @throws NoSuchAlgorithmException
     * @throws InvalidKeyException
     * @throws IOException
     * @throws SignatureException
     * @throws ZipIOException
     */
    public void setSignaturePublicKey(PublicKey pub) throws ZipIOException {
        this.publicKey = pub;
        ensureSignatureDetails();
        try {
            signature = Signature.getInstance("Sha256WithRSA");
        } catch (NoSuchAlgorithmException e) {
            throw ZipIOException.wrapOrAddSurpressed(null, e, null);
        }
    }

    protected void ensureSignatureDetails() throws ZipIOException {
        if (signatureType != null) {
            return;
        }
        String comment = null;
        if (byteArray != null) {
            byte[] tail = byteArray;
            try {
                comment = readComment(tail);
            } catch (SignatureException e) {
                throw ZipIOException.wrapOrAddSurpressed(null, e, null);
            }
        } else {
            if (JVMVersion.getVersion().isLowerThan(JavaVersion.JVM_1_7)) {
                ZipIOException exception = null;
                RandomAccessFile file = null;
                try {
                    file = new RandomAccessFile(zipFile, "r");
                    ;
                    file.seek(Math.max(0, zipFile.length() - 65000));
                    int read = 65000;
                    if (zipFile.length() < read) {
                        read = (int) zipFile.length();
                    }
                    byte[] tail = new byte[read];
                    file.readFully(tail);
                    comment = readComment(tail);
                } catch (IOException e) {
                    exception = ZipIOException.wrapOrAddSurpressed(exception, e, null);
                } catch (SignatureException e) {
                    exception = ZipIOException.wrapOrAddSurpressed(exception, e, null);
                } finally {
                    try {
                        if (file != null) {
                            file.close();
                        }
                    } catch (IOException e) {
                        exception = ZipIOException.wrapOrAddSurpressed(exception, e, null);
                    }
                    if (exception != null) {
                        throw exception;
                    }
                }
            } else {
                comment = zip.getComment();
            }
        }
        try {
            Map<String, Object> cont = Deser.get().fromString(comment, TypeRef.MAP);
            indexSignature = Base64.decode((String) cont.get(ZipIOWriter.I_SIG));
            salt = Base64.decode((String) cont.get(ZipIOWriter.SIG_SALT));
            signatureType = (String) cont.get(ZipIOWriter.TYPE);
            // System.out.println("Index:\r\n" + (String) cont.get("INDEX"));
            if (!ZipIOWriter.AWZ_SIG1.equals(signatureType)) {
                throw ZipIOException.wrapOrAddSurpressed(null, new SignatureException("Unsupported Signature type: " + signatureType), null);
            }
        } catch (SerializerException e) {
            throw ZipIOException.wrapOrAddSurpressed(null, e, null);
        }
    }

    public String getSignatureType() {
        try {
            ensureSignatureDetails();
        } catch (ZipIOException e) {
        }
        return signatureType;
    }

    public boolean isSigned() {
        try {
            ensureSignatureDetails();
        } catch (ZipIOException e) {
        }
        if (signatureType != null) {
            return true;
        } else {
            return false;
        }
    }

    protected void ensureIndexIsVerified() throws ZipIOException {
        if (signature != null) {
            try {
                if (indexVerifyResult != null) {
                    if (indexVerifyResult == Boolean.TRUE) {
                        return;
                    } else {
                        throw new SignatureException("Index Signature Failed");
                    }
                }
                try {
                    signature.initVerify(publicKey);
                    signature.update(salt);
                    for (ZipEntry e : getZipEntriesInternal()) {
                        signature.update(((e.isDirectory() ? "[D]" : "[F]") + e.getName()).getBytes("UTF-8"));
                    }
                    if (!signature.verify(indexSignature)) {
                        zipEntries = null;
                        throw new SignatureException("Index Signature Failed");
                    }
                    indexVerifyResult = Boolean.TRUE;
                } finally {
                    if (indexVerifyResult == null) {
                        indexVerifyResult = Boolean.FALSE;
                    }
                }
            } catch (UnsupportedEncodingException e) {
                zipEntries = null;
                throw ZipIOException.wrapOrAddSurpressed(null, e, null);
            } catch (GeneralSecurityException e) {
                zipEntries = null;
                throw ZipIOException.wrapOrAddSurpressed(null, e, null);
            }
        }
    }

    protected String readComment(byte[] tail) throws SignatureException {
        try {
            int eocdSignature = 0x06054b50;
            int eocdSize = 22; // Minimal EOCD size without comment
            // Search the last 64kb for a EOCD signature
            for (int i = tail.length - eocdSize; i >= 0; i--) {
                if ((tail[i] & 0xFF) == (eocdSignature & 0xFF) && (tail[i + 1] & 0xFF) == ((eocdSignature >> 8) & 0xFF) && (tail[i + 2] & 0xFF) == ((eocdSignature >> 16) & 0xFF) && (tail[i + 3] & 0xFF) == ((eocdSignature >> 24) & 0xFF)) {
                    // get comment length (last 2 bytes in EOCD header)
                    int commentLength = (tail[i + 20] & 0xFF) | ((tail[i + 21] & 0xFF) << 8);
                    if (commentLength > 0 && i + eocdSize + commentLength <= tail.length) {
                        return new String(tail, i + eocdSize, commentLength, "UTF-8");
                    } else {
                        throw new SignatureException("No comment with signature information found");
                    }
                }
            }
            throw new SignatureException("No comment with signature information found");
        } catch (UnsupportedEncodingException e) {
            throw new SignatureException(e);
        }
    }

    /**
     * @param e
     * @throws IOException
     * @throws UnsupportedEncodingException
     * @throws SignatureException
     * @throws ZipIOException
     * @throws InvalidKeyException
     *
     */
    public void verify(ZipEntry e) throws SignatureException, ZipIOException {
        ensureIndexIsVerified();
        verifyPathSignature(e);
    }

    public void verify() throws SignatureException, ZipIOException {
        for (ZipEntry ze : getZipFiles()) {
            if (ze.isDirectory()) {
                verifyPathSignature(ze);
            } else {
                extract(ze, new NullOutputStream());
            }
        }
    }

    /**
     * @param pub
     * @return
     * @throws IOException
     * @throws SignatureException
     * @throws NoSuchAlgorithmException
     * @throws InvalidKeyException
     */
    public ZipIOReader publickey(PublicKey pub) throws InvalidKeyException, NoSuchAlgorithmException, SignatureException, IOException {
        this.setSignaturePublicKey(pub);
        return this;
    }

    /**
     * @param defaultLogger
     * @return
     */
    public ZipIOReader logger(LogInterface logger) {
        setLogger(logger);
        return this;
    }
}
