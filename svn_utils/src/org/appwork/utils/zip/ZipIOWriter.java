/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2024, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
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

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.Signature;
import java.security.SignatureException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.appwork.serializer.Deser;
import org.appwork.serializer.SC;
import org.appwork.utils.Exceptions;
import org.appwork.utils.Hash;
import org.appwork.utils.StringUtils;
import org.appwork.utils.crypto.AWSign;
import org.appwork.utils.encoding.Base64;
import org.appwork.utils.logging2.LogInterface;

public class ZipIOWriter {
    /**
     * @author thomas
     * @date 18.10.2024
     *
     */
    public static class EmptyPathZipIOException extends ZipIOException {
        /**
         * @param message
         * @param entry
         */
        public EmptyPathZipIOException(ZipEntry entry) {
            super("Empty path is not allowed!", entry);
        }
    }

    /**
     *
     */
    public static final String AWZ_SIG1     = "AWZSig1";
    public static final String C_SIG        = "cSig";
    /**
     *
     */
    public static final String I_SIG        = "iSig";
    /**
     *
     */
    public static final String ORG          = "org";
    /**
     *
     */
    public static final String P_SIG        = "pSig";
    /**
     *
     */
    public static final int    SALT_SIZE    = 32;
    public static final String SIG_SALT     = "salt";
    /**
     *
     */
    public static final String TYPE         = "type";
    protected final byte[]     buf          = new byte[16384];
    private HashSet<String>    dupes        = new HashSet<String>();
    private List<ZipEntry>     entries      = new ArrayList<ZipEntry>();
    private LogInterface       logger;
    protected OutputStream     outputStream = null;
    private PrivateKey         privateKey;
    private byte[]             salt;
    private Signature          signature;
    protected File             zipFile      = null;
    /**
     *
     */
    protected ZipOutputStream  zipStream    = null;

    public ZipIOWriter(final ByteArrayOutputStream stream) throws ZipIOException {
        this.outputStream = stream;
        this.zipStream = new ZipOutputStream(this.outputStream);
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
        this.outputStream = stream;
        this.zipStream = new ZipOutputStream(this.outputStream);
    }

    public void add(final byte[] data, final boolean compress, final String... pathElements) throws ZipIOException, IOException {
        boolean zipEntryAdded = false;
        String fullPath = join(pathElements);
        final ZipEntry zipAdd = new ZipEntry(fullPath);
        if (data == null) {
            throw new ZipIOException("data array is invalid", zipAdd);
        }
        if (!dupes.add(fullPath)) {
            throw new ZipIOException("Cannot add the same path multiple times...", zipAdd);
        }
        ZipIOException exception = null;
        try {
            zipAdd.setSize(data.length);
            if (compress) {
                zipAdd.setMethod(ZipEntry.DEFLATED);
            } else {
                zipAdd.setMethod(ZipEntry.STORED);
                zipAdd.setCompressedSize(data.length);
                /* STORED must have a CRC32! */
                zipAdd.setCrc(Hash.getCRC32(data));
            }
            HashMap<String, Object> cont = new HashMap<String, Object>();
            if (ignore(zipAdd)) {
                return;
            }
            initEntrySignature(zipAdd, cont);
            zipStream.putNextEntry(zipAdd);
            zipEntryAdded = true;
            this.zipStream.write(data, 0, data.length);
            if (signature != null) {
                signature.update(data, 0, data.length);
            }
            finishSignatureToEntry(zipAdd, cont);
            notify(zipAdd, data.length, data.length);
        } catch (IOException e) {
            exception = ZipIOException.wrapOrAddSurpressed(exception, e, zipAdd);
        } catch (SignatureException e) {
            exception = ZipIOException.wrapOrAddSurpressed(exception, e, zipAdd);
        } finally {
            if (zipEntryAdded) {
                try {
                    this.zipStream.closeEntry();
                } catch (IOException e) {
                    exception = ZipIOException.wrapOrAddSurpressed(exception, e, zipAdd);
                }
            }
            if (exception != null) {
                throw exception;
            }
        }
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
    public void add(final File add, final boolean compress, String... pathElements) throws ZipIOException {
        if (add == null) {
            throw new ZipIOException("File is null");
        }
        if (!add.exists()) {
            throw new ZipIOException(new FileNotFoundException(add.getAbsolutePath()));
        }
        if (add.isFile()) {
            this.addFile(add, compress, join(pathElements));
        } else if (add.isDirectory()) {
            this.addDirectoryRecursive(add, compress, join(pathElements));
        } else {
            throw new ZipIOException("add " + add.getAbsolutePath() + " invalid");
        }
    }

    protected void addDirectoryRecursive(final File addDirectory, final boolean compress, final String path) throws ZipIOException {
        if (addDirectory == null) {
            throw new ZipIOException("addDirectory invalid: null");
        }
        if (!addDirectory.exists() && throwExceptionOnFileGone(addDirectory)) {
            throw new ZipIOException("addDirectory " + addDirectory.getAbsolutePath() + " invalid: does not exist");
        }
        addFolder(path);
        final File[] list = addDirectory.listFiles();
        if (list != null) {
            for (final File add : list) {
                if (add.isFile()) {
                    this.addFile(add, compress, join(path, add.getName()));
                } else if (add.isDirectory()) {
                    this.addDirectoryRecursive(add, compress, join(path, add.getName()));
                } else if (!add.exists() && throwExceptionOnFileGone(add)) {
                    throw new ZipIOException("addDirectory: " + add.getAbsolutePath() + "(File:" + add.isFile() + "|Directory:" + add.isDirectory() + ")");
                }
            }
        }
    }

    protected void addFile(final File addFile, final boolean compress, final String fullPath) throws ZipIOException {
        if (addFile == null) {
            throw new ZipIOException("addFile invalid:null");
        }
        if (addFile.equals(zipFile)) {
            throw new ZipIOException("Tried to add the Zipfile itself.Zip-Ception?");
        }
        InputStream fin = null;
        boolean zipEntryAdded = false;
        ZipIOException exception = null;
        final ZipEntry zipAdd = new ZipEntry(fullPath);
        if (StringUtils.isEmpty(new File(fullPath).getName())) {
            throw new EmptyPathZipIOException(zipAdd);
        }
        if (!dupes.add(fullPath)) {
            throw new ZipIOException("Cannot add the same path multiple times...:" + fullPath, zipAdd);
        }
        try {
            autoCreateFolders(fullPath);
            fin = new BufferedInputStream(new FileInputStream(addFile));
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
            if (ignore(zipAdd)) {
                return;
            }
            long total = 0;
            HashMap<String, Object> cont = new HashMap<String, Object>();
            initEntrySignature(zipAdd, cont);
            zipStream.putNextEntry(zipAdd);
            int len;
            while ((len = fin.read(this.buf)) >= 0) {
                if (len == 0) {
                    continue;
                }
                this.zipStream.write(this.buf, 0, len);
                updateSigner(len, buf, zipAdd);
                total += len;
                notify(zipAdd, len, total);
            }
            finishSignatureToEntry(zipAdd, cont);
            zipEntryAdded = true;
        } catch (FileNotFoundException e) {
            if (addFile.exists() == false) {
                if (throwExceptionOnFileGone(addFile)) {
                    exception = ZipIOException.wrapOrAddSurpressed(exception, e, zipAdd);
                }
                return;
            }
            exception = ZipIOException.wrapOrAddSurpressed(exception, e, zipAdd);
        } catch (IOException e) {
            exception = ZipIOException.wrapOrAddSurpressed(exception, e, zipAdd);
        } catch (SignatureException e) {
            exception = ZipIOException.wrapOrAddSurpressed(exception, e, zipAdd);
        } finally {
            if (fin != null) {
                try {
                    fin.close();
                } catch (IOException e) {
                    exception = ZipIOException.wrapOrAddSurpressed(exception, e, zipAdd);
                }
            }
            if (zipEntryAdded) {
                try {
                    this.zipStream.closeEntry();
                } catch (IOException e) {
                    exception = ZipIOException.wrapOrAddSurpressed(exception, e, zipAdd);
                }
            }
            if (exception != null) {
                throw exception;
            }
        }
    }

    private boolean folderAutoCreationEnabled = false;

    public boolean isFolderAutoCreationEnabled() {
        return folderAutoCreationEnabled;
    }

    public void setFolderAutoCreationEnabled(boolean folderAutoCreationEnabled) {
        this.folderAutoCreationEnabled = folderAutoCreationEnabled;
    }

    protected void autoCreateFolders(final String fullPath) throws ZipIOException {
        if (!isFolderAutoCreationEnabled()) {
            return;
        }
        String p = "";
        String[] elements = fullPath.split("/");
        for (int i = 0; i < elements.length - 1; i++) {
            if (StringUtils.isEmpty(elements[i])) {
                continue;
            }
            p = join(p, elements[i]);
            if (dupes.add(p)) {
                addFolder(p);
            }
        }
    }

    public synchronized void addFolder(String fullPath) throws ZipIOException {
        if (StringUtils.isEmpty(fullPath)) {
            return;
        }
        if (!fullPath.endsWith("/")) {
            fullPath = fullPath + "/";
        }
        autoCreateFolders(fullPath);
        if (!dupes.add(fullPath)) {
            return;
        }
        ZipIOException exception = null;
        boolean zipEntryAdded = false;
        final ZipEntry zipAdd = new ZipEntry(fullPath);
        try {
            if (ignore(zipAdd)) {
                return;
            }
            HashMap<String, Object> cont = new HashMap<String, Object>();
            initEntrySignature(zipAdd, cont);
            this.zipStream.putNextEntry(zipAdd);
            finishSignatureToEntry(zipAdd, cont);
            notify(zipAdd, -1, -1);
            zipEntryAdded = true;
        } catch (SignatureException e) {
            exception = ZipIOException.wrapOrAddSurpressed(exception, e, zipAdd);
        } catch (IOException e) {
            exception = ZipIOException.wrapOrAddSurpressed(exception, e, zipAdd);
        } finally {
            if (zipEntryAdded) {
                try {
                    this.zipStream.closeEntry();
                } catch (IOException e) {
                    exception = ZipIOException.wrapOrAddSurpressed(exception, e, zipAdd);
                }
            }
            if (exception != null) {
                throw exception;
            }
        }
    }

    /**
     * closes the ZipFile
     *
     * @throws Throwable
     */
    public synchronized void close() throws ZipIOException {
        ZipIOException exception = null;
        try {
            if (signature != null) {
                HashMap<String, Object> con = new HashMap<String, Object>();
                con.put(TYPE, AWZ_SIG1);
                try {
                    signature.initSign(privateKey);
                    signature.update(salt);
                    for (ZipEntry e : entries) {
                        signature.update(((e.isDirectory() ? "[D]" : "[F]") + e.getName()).getBytes("UTF-8"));
                    }
                    con.put(I_SIG, Base64.encodeToString(signature.sign()));
                    con.put(SIG_SALT, Base64.encodeToString(salt));
                    zipStream.setComment(Deser.get().toString(con, SC.STORAGE));
                } catch (SignatureException e) {
                    exception = ZipIOException.wrapOrAddSurpressed(exception, e, null);
                } catch (InvalidKeyException e) {
                    exception = ZipIOException.wrapOrAddSurpressed(exception, e, null);
                } catch (UnsupportedEncodingException e) {
                    exception = ZipIOException.wrapOrAddSurpressed(exception, e, null);
                }
            }
            try {
                flushClose(zipStream);
            } catch (IOException e) {
                exception = ZipIOException.wrapOrAddSurpressed(exception, e, null);
            }
            try {
                flushClose(outputStream);
            } catch (IOException e) {
                exception = ZipIOException.wrapOrAddSurpressed(exception, e, null);
            }
        } finally {
            privateKey = null;
            signature = null;
            this.zipStream = null;
            this.outputStream = null;
            if (exception != null) {
                throw exception;
            }
        }
    }

    protected void finishSignatureToEntry(final ZipEntry zipAdd, HashMap<String, Object> cont) throws SignatureException {
        if (signature != null) {
            cont.put(C_SIG, Base64.encodeToString(signature.sign()));
            if (StringUtils.isNotEmpty(zipAdd.getComment())) {
                // keep original Comment
                cont.put(ORG, zipAdd.getComment());
            }
            zipAdd.setComment(Deser.get().toString(cont, SC.STORAGE));
            this.entries.add(zipAdd);
        }
    }

    /**
     * @param zipStream2
     * @throws IOException
     */
    private void flushClose(OutputStream os) throws IOException {
        if (os != null) {
            try {
                os.flush();
                // ensure that data is written to disk, else the data might not be on disk after close
                if (os instanceof FileOutputStream) {
                    try {
                        ((FileOutputStream) os).getChannel().force(true);
                    } catch (IOException e) {
                        // ignore.
                    }
                }
            } catch (IOException e) {
                try {
                    os.close();
                } catch (IOException ee) {
                    Exceptions.addSuppressed(e, ee);
                }
                throw e;
            }
            os.close();
        }
    }

    public LogInterface getLogger() {
        return logger;
    }

    protected byte[] getSalt() {
        return AWSign.getByteSalt(SALT_SIZE);
    }

    public ZipOutputStream getZipStream() {
        return zipStream;
    }

    /**
     * @param zipAdd
     * @return
     */
    public boolean ignore(ZipEntry e) {
        return false;
    }

    protected void initEntrySignature(final ZipEntry zipAdd, HashMap<String, Object> cont) throws ZipIOException {
        byte[] cSalt = null;
        if (logger != null) {
            logger.info("Add Path " + zipAdd.getName() + " " + zipAdd.getSize() + " bytes uncompressed");
        }
        if (signature != null) {
            try {
                signature.initSign(privateKey);
                signature.update(cSalt = getSalt());
                signature.update(salt);
                signature.update(zipAdd.getName().getBytes("UTF-8"));
                cont.put(SIG_SALT, Base64.encodeToString(cSalt));
                cont.put(P_SIG, Base64.encodeToString(signature.sign()));
                signature.initSign(privateKey);
                signature.update(cSalt);
                signature.update(salt);
            } catch (final UnsupportedEncodingException e) {
                throw new ZipIOException("Signature error", e);
            } catch (InvalidKeyException e) {
                throw new ZipIOException("Signature error", e);
            } catch (SignatureException e) {
                throw new ZipIOException("Signature error", e);
            }
        }
    }

    /**
     * @param parentPath
     * @param name
     * @return
     */
    private String join(String... elements) {
        if (elements == null || elements.length == 0 || (elements.length == 1 && elements[0] == null)) {
            return "";
        }
        StringBuilder sb = new StringBuilder();
        for (String e : elements) {
            e = e.trim();
            if (e.length() == 0) {
                continue;
            }
            while (e.length() > 0 && e.charAt(0) == '/') {
                e = e.substring(1);
            }
            if (e.length() == 0) {
                continue;
            }
            if (sb.length() > 0 && sb.charAt(sb.length() - 1) != '/') {
                sb.append("/");
            }
            sb.append(e);
        }
        return sb.toString();
    }

    protected void notify(final ZipEntry entry, final long bytesWrite, final long bytesProcessed) {
    }

    /**
     * opens the zipFile for further use
     *
     * @param overwrite
     *            overwrite existing zipFiles?
     * @throws ZipIOException
     * @throws FileNotFoundException
     */
    private void openZip(final boolean overwrite) throws ZipIOException {
        if (this.outputStream != null && this.zipStream != null) {
            return;
        }
        if (this.zipFile == null || this.zipFile.isDirectory()) {
            throw new ZipIOException("invalid zipFile");
        }
        if (this.zipFile.exists() && !overwrite) {
            throw new ZipIOException("zipFile already exists");
        }
        try {
            this.outputStream = new FileOutputStream(this.zipFile);
        } catch (FileNotFoundException e) {
            throw new ZipIOException(e);
        }
        this.zipStream = new ZipOutputStream(this.outputStream);
    }

    public void setLogger(LogInterface logger) {
        this.logger = logger;
    }

    /**
     * @param priv
     * @throws NoSuchAlgorithmException
     * @throws InvalidKeyException
     * @throws SignatureException
     */
    public void setSignaturePrivateKey(PrivateKey priv) throws NoSuchAlgorithmException {
        privateKey = priv;
        salt = getSalt();
        signature = Signature.getInstance("Sha256WithRSA");
    }

    protected boolean throwExceptionOnFileGone(File file) {
        return true;
    }

    protected void updateSigner(int len, byte[] buf, ZipEntry zipAdd) throws SignatureException {
        if (signature != null) {
            signature.update(buf, 0, len);
        }
    }
}
