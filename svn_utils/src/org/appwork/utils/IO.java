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
package org.appwork.utils;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.EOFException;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PushbackInputStream;
import java.io.RandomAccessFile;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.net.URL;
import java.nio.CharBuffer;
import java.nio.channels.ClosedByInterruptException;
import java.nio.channels.FileChannel;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.zip.GZIPInputStream;

import org.appwork.utils.Files.AbstractHandler;
import org.appwork.utils.IO.BOM.BOMInputStream;
import org.appwork.utils.net.Base64InputStream;
import org.appwork.utils.net.CharSequenceInputStream;
import org.appwork.utils.os.CrossSystem;

public class IO {
    public static enum SYNC {
        /**
         * do not sync filesystem at all
         */
        NONE,
        /**
         * sync written data to filesystem
         */
        DATA,
        /**
         * sync written data and its meta-data (filesystem information)
         */
        META_AND_DATA
    }

    /**
     * does NOT create the parent
     *
     * @param in
     * @param out
     * @throws IOException
     */
    public static void copyFile(final File in, final File out) throws IOException {
        IO.copyFile(in, out, null);
    }

    /**
     * does NOT create the parent
     *
     * @param in
     * @param out
     * @param sync
     * @throws IOException
     */
    public static void copyFile(final File in, final File out, final SYNC sync) throws IOException {
        IO.copyFile(null, in, out, sync);
    }

    /**
     * does NOT create the parent
     *
     * @param progress
     * @param in
     * @param out
     * @param sync
     * @throws IOException
     */
    public static void copyFile(ProgressFeedback progress, final File in, final File out, final SYNC sync) throws IOException {
        FileInputStream fis = null;
        FileOutputStream fos = null;
        FileChannel inChannel = null;
        FileChannel outChannel = null;
        try {
            if (out.exists()) {
                throw new IOException("Cannot overwrite " + out);
            }
            if (!in.exists()) {
                throw new FileNotFoundException(in.getAbsolutePath());
            }
            fis = new FileInputStream(in);
            fos = new FileOutputStream(out);
            inChannel = fis.getChannel();
            outChannel = fos.getChannel();
            if (progress != null) {
                progress.setBytesTotal(in.length());
            }
            if (CrossSystem.isWindows()) {
                // magic number for Windows, 64Mb - 32Kb)
                // On the Windows plateform, you can't copy a file bigger
                // than
                // 64Mb,
                // an Exception in thread "main" java.io.IOException:
                // Insufficient
                // system resources exist to complete the requested service
                // is
                // thrown.
                //
                // For a discussion about this see :
                // http://forum.java.sun.com/thread.jspa?threadID=439695&messageID=2917510
                final int maxCount = 64 * 1024 * 1024 - 32 * 1024;
                final long size = inChannel.size();
                long position = 0;
                while (position < size) {
                    position += inChannel.transferTo(position, maxCount, outChannel);
                    if (progress != null) {
                        progress.setBytesProcessed(position);
                    }
                }
            } else {
                /* we also loop here to make sure all data got transfered! */
                final int maxCount = 64 * 1024 * 1024 - 32 * 1024;
                final long size = inChannel.size();
                long position = 0;
                while (position < size) {
                    position += inChannel.transferTo(position, maxCount, outChannel);
                    if (progress != null) {
                        progress.setBytesProcessed(position);
                    }
                }
            }
            if (sync != null) {
                switch (sync) {
                case DATA:
                    outChannel.force(false);
                    break;
                case META_AND_DATA:
                    outChannel.force(true);
                    break;
                case NONE:
                default:
                    break;
                }
            }
        } catch (final IOException e) {
            throw e;
        } finally {
            try {
                fos.close();
            } catch (final Throwable e) {
            }
            try {
                fis.close();
            } catch (final Throwable e) {
            }
        }
    }

    public static void copyFolderRecursive(final File src, final File dest, final boolean overwriteFiles) throws IOException {
        IO.copyFolderRecursive(src, dest, overwriteFiles, SYNC.NONE);
    }

    public static void copyFolderRecursive(final File src, final File dest, final boolean overwriteFiles, final FileFilter filter, final SYNC sync) throws IOException {
        Files.walkThroughStructure(new AbstractHandler<IOException>() {
            @Override
            public void onFile(final File f) throws IOException {
                if (filter != null && !filter.accept(f)) {
                    return;
                }
                final String path = Files.getRelativePath(src, f);
                if (path == null) {
                    throw new IOException("No rel Path " + src + "-" + f);
                }
                if (f.isDirectory()) {
                    new File(dest, path).mkdirs();
                } else {
                    final File dst = new File(dest, path);
                    if (overwriteFiles && dst.exists()) {
                        if (!dst.delete()) {
                            //
                            throw new IOException("Cannot overwrite " + dst);
                        }
                    }
                    dst.getParentFile().mkdirs();
                    IO.copyFile(f, dst, sync);
                }
                return;
            }
        }, src);
    }

    /**
     * @param overwriteFiles
     *            TODO
     * @param dist
     * @param dist2
     * @throws IOException
     */
    public static void copyFolderRecursive(final File src, final File dest, final boolean overwriteFiles, final SYNC sync) throws IOException {
        IO.copyFolderRecursive(src, dest, overwriteFiles, null, sync);
    }

    @Deprecated
    public static IOErrorHandler getErrorHandler() {
        return null;
    }

    public static enum BOM {
        UTF8(new byte[] { (byte) 239, (byte) 187, (byte) 191 }, "UTF-8"),
        UTF16BE(new byte[] { (byte) 254, (byte) 255 }, "UTF-16BE"),
        UTF16LE(new byte[] { (byte) 255, (byte) 254 }, "UTF-16LE"),
        UTF32BE(new byte[] { (byte) 0, (byte) 0, (byte) 254, (byte) 255 }, "UTF-32BE"),
        UTF32LE(new byte[] { (byte) 0, (byte) 0, (byte) 255, (byte) 254 }, "UTF-32LE");
        public static class BOMInputStream extends FilterInputStream {
            private final BOM bom;

            public BOM getBOM() {
                return bom;
            }

            private BOMInputStream(InputStream is, BOM bom) {
                super(is);
                this.bom = bom;
            }
        }

        private final byte[]  bomMarker;
        private final Charset charSet;

        public final Charset getCharSet() {
            return charSet;
        }

        private BOM(final byte[] bomMarker, final String charSet) {
            this.bomMarker = bomMarker;
            this.charSet = Charset.forName(charSet);
        }

        public final int length() {
            return bomMarker.length;
        }

        public byte[] getBOM() {
            return bomMarker.clone();
        }

        public static BOM get(final byte[] bytes) {
            for (final BOM bom : BOM.values()) {
                if (bom.startsWith(bytes)) {
                    return bom;
                }
            }
            return null;
        }

        private boolean startsWith(final byte[] bytes) {
            if (bytes != null && bytes.length >= length()) {
                for (int index = 0; index < length(); index++) {
                    if (bytes[index] != bomMarker[index]) {
                        return false;
                    }
                }
                return true;
            }
            return false;
        }

        public static BOMInputStream wrap(final InputStream is) throws IOException {
            final byte[] peekBuf = new byte[4];
            int peekIndex = 0;
            BOM bom = null;
            while (peekIndex < peekBuf.length && bom == null) {
                final int read = is.read();
                if (read == -1) {
                    break;
                } else {
                    peekBuf[peekIndex++] = (byte) (read & 0xff);
                    bom = get(peekBuf);
                }
            }
            if (bom == null) {
                if (peekIndex == 0) {
                    return new BOMInputStream(is, null);
                } else {
                    final int pushBackSize = peekIndex - 0;
                    final PushbackInputStream pbis = new PushbackInputStream(is, pushBackSize);
                    pbis.unread(peekBuf, 0, peekIndex);
                    return new BOMInputStream(pbis, null);
                }
            } else {
                if (peekIndex == bom.length()) {
                    return new BOMInputStream(is, bom);
                } else {
                    final int pushBackSize = peekIndex - bom.length();
                    final PushbackInputStream pbis = new PushbackInputStream(is, pushBackSize);
                    pbis.unread(peekBuf, bom.length(), peekIndex - bom.length());
                    return new BOMInputStream(pbis, bom);
                }
            }
        }

        public static String read(final byte[] bytes, final Charset defaultCharset) throws IOException {
            final BOM bom = get(bytes);
            if (bom != null) {
                return new String(bytes, bom.length(), bytes.length - bom.length(), bom.getCharSet());
            } else if (defaultCharset != null) {
                return new String(bytes, defaultCharset);
            } else {
                return null;
            }
        }
    }

    public static void moveTo(final File source, final File dest, final FileFilter filter) throws IOException {
        final java.util.List<File> files = Files.getFiles(filter, source);
        // TODO Proper delete
        for (final File src : files) {
            final String rel = Files.getRelativePath(source, src);
            final File file = new File(dest, rel);
            if (src.isDirectory()) {
                file.mkdirs();
            } else {
                file.getParentFile().mkdirs();
                if (!src.renameTo(file)) {
                    throw new IOException("Could not move file " + src + " to " + file);
                }
            }
        }
    }

    public static RandomAccessFile open(File file, String mode) throws IOException {
        if (CrossSystem.isWindows()) {
            int retry = 0;
            while (true) {
                try {
                    return new RandomAccessFile(file, mode);
                } catch (final FileNotFoundException e) {
                    /**
                     * too fast file opening/extraction (eg image gallery) can result in "access denied" exception
                     */
                    if (retry < 3) {
                        if (retry == 2 && CrossSystem.isWindows()) {
                            // http://stackoverflow.com/questions/10516472/file-createnewfile-randomly-fails
                            // http://bugs.java.com/bugdatabase/view_bug.do?bug_id=6213298
                            System.gc();
                        }
                        try {
                            Thread.sleep(500 * retry++);
                        } catch (InterruptedException ie) {
                            Thread.currentThread().interrupt();
                            throw Exceptions.addSuppressed(new ClosedByInterruptException(), e);
                        }
                    } else {
                        throw e;
                    }
                }
            }
        } else {
            return new RandomAccessFile(file, mode);
        }
    }

    public static byte[] readFile(final File ressource) throws IOException {
        final int maxRead;
        final long fileSize = ressource.length();
        if (fileSize > 0 && fileSize < Integer.MAX_VALUE) {
            maxRead = (int) fileSize;
        } else {
            maxRead = -1;
        }
        return IO.readFile(ressource, maxRead);
    }

    /*
     * this function reads a line from a bufferedinputstream up to a maxLength. in case the line is longer than maxLength the rest of the
     * line is read but not returned
     * 
     * this function skips emtpy lines
     */
    public static byte[] readFile(final File ressource, final int maxSize) throws IOException {
        final FileInputStream fis = new FileInputStream(ressource);
        try {
            return IO.readStream(maxSize, fis);
        } finally {
            try {
                fis.close();
            } catch (final Throwable e) {
            }
        }
    }

    public static String importFileToString(final File file, final int maxSize) throws IOException {
        final FileInputStream fis = new FileInputStream(file);
        try {
            return readStreamToString(fis, maxSize, true);
        } finally {
            try {
                fis.close();
            } catch (final Throwable e) {
            }
        }
    }

    public static String readStreamToString(final InputStream is, final int maxSize, final boolean closeStream) throws IOException {
        try {
            final BOMInputStream bis;
            if (is instanceof BOMInputStream) {
                bis = (BOMInputStream) is;
            } else {
                bis = BOM.wrap(is);
            }
            final ReusableByteArrayOutputStream os = new ReusableByteArrayOutputStream(maxSize > 0 ? maxSize : 32);
            IO.readStreamToOutputStream(maxSize, bis, os, false);// closed in finally
            final String ret = new String(os.getInternalBuffer(), 0, os.size(), bis.getBOM() != null ? bis.getBOM().getCharSet() : BOM.UTF8.getCharSet());
            return ret;
        } finally {
            if (closeStream) {
                try {
                    is.close();
                } catch (final Exception e) {
                }
            }
        }
    }

    @Deprecated
    public static String readStreamToString(final InputStream is, final int maxSize) throws IOException {
        return readStreamToString(is, maxSize, true);
    }

    public static String readFileToString(final File file) throws IOException {
        return IO.importFileToString(file, -1);
    }

    public static String readFileToTrimmedString(final File file) throws IOException {
        final String ret = readFileToString(file);
        if (ret != null) {
            return ret.trim();
        } else {
            return null;
        }
    }

    /**
     * This method returns different newline separator depending on System.getProperty("line.separator")! Use #readStreamToString instead.
     *
     * @param f
     * @return
     * @throws IOException
     */
    @Deprecated
    public static String readInputStreamToString(final InputStream is) throws IOException {
        final BOMInputStream bis;
        if (is instanceof BOMInputStream) {
            bis = (BOMInputStream) is;
        } else {
            bis = BOM.wrap(is);
        }
        return readToString(new BufferedReader(new InputStreamReader(bis, bis.getBOM() != null ? bis.getBOM().getCharSet() : BOM.UTF8.getCharSet())));
    }

    /**
     * This method returns different newline separator depending on System.getProperty("line.separator")! Use #readStreamToString instead.
     *
     * @param f
     * @return
     * @throws IOException
     */
    @Deprecated
    public static String readToString(final Reader f) throws IOException {
        try {
            final BufferedReader bf = f instanceof BufferedReader ? (BufferedReader) f : new BufferedReader(f);
            String line;
            final StringBuilder ret = new StringBuilder();
            final String sep = System.getProperty("line.separator");
            while ((line = bf.readLine()) != null) {
                if (ret.length() > 0) {
                    ret.append(sep);
                } else if (line.startsWith("\uFEFF")) {
                    /*
                     * Workaround for this bug: http://bugs.sun.com/view_bug.do?bug_id=4508058
                     * http://bugs.sun.com/view_bug.do?bug_id=6378911
                     */
                    line = line.substring(1);
                }
                ret.append(line);
            }
            return ret.toString();
        } finally {
            try {
                f.close();
            } catch (final Throwable e) {
            }
        }
    }

    public static String readLine(final BufferedInputStream is, final byte[] array) throws IOException {
        Arrays.fill(array, 0, array.length, (byte) 0);
        int read = 0;
        int total = 0;
        int totalString = 0;
        boolean nextLineReached = false;
        while (true) {
            read = is.read();
            if (read == -1 && total == 0) {
                /* EOS */
                return null;
            }
            if (read == 13 || read == 10) {
                /* line break found, mark in inputstream */
                nextLineReached = true;
                is.mark(1024);
            } else if (nextLineReached) {
                /* new text found */
                is.reset();
                total--;
                break;
            } else if (total < array.length) {
                /* only write to outputstream if maxlength not reached yet */
                array[totalString++] = (byte) read;
            }
            total++;
        }
        return new String(array, 0, totalString, BOM.UTF8.getCharSet());
    }

    public static byte[] readStream(final int maxSize, final InputStream input) throws IOException {
        final ByteArrayOutputStream baos;
        if (maxSize > 0) {
            baos = new ByteArrayOutputStream(maxSize);
        } else {
            baos = new ByteArrayOutputStream();
        }
        return IO.readStream(maxSize, input, baos, true);
    }

    public static byte[] readStream(final int maxSize, final InputStream input, final ByteArrayOutputStream baos) throws IOException {
        return IO.readStream(maxSize, input, baos, true);
    }

    public static byte[] readStream(final int maxSize, final InputStream input, ByteArrayOutputStream baos, boolean closeInput) throws IOException {
        if (baos == null) {
            baos = new ByteArrayOutputStream();
        }
        IO.readStreamToOutputStream(maxSize, input, baos, closeInput);
        return baos.toByteArray();
    }

    /**
     * Write a long to the stream, but uses as less bytes as possible. if out is null, the method will just return the amount of required
     * bytes
     *
     * @param value
     * @param out
     * @return the amount of written bytes
     * @throws IOException
     */
    public static int writeLongOptimized(final long value, final OutputStream out) throws IOException {
        return writeLongOptimized(value, null, out);
    }

    public static int writeLongOptimized(final long value, byte[] outBuf, final OutputStream out) throws IOException {
        // TODO: update to use full 8 bits of last byte
        // TODO: check value larger than supported
        if (value < 0) {
            //
            throw new NumberFormatException("value must be >=0");
        } else if (outBuf == null || outBuf.length < 8) {
            outBuf = new byte[8];
        }
        long rest = value;
        int bufferPosition = 0;
        while (true) {
            final int write = (int) ((rest & 127) << 1 & 0xFF);
            outBuf[bufferPosition] = (byte) write;
            rest = rest >>> 7;
            if (rest == 0) {
                out.write(outBuf, 0, bufferPosition + 1);
                return bufferPosition + 1;
            }
            outBuf[bufferPosition] = (byte) (outBuf[bufferPosition] | 1);
            bufferPosition++;
        }
    }

    /**
     * reads a long from the stream that has been written with the {@link #writeLongOptimized(long, OutputStream)} method
     *
     * @param in
     * @return
     * @throws IOException
     */
    public static long readLongOptimized(final InputStream in) throws IOException {
        long value = 0;
        int position = 0;
        while (true) {
            final long read = in.read();
            if (read < 0) {
                throw new EOFException("Expected bytes: position=" + position);
            }
            value = value + (read >>> 1 << position * 7);
            if ((read & 1) == 0) {
                return value;
            }
            position++;
        }
    }

    public static long readStreamToOutputStream(final int maxSize, final InputStream input, final OutputStream baos, boolean closeInput) throws IOException, Error {
        try {
            if (maxSize == 0) {
                return 0;
            } else {
                final byte[] buffer = new byte[32767];
                int len;
                if (maxSize > 0) {
                    int done = 0;
                    while (done < maxSize && (len = input.read(buffer, 0, Math.min(buffer.length, maxSize - done))) != -1) {
                        if (Thread.currentThread().isInterrupted()) {
                            throw new ClosedByInterruptException();
                        }
                        if (len > 0) {
                            baos.write(buffer, 0, len);
                            done += len;
                        }
                    }
                    return done;
                } else {
                    long done = 0;
                    while ((len = input.read(buffer)) != -1) {
                        if (Thread.currentThread().isInterrupted()) {
                            throw new ClosedByInterruptException();
                        }
                        if (len > 0) {
                            baos.write(buffer, 0, len);
                            done += len;
                        }
                    }
                    return done;
                }
            }
        } finally {
            if (closeInput) {
                try {
                    input.close();
                } catch (final Exception e) {
                }
            }
        }
    }

    /**
     * @param f
     * @return
     * @throws IOException
     */
    public static byte[] readURL(final URL f) throws IOException {
        return IO.readURL(f, -1);
    }

    /**
     * @param url
     * @param maxSize
     * @return
     * @throws IOException
     */
    public static byte[] readURL(final URL url, final int maxSize) throws IOException {
        final InputStream input = URLStream.openStream(url);
        try {
            return IO.readStream(maxSize, input);
        } finally {
            try {
                input.close();
            } catch (final Throwable e) {
            }
        }
    }

    /**
     *
     * This method returns different newline separator depending on System.getProperty("line.separator")! Use #readStreamToString instead.
     *
     * @param ressourceURL
     * @return
     * @throws IOException
     * @throws UnsupportedEncodingException
     */
    public static String readURLToString(final URL ressourceURL) throws IOException {
        final InputStream fis = URLStream.openStream(ressourceURL);
        try {
            return IO.readInputStreamToString(fis);
        } finally {
            try {
                fis.close();
            } catch (final Throwable e) {
            }
        }
    }

    /**
     * This method autocreates the parent
     *
     * @param file
     * @param bytes
     * @param none
     * @throws IOException
     */
    public static void secureWrite(final File file, final byte[] bytes) throws IOException {
        IO.secureWrite(file, bytes, SYNC.META_AND_DATA);
    }

    /**
     * This method autocreates the parent
     *
     * @param file
     * @param bytes
     * @param sync
     * @throws IOException
     */
    public static void secureWrite(final File file, final byte[] bytes, final SYNC sync) throws IOException {
        secureWrite(file, new WriteToFileCallback() {
            @Override
            public void writeTo(OutputStream os) throws IOException {
                os.write(bytes);
            }

            @Override
            public void onIOException(IOException e) throws IOException {
            }

            @Override
            public void onClosed() {
            }
        }, sync);
    }

    /**
     * This method autocreates the parent
     *
     * @param dstFile
     * @param writeToFileCallback
     * @param sync
     * @throws IOException
     */
    public static void secureWrite(final File dstFile, final WriteToFileCallback writeToFileCallback, final SYNC sync) throws IOException {
        new NonInterruptibleRunnableException<IOException>() {
            @Override
            protected void execute() throws IOException, InterruptedException {
                final File tmpFile = new File(dstFile.getAbsolutePath() + ".bac");
                if (!dstFile.getParentFile().isDirectory()) {
                    if (!dstFile.getParentFile().mkdirs()) {
                        throw new IOException("failed to create parent for " + dstFile);
                    }
                }
                if (!tmpFile.delete() && tmpFile.exists()) {
                    throw new IOException("could not remove tmpFile" + tmpFile);
                }
                boolean finallyDeleteFileFlag = true;
                try {
                    IO.writeToFile(tmpFile, writeToFileCallback, sync);
                    if (!dstFile.delete() && dstFile.exists()) {
                        throw new IOException("could not remove dstFile" + dstFile);
                    }
                    final long timeStamp = Time.systemIndependentCurrentJVMTimeMillis();
                    int retry = 0;
                    while (!tmpFile.renameTo(dstFile)) {
                        retry++;
                        Thread.sleep(retry * 10);
                        if (Time.systemIndependentCurrentJVMTimeMillis() - timeStamp > 1000) {
                            throw new IOException("could not rename " + tmpFile + " to " + dstFile.exists());
                        }
                    }
                    finallyDeleteFileFlag = false;
                } finally {
                    if (finallyDeleteFileFlag) {
                        tmpFile.delete();
                    }
                }
            }
        }.startAndWait();
    }

    /**
     * This method autocreates the parent
     *
     * @param latestTimestampFile
     * @param serializeToJson
     * @param none
     * @throws IOException
     * @throws UnsupportedEncodingException
     */
    public static void secureWrite(final File file, final String utf8String, final SYNC sync) throws IOException {
        IO.secureWrite(file, utf8String.getBytes(BOM.UTF8.getCharSet()), sync);
    }

    /**
     * Want to get informed in case of any io problems, set this handler
     *
     * @param handler
     */
    @Deprecated
    public static void setErrorHandler(final IOErrorHandler handler) {
    }

    /**
     * This methods does NOT create the parent but throws an exception if the arent of file does not exist
     *
     * @param file
     * @param string
     * @throws IOException
     */
    public static void writeStringToFile(final File file, final String string) throws IOException {
        IO.writeStringToFile(file, string, false, SYNC.META_AND_DATA);
    }

    /**
     * This methods does NOT create the parent but throws an exception if the arent of file does not exist
     *
     * @param file
     * @param string
     * @param append
     * @throws IOException
     */
    public static void writeStringToFile(final File file, final String string, final boolean append) throws IOException {
        IO.writeStringToFile(file, string, append, SYNC.META_AND_DATA);
    }

    /**
     * This methods does NOT create the parent but throws an exception if the arent of file does not exist
     *
     * @param file
     * @param data
     * @throws IOException
     */
    public static void writeToFile(final File file, final byte[] data) throws IOException {
        IO.writeToFile(file, data, SYNC.META_AND_DATA);
    }

    public static interface WriteToFileCallback {
        public void writeTo(final OutputStream os) throws IOException;

        public void onIOException(IOException e) throws IOException;

        public void onClosed();
    }

    /**
     * This methods does NOT create the parent but throws an exception if the arent of file does not exist
     *
     * @param file
     * @param data
     * @param sync
     * @throws IOException
     */
    public static void writeToFile(final File file, final byte[] data, final SYNC sync) throws IOException {
        writeToFile(file, new WriteToFileCallback() {
            @Override
            public void writeTo(OutputStream os) throws IOException {
                os.write(data);
            }

            @Override
            public void onIOException(IOException e) throws IOException {
            }

            @Override
            public void onClosed() {
            }
        }, sync);
    }

    /**
     * This methods does NOT create the parent but throws an exception if the arent of file does not exist
     *
     * @param file
     * @param string
     * @param append
     * @param sync
     * @throws IOException
     */
    public static void writeStringToFile(final File file, final String string, final boolean append, final SYNC sync) throws IOException {
        if (file == null) {
            throw new IllegalArgumentException("File is null.");
        } else if (file.exists() && !append) {
            throw new IllegalArgumentException("File already exists: " + file);
        } else if (!file.exists()) {
            file.createNewFile();
        }
        if (!file.isFile()) {
            throw new IllegalArgumentException("Is not a file: " + file);
        } else if (!file.canWrite()) {
            throw new IllegalArgumentException("Cannot write to file: " + file);
        }
        boolean finallyDeleteFileFlag = true;
        try {
            final FileOutputStream fileOutputStream = new FileOutputStream(file, append);
            try {
                final Writer writer = new OutputStreamWriter(fileOutputStream, BOM.UTF8.getCharSet());
                writer.write(string);
                writer.flush();
                if (sync != null) {
                    switch (sync) {
                    case DATA:
                        fileOutputStream.getChannel().force(false);
                        break;
                    case META_AND_DATA:
                        fileOutputStream.getChannel().force(true);
                        break;
                    case NONE:
                    default:
                        break;
                    }
                }
                writer.close();
                finallyDeleteFileFlag = false;
            } finally {
                try {
                    fileOutputStream.close();
                } catch (final Throwable e) {
                }
            }
        } finally {
            if (finallyDeleteFileFlag) {
                file.delete();
            }
        }
    }

    /**
     * This methods does NOT create the parent but throws an exception if the arent of file does not exist
     *
     * @param file
     * @param writeToFileCallback
     * @param sync
     * @throws IOException
     */
    public static void writeToFile(final File file, final WriteToFileCallback writeToFileCallback, final SYNC sync) throws IOException {
        if (file == null) {
            throw new IllegalArgumentException("File is null.");
        } else if (file.exists()) {
            throw new IllegalArgumentException("File already exists: " + file);
        } else if (writeToFileCallback == null) {
            throw new IllegalArgumentException("WriteToFileCallback is null.");
        }
        file.createNewFile();
        if (!file.isFile()) {
            throw new IllegalArgumentException("Is not a file: " + file);
        } else if (!file.canWrite()) {
            throw new IllegalArgumentException("Cannot write to file: " + file);
        }
        boolean finallyDeleteFileFlag = true;
        try {
            final FileOutputStream fileOutputStream = new FileOutputStream(file);
            try {
                writeToFileCallback.writeTo(new OutputStream() {
                    @Override
                    public void write(int b) throws IOException {
                        fileOutputStream.write(b);
                    }

                    @Override
                    public void write(byte[] b, int off, int len) throws IOException {
                        fileOutputStream.write(b, off, len);
                    }

                    @Override
                    public void write(byte[] b) throws IOException {
                        fileOutputStream.write(b);
                    }

                    @Override
                    public void flush() throws IOException {
                        fileOutputStream.flush();
                    }

                    @Override
                    public void close() throws IOException {
                    }
                });
                fileOutputStream.flush();
                if (sync != null) {
                    switch (sync) {
                    case DATA:
                        fileOutputStream.getChannel().force(false);
                        break;
                    case META_AND_DATA:
                        fileOutputStream.getChannel().force(true);
                        break;
                    case NONE:
                    default:
                        break;
                    }
                }
                fileOutputStream.close();
                finallyDeleteFileFlag = false;
            } finally {
                try {
                    fileOutputStream.close();
                } catch (final Throwable e) {
                }
                writeToFileCallback.onClosed();
            }
        } catch (final IOException e) {
            writeToFileCallback.onIOException(e);
            throw e;
        } finally {
            if (finallyDeleteFileFlag) {
                file.delete();
            }
        }
    }

    /**
     * @param ico
     * @return
     * @throws IOException
     */
    public static byte[] dataUrlToBytes(String dataURL) throws IOException {
        return readStream(-1, dataUrlToInputStream(dataURL));
    }

    /**
     * @param dataURL
     * @return
     * @throws IOException
     */
    public static InputStream dataUrlToInputStream(String dataURL) throws IOException {
        final int base64Index = dataURL.indexOf(";base64,");
        final CharBuffer cb;
        if (base64Index > 0 && base64Index + 8 < dataURL.length()) {
            cb = CharBuffer.wrap(dataURL, base64Index + 8, dataURL.length());
        } else {
            cb = CharBuffer.wrap(dataURL);
        }
        final InputStream is = new Base64InputStream(new CharSequenceInputStream(cb, BOM.UTF8.charSet));
        final String gzip = "data:application/gzip;";
        if (dataURL.length() > gzip.length() && dataURL.substring(0, gzip.length()).equalsIgnoreCase(gzip)) {
            return new GZIPInputStream(is);
        } else {
            return is;
        }
    }
}
