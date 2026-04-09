/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.loggingv3.simple.sink;

import java.io.BufferedInputStream;
import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.appwork.utils.IO;

/**
 * Iterates log entries of a {@link LogToFileSink} from newest to oldest. Opens one segment file at a time; each file is read backwards in
 * chunks. Entries are separated by a fixed byte sequence (default CRLF) — no regular expressions.
 * <p>
 * The current segment is backed by an open {@link RandomAccessFile}. If iteration stops before {@link #hasNext()} returns false, call
 * {@link #close()} (for example in a {@code finally} block) so the file handle and any temporary zip materialization are released.
 *
 * @author Thomas
 */
public final class LogIterator implements Iterator<String>, Closeable {
    /**
     * Default entry delimiter: CRLF ({@code \\r\\n}).
     */
    public static final byte[]                DEFAULT_ENTRY_DELIMITER = new byte[] { '\r', '\n' };
    private final LatestFirstLogEntryIterator delegate;
    private static final Charset              UTF8                    = Charset.forName("UTF-8");

    /**
     * Same as {@link #LogIterator(LogToFileSink, byte[])} with {@link #DEFAULT_ENTRY_DELIMITER}.
     */
    public LogIterator(final LogToFileSink sink) {
        this(sink, DEFAULT_ENTRY_DELIMITER);
    }

    /**
     * @param sink
     *            source sink; uses {@link LogToFileSink#listLogDataFilesNewestFirst()}
     * @param entryDelimiter
     *            non-empty byte sequence between entries (matched exactly, scanned backwards)
     */
    public LogIterator(final LogToFileSink sink, final byte[] entryDelimiter) {
        if (entryDelimiter.length == 0) {
            throw new IllegalArgumentException("entryDelimiter must not be empty");
        }
        this.delegate = new LatestFirstLogEntryIterator(sink.listLogDataFilesNewestFirst(), Arrays.copyOf(entryDelimiter, entryDelimiter.length));
    }

    @Override
    public boolean hasNext() {
        return this.delegate.hasNext();
    }

    @Override
    public String next() {
        return this.delegate.next();
    }

    /**
     * Closes the open log segment (if any) and marks this iterator as finished. Safe to call multiple times. After {@code close},
     * {@link #hasNext()} is false and {@link #next()} throws {@link NoSuchElementException}.
     */
    @Override
    public void close() throws IOException {
        this.delegate.close();
    }

    private static int lastIndexOfBytes(final byte[] haystack, final byte[] needle) {
        if (needle.length == 0 || haystack.length < needle.length) {
            return -1;
        }
        outer: for (int i = haystack.length - needle.length; i >= 0; i--) {
            for (int j = 0; j < needle.length; j++) {
                if (haystack[i + j] != needle[j]) {
                    continue outer;
                }
            }
            return i;
        }
        return -1;
    }

    private static File materializeZipLogToTemp(final File zipFile) throws IOException {
        final File tmp = File.createTempFile("logiter-", ".txt");
        tmp.deleteOnExit();
        final FileInputStream fis = new FileInputStream(zipFile);
        try {
            final ZipInputStream zin = new ZipInputStream(new BufferedInputStream(fis));
            final ZipEntry ze = zin.getNextEntry();
            if (ze == null) {
                return null;
            }
            final FileOutputStream fos = new FileOutputStream(tmp);
            try {
                IO.readStreamToOutputStream(-1, zin, fos, true);
            } finally {
                fos.close();
            }
        } finally {
            fis.close();
        }
        return tmp;
    }

    private static final class LatestFirstLogEntryIterator implements Iterator<String> {
        private final List<File>             files;
        private final byte[]                 delimiter;
        private int                          fileIndex;
        private ReverseByteDelimitedIterator current;
        private boolean                      closed;

        LatestFirstLogEntryIterator(final List<File> files, final byte[] delimiter) {
            this.files = new ArrayList<File>(files);
            this.delimiter = delimiter;
            this.fileIndex = 0;
            this.current = null;
            this.closed = false;
        }

        void close() {
            if (this.closed) {
                return;
            }
            this.closed = true;
            if (this.current != null) {
                this.current.closeQuietly();
                this.current = null;
            }
            this.fileIndex = this.files.size();
        }

        private void openNextNonEmptyFile() {
            if (this.closed) {
                return;
            }
            this.current = null;
            while (this.fileIndex < this.files.size()) {
                final File file = this.files.get(this.fileIndex++);
                try {
                    if (file.getName().endsWith(".zip")) {
                        final File zipFirstEntry = materializeZipLogToTemp(file);
                        if (zipFirstEntry == null) {
                            continue;
                        }
                        this.current = new ReverseByteDelimitedIterator(zipFirstEntry, this.delimiter, true);
                    } else {
                        this.current = new ReverseByteDelimitedIterator(file, this.delimiter, false);
                    }
                    if (this.current.hasNext()) {
                        return;
                    }
                    this.current.closeQuietly();
                    this.current = null;
                } catch (final IOException e) {
                    if (this.current != null) {
                        this.current.closeQuietly();
                    }
                    // UncheckedIOException is java 1.8 only
                    throw new RuntimeException(e);
                }
            }
        }

        @Override
        public boolean hasNext() {
            if (this.closed) {
                return false;
            }
            while (true) {
                if (this.current != null) {
                    if (this.current.hasNext()) {
                        return true;
                    }
                    this.current.closeQuietly();
                    this.current = null;
                }
                if (this.fileIndex >= this.files.size()) {
                    return false;
                }
                openNextNonEmptyFile();
                if (this.current != null) {
                    return true;
                }
            }
        }

        @Override
        public String next() {
            if (this.closed) {
                throw new NoSuchElementException();
            }
            if (!hasNext()) {
                throw new NoSuchElementException();
            }
            return this.current.next();
        }
    }

    private static final class ReverseByteDelimitedIterator implements Iterator<String> {
        private static final int       CHUNK = 64 * 1024;
        private final RandomAccessFile raf;
        private final File             physicalFile;
        private final boolean          deletePhysicalWhenDone;
        private final byte[]           delimiter;
        private long                   readBefore;
        private byte[]                 carry = new byte[0];
        private String                 prefetchedNext;
        private boolean                eofReached;
        private boolean                closed;

        ReverseByteDelimitedIterator(final File f, final byte[] delimiter, final boolean deleteWhenDone) throws IOException {
            this.raf = new RandomAccessFile(f, "r");
            this.physicalFile = f;
            this.deletePhysicalWhenDone = deleteWhenDone;
            this.delimiter = delimiter;
            this.readBefore = this.raf.length();
        }

        private void prependChunk(final byte[] buf, final int off, final int len) {
            if (len <= 0) {
                return;
            }
            final byte[] n = new byte[len + this.carry.length];
            System.arraycopy(buf, off, n, 0, len);
            System.arraycopy(this.carry, 0, n, len, this.carry.length);
            this.carry = n;
        }

        private void readPreviousChunk() throws IOException {
            final long start = Math.max(0, this.readBefore - CHUNK);
            final int len = (int) (this.readBefore - start);
            this.raf.seek(start);
            final byte[] buf = new byte[len];
            this.raf.readFully(buf);
            this.readBefore = start;
            prependChunk(buf, 0, len);
        }

        private void prefetch() {
            if (this.prefetchedNext != null || this.eofReached || this.closed) {
                return;
            }
            try {
                while (true) {
                    final int idx = lastIndexOfBytes(this.carry, this.delimiter);
                    if (idx >= 0) {
                        final int entryStart = idx + this.delimiter.length;
                        final byte[] entryBytes = Arrays.copyOfRange(this.carry, entryStart, this.carry.length);
                        this.carry = Arrays.copyOfRange(this.carry, 0, idx);
                        if (entryBytes.length > 0) {
                            this.prefetchedNext = new String(entryBytes, UTF8);
                            return;
                        }
                        continue;
                    }
                    if (this.readBefore > 0) {
                        readPreviousChunk();
                        continue;
                    }
                    if (this.carry.length > 0) {
                        this.prefetchedNext = new String(this.carry, UTF8);
                        this.carry = new byte[0];
                    }
                    this.eofReached = true;
                    closeQuietly();
                    return;
                }
            } catch (final IOException e) {
                closeQuietly();
                // UncheckedIOException is java 1.8 only
                throw new RuntimeException(e);
            }
        }

        void closeQuietly() {
            if (this.closed) {
                return;
            }
            this.closed = true;
            try {
                this.raf.close();
            } catch (final IOException ignore) {
            }
            if (this.deletePhysicalWhenDone && this.physicalFile != null) {
                this.physicalFile.delete();
            }
        }

        @Override
        public boolean hasNext() {
            prefetch();
            return this.prefetchedNext != null;
        }

        @Override
        public String next() {
            prefetch();
            final String next = this.prefetchedNext;
            if (next == null) {
                throw new NoSuchElementException();
            }
            this.prefetchedNext = null;
            return next;
        }
    }
}
