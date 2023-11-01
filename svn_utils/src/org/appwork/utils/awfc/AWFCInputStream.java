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
package org.appwork.utils.awfc;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;

import org.appwork.utils.net.LimitedInputStream;

/**
 * @author Daniel Wilhelm
 *
 */
public class AWFCInputStream extends InputStream {
    private final InputStream  is;
    private LimitedInputStream lis          = null;
    private MessageDigest      md           = null;
    private boolean            headerRead   = false;
    private AWFCEntryOptions   currentEntry = null;
    private final byte[]       skipBuffer   = new byte[32767];
    private AWFCUtils          utils;
    private int                version      = -1;

    public AWFCInputStream(final InputStream is) {
        this.is = is;
        this.utils = new AWFCUtils() {
            @Override
            public InputStream getCurrentInputStream() throws IOException {
                return AWFCInputStream.this.getCurrentInputStream();
            }
        };
    }

    @Override
    public int available() throws IOException {
        return this.getCurrentInputStream().available();
    }

    @Override
    public void close() throws IOException {
        this.getCurrentInputStream().close();
    }

    protected InputStream getInputStream() throws IOException {
        return this.is;
    }

    private synchronized InputStream getCurrentInputStream() throws IOException {
        if (this.lis != null) {
            return this.lis;
        } else {
            return this.is;
        }
    }

    private void verifyHash(AWFCEntryOptions entryOptions, final byte[] streamHash) throws IOException {
        final AWFCEntry entry = entryOptions.getEntry();
        final byte[] storedEntryHash = entry.getHash();
        if (!Arrays.equals(streamHash, storedEntryHash)) {
            if (storedEntryHash == null && entry.getSize() == 0 && getVersion() == 1) {
                // version 1 does not store hash for size=0 entries
                return;
            }
            throw new IOException("Wrong hash for Entry: " + entry);
        }
    }

    protected LimitedInputStream getLimitedInputStream(final AWFCEntryOptions entry) throws IOException {
        if (entry != null) {
            final MessageDigest md;
            if (this.md != null) {
                this.md.reset();
                md = this.md;
            } else {
                md = null;
            }
            final long inputLimit = entry.getEntry().getSize();
            final boolean hasPayLoad = entry.hasPayLoad();
            return new LimitedInputStream(this.is, hasPayLoad ? inputLimit : 0) {
                private byte[] hash = null;

                @Override
                public int available() throws IOException {
                    final long ret = this.getLimit() - this.transferedBytes();
                    if (ret > Integer.MAX_VALUE) {
                        return Integer.MAX_VALUE;
                    }
                    return (int) ret;
                }

                @Override
                public int read() throws IOException {
                    final int ret = super.read();
                    if (hasPayLoad && md != null) {
                        if (ret != -1) {
                            md.update((byte) ret);
                        } else if (ret == -1) {
                            if (hash == null) {
                                hash = md.digest();
                            }
                            verifyHash(currentEntry, hash);
                        }
                    }
                    return ret;
                }

                @Override
                public int read(final byte[] b, final int off, final int len) throws IOException {
                    final int ret = super.read(b, off, len);
                    if (hasPayLoad && md != null) {
                        if (ret > 0) {
                            md.update(b, off, ret);
                        } else if (ret == -1) {
                            if (hash == null) {
                                hash = md.digest();
                            }
                            verifyHash(currentEntry, hash);
                        }
                    }
                    return ret;
                }

                @Override
                public long skip(final long n) throws IOException {
                    if (n < AWFCInputStream.this.skipBuffer.length) {
                        return this.read(AWFCInputStream.this.skipBuffer, 0, (int) n);
                    } else {
                        return this.read(AWFCInputStream.this.skipBuffer, 0, this.skipBuffer.length);
                    }
                }
            };
        }
        return null;
    }

    protected synchronized void skipEntry(AWFCEntryOptions entry) throws IOException {
        while (this.lis.available() > 0) {
            this.lis.skip(this.lis.available());
        }
    }

    public synchronized AWFCEntry getNextEntry() throws IOException {
        this.readAWFCHeader();
        if (this.currentEntry != null) {
            try {
                skipEntry(currentEntry);
            } finally {
                this.lis = null;
                this.currentEntry = null;
            }
        }
        this.currentEntry = this.readAWFCEntry();
        if (this.currentEntry == null) {
            return null;
        } else {
            this.lis = getLimitedInputStream(currentEntry);
            return this.currentEntry.getEntry();
        }
    }

    @Override
    public synchronized void mark(final int readlimit) {
    }

    @Override
    public boolean markSupported() {
        return false;
    }

    @Override
    public int read() throws IOException {
        return this.getCurrentInputStream().read();
    }

    @Override
    public int read(final byte[] b) throws IOException {
        return this.getCurrentInputStream().read(b);
    }

    @Override
    public int read(final byte[] b, final int off, final int len) throws IOException {
        return this.getCurrentInputStream().read(b, off, len);
    }

    private AWFCEntryOptions readAWFCEntry() throws IOException {
        int stringSize = 0;
        try {
            stringSize = this.utils.readShort();
        } catch (final EOFException e) {
            return null;
        }
        final String path = this.utils.readString(stringSize);
        final int entryOptions = this.utils.ensureRead();
        final boolean isFolder = (entryOptions & 1) == 0;
        final boolean hasPayLoad = (entryOptions & 2) == 0;
        final AWFCEntry entry;
        if (isFolder) {
            entry = new AWFCEntry(path + "/", 0, null);
        } else {
            final long size = this.utils.readLongOptimized();
            final byte[] hash;
            if (this.md != null && (size > 0 || getVersion() >= 2)) {
                hash = this.utils.ensureRead(this.md.getDigestLength(), null);
            } else {
                hash = null;
            }
            entry = new AWFCEntry(path, size, hash);
        }
        return new AWFCEntryOptions(entry, !hasPayLoad);
    }

    public int getVersion() {
        return version;
    }

    public synchronized boolean readAWFCHeader() throws IOException {
        if (this.headerRead == false) {
            version = this.utils.ensureRead();
            switch (version) {
            case 1:
                // version 1, doesn't store hash for files with size == 0
            case 2:
                // version 2, does store hash for files with size == 0
                break;
            default:
                throw new IOException("Unknown AWFC Version " + version);
            }
            if (this.utils.readBoolean()) {
                final int stringSize = this.utils.readShort();
                final String mdAlgo = this.utils.readString(stringSize);
                try {
                    this.md = MessageDigest.getInstance(mdAlgo);
                } catch (final NoSuchAlgorithmException e) {
                    throw new IOException(e);
                }
                if (this.md.getDigestLength() != this.utils.readShort()) {
                    throw new IOException("Hashlength does not match for given md: " + mdAlgo);
                }
            }
            this.headerRead = true;
            return true;
        } else {
            return false;
        }
    }

    @Override
    public synchronized void reset() throws IOException {
    }

    @Override
    public long skip(final long n) throws IOException {
        return this.getCurrentInputStream().skip(n);
    }
}
