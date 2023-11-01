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

import java.io.IOException;
import java.io.OutputStream;
import java.security.MessageDigest;
import java.util.Arrays;

import org.appwork.utils.net.CountingOutputStream;

/**
 * @author Daniel Wilhelm
 *
 */
public class AWFCOutputStream extends OutputStream {
    /**
     * Appwork FileContainer *
     */
    private final OutputStream   os;
    private AWFCEntryOptions     currentEntry                = null;
    private CountingOutputStream currentCountingOutputStream = null;
    private final MessageDigest  md;
    protected boolean            headerWritten               = false;
    protected final AWFCUtils    utils;
    private boolean              closing                     = false;
    private int                  version                     = 1;

    public synchronized void setVersion(int version) throws IllegalArgumentException, IllegalStateException {
        if (headerWritten) {
            throw new IllegalStateException("header already written");
        }
        switch (version) {
        case 1:
            // version 1, doesn't store hash for files with size == 0
        case 2:
            // version 2, does store hash for files with size == 0
            break;
        default:
            throw new IllegalArgumentException("Unknown AWFC Version " + version);
        }
        this.version = version;
    }

    public AWFCOutputStream(final OutputStream os, final MessageDigest md) {
        this.os = os;
        this.md = md;
        this.utils = new AWFCUtils() {
            @Override
            public OutputStream getCurrentOutputStream() throws IOException {
                return AWFCOutputStream.this.getCurrentOutputStream();
            }
        };
    }

    @Override
    public synchronized void close() throws IOException {
        this.closing = true;
        if (this.headerWritten == false) {
            this.writeAWFCHeader();
        }
        this.getCurrentOutputStream().close();
    }

    private synchronized void closeLastEntry() throws IOException {
        if (this.currentEntry != null) {
            /* verify if currentEntry is complete */
            final long bytesWritten = this.currentCountingOutputStream.transferedBytes();
            final AWFCEntry entry = this.currentEntry.getEntry();
            if (this.currentEntry.hasPayLoad()) {
                if (entry.getSize() != bytesWritten) {
                    throw new IOException("Wrong size for Entry: " + entry + " != " + bytesWritten);
                }
                if (entry.isFile() && this.md != null && !Arrays.equals(entry.getHash(), this.md.digest())) {
                    throw new IOException("Wrong hash for Entry: " + entry);
                }
            } else {
                if (bytesWritten > 0) {
                    throw new IOException("Entry must not have payLoad: " + entry + " != " + bytesWritten);
                }
            }
            /* we want to write on original OutputStream again */
            this.currentCountingOutputStream = null;
            this.currentEntry = null;
        } else {
            throw new IOException("No lastEntry to close!");
        }
    }

    @Override
    public synchronized void flush() throws IOException {
        this.getCurrentOutputStream().flush();
    }

    protected synchronized OutputStream getCurrentOutputStream() throws IOException {
        if (this.currentCountingOutputStream != null) {
            return this.currentCountingOutputStream;
        }
        if (this.currentEntry != null) {
            return this.os;
        }
        if (this.closing) {
            return this.os;
        }
        throw new IOException("No Entry added yet!");
    }

    public synchronized void putNextEntry(final AWFCEntry e) throws IOException {
        this.putNextEntry(e, false);
    }

    public synchronized void putNextEntry(final AWFCEntry entry, boolean noPayLoad) throws IOException {
        if (this.currentEntry != null) {
            this.closeLastEntry();
        }
        if (entry.isFile() == false) {
            /* folders do not have any payload */
            noPayLoad = true;
        }
        /* wrap AWFCEntry into AWFCEntryOptions */
        this.currentEntry = new AWFCEntryOptions(entry, noPayLoad);
        if (this.headerWritten == false) {
            this.writeAWFCHeader();
        }
        /* write AWFCHeader */
        this.writeAWFCEntry(this.currentEntry);
        if (this.md != null) {
            this.md.reset();
        }
        this.currentCountingOutputStream = new CountingOutputStream(this.os) {
            @Override
            public void close() throws IOException {
                try {
                    AWFCOutputStream.this.closeLastEntry();
                } finally {
                    super.close();
                }
            }

            @Override
            public void write(final byte[] b) throws IOException {
                super.write(b);
                if (AWFCOutputStream.this.md != null) {
                    AWFCOutputStream.this.md.update(b);
                }
            }

            @Override
            public void write(final byte[] b, final int off, final int len) throws IOException {
                super.write(b, off, len);
                if (AWFCOutputStream.this.md != null) {
                    AWFCOutputStream.this.md.update(b, off, len);
                }
            }

            @Override
            public void write(final int b) throws IOException {
                super.write(b);
                if (AWFCOutputStream.this.md != null) {
                    AWFCOutputStream.this.md.update((byte) b);
                }
            }
        };
    }

    @Override
    public synchronized void write(final byte b[]) throws IOException {
        this.getCurrentOutputStream().write(b);
    }

    @Override
    public synchronized void write(final byte[] b, final int off, final int len) throws IOException {
        this.getCurrentOutputStream().write(b, off, len);
    }

    @Override
    public synchronized void write(final int b) throws IOException {
        this.getCurrentOutputStream().write(b);
    }

    public int getVersion() {
        return version;
    }

    protected void writeAWFCEntry(final AWFCEntryOptions awfcEntryOptions) throws IOException {
        final AWFCEntry entry = awfcEntryOptions.getEntry();
        /* write filePath */
        this.utils.writeString(entry.getPath());
        int entryOptions = 0;
        if (entry.isFile()) {
            /* entry is a File */
            entryOptions = entryOptions | 1;
        }
        if (awfcEntryOptions.hasPayLoad() == false) {
            /* entry has no payLoad */
            entryOptions = entryOptions | 2;
        }
        /* write entryOptions */
        this.write(entryOptions);
        if (entry.isFile()) {
            /* write entrySize */
            this.utils.writeLongOptimized(entry.getSize());
            if (this.md != null && (entry.getSize() > 0 || getVersion() >= 2)) {
                /* only write Hash when MessageDigest is set */
                if (entry.getHash() == null) {
                    throw new IOException("Hash is missing for Entry: " + entry);
                }
                if (entry.getHash().length != this.md.getDigestLength()) {
                    throw new IOException("Hashlength does not match for Entry: " + entry);
                }
                this.write(entry.getHash());
            }
        }
    }

    protected synchronized void writeAWFCHeader() throws IOException {
        this.headerWritten = true;
        version = getVersion();
        /* write AWFC Version */
        this.write(version);
        /* write MessageDigest set */
        this.utils.writeBoolean(this.md != null);
        if (this.md != null) {
            /* write MessageDigest Type */
            this.utils.writeString(this.md.getAlgorithm());
            /* write MessageDigest Length */
            this.utils.writeShort(this.md.getDigestLength());
        }
    }
}
