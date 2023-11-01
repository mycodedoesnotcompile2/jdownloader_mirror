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
package org.appwork.utils.net.usenet;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PushbackInputStream;
import java.util.Arrays;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.appwork.utils.Exceptions;

public class YEncInputStream extends InputStream {
    public class YEncIndexException extends IOException {
        private final String messageID;
        private final int    expected;

        public String getMessageID() {
            return messageID;
        }

        public int getIndex() {
            return index;
        }

        public int getExpected() {
            return expected;
        }

        private final int index;

        protected YEncIndexException(final int index) {
            super("part-index-error:" + index + "<->" + YEncInputStream.this.getPartIndex() + "|yEncTrailer:" + getYEncTrailer());
            this.index = index;
            this.messageID = getMessageID();
            this.expected = getPartIndex();
        }
    }

    public class YEncSizeException extends IOException {
        private final long size;

        public long getSize() {
            return size;
        }

        public String getMessageID() {
            return messageID;
        }

        private final long expected;

        public long getExpected() {
            return expected;
        }

        private final String  messageID;
        private final boolean isMultiPart;

        public boolean isMultiPart() {
            return isMultiPart;
        }

        protected YEncSizeException(final long size) {
            super((YEncInputStream.this.isMultiPart() ? "part-" : "") + "size-error:" + size + "<->" + YEncInputStream.this.getPartSize() + "|yEncTrailer:" + getYEncTrailer());
            this.size = size;
            this.expected = getPartSize();
            this.messageID = getMessageID();
            this.isMultiPart = isMultiPart();
        }
    }

    public class YEncDecodedSizeException extends IOException {
        private final long size;

        public long getSize() {
            return size;
        }

        public long getExpected() {
            return expected;
        }

        public String getMessageID() {
            return messageID;
        }

        private final long   expected;
        private final String messageID;

        protected YEncDecodedSizeException(final long size, final long expected) {
            super("decoded-size-error:" + size + "<->" + expected + "|yEncTrailer:" + getYEncTrailer());
            this.size = size;
            this.expected = expected;
            this.messageID = getMessageID();
        }
    }

    /**
     * http://www.yenc.org/yenc-draft.1.3.txt
     */
    private final InputStream           inputStream;
    private final ByteArrayOutputStream buffer;
    private final long                  size;
    private final String                name;
    private String                      yEncTrailer  = null;
    private final int                   lineLength;
    private final boolean               isMultiPart;
    private final long                  partBegin;
    private long                        decodedBytes = 0;
    private boolean                     eof          = false;
    protected final static Pattern      NUMBER       = Pattern.compile("\\d+");
    protected final static Pattern      CRC32        = Pattern.compile("[a-fA-F0-9]+");

    private final static byte           LF           = '\n';
    private final static byte           CR           = '\r';
    private final static byte           ESCAPE       = '=';
    private final static byte           DOT          = '.';

    /**
     * returns the starting points, in bytes, of the block in the original file
     *
     * @return
     */
    public long getPartBegin() {
        return partBegin;
    }

    /**
     * returns the ending points, in bytes, of the block in the original file
     *
     * @return
     */
    public long getPartEnd() {
        return partEnd;
    }

    private final long partEnd;

    /**
     * is the original file multi-part yEnc encoded
     *
     * @return
     */
    public boolean isMultiPart() {
        return isMultiPart;
    }

    /**
     * returns the part-index of multi-part yEnc encoded original file
     *
     * @return
     */
    public int getPartIndex() {
        return partIndex;
    }

    private final int          partIndex;
    private final int          partTotal;
    private final SimpleUseNet client;
    private final String       messageID;

    public String getMessageID() {
        return messageID;
    }

    /**
     * returns the number of total parts in a multi-part yEnc encoded original file
     *
     * return -1 for unknown total parts (older yEnc versions)
     *
     * @return
     */
    public int getPartTotal() {
        return partTotal;
    }

    @Override
    public int available() throws IOException {
        if (decodedIndex < decodedLength) {
            return decodedLength - decodedIndex;
        }
        return 0;
    }

    protected YEncInputStream(SimpleUseNet client, final String messageID, ByteArrayOutputStream buffer) throws IOException {
        this.messageID = messageID;
        this.client = client;
        this.inputStream = client.getInputStream();
        this.buffer = buffer;
        String line = new String(buffer.toByteArray(), 0, buffer.size(), client.getCharSet());
        if (!line.startsWith("=ybegin")) {
            throw new IOException("missing =ybegin");
        }
        final String lineValue = getValue(line, "line", NUMBER);
        this.lineLength = lineValue != null ? Integer.parseInt(lineValue) : -1;
        name = getValue(line, "name", null);
        final String sizeValue = getValue(line, "size", NUMBER);
        this.size = sizeValue != null ? Long.parseLong(sizeValue) : -1l;
        final String partValue = getValue(line, "part", NUMBER);
        partIndex = partValue != null ? Integer.parseInt(partValue) : -1;
        isMultiPart = partIndex != -1;
        if (isMultiPart) {
            final String totalValue = getValue(line, "total", NUMBER);
            partTotal = totalValue != null ? Integer.parseInt(totalValue) : -1;
        } else {
            partTotal = -1;
        }
        if (isMultiPart) {
            buffer.reset();
            line = client.readLine(buffer);
            if (!line.startsWith("=ypart")) {
                throw new IOException("missing =ypart");
            }
            final String beginValue = getValue(line, "begin", NUMBER);
            partBegin = beginValue != null ? Long.parseLong(beginValue) : -1;
            final String endValue = getValue(line, "end", NUMBER);
            partEnd = endValue != null ? Long.parseLong(endValue) : -1;
        } else {
            partBegin = -1;
            partEnd = -1;
        }
        encodedBuffer = new byte[8192];
        decodedBuffer = new byte[encodedBuffer.length];
        if (encodedBuffer.length < 5) {
            throw new IllegalArgumentException("encodedBuffer requires minimum size of 5 '=yend'!");
        }
    }

    public int getDecodedBufferSize() {
        return decodedBuffer.length;
    }

    protected final InputStream getInputStream() {
        return inputStream;
    }

    private String crc32Value = null;

    public String getFileCRC32() {
        return crc32Value;
    }

    private String pcrc32Value = null;

    public String getPartCRC32() {
        return pcrc32Value;
    }

    private final byte[] decodedBuffer;
    private int          decodedIndex  = 0;
    private int          decodedLength = 0;

    private synchronized final int readNextDecodedByte() throws IOException {
        if (decodedIndex < decodedLength) {
            final int ret = decodedBuffer[decodedIndex++] & 0xff;
            return ret;
        } else {
            return -1;
        }
    }

    private synchronized final int readNextDecodedBytes(final byte[] b, final int off, final int len) throws IOException {
        if (len > 0) {
            if (decodedIndex < decodedLength) {
                final int ret = Math.min(len, decodedLength - decodedIndex);
                System.arraycopy(decodedBuffer, decodedIndex, b, off, ret);
                decodedIndex += ret;
                return ret;
            } else {
                return -1;
            }
        } else {
            return 0;
        }
    }

    private final byte[] encodedBuffer;
    private int          encodedLength = 0;

    private int indexOf(byte[] buf, int size, byte indexOf) {
        for (int index = 0; index < size; index++) {
            if (buf[index] == indexOf) {
                return index;
            }
        }
        return -1;
    }

    private byte[] removeCRLF(final byte[] buf, final int length) {
        final byte[] ret = new byte[length];
        int count = 0;
        for (int index = 0; index < ret.length; index++) {
            final byte b = buf[index];
            if (b != CR && b != LF) {
                ret[count++] = b;
            }
        }
        if (count == 0) {
            // empty line
            return null;
        } else {
            return Arrays.copyOf(ret, count);
        }
    }

    private synchronized final int decodeLine() throws IOException {
        final int lf_index = indexOf(encodedBuffer, encodedLength, LF);
        if (lf_index == -1) {
            return 0;
        } else if (encodedBuffer[0] == '=' && encodedBuffer[1] == 'y' && encodedBuffer[2] == 'e' && encodedBuffer[3] == 'n' && encodedBuffer[4] == 'd') {
            eof = true;
            final PushbackInputStream inputStream = new PushbackInputStream(getInputStream(), encodedLength);
            inputStream.unread(encodedBuffer, 0, encodedLength);
            parseTrailer(inputStream);
            return 0;
        } else {
            final byte[] buf = removeCRLF(encodedBuffer, lf_index);
            int decoded = 0;
            if (buf != null) {
                int index = 0;
                if (buf.length > 1 && buf[0] == DOT && buf[1] == DOT) {
                    // special nntp double dot handling
                    index++;
                }
                while (index < buf.length) {
                    byte b = buf[index++];
                    switch (b) {
                    case LF:
                    case CR:
                        continue;
                    case ESCAPE:
                        escape: while (index < buf.length) {
                            b = buf[index++];
                            switch (b) {
                            case LF:
                            case CR:
                                continue;
                            default:
                                b = (byte) (((byte) (b - 64)) & 0xff);
                                decodedBuffer[decodedLength + (decoded++)] = (byte) (((byte) (b - 42)) & 0xff);
                                break escape;
                            }
                        }
                        break;
                    default:
                        decodedBuffer[decodedLength + (decoded++)] = (byte) (((byte) (b - 42)) & 0xff);
                        break;
                    }
                }
            }
            if (lf_index < encodedLength) {
                final int encodedLeft = encodedLength - lf_index - 1;
                if (encodedLeft > 0) {
                    System.arraycopy(encodedBuffer, lf_index + 1, encodedBuffer, 0, encodedLeft);
                }
                encodedLength = encodedLeft;
            } else {
                encodedLength = 0;
            }
            decodedBytes += decoded;
            if (decoded == 0) {
                return decodeLine();
            } else {
                return decoded;
            }
        }
    }

    private synchronized final int fillDecodedBuffer() throws IOException {
        if (decodedIndex < decodedLength) {
            return decodedLength - decodedIndex;
        } else {
            decodedIndex = 0;
            decodedLength = 0;
            int lineDecoded = decodeLine();
            if (lineDecoded > 0) {
                decodedLength += lineDecoded;
                return decodedLength - decodedIndex;
            } else if (eof) {
                return -1;
            } else {
                final int maxRead = encodedBuffer.length - encodedLength;
                if (maxRead == 0) {
                    throw new IllegalStateException("maxRead=0");
                }
                final int encodedRead = getInputStream().read(encodedBuffer, encodedLength, maxRead);
                if (encodedRead == -1) {
                    eof = true;
                    return -1;
                } else if (encodedRead == 0) {
                    return 0;
                } else {
                    encodedLength += encodedRead;
                }
                lineDecoded = decodeLine();
                if (lineDecoded > 0) {
                    decodedLength += lineDecoded;
                    return decodedLength - decodedIndex;
                } else {
                    return 0;
                }
            }
        }
    }

    @Override
    public synchronized int read() throws IOException {
        int ret = readNextDecodedByte();
        if (ret == -1) {
            while (true) {
                final int available = fillDecodedBuffer();
                if (available > 0) {
                    break;
                } else if (available == -1) {
                    return -1;
                } else {
                    try {
                        Thread.sleep(1);
                    } catch (final InterruptedException e) {
                        throw new IOException(e);
                    }
                }
            }
            ret = readNextDecodedByte();
        }
        return ret;
    }

    @Override
    public synchronized int read(final byte[] b, final int off, final int len) throws IOException {
        if (len > 0) {
            int ret = readNextDecodedBytes(b, off, len);
            if (ret == -1) {
                while (true) {
                    final int available = fillDecodedBuffer();
                    if (available > 0) {
                        break;
                    } else if (available == -1) {
                        return -1;
                    } else {
                        try {
                            Thread.sleep(1);
                        } catch (final InterruptedException e) {
                            throw new IOException(e);
                        }
                    }
                }
                ret = readNextDecodedBytes(b, off, len);
            }
            return ret;
        } else {
            return 0;
        }
    }

    /**
     * read and parse yEnc trailer
     *
     * @param inputStream
     * @throws IOException
     */
    private void parseTrailer(final InputStream inputStream) throws IOException {
        buffer.reset();
        final int lineSize = readLine(inputStream);
        final byte[] lineBuffer = buffer.toByteArray();
        yEncTrailer = new String(lineBuffer, 0, lineSize, client.getCharSet());
        final String sizeValue = getValue(getYEncTrailer(), "size", NUMBER);
        final long size = sizeValue != null ? Long.parseLong(sizeValue) : -1;
        pcrc32Value = getValue(getYEncTrailer(), "pcrc32", CRC32);
        crc32Value = getValue(getYEncTrailer(), " crc32", CRC32);// space is important to differ between pcrc32 and crc32
        // read body to end to drain inputstream
        IOException bodyEndException = null;
        try {
            readBodyEnd(inputStream);
        } catch (IOException e) {
            bodyEndException = e;
        }
        try {
            // error checks
            if (isMultiPart()) {
                if (size != getPartSize()) {
                    throw new YEncSizeException(size);
                }
                final String partValueString = getValue(getYEncTrailer(), "part", NUMBER);
                if (partValueString != null) {
                    final int partValueInt = Integer.parseInt(partValueString);
                    if (partValueInt != getPartIndex()) {
                        throw new YEncIndexException(partValueInt);
                    }
                }
            } else {
                if (size != getSize()) {
                    throw new YEncSizeException(size);
                }
            }
            if (decodedBytes < size) {
                throw new YEncDecodedSizeException(decodedBytes, size);
            }
        } catch (IOException e) {
            throw Exceptions.addSuppressed(e, bodyEndException);
        }
        if (bodyEndException != null) {
            throw bodyEndException;
        }
    }

    public String getYEncTrailer() {
        return yEncTrailer;
    }

    protected int readLine(InputStream is) throws IOException {
        return client.readLine(is, buffer);
    }

    /**
     * read body end until "."
     *
     * @param is
     * @throws IOException
     */
    private void readBodyEnd(final InputStream is) throws IOException {
        final BodyInputStream bodyInputStream = new BodyInputStream(is);
        final byte[] bodyEndBuf = new byte[32];
        while (true) {
            if (bodyInputStream.read(bodyEndBuf) == -1) {
                bodyInputStream.close();
                break;
            }
        }
    }

    @Override
    public void close() throws IOException {
    }

    /**
     * returns the name of the original file
     *
     * @return
     */
    public String getName() {
        return name;
    }

    /**
     * returns the complete filesize of the original file
     *
     * @return
     */
    public long getSize() {
        return size;
    }

    /**
     * returns the size of the current part
     *
     * @return
     */
    public long getPartSize() {
        if (isMultiPart) {
            return getPartEnd() - getPartBegin() + 1;
        } else {
            return -1;
        }
    }

    /**
     * returns the line length of yEnc encoding
     *
     * @return
     */
    public int getLineLength() {
        return lineLength;
    }

    protected String getValue(final String line, final String key, final Pattern valuePattern) {
        final String search = key + "=";
        final int start = line.indexOf(search);
        if (start == -1) {
            return null;
        } else {
            final int end;
            if ("name".equals(key)) {
                /* special handling for name(last key/value to allow spaces) */
                end = line.length();
            } else {
                final int index = line.indexOf(" ", start + search.length());
                if (index == -1) {
                    end = line.length();
                } else {
                    end = index;
                }
            }
            final String ret = line.substring(start + search.length(), end);
            if (valuePattern != null) {
                final Matcher matcher = valuePattern.matcher(ret);
                if (matcher.find()) {
                    return matcher.group();
                }
            }
            return ret;
        }
    }
}
