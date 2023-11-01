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
package org.appwork.utils.io.streamingio;

import java.io.IOException;
import java.io.InputStream;

/**
 * @author daniel
 * 
 */
public class StreamingInputStream extends InputStream {

    /**
     * 
     */
    public static final String DOWNLOAD_STREAM_IS_CLOSED = "DownloadStream is closed";
    protected final Streaming streaming;
    protected final long      startPosition;
    protected final long      endPosition;
    protected StreamingChunk  currentChunk     = null;
    protected byte[]          singleReadBuffer = new byte[1];
    protected long            currentPosition;

    protected StreamingInputStream(final Streaming streaming, final long startPosition, final long endPosition) {
        this.streaming = streaming;
        this.startPosition = startPosition;
        this.endPosition = endPosition;
        this.currentPosition = startPosition;
    }

    protected int checkEOF(int wishedReadLength) {
        if (this.endPosition < 0) { return wishedReadLength; }
        wishedReadLength = Math.min(wishedReadLength, (int) (this.endPosition - this.currentPosition));
        if (wishedReadLength <= 0) { return -1; }
        return wishedReadLength;
    }

    @Override
    public void close() {
        this.streaming.closeInputStream(this);
    }

    protected StreamingChunk getCurrentChunk() {
        return this.currentChunk;
    }

    public long getCurrentPosition() {
        return this.currentPosition;
    }

    public long getEndPosition() {
        return this.endPosition;
    }

    public long getStartPosition() {
        return this.startPosition;
    }

    @Override
    public void mark(final int readlimit) {
    }

    @Override
    public boolean markSupported() {
        return false;
    }

    @Override
    public int read() throws IOException {
        if (this.currentChunk == null) { throw new IOException("inputstream is closed"); }
        final int len = this.checkEOF(1);
        if (len == -1) { return -1; }
        final int ret = this.streaming.readChunkData(this, this.singleReadBuffer, 0, len);
        if (ret == -1) { return -1; }
        if (ret == 1) {
            this.currentPosition += 1;
            return this.singleReadBuffer[0];
        }
        throw new IOException("unknown IOException, ret=" + ret);
    }

    @Override
    public int read(final byte[] b, final int off, int len) throws IOException {
        if (this.currentChunk == null) {
            //
            throw new IOException(DOWNLOAD_STREAM_IS_CLOSED);
        }
        len = this.checkEOF(len);
        if (len == -1) { return -1; }
        final int ret = this.streaming.readChunkData(this, b, off, len);
        if (ret == -1) { return -1; }
        this.currentPosition += ret;
        return ret;
    }

    @Override
    public void reset() throws IOException {
        throw new IOException("mark/reset not supported");
    }

    protected void setCurrentChunk(final StreamingChunk currentChunk) {
        this.currentChunk = currentChunk;
    }

}
