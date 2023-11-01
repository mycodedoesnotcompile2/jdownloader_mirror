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

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;

import org.appwork.exceptions.WTFException;
import org.appwork.utils.Regex;

/**
 * @author daniel
 * 
 */
public abstract class Streaming {

    protected java.util.List<StreamingChunk>                       availableChunks        = new ArrayList<StreamingChunk>();

    protected java.util.List<WeakReference<StreamingInputStream>>  connectedInputStreams  = new ArrayList<WeakReference<StreamingInputStream>>();
    protected java.util.List<WeakReference<StreamingOutputStream>> connectedOutputStreams = new ArrayList<WeakReference<StreamingOutputStream>>();

    protected final String                                         outputFile;
    private boolean                                                isClosed               = false;

    private final Comparator<StreamingChunk>                       comparator             = new Comparator<StreamingChunk>() {

                                                                                              @Override
                                                                                              public int compare(final StreamingChunk o1, final StreamingChunk o2) {
                                                                                                  final long x = o1.getChunkStartPosition();
                                                                                                  final long y = o2.getChunkStartPosition();
                                                                                                  return x < y ? 1 : x == y ? 0 : -1;
                                                                                              }
                                                                                          };

    public Streaming(final String outputFile) throws IOException {
        this.outputFile = outputFile;
        final String outputFilename = new File(outputFile).getName();
        final File[] foundChunks = new File(outputFile).getParentFile().listFiles(new FileFilter() {

            @Override
            public boolean accept(final File pathname) {
                return pathname.isFile() && pathname.getName().startsWith(outputFilename) && new Regex(pathname.getName(), "\\.chk\\d+$").matches();
            }
        });
        if (foundChunks != null) {
            for (final File chunk : foundChunks) {
                final String startPosition = new Regex(chunk.getName(), "\\.chk(\\d+)$").getMatch(0);
                this.availableChunks.add(new StreamingChunk(chunk, Long.parseLong(startPosition)));
            }
            Collections.sort(this.availableChunks, this.comparator);
        }
    }

    /**
     * close this StreamingController. also closes all input/outputstreams and
     * all StreamingChunks
     * 
     * NOTE: you have to create new Instance of Streaming if you want to open
     * new
     */
    public synchronized void close() {
        this.isClosed = true;
        try {
            final Iterator<WeakReference<StreamingInputStream>> it = this.connectedInputStreams.iterator();
            while (it.hasNext()) {
                final WeakReference<StreamingInputStream> next = it.next();
                final StreamingInputStream current = next.get();
                it.remove();
                if (current != null) {
                    current.setCurrentChunk(null);
                }
            }
        } catch (final Throwable e) {
        }
        try {
            final Iterator<WeakReference<StreamingOutputStream>> it = this.connectedOutputStreams.iterator();
            while (it.hasNext()) {
                final WeakReference<StreamingOutputStream> next = it.next();
                final StreamingOutputStream current = next.get();
                it.remove();
                if (current != null) {
                    current.setCurrentChunk(null);
                }
            }
        } catch (final Throwable e) {
        }
        try {
            for (final StreamingChunk chunk : this.availableChunks) {
                try {
                    chunk.close();
                } catch (final Throwable e) {
                }
            }
            this.availableChunks.clear();
        } catch (final Throwable e) {
        }
    }

    /**
     * removes given StreamingInputStream from this StreamingController
     * 
     * @param streamingInputStream
     */
    protected synchronized void closeInputStream(final StreamingInputStream streamingInputStream) {
        try {
            final Iterator<WeakReference<StreamingInputStream>> it = this.connectedInputStreams.iterator();
            while (it.hasNext()) {
                StreamingInputStream current = null;
                final WeakReference<StreamingInputStream> next = it.next();
                if ((current = next.get()) == null || current == streamingInputStream) {
                    it.remove();
                }
            }
        } finally {
            streamingInputStream.setCurrentChunk(null);
        }
    }

    /**
     * removes given StreamingOutputStream from this StreamingController. also
     * set canGrow to false on its StreamingChunk
     * 
     * @param streamingOutputStream
     */
    protected synchronized void closeOutputStream(final StreamingOutputStream streamingOutputStream) {
        try {
            final Iterator<WeakReference<StreamingOutputStream>> it = this.connectedOutputStreams.iterator();
            while (it.hasNext()) {
                StreamingOutputStream current = null;
                final WeakReference<StreamingOutputStream> next = it.next();
                if ((current = next.get()) == null || current == streamingOutputStream) {
                    it.remove();
                }
            }
        } finally {
            final StreamingChunk currentChunk = streamingOutputStream.getCurrentChunk();
            if (currentChunk != null) {
                currentChunk.setCanGrow(false);
            }
            streamingOutputStream.setCurrentChunk(null);
        }
    }

    protected boolean connectStreamingOutputStream(final StreamingChunk streamingChunk, final long startPosition, final long endPosition) throws IOException {
        final StreamingOutputStream streamingOutputStream = this.streamingOutputStreamFactory();
        streamingOutputStream.setCurrentChunk(streamingChunk);
        if (this.connectStreamingOutputStream(streamingOutputStream, startPosition, endPosition) == false) { return false; }
        this.connectedOutputStreams.add(new WeakReference<StreamingOutputStream>(streamingOutputStream));
        streamingChunk.setCanGrow(true);
        return true;
    }

    public abstract boolean connectStreamingOutputStream(StreamingOutputStream streamingOutputStream, final long startPosition, final long endPosition) throws IOException;

    protected synchronized void detectOverlappingChunks(final StreamingChunk currentChunk) throws IOException {
        final long overlapCheck = currentChunk.getChunkStartPosition() + currentChunk.getAvailableChunkSize();
        final int chunkIndex = this.availableChunks.indexOf(currentChunk);
        if (chunkIndex >= 1) {
            final StreamingChunk overlapChunk = this.availableChunks.get(chunkIndex - 1);
            if (overlapCheck > overlapChunk.getChunkStartPosition()) { throw new StreamingOverlapWrite(); }
        }
    }

    protected synchronized java.util.List<StreamingInputStream> findAllStreamingInputStreamsFor(final StreamingOutputStream streamingOutputStream) {
        final StreamingChunk chunk = streamingOutputStream.getCurrentChunk();
        final java.util.List<StreamingInputStream> ret = new ArrayList<StreamingInputStream>();
        if (chunk != null) {
            final Iterator<WeakReference<StreamingInputStream>> it = this.connectedInputStreams.iterator();
            while (it.hasNext()) {
                final WeakReference<StreamingInputStream> next = it.next();
                final StreamingInputStream current = next.get();
                if (current != null && current.getCurrentChunk() == chunk) {
                    ret.add(current);
                }
            }
        }
        return null;
    }

    protected synchronized StreamingOutputStream findLastStreamingOutputStreamFor(final StreamingInputStream streamingInputStream) {
        final StreamingChunk chunk = streamingInputStream.getCurrentChunk();
        if (chunk != null) {
            final Iterator<WeakReference<StreamingOutputStream>> it = this.connectedOutputStreams.iterator();
            while (it.hasNext()) {
                final WeakReference<StreamingOutputStream> next = it.next();
                final StreamingOutputStream current = next.get();
                if (current != null && current.getCurrentChunk() == chunk) { return current; }
            }
        }
        return null;
    }

    public abstract long getFinalFileSize();

    public synchronized StreamingInputStream getInputStream(final long startPosition, final long endPosition) throws IOException {
        if (this.isClosed) { throw new IOException("streaming file is closed!"); }
        if (startPosition < 0) { throw new IllegalArgumentException("startPosition <0"); }
        if (endPosition >= 0 && endPosition <= startPosition) { throw new IllegalArgumentException("endposition <= startPosition"); }
        if (this.getFinalFileSize() > 0 && startPosition >= this.getFinalFileSize()) { throw new IllegalArgumentException("startPosition >= filesize"); }
        /* create new StreamingInputStream instance */
        final StreamingInputStream streamingInputStream = this.streamingInputStreamFactory(startPosition, endPosition);
        final StreamingChunk streamingChunk = this.getNextStreamingChunk(startPosition, endPosition);
        if (streamingChunk == null) { throw new IOException("no inputStream for requested range available"); }
        streamingInputStream.setCurrentChunk(streamingChunk);
        this.connectedInputStreams.add(new WeakReference<StreamingInputStream>(streamingInputStream));
        return streamingInputStream;
    }

    protected synchronized StreamingChunk getNextStreamingChunk(final long startPosition, final long endPosition) throws IOException {
        if (this.isClosed()) { return null; }
        if (this.getFinalFileSize() > 0 && startPosition >= this.getFinalFileSize()) { return null; }
        /* find available chunk */
        StreamingChunk streamingChunk = null;
        boolean need2ConnectStreamingOutputStream = false;
        for (final StreamingChunk chunk : this.availableChunks) {
            if (chunk.getChunkStartPosition() <= startPosition) {
                /* startPosition is okay, now lets check chunkSize */
                if (chunk.getChunkStartPosition() + chunk.getAvailableChunkSize() > startPosition) {
                    /* chunk has enough data for requested startPosition */
                    streamingChunk = chunk;
                    break;
                } else if (chunk.getChunkStartPosition() + chunk.getAvailableChunkSize() == startPosition) {
                    /* chunk can still grow and startPosition is okay */
                    streamingChunk = chunk;
                    if (chunk.canGrow() == false) {
                        need2ConnectStreamingOutputStream = true;
                    }
                    break;
                }
            }
        }
        if (streamingChunk == null) {
            /* no chunk available, create new one */
            final File chunkFile = new File(this.outputFile + ".chk" + startPosition);
            try {
                streamingChunk = new StreamingChunk(chunkFile, startPosition);
                if (this.connectStreamingOutputStream(streamingChunk, startPosition, endPosition) == false) { return null; }
            } catch (final IOException e) {
                try {
                    streamingChunk.close();
                } catch (final Throwable dontcare) {
                } finally {
                    chunkFile.delete();
                }
                throw e;
            }
            this.availableChunks.add(streamingChunk);
            Collections.sort(this.availableChunks, this.comparator);
        }
        if (need2ConnectStreamingOutputStream) {
            if (this.connectStreamingOutputStream(streamingChunk, startPosition, endPosition) == false) { return null; }
        }
        return streamingChunk;
    }

    public String getOutputFile() {
        return this.outputFile;
    }

    public boolean isClosed() {
        return this.isClosed;
    }

    protected int readChunkData(final StreamingInputStream streamingInputStream, final byte[] b, final int off, final int len) throws IOException {
        if (this.isClosed()) { return -1; }
        StreamingChunk currentChunk = streamingInputStream.getCurrentChunk();
        try {
            while (true) {
                final int ret = currentChunk.read(b, off, len, streamingInputStream.getCurrentPosition());
                if (ret > 0) { return ret; }
                if (ret == -1) {
                    /*
                     * this chunk is finished, try to find next one and read
                     * from it
                     */
                    currentChunk = this.getNextStreamingChunk(streamingInputStream.getCurrentPosition(), streamingInputStream.getEndPosition());
                    if (currentChunk == null) { return -1; }
                    streamingInputStream.setCurrentChunk(currentChunk);
                    continue;
                }
                throw new WTFException("How could this happen?!");
            }
        } catch (final InterruptedException e) {
            throw new IOException(e);
        }
    }

    protected StreamingInputStream streamingInputStreamFactory(final long startPosition, final long endPosition) {
        return new StreamingInputStream(this, startPosition, endPosition);
    }

    protected StreamingOutputStream streamingOutputStreamFactory() {
        return new StreamingOutputStream(this);
    }

    protected void writeChunkData(final StreamingOutputStream streamingOutputStream, final byte[] b, final int off, final int len) throws IOException {
        final StreamingChunk currentChunk = streamingOutputStream.getCurrentChunk();
        currentChunk.write(b, off, len);
        if (this.isClosed()) { throw new IOException("closed"); }
        /* check for overlap */
        this.detectOverlappingChunks(currentChunk);
    }

}
