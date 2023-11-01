package org.appwork.io.unixsplit;

import java.io.IOException;
import java.io.OutputStream;

public interface StreamProvider {

    public static class PartInfo {
        public final long         size;
        public final OutputStream stream;

        public PartInfo(final OutputStream stream, final long maxSize) throws IOException {
            this.size = maxSize <= 0 ? Long.MAX_VALUE : maxSize;
            this.stream = stream;
        }

        /**
         * @param transferedBytes
         */
        public long getRemaining(long transferedBytes) {
            return size - transferedBytes;
        }

        /**
         * @throws IOException
         *
         */
        public void close() throws IOException {
            stream.close();
        }

    }

    PartInfo getNextStream(int i) throws IOException;

}
