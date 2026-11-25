package org.jdownloader.downloader.segment;

import java.io.IOException;

import jd.http.Browser;
import jd.http.Request;
import jd.http.URLConnectionAdapter;
import jd.http.requests.GetRequest;
import jd.plugins.download.raf.ChunkRange;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils;

public class Segment {
    /* Value of the expected length if it is not known */
    public static final long UNKNOWN_LENGTH = -1;
    protected final String   url;
    private final long       expectedLength;

    public boolean isLoaded() {
        return chunkRange.isValidLoaded();
    }

    private final ChunkRange chunkRange;

    public String getUrl() {
        return url;
    }

    public Segment(String url) {
        this(url, null, null, UNKNOWN_LENGTH);
    }

    /**
     * @param expectedLength
     *            Number of bytes the complete segment has, {@link #UNKNOWN_LENGTH} (or any negative value) if it is not known
     */
    public Segment(String url, long expectedLength) {
        this(url, null, null, expectedLength);
    }

    public Segment(String url, Long range_from, Long range_to) {
        this(url, range_from, range_to, UNKNOWN_LENGTH);
    }

    /**
     * @param expectedLength
     *            Number of bytes the complete segment has (of the range if there is one), {@link #UNKNOWN_LENGTH} (or any negative value)
     *            if it is not known. If it is known, a response with a different Content-Length is treated as invalid.
     * @throws IllegalArgumentException
     *             if the url is missing, range_to is set without range_from, the range is invalid (see ChunkRange) or the length of the range
     *             is not the expected length
     */
    public Segment(String url, Long range_from, Long range_to, long expectedLength) {
        if (url == null) {
            throw new IllegalArgumentException("url is null");
        } else if (range_from == null && range_to != null) {
            throw new IllegalArgumentException("range_to(" + range_to + ") without range_from");
        }
        /* ChunkRange throws an IllegalArgumentException if from < 0 or from > to */
        final ChunkRange chunkRange = range_from != null ? new ChunkRange(range_from.longValue(), range_to) : new ChunkRange();
        final long rangeLength = chunkRange.getLength();
        if (expectedLength >= 0 && rangeLength >= 0 && rangeLength != expectedLength) {
            if (rangeLength > expectedLength) {
                throw new IllegalArgumentException("range_to(" + range_to + ") is behind the expected length(" + expectedLength + "): range length=" + rangeLength);
            } else {
                throw new IllegalArgumentException("range length(" + rangeLength + ") is shorter than the expected length(" + expectedLength + ")");
            }
        }
        this.url = url;
        this.expectedLength = expectedLength < 0 ? UNKNOWN_LENGTH : expectedLength;
        this.chunkRange = chunkRange;
    }

    /** Number of bytes the complete segment has or {@link #UNKNOWN_LENGTH} */
    public long getExpectedLength() {
        return expectedLength;
    }

    public ChunkRange getChunkRange() {
        return chunkRange;
    }

    protected Request createRequest() throws IOException {
        final GetRequest ret = new GetRequest(getUrl());
        ret.getHeaders().put(new HTTPHeader(HTTPConstants.HEADER_REQUEST_ACCEPT_ENCODING, "identity", false));
        final ChunkRange chunkRange = getChunkRange();
        if (chunkRange != null && chunkRange.isRangeRequested()) {
            final String requestedRange = chunkRange.getRangeHeaderContent(false);
            ret.getHeaders().put(new HTTPHeader(HTTPConstants.HEADER_REQUEST_RANGE, requestedRange, false));
        }
        return ret;
    }

    protected URLConnectionAdapter open(Browser br, Request request) throws IOException {
        br.setRequest(null);
        final URLConnectionAdapter ret = br.openRequestConnection(request);
        return ret;
    }

    protected boolean isConnectionValid(URLConnectionAdapter con) throws IOException {
        return isConnectionValid(con, 0);
    }

    /**
     * Checks the response of the server: Status 200 or 206 and, if a range was requested, that the server answered with exactly the
     * requested part.
     *
     * @param loadedBytes
     *            Number of bytes of the segment which are already loaded: The request of a continued segment starts behind them.
     */
    protected boolean isConnectionValid(URLConnectionAdapter con, final long loadedBytes) throws IOException {
        final int responseCode = con.getResponseCode();
        if (responseCode != 200 && responseCode != 206) {
            return false;
        }
        final ChunkRange chunkRange = getChunkRange();
        if (chunkRange == null || (!chunkRange.isRangeRequested() && loadedBytes <= 0)) {
            /* no range was requested: the complete segment is expected */
            return true;
        }
        final long start = chunkRange.getFrom() + loadedBytes;
        final Long to = chunkRange.getTo();
        final boolean hasEnd = to != null && to >= 0;
        if (responseCode == 200) {
            /* The server ignored the range and sends the complete resource: Only valid if the range was the complete resource anyway. */
            if (start != 0) {
                return false;
            }
            final long contentLength = con.getLongContentLength();
            return !hasEnd || contentLength == -1 || contentLength == to.longValue() + 1;
        }
        final long[] contentRange = HTTPConnectionUtils.parseContentRange(con);
        if (contentRange == null || contentRange[0] != start) {
            return false;
        }
        /* The server may shorten the end of the range (end of the file), but it must not send more than requested. */
        return !hasEnd || contentRange[1] == -1 || contentRange[1] <= to.longValue();
    }

    protected Boolean retrySegmentConnection(Browser br, Segment segment, int segmentRetryCounter) {
        return null;
    }

    @Override
    public String toString() {
        final ChunkRange chunkRange = getChunkRange();
        return "URL:" + getUrl() + "|" + chunkRange;
    }
}
