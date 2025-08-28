package org.jdownloader.downloader.hls;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import jd.http.Browser;

import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.jdownloader.downloader.hls.M3U8Playlist.M3U8Segment.X_KEY_METHOD;

public class M3U8Playlist {
    public static class M3U8Segment {
        public static enum X_KEY_METHOD {
            // https://tools.ietf.org/html/draft-pantos-hls-rfc8216bis-02#section-4.4.2.4
            // Media Segments are not encrypted
            NONE("NONE"),
            // 128-bit key, Cipher Block Chaining, and PKCS7 padding
            AES_128("AES-128"),
            // SAMPLE-AES means that the Media Segments contain media samples, such as audio or video, that are encrypted
            // https://developer.apple.com/library/archive/documentation/AudioVideo/Conceptual/HLS_Sample_Encryption/Encryption/Encryption.html
            SAMPLE_AES("SAMPLE-AES");
            private final String method;

            public String getMethod() {
                return method;
            }

            private X_KEY_METHOD(final String method) {
                this.method = method;
            }

            public boolean isEncrypted() {
                return this != NONE;
            }

            public static X_KEY_METHOD get(final String method) {
                if (method == null || method.trim().length() == 0) {
                    return NONE;
                } else {
                    for (X_KEY_METHOD ret : values()) {
                        if (ret.getMethod().equals(method)) {
                            return ret;
                        }
                    }
                    return null;
                }
            }
        }

        private X_KEY_METHOD xKeyMethod = X_KEY_METHOD.NONE;
        private String       xKeyIV     = null;

        public String getxKeyIV() {
            return xKeyIV;
        }

        protected void setxKeyIV(String xKeyIV) {
            this.xKeyIV = xKeyIV;
        }

        public String getxKeyURI() {
            return xKeyURI;
        }

        protected void setxKeyURI(String xKeyURI) {
            this.xKeyURI = xKeyURI;
        }

        private String xKeyURI = null;

        public X_KEY_METHOD getxKeyMethod() {
            return xKeyMethod;
        }

        public boolean isEncrypted() {
            return getxKeyMethod().isEncrypted();
        }

        protected void setxKeyMethod(X_KEY_METHOD xKeyMethod) {
            if (xKeyMethod == null) {
                this.xKeyMethod = X_KEY_METHOD.NONE;
            } else {
                this.xKeyMethod = xKeyMethod;
            }
        }

        public static final String toExtInfDuration(final long duration) {
            final String value = Long.toString(duration);
            switch (value.length()) {
            case 0:
                return "0.000";
            case 1:
                return "0.00".concat(value);
            case 2:
                return "0.0".concat(value);
            case 3:
                return "0.".concat(value);
            default:
                return value.replaceFirst("(\\d{3})$", ".$1").replaceFirst("^\\.", "0.");
            }
        }

        public static long fromExtInfDuration(String extInf) {
            final String[] duration = new Regex(extInf, "#EXTINF:(\\d+)(\\.(\\d+))?").getRow(0);
            if (duration != null) {
                final String secs = duration[0];
                final String msns = duration[2];
                if (duration.length == 1 || msns == null) {
                    return Long.parseLong(secs) * 1000;
                } else {
                    long ret = Long.parseLong(secs) * 1000;
                    if (msns.length() == 1) {
                        ret += Long.parseLong(msns) * 100;
                    } else if (msns.length() == 2) {
                        ret += Long.parseLong(msns) * 10;
                    } else if (msns.length() == 3) {
                        ret += Long.parseLong(msns);
                    } else {
                        ret += Long.parseLong(msns.substring(0, 3));
                    }
                    return ret;
                }
            }
            return -1;
        }

        private final String  url;
        private volatile long loaded = -1;

        public long getLoaded() {
            return loaded;
        }

        public void setLoaded(long loaded) {
            if (loaded >= 0) {
                this.loaded = loaded;
            } else {
                this.loaded = -1;
            }
        }

        public long getSize() {
            if (isByteRange()) {
                return getByteRange()[0];
            } else {
                return size;
            }
        }

        public void setSize(long size) {
            if (size == -1) {
                this.size = -1;
            } else {
                this.size = Math.max(size, 0);
            }
        }

        public String getUrl() {
            return url;
        }

        public long getDuration() {
            return duration;
        }

        private volatile long duration;

        private void setDuration(long duration) {
            if (duration < 0) {
                this.duration = -1;
            } else {
                this.duration = duration;
            }
        }

        private volatile long size      = -1;
        private long[]        byteRange = null;

        public long[] getByteRange() {
            return byteRange;
        }

        public boolean isByteRange() {
            return getByteRange() != null;
        }

        private void setByteRange(long[] byteRange) {
            this.byteRange = byteRange;
        }

        public M3U8Segment(final String url, long duration) {
            this.url = url;
            setDuration(duration);
        }

        @Override
        public String toString() {
            if (isByteRange()) {
                return "M3U8Segment:Encrypted:" + isEncrypted() + "|Duration:" + getDuration() + "ms|ByteRange:" + Arrays.toString(getByteRange()) + "|URL:" + getUrl();
            } else {
                return "M3U8Segment:Encrypted:" + isEncrypted() + "|Duration:" + getDuration() + "ms|URL:" + getUrl();
            }
        }
    }

    @Override
    public String toString() {
        return "M3U8:Encrypted:" + getEncryptionMethod() + "|Segments:" + size() + "|Duration:" + getEstimatedDuration() + "ms|Estimated Size:" + getEstimatedSize();
    }

    private static final boolean X_BYTERANGE_SUPPORT = false;

    public static List<M3U8Playlist> loadM3U8(final String m3u8, final Browser br) throws IOException {
        return loadM3U8(m3u8, br, X_BYTERANGE_SUPPORT);
    }

    /*
     * https://tools.ietf.org/html/draft-pantos-http-live-streaming-20
     */
    public static List<M3U8Playlist> loadM3U8(final String m3u8, final Browser br, final boolean X_BYTERANGE_SUPPORT) throws IOException {
        br.getPage(m3u8);
        br.followRedirect();
        if (br.getHttpConnection().getResponseCode() != 200 && br.getHttpConnection().getResponseCode() != 206) {
            throw new IOException("ResponseCode must be 200 or 206!");
        }
        return parseM3U8(br, X_BYTERANGE_SUPPORT);
    }

    public static List<M3U8Playlist> parseM3U8(final Browser br) throws IOException {
        return parseM3U8(br, X_BYTERANGE_SUPPORT);
    }

    /**
     * https://en.wikipedia.org/wiki/M3U
     *
     * @param br
     * @param X_BYTERANGE_SUPPORT
     * @return
     * @throws IOException
     */
    public static List<M3U8Playlist> parseM3U8(final Browser br, final Boolean X_BYTERANGE_SUPPORT) throws IOException {
        final List<M3U8Playlist> ret = new ArrayList<M3U8Playlist>();
        M3U8Playlist current = new M3U8Playlist();
        long lastSegmentDuration = -1;
        long byteRange[] = null;
        int sequenceOffset = 0;
        String extXMapURL = null;
        M3U8Segment.X_KEY_METHOD xKeyMethod = M3U8Segment.X_KEY_METHOD.NONE;
        String xKeyIV = null;
        String xKeyURI = null;
        for (final String line : Regex.getLines(br.toString())) {
            if (StringUtils.isEmpty(line)) {
                continue;
            }
            if (line.startsWith("#EXT-X-DISCONTINUITY")) {
                if (current != null && current.size() > 0) {
                    ret.add(current);
                }
                M3U8Playlist before = current;
                current = new M3U8Playlist();
                current.setMediaSequenceOffset(sequenceOffset);
                current.setExtTargetDuration(before.getExtTargetDuration());
                lastSegmentDuration = -1;
            }
            if (StringUtils.startsWithCaseInsensitive(line, "concat") || StringUtils.contains(line, "file:")) {
                // http://habrahabr.ru/company/mailru/blog/274855/
            } else if (line.matches("^https?://.+") || !line.trim().startsWith("#") || line.startsWith("#EXT-X-MAP:URI")) {
                final String segmentURL;
                if (line.matches("^https?://.+") || !line.trim().startsWith("#")) {
                    segmentURL = br.getURL(line).toString();
                } else {
                    // TODO: add BYTERANGE support
                    final String URI = new Regex(line, "URI\\s*=\\s*\"(.*?)\"").getMatch(0);
                    if (URI == null) {
                        throw new IOException("Unsupported EXT-X-MAP:URI:" + line);
                    } else {
                        segmentURL = br.getURL(URI).toString();
                        extXMapURL = segmentURL;
                    }
                }
                final M3U8Segment existing = current.getSegment(segmentURL, null);
                if (existing == null || existing.isByteRange()) {
                    final M3U8Segment lastSegment = current.getLastSegment();
                    final int index = current.addSegment(segmentURL, lastSegmentDuration);
                    if (!M3U8Segment.X_KEY_METHOD.NONE.equals(xKeyMethod)) {
                        final M3U8Segment segment = current.getSegment(index);
                        segment.setxKeyMethod(xKeyMethod);
                        segment.setxKeyIV(xKeyIV);
                        segment.setxKeyURI(xKeyURI);
                    }
                    if (byteRange != null) {
                        final M3U8Segment segment = current.getSegment(index);
                        if (X_BYTERANGE_SUPPORT) {
                            if (lastSegment == null || !lastSegment.getUrl().endsWith(segmentURL)) {
                                byteRange[0] = byteRange[0] + byteRange[1];
                                byteRange[1] = 0;
                            }
                            segment.setByteRange(byteRange);
                        }
                        segment.setSize(byteRange[0]);
                    }
                } else if (existing != null && byteRange != null) {
                    if (lastSegmentDuration > 0) {
                        existing.setDuration(existing.getDuration() + lastSegmentDuration);
                    }
                    if (existing.getSize() > 0) {
                        existing.setSize(existing.getSize() + byteRange[0]);
                    }
                }
                lastSegmentDuration = -1;
                byteRange = null;
            } else {
                if (line.startsWith("#EXT-X-BYTERANGE")) {
                    // TODO: extract BYTERANGE parser into own method
                    final long byteRangeLength = Long.parseLong(new Regex(line, "#EXT-X-BYTERANGE:(\\d+)").getMatch(0));
                    final String byteRangeStart = new Regex(line, "#EXT-X-BYTERANGE:\\d+@(\\d+)").getMatch(0);
                    if (byteRangeStart != null) {
                        byteRange = new long[] { byteRangeLength, Long.parseLong(byteRangeStart) };
                    } else {
                        final M3U8Segment lastSegment = current.getLastSegment();
                        if (lastSegment != null && lastSegment.isByteRange()) {
                            byteRange = new long[] { byteRangeLength, lastSegment.getByteRange()[0] + lastSegment.getByteRange()[1] };
                        } else {
                            byteRange = new long[] { byteRangeLength, -1l };
                        }
                    }
                } else if (line.startsWith("#EXT-X-MEDIA-SEQUENCE")) {
                    sequenceOffset = Integer.parseInt(new Regex(line, "#EXT-X-MEDIA-SEQUENCE:(\\d+)").getMatch(0));
                    current.setMediaSequenceOffset(sequenceOffset);
                } else if (line.startsWith("#EXTINF:")) {
                    lastSegmentDuration = M3U8Segment.fromExtInfDuration(line);
                    sequenceOffset++;
                } else if (line.startsWith("#EXT-X-KEY")) {
                    xKeyMethod = M3U8Segment.X_KEY_METHOD.get(new Regex(line, "METHOD=(NONE|AES-128|SAMPLE-AES)").getMatch(0));
                    xKeyIV = new Regex(line, "IV=0x([a-fA-F0-9]{32})").getMatch(0);
                    xKeyURI = new Regex(line, "URI=\"(.*?)\"").getMatch(0);
                    if (xKeyURI != null) {
                        xKeyURI = br.getURL(xKeyURI).toString();
                    }
                } else if (line.startsWith("#EXT-X-TARGETDURATION")) {
                    final String targetDuration = new Regex(line, "#EXT-X-TARGETDURATION:(\\d+)").getMatch(0);
                    if (targetDuration != null) {
                        current.setExtTargetDuration(Long.parseLong(targetDuration));
                    }
                }
            }
        }
        if (current != null && current.size() > 0) {
            if (extXMapURL != null && false) {
                // continue with old handling as normal M3U8Segment
                final M3U8Segment extXMapSegment = current.getSegment(extXMapURL, null);
                if (extXMapSegment != null) {
                    current.extXMap = extXMapSegment;
                    current.removeSegment(current.indexOf(extXMapSegment));
                }
            }
            ret.add(current);
        }
        return ret;
    }

    public int addSegment(final String segmentURL, final long segmentDuration) {
        final M3U8Segment segment = new M3U8Segment(segmentURL, segmentDuration);
        return addSegment(segment);
    }

    protected final ArrayList<M3U8Segment>           segments            = new ArrayList<M3U8Segment>();
    private final HashMap<String, List<M3U8Segment>> map                 = new HashMap<String, List<M3U8Segment>>();
    protected int                                    mediaSequenceOffset = 0;
    protected long                                   averageBandwidth    = -1;
    protected M3U8Segment                            extXMap             = null;

    public M3U8Segment getExtXMap() {
        return extXMap;
    }

    public void setAverageBandwidth(long averageBandwidth) {
        this.averageBandwidth = averageBandwidth;
    }

    public long getAverageBandwidth() {
        return averageBandwidth;
    }

    public int getMediaSequenceOffset() {
        return mediaSequenceOffset;
    }

    protected void setMediaSequenceOffset(int mediaSequenceOffset) {
        this.mediaSequenceOffset = mediaSequenceOffset;
    }

    protected long targetDuration = -1;

    public long getTargetDuration() {
        long ret = -1;
        for (final M3U8Segment segment : segments) {
            ret = Math.max(segment.getDuration(), ret);
        }
        return Math.max(ret, getExtTargetDuration());
    }

    public long getExtTargetDuration() {
        return targetDuration;
    }

    public void setExtTargetDuration(long targetDuration) {
        if (targetDuration <= 0) {
            this.targetDuration = -1;
        } else {
            this.targetDuration = targetDuration;
        }
    }

    public int indexOf(M3U8Segment segment) {
        return segments.indexOf(segment);
    }

    public X_KEY_METHOD getEncryptionMethod() {
        for (final M3U8Segment segment : segments) {
            if (!M3U8Segment.X_KEY_METHOD.NONE.equals(segment.getxKeyMethod())) {
                return segment.getxKeyMethod();
            }
        }
        return M3U8Segment.X_KEY_METHOD.NONE;
    }

    public long getSegmentLoaded(final int index) {
        if (index >= 0 && index < segments.size()) {
            return segments.get(index).getLoaded();
        } else {
            return -1;
        }
    }

    public void setSegmentLoaded(final int index, long loaded) {
        if (index >= 0 && index < segments.size()) {
            segments.get(index).setLoaded(loaded);
        }
    }

    public void clearLoadedSegments() {
        for (final M3U8Segment segment : segments) {
            segment.setLoaded(-1);
        }
    }

    public M3U8Segment getSegment(final int index) {
        if (index >= 0 && index < segments.size()) {
            return segments.get(index);
        } else {
            return null;
        }
    }

    public boolean containsSegmentURL(final String segmentURL) {
        return map.containsKey(segmentURL);
    }

    public int size() {
        return segments.size();
    }

    private void addSegmentToMap(M3U8Segment segment) {
        if (segment != null) {
            final String url = segment.getUrl();
            List<M3U8Segment> list = map.get(url);
            if (list == null) {
                list = new ArrayList<M3U8Segment>();
                map.put(url, list);
            }
            if (!list.contains(segment)) {
                list.add(segment);
            }
        }
    }

    private void removeSegmentFromMap(M3U8Segment segment) {
        if (segment != null) {
            final String url = segment.getUrl();
            final List<M3U8Segment> list = map.get(url);
            if (list != null && list.remove(segment) && list.size() == 0) {
                map.remove(url);
            }
        }
    }

    public int addSegment(M3U8Segment segment) {
        if (segment != null) {
            int index = segments.indexOf(segment);
            if (index == -1) {
                index = segments.size();
                segments.add(segment);
                addSegmentToMap(segment);
            }
            return index;
        }
        return -1;
    }

    public M3U8Segment setSegment(int index, M3U8Segment segment) {
        if (segment != null && index >= 0 && index < segments.size()) {
            final M3U8Segment ret = segments.set(index, segment);
            removeSegmentFromMap(ret);
            addSegment(segment);
            return ret;
        } else {
            return null;
        }
    }

    /** Returns estimated duration in milliseconds. */
    public long getEstimatedDuration() {
        return getEstimatedDuration(Arrays.asList(new M3U8Playlist[] { this }));
    }

    /** Returns estimated duration in milliseconds. */
    public static long getEstimatedDuration(List<M3U8Playlist> list) {
        long duration = 0;
        for (final M3U8Playlist playList : list) {
            for (final M3U8Segment segment : playList.segments) {
                duration += Math.max(0, segment.getDuration());
            }
        }
        return duration;
    }

    public static long getEstimatedSize(List<M3U8Playlist> list) {
        long size = -1;
        long duration = 0;
        long unknownSizeDuration = 0;
        if (list != null) {
            for (M3U8Playlist playList : list) {
                final long averageBandwidth = playList.getAverageBandwidth();
                for (final M3U8Segment segment : playList.segments) {
                    if (segment.getSize() != -1) {
                        size += segment.getSize();
                        duration += segment.getDuration();
                    } else if (averageBandwidth > 0 && segment.getDuration() > 0) {
                        size += averageBandwidth / 8 * (segment.getDuration() / 1000);
                        duration += segment.getDuration();
                    } else {
                        unknownSizeDuration += segment.getDuration();
                    }
                }
            }
        }
        if (unknownSizeDuration == 0 && duration > 0) {
            return size + 1;
        } else if (size > 0 && duration > 0) {
            final double averagePerSecond = (Math.max(1, size)) / (duration / 1000d);
            return size + (long) (averagePerSecond * (unknownSizeDuration / 1000d));
        } else {
            return -1;
        }
    }

    public long getEstimatedSize() {
        return getEstimatedSize(Arrays.asList(new M3U8Playlist[] { this }));
    }

    protected M3U8Segment getLastSegment() {
        if (segments.size() > 0) {
            return segments.get(segments.size() - 1);
        } else {
            return null;
        }
    }

    protected M3U8Segment getSegment(final String url, long[] byteRange) {
        final List<M3U8Segment> list = map.get(url);
        if (list != null && list.size() > 0) {
            return list.get(0);
        } else {
            return null;
        }
    }

    public M3U8Segment removeSegment(int index) {
        if (index >= 0 && index < segments.size()) {
            final M3U8Segment ret = segments.remove(index);
            removeSegmentFromMap(ret);
            return ret;
        } else {
            return null;
        }
    }

    public boolean addSegment(int index, M3U8Segment segment) {
        if (segment != null && index >= 0) {
            segments.add(index, segment);
            addSegmentToMap(segment);
            return true;
        } else {
            return false;
        }
    }
}
