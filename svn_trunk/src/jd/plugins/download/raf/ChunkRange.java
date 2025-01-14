package jd.plugins.download.raf;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;

public class ChunkRange {
    private final long from;

    public long getFrom() {
        return from;
    }

    public Long getTo() {
        return to;
    }

    public long getLength() {
        final Long to = getTo();
        if (to == null || to < 0) {
            return -1;
        } else {
            final long from = getFrom();
            return to - from + 1;
        }
    }

    private final Long          to;
    private final AtomicLong    loaded      = new AtomicLong(0);
    private final AtomicBoolean validLoaded = new AtomicBoolean(false);
    private final boolean       rangeRequested;

    public boolean isRangeRequested() {
        return rangeRequested;
    }

    public void reset() {
        validLoaded.set(false);
        loaded.set(0);
    }

    public boolean isValidLoaded() {
        return validLoaded.get();
    }

    public void setValidLoaded(boolean validLoaded) {
        this.validLoaded.set(validLoaded);
    }

    public ChunkRange() {
        rangeRequested = false;
        from = 0;
        to = null;
    }

    public ChunkRange(final long from) {
        this(from, null);
    }

    public String getRangeHeaderContent(final boolean openEnd) {
        final long from = getFrom();
        final Long to = getTo();
        if (from < 0) {
            return null;
        } else {
            if (to == null || to < 0 || openEnd) {
                return "bytes=" + from + "-";
            } else {
                return "bytes=" + from + "-" + to;
            }
        }
    }

    /* create a ChunkRange from index 'from' to index 'to' (included) */
    public ChunkRange(final long from, final Long to) {
        if (from < 0) {
            throw new IllegalArgumentException("from(" + from + ") < 0");
        } else if (to != null && to >= 0 && from > to) {
            throw new IllegalArgumentException("from(" + from + ") > to(" + to + ")");
        }
        rangeRequested = true;
        this.from = from;
        this.to = to;
    }

    public long getLoaded() {
        return loaded.get();
    }

    public long getPosition() {
        return from + getLoaded();
    }

    public void incLoaded(long incr) {
        if (incr > 0) {
            this.loaded.addAndGet(incr);
        }
    }

    @Override
    public String toString() {
        return "ChunkRange: " + getFrom() + "-" + getTo() + "/" + getLength() + "|" + getLoaded() + "=" + isValidLoaded();
    }
}
