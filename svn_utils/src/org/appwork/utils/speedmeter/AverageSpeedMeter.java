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
package org.appwork.utils.speedmeter;

import org.appwork.utils.Time;

/**
 * @author daniel
 *
 */
public class AverageSpeedMeter implements SpeedMeterInterface {
    private final long[]     bytes;
    private final long[]     times;
    private final int        size;
    private int              index;
    private final Object     LOCK        = new Object();
    private long             stalled     = 0;
    private long             timeout     = -1;          /*
                                                          * no timeout for stalled connections
                                                          */
    private final Resolution resolution;
    private long             totalValue;
    private long             totalTime;
    private long             lastPutTime = -1;

    public final Resolution getResolution() {
        return resolution;
    }

    protected final long getMinimumDuration() {
        return getResolution().factor;
    }

    /**
     * constructor for AverageSpeedMeter with default size 5
     *
     * @param milliSeconds
     * @param size2
     */
    public AverageSpeedMeter(int size, Resolution resolution) {
        this.size = size;
        this.bytes = new long[this.size];
        this.times = new long[this.size];
        this.index = 0;
        if (resolution == null) {
            throw new IllegalArgumentException("Resolution is null!");
        }
        this.resolution = resolution;
        this.resetSpeedmeter();
    }

    /**
     * constructor for AverageSpeedMeter with custom size
     *
     * @param size
     */
    public AverageSpeedMeter(final int size) {
        this(size, Resolution.MILLI_SECONDS);
    }

    /**
     *
     */
    public AverageSpeedMeter() {
        this(5);
    }

    /**
     * returns the current value of the speedmeter. Use the scalingfactor to define the require unit. <br>
     * example:<br>
     *
     *
     */
    public long getValue(Resolution requestedResolution) {
        return getValue(requestedResolution, false);
    }

    public long getValue(Resolution requestedResolution, boolean untilNow) {
        final long time;
        final long value;
        synchronized (this.LOCK) {
            if (untilNow) {
                time = totalTime;
            } else {
                time = totalTime + Time.systemIndependentCurrentJVMTimeMillis() - (lastPutTime < 0 ? 0 : lastPutTime);
            }
            value = totalValue;
        }
        if (time > getMinimumDuration()) {
            return (long) ((value * (getResolution().factor / (double) requestedResolution.factor)) / time);
        } else if (time > 0) {
            double factor = getMinimumDuration() / time * 1.0d;
            return (long) (factor * ((value * (getResolution().factor / (double) requestedResolution.factor)) / time));
        } else {
            return 0;
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.speedmeter.SpeedMeterInterface#getSpeedMeter()
     */
    public void putBytes(long x, long time) {
        final long now = Time.systemIndependentCurrentJVMTimeMillis();
        x = Math.max(0, x);
        time = Math.max(0, time);
        synchronized (this.LOCK) {
            lastPutTime = now;
            if (x == 0) {
                this.stalled += time;
                if (this.timeout > 0 && this.stalled > this.timeout) {
                    this.resetSpeedmeter();
                }
            } else {
                int index = this.index;
                final long[] bytes = this.bytes;
                final long[] times = this.times;
                final long lastBytes = bytes[index];
                bytes[index] = x;
                final long lastTime = times[index];
                final long thisTime = time + this.stalled;
                times[index] = thisTime;
                totalValue = totalValue - lastBytes + x;
                totalTime = totalTime - lastTime + thisTime;
                this.stalled = 0;
                this.index = (index + 1) % this.size;
            }
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.speedmeter.SpeedMeterInterface#putSpeedMeter(long, long)
     */
    public void resetSpeedmeter() {
        synchronized (this.LOCK) {
            final int size = this.size;
            final long[] bytes = this.bytes;
            final long[] times = this.times;
            for (int index = 0; index < size; index++) {
                bytes[index] = -1;
                times[index] = 0;
            }
            this.lastPutTime = -1;
            this.index = 0;
            this.totalTime = 0;
            this.totalValue = 0;
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.speedmeter.SpeedMeterInterface#resetSpeedMeter()
     */
    public void setStallTimeout(final long timeout) {
        if (timeout <= 0) {
            this.timeout = -1;
        } else {
            this.timeout = timeout;
        }
    }

    /**
     * @param i
     */
    public void putBytes(long x) {
        synchronized (this.LOCK) {
            final long now = Time.systemIndependentCurrentJVMTimeMillis();
            if (lastPutTime == -1) {
                lastPutTime = now;
            } else {
                putBytes(x, now - lastPutTime);
            }
        }
    }
}
