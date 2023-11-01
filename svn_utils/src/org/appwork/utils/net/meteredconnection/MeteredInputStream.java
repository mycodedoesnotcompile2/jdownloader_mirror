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
package org.appwork.utils.net.meteredconnection;

import java.io.IOException;
import java.io.InputStream;

import org.appwork.utils.speedmeter.SpeedMeterInterface;

/**
 * @author daniel
 *
 */
public class MeteredInputStream extends InputStream implements SpeedMeterInterface {
    private final InputStream         in;
    private final SpeedMeterInterface speedmeter;
    private volatile long             transfered = 0;

    public long getTransfered() {
        return transfered;
    }

    public final static int LOWStep = 1024;

    /**
     * constructor for MeterdInputStream
     *
     * @param in
     */
    @Deprecated
    public MeteredInputStream(InputStream in) {
        this(in, null);
    }

    /**
     * constructor for MeteredInputStream with custom SpeedMeter
     *
     * @param in
     * @param speedmeter
     */
    public MeteredInputStream(InputStream in, SpeedMeterInterface speedmeter) {
        this.in = in;
        this.speedmeter = speedmeter;
    }

    @Override
    public int read() throws IOException {
        final int ret = in.read();
        if (ret != -1) {
            transfered++;
        }
        return ret;
    }

    private int checkStep = 1024;

    public int getCheckStepSize() {
        return checkStep;
    }

    public void setCheckStepSize(int step) {
        checkStep = Math.max(LOWStep, step);
    }

    @Override
    public int read(byte b[], int off, int len) throws IOException {
        int offset = off;
        int rest = len;
        int readNum = 0;
        final Resolution resolution = getResolution();
        /* we want 5 update per second */
        final long maxReadTime = resolution.factor / 5;
        int maxRead = checkStep;
        while (rest != 0) {
            int toRead = rest;
            final boolean limitedStep;
            if (toRead > maxRead) {
                toRead = maxRead;
                limitedStep = true;
            } else {
                limitedStep = false;
            }
            final long timeForCheckStep = getTime();
            final int read = in.read(b, offset, toRead);
            final long timeCheck = getTime() - timeForCheckStep;
            if (read == -1) {
                if (readNum == 0) {
                    return -1;
                } else {
                    break;
                }
            }
            if (timeCheck == 0) {
                if (limitedStep && read == toRead) {
                    /* we increase in little steps */
                    final long nextRead = maxRead + LOWStep;// avoid Integer overflow
                    maxRead = Math.max(LOWStep, (int) Math.min(Integer.MAX_VALUE, nextRead));
                    checkStep = maxRead;
                }
            } else if (timeCheck > maxReadTime) {
                final long nextCheckStep = (read / timeCheck) * maxReadTime;
                maxRead = Math.max(LOWStep, (int) nextCheckStep);
                checkStep = maxRead;
            }
            readNum += read;
            transfered += read;
            rest -= read;
            offset += read;
        }
        return readNum;
    }

    @Override
    public int available() throws IOException {
        return in.available();
    }

    @Override
    public synchronized void mark(int readlimit) {
        in.mark(readlimit);
    }

    @Override
    public synchronized void reset() throws IOException {
        in.reset();
    }

    @Override
    public boolean markSupported() {
        return in.markSupported();
    }

    @Override
    public long skip(long n) throws IOException {
        return in.skip(n);
    }

    @Override
    public void close() throws IOException {
        in.close();
    }

    private long transfered2 = 0;
    private long time        = 0;
    private long speed       = 0;

    public synchronized long getValue(Resolution resolution) {
        final long now = getTime();
        final long trans = transfered;
        if (time == 0) {
            time = now;
            transfered2 = trans;
            return 0;
        }
        final long last = now - time;
        if (last < resolution.factor) {
            if (speedmeter != null) {
                return speedmeter.getValue(resolution);
            } else {
                return speed;
            }
        }
        time = now;
        final long diff = trans - transfered2;
        transfered2 = trans;
        if (speedmeter != null) {
            speedmeter.putBytes(diff, last);
            speed = speedmeter.getValue(resolution);
            return speed;
        } else {
            // TODO: remove deprecated constructor/mode or add support for different internal/external resolution
            final Resolution internal = getResolution();
            speed = (diff / last) * internal.factor;
            return speed;
        }
    }

    protected long getTime() {
        return getResolution().getTime();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.speedmeter.SpeedMeterInterface#getResolution()
     */
    @Override
    public Resolution getResolution() {
        if (speedmeter != null) {
            return speedmeter.getResolution();
        } else {
            return Resolution.MILLI_SECONDS;
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.SpeedMeterInterface#putSpeedMeter(long, long)
     */
    public void putBytes(long bytes, long time) {
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.SpeedMeterInterface#resetSpeedMeter()
     */
    public synchronized void resetSpeedmeter() {
        if (speedmeter != null) {
            speedmeter.resetSpeedmeter();
        }
        speed = 0;
        transfered2 = transfered;
        time = getTime();
    }
}
