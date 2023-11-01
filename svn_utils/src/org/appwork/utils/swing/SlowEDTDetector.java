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
package org.appwork.utils.swing;

import java.util.Iterator;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;

import org.appwork.scheduler.DelayedRunnable;
import org.appwork.utils.Time;
import org.appwork.utils.logging2.LogInterface;

/**
 * @author daniel
 * 
 */
public class SlowEDTDetector {
    private final AtomicLong      lastInvoke        = new AtomicLong(-1);
    private final AtomicLong      lastEDT           = new AtomicLong(-1);
    private final AtomicBoolean   detectEDTBlocking = new AtomicBoolean(false);
    private final AtomicBoolean   doLog             = new AtomicBoolean(true);
    private final DelayedRunnable detector;
    private final long            maxEDTBlockingTime;

    public SlowEDTDetector(final long maxEDTBlockingTime, final LogInterface logger) {
        this.maxEDTBlockingTime = maxEDTBlockingTime;
        this.detector = new DelayedRunnable(maxEDTBlockingTime, maxEDTBlockingTime) {

            @Override
            public void delayedrun() {
                try {
                    long blocking = SlowEDTDetector.this.lastEDT.get();
                    if (blocking < 0) {
                        blocking = now() - SlowEDTDetector.this.lastInvoke.get();
                    }
                    if (blocking >= SlowEDTDetector.this.maxEDTBlockingTime) {
                        if (SlowEDTDetector.this.doLog.get()) {
                            try {
                                final Iterator<Entry<Thread, StackTraceElement[]>> it = Thread.getAllStackTraces().entrySet().iterator();
                                while (it.hasNext()) {
                                    final Entry<Thread, StackTraceElement[]> next = it.next();
                                    final StringBuilder sb = new StringBuilder();
                                    sb.append("BlockingEDT Detected(" + blocking + "ms)->Thread: " + next.getKey().getName() + "\r\n");
                                    for (final StackTraceElement stackTraceElement : next.getValue()) {
                                        sb.append("\tat " + stackTraceElement + "\r\n");
                                    }
                                    logger.severe(sb.toString());
                                }
                            } finally {
                                SlowEDTDetector.this.doLog.set(false);
                            }
                        }
                    } else {
                        SlowEDTDetector.this.doLog.set(true);
                    }
                } catch (final Throwable e) {
                    logger.log(e);
                }
                SlowEDTDetector.this.invokeEDT();

            }
        };
        this.invokeEDT();
    }

    protected long now() {
        return Time.systemIndependentCurrentJVMTimeMillis();
    }

    protected void invokeEDT() {
        if (this.detectEDTBlocking.compareAndSet(false, true)) {
            this.lastEDT.set(-1);
            this.lastInvoke.set(now());
            new EDTRunner() {

                @Override
                protected void runInEDT() {
                    SlowEDTDetector.this.lastEDT.set(now() - SlowEDTDetector.this.lastInvoke.get());
                    SlowEDTDetector.this.detectEDTBlocking.set(false);
                }
            };
        }
        this.detector.run();
    }
}
