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
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils;

import java.util.concurrent.TimeUnit;

/**
 * @author Thomas
 * @date 13.08.2019
 *
 */
public class Time {
    private static final long uptimeNanoTimeReference      = System.nanoTime();
    private static final long uptimeCurrentMillisReference = System.currentTimeMillis();

    /**
     * This method can only be used to measure elapsed time but does't reflect system or wall-clock time.
     *
     * Differences in successive calls that span greater than approximately 292 years (263 nanoseconds) will not correctly compute elapsed
     * time due to numerical overflow, see {@link java.lang.System#nanoTime()}
     *
     * The values returned by this method become meaningful only when the difference between two such values, obtained within the same
     * instance of a Java virtual machine, is computed.
     *
     * @return
     */
    public static final long systemIndependentCurrentJVMTimeMillis() {
        final long difference = System.nanoTime() - uptimeNanoTimeReference;
        return uptimeCurrentMillisReference + TimeUnit.NANOSECONDS.toMillis(difference);
    }

    /**
     * this method returns the elapsed ns since the JVM got started. WARNING: THIS IS NOT A UNIX TIMESTAMP!. Since it it is based on
     * System.nanoTime(), the value is not affected if the user changes his local system time or timezone. This method ensures that the
     * counter. WARNING: Keep in mind that it might happen, that 2 subsequent System.nanoTime() calls may result in the first call haveing a
     * higher number than the second. (https://www.kapsi.de/blog/a-big-flaw-in-javas-nanotime). However.. I could not provoke this issue in
     * my testcase.
     *
     *
     * * https://docs.oracle.com/javase/9/docs/api/java/lang/System.html#nanoTime-- Returns the current value of the running Java Virtual
     * Machine's high-resolution time source, in nanoseconds. This method can only be used to measure elapsed time and is not related to any
     * other notion of system or wall-clock time. The value returned represents nanoseconds since some fixed but arbitrary origin time
     * (perhaps in the future, so values may be negative). The same origin is used by all invocations of this method in an instance of a
     * Java virtual machine; other virtual machine instances are likely to use a different origin.
     *
     * This method provides nanosecond precision, but not necessarily nanosecond resolution (that is, how frequently the value changes) - no
     * guarantees are made except that the resolution is at least as good as that of currentTimeMillis().
     *
     * Differences in successive calls that span greater than approximately 292 years (263 nanoseconds) will not correctly compute elapsed
     * time due to numerical overflow.
     *
     * The values returned by this method become meaningful only when the difference between two such values, obtained within the same
     * instance of a Java virtual machine, is computed.
     *
     * For example, to measure how long some code takes to execute:
     *
     *
     * long startTime = System.nanoTime(); // ... the code being measured ... long elapsedNanos = System.nanoTime() - startTime; To compare
     * elapsed time against a timeout, use
     *
     *
     * if (System.nanoTime() - startTime >= timeoutNanos) ... instead of
     *
     * if (System.nanoTime() >= startTime + timeoutNanos) ... because of the possibility of numerical overflow.
     *
     *
     * @return
     */
    public final static long getNanoSeconds() {
        return System.nanoTime();
    }

    /**
     * System.currentTimeMillis() keep in mind that this this value changes if the user changes his local time or the time is synced
     *
     * @return
     */
    public static long timestamp() {
        return System.currentTimeMillis();
    }

    /**
     * shorter call for {@link Time#timestamp() timestamp} method
     *
     * @return
     */
    public static long now() {
        return timestamp();
    }

    /**
     * @param started
     * @return
     */
    public static long since(long started) {
        return Time.systemIndependentCurrentJVMTimeMillis() - started;
    }
}
