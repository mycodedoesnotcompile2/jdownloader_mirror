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
package org.appwork.utils.net.httpconnection;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.concurrent.TimeUnit;

import org.appwork.utils.CompareUtils;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;

/**
 * @author Thomas
 * @date 15.08.2019
 *
 */
public class HTTPConnectionProfiler implements HTTPConnectionProfilerInterface {

    protected Long socketConnectedNanos;

    /**
     * WARNING: this might be null for keep alive connections!
     *
     * @return the socketConnectedNanos
     */
    public Long getSocketConnectedNanos() {
        return socketConnectedNanos;
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        ArrayList<Field> fields = new ArrayList<Field>();
        int length = 0;
        for (Field f : getClass().getDeclaredFields()) {
            if (f.getType() == Long.class) {
                f.setAccessible(true);
                fields.add(f);
                length = Math.max(length, f.getName().length());
            }
        }
        Collections.sort(fields, new Comparator<Field>() {

            @Override
            public int compare(Field o1, Field o2) {
                try {
                    Long l1 = o1.get(HTTPConnectionProfiler.this) != null ? ((Long) o1.get(HTTPConnectionProfiler.this)) : null;
                    Long l2 = o2.get(HTTPConnectionProfiler.this) != null ? ((Long) o2.get(HTTPConnectionProfiler.this)) : null;
                    return CompareUtils.compareComparable(l1, l2);
                } catch (IllegalArgumentException e) {
                    return 0;
                } catch (IllegalAccessException e) {
                    return 0;
                }
            }

        });
        StringBuilder sb = new StringBuilder();
        Field last = null;
        for (Field f : fields) {
            try {
                if (last != null) {
                    sb.append("\r\n");
                }
                sb.append(StringUtils.fillPost(f.getName(), " ", length + 1));
                Long nanos = (Long) f.get(this);
                if (nanos == null) {
                    sb.append("-not set-");
                } else {
                    sb.append(nanos + " ");
                    if (last == null) {
                        sb.append(0);
                    } else {
                        long ns = (nanos - (Long) last.get(this));
                        long sinceStart = nanos - start;
                        sb.append(StringUtils.fillPost(formatNStoMS(sinceStart), " ", 10)).append("+ " + formatNStoMS(ns));
                    }
                }
                last = f;
            } catch (IllegalArgumentException e) {
                e.printStackTrace();
            } catch (IllegalAccessException e) {
                e.printStackTrace();
            }
        }
        return sb.toString();
    }

    protected String formatNStoMS(long sinceStart) {
        return TimeUnit.NANOSECONDS.toMillis(sinceStart) + "." + (sinceStart % TimeUnit.MILLISECONDS.toNanos(1));
    }

    /**
     * @return the firstHeaderByteReadNanos
     */
    public Long getFirstHeaderByteReadNanos() {
        return firstHeaderByteReadNanos;
    }

    /**
     * @return the allHeadersReadsNanos
     */
    public Long getAllResponseHeadersReadNanos() {
        return allResponseHeadersReadNanos;
    }

    /**
     * @return the inputStreamConnectedNanos
     */
    public Long getInputStreamConnectedNanos() {
        return inputStreamConnectedNanos;
    }

    /**
     * @return the requestSentNanos
     */
    public Long getRequestSentInclPostNanos() {
        return requestSentInclPostNanos;
    }

    protected Long firstHeaderByteReadNanos;
    protected Long allResponseHeadersReadNanos;
    protected Long inputStreamConnectedNanos;
    protected Long requestSentInclPostNanos;
    private Long   allRequestHeadersSentNanos;
    private Long   connectionFinalizedNanos;
    private Long   start;

    /**
     * @return the start
     */
    public Long getStart() {
        return start;
    }

    // /**
    // * System.nanoTime at which the socket connect has been established. this should be roughly the same time at which the server gets the
    // * serversocket.accept(). This can be null if the even has not been triggered
    // *
    // * @return
    // */
    // public Long getFirstHeaderByteReadNanos();
    //
    // /**
    // * This can be null if the even has not been triggered
    // *
    // * @return the socketConnectedNanos
    // */
    // public Long getSocketConnectedNanos();
    //
    // /**
    // * This can be null if the even has not been triggered
    // *
    // * @return
    // */
    // public Long getAllHeadersReadsNanos();
    //
    // /**
    // * This can be null if the even has not been triggered AFTER the .getInputStream method returned
    // *
    // * @return
    // */
    // public Long getInputStreamConnectedNanos();
    //
    // /**
    // * After the full request incl. headers and postdata has been sent
    // *
    // * @return the requestSentNanos
    // */
    // public Long getRequestSentNanos();
    /**
     *
     */
    @Override
    public void onConnected(HTTPConnection httpConnectionImp) {
        this.socketConnectedNanos = Time.getNanoSeconds();
    }

    @Override
    public void onDisconnected(HTTPConnection httpConnectionImp) {
    }

    @Override
    public void onDisconnect(HTTPConnection httpConnectionImp) {
    }

    /**
     *
     */
    @Override
    public void onBeforeSocketGetInputStream(HTTPConnection httpConnectionImp) {
        if (requestSentInclPostNanos == null) {
            requestSentInclPostNanos = Time.getNanoSeconds();
        }
    }

    /**
     *
     */
    @Override
    public void onAfterSocketGetInputStream(HTTPConnection httpConnectionImp) {
        inputStreamConnectedNanos = Time.getNanoSeconds();
    }

    /**
     *
     */
    @Override
    public void onFirstHeaderByteRead(HTTPConnection httpConnectionImp) {
        firstHeaderByteReadNanos = Time.getNanoSeconds();
    }

    /**
     *
     */
    @Override
    public void onAllResponseHeadersRead(HTTPConnection httpConnectionImp) {
        allResponseHeadersReadNanos = Time.getNanoSeconds();
    }

    /**
     *
     */
    @Override
    public void onBeforeRequestHeadersSent(HTTPConnection httpConnectionImp, CharSequence request) {
    }

    @Override
    public void onAfterRequestHeadersSent(HTTPConnection httpConnectionImp) {
        allRequestHeadersSentNanos = Time.getNanoSeconds();
    }

    /**
     * @return the allRequestHeadersSentNanos
     */
    public Long getAllRequestHeadersSentNanos(HTTPConnection httpConnectionImp) {
        return allRequestHeadersSentNanos;
    }

    /**
     * @param httpConnectionImpl
     */
    @Override
    public void onFinalizeConnection(HTTPConnection httpConnectionImpl) {
        connectionFinalizedNanos = Time.getNanoSeconds();
    }

    /**
     * @return the connectionFinalizedNanos
     */
    public Long getConnectionFinalizedNanos() {
        return connectionFinalizedNanos;
    }

    /**
     * @param httpConnectionImpl
     */
    @Override
    public void onConnect(HTTPConnection httpConnectionImpl) {
        if (start == null) {
            start = Time.getNanoSeconds();
        }
    }

    @Override
    public void onSendRequest(HTTPConnection httpConnectionImp) {
    }

}
