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
package org.appwork.utils.net;

import java.util.ArrayList;
import java.util.Iterator;

import org.appwork.utils.event.BasicEvent;
import org.appwork.utils.event.BasicEventSender;


/**
 * @author daniel
 * 
 */
public class ConnectionLimiter {

    private static final Object LOCK = new Object();
    public static final int CONNECTION_OPENED = 0;
    public static final int CONNECTION_CLOSED = 1;
    /**
     * fired when more than {@link #getMaxConcurrent()} connections are open.
     * waits until {@link #WAIT_DUETO_MAXSIMULTAN_LIMIT_END} is called. No
     * parameter:ms waited until noe
     */
    public static final int WAIT_DUETO_MAXSIMULTAN_LIMIT_END = 2;
    /**
     * @see #WAIT_DUETO_MAXSIMULTAN_LIMIT_START parameter: ms waited total
     */
    public static final int WAIT_DUETO_MAXSIMULTAN_LIMIT_START = 3;
    /**
     * IOs fired when the {@link #getConnectionTimeLimit()} limit is reached.
     * 
     * parameter: ms to wait
     * 
     */
    public static final int WAIT_DUETO_CONNECTIONSPERMINUTE_LIMIT_START = 4;

    public static final int WAIT_DUETO_CONNECTIONSPERMINUTE_LIMIT_END = 5;
    private int connectioncount;
    private final java.util.List<Long> list = new ArrayList<Long>();
    private int maxConcurrent = -1;
    private int timeConnections = -1;
    private long timeTime = -1;
    private final BasicEventSender<Long> eventSender;

    public ConnectionLimiter() {
        this.eventSender = new BasicEventSender<Long>();
    }

    /**
     * DON'T forget to call this after connection is closed
     */
    public void closedConnection() {
        synchronized (ConnectionLimiter.LOCK) {
            this.connectioncount--;
        }
        this.eventSender.fireEvent(new BasicEvent<Long>(this, ConnectionLimiter.CONNECTION_CLOSED, null, null));

    }

    /**
     * get current allowed connection in specific time or -1,-1 if none set
     * 
     * @return
     */
    public long[] getConnectionTimeLimit() {
        return new long[] { this.timeConnections, this.timeTime };
    }

    /**
     * @return the eventSender
     */
    public BasicEventSender<Long> getEventSender() {
        return this.eventSender;
    }

    /**
     * returns current maximum of allowed concurrent connections or -1 for
     * unlimited
     * 
     * @return
     */
    public int getMaxConcurrent() {
        return this.maxConcurrent;
    }

    /**
     * call this if before you open a new connection
     * 
     * @throws InterruptedException
     */
    public synchronized void openedConnection() throws InterruptedException {
        if (this.maxConcurrent > 0) {
            boolean waiting = false;
            long ms = -1;
            while (true) {
                synchronized (ConnectionLimiter.LOCK) {
                    if (this.connectioncount < this.maxConcurrent) {
                        if (waiting) {
                            this.eventSender.fireEvent(new BasicEvent<Long>(this, ConnectionLimiter.WAIT_DUETO_MAXSIMULTAN_LIMIT_END, System.currentTimeMillis() - ms, null));

                        }
                        break;
                    }
                }
                if (ms < 0) {
                    ms = System.currentTimeMillis();
                }
                      org.appwork.loggingv3.LogV3.warning("block 250 ms for " + this.maxConcurrent + " connectionlimit");
                this.eventSender.fireEvent(new BasicEvent<Long>(this, ConnectionLimiter.WAIT_DUETO_MAXSIMULTAN_LIMIT_START, System.currentTimeMillis() - ms, null));
                waiting = true;
                Thread.sleep(250);
            }
        }
        boolean waiting = false;
        if (this.timeConnections > 0) {
            while (true) {
                final Iterator<Long> it = this.list.iterator();
                while (true) {
                    if (it.hasNext()) {
                        if (it.next() + this.timeTime < System.currentTimeMillis()) {
                            it.remove();
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }
                if (this.list.size() >= this.timeConnections) {
                    long wait = this.timeConnections;
                    if (it.hasNext()) {
                        /* calculate how long we have to wait */
                        wait = Math.max(250, it.next() - (System.currentTimeMillis() - this.timeTime));
                    }
                    waiting = true;
                          org.appwork.loggingv3.LogV3.warning("wait " + wait + " ms because we got " + this.list.size() + " connections the last minute");
                    this.eventSender.fireEvent(new BasicEvent<Long>(this, ConnectionLimiter.WAIT_DUETO_CONNECTIONSPERMINUTE_LIMIT_START, wait, null));

                    Thread.sleep(wait);
                } else {
                    break;
                }
            }
            this.list.add(System.currentTimeMillis());
            // System.out.println(list.size());
        }
        synchronized (ConnectionLimiter.LOCK) {
            this.connectioncount++;
        }
        if (waiting) {
            this.eventSender.fireEvent(new BasicEvent<Long>(this, ConnectionLimiter.WAIT_DUETO_CONNECTIONSPERMINUTE_LIMIT_END, null, null));

        }
        this.eventSender.fireEvent(new BasicEvent<Long>(this, ConnectionLimiter.CONNECTION_OPENED, null, null));

    }

    /**
     * set max allowed connections in specific time (ms)
     * 
     * @param connections
     * @param time
     */
    public synchronized void setConnectionTimeLimit(final int connections, final long time) {
        if (connections > 0 && time > 0) {
            this.timeConnections = connections;
            this.timeTime = time;
        } else {
            this.timeConnections = -1;
            this.timeTime = -1;
        }
    }

    /**
     * set max allowed concurrent connections
     * 
     * @param max
     */
    public synchronized void setMaxConcurrent(int max) {
        if (max <= 0) {
            max = -1;
        }
        this.maxConcurrent = max;
    }
}
