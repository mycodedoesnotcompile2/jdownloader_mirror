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
package org.appwork.remoteapi.events;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Pattern;

/**
 * @author daniel
 *
 */
public class Subscriber {
    protected static final AtomicLong ID = new AtomicLong(System.currentTimeMillis());

    private static long createUniqueAlltimeID() {
        long id = -1;
        while (true) {
            final long lastID = ID.get();
            id = System.currentTimeMillis();
            if (id < lastID) {
                /* WTF?! timestamp is smaller as previous timestamp */
                id = lastID + 1;
            } else if (id == lastID) {
                /* same timestamp, increase by 1 */
                id = id + 1;
            }
            if (ID.compareAndSet(lastID, id)) {
                return id;
            }
        }
    }

    protected volatile Pattern[]            subscriptions;
    protected volatile Pattern[]            exclusions;
    protected final ArrayDeque<EventObject> events              = new ArrayDeque<EventObject>();
    protected final long                    subscriptionID;
    protected long                          lastPolledTimestamp = System.currentTimeMillis();
    protected long                          pollTimeout         = 25 * 1000l;
    protected long                          maxKeepalive        = 120 * 1000l;
    protected final AtomicBoolean           alive               = new AtomicBoolean(true);

    public boolean isAlive() {
        return alive.get();
    }

    public void kill() {
        this.alive.set(false);
    }

    protected Subscriber(final Pattern[] subscriptions, final Pattern[] exclusions) {
        this.setSubscriptions(subscriptions);
        this.setExclusions(exclusions);
        subscriptionID = createUniqueAlltimeID();
    }

    protected Subscriber(final long subscriptionID, final Pattern[] subscriptions, final Pattern[] exclusions) {
        this.setSubscriptions(subscriptions);
        this.setExclusions(exclusions);
        this.subscriptionID = subscriptionID;
    }

    public Pattern[] getExclusions() {
        return this.exclusions.clone();
    }

    public long getLastPolledTimestamp() {
        return this.lastPolledTimestamp;
    }

    /**
     * @return the maxKeepalive
     */
    public long getMaxKeepalive() {
        return this.maxKeepalive;
    }

    protected Object getModifyLock() {
        return this;
    }

    /**
     * @return the pollTimeout
     */
    public long getPollTimeout() {
        return this.pollTimeout;
    }

    public long getSubscriptionID() {
        return this.subscriptionID;
    }

    public Pattern[] getSubscriptions() {
        return this.subscriptions.clone();
    }

    protected boolean isSubscribed(final EventObject event) {
        if (this.subscriptions.length == 0 || !isAlive() || isExpired()) {
            /* no subscriptions = no interest in any event */
            return false;
        }
        final String eventID = event.getPublisher().getPublisherName().concat(".").concat(event.getEventid());
        return isSubscribed(eventID);
    }

    public boolean isSubscribed(final String eventID) {
        if (isAlive() && !isExpired() && this.subscriptions.length > 0) {
            for (final Pattern subscription : this.subscriptions) {
                try {
                    if (subscription.matcher(eventID).find()) {
                        /* we have a subscription match */
                        for (final Pattern exclusion : this.exclusions) {
                            try {
                                if (exclusion.matcher(eventID).find()) {
                                    /*
                                     * there exists an exclusion, no interest in this event
                                     */
                                    return false;
                                }
                            } catch (final Throwable e) {
                                e.printStackTrace();
                            }
                        }
                        return true;
                    }
                } catch (final Throwable e) {
                    e.printStackTrace();
                }
            }
        }
        return false;
    }

    protected void keepAlive() {
        this.lastPolledTimestamp = System.currentTimeMillis();
    }

    public boolean isExpired() {
        return lastPolledTimestamp + maxKeepalive < System.currentTimeMillis();
    }

    protected void notifyListener() {
        synchronized (this.events) {
            this.events.notifyAll();
        }
    }

    protected EventObject poll(final long waitfor) throws InterruptedException {
        synchronized (this.events) {
            keepAlive();
            EventObject ret = this.events.poll();
            if (ret == null && waitfor > 0) {
                this.events.wait(waitfor);
                ret = this.events.poll();
            }
            return ret;
        }
    }

    /**
     * @param filtered
     */
    public void push(List<EventObject> filtered) {
        synchronized (this.events) {
            for (EventObject event : filtered) {
                push(event);
            }
        }
    }

    protected void push(final EventObject event) {
        if (event == null) {
            return;
        }
        synchronized (this.events) {
            if (event.getCollapseKey() != null) {
                /*
                 * event has a collapseKey, so let's search for existing event to replace/remove
                 */
                final Iterator<EventObject> it = this.events.descendingIterator();
                while (it.hasNext()) {
                    final EventObject next = it.next();
                    if (next.getCollapseKey() != null && next.getCollapseKey().equals(event.getCollapseKey())) {
                        it.remove();
                        break;
                    }
                }
            }
            this.events.offerLast(event);
        }
    }

    protected void pushBack(final List<EventObject> pushBackEvents) {
        if (pushBackEvents.size() == 0) {
            return;
        }
        synchronized (this.events) {
            if (this.events.size() == 0) {
                /*
                 * fast, current eventqueue is empty, so we can pushBack all at once
                 */
                this.events.addAll(pushBackEvents);
                return;
            }
            final ArrayList<EventObject> addFirst = new ArrayList<EventObject>(pushBackEvents.size());
            addFirstLoop: for (final EventObject pushBackEvent : pushBackEvents) {
                if (pushBackEvent.getCollapseKey() != null) {
                    for (final EventObject currentEvent : this.events) {
                        if (currentEvent.getCollapseKey() != null && currentEvent.getCollapseKey().equals(pushBackEvent.getCollapseKey())) {
                            continue addFirstLoop;
                        }
                    }
                }
                addFirst.add(pushBackEvent);
            }
            if (addFirst.size() == 0) {
                /* all pushBackEvents were collapsed */
                return;
            }
            /*
             * clear current eventqueue and add all pushBack ones first, then the backup of current ones
             */
            final ArrayList<EventObject> backup = new ArrayList<EventObject>(this.events);
            this.events.clear();
            this.events.addAll(addFirst);
            this.events.addAll(backup);
        }
    }

    protected void setExclusions(final Pattern[] exclusions) {
        if (exclusions == null) {
            this.exclusions = new Pattern[0];
        } else {
            this.exclusions = this.uniquify(exclusions);
        }
    }

    /**
     * @param maxKeepalive
     *            the maxKeepalive to set
     */
    protected void setMaxKeepalive(long maxKeepalive) {
        maxKeepalive = Math.min(maxKeepalive, 3600 * 1000l);
        maxKeepalive = Math.max(maxKeepalive, 30 * 1000l);
        this.maxKeepalive = maxKeepalive;
    }

    /**
     * @param pollTimeout
     *            the pollTimeout to set
     */
    protected void setPollTimeout(long pollTimeout) {
        /*
         * http://gabenell.blogspot.de/2010/11/connection-keep-alive-timeouts-for .html
         */
        pollTimeout = Math.min(pollTimeout, 360 * 1000l);
        pollTimeout = Math.max(pollTimeout, 1 * 1000l);
        this.pollTimeout = pollTimeout;
    }

    protected void setSubscriptions(final Pattern[] subscriptions) {
        if (subscriptions == null) {
            this.subscriptions = new Pattern[0];
        } else {
            this.subscriptions = this.uniquify(subscriptions);
        }
    }

    public int size() {
        synchronized (this.events) {
            return this.events.size();
        }
    }

    private Pattern[] uniquify(final Pattern[] input) {
        if (input == null || input.length == 0) {
            return new Pattern[0];
        }
        return new HashSet<Pattern>(Arrays.asList(input)).toArray(new Pattern[] {});
    }
}
