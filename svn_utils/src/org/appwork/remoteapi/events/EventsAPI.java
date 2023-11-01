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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.regex.Pattern;

import org.appwork.loggingv3.LogV3;
import org.appwork.remoteapi.RemoteAPIRequest;
import org.appwork.remoteapi.RemoteAPIResponse;
import org.appwork.remoteapi.events.json.EventObjectStorable;
import org.appwork.remoteapi.events.json.PublisherResponse;
import org.appwork.remoteapi.events.json.SubscriptionResponse;
import org.appwork.remoteapi.events.json.SubscriptionStatusResponse;
import org.appwork.remoteapi.events.local.LocalEventsAPIEvent;
import org.appwork.remoteapi.events.local.LocalEventsAPIEventSender;
import org.appwork.remoteapi.exceptions.APIFileNotFoundException;
import org.appwork.remoteapi.exceptions.InternalApiException;

/**
 * @author daniel
 *
 */
public class EventsAPI implements EventsAPIInterface, RemoteAPIEventsSender {
    private final LocalEventsAPIEventSender localEventSender;

    /**
     *
     */
    public EventsAPI() {
        localEventSender = new LocalEventsAPIEventSender();
    }

    public LocalEventsAPIEventSender getLocalEventSender() {
        return localEventSender;
    }

    protected final CopyOnWriteArrayList<Subscriber> subscribers = new CopyOnWriteArrayList<Subscriber>();

    public List<Subscriber> getSubscribers() {
        return Collections.unmodifiableList(this.subscribers);
    }

    protected final CopyOnWriteArrayList<EventPublisher> publishers             = new CopyOnWriteArrayList<EventPublisher>();
    protected final Object                               subscribersCleanupLock = new Object();
    protected Thread                                     cleanupThread          = null;

    @Override
    public SubscriptionResponse addsubscription(final long subscriptionid, final String[] subscriptions, final String[] exclusions) {
        final Subscriber subscriber = getSubscriber(subscriptionid);
        if (subscriber == null) {
            return new SubscriptionResponse();
        } else {
            synchronized (subscriber.getModifyLock()) {
                if (exclusions != null) {
                    final List<Pattern> newExclusions = new ArrayList<Pattern>(Arrays.asList(subscriber.getExclusions()));
                    newExclusions.addAll(convertToPatternList(exclusions));
                    subscriber.setExclusions(newExclusions.toArray(new Pattern[] {}));
                }
                if (subscriptions != null) {
                    final List<Pattern> newSubscriptions = new ArrayList<Pattern>(Arrays.asList(subscriber.getSubscriptions()));
                    newSubscriptions.addAll(convertToPatternList(subscriptions));
                    subscriber.setSubscriptions(newSubscriptions.toArray(new Pattern[] {}));
                }
            }
            final SubscriptionResponse ret = new SubscriptionResponse(subscriber);
            try {
                localEventSender.fireEvent(new LocalEventsAPIEvent(this, LocalEventsAPIEvent.Type.CHANNEL_UPDATE, subscriber));
            } catch (final Throwable e) {
            }
            return ret;
        }
    }

    private List<Pattern> convertToPatternList(String[] pattern) {
        final List<Pattern> ret = new ArrayList<Pattern>();
        if (pattern != null) {
            for (final String pat : pattern) {
                try {
                    ret.add(Pattern.compile(pat));
                } catch (final Throwable e) {
                    LogV3.log(e);
                }
            }
        }
        return ret;
    }

    private Pattern[] convertToPatternArray(String[] pattern) {
        return convertToPatternList(pattern).toArray(new Pattern[0]);
    }

    protected Subscriber getSubscriber(long subscriptionid) {
        for (final Subscriber subscriber : subscribers) {
            if (subscriptionid == subscriber.getSubscriptionID()) {
                return subscriber;
            }
        }
        return null;
    }

    protected EventObject pollEvent(Subscriber subscriber, long waitfor) throws InterruptedException {
        return subscriber.poll(waitfor);
    }

    protected void pushBackEvent(Subscriber subscriber, List<EventObject> events) {
        subscriber.pushBack(events);
    }

    @Override
    public SubscriptionResponse changesubscriptiontimeouts(final long subscriptionid, final long polltimeout, final long maxkeepalive) {
        final Subscriber subscriber = getSubscriber(subscriptionid);
        if (subscriber == null) {
            return new SubscriptionResponse();
        } else {
            subscriber.setMaxKeepalive(maxkeepalive);
            subscriber.setPollTimeout(polltimeout);
            subscriber.notifyListener();
            final SubscriptionResponse ret = new SubscriptionResponse(subscriber);
            try {
                localEventSender.fireEvent(new LocalEventsAPIEvent(this, LocalEventsAPIEvent.Type.CHANNEL_UPDATE, subscriber));
            } catch (final Throwable e) {
            }
            return ret;
        }
    }

    @Override
    public SubscriptionResponse getsubscription(final long subscriptionid) {
        final Subscriber subscriber = getSubscriber(subscriptionid);
        if (subscriber != null) {
            final SubscriptionResponse ret = new SubscriptionResponse(subscriber);
            return ret;
        } else {
            return new SubscriptionResponse();
        }
    }

    @Override
    public SubscriptionStatusResponse getsubscriptionstatus(final long subscriptionid) {
        final Subscriber subscriber = getSubscriber(subscriptionid);
        if (subscriber == null) {
            return new SubscriptionStatusResponse();
        } else {
            subscriber.keepAlive();
            final SubscriptionStatusResponse ret = new SubscriptionStatusResponse(subscriber);
            return ret;
        }
    }

    public List<EventPublisher> list() {
        return Collections.unmodifiableList(this.publishers);
    }

    @Override
    public void listen(final RemoteAPIRequest request, final RemoteAPIResponse response, final long subscriptionid) throws APIFileNotFoundException, InternalApiException {
        final Subscriber subscriber = getSubscriber(subscriptionid);
        if (subscriber == null) {
            throw new APIFileNotFoundException();
        }
        final ArrayList<EventObject> events = new ArrayList<EventObject>();
        final ArrayList<EventObjectStorable> eventStorables = new ArrayList<EventObjectStorable>();
        try {
            EventObject event;
            while (getSubscriber(subscriptionid) == subscriber && (event = pollEvent(subscriber, events.size() == 0 ? subscriber.getPollTimeout() : 0)) != null) {
                events.add(event);
                eventStorables.add(new EventObjectStorable(event));
            }
        } catch (final InterruptedException e) {
        }
        try {
            response.getRemoteAPI().writeStringResponse(eventStorables, null, request, response);
        } catch (final Throwable e) {
            subscriber.pushBack(events);
            throw new InternalApiException(e);
        }
    }

    @Override
    public List<PublisherResponse> listpublisher() {
        final ArrayList<PublisherResponse> ret = new ArrayList<PublisherResponse>();
        for (final EventPublisher publisher : this.publishers) {
            ret.add(new PublisherResponse(publisher));
        }
        return ret;
    }

    @Override
    public boolean hasSubscriptionFor(EventPublisher publisher, String eventID) {
        if (eventID != null && subscribers.size() > 0) {
            final String event = publisher.getPublisherName().concat(".").concat(eventID);
            for (final Subscriber subscriber : subscribers) {
                if (subscriber.isAlive() && subscriber.isSubscribed(event)) {
                    return true;
                }
            }
        }
        return false;
    }

    public List<Long> publishEvent(final EventObject event, final List<Long> subscriptionids) {
        final ArrayList<Long> ret = new ArrayList<Long>();
        final String eventID = getEventID(event);
        if (subscriptionids != null && subscriptionids.size() > 0) {
            /* publish to given subscriptionids */
            for (final long subscriptionid : subscriptionids) {
                final Subscriber subscriber = getSubscriber(subscriptionid);
                if (subscriber != null) {
                    if (push(subscriber, eventID, event)) {
                        ret.add(subscriber.getSubscriptionID());
                    }
                }
            }
        } else {
            /* publish to all subscribers */
            for (final Subscriber subscriber : subscribers) {
                if (push(subscriber, eventID, event)) {
                    ret.add(subscriber.getSubscriptionID());
                }
            }
        }
        return ret;
    }

    public synchronized boolean register(final EventPublisher publisher) {
        if (publisher == null) {
            throw new NullPointerException();
        }
        if (publisher.getPublisherName() == null) {
            throw new IllegalArgumentException("no Publishername given");
        }
        for (final EventPublisher existingPublisher : this.publishers) {
            if (existingPublisher == publisher) {
                return false;
            }
            if (publisher.getPublisherName().equalsIgnoreCase(existingPublisher.getPublisherName())) {
                throw new IllegalArgumentException("publisher with same name already registered");
            }
        }
        this.publishers.add(publisher);
        publisher.register(this);
        return true;
    }

    @Override
    public SubscriptionResponse removesubscription(final long subscriptionid, final String[] subscriptions, final String[] exclusions) {
        final Subscriber subscriber = getSubscriber(subscriptionid);
        if (subscriber == null) {
            return new SubscriptionResponse();
        } else {
            synchronized (subscriber.getModifyLock()) {
                if (exclusions != null) {
                    final List<Pattern> newExclusions = new ArrayList<Pattern>(Arrays.asList(subscriber.getExclusions()));
                    newExclusions.removeAll(convertToPatternList(exclusions));
                    subscriber.setExclusions(newExclusions.toArray(new Pattern[] {}));
                }
                if (subscriptions != null) {
                    final List<Pattern> newSubscriptions = new ArrayList<Pattern>(Arrays.asList(subscriber.getSubscriptions()));
                    newSubscriptions.removeAll(convertToPatternList(subscriptions));
                    subscriber.setSubscriptions(newSubscriptions.toArray(new Pattern[] {}));
                }
            }
            final SubscriptionResponse ret = new SubscriptionResponse(subscriber);
            try {
                localEventSender.fireEvent(new LocalEventsAPIEvent(this, LocalEventsAPIEvent.Type.CHANNEL_UPDATE, subscriber));
            } catch (final Throwable e) {
            }
            return ret;
        }
    }

    @Override
    public SubscriptionResponse setsubscription(final long subscriptionid, final String[] subscriptions, final String[] exclusions) {
        Subscriber subscriber = getSubscriber(subscriptionid);
        if (subscriber == null) {
            subscriber = new Subscriber(subscriptionid, convertToPatternArray(subscriptions), convertToPatternArray(exclusions));
            if (addSubscriber(subscriber)) {
                return new SubscriptionResponse(subscriber);
            } else {
                return new SubscriptionResponse();
            }
        } else {
            synchronized (subscriber.getModifyLock()) {
                final List<Pattern> newExclusions = new ArrayList<Pattern>();
                if (exclusions != null) {
                    newExclusions.addAll(convertToPatternList(exclusions));
                }
                subscriber.setExclusions(newExclusions.toArray(new Pattern[] {}));
                final List<Pattern> newSubscriptions = new ArrayList<Pattern>();
                if (subscriptions != null) {
                    newSubscriptions.addAll(convertToPatternList(subscriptions));
                }
                subscriber.setSubscriptions(newSubscriptions.toArray(new Pattern[] {}));
            }
            final SubscriptionResponse ret = new SubscriptionResponse(subscriber);
            try {
                localEventSender.fireEvent(new LocalEventsAPIEvent(this, LocalEventsAPIEvent.Type.CHANNEL_UPDATE, subscriber));
            } catch (final Throwable e) {
            }
            return ret;
        }
    }

    public boolean addSubscriber(Subscriber subscriber) {
        if (subscriber != null && subscriber.isAlive()) {
            final Subscriber existing = getSubscriber(subscriber.getSubscriptionID());
            if (existing == null && subscribers.addIfAbsent(subscriber)) {
                this.subscribersCleanupThread();
                try {
                    localEventSender.fireEvent(new LocalEventsAPIEvent(this, LocalEventsAPIEvent.Type.CHANNEL_OPENED, subscriber));
                } catch (final Throwable e) {
                }
                return true;
            }
        }
        return false;
    }

    public boolean removeSubscriber(Subscriber subscriber) {
        if (subscriber != null && subscribers.remove(subscriber)) {
            subscriber.kill();
            subscriber.notifyListener();
            try {
                localEventSender.fireEvent(new LocalEventsAPIEvent(this, LocalEventsAPIEvent.Type.CHANNEL_CLOSED, subscriber));
            } catch (final Throwable e) {
            }
            return true;
        }
        return false;
    }

    @Override
    public SubscriptionResponse subscribe(final String[] subscriptions, final String[] exclusions) {
        final Subscriber subscriber = new Subscriber(convertToPatternArray(subscriptions), convertToPatternArray(exclusions));
        this.subscribers.add(subscriber);
        this.subscribersCleanupThread();
        final SubscriptionResponse ret = new SubscriptionResponse(subscriber);
        try {
            localEventSender.fireEvent(new LocalEventsAPIEvent(this, LocalEventsAPIEvent.Type.CHANNEL_OPENED, subscriber));
        } catch (final Throwable e) {
        }
        return ret;
    }

    /*
     * starts a cleanupThread (if needed) to remove subscribers that are no longer alive
     *
     * current implementation has a minimum delay of 1 minute
     */
    protected void subscribersCleanupThread() {
        synchronized (this.subscribersCleanupLock) {
            if (this.cleanupThread == null || this.cleanupThread.isAlive() == false) {
                this.cleanupThread = null;
            } else {
                return;
            }
            this.cleanupThread = new Thread("EventsAPI:subscribersCleanupThread") {
                @Override
                public void run() {
                    try {
                        while (Thread.currentThread() == EventsAPI.this.cleanupThread) {
                            try {
                                Thread.sleep(60 * 1000);
                                final Iterator<Subscriber> it = subscribers.iterator();
                                while (it.hasNext()) {
                                    final Subscriber subscriber = it.next();
                                    if (!subscriber.isAlive() || subscriber.isExpired()) {
                                        if (subscribers.remove(subscriber)) {
                                            subscriber.kill();
                                            subscriber.notifyListener();
                                            try {
                                                localEventSender.fireEvent(new LocalEventsAPIEvent(EventsAPI.this, LocalEventsAPIEvent.Type.CHANNEL_CLOSED, subscriber));
                                            } catch (final Throwable e) {
                                                e.printStackTrace();
                                            }
                                        }
                                    }
                                }
                                synchronized (EventsAPI.this.subscribersCleanupLock) {
                                    if (EventsAPI.this.subscribers.size() == 0) {
                                        EventsAPI.this.cleanupThread = null;
                                        break;
                                    }
                                }
                            } catch (final Throwable e) {
                            }
                        }
                    } finally {
                        synchronized (EventsAPI.this.subscribersCleanupLock) {
                            if (Thread.currentThread() == EventsAPI.this.cleanupThread) {
                                EventsAPI.this.cleanupThread = null;
                            }
                        }
                    }
                };
            };
            this.cleanupThread.setDaemon(true);
            this.cleanupThread.start();
        }
    }

    public synchronized boolean unregister(final EventPublisher publisher) {
        if (publisher == null) {
            throw new NullPointerException();
        }
        final boolean removed = this.publishers.remove(publisher);
        publisher.unregister(this);
        return removed;
    }

    @Override
    public SubscriptionResponse unsubscribe(final long subscriptionid) {
        final Subscriber subscriber = getSubscriber(subscriptionid);
        if (subscriber != null && subscribers.remove(subscriber)) {
            subscriber.kill();
            subscriber.notifyListener();
            try {
                localEventSender.fireEvent(new LocalEventsAPIEvent(this, LocalEventsAPIEvent.Type.CHANNEL_CLOSED, subscriber));
            } catch (final Throwable e) {
            }
            return new SubscriptionResponse(subscriber);
        }
        return new SubscriptionResponse();
    }

    public static final String getEventID(EventObject eventObject) {
        if (eventObject != null && eventObject.getEventid() != null) {
            return eventObject.getPublisher().getPublisherName().concat(".").concat(eventObject.getEventid());
        } else {
            return null;
        }
    }

    public boolean push(Subscriber subscriber, EventObject eventObject) {
        if (subscriber.isAlive() && subscriber.isSubscribed(eventObject)) {
            subscriber.push(eventObject);
            subscriber.notifyListener();
            return true;
        }
        return false;
    }

    public boolean push(Subscriber subscriber, final String eventID, EventObject eventObject) {
        if (subscriber.isAlive() && subscriber.isSubscribed(eventID)) {
            subscriber.push(eventObject);
            subscriber.notifyListener();
            return true;
        }
        return false;
    }

    public void push(Subscriber subscriber, List<EventObject> events) {
        if (subscriber.isAlive()) {
            boolean notify = false;
            for (final EventObject event : events) {
                if (subscriber.isSubscribed(event)) {
                    notify = true;
                    subscriber.push(event);
                }
            }
            if (notify) {
                subscriber.notifyListener();
            }
        }
    }

    public void push(Subscriber subscriber, final String eventID, List<EventObject> events) {
        if (subscriber.isAlive()) {
            boolean notify = false;
            for (final EventObject event : events) {
                if (subscriber.isSubscribed(eventID)) {
                    notify = true;
                    subscriber.push(event);
                }
            }
            if (notify) {
                subscriber.notifyListener();
            }
        }
    }
}
