package org.jdownloader.api.linkcollector;

import java.util.HashMap;
import java.util.concurrent.CopyOnWriteArraySet;

import jd.controlling.linkcollector.LinkCollectingJob;
import jd.controlling.linkcollector.LinkCollector;
import jd.controlling.linkcollector.LinkCollectorCrawler;
import jd.controlling.linkcollector.LinkCollectorEvent;
import jd.controlling.linkcollector.LinkCollectorListener;
import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.CrawledLinkProperty;

import org.appwork.remoteapi.events.EventPublisher;
import org.appwork.remoteapi.events.RemoteAPIEventsSender;
import org.appwork.remoteapi.events.SimpleEventObject;

public class LinkCollectorEventPublisher implements EventPublisher, LinkCollectorListener {

    private final CopyOnWriteArraySet<RemoteAPIEventsSender> eventSenders = new CopyOnWriteArraySet<RemoteAPIEventsSender>();
    private final String[]                                   eventIDs;

    private enum EVENTID {
        CONTENT_ADDED,
        LINK_ADDED,
        LINK_UPDATE,
        STRUCTURE_REFRESH,
        LIST_LOADED,
        DATA_REFRESH,
        FILTERED_LINKS_EMPTY,
        FILTERED_LINKS_AVAILABLE,
        ABORT,
        DUPE_ADDED,
        CONTENT_REMOVED
    }

    public LinkCollectorEventPublisher() {
        eventIDs = new String[] { EVENTID.CONTENT_REMOVED.name(), EVENTID.DUPE_ADDED.name(), EVENTID.ABORT.name(), EVENTID.CONTENT_ADDED.name(), EVENTID.LINK_ADDED.name(), EVENTID.STRUCTURE_REFRESH.name(), EVENTID.LIST_LOADED.name(), EVENTID.DATA_REFRESH.name(), EVENTID.FILTERED_LINKS_EMPTY.name(), EVENTID.FILTERED_LINKS_AVAILABLE.name() };

    }

    @Override
    public String[] getPublisherEventIDs() {
        return eventIDs;
    }

    @Override
    public String getPublisherName() {
        return "linkcollector";
    }

    private final boolean hasSubscriptionFor(final String eventID) {
        if (eventSenders.size() > 0) {
            for (final RemoteAPIEventsSender eventSender : eventSenders) {
                if (eventSender.hasSubscriptionFor(this, eventID)) {
                    return true;
                }
            }
        }
        return false;
    }

    @Override
    public void onLinkCollectorAbort(LinkCollectorEvent event) {
        sendEventID(EVENTID.ABORT);
    }

    private final void sendEventID(EVENTID eventid) {
        if (eventid != null && hasSubscriptionFor(eventid.name())) {
            final SimpleEventObject eventObject = new SimpleEventObject(this, eventid.name(), eventid.name());
            for (final RemoteAPIEventsSender eventSender : eventSenders) {
                eventSender.publishEvent(eventObject, null);
            }
        }
    }

    private void fire(final String eventID, final Object dls, final String collapseKey) {
        if (eventID != null && hasSubscriptionFor(eventID)) {
            final SimpleEventObject eventObject = new SimpleEventObject(this, eventID, dls, collapseKey);
            for (final RemoteAPIEventsSender eventSender : eventSenders) {
                eventSender.publishEvent(eventObject, null);
            }
        }
    }

    @Override
    public void onLinkCollectorFilteredLinksAvailable(LinkCollectorEvent event) {
        sendEventID(EVENTID.FILTERED_LINKS_AVAILABLE);
    }

    @Override
    public void onLinkCollectorFilteredLinksEmpty(LinkCollectorEvent event) {
        sendEventID(EVENTID.FILTERED_LINKS_EMPTY);
    }

    private static final String LINK_UPDATE_name         = EVENTID.LINK_UPDATE.name() + ".name";
    private static final String LINK_UPDATE_enabled      = EVENTID.LINK_UPDATE.name() + ".enabled";
    private static final String LINK_UPDATE_availability = EVENTID.LINK_UPDATE.name() + ".availability";
    private static final String LINK_UPDATE_priority     = EVENTID.LINK_UPDATE.name() + ".priority";

    @Override
    public void onLinkCollectorDataRefresh(LinkCollectorEvent event) {
        sendEventID(EVENTID.DATA_REFRESH);
        final Object param = event.getParameter(1);
        if (param instanceof CrawledLinkProperty) {
            final CrawledLinkProperty property = (CrawledLinkProperty) param;
            final CrawledLink cl = property.getCrawledLink();
            switch (property.getProperty()) {
            case ENABLED:
                if (hasSubscriptionFor(LINK_UPDATE_enabled)) {
                    final HashMap<String, Object> dls = new HashMap<String, Object>();
                    dls.put("uuid", cl.getUniqueID().getID());
                    dls.put("enabled", cl.isEnabled());
                    fire(LINK_UPDATE_enabled, dls, LINK_UPDATE_enabled + "." + cl.getUniqueID().getID());
                }
                break;
            case NAME:
                if (hasSubscriptionFor(LINK_UPDATE_name)) {
                    final HashMap<String, Object> dls = new HashMap<String, Object>();
                    dls.put("uuid", cl.getUniqueID().getID());
                    dls.put("name", cl.getName());
                    fire(LINK_UPDATE_name, dls, LINK_UPDATE_name + "." + cl.getUniqueID().getID());
                }
                break;
            case AVAILABILITY:
                if (hasSubscriptionFor(LINK_UPDATE_availability)) {
                    final HashMap<String, Object> dls = new HashMap<String, Object>();
                    dls.put("uuid", cl.getUniqueID().getID());
                    dls.put("availability", property.getValue());
                    fire(LINK_UPDATE_availability, dls, LINK_UPDATE_availability + "." + cl.getUniqueID().getID());
                }
                break;
            case PRIORITY:
                if (hasSubscriptionFor(LINK_UPDATE_priority)) {
                    final HashMap<String, Object> dls = new HashMap<String, Object>();
                    dls.put("uuid", cl.getUniqueID().getID());
                    dls.put("priority", org.jdownloader.myjdownloader.client.bindings.PriorityStorable.get(cl.getPriorityEnum().name()));
                    fire(LINK_UPDATE_priority, dls, LINK_UPDATE_priority + "." + cl.getUniqueID().getID());
                }
                break;
            default:
                break;
            }
        }
    }

    @Override
    public void onLinkCollectorStructureRefresh(LinkCollectorEvent event) {
        sendEventID(EVENTID.STRUCTURE_REFRESH);
    }

    @Override
    public void onLinkCollectorContentRemoved(LinkCollectorEvent event) {
        sendEventID(EVENTID.CONTENT_REMOVED);
    }

    @Override
    public void onLinkCollectorContentAdded(LinkCollectorEvent event) {
        sendEventID(EVENTID.CONTENT_ADDED);
    }

    @Override
    public void onLinkCollectorLinkAdded(LinkCollectorEvent event, CrawledLink parameter) {
        sendEventID(EVENTID.LINK_ADDED);
    }

    @Override
    public void onLinkCollectorDupeAdded(LinkCollectorEvent event, CrawledLink parameter) {
        sendEventID(EVENTID.DUPE_ADDED);
    }

    @Override
    public synchronized void register(RemoteAPIEventsSender eventsAPI) {
        final boolean wasEmpty = eventSenders.isEmpty();
        eventSenders.add(eventsAPI);
        if (wasEmpty && eventSenders.isEmpty() == false) {
            LinkCollector.getInstance().getEventsender().addListener(this, true);
        }
    }

    @Override
    public synchronized void unregister(RemoteAPIEventsSender eventsAPI) {
        eventSenders.remove(eventsAPI);
        if (eventSenders.isEmpty()) {
            LinkCollector.getInstance().getEventsender().removeListener(this);
        }
    }

    @Override
    public void onLinkCrawlerAdded(LinkCollectorCrawler parameter) {
    }

    @Override
    public void onLinkCrawlerStarted(LinkCollectorCrawler parameter) {
    }

    @Override
    public void onLinkCrawlerStopped(LinkCollectorCrawler parameter) {
    }

    @Override
    public void onLinkCrawlerNewJob(LinkCollectingJob job) {
    }

    @Override
    public void onLinkCrawlerFinished() {
    }

}
