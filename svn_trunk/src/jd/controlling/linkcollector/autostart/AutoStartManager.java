package jd.controlling.linkcollector.autostart;

import java.util.ArrayList;
import java.util.List;
import java.util.WeakHashMap;

import org.appwork.scheduler.DelayedRunnable;
import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.utils.Application;
import org.appwork.utils.event.queue.QueueAction;
import org.appwork.utils.swing.EDTHelper;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.linkgrabber.LinkGrabberTable;
import org.jdownloader.gui.views.linkgrabber.contextmenu.ConfirmLinksContextAction;
import org.jdownloader.myjdownloader.client.json.AvailableLinkState;
import org.jdownloader.settings.staticreferences.CFG_LINKGRABBER;

import jd.controlling.linkcollector.LinkCollectingInformation;
import jd.controlling.linkcollector.LinkCollector;
import jd.controlling.linkcollector.LinkCollector.ConfirmLinksSettings;
import jd.controlling.linkcollector.LinkCollector.JobLinkCrawler;
import jd.controlling.linkcollector.LinkCollector.MoveLinksMode;
import jd.controlling.linkcollector.LinkCollectorCrawler;
import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.CrawledPackage;
import jd.controlling.packagecontroller.AbstractNode;

public class AutoStartManager implements GenericConfigEventListener<Boolean> {
    private final DelayedRunnable             delayer;
    private volatile boolean                  globalAutoStart;
    private volatile boolean                  globalAutoConfirm;
    private final AutoStartManagerEventSender eventSender;

    public AutoStartManagerEventSender getEventSender() {
        return eventSender;
    }

    public AutoStartManager() {
        eventSender = new AutoStartManagerEventSender();
        CFG_LINKGRABBER.LINKGRABBER_AUTO_START_ENABLED.getEventSender().addListener(this, true);
        CFG_LINKGRABBER.LINKGRABBER_AUTO_CONFIRM_ENABLED.getEventSender().addListener(this, true);
        globalAutoStart = CFG_LINKGRABBER.LINKGRABBER_AUTO_START_ENABLED.isEnabled();
        globalAutoConfirm = CFG_LINKGRABBER.LINKGRABBER_AUTO_CONFIRM_ENABLED.isEnabled();
        final int minDelay = Math.max(1, CFG_LINKGRABBER.CFG.getAutoConfirmDelay());
        int maxDelay = CFG_LINKGRABBER.CFG.getAutoConfirmMaxDelay();
        if (maxDelay <= 0) {
            maxDelay = -1;
        } else if (maxDelay < minDelay) {
            maxDelay = minDelay;
        }
        delayer = new DelayedRunnable(minDelay, maxDelay) {
            @Override
            public String getID() {
                return "AutoConfirmButton";
            }

            @Override
            public void delayedrun() {
                final SelectionInfo<CrawledPackage, CrawledLink> selectionInfo;
                if (!Application.isHeadless() && CFG_LINKGRABBER.CFG.isAutoStartConfirmSidebarFilterEnabled()) {
                    /* dirty workaround */
                    selectionInfo = new EDTHelper<SelectionInfo<CrawledPackage, CrawledLink>>() {
                        @Override
                        public SelectionInfo<CrawledPackage, CrawledLink> edtRun() {
                            LinkGrabberTable.getInstance().getModel().fireStructureChange(true);
                            return LinkGrabberTable.getInstance().getSelectionInfo(false, true);
                        }
                    }.getReturnValue();
                } else {
                    selectionInfo = LinkCollector.getInstance().getSelectionInfo();
                }
                LinkCollector.getInstance().getQueue().add(new QueueAction<Void, RuntimeException>() {
                    @Override
                    protected Void run() throws RuntimeException {
                        if (eventSender.hasListener()) {
                            eventSender.fireEvent(new AutoStartManagerEvent(this, AutoStartManagerEvent.Type.RUN));
                        }
                        final List<AbstractNode> list = new ArrayList<AbstractNode>(selectionInfo.getChildren().size());
                        boolean createNewSelection = false;
                        for (final CrawledLink child : selectionInfo.getChildren()) {
                            if (child.getLinkState() == AvailableLinkState.OFFLINE) {
                                /* Skip offline items since they cannot be downloaded anyways */
                                createNewSelection = true;
                                continue;
                            }
                            if (isEligibleForAutoStart(child)) {
                                list.add(child);
                            } else {
                                createNewSelection = true;
                            }
                        }
                        if (list.size() > 0) {
                            final SelectionInfo<CrawledPackage, CrawledLink> si;
                            if (createNewSelection) {
                                si = new SelectionInfo<CrawledPackage, CrawledLink>(null, list);
                            } else {
                                si = selectionInfo;
                            }
                            final ConfirmLinksSettings cls = new ConfirmLinksSettings(MoveLinksMode.AUTO);
                            ConfirmLinksContextAction.confirmSelection(si, cls);
                        }
                        if (delayer.isDelayerActive() == false && eventSender.hasListener()) {
                            eventSender.fireEvent(new AutoStartManagerEvent(this, AutoStartManagerEvent.Type.DONE));
                        }
                        return null;
                    }
                });
            }
        };
    }

    private final WeakHashMap<LinkCollectorCrawler, Boolean> resetMap = new WeakHashMap<LinkCollectorCrawler, Boolean>();

    public void onCrawlerFinished(LinkCollectorCrawler linkCrawler) {
        final Boolean resetFlag;
        synchronized (resetMap) {
            resetFlag = resetMap.get(linkCrawler);
        }
        if (Boolean.TRUE.equals(resetFlag)) {
            resetAndStart(false);
        }
    }

    /**
     * Checks whether the given link is eligible for automatic processing (auto confirm / auto start).
     *
     * A link is eligible if the global auto confirm or global auto start setting is active, or if the link itself carries any of the auto
     * confirm, auto start or forced auto start flags (e.g. set by a Packagizer rule). AutoStart and ForcedAutoStart imply a confirm here,
     * because a download can only start once the link has been moved to the downloadlist.
     *
     * This is the single source of truth shared by {@link #onLinkAdded(CrawledLink)} (which decides whether to arm the auto confirm
     * delayer) and the delayed run (which decides which links actually get confirmed). Both must stay in sync: otherwise a link that only
     * has AutoStart/ForcedAutoStart set would arm the auto confirm button but never actually get moved to the downloadlist.
     *
     * The offline check is intentionally kept separate at the call site.
     */
    private boolean isEligibleForAutoStart(final CrawledLink link) {
        return globalAutoStart || globalAutoConfirm || link.isAutoConfirmEnabled() || link.isAutoStartEnabled() || link.isForcedAutoStartEnabled();
    }

    public void onLinkAdded(CrawledLink link) {
        if (!isEligibleForAutoStart(link)) {
            return;
        }
        final LinkCollectingInformation collectingInfo = link.getCollectingInfo();
        if (collectingInfo != null) {
            final JobLinkCrawler linkCrawler = collectingInfo.getLinkCrawler();
            synchronized (resetMap) {
                resetMap.put(linkCrawler, Boolean.TRUE);
                if (delayer.getMaximumDelay() == -1 && linkCrawler.isCollecting()) {
                    resetAndStart(true);
                    return;
                } else {
                    resetMap.remove(linkCrawler);
                }
            }
        }
        resetAndStart(false);
    }

    protected void resetAndStart(final boolean onlyWhenActive) {
        if (!onlyWhenActive || delayer.isDelayerActive()) {
            delayer.resetAndStart();
            if (eventSender.hasListener()) {
                eventSender.fireEvent(new AutoStartManagerEvent(this, AutoStartManagerEvent.Type.RESET));
            }
        }
    }

    @Override
    public void onConfigValidatorError(KeyHandler<Boolean> keyHandler, Boolean invalidValue, ValidationException validateException) {
    }

    @Override
    public void onConfigValueModified(KeyHandler<Boolean> keyHandler, Boolean newValue) {
        globalAutoStart = CFG_LINKGRABBER.LINKGRABBER_AUTO_START_ENABLED.isEnabled();
        globalAutoConfirm = CFG_LINKGRABBER.LINKGRABBER_AUTO_CONFIRM_ENABLED.isEnabled();
    }

    public int getMaximum() {
        return (int) (delayer.getMinimumDelay());
    }

    public int getValue() {
        return (int) (delayer.getEstimatedNextRun());
    }

    public boolean isRunning() {
        return delayer != null && delayer.isDelayerActive();
    }

    public void interrupt() {
        if (delayer.stop() && eventSender.hasListener()) {
            eventSender.fireEvent(new AutoStartManagerEvent(this, AutoStartManagerEvent.Type.DONE));
        }
    }
}
