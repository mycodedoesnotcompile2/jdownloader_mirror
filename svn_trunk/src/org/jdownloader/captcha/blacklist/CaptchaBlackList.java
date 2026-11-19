package org.jdownloader.captcha.blacklist;

import java.util.ArrayList;
import java.util.List;
import java.util.WeakHashMap;
import java.util.concurrent.atomic.AtomicLong;

import jd.controlling.downloadcontroller.DownloadLinkCandidate;
import jd.controlling.downloadcontroller.DownloadLinkCandidateResult;
import jd.controlling.downloadcontroller.DownloadWatchDog;
import jd.controlling.downloadcontroller.DownloadWatchDogProperty;
import jd.controlling.downloadcontroller.PrePluginCheckDummyChallenge;
import jd.controlling.downloadcontroller.SingleDownloadController;
import jd.controlling.downloadcontroller.event.DownloadWatchdogListener;
import jd.controlling.linkcollector.LinkCollectingJob;
import jd.controlling.linkcollector.LinkCollector;
import jd.controlling.linkcollector.LinkCollectorCrawler;
import jd.controlling.linkcollector.LinkCollectorEvent;
import jd.controlling.linkcollector.LinkCollectorListener;
import jd.controlling.linkcrawler.CrawledLink;
import jd.plugins.DownloadLink;

import org.jdownloader.captcha.v2.Challenge;

public class CaptchaBlackList implements DownloadWatchdogListener, LinkCollectorListener {
    private static final CaptchaBlackList   INSTANCE = new CaptchaBlackList();
    private final ArrayList<BlacklistEntry> entries  = new ArrayList<BlacklistEntry>();
    /* Changes whenever an entry is added or removed, so GUI components can tell cheaply whether they have to read the entries again. */
    private final AtomicLong                version  = new AtomicLong(0);

    private CaptchaBlackList() {
        DownloadWatchDog.getInstance().getEventSender().addListener(this);
        LinkCollector.getInstance().getEventsender().addListener(this);
    }

    public static CaptchaBlackList getInstance() {
        return INSTANCE;
    }

    /** Changes whenever an entry is added or removed. */
    public long getVersion() {
        return version.get();
    }

    /** Returns a copy of the current entries. */
    public List<BlacklistEntry> listEntries() {
        synchronized (entries) {
            return new ArrayList<BlacklistEntry>(entries);
        }
    }

    /** Removes the given entry, so the captchas it blocked are not skipped any more. Returns true if the entry was part of the list. */
    public boolean remove(final BlacklistEntry entry) {
        final boolean removed;
        synchronized (entries) {
            removed = entries.remove(entry);
        }
        if (removed) {
            version.incrementAndGet();
        }
        return removed;
    }

    public void add(BlacklistEntry entry) {
        if (entry != null) {
            synchronized (entries) {
                entries.add(entry);
            }
            version.incrementAndGet();
        }
        synchronized (whitelist) {
            final ArrayList<DownloadLink> rem = new ArrayList<DownloadLink>();
            for (final DownloadLink link : whitelist.keySet()) {
                if (entry.matches(new PrePluginCheckDummyChallenge(link))) {
                    rem.add(link);
                }
            }
            whitelist.keySet().removeAll(rem);
        }
    }

    public BlacklistEntry matches(Challenge<?> c) {
        if (c == null || Boolean.TRUE.equals(c.isAccountLogin())) {
            return null;
        } else {
            return matches(c, false);
        }
    }

    private BlacklistEntry matches(Challenge<?> c, boolean bypasswhitelist) {
        if (c != null) {
            if (!bypasswhitelist) {
                final DownloadLink link = c.getDownloadLink();
                if (link != null) {
                    synchronized (whitelist) {
                        if (whitelist.containsKey(link)) {
                            return null;
                        }
                    }
                }
            }
            synchronized (entries) {
                final ArrayList<BlacklistEntry> cleanups = new ArrayList<BlacklistEntry>();
                try {
                    for (final BlacklistEntry e : entries) {
                        if (e.canCleanUp()) {
                            cleanups.add(e);
                        } else if (e.matches(c)) {
                            return e;
                        }
                    }
                } finally {
                    if (entries.removeAll(cleanups)) {
                        version.incrementAndGet();
                    }
                }
            }
        }
        return null;
    }

    @Override
    public void onDownloadWatchdogDataUpdate() {
    }

    @Override
    public void onDownloadWatchdogStateIsIdle() {
    }

    @Override
    public void onDownloadWatchdogStateIsPause() {
    }

    @Override
    public void onDownloadWatchdogStateIsRunning() {
    }

    @Override
    public void onDownloadWatchdogStateIsStopped() {
        synchronized (entries) {
            for (int index = entries.size() - 1; index >= 0; index--) {
                final BlacklistEntry entry = entries.get(index);
                if (entry.canCleanUp() || entry instanceof SessionBlackListEntry) {
                    entries.remove(index);
                    version.incrementAndGet();
                }
            }
        }
        synchronized (whitelist) {
            whitelist.clear();
        }
    }

    @Override
    public void onDownloadWatchdogStateIsStopping() {
    }

    @Override
    public void onDownloadControllerStart(SingleDownloadController downloadController, DownloadLinkCandidate candidate) {
    }

    @Override
    public void onDownloadControllerStopped(SingleDownloadController downloadController, DownloadLinkCandidate candidate, DownloadLinkCandidateResult result) {
        cleanup();
    }

    private final WeakHashMap<DownloadLink, Object> whitelist = new WeakHashMap<DownloadLink, Object>();

    public void addWhitelist(DownloadLink link) {
        if (matches(new PrePluginCheckDummyChallenge(link), true) != null) {
            synchronized (whitelist) {
                whitelist.put(link, this);
            }
            cleanup();
        }
    }

    protected void cleanup() {
        synchronized (entries) {
            for (int index = entries.size() - 1; index >= 0; index--) {
                if (entries.get(index).canCleanUp()) {
                    entries.remove(index);
                    version.incrementAndGet();
                }
            }
        }
    }

    public boolean isWhitelisted(DownloadLink downloadLink) {
        synchronized (whitelist) {
            return whitelist.containsKey(downloadLink);
        }
    }

    @Override
    public void onDownloadWatchDogPropertyChange(DownloadWatchDogProperty propertyChange) {
    }

    @Override
    public void onLinkCollectorAbort(LinkCollectorEvent event) {
    }

    @Override
    public void onLinkCollectorFilteredLinksAvailable(LinkCollectorEvent event) {
    }

    @Override
    public void onLinkCollectorFilteredLinksEmpty(LinkCollectorEvent event) {
    }

    @Override
    public void onLinkCollectorDataRefresh(LinkCollectorEvent event) {
    }

    @Override
    public void onLinkCollectorStructureRefresh(LinkCollectorEvent event) {
    }

    @Override
    public void onLinkCollectorContentRemoved(LinkCollectorEvent event) {
    }

    @Override
    public void onLinkCollectorContentAdded(LinkCollectorEvent event) {
    }

    @Override
    public void onLinkCollectorLinkAdded(LinkCollectorEvent event, CrawledLink link) {
    }

    @Override
    public void onLinkCollectorDupeAdded(LinkCollectorEvent event, CrawledLink link) {
    }

    @Override
    public void onLinkCrawlerAdded(LinkCollectorCrawler crawler) {
    }

    @Override
    public void onLinkCrawlerStarted(LinkCollectorCrawler crawler) {
    }

    @Override
    public void onLinkCrawlerStopped(LinkCollectorCrawler crawler) {
        cleanup();
    }

    @Override
    public void onLinkCrawlerFinished() {
    }

    @Override
    public void onLinkCrawlerNewJob(LinkCollectingJob job) {
    }
}
