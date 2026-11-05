package jd.controlling.linkcollector;

import jd.controlling.linkcrawler.CrawledLink;

public class DefaultLinkCollectorListener implements LinkCollectorListener {

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
    }

    @Override
    public void onLinkCrawlerFinished() {
    }

    @Override
    public void onLinkCrawlerNewJob(LinkCollectingJob job) {
    }

}
