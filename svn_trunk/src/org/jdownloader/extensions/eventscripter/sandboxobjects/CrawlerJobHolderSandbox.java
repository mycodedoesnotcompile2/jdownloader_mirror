package org.jdownloader.extensions.eventscripter.sandboxobjects;

import java.util.ArrayList;
import java.util.List;

import jd.controlling.linkcollector.LinkCollector.JobLinkCrawler;
import jd.controlling.linkcrawler.CrawledLink;

public class CrawlerJobHolderSandbox {

    protected final JobLinkCrawler linkCrawler;

    public CrawlerJobHolderSandbox() {
        this(null);
    }

    public CrawlerJobHolderSandbox(JobLinkCrawler linkCrawler) {
        this.linkCrawler = linkCrawler;
    }

    public boolean isRunning() {
        return linkCrawler != null && linkCrawler.isRunning();
    }

    public boolean isAborted() {
        return linkCrawler != null && linkCrawler.isAborted();
    }

    public CrawlerJobSandbox getJob() {
        if (linkCrawler != null) {
            return new CrawlerJobSandbox(linkCrawler.getJob());
        }
        return null;
    }

    protected CrawledLinkSandbox[] crawledLinks;

    public CrawledLinkSandbox[] getCrawledLinks() {
        final List<CrawledLink> lcCrawledLinks = linkCrawler == null ? new ArrayList<CrawledLink>(0) : linkCrawler.getCrawledLinks();
        if (crawledLinks == null || crawledLinks.length != lcCrawledLinks.size()) {
            crawledLinks = CrawledLinkSandbox.wrapSandBox(lcCrawledLinks);
        }
        return crawledLinks;
    }

    protected CrawledLinkSandbox[] filteredLinks;

    public CrawledLinkSandbox[] getFilteredLinks() {
        final List<CrawledLink> lcFilteredLinks = linkCrawler == null ? new ArrayList<CrawledLink>(0) : linkCrawler.getFilteredLinks();
        if (filteredLinks == null || filteredLinks.length != lcFilteredLinks.size()) {
            filteredLinks = CrawledLinkSandbox.wrapSandBox(lcFilteredLinks);
        }
        return filteredLinks;
    }

    protected CrawledLinkSandbox[] brokenLinks;

    public CrawledLinkSandbox[] getBrokenLinks() {
        final List<CrawledLink> lcBrokenLinks = linkCrawler == null ? new ArrayList<CrawledLink>(0) : linkCrawler.getBrokenLinks();
        if (brokenLinks == null || brokenLinks.length != lcBrokenLinks.size()) {
            brokenLinks = CrawledLinkSandbox.wrapSandBox(lcBrokenLinks);
        }
        return brokenLinks;
    }

    protected CrawledLinkSandbox[] unhandledLinks;

    public CrawledLinkSandbox[] getUnhandledLinks() {
        final List<CrawledLink> lcUnhandledLinks = linkCrawler == null ? new ArrayList<CrawledLink>(0) : linkCrawler.getUnhandledLinks();
        if (unhandledLinks == null || unhandledLinks.length != lcUnhandledLinks.size()) {
            unhandledLinks = CrawledLinkSandbox.wrapSandBox(lcUnhandledLinks);
        }
        return unhandledLinks;
    }
}
