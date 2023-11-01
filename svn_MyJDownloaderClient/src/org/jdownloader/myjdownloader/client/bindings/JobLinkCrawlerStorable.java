package org.jdownloader.myjdownloader.client.bindings;

import org.jdownloader.myjdownloader.client.json.AbstractJsonData;

public class JobLinkCrawlerStorable extends AbstractJsonData {
    
    private boolean crawling;
    private boolean checking;
    private long    jobId;
    private long crawlerId;
    private int     broken;
    private int     filtered;
    private int     unhandled;
    private int     crawled;
    
    public long getCrawlerId() {
        return crawlerId;
    }

    public void setCrawlerId(long crawlerId) {
        this.crawlerId = crawlerId;
    }
    
    public boolean isCrawling() {
        return this.crawling;
    }
    
    public void setCrawling(boolean crawling) {
        this.crawling = crawling;
    }
    
    public boolean isChecking() {
        return this.checking;
    }
    
    public void setChecking(boolean checking) {
        this.checking = checking;
    }
    
    public long getJobId() {
        return this.jobId;
    }
    
    public void setJobId(long jobId) {
        this.jobId = jobId;
    }
    
    public int getBroken() {
        return this.broken;
    }
    
    public void setBroken(int broken) {
        this.broken = broken;
    }
    
    public int getFiltered() {
        return this.filtered;
    }
    
    public void setFiltered(int filtered) {
        this.filtered = filtered;
    }
    
    public int getUnhandled() {
        return this.unhandled;
    }
    
    public void setUnhandled(int unhandled) {
        this.unhandled = unhandled;
    }
    
    public int getCrawled() {
        return this.crawled;
    }
    
    public void setCrawled(int crawled) {
        this.crawled = crawled;
    }
}
