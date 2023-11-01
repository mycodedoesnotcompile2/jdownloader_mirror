package org.jdownloader.myjdownloader.client.bindings;

public class LinkCrawlerJobQuery {
    private long[]  jobIds;
    private boolean collectorInfo;

    public long[] getJobIds() {
        return jobIds;
    }

    public void setJobIds(long[] jobIds) {
        this.jobIds = jobIds;
    }

    public boolean isCollectorInfo() {
        return collectorInfo;
    }

    public void setCollectorInfo(boolean collectorInfo) {
        this.collectorInfo = collectorInfo;
    }
}