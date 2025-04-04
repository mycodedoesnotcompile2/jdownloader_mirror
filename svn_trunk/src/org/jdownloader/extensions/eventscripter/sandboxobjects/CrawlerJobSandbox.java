package org.jdownloader.extensions.eventscripter.sandboxobjects;

import java.util.List;

import jd.controlling.linkcollector.LinkCollectingJob;
import jd.controlling.linkcollector.LinkOrigin;
import jd.controlling.linkcollector.LinkOriginDetails;

public class CrawlerJobSandbox {
    protected final LinkCollectingJob job;

    public CrawlerJobSandbox(LinkCollectingJob job) {
        this.job = job;
    }

    @Override
    public int hashCode() {
        if (job != null) {
            return job.hashCode();
        } else {
            return super.hashCode();
        }
    }

    public boolean isAssignJobID() {
        return job != null && job.isAssignJobID();
    }

    public void setAssignJobID(boolean b) {
        if (job != null) {
            job.setAssignJobID(b);
        }
    }

    public String getUUID() {
        if (job != null) {
            return job.getUniqueAlltimeID().toString();
        } else {
            return null;
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof CrawlerJobSandbox) {
            return ((CrawlerJobSandbox) obj).job == job;
        } else {
            return super.equals(obj);
        }
    }

    public CrawlerJobSandbox() {
        job = new LinkCollectingJob(LinkOriginDetails.getInstance(LinkOrigin.PASTE_LINKS_ACTION, "Test Job for EventScripter"));
        job.setText("http://jdownloader.org/_media/knowledge/wiki/development/open_preferences.png");
        job.setDeepAnalyse(false);
    }

    public String getOrigin() {
        return job.getOrigin() == null ? null : job.getOrigin().getOrigin().name();
    }

    public String getPassword() {
        return job.getCrawlerPassword();
    }

    public String getSourceUrl() {
        return job.getCustomSourceUrl();
    }

    public void setPassword(String text) {
        job.setCrawlerPassword(text);
    }

    public String getText() {
        return job.getText();
    }

    public boolean isDeepAnalysisEnabled() {
        return job.isDeepAnalyse();
    }

    public boolean isOverwritesPackagizerRulesEnabled() {
        return job.getPostPackagizerModifier().size() > 0;
    }

    public void setDeepAnalysisEnabled(boolean enabled) {
        job.setDeepAnalyse(enabled);
    }

    public List<String> getArchivPasswords() {
        return job.getArchivPasswords();
    }

    public void setText(String text) {
        job.setText(text);
    }
}
