package jd.controlling.linkcollector;

import java.util.Collections;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicReference;

import jd.controlling.linkcrawler.CrawledLinkModifier;

import org.appwork.utils.StringUtils;
import org.jdownloader.controlling.UniqueAlltimeID;

public class LinkCollectingJob {
    public final static class UniqueAlltimeLongID extends UniqueAlltimeID {

        private UniqueAlltimeLongID() {
            super();
        }

        private Long idLong = null;

        public void setID(long ID) {
            super.setID(ID);
            this.idLong = Long.valueOf(ID);
        }

        public Long getIDLong() {
            final Long ret = idLong;
            if (ret == null) {
                setID(getID());
                return getIDLong();
            } else {
                return ret;
            }
        }

    }

    private String                    customSourceUrl;
    private final UniqueAlltimeLongID uniqueAlltimeID = new UniqueAlltimeLongID();
    private boolean                   assignJobID     = false;

    public boolean isAssignJobID() {
        return assignJobID;
    }

    public void setAssignJobID(boolean assignJobID) {
        this.assignJobID = assignJobID;
    }

    public UniqueAlltimeLongID getUniqueAlltimeID() {
        return uniqueAlltimeID;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append("ID:" + getUniqueAlltimeID());
        sb.append("|Origin:" + getOrigin().getOrigin());
        return sb.toString();
    }

    private final CopyOnWriteArrayList<CrawledLinkModifier> prePackagizerModifier = new CopyOnWriteArrayList<CrawledLinkModifier>();

    public List<CrawledLinkModifier> getPrePackagizerModifier() {
        return prePackagizerModifier;
    }

    public boolean addPrePackagizerModifier(CrawledLinkModifier modifier) {
        return modifier != null && prePackagizerModifier.addIfAbsent(modifier);
    }

    public boolean removePrePackagizerModifier(CrawledLinkModifier modifier) {
        return modifier != null && prePackagizerModifier.remove(modifier);
    }

    private final CopyOnWriteArrayList<CrawledLinkModifier> postPackagizerModifier = new CopyOnWriteArrayList<CrawledLinkModifier>();

    public List<CrawledLinkModifier> getPostPackagizerModifier() {
        return postPackagizerModifier;
    }

    public boolean addPostPackagizerModifier(CrawledLinkModifier modifier) {
        return modifier != null && postPackagizerModifier.addIfAbsent(modifier);
    }

    public boolean removePostPackagizerModifier(CrawledLinkModifier modifier) {
        return modifier != null && postPackagizerModifier.remove(modifier);
    }

    private boolean      deepAnalyse;
    private String       crawlerPassword = null;
    private List<String> archivPasswords = null;

    public String getCrawlerPassword() {
        return crawlerPassword;
    }

    public void setCrawlerPassword(String crawlerPassword) {
        this.crawlerPassword = crawlerPassword;
    }

    public List<String> getArchivPasswords() {
        return archivPasswords;
    }

    public void setArchivPasswords(List<String> archivPasswords) {
        if (archivPasswords == null || archivPasswords.size() == 0) {
            this.archivPasswords = null;
        } else {
            this.archivPasswords = Collections.unmodifiableList(archivPasswords);
        }
    }

    public boolean isDeepAnalyse() {
        return deepAnalyse;
    }

    public void setDeepAnalyse(boolean deepAnalyse) {
        this.deepAnalyse = deepAnalyse;
    }

    public String getCustomSourceUrl() {
        return customSourceUrl;
    }

    public void setCustomSourceUrl(String customSource) {
        if (StringUtils.startsWithCaseInsensitive(customSource, "http://") || StringUtils.startsWithCaseInsensitive(customSource, "https://") || StringUtils.startsWithCaseInsensitive(customSource, "ftp://")) {
            this.customSourceUrl = customSource;
        }
    }

    public LinkCollectingJob(LinkOriginDetails origin) {
        this(origin, "DUMMY");
    }

    public LinkCollectingJob(LinkOriginDetails origin, String jobContent) {
        if (origin == null) {
            throw new IllegalArgumentException("origin is null");
        }
        this.jobContent.set(jobContent);
        this.origin = origin;
    }

    private final AtomicReference<CharSequence> jobContent = new AtomicReference<CharSequence>();

    public String getText() {
        final CharSequence ret = jobContent.get();
        if (ret == null) {
            return null;
        }
        return ret.toString();
    }

    protected String consumeText() {
        while (true) {
            final CharSequence content = jobContent.get();
            if (content == null) {
                // already consumed
                return null;
            } else if (content instanceof String) {
                // not yet consumed
                if (jobContent.compareAndSet(content, new StringBuffer(content))) {
                    // change to consumed
                    return (String) content;
                }
            } else if (content instanceof CharSequence) {
                // CharSequence -> already consumed
                return null;
            }
        }
    }

    public boolean isConsumed() {
        return !(jobContent.get() instanceof String);
    }

    public boolean setText(String text) {
        while (true) {
            final CharSequence content = jobContent.get();
            if (content == null) {
                // already consumed -> no longer can change it
                return false;
            } else if (content instanceof String) {
                // not yet consumed
                if (jobContent.compareAndSet(content, text)) {
                    // we could change
                    return true;
                }
            } else if (content instanceof CharSequence) {
                // already consumed -> no longer can change it
                if (text == null && jobContent.compareAndSet(content, null)) {
                    // except to null
                    return true;
                }
                return false;
            }
        }
    }

    private final LinkOriginDetails origin;

    public LinkOriginDetails getOrigin() {
        return origin;
    }
}
