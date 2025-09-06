package jd.plugins;

import java.util.LinkedHashSet;

import jd.controlling.linkcollector.LinkOrigin;
import jd.controlling.linkcollector.LinkOriginDetails;
import jd.controlling.linkcrawler.ArchiveInfoStorable;
import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.LinkCrawler;

import org.appwork.storage.Storable;
import org.appwork.storage.StorableAllowPrivateAccessModifier;
import org.appwork.storage.TypeRef;
import org.jdownloader.extensions.extraction.BooleanStatus;

public class CrawledLinkStorable implements Storable {
    public static final TypeRef<CrawledLinkStorable> TYPEREF = new TypeRef<CrawledLinkStorable>() {
    };
    private CrawledLink                              link;
    private String                                   id      = null;
    private long                                     UID     = -1;

    public String getID() {
        return id;
    }

    public void setID(String id) {
        this.id = id;
    }

    public String getName() {
        return link._getName();
    }

    public void setName(String name) {
        link.setName(name);
    }

    public boolean isEnabled() {
        return link.isEnabled();
    }

    public void setEnabled(boolean enabled) {
        link.setEnabled(enabled);
    }

    @SuppressWarnings("unused")
    @StorableAllowPrivateAccessModifier
    private CrawledLinkStorable(/* Storable */) {
        this.link = new CrawledLink((String) null);
    }

    public CrawledLinkStorable(CrawledLink link) {
        this.link = link;
    }

    public void setSourceUrls(String[] urls) {
        if (urls != null) {
            final LinkedHashSet<String> cleanURLs = new LinkedHashSet<String>();
            for (final String url : urls) {
                final String cleanURL = LinkCrawler.cleanURL(url);
                if (cleanURL != null) {
                    if (cleanURLs.contains(cleanURL)) {
                        continue;
                    }
                    if (cleanURLs.contains(cleanURL + "/")) {
                        continue;
                    }
                    if (cleanURL.endsWith("/") && cleanURLs.contains(cleanURL.substring(0, cleanURL.length() - 1))) {
                        continue;
                    }
                    cleanURLs.add(cleanURL);
                }
            }
            urls = cleanURLs.toArray(new String[cleanURLs.size()]);
        }
        link.setSourceUrls(urls);
    }

    public String[] getSourceUrls() {
        return link.getSourceUrls();
    }

    public static class LinkOriginStorable implements Storable {
        public LinkOriginStorable(/* Storable */) {
        }

        public LinkOriginStorable(LinkOriginDetails origin) {
            this.id = origin.getOrigin().name();
            this.details = origin.getDetails();
        }

        private String id;
        private String details;

        public String getId() {
            return id;
        }

        public void setId(String id) {
            this.id = id;
        }

        public String getDetails() {
            return details;
        }

        public void setDetails(String details) {
            this.details = details;
        }
    }

    public LinkOriginStorable getOriginDetails() {
        final LinkOriginDetails origin = link.getOrigin();
        if (origin == null) {
            return null;
        }
        return new LinkOriginStorable(origin);
    }

    public void setOriginDetails(LinkOriginStorable origin) {
        if (origin != null) {
            try {
                final LinkOrigin enu = LinkOrigin.valueOf(origin.id);
                link.setOrigin(LinkOriginDetails.getInstance(enu, origin.details));
            } catch (Throwable e) {
                e.printStackTrace();
            }
        }
    }

    public long getUID() {
        final DownloadLink dll = _getDownloadLink();
        if (dll != null) {
            return dll.getUniqueID().getID();
        } else {
            return link.getUniqueID().getID();
        }
    }

    public void setUID(long id) {
        this.UID = id;
    }

    public DownloadLinkStorable getDownloadLink() {
        return new DownloadLinkStorable(_getDownloadLink());
    }

    public void setDownloadLink(DownloadLinkStorable link) {
        this.link.setDownloadLink(link._getDownloadLink());
    }

    public DownloadLink _getDownloadLink() {
        return link.getDownloadLink();
    }

    /**
     * @param created
     *            the created to set
     */
    public void setCreated(long created) {
        link.setCreated(created);
    }

    /**
     * @return the created
     */
    public long getCreated() {
        return link.getCreated();
    }

    public CrawledLink _getCrawledLink() {
        final DownloadLink dll = _getDownloadLink();
        if (dll != null) {
            if (UID != -1) {
                dll.getUniqueID().setID(UID);
            }
        }
        if (UID != -1) {
            link.getUniqueID().setID(UID);
        }
        return _finalizeDeserialization(link, dll);
    }

    public CrawledLink _finalizeDeserialization(CrawledLink crawledLink, DownloadLink downloadLink) {
        return crawledLink;
    }

    public ArchiveInfoStorable getArchiveInfo() {
        if (link.hasArchiveInfo()) {
            return new ArchiveInfoStorable(link.getArchiveInfo());
        } else {
            return null;
        }
    }

    public void setArchiveInfo(ArchiveInfoStorable info) {
        if (info != null) {
            boolean setArchiveInfo = !BooleanStatus.UNSET.equals(info.getAutoExtract());
            if (setArchiveInfo == false) {
                setArchiveInfo = info.getExtractionPasswords() != null && info.getExtractionPasswords().size() > 0;
            }
            if (setArchiveInfo) {
                link.setArchiveInfo(info._getArchiveInfo());
            }
        }
    }
}
