package org.jdownloader.extensions.eventscripter.sandboxobjects;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;

import javax.swing.Icon;

import jd.controlling.linkcollector.LinkCollectingJob;
import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.CrawledPackage;
import jd.controlling.packagecontroller.AbstractNode;
import jd.controlling.packagecontroller.PackageController;
import jd.plugins.DownloadLink;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.JsonKeyValueStorage;
import org.appwork.storage.Storable;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.reflection.Clazz;
import org.jdownloader.api.linkcollector.v2.CrawledLinkAPIStorableV2;
import org.jdownloader.api.linkcollector.v2.LinkCollectorAPIImplV2;
import org.jdownloader.controlling.Priority;
import org.jdownloader.extensions.eventscripter.ScriptAPI;
import org.jdownloader.extensions.extraction.Archive;
import org.jdownloader.extensions.extraction.bindings.crawledlink.CrawledLinkFactory;
import org.jdownloader.extensions.extraction.contextmenu.downloadlist.ArchiveValidator;
import org.jdownloader.gui.views.components.packagetable.LinkTreeUtils;
import org.jdownloader.myjdownloader.client.json.AvailableLinkState;
import org.jdownloader.myjdownloader.client.json.JsonMap;
import org.jdownloader.plugins.ConditionalSkipReason;
import org.jdownloader.plugins.CustomConditionalSkipReasonMessageIcon;
import org.jdownloader.settings.UrlDisplayType;

@ScriptAPI(description = "The context linkgrabber list link")
public class CrawledLinkSandbox {
    protected final CrawledLink                                            link;
    private final static WeakHashMap<CrawledLink, HashMap<String, Object>> SESSIONPROPERTIES = new WeakHashMap<CrawledLink, HashMap<String, Object>>();

    public CrawledLinkSandbox(CrawledLink link) {
        this.link = link;
    }

    public String getAvailableState() {
        if (link != null) {
            return link.getLinkState().name();
        } else {
            return AvailableLinkState.UNKNOWN.name();
        }
    }

    public String getPriority() {
        if (link != null) {
            return link.getPriority().name();
        } else {
            return Priority.DEFAULT.name();
        }
    }

    public void setPriority(final String priority) {
        if (link != null) {
            try {
                link.setPriority(Priority.valueOf(priority));
            } catch (final Throwable e) {
                link.setPriority(Priority.DEFAULT);
            }
        }
    }

    private CrawledLinkAPIStorableV2 _getStatus() {
        if (link != null) {
            final CrawledLinkAPIStorableV2 ret = new CrawledLinkAPIStorableV2(link);
            LinkCollectorAPIImplV2.setStatus(ret, link, new CustomConditionalSkipReasonMessageIcon() {
                @Override
                public String getMessage(ConditionalSkipReason conditionalSkipReason, AbstractNode node) {
                    return conditionalSkipReason.getMessage(conditionalSkipReason, node);
                }

                @Override
                public Icon getIcon(ConditionalSkipReason conditionalSkipReason, AbstractNode node) {
                    return conditionalSkipReason.getIcon(conditionalSkipReason, node);
                }
            });
            return ret;
        } else {
            return null;
        }
    }

    public JsonMap getAdvancedStatus() {
        final CrawledLinkAPIStorableV2 ret = _getStatus();
        if (ret != null) {
            return ret.getAdvancedStatus();
        }
        return null;
    }

    public CrawlerJobSandbox getSourceJob() {
        if (link != null) {
            final LinkCollectingJob job = link.getSourceJob();
            if (job != null) {
                return new CrawlerJobSandbox(job);
            } else {
                return null;
            }
        } else {
            return null;
        }
    }

    public String getArchiveID() {
        return link != null ? link.getArchiveID() : null;
    }

    public Boolean isPartOfAnArchive() {
        if (link != null) {
            final DownloadLink downloadLink = link.getDownloadLink();
            if (downloadLink != null) {
                return downloadLink.isPartOfAnArchive();
            }
        }
        return null;
    }

    public long getAddedDate() {
        if (link != null) {
            return link.getCreated();
        } else {
            return -1;
        }
    }

    public CrawledLinkSandbox() {
        this(null);
    }

    @Override
    public int hashCode() {
        if (link != null) {
            return link.hashCode();
        } else {
            return super.hashCode();
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof CrawledLinkSandbox) {
            return ((CrawledLinkSandbox) obj).link == link;
        } else {
            return super.equals(obj);
        }
    }

    public Object getProperty(String key) {
        if (link != null) {
            final DownloadLink downloadLink = link.getDownloadLink();
            if (downloadLink != null) {
                return downloadLink.getProperty(key);
            }
        }
        return null;
    }

    public Object getSessionProperty(final String key) {
        if (this.link != null) {
            synchronized (SESSIONPROPERTIES) {
                final HashMap<String, Object> properties = SESSIONPROPERTIES.get(this.link);
                if (properties != null) {
                    return properties.get(key);
                }
            }
        }
        return null;
    }

    public String getContentURL() {
        if (link != null) {
            return LinkTreeUtils.getUrlByType(UrlDisplayType.CONTENT, link);
        } else {
            return null;
        }
    }

    public String getContainerURL() {
        if (link != null) {
            return LinkTreeUtils.getUrlByType(UrlDisplayType.CONTAINER, link);
        } else {
            return null;
        }
    }

    public String getPluginURL() {
        if (link != null) {
            final DownloadLink downloadLink = link.getDownloadLink();
            if (downloadLink != null) {
                return downloadLink.getPluginPatternMatcher();
            }
        }
        return null;
    }

    private DownloadLinkSandBox downloadLinkSandbox = null;

    public DownloadLinkSandBox getDownloadLink() {
        if (downloadLinkSandbox != null) {
            return downloadLinkSandbox;
        }
        if (link != null) {
            final DownloadLink downloadLink = link.getDownloadLink();
            if (downloadLink != null) {
                downloadLinkSandbox = new DownloadLinkSandBox(downloadLink);
                return downloadLinkSandbox;
            } else {
                return null;
            }
        } else {
            return null;
        }
    }

    public String getDownloadHost() {
        if (link != null) {
            final DownloadLink downloadLink = link.getDownloadLink();
            return downloadLink != null ? downloadLink.getServiceHost(true) : link.getHost();
        } else {
            return null;
        }
    }

    public String getOriginURL() {
        if (link != null) {
            return LinkTreeUtils.getUrlByType(UrlDisplayType.ORIGIN, link);
        } else {
            return null;
        }
    }

    public String getReferrerURL() {
        if (link != null) {
            return LinkTreeUtils.getUrlByType(UrlDisplayType.REFERRER, link);
        } else {
            return null;
        }
    }

    public boolean remove() {
        if (this.link != null) {
            final CrawledPackage pkg = this.link.getParentNode();
            if (pkg != null) {
                final PackageController<CrawledPackage, CrawledLink> controller = pkg.getControlledBy();
                if (controller != null) {
                    final ArrayList<CrawledLink> children = new ArrayList<CrawledLink>();
                    children.add(link);
                    controller.removeChildren(children);
                    return true;
                }
            }
        }
        return false;
    }

    public void setSessionProperty(final String key, final Object value) {
        if (link != null) {
            if (value != null) {
                if (!canStore(value)) {
                    throw new WTFException("Type " + value.getClass().getSimpleName() + " is not supported");
                }
            }
            synchronized (SESSIONPROPERTIES) {
                HashMap<String, Object> properties = SESSIONPROPERTIES.get(link);
                if (properties == null) {
                    properties = new HashMap<String, Object>();
                    SESSIONPROPERTIES.put(link, properties);
                }
                properties.put(key, value);
            }
        }
    }

    public String getUUID() {
        if (link != null) {
            return link.getUniqueID().toString();
        } else {
            return null;
        }
    }

    public String getLinkID() {
        final DownloadLinkSandBox downloadLinkSandBox = getDownloadLink();
        return downloadLinkSandBox != null ? downloadLinkSandBox.getLinkID() : null;
    }

    public String getHashInfo() {
        final DownloadLinkSandBox downloadLinkSandBox = getDownloadLink();
        return downloadLinkSandBox != null ? downloadLinkSandBox.getHashInfo() : null;
    }

    public void setProperty(String key, Object value) {
        final DownloadLinkSandBox downloadLinkSandBox = getDownloadLink();
        if (downloadLinkSandBox != null) {
            downloadLinkSandBox.setProperty(key, value);
        }
    }

    public Map<String, Object> getProperties() {
        final DownloadLinkSandBox downloadLinkSandBox = getDownloadLink();
        return downloadLinkSandBox != null ? downloadLinkSandBox.getProperties() : null;
    }

    private boolean canStore(final Object value) {
        return value == null || Clazz.isPrimitive(value.getClass()) || JsonKeyValueStorage.isWrapperType(value.getClass()) || value instanceof Storable;
    }

    public ArchiveSandbox getArchive() {
        if (link == null || ArchiveValidator.EXTENSION == null) {
            return null;
        }
        final Archive archive = ArchiveValidator.EXTENSION.getArchiveByFactory(new CrawledLinkFactory(link));
        if (archive != null) {
            return new ArchiveSandbox(archive);
        }
        final ArrayList<Object> list = new ArrayList<Object>();
        list.add(link);
        final List<Archive> archives = ArchiveValidator.getArchivesFromPackageChildren(list);
        return (archives == null || archives.size() == 0) ? null : new ArchiveSandbox(archives.get(0));
    }

    public String getComment() {
        if (link != null) {
            return link.getComment();
        } else {
            return null;
        }
    }

    public void setComment(String comment) {
        if (link != null) {
            link.setComment(comment);
        }
    }

    public void setEnabled(boolean b) {
        if (link != null) {
            link.setEnabled(b);
        }
    }

    public String getDownloadPath() {
        if (link == null) {
            switch (CrossSystem.getOSFamily()) {
            case WINDOWS:
                return "c:\\I am a dummy folder\\Test.txt";
            default:
                return "/mnt/Text.txt";
            }
        }
        final String downloadFolder = getPackage().getDownloadFolder();
        if (link.isNameSet()) {
            return new File(downloadFolder, link.getName()).getAbsolutePath();
        } else {
            return link.getDownloadLink().getFileOutput(downloadFolder, false, false);
        }
    }

    @ScriptAPI(description = "Sets a new filename", parameters = { "new Name" })
    public void setName(String name) {
        if (link != null) {
            link.setName(name);
        }
    }

    public String getUrl() {
        if (link != null) {
            return link.getURL();
        } else {
            return null;
        }
    }

    public long getBytesTotal() {
        if (link != null) {
            return link.getSize();
        } else {
            return -1;
        }
    }

    public long getBytesTotalVerified() {
        final DownloadLinkSandBox downloadLinkSandBox = getDownloadLink();
        return downloadLinkSandBox != null ? downloadLinkSandBox.getBytesTotalVerified() : -1;
    }

    public String getName() {
        if (link == null) {
            return "Test.txt";
        } else {
            return link.getName();
        }
    }

    public String getForcedName() {
        if (isNameSet()) {
            return getName();
        } else {
            final DownloadLinkSandBox downloadLinkSandBox = getDownloadLink();
            return downloadLinkSandBox != null ? downloadLinkSandBox.getForcedName() : null;
        }
    }

    public String getFinalName() {
        final DownloadLinkSandBox downloadLinkSandBox = getDownloadLink();
        return downloadLinkSandBox != null ? downloadLinkSandBox.getFinalName() : null;
    }

    public boolean isNameSet() {
        return link != null && link.isNameSet();
    }

    public CrawledPackageSandbox getPackage() {
        if (link == null) {
            return new CrawledPackageSandbox();
        } else {
            final CrawledPackage pkg = link.getParentNode();
            if (pkg != null) {
                return new CrawledPackageSandbox(pkg);
            } else {
                return null;
            }
        }
    }

    public LinkInfoSandbox getLinkInfo() {
        if (link == null) {
            return null;
        } else {
            return new LinkInfoSandbox(link.getLinkInfo());
        }
    }

    public String getHost() {
        if (link != null) {
            return link.getHost();
        } else {
            return null;
        }
    }

    public boolean isEnabled() {
        return link != null && link.isEnabled();
    }

    @Override
    public String toString() {
        return "CrawledLink Instance: " + getName();
    }
}
