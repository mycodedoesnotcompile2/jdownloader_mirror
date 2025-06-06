package org.jdownloader.extensions.eventscripter.sandboxobjects;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;

import javax.swing.Icon;

import jd.controlling.downloadcontroller.DownloadWatchDog;
import jd.controlling.downloadcontroller.SingleDownloadController;
import jd.controlling.packagecontroller.AbstractNode;
import jd.controlling.packagecontroller.PackageController;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.FilePackage;
import jd.plugins.PluginProgress;
import jd.plugins.download.HashInfo;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.JsonKeyValueStorage;
import org.appwork.storage.Storable;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.reflection.Clazz;
import org.jdownloader.api.downloads.v2.DownloadLinkAPIStorableV2;
import org.jdownloader.api.downloads.v2.DownloadsAPIV2Impl;
import org.jdownloader.controlling.Priority;
import org.jdownloader.extensions.eventscripter.ScriptAPI;
import org.jdownloader.extensions.extraction.Archive;
import org.jdownloader.extensions.extraction.ExtractionStatus;
import org.jdownloader.extensions.extraction.bindings.downloadlink.DownloadLinkArchiveFactory;
import org.jdownloader.extensions.extraction.contextmenu.downloadlist.ArchiveValidator;
import org.jdownloader.gui.views.components.packagetable.LinkTreeUtils;
import org.jdownloader.myjdownloader.client.json.AvailableLinkState;
import org.jdownloader.myjdownloader.client.json.JsonMap;
import org.jdownloader.plugins.ConditionalSkipReason;
import org.jdownloader.plugins.CustomConditionalSkipReasonMessageIcon;
import org.jdownloader.plugins.DownloadPluginProgress;
import org.jdownloader.plugins.FinalLinkState;
import org.jdownloader.plugins.SkipReason;
import org.jdownloader.plugins.TimeOutCondition;
import org.jdownloader.settings.UrlDisplayType;

@ScriptAPI(description = "The context download list link")
public class DownloadLinkSandBox {
    protected final DownloadLink                                            downloadLink;
    private final static WeakHashMap<DownloadLink, HashMap<String, Object>> SESSIONPROPERTIES = new WeakHashMap<DownloadLink, HashMap<String, Object>>();

    public DownloadLinkSandBox(DownloadLink downloadLink) {
        this.downloadLink = downloadLink;
    }

    public DownloadLinkSandBox() {
        this(null);
    }

    public String getLinkID() {
        if (downloadLink != null) {
            return downloadLink.getLinkID();
        } else {
            return null;
        }
    }

    public String getPriority() {
        if (downloadLink != null) {
            return downloadLink.getPriorityEnum().name();
        } else {
            return Priority.DEFAULT.name();
        }
    }

    public void setPriority(final String priority) {
        if (downloadLink != null) {
            try {
                downloadLink.setPriorityEnum(Priority.valueOf(priority));
            } catch (final Throwable e) {
                downloadLink.setPriorityEnum(Priority.DEFAULT);
            }
        }
    }

    /**
     * returns how long the downloadlink is in progress
     *
     * @return
     */
    public long getDownloadTime() {
        if (downloadLink != null) {
            final long ret = downloadLink.getView().getDownloadTime();
            final PluginProgress progress = downloadLink.getPluginProgress();
            if (progress instanceof DownloadPluginProgress) {
                final long ret2 = ret + ((DownloadPluginProgress) progress).getDuration();
                return ret2;
            }
            return ret;
        }
        return -1;
    }

    public String getContentURL() {
        if (downloadLink != null) {
            return LinkTreeUtils.getUrlByType(UrlDisplayType.CONTENT, downloadLink);
        } else {
            return null;
        }
    }

    public String getPluginURL() {
        if (downloadLink != null) {
            return downloadLink.getPluginPatternMatcher();
        } else {
            return null;
        }
    }

    public String getContainerURL() {
        if (downloadLink != null) {
            return LinkTreeUtils.getUrlByType(UrlDisplayType.CONTAINER, downloadLink);
        } else {
            return null;
        }
    }

    public String getOriginURL() {
        if (downloadLink != null) {
            return LinkTreeUtils.getUrlByType(UrlDisplayType.ORIGIN, downloadLink);
        } else {
            return null;
        }
    }

    public String getReferrerURL() {
        if (downloadLink != null) {
            return LinkTreeUtils.getUrlByType(UrlDisplayType.REFERRER, downloadLink);
        } else {
            return null;
        }
    }

    public String getHashInfo() {
        if (downloadLink != null) {
            final HashInfo hashInfo = downloadLink.getHashInfo();
            return hashInfo != null ? hashInfo.exportAsString() : null;
        } else {
            return null;
        }
    }

    public long getAddedDate() {
        if (downloadLink != null) {
            return downloadLink.getCreated();
        } else {
            return -1;
        }
    }

    public long getFinishedDate() {
        if (downloadLink != null) {
            return downloadLink.getFinishedDate();
        } else {
            return -1;
        }
    }

    public void abort() {
        if (downloadLink != null) {
            final List<DownloadLink> abort = new ArrayList<DownloadLink>();
            abort.add(downloadLink);
            DownloadWatchDog.getInstance().abort(abort);
        }
    }

    public boolean isAborting() {
        if (downloadLink != null) {
            final SingleDownloadController controller = downloadLink.getDownloadLinkController();
            return controller != null && controller.isAborting();
        } else {
            return false;
        }
    }

    public boolean isActive() {
        if (downloadLink != null) {
            final SingleDownloadController controller = downloadLink.getDownloadLinkController();
            return controller != null && controller.isActive();
        } else {
            return false;
        }
    }

    public Object getProperty(String key) {
        if (downloadLink != null) {
            return downloadLink.getProperty(key);
        } else {
            return null;
        }
    }

    public Object getTempProperty(String key) {
        if (downloadLink != null) {
            return downloadLink.getTempProperties().getProperty(key);
        } else {
            return null;
        }
    }

    public Object getSessionProperty(final String key) {
        if (downloadLink != null) {
            synchronized (SESSIONPROPERTIES) {
                final HashMap<String, Object> properties = SESSIONPROPERTIES.get(downloadLink);
                if (properties != null) {
                    return properties.get(key);
                }
            }
        }
        return null;
    }

    public void setSessionProperty(final String key, final Object value) {
        if (downloadLink != null) {
            if (value != null) {
                if (!canStore(value)) {
                    throw new WTFException("Type " + value.getClass().getSimpleName() + " is not supported");
                }
            }
            synchronized (SESSIONPROPERTIES) {
                HashMap<String, Object> properties = SESSIONPROPERTIES.get(downloadLink);
                if (properties == null) {
                    properties = new HashMap<String, Object>();
                    SESSIONPROPERTIES.put(downloadLink, properties);
                }
                properties.put(key, value);
            }
        }
    }

    public String getUUID() {
        if (downloadLink != null) {
            return downloadLink.getUniqueID().toString();
        } else {
            return null;
        }
    }

    public boolean remove() {
        if (downloadLink != null) {
            final FilePackage filePackage = downloadLink.getParentNode();
            if (filePackage != null && !FilePackage.isDefaultFilePackage(filePackage)) {
                final PackageController<FilePackage, DownloadLink> controller = filePackage.getControlledBy();
                if (controller != null) {
                    final ArrayList<DownloadLink> children = new ArrayList<DownloadLink>();
                    children.add(downloadLink);
                    controller.removeChildren(children);
                    return true;
                }
            }
        }
        return false;
    }

    public void setProperty(String key, Object value) {
        if (downloadLink != null) {
            if (value != null) {
                if (!canStore(value)) {
                    throw new WTFException("Type " + value.getClass().getSimpleName() + " is not supported");
                }
            }
            downloadLink.setProperty(key, value);
        }
    }

    public void setTempProperty(String key, Object value) {
        if (downloadLink != null) {
            if (value != null) {
                if (!canStore(value)) {
                    throw new WTFException("Type " + value.getClass().getSimpleName() + " is not supported");
                }
            }
            downloadLink.getTempProperties().setProperty(key, value);
        }
    }

    public Map<String, Object> getProperties() {
        if (downloadLink != null) {
            return downloadLink.getProperties();
        } else {
            return null;
        }
    }

    public Map<String, Object> getTempProperties() {
        if (downloadLink != null) {
            return downloadLink.getTempProperties().getProperties();
        } else {
            return null;
        }
    }

    private boolean canStore(final Object value) {
        return value == null || Clazz.isPrimitive(value.getClass()) || JsonKeyValueStorage.isWrapperType(value.getClass()) || value instanceof Storable;
    }

    public long getDownloadSessionDuration() {
        if (downloadLink != null) {
            final SingleDownloadController controller = downloadLink.getDownloadLinkController();
            if (controller != null) {
                return System.currentTimeMillis() - controller.getStartTimestamp();
            }
        }
        return -1;
    }

    public long getDownloadDuration() {
        if (downloadLink != null) {
            final PluginProgress progress = downloadLink.getPluginProgress();
            if (progress instanceof DownloadPluginProgress) {
                return ((DownloadPluginProgress) progress).getDuration();
            }
        }
        return -1;
    }

    public void reset() {
        if (downloadLink != null) {
            final ArrayList<DownloadLink> l = new ArrayList<DownloadLink>();
            l.add(downloadLink);
            DownloadWatchDog.getInstance().reset(l);
        }
    }

    public void resume() {
        if (downloadLink != null) {
            final ArrayList<DownloadLink> l = new ArrayList<DownloadLink>();
            l.add(downloadLink);
            DownloadWatchDog.getInstance().resume(l);
        }
    }

    public long getEta() {
        if (downloadLink == null) {
            return -1l;
        } else {
            final PluginProgress progress = downloadLink.getPluginProgress();
            if (progress != null) {
                final long ret = progress.getETA();
                return ret;
            } else {
                final ConditionalSkipReason conditionalSkipReason = downloadLink.getConditionalSkipReason();
                if (conditionalSkipReason != null && !conditionalSkipReason.isConditionReached()) {
                    if (conditionalSkipReason instanceof TimeOutCondition) {
                        final long ret = ((TimeOutCondition) conditionalSkipReason).getTimeOutLeft();
                        return ret;
                    }
                }
                return -1;
            }
        }
    }

    public String getArchiveID() {
        return downloadLink != null ? downloadLink.getArchiveID() : null;
    }

    public Boolean isPartOfAnArchive() {
        return downloadLink != null ? downloadLink.isPartOfAnArchive() : null;
    }

    public ArchiveSandbox getArchive() {
        if (downloadLink == null || ArchiveValidator.EXTENSION == null) {
            return null;
        }
        final Archive archive = ArchiveValidator.EXTENSION.getArchiveByFactory(new DownloadLinkArchiveFactory(downloadLink));
        if (archive != null) {
            return new ArchiveSandbox(archive);
        }
        final ArrayList<Object> list = new ArrayList<Object>();
        list.add(downloadLink);
        final List<Archive> archives = ArchiveValidator.getArchivesFromPackageChildren(list);
        return (archives == null || archives.size() == 0) ? null : new ArchiveSandbox(archives.get(0));
    }

    public String getComment() {
        if (downloadLink != null) {
            return downloadLink.getComment();
        } else {
            return null;
        }
    }

    public void setComment(String comment) {
        if (downloadLink != null) {
            downloadLink.setComment(comment);
        }
    }

    public void setEnabled(boolean b) {
        if (downloadLink != null) {
            downloadLink.setEnabled(b);
        }
    }

    public String getDownloadPath() {
        if (downloadLink == null) {
            switch (CrossSystem.getOSFamily()) {
            case WINDOWS:
                return "c:\\I am a dummy folder\\Test.txt";
            default:
                return "/mnt/Text.txt";
            }
        }
        return downloadLink.getFileOutput();
    }

    @ScriptAPI(description = "Sets a new filename", parameters = { "new Name" })
    public void setName(String name) {
        if (downloadLink != null) {
            DownloadWatchDog.getInstance().renameLink(downloadLink, name);
        }
    }

    public String getUrl() {
        if (downloadLink != null) {
            return downloadLink.getView().getDisplayUrl();
        } else {
            return null;
        }
    }

    public long getBytesLoaded() {
        if (downloadLink != null) {
            return downloadLink.getView().getBytesLoaded();
        } else {
            return -1;
        }
    }

    public long getBytesTotal() {
        if (downloadLink != null) {
            return downloadLink.getView().getBytesTotal();
        } else {
            return -1;
        }
    }

    public long getBytesTotalVerified() {
        if (downloadLink == null) {
            return -1;
        } else {
            return downloadLink.getView().getBytesTotalVerified();
        }
    }

    public String getName() {
        if (downloadLink == null) {
            return "Test.txt";
        } else {
            return downloadLink.getName();
        }
    }

    public String getForcedName() {
        if (downloadLink == null) {
            return "Test.txt";
        } else {
            return downloadLink.getForcedFileName();
        }
    }

    public String getFinalName() {
        if (downloadLink == null) {
            return "Test.txt";
        } else {
            return downloadLink.getFinalFileName();
        }
    }

    public long getSpeed() {
        if (downloadLink != null) {
            return downloadLink.getView().getSpeedBps();
        } else {
            return 0;
        }
    }

    public String getStatus() {
        final DownloadLinkAPIStorableV2 ret = _getStatus();
        if (ret != null) {
            return ret.getStatus();
        }
        return null;
    }

    private DownloadLinkAPIStorableV2 _getStatus() {
        if (downloadLink != null) {
            final DownloadLinkAPIStorableV2 ret = new DownloadLinkAPIStorableV2(downloadLink);
            DownloadsAPIV2Impl.setStatus(ret, downloadLink, new CustomConditionalSkipReasonMessageIcon() {
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
        final DownloadLinkAPIStorableV2 ret = _getStatus();
        if (ret != null) {
            return ret.getAdvancedStatus();
        }
        return null;
    }

    public String getHost() {
        if (downloadLink != null) {
            return downloadLink.getHost();
        } else {
            return null;
        }
    }

    public String getDownloadHost() {
        if (downloadLink != null) {
            return downloadLink.getServiceHost(true);
        } else {
            return null;
        }
    }

    public HTTPProxySandbox getDownloadProxy() {
        if (downloadLink != null) {
            final SingleDownloadController controller = downloadLink.getDownloadLinkController();
            if (controller != null) {
                return new HTTPProxySandbox(controller.getProxySelector());
            }
        }
        return null;
    }

    public boolean isSkipped() {
        return downloadLink != null && downloadLink.isSkipped();
    }

    public LinkInfoSandbox getLinkInfo() {
        if (downloadLink == null) {
            return null;
        } else {
            return new LinkInfoSandbox(downloadLink.getLinkInfo());
        }
    }

    public String getSkippedReason() {
        if (downloadLink != null) {
            final SkipReason skipped = downloadLink.getSkipReason();
            if (skipped != null) {
                return skipped.name();
            }
        }
        return null;
    }

    public ConditionalSkipReasonSandbox getConditionalSkipReason() {
        if (downloadLink != null) {
            final ConditionalSkipReason condition = downloadLink.getConditionalSkipReason();
            if (condition != null) {
                return new ConditionalSkipReasonSandbox(downloadLink, condition);
            }
        }
        return null;
    }

    public String getFinalLinkStatus() {
        if (downloadLink != null) {
            final FinalLinkState state = downloadLink.getFinalLinkState();
            if (state != null) {
                return state.name();
            }
        }
        return null;
    }

    public void setSkipped(boolean b) {
        if (downloadLink == null) {
            return;
        }
        if (b) {
            if (!downloadLink.isSkipped()) {
                // keep skipreason if a reason is set
                downloadLink.setSkipReason(SkipReason.MANUAL);
            }
        } else {
            final List<DownloadLink> unSkip = new ArrayList<DownloadLink>();
            unSkip.add(downloadLink);
            DownloadWatchDog.getInstance().unSkip(unSkip);
        }
    }

    public boolean isRunning() {
        return downloadLink != null && downloadLink.getDownloadLinkController() != null;
    }

    public boolean isEnabled() {
        return downloadLink != null && downloadLink.isEnabled();
    }

    @Override
    public int hashCode() {
        if (downloadLink != null) {
            return downloadLink.hashCode();
        } else {
            return super.hashCode();
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof DownloadLinkSandBox) {
            return ((DownloadLinkSandBox) obj).downloadLink == downloadLink;
        } else {
            return super.equals(obj);
        }
    }

    public String getAvailableState() {
        if (downloadLink != null) {
            final AvailableStatus availableStatus = downloadLink.getAvailableStatus();
            if (availableStatus != null) {
                switch (availableStatus) {
                case TRUE:
                    return AvailableLinkState.ONLINE.name();
                case FALSE:
                    return AvailableLinkState.OFFLINE.name();
                case UNCHECKED:
                    return AvailableLinkState.UNKNOWN.name();
                case UNCHECKABLE:
                    return AvailableLinkState.TEMP_UNKNOWN.name();
                default:
                    return AvailableLinkState.UNKNOWN.name();
                }
            }
        }
        return AvailableLinkState.UNKNOWN.name();
    }

    public FilePackageSandBox getPackage() {
        if (downloadLink == null) {
            return new FilePackageSandBox();
        } else {
            final FilePackage fp = downloadLink.getFilePackage();
            if (fp == null || FilePackage.isDefaultFilePackage(fp)) {
                return null;
            } else {
                return new FilePackageSandBox(fp);
            }
        }
    }

    public boolean isResumeable() {
        return downloadLink != null && downloadLink.isResumeable();
    }

    @Override
    public String toString() {
        return "DownloadLink Instance: " + getName();
    }

    public boolean isFinished() {
        return downloadLink != null && FinalLinkState.CheckFinished(downloadLink.getFinalLinkState());
    }

    public String getExtractionStatus() {
        if (downloadLink == null) {
            return null;
        } else {
            final ExtractionStatus ret = downloadLink.getExtractionStatus();
            return ret == null ? null : ret.name();
        }
    }
}
