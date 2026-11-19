//    jDownloader - Downloadmanager
//    Copyright (C) 2008  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;

import org.appwork.storage.config.JsonConfig;
import org.appwork.swing.exttable.columns.ExtProgressColumn;
import org.appwork.utils.ModifyLock;
import org.appwork.utils.StringUtils;
import org.appwork.utils.logging2.extmanager.LoggerFactory;
import org.appwork.utils.os.CrossSystem;
import org.jdownloader.DomainInfo;
import org.jdownloader.controlling.Priority;
import org.jdownloader.controlling.UniqueAlltimeID;
import org.jdownloader.settings.GeneralSettings;
import org.jdownloader.translate._JDT;

import jd.config.Property;
import jd.controlling.linkcrawler.LinkCrawler;
import jd.controlling.packagecontroller.AbstractNode;
import jd.controlling.packagecontroller.AbstractNodeNotifier;
import jd.controlling.packagecontroller.AbstractPackageChildrenNode;
import jd.controlling.packagecontroller.AbstractPackageNode;
import jd.controlling.packagecontroller.PackageController;
import jd.controlling.packagecontroller.PackageControllerComparator;

/**
 * This class represents a package of DownloadLinks
 *
 * @author JD-Team
 */
public class FilePackage extends Property implements Serializable, AbstractPackageNode<DownloadLink, FilePackage> {
    private static final GeneralSettings GENERALSETTINGS  = JsonConfig.create(GeneralSettings.class);
    private static final long            serialVersionUID = -8859842964299890820L;
    private String                       downloadDirectory;
    private ArrayList<DownloadLink>      downloadLinkList;
    private static FilePackage           FP               = null;
    static {
        /*
         * Default package, see getDefaultFilePackage(): Its view reports an empty and disabled package and all modifications of its
         * children are ignored.
         */
        FP = new FilePackage() {
            final private FilePackageView view             = new FilePackageView(this) {
                                                               @Override
                                                               public boolean isEnabled() {
                                                                   return false;
                                                               }

                                                               @Override
                                                               public int size() {
                                                                   return 0;
                                                               }

                                                               @Override
                                                               public PluginStateCollection getPluginStates() {
                                                                   return super.getPluginStates();
                                                               }

                                                               @Override
                                                               public DomainInfo[] getDomainInfos() {
                                                                   return new DomainInfo[0];
                                                               }

                                                               @Override
                                                               public long getSize() {
                                                                   return -1l;
                                                               }

                                                               @Override
                                                               public long getDone() {
                                                                   return -1;
                                                               }

                                                               @Override
                                                               public long getETA() {
                                                                   return -1;
                                                               }

                                                               @Override
                                                               public boolean isFinished() {
                                                                   return false;
                                                               }

                                                               @Override
                                                               public int getDisabledCount() {
                                                                   return 0;
                                                               }

                                                               @Override
                                                               public long getFinishedDate() {
                                                                   return -1l;
                                                               }

                                                               @Override
                                                               public FilePackageView aggregate() {
                                                                   return this;
                                                               }

                                                               @Override
                                                               public String getCommonSourceUrl() {
                                                                   return null;
                                                               }

                                                               @Override
                                                               public int getOfflineCount() {
                                                                   return 0;
                                                               }

                                                               @Override
                                                               public int getOnlineCount() {
                                                                   return 0;
                                                               }

                                                               @Override
                                                               public void requestUpdate() {
                                                               }

                                                               @Override
                                                               public boolean updateRequired() {
                                                                   return false;
                                                               }

                                                               @Override
                                                               public ChildrenAvailablility getAvailability() {
                                                                   return ChildrenAvailablility.UNKNOWN;
                                                               }

                                                               @Override
                                                               public String getMessage(Object requestor) {
                                                                   return null;
                                                               }

                                                               public PackageController<FilePackage, DownloadLink> getControlledby() {
                                                                   return null;
                                                               }
                                                           };
            private static final long     serialVersionUID = 1L;

            @Override
            public void _add(DownloadLink... links) {
            }

            @Override
            public FilePackageView getView() {
                return view;
            }

            @Override
            public void remove(DownloadLink... links) {
            }

            @Override
            public void setControlledBy(PackageController<FilePackage, DownloadLink> controller) {
            }
        };
        FP.setName(_JDT.T.controller_packages_defaultname());
        FP.downloadLinkList = new ArrayList<DownloadLink>() {
            /**
             *
             */
            private static final long serialVersionUID = 1L;

            @Override
            public boolean isEmpty() {
                return true;
            }

            @Override
            public DownloadLink set(int index, DownloadLink element) {
                return null;
            }

            @Override
            public boolean add(DownloadLink e) {
                return true;
            }

            @Override
            public void add(int index, DownloadLink element) {
            }

            @Override
            public boolean addAll(Collection<? extends DownloadLink> c) {
                return false;
            }

            @Override
            public boolean addAll(int index, Collection<? extends DownloadLink> c) {
                return false;
            }

            public ArrayList<DownloadLink> getDownloadLinkList() {
                return new ArrayList<DownloadLink>();
            }
        };
    }

    /**
     * returns defaultFilePackage, used only to avoid NullPointerExceptions, you cannot add/remove links in it
     *
     * @return
     */
    public static FilePackage getDefaultFilePackage() {
        return FP;
    }

    public static boolean isDefaultFilePackage(AbstractPackageNode<?, ?> fp) {
        return FP == fp;
    }

    private String                                                                 name                    = null;
    private long                                                                   created                 = -1l;
    private long                                                                   modified                = -1l;
    private Boolean                                                                isExpanded              = null;
    private PackageController<FilePackage, DownloadLink>                           controlledby            = null;
    public static final String                                                     PROPERTY_EXPANDED       = "EXPANDED";
    private static final String                                                    PROPERTY_COMMENT        = "COMMENT";
    private static final String                                                    PROPERTY_PRIORITY       = "PRIORITY";
    public static final String                                                     PROPERTY_PACKAGE_KEY    = "PACKAGE_KEY";
    private final static AtomicReferenceFieldUpdater<FilePackage, UniqueAlltimeID> UNIQUEALLTIMEID_UPDATER = AtomicReferenceFieldUpdater.newUpdater(FilePackage.class, UniqueAlltimeID.class, "uniqueID");
    private volatile UniqueAlltimeID                                               uniqueID                = null;

    /**
     * @return the uniqueID
     */
    public UniqueAlltimeID getUniqueID() {
        while (true) {
            UniqueAlltimeID ret = uniqueID;
            if (ret != null) {
                return ret;
            }
            if (UNIQUEALLTIMEID_UPDATER.compareAndSet(this, null, ret = new UniqueAlltimeID())) {
                return ret;
            }
        }
    }

    /* Sorter for the children of this package, null if the children are not sorted automatically (see constructor) */
    private PackageControllerComparator<AbstractNode> sorter;

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        return getUniqueID().hashCode();
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        } else if (obj == this) {
            return true;
        } else if (!(obj instanceof FilePackage)) {
            return false;
        } else if (this.uniqueID != null) {
            /* Packages are equal if they share the same UniqueAlltimeID instance */
            return ((FilePackage) obj).uniqueID == this.uniqueID;
        } else {
            /* The ID of this package was never requested, so it can only be equal to itself (checked above) */
            return false;
        }
    }

    /**
     * return a new FilePackage instance
     *
     * @return
     */
    public static FilePackage getInstance() {
        return new FilePackage();
    }

    /**
     * private constructor for FilePackage, sets created/modified timestamp, downloadDirectory, default name and sorter
     */
    private FilePackage() {
        downloadDirectory = GENERALSETTINGS.getDefaultDownloadFolder();
        created = System.currentTimeMillis();
        modified = created;
        /* till refactoring is complete */
        this.downloadLinkList = new ArrayList<DownloadLink>();
        setName(null);
        if (GENERALSETTINGS.isAutoSortChildrenEnabled()) {
            sorter = PackageControllerComparator.SORTER_ASC;
        }
    }

    /**
     * return this FilePackage created timestamp
     *
     * @return
     */
    public long getCreated() {
        return created;
    }

    /**
     * set this FilePackage created timestamp
     *
     * @param created
     */
    public void setCreated(long created) {
        this.created = created;
    }

    /**
     * add given DownloadLink to this FilePackage. delegates the call to the PackageController if it is set
     *
     * @param link
     */
    public void add(DownloadLink link) {
        _add(link);
    }

    /**
     * add the given DownloadLinks to this FilePackage. delegates the call to the PackageController if it is set
     *
     * @param links
     */
    public void addLinks(ArrayList<DownloadLink> links) {
        if (links == null || links.size() == 0) {
            return;
        }
        _add(links.toArray(new DownloadLink[links.size()]));
    }

    /**
     * add the given DownloadLinks to this FilePackage. delegates the call to the PackageController if it is set
     *
     * @param links
     */
    public void _add(DownloadLink... links) {
        if (links == null || links.length == 0) {
            return;
        }
        if (this.controlledby != null) {
            /* The controller has to do the modification, it also takes care of its own maps and events */
            this.controlledby.moveOrAddAt(this, Arrays.asList(links), -1);
            return;
        }
        /* Not controlled by a PackageController: modify the children directly */
        getModifyLock().writeLock();
        boolean notifyStructureChanges = false;
        try {
            for (DownloadLink link : links) {
                /* Links that are already part of this package are ignored */
                if (!this.downloadLinkList.contains(link)) {
                    notifyStructureChanges = true;
                    link.setParentNode(this);
                    this.downloadLinkList.add(link);
                }
            }
        } finally {
            getModifyLock().writeUnlock();
            /* Notify after the write lock has been released */
            if (notifyStructureChanges) {
                nodeUpdated(this, NOTIFY.STRUCTURE_CHANGE, null);
            }
        }
    }

    @Override
    public void setCurrentSorter(PackageControllerComparator<AbstractNode> comparator) {
        sorter = comparator;
    }

    /**
     * return the download folder of this FilePackage
     *
     * @return
     */
    public String getDownloadDirectory() {
        return downloadDirectory;
    }

    /**
     * return the name of this FilePackage
     *
     * @return
     */
    public String getName() {
        return name;
    }

    /**
     * remove the given DownloadLinks from this FilePackage. delegates the call to the PackageController if it is set
     *
     * @param links
     */
    public void remove(DownloadLink... links) {
        if (links == null || links.length == 0) {
            return;
        }
        if (this.controlledby != null) {
            /* The controller has to do the modification, it also takes care of its own maps and events */
            this.controlledby.removeChildren(this, Arrays.asList(links), true);
            return;
        }
        /* Not controlled by a PackageController: modify the children directly */
        getModifyLock().writeLock();
        boolean notifyStructureChanges = false;
        try {
            for (DownloadLink link : links) {
                if (this.downloadLinkList.remove(link)) {
                    notifyStructureChanges = true;
                    /* Reset the parent node of the link, but only if it still belongs to this FilePackage */
                    if (link.getFilePackage() == this) {
                        link.setParentNode(null);
                    }
                }
            }
        } finally {
            getModifyLock().writeUnlock();
            if (notifyStructureChanges) {
                nodeUpdated(this, NOTIFY.STRUCTURE_CHANGE, null);
            }
        }
    }

    public void setComment(String comment) {
        final boolean changed;
        /* An empty comment removes the property completely */
        if (comment == null || comment.length() == 0) {
            changed = this.setProperty(PROPERTY_COMMENT, Property.NULL);
        } else {
            changed = this.setProperty(PROPERTY_COMMENT, comment);
        }
        if (changed && hasNotificationListener()) {
            nodeUpdated(this, AbstractNodeNotifier.NOTIFY.PROPERTY_CHANGE, new FilePackageProperty(this, FilePackageProperty.Property.COMMENT, getComment()));
        }
    }

    public String getComment() {
        return this.getStringProperty(PROPERTY_COMMENT, null);
    }

    /**
     * set the download folder for this FilePackage
     *
     * @param folder
     */
    public void setDownloadDirectory(String folder) {
        if (StringUtils.isEmpty(folder)) {
            /* No folder given: use the default download folder */
            folder = GENERALSETTINGS.getDefaultDownloadFolder();
        } else if (!CrossSystem.isAbsolutePath(folder)) {
            /* A FilePackage always contains the full absolute path, relative paths are rejected */
            LoggerFactory.I().getDefaultLogger().severe("FilePackage: setDownloadDirectory only allows absolute paths(" + folder + ")! Using default one!");
            folder = GENERALSETTINGS.getDefaultDownloadFolder();
        }
        final String lFolder = getDownloadDirectory();
        if (lFolder != null && lFolder.equals(folder)) {
            /* Nothing changed */
            return;
        }
        downloadDirectory = dedupeString(folder.trim());
        if (hasNotificationListener()) {
            nodeUpdated(this, AbstractNodeNotifier.NOTIFY.PROPERTY_CHANGE, new FilePackageProperty(this, FilePackageProperty.Property.FOLDER, getDownloadDirectory()));
        }
    }

    /**
     * set the name of this FilePackage
     *
     * @param name
     */
    public void setName(String name) {
        final String lName = getName();
        if (StringUtils.isEmpty(name)) {
            /* Packages always have a name, use the default one if none is given */
            name = _JDT.T.controller_packages_defaultname();
        }
        if (lName != null && lName.equals(name)) {
            /* Nothing changed */
            return;
        }
        this.name = name.trim();
        if (hasNotificationListener()) {
            nodeUpdated(this, AbstractNodeNotifier.NOTIFY.PROPERTY_CHANGE, new FilePackageProperty(this, FilePackageProperty.Property.NAME, getName()));
        }
    }

    public void setAllowMerge(final Boolean merge) {
        this.setProperty(LinkCrawler.PACKAGE_ALLOW_MERGE, merge);
    }

    public Boolean isAllowMerge() {
        return this.getBooleanProperty(LinkCrawler.PACKAGE_ALLOW_MERGE, (Boolean) null);
    }

    /**
     * If set to true, "child-packages" can inherit this package name. </br>
     * Useful e.g. if a crawler crawls posts of a profile which then go into a crawler again but should all be placed into one package.
     */
    public void setAllowInheritance(final Boolean inheritance) {
        this.setProperty(LinkCrawler.PACKAGE_ALLOW_INHERITANCE, inheritance);
    }

    public Boolean isAllowInheritance() {
        return this.getBooleanProperty(LinkCrawler.PACKAGE_ALLOW_INHERITANCE, (Boolean) null);
    }

    /**
     * true = Set package name will be auto-cleaned. </br>
     * false = Set package name will not be modified. </br>
     * Default = true
     */
    public void setCleanupPackageName(final Boolean cleanup) {
        this.setProperty(LinkCrawler.PACKAGE_CLEANUP_NAME, cleanup);
    }

    public boolean isCleanupPackageName() {
        return this.getBooleanProperty(LinkCrawler.PACKAGE_CLEANUP_NAME, true);
    }

    /**
     * @deprecated Not needed anymore: The LinkCollector never moves items to the "Various" package if their FilePackage has a name or a
     *             package key set. Only items without such a FilePackage can end up in "Various". Setting a package name (or package key)
     *             is therefore enough, there is no need to set this flag additionally.
     */
    @Deprecated
    public void setIgnoreVarious(final Boolean ignorevarious) {
        this.setProperty(LinkCrawler.PACKAGE_IGNORE_VARIOUS, ignorevarious);
    }

    /**
     * @deprecated Not needed anymore: The LinkCollector never moves items to the "Various" package if their FilePackage has a name or a
     *             package key set. Only items without such a FilePackage can end up in "Various". Setting a package name (or package key)
     *             is therefore enough, there is no need to set this flag additionally.
     */
    @Deprecated
    public Boolean isIgnoreVarious() {
        return this.getBooleanProperty(LinkCrawler.PACKAGE_IGNORE_VARIOUS, (Boolean) null);
    }

    public void setPackageKey(final String key) {
        if (key == null) {
            this.removeProperty(PROPERTY_PACKAGE_KEY);
        } else {
            this.setProperty(PROPERTY_PACKAGE_KEY, key);
        }
    }

    public String getPackageKey() {
        return this.getStringProperty(PROPERTY_PACKAGE_KEY);
    }

    /**
     * return number of DownloadLinks in this FilePackage
     *
     * @return
     */
    public int size() {
        final boolean readL = getModifyLock().readLock();
        try {
            return downloadLinkList.size();
        } finally {
            if (readL) {
                getModifyLock().readUnlock(readL);
            }
        }
    }

    @Override
    public String toString() {
        return this.getName();
    }

    /**
     * return if this FilePackage is in expanded state
     *
     * @return
     */
    public boolean isExpanded() {
        if (isExpanded != null) {
            return isExpanded.booleanValue();
        }
        /* Read the stored state once and cache it */
        isExpanded = getBooleanProperty(PROPERTY_EXPANDED, false);
        return isExpanded;
    }

    /**
     * set the expanded state of this FilePackage
     *
     * @param b
     */
    public void setExpanded(boolean b) {
        if (this.isExpanded != null && this.isExpanded == b) {
            return;
        }
        this.isExpanded = b;
        /* The default (collapsed) is not stored: Property.NULL removes the property */
        if (b == false) {
            setProperty(PROPERTY_EXPANDED, Property.NULL);
        } else {
            setProperty(PROPERTY_EXPANDED, b);
        }
    }

    public void setPriorityEnum(Priority priority) {
        if (priority == null) {
            priority = Priority.DEFAULT;
        }
        if (getPriorityEnum() == priority) {
            return;
        }
        /* The default priority is not stored: Property.NULL removes the property */
        if (Priority.DEFAULT.equals(priority)) {
            setProperty(PROPERTY_PRIORITY, Property.NULL);
        } else {
            setProperty(PROPERTY_PRIORITY, priority.name());
        }
        if (hasNotificationListener()) {
            nodeUpdated(this, AbstractNodeNotifier.NOTIFY.PROPERTY_CHANGE, new FilePackageProperty(this, FilePackageProperty.Property.PRIORITY, priority));
        }
    }

    public Priority getPriorityEnum() {
        try {
            final String priority = getStringProperty(PROPERTY_PRIORITY, null);
            if (priority == null) {
                return Priority.DEFAULT;
            }
            return Priority.valueOf(priority);
        } catch (final Throwable e) {
            /* Unknown or invalid stored value */
            return Priority.DEFAULT;
        }
    }

    public boolean isEnabled() {
        return this.getView().isEnabled();
    }

    /**
     * Returns the internal list of children, not a copy. Callers have to hold the modify lock while they iterate over it.
     */
    public List<DownloadLink> getChildren() {
        return downloadLinkList;
    }

    /**
     * @return The number of bytes that have been downloaded by all links of this package
     */
    public long getBytesLoaded() {
        long loaded = 0;
        final boolean readL = getModifyLock().readLock();
        try {
            for (final DownloadLink link : getChildren()) {
                loaded += link.getView().getBytesLoaded();
            }
        } finally {
            getModifyLock().readUnlock(readL);
        }
        return loaded;
    }

    /**
     * @return The sum of the download times of all links of this package in milliseconds
     */
    public long getDownloadTime() {
        long time = 0;
        final boolean readL = getModifyLock().readLock();
        try {
            for (final DownloadLink link : getChildren()) {
                time += link.getView().getDownloadTime();
            }
        } finally {
            getModifyLock().readUnlock(readL);
        }
        return time;
    }

    /**
     * @return The download progress of this package in percent (0 to 100) or -1 if the total size is unknown
     */
    public double getDownloadProgressPercent() {
        final long total = new FilePackageView(this).aggregate().getSize();
        if (total <= 0) {
            return -1;
        }
        /* Same calculation as the progress column: Cut off after two decimals instead of rounding */
        return Math.min(100d, ExtProgressColumn.getPercentString(getBytesLoaded(), total));
    }

    public PackageController<FilePackage, DownloadLink> getControlledBy() {
        return controlledby;
    }

    public void setControlledBy(PackageController<FilePackage, DownloadLink> controller) {
        controlledby = controller;
    }

    public void setEnabled(boolean b) {
        /* Work on a copy, so the lock is not held while the links are modified */
        final boolean readL = getModifyLock().readLock();
        final ArrayList<DownloadLink> links;
        try {
            links = new ArrayList<DownloadLink>(getChildren());
        } finally {
            if (readL) {
                getModifyLock().readUnlock(readL);
            }
        }
        for (DownloadLink link : links) {
            link.setEnabled(b);
        }
    }

    public int indexOf(DownloadLink child) {
        final boolean readL = getModifyLock().readLock();
        try {
            return downloadLinkList.indexOf(child);
        } finally {
            if (readL) {
                getModifyLock().readUnlock(readL);
            }
        }
    }

    private final static AtomicReferenceFieldUpdater<FilePackage, FilePackageView> FILEPACKAGEVIEW_UPDATER = AtomicReferenceFieldUpdater.newUpdater(FilePackage.class, FilePackageView.class, "view");
    private volatile FilePackageView                                               view                    = null;

    @Override
    public FilePackageView getView() {
        while (true) {
            FilePackageView ret = view;
            if (ret != null) {
                return ret;
            }
            if (FILEPACKAGEVIEW_UPDATER.compareAndSet(this, null, ret = new FilePackageView(this))) {
                return ret;
            }
        }
    }

    public void setModified(long modified) {
        this.modified = modified;
    }

    public long getModified() {
        return modified;
    }

    @Override
    public long getFinishedDate() {
        return this.getView().getFinishedDate();
    }

    @Override
    public PackageControllerComparator<AbstractNode> getCurrentSorter() {
        return sorter;
    }

    @Override
    public void nodeUpdated(AbstractNode source, NOTIFY notify, Object param) {
        if (source == this && NOTIFY.STRUCTURE_CHANGE.equals(notify)) {
            /* Structure changes of this package itself only update the modified timestamp and are not forwarded to the controller */
            setModified(System.currentTimeMillis());
            return;
        }
        final PackageController<FilePackage, DownloadLink> n = getControlledBy();
        if (n == null) {
            /* Not controlled by a PackageController, nobody to notify */
            return;
        }
        /* No source means that this package itself changed */
        final AbstractNode lsource;
        if (source == null) {
            lsource = this;
        } else {
            lsource = source;
        }
        if (lsource instanceof AbstractPackageChildrenNode) {
            /* A child changed, the aggregated data of the view is outdated */
            final FilePackageView view = this.view;
            if (view != null) {
                view.requestUpdate();
            }
        }
        n.nodeUpdated(lsource, notify, param);
    }

    @Override
    public boolean hasNotificationListener() {
        final PackageController<FilePackage, DownloadLink> n = getControlledBy();
        return n != null && n.hasNotificationListener();
    }

    private final static AtomicReferenceFieldUpdater<FilePackage, ModifyLock> MODIFYLOCK_UPDATER = AtomicReferenceFieldUpdater.newUpdater(FilePackage.class, ModifyLock.class, "lock");
    private volatile ModifyLock                                               lock               = null;

    @Override
    public ModifyLock getModifyLock() {
        /* Created lazily and lock-free: If another thread set a lock first, the loop picks up that one */
        while (true) {
            ModifyLock ret = lock;
            if (ret != null) {
                return ret;
            }
            if (MODIFYLOCK_UPDATER.compareAndSet(this, null, ret = new ModifyLock())) {
                return ret;
            }
        }
    }

    public void copyPropertiesTo(FilePackage dest) {
        if (dest == null || dest == this) {
            return;
        }
        /* Only selected values are copied, not the whole property map (e.g. the expanded state is not copied) */
        /* The name is assigned directly, bypassing setName (no default name handling, no trimming, no change notification) */
        dest.name = name;
        dest.setComment(getComment());
        dest.setDownloadDirectory(getView().getDownloadDirectory());
        dest.setPriorityEnum(getPriorityEnum());
    }

    /**
     * Wrapper <br>
     * TODO: Remove this
     */
    @Override
    public void setDownloadFolder(String directory) {
        this.setDownloadDirectory(directory);
    }
}