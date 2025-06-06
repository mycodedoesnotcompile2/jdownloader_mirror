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

import org.appwork.storage.config.JsonConfig;
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
 * Diese Klasse verwaltet Pakete
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

    private String                                       name                 = null;
    private long                                         created              = -1l;
    private long                                         modified             = -1l;
    private Boolean                                      isExpanded           = null;
    private PackageController<FilePackage, DownloadLink> controlledby         = null;
    private UniqueAlltimeID                              uniqueID             = null;
    private ModifyLock                                   lock                 = null;
    public static final String                           PROPERTY_EXPANDED    = "EXPANDED";
    private static final String                          PROPERTY_COMMENT     = "COMMENT";
    private static final String                          PROPERTY_PRIORITY    = "PRIORITY";
    public static final String                           PROPERTY_PACKAGE_KEY = "PACKAGE_KEY";

    /**
     * @return the uniqueID
     */
    public UniqueAlltimeID getUniqueID() {
        if (uniqueID == null) {
            synchronized (this) {
                if (uniqueID == null) {
                    uniqueID = new UniqueAlltimeID();
                }
            }
        }
        return uniqueID;
    }

    private FilePackageView                           fpInfo = null;
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
            return ((FilePackage) obj).uniqueID == this.uniqueID;
        } else {
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
     * private constructor for FilePackage, sets created timestamp and downloadDirectory
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
     * add given DownloadLink to this FilePackage. delegates the call to DownloadControllerInterface if it is set
     *
     * @param link
     */
    public void add(DownloadLink link) {
        _add(link);
    }

    /**
     * add the given DownloadLinks to this FilePackage. delegates the call to the DownloadControllerInterface if it is set
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
     * add the given DownloadLinks to this FilePackage. delegates the call to the DownloadControllerInterface if it is set
     *
     * @param links
     */
    public void _add(DownloadLink... links) {
        if (links == null || links.length == 0) {
            return;
        }
        if (this.controlledby == null) {
            getModifyLock().writeLock();
            boolean notifyStructureChanges = false;
            try {
                for (DownloadLink link : links) {
                    if (!this.downloadLinkList.contains(link)) {
                        notifyStructureChanges = true;
                        link.setParentNode(this);
                        this.downloadLinkList.add(link);
                    }
                }
            } finally {
                getModifyLock().writeUnlock();
                if (notifyStructureChanges) {
                    nodeUpdated(this, NOTIFY.STRUCTURE_CHANGE, null);
                }
            }
        } else {
            this.controlledby.moveOrAddAt(this, Arrays.asList(links), -1);
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
     * remove the given DownloadLinks from this FilePackage. delegates remove call to DownloadControllerInterface if it is set
     *
     * @param link
     */
    public void remove(DownloadLink... links) {
        if (links == null || links.length == 0) {
            return;
        }
        if (this.controlledby == null) {
            getModifyLock().writeLock();
            boolean notifyStructureChanges = false;
            try {
                for (DownloadLink link : links) {
                    if ((this.downloadLinkList.remove(link))) {
                        notifyStructureChanges = true;
                        /*
                         * set FilePackage to null if the link was controlled by this FilePackage
                         */
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
        } else {
            this.controlledby.removeChildren(this, Arrays.asList(links), true);
        }
    }

    public void setComment(String comment) {
        final boolean changed;
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
     * @param subFolder
     */
    public void setDownloadDirectory(String folder) {
        if (StringUtils.isEmpty(folder)) {
            folder = GENERALSETTINGS.getDefaultDownloadFolder();
        } else if (!CrossSystem.isAbsolutePath(folder)) {
            LoggerFactory.I().getDefaultLogger().severe("FilePackage: setDownloadDirectory only allows absolute paths(" + folder + ")! Using default one!");
            folder = GENERALSETTINGS.getDefaultDownloadFolder();
        }
        final String lFolder = getDownloadDirectory();
        if (lFolder != null && lFolder.equals(folder)) {
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
            name = _JDT.T.controller_packages_defaultname();
        }
        if (lName != null && lName.equals(name)) {
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

    public void setIgnoreVarious(final Boolean ignorevarious) {
        this.setProperty(LinkCrawler.PACKAGE_IGNORE_VARIOUS, ignorevarious);
    }

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
        if (getPriorityEnum() != priority) {
            if (Priority.DEFAULT.equals(priority)) {
                setProperty(PROPERTY_PRIORITY, Property.NULL);
            } else {
                setProperty(PROPERTY_PRIORITY, priority.name());
            }
            if (hasNotificationListener()) {
                nodeUpdated(this, AbstractNodeNotifier.NOTIFY.PROPERTY_CHANGE, new FilePackageProperty(this, FilePackageProperty.Property.PRIORITY, priority));
            }
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
            return Priority.DEFAULT;
        }
    }

    public boolean isEnabled() {
        return this.getView().isEnabled();
    }

    public List<DownloadLink> getChildren() {
        return downloadLinkList;
    }

    public PackageController<FilePackage, DownloadLink> getControlledBy() {
        return controlledby;
    }

    public void setControlledBy(PackageController<FilePackage, DownloadLink> controller) {
        controlledby = controller;
    }

    public void setEnabled(boolean b) {
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

    @Override
    public FilePackageView getView() {
        if (fpInfo == null) {
            synchronized (this) {
                if (fpInfo == null) {
                    final FilePackageView lfpInfo = new FilePackageView(this);
                    fpInfo = lfpInfo;
                }
            }
        }
        return fpInfo;
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
            setModified(System.currentTimeMillis());
        } else {
            final PackageController<FilePackage, DownloadLink> n = getControlledBy();
            if (n == null) {
                return;
            }
            final AbstractNode lsource;
            if (source == null) {
                lsource = this;
            } else {
                lsource = source;
            }
            if (lsource instanceof AbstractPackageChildrenNode) {
                final FilePackageView lfpInfo = fpInfo;
                if (lfpInfo != null) {
                    lfpInfo.requestUpdate();
                }
            }
            n.nodeUpdated(lsource, notify, param);
        }
    }

    @Override
    public boolean hasNotificationListener() {
        final PackageController<FilePackage, DownloadLink> n = getControlledBy();
        return n != null && n.hasNotificationListener();
    }

    @Override
    public ModifyLock getModifyLock() {
        if (lock == null) {
            synchronized (this) {
                if (lock == null) {
                    lock = new ModifyLock();
                }
            }
        }
        return lock;
    }

    public void copyPropertiesTo(FilePackage dest) {
        if (dest != null && dest != this) {
            /* do not copy Property Properties as it currently only contains comment/expanded state */
            dest.name = name;
            dest.setComment(getComment());
            dest.setDownloadDirectory(getView().getDownloadDirectory());
            dest.setPriorityEnum(getPriorityEnum());
            return;
        }
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