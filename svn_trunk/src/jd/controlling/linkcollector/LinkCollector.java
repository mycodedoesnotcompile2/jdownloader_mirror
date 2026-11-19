package jd.controlling.linkcollector;

import java.awt.Toolkit;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.nio.charset.Charset;
import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.WeakHashMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import org.appwork.controlling.SingleReachableState;
import org.appwork.exceptions.WTFException;
import org.appwork.scheduler.DelayedRunnable;
import org.appwork.shutdown.ShutdownController;
import org.appwork.shutdown.ShutdownEvent;
import org.appwork.shutdown.ShutdownRequest;
import org.appwork.shutdown.ShutdownVetoException;
import org.appwork.shutdown.ShutdownVetoListener;
import org.appwork.storage.SimpleMapper;
import org.appwork.storage.TypeRef;
import org.appwork.storage.config.JsonConfig;
import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.uio.UIOManager;
import org.appwork.utils.Application;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Files;
import org.appwork.utils.IO;
import org.appwork.utils.JVMVersion;
import org.appwork.utils.StringUtils;
import org.appwork.utils.event.queue.Queue;
import org.appwork.utils.event.queue.Queue.QueuePriority;
import org.appwork.utils.event.queue.QueueAction;
import org.appwork.utils.io.J7FileList;
import org.appwork.utils.logging2.LogSource;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.dialog.ConfirmDialog;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;
import org.appwork.utils.swing.dialog.DialogNoAnswerException;
import org.jdownloader.captcha.blacklist.BlockAllCrawlerCaptchasEntry;
import org.jdownloader.captcha.blacklist.CaptchaBlackList;
import org.jdownloader.controlling.FileCreationManager;
import org.jdownloader.controlling.Priority;
import org.jdownloader.controlling.UniqueAlltimeID;
import org.jdownloader.controlling.filter.CompiledFiletypeFilter;
import org.jdownloader.controlling.filter.CompiledFiletypeFilter.ArchiveExtensions;
import org.jdownloader.controlling.filter.CompiledFiletypeFilter.DocumentExtensions;
import org.jdownloader.controlling.filter.CompiledFiletypeFilter.ExtensionsFilterInterface;
import org.jdownloader.controlling.filter.LinkFilterController;
import org.jdownloader.controlling.linkcrawler.GenericVariants;
import org.jdownloader.controlling.linkcrawler.LinkVariant;
import org.jdownloader.controlling.packagizer.PackagizerController;
import org.jdownloader.extensions.extraction.Archive;
import org.jdownloader.extensions.extraction.ExtractionExtension;
import org.jdownloader.extensions.extraction.bindings.crawledlink.CrawledLinkFactory;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.SelectionInfo.PackageView;
import org.jdownloader.gui.views.components.packagetable.LinkTreeUtils;
import org.jdownloader.gui.views.linkgrabber.LinkGrabberTable;
import org.jdownloader.gui.views.linkgrabber.LinkGrabberTableModel;
import org.jdownloader.gui.views.linkgrabber.LinkgrabberSearchField;
import org.jdownloader.gui.views.linkgrabber.addlinksdialog.LinkgrabberSettings;
import org.jdownloader.gui.views.linkgrabber.contextmenu.ConfirmLinksContextAction.AutoStartOptions;
import org.jdownloader.gui.views.linkgrabber.contextmenu.ConfirmLinksContextAction.ConfirmationDialogBehavior;
import org.jdownloader.gui.views.linkgrabber.contextmenu.ConfirmLinksContextAction.OnDupesLinksAction;
import org.jdownloader.gui.views.linkgrabber.contextmenu.ConfirmLinksContextAction.OnOfflineLinksAction;
import org.jdownloader.gui.views.linkgrabber.contextmenu.ConfirmLinksContextAction.PackageExpandBehavior;
import org.jdownloader.gui.views.linkgrabber.contextmenu.MenuManagerLinkgrabberTableContext;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.logging.LogController;
import org.jdownloader.myjdownloader.client.json.AvailableLinkState;
import org.jdownloader.plugins.FinalLinkState;
import org.jdownloader.plugins.controller.host.LazyHostPlugin;
import org.jdownloader.plugins.controller.host.PluginFinder;
import org.jdownloader.settings.GeneralSettings;
import org.jdownloader.settings.staticreferences.CFG_GUI;
import org.jdownloader.settings.staticreferences.CFG_LINKCOLLECTOR;
import org.jdownloader.settings.staticreferences.CFG_LINKGRABBER;
import org.jdownloader.translate._JDT;

import jd.config.Property;
import jd.controlling.TaskQueue;
import jd.controlling.downloadcontroller.DownloadController;
import jd.controlling.downloadcontroller.DownloadSession;
import jd.controlling.downloadcontroller.DownloadWatchDog;
import jd.controlling.downloadcontroller.DownloadWatchDogJob;
import jd.controlling.linkchecker.LinkChecker;
import jd.controlling.linkchecker.LinkCheckerHandler;
import jd.controlling.linkcollector.autostart.AutoStartManager;
import jd.controlling.linkcrawler.CheckableLink;
import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.CrawledLinkModifier;
import jd.controlling.linkcrawler.CrawledLinkProperty;
import jd.controlling.linkcrawler.CrawledPackage;
import jd.controlling.linkcrawler.CrawledPackage.TYPE;
import jd.controlling.linkcrawler.LinkCrawler;
import jd.controlling.linkcrawler.LinkCrawlerDeepInspector;
import jd.controlling.linkcrawler.LinkCrawlerFilter;
import jd.controlling.linkcrawler.LinkCrawlerHandler;
import jd.controlling.linkcrawler.LinkCrawlerRule;
import jd.controlling.linkcrawler.LinkCrawlerRule.RULE;
import jd.controlling.linkcrawler.PackageInfo;
import jd.controlling.packagecontroller.AbstractNode;
import jd.controlling.packagecontroller.AbstractPackageChildrenNodeFilter;
import jd.controlling.packagecontroller.PackageController;
import jd.controlling.packagecontroller.PackageControllerQueue.ReadOnlyQueueAction;
import jd.gui.swing.jdgui.JDGui;
import jd.gui.swing.jdgui.WarnLevel;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.parser.Regex;
import jd.plugins.CrawledLinkStorable;
import jd.plugins.CrawledPackageStorable;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.Plugin;
import jd.plugins.PluginForHost;
import jd.utils.JDUtilities;

/**
 * The linkgrabber: Collects the CrawledLinks found by the LinkCrawler jobs, checks them (LinkChecker), sorts them into CrawledPackages,
 * avoids duplicates and persists the list. All modifications of packages and links are done inside the queue of this controller.
 */
public class LinkCollector extends PackageController<CrawledPackage, CrawledLink> implements LinkCheckerHandler<CrawledLink>, LinkCrawlerHandler, ShutdownVetoListener {
    public static final String                        SOURCE_VARIANT_ID = "SOURCE_VARIANT_ID";
    /* All crawlers created for jobs. They are only held weakly, so finished crawlers can be garbage collected. */
    private final WeakHashMap<JobLinkCrawler, Object> jobLinkCrawlers   = new WeakHashMap<JobLinkCrawler, Object>();

    /**
     * LinkChecker of a single job, it knows the crawler of the job it belongs to.
     */
    public static final class JobLinkChecker extends LinkChecker<CrawledLink> {
        private final JobLinkCrawler jobLinkCrawler;

        public final JobLinkCrawler getJobLinkCrawler() {
            return jobLinkCrawler;
        }

        protected JobLinkChecker(JobLinkCrawler jobLinkCrawler) {
            this.jobLinkCrawler = jobLinkCrawler;
        }
    }

    /**
     * Crawler of a single LinkCollectingJob. It reports its results to the LinkCollector and has its own LinkChecker, so a job can be
     * aborted without touching other jobs.
     */
    public static final class JobLinkCrawler extends LinkCollectorCrawler {
        private final LinkCollectingJob         job;
        private final LinkCollectingInformation collectingInfo;
        private final LinkCollector             linkCollector;
        /* Makes sure that "crawler added" and "crawler stopped" are only reported once per run and always in pairs */
        private final AtomicBoolean             crawlerAdded   = new AtomicBoolean(false);
        private final JobLinkChecker            linkChecker;
        /* Number of links of this job that are waiting in the queue of the LinkCollector */
        private final AtomicInteger             waitingInQueue = new AtomicInteger(0);

        protected LinkCollector getLinkCollector() {
            return linkCollector;
        }

        public LinkChecker<CrawledLink> getLinkChecker() {
            return linkChecker;
        }

        /**
         * @return true while this job is crawling, checking links or still has links waiting in the queue
         */
        public boolean isCollecting() {
            return isRunning() || getLinkChecker().isRunning() || hasWaitingInQueue();
        }

        public final boolean hasWaitingInQueue() {
            return getQueueSize() > 0;
        }

        public final int getQueueSize() {
            return waitingInQueue.get();
        }

        protected void enqueuOrDequeue(CrawledLink link, boolean enqueueOrDequeue) {
            if (enqueueOrDequeue) {
                waitingInQueue.incrementAndGet();
            } else {
                waitingInQueue.decrementAndGet();
            }
        }

        private final AtomicBoolean abortedFlag = new AtomicBoolean(false);

        public boolean isAborted() {
            return abortedFlag.get();
        }

        /**
         * Aborts this job: stops crawling and link checking and blocks all further crawler captchas of this job.
         *
         * @return true if this call aborted the job or if the job was still collecting
         */
        public boolean abort() {
            final boolean aborted = abortedFlag.compareAndSet(false, true);
            if (aborted) {
                CaptchaBlackList.getInstance().add(new BlockAllCrawlerCaptchasEntry(this));
            }
            final boolean wasCollecting = isCollecting();
            getLinkChecker().stopChecking();
            stopCrawling();
            return aborted || wasCollecting;
        }

        @Override
        public boolean isDoDuplicateFinderFinalCheck() {
            if (!linkCollector.isDupeManagerEnabled()) {
                return false;
            }
            return super.isDoDuplicateFinderFinalCheck();
        }

        protected JobLinkCrawler(final LinkCollector linkCollector, final LinkCollectingJob job) {
            this.job = job;
            this.linkCollector = linkCollector;
            this.collectingInfo = new LinkCollectingInformation(this);
            this.linkChecker = new JobLinkChecker(this);
            this.linkChecker.setLinkCheckHandler(linkCollector);
            final LinkCrawlerHandler defaultHandler = defaultHandlerFactory();
            /* Every result is passed to the LinkCollector first and then to the default handler of the crawler */
            setHandler(new LinkCrawlerHandler() {
                @Override
                public void handleUnHandledLink(CrawledLink link) {
                    linkCollector.handleUnHandledLink(link);
                    defaultHandler.handleUnHandledLink(link);
                }

                @Override
                public void handleFinalLink(CrawledLink link) {
                    link.setCollectingInfo(collectingInfo);
                    final LinkCollectingJob job = getJob();
                    link.setSourceJob(job);
                    if (job.isAssignJobID()) {
                        final DownloadLink downloadLink = link.getDownloadLink();
                        if (downloadLink != null) {
                            downloadLink.setProperty(DownloadLink.PROPERTY_JOB_ID, job.getUniqueAlltimeID().getIDLong());
                        }
                    }
                    linkCollector.handleFinalLink(link, getLinkChecker());
                    defaultHandler.handleFinalLink(link);
                }

                @Override
                public void handleFilteredLink(CrawledLink link) {
                    link.setSourceJob(getJob());
                    linkCollector.handleFilteredLink(link);
                    defaultHandler.handleFilteredLink(link);
                }

                @Override
                public void handleBrokenLink(CrawledLink link) {
                    linkCollector.handleBrokenLink(link);
                    defaultHandler.handleBrokenLink(link);
                }
            });
            synchronized (linkCollector.jobLinkCrawlers) {
                linkCollector.jobLinkCrawlers.put(this, Boolean.TRUE);
            }
        }

        @Override
        protected void enqueueFinalCrawledLink(final LinkCrawlerGeneration generation, final CrawledLink link) {
            if (!linkCollector.isDupeManagerEnabled()) {
                super.enqueueFinalCrawledLink(generation, link);
                return;
            }
            final LinkCrawlerTask task = checkStartNotify(generation, "enqueueFinalCrawledLink");
            if (task == null) {
                return;
            }
            linkCollector.getQueue().add(new QueueAction<Boolean, RuntimeException>() {
                @Override
                protected void postRun() {
                    checkFinishNotify(task);
                }

                @Override
                protected Boolean run() throws RuntimeException {
                    if (!generation.isValid()) {
                        return null;
                    }
                    final CrawledLink existing = linkCollector.findDupe(link);
                    if (existing != null && existing.getMatchingFilter() == null) {
                        /* TODO: Add logger for this case */
                        onCrawledLinkDuplicate(link, DUPLICATE.FINAL);
                        return Boolean.FALSE;
                    }
                    JobLinkCrawler.super.enqueueFinalCrawledLink(generation, link);
                    return Boolean.TRUE;
                }
            });
        }

        public LinkCollectingJob getJob() {
            return job;
        }

        @Override
        public LinkCrawlerFilter getFilter() {
            return linkCollector.getCrawlerFilter();
        }

        @Override
        protected CrawledLink crawledLinkFactorybyURL(CharSequence url) {
            final CrawledLink ret = super.crawledLinkFactorybyURL(url);
            final LinkCollectingJob job = getJob();
            if (job != null) {
                ret.setOrigin(job.getOrigin());
                ret.setSourceJob(job);
            }
            return ret;
        }

        @Override
        protected void crawlerStopped() {
            if (crawlerAdded.compareAndSet(true, false)) {
                linkCollector.onCrawlerStopped(this);
            }
            super.crawlerStopped();
        }

        @Override
        protected void crawlerStarted() {
            if (crawlerAdded.compareAndSet(false, true)) {
                linkCollector.onCrawlerAdded(this);
            }
            linkCollector.onCrawlerStarted(this);
            super.crawlerStarted();
        }

        @Override
        protected void crawlerFinished() {
            linkCollector.onCrawlerFinished(this);
            super.crawlerFinished();
        }
    }

    /**
     * Identifies the package a link belongs to: package ID (e.g. package key or archive ID), package name and download folder. The name is
     * normalized (special characters removed, lower case) and the folder is compared case insensitive on Windows, so different spellings of
     * the same package end up in the same package.
     */
    private static class CrawledPackageMappingID {
        private final String id;

        private String getId() {
            return id;
        }

        /**
         * @return true if neither package name nor download folder is set. The ID is not taken into account.
         */
        private boolean isNull() {
            return getPackageName() == null && getDownloadFolder() == null && getDownloadFolderRaw() == null;
        }

        private String getPackageName() {
            return packageName;
        }

        private String getDownloadFolder() {
            return downloadFolder;
        }

        private String getDownloadFolderRaw() {
            return downloadFolderRaw;
        }

        /**
         * @return the serialized form that is stored in the saved list, it is restored by {@link #get(String)}
         */
        private String getMappingID() {
            return getId() + "|_|" + getPackageName() + "|_|" + getDownloadFolderRaw();
        }

        private final String packageName;
        private final String downloadFolder;
        private final String downloadFolderRaw;
        private final int    hashCode;

        private static CrawledPackageMappingID get(String combined) {
            if (combined == null) {
                return null;
            }
            String[] infos = new Regex(combined, "^(.*?)\\|_\\|(.*?)\\|_\\|(.*?)$").getRow(0);
            if (infos == null) {
                /* compatibility */
                infos = new Regex(combined, "^(.*?)_\\|\\|_(.*?)_\\|\\|_(.*?)$").getRow(0);
            }
            if (infos == null || infos.length != 3) {
                return null;
            }
            if ("Null".equalsIgnoreCase(infos[0])) {
                infos[0] = null;
            }
            if ("Null".equalsIgnoreCase(infos[1])) {
                infos[1] = null;
            }
            if ("Null".equalsIgnoreCase(infos[2])) {
                infos[2] = null;
            }
            if (infos[0] == null && infos[1] == null && infos[2] == null) {
                return null;
            }
            return new CrawledPackageMappingID(infos[0], infos[1], infos[2]);
        }

        private CrawledPackageMappingID(String id, String packageName, String downloadFolderRaw) {
            if (packageName != null) {
                /**
                 * we remove all non words/digits because some hoster replace/remove other chars
                 */
                if (JVMVersion.isMinimum(JVMVersion.JAVA_1_7)) {
                    packageName = packageName.replaceAll("(?U)[^\\w \\u2460-\\u24FF]", "").replaceAll("(?U)[_]", "").toLowerCase(Locale.ENGLISH);
                } else {
                    packageName = packageName.replaceAll("[^a-zA-Z0-9 \\u2460-\\u24FF]", "").toLowerCase(Locale.ENGLISH);
                }
                packageName = packageName.replaceAll("[ ]+", " ").trim();
            }
            this.id = id;
            this.packageName = packageName;
            this.downloadFolderRaw = Property.dedupeString(downloadFolderRaw);
            if (CrossSystem.isWindows() && downloadFolderRaw != null) {
                /**
                 * windows has case insensitive filesystem
                 */
                final String downloadFolder = downloadFolderRaw.toLowerCase(Locale.ENGLISH);
                this.downloadFolder = Property.dedupeString(downloadFolder);
            } else {
                this.downloadFolder = this.downloadFolderRaw;
            }
            this.hashCode = (this.id + this.packageName + this.downloadFolder).hashCode();
        }

        @Override
        public int hashCode() {
            return hashCode;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == this) {
                return true;
            }
            if (!(obj instanceof CrawledPackageMappingID)) {
                return false;
            }
            final CrawledPackageMappingID other = (CrawledPackageMappingID) obj;
            if (!StringUtils.equals(downloadFolder, other.downloadFolder)) {
                return false;
            }
            if (!StringUtils.equals(packageName, other.packageName)) {
                return false;
            }
            return StringUtils.equals(id, other.id);
        }
    }

    public static LinkCollector getInstance() {
        return INSTANCE;
    }

    private void onCrawlerAdded(LinkCollectorCrawler jobLinkCrawler) {
        eventsender.fireEvent(new LinkCollectorEvent(this, LinkCollectorEvent.TYPE.CRAWLER_ADDED, jobLinkCrawler, QueuePriority.NORM));
    }

    public void onCrawlerStopped(LinkCollectorCrawler crawledLinkCrawler) {
        getEventsender().removeListener(crawledLinkCrawler);
        eventsender.fireEvent(new LinkCollectorEvent(this, LinkCollectorEvent.TYPE.CRAWLER_STOPPED, crawledLinkCrawler, QueuePriority.NORM));
    }

    public void onCrawlerFinished(LinkCollectorCrawler crawledLinkCrawler) {
        getEventsender().removeListener(crawledLinkCrawler);
        eventsender.fireEvent(new LinkCollectorEvent(this, LinkCollectorEvent.TYPE.CRAWLER_FINISHED, crawledLinkCrawler, QueuePriority.NORM));
        autoStartManager.onCrawlerFinished(crawledLinkCrawler);
    }

    public void onCrawlerStarted(LinkCollectorCrawler crawledLinkCrawler) {
        getEventsender().addListener(crawledLinkCrawler, true);
        eventsender.fireEvent(new LinkCollectorEvent(this, LinkCollectorEvent.TYPE.CRAWLER_STARTED, crawledLinkCrawler, QueuePriority.NORM));
    }

    private transient LinkCollectorEventSender                         eventsender           = new LinkCollectorEventSender();
    public final ScheduledExecutorService                              TIMINGQUEUE           = DelayedRunnable.getNewScheduledExecutorService();
    public static SingleReachableState                                 CRAWLERLIST_LOADED    = new SingleReachableState("CRAWLERLIST_COMPLETE");
    private static LinkCollector                                       INSTANCE              = new LinkCollector();
    private volatile LinkChecker<CrawledLink>                          defaultLinkChecker    = null;
    /**
     * Maps a linkID to the set of all currently collected CrawledLinks that share it. This is the single, always-live source of truth for
     * duplicate detection: every add registers the link here, every remove unregisters it, so the number of occurrences of a linkID is
     * known at all times without ever having to re-scan the whole linkgrabber. The map is built regardless of the dupe manager mode; the
     * mode only decides whether duplicate <b>avoidance</b> reacts to it - the check lives centrally in
     * {@link #getExistingCrawledLinkByLinkIDOnDupeManagerEnabled(String)} (which returns null while the dupe manager is disabled), through
     * which every avoidance consumer (findDupe, addGenericVariant, setActiveVariantForLink, addAdditional) funnels, while
     * {@link #isLinkgrabberDupe(CrawledLink)} reads the map directly and always reflects the actual occurrence count.
     *
     * Each per-linkID set is an identity-based weak set: the CrawledLinks are held weakly (so a link that is dropped without an explicit
     * unregister does not leak and simply disappears on GC) and membership is by identity, which makes add/remove/contains O(1) - the
     * repeated registration of the same link during an add (once directly, once via moveOrAddAt) stays cheap even when a single linkID
     * accumulates many occurrences while the dupe manager is disabled.
     *
     * NOTE: the map is only ever mutated inside the IOEQ (single-threaded). The one exception is {@link #isLinkgrabberDupe(CrawledLink)},
     * which reads it from the view-filter thread; that is why the map is a {@link ConcurrentHashMap} and every access to a per-linkID set
     * (iteration included) synchronizes on that set - {@link Collections#synchronizedSet(Set)} uses the set itself as its monitor, so the
     * writer's add/remove and the reader's iteration are mutually exclusive.
     */
    private final ConcurrentHashMap<String, Set<CrawledLink>>          dupeCheckMap          = new ConcurrentHashMap<String, Set<CrawledLink>>();
    /* Lookup between mapping IDs and the normal packages they belong to, in both directions */
    private final Map<CrawledPackageMappingID, CrawledPackage>         packageMapIDtoPackage = new HashMap<CrawledPackageMappingID, CrawledPackage>();
    private final Map<CrawledPackage, CrawledPackageMappingID>         packageMapPackageToID = new HashMap<CrawledPackage, CrawledPackageMappingID>();
    /* Links that were dropped by the crawler filters, kept so the user can restore them */
    private final List<CrawledLink>                                    filteredStuff         = new CopyOnWriteArrayList<CrawledLink>();
    private volatile ExtractionExtension                               archiver;
    /* Saves the list a short time after the last change instead of after every single change */
    private final DelayedRunnable                                      asyncSaving;
    protected volatile CrawledPackage                                  offlinePackage;
    protected volatile CrawledPackage                                  variousPackage;
    protected volatile CrawledPackage                                  permanentofflinePackage;
    private final CopyOnWriteArrayList<File>                           linkcollectorLists    = new CopyOnWriteArrayList<File>();
    /*
     * SHA-1 over the raw JSON of all package/link entries (without extraInfo) of the last list that was written or loaded by this session,
     * null until the first save or load.
     */
    private volatile byte[]                                            lastSavedDigest       = null;
    /*
     * Links that were put into the offline/various package, grouped by the mapping ID of the package they would belong to. As soon as a
     * package with this ID is created, the links are moved into it.
     */
    private final HashMap<CrawledPackageMappingID, List<CrawledLink>>  offlineMap            = new HashMap<CrawledPackageMappingID, List<CrawledLink>>();
    private final HashMap<CrawledPackageMappingID, List<CrawledLink>>  variousMap            = new HashMap<CrawledPackageMappingID, List<CrawledLink>>();
    /*
     * Links without a usable package name (only ID and folder), grouped by ID and folder. They are moved into a package that contains a
     * link with the same container or origin URL.
     */
    private final HashMap<CrawledPackageMappingID, List<CrawledLink>>  badMappingMap         = new HashMap<CrawledPackageMappingID, List<CrawledLink>>();
    /* Per package cache for autoFileNameCorrection, cleared regularly by asyncCacheCleanup */
    private final WeakHashMap<CrawledPackage, HashMap<Object, Object>> autoRenameCache       = new WeakHashMap<CrawledPackage, HashMap<Object, Object>>();
    private final DelayedRunnable                                      asyncCacheCleanup;
    private final AutoStartManager                                     autoStartManager;
    private final boolean                                              isDupeManagerEnabled;

    private LinkCollector() {
        autoStartManager = new AutoStartManager();
        ShutdownController.getInstance().addShutdownVetoListener(this);
        ShutdownController.getInstance().addShutdownEvent(new ShutdownEvent() {
            @Override
            public long getMaxDuration() {
                return 0;
            }

            @Override
            public void onShutdown(final ShutdownRequest shutdownRequest) {
                /* Stop all crawlers first, then save the list and wait until it is written */
                LinkCollector.this.abort();
                QUEUE.addWait(new QueueAction<Void, RuntimeException>(Queue.QueuePriority.HIGH) {
                    @Override
                    protected Void run() throws RuntimeException {
                        saveLinkCollectorLinks(true);
                        return null;
                    }
                });
            }

            @Override
            public String toString() {
                return "save linkcollector...";
            }
        });
        final LinkCollectorConfig cfg = JsonConfig.create(LinkCollectorConfig.class);
        this.isDupeManagerEnabled = cfg.isDupeManagerEnabled();
        /* The list is saved after a pause of at least 5 seconds (1 ms inside the IDE). A maximum delay <= 0 means there is no maximum. */
        final long minimumDelay = Math.max(DebugMode.TRUE_IN_IDE_ELSE_FALSE ? 1 : 5000, cfg.getMinimumSaveDelay());
        long maximumDelay = cfg.getMaximumSaveDelay();
        if (maximumDelay <= 0) {
            maximumDelay = -1;
        } else {
            maximumDelay = Math.max(maximumDelay, minimumDelay);
        }
        asyncSaving = new DelayedRunnable(TIMINGQUEUE, minimumDelay, maximumDelay) {
            private final boolean ignoreShutDown = false;

            @Override
            public void run() {
                if (!isSavingAllowed(ignoreShutDown)) {
                    return;
                }
                super.run();
            }

            @Override
            public String getID() {
                return "LinkCollectorSave";
            }

            @Override
            public void delayedrun() {
                if (!isSavingAllowed(ignoreShutDown)) {
                    return;
                }
                QUEUE.addWait(new QueueAction<Void, RuntimeException>() {
                    @Override
                    protected Void run() throws RuntimeException {
                        saveLinkCollectorLinks(ignoreShutDown);
                        return null;
                    }
                });
            }
        };
        asyncCacheCleanup = new DelayedRunnable(TIMINGQUEUE, 30000l, 120000l) {
            @Override
            public String getID() {
                return "LinkCollectorCleanup";
            }

            @Override
            public void delayedrun() {
                QUEUE.add(new QueueAction<Void, RuntimeException>() {
                    @Override
                    protected Void run() throws RuntimeException {
                        autoRenameCache.clear();
                        return null;
                    }
                });
            }
        };
        this.eventsender.addListener(new LinkCollectorListener() {
            public void onLinkCollectorAbort(LinkCollectorEvent event) {
                asyncSaving.run();
            }

            public void onLinkCollectorFilteredLinksAvailable(LinkCollectorEvent event) {
                asyncSaving.run();
            }

            public void onLinkCollectorFilteredLinksEmpty(LinkCollectorEvent event) {
                asyncSaving.run();
            }

            public void onLinkCollectorDataRefresh(LinkCollectorEvent event) {
                asyncSaving.run();
            }

            public void onLinkCollectorStructureRefresh(LinkCollectorEvent event) {
                asyncSaving.run();
            }

            public void onLinkCollectorLinkAdded(LinkCollectorEvent event, CrawledLink parameter) {
                asyncCacheCleanup.run();
            }

            @Override
            public void onLinkCollectorDupeAdded(LinkCollectorEvent event, CrawledLink parameter) {
            }

            @Override
            public void onLinkCollectorContentRemoved(LinkCollectorEvent event) {
                asyncSaving.run();
            }

            @Override
            public void onLinkCollectorContentAdded(LinkCollectorEvent event) {
            }

            @Override
            public void onLinkCrawlerAdded(LinkCollectorCrawler parameter) {
            }

            @Override
            public void onLinkCrawlerStarted(LinkCollectorCrawler parameter) {
            }

            @Override
            public void onLinkCrawlerStopped(LinkCollectorCrawler parameter) {
            }

            @Override
            public void onLinkCrawlerNewJob(LinkCollectingJob job) {
            }

            @Override
            public void onLinkCrawlerFinished() {
            }
        });
        org.jdownloader.settings.staticreferences.CFG_LINKCOLLECTOR.DO_LINK_CHECK.getEventSender().addListener(new GenericConfigEventListener<Boolean>() {
            @Override
            public void onConfigValueModified(KeyHandler<Boolean> keyHandler, Boolean newValue) {
                logger.info("LinkChecker Changed:" + newValue);
            }

            @Override
            public void onConfigValidatorError(KeyHandler<Boolean> keyHandler, Boolean invalidValue, ValidationException validateException) {
            }
        });
        logger.info("LinkChecker Enabled:" + org.jdownloader.settings.staticreferences.CFG_LINKCOLLECTOR.DO_LINK_CHECK.isEnabled());
    }

    public AutoStartManager getAutoStartManager() {
        return autoStartManager;
    }

    public LinkCollectorEventSender getEventsender() {
        return eventsender;
    }

    @Override
    protected void _controllerPackageNodeAdded(CrawledPackage pkg, QueuePriority priority) {
        eventsender.fireEvent(new LinkCollectorEvent(LinkCollector.this, LinkCollectorEvent.TYPE.ADD_CONTENT, pkg, priority));
    }

    @Override
    protected void _controllerPackageNodeRemoved(CrawledPackage pkg, QueuePriority priority) {
        /* Forget the reference to a removed special package, a new one is created when it is needed again */
        if (TYPE.NORMAL != pkg.getType()) {
            switch (pkg.getType()) {
            case OFFLINE:
                offlinePackage = null;
                break;
            case POFFLINE:
                permanentofflinePackage = null;
                break;
            case VARIOUS:
                variousPackage = null;
                break;
            }
        }
        autoRenameCache.remove(pkg);
        final CrawledPackageMappingID mapping = removePackageMapping(pkg);
        eventsender.fireEvent(new LinkCollectorEvent(LinkCollector.this, LinkCollectorEvent.TYPE.REMOVE_CONTENT, pkg, priority));
        cleanupMaps(pkg, mapping, getChildrenCopy(pkg));
    }

    /**
     * NOTE: only access the IOEQ
     */
    private CrawledPackageMappingID removePackageMapping(CrawledPackage pkg) {
        final CrawledPackageMappingID id = packageMapPackageToID.remove(pkg);
        if (id == null) {
            return null;
        }
        final boolean removedFlag;
        if (JVMVersion.isMinimum(JVMVersion.JAVA_1_8)) {
            removedFlag = packageMapIDtoPackage.remove(id, pkg);
        } else if (packageMapIDtoPackage.get(id) == pkg) {
            removedFlag = (packageMapIDtoPackage.remove(id) == pkg);
        } else {
            removedFlag = false;
        }
        if (!removedFlag) {
            return null;
        }
        return id;
    }

    private CrawledPackageMappingID getPackageMapping(CrawledPackage pkg) {
        final CrawledPackageMappingID id = packageMapPackageToID.get(pkg);
        if (id == null || packageMapIDtoPackage.get(id) != pkg) {
            return null;
        }
        return id;
    }

    private void addPackageMapping(CrawledPackage pkg, CrawledPackageMappingID id) {
        packageMapIDtoPackage.put(id, pkg);
        packageMapPackageToID.put(pkg, id);
    }

    @Override
    protected void _controllerParentlessLinks(CrawledPackage pkg, List<CrawledLink> links, QueuePriority priority) {
        eventsender.fireEvent(new LinkCollectorEvent(LinkCollector.this, LinkCollectorEvent.TYPE.REMOVE_CONTENT, new ArrayList<CrawledLink>(links), priority));
        final CrawledPackageMappingID mapping = getPackageMapping(pkg);
        cleanupMaps(pkg, mapping, links);
    }

    @Override
    protected void _controllerStructureChanged(QueuePriority priority) {
        eventsender.fireEvent(new LinkCollectorEvent(LinkCollector.this, LinkCollectorEvent.TYPE.REFRESH_STRUCTURE, priority));
    }

    public void abort() {
        eventsender.fireEvent(new LinkCollectorEvent(LinkCollector.this, LinkCollectorEvent.TYPE.ABORT));
        synchronized (jobLinkCrawlers) {
            for (final JobLinkCrawler jobLinkCrawler : jobLinkCrawlers.keySet()) {
                jobLinkCrawler.abort();
            }
        }
    }

    public boolean isCollecting() {
        synchronized (jobLinkCrawlers) {
            for (final JobLinkCrawler jobLinkCrawler : jobLinkCrawlers.keySet()) {
                if (jobLinkCrawler.isCollecting()) {
                    return true;
                }
            }
        }
        return false;
    }

    public List<JobLinkCrawler> getJobLinkCrawlerByJobId(long... ids) {
        final List<JobLinkCrawler> result = new ArrayList<LinkCollector.JobLinkCrawler>();
        final Set<UniqueAlltimeID> jobs = UniqueAlltimeID.createSet(ids);
        synchronized (jobLinkCrawlers) {
            // TODO: optimize via Map<ID,WeakReference<JobLinkCrawler>>
            for (final JobLinkCrawler jobLinkCrawler : jobLinkCrawlers.keySet()) {
                if (jobs.remove(jobLinkCrawler.getJob().getUniqueAlltimeID())) {
                    result.add(jobLinkCrawler);
                }
                if (jobs.size() == 0) {
                    break;
                }
            }
        }
        return result;
    }

    /**
     * Lets hosters that manipulate filenames learn from the filenames of the other hosters in the same package, and corrects their names.
     * Only online links with a name set by the plugin are used.
     */
    protected void autoFileNameCorrection(List<CrawledLink> pkgchildren, CrawledPackage pkg) {
        if (CFG_LINKGRABBER.AUTO_FILENAME_CORRECTION_ENABLED.isEnabled() == false) {
            /* Do nothing */
            return;
        }
        final HashSet<LazyHostPlugin> hosts = new HashSet<LazyHostPlugin>();
        final ArrayList<DownloadLink> dlinks = new ArrayList<DownloadLink>();
        final ArrayList<DownloadLink> maybebadfilenames = new ArrayList<DownloadLink>();
        final ArrayList<CrawledLink> processLinks = new ArrayList<CrawledLink>();
        HashMap<Object, Object> cache = autoRenameCache.get(pkg);
        HashSet<String> nameCache = null;
        if (cache != null) {
            nameCache = (HashSet<String>) cache.get("nameCache");
        }
        if (nameCache == null) {
            nameCache = new HashSet<String>();
            if (cache != null) {
                cache.put("nameCache", nameCache);
            }
        }
        boolean newNames = false;
        for (final CrawledLink link : pkgchildren) {
            final String name = link.getDownloadLink().getNameSetbyPlugin();
            if (!AvailableLinkState.ONLINE.equals(link.getLinkState()) || name == null) {
                continue;
            }
            hosts.add(link.gethPlugin().getLazyP());
            if (link.gethPlugin().isHosterManipulatesFilenames()) {
                processLinks.add(link);
                maybebadfilenames.add(link.getDownloadLink());
                continue;
            }
            nameCache.add(name);
            dlinks.add(link.getDownloadLink());
        }
        /*
         * With only one single hoster we can hardly learn anything, and without links that may have bad names there is nothing to correct
         */
        if (hosts.size() <= 1 || processLinks.size() == 0) {
            return;
        }
        for (CrawledLink link : processLinks) {
            final String name = link.getDownloadLink().getNameSetbyPlugin();
            if (newNames == false && nameCache != null && !nameCache.contains(name)) {
                newNames = true;
                break;
            }
        }
        if (!newNames) {
            return;
        }
        if (cache == null) {
            cache = new HashMap<Object, Object>();
            autoRenameCache.put(pkg, cache);
        }
        cache.put("nameCache", nameCache);
        for (CrawledLink link : processLinks) {
            final String name = link.getDownloadLink().getNameSetbyPlugin();
            if (name == null) {
                continue;
            }
            nameCache.add(name);
            String newName = link.gethPlugin().autoFilenameCorrection(cache, name, link.getDownloadLink(), dlinks);
            if (newName == null) {
                newName = link.gethPlugin().autoFilenameCorrection(cache, name, link.getDownloadLink(), maybebadfilenames);
            }
            if (newName == null || name.equals(newName)) {
                /* we do not force a filename if newName equals to name set by plugin! */
                continue;
            }
            logger.info("Renamed file " + name + " to " + newName);
            link.getDownloadLink().setForcedFileName(newName);
        }
    }

    @Override
    public void moveOrAddAt(final CrawledPackage pkg, final List<CrawledLink> movechildren, final int moveChildrenindex, final int pkgIndex) {
        QUEUE.add(new QueueAction<Void, RuntimeException>() {
            @Override
            protected Void run() throws RuntimeException {
                /* Register the links in the dupeCheckMap before they are moved, they may come from outside of the linkgrabber */
                for (CrawledLink l : movechildren) {
                    addCrawledLinkByLinkID(l);
                }
                LinkCollector.super.moveOrAddAt(pkg, movechildren, moveChildrenindex, pkgIndex);
                return null;
            }
        });
    }

    private List<CrawledLink> getIdentifiedMap(final CrawledPackageMappingID crawledPackageMappingID, HashMap<CrawledPackageMappingID, List<CrawledLink>> map) {
        final List<CrawledLink> existing = map.get(crawledPackageMappingID);
        if (existing != null) {
            return existing;
        }
        final List<CrawledLink> list = new ArrayList<CrawledLink>();
        if (crawledPackageMappingID == null) {
            /* a list without ID can not be stored in the map */
            return list;
        }
        map.put(crawledPackageMappingID, list);
        return list;
    }

    /**
     * Adds the link to the linkgrabber (asynchronously, inside the queue). The target package is found or created in this order: existing
     * package with the same mapping ID, permanent offline package, offline package, various package, new package. Duplicates are dropped if
     * the dupe manager is enabled.
     */
    public void addCrawledLink(final CrawledLink link) {
        final LinkCollectingInformation info = link.getCollectingInfo();
        if (info != null && info.isAborted()) {
            clearCrawledLinkReferences(link);
            return;
        }
        /* try to find good matching package or create new one */
        QUEUE.add(new QueueAction<Void, RuntimeException>() {
            private LinkCollectingInformation lci = info;

            @Override
            protected void onEnqueu(Queue queue) {
                if (info != null) {
                    info.enqueu(this, link);
                }
            }

            @Override
            protected void postRun() {
                dequeu();
            }

            private void dequeu() {
                final LinkCollectingInformation info = this.lci;
                this.lci = null;
                if (info != null) {
                    info.dequeu(this, link);
                }
            }

            /**
             * Creates a new package for the links. Links that were collected for the same mapping ID in the offline/various/bad mapping
             * maps are moved into it as well.
             */
            private CrawledPackage addToNewPackage(final List<CrawledLink> links, String newPackageName, final CrawledPackageMappingID crawledPackageMappingID) {
                final CrawledPackage pkg = new CrawledPackage();
                pkg.setExpanded(CFG_LINKCOLLECTOR.CFG.isPackageAutoExpanded());
                pkg.setName(newPackageName);
                pkg.setDownloadFolder(crawledPackageMappingID.getDownloadFolderRaw());
                addPackageMapping(pkg, crawledPackageMappingID);
                if (links != null && links.size() > 0) {
                    LinkCollector.this.moveOrAddAt(pkg, links, -1);
                }
                if (crawledPackageMappingID.getPackageName() == null) {
                    putBadMappings(newPackageName, crawledPackageMappingID, links);
                    return pkg;
                }
                // check if we have matching links in offline maper
                final List<CrawledLink> offline = offlineMap.remove(crawledPackageMappingID);
                if (offline != null && offline.size() > 0) {
                    LinkCollector.this.moveOrAddAt(pkg, offline, -1);
                }
                final List<CrawledLink> various = variousMap.remove(crawledPackageMappingID);
                if (various != null && various.size() > 0) {
                    LinkCollector.this.moveOrAddAt(pkg, various, -1);
                }
                final List<CrawledLink> bad = getBadMappings(crawledPackageMappingID, pkg);
                if (bad != null && bad.size() > 0) {
                    LinkCollector.this.moveOrAddAt(pkg, bad, -1);
                }
                return pkg;
            }

            /**
             * Adds the links to an existing package. Unless it is the various package, links that were collected for the same mapping ID in
             * the offline/various/bad mapping maps are moved into it as well.
             */
            private CrawledPackage addToExistingPackage(final List<CrawledLink> links, CrawledPackage pkg, final CrawledPackageMappingID crawledPackageMappingID) {
                final String packageName = pkg.getName();
                if (links != null && links.size() > 0) {
                    LinkCollector.this.moveOrAddAt(pkg, links, -1);
                }
                if (crawledPackageMappingID.getPackageName() == null) {
                    putBadMappings(packageName, crawledPackageMappingID, links);
                    return pkg;
                }
                if (TYPE.VARIOUS.equals(pkg.getType())) {
                    return pkg;
                }
                // check if we have matching links in offline maper
                final List<CrawledLink> offline = offlineMap.remove(crawledPackageMappingID);
                if (offline != null && offline.size() > 0) {
                    LinkCollector.this.moveOrAddAt(pkg, offline, -1);
                }
                final List<CrawledLink> various = variousMap.remove(crawledPackageMappingID);
                if (various != null && various.size() > 0) {
                    LinkCollector.this.moveOrAddAt(pkg, various, -1);
                }
                final List<CrawledLink> bad = getBadMappings(crawledPackageMappingID, pkg);
                if (bad != null && bad.size() > 0) {
                    LinkCollector.this.moveOrAddAt(pkg, bad, -1);
                }
                return pkg;
            }

            private void putBadMappings(String newPackageName, CrawledPackageMappingID crawledPackageMappingID, List<CrawledLink> links) {
                if (links == null) {
                    return;
                }
                final CrawledPackageMappingID badID = new CrawledPackageMappingID(crawledPackageMappingID.getId(), null, crawledPackageMappingID.getDownloadFolderRaw());
                List<CrawledLink> badMappings = badMappingMap.get(badID);
                for (CrawledLink link : links) {
                    final DownloadLink dlLink = link.getDownloadLink();
                    if (dlLink.getContainerUrl() == null && dlLink.getOriginUrl() == null) {
                        continue;
                    }
                    if (badMappings == null) {
                        badMappings = new ArrayList<CrawledLink>();
                        badMappingMap.put(badID, badMappings);
                    }
                    badMappings.add(link);
                }
            }

            /**
             * Returns (and removes from the bad mapping map) the links without package name that share a container or origin URL with a
             * link that is already in the given package.
             */
            private List<CrawledLink> getBadMappings(CrawledPackageMappingID crawledPackageMappingID, CrawledPackage pkg) {
                final List<CrawledLink> ret = new ArrayList<CrawledLink>();
                final CrawledPackageMappingID badID = new CrawledPackageMappingID(crawledPackageMappingID.getId(), null, crawledPackageMappingID.getDownloadFolderRaw());
                final List<CrawledLink> badMappings = badMappingMap.get(badID);
                if (badMappings == null) {
                    return ret;
                }
                final HashSet<String> searchFor = new HashSet<String>();
                final boolean readL = pkg.getModifyLock().readLock();
                try {
                    for (final CrawledLink cLink : pkg.getChildren()) {
                        final DownloadLink dlLink = cLink.getDownloadLink();
                        searchFor.add(dlLink.getContainerUrl());
                        searchFor.add(dlLink.getOriginUrl());
                    }
                } finally {
                    pkg.getModifyLock().readUnlock(readL);
                }
                searchFor.remove(null);
                for (final CrawledLink cLink : badMappings) {
                    final DownloadLink dlLink = cLink.getDownloadLink();
                    if (searchFor.contains(dlLink.getContainerUrl()) || searchFor.contains(dlLink.getOriginUrl())) {
                        ret.add(cLink);
                    }
                }
                badMappings.removeAll(ret);
                if (badMappings.size() == 0) {
                    badMappingMap.remove(badID);
                }
                return ret;
            }

            /**
             * Returns the package for the mapping ID. If the ID has no package name, a package is searched that contains a link with the
             * same container or origin URL (packages with a name are preferred).
             */
            private CrawledPackage getCrawledPackage(CrawledPackageMappingID crawledPackageMappingID, CrawledLink mappingLink) {
                CrawledPackage ret = packageMapIDtoPackage.get(crawledPackageMappingID);
                if (ret != null || crawledPackageMappingID.getPackageName() != null) {
                    return ret;
                }
                final String containerURL = mappingLink.getDownloadLink().getContainerUrl();
                final String originURL = mappingLink.getDownloadLink().getOriginUrl();
                if (containerURL == null && originURL == null) {
                    return ret;
                }
                final HashMap<Integer, HashMap<CrawledPackageMappingID, CrawledPackage>> bestMappings = new HashMap<Integer, HashMap<CrawledPackageMappingID, CrawledPackage>>();
                for (final Entry<CrawledPackageMappingID, CrawledPackage> chance : packageMapIDtoPackage.entrySet()) {
                    int equals = 0;
                    if (StringUtils.equals(crawledPackageMappingID.getId(), chance.getKey().getId())) {
                        equals++;
                    }
                    if (StringUtils.equals(crawledPackageMappingID.getDownloadFolder(), chance.getKey().getDownloadFolder())) {
                        equals++;
                    }
                    if (equals == 0) {
                        continue;
                    }
                    final Integer eq = Integer.valueOf(equals);
                    HashMap<CrawledPackageMappingID, CrawledPackage> mappings = bestMappings.get(eq);
                    if (mappings == null) {
                        mappings = new HashMap<CrawledPackageMappingID, CrawledPackage>();
                        bestMappings.put(eq, mappings);
                    }
                    mappings.put(chance.getKey(), chance.getValue());
                }
                for (int x = 2; x > 0; x--) {
                    final HashMap<CrawledPackageMappingID, CrawledPackage> mappings = bestMappings.get(Integer.valueOf(x));
                    if (mappings == null) {
                        continue;
                    }
                    for (final Entry<CrawledPackageMappingID, CrawledPackage> mapping : mappings.entrySet()) {
                        final CrawledPackage pkg = mapping.getValue();
                        final boolean readL = pkg.getModifyLock().readLock();
                        try {
                            for (final CrawledLink cLink : pkg.getChildren()) {
                                final DownloadLink dlLink = cLink.getDownloadLink();
                                if (dlLink == null) {
                                    continue;
                                }
                                final boolean containerMatches = containerURL != null && StringUtils.equals(dlLink.getContainerUrl(), containerURL);
                                final boolean originMatches = originURL != null && StringUtils.equals(dlLink.getOriginUrl(), originURL);
                                if (!containerMatches && !originMatches) {
                                    continue;
                                }
                                final CrawledPackageMappingID id = mapping.getKey();
                                if (id.getPackageName() != null) {
                                    return pkg;
                                }
                                if (ret == null) {
                                    ret = pkg;
                                }
                            }
                        } finally {
                            pkg.getModifyLock().readUnlock(readL);
                        }
                    }
                }
                return ret;
            }

            @Override
            protected Void run() throws RuntimeException {
                try {
                    if (info != null && info.isAborted()) {
                        return null;
                    }
                    final CrawledLink existingLink = findDupe(link);
                    if (existingLink != null && existingLink != link) {
                        final boolean replaceFilteredLink = existingLink.getMatchingFilter() != null && filteredStuff.remove(existingLink);
                        if (!replaceFilteredLink) {
                            clearCrawledLinkReferences(link);
                            eventsender.fireEvent(new LinkCollectorEvent(LinkCollector.this, LinkCollectorEvent.TYPE.DUPE_LINK, link, QueuePriority.NORM));
                            return null;
                        }
                        /* remove filtered entry and add new link */
                        clearCrawledLinkReferences(existingLink);
                        cleanupMaps(null, null, Arrays.asList(new CrawledLink[] { existingLink }));
                        eventsender.fireEvent(new LinkCollectorEvent(LinkCollector.this, filteredStuff.size() > 0 ? LinkCollectorEvent.TYPE.FILTERED_AVAILABLE : LinkCollectorEvent.TYPE.FILTERED_EMPTY));
                    }
                    if (info != null && info.isAborted()) {
                        /* the collecting job may have been aborted while the (potentially slow) dupe handling above ran */
                        return null;
                    }
                    addCrawledLinkByLinkID(link);
                    if (link.getDownloadLink() != null) {
                        /* set CrawledLink as changeListener to its DownloadLink */
                        link.getDownloadLink().setNodeChangeListener(link);
                    }
                    /* Package information set by the crawler or the packagizer */
                    PackageInfo dpi = link.getDesiredPackageInfo();
                    UniqueAlltimeID uID = null;
                    String crawledPackageName = null;
                    String crawledPackageID = null;
                    /*
                     * The only effect of the default rule "Create subfolder by package name" is the download folder "<jd:packagename>". It
                     * is neither a custom download folder nor a customized package, so it must not disable the various package handling.
                     */
                    final boolean packagenameSubfolderOnly = dpi != null && dpi.isPackagenameSubfolderOnly();
                    /*
                     * Links with a package set by the crawler (FilePackage) or by a rule keep this package. The various package is only for
                     * links without a set package.
                     */
                    /*
                     * Links with a matching packagizer rule, an explicit "ignore various" flag or a custom download folder are never moved
                     * into the offline/various packages
                     */
                    boolean ignoreSpecialPackages = dpi != null && dpi.isIgnoreSpecialPackages();
                    final String downloadFolder;
                    if (dpi != null) {
                        crawledPackageName = dpi.getName();
                        String destinationFolder = dpi.getDestinationFolder();
                        if (destinationFolder == null) {
                            // no download folder, check for custom default download folder
                            destinationFolder = dpi.getDestinationFolderRoot();
                        } else if (!CrossSystem.isAbsolutePath(destinationFolder) && dpi.getDestinationFolderRoot() != null) {
                            // download folder is not absolute, combine with custom default download folder
                            destinationFolder = new File(dpi.getDestinationFolderRoot(), destinationFolder).getPath();
                        }
                        downloadFolder = destinationFolder;
                        if (downloadFolder != null && ignoreSpecialPackages == false && !packagenameSubfolderOnly) {
                            /* this regex cuts off trailing / and \ for equals check */
                            final String compareCustom = downloadFolder.replaceAll("(.+?)(/|\\\\)+$", "$1");
                            String compareDefault = JsonConfig.create(GeneralSettings.class).getDefaultDownloadFolder();
                            if (compareDefault != null) {
                                compareDefault = compareDefault.replaceAll("(.+?)(/|\\\\)+$", "$1");
                            }
                            /* check for custom downloadFolder. let's not use various package then */
                            // if (CrossSystem.isWindows() ||
                            // JsonConfig.create(GeneralSettings.class).isForceMirrorDetectionCaseInsensitive()) {
                            if (CrossSystem.isWindows()) {
                                if (!compareCustom.equalsIgnoreCase(compareDefault)) {
                                    ignoreSpecialPackages = true;
                                }
                            } else if (!compareCustom.equals(compareDefault)) {
                                ignoreSpecialPackages = true;
                            }
                        }
                        if ((uID = dpi.getUniqueId()) != null) {
                            crawledPackageID = dpi.getUniqueId().toString();
                            if (ignoreSpecialPackages && LinkCrawler.PERMANENT_OFFLINE_ID == uID) {
                                crawledPackageID = null;
                            }
                        }
                        if (StringUtils.isNotEmpty(dpi.getPackageKey())) {
                            crawledPackageID = dpi.getPackageKey();
                        }
                    } else {
                        downloadFolder = null;
                    }
                    if (crawledPackageName == null) {
                        /* Try to derive package name from filename */
                        final DownloadLink dlLink = link.getDownloadLink();
                        final String fileName;
                        if (link.isNameSet() || (dlLink != null && dlLink.isNameSet())) {
                            fileName = link.getName();
                        } else {
                            fileName = LinkCrawler.getUnsafeName(link.getName(), null);
                        }
                        String filenameForPackagenameDerivation = getFilenameForPackagenameDerivation(link, fileName);
                        if (filenameForPackagenameDerivation == null && link.getLinkInfo().getExtension() instanceof ArchiveExtensions) {
                            /* Create package name out of archive filename */
                            final ExtractionExtension lArchiver = archiver;
                            if (lArchiver != null && org.jdownloader.settings.staticreferences.CFG_LINKGRABBER.ARCHIVE_PACKAGIZER_ENABLED.isEnabled()) {
                                final CrawledLinkFactory clf = new CrawledLinkFactory(link);
                                final Archive archive = lArchiver.buildArchive(clf);
                                if (archive != null && archive.getArchiveFiles().size() > 1) {
                                    if (crawledPackageID == null) {
                                        crawledPackageID = archive.getArchiveID();
                                    }
                                    filenameForPackagenameDerivation = archive.getName();
                                }
                            }
                        }
                        if (filenameForPackagenameDerivation == null) {
                            /* Create package name out of non-archive filename */
                            filenameForPackagenameDerivation = link.getName();
                        }
                        if (filenameForPackagenameDerivation != null) {
                            crawledPackageName = LinknameCleaner.derivePackagenameFromFilename(filenameForPackagenameDerivation);
                            crawledPackageName = LinknameCleaner.cleanPackagename(crawledPackageName, true);
                        }
                    }
                    /* Find the target package: existing package for this mapping ID, special package or new package */
                    final CrawledPackageMappingID crawledPackageMapID = new CrawledPackageMappingID(crawledPackageID, crawledPackageName, downloadFolder);
                    final CrawledPackage pkg = getCrawledPackage(crawledPackageMapID, link);
                    if (pkg != null) {
                        final List<CrawledLink> add = new ArrayList<CrawledLink>(1);
                        add.add(link);
                        addToExistingPackage(add, pkg, crawledPackageMapID);
                    } else if (!ignoreSpecialPackages && LinkCrawler.PERMANENT_OFFLINE_ID == uID) {
                        /* these links will never come back online */
                        final List<CrawledLink> add = new ArrayList<CrawledLink>(1);
                        add.add(link);
                        final List<CrawledLink> list = getIdentifiedMap(crawledPackageMapID, offlineMap);
                        list.add(link);
                        LinkCollector.this.moveOrAddAt(getPermanentOfflineCrawledPackage(), add, -1);
                    } else if (!ignoreSpecialPackages && link.getLinkState() == AvailableLinkState.OFFLINE && org.jdownloader.settings.staticreferences.CFG_LINKGRABBER.OFFLINE_PACKAGE_ENABLED.isEnabled()) {
                        final List<CrawledLink> add = new ArrayList<CrawledLink>(1);
                        add.add(link);
                        final List<CrawledLink> list = getIdentifiedMap(crawledPackageMapID, offlineMap);
                        list.add(link);
                        LinkCollector.this.moveOrAddAt(getOfflineCrawledPackage(), add, -1);
                    } else if (!ignoreSpecialPackages && org.jdownloader.settings.staticreferences.CFG_LINKGRABBER.VARIOUS_PACKAGE_LIMIT.getValue() > 0 && CFG_LINKGRABBER.VARIOUS_PACKAGE_ENABLED.isEnabled()) {
                        /*
                         * Collect the links of this mapping ID. Once there are more than the limit, they get a package of their own,
                         * otherwise they stay in the various package. Offline links without any package information are not collected.
                         */
                        final List<CrawledLink> list;
                        if (!crawledPackageMapID.isNull() || !AvailableLinkState.OFFLINE.equals(link.getLinkState())) {
                            list = getIdentifiedMap(crawledPackageMapID, variousMap);
                            list.add(link);
                        } else {
                            list = null;
                        }
                        if (list != null && list.size() > org.jdownloader.settings.staticreferences.CFG_LINKGRABBER.VARIOUS_PACKAGE_LIMIT.getValue()) {
                            final CrawledPackage newPkg = addToNewPackage(list, crawledPackageName, crawledPackageMapID);
                            if (dpi != null) {
                                newPkg.setComment(dpi.getComment());
                            }
                        } else {
                            final List<CrawledLink> add = new ArrayList<CrawledLink>(1);
                            add.add(link);
                            addToExistingPackage(add, getVariousCrawledPackage(), crawledPackageMapID);
                        }
                    } else {
                        final List<CrawledLink> add = new ArrayList<CrawledLink>(1);
                        add.add(link);
                        final CrawledPackage newPkg = addToNewPackage(add, crawledPackageName, crawledPackageMapID);
                        if (dpi != null) {
                            newPkg.setComment(dpi.getComment());
                        }
                    }
                    dequeu();
                    eventsender.fireEvent(new LinkCollectorEvent(LinkCollector.this, LinkCollectorEvent.TYPE.ADDED_LINK, link, QueuePriority.NORM));
                    autoStartManager.onLinkAdded(link);
                    return null;
                } catch (Throwable e) {
                    removeCrawledLinkByLinkID(link);
                    logger.log(e);
                    if (e instanceof RuntimeException) {
                        throw (RuntimeException) e;
                    }
                    throw new RuntimeException(e);
                } finally {
                    clearCrawledLinkReferences(link);
                }
            }
        });
    }

    /**
     * Returns the filename the package name shall be derived from, or null if the filename of the given link is not suitable for this.
     */
    private String getFilenameForPackagenameDerivation(final CrawledLink link, final String fileName) {
        if (fileName == null) {
            return null;
        }
        if (AvailableLinkState.ONLINE.equals(link.getLinkState())) {
            return fileName;
        }
        final ExtensionsFilterInterface extension = link.getLinkInfo().getExtension();
        if ("".equalsIgnoreCase(extension.name())) {
            return null;
        }
        if (!DocumentExtensions.HTML.equals(extension.getSource()) && !DocumentExtensions.PHP.equals(extension.getSource())) {
            return fileName;
        }
        final String tmpFileName = fileName.replaceFirst("(?i)\\.(html?|php)$", "");
        final ExtensionsFilterInterface tmpExtension = CompiledFiletypeFilter.getExtensionsFilterInterface(Files.getExtension(tmpFileName));
        if (tmpExtension == null) {
            return null;
        }
        return tmpFileName;
    }

    private CrawledPackage getPermanentOfflineCrawledPackage() {
        CrawledPackage lpermanentofflinePackage = permanentofflinePackage;
        if (lpermanentofflinePackage != null && TYPE.POFFLINE == lpermanentofflinePackage.getType()) {
            return lpermanentofflinePackage;
        }
        lpermanentofflinePackage = new CrawledPackage();
        lpermanentofflinePackage.setExpanded(CFG_LINKCOLLECTOR.CFG.isPackageAutoExpanded());
        lpermanentofflinePackage.setName(_GUI.T.Permanently_Offline_Package());
        lpermanentofflinePackage.setType(TYPE.POFFLINE);
        permanentofflinePackage = lpermanentofflinePackage;
        return lpermanentofflinePackage;
    }

    private CrawledPackage getVariousCrawledPackage() {
        CrawledPackage lvariousPackage = variousPackage;
        if (lvariousPackage != null && TYPE.VARIOUS == lvariousPackage.getType()) {
            return lvariousPackage;
        }
        lvariousPackage = new CrawledPackage();
        lvariousPackage.setExpanded(CFG_LINKCOLLECTOR.CFG.isPackageAutoExpanded());
        lvariousPackage.setName(_JDT.T.LinkCollector_addCrawledLink_variouspackage());
        lvariousPackage.setType(TYPE.VARIOUS);
        variousPackage = lvariousPackage;
        return lvariousPackage;
    }

    private CrawledPackage getOfflineCrawledPackage() {
        CrawledPackage lofflinePackage = offlinePackage;
        if (lofflinePackage != null && TYPE.OFFLINE == lofflinePackage.getType()) {
            return lofflinePackage;
        }
        lofflinePackage = new CrawledPackage();
        lofflinePackage.setExpanded(CFG_LINKCOLLECTOR.CFG.isPackageAutoExpanded());
        lofflinePackage.setName(_JDT.T.LinkCollector_addCrawledLink_offlinepackage());
        lofflinePackage.setType(TYPE.OFFLINE);
        offlinePackage = lofflinePackage;
        return lofflinePackage;
    }

    public LinkCrawler addCrawlerJob(final List<CrawledLink> links, final LinkCollectingJob job) {
        if (ShutdownController.getInstance().isShutDownRequested()) {
            return null;
        }
        if (links == null || links.size() == 0) {
            throw new IllegalArgumentException("no links");
        }
        final JobLinkCrawler lc = newJobLinkCrawler(job);
        lc.crawl(new ArrayList<CrawledLink>(links));
        return lc;
    }

    public JobLinkCrawler newJobLinkCrawler(final LinkCollectingJob job) {
        final JobLinkCrawler ret = new JobLinkCrawler(this, job);
        getEventsender().fireEvent(new LinkCollectorEvent(LinkCollector.this, LinkCollectorEvent.TYPE.NEW_CRAWLER_JOB, job, QueuePriority.NORM));
        logger.info("Added CrawlerJob:" + job);
        return ret;
    }

    public LinkCrawler addCrawlerJob(final LinkCollectingJob job) {
        if (ShutdownController.getInstance().isShutDownRequested()) {
            return null;
        }
        if (job == null) {
            throw new IllegalArgumentException("job is null");
        }
        try {
            final JobLinkCrawler lc = newJobLinkCrawler(job);
            /*
             * we don't want to keep reference on text during the whole link grabbing/checking/collecting way
             */
            final String jobText = job.consumeText();
            // keep text if it is tiny.
            // if we have the text in the job, we can display it for example in the balloons
            if (StringUtils.isNotEmpty(jobText) && jobText.length() > 500) {
                job.setText(null);
            }
            lc.crawl(jobText, job.getCustomSourceUrl(), job.isDeepAnalyse());
            return lc;
        } catch (VerifyError e) {
            logger.log(e);
            throw e;
        }
    }

    /**
     * Stores a link that was dropped by a filter, so it can be restored later. The link is only kept if the restore button is enabled.
     *
     * @param checkDupe
     *            true: drop the link if an equal link already exists
     */
    private void addFilteredStuff(final CrawledLink filtered, final boolean checkDupe) {
        final LinkCollectingInformation info = filtered.getCollectingInfo();
        filtered.setCollectingInfo(null);
        if (CFG_LINKGRABBER.RESTORE_BUTTON_ENABLED.isEnabled() == false || Application.isHeadless()) {
            /** RestoreButton is disabled, no need to save the filtered links */
            /** Headless, no api/restore support yet */
            clearCrawledLinkReferences(filtered);
            return;
        }
        QUEUE.addAsynch(new QueueAction<Void, RuntimeException>() {
            @Override
            protected void onEnqueu(Queue queue) {
                if (info != null) {
                    info.enqueu(this, filtered);
                }
            }

            @Override
            protected boolean allowAsync() {
                return true;
            }

            @Override
            protected void postRun() {
                if (info != null) {
                    info.dequeu(this, filtered);
                }
            }

            @Override
            protected Void run() throws RuntimeException {
                final CrawledLink existingLink = checkDupe ? findDupe(filtered) : null;
                if (existingLink != null) {
                    clearCrawledLinkReferences(filtered);
                    eventsender.fireEvent(new LinkCollectorEvent(LinkCollector.this, LinkCollectorEvent.TYPE.DUPE_LINK, filtered, QueuePriority.NORM));
                    return null;
                }
                addCrawledLinkByLinkID(filtered);
                filteredStuff.add(filtered);
                eventsender.fireEvent(new LinkCollectorEvent(LinkCollector.this, LinkCollectorEvent.TYPE.FILTERED_AVAILABLE));
                return null;
            }
        });
    }

    /**
     * Returns dupe if dupe manager is enabled and dupe exists (the enabled check lives in
     * getExistingCrawledLinkByLinkIDOnDupeManagerEnabled).
     */
    protected CrawledLink findDupe(final CrawledLink cl) {
        String linkID = cl.getLinkID();
        CrawledLink existingLink = getExistingCrawledLinkByLinkIDOnDupeManagerEnabled(linkID);
        /* give the hPlugin a chance to fix this */
        while (existingLink != null) {
            final PluginForHost hPlugin = cl.gethPlugin();
            if (hPlugin == null || !hPlugin.onLinkCollectorDupe(existingLink, cl)) {
                break;
            }
            linkID = cl.getLinkID();
            existingLink = getExistingCrawledLinkByLinkIDOnDupeManagerEnabled(linkID);
        }
        return existingLink;
    }

    /* clean up offline/various/badMapping/dupeCheck maps */
    protected void cleanupMaps(final CrawledPackage pkg, final CrawledPackageMappingID lastKnownMapping, List<CrawledLink> links) {
        if (links == null || links.size() == 0) {
            return;
        }
        final TYPE type = pkg != null ? pkg.getType() : null;
        for (final CrawledLink l : links) {
            removeCrawledLinkByLinkID(l);
            if (lastKnownMapping != null) {
                /* the link can only be in one of the maps, stop at the first one that contained it */
                if (removeFromMap(variousMap, lastKnownMapping, l) != null) {
                    continue;
                }
                if (removeFromMap(offlineMap, lastKnownMapping, l) != null) {
                    continue;
                }
                removeFromMap(badMappingMap, lastKnownMapping, l);
                continue;
            }
            if (TYPE.VARIOUS == type && removeFromMap(variousMap, null, l) != null) {
                continue;
            }
            if (TYPE.OFFLINE == type && removeFromMap(offlineMap, null, l) != null) {
                continue;
            }
            if (removeFromMap(badMappingMap, null, l) != null) {
                continue;
            }
            if (removeFromMap(variousMap, null, l) != null) {
                continue;
            }
            removeFromMap(offlineMap, null, l);
        }
    }

    public void clearFilteredLinks() {
        QUEUE.add(new QueueAction<Void, RuntimeException>() {
            @Override
            protected Void run() throws RuntimeException {
                cleanupMaps(null, null, filteredStuff);
                filteredStuff.clear();
                eventsender.fireEvent(new LinkCollectorEvent(LinkCollector.this, LinkCollectorEvent.TYPE.FILTERED_EMPTY));
                return null;
            }
        });
    }

    @Override
    public void clear() {
        QUEUE.add(new QueueAction<Void, RuntimeException>() {
            @Override
            protected Void run() throws RuntimeException {
                LinkCollector.super.clear();
                filteredStuff.clear();
                dupeCheckMap.clear();
                offlinePackage = null;
                variousPackage = null;
                permanentofflinePackage = null;
                variousMap.clear();
                badMappingMap.clear();
                offlineMap.clear();
                autoRenameCache.clear();
                asyncCacheCleanup.stop();
                asyncCacheCleanup.delayedrun();
                eventsender.fireEvent(new LinkCollectorEvent(LinkCollector.this, LinkCollectorEvent.TYPE.FILTERED_EMPTY));
                return null;
            }
        });
    }

    /*
     * converts a CrawledPackage into a FilePackage
     *
     * if plinks is not set, then the original children of the CrawledPackage will get added to the FilePackage
     *
     * if plinks is set, then only plinks will get added to the FilePackage
     */
    private FilePackage createFilePackage(final CrawledPackage pkg, List<CrawledLink> plinks) {
        FilePackage ret = FilePackage.getInstance();
        ret.setPriorityEnum(pkg.getPriorityEnum());
        /* set values */
        ret.setName(pkg.getName());
        /* FilePackage contains full absolute path! */
        ret.setDownloadDirectory(LinkTreeUtils.getDownloadDirectory(pkg).getAbsolutePath());
        ret.setCreated(pkg.getCreated());
        ret.setExpanded(pkg.isExpanded());
        ret.setComment(pkg.getComment());
        List<CrawledLink> pkgLinks = null;
        if (plinks != null && plinks.size() > 0) {
            pkgLinks = new ArrayList<CrawledLink>(plinks);
        } else {
            boolean readL = pkg.getModifyLock().readLock();
            try {
                /* add Children from CrawledPackage to FilePackage */
                pkgLinks = new ArrayList<CrawledLink>(pkg.getChildren());
            } finally {
                pkg.getModifyLock().readUnlock(readL);
            }
        }
        List<DownloadLink> links = new ArrayList<DownloadLink>(pkgLinks.size());
        for (CrawledLink link : pkgLinks) {
            /* extract DownloadLink from CrawledLink */
            DownloadLink dl = link.getDownloadLink();
            if (dl == null) {
                continue;
            }
            /* remove reference to crawledLink */
            dl.setNodeChangeListener(null);
            /*
             * change filename if it is different than original downloadlink
             */
            if (link.isNameSet()) {
                dl.setForcedFileName(link.getName());
            }
            /* set correct enabled/disabled state */
            dl.setEnabled(link.isEnabled());
            dl.setCreated(link.getCreated());
            links.add(dl);
            /* set correct Parent node */
            dl.setParentNode(ret);
        }
        /* add all children to FilePackage */
        ret.getChildren().addAll(links);
        return ret;
    }

    /**
     * @return the crawlerFilter
     */
    public LinkCrawlerFilter getCrawlerFilter() {
        return LinkFilterController.getInstance();
    }

    public List<CrawledLink> getFilteredStuff(final boolean clearAfterGet) {
        return QUEUE.addWait(new QueueAction<List<CrawledLink>, RuntimeException>() {
            @Override
            protected List<CrawledLink> run() throws RuntimeException {
                final List<CrawledLink> ret2 = new ArrayList<CrawledLink>(filteredStuff);
                if (clearAfterGet) {
                    filteredStuff.clear();
                    cleanupMaps(null, null, ret2);
                    eventsender.fireEvent(new LinkCollectorEvent(LinkCollector.this, LinkCollectorEvent.TYPE.FILTERED_EMPTY));
                }
                return ret2;
            }
        });
    }

    public int getfilteredStuffSize() {
        return filteredStuff.size();
    }

    public synchronized LinkChecker<CrawledLink> getDefaultLinkChecker() {
        if (defaultLinkChecker == null) {
            final LinkChecker<CrawledLink> linkChecker = new LinkChecker<CrawledLink>();
            linkChecker.setLinkCheckHandler(this);
            defaultLinkChecker = linkChecker;
        }
        return defaultLinkChecker;
    }

    public void handleFilteredLink(CrawledLink link) {
        /* this method is called from LinkCrawler directly, we have to update dupeCheckMap */
        addFilteredStuff(link, true);
    }

    private final void clearCrawledLinkReferences(final CrawledLink link) {
        if (link == null) {
            return;
        }
        link.setBrokenCrawlerHandler(null);
        link.setCustomCrawledLinkModifier(null);
        link.setUnknownHandler(null);
        link.setDesiredPackageInfo(null);
        link.setSourceLink(null);
        link.setCollectingInfo(null);
        link.setSourceJob(null);
        link.setMatchingFilter(null);
        link.setMatchingRule(null);
    }

    public void handleFinalLink(final CrawledLink link) {
        handleFinalLink(link, getDefaultLinkChecker());
    }

    /**
     * Called for every link the crawler found. With link check enabled, the link is dropped if it is a dupe, otherwise it is passed to the
     * link checker, which calls linkCheckDone afterwards. With link check disabled, the link is added without being checked.
     */
    public void handleFinalLink(final CrawledLink link, final LinkChecker<CrawledLink> linkChecker) {
        final LinkCollectingInformation info = link.getCollectingInfo();
        if (info != null && info.isAborted()) {
            clearCrawledLinkReferences(link);
            return;
        }
        if (!org.jdownloader.settings.staticreferences.CFG_LINKCOLLECTOR.DO_LINK_CHECK.isEnabled()) {
            /* Linkcheck is disabled */
            QUEUE.add(new QueueAction<Void, RuntimeException>(Queue.QueuePriority.LOW) {
                private LinkCollectingInformation lci = info;

                @Override
                protected void onEnqueu(Queue queue) {
                    if (info != null) {
                        info.enqueu(this, link);
                    }
                }

                @Override
                protected void postRun() {
                    dequeu();
                }

                private void dequeu() {
                    final LinkCollectingInformation info = this.lci;
                    this.lci = null;
                    if (info != null) {
                        info.dequeu(this, link);
                    }
                }

                @Override
                protected Void run() throws RuntimeException {
                    if (info != null && info.isAborted()) {
                        clearCrawledLinkReferences(link);
                        return null;
                    }
                    applyJobCrawledLinkModifier(link, true, logger);
                    final PackagizerInterface pc = PackagizerController.getInstance();
                    if (pc != null) {
                        /* run packagizer on un-checked link */
                        pc.runByUrl(link);
                    }
                    applyJobCrawledLinkModifier(link, false, logger);
                    dequeu();
                    addCrawledLink(link);
                    return null;
                }
            });
            /* Link check is disabled: the link is added directly, do not send it through the link checker as well */
            return;
        }
        QUEUE.add(new QueueAction<Void, RuntimeException>(Queue.QueuePriority.LOW) {
            @Override
            protected void onEnqueu(Queue queue) {
                if (info != null) {
                    info.enqueu(this, link);
                }
            }

            @Override
            protected void postRun() {
                if (info != null) {
                    info.dequeu(this, link);
                }
            }

            @Override
            protected Void run() throws RuntimeException {
                if (info != null && info.isAborted()) {
                    clearCrawledLinkReferences(link);
                    return null;
                }
                /* avoid additional linkCheck when linkID already exists */
                final CrawledLink existingLink = findDupe(link);
                if (existingLink == null || existingLink.getMatchingFilter() != null) {
                    /* do not stop here if existing link is filtered, as new link is still unfiltered! */
                    linkChecker.check(link);
                    return null;
                }
                final String linkID = link.getLinkID();
                logger.info("Filtered Dupe: " + linkID);
                clearCrawledLinkReferences(link);
                eventsender.fireEvent(new LinkCollectorEvent(LinkCollector.this, LinkCollectorEvent.TYPE.DUPE_LINK, link, QueuePriority.NORM));
                return null;
            }
        });
        return;
    }

    public void linkCheckDone(final CrawledLink link) {
        /* This method is called by LinkChecker. The link is registered in the dupeCheckMap when it is added or filtered below */
        if (getCrawlerFilter().dropByFileProperties(link)) {
            addFilteredStuff(link, false);
            return;
        }
        if (!addGenericVariant(link.getDownloadLink())) {
            /* Link was dropped as dupe variant, release its references */
            clearCrawledLinkReferences(link);
            return;
        }
        QUEUE.add(new QueueAction<Void, RuntimeException>(Queue.QueuePriority.LOW) {
            @Override
            protected Void run() throws RuntimeException {
                applyJobCrawledLinkModifier(link, true, logger);
                final PackagizerInterface pc = PackagizerController.getInstance();
                if (pc != null) {
                    /* run packagizer on checked link */
                    pc.runByFile(link);
                }
                applyJobCrawledLinkModifier(link, false, logger);
                addCrawledLink(link);
                return null;
            }
        });
    }

    /**
     * Applies the modifiers of the job the link comes from.
     *
     * @param prePackagizer
     *            true: modifiers that have to run before the packagizer, false: modifiers that run after it
     */
    public static void applyJobCrawledLinkModifier(final CrawledLink link, final boolean prePackagizer, final LogSource logger) {
        if (link == null) {
            return;
        }
        final LinkCollectingJob job = link.getSourceJob();
        if (job == null) {
            return;
        }
        final List<CrawledLinkModifier> modifiers = prePackagizer ? job.getPrePackagizerModifier() : job.getPostPackagizerModifier();
        for (final CrawledLinkModifier modifier : modifiers) {
            try {
                modifier.modifyCrawledLink(link);
            } catch (final Throwable e) {
                logger.log(e);
            }
        }
    }

    /**
     * Adds the generic variants (original plus the conversions offered by the plugin) to links of plugins that do not have variant support
     * on their own.
     *
     * @return false if a link with the same variant already exists and the link has to be dropped
     */
    private boolean addGenericVariant(final DownloadLink downloadLink) {
        if (downloadLink.hasVariantSupport()) {
            return true;
        }
        final List<GenericVariants> converts = downloadLink.getDefaultPlugin().getGenericVariants(downloadLink);
        if (converts == null || converts.size() == 0) {
            return true;
        }
        final List<LinkVariant> variants = new ArrayList<LinkVariant>();
        variants.add(GenericVariants.ORIGINAL);
        variants.addAll(converts);
        if (variants.size() <= 1) {
            return true;
        }
        return Boolean.TRUE.equals(getQueue().addWait(new QueueAction<Boolean, RuntimeException>() {
            @Override
            protected Boolean run() throws RuntimeException {
                downloadLink.setProperty("GENERIC_VARIANTS", true);
                downloadLink.setVariants(variants);
                downloadLink.setVariant(GenericVariants.ORIGINAL);
                /* Avoid dupe (getExistingCrawledLinkByLinkIDOnDupeManagerEnabled returns null while the dupe manager is disabled) */
                final CrawledLink existing = getExistingCrawledLinkByLinkIDOnDupeManagerEnabled(downloadLink.getLinkID());
                if (existing != null && existing.getDownloadLink() != downloadLink) {
                    logger.info("Dupecheck Filtered Variant");
                    return Boolean.FALSE;
                }
                downloadLink.setVariantSupport(true);
                return Boolean.TRUE;
            }
        }));
    }

    /**
     * Converts the given links into FilePackages (one per CrawledPackage) for the downloadlist.
     *
     * @param removeLinks
     *            true: remove the links from the linkgrabber
     * @return null if there are no links
     */
    public List<FilePackage> convert(final List<CrawledLink> links, final boolean removeLinks) {
        if (links == null || links.size() == 0) {
            return null;
        }
        return QUEUE.addWait(new QueueAction<List<FilePackage>, RuntimeException>() {
            @Override
            protected List<FilePackage> run() throws RuntimeException {
                final List<FilePackage> ret = new ArrayList<FilePackage>();
                final HashMap<CrawledPackage, List<CrawledLink>> map = new HashMap<CrawledPackage, List<CrawledLink>>();
                for (final CrawledLink link : links) {
                    final CrawledPackage parent = link.getParentNode();
                    if (parent == null || parent.getControlledBy() != LinkCollector.this) {
                        logger.log(new Throwable("not controlled by this packagecontroller"));
                        continue;
                    }
                    List<CrawledLink> pkg_links = map.get(parent);
                    if (pkg_links == null) {
                        pkg_links = new ArrayList<CrawledLink>();
                        map.put(parent, pkg_links);
                    }
                    pkg_links.add(link);
                }
                final Iterator<Entry<CrawledPackage, List<CrawledLink>>> it = map.entrySet().iterator();
                while (it.hasNext()) {
                    final Entry<CrawledPackage, List<CrawledLink>> next = it.next();
                    ret.add(createFilePackage(next.getKey(), next.getValue()));
                    if (removeLinks) {
                        LinkCollector.this.removeChildren(next.getKey(), next.getValue(), true);
                    }
                }
                return ret;
            }
        });
    }

    /**
     * Removes the link from the given map. If no mapping ID is given, all lists of the map are searched.
     *
     * @return the mapping ID the link was removed from, or null if the link was not contained in the map
     */
    private CrawledPackageMappingID removeFromMap(final Map<CrawledPackageMappingID, List<CrawledLink>> idListMap, final CrawledPackageMappingID lastKnownMapping, final CrawledLink l) {
        if (lastKnownMapping != null) {
            final List<CrawledLink> list = idListMap.get(lastKnownMapping);
            if (list == null || !list.remove(l)) {
                return null;
            }
            if (list.size() == 0) {
                idListMap.remove(lastKnownMapping);
            }
            return lastKnownMapping;
        }
        final Iterator<Entry<CrawledPackageMappingID, List<CrawledLink>>> mapIt = idListMap.entrySet().iterator();
        while (mapIt.hasNext()) {
            final Entry<CrawledPackageMappingID, List<CrawledLink>> map = mapIt.next();
            final List<CrawledLink> list = map.getValue();
            if (list == null || !list.remove(l)) {
                continue;
            }
            if (list.size() == 0) {
                mapIt.remove();
            }
            return map.getKey();
        }
        return null;
    }

    /**
     * Builds a reverse lookup (link -> mapping ID) for the given map in one pass. Looking up every link with a linear search through all
     * lists would be O(links * list entries). If a link is contained in several entries, the first one in iteration order wins, same as the
     * former linear search.
     */
    private Map<CrawledLink, CrawledPackageMappingID> createIDLookup(final Map<CrawledPackageMappingID, List<CrawledLink>> idListMap) {
        final Map<CrawledLink, CrawledPackageMappingID> ret = new IdentityHashMap<CrawledLink, CrawledPackageMappingID>();
        for (final Entry<CrawledPackageMappingID, List<CrawledLink>> elem : idListMap.entrySet()) {
            final List<CrawledLink> mapElems = elem.getValue();
            if (mapElems == null) {
                continue;
            }
            for (final CrawledLink link : mapElems) {
                if (ret.containsKey(link)) {
                    continue;
                }
                ret.put(link, elem.getKey());
            }
        }
        return ret;
    }

    private ArrayList<File> findAvailableCollectorLists() {
        logger.info("Collect Lists");
        File[] filesInCfg = null;
        final File cfg = Application.getResource("cfg/");
        if (Application.getJavaVersion() >= Application.JAVA17) {
            try {
                filesInCfg = J7FileList.findFiles(Pattern.compile("^linkcollector.*?\\.zip$", Pattern.CASE_INSENSITIVE), cfg, true).toArray(new File[0]);
            } catch (IOException e) {
                logger.log(e);
            }
        }
        if (filesInCfg == null || filesInCfg.length == 0) {
            filesInCfg = Application.getResource("cfg/").listFiles();
        }
        ArrayList<Long> sortedAvailable = new ArrayList<Long>();
        ArrayList<File> ret = new ArrayList<File>();
        if (filesInCfg != null) {
            for (File collectorList : filesInCfg) {
                final String name = collectorList.getName();
                if (!name.startsWith("linkcollector") || !collectorList.isFile()) {
                    continue;
                }
                final String counter = new Regex(name, "linkcollector(\\d+)\\.zip$").getMatch(0);
                if (counter == null) {
                    continue;
                }
                sortedAvailable.add(Long.parseLong(counter));
            }
            Collections.sort(sortedAvailable, Collections.reverseOrder());
        }
        for (Long loadOrder : sortedAvailable) {
            ret.add(Application.getResource("cfg/linkcollector" + loadOrder + ".zip"));
        }
        if (Application.getResource("cfg/linkcollector.zip").exists()) {
            ret.add(Application.getResource("cfg/linkcollector.zip"));
        }
        logger.info("Lists: " + ret);
        linkcollectorLists.addAll(ret);
        return ret;
    }

    /**
     * load all CrawledPackages/CrawledLinks from Database
     */
    public void initLinkCollector() {
        QUEUE.add(new QueueAction<Void, RuntimeException>(Queue.QueuePriority.HIGH) {
            @Override
            protected Void run() throws RuntimeException {
                if (!JsonConfig.create(GeneralSettings.class).isSaveLinkgrabberListEnabled()) {
                    CRAWLERLIST_LOADED.setReached();
                    return null;
                }
                final ArrayList<File> availableCollectorLists = findAvailableCollectorLists();
                LinkedList<CrawledPackage> lpackages = null;
                final HashMap<CrawledPackage, CrawledPackageStorable> restoreMap = new HashMap<CrawledPackage, CrawledPackageStorable>();
                File loadedList = null;
                /* The lists are sorted newest first, the first one that can be loaded is used */
                for (File collectorList : availableCollectorLists) {
                    try {
                        restoreMap.clear();
                        lpackages = load(collectorList, restoreMap);
                        if (lpackages != null) {
                            /* remember the list that was actually loaded, it is the one to back up if the import fails */
                            loadedList = collectorList;
                            break;
                        }
                    } catch (final Throwable e) {
                        logger.log(e);
                    }
                }
                if (lpackages != null) {
                    int links = 0;
                    for (CrawledPackage cp : lpackages) {
                        links += cp.getChildren().size();
                    }
                    logger.info("CollectorList found: " + lpackages.size() + "/" + links);
                } else {
                    logger.info("CollectorList empty!");
                    restoreMap.clear();
                    lpackages = new LinkedList<CrawledPackage>();
                }
                /* add loaded Packages to this controller */
                try {
                    importList(lpackages, restoreMap);
                } catch (final Throwable e) {
                    if (loadedList != null) {
                        final File renameTo = new File(loadedList.getAbsolutePath() + ".backup");
                        boolean backup = false;
                        try {
                            if (loadedList.exists()) {
                                if (loadedList.renameTo(renameTo) == false) {
                                    IO.copyFile(loadedList, renameTo);
                                }
                                backup = true;
                            }
                        } catch (final Throwable e2) {
                            logger.log(e2);
                        }
                        logger.severe("Could backup " + loadedList + " to " + renameTo + " ->" + backup);
                    }
                    logger.log(e);
                } finally {
                    CRAWLERLIST_LOADED.setReached();
                }
                return null;
            }
        });
    }

    /**
     * Adds the loaded packages to the linkgrabber (at the top) and rebuilds the mappings of special packages and the dupeCheckMap.
     *
     * @param restoreMap
     *            the stored data of the packages, needed to restore the mapping IDs
     */
    public void importList(final LinkedList<CrawledPackage> lpackages, final Map<CrawledPackage, CrawledPackageStorable> restoreMap) {
        QUEUE.add(new QueueAction<Void, RuntimeException>(Queue.QueuePriority.HIGH) {
            @Override
            protected Void run() throws RuntimeException {
                if (lpackages == null) {
                    return null;
                }
                preProcessCrawledPackages(lpackages);
                writeLock();
                try {
                    for (final CrawledPackage filePackage : lpackages) {
                        for (final CrawledLink link : filePackage.getChildren()) {
                            if (link.getDownloadLink() == null) {
                                continue;
                            }
                            /* set CrawledLink as changeListener to its DownloadLink */
                            link.getDownloadLink().setNodeChangeListener(link);
                        }
                        filePackage.setControlledBy(LinkCollector.this);
                        if (restoreMap == null) {
                            continue;
                        }
                        final CrawledPackageStorable storable = restoreMap.get(filePackage);
                        switch (filePackage.getType()) {
                        case NORMAL:
                            if (storable.getPackageID() == null) {
                                break;
                            }
                            final CrawledPackageMappingID normalPackageID = CrawledPackageMappingID.get(storable.getPackageID());
                            if (normalPackageID != null && !normalPackageID.isNull()) {
                                addPackageMapping(filePackage, normalPackageID);
                            }
                            break;
                        case VARIOUS:
                            if (variousPackage == null) {
                                variousPackage = filePackage;
                            }
                            for (final CrawledLinkStorable link : storable.getLinks()) {
                                final String id = link.getID();
                                if (id == null) {
                                    continue;
                                }
                                final CrawledPackageMappingID packageID = CrawledPackageMappingID.get(id);
                                if (packageID == null) {
                                    continue;
                                }
                                final List<CrawledLink> list = getIdentifiedMap(packageID, variousMap);
                                list.add(link._getCrawledLink());
                            }
                            break;
                        case OFFLINE:
                            if (offlinePackage == null) {
                                offlinePackage = filePackage;
                            }
                            for (final CrawledLinkStorable link : storable.getLinks()) {
                                final String id = link.getID();
                                if (id == null) {
                                    continue;
                                }
                                final CrawledPackageMappingID packageID = CrawledPackageMappingID.get(id);
                                if (packageID == null) {
                                    continue;
                                }
                                final List<CrawledLink> list = getIdentifiedMap(packageID, offlineMap);
                                list.add(link._getCrawledLink());
                            }
                            break;
                        case POFFLINE:
                            if (permanentofflinePackage == null) {
                                permanentofflinePackage = filePackage;
                            }
                            break;
                        }
                    }
                    updateUniqueAlltimeIDMaps(lpackages);
                    for (final CrawledPackage filePackage : lpackages) {
                        for (final CrawledLink link : filePackage.getChildren()) {
                            try {
                                addCrawledLinkByLinkID(link);
                            } catch (Throwable e) {
                                logger.log(e);
                            }
                        }
                    }
                    packages.addAll(0, lpackages);
                } finally {
                    writeUnlock();
                }
                final long version = backendChanged.incrementAndGet();
                childrenChanged.set(version);
                structureChanged.set(version);
                eventsender.fireEvent(new LinkCollectorEvent(LinkCollector.this, LinkCollectorEvent.TYPE.REFRESH_STRUCTURE));
                return null;
            }
        });
    }

    private void preProcessCrawledPackages(LinkedList<CrawledPackage> fps) {
        if (fps == null || fps.size() == 0) {
            return;
        }
        final Iterator<CrawledPackage> iterator = fps.iterator();
        final PluginFinder pluginFinder = new PluginFinder(logger);
        while (iterator.hasNext()) {
            final CrawledPackage fp = iterator.next();
            if (fp.getChildren() != null) {
                final Iterator<CrawledLink> it = fp.getChildren().iterator();
                while (it.hasNext()) {
                    final CrawledLink localLink = it.next();
                    final DownloadLink dlLink = localLink.getDownloadLink();
                    if (dlLink == null) {
                        /* remove crawledLinks without DownloadLink */
                        it.remove();
                        continue;
                    }
                    /* assign defaultPlugin matching the hostname */
                    pluginFinder.assignPlugin(dlLink, true);
                }
            }
            if (fp.getChildren() == null || fp.getChildren().size() == 0) {
                /* remove empty packages */
                iterator.remove();
            }
        }
    }

    public void checkPluginUpdates() {
        if (!CRAWLERLIST_LOADED.isReached()) {
            return;
        }
        QUEUE.add(new QueueAction<Void, RuntimeException>() {
            private final PluginFinder finder = new PluginFinder(logger);

            @Override
            protected Void run() throws RuntimeException {
                getChildrenByFilter(new AbstractPackageChildrenNodeFilter<CrawledLink>() {
                    @Override
                    public int returnMaxResults() {
                        return 0;
                    }

                    private final void updatePluginInstance(DownloadLink link) {
                        final long currentDefaultVersion;
                        final String currentDefaultHost;
                        final PluginForHost defaultPlugin = link.getDefaultPlugin();
                        if (defaultPlugin != null) {
                            currentDefaultHost = defaultPlugin.getLazyP().getHost();
                            currentDefaultVersion = defaultPlugin.getLazyP().getVersion();
                        } else {
                            currentDefaultHost = null;
                            currentDefaultVersion = -1;
                        }
                        final PluginForHost newDefaultPlugin = finder.assignPlugin(link, true);
                        final long newDefaultVersion;
                        final String newDefaultHost;
                        if (newDefaultPlugin != null) {
                            newDefaultVersion = newDefaultPlugin.getLazyP().getVersion();
                            newDefaultHost = newDefaultPlugin.getLazyP().getHost();
                        } else {
                            newDefaultVersion = -1;
                            newDefaultHost = null;
                        }
                        if (newDefaultPlugin == null) {
                            return;
                        }
                        if (currentDefaultVersion == newDefaultVersion && StringUtils.equals(currentDefaultHost, newDefaultHost)) {
                            return;
                        }
                        logger.info("Update Plugin for: " + link.getName() + ":" + link.getHost() + ":" + currentDefaultVersion + " to " + newDefaultPlugin.getLazyP().getDisplayName() + ":" + newDefaultPlugin.getLazyP().getVersion());
                        if (link.getFinalLinkState() == FinalLinkState.PLUGIN_DEFECT) {
                            link.setFinalLinkState(null);
                        }
                    }

                    @Override
                    public boolean acceptNode(final CrawledLink node) {
                        final DownloadLink downloadLink = node.getDownloadLink();
                        if (downloadLink == null) {
                            return false;
                        }
                        updatePluginInstance(downloadLink);
                        return false;
                    }
                });
                return null;
            }
        });
    }

    /**
     * A package and its children while a list is being read. The children are attached in the order of their stored index.
     */
    private class LoadedPackage {
        private CrawledPackage                      crawledPackage = null;
        private final HashMap<Integer, CrawledLink> crawledLinks   = new HashMap<Integer, CrawledLink>();

        private CrawledPackage getLoadedPackage() {
            if (crawledPackage == null) {
                return null;
            }
            if (crawledPackage.getChildren().size() > 0) {
                return crawledPackage;
            }
            final List<Integer> childIndices = new ArrayList<Integer>(crawledLinks.keySet());
            Collections.sort(childIndices);
            for (final Integer childIndex : childIndices) {
                final CrawledLink child = crawledLinks.get(childIndex);
                crawledPackage.getChildren().add(child);
                child.setParentNode(crawledPackage);
            }
            return crawledPackage;
        }
    }

    private LinkedList<CrawledPackage> load(File file, HashMap<CrawledPackage, CrawledPackageStorable> restoreMap) {
        try {
            MessageDigest digest = null;
            try {
                digest = MessageDigest.getInstance("SHA-1");
            } catch (final Exception e) {
                /* optional optimization only, a missing digest must never make the load fail (and the list get renamed) */
                logger.log(e);
            }
            final LinkedList<CrawledPackage> ret = loadFile(file, restoreMap, false, digest);
            if (ret != null && digest != null) {
                /*
                 * Same fingerprint save() builds for this content. The lists are tried newest first and the first loadable one wins, so
                 * this is the content a restart would load again. If it is unchanged until the next save, nothing needs to be written. This
                 * also holds if a newer list was broken and moved away: the content is still on disk.
                 */
                lastSavedDigest = digest.digest();
            }
            return ret;
        } catch (Throwable e) {
            logger.log(e);
            if (file == null) {
                return null;
            }
            final File backupTo = new File(file.getAbsolutePath() + ".backup");
            boolean backupSucceeded = false;
            Long size = null;
            writeBackup: try {
                if (!file.exists()) {
                    break writeBackup;
                }
                size = file.length();
                if (size <= 0) {
                    file.delete();
                    break writeBackup;
                }
                if (file.renameTo(backupTo)) {
                    backupSucceeded = backupTo.exists();
                    break writeBackup;
                }
                IO.copyFile(file, backupTo);
                backupSucceeded = backupTo.exists();
                if (!backupSucceeded || !file.exists()) {
                    break writeBackup;
                }
                if (file.delete() == false) {
                    file.deleteOnExit();
                }
            } catch (final Throwable e2) {
                logger.log(e2);
            }
            if (backupSucceeded) {
                logger.severe("Could backup " + file + "<to>" + backupTo);
            } else {
                logger.severe("Could not backup " + file + "<to>" + backupTo + " because size=" + size);
            }
        }
        return null;
    }

    /**
     * Updates the download folders of all packages that are located inside the JDownloader root the list was saved with, to the current
     * JDownloader root. This is needed if the user moved the JDownloader folder.
     */
    private void convertPackageFoldersToCurrentJDRoot(final LinkCollectorStorable lcs, final List<CrawledPackage> packages) {
        try {
            final String oldRootPath = lcs.getRootPath();
            if (StringUtils.isEmpty(oldRootPath)) {
                return;
            }
            final String newRoot = JDUtilities.getJDHomeDirectoryFromEnvironment().getAbsolutePath();
            if (oldRootPath.equals(newRoot)) {
                return;
            }
            for (final CrawledPackage pkg : packages) {
                if (!CrossSystem.isAbsolutePath(pkg.getDownloadFolder())) {
                    /* no need to convert relative paths */
                    continue;
                }
                final String pkgPath = LinkTreeUtils.getDownloadDirectory(pkg).toString();
                if (!pkgPath.startsWith(oldRootPath + "/") && !pkgPath.startsWith(oldRootPath + "\\")) {
                    /* folder is outside of the old JDRoot */
                    continue;
                }
                /* folder is inside JDRoot, lets update it */
                String restPath = pkgPath.substring(oldRootPath.length());
                /* cut off leading path separator */
                restPath = restPath.replaceFirst("^(/+|\\\\+)", "");
                /* fix path separators */
                restPath = CrossSystem.fixPathSeparators(restPath);
                final String newPath = new File(newRoot, restPath).toString();
                if (StringUtils.equals(pkgPath, newPath)) {
                    continue;
                }
                pkg.setDownloadFolder(newPath);
            }
        } catch (final Throwable e) {
            /* this method can throw exceptions, eg in SVN */
            logger.log(e);
        }
    }

    public LinkedList<CrawledPackage> loadFile(File file, Map<CrawledPackage, CrawledPackageStorable> restoreMap, boolean rescueMode) throws IOException {
        return loadFile(file, restoreMap, rescueMode, null);
    }

    /**
     * @param digest
     *            optional: receives the raw JSON of all package/link entries in file order (without extraInfo), see save()
     */
    private LinkedList<CrawledPackage> loadFile(File file, Map<CrawledPackage, CrawledPackageStorable> restoreMap, boolean rescueMode, final MessageDigest digest) throws IOException {
        if (file == null || !file.exists()) {
            return null;
        }
        FileInputStream fis = null;
        ZipInputStream zis = null;
        final SimpleMapper mapper = new SimpleMapper() {
            @Override
            protected void initMapper() {
            }
        };
        try {
            fis = new FileInputStream(file);
            zis = new ZipInputStream(new BufferedInputStream(fis, 1 * 1024 * 1024));
            final List<CrawledLink> loadedCrawledLinks = new ArrayList<CrawledLink>();
            /* lets restore the CrawledPackages from Json */
            final HashMap<Integer, LoadedPackage> packageMap = new HashMap<Integer, LoadedPackage>();
            LinkCollectorStorable lcs = null;
            final TypeRef<CrawledLinkStorable> crawledLinkStorable = new TypeRef<CrawledLinkStorable>() {
            };
            final TypeRef<CrawledPackageStorable> crawledPackageStorable = new TypeRef<CrawledPackageStorable>() {
            };
            final TypeRef<LinkCollectorStorable> linkCollectorStorable = new TypeRef<LinkCollectorStorable>() {
            };
            ZipEntry entry = null;
            final ZipInputStream finalZis = zis;
            final InputStream entryInputStream = new BufferedInputStream(new InputStream() {
                @Override
                public int read() throws IOException {
                    return finalZis.read();
                }

                @Override
                public int read(byte[] b, int off, int len) throws IOException {
                    return finalZis.read(b, off, len);
                }

                @Override
                public long skip(long n) throws IOException {
                    return finalZis.skip(n);
                }

                @Override
                public int available() throws IOException {
                    return finalZis.available();
                }

                @Override
                public boolean markSupported() {
                    return false;
                }

                @Override
                public void close() throws IOException {
                }

                @Override
                public synchronized void mark(int readlimit) {
                }
            }, 1024) {
                @Override
                public void close() throws IOException {
                }
            };
            int entries = 0;
            final Pattern entryType = Pattern.compile("(\\d+)(?:_(\\d+))?|extraInfo", Pattern.CASE_INSENSITIVE);
            final ByteArrayOutputStream bos = new ByteArrayOutputStream() {
                @Override
                public synchronized byte[] toByteArray() {
                    return buf;
                };
            };
            final Charset UTF8 = Charset.forName("UTF-8");
            while ((entry = zis.getNextEntry()) != null) {
                try {
                    entries++;
                    final Matcher entryName = entryType.matcher(entry.getName());
                    if (!entryName.matches()) {
                        continue;
                    }
                    if (entryName.group(2) != null) {
                        /* \\d+_\\d+ CrawledLinkStorable */
                        final Integer packageIndex = Integer.valueOf(entryName.group(1));
                        final Integer childIndex = Integer.valueOf(entryName.group(2));
                        LoadedPackage loadedPackage = packageMap.get(packageIndex);
                        if (loadedPackage == null) {
                            loadedPackage = new LoadedPackage();
                            packageMap.put(packageIndex, loadedPackage);
                        }
                        bos.reset();
                        IO.readStream((int) entry.getSize(), entryInputStream, bos);
                        if (digest != null) {
                            digest.update(bos.toByteArray(), 0, bos.size());
                        }
                        final CrawledLinkStorable storable = mapper.stringToObject(new String(bos.toByteArray(), 0, bos.size(), UTF8), crawledLinkStorable);
                        if (storable == null) {
                            throw new WTFException("restored a null CrawledLinkStorable");
                        }
                        final CrawledLink crawledLink = storable._getCrawledLink();
                        loadedCrawledLinks.add(crawledLink);
                        loadedPackage.crawledLinks.put(childIndex, crawledLink);
                    } else if (entryName.group(1) != null) {
                        /* \\d+ CrawledPackageStorable */
                        final Integer packageIndex = Integer.valueOf(entry.getName());
                        bos.reset();
                        IO.readStream((int) entry.getSize(), entryInputStream, bos);
                        if (digest != null) {
                            digest.update(bos.toByteArray(), 0, bos.size());
                        }
                        final CrawledPackageStorable storable = mapper.stringToObject(new String(bos.toByteArray(), 0, bos.size(), UTF8), crawledPackageStorable);
                        if (storable == null) {
                            throw new WTFException("restored a null CrawledPackageStorable");
                        }
                        LoadedPackage loadedPackage = packageMap.get(packageIndex);
                        if (loadedPackage == null) {
                            loadedPackage = new LoadedPackage();
                            packageMap.put(packageIndex, loadedPackage);
                        }
                        loadedPackage.crawledPackage = storable._getCrawledPackage();
                        if (restoreMap != null) {
                            restoreMap.put(storable._getCrawledPackage(), storable);
                        }
                    } else {
                        /* extraInfo */
                        lcs = mapper.inputStreamToObject(entryInputStream, linkCollectorStorable);
                    }
                } catch (final Throwable e) {
                    logger.log(e);
                    if (entry != null) {
                        logger.info("Entry:" + entry + "|Size:" + entry.getSize() + "|Compressed Size:" + entry.getCompressedSize());
                    }
                    if (rescueMode) {
                        break;
                    }
                    throw e;
                }
            }
            if (entries == 0) {
                throw new WTFException("Empty/Invalid Zip:" + file + "|Size:" + file.length());
            }
            {
                final HashMap<String, String> stringDeduplication = new HashMap<String, String>();
                for (CrawledLink crawledLink : loadedCrawledLinks) {
                    final String[] sourceURLs = crawledLink.getSourceUrls();
                    if (sourceURLs == null) {
                        continue;
                    }
                    final DownloadLink downloadLink = crawledLink.getDownloadLink();
                    for (int i = 0; i < sourceURLs.length; i++) {
                        final String sourceURL = sourceURLs[i];
                        String dedupeString = null;
                        if (downloadLink != null && StringUtils.equals(sourceURL, downloadLink.getPluginPatternMatcher())) {
                            dedupeString = downloadLink.getPluginPatternMatcher();
                        }
                        if (dedupeString == null) {
                            dedupeString = Property.returnDedupeStringInstance(sourceURL);
                        }
                        if (dedupeString == null) {
                            dedupeString = stringDeduplication.get(sourceURL);
                        }
                        if (dedupeString == null) {
                            stringDeduplication.put(sourceURL, sourceURL);
                            dedupeString = sourceURL;
                        }
                        sourceURLs[i] = dedupeString;
                    }
                    crawledLink.setSourceUrls(sourceURLs);
                }
            }
            /* sort positions */
            final List<Integer> packageIndices = new ArrayList<Integer>(packageMap.keySet());
            Collections.sort(packageIndices);
            /* build final ArrayList of CrawledPackage */
            final List<CrawledPackage> ret2 = new ArrayList<CrawledPackage>(packageIndices.size());
            for (final Integer packageIndex : packageIndices) {
                final LoadedPackage loadedPackage = packageMap.get(packageIndex);
                final CrawledPackage crawledPackage = loadedPackage.getLoadedPackage();
                if (crawledPackage == null) {
                    throw new WTFException("CrawledPackage at Index " + packageIndex + " is missing!");
                }
                ret2.add(crawledPackage);
            }
            if (lcs != null && JsonConfig.create(GeneralSettings.class).isConvertRelativePathsJDRoot()) {
                convertPackageFoldersToCurrentJDRoot(lcs, ret2);
            }
            return new LinkedList<CrawledPackage>(ret2);
        } catch (final Throwable e) {
            if (e instanceof IOException) {
                throw (IOException) e;
            } else {
                throw new IOException(e);
            }
        } finally {
            try {
                if (zis != null) {
                    zis.close();
                } else if (fis != null) {
                    fis.close();
                }
            } catch (final Throwable ignore) {
            }
        }
    }

    /**
     * saves List of CrawledPackages to given File as ZippedJSon
     *
     * @param packages
     * @param file
     */
    private void save(final List<CrawledPackage> packages, final File destFile) {
        QUEUE.add(new ReadOnlyQueueAction<Void, RuntimeException>(Queue.QueuePriority.HIGH) {
            @Override
            protected Void run() throws RuntimeException {
                final boolean isShuttingDown = ShutdownController.getInstance().isShuttingDown();
                final boolean isLinkCollectorFile = destFile == null;
                File file = destFile;
                if (file == null) {
                    if (linkcollectorLists.size() > 0) {
                        String counter = new Regex(linkcollectorLists.get(0).getName(), "linkcollector(\\d+)\\.zip").getMatch(0);
                        long count = 1;
                        if (counter != null) {
                            count = Long.parseLong(counter) + 1;
                        }
                        file = Application.getResource("cfg/linkcollector" + count + ".zip");
                    }
                    if (file == null) {
                        file = Application.getResource("cfg/linkcollector.zip");
                    }
                }
                final SimpleMapper mapper = new SimpleMapper() {
                    @Override
                    protected void initMapper() {
                    }

                    @Override
                    public boolean isPrettyPrintEnabled() {
                        return false;
                    }
                };
                boolean deleteFile = true;
                ZipOutputStream zos = null;
                FileOutputStream fos = null;
                final long lastFileLength = linkcollectorLists.size() > 0 ? linkcollectorLists.get(0).length() : 0;
                final int bufferSize;
                if (lastFileLength > 0) {
                    final int paddedFileLength = (((int) lastFileLength / 32768) + 1) * 32768;
                    bufferSize = Math.max(32768, Math.min(1024 * 1024, paddedFileLength));
                } else {
                    bufferSize = 32768;
                }
                if (packages == null || file == null) {
                    return null;
                }
                try {
                    if (file.exists()) {
                        if (file.isDirectory()) {
                            throw new IOException("File " + file + " is a directory");
                        }
                        if (FileCreationManager.getInstance().delete(file, null) == false) {
                            throw new IOException("Could not delete file " + file);
                        }
                    } else {
                        if (file.getParentFile().exists() == false && FileCreationManager.getInstance().mkdir(file.getParentFile()) == false) {
                            throw new IOException("Could not create parentFolder for file " + file);
                        }
                    }
                    /*
                     * Entries of the zip file: "<packageIndex>" for a package, "<packageIndex>_<childIndex>" for a link of that package and
                     * "extraInfo". The indices are zero padded, so the entries stay in order.
                     */
                    /* prepare formatter for package filenames in zipfiles */
                    final String packageFormat;
                    if (packages.size() >= 10) {
                        packageFormat = String.format(Locale.ROOT, "%%0%dd", (int) Math.log10(packages.size()) + 1);
                    } else {
                        packageFormat = "%02d";
                    }
                    /* Set when the content equals the last saved list: the new file is thrown away, so the expensive fsync is skipped. */
                    final AtomicBoolean unchanged = new AtomicBoolean(false);
                    final MessageDigest digest = MessageDigest.getInstance("SHA-1");
                    fos = new FileOutputStream(file) {
                        @Override
                        public void close() throws IOException {
                            try {
                                if (!unchanged.get() && getChannel().isOpen()) {
                                    getChannel().force(true);
                                }
                            } finally {
                                super.close();
                            }
                        }
                    };
                    zos = new ZipOutputStream(new BufferedOutputStream(fos, bufferSize));
                    final ZipOutputStream finalZos = zos;
                    final OutputStream entryOutputStream = new OutputStream() {
                        @Override
                        public void write(int b) throws IOException {
                            digest.update((byte) b);
                            finalZos.write(b);
                        }

                        @Override
                        public void write(byte[] b, int off, int len) throws IOException {
                            digest.update(b, off, len);
                            finalZos.write(b, off, len);
                        }

                        @Override
                        public void close() throws IOException {
                            finalZos.flush();
                        }

                        @Override
                        public void flush() throws IOException {
                            finalZos.flush();
                        }
                    };
                    /* one pass instead of a linear search through all mapping lists for every single link */
                    final Map<CrawledLink, CrawledPackageMappingID> variousIDs = createIDLookup(variousMap);
                    final Map<CrawledLink, CrawledPackageMappingID> offlineIDs = createIDLookup(offlineMap);
                    int packageIndex = 0;
                    for (final CrawledPackage pkg : packages) {
                        final boolean readL = pkg.getModifyLock().readLock();
                        try {
                            final int childrenSize = pkg.getChildren().size();
                            if (childrenSize <= 0) {
                                /* empty packages are not saved */
                                continue;
                            }
                            final String packageEntryID = String.format(Locale.ROOT, packageFormat, packageIndex++);
                            {
                                /* convert CrawledPackage to JSON */
                                final CrawledPackageStorable packageStorable = new CrawledPackageStorable(pkg, false);
                                /* save packageID */
                                final CrawledPackageMappingID crawledPackageMappingID = packageMapPackageToID.get(pkg);
                                if (crawledPackageMappingID != null) {
                                    packageStorable.setPackageID(crawledPackageMappingID.getMappingID());
                                }
                                final ZipEntry packageEntry = new ZipEntry(packageEntryID);
                                packageEntry.setMethod(ZipEntry.DEFLATED);
                                final byte[] entryBytes = mapper.objectToByteArray(packageStorable);
                                packageEntry.setSize(entryBytes.length);
                                zos.putNextEntry(packageEntry);
                                entryOutputStream.write(entryBytes);
                                zos.closeEntry();
                            }
                            final String childFormat;
                            if (childrenSize >= 10) {
                                childFormat = String.format(Locale.ROOT, "%%0%dd", (int) Math.log10(childrenSize) + 1);
                            } else {
                                childFormat = "%02d";
                            }
                            int childIndex = 0;
                            for (final CrawledLink link : pkg.getChildren()) {
                                if (!isShuttingDown && DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                                    final QueueAction<?, ? extends Throwable> waiting = LinkCollector.this.getQueue().peek();
                                    if (waiting instanceof ReadOnlyQueueAction) {
                                        LinkCollector.this.getQueue().executeQueuedAction(waiting);
                                    }
                                }
                                final CrawledLinkStorable linkStorable = new CrawledLinkStorable(link);
                                CrawledPackageMappingID id = null;
                                switch (pkg.getType()) {
                                case VARIOUS:
                                    id = variousIDs.get(link);
                                    break;
                                case OFFLINE:
                                    id = offlineIDs.get(link);
                                    break;
                                default:
                                    break;
                                }
                                if (id != null) {
                                    linkStorable.setID(id.getMappingID());
                                }
                                final String childEntryID = String.format(Locale.ROOT, childFormat, childIndex++);
                                final ZipEntry linkEntry = new ZipEntry(packageEntryID + "_" + childEntryID);
                                linkEntry.setMethod(ZipEntry.DEFLATED);
                                final byte[] entryBytes = mapper.objectToByteArray(linkStorable);
                                linkEntry.setSize(entryBytes.length);
                                zos.putNextEntry(linkEntry);
                                entryOutputStream.write(entryBytes);
                                zos.closeEntry();
                            }
                        } finally {
                            pkg.getModifyLock().readUnlock(readL);
                        }
                    }
                    final LinkCollectorStorable lcs = new LinkCollectorStorable();
                    try {
                        /*
                         * set current RootPath of JDownloader, so we can update it when user moves JDownloader folder
                         */
                        lcs.setRootPath(JDUtilities.getJDHomeDirectoryFromEnvironment().getAbsolutePath());
                    } catch (final Throwable e) {
                        /* the method above can throw exceptions, eg in SVN */
                        logger.log(e);
                    }
                    /*
                     * The fingerprint must be taken before the extraInfo entry: it contains a timestamp that differs on every save and
                     * would make every list look changed.
                     */
                    final byte[] currentDigest = digest.digest();
                    final ZipEntry linkCollectorEntry = new ZipEntry("extraInfo");
                    linkCollectorEntry.setMethod(ZipEntry.DEFLATED);
                    zos.putNextEntry(linkCollectorEntry);
                    mapper.writeObject(entryOutputStream, lcs);
                    zos.closeEntry();
                    if (isLinkCollectorFile && linkcollectorLists.size() > 0 && MessageDigest.isEqual(lastSavedDigest, currentDigest)) {
                        /*
                         * Nothing changed since the last save: keep the existing lists untouched. Rotating now would only push an identical
                         * copy in and drop the oldest real backup.
                         */
                        unchanged.set(true);
                        logger.info("CollectorList unchanged since last save, nothing written: " + linkcollectorLists.get(0));
                        zos.close();
                        zos = null;
                        fos = null;
                        /* deleteFile stays true, the finally block removes the redundant file */
                        return null;
                    }
                    zos.close();
                    zos = null;
                    fos = null;
                    deleteFile = false;
                    lastSavedDigest = currentDigest;
                    try {
                        final int keepXOld = Math.max(JsonConfig.create(GeneralSettings.class).getKeepXOldLists(), 0);
                        while (linkcollectorLists.size() > keepXOld) {
                            final File remove = linkcollectorLists.remove(linkcollectorLists.size() - 1);
                            if (remove == null) {
                                continue;
                            }
                            final boolean delete = FileCreationManager.getInstance().delete(remove, null);
                            if (LogController.getInstance().isDebugMode()) {
                                logger.info("Delete outdated CollectorList: " + remove + " " + delete);
                            }
                        }
                    } catch (final Throwable e) {
                        logger.log(e);
                    } finally {
                        linkcollectorLists.add(0, file);
                    }
                    return null;
                } catch (final Throwable e) {
                    logger.log(e);
                } finally {
                    try {
                        if (zos != null) {
                            zos.close();
                        } else if (fos != null) {
                            fos.close();
                        }
                    } catch (final Throwable e) {
                        logger.log(e);
                    }
                    if (deleteFile && file.exists()) {
                        FileCreationManager.getInstance().delete(file, null);
                    }
                }
                return null;
            }
        });
    }

    private boolean isSavingAllowed(final boolean ignoreShutDown) {
        return CRAWLERLIST_LOADED.isReached() && (ignoreShutDown || !ShutdownController.getInstance().isShuttingDown()) && JsonConfig.create(GeneralSettings.class).isSaveLinkgrabberListEnabled();
    }

    /**
     * save the current CrawledPackages/CrawledLinks controlled by this LinkCollector
     */
    private void saveLinkCollectorLinks(final boolean ignoreShutdown) {
        if (!isSavingAllowed(ignoreShutdown)) {
            return;
        }
        /* save as new Json ZipFile */
        try {
            save(getPackagesCopy(), null);
        } catch (final Throwable e) {
            logger.log(e);
        }
    }

    public void setArchiver(ExtractionExtension archiver) {
        this.archiver = archiver;
    }

    @Override
    public void handleBrokenLink(CrawledLink link) {
    }

    @Override
    public void handleUnHandledLink(CrawledLink link) {
    }

    @Override
    public void nodeUpdated(AbstractNode source, jd.controlling.packagecontroller.AbstractNodeNotifier.NOTIFY notify, Object param) {
        super.nodeUpdated(source, notify, param);
        switch (notify) {
        case PROPERTY_CHANGE:
            if (param instanceof CrawledLinkProperty) {
                CrawledLinkProperty eventPropery = (CrawledLinkProperty) param;
                switch (eventPropery.getProperty()) {
                case NAME:
                case ENABLED:
                case AVAILABILITY:
                case PRIORITY:
                    /* These properties are part of the aggregated package view, which has to be recalculated */
                    final CrawledPackage parent = eventPropery.getCrawledLink().getParentNode();
                    if (parent != null) {
                        parent.getView().requestUpdate();
                    }
                    break;
                }
            }
            eventsender.fireEvent(new LinkCollectorEvent(LinkCollector.this, LinkCollectorEvent.TYPE.REFRESH_DATA, new Object[] { source, param }, QueuePriority.LOW));
            break;
        case STRUCTURE_CHANGE:
            eventsender.fireEvent(new LinkCollectorEvent(LinkCollector.this, LinkCollectorEvent.TYPE.REFRESH_STRUCTURE, new Object[] { source, param }, QueuePriority.LOW));
            break;
        }
    }

    @Override
    protected void _controllerPackageNodeStructureChanged(CrawledPackage pkg, QueuePriority priority) {
        eventsender.fireEvent(new LinkCollectorEvent(LinkCollector.this, LinkCollectorEvent.TYPE.REFRESH_STRUCTURE, pkg, priority));
    }

    private Object shutdownLock = new Object();

    @Override
    public void onShutdownVetoRequest(ShutdownRequest request) throws ShutdownVetoException {
        if (request.hasVetos()) {
            return;
        }
        if (request.isSilent()) {
            /* A silent shutdown cannot ask the user: veto while links are still being crawled or checked */
            synchronized (shutdownLock) {
                if (LinkChecker.isChecking() || LinkCrawler.isCrawling()) {
                    throw new ShutdownVetoException("LinkCollector is still running", this);
                }
            }
            return;
        }
        synchronized (shutdownLock) {
            if (!LinkChecker.isChecking() && !LinkCrawler.isCrawling()) {
                return;
            }
            if (!JDGui.bugme(WarnLevel.NORMAL)) {
                return;
            }
            if (!UIOManager.I().showConfirmDialog(Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN | UIOManager.LOGIC_DONT_SHOW_AGAIN_IGNORES_CANCEL, _JDT.T.LinkCollector_onShutdownRequest_(), _JDT.T.LinkCollector_onShutdownRequest_msg(), new AbstractIcon(IconKey.ICON_LINKGRABBER, 32), _JDT.T.literally_yes(), null)) {
                throw new ShutdownVetoException("LinkCollector is still running", this);
            }
        }
    }

    @Override
    public long getShutdownVetoPriority() {
        return 0;
    }

    @Override
    public void onShutdown(ShutdownRequest request) {
    }

    @Override
    public void onShutdownVeto(ShutdownRequest request) {
    }

    @Override
    public boolean hasNotificationListener() {
        return true;
    }

    /**
     * MANUAL: the user moves the links to the downloadlist. AUTO: the links are moved automatically (auto confirm), the defaults of the
     * auto confirm manager apply.
     */
    public static enum MoveLinksMode {
        MANUAL,
        AUTO
    }

    /**
     * Defines how a link is moved from the linkgrabber to the downloadlist, e.g. by a Packagizer rule or the "add links" action. Every mode
     * except UNCHANGED implies that the link is confirmed (moved to the downloadlist) - starting a download is only possible once the link
     * is in the downloadlist.
     */
    public static enum MoveToDownloadlistMode implements LabelInterface {
        /* Do not touch the link, the global settings apply. Not offered where "unchanged" is expressed differently (e.g. Packagizer). */
        UNCHANGED {
            @Override
            public String getLabel() {
                return _GUI.T.do_not_change_use_global_settings();
            }
        },
        /* Only move the link to the downloadlist */
        CONFIRM {
            @Override
            public String getLabel() {
                return _GUI.T.MoveToDownloadlistMode_CONFIRM();
            }
        },
        /* Move the link to the downloadlist and start the downloads normally */
        CONFIRM_AND_START {
            @Override
            public String getLabel() {
                return _GUI.T.MoveToDownloadlistMode_CONFIRM_AND_START();
            }
        },
        /* Move the link to the downloadlist and start its download forced */
        CONFIRM_AND_START_FORCED {
            @Override
            public String getLabel() {
                return _GUI.T.MoveToDownloadlistMode_CONFIRM_AND_START_FORCED();
            }
        };

        /**
         * Sets the auto confirm / auto start / forced auto start flags of the link according to this mode. The flags are set explicitly, so
         * a mode replaces the flags set by an earlier rule or action.
         */
        public void applyTo(final CrawledLink link) {
            switch (this) {
            case CONFIRM:
                link.setAutoConfirmEnabled(true);
                link.setAutoStartEnabled(false);
                link.setForcedAutoStartEnabled(false);
                break;
            case CONFIRM_AND_START:
                link.setAutoConfirmEnabled(true);
                link.setAutoStartEnabled(true);
                link.setForcedAutoStartEnabled(false);
                break;
            case CONFIRM_AND_START_FORCED:
                link.setAutoConfirmEnabled(true);
                link.setAutoStartEnabled(false);
                link.setForcedAutoStartEnabled(true);
                break;
            case UNCHANGED:
            default:
                break;
            }
        }
    }

    /**
     * This class describes, how a "move links to downloadlist" action shall behave. </br>
     * Examples of what it can influence: </br>
     * - define specific properties that should be set on the items to move e.g. set highest priority </br>
     * - define what should happen afterwards such as "force download-start of added items" </br>
     * - define what happens in linkgrabber afterwards such as "clean all remaining items in linkgrabber"
     */
    public final static class ConfirmLinksSettings {
        public static enum SwitchToDownloadlistBehavior implements LabelInterface {
            GLOBAL_DEFAULT {
                @Override
                public String getLabel() {
                    return _JDT.T.ConfirmLinksSettings_SwitchToDownloadlistBehavior_GLOBAL_DEFAULT();
                }
            },
            ALWAYS {
                @Override
                public String getLabel() {
                    return _JDT.T.ConfirmLinksSettings_SwitchToDownloadlistBehavior_ALWAYS();
                }
            },
            ONLY_IF_LINKGRABBER_EMPTY {
                @Override
                public String getLabel() {
                    return _JDT.T.ConfirmLinksSettings_SwitchToDownloadlistBehavior_ONLY_IF_LINKGRABBER_EMPTY();
                }
            },
            NEVER {
                @Override
                public String getLabel() {
                    return _JDT.T.ConfirmLinksSettings_SwitchToDownloadlistBehavior_NEVER();
                }
            };
        }

        public final MoveLinksMode getMoveLinksMode() {
            return moveLinksMode;
        }

        public final boolean getAutoStartDownloads() {
            return autoStartDownloads;
        }

        public final ConfirmLinksSettings setAutoStartDownloads(boolean autoStartDownloads) {
            this.autoStartDownloads = autoStartDownloads;
            return this;
        }

        public final boolean isForceDownloads() {
            return forceDownloads;
        }

        public final ConfirmLinksSettings setForceDownloads(boolean forceDownloads) {
            this.forceDownloads = forceDownloads;
            return this;
        }

        public final OnOfflineLinksAction getHandleOffline() {
            final OnOfflineLinksAction ret = handleOffline;
            if (ret != null) {
                return ret.getSelectedAction();
            }
            return defaultHandleOffline.getSelectedAction();
        }

        public final ConfirmLinksSettings setHandleOffline(OnOfflineLinksAction handleOffline) {
            if (handleOffline == null || handleOffline.getSelectedAction() == defaultHandleOffline.getSelectedAction()) {
                this.handleOffline = null;
            } else {
                this.handleOffline = handleOffline;
            }
            return this;
        }

        public final boolean isClearLinkgrabberlistOnConfirm() {
            return clearLinkgrabberlistOnConfirm;
        }

        public final ConfirmLinksSettings setClearLinkgrabberlistOnConfirm(boolean clearLinkgrabberlistOnConfirm) {
            this.clearLinkgrabberlistOnConfirm = clearLinkgrabberlistOnConfirm;
            return this;
        }

        /**
         * Never returns GLOBAL_DEFAULT - resolves it to whatever {@link #getDefaultSwitchToDownloadlistBehavior()} currently computes (i.e.
         * the global default config), same as every other getter here resolving unset values to their default.
         */
        public final SwitchToDownloadlistBehavior getSwitchToDownloadlistBehavior() {
            final SwitchToDownloadlistBehavior ret = switchToDownloadlistBehavior;
            if (ret == null) {
                return defaultSwitchToDownloadlistBehavior;
            }
            if (ret == SwitchToDownloadlistBehavior.GLOBAL_DEFAULT) {
                return getDefaultSwitchToDownloadlistBehavior();
            }
            return ret;
        }

        public final ConfirmLinksSettings setSwitchToDownloadlistBehavior(SwitchToDownloadlistBehavior switchToDownloadlistBehavior) {
            if (switchToDownloadlistBehavior == null || switchToDownloadlistBehavior == defaultSwitchToDownloadlistBehavior) {
                this.switchToDownloadlistBehavior = null;
            } else {
                this.switchToDownloadlistBehavior = switchToDownloadlistBehavior;
            }
            return this;
        }

        public final Priority getPriority() {
            final Priority ret = priority;
            if (ret != null) {
                return ret;
            }
            return defaultPriority;
        }

        public final ConfirmLinksSettings setPriority(Priority priority) {
            if (priority == null || priority == defaultPriority) {
                this.priority = null;
            } else {
                this.priority = priority;
            }
            return this;
        }

        public final ConfirmLinksSettings setHandleDupes(OnDupesLinksAction handleDupes) {
            if (handleDupes == null || handleDupes.getSelectedAction() == defaultHandleDupes.getSelectedAction()) {
                this.handleDupes = null;
            } else {
                this.handleDupes = handleDupes;
            }
            return this;
        }

        public final OnDupesLinksAction getHandleDupes() {
            final OnDupesLinksAction ret = handleDupes;
            if (ret != null) {
                return ret.getSelectedAction();
            }
            return defaultHandleDupes.getSelectedAction();
        }

        public PackageExpandBehavior getPackageExpandBehavior() {
            final PackageExpandBehavior ret = packageExpandBehavior;
            if (ret != null) {
                return ret;
            }
            return defaultPackageExpandBehavior;
        }

        public ConfirmLinksSettings setPackageExpandBehavior(PackageExpandBehavior packageExpandBehavior) {
            if (packageExpandBehavior == null || packageExpandBehavior == defaultPackageExpandBehavior) {
                this.packageExpandBehavior = null;
            } else {
                this.packageExpandBehavior = packageExpandBehavior;
            }
            return this;
        }

        public final ConfirmationDialogBehavior getConfirmationDialogBehavior() {
            final ConfirmationDialogBehavior ret = confirmationDialogBehavior;
            if (ret != null) {
                return ret;
            }
            return defaultConfirmationDialogBehavior;
        }

        public final ConfirmLinksSettings setConfirmationDialogBehavior(ConfirmationDialogBehavior confirmationDialogBehavior) {
            if (confirmationDialogBehavior == null || confirmationDialogBehavior == defaultConfirmationDialogBehavior) {
                this.confirmationDialogBehavior = null;
            } else {
                this.confirmationDialogBehavior = confirmationDialogBehavior;
            }
            return this;
        }

        public final int getConfirmationDialogThresholdMinPackages() {
            final Integer ret = confirmationDialogThresholdMinPackages;
            if (ret != null) {
                return ret.intValue();
            }
            return defaultConfirmationDialogThresholdMinPackages;
        }

        public final ConfirmLinksSettings setConfirmationDialogThresholdMinPackages(Integer confirmationDialogThresholdMinPackages) {
            if (confirmationDialogThresholdMinPackages == null || confirmationDialogThresholdMinPackages.intValue() < 0 || confirmationDialogThresholdMinPackages.intValue() == defaultConfirmationDialogThresholdMinPackages) {
                this.confirmationDialogThresholdMinPackages = null;
            } else {
                this.confirmationDialogThresholdMinPackages = confirmationDialogThresholdMinPackages;
            }
            return this;
        }

        public final int getConfirmationDialogThresholdMinLinks() {
            final Integer ret = confirmationDialogThresholdMinLinks;
            if (ret != null) {
                return ret.intValue();
            }
            return defaultConfirmationDialogThresholdMinLinks;
        }

        public final ConfirmLinksSettings setConfirmationDialogThresholdMinLinks(Integer confirmationDialogThresholdMinLinks) {
            if (confirmationDialogThresholdMinLinks == null || confirmationDialogThresholdMinLinks.intValue() < 0 || confirmationDialogThresholdMinLinks.intValue() == defaultConfirmationDialogThresholdMinLinks) {
                this.confirmationDialogThresholdMinLinks = null;
            } else {
                this.confirmationDialogThresholdMinLinks = confirmationDialogThresholdMinLinks;
            }
            return this;
        }

        public boolean isMergeSameNamedPackagesInDownloadlistInExistingPackagesOnConfirm() {
            return mergeSameNamedPackagesInDownloadlistInExistingPackagesOnConfirm;
        }

        public final ConfirmLinksSettings setMergeSameNamedPackagesInDownloadlistInExistingPackagesOnConfirm(boolean mergeSameNamedPackagesInDownloadlistInExistingPackagesOnConfirm) {
            this.mergeSameNamedPackagesInDownloadlistInExistingPackagesOnConfirm = mergeSameNamedPackagesInDownloadlistInExistingPackagesOnConfirm;
            return this;
        }

        private final MoveLinksMode                moveLinksMode;
        /*
         * The defaults below depend on moveLinksMode. Field initializers run before the constructor body, so they are assigned in the
         * constructor after moveLinksMode has been set. Otherwise getMoveLinksMode() would still return null.
         */
        private final boolean                      defaultAutoStartDownloads;
        private boolean                            autoStartDownloads;
        private final boolean                      defaultForceDownloads;
        private boolean                            forceDownloads;
        private final boolean                      defaultClearLinkgrabberlistOnConfirm;
        private boolean                            clearLinkgrabberlistOnConfirm;
        private final Priority                     defaultPriority;
        private Priority                           priority                                                               = null;
        private final SwitchToDownloadlistBehavior defaultSwitchToDownloadlistBehavior                                    = getDefaultSwitchToDownloadlistBehavior();
        private SwitchToDownloadlistBehavior       switchToDownloadlistBehavior                                           = null;
        private final OnOfflineLinksAction         defaultHandleOffline                                                   = getDefaultHandleOffline();
        private OnOfflineLinksAction               handleOffline                                                          = null;
        private final OnDupesLinksAction           defaultHandleDupes                                                     = getDefaultHandleDupes();
        private OnDupesLinksAction                 handleDupes                                                            = null;
        private final PackageExpandBehavior        defaultPackageExpandBehavior                                           = getDefaultPackageExpandBehavior();
        private PackageExpandBehavior              packageExpandBehavior                                                  = null;
        private final ConfirmationDialogBehavior   defaultConfirmationDialogBehavior                                      = getDefaultConfirmationDialogBehavior();
        private ConfirmationDialogBehavior         confirmationDialogBehavior                                             = null;
        private final int                          defaultConfirmationDialogThresholdMinPackages                          = getDefaultcConfirmationDialogThresholdMinPackages();
        private Integer                            confirmationDialogThresholdMinPackages                                 = null;
        private final int                          defaultConfirmationDialogThresholdMinLinks                             = getDefaultConfirmationDialogThresholdMinLinks();
        private Integer                            confirmationDialogThresholdMinLinks                                    = null;
        private final boolean                      defaultMergeSameNamedPackagesInDownloadlistInExistingPackagesOnConfirm = getDefaultMergeSameNamedPackagesInDownloadlistInExistingPackagesOnConfirm();
        private boolean                            mergeSameNamedPackagesInDownloadlistInExistingPackagesOnConfirm        = defaultMergeSameNamedPackagesInDownloadlistInExistingPackagesOnConfirm;

        public ConfirmLinksSettings(final MoveLinksMode mode) {
            this.moveLinksMode = mode != null ? mode : MoveLinksMode.AUTO;
            this.defaultAutoStartDownloads = getDefaultAutoStartDownloads();
            this.autoStartDownloads = defaultAutoStartDownloads;
            this.defaultForceDownloads = getDefaultForceDownloads();
            this.forceDownloads = defaultForceDownloads;
            this.defaultClearLinkgrabberlistOnConfirm = getDefaultClearLinkgrabberlistOnConfirm();
            this.clearLinkgrabberlistOnConfirm = defaultClearLinkgrabberlistOnConfirm;
            this.defaultPriority = getDefaultPriority();
        }

        private int getDefaultConfirmationDialogThresholdMinLinks() {
            return 1;
        }

        private int getDefaultcConfirmationDialogThresholdMinPackages() {
            return 1;
        }

        private ConfirmationDialogBehavior getDefaultConfirmationDialogBehavior() {
            return ConfirmationDialogBehavior.DISABLED;
        }

        private PackageExpandBehavior getDefaultPackageExpandBehavior() {
            return PackageExpandBehavior.UNCHANGED;
        }

        private OnDupesLinksAction getDefaultHandleDupes() {
            final OnDupesLinksAction ret = CFG_LINKGRABBER.CFG.getDefaultOnAddedDupesLinksAction();
            if (ret != null) {
                return ret.getSelectedAction();
            }
            return OnDupesLinksAction.ASK;
        }

        private OnOfflineLinksAction getDefaultHandleOffline() {
            final OnOfflineLinksAction ret = CFG_LINKGRABBER.CFG.getDefaultOnAddedOfflineLinksAction();
            if (ret != null) {
                return ret.getSelectedAction();
            }
            return OnOfflineLinksAction.ASK;
        }

        /**
         * Never returns GLOBAL_DEFAULT - it resolves the global "AutoSwitchToDownloadTableOnConfirmDefaultEnabled" config into a concrete
         * ALWAYS/NEVER, which is what GLOBAL_DEFAULT (as chosen by the user) falls back to.
         */
        private SwitchToDownloadlistBehavior getDefaultSwitchToDownloadlistBehavior() {
            if (JsonConfig.create(LinkgrabberSettings.class).isAutoSwitchToDownloadTableOnConfirmDefaultEnabled()) {
                return SwitchToDownloadlistBehavior.ALWAYS;
            } else {
                return SwitchToDownloadlistBehavior.NEVER;
            }
        }

        private Priority getDefaultPriority() {
            if (MoveLinksMode.AUTO.equals(getMoveLinksMode()) && CFG_LINKGRABBER.CFG.isAutoConfirmManagerAssignPriorityEnabled()) {
                final Priority ret = CFG_LINKGRABBER.CFG.getAutoConfirmManagerPriority();
                if (ret != null) {
                    return ret;
                }
            }
            return Priority.DEFAULT;
        }

        private boolean getDefaultAutoStartDownloads() {
            if (MoveLinksMode.AUTO.equals(getMoveLinksMode())) {
                final AutoStartOptions ret = CFG_LINKGRABBER.CFG.getAutoConfirmManagerAutoStart();
                if (ret != null) {
                    return ret.isEnabled();
                }
            }
            return AutoStartOptions.AUTO.isEnabled();
        }

        private boolean getDefaultClearLinkgrabberlistOnConfirm() {
            if (MoveLinksMode.AUTO.equals(getMoveLinksMode())) {
                return CFG_LINKGRABBER.CFG.isAutoConfirmManagerClearListAfterConfirm();
            }
            return false;
        }

        private boolean getDefaultForceDownloads() {
            if (MoveLinksMode.AUTO.equals(getMoveLinksMode())) {
                return CFG_LINKGRABBER.CFG.isAutoConfirmManagerForceDownloads();
            }
            return false;
        }

        private boolean getDefaultMergeSameNamedPackagesInDownloadlistInExistingPackagesOnConfirm() {
            return CFG_LINKGRABBER.CFG.isMergeSameNamedPackagesInDownloadlistInExistingPackagesOnConfirmDefaultEnabled();
        }
    }

    /**
     * Moves the selected links from the linkgrabber to the downloadlist and optionally starts or forces their downloads, depending on the
     * given settings.
     */
    public void moveLinksToDownloadList(final SelectionInfo<CrawledPackage, CrawledLink> selection, final ConfirmLinksSettings moveLinksSettings) {
        final List<FilePackage> filePackagesToAdd = new ArrayList<FilePackage>();
        final List<DownloadLink> force = new ArrayList<DownloadLink>();
        final boolean forcedAutoStart = moveLinksSettings.isForceDownloads();
        final boolean autoMode = MoveLinksMode.AUTO.equals(moveLinksSettings.getMoveLinksMode());
        boolean autoStartLinks = false;
        /* convert all selected CrawledLinks to FilePackages */
        for (final PackageView<CrawledPackage, CrawledLink> packageView : selection.getPackageViews()) {
            final List<CrawledLink> links = packageView.getChildren();
            final List<FilePackage> convertedLinks = LinkCollector.getInstance().convert(links, true);
            /*
             * Guard on the chosen target priority (moveLinksSettings.getPriority()), not on the link's current priority: a deliberately
             * assigned priority must win even for links that already carry a non-default priority. getPriority() never returns null, so the
             * old "!= null" check was dead and the old "cl.getPriority() == DEFAULT" check silently dropped the assigned priority for
             * already-prioritized links.
             */
            final Priority targetPriority = moveLinksSettings.getPriority();
            final boolean assignTargetPriority = targetPriority != null && !Priority.DEFAULT.equals(targetPriority);
            for (final CrawledLink cl : links) {
                autoStartLinks |= cl.isAutoStartEnabled();
                if (forcedAutoStart || (autoMode && cl.isForcedAutoStartEnabled())) {
                    force.add(cl.getDownloadLink());
                }
                if (assignTargetPriority) {
                    cl.setPriority(targetPriority);
                }
            }
            if (convertedLinks != null) {
                filePackagesToAdd.addAll(convertedLinks);
            }
        }
        final PackageExpandBehavior packageExpandBehavior = moveLinksSettings.getPackageExpandBehavior();
        if (packageExpandBehavior != PackageExpandBehavior.UNCHANGED) {
            /* Collapse/expand package */
            final boolean expand = packageExpandBehavior == PackageExpandBehavior.EXPANDED;
            for (final FilePackage fp : filePackagesToAdd) {
                fp.setExpanded(expand);
            }
        }
        final boolean addTop = org.jdownloader.settings.staticreferences.CFG_LINKGRABBER.LINKGRABBER_ADD_AT_TOP.isEnabled();
        /* add the converted FilePackages to DownloadController */
        /**
         * addTop = 0, to insert the packages at the top
         *
         * addBottom = negative number -> add at the end
         */
        final boolean finalAutoStart;
        if (autoMode) {
            finalAutoStart = autoStartLinks || moveLinksSettings.getAutoStartDownloads();
        } else {
            finalAutoStart = moveLinksSettings.getAutoStartDownloads();
        }
        DownloadController.getInstance().addAllAt(filePackagesToAdd, addTop ? 0 : -(filePackagesToAdd.size() + 10));
        if (moveLinksSettings.isMergeSameNamedPackagesInDownloadlistInExistingPackagesOnConfirm() && filePackagesToAdd.size() > 0) {
            /*
             * Fold each freshly added package into an existing same named package in the downloadlist. The merge target follows the insert
             * position: with "add at top" the topmost existing package is used, otherwise the bottom-most one. Runs on the same
             * DownloadController queue as addAllAt above, so the added packages are already present when this runs.
             */
            DownloadController.getInstance().mergeAddedPackagesIntoExistingSameNamedPackages(filePackagesToAdd, addTop);
        }
        DownloadController.getInstance().getQueue().add(new QueueAction<Void, RuntimeException>() {
            @Override
            protected Void run() throws RuntimeException {
                if (force.size() == 0) {
                    if (finalAutoStart) {
                        DownloadWatchDog.getInstance().startDownloads();
                    }
                    return null;
                }
                DownloadWatchDog.getInstance().forceDownload(force);
                if (!finalAutoStart) {
                    return null;
                }
                DownloadWatchDog.getInstance().enqueueJob(new DownloadWatchDogJob() {
                    @Override
                    public void interrupt() {
                    }

                    @Override
                    public void execute(DownloadSession currentSession) {
                        currentSession.setForcedOnlyModeEnabled(false);
                    }

                    @Override
                    public boolean isHighPriority() {
                        return false;
                    }
                });
                return null;
            }
        });
    }

    public void removeChildren(final List<CrawledLink> removechildren) {
        if (removechildren == null || removechildren.size() == 0) {
            /* Do nothing */
            return;
        }
        QUEUE.add(new QueueAction<Void, RuntimeException>() {
            @Override
            protected Void run() throws RuntimeException {
                internalRemoveChildren(removechildren);
                _controllerStructureChanged(this.getQueuePrio());
                return null;
            }
        });
    }

    /**
     * Returns a thread that crawls the job. If nothing could be handled but there are unhandled links, the user is asked whether these
     * should be analyzed deeper (or, for configured origins with a single link, the file extension is learned automatically).
     *
     * @param lcReference
     *            optional, receives the crawler(s) used by the thread
     */
    public Thread getAddLinksThread(final LinkCollectingJob job, final AtomicReference<LinkCrawler> lcReference) {
        return new Thread("AddLinksThread:" + job.getOrigin().getOrigin()) {
            private final HashSet<String> autoExtensionLearnBlackList = new HashSet<String>();
            {
                autoExtensionLearnBlackList.add("shtml");
                autoExtensionLearnBlackList.add("phtml");
                autoExtensionLearnBlackList.add("html");
                autoExtensionLearnBlackList.add("htm");
                autoExtensionLearnBlackList.add("php");
                autoExtensionLearnBlackList.add("js");
                autoExtensionLearnBlackList.add("css");
            }

            public void run() {
                LinkCrawler lc = LinkCollector.getInstance().addCrawlerJob(job);
                if (lcReference != null) {
                    lcReference.set(lc);
                }
                if (lc == null) {
                    return;
                }
                lc.waitForCrawling();
                if (job.isDeepAnalyse()) {
                    return;
                }
                if (lc.getProcessedLinksCounter() != 0 || lc.getUnhandledLinksFoundCounter() <= 0) {
                    return;
                }
                final List<CrawledLink> unhandledLinks = new ArrayList<CrawledLink>(lc.getUnhandledLinks());
                final LinkOrigin origin = job.getOrigin().getOrigin();
                for (CrawledLink unhandledLink : unhandledLinks) {
                    unhandledLink.setCrawlDeep(true);
                }
                final String[] origins = LinkCrawler.getConfig().getAutoLearnExtensionOrigins();
                final boolean autoExtensionLearning;
                if (origins != null && unhandledLinks.size() == 1) {
                    autoExtensionLearning = Arrays.asList(origins).contains(origin.name());
                } else {
                    autoExtensionLearning = false;
                }
                if (!autoExtensionLearning) {
                    try {
                        final ConfirmDialog dialog = new ConfirmDialog(0, _GUI.T.AddLinksAction_actionPerformed_deep_title(), _GUI.T.AddLinksAction_actionPerformed_deep_msg(), null, _GUI.T.literally_yes(), _GUI.T.literall_no());
                        dialog.show().throwCloseExceptions();
                    } catch (DialogNoAnswerException e) {
                        e.printStackTrace();
                        if (!e.isCausedByDontShowAgain()) {
                            return;
                        }
                    }
                }
                lc = LinkCollector.getInstance().addCrawlerJob(unhandledLinks, job);
                if (lcReference != null) {
                    lcReference.set(lc);
                }
                if (lc == null) {
                    return;
                }
                if (autoExtensionLearning) {
                    final LinkCrawlerDeepInspector defaultDeepInspector = lc.defaultDeepInspector();
                    lc.setDeepInspector(new LinkCrawlerDeepInspector() {
                        private final LinkCrawlerRule getDirectHTTPRule(LinkCrawler lc, final URLConnectionAdapter urlConnection) {
                            final List<LinkCrawlerRule> rules = lc.getLinkCrawlerRules();
                            if (rules == null) {
                                return null;
                            }
                            final URL url = urlConnection.getURL();
                            for (final LinkCrawlerRule rule : rules) {
                                if (RULE.DIRECTHTTP.equals(rule.getRule()) && rule.matches(url)) {
                                    return rule;
                                }
                            }
                            return null;
                        }

                        @Override
                        public List<CrawledLink> deepInspect(LinkCrawler lc, final LinkCrawler.LinkCrawlerGeneration generation, Browser br, URLConnectionAdapter urlConnection, CrawledLink link) throws Exception {
                            if (urlConnection.getResponseCode() != 200 || urlConnection.getRequest().getLocation() != null) {
                                return defaultDeepInspector.deepInspect(lc, generation, br, urlConnection, link);
                            }
                            final LinkCrawlerRule matchingRule = link.getMatchingRule();
                            if (matchingRule != null || !looksLikeDownloadableContent(urlConnection)) {
                                return defaultDeepInspector.deepInspect(lc, generation, br, urlConnection, link);
                            }
                            LinkCrawlerRule rule = null;
                            final URL url = urlConnection.getURL();
                            if (url.getPath() != null && url.getPath().matches(".*\\.(php|aspx)$") && url.getQuery() != null) {
                                /* hoster.domain/script.php?somevalue=somekey.....->Download */
                                if ((rule = getDirectHTTPRule(lc, urlConnection)) == null) {
                                    final String domain = Browser.getHost(url, false);
                                    rule = new LinkCrawlerRule();
                                    rule.setName("Learned php script download: " + domain + url.getPath());
                                    rule.setPattern("(?i)https?://.*?" + Pattern.quote(domain) + Pattern.quote(url.getPath()) + "\\?.+");
                                    rule.setRule(RULE.DIRECTHTTP);
                                    lc.addLinkCrawlerRule(rule);
                                }
                            } else {
                                final String fileName = Plugin.getFileNameFromURL(url);
                                final String fileExtension = Files.getExtension(fileName);
                                if (StringUtils.isNotEmpty(fileExtension) && !autoExtensionLearnBlackList.contains(fileExtension)) {
                                    if ((rule = getDirectHTTPRule(lc, urlConnection)) == null) {
                                        rule = new LinkCrawlerRule();
                                        rule.setName("Learned file extension: " + fileExtension);
                                        rule.setPattern("(?i)https?://.*\\." + Pattern.quote(fileExtension) + "($|\\?.*$)");
                                        rule.setRule(RULE.DIRECTHTTP);
                                        lc.addLinkCrawlerRule(rule);
                                    }
                                }
                            }
                            urlConnection.disconnect();
                            final ArrayList<CrawledLink> ret = new ArrayList<CrawledLink>();
                            final CrawledLink direct = lc.createDirectHTTPCrawledLink(link, null, urlConnection);
                            if (direct != null) {
                                direct.setMatchingRule(rule);
                                ret.add(direct);
                            }
                            return ret;
                        }
                    });
                }
                lc.waitForCrawling();
            }
        };
    }

    /**
     * Resets the linkgrabber as requested (delete links, cancel crawler jobs, clear filtered links, reset search and sorting). The user is
     * asked first, unless the dialog is bypassed.
     */
    public static void requestDeleteLinks(final List<CrawledLink> nodesToDelete, final boolean containsOnline, final String string, final boolean byPassDialog, final boolean isCancelLinkcrawlerJobs, final boolean isResetTableSorter, final boolean isClearSearchFilter, final boolean isClearFilteredLinks) {
        TaskQueue.getQueue().add(new QueueAction<Void, RuntimeException>() {
            @Override
            protected Void run() throws RuntimeException {
                boolean taskToDo = false;
                taskToDo = taskToDo || (nodesToDelete.size() > 0);
                if (!Application.isHeadless()) {
                    taskToDo = taskToDo || ((isClearSearchFilter) && !LinkgrabberSearchField.getInstance().isEmpty());
                }
                taskToDo = taskToDo || ((isResetTableSorter) && LinkGrabberTableModel.getInstance().getSortColumn() != null);
                taskToDo = taskToDo || ((isCancelLinkcrawlerJobs) && LinkCollector.getInstance().isCollecting());
                taskToDo = taskToDo || ((isClearFilteredLinks) && LinkCollector.getInstance().getfilteredStuffSize() > 0);
                if (!taskToDo) {
                    if (!Application.isHeadless()) {
                        Toolkit.getDefaultToolkit().beep();
                    }
                    return null;
                }
                final WarnLevel level;
                if (containsOnline) {
                    level = WarnLevel.NORMAL;
                } else {
                    level = WarnLevel.LOW;
                }
                final boolean finalByPassDialog = !JDGui.bugme(level) || byPassDialog;
                if (finalByPassDialog || CFG_GUI.CFG.isBypassAllRlyDeleteDialogsEnabled()) {
                    if (isCancelLinkcrawlerJobs) {
                        LinkCollector.getInstance().abort();
                    }
                    if (isResetTableSorter) {
                        new EDTRunner() {
                            @Override
                            protected void runInEDT() {
                                final LinkGrabberTable table = MenuManagerLinkgrabberTableContext.getInstance().getTable();
                                table.getModel().setSortColumn(null);
                                table.getModel().refreshSort();
                                table.getTableHeader().repaint();
                            }
                        };
                    }
                    if (isClearSearchFilter) {
                        LinkgrabberSearchField.getInstance().setText("");
                        LinkgrabberSearchField.getInstance().onChanged();
                    }
                    if (isClearFilteredLinks) {
                        LinkCollector.getInstance().clearFilteredLinks();
                    }
                    if (nodesToDelete.size() > 0) {
                        LinkCollector.getInstance().removeChildren(nodesToDelete);
                    }
                    return null;
                }
                final GenericResetLinkgrabberRlyDialog dialog = new GenericResetLinkgrabberRlyDialog(nodesToDelete, containsOnline, string, isCancelLinkcrawlerJobs, isClearFilteredLinks, isClearSearchFilter, isResetTableSorter);
                try {
                    Dialog.getInstance().showDialog(dialog);
                    if (dialog.isCancelCrawler()) {
                        LinkCollector.getInstance().abort();
                    }
                    if (dialog.isResetSort()) {
                        new EDTRunner() {
                            @Override
                            protected void runInEDT() {
                                final LinkGrabberTable table = MenuManagerLinkgrabberTableContext.getInstance().getTable();
                                table.getModel().setSortColumn(null);
                                table.getModel().refreshSort();
                                table.getTableHeader().repaint();
                            }
                        };
                    }
                    if (dialog.isResetSearch()) {
                        LinkgrabberSearchField.getInstance().setText("");
                        LinkgrabberSearchField.getInstance().onChanged();
                    }
                    if (dialog.isDeleteLinks()) {
                        LinkCollector.getInstance().removeChildren(nodesToDelete);
                    }
                    if (dialog.isClearFiltered()) {
                        LinkCollector.getInstance().clearFilteredLinks();
                    }
                } catch (DialogClosedException e) {
                    e.printStackTrace();
                } catch (DialogCanceledException e) {
                    e.printStackTrace();
                }
                return null;
            }
        });
    }

    public void setActiveVariantForLink(final CrawledLink crawledLink, final LinkVariant linkVariant) {
        getQueue().add(new QueueAction<Void, RuntimeException>() {
            @Override
            protected Void run() throws RuntimeException {
                CrawledPackage parent = crawledLink.getParentNode();
                if (parent == null || parent.getControlledBy() != LinkCollector.this) {
                    /* link is no longer controlled by this controller */
                    return null;
                }
                if (!crawledLink.hasVariantSupport()) {
                    /* link does not support variants */
                    return null;
                }
                setActiveVariantForLink(crawledLink.getDownloadLink(), linkVariant);
                List<CheckableLink> checkableLinks = new ArrayList<CheckableLink>(1);
                checkableLinks.add(crawledLink);
                LinkChecker<CheckableLink> linkChecker = new LinkChecker<CheckableLink>(true);
                linkChecker.check(checkableLinks);
                return null;
            }
        });
    }

    public void setActiveVariantForLink(final DownloadLink link, final LinkVariant variant) {
        getQueue().add(new QueueAction<Void, RuntimeException>() {
            @Override
            protected Void run() throws RuntimeException {
                final LinkVariant oldVariant = link.getDefaultPlugin().getActiveVariantByLink(link);
                link.getDefaultPlugin().setActiveVariantByLink(link, variant);
                final String newLinkID = link.getLinkID();
                /*
                 * getExistingCrawledLinkByLinkIDOnDupeManagerEnabled returns null while the dupe manager is disabled -> any variant is
                 * allowed
                 */
                final CrawledLink existing = getExistingCrawledLinkByLinkIDOnDupeManagerEnabled(newLinkID);
                if (existing == null || link == existing.getDownloadLink()) {
                    return null;
                }
                logger.info("Dupecheck Filtered Variant");
                /* A link with this variant already exists, restore the old variant */
                link.getDefaultPlugin().setActiveVariantByLink(link, oldVariant);
                return null;
            }
        });
    }

    /**
     * Unregisters the given link from the dupeCheckMap.
     *
     * @return true if a set for the link's linkID existed and was processed, false if there was no entry to remove at all (nothing to do).
     *         The return value is currently not evaluated by any caller.
     */
    private boolean removeCrawledLinkByLinkID(final CrawledLink link) {
        /*
         * Known limitation: the link is looked up under its CURRENT linkID. If the linkID changed after the link was registered (e.g. after
         * a link check or a variant change), the old registration is not found here and stays in the set under the old ID. It is only
         * cleaned up when the weak reference is collected or when getExistingCrawledLinkByLinkID detects the pollution under the old ID.
         * Until then the new ID is not recognized as a dupe of this entry.
         */
        final String linkID = link.getLinkID();
        final Set<CrawledLink> set = dupeCheckMap.get(linkID);
        if (set == null) {
            return false;
        }
        synchronized (set) {
            set.remove(link);
            if (set.isEmpty()) {
                dupeCheckMap.remove(linkID);
            }
        }
        return true;
    }

    /**
     * Whether the dupe manager is enabled. It controls duplicate <b>avoidance</b> only, not duplicate detection: when enabled, duplicates
     * are kept out of the linkgrabber (the avoidance consumers funnel through
     * {@link #getExistingCrawledLinkByLinkIDOnDupeManagerEnabled(String)}); when disabled, duplicates are allowed in and merely surfaced
     * via {@link #isLinkgrabberDupe(CrawledLink)}. <br>
     * The value is read once from the config at construction time and stays fixed for the lifetime of this LinkCollector.
     */
    public boolean isDupeManagerEnabled() {
        return isDupeManagerEnabled;
    }

    /**
     * Returns the set of currently collected CrawledLinks that share the given linkID (the raw dupeCheckMap entry), or null if none are
     * registered under it. The returned set must only be iterated/mutated while synchronizing on it (see {@link #dupeCheckMap}).
     */
    private Set<CrawledLink> getLinkgrabberDupes(final String linkID) {
        return dupeCheckMap.get(linkID);
    }

    /**
     * Returns true if the given link is a duplicate inside the linkgrabber, i.e. its linkID is that of at least two links currently present
     * as children of a package. In that case every occurrence - including the first one - is reported as a duplicate.
     *
     * The answer is read straight from the always-live {@link #dupeCheckMap}, so no per-lookup scan of the whole linkgrabber is required
     * and it is purely occurrence-based - independent of the dupe manager mode. While the dupe manager is enabled duplicates are normally
     * blocked at add time and thus never reach two occurrences, but if duplicates do exist anyway (e.g. loaded from a list saved while the
     * manager was disabled) they are still reported. Only currently registered package children are counted; filtered-out links (which have
     * no parent node) are ignored, matching the set of links the view filter operates on.
     */
    public boolean isLinkgrabberDupe(final CrawledLink link) {
        /**
         * TODO: Remove code down below: See even if dupe manager is enabled and dupes are avoided, dupes can theoretically happen if: <br>
         * - user imports a linkgrabber backup(?) <br>
         * - user disables dupe manager -> Adds dupe links -> Enables dupe manager -> dupe links are still left in linkgrabber <br>
         * - link-ids change e.g. after a link-check
         */
        // if (isDupeManagerEnabled()) {
        // /* Duplicates are not allowed -> No need for us to check */
        // // TODO: Remove this check if user is allowed to enable/disable dupe manager during runtime
        // return false;
        // }
        final Set<CrawledLink> set = getLinkgrabberDupes(link.getLinkID());
        if (set == null) {
            return false;
        }
        // Count only links that are still attached to a controlled package; filtered-out links (parent node null) are ignored. Iteration
        // synchronizes on the set (its own monitor), so counting from the view-filter thread while the IOEQ adds/removes is safe.
        int occurrences = 0;
        synchronized (set) {
            for (final CrawledLink itemLink : set) {
                final CrawledPackage parent = itemLink.getParentNode();
                if (parent == null || parent.getControlledBy() == null) {
                    continue;
                }
                if (++occurrences >= 2) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Central gate for duplicate avoidance: with the dupe manager disabled this returns null, so every avoidance consumer that funnels
     * through here sees "no existing link" and duplicates are allowed in. The dupeCheckMap itself is still maintained; isLinkgrabberDupe
     * reads it directly (not via this method) and therefore keeps reflecting the actual occurrence count. <br>
     * While the dupe manager is enabled this delegates to {@link #getExistingCrawledLinkByLinkID(String)}.
     */
    private CrawledLink getExistingCrawledLinkByLinkIDOnDupeManagerEnabled(final String linkID) {
        if (!isDupeManagerEnabled()) {
            return null;
        }
        return getExistingCrawledLinkByLinkID(linkID);
    }

    /**
     * Returns the first still-registered link sharing the given linkID (or null), pruning GC'd/re-homed entries. Ignores the dupe manager
     * mode.
     */
    private CrawledLink getExistingCrawledLinkByLinkID(final String linkID) {
        final Set<CrawledLink> set = getLinkgrabberDupes(linkID);
        if (set == null) {
            return null;
        }
        CrawledLink ret = null;
        List<CrawledLink> reHome = null;
        synchronized (set) {
            final Iterator<CrawledLink> it = set.iterator();
            while (it.hasNext()) {
                final CrawledLink itemLink = it.next();
                if (StringUtils.equals(itemLink.getLinkID(), linkID)) {
                    if (ret == null) {
                        ret = itemLink;
                    }
                    continue;
                }
                /* TODO: Maybe remove this as this covers a rare edge case */
                /* pollution: the link's id changed after insertion -> re-home it under its current id (outside this lock) */
                logger.warning("DupeCheckMap pollution detected: " + linkID);
                it.remove();
                if (reHome == null) {
                    reHome = new ArrayList<CrawledLink>();
                }
                reHome.add(itemLink);
            }
            if (set.isEmpty()) {
                dupeCheckMap.remove(linkID);
            }
        }
        if (reHome != null) {
            for (final CrawledLink itemLink : reHome) {
                addCrawledLinkByLinkID(itemLink);
            }
        }
        return ret;
    }

    /** Registers the link in the dupeCheckMap under its linkID (idempotent). Always maintained, regardless of the dupe manager mode. */
    private void addCrawledLinkByLinkID(final CrawledLink link) {
        final String linkID = link.getLinkID();
        Set<CrawledLink> set = dupeCheckMap.get(linkID);
        if (set == null) {
            // Identity-based weak set: keys held weakly (GC-safe), membership by identity -> O(1) add and idempotent registration, so the
            // repeated put of the same link during an add and large per-linkID piles (dupe manager disabled) stay cheap.
            set = Collections.synchronizedSet(Collections.newSetFromMap(new WeakHashMap<CrawledLink, Boolean>()));
            dupeCheckMap.put(linkID, set);
        }
        set.add(link);
    }

    /**
     * Adds a copy of the link with the given variant directly after the original.
     *
     * @return the new link, or null if a link with this variant already exists or the original link is no longer in a package
     */
    public CrawledLink addAdditional(final CrawledLink link, final LinkVariant o) {
        final DownloadLink dllink = new DownloadLink(link.getDownloadLink().getDefaultPlugin(), link.getDownloadLink().getView().getDisplayName(), link.getDownloadLink().getHost(), link.getDownloadLink().getPluginPatternMatcher(), true);
        dllink.setProperties(link.getDownloadLink().getProperties());
        dllink.setProperty(SOURCE_VARIANT_ID, link.getUniqueID().getID());
        // so plugins like youtube set inherent browserurl (not the youtubev2:// link)
        dllink.setOriginUrl(link.getDownloadLink().getOriginUrl());
        dllink.setContentUrl(link.getDownloadLink().getContentUrl());
        dllink.setContainerUrl(link.getDownloadLink().getContainerUrl());
        dllink.setReferrerUrl(link.getDownloadLink().getReferrerUrl());
        final CrawledLink cl = new CrawledLink(dllink);
        dllink.setNodeChangeListener(cl);
        cl.getDownloadLink().getDefaultPlugin().setActiveVariantByLink(cl.getDownloadLink(), o);
        return LinkCollector.getInstance().getQueue().addWait(new QueueAction<CrawledLink, RuntimeException>() {
            @Override
            protected CrawledLink run() throws RuntimeException {
                final LinkCollector lc = LinkCollector.getInstance();
                /* Avoid dupe (getExistingCrawledLinkByLinkIDOnDupeManagerEnabled returns null while the dupe manager is disabled) */
                if (lc.getExistingCrawledLinkByLinkIDOnDupeManagerEnabled(cl.getLinkID()) != null) {
                    return null;
                }
                final CrawledPackage parent = link.getParentNode();
                if (parent == null) {
                    /* Source link was removed in the meantime */
                    return null;
                }
                final ArrayList<CrawledLink> list = new ArrayList<CrawledLink>();
                list.add(cl);
                LinkCollector.getInstance().moveOrAddAt(parent, list, parent.indexOf(link) + 1);
                return cl;
            }
        });
    }
}
