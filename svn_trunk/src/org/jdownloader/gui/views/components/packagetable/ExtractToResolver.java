package org.jdownloader.gui.views.components.packagetable;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.WeakHashMap;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;

import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.utils.StringUtils;
import org.jdownloader.extensions.extraction.Archive;
import org.jdownloader.extensions.extraction.ArchiveController;
import org.jdownloader.extensions.extraction.ArchiveFactory;
import org.jdownloader.extensions.extraction.CFG_EXTRACTION;
import org.jdownloader.extensions.extraction.ExtractionConfig;
import org.jdownloader.extensions.extraction.ExtractionExtension;
import org.jdownloader.extensions.extraction.ExtractionInfo;
import org.jdownloader.extensions.extraction.bindings.crawledlink.CrawledLinkFactory;
import org.jdownloader.extensions.extraction.bindings.downloadlink.DownloadLinkArchiveFactory;
import org.jdownloader.extensions.extraction.contextmenu.downloadlist.ArchiveValidator;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.settings.staticreferences.CFG_GENERAL;
import org.jdownloader.settings.staticreferences.CFG_LINKGRABBER;

import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.CrawledPackage;
import jd.controlling.packagecontroller.AbstractNode;
import jd.controlling.packagecontroller.AbstractPackageChildrenNode;
import jd.controlling.packagecontroller.AbstractPackageNode;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

/**
 * Resolves the extraction destination of a table node (link or package) for the "Extract to" column.<br>
 * Building archives may access the file system and may take a while, so it must never happen in the EDT.
 * {@link #get(AbstractNode, boolean, Runnable)} therefore only reads a cache. Missing or outdated values are calculated by a background
 * thread, which notifies the caller afterwards.
 */
public class ExtractToResolver {
    /**
     * Text and tooltip of a node. The tooltip is HTML.
     */
    public static final class Result {
        public static final Result EMPTY = new Result("", null);
        private final String       text;
        private final String       tooltip;

        private Result(final String text, final String tooltip) {
            this.text = text;
            this.tooltip = tooltip;
        }

        public String getText() {
            return text;
        }

        public String getTooltip() {
            return tooltip;
        }
    }

    /**
     * Destination of one archive.
     */
    private static final class Destination {
        private final String  text;
        private final String  tooltip;
        private final boolean hasSubfolder;

        private Destination(final String text, final String tooltip, final boolean hasSubfolder) {
            this.text = text;
            this.tooltip = tooltip;
            this.hasSubfolder = hasSubfolder;
        }
    }

    private static final class Entry {
        private final Result  result;
        private final String  inputKey;
        private final long    configCounter;
        private final long    archiveCounter;
        private final boolean extensionAvailable;

        private Entry(final Result result, final String inputKey, final long configCounter, final long archiveCounter, final boolean extensionAvailable) {
            this.result = result;
            this.inputKey = inputKey;
            this.configCounter = configCounter;
            this.archiveCounter = archiveCounter;
            this.extensionAvailable = extensionAvailable;
        }
    }

    private static final ExtractToResolver INSTANCE = new ExtractToResolver();

    public static ExtractToResolver getInstance() {
        return INSTANCE;
    }

    /* Increased whenever a setting that influences the extraction destination changes */
    private static final AtomicLong        CONFIG_COUNTER                 = new AtomicLong(0);
    private static final AtomicBoolean     EXTENSION_LISTENERS_REGISTERED = new AtomicBoolean(false);
    private final Map<AbstractNode, Entry> cache                          = Collections.synchronizedMap(new WeakHashMap<AbstractNode, Entry>());
    private final Set<AbstractNode>        pending                        = Collections.newSetFromMap(Collections.synchronizedMap(new IdentityHashMap<AbstractNode, Boolean>()));
    private final ThreadPoolExecutor       executor;

    private ExtractToResolver() {
        executor = new ThreadPoolExecutor(1, 1, 10, TimeUnit.SECONDS, new LinkedBlockingQueue<Runnable>(), new ThreadFactory() {
            @Override
            public Thread newThread(final Runnable r) {
                final Thread thread = new Thread(r, "ExtractToResolver");
                thread.setDaemon(true);
                return thread;
            }
        });
        executor.allowCoreThreadTimeOut(true);
        addChangeListener(CFG_GENERAL.DEFAULT_DOWNLOAD_FOLDER);
        addChangeListener(CFG_LINKGRABBER.AUTO_EXTRACTION_ENABLED);
    }

    @SuppressWarnings({ "rawtypes", "unchecked" })
    private static void addChangeListener(final KeyHandler keyHandler) {
        keyHandler.getEventSender().addListener(new GenericConfigEventListener<Object>() {
            @Override
            public void onConfigValueModified(final KeyHandler<Object> keyHandler, final Object newValue) {
                CONFIG_COUNTER.incrementAndGet();
            }

            @Override
            public void onConfigValidatorError(final KeyHandler<Object> keyHandler, final Object invalidValue, final ValidationException validateException) {
            }
        });
    }

    /* Done lazily, because the extension settings are only needed if the extension is available. */
    private static void registerExtensionListeners() {
        if (EXTENSION_LISTENERS_REGISTERED.compareAndSet(false, true)) {
            addChangeListener(CFG_EXTRACTION.CUSTOM_EXTRACTION_PATH);
            addChangeListener(CFG_EXTRACTION.CUSTOM_EXTRACTION_PATH_ENABLED);
            addChangeListener(CFG_EXTRACTION.SUBPATH_ENABLED);
            addChangeListener(CFG_EXTRACTION.SUB_PATH);
        }
    }

    private static boolean isSupported(final AbstractNode node) {
        return node instanceof DownloadLink || node instanceof FilePackage || node instanceof CrawledLink || node instanceof CrawledPackage;
    }

    /**
     * Returns the cached result without blocking.<br>
     * A node is calculated once, when its value is needed for the first time and the column is visible: A package that arrives in the
     * LinkGrabber and, as the nodes are replaced, once more after it was moved to the download list. There is no periodic refresh. An
     * existing value is only recalculated if something that influences it changed (see {@link #isOutdated(Entry, AbstractNode)}).
     *
     * @param compute
     *            false: only read the cache, never schedule a calculation (e.g. if the column is hidden)
     * @param onUpdated
     *            called (in the background thread) as soon as a new result is available
     * @return the cached result, which may be outdated, or null if there is none yet
     */
    public Result get(final AbstractNode node, final boolean compute, final Runnable onUpdated) {
        if (!isSupported(node)) {
            return null;
        }
        final Entry entry = cache.get(node);
        if (compute && (entry == null || isOutdated(entry, node))) {
            schedule(node, onUpdated);
        }
        if (entry == null) {
            return null;
        }
        return entry.result;
    }

    private boolean isOutdated(final Entry entry, final AbstractNode node) {
        final boolean extensionAvailable = ArchiveValidator.EXTENSION != null;
        if (entry.extensionAvailable != extensionAvailable) {
            return true;
        } else if (entry.configCounter != CONFIG_COUNTER.get()) {
            return true;
        } else if (extensionAvailable && entry.archiveCounter != ArchiveController.getSettingsChangeCounter()) {
            return true;
        } else {
            return !StringUtils.equals(entry.inputKey, createInputKey(node));
        }
    }

    /**
     * Cheap key of everything the node itself contributes to its destination (download folder, names).
     */
    @SuppressWarnings("rawtypes")
    private static String createInputKey(final AbstractNode node) {
        final StringBuilder sb = new StringBuilder();
        sb.append(LinkTreeUtils.getRawDownloadDirectory(node).getPath());
        final AbstractPackageNode pkg;
        if (node instanceof AbstractPackageNode) {
            pkg = (AbstractPackageNode) node;
        } else if (node instanceof AbstractPackageChildrenNode) {
            pkg = (AbstractPackageNode) ((AbstractPackageChildrenNode) node).getParentNode();
            sb.append('|').append(node.getName());
        } else {
            pkg = null;
        }
        if (pkg != null) {
            sb.append('|').append(pkg.getName());
        }
        return sb.toString();
    }

    private void schedule(final AbstractNode node, final Runnable onUpdated) {
        if (!pending.add(node)) {
            return;
        }
        executor.execute(new Runnable() {
            @Override
            public void run() {
                try {
                    cache.put(node, createEntry(node));
                } finally {
                    pending.remove(node);
                }
                if (onUpdated != null) {
                    onUpdated.run();
                }
            }
        });
    }

    private Entry createEntry(final AbstractNode node) {
        final String inputKey = createInputKey(node);
        /*
         * Read before calculating: a setting that changes during the calculation must make this entry outdated. The archive counter is read
         * afterwards, because the calculation may create archive settings.
         */
        final long configCounter = CONFIG_COUNTER.get();
        Result result;
        try {
            result = resolve(node);
        } catch (final Throwable e) {
            /* Keep the entry (empty) so we do not retry on every repaint. It is retried when something changes. */
            org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().log(e);
            result = Result.EMPTY;
        }
        final boolean extensionAvailable = ArchiveValidator.EXTENSION != null;
        return new Entry(result, inputKey, configCounter, extensionAvailable ? ArchiveController.getSettingsChangeCounter() : 0, extensionAvailable);
    }

    private Result resolve(final AbstractNode node) {
        final ExtractionExtension extension = ArchiveValidator.EXTENSION;
        if (extension == null) {
            /* We cannot tell which nodes are archives, so every node is shown as disabled. */
            final String text = _GUI.T.ExtractToColumn_extraction_disabled();
            return new Result(text, toHtml(text));
        }
        registerExtensionListeners();
        final List<Archive> archives = getArchives(extension, node);
        if (archives.isEmpty()) {
            return Result.EMPTY;
        }
        /* Linkgrabber nodes show the raw path (with tags), download list nodes the resolved path */
        final boolean raw = node instanceof CrawledLink || node instanceof CrawledPackage;
        final Map<String, Destination> destinations = new LinkedHashMap<String, Destination>();
        for (final Archive archive : archives) {
            try {
                final Destination destination = describe(extension, archive, raw);
                if (destination.text.length() > 0 && !destinations.containsKey(destination.text)) {
                    destinations.put(destination.text, destination);
                }
            } catch (final Throwable e) {
                org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().log(e);
            }
        }
        if (destinations.isEmpty()) {
            return Result.EMPTY;
        }
        final StringBuilder tooltip = new StringBuilder();
        boolean hasSubfolder = false;
        for (final Destination destination : destinations.values()) {
            if (tooltip.length() > 0) {
                tooltip.append('\n');
            }
            tooltip.append(destination.tooltip);
            hasSubfolder |= destination.hasSubfolder;
        }
        if (hasSubfolder) {
            tooltip.append("\n\n").append(_GUI.T.ExtractToColumn_subfolder_hint());
        }
        final String text;
        if (destinations.size() == 1) {
            text = destinations.values().iterator().next().text;
        } else {
            text = _GUI.T.ExtractToColumn_multiple_destinations();
        }
        return new Result(text, toHtml(tooltip.toString()));
    }

    @SuppressWarnings({ "rawtypes", "unchecked" })
    private static List<Archive> getArchives(final ExtractionExtension extension, final AbstractNode node) {
        if (node instanceof AbstractPackageNode) {
            final AbstractPackageNode pkg = (AbstractPackageNode) node;
            final List<Object> children;
            final boolean readL = pkg.getModifyLock().readLock();
            try {
                children = new ArrayList<Object>(pkg.getChildren());
            } finally {
                pkg.getModifyLock().readUnlock(readL);
            }
            if (children.isEmpty()) {
                return Collections.emptyList();
            }
            return extension.getArchivesFromPackageChildren(children, null, 0);
        }
        final ArchiveFactory factory;
        if (node instanceof DownloadLink) {
            factory = new DownloadLinkArchiveFactory((DownloadLink) node);
        } else if (node instanceof CrawledLink) {
            factory = new CrawledLinkFactory((CrawledLink) node);
        } else {
            return Collections.emptyList();
        }
        final Archive archive = extension.getArchiveByFactory(factory);
        if (archive == null) {
            return Collections.emptyList();
        }
        return Collections.singletonList(archive);
    }

    private static Destination describe(final ExtractionExtension extension, final Archive archive, final boolean raw) {
        if (!extension.isAutoExtractEnabled(archive)) {
            /* Extracted manually: Auto extract is still disabled, but we know where the archive was extracted to. */
            final ExtractionInfo extractionInfo = archive.getSettings().getExtractionInfo();
            if (extractionInfo != null && !StringUtils.isEmpty(extractionInfo.getExtractToFolder())) {
                final String extractedTo = extractionInfo.getExtractToFolder();
                return new Destination(extractedTo, extractedTo, false);
            }
            /* Auto extract disabled && manual extraction hasn't happened either. */
            final String text = _GUI.T.ExtractToColumn_auto_extract_disabled();
            return new Destination(text, text, false);
        }
        /* The subfolder is not part of the folder, because it depends on the archive content, which is often not known yet */
        final String subRaw = getSubfolder(extension, archive, true);
        final String subResolved = getSubfolder(extension, archive, false);
        final String suffixShown = createSuffix(raw ? subRaw : subResolved);
        final String suffixResolved = createSuffix(subResolved);
        File resolved = extension.getFinalExtractToFolder(archive, false, false);
        if (resolved == null) {
            /* No path configured: the archive is extracted into its own folder. Always show the path, never a placeholder text. */
            final String downloadFolder = archive.getFactory().createDefaultExtractToPath(archive);
            if (StringUtils.isEmpty(downloadFolder)) {
                return new Destination("", "", false);
            }
            resolved = new File(downloadFolder);
        }
        File shown = resolved;
        if (raw) {
            final File rawFolder = extension.getFinalExtractToFolder(archive, true, false);
            if (rawFolder != null) {
                shown = rawFolder;
            }
        }
        final String text = shown.getPath() + suffixShown;
        final String resolvedText = resolved.getPath() + suffixResolved;
        final String tooltip;
        if (StringUtils.equals(text, resolvedText)) {
            tooltip = text;
        } else {
            /* Show the resolved path, too, if the shown one contains tags */
            tooltip = text + "\n" + resolvedText;
        }
        return new Destination(text, tooltip, suffixShown.length() > 0);
    }

    /**
     * @return the configured subfolder or null if no subfolder is used
     */
    private static String getSubfolder(final ExtractionExtension extension, final Archive archive, final boolean raw) {
        final ExtractionConfig config = extension.getSettings();
        if (!config.isSubpathEnabled()) {
            return null;
        }
        String sub = config.getSubPath();
        if (StringUtils.isEmpty(sub)) {
            return null;
        }
        if (!raw) {
            final String resolved = archive.getFactory().createExtractSubPath(sub, archive);
            if (!StringUtils.isEmpty(resolved)) {
                sub = resolved;
            }
        }
        sub = sub.trim();
        while (sub.startsWith("/") || sub.startsWith("\\")) {
            sub = sub.substring(1);
        }
        if (sub.length() == 0) {
            return null;
        }
        return sub;
    }

    private static String createSuffix(final String subfolder) {
        if (subfolder == null) {
            return "";
        }
        return File.separator + subfolder;
    }

    private static String toHtml(final String text) {
        final String escaped = text.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\n", "<br>");
        return "<html>" + escaped + "</html>";
    }
}
