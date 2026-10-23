package jd.controlling.faviconcontroller;

import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RoundRectangle2D;
import java.awt.image.BufferedImage;
import java.awt.image.Raster;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.ConnectException;
import java.net.MalformedURLException;
import java.net.SocketTimeoutException;
import java.net.URL;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Pattern;

import javax.imageio.ImageIO;
import javax.swing.Icon;
import javax.swing.ImageIcon;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.resources.MultiResolutionImageHelper;
import org.appwork.shutdown.ShutdownController;
import org.appwork.shutdown.ShutdownEvent;
import org.appwork.shutdown.ShutdownRequest;
import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.Application;
import org.appwork.utils.DataURLInputStream;
import org.appwork.utils.Exceptions;
import org.appwork.utils.Files;
import org.appwork.utils.IO;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.ImageProvider.ImageProvider;
import org.appwork.utils.images.IconIO;
import org.appwork.utils.images.svg.SVGFactory;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.logging2.LogSource;
import org.appwork.utils.net.PublicSuffixList;
import org.appwork.utils.net.URLHelper;
import org.jdownloader.controlling.FileCreationManager;
import org.jdownloader.controlling.filter.CompiledFiletypeFilter;
import org.jdownloader.gui.IconKey;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.images.NewTheme;
import org.jdownloader.logging.LogController;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.plugins.controller.crawler.CrawlerPluginController;
import org.jdownloader.plugins.controller.crawler.LazyCrawlerPlugin;
import org.jdownloader.plugins.controller.host.LazyHostPlugin;
import org.jdownloader.plugins.controller.host.PluginFinder;
import org.jdownloader.updatev2.gui.LAFOptions;

import jd.captcha.utils.GifDecoder;
import jd.http.Browser;
import jd.http.Browser.BlockedByException;
import jd.http.Browser.BrowserException;
import jd.http.Request;
import jd.http.URLConnectionAdapter;
import jd.http.requests.GetRequest;
import jd.plugins.PluginForHost;
import net.sf.image4j.codec.ico.ICODecoder;

public class FavIcons {
    private static final int                                 BASE_SIZE       = 16;
    private static final ThreadPoolExecutor                  THREAD_POOL;
    private static final Object                              LOCK            = new Object();
    private static final Map<String, List<FavIconRequestor>> QUEUE           = new LinkedHashMap<String, List<FavIconRequestor>>();
    private static final Map<String, ImageIcon>              FAILED_ICONS    = new HashMap<String, ImageIcon>();
    private static final Set<String>                         REFRESHED_ICONS = new HashSet<String>();
    private static final Map<String, ImageIcon>              DEFAULT_ICONS   = new HashMap<String, ImageIcon>();
    private static final FavIconsConfig                      CONFIG          = JsonConfig.create(FavIconsConfig.class);
    private static final long                                REFRESH_TIMEOUT = 1000l * 60 * 60 * 24 * 7;
    private static final long                                RETRY_TIMEOUT   = 1000l * 60 * 60 * 24 * 7;
    static {
        int maxThreads = Math.max(CONFIG.getMaxThreads(), 1);
        int keepAlive = Math.max(CONFIG.getThreadKeepAlive(), 100);
        THREAD_POOL = new ThreadPoolExecutor(0, maxThreads, keepAlive, TimeUnit.MILLISECONDS, new LinkedBlockingQueue<Runnable>(), new ThreadFactory() {
            public Thread newThread(Runnable r) {
                Thread t = new Thread(r);
                t.setDaemon(true);
                t.setName("FavIconLoader: idle");
                return t;
            }
        }, new ThreadPoolExecutor.AbortPolicy()) {
            @Override
            protected void afterExecute(Runnable r, Throwable t) {
                super.afterExecute(r, t);
                Thread.currentThread().setName("FavIconLoader: idle");
            }

            @Override
            protected void beforeExecute(Thread t, Runnable r) {
                super.beforeExecute(t, r);
                if (r instanceof FavIconsRunnable) {
                    Thread.currentThread().setName("FavIconLoader: " + ((FavIconsRunnable) r).host);
                }
                /*
                 * WORKAROUND for stupid SUN /ORACLE way of "how a threadpool should work" !
                 */
                int working = THREAD_POOL.getActiveCount();
                int active = THREAD_POOL.getPoolSize();
                int max = THREAD_POOL.getMaximumPoolSize();
                if (active < max && working == active) {
                    /*
                     * we can increase max pool size so new threads get started
                     */
                    THREAD_POOL.setCorePoolSize(Math.min(max, active + 1));
                }
            }
        };
        THREAD_POOL.allowCoreThreadTimeOut(true);
        /* load failed hosts list */
        try {
            ArrayList<String> FAILED_ARRAY_LIST = JsonConfig.create(FavIconsConfig.class).getFailedHosts();
            if (FAILED_ARRAY_LIST == null || (System.currentTimeMillis() - CONFIG.getLastRefresh()) > RETRY_TIMEOUT) {
                CONFIG.setLastRefresh(System.currentTimeMillis());
                /* timeout is over, lets try again the failed ones */
                FAILED_ARRAY_LIST = new ArrayList<String>();
                CONFIG.setFailedHosts(FAILED_ARRAY_LIST);
            }
            synchronized (LOCK) {
                for (String host : FAILED_ARRAY_LIST) {
                    FAILED_ICONS.put(host, null);
                }
            }
        } catch (Exception e) {
            // avoid java.lang.ExceptionInInitializerError
            LogController.CL(false).log(e);
        }
        ShutdownController.getInstance().addShutdownEvent(new ShutdownEvent() {
            @Override
            public String toString() {
                return "ShutdownEvent: Save Favicons";
            }

            @Override
            public void onShutdown(final ShutdownRequest shutdownRequest) {
                final ArrayList<String> failedHosts;
                synchronized (LOCK) {
                    failedHosts = new ArrayList<String>(FAILED_ICONS.keySet());
                }
                try {
                    CONFIG.setFailedHosts(failedHosts);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        });
    }

    public static Icon getFavIcon(String host, FavIconRequestor requestor) {
        return getFavIcon(host, requestor, true);
    }

    public static Icon getFavIcon(String host, FavIconRequestor requestor, final boolean updatePermission) {
        if (host == null) {
            return null;
        }
        /*
         * check if we already have a favicon? The lookup does file system calls and does not touch shared state, so it runs outside LOCK.
         * writeFavIcon replaces the file via temporary file, so the lookup never finds a partially written file.
         */
        /*
         * TODO: cache the lookup result per host to avoid repeated file system/classpath calls. getIconURL is called without ThemeContext,
         * so nothing is cached and every call checks up to 2 keys x 5 extensions (x 2 theme paths if the current theme is not "standard"),
         * each with a file check and a classpath lookup. This is especially wasteful for hosts without favicon (nothing found, host
         * already in FAILED_ICONS/QUEUE) and for invalid files (file exists but cannot be loaded). Idea: session map host -> key + URL or
         * NOT_FOUND marker, updated by writeFavIcon after a successful write, cleared if NewTheme.I().getPath() changes, optionally with a
         * short expiry for NOT_FOUND entries in case favicons are added from outside during the session.
         */
        String key = "fav/" + host;
        URL url = NewTheme.I().getIconURL(key);
        if (url == null) {
            key = "fav/big." + host;
            url = NewTheme.I().getIconURL(key);
        }
        if (url != null) {
            refreshIfExpired(host, url, requestor, updatePermission);
            /* load favIcon from disk */
            return new AbstractIcon(key, -1);
        }
        if (!updatePermission) {
            return null;
        }
        /* add to queue list */
        return add(host, requestor);
    }

    /**
     * Enqueues a refresh of an already existing favicon file once per session if the file is older than REFRESH_TIMEOUT. Should not be
     * called while holding LOCK, because it does file system calls; only the REFRESHED_ICONS check is synchronized.
     *
     * Without updatePermission this method does nothing at all. It must neither mark the host in REFRESHED_ICONS nor reset the file
     * timestamp in that case, otherwise a due refresh would be lost: later calls with updatePermission in the same session would skip the
     * host, and the reset timestamp would make the file look fresh for another REFRESH_TIMEOUT.
     */
    private static void refreshIfExpired(final String host, final URL url, final FavIconRequestor requestor, final boolean updatePermission) {
        if (!updatePermission) {
            /* no permission to trigger a (network) refresh -> keep refresh state untouched so a later call with permission can do it */
            return;
        }
        synchronized (LOCK) {
            if (!REFRESHED_ICONS.add(host)) {
                /* refresh already checked/enqueued in this session */
                return;
            }
        }
        if (!"file".equalsIgnoreCase(url.getProtocol())) {
            /* e.g. icon from jar/classpath -> cannot be refreshed */
            return;
        }
        try {
            final File file = new File(url.toURI());
            if (!file.getName().equalsIgnoreCase(getFavIconFileName(host))) {
                /*
                 * The found icon is not the file a refresh would write (e.g. <host>.svg, <host>.ico or big.<host>.png). Refreshing would
                 * either be pointless (.svg is looked up before .png, so the new file is never used) or replace a deliberately provided
                 * icon (big.<host>), so only downloaded favicons get refreshed. Only the file name is compared, so a favicon from the
                 * standard theme can still be refreshed into the current theme.
                 */
                return;
            }
            /* isFile, canWrite and lastModified are file system calls, so each of them is called only once */
            if (!file.isFile()) {
                return;
            }
            if (!file.canWrite()) {
                // do not try to refresh write protected files
                return;
            }
            final long lastModified = file.lastModified();
            if (lastModified <= 0 || System.currentTimeMillis() - lastModified <= REFRESH_TIMEOUT) {
                return;
            }
            file.setLastModified(System.currentTimeMillis());// avoid retry before expired
            add(host, requestor);
        } catch (final Throwable e) {
            e.printStackTrace();
        }
    }

    /**
     * Returns the file name a downloaded favicon is buffered to (see writeFavIcon).
     */
    private static String getFavIconFileName(final String host) {
        return host + ".png";
    }

    public static ImageIcon getDefaultIcon(String host, boolean clearAfterGet) {
        final LogSource logger = getLogger(host, null);
        try {
            return getDefaultIcon(logger, host, clearAfterGet);
        } finally {
            logger.close();
        }
    }

    /**
     * Returns the default (dummy) icon of the given host. Only the DEFAULT_ICONS access is synchronized on LOCK; the icon itself is
     * rendered outside LOCK, so this method should not be called while holding LOCK.
     */
    public static ImageIcon getDefaultIcon(final LogSource logger, String host, boolean clearAfterGet) {
        synchronized (LOCK) {
            final ImageIcon cached = DEFAULT_ICONS.get(host);
            if (cached != null) {
                if (clearAfterGet) {
                    DEFAULT_ICONS.remove(host);
                }
                return cached;
            }
        }
        /* rendering is expensive -> outside LOCK. Two threads may render the same icon concurrently, which is harmless. */
        final ImageIcon created = new ImageIcon(createDefaultFavIcon(logger, host));
        if (clearAfterGet) {
            return created;
        }
        synchronized (LOCK) {
            final ImageIcon cached = DEFAULT_ICONS.get(host);
            if (cached != null) {
                /* another thread was faster, use its icon so all callers share the same instance */
                return cached;
            }
            DEFAULT_ICONS.put(host, created);
            return created;
        }
    }

    private static LogSource getLogger(final String host, FavIconRequestor requestor) {
        final LogSource ret = LogController.getFastPluginLogger("FavIcons");
        if (CONFIG.isDebugFlag()) {
            ret.setInstantFlush(true);
        }
        return ret;
    }

    private static LazyHostPlugin getPlugin(final String host) {
        final LazyHostPlugin existingHostPlugin = new PluginFinder()._assignHost(host);
        if (existingHostPlugin != null) {
            return existingHostPlugin;
        }
        final String domain = PublicSuffixList.getInstance().getDomain(host);
        if (host.equalsIgnoreCase(domain)) {
            return null;
        }
        return new PluginFinder()._assignHost(domain);
    }

    private static abstract class FavIconsRunnable implements Runnable {
        private final String host;

        private FavIconsRunnable(final String host) {
            this.host = host;
        }
    }

    private static ImageIcon add(final String host, FavIconRequestor requestor) {
        final LogSource logger = getLogger(host, requestor);
        final AtomicReference<LogSource> loggerReference = new AtomicReference<LogSource>(logger);
        try {
            final boolean failed;
            boolean startLoader = false;
            synchronized (LOCK) {
                /* don't try this host again? */
                failed = FAILED_ICONS.containsKey(host);
                if (failed) {
                    final ImageIcon failedIcon = FAILED_ICONS.get(host);
                    if (failedIcon != null) {
                        return failedIcon;
                    }
                } else {
                    /* enqueue this host for favicon loading */
                    List<FavIconRequestor> requestors = QUEUE.get(host);
                    if (requestors == null) {
                        requestors = new ArrayList<FavIconRequestor>();
                        QUEUE.put(host, requestors);
                        startLoader = true;
                    }
                    /* add to queue */
                    if (requestor != null) {
                        requestors.add(requestor);
                    }
                }
            }
            /* default icons are rendered outside LOCK (expensive), state changes are synchronized again afterwards */
            if (failed) {
                /* failed host without icon yet (e.g. from the persisted failed hosts list) */
                final ImageIcon defaultIcon = getDefaultIcon(logger, host, true);
                synchronized (LOCK) {
                    final ImageIcon failedIcon = FAILED_ICONS.get(host);
                    if (failedIcon != null) {
                        /* another thread was faster */
                        return failedIcon;
                    }
                    FAILED_ICONS.put(host, defaultIcon);
                }
                return defaultIcon;
            }
            /*
             * create the default icon before the loader is started: a failing loader removes it from DEFAULT_ICONS again
             * (getDefaultIcon(..., true)), so creating it afterwards could leave a stale DEFAULT_ICONS entry
             */
            final ImageIcon icon = getDefaultIcon(logger, host, false);
            if (!startLoader) {
                /* already enqueued, requestor gets notified by the running loader */
                return icon;
            }
            /* the runnable takes over the logger and closes it */
            loggerReference.getAndSet(null);
            THREAD_POOL.execute(new FavIconsRunnable(host) {
                public void run() {
                    try {
                        loadFavIcon(host, logger);
                    } finally {
                        logger.close();
                    }
                }
            });
            return icon;
        } finally {
            if (loggerReference.getAndSet(null) == logger) {
                logger.close();
            }
        }
    }

    /**
     * Loads the favicon of the given host (via plugin or website) and buffers it to disk. Runs in THREAD_POOL.
     */
    private static void loadFavIcon(final String host, final LogSource logger) {
        final CopyOnWriteArrayList<String> tryHosts = new CopyOnWriteArrayList<String>();
        BufferedImage favicon = null;
        String[] siteSupportedNames = null;
        final LazyHostPlugin existingHostPlugin = getPlugin(host);
        if (existingHostPlugin != null) {
            boolean pluginProvidedFavIcon = false;
            if (existingHostPlugin.hasFeature(LazyPlugin.FEATURE.FAVICON)) {
                try {
                    final PluginForHost pluginInstance = existingHostPlugin.newInstance(null, false);
                    pluginInstance.setLogger(logger);
                    siteSupportedNames = pluginInstance.siteSupportedNames();
                    if (siteSupportedNames == null) {
                        siteSupportedNames = new String[0];
                    }
                    final Object result = pluginInstance.getFavIcon(host);
                    if (result instanceof Icon) {
                        favicon = IconIO.toBufferedImage((Icon) result);
                        logger.clear();
                    } else if (result instanceof Image) {
                        favicon = IconIO.toBufferedImage((Image) result);
                        logger.clear();
                    } else if (result instanceof String) {
                        tryHosts.addIfAbsent((String) result);
                    } else if (result instanceof Collection) {
                        for (final Object elem : (Collection) result) {
                            if (elem instanceof String) {
                                tryHosts.addIfAbsent((String) elem);
                            }
                        }
                    }
                    pluginProvidedFavIcon = favicon != null || tryHosts.size() > 0;
                } catch (Exception e) {
                    logger.log(e);
                }
            }
            if (!pluginProvidedFavIcon && existingHostPlugin.hasFeature(LazyPlugin.FEATURE.INTERNAL)) {
                onLoadFailed(logger, host);
                return;
            }
        }
        if (favicon == null) {
            if (existingHostPlugin != null) {
                tryHosts.addIfAbsent(existingHostPlugin.getHost());
            }
            tryHosts.addIfAbsent(host);
            addParentDomains(host, tryHosts);
            favicon = downloadFavIcon(logger, tryHosts, siteSupportedNames);
        }
        if (favicon == null) {
            onLoadFailed(logger, host);
            return;
        }
        /* file system calls and png encoding outside LOCK */
        final boolean written = writeFavIcon(logger, host, favicon);
        final List<FavIconRequestor> requestors;
        synchronized (LOCK) {
            /*
             * remove from QUEUE only after the file has been written: getFavIcon calls during the write find no file yet (or the old one)
             * and must see the host as already enqueued, so they only add their requestor instead of starting a second loader
             */
            requestors = QUEUE.remove(host);
        }
        if (!written || requestors == null || requestors.size() == 0) {
            return;
        }
        /* callbacks outside LOCK: setFavIcon loads and scales the icon from disk and calls foreign code */
        notifyRequestors(logger, host, requestors);
    }

    /**
     * Removes the host from QUEUE and remembers it as failed unless its existing favicon was only refreshed. Must not be called while
     * holding LOCK.
     */
    private static void onLoadFailed(final LogSource logger, final String host) {
        /* rendering is expensive -> outside LOCK, even if the icon is not needed in the end (refreshed or already failed host) */
        final ImageIcon defaultIcon = getDefaultIcon(logger, host, true);
        synchronized (LOCK) {
            QUEUE.remove(host);
            if (REFRESHED_ICONS.contains(host) || FAILED_ICONS.get(host) != null) {
                return;
            }
            FAILED_ICONS.put(host, defaultIcon);
        }
    }

    /**
     * Sets the freshly written favicon on all requestors that were queued while it was loaded. Must not be called while holding LOCK.
     */
    private static void notifyRequestors(final LogSource logger, final String host, final List<FavIconRequestor> requestors) {
        /* load and scale it again */
        final Icon image = new AbstractIcon("fav/" + host, -1);
        /* refresh icons for all queued plugins */
        for (final FavIconRequestor requestor : requestors) {
            try {
                requestor.setFavIcon(image);
            } catch (Throwable e) {
                /* one failing requestor must not prevent the others from being notified */
                logger.log(e);
            }
        }
    }

    /**
     * Adds every parent domain of host to tryHosts, down to the registrable domain.
     */
    private static void addParentDomains(final String host, final CopyOnWriteArrayList<String> tryHosts) {
        if (host.matches("\\d+\\.\\d+\\.\\d+\\.\\d+")) {
            return;
        }
        final String domain = PublicSuffixList.getInstance() != null ? PublicSuffixList.getInstance().getDomain(host) : null;
        String tryHost = host;
        while (true) {
            /*
             * this loop adds every subdomain and the tld to tryHosts and we try to fetch a favIcon in same order
             */
            final int index = tryHost.indexOf(".");
            if (index < 0 || tryHost.length() < index + 1) {
                return;
            }
            tryHost = tryHost.substring(index + 1);
            if (domain != null && !tryHost.contains(domain) || tryHost.indexOf('.') == -1) {
                return;
            }
            tryHosts.addIfAbsent(tryHost);
        }
    }

    /**
     * Buffers the favicon to disk (see writeFileIfChanged). Must not be called while holding LOCK.
     *
     * getFavIcon looks up the file without LOCK and a lazy AbstractIcon may load it at any time, so the file must never be missing or
     * partially written. Otherwise loading fails and Theme caches the "not available" placeholder (red square in IDE, empty icon otherwise)
     * for the rest of the session.
     *
     * @return true if the favicon is available on disk (written or already up to date)
     */
    private static boolean writeFavIcon(final LogSource logger, final String host, final BufferedImage favicon) {
        try {
            final ByteArrayOutputStream bos = new ByteArrayOutputStream();
            ImageProvider.writeImage(favicon, "png", bos);
            final File outputFile = Application.getResource(NewTheme.I().getPath() + "/images/fav/" + getFavIconFileName(host));
            FileCreationManager.getInstance().mkdir(outputFile.getParentFile());
            writeFileIfChanged(outputFile, bos.toByteArray());
            return true;
        } catch (Throwable e) {
            logger.log(e);
            return false;
        }
    }

    /**
     * Writes bytes to file so that a concurrent reader always sees either the complete old or the complete new content, never a missing or
     * partially written file:
     * <ul>
     * <li>If the file already has exactly the same content (e.g. a favicon refresh of a plugin that always returns the same icon), nothing
     * is written at all.</li>
     * <li>Otherwise the bytes are written to a temporary file in the same directory (file name + ".tmp"), which then replaces the file in one
     * step (Files.move with REPLACE_EXISTING). IO.secureWrite is not used on purpose: it deletes the target file before renaming, so the
     * file would be missing for a short moment.</li>
     * <li>On Windows the replacement fails while another thread has the file open for reading, so it is retried for a short time.</li>
     * </ul>
     * If anything fails, an existing file stays intact and the temporary file is removed. The parent directory must exist.
     *
     * @return true if the file has been written, false if it was already up to date
     */
    private static boolean writeFileIfChanged(final File file, final byte[] bytes) throws IOException {
        /* length() is 0 for a missing file, so the file content is only read if the size matches */
        if (file.length() == bytes.length) {
            try {
                if (Arrays.equals(IO.readFile(file), bytes)) {
                    return false;
                }
            } catch (IOException e) {
                /* cannot compare -> simply write it again */
            }
        }
        final File tmpFile = new File(file.getAbsolutePath() + ".tmp");
        boolean deleteTmpFile = true;
        try {
            /* leftover of an aborted write, IO.writeToFile refuses to overwrite existing files */
            tmpFile.delete();
            /* no need to sync the file system, the replacement below is what matters for concurrent readers */
            IO.writeToFile(tmpFile, bytes, IO.SYNC.NONE);
            int retry = 0;
            while (true) {
                try {
                    IO.renameTo(tmpFile, file, true);
                    deleteTmpFile = false;
                    return true;
                } catch (final IOException e) {
                    if (++retry > 10) {
                        throw e;
                    }
                    try {
                        /* 10ms, 20ms, ... 100ms -> at most ~550ms in total */
                        Thread.sleep(retry * 10);
                    } catch (final InterruptedException ie) {
                        Thread.currentThread().interrupt();
                        throw new IOException(ie);
                    }
                }
            }
        } finally {
            if (deleteTmpFile) {
                /* failed -> remove the temporary file, the existing file stays intact */
                tmpFile.delete();
            }
        }
    }

    private static Image createDefaultFavIcon(final LogSource logger, final String host) {
        Image base = createDefaultFavIcon(logger, host, BASE_SIZE, BASE_SIZE);
        if (!MultiResolutionImageHelper.isSupported()) {
            return base;
        }
        double highest = MultiResolutionImageHelper.getHighestMonitorScaling();
        if (highest <= 1d) {
            return base;
        }
        Image bigger = createDefaultFavIcon(logger, host, (int) Math.round(BASE_SIZE * highest), (int) Math.round(BASE_SIZE * highest));
        return MultiResolutionImageHelper.create(Arrays.asList(base, bigger));
    }

    /**
     * Creates a dummy hoster icon: a rounded rectangle with a two letter abbreviation of the host (see getDefaultIconAbbreviation) and the
     * tld in the lower right corner.
     */
    private static Image createDefaultFavIcon(final LogSource logger, final String host, final int w, final int h) {
        final int size = w - 7;
        final Color fg = Color.BLACK;
        Color bg = Color.WHITE;
        logger.info("createDefaultFavIcon: " + host);
        try {
            if (!Application.isHeadless()) {
                final Color c = LAFOptions.getInstance().getColorForPanelHeaderBackground();
                if (c != null) {
                    bg = c;
                }
            }
        } catch (final Throwable e) {
            logger.log(e);
        }
        final BufferedImage image = IconIO.createEmptyImage(w, h);
        final Graphics2D g = image.createGraphics();
        try {
            final String fontName = ImageProvider.getDrawFontName();
            g.setFont(new Font(fontName, Font.BOLD, size));
            /* getFontMetrics also detects missing fonts/headless java (see isBuggyFontEnvironment below) */
            final FontMetrics abbreviationMetrics = g.getFontMetrics();
            g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            String tld = Files.getExtension(host);
            if (tld != null) {
                tld = tld.toLowerCase(Locale.ROOT);
            }
            final String abbreviation = getDefaultIconAbbreviation(host);
            /* background */
            final RoundRectangle2D roundedRectangle = new RoundRectangle2D.Float(0, 0, w - 1, h - 1, 5, 5);
            g.setColor(bg);
            g.fill(roundedRectangle);
            g.setColor(bg.darker());
            g.draw(roundedRectangle);
            /* abbreviation, centered (moved up by one pixel if there is a tld below it) */
            g.setColor(fg);
            final Rectangle2D abbreviationBounds = abbreviationMetrics.getStringBounds(abbreviation, g);
            g.drawString(abbreviation, (int) (w - abbreviationBounds.getWidth()) / 2, (int) (-abbreviationBounds.getY() + (h - abbreviationBounds.getHeight()) / 2) - (tld == null ? 0 : 1));
            if (tld != null) {
                /* tld, right aligned in the lower right corner. The bounds must be measured with the metrics of the tld font. */
                g.setFont(new Font(fontName, Font.PLAIN, w - 10));
                final Rectangle2D tldBounds = g.getFontMetrics().getStringBounds("." + tld, g);
                g.drawString("." + tld, (int) (w - tldBounds.getWidth()) - 2, h - 2);
            }
            return image;
        } catch (Throwable e) {
            logger.log(e);
            if (ImageProvider.isBuggyFontEnvironment(e)) {
                return IconIO.toBufferedImage(new AbstractIcon(IconKey.ICON_ERROR, 16));
            }
            throw new RuntimeException(e);
        } finally {
            g.dispose();
        }
    }

    /**
     * Returns the two letter abbreviation shown in the default icon of the given host: the first and the middle consonant of the host
     * without tld (digits, non-word characters, vowels and 'J' are removed). Falls back to the first two characters of the host if no
     * consonant is left (e.g. "aio.com" -> "AI").
     */
    private static String getDefaultIconAbbreviation(final String host) {
        String name = host.toUpperCase(Locale.ROOT);
        /* remove tld */
        final int tldIndex = name.lastIndexOf('.');
        if (tldIndex != -1) {
            name = name.substring(0, tldIndex);
        }
        /* keep consonants only */
        name = name.replaceAll("[\\d\\WEIOAJU]", "");
        if (name.length() == 0) {
            return host.substring(0, Math.min(host.length(), 2)).toUpperCase(Locale.ROOT);
        }
        return "" + name.charAt(0) + name.charAt(name.length() / 2);
    }

    /*
     * dirty hack to count number of unique colors, use only for small images like favicons!
     */
    private static int countColors(BufferedImage image) {
        final HashSet<Integer> color = new HashSet<Integer>();
        for (int x = 0; x < image.getWidth(); x++) {
            for (int y = 0; y < image.getHeight(); y++) {
                color.add(image.getRGB(x, y));
            }
        }
        return color.size();
    }

    /**
     * Returns the best image of the given candidates (e.g. all resolutions of an ico file), cropped to its non-transparent area.
     *
     * Selection rules:
     * <ul>
     * <li>The image with the biggest pixel area (width * height after cropping) wins, because favicons get downscaled anyway and a bigger
     * source image results in better quality, especially on highDPI screens.</li>
     * <li>The number of unique colors is only used as tie breaker for images of the same size (e.g. 16x16 with 16 colors vs. 16x16 with 32
     * bit).</li>
     * <li>If size and number of colors are equal, the first image wins.</li>
     * </ul>
     * The result does not depend on the order of the candidates (apart from complete ties).
     *
     * @return the best image or null if there is no image bigger than 1x1
     */
    private static BufferedImage returnBestImage(List<BufferedImage> images) {
        if (images == null || images.size() == 0) {
            return null;
        }
        BufferedImage ret = null;
        int bestSize = -1;
        int bestColors = -1;
        for (final BufferedImage img : images) {
            /*
             * remove transparent area
             */
            final BufferedImage croppedImage = getCroppedImage(img, false);
            if (croppedImage == null) {
                continue;
            }
            final int size = croppedImage.getWidth() * croppedImage.getHeight();
            if (ret != null && size < bestSize) {
                /* smaller than current best image -> never better, no matter how many colors it has */
                continue;
            }
            /* countColors is expensive, so it is only computed for images that are at least as big as the current best image */
            final int colors = countColors(croppedImage);
            if (ret != null && size == bestSize && colors <= bestColors) {
                /* same size but not more colors -> keep current best image */
                continue;
            }
            /* either first image, bigger image or same size with more colors */
            ret = croppedImage;
            bestSize = size;
            bestColors = colors;
        }
        return getValidImage(ret);
    }

    public static BufferedImage getCroppedImage(final BufferedImage source, final boolean keepAspectRation) {
        if (source == null) {
            return null;
        }
        try {
            final Raster alphaRaster = source.getAlphaRaster();
            if (alphaRaster == null) {
                return source;
            }
            int[] alphaPixel = new int[alphaRaster.getNumBands()];
            final int width = alphaRaster.getWidth();
            final int height = alphaRaster.getHeight();
            int i;
            int j;
            leftLoop: for (i = 0; i < width; i++) {
                for (j = 0; j < height; j++) {
                    if (alphaRaster.getPixel(i, j, alphaPixel)[0] != 0) {
                        break leftLoop;
                    }
                }
            }
            int x0 = i;
            topLoop: for (j = 0; j < height; j++) {
                for (i = 0; i < width; i++) {
                    if (alphaRaster.getPixel(i, j, alphaPixel)[0] != 0) {
                        break topLoop;
                    }
                }
            }
            int y0 = j;
            rightLoop: for (i = width - 1; i >= 0; i--) {
                for (j = 0; j < height; j++) {
                    if (alphaRaster.getPixel(i, j, alphaPixel)[0] != 0) {
                        break rightLoop;
                    }
                }
            }
            int x1 = i + 1;
            bottomLoop: for (j = height - 1; j >= 0; j--) {
                for (i = 0; i < width; i++) {
                    if (alphaRaster.getPixel(i, j, alphaPixel)[0] != 0) {
                        break bottomLoop;
                    }
                }
            }
            int y1 = j + 1;
            if (x0 == width && y0 == height) {
                // complete empty/transparent picture
                return source;
            }
            if (x0 == 0 && y0 == 0 && x1 - x0 == width && y1 - y0 == height) {
                // no transparency cropping
                return source;
            }
            final boolean sameAspectRatio = Math.abs(((double) width / height) - ((double) (x1 - x0) / (y1 - y0))) < 0.00001d;
            if (keepAspectRation && !sameAspectRatio) {
                // IDEA: adapt region to have same aspect ratio
                return source;
            }
            return source.getSubimage(x0, y0, x1 - x0, y1 - y0);
        } catch (Throwable e) {
            LogController.CL().log(e);
        }
        return source;
    }

    private static boolean isSameDomain(String[] siteSupportedNames, final String domain, final URL url) {
        if (siteSupportedNames == null || siteSupportedNames.length == 0) {
            return false;
        }
        for (String siteSupportedName : siteSupportedNames) {
            if (StringUtils.equalsIgnoreCase(siteSupportedName, domain) || StringUtils.equalsIgnoreCase(siteSupportedName, url.getHost())) {
                return true;
            }
        }
        return false;
    }

    private static boolean isSameDomain(final Browser br, String host, String[] siteSupportedNames) throws IOException {
        if (host.matches("(?i)^https?://.+")) {
            host = new URL(host).getHost();
        }
        final URL url = br != null ? br._getURL() : null;
        if (url == null) {
            return false;
        }
        if (StringUtils.containsIgnoreCase(url.getHost(), host)) {
            return true;
        }
        final String domain = br.getHost();
        if (!StringUtils.containsIgnoreCase(domain, host)) {
            // different domain, check for different ld
            final String domainTld = PublicSuffixList.getInstance().getTopLevelDomain(domain);
            final String hostTld = PublicSuffixList.getInstance().getTopLevelDomain(host);
            final String compareDomain = domain.replaceAll("\\." + domainTld + "$", "");
            final String compareHost = host.replaceAll("\\." + hostTld + "$", "");
            if (StringUtils.equalsIgnoreCase(compareDomain, compareHost)) {
                return true;
            }
        }
        if (siteSupportedNames == null) {
            siteSupportedNames = getSiteSupportedNames(br, host);
        }
        if (isSameDomain(siteSupportedNames, domain, url)) {
            return true;
        }
        final List<LazyCrawlerPlugin> crawlerPlugins = CrawlerPluginController.getInstance().list(true);
        for (LazyCrawlerPlugin crawlerPlugin : crawlerPlugins) {
            final String[] siteSupported = crawlerPlugin.getSitesSupported();
            if (isSameDomain(siteSupported, domain, url)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Returns the siteSupportedNames of the host plugin responsible for host, or null if there is none.
     */
    private static String[] getSiteSupportedNames(final Browser br, final String host) {
        final LazyHostPlugin existingHostPlugin = getPlugin(host);
        if (existingHostPlugin == null) {
            return null;
        }
        final LogInterface logger = br.getLogger();
        try {
            final PluginForHost pluginInstance = existingHostPlugin.newInstance(null, false);
            pluginInstance.setBrowser(br.cloneBrowser());
            pluginInstance.setLogger(br.getLogger());
            return pluginInstance.siteSupportedNames();
        } catch (Exception e) {
            logger.log(e);
            return null;
        }
    }

    private static String getFavIconURL(final Browser br, String url) {
        try {
            if (url == null) {
                return null;
            }
            if (url.matches("(?i)^data:image/.+")) {
                return url;
            }
            if (!url.matches("(?i)^https?://.+")) {
                if (br == null) {
                    return null;
                }
                return getFavIconURL(br, URLHelper.parseLocation(br._getURL(), url));
            }
            /* use getPath not getFile so that a query string (e.g. favicon.ico?v=3) does not hide the file extension */
            final String path = new URL(url).getPath();
            if (!path.matches("(?i).+\\.(ico|png|svg|jpe?g|gif)$")) {
                /* Unsupported or no file extension */
                return null;
            }
            return url;
        } catch (MalformedURLException e) {
            return null;
        }
    }

    public static BufferedImage download_FavIconTag(final String host, String[] siteSupportedNames, LogInterface logger) throws IOException {
        final Browser favBr = new Browser();
        if (CONFIG.isDebugFlag()) {
            favBr.setDebug(true);
            favBr.setVerbose(true);
        }
        favBr.getHeaders().put("User-Agent", "Mozilla/5.0 (X11; Linux x86_64; rv:145.0) Gecko/20100101 Firefox/145.0");
        favBr.getHeaders().put("Sec-GPC", "1");
        favBr.getHeaders().put("Connection", "keep-alive");
        favBr.getHeaders().put("Upgrade-Insecure-Requests", "1");
        favBr.getHeaders().put("Sec-Fetch-Dest", "document");
        favBr.getHeaders().put("Sec-Fetch-Mode", "navigate");
        favBr.getHeaders().put("Sec-Fetch-Site", "none");
        favBr.getHeaders().put("Sec-Fetch-User", "?1");
        favBr.getHeaders().put("Priority", "u=0, i");
        favBr.getHeaders().put("Pragma", "no-cache");
        favBr.getHeaders().put("Cache-Control", "no-cache");
        favBr.getHeaders().put("TE", "trailers");
        favBr.setLogger(logger);
        favBr.setConnectTimeout(10000);
        favBr.setReadTimeout(10000);
        final List<String> websites = new ArrayList<String>();
        if (getFavIconURL(null, host) == null) {
            if (host.matches("(?i)^https?://.+")) {
                websites.add(host);
            } else {
                websites.addAll(Arrays.asList(new String[] { "https://" + host, "http://" + host }));
            }
        }
        for (final String website : websites) {
            favBr.setRequest(null);
            boolean retryFlag = true;
            int retryCount = 0;
            while (retryFlag) {
                retryFlag = false;
                try {
                    try {
                        favBr.getPage(website);
                    } catch (BrowserException e) {
                        if (!Exceptions.containsInstanceOf(e, UnknownHostException.class)) {
                            throw e;
                        }
                        final String unknownHost = Browser.getHost(website, true);
                        if (!StringUtils.startsWithCaseInsensitive(host, "www.")) {
                            final String wwwSubdomain = website.replaceFirst("//" + Pattern.quote(unknownHost), "//www." + unknownHost);
                            favBr.getPage(wwwSubdomain);
                        }
                    }
                    final String redirectOrRefresh = StringUtils.firstNotEmpty(favBr.getRedirectLocation(), favBr.getRequest().getHTMLRefresh());
                    if (redirectOrRefresh != null) {
                        try {
                            Thread.sleep(500);// helps to prevent cloudflare protection
                        } catch (InterruptedException e) {
                            throw new IOException(e);
                        }
                        favBr.getPage(redirectOrRefresh);
                        if (!isSameDomain(favBr, host, siteSupportedNames)) {
                            throw new IOException("redirect to different domain?" + favBr._getURL().getHost() + "!=" + host);
                        }
                    }
                    final BufferedImage ret = download_FavIconTag(favBr, host);
                    if (ret != null) {
                        return ret;
                    }
                    break;
                } catch (final BrowserException e) {
                    if (Exceptions.containsInstanceOf(e, UnknownHostException.class, ConnectException.class, SocketTimeoutException.class)) {
                        continue;
                    }
                    if (e instanceof BlockedByException) {
                        try {
                            final Browser br = favBr.cloneBrowser();
                            br.setRequest(null);
                            final BufferedImage ret = download_FavIconTag(br, favBr.getURL("/favicon.ico").toExternalForm(), host);
                            if (ret != null) {
                                return ret;
                            }
                        } catch (IOException ignore) {
                        }
                    }
                    logger.log(e);
                    final Request reg = e.getRequest();
                    if (reg != null && reg.getHttpConnection() != null && reg.getHttpConnection().getResponseCode() == 429 && retryCount == 0) {
                        try {
                            int timeout = 2000;// minimum wait timeout
                            final String retryAfter = reg.getHttpConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_RETRY_AFTER);
                            if (retryAfter != null && retryAfter.matches("^\\s*\\d+\\s*$")) {
                                timeout = Math.max(timeout, Math.min(Integer.parseInt(retryAfter) * 1000, 20000));
                            }
                            Thread.sleep(timeout);
                            retryFlag = true;
                            retryCount = 1;
                        } catch (InterruptedException ignore) {
                        }
                    }
                } catch (final IOException e) {
                    logger.log(e);
                }
            }
        }
        if (websites.size() > 0) {
            return null;
        }
        // host is absolute URL
        return download_FavIconTag(favBr, host);
    }

    private static BufferedImage load_DataURL(DataURLInputStream is) throws IOException {
        /* getMimeType returns the header without the "data:" prefix and may contain parameters, e.g. "image/svg+xml;charset=utf-8" */
        String mimeType = is.getMimeType();
        if (mimeType == null) {
            return null;
        }
        final int parameterIndex = mimeType.indexOf(';');
        if (parameterIndex != -1) {
            mimeType = mimeType.substring(0, parameterIndex);
        }
        mimeType = mimeType.trim().toLowerCase(Locale.ROOT);
        if ("image/svg+xml".equals(mimeType)) {
            final SVGFactory svgFactory = IconIO.getSvgFactory();
            if (svgFactory == null) {
                return null;
            }
            final Image img = svgFactory.getImageFromSVG(is, null, 64, 64, null);
            if (img == null) {
                return null;
            }
            return IconIO.toBufferedImage(img);
        }
        if (!mimeType.startsWith("image/")) {
            return null;
        }
        final byte[] dataURL_data = IO.readStream(-1, is);
        if (CompiledFiletypeFilter.ImageExtensions.ICO.matchesMagic(new ByteArrayInputStream(dataURL_data))) {
            /* ImageIO cannot read ico */
            return returnBestImage(ICODecoder.read(new ByteArrayInputStream(dataURL_data)));
        }
        final BufferedImage ret = ImageIO.read(new ByteArrayInputStream(dataURL_data));
        if (ret != null) {
            return ret;
        }
        final GifDecoder gifDecoder = new GifDecoder();
        if (gifDecoder.read(new ByteArrayInputStream(dataURL_data)) != 0) {
            return null;
        }
        return gifDecoder.getImage();
    }

    public static BufferedImage download_FavIconTag(Browser favBr, String host) throws IOException {
        final Set<String> favIconURLs = getFavIconURLs(favBr, host);
        final LogInterface logger = favBr.getLogger();
        final List<BufferedImage> images = new ArrayList<BufferedImage>();
        for (final String favIconURL : favIconURLs) {
            boolean retryFlag = true;
            int retryCount = 0;
            while (retryFlag) {
                retryFlag = false;
                try {
                    final DataURLInputStream is = DataURLInputStream.open(favIconURL);
                    if (is != null) {
                        try {
                            final BufferedImage ret = load_DataURL(is);
                            if (ret != null) {
                                images.add(ret);
                            }
                        } finally {
                            is.close();
                        }
                        continue;
                    }
                    final Browser brc = favBr.cloneBrowser();
                    try {
                        Thread.sleep(250);// helps to prevent cloudflare protection
                    } catch (InterruptedException e) {
                        throw new IOException(e);
                    }
                    final BufferedImage ret = download_FavIconTag(brc, favIconURL, host);
                    if (ret != null) {
                        images.add(ret);
                    }
                } catch (final BrowserException e) {
                    logger.log(e);
                    final Request reg = e.getRequest();
                    if (reg != null && reg.getHttpConnection() != null && reg.getHttpConnection().getResponseCode() == 429 && retryCount < 1) {
                        try {
                            int timeout = 2000;// minimum wait timeout
                            final String retryAfter = reg.getHttpConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_RETRY_AFTER);
                            if (retryAfter != null && retryAfter.matches("^\\s*\\d+\\s*$")) {
                                timeout = Math.max(timeout, Math.min(Integer.parseInt(retryAfter) * 1000, 20000));
                            }
                            Thread.sleep(timeout);
                            retryFlag = true;
                            retryCount++;
                        } catch (InterruptedException ignore) {
                        }
                    }
                } catch (IOException e) {
                    logger.log(e);
                }
            }
        }
        final BufferedImage best = returnBestImage(images);
        return best;
    }

    public static Set<String> getFavIconURLs(final Browser favBr, final String host) throws IOException {
        final Set<String> ret = new LinkedHashSet<String>();
        String favIconURL = getFavIconURL(null, host);
        if (favIconURL != null) {
            ret.add(favIconURL);
            return ret;
        }
        final String requestHtml = favBr.toString().replaceAll("(?s)<!--.*?-->", "");
        String urls[] = new Regex(requestHtml, "(?i) rel\\s*=\\s*(['\"])" + "(?:icon|shortcut icon|apple-touch-icon)" + "\\1[^<>]*? href\\s*=\\s*((?:\"[^\"]*\")|(?:'[^']*'))").getColumn(1);
        if (urls != null) {
            for (String url : urls) {
                url = url.substring(1, url.length() - 1);// remove " and '
                favIconURL = getFavIconURL(favBr, url);
                if (favIconURL == null) {
                    continue;
                }
                ret.add(favIconURL);
            }
        }
        urls = new Regex(requestHtml, "(?i) href\\s*=\\s*((?:\"[^\"]*\")|(?:'[^']*'))[^<>]*? rel\\s*=\\s*(['\"])" + "(?:icon|shortcut icon|apple-touch-icon)" + "\\2").getColumn(0);
        if (urls != null) {
            for (String url : urls) {
                url = url.substring(1, url.length() - 1);// remove " and '
                favIconURL = getFavIconURL(favBr, url);
                if (favIconURL == null) {
                    continue;
                }
                ret.add(favIconURL);
            }
        }
        ret.add("/favicon.ico");
        return ret;
    }

    public static BufferedImage download_FavIconTag(final Browser favBr, final String url, final String host) throws IOException {
        URLConnectionAdapter con = null;
        byte[] bytes = null;
        try {
            final LogInterface logger = favBr.getLogger();
            if (StringUtils.isEmpty(url)) {
                return null;
            }
            /* favicon tag with ico extension */
            favBr.setFollowRedirects(true);
            GetRequest request = favBr.createGetRequest(url);
            con = favBr.openRequestConnection(request);
            /* we use bufferedinputstream to reuse it later if needed */
            bytes = IO.readStream(-1, con.getInputStream());
            if (!con.isOK() || StringUtils.containsIgnoreCase(con.getContentType(), "text") || bytes == null || bytes.length == 0) {
                favBr.getLogger().info("could not load:" + con.getURL() + "|responseCode=" + con.getResponseCode() + "|contentType=" + con.getContentType() + "|bytes=" + (bytes == null ? -1 : bytes.length));
                return null;
            }
            try {
                if (CompiledFiletypeFilter.ImageExtensions.PNG.matchesMagic(new ByteArrayInputStream(bytes))) {
                    // PNG magic
                    return downloadImage(con, logger, new ByteArrayInputStream(bytes));
                }
                if (CompiledFiletypeFilter.ImageExtensions.ICO.matchesMagic(new ByteArrayInputStream(bytes))) {
                    // ICO magic
                    return returnBestImage(ICODecoder.read(new ByteArrayInputStream(bytes)));
                }
                if (CompiledFiletypeFilter.ImageExtensions.SVG.matchesMagic(new ByteArrayInputStream(bytes)) || CompiledFiletypeFilter.ImageExtensions.SVG.matchesMimeType(con.getContentType()) > 0) {
                    // SVG magic
                    return downloadImage(con, logger, new ByteArrayInputStream(bytes));
                }
                if (CompiledFiletypeFilter.ImageExtensions.JPG.matchesMagic(new ByteArrayInputStream(bytes))) {
                    // JPG magic
                    return downloadImage(con, logger, new ByteArrayInputStream(bytes));
                }
                if (CompiledFiletypeFilter.ImageExtensions.GIF.matchesMagic(new ByteArrayInputStream(bytes))) {
                    // smalltest single pixel gif has 35 bytes with GIF magic
                    final GifDecoder gifDecoder = new GifDecoder();
                    /* reset bufferedinputstream to begin from start */
                    if (gifDecoder.read(new ByteArrayInputStream(bytes)) == 0) {
                        return getValidImage(gifDecoder.getImage());
                    }
                }
                /* try first with iconloader */
                if (bytes.length < 70) {
                    // smallest single pixel icon has 70 bytes
                    return downloadImage(con, logger, new ByteArrayInputStream(bytes));
                }
                List<BufferedImage> ret = null;
                try {
                    ret = ICODecoder.read(new ByteArrayInputStream(bytes));
                } catch (final IOException e) {
                    final String max = new Regex(e.getMessage(), "Failed to read image #\\s*(\\d+)").getMatch(0);
                    if (max == null || Integer.parseInt(max) <= 1) {
                        throw e;
                    }
                    final byte[] copy = bytes.clone();
                    // TODO: modify icon header to stop at last okay image
                    ret = ICODecoder.read(new ByteArrayInputStream(copy));
                }
                final BufferedImage img = returnBestImage(ret);
                if (img == null) {
                    throw new Throwable("Try again with other ImageLoader");
                }
                return img;
            } catch (Throwable e) {
                /* maybe redirect to different icon format? */
                final BufferedImage img = downloadImage(con, logger, new ByteArrayInputStream(bytes));
                if (img != null) {
                    return img;
                }
                logger.log(e);
            }
            return null;
        } finally {
            if (con != null) {
                try {
                    con.disconnect();
                } catch (final Throwable e) {
                }
            }
        }
    }

    /**
     * Returns the image if it is bigger than 1x1, else null. Some hosts have fake favicons with 1x1 size.
     */
    private static BufferedImage getValidImage(final BufferedImage img) {
        if (img == null || img.getHeight() <= 1 || img.getWidth() <= 1) {
            return null;
        }
        return img;
    }

    public static BufferedImage downloadFavIcon(LogSource logger, List<String> hosts, String[] siteSupportedNames) {
        for (final String host : hosts) {
            final BufferedImage ret = downloadFavIcon(logger, host, siteSupportedNames);
            if (ret != null) {
                return ret;
            }
        }
        return null;
    }

    @Deprecated
    public static BufferedImage downloadFavIcon(String host) {
        final LogSource logger = LogController.getFastPluginLogger("FavIcons");
        try {
            if (CONFIG.isDebugFlag()) {
                logger.setInstantFlush(true);
            }
            return downloadFavIcon(logger, host, null);
        } finally {
            logger.close();
        }
    }

    /**
     * downloads a favicon from the given host, icon must be bigger than 1x1, cause some hosts have fake favicon.ico with 1x1 size
     */
    public static BufferedImage downloadFavIcon(LogSource logger, String host, String[] siteSupportedNames) {
        logger.info("Download FavIcon for " + host);
        BufferedImage ret = null;
        try {
            /* first try to get the FavIcon specified in FavIconTag */
            ret = download_FavIconTag(host, siteSupportedNames, logger);
        } catch (Throwable ignore) {
            logger.log(ignore);
        } finally {
            if (ret != null) {
                logger.clear();
            } else {
                logger.severe("Could not download FavIcon for " + host);
            }
        }
        return ret;
    }

    /**
     * Decodes the image (svg, any ImageIO format or gif) from the given stream.
     *
     * @return the image, or null if it cannot be decoded or is not a valid favicon (see getValidImage)
     */
    private static BufferedImage downloadImage(URLConnectionAdapter con, LogInterface logger, InputStream is) {
        try {
            if (StringUtils.endsWithCaseInsensitive(con.getURL().getPath(), ".svg")) {
                try {
                    final SVGFactory svgFactory = IconIO.getSvgFactory();
                    if (svgFactory == null) {
                        return null;
                    }
                    final Image img = svgFactory.getImageFromSVG(is, null, 64, 64, null);
                    if (img == null) {
                        return null;
                    }
                    final BufferedImage ret = IconIO.toBufferedImage(img);
                    if (ret != null) {
                        return getValidImage(ret);
                    }
                } catch (IOException e) {
                    logger.log(e);
                    return null;
                }
            }
            final BufferedImage ret = ImageIO.read(is);
            if (ret != null) {
                return getValidImage(ret);
            }
            /* workaround for gif images */
            final GifDecoder d = new GifDecoder();
            /* reset bufferedinputstream to begin from start */
            is.reset();
            if (d.read(is) != 0) {
                return null;
            }
            return getValidImage(d.getImage());
        } catch (Throwable e) {
            logger.log(e);
        }
        return null;
    }
}
