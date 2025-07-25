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
package jd.http;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.Vector;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Pattern;

import jd.http.requests.FormData;
import jd.http.requests.GetRequest;
import jd.http.requests.HeadRequest;
import jd.http.requests.PostFormDataRequest;
import jd.http.requests.PostRequest;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.parser.html.InputField;

import org.appwork.exceptions.WTFException;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Exceptions;
import org.appwork.utils.KeyValueStringEntry;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.logging2.ConsoleLogImpl;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.PublicSuffixList;
import org.appwork.utils.net.URLHelper;
import org.appwork.utils.net.httpconnection.HTTPConnection;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils.IPVERSION;
import org.appwork.utils.net.httpconnection.HTTPProxy;
import org.appwork.utils.net.httpconnection.ProxyAuthException;
import org.appwork.utils.net.httpconnection.SSLSocketStreamOptionsModifier;
import org.appwork.utils.parser.UrlQuery;

public class Browser {
    public class BlockedByException extends BrowserException {
        private static final long          serialVersionUID = 9215538386994469323L;
        private final BlockedTypeInterface blockedBy;

        public BlockedTypeInterface getBlockedBy() {
            return this.blockedBy;
        }

        public BlockedByException(final Request request, final BlockedTypeInterface blockedBy) {
            super(blockedBy.getLabel(), request, null);
            this.blockedBy = blockedBy;
        }
    }

    public class BrowserException extends IOException {
        private static final long serialVersionUID = 1509988898224037320L;
        private Request           request          = null;

        public BrowserException(final String message, final Request request, final Exception e) {
            super(message, e);
            this.request = request;
        }

        @Override
        public String getMessage() {
            final Request request = this.getRequest();
            if (request != null) {
                final StringBuilder sb = new StringBuilder();
                try {
                    sb.append(request.printHeaders());
                } catch (Exception e) {
                    Exceptions.getStackTrace(sb, e);
                }
                final String message = super.getMessage();
                if (StringUtils.isNotEmpty(message)) {
                    sb.append("\r\n");
                    sb.append(message);
                }
                return sb.toString();
            } else {
                return super.getMessage();
            }
        }

        public Request removeRequest() {
            final Request ret = this.request;
            this.request = null;
            return ret;
        }

        public String getSuperMessage() {
            return super.getMessage();
        }

        public Request getRequest() {
            return this.request;
        }
    }

    private static final HashMap<String, Cookies> COOKIES                   = new HashMap<String, Cookies>();
    private static ProxySelectorInterface         GLOBAL_PROXY              = null;
    private static LogInterface                   LOGGER                    = new ConsoleLogImpl();
    private static int                            TIMEOUT_CONNECT           = 30000;
    private static int                            TIMEOUT_READ              = 30000;
    private Boolean                               defaultSSLTrustALL        = null;
    protected static IPVERSION                    GLOBAL_IPVERSION          = null;
    protected IPVERSION                           ipVersion                 = null;
    private boolean                               throwExceptionOnBlockedBy = true;

    public static IPVERSION getGlobalIPVersion() {
        return Browser.GLOBAL_IPVERSION;
    }

    public static void setGlobalIPVersion(IPVERSION ipVersion) {
        Browser.GLOBAL_IPVERSION = ipVersion;
    }

    public IPVERSION getIPVersion() {
        final IPVERSION ipVersion = this.ipVersion;
        if (ipVersion != null) {
            return ipVersion;
        } else {
            return Browser.getGlobalIPVersion();
        }
    }

    public void setIPVersion(IPVERSION ipVersion) {
        this.ipVersion = ipVersion;
    }

    public boolean getThrowExceptionOnBlockedBy(Request request) {
        return this.throwExceptionOnBlockedBy && request != null;
    }

    public void setThrowExceptionOnBlockedBy(final boolean b) {
        this.throwExceptionOnBlockedBy = b;
    }

    public static ProxySelectorInterface _getGlobalProxy() {
        return Browser.GLOBAL_PROXY;
    }

    public static int getGlobalReadTimeout() {
        return Browser.TIMEOUT_READ;
    }

    /** Returns domain without subdomain. */
    public static String getHost(final String url) {
        return Browser.getHost(url, false);
    }

    /** Returns domain without subdomain. */
    public static String getHost(final URL url) {
        return Browser.getHost(url, false);
    }

    private static final Pattern HOST_IP_PATTERN1     = Pattern.compile("^(?:[a-z0-9]{2,64}://)?[a-z0-9]{2,64}://(?:[^\\s@/]+?@)?(\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3})(?::\\d+)?(?:/|/.+|)$", Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
    private static final Pattern HOST_IP_PATTERN2     = Pattern.compile("^(\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3})(?::\\d+)?(?:/|/.+|)$", Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
    private static final Pattern HOST_DOMAIN_PATTERN  = Pattern.compile("^(?:[a-z0-9]{2,64}://)?[a-z0-9]{2,64}://(?:[^\\s@/]+?@)?([a-z0-9\\-\\.]+?)(/|$|:\\d+$|:\\d+/)", Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
    private static final Pattern HOST_DOMAIN_PATTERN2 = Pattern.compile("^(?:[^\\s@/]+?@)?([a-z0-9\\-\\.]+?)(/|$|:\\d+$|:\\d+/)", Pattern.CASE_INSENSITIVE | Pattern.DOTALL);

    /*
     * this method extracts domain/ip from given url. optional keeps existing subdomains
     */
    public static String getHost(final String url, final boolean includeSubDomains) {
        if (url == null) {
            return null;
        }
        final String trimURL = url.trim();
        try {
            return Browser.getHost(new URL(trimURL), includeSubDomains);
        } catch (Throwable e) {
        }
        /* direct ip with protocol */
        String ret = new Regex(trimURL, Browser.HOST_IP_PATTERN1).getMatch(0);
        if (ret == null) {
            /* direct ip without protocol */
            ret = new Regex(trimURL, Browser.HOST_IP_PATTERN2).getMatch(0);
        }
        if (ret != null) {
            return ret;
        }
        /* normal url with protocol */
        ret = new Regex(trimURL, Browser.HOST_DOMAIN_PATTERN).getMatch(0);
        if (ret == null) {
            /* without procotol */
            ret = new Regex(trimURL, Browser.HOST_DOMAIN_PATTERN2).getMatch(0);
        }
        if (ret != null && includeSubDomains == false) {
            /* cut off all subdomains */
            final PublicSuffixList psl = PublicSuffixList.getInstance();
            if (psl != null) {
                final String domain = psl.getDomain(ret.toLowerCase(Locale.ENGLISH));
                if (domain != null) {
                    return domain;
                }
            }
            int indexPoint = ret.lastIndexOf(".");
            indexPoint = ret.lastIndexOf(".", indexPoint - 1);
            if (indexPoint >= 0) {
                /* we enter this branch only if a subdomain exists */
                ret = ret.substring(indexPoint + 1);
            }
        }
        if (ret != null) {
            return ret.toLowerCase(Locale.ENGLISH);
        } else {
            return url;
        }
    }

    public static String getSubdomain(final URL uri, final boolean removeTrailingDelimiter) {
        final String withoutSubdomain = Browser.getHost(uri, false);
        final String withSubdomain = Browser.getHost(uri, true);
        if (StringUtils.equals(withoutSubdomain, withSubdomain)) {
            return null;
        } else {
            return withSubdomain.replaceFirst((removeTrailingDelimiter ? "\\." : "") + Pattern.quote(withoutSubdomain) + "$", "");
        }
    }

    public static String getSubdomain(final String url, final boolean removeTrailingDelimiter) throws MalformedURLException {
        return Browser.getSubdomain(new URL(url), removeTrailingDelimiter);
    }

    public static String getHost(final URL uri, final boolean includeSubDomains) {
        if (uri == null) {
            return null;
        }
        String ret = uri.getHost();
        if (Browser.HOST_IP_PATTERN2.matcher(ret).matches()) {
            return ret;
        }
        if (ret != null && includeSubDomains == false) {
            /* cut off all subdomains */
            final PublicSuffixList psl = PublicSuffixList.getInstance();
            if (psl != null) {
                final String domain = psl.getDomain(ret.toLowerCase(Locale.ENGLISH));
                if (domain != null) {
                    return domain;
                }
            }
            int indexPoint = ret.lastIndexOf(".");
            indexPoint = ret.lastIndexOf(".", indexPoint - 1);
            if (indexPoint >= 0) {
                /* we enter this branch only if a subdomain exists */
                ret = ret.substring(indexPoint + 1);
            }
        }
        if (ret != null) {
            return ret.toLowerCase(Locale.ENGLISH);
        } else {
            return ret;
        }
    }

    /**
     * Sets the global connect timeout
     *
     * @param valueMS
     */
    public static void setGlobalConnectTimeout(final int valueMS) {
        Browser.TIMEOUT_CONNECT = valueMS;
    }

    public static void setGlobalLogger(final LogInterface logger) {
        Browser.LOGGER = logger;
    }

    public static void setGlobalProxy(final ProxySelectorInterface p) {
        Browser.GLOBAL_PROXY = p;
    }

    /**
     * Sets the global readtimeout in ms
     *
     * @param valueMS
     */
    public static void setGlobalReadTimeout(final int valueMS) {
        Browser.TIMEOUT_READ = valueMS;
    }

    public static void setGlobalVerbose(final boolean b) {
        Browser.VERBOSE = b;
    }

    private boolean        keepResponseContentBytes = false;
    private int[]          allowedResponseCodes     = new int[0];
    private static boolean VERBOSE                  = false;

    /**
     * Lädt über eine URLConnection eine Datei herunter. Zieldatei ist file.
     *
     * @param file
     * @param con
     * @return Erfolg true/false
     * @throws IOException
     */
    public static void download(final File file, final URLConnectionAdapter con) throws IOException {
        if (con == null) {
            throw new IOException("con is null");
        }
        if (file.isFile()) {
            if (file.exists() && !file.delete()) {
                throw new IOException("Could not overwrite file: " + file);
            }
        }
        final File parentFile = file.getParentFile();
        if (parentFile != null && !parentFile.exists()) {
            parentFile.mkdirs();
        }
        FileOutputStream fos = null;
        InputStream input = null;
        boolean okay = false;
        try {
            input = con.getInputStream();
            file.createNewFile();
            fos = new FileOutputStream(file, false);
            final long length;
            if (con.isContentDecoded()) {
                length = -1;
            } else {
                length = con.getCompleteContentLength();
            }
            final byte[] b = new byte[32767];
            long done = 0;
            int len;
            while ((len = input.read(b)) != -1) {
                fos.write(b, 0, len);
                done += len;
            }
            if (length > 0 && length != done) {
                throw new IOException("Incomplete:" + length + "<=>" + done);
            }
            okay = true;
        } finally {
            try {
                input.close();
            } catch (final Throwable e) {
            }
            try {
                fos.close();
            } catch (final Throwable e) {
            }
            if (okay == false) {
                file.delete();
            }
        }
    }

    public static int getGlobalConnectTimeout() {
        return Browser.TIMEOUT_CONNECT;
    }

    public static LogInterface getGlobalLogger() {
        return Browser.LOGGER;
    }

    /**
     * Sets given limit on given host excluding subdomains. </br>
     *
     * @param requestInterval
     *            Milliseconds
     * @param host
     *            Domain
     */
    public static void setRequestIntervalLimitGlobal(final String host, final int requestInterval) {
        Browser.setRequestIntervalLimitGlobal(host, false, requestInterval);
    }

    public static void setRequestIntervalLimitGlobal(final String host, boolean includeSubdomain, final int requestInterval) {
        final String domain = Browser.getHost(host, includeSubdomain);
        if (domain != null) {
            synchronized (Browser.REQUEST_INTERVAL_MAP) {
                boolean notifyFlag = false;
                if (requestInterval <= 0) {
                    final Integer existingRequestInterval = Browser.REQUEST_INTERVAL_MAP.remove(domain);
                    final AtomicLong lastRequest = Browser.REQUESTTIME_MAP.remove(domain);
                    if (lastRequest != null) {
                        final long timeStamp = lastRequest.getAndSet(-1);
                        final Object[] nextLimitEntry = Browser.getValueFromMap(Browser.REQUESTTIME_MAP, domain);
                        if (nextLimitEntry != null) {
                            final AtomicLong entryRequest = (AtomicLong) nextLimitEntry[1];
                            if (timeStamp > entryRequest.get()) {
                                entryRequest.set(timeStamp);
                            }
                        }
                    }
                    notifyFlag = existingRequestInterval != null || lastRequest != null;
                } else {
                    final Integer existingRequestInterval = Browser.REQUEST_INTERVAL_MAP.put(domain, requestInterval);
                    notifyFlag = existingRequestInterval == null || existingRequestInterval.intValue() != requestInterval;
                }
                if (notifyFlag) {
                    Browser.REQUEST_INTERVAL_MAP.notifyAll();
                }
            }
        }
    }

    /**
     * always synchronize on REQUEST_INTERVAL_LIMIT_MAP
     */
    private static final HashMap<String, Integer>       REQUEST_INTERVAL_MAP        = new HashMap<String, Integer>();
    private static final HashMap<String, AtomicLong>    REQUESTTIME_MAP             = new HashMap<String, AtomicLong>();
    private static final HashMap<String, List<Request>> REQUEST_INTERVAL_QUEUE      = new HashMap<String, List<Request>>();
    private static final HashMap<String, Integer>       REQUESTS_BURST_INTERVAL_MAP = new HashMap<String, Integer>();
    private static final HashMap<String, Vector<Long>>  REQUESTS_BURST_REQUESTS_MAP = new HashMap<String, Vector<Long>>();

    /**
     * sets request thresholds based on upper burstable limit. eg. 20(x) requests over 60000(y)[=1min]. Then on after it sets limit between
     * interval.
     *
     * @author raztoki
     * @since JD2
     * @param host
     * @param i
     *            (ms)
     * @param x
     *            (requests)
     * @param y
     *            (ms)
     * @throws Exception
     */
    public static void setBurstRequestIntervalLimitGlobal(final String host, final int requestInterval, final int burstRequests, final int burstInterval) {
        Browser.setBurstRequestIntervalLimitGlobal(host, false, requestInterval, burstRequests, burstInterval);
    }

    public static synchronized void setBurstRequestIntervalLimitGlobal(final String host, boolean includeSubdomain, final int requestInterval, final int burstRequests, final int burstInterval) {
        final String domain = Browser.getHost(host, includeSubdomain);
        if (domain == null) {
            throw new WTFException("Browser.getHost(host) returned null:" + host);
        }
        synchronized (Browser.REQUEST_INTERVAL_MAP) {
            Browser.setRequestIntervalLimitGlobal(domain, includeSubdomain, requestInterval);
            boolean notifyFlag = false;
            if (requestInterval <= 0 || burstRequests <= 0 || burstInterval <= 0) {
                notifyFlag = Browser.REQUESTS_BURST_INTERVAL_MAP.remove(domain) != null;
                // TODO merge/cleanup REQUESTS_BURST_REQUESTS_MAP
            } else {
                final Integer existingBurstInterval = Browser.REQUESTS_BURST_INTERVAL_MAP.put(domain, burstInterval);
                notifyFlag = existingBurstInterval == null || existingBurstInterval.intValue() != burstInterval;
                Vector<Long> vector = Browser.REQUESTS_BURST_REQUESTS_MAP.get(domain);
                if (vector == null) {
                    vector = new Vector<Long>(burstRequests);
                    Browser.REQUESTS_BURST_REQUESTS_MAP.put(domain, vector);
                } else if (vector.capacity() != burstRequests) {
                    vector.ensureCapacity(burstRequests);
                    notifyFlag = true;
                }
            }
            if (notifyFlag) {
                Browser.REQUEST_INTERVAL_MAP.notifyAll();
            }
        }
    }

    private static Object[] getValueFromMap(Map<String, ? extends Object> map, String host) {
        while (host.contains(".")) {
            final Object value = map.get(host);
            if (value != null) {
                return new Object[] { host, value };
            } else {
                host = host.replaceFirst("(.*?)\\.", "");
            }
        }
        return null;
    }

    private static void waitForPageAccess(final Browser browser, final Request request) throws InterruptedException {
        final String host = request.getURL().getHost();
        Set<List<Request>> requestQueues = null;
        try {
            AtomicLong lastRequest = null;
            Object[] intervalHolder = null;
            Object[] burstRequestHolder = null;
            synchronized (Browser.REQUEST_INTERVAL_MAP) {
                while (true) {
                    intervalHolder = Browser.getValueFromMap(Browser.REQUEST_INTERVAL_MAP, host);
                    final Object[] burstHolder = Browser.getValueFromMap(Browser.REQUESTS_BURST_INTERVAL_MAP, host);
                    final LinkedList<Request> requestQueue;
                    if (intervalHolder == null) {
                        // there is no limit for host, abort waitForPageAccess
                        return;
                    } else {
                        final Object[] requestQueueHolder = Browser.getValueFromMap(Browser.REQUEST_INTERVAL_QUEUE, (String) intervalHolder[0]);
                        if (requestQueueHolder == null) {
                            // there is no request queue map entry for host, create new entry
                            Browser.REQUEST_INTERVAL_QUEUE.put((String) intervalHolder[0], new LinkedList<Request>());
                            continue;
                        } else if (!StringUtils.equals((String) intervalHolder[0], (String) requestQueueHolder[0])) {
                            // there is a request queue map entry but with different host, create new entry that match host
                            final List<Request> newList = new LinkedList<Request>();
                            requestQueue = (LinkedList<Request>) requestQueueHolder[1];
                            for (final Request requestQueueEntry : requestQueue) {
                                if (StringUtils.endsWithCaseInsensitive(requestQueueEntry.getURL().getHost(), (String) intervalHolder[0])) {
                                    newList.add(requestQueueEntry);
                                }
                            }
                            Browser.REQUEST_INTERVAL_QUEUE.put((String) intervalHolder[0], newList);
                            continue;
                        } else {
                            requestQueue = (LinkedList<Request>) requestQueueHolder[1];
                        }
                        final Object[] lastRequestHolder = Browser.getValueFromMap(Browser.REQUESTTIME_MAP, (String) intervalHolder[0]);
                        if (lastRequestHolder == null) {
                            // there is no request time map entry for host, create new entry
                            Browser.REQUESTTIME_MAP.put((String) intervalHolder[0], new AtomicLong(-1));
                            continue;
                        } else if (!StringUtils.equals((String) intervalHolder[0], (String) lastRequestHolder[0])) {
                            // there is a request time map entry but with different host, create new entry with same value
                            lastRequest = (AtomicLong) lastRequestHolder[1];
                            Browser.REQUESTTIME_MAP.put((String) intervalHolder[0], new AtomicLong(lastRequest.get()));
                            continue;
                        } else {
                            lastRequest = (AtomicLong) lastRequestHolder[1];
                        }
                        if (burstHolder != null) {
                            burstRequestHolder = Browser.getValueFromMap(Browser.REQUESTS_BURST_REQUESTS_MAP, (String) burstHolder[0]);
                            if (burstRequestHolder == null) {
                                throw new WTFException("FIXME");
                            } else {
                                final long now = Time.systemIndependentCurrentJVMTimeMillis();
                                final Vector<Long> vector = (Vector<Long>) burstRequestHolder[1];
                                if (vector.capacity() - vector.size() == 0) {
                                    final long removeOlderThan = now - ((Number) burstHolder[1]).intValue();
                                    final Iterator<Long> it = vector.iterator();
                                    while (it.hasNext()) {
                                        final Long next = it.next();
                                        if (next.longValue() < removeOlderThan) {
                                            it.remove();
                                        }
                                    }
                                }
                                if (vector.capacity() - vector.size() > 0) {
                                    vector.add(now);
                                    lastRequest.set(now);
                                    // we can still do burst requests, abort waitForPageAccess
                                    return;
                                }
                            }
                        } else {
                            burstRequestHolder = null;
                        }
                    }
                    if (!requestQueue.contains(request)) {
                        requestQueue.add(request);
                        if (requestQueues == null) {
                            requestQueues = new HashSet<List<Request>>();
                        }
                        requestQueues.add(requestQueue);
                    }
                    if (requestQueue.peekFirst() != request) {
                        Browser.REQUEST_INTERVAL_MAP.wait();
                        continue;
                    } else {
                        break;
                    }
                }
            }
            final long lastRequestTimeStamp = lastRequest.get();
            final int interval = ((Number) intervalHolder[1]).intValue();
            long waitFor = Math.max(interval - (Time.systemIndependentCurrentJVMTimeMillis() - lastRequestTimeStamp), 0);
            while (lastRequest.get() == lastRequestTimeStamp && waitFor > 0) {
                synchronized (Browser.REQUEST_INTERVAL_MAP) {
                    if (!Browser.REQUESTTIME_MAP.containsKey(intervalHolder[0])) {
                        // request limit has been removed, abort waitForPageAccess
                        return;
                    }
                }
                final int sleep = (int) Math.max(1, Math.min(10, waitFor / 2));
                Thread.sleep(sleep);
                waitFor -= sleep;
            }
            synchronized (Browser.REQUEST_INTERVAL_MAP) {
                if (!Browser.REQUESTTIME_MAP.containsKey(intervalHolder[0])) {
                    // request limit has been removed, abort waitForPageAccess
                    return;
                } else {
                    final long now = Time.systemIndependentCurrentJVMTimeMillis();
                    if (lastRequest.compareAndSet(lastRequestTimeStamp, now)) {
                        if (burstRequestHolder != null) {
                            final Vector<Long> vector = (Vector<Long>) burstRequestHolder[1];
                            if (vector.capacity() - vector.size() == 0) {
                                vector.remove(vector.capacity() - 1);
                            } else {
                                vector.add(now);
                            }
                        }
                    }
                }
            }
        } finally {
            if (requestQueues != null) {
                synchronized (Browser.REQUEST_INTERVAL_MAP) {
                    for (final List<Request> requestQueue : requestQueues) {
                        requestQueue.remove(request);
                    }
                    Browser.REQUEST_INTERVAL_MAP.notifyAll();
                }
            }
        }
    }

    private String                              acceptLanguage        = "de, en-gb;q=0.9, en;q=0.8";
    /*
     * -1 means use default Timeouts
     *
     * 0 means infinite (DO NOT USE if not needed)
     */
    private int                                 connectTimeout        = -1;
    private HashMap<String, Cookies>            cookies               = new HashMap<String, Cookies>();
    private boolean                             cookiesExclusive      = true;
    private Object                              currentURL            = null;
    private String                              customCharset         = null;
    private boolean                             debug                 = false;
    private boolean                             doRedirects           = false;
    private RequestHeader                       headers;
    private int                                 limit                 = -1;
    private LogInterface                        logger                = null;
    private ProxySelectorInterface              proxy;
    private int                                 readTimeout           = -1;
    private Request                             request;
    private boolean                             verbose               = false;
    private volatile List<BlockedTypeInterface> blockedTypeInterfaces = null;

    public Browser() {
        final Thread currentThread = Thread.currentThread();
        /**
         * use BrowserSettings from current thread if available
         */
        if (currentThread != null && currentThread instanceof BrowserSettings) {
            final BrowserSettings settings = (BrowserSettings) currentThread;
            this.setProxySelector(settings.getProxySelector());
            this.setDebug(settings.isDebug());
            this.setVerbose(settings.isVerbose());
            this.setLogger(settings.getLogger());
        }
    }

    /**
     * Assures that the browser does not download any binary files in textmode
     *
     * @param request
     * @throws BrowserException
     */
    private void checkContentLengthLimit(final Request request) throws BrowserException {
        if (request != null && request.getHttpConnection() != null && !(request instanceof HeadRequest)) {
            final int limit = this.getLoadLimit();
            request.setReadLimit(limit);
            final long length = request.getHttpConnection().getLongContentLength();
            if (length >= 0 && length > limit) {
                request.disconnect();
                throw new BrowserException("Content-length too big:" + length + ">" + limit, request, null);
            }
        }
    }

    public int getDefaultLoadLimit() {
        return 16 * 1024 * 1024;
    }

    /**
     * Clears all cookies for the given URL. URL has to be a valid. Íf (url == null), all cookies are cleared.
     *
     * @param url
     */
    public void clearCookies(final String url) {
        final Map<String, Cookies> map = this.getCookies();
        synchronized (map) {
            if (url == null) {
                map.clear();
            } else {
                final String host = Browser.getHost(url);
                final Iterator<Entry<String, Cookies>> it = map.entrySet().iterator();
                while (it.hasNext()) {
                    final Entry<String, Cookies> next = it.next();
                    if (next.getKey() == null) {
                        it.remove();
                    } else if (StringUtils.equalsIgnoreCase(next.getKey(), host)) {
                        next.getValue().clear();
                        break;
                    }
                }
            }
        }
    }

    protected SSLSocketStreamOptionsModifier sslSocketStreamOptionsModifier = null;

    public SSLSocketStreamOptionsModifier getSSLSocketStreamOptions() {
        return this.sslSocketStreamOptionsModifier;
    }

    public SSLSocketStreamOptionsModifier setSSLSocketStreamOptions(SSLSocketStreamOptionsModifier sslSocketStreamOptionsModifier) {
        final SSLSocketStreamOptionsModifier ret = this.sslSocketStreamOptionsModifier;
        this.sslSocketStreamOptionsModifier = sslSocketStreamOptionsModifier;
        return ret;
    }

    public void clearHeaders() {
        if (this.headers != null) {
            this.headers.clear();
        }
    }

    /** Removes Authorizations, Headers and Cookies */
    public void clearAll() {
        this.clearAuthentications();
        this.clearCookies(null);
        this.clearHeaders();
    }

    public Browser createNewBrowserInstance() {
        return new Browser();
    }

    public Browser cloneBrowser() {
        final Browser br = this.createNewBrowserInstance();
        return this.cloneBrowser(br);
    }

    public List<BlockedTypeInterface> getBlockedTypeInterfaces() {
        final List<BlockedTypeInterface> ret = this.blockedTypeInterfaces;
        if (ret == null) {
            return new ArrayList<BlockedTypeInterface>(Arrays.asList(GenericSupportedBlockTypes.values()));
        } else {
            return ret;
        }
    }

    public List<BlockedTypeInterface> setBlockedTypeInterfaces(List<BlockedTypeInterface> blockedTypeInterfaces) {
        final List<BlockedTypeInterface> ret = this.blockedTypeInterfaces;
        this.blockedTypeInterfaces = blockedTypeInterfaces;
        return ret;
    }

    public Browser cloneBrowser(final Browser br) {
        br.acceptLanguage = this.acceptLanguage;
        br.connectTimeout = this.connectTimeout;
        br.currentURL = this.currentURL;
        br.doRedirects = this.doRedirects;
        br.defaultSSLTrustALL = this.defaultSSLTrustALL;
        br.setCustomCharset(this.customCharset);
        br.getHeaders().putAll(this.getHeaders());
        br.limit = this.limit;
        br.sslSocketStreamOptionsModifier = this.sslSocketStreamOptionsModifier;
        br.readTimeout = this.readTimeout;
        br.request = this.getRequest();
        br.cookies = this.cookies;
        br.browserParentID = this.getBrowserID();
        br.getBrowserID();
        br.authentications = this.authentications;
        br.cookiesExclusive = this.cookiesExclusive;
        br.authenticationFactory = this.authenticationFactory;
        br.debug = this.debug;
        br.verbose = this.verbose;
        br.logger = this.logger;
        br.proxy = this.proxy;
        br.blockedTypeInterfaces = this.blockedTypeInterfaces;
        br.ipVersion = this.ipVersion;
        br.keepResponseContentBytes = this.keepResponseContentBytes;
        br.allowedResponseCodes = this.allowedResponseCodes;
        br.throwExceptionOnBlockedBy = this.throwExceptionOnBlockedBy;
        return br;
    }

    public boolean containsHTML(final String regex) {
        return new Regex(this, regex).matches();
    }

    /** Wrapper, will use 'EEE, dd MMM yyyy HH:mm:ss z' as formatter value. */
    public long getCurrentServerTime(final long fallback) {
        return this.getCurrentServerTime("EEE, dd MMM yyyy HH:mm:ss z", fallback);
    }

    /**
     * Tries to convert response Header 'Date' into milliseconds-timestamp. Returns fallback on failure. Usually you'd use
     * System.currentTimeMillis as fallback value.
     */
    public long getCurrentServerTime(final String formatter, final long fallback) {
        long serverTime = -1;
        final URLConnectionAdapter con = this.getHttpConnection();
        if (con != null) {
            // lets use server time to determine time out value; we then need to adjust timeformatter reference +- time against server time
            final String dateString = con.getHeaderField("Date");
            if (dateString != null) {
                if (StringUtils.isNotEmpty(formatter)) {
                    serverTime = TimeFormatter.getMilliSeconds(dateString, formatter, Locale.ENGLISH);
                }
                if (serverTime == -1) {
                    final Date date = TimeFormatter.parseDateString(dateString);
                    serverTime = date != null ? date.getTime() : -1;
                }
            }
        }
        if (serverTime == -1) {
            /* Fallback */
            serverTime = fallback;
        }
        return serverTime;
    }

    /**
     * Creates a new Request object based on a form
     *
     * @param form
     * @return
     * @throws Exception
     */
    public Request createFormRequest(final Form form) throws IOException {
        URL base = null;
        final Request lRequest = this.getRequest();
        if (lRequest != null) {
            /* take current url as base url */
            base = lRequest.getURL();
        }
        try {
            // we have no method to validate html tags.. could be faked, or multiples
            final String baseTag = this.getRegex("<\\s*base\\s+[^>]*>").getMatch(-1);
            if (baseTag != null) {
                String sourceBase = new Regex(baseTag, "href\\s*=\\s*(\"|')(.+?)\\1").getMatch(1);
                if (sourceBase == null) {
                    sourceBase = new Regex(baseTag, "\\s+href\\s*=([^\\s]+)").getMatch(0);
                }
                if (sourceBase != null) {
                    /* take baseURL in case we've found one in current request */
                    final URL sourceBaseURL = URLHelper.createURL(sourceBase.trim());
                    // simple validation, we should only allow base to current domain! -raztoki20160304
                    final String domainHostBase = base != null ? Browser.getHost(Request.getLocation(base.toString(), lRequest)) : null;
                    final String domainSourceBase = Browser.getHost(Request.getLocation(sourceBase, lRequest));
                    if (domainHostBase != null && domainSourceBase != null && domainHostBase.equals(domainSourceBase)) {
                        base = sourceBaseURL;
                    }
                }
            }
        } catch (final Throwable e) {
        }
        final String formAction = lRequest != null ? form.getAction(lRequest.getURL()) : form.getAction(base);
        if (formAction == null) {
            throw new NullPointerException("no valid action url");
        }
        final List<FormData> requestVariables = form.getFormData();
        switch (form.getMethod()) {
        case GET:
            String varString = form.getPropertyString();
            if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                final String newVarString = this.toURLQuery(form, requestVariables);
                if (!StringUtils.equals(varString, newVarString)) {
                    throw new IOException("FixMe:" + varString + " != " + newVarString);
                }
            }
            final String getAction;
            if (varString != null && !varString.matches("[\\s]*")) {
                getAction = URLHelper.parseLocation(URLHelper.createURL(formAction), "&" + varString);
            } else {
                getAction = formAction;
            }
            return this.createGetRequest(getAction);
        case POST:
            if (form.getEncoding() == null || !form.getEncoding().toLowerCase().endsWith("form-data")) {
                return this.createPostRequest(formAction, this.toKeyValueStringEntries(form, requestVariables), form.getEncoding());
            } else {
                final PostFormDataRequest request = this.createPostFormDataRequest(formAction);
                if (form.getEncoding() != null) {
                    request.setEncodeType(form.getEncoding());
                }
                for (final FormData variable : requestVariables) {
                    request.addFormData(variable);
                }
                return request;
            }
        default:
            throw new IOException("Unsupported method:" + form.getMethod());
        }
    }

    protected String toURLQuery(Form form, List<FormData> requestVariables) throws IOException {
        final StringBuilder sb = new StringBuilder();
        for (final FormData variable : requestVariables) {
            switch (variable.getType()) {
            case VARIABLE:
                if (sb.length() > 0) {
                    sb.append("&");
                }
                sb.append(variable.getName());
                sb.append("=");
                sb.append(variable.getValue());
                break;
            default:
                if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                    throw new IOException("Cannot convert to KeyValueStringEntry:" + variable);
                }
                break;
            }
        }
        return sb.toString();
    }

    protected List<KeyValueStringEntry> toKeyValueStringEntries(Form form, List<FormData> requestVariables) throws IOException {
        final List<KeyValueStringEntry> ret = new ArrayList<KeyValueStringEntry>();
        for (final FormData variable : requestVariables) {
            switch (variable.getType()) {
            case VARIABLE:
                ret.add(new KeyValueStringEntry(variable.getName(), variable.getValue()));
                break;
            default:
                if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                    throw new IOException("Cannot convert to KeyValueStringEntry:" + variable);
                }
                break;
            }
        }
        return ret;
    }

    public GetRequest createGetRequest(String url) throws IOException {
        return new GetRequest(this.getURL(url));
    }

    public HeadRequest createHeadRequest(String url) throws IOException {
        return new HeadRequest(this.getURL(url));
    }

    public PostFormDataRequest createPostFormDataRequest(String url) throws IOException {
        return new PostFormDataRequest(this.getURL(url));
    }

    /**
     * Creates a new postrequest based an an requestVariable ArrayList
     *
     * @deprecated use {@link #createPostRequest(String, UrlQuery, String)
     *
     *
     * 
     */
    @Deprecated
    public PostRequest createPostRequest(String url, final List<KeyValueStringEntry> post, final String encoding) throws IOException {
        return this.createPostRequest(url, UrlQuery.get(post), encoding);
    }

    public PostRequest createJSonPostRequest(final String url, final Map<String, Object> postdata) throws IOException {
        return this.createJSonPostRequest(url, JSonStorage.serializeToJson(postdata));
    }

    public PostRequest createJSonPostRequest(final String url, final String jsonPostString) throws IOException {
        final PostRequest request = new PostRequest(this.getURL(url));
        request.setPostDataString(jsonPostString);
        request.setContentType("application/json; charset=UTF-8");
        return request;
    }

    public PostRequest createPostRequest(String url, UrlQuery post, final String encoding) throws IOException {
        final PostRequest request = new PostRequest(this.getURL(url));
        if (post != null) {
            request.addAll(post.list());
        }
        String requestContentType = encoding;
        final RequestHeader lHeaders = this.headers;
        if (lHeaders != null) {
            final HTTPHeader contentType = lHeaders.remove(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE);
            if (requestContentType == null && contentType != null) {
                requestContentType = contentType.getValue();
            }
        }
        if (requestContentType == null) {
            requestContentType = "application/x-www-form-urlencoded; charset=UTF-8";
        }
        request.setContentType(requestContentType);
        return request;
    }

    /**
     * Creates a new PostRequest based on a variable HashMap
     */
    public PostRequest createPostRequest(final String url, final UrlQuery post) throws IOException {
        return this.createPostRequest(url, post, null);
    }

    public boolean probeJSonContent(final String post) {
        return post != null && post.matches("(?s)^\\s*\\{.*?\\}\\s*$");
    }

    public boolean probeJSonContent(final byte post[]) {
        return post != null && post.length >= 2 && post[0] == '{' && post[post.length - 1] == '{';
    }

    /**
     * Creates a PostRequest based on a querystring
     */
    public PostRequest createPostRequest(final String url, final String post) throws MalformedURLException, IOException {
        if (this.probeJSonContent(post)) {
            return this.createJSonPostRequest(url, post);
        } else {
            return this.createPostRequest(url, Request.parseQuery(post), null);
        }
    }

    /** Follows a single redirect. */
    public String followRedirect() throws IOException {
        return this.followRedirect(false);
    }

    public String followRedirect(final boolean followAllRedirects) throws IOException {
        final Request lRequest = this.getRequest();
        if (lRequest == null) {
            throw new IllegalStateException("Request is null");
        } else if (lRequest.getLocation() != null) {
            if (lRequest.getHtmlCode() == null) {
                this.loadConnection(lRequest.getHttpConnection());
            }
            final Request redirectRequest = this.createRedirectFollowingRequest(this.request);
            return this.loadConnection(this.openRequestConnection(redirectRequest, followAllRedirects)).getHTMLSource();
        } else if (lRequest.getHtmlCode() != null) {
            return lRequest.getHTMLSource();
        } else {
            return this.loadConnection(lRequest.getHttpConnection()).getHTMLSource();
        }
    }

    /**
     * rfc2616
     *
     * @param request
     * @return
     * @throws BrowserException
     */
    public Request createRedirectFollowingRequest(final Request request) throws IOException {
        if (request == null) {
            throw new IllegalArgumentException("Request is null");
        }
        final String location = request.getLocation();
        if (StringUtils.isEmpty(location)) {
            throw new IllegalStateException("Request does not contain a redirect");
        }
        final URL newURL = this.getURL(location);
        final int responseCode = request.getHttpConnection().getResponseCode();
        final Request newRequest;
        switch (responseCode) {
        case 200:// Ok
        case 201:// Created
            if (request instanceof HeadRequest) {
                newRequest = request.cloneRequest();
            } else {
                newRequest = new GetRequest(request);
            }
            break;
        case 301:// Moved Permanently
            // The resource has been permanently moved and request method conversion from POST to GET is allowed.
            if (request instanceof HeadRequest || request instanceof GetRequest) {
                newRequest = request.cloneRequest();
            } else {
                newRequest = new GetRequest(request);
            }
            break;
        case 302:// Found
            // The resource has been temporarily moved and request method conversion from POST to GET is allowed.
        case 303:// See Other
            if (request instanceof HeadRequest || request instanceof GetRequest) {
                newRequest = request.cloneRequest();
            } else {
                newRequest = new GetRequest(request);
            }
            break;
        case 307:// Temporary Redirect
            // The resource has been temporarily moved and request method conversion from POST to GET is forbidden.
        case 308:// Permanent Redirect
            // The resource has been permanently moved and request method conversion from POST to GET is forbidden.
            newRequest = request.cloneRequest();
            break;
        default:
            final LogInterface logger = this.getLogger();
            if (logger != null && this.isVerbose()) {
                logger.log(new IllegalStateException("ResponseCode " + responseCode + " is unsupported!"));
            }
            return null;
        }
        newRequest.setURL(newURL);
        newRequest.setRedirectOrigin(request);
        return newRequest;
    }

    public Request createRequest(final Form form) throws Exception {
        return this.createFormRequest(form);
    }

    public GetRequest createRequest(final String downloadURL) throws Exception {
        return this.createGetRequest(downloadURL);
    }

    public void disconnect() {
        try {
            final Request request = this.getRequest();
            if (request != null) {
                request.disconnect();
            }
        } catch (final Throwable ignore) {
        }
    }

    /**
     * Downloads the contents behind con to file. if(con ==null), the latest request is downloaded. Useful for redirects
     *
     * @param file
     * @param con
     * @throws IOException
     */
    public void downloadConnection(final File file, final URLConnectionAdapter con) throws IOException {
        if (con == null) {
            Browser.download(file, this.getHttpConnection());
        } else {
            Browser.download(file, con);
        }
    }

    public String followConnection(boolean ignoreResponseCode) throws IOException {
        final Request request = this.getRequest();
        if (request == null) {
            throw new IllegalStateException("Request is null");
        } else if (request.getHtmlCode() != null) {
            final LogInterface logger = this.getLogger();
            if (logger != null) {
                logger.warning("Request has already been read");
            }
            return request.getHTMLSource();
        } else {
            final URLConnectionAdapter httpConnection = request.getHttpConnection();
            if (httpConnection == null || !httpConnection.isConnected()) {
                final LogInterface logger = this.getLogger();
                if (logger != null) {
                    logger.warning("Request has already been read");
                }
                return request.toString();
            } else {
                if (ignoreResponseCode) {
                    httpConnection.setAllResponseCodesAllowed(true);
                }
                return this.loadConnection(httpConnection).getHTMLSource();
            }
        }
    }

    public String followConnection() throws IOException {
        return this.followConnection(false);
    }

    public void forwardCookies(final Request request) {
        if (request != null) {
            final String host = Browser.getHost(request.getURL());
            final Cookies cookies = this.getCookies(host);
            if (cookies != null) {
                final Cookies requestCookies = request.getCookies();
                for (final Cookie cookie : cookies.getCookies()) {
                    if (!cookie.isExpired()) {
                        requestCookies.add(cookie);
                    }
                }
            }
        }
    }

    public String getAcceptLanguage() {
        return this.acceptLanguage;
    }

    /**
     * @return the allowedResponseCodes
     */
    public int[] getAllowedResponseCodes() {
        return this.allowedResponseCodes;
    }

    public String getBaseURL() throws MalformedURLException {
        final Request lRequest = this.getRequest();
        if (lRequest != null) {
            return URLHelper.getBaseURL(lRequest.getURL());
        } else {
            return null;
        }
    }

    /**
     * returns current ConnectTimeout
     *
     * @return
     */
    public int getConnectTimeout() {
        return this.connectTimeout < 0 ? Browser.TIMEOUT_CONNECT : this.connectTimeout;
    }

    public String getCookie(final String url, final String key, String pattern) {
        final Cookies cookies = this.getCookies(url);
        final Cookie cookie = cookies.get(key, pattern);
        return cookie != null ? cookie.getValue() : null;
    }

    public String getHostCookie(final String key, final String pattern) {
        return this.getCookie(this.getHost(), key, pattern);
    }

    public String getHostCookie(final String key) {
        return this.getHostCookie(key, null);
    }

    public String getCookie(final String url, final String key) {
        return this.getCookie(url, key, null);
    }

    public HashMap<String, Cookies> getCookies() {
        return this.cookiesExclusive ? this.cookies : Browser.COOKIES;
    }

    public Cookies getCookies(final String url) {
        final String host = Browser.getHost(url);
        final Map<String, Cookies> map = this.getCookies();
        synchronized (map) {
            Cookies cookies = map.get(host);
            if (cookies == null) {
                map.put(host, cookies = new Cookies());
            }
            return cookies;
        }
    }

    @Deprecated
    public Cookies getCookiesWithUserAgent(final String url) {
        final Cookies cookies = this.getCookies(url);
        String userAgent = null;
        if (this.getRequest() != null) {
            userAgent = this.getHeaders().getValue("User-Agent");
            if (userAgent != null) {
                cookies.setUserAgent(userAgent);
            }
        }
        return cookies;
    }

    public void getDownload(final File file, final String urlString) throws IOException {
        URLConnectionAdapter con = null;
        try {
            con = this.openGetConnection(urlString);
            Browser.download(file, con);
        } finally {
            if (con != null) {
                con.disconnect();
            }
        }
    }

    public Form getForm(final int i) {
        final Form[] forms = this.getForms();
        return forms.length <= i ? null : forms[i];
    }

    /**
     * Returns the first form that has an input filed with name key
     *
     * @param key
     * @return
     */
    public Form getFormbyKey(final String key) {
        for (final Form f : this.getForms()) {
            if (f.hasInputFieldByName(key)) {
                return f;
            }
        }
        return null;
    }

    /**
     * Returns the first form that has input field with 'key' that equals 'value'.
     *
     * @since JD2
     * @param key
     * @param value
     * @return
     */
    public Form getFormByInputFieldKeyValue(final String key, final String value) {
        for (final Form f : this.getForms()) {
            for (final InputField field : f.getInputFields()) {
                if (key != null && key.equals(field.getKey())) {
                    if (value == null && field.getValue() == null) {
                        return f;
                    } else if (value != null && value.equals(field.getValue())) {
                        return f;
                    }
                }
            }
        }
        return null;
    }

    /**
     * Returns the first form that has input field with a property with 'key' that equals 'value'.
     *
     * @since JD2
     * @param key
     * @param value
     * @return
     */
    public Form getFormByInputFieldPropertyKeyValue(final String key, final String value) {
        for (final Form f : this.getForms()) {
            for (final InputField field : f.getInputFields()) {
                if (field.containsPropertyKeyValue(key, value)) {
                    return f;
                }
            }
        }
        return null;
    }

    public Form getFormbyProperty(final String property, final String name) {
        for (final Form form : this.getForms()) {
            if (form.getStringProperty(property) != null && form.getStringProperty(property).equalsIgnoreCase(name)) {
                return form;
            }
        }
        return null;
    }

    /**
     * returns first found form with given Action.
     *
     * @author raztoki
     * @since JD2
     * @param action
     * @return
     */
    public final Form getFormbyAction(final String action) {
        for (final Form form : this.getForms()) {
            if (action.equalsIgnoreCase(form.getAction())) {
                return form;
            }
        }
        return null;
    }

    /**
     * returns first found form with given Action. Searches performed by Regex,
     *
     * @author raztoki
     * @since JD2
     * @param regex
     * @return
     */
    public final Form getFormbyActionRegex(final String regex) {
        if (regex == null) {
            return null;
        }
        for (final Form form : this.getForms()) {
            if (form.getAction() != null && new Regex(form.getAction(), regex).patternFind()) {
                return form;
            }
        }
        return null;
    }

    /**
     * Returns the first form with an Submitvalue of name<br />
     * Note: String needs to be urlEncoded as values it's comparing against are!
     *
     *
     * @param name
     * @return
     */
    public Form getFormBySubmitvalue(final String name) {
        for (final Form form : this.getForms()) {
            try {
                form.setPreferredSubmit(name);
                return form;
            } catch (final IllegalArgumentException e) {
            }
        }
        return null;
    }

    public Form[] getForms() {
        return Form.getForms(this);
    }

    /**
     *
     * same as getFormbyAction
     *
     * @author raztoki
     * @since JD2
     * @param action
     * @return
     */
    public Form[] getFormsByAction(final String action) {
        final ArrayList<Form> results = new ArrayList<Form>();
        for (final Form form : this.getForms()) {
            if (StringUtils.equalsIgnoreCase(form.getAction(), action)) {
                results.add(form);
            }
        }
        return results.toArray(new Form[results.size()]);
    }

    /**
     * same as getFormbyActionRegex
     *
     * @author raztoki
     * @since JD2
     * @param action
     * @return
     */
    public Form[] getFormsByActionRegex(final String action) {
        if (action == null) {
            return null;
        }
        final ArrayList<Form> results = new ArrayList<Form>();
        for (final Form form : this.getForms()) {
            if (form.getAction() != null && new Regex(form.getAction(), action).matches()) {
                results.add(form);
            }
        }
        return results.toArray(new Form[results.size()]);
    }

    /**
     * Returns first Form which contains html that matches specified RegEx.
     *
     * @author raztoki
     * @param regex
     * @return
     **/
    public final Form getFormByRegex(final String regex) {
        final Form[] results = this.getFormsByRegex(regex);
        if (results != null && results.length > 0) {
            return results[0];
        } else {
            return null;
        }
    }

    /**
     * finds All forms that matches regex
     *
     * @author raztoki
     * @param regex
     * @return
     */
    public final Form[] getFormsByRegex(final String regex) {
        if (regex == null) {
            return null;
        }
        final ArrayList<Form> results = new ArrayList<Form>();
        final Form[] forms = this.getForms();
        for (final Form form : forms) {
            if (form.containsHTML(regex)) {
                results.add(form);
            }
        }
        return results.toArray(new Form[results.size()]);
    }

    public RequestHeader getHeaders() {
        RequestHeader lHeaders = this.headers;
        if (lHeaders == null) {
            lHeaders = new RequestHeader();
            this.headers = lHeaders;
        }
        return lHeaders;
    }

    public String getHost() {
        final Request lRequest = this.getRequest();
        return lRequest == null ? null : Browser.getHost(lRequest.getURL(), false);
    }

    public String getHost(final boolean subdomain) {
        final Request lRequest = this.getRequest();
        return lRequest == null ? null : Browser.getHost(lRequest.getURL(), subdomain);
    }

    public URLConnectionAdapter getHttpConnection() {
        final Request lRequest = this.getRequest();
        if (lRequest == null) {
            return null;
        } else {
            return lRequest.getHttpConnection();
        }
    }

    /**
     * Gets Browser upper page load limit Byte value.
     *
     * @since JD2
     * @param i
     */
    public int getLoadLimit() {
        final int ret = this.limit;
        if (ret == -1) {
            return this.getDefaultLoadLimit();
        } else {
            return ret;
        }
    }

    public LogInterface getLogger() {
        final LogInterface llogger = this.logger;
        if (llogger != null) {
            return llogger;
        }
        return Browser.LOGGER;
    }

    public String getMatch(final String string) {
        return this.getRegex(string).getMatch(0);
    }

    public String getPage(final String string) throws IOException {
        return this.getPage(this.createGetRequest(string));
    }

    public String getPage(final Request request) throws IOException {
        final Request con = this.loadConnection(this.openRequestConnection(request));
        return con.getHTMLSource();
    }

    public String getPage(final URL url) throws IOException {
        return this.getPage(url.toString());
    }

    public ProxySelectorInterface getProxy() {
        return this.proxy;
    }

    /**
     * returns current ReadTimeout
     *
     * @return
     */
    public int getReadTimeout() {
        return this.readTimeout < 0 ? Browser.TIMEOUT_READ : this.readTimeout;
    }

    /**
     * If automatic redirectfollowing is disabled, you can get the redirect URL if there is any.
     *
     * @return
     */
    public String getRedirectLocation() {
        final Request lRequest = this.getRequest();
        if (lRequest == null) {
            return null;
        }
        return lRequest.getLocation();
    }

    public Regex getRegex(final Pattern compile) {
        return new Regex(this, compile);
    }

    public Regex getRegex(final String string) {
        return new Regex(this, string);
    }

    /**
     * Gets the latest request
     *
     * @return
     */
    public Request getRequest() {
        return this.request;
    }

    public ProxySelectorInterface getThreadProxy() {
        final Thread currentThread = Thread.currentThread();
        /**
         * return BrowserSettings from current thread if available
         */
        if (currentThread != null && currentThread instanceof BrowserSettings) {
            final BrowserSettings settings = (BrowserSettings) currentThread;
            return settings.getProxySelector();
        }
        return null;
    }

    public String getURL() {
        return this.getURL(false);
    }

    public String getURL(final boolean withUserInfo) {
        final Request lRequest = this.getRequest();
        return lRequest == null ? null : lRequest.getUrl(withUserInfo);
    }

    public URL _getURL() {
        return this._getURL(false);
    }

    public URL _getURL(final boolean withUserInfo) {
        final Request lRequest = this.getRequest();
        return lRequest == null ? null : lRequest.getURL(withUserInfo);
    }

    private final List<Object[]> LOCATION_MAP = new ArrayList<Object[]>();

    /**
     * Tries to get a full URL out of (relative URL-)string.
     *
     * @throws BrowserException
     */
    public URL getURL(String location) throws IOException {
        if (location == null) {
            location = this.getRedirectLocation();
        }
        if (location == null) {
            throw new IllegalArgumentException("location is null");
        }
        try {
            if (location.matches("^(https?|ftp)://.+")) {
                return URLHelper.fixPathTraversal(URLHelper.createURL(location.replaceAll(" ", "%20")));
            } else {
                final Request lRequest = this.getRequest();
                if (lRequest == null) {
                    throw new IOException("No request available:" + location);
                } else {
                    final URL url = URLHelper.createURL(URLHelper.parseLocation(lRequest.getURL(), location));
                    synchronized (this.LOCATION_MAP) {
                        // clean
                        this.getSourceLocationForURL(null);
                        this.LOCATION_MAP.add(new Object[] { new WeakReference<URL>(url), location });
                    }
                    return url;
                }
            }
        } catch (final MalformedURLException e) {
            final Request lRequest = this.getRequest();
            if (lRequest == null) {
                throw new IOException("No request available:" + location, e);
            } else {
                final URL url = URLHelper.createURL(URLHelper.parseLocation(lRequest.getURL(), location));
                synchronized (this.LOCATION_MAP) {
                    // clean
                    this.getSourceLocationForURL(null);
                    this.LOCATION_MAP.add(new Object[] { new WeakReference<URL>(url), location });
                }
                return url;
            }
        }
    }

    public String getSourceLocationForURL(final URL url) {
        String source = null;
        synchronized (this.LOCATION_MAP) {
            final Iterator<Object[]> it = this.LOCATION_MAP.iterator();
            while (it.hasNext()) {
                final Object[] next = it.next();
                final URL dest = ((Reference<URL>) next[0]).get();
                if (dest == null) {
                    it.remove();
                } else if (dest == url) {
                    source = (String) next[1];
                }
            }
        }
        return source;
    }

    public boolean isCookiesExclusive() {
        return this.cookiesExclusive;
    }

    public boolean isDebug() {
        return this.debug || this.isVerbose();
    }

    public boolean isFollowingRedirects() {
        return this.doRedirects;
    }

    public boolean isKeepResponseContentBytes() {
        return this.keepResponseContentBytes;
    }

    public boolean isVerbose() {
        return Browser.VERBOSE || this.verbose;
    }

    /**
     * Reads the content behind a con and returns them. Note: if con==null, the current request is read. This is useful for redirects. Note
     * #2: if a connection is loaded, data is not stored in the browser instance.
     *
     * @param con
     * @return
     * @throws IOException
     */
    public Request loadConnection(final URLConnectionAdapter connection) throws IOException {
        final Request requ;
        if (connection == null) {
            requ = this.getRequest();
        } else if (connection.getRequest() != null) {
            requ = connection.getRequest();
        } else {
            requ = new Request(connection) {
                @Override
                public long postRequest() throws IOException {
                    return 0;
                }

                @Override
                public void preRequest() throws IOException {
                }
            };
        }
        try {
            if (requ == null) {
                throw new IOException("No request available");
            } else {
                this.checkContentLengthLimit(requ);
                this.prepareBlockDetectionBeforeLoadConnection(requ);
                requ.read(this.isKeepResponseContentBytes());
                if (this.isVerbose() && requ != null) {
                    final LogInterface llogger = this.getLogger();
                    if (llogger != null) {
                        llogger.finest("\r\nBrowserID:" + requ.getBrowserID() + "|RequestID:" + requ.getRequestID() + "|URL:" + requ.getURL() + "\r\n----------------Request Content-------------\r\n" + requ.getHTMLSource() + "\r\n");
                    }
                }
                this.checkForBlockedByAfterLoadConnection(requ);
                return requ;
            }
        } catch (final BrowserException e) {
            throw e;
        } catch (final IOException e) {
            throw new BrowserException(null, requ, e);
        } finally {
            if (requ != null) {
                requ.disconnect();
            }
        }
    }

    protected void mergeHeaders(final Request request) {
        final RequestHeader lHeaders = this.headers;
        if (request != null && lHeaders != null) {
            final RequestHeader requestHeaders = request.getHeaders();
            if (lHeaders.isDominant()) {
                requestHeaders.clear();
            }
            requestHeaders.putAll(lHeaders);
        }
    }

    /**
     * https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Sec-Fetch-Site </br>
     * auto completes Sec-Fetch-Site, some websites(eg facebook) check it
     */
    protected void autoCompleteHeaders(final Request request) {
        if (request != null) {
            final RequestHeader requestHeaders = request.getHeaders();
            if (requestHeaders.getValue("Sec-Fetch-Site") == null) {
                boolean addSecFetchSite = false;
                final String firefoxVersionString = new Regex(requestHeaders.getValue("User-Agent"), "FireFox/(\\d+)").getMatch(0);
                final int firefoxVersion = firefoxVersionString != null ? Integer.parseInt(firefoxVersionString) : -1;
                if (firefoxVersion >= 90) {
                    addSecFetchSite = true;
                }
                final String chromeVersionString = new Regex(requestHeaders.getValue("User-Agent"), "Chrome/(\\d+)").getMatch(0);
                final int chromeVersion = chromeVersionString != null ? Integer.parseInt(chromeVersionString) : -1;
                if (chromeVersion >= 76) {
                    addSecFetchSite = true;
                }
                final String operaVersionString = new Regex(requestHeaders.getValue("User-Agent"), "OPR/(\\d+)").getMatch(0);
                final int operaVersion = operaVersionString != null ? Integer.parseInt(operaVersionString) : -1;
                if (operaVersion >= 63) {
                    addSecFetchSite = true;
                }
                final String edgeVersionString = new Regex(requestHeaders.getValue("User-Agent"), "Edg/(\\d+)").getMatch(0);
                final int edgeVersion = edgeVersionString != null ? Integer.parseInt(edgeVersionString) : -1;
                if (edgeVersion >= 79) {
                    addSecFetchSite = true;
                }
                if (addSecFetchSite) {
                    requestHeaders.put(new HTTPHeader("Sec-Fetch-Site", "same-origin"));
                }
            }
        }
    }

    /**
     * Opens a new connection based on a Form
     *
     * @param form
     * @return
     * @throws Exception
     */
    public URLConnectionAdapter openFormConnection(final Form form) throws Exception {
        return this.openRequestConnection(this.createFormRequest(form));
    }

    /**
     * Opens a new get connection
     *
     * @param string
     * @return
     * @throws IOException
     */
    public URLConnectionAdapter openGetConnection(final String string) throws IOException {
        return this.openRequestConnection(this.createGetRequest(string));
    }

    /**
     * @since JD2
     **/
    public URLConnectionAdapter openHeadConnection(final String string) throws IOException {
        return this.openRequestConnection(this.createHeadRequest(string));
    }

    /**
     * Opens a Post Connection based on a variable HashMap
     *
     * @deprecated Use {@link #openPostConnection(String, UrlQuery)} instead
     */
    @Deprecated
    public URLConnectionAdapter openPostConnection(final String url, final LinkedHashMap<String, String> post) throws IOException {
        return this.openPostConnection(url, UrlQuery.get(post));
    }

    /**
     * OPens a new Post connection based on a query string
     *
     * @deprecated Use {@link #openPostConnection(String, UrlQuery)} instead
     */
    @Deprecated
    public URLConnectionAdapter openPostConnection(final String url, final String post) throws IOException {
        return this.openRequestConnection(this.createPostRequest(url, post));
    }

    public URLConnectionAdapter openPostConnection(String url, UrlQuery query) throws IOException {
        return this.openRequestConnection(this.createPostRequest(url, query));
    }

    protected void setRequestProperties(final Request sourceRequest, final Request nextRequest, final String refererURL) {
        if (nextRequest != null) {
            nextRequest.setSSLSocketStreamOptions(this.getSSLSocketStreamOptions());
            if (nextRequest.isSSLTrustALLSet() == null) {
                nextRequest.setSSLTrustALL(this.getDefaultSSLTrustALL());
            }
            this.forwardCookies(nextRequest);
            if (nextRequest.getCustomCharset() == null) {
                nextRequest.setCustomCharset(this.customCharset);
            }
            if (!nextRequest.getHeaders().contains(HTTPConstants.HEADER_REQUEST_ACCEPT_LANGUAGE)) {
                nextRequest.getHeaders().put(HTTPConstants.HEADER_REQUEST_ACCEPT_LANGUAGE, this.getAcceptLanguage());
            }
            nextRequest.setConnectTimeout(this.getConnectTimeout());
            nextRequest.setReadTimeout(this.getReadTimeout());
            final boolean allowRefererURL;
            if (sourceRequest != null && StringUtils.startsWithCaseInsensitive(sourceRequest.getURL().getProtocol(), "https")) {
                // http://allben.net/post/2009/02/25/Null-Url-Referrer-going-from-HTTPS-to-HTTP
                allowRefererURL = StringUtils.startsWithCaseInsensitive(nextRequest.getURL().getProtocol(), "https");
            } else {
                allowRefererURL = true;
            }
            if (allowRefererURL && refererURL != null && !nextRequest.getHeaders().contains(HTTPConstants.HEADER_REQUEST_REFERER)) {
                nextRequest.getHeaders().put(HTTPConstants.HEADER_REQUEST_REFERER, refererURL);
            }
            this.mergeHeaders(nextRequest);
            this.autoCompleteHeaders(nextRequest);
        }
    }

    public URLConnectionAdapter openRequestConnection(final Request request) throws IOException {
        return this.openRequestConnection(request, this.isFollowingRedirects());
    }

    private final static AtomicLong          BROWSERIDS                   = new AtomicLong(0);
    private final AtomicLong                 requestID                    = new AtomicLong(0);
    protected static AuthenticationFactory   DEFAULTAUTHENTICATIONFACTORY = new URLUserInfoAuthentication();
    protected volatile AuthenticationFactory authenticationFactory        = null;

    public static AuthenticationFactory getDefaultAuthenticationFactory() {
        return Browser.DEFAULTAUTHENTICATIONFACTORY;
    }

    public static void setDefaultAuthenticationFactory(AuthenticationFactory authenticationFactory) {
        Browser.DEFAULTAUTHENTICATIONFACTORY = authenticationFactory;
    }

    public AuthenticationFactory getCustomAuthenticationFactory() {
        return this.authenticationFactory;
    }

    public AuthenticationFactory getAuthenticationFactory() {
        final AuthenticationFactory ret = this.getCustomAuthenticationFactory();
        if (ret != null) {
            return ret;
        } else {
            return Browser.getDefaultAuthenticationFactory();
        }
    }

    public void setCustomAuthenticationFactory(AuthenticationFactory authenticationFactory) {
        this.authenticationFactory = authenticationFactory;
    }

    public void clearAuthentications() {
        this.authentications.clear();
    }

    private long browserID       = -1;
    private long browserParentID = -1;

    public synchronized long getBrowserID() {
        if (this.browserID == -1) {
            this.browserID = Browser.BROWSERIDS.incrementAndGet();
        }
        return this.browserID;
    }

    public long getBrowserParentID() {
        return this.browserParentID;
    }

    protected long getNextRequestID() {
        return this.requestID.incrementAndGet();
    }

    protected CopyOnWriteArrayList<Authentication> authentications = new CopyOnWriteArrayList<Authentication>();

    public boolean addAuthentication(Authentication authentication) {
        if (authentication != null) {
            return this.authentications.addIfAbsent(authentication);
        } else {
            return false;
        }
    }

    public boolean removeAuthentication(Authentication authentication) {
        return authentication != null && this.authentications.remove(authentication);
    }

    public List<Authentication> getAuthentications() {
        return this.authentications;
    }

    protected Authentication applyAuthentication(Request request) throws IOException {
        final Authentication authentication = request.getAuthentication();
        if (authentication == null) {
            final AuthenticationFactory authenticationFactory = this.getAuthenticationFactory();
            if (authenticationFactory != null) {
                final Authentication authenticationByFactory = authenticationFactory.authorize(this, request);
                if (authenticationByFactory != null) {
                    request.setAuthentication(authenticationByFactory);
                    return authenticationByFactory;
                }
            }
            for (final Authentication browserAuthentication : this.getAuthentications()) {
                if (browserAuthentication.authorize(this, request)) {
                    request.setAuthentication(browserAuthentication);
                    return browserAuthentication;
                }
            }
        } else {
            if (authentication.authorize(this, request)) {
                return authentication;
            }
        }
        return null;
    }

    protected boolean retryAuthentication(Request request) throws IOException {
        final Authentication requestAuthentication = request.getAuthentication();
        final AuthenticationFactory authenticationFactory = this.getAuthenticationFactory();
        if (requestAuthentication != null) {
            if (authenticationFactory != null) {
                return authenticationFactory.retry(requestAuthentication, this, request);
            } else {
                return requestAuthentication.retry(this, request);
            }
        } else if (requestAuthentication == null && authenticationFactory != null) {
            final Authentication authentication = authenticationFactory.buildAuthentication(this, request);
            if (authentication != null) {
                request.setAuthentication(authentication);
            }
            return authentication != null;
        }
        return false;
    }

    public URLConnectionAdapter openRequestConnection(Request request, final boolean followRedirects) throws IOException {
        if (request == null) {
            throw new IllegalStateException("Request is null");
        }
        int redirectLoopPrevention = 0;
        final Request originalRequest = request;
        final String refererURL = this.getRefererURL(originalRequest);
        while (true) {
            this.setRequestProperties(originalRequest, request, refererURL);
            int connectRetryCounter = 0;
            connectLoop: while (true) {
                try {
                    // connect may throw ProxyAuthException for https or direct connection method requests
                    try {
                        Browser.waitForPageAccess(this, request);
                    } catch (final InterruptedException e) {
                        throw new BrowserException("requestIntervalTime Exception", request, e);
                    }
                    final URLConnectionAdapter connection;
                    try {
                        if (request.getProxy() == null) {
                            final List<HTTPProxy> proxies = this.selectProxies(request.getURL());
                            // choose first one
                            request.setProxy(proxies.get(0));
                        }
                        this.applyAuthentication(request);
                        this.onBeforeRequestConnect(request);
                        connection = request.connect(this).getHttpConnection();
                        this.onAfterRequestConnect(request);
                        this.checkForBlockedByBeforeLoadConnection(request);
                    } finally {
                        this.updateCookies(request);
                        if (this.isDebug()) {
                            final LogInterface llogger = this.getLogger();
                            if (llogger != null) {
                                try {
                                    llogger.finest("\r\n" + request.printHeaders());
                                } catch (final Throwable e) {
                                    llogger.log(e);
                                }
                            }
                        }
                    }
                    if (connection != null) {
                        if (this.retryAuthentication(request)) {
                            this.loadConnection(connection);
                            request.resetConnection();
                            continue connectLoop;
                        }
                        connection.setAllowedResponseCodes(this.getAllowedResponseCodes());
                        if (connection.getResponseCode() == 407) {
                            throw new ProxyAuthException(request.getProxy());
                        }
                    } else {
                        throw new BrowserException("connection is null", request, null);
                    }
                    break;
                } catch (BrowserException e) {
                    request.disconnect();
                    throw e;
                } catch (IOException e) {
                    request.disconnect();
                    final LogInterface llogger = this.getLogger();
                    if (llogger != null) {
                        llogger.log(e);
                    }
                    connectRetryCounter++;
                    if (this.reportConnectException(connectRetryCounter, e, request) || e instanceof ProxyAuthException && this.updateProxy(connectRetryCounter, request)) {
                        if (llogger != null) {
                            llogger.info("Retry openRequestConnection(" + request.getUrl() + "):" + connectRetryCounter);
                        }
                        // reset proxy
                        request.setProxy(null);
                        continue connectLoop;
                    } else {
                        throw new BrowserException(null, request, e);
                    }
                }
            }
            final String redirect = request.getLocation();
            if (followRedirects && redirect != null) {
                if (redirectLoopPrevention++ > 20) {
                    request.disconnect();
                    throw new BrowserException("Too many redirects!", originalRequest, null);
                }
                /**
                 * needs loadConnection for keep-Alive
                 */
                this.loadConnection(request.getHttpConnection());
                final Request redirectRequest = this.createRedirectFollowingRequest(request);
                this.setRequest(request);
                if (redirectRequest == null) {
                    return request.getHttpConnection();
                } else {
                    request = redirectRequest;
                }
            } else {
                this.setRequest(request);
                return request.getHttpConnection();
            }
        }
    }

    protected void onAfterRequestConnect(Request request) throws IOException {
    }

    protected void onBeforeRequestConnect(Request request) throws IOException {
    }

    private boolean reportConnectException(final int proxyRetryCounter, final IOException e, final Request request) {
        final ProxySelectorInterface selector;
        if (this.proxy != null) {
            selector = this.proxy;
        } else {
            selector = Browser.GLOBAL_PROXY;
        }
        return selector != null && selector.reportConnectException(request, proxyRetryCounter, e);
    }

    /**
     * loads a new page (post)
     *
     * @deprecated Use {@link #postPage(String, UrlQuery)} instead
     */
    @Deprecated
    public String postPage(final String url, final LinkedHashMap<String, String> post) throws IOException {
        return this.postPage(url, UrlQuery.get(post));
    }

    /**
     * loads a new page (POST)
     *
     * @deprecated Use {@link #postPage(String, UrlQuery)} or {@link #postPageRaw(String, String) instead
     */
    @Deprecated
    public String postPage(final String url, final String post) throws IOException {
        final PostRequest postRequest = this.createPostRequest(url, post);
        return this.getPage(postRequest);
    }

    public String postPage(String url, UrlQuery queryInfo) throws IOException {
        return this.getPage(this.createPostRequest(url, queryInfo));
    }

    public String postPageRaw(final String url, final byte[] post) throws IOException {
        final PostRequest postRequest;
        if (this.probeJSonContent(post)) {
            postRequest = this.createJSonPostRequest(url, new String(post, "UTF-8"));
        } else {
            postRequest = this.createPostRequest(url, new ArrayList<KeyValueStringEntry>(), null);
            postRequest.setPostBytes(post);
        }
        return this.getPage(postRequest);
    }

    /**
     * loads a new page (post) the postdata is given by the poststring. It will be sent as is
     */
    public String postPageRaw(final String url, final String post) throws IOException {
        final PostRequest postRequest;
        if (this.probeJSonContent(post)) {
            postRequest = this.createJSonPostRequest(url, post);
        } else {
            postRequest = this.createPostRequest(url, new UrlQuery(), null);
            postRequest.setPostDataString(post);
        }
        return this.getPage(postRequest);
    }

    public List<HTTPProxy> selectProxies(final URL url) throws IOException {
        final ProxySelectorInterface selector;
        if (this.proxy != null) {
            selector = this.proxy;
        } else {
            selector = Browser.GLOBAL_PROXY;
        }
        if (selector == null) {
            final ArrayList<HTTPProxy> ret = new ArrayList<HTTPProxy>();
            ret.add(HTTPProxy.NONE);
            return ret;
        }
        final List<HTTPProxy> list;
        try {
            list = selector.getProxiesByURL(url);
        } catch (Throwable e) {
            throw new NoGateWayException(selector, "ProxySelector failed: " + url + "|Selector:" + selector, e);
        }
        if (list == null || list.size() == 0) {
            throw new NoGateWayException(selector, "No Gateway or Proxy Found: " + url + "|Selector:" + selector);
        }
        return list;
    }

    public void setAcceptLanguage(final String acceptLanguage) {
        this.acceptLanguage = acceptLanguage;
    }

    /**
     * @param allowedResponseCodes
     *            the allowedResponseCodes to set
     * @since JD2
     */
    public void setAllowedResponseCodes(final int... allowedResponseCodes) {
        this.allowedResponseCodes = allowedResponseCodes;
    }

    public boolean hasAllowedResponseCode(final int input) {
        final int[] original = this.getAllowedResponseCodes();
        for (final int a : original) {
            if (a == input) {
                return true;
            }
        }
        return false;
    }

    /**
     * Adds input to existing response codes. This solves the issue were setAllowedResponseCodes(int...) destroys old with new.
     *
     * @param input
     * @author raztoki
     * @since JD2
     */
    public void addAllowedResponseCodes(final int... allowResponseCodes) {
        final int[] outcome = Browser.buildAllowedResponseCodes(this.getAllowedResponseCodes(), allowResponseCodes);
        this.setAllowedResponseCodes(outcome);
    }

    protected static int[] buildAllowedResponseCodes(final int[] allowedResponseCodes, final int... allowResponseCodes) {
        final HashSet<Integer> dupe = new HashSet<Integer>();
        if (allowedResponseCodes != null) {
            for (final int a : allowedResponseCodes) {
                dupe.add(a);
            }
        }
        for (final int a : allowResponseCodes) {
            dupe.add(a);
        }
        final int[] outcome = new int[dupe.size()];
        int index = 0;
        for (final Integer i : dupe) {
            outcome[index++] = i;
        }
        return outcome;
    }

    public void setConnectTimeout(final int connectTimeout) {
        this.connectTimeout = connectTimeout;
    }

    public void setCookie(final String url, final String key, final String value) {
        final Cookies cookies = this.getCookies(url);
        cookies.add(new Cookie(Browser.getHost(url), key, value));
    }

    public void setCookie(final Cookie cookie) {
        final Cookies cookies = this.getCookies(cookie.getHost());
        cookies.add(cookie);
    }

    /**
     * Adds given Cookies to current Cookies session for given host. Will also set User-Agent if given.
     *
     * @author raztoki
     * @since JD2
     * @param url
     * @param iCookies
     * @param replace
     */
    public void setCookies(final String url, final Cookies iCookies) {
        this.setCookies(url, iCookies, false);
        final String userAgent = iCookies.getUserAgent();
        if (!StringUtils.isEmpty(userAgent)) {
            this.getHeaders().put("User-Agent", userAgent);
        }
    }

    /**
     * Adds given Cookies to current Cookies session for each cookies' respective host. Will also set User-Agent if given.
     *
     * @author pspzockerscene
     * @param iCookies
     */
    public void setCookies(final Cookies iCookies) {
        this.setCookies(null, iCookies, false);
        final String userAgent = iCookies.getUserAgent();
        if (!StringUtils.isEmpty(userAgent)) {
            this.getHeaders().put("User-Agent", userAgent);
        }
    }

    /**
     * Adds given Cookies to current Cookies session for either a given host or cookies' respective host. replace when true will dump _all_
     * Cookies
     *
     * @author raztoki
     * @since JD2
     * @param url
     * @param iCookies
     * @param replace
     */
    public void setCookies(final String url, final Cookies iCookies, final boolean replace) {
        if (url == null) {
            /* Set all cookies on their (hopefully given) host. */
            if (replace) {
                this.clearCookies(null);
            }
            final List<Cookie> cookieArray = iCookies.getCookies();
            for (final Cookie cookie : cookieArray) {
                this.setCookie(cookie);
            }
        } else {
            /* Get cookies for pre-defined host and set them */
            final Cookies cookies = this.getCookies(url);
            if (replace) {
                cookies.clear();
                cookies.add(iCookies);
            } else {
                cookies.add(iCookies);
            }
        }
    }

    public boolean setCookiesExclusive(final boolean b) {
        if (this.cookiesExclusive == b) {
            return false;
        }
        if (b) {
            this.cookies.clear();
            for (final Entry<String, Cookies> next : Browser.COOKIES.entrySet()) {
                Cookies tmp;
                this.cookies.put(next.getKey(), tmp = new Cookies());
                tmp.add(next.getValue());
            }
        } else {
            this.cookies.clear();
        }
        // this needs to be last so you can drop to frame, in test situations. Otherwise this reference is destroyed.
        this.cookiesExclusive = b;
        return true;
    }

    /**
     *
     * sets CurrentURL (used for referer)
     *
     * null -> null
     *
     * empty -> do not set referer for next request
     *
     * other -> use given referer for next request
     *
     * @param string
     * @since JD2
     */
    public void setCurrentURL(final String url) throws MalformedURLException {
        if (url == null) {
            this.currentURL = null;
        } else if (StringUtils.isEmpty(url)) {
            this.currentURL = "";
        } else {
            this.currentURL = url;
        }
    }

    /**
     * returns referer for next request
     *
     * 1.) getAndClear referer from browser.getHeaders
     *
     * 2.) if 1==null, get currentURL (String)
     *
     * 3.) if 2==null, get url from getURL(returns url from last request)
     *
     * @return
     */
    private String getRefererURL(Request request) {
        final HTTPHeader referer = this.getHeaders().remove(HTTPConstants.HEADER_REQUEST_REFERER);
        final String refererURLHeader = referer != null ? referer.getValue() : null;
        if (refererURLHeader == null) {
            final Object lCurrentURL = this.currentURL;
            if (lCurrentURL != null && lCurrentURL instanceof String) {
                return (String) lCurrentURL;
            } else if (lCurrentURL != null && lCurrentURL instanceof Request) {
                return ((Request) lCurrentURL).getUrl();
            } else {
                return this.getURL();
            }
        } else {
            return refererURLHeader;
        }
    }

    public void setCustomCharset(final String charset) {
        this.customCharset = charset;
    }

    public void setDebug(final boolean debug) {
        this.debug = debug;
    }

    public void setFollowRedirects(final boolean b) {
        this.doRedirects = b;
    }

    /**
     * do not below revision 10000
     *
     * @since JD2
     */
    public void setHeader(final String field, final String value) {
        this.getHeaders().put(field, value);
    }

    public void setHeaders(final RequestHeader h) {
        this.headers = h;
    }

    public void setKeepResponseContentBytes(final boolean keepResponseContentBytes) {
        this.keepResponseContentBytes = keepResponseContentBytes;
    }

    /**
     * Sets Browser upper page load limit Byte value. </br>
     * Use Integer.MAX_VALUE for "unlimited" (do not use "-1"!).
     *
     * @since JD2
     * @param i
     */
    public void setLoadLimit(final int i) {
        this.limit = Math.max(-1, i);
    }

    public void setLogger(final LogInterface logger) {
        this.logger = logger;
    }

    @Deprecated
    /**
     * @deprecated
     * @param proxy2
     */
    public void setProxy(final HTTPProxy proxy2) {
        this.setProxySelector(new StaticProxySelector(proxy2));
    }

    @Deprecated
    /**
     * for usage in plugins for stable compatibility only
     *
     * @param threadProxy
     */
    public void setProxy(final ProxySelectorInterface threadProxy) {
        this.setProxySelector(threadProxy);
    }

    public void setProxySelector(ProxySelectorInterface proxy) {
        final ProxySelectorInterface wished = proxy;
        if (proxy == null) {
            proxy = this.getThreadProxy();
        }
        if (proxy == this.proxy) {
            return;
        }
        this.proxy = proxy;
        if (this.isDebug()) {
            final LogInterface llogger = this.getLogger();
            if (llogger != null) {
                llogger.info("Use local proxy: " + proxy + " wished: " + wished);
            }
        }
    }

    public void setReadTimeout(final int readTimeout) {
        this.readTimeout = readTimeout;
    }

    public void setRequest(final Request request) {
        if (request == null) {
            this.currentURL = null;
        } else {
            this.currentURL = request.getUrl();
        }
        this.updateCookies(request);
        this.request = request;
    }

    public void setVerbose(final boolean b) {
        this.verbose = b;
    }

    public String submitForm(final Form form) throws IOException {
        return this.getPage(this.createFormRequest(form));
    }

    @Override
    public String toString() {
        final Request lRequest = this.getRequest();
        if (lRequest == null) {
            return "Browser. No request yet";
        }
        return lRequest.getHTMLSource();
    }

    public void updateCookies(final Request request) {
        if (request != null && request.hasCookies()) {
            final Cookies cookies = this.getCookies(Browser.getHost(request.getURL()));
            cookies.add(request.getCookies());
        }
    }

    /**
     * can update the connection information - for example ask for proxy auth.
     *
     * @param request
     * @param proxyRetryCounter
     *
     * @return true if a failed request should be done again.
     */
    protected boolean updateProxy(final int proxyRetryCounter, final Request request) {
        final HTTPProxy proxy = request != null ? request.getProxy() : null;
        if (proxy != null) {
            switch (proxy.getType()) {
            case SOCKS5:
                if (proxyRetryCounter <= 2) {
                    try {
                        Thread.sleep(5000 * Math.max(1, proxyRetryCounter));
                    } catch (final InterruptedException e) {
                        return false;
                    }
                }
                return true;
            default:
                break;
            }
        }
        final ProxySelectorInterface selector;
        if (this.proxy != null) {
            selector = this.proxy;
        } else {
            selector = Browser.GLOBAL_PROXY;
        }
        return selector != null && selector.updateProxy(request, proxyRetryCounter);
    }

    /** Checks for header based blocks by Firewalls and similar. */
    public void checkForBlockedByBeforeLoadConnection(final Request request) throws IOException {
        if (this.getThrowExceptionOnBlockedBy(request)) {
            final BlockedTypeInterface blockedType = this.getBlockedType(request);
            if (blockedType != null) {
                throw new BlockedByException(request, blockedType);
            }
        }
    }

    protected void prepareBlockDetectionBeforeLoadConnection(final Request request) throws IOException {
        if (this.getThrowExceptionOnBlockedBy(request)) {
            final List<BlockedTypeInterface> blockedTypeInterfaces = request != null ? this.getBlockedTypeInterfaces() : null;
            if (blockedTypeInterfaces != null) {
                for (final BlockedTypeInterface blockedTypeInterface : blockedTypeInterfaces) {
                    if (Boolean.TRUE.equals(blockedTypeInterface.prepareBlockDetection(this, request))) {
                        break;
                    }
                }
            }
        }
    }

    /**
     * Checks for block by firewalls and similar. </br>
     * To be called after a sent request.
     */
    public void checkForBlockedByAfterLoadConnection(Request request) throws IOException {
        if (this.getThrowExceptionOnBlockedBy(request)) {
            final BlockedTypeInterface blockedType = this.getBlockedType(request);
            if (blockedType != null) {
                throw new BlockedByException(request, blockedType);
            }
        }
    }

    /**
     * https://developers.cloudflare.com/support/troubleshooting/cloudflare-errors/troubleshooting-cloudflare-10xxx-errors/
     *
     *
     * https://developers.cloudflare.com/support/troubleshooting/cloudflare-errors/troubleshooting-cloudflare-1xxx-errors/
     *
     * https://developers.cloudflare.com/support/troubleshooting/cloudflare-errors/troubleshooting-cloudflare-5xx-errors/
     *
     */
    protected static CloudflareBlockedType getCloudflareBlock(Browser browser, Request request) {
        final HTTPConnection con;
        if (request == null || !request.isLoaded() || (con = request.getHttpConnection()) == null) {
            return null;
        }
        // final boolean isCloudflareHeaderCfRayExistent = req.getResponseHeader("cf-ray") != null;
        final boolean isCloudflareServer = StringUtils.containsIgnoreCase(request.getResponseHeader(HTTPConstants.HEADER_RESPONSE_SERVER), "cloudflare");
        final boolean isTypicalCloudflareResponseCode = con.getResponseCode() == 403 || con.getResponseCode() == 502 || con.getResponseCode() == 503 || con.getResponseCode() == 429 || con.getResponseCode() == 522 || con.getResponseCode() == 523;
        /**
         * TODO: 2023-12-21: Maybe remove reliance on http status-code as it looks like literally any status code can be returned when a
         * Cloudflare block happens. </br>
         * I've just added code 502 to the list of "Cloudflare response-codes".
         */
        /*
         * It is really important to also check for Cloudflare html else stuff will fail/break e.g. icerbox.com wrong login -> Cloudflare
         * check without html --> Fails!
         */
        if (isCloudflareServer && isTypicalCloudflareResponseCode) {
            final boolean isCloudflareChallengeHTML = request.containsHTML("(?i)<title>\\s*Attention Required!(\\s*\\| Cloudflare)?\\s*</title>") || request.containsHTML("data-translate=\"challenge_headline\"") || request.containsHTML("<title>\\s*Just a moment\\.*\\s*</title>") && request.containsHTML("<form id\\s*=\\s*\"challenge-form\"");
            final boolean isCloudflareOtherHTML = request.containsHTML("(?i)<title>\\s*Access denied\\s*\\| [^<]* used Cloudflare to restrict access\\s*</title>") || request.containsHTML("class\\s*=\\s*\"(ray-id|cf-error-title)\"") || request.containsHTML("(class|id)\\s*=\\s*\"cf-error-details\"") || request.containsHTML("window\\._cf_chl_opt");
            if (isCloudflareChallengeHTML || isCloudflareOtherHTML) {
                String errorCode = request.getRegex("<span class\\s*=\\s*\"(?:cf-)?code-label\"\\s*>\\s*Error code\\s*(?:<span>)?(\\d+)(?:</span>)?\\s*</span>").getMatch(0);
                if (errorCode == null) {
                    errorCode = request.getRegex("cloudflare\\.com[^\"]*\\?utm_source=(\\d+)_error").getMatch(0);
                    if (errorCode == null) {
                        errorCode = request.getRegex("cloudflare\\.com[^\"]*\\?utm_source=errorcode_(\\d+)").getMatch(0);
                        if (errorCode == null) {
                            errorCode = request.getRegex("(?i)<p>Error code\\s*:\\s*(\\d+)</p>").getMatch(0);
                        }
                    }
                }
                /* 2023-06-06: This is only a text output. Do not use the errormessage/text for anything else at this moment!! */
                final String errorText = request.getRegex("<h1[^>]*>\\s*(.*?)\\s*</h1>").getMatch(0);
                browser.getLogger().info("Cloudflare parsed errormessage: " + errorText);
                if (errorCode != null) {
                    if (errorCode.matches("5\\d{2}")) {
                        /* e.g. 502 */
                        return CloudflareBlockedType.CLOUDFLARE_SITE_OFFLINE;
                    } else if ("1020".equals(errorCode)) {
                        return CloudflareBlockedType.CLOUDFLARE_IP_BLOCK;
                    }
                }
                return CloudflareBlockedType.CLOUDFLARE_SITE;
            }
        }
        return null;
    }

    protected static Boolean prepareCloudflareBlockDetection(Browser browser, Request request) {
        final HTTPConnection con;
        if (request == null || !request.isRequested() || (con = request.getHttpConnection()) == null) {
            return null;
        }
        final boolean isCloudflareServer = StringUtils.containsIgnoreCase(request.getResponseHeader(HTTPConstants.HEADER_RESPONSE_SERVER), "cloudflare");
        if (isCloudflareServer && !con.isOK()) {
            con.setAllowedResponseCodes(Browser.buildAllowedResponseCodes(con.getAllowedResponseCodes(), con.getResponseCode()));
            return Boolean.TRUE;
        }
        return null;
    }

    public static enum CloudflareBlockedType implements LabelInterface, BlockedTypeInterface {
        CLOUDFLARE_SITE_OFFLINE {
            @Override
            public String getLabel() {
                return "Cloudflare Site Offline";
            }

            @Override
            public BlockedTypeInterface isBlocked(Browser browser, Request request) {
                if (Browser.getCloudflareBlock(browser, request) == this) {
                    return this;
                } else {
                    return null;
                }
            }

            @Override
            public BlockLevelType getBlockLevelType() {
                return BlockLevelType.DOWN;
            }

            @Override
            public BlockSourceType getBlockSourceType() {
                return BlockSourceType.SERVICE;
            }

            @Override
            public Boolean prepareBlockDetection(Browser browser, Request request) {
                return Browser.prepareCloudflareBlockDetection(browser, request);
            }
        },
        CLOUDFLARE_GEO_BLOCK {
            @Override
            public String getLabel() {
                return "Cloudflare GEO-block";
            }

            @Override
            public BlockedTypeInterface isBlocked(Browser browser, Request request) {
                if (Browser.getCloudflareBlock(browser, request) == this) {
                    return this;
                } else {
                    return null;
                }
            }

            @Override
            public BlockLevelType getBlockLevelType() {
                return BlockLevelType.GEO;
            }

            @Override
            public BlockSourceType getBlockSourceType() {
                return BlockSourceType.SERVICE;
            }

            @Override
            public Boolean prepareBlockDetection(Browser browser, Request request) {
                return null;
            }
        },
        CLOUDFLARE_IP_BLOCK {
            @Override
            public String getLabel() {
                return "Cloudflare IP-block";
            }

            @Override
            public BlockedTypeInterface isBlocked(Browser browser, Request request) {
                if (Browser.getCloudflareBlock(browser, request) == this) {
                    return this;
                } else {
                    return null;
                }
            }

            @Override
            public BlockLevelType getBlockLevelType() {
                return BlockLevelType.IP;
            }

            @Override
            public BlockSourceType getBlockSourceType() {
                return BlockSourceType.SERVICE;
            }

            @Override
            public Boolean prepareBlockDetection(Browser browser, Request request) {
                return null;
            }
        },
        CLOUDFLARE_SITE {
            @Override
            public String getLabel() {
                return "Cloudflare Site-Protection";
            }

            @Override
            public BlockedTypeInterface isBlocked(Browser browser, Request request) {
                if (Browser.getCloudflareBlock(browser, request) == this) {
                    return this;
                } else {
                    return null;
                }
            }

            @Override
            public BlockLevelType getBlockLevelType() {
                return BlockLevelType.SITE;
            }

            @Override
            public BlockSourceType getBlockSourceType() {
                return BlockSourceType.SERVICE;
            }

            @Override
            public Boolean prepareBlockDetection(Browser browser, Request request) {
                return null;
            }
        }
    }

    public static enum GenericSupportedBlockTypes implements LabelInterface, BlockedTypeInterface {
        CLOUDFLARE {
            @Override
            public String getLabel() {
                return "Cloudflare";
            }

            @Override
            public BlockedTypeInterface isBlocked(Browser browser, Request request) {
                return Browser.getCloudflareBlock(browser, request);
            }

            @Override
            public BlockLevelType getBlockLevelType() {
                return BlockLevelType.SITE;
            }

            @Override
            public BlockSourceType getBlockSourceType() {
                return BlockSourceType.SERVICE;
            }

            @Override
            public Boolean prepareBlockDetection(Browser browser, Request request) {
                return Browser.prepareCloudflareBlockDetection(browser, request);
            }
        },
        DDOS_GUARD {
            @Override
            public String getLabel() {
                return "DDoS-GUARD";
            }

            @Override
            public BlockedTypeInterface isBlocked(Browser browser, Request request) {
                final HTTPConnection con;
                if (request == null || !request.isLoaded() || (con = request.getHttpConnection()) == null) {
                    return null;
                } else {
                    if (con.getResponseCode() == 403 && StringUtils.containsIgnoreCase(request.getResponseHeader(HTTPConstants.HEADER_RESPONSE_SERVER), "ddos-guard")) {
                        if (request.containsHTML("<title>\\s*DDoS-Guard\\s*</title>") || request.containsHTML("link\\s*=\\s*\"https?://ddos-guard\\.net/")) {
                            return this;
                        }
                    }
                }
                return null;
            }

            @Override
            public BlockLevelType getBlockLevelType() {
                return BlockLevelType.SITE;
            }

            @Override
            public BlockSourceType getBlockSourceType() {
                return BlockSourceType.SERVICE;
            }

            @Override
            public Boolean prepareBlockDetection(Browser browser, Request request) {
                return null;
            }
        },
        DNSPROXY_ORG {
            @Override
            public String getLabel() {
                return "DNSProxy.org";
            }

            @Override
            public BlockedTypeInterface isBlocked(Browser browser, Request request) {
                if (request == null || !request.isLoaded() || request.getHttpConnection() == null) {
                    return null;
                } else {
                    if (request.containsHTML("<title>Browser Check - DNSProxy\\.org</title>") && request.containsHTML("DNSProxy\\.org\\. All rights reserved\\.?<")) {
                        return this;
                    }
                }
                return null;
            }

            @Override
            public BlockLevelType getBlockLevelType() {
                return BlockLevelType.SITE;
            }

            @Override
            public BlockSourceType getBlockSourceType() {
                return BlockSourceType.SERVICE;
            }

            @Override
            public Boolean prepareBlockDetection(Browser browser, Request request) {
                return null;
            }
        },
        DNS_BLOCK_CUII {
            @Override
            public String getLabel() {
                return "DNS block by cuii.info";
            }

            @Override
            public BlockedTypeInterface isBlocked(Browser browser, Request request) {
                if (request == null || !request.isRequested()) {
                    return null;
                } else {
                    final String regex = "^(?i).+\\.cuii\\.info";
                    if (request.getURL().getHost().matches(regex)) {
                        /* If we were originally NOT coming from cuii.info, we were blocked. */
                        Request redirectOrigin = request.getRedirectOrigin();
                        while (redirectOrigin != null) {
                            if (!redirectOrigin.getURL().getHost().matches(regex)) {
                                return this;
                            } else {
                                redirectOrigin = redirectOrigin.getRedirectOrigin();
                            }
                        }
                    }
                    return null;
                }
            }

            @Override
            public BlockLevelType getBlockLevelType() {
                return BlockLevelType.DNS;
            }

            @Override
            public BlockSourceType getBlockSourceType() {
                return BlockSourceType.ISP;
            }

            @Override
            public Boolean prepareBlockDetection(Browser browser, Request request) {
                return null;
            }
        },
        SHIELD_SQUARE {
            @Override
            public String getLabel() {
                return "ShieldSquare";
            }

            @Override
            public BlockedTypeInterface isBlocked(Browser browser, Request request) {
                final HTTPConnection con;
                if (request == null || !request.isLoaded() || (con = request.getHttpConnection()) == null) {
                    return null;
                } else if (con.getResponseCode() == 200) {
                    if (request.containsHTML("(?i)<title>\\s*ShieldSquare Captcha\\s*</title>")) {
                        return this;
                    }
                }
                return null;
            }

            @Override
            public BlockLevelType getBlockLevelType() {
                return BlockLevelType.SITE;
            }

            @Override
            public BlockSourceType getBlockSourceType() {
                return BlockSourceType.SERVICE;
            }

            @Override
            public Boolean prepareBlockDetection(Browser browser, Request request) {
                return null;
            }
        },
        CLOUDFILT {
            @Override
            public String getLabel() {
                return "CloudFilt";
            }

            @Override
            public BlockedTypeInterface isBlocked(Browser browser, Request request) {
                final HTTPConnection con;
                if (request == null || !request.isLoaded() || (con = request.getHttpConnection()) == null) {
                    return null;
                } else if (con.getResponseCode() == 200 && browser.getURL().contains("cloudfilt.com/stop-")) {
                    return this;
                }
                return null;
            }

            @Override
            public BlockLevelType getBlockLevelType() {
                return BlockLevelType.SITE;
            }

            @Override
            public BlockSourceType getBlockSourceType() {
                return BlockSourceType.SERVICE;
            }

            @Override
            public Boolean prepareBlockDetection(Browser browser, Request request) {
                return null;
            }
        },
        NETWORK_SECURITY_WIREFILTER {
            @Override
            public String getLabel() {
                // Typically used as government Internet filter
                return "WireFilter";
            }

            @Override
            public BlockedTypeInterface isBlocked(Browser browser, Request request) {
                final HTTPConnection con;
                if (request == null || !request.isRequested() || (con = request.getHttpConnection()) == null) {
                    return null;
                } else if (con.getResponseCode() == 403 && StringUtils.containsIgnoreCase(request.getResponseHeader(HTTPConstants.HEADER_RESPONSE_SERVER), "Protected by WireFilter")) {
                    return this;
                } else {
                    return null;
                }
            }

            @Override
            public BlockLevelType getBlockLevelType() {
                return BlockLevelType.SITE;
            }

            @Override
            public BlockSourceType getBlockSourceType() {
                return BlockSourceType.SOFTWARE;
            }

            @Override
            public Boolean prepareBlockDetection(Browser browser, Request request) {
                return null;
            }
        },
        NETWORK_SECURITY_ZSCALER {
            @Override
            public String getLabel() {
                return "Zscaler";
            }

            @Override
            public BlockedTypeInterface isBlocked(Browser browser, Request request) {
                final HTTPConnection con;
                if (request == null || !request.isRequested() || (con = request.getHttpConnection()) == null) {
                    return null;
                } else if (con.getResponseCode() == 403 && StringUtils.containsIgnoreCase(request.getResponseHeader(HTTPConstants.HEADER_RESPONSE_SERVER), "Zscaler/")) {
                    return this;
                } else {
                    return null;
                }
            }

            @Override
            public BlockLevelType getBlockLevelType() {
                return BlockLevelType.SITE;
            }

            @Override
            public BlockSourceType getBlockSourceType() {
                return BlockSourceType.SOFTWARE;
            }

            @Override
            public Boolean prepareBlockDetection(Browser browser, Request request) {
                return null;
            }
        },
        NETWORK_SECURITY_SUCURI {
            @Override
            public String getLabel() {
                return "Sucuri.net Website Firewall";
            }

            @Override
            public BlockedTypeInterface isBlocked(Browser browser, Request request) {
                final HTTPConnection con;
                if (request == null || !request.isRequested() || (con = request.getHttpConnection()) == null) {
                    return null;
                } else if (con.getResponseCode() == 403 && request.getResponseHeader("X-Sucuri-Block") != null && request.getHtmlCode() != null && request.getHtmlCode().contains("sucuri.net")) {
                    return this;
                } else {
                    return null;
                }
            }

            @Override
            public BlockLevelType getBlockLevelType() {
                return BlockLevelType.SITE;
            }

            @Override
            public BlockSourceType getBlockSourceType() {
                return BlockSourceType.SERVICE;
            }

            @Override
            public Boolean prepareBlockDetection(Browser browser, Request request) {
                return null;
            }
        },
        FIREWALL_BITDEFENDER {
            @Override
            public String getLabel() {
                return "Firewall Bitdefender";
            }

            @Override
            public BlockedTypeInterface isBlocked(Browser browser, Request request) {
                final HTTPConnection con;
                if (request == null || !request.isRequested() || (con = request.getHttpConnection()) == null) {
                    return null;
                } else if (con.getResponseCode() == 403 && StringUtils.containsIgnoreCase(con.getResponseMessage(), "Blocked by Bitdefender")) {
                    return this;
                } else {
                    return null;
                }
            }

            @Override
            public BlockLevelType getBlockLevelType() {
                return BlockLevelType.SITE;
            }

            @Override
            public BlockSourceType getBlockSourceType() {
                return BlockSourceType.SOFTWARE;
            }

            @Override
            public Boolean prepareBlockDetection(Browser browser, Request request) {
                return null;
            }
        },
        FIREWALL_ESET_NOD32 {
            @Override
            public String getLabel() {
                return "Firewall ESET NOD32";
            }

            @Override
            public BlockedTypeInterface isBlocked(Browser browser, Request request) {
                if (request == null || !request.isLoaded() || request.getHttpConnection() == null) {
                    return null;
                }
                if (true) { /*
                             * TODO: Add header based detection too -> At least check "server" header so we do not only rely on html code.
                             */
                    /* See new ESET NOD32 html code 2023: https://board.jdownloader.org/showthread.php?t=91433 */
                    return null;
                } else if (request.containsHTML("<div class\\s*=\\s*\"prodhead\">\\s*<div class\\s*=\\s*\"logoimg\">\\s*<span class\\s*=\\s*\"logotxt\">\\s*ESET NOD32 Antivirus\\s*</span>\\s*</div>\\s*</div>") && request.containsHTML("- ESET NOD32 Antivirus\\s*</title>")) {
                    return this;
                } else {
                    return null;
                }
            }

            @Override
            public BlockLevelType getBlockLevelType() {
                return BlockLevelType.SITE;
            }

            @Override
            public BlockSourceType getBlockSourceType() {
                return BlockSourceType.SOFTWARE;
            }

            @Override
            public Boolean prepareBlockDetection(Browser browser, Request request) {
                return null;
            }
        },
        FIREWALL_ESET_INTERNET_SECURITY {
            @Override
            public String getLabel() {
                return "Firewall ESET Internet Security";
            }

            @Override
            public BlockedTypeInterface isBlocked(Browser browser, Request request) {
                final HTTPConnection con;
                if (request == null || !request.isRequested() || (con = request.getHttpConnection()) == null) {
                    return null;
                } else if (con.getResponseCode() == 403 && StringUtils.containsIgnoreCase(con.getResponseMessage(), "Blocked by ESET Security")) {
                    return this;
                } else {
                    return null;
                }
            }

            @Override
            public BlockLevelType getBlockLevelType() {
                return BlockLevelType.SITE;
            }

            @Override
            public BlockSourceType getBlockSourceType() {
                return BlockSourceType.SOFTWARE;
            }

            @Override
            public Boolean prepareBlockDetection(Browser browser, Request request) {
                return null;
            }
        },
        FIREWALL_WEBGUARD {
            @Override
            public String getLabel() {
                return "Firewall WebGuard";
            }

            @Override
            public BlockedTypeInterface isBlocked(Browser browser, Request request) {
                final HTTPConnection con;
                if (request == null || !request.isRequested() || (con = request.getHttpConnection()) == null) {
                    return null;
                } else if (con.getResponseCode() == 403 && StringUtils.containsIgnoreCase(request.getResponseHeader(HTTPConstants.HEADER_RESPONSE_SERVER), "WebGuard")) {
                    return this;
                } else {
                    return null;
                }
            }

            @Override
            public BlockLevelType getBlockLevelType() {
                return BlockLevelType.SITE;
            }

            @Override
            public BlockSourceType getBlockSourceType() {
                return BlockSourceType.SOFTWARE;
            }

            @Override
            public Boolean prepareBlockDetection(Browser browser, Request request) {
                return null;
            }
        },
        COVENANTEYES {
            @Override
            public String getLabel() {
                return "CovenantEyesVPN";
            }

            @Override
            public BlockedTypeInterface isBlocked(Browser browser, Request request) {
                final HTTPConnection con;
                if (request == null || !request.isLoaded() || (con = request.getHttpConnection()) == null) {
                    return null;
                } else if (con.getResponseCode() == 200 && request.getResponseHeader("x-ce-page") != null) {
                    if (request.containsHTML("(?i)<title>\\s*Domain Blocked:")) {
                        return this;
                    }
                    if (request.containsHTML("(?i)>\\s*Contact your Filter Guardian if you wish")) {
                        return this;
                    }
                }
                return null;
            }

            @Override
            public BlockLevelType getBlockLevelType() {
                return BlockLevelType.SITE;
            }

            @Override
            public BlockSourceType getBlockSourceType() {
                return BlockSourceType.SERVICE;
            }

            @Override
            public Boolean prepareBlockDetection(Browser browser, Request request) {
                return null;
            }
        },
        AMAZON_AWS_WAF_WEB_APPLICATION_FIREWALL {
            @Override
            public String getLabel() {
                return "Amazon AWS Web Application Firewall";
            }

            @Override
            public BlockedTypeInterface isBlocked(Browser browser, Request request) {
                final HTTPConnection con;
                if (request == null || !request.isLoaded() || (con = request.getHttpConnection()) == null) {
                    return null;
                }
                if (con.getResponseCode() == 202 && request.getResponseHeader("x-amzn-waf-action") != null && StringUtils.containsIgnoreCase(request.getResponseHeader("server"), "awselb")) {
                    /**
                     * Typical reaponse-headers: <br>
                     * Server: awselb/2.0 <br>
                     * x-amzn-waf-action: challenge <br>
                     * Access-Control-Allow-Headers: x-amzn-waf-action
                     */
                    return this;
                }
                return null;
            }

            @Override
            public BlockLevelType getBlockLevelType() {
                return BlockLevelType.SITE;
            }

            @Override
            public BlockSourceType getBlockSourceType() {
                return BlockSourceType.SERVICE;
            }

            @Override
            public Boolean prepareBlockDetection(Browser browser, Request request) {
                return null;
            }
        }
    }

    public BlockedTypeInterface getBlockedType(final Request request) {
        final List<BlockedTypeInterface> blockedTypeInterfaces = request != null ? this.getBlockedTypeInterfaces() : null;
        if (blockedTypeInterfaces != null) {
            for (final BlockedTypeInterface blockedTypeInterface : blockedTypeInterfaces) {
                final BlockedTypeInterface block = blockedTypeInterface.isBlocked(this, request);
                if (block != null) {
                    return block;
                }
            }
        }
        return null;
    }

    /**
     * Returns true if any antiddos provider/other sort of blocking is blocking at this moment. </br>
     * See also: https://svn.jdownloader.org/issues/89834
     */
    public boolean isBlocked() {
        final Request request = this.getRequest();
        return request != null && this.getBlockedType(request) != null;
    }

    /**
     * @return the defaultSSLTrustALL
     */
    public Boolean getDefaultSSLTrustALL() {
        return this.defaultSSLTrustALL;
    }

    /**
     * @param defaultSSLTrustALL
     *            the defaultSSLTrustALL to set
     */
    public void setDefaultSSLTrustALL(Boolean defaultSSLTrustALL) {
        this.defaultSSLTrustALL = defaultSSLTrustALL;
    }
}
