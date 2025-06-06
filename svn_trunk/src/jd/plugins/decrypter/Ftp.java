package jd.plugins.decrypter;

import java.io.IOException;
import java.net.Socket;
import java.net.URL;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.LinkCrawler;
import jd.controlling.linkcrawler.LinkCrawlerLock;
import jd.controlling.proxy.ProxyController;
import jd.http.Browser;
import jd.http.BrowserSettingsThread;
import jd.http.NoGateWayException;
import jd.http.ProxySelectorInterface;
import jd.http.SocketConnectionFactory;
import jd.nutils.SimpleFTP;
import jd.nutils.SimpleFTP.SimpleFTPListEntry;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterException;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.PluginForDecrypt;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.config.WeakHashSet;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.net.httpconnection.HTTPProxy;
import org.appwork.utils.net.httpconnection.HTTPProxyException;
import org.jdownloader.auth.AuthenticationController;
import org.jdownloader.auth.AuthenticationInfo;
import org.jdownloader.auth.AuthenticationInfo.Type;
import org.jdownloader.auth.Login;
import org.jdownloader.plugins.controller.crawler.LazyCrawlerPlugin;

@DecrypterPlugin(revision = "$Revision: 51066 $", interfaceVersion = 2, names = { "ftp" }, urls = { "ftp://.*?\\.[\\p{L}\\p{Nd}a-zA-Z0-9]{1,}(:\\d+)?/([^\\?&\"\r\n ]+|$)" })
public class Ftp extends PluginForDecrypt {
    private static Map<String, Integer>     LIMITS = new HashMap<String, Integer>();
    private static Map<String, Set<Thread>> LOCKS  = new HashMap<String, Set<Thread>>();

    public Ftp(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public ArrayList<DownloadLink> decryptIt(final CryptedLink cLink, ProgressController progress) throws Exception {
        final String lockHost = Browser.getHost(cLink.getCryptedUrl());
        final DownloadLink downloadLink = cLink.getDownloadLink();
        if (downloadLink != null && downloadLink.hasProperty(jd.plugins.hoster.Ftp.MAX_FTP_CONNECTIONS)) {
            synchronized (LIMITS) {
                LIMITS.put(lockHost, downloadLink.getIntegerProperty(jd.plugins.hoster.Ftp.MAX_FTP_CONNECTIONS, 1));
            }
        }
        final Set<Thread> hostLocks;
        synchronized (LOCKS) {
            Set<Thread> tmp = LOCKS.get(lockHost);
            if (tmp == null) {
                tmp = new WeakHashSet<Thread>();
                LOCKS.put(lockHost, tmp);
            }
            hostLocks = tmp;
        }
        int retryCounter = 0;
        final Thread thread = Thread.currentThread();
        while (true) {
            try {
                while (true) {
                    final int maxConcurrent;
                    synchronized (LIMITS) {
                        final Integer setLimit = LIMITS.get(lockHost);
                        if (LIMITS.containsKey(lockHost)) {
                            if (setLimit == null) {
                                maxConcurrent = 1;
                            } else {
                                maxConcurrent = Math.max(1, setLimit.intValue());
                            }
                        } else {
                            break;
                        }
                    }
                    synchronized (hostLocks) {
                        if (isAbort()) {
                            throw new InterruptedException();
                        } else if (hostLocks.size() < maxConcurrent) {
                            hostLocks.add(thread);
                            break;
                        } else if (hostLocks.size() == maxConcurrent) {
                            hostLocks.wait(5000);
                            if (retryCounter++ > 10) {
                                throw new InterruptedException();
                            }
                        } else if (hostLocks.size() > maxConcurrent) {
                            hostLocks.wait(5000);
                        }
                    }
                }
                try {
                    return internalDecryptIt(cLink, progress);
                } catch (WTFException e) {
                    logger.log(e);
                    if (StringUtils.startsWithCaseInsensitive(e.getMessage(), "retry")) {
                        continue;
                    } else {
                        throw e;
                    }
                }
            } catch (InterruptedException e) {
                if (retryCounter > 10 || (retryCounter > 0 && isAbort())) {
                    throw new DecrypterRetryException(RetryReason.HOST, "Too many concurrent connections! Try again later");
                } else {
                    throw e;
                }
            } finally {
                synchronized (hostLocks) {
                    hostLocks.remove(thread);
                    hostLocks.notify();
                }
            }
        }
    }

    private ArrayList<DownloadLink> internalDecryptIt(final CryptedLink cLink, ProgressController progress) throws Exception {
        final URL url = new URL(cLink.getCryptedUrl());
        final String lockHost = Browser.getHost(url);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>() {
            @Override
            public boolean add(final DownloadLink link) {
                final Integer maxFTPConnections;
                synchronized (LIMITS) {
                    maxFTPConnections = LIMITS.get(lockHost);
                }
                if (maxFTPConnections != null && maxFTPConnections.intValue() > 0) {
                    link.setProperty(jd.plugins.hoster.Ftp.MAX_FTP_CONNECTIONS, maxFTPConnections);
                }
                return super.add(link);
            }
        };
        final List<HTTPProxy> proxies = selectProxies(url);
        final HTTPProxy proxy = proxies.get(0);
        final SimpleFTP ftp = new SimpleFTP(proxy, logger) {
            @Override
            protected Socket createSocket() {
                return SocketConnectionFactory.createSocket(getProxy());
            }

            @Override
            protected boolean AUTH_TLS_CC() throws IOException {
                final Set<String> set = jd.plugins.hoster.Ftp.AUTH_TLS_DISABLED;
                final String host = url.getHost().toLowerCase(Locale.ENGLISH);
                synchronized (set) {
                    if (set.contains(host)) {
                        return false;
                    }
                }
                final boolean ret;
                try {
                    ret = super.AUTH_TLS_CC();
                } catch (IOException e) {
                    synchronized (set) {
                        set.add(host);
                    }
                    throw e;
                }
                if (!ret) {
                    synchronized (set) {
                        set.add(host);
                    }
                }
                return ret;
            }
        };
        try {
            try {
                ftp.connect(url);
            } catch (IOException e) {
                logger.log(e);
                final Integer limit = ftp.getConnectionLimitByException(e);
                if (limit != null) {
                    synchronized (LIMITS) {
                        LIMITS.put(lockHost, limit);
                    }
                    getCrawler().addSequentialLockObject(new LinkCrawlerLock(getLazyC()) {
                        private final String host = Browser.getHost(cLink.getCryptedUrl());

                        @Override
                        public int getMaxConcurrency() {
                            return 1;
                        }

                        @Override
                        public String getMatchingIdentifier() {
                            return super.getMatchingIdentifier().concat(host);
                        }

                        @Override
                        public String toString() {
                            return super.toString() + "|" + host;
                        }

                        @Override
                        public boolean matches(LazyCrawlerPlugin plugin, CrawledLink crawledLink) {
                            return super.matches(plugin, crawledLink) && StringUtils.equalsIgnoreCase(host, Browser.getHost(crawledLink.getURL()));
                        }
                    });
                    sleep(5000, cLink);
                    throw new WTFException("retry limit:" + lockHost + "|" + limit, e);
                } else if (ftp.isWrongLoginException(e)) {
                    final DownloadLink dummyLink = new DownloadLink(null, null, url.getHost(), cLink.getCryptedUrl(), true);
                    final Login login = requestLogins(org.jdownloader.translate._JDT.T.DirectHTTP_getBasicAuth_message(), null, dummyLink);
                    if (login != null) {
                        final String host = url.getHost();
                        int port = url.getPort();
                        if (port <= 0) {
                            port = url.getDefaultPort();
                        }
                        try {
                            ftp.connect(host, port, login.getUsername(), login.getPassword());
                            if (login.isRememberSelected()) {
                                final AuthenticationInfo auth = new AuthenticationInfo();
                                auth.setUsername(login.getUsername());
                                auth.setPassword(login.getPassword());
                                auth.setHostmask(host);
                                auth.setType(Type.FTP);
                                AuthenticationController.getInstance().add(auth);
                            }
                        } catch (IOException e2) {
                            if (ftp.isWrongLoginException(e)) {
                                throw new DecrypterException(DecrypterException.PASSWORD, e2);
                            } else {
                                throw e2;
                            }
                        }
                    } else {
                        throw new DecrypterException(DecrypterException.PASSWORD, e);
                    }
                } else {
                    throw e;
                }
            }
            final String currentDir = ftp.getDir();
            String filePath = url.getPath();
            if (!filePath.startsWith("/")) {
                filePath = "/" + filePath;
            }
            if (!filePath.startsWith(currentDir)) {
                if (currentDir.endsWith("/")) {
                    filePath = currentDir + filePath.substring(1);
                } else {
                    filePath = currentDir + filePath;
                }
            }
            final String finalFilePath = filePath;
            String nameString = filePath.substring(filePath.lastIndexOf("/") + 1);
            String nameStringUpper = nameString.toUpperCase(Locale.ENGLISH);
            final byte[] nameBytes, nameBytesUpper;
            if (StringUtils.isEmpty(nameString)) {
                nameString = null;
                nameStringUpper = null;
                nameBytes = null;
                nameBytesUpper = null;
            } else {
                nameBytes = SimpleFTP.toRawBytes(nameString);
                nameBytesUpper = SimpleFTP.toRawBytes(nameStringUpper);
            }
            filePath = filePath.substring(0, filePath.lastIndexOf("/") + 1);
            String packageName = new Regex(filePath, "/([^/]+)(/$|$)").getMatch(0);
            final String auth;
            if (!StringUtils.equals("anonymous", ftp.getUser()) || !StringUtils.equals("anonymous", ftp.getUser())) {
                auth = ftp.getUser() + ":" + ftp.getPass() + "@";
            } else {
                auth = "";
            }
            // ftp.listFeatures();
            // final List<FEATURE> features = ftp.listFeatures();
            // ftp.sendClientID("JDownloader");
            // ftp.setUTF8(true);
            final DownloadLink direct = checkLinkFile(ftp, nameString, auth, url, finalFilePath);
            if (direct == null && ftp.ascii() && ftp.cwd(filePath)) {
                SimpleFTPListEntry[] entries = ftp.listEntries();
                if (entries != null) {
                    /*
                     * logic for only adding a given file, ie. ftp://domain/directory/file.exe, you could also have subdirectory of the same
                     * name ftp.../file.exe/file.exe -raztoki
                     */
                    SimpleFTPListEntry found = null;
                    if (nameBytes != null && nameBytesUpper != null) {
                        for (final SimpleFTPListEntry entry : entries) {
                            // we compare bytes because of hex encoding
                            if (Arrays.equals(SimpleFTP.toRawBytes(entry.getName()), nameBytes)) {
                                found = entry;
                                break;
                            }
                        }
                        if (found == null) {
                            for (final SimpleFTPListEntry entry : entries) {
                                // we compare bytes because of hex encoding
                                if (Arrays.equals(SimpleFTP.toRawBytes(entry.getName()), nameBytesUpper)) {
                                    found = entry;
                                    break;
                                }
                            }
                        }
                    }
                    if (found != null) {
                        final DownloadLink linkFile = found.isLink() ? checkLinkFile(ftp, found) : null;
                        if (linkFile != null) {
                            ret.add(linkFile);
                        } else if (found.isFile()) {
                            ret.add(createDirectFile(found));
                        } else if (found.isDir()) {
                            if (ftp.cwd(found.getName())) {
                                entries = ftp.listEntries();
                            } else {
                                entries = new SimpleFTPListEntry[0];
                            }
                        }
                    }
                    /*
                     * logic for complete directory! will not walk aka recursive! - raztoki
                     */
                    if (ret.isEmpty()) {
                        /*
                         * if 'name' == file then packagename == correct, ELSE if name != file then it should be packagename! -raztoki
                         */
                        if (nameString != null) {
                            packageName = nameString;
                        }
                        int maxDepth = 0;
                        if (url.getRef() != null) {
                            final String maxDepthString = new Regex(url.getRef(), "max_depth=(-?\\d+)").getMatch(0);
                            if (maxDepthString != null) {
                                maxDepth = Integer.parseInt(maxDepthString);
                            }
                        }
                        for (final SimpleFTPListEntry entry : entries) {
                            final DownloadLink linkFile = entry.isLink() ? checkLinkFile(ftp, entry) : null;
                            if (linkFile != null) {
                                ret.add(linkFile);
                            } else if (entry.isFile()) {
                                ret.add(createDirectFile(entry));
                            } else if (entry.isDir() && (maxDepth == -1 || maxDepth > 0)) {
                                ret.add(createFolder(entry, maxDepth));
                            }
                        }
                    }
                }
            } else if (direct != null) {
                ret.add(direct);
            }
            if (ret.size() > 0 && packageName != null) {
                final FilePackage fp = FilePackage.getInstance();
                fp.setName(SimpleFTP.BestEncodingGuessingURLDecode(packageName));
                fp.setProperty(LinkCrawler.PACKAGE_ALLOW_MERGE, Boolean.TRUE);
                fp.addLinks(ret);
            }
        } catch (UnknownHostException e) {
            throw new DecrypterRetryException(RetryReason.HOST, null, null, e);
        } catch (HTTPProxyException e) {
            ProxyController.getInstance().reportHTTPProxyException(proxy, url, e);
            throw e;
        } finally {
            ftp.disconnect();
        }
        return ret;
    }

    private DownloadLink checkLinkFile(SimpleFTP ftp, final String nameString, final String auth, final URL url, final String finalFilePath) throws IOException {
        if (nameString != null && ftp.bin()) {
            final long size = ftp.getSize(finalFilePath);
            if (size >= 0) {
                final DownloadLink link = createDownloadlink("ftpviajd://" + auth + url.getHost() + (url.getPort() != -1 ? (":" + url.getPort()) : "") + finalFilePath);
                link.setAvailable(true);
                link.setVerifiedFileSize(size);
                link.setFinalFileName(SimpleFTP.BestEncodingGuessingURLDecode(nameString));
                return link;
            }
        }
        return null;
    }

    private DownloadLink checkLinkFile(final SimpleFTP ftp, final SimpleFTPListEntry entry) throws IOException {
        if (entry != null && ftp.bin()) {
            final long size = ftp.getSize(entry.getURL().getPath());
            if (size >= 0) {
                final String url = entry.getURL().toExternalForm();
                final DownloadLink ret = createDownloadlink(url);
                ret.setAvailable(true);
                ret.setVerifiedFileSize(size);
                ret.setFinalFileName(SimpleFTP.BestEncodingGuessingURLDecode(entry.getName()));
                return ret;
            }
        }
        return null;
    }

    protected DownloadLink createDownloadlink(String link, boolean urlDecode) {
        // do not URLDecode but keep original name
        return new DownloadLink(null, null, getHost(), link.replace("ftp://", "ftpviajd://"), true);
    }

    private DownloadLink createFolder(SimpleFTPListEntry entry, int maxDepth) throws IOException {
        if (!entry.isDir()) {
            return null;
        }
        if (maxDepth != -1) {
            maxDepth = Math.max(0, maxDepth - 1);
        }
        final String link = entry.getURL().toExternalForm() + ((maxDepth == -1 || maxDepth > 0) ? ("#max_depth=" + maxDepth) : "");
        return new DownloadLink(null, null, getHost(), link, true);
    }

    private DownloadLink createDirectFile(SimpleFTPListEntry entry) throws IOException {
        final DownloadLink ret = createDownloadlink(entry.getURL().toExternalForm());
        ret.setAvailable(true);
        final long size = entry.getSize();
        if (size >= 0) {
            ret.setVerifiedFileSize(size);
        }
        ret.setFinalFileName(SimpleFTP.BestEncodingGuessingURLDecode(entry.getName()));
        return ret;
    }

    protected ProxySelectorInterface getProxySelector() {
        return BrowserSettingsThread.getThreadProxySelector();
    }

    protected List<HTTPProxy> selectProxies(URL url) throws IOException {
        final ProxySelectorInterface selector = getProxySelector();
        if (selector == null) {
            final ArrayList<HTTPProxy> ret = new ArrayList<HTTPProxy>();
            ret.add(HTTPProxy.NONE);
            return ret;
        }
        final List<HTTPProxy> list;
        try {
            list = selector.getProxiesByURL(url);
        } catch (Throwable e) {
            throw new NoGateWayException(selector, e);
        }
        if (list == null || list.size() == 0) {
            throw new NoGateWayException(selector, "No Gateway or Proxy Found: " + url);
        } else {
            return list;
        }
    }

    @Override
    public Boolean siteTesterDisabled() {
        return true;
    }
}