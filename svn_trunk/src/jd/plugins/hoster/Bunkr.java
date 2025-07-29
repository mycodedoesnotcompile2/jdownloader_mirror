package jd.plugins.hoster;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Pattern;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.HTMLParser;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.PluginJSonUtils;
import jd.plugins.decrypter.BunkrAlbum;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.Base64;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.plugins.components.config.BunkrConfig;
import org.jdownloader.plugins.config.PluginJsonConfig;

@HostPlugin(revision = "$Revision: 51270 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { BunkrAlbum.class })
public class Bunkr extends PluginForHost {
    public Bunkr(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getMirrorID(DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            return getHost() + "://" + fid;
        } else {
            return super.getMirrorID(link);
        }
    }

    @Override
    public String getAGBLink() {
        return "https://" + BunkrAlbum.MAIN_BUNKR_DOMAIN + "/faq";
    }

    public static List<String[]> getPluginDomains() {
        return BunkrAlbum.getPluginDomains();
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        return buildAnnotationUrls(getPluginDomains());
    }

    private final static Pattern PATH_FUID_NUMBERS = Pattern.compile("/file/(\\d+)");

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://get\\." + buildHostsPatternPart(domains) + PATH_FUID_NUMBERS.pattern());
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String rewriteHost(final String host) {
        /* This host is frequently changing its' main domain. */
        /* 2023-01-03: Main domain changed from bunkr.la to bunkrr.su */
        return this.rewriteHost(getPluginDomains(), host);
    }

    public static List<String> getDeadCDNDomains() {
        return Arrays.asList(new String[] { "bunkr.su" });
    }

    private int getMaxChunks(final Account account) {
        return 1;
    }

    /* Don't touch the following! */
    private static AtomicInteger freeRunning                                         = new AtomicInteger(0);
    private final static Pattern PATTERN_FID                                         = Pattern.compile("(-([A-Za-z0-9]{8}))(\\.[^\\.]+)?$");
    /* Plugin properties */
    private static final String  PROPERTY_LAST_GRABBED_DIRECTURL                     = "last_grabbed_directurl";
    private static final String  PROPERTY_LAST_KNOWN_FID                             = "last_known_fid";
    private static final String  PROPERTY_LAST_GRABBED_VIDEO_STREAM_DIRECTURL        = "last_grabbed_video_stream_directurl";
    private static final String  PROPERTY_LAST_GRABBED_IMAGE_FULLSIZE_VIEW_DIRECTURL = "last_grabbed_image_fullsize_view_directurl";
    private static final String  PROPERTY_LAST_USED_SINGLE_FILE_URL                  = "last_used_single_file_url";
    public static final String   PROPERTY_FILENAME_FROM_ALBUM                        = "filename_from_album";
    public static final String   PROPERTY_PARSED_FILESIZE                            = "parsed_filesize";

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        final int max = getMaxSimultaneousFreeAnonymousDownloads();
        if (max == -1) {
            return -1;
        } else {
            final int running = freeRunning.get();
            return running + 1;
        }
    }

    private int getMaxSimultaneousFreeAnonymousDownloads() {
        return Integer.MAX_VALUE;
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            return "bunkr://" + fid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        // getFID should be speed/memory optimized
        String fid = link.getStringProperty(PROPERTY_LAST_KNOWN_FID);
        if (fid != null) {
            return fid;
        }
        final String lastStoredDirecturl = link.getStringProperty(PROPERTY_LAST_GRABBED_DIRECTURL);
        if (lastStoredDirecturl != null) {
            fid = getFidFromURL(lastStoredDirecturl);
        }
        if (fid == null) {
            fid = getFidFromURL(link.getPluginPatternMatcher());
        }
        if (fid == null) {
            /* Fallback: Use filename as FID. */
            fid = getFilenameFromURL(link);
        }
        link.setProperty(PROPERTY_LAST_KNOWN_FID, fid);
        return fid;
    }

    private String getFilenameFromURL(final DownloadLink link) {
        String filenameFromURL = getNameFromURL(this, link.getPluginPatternMatcher());
        if (filenameFromURL != null) {
            return filenameFromURL;
        } else {
            return null;
        }
    }

    public static String getNameFromURL(final Plugin plugin, final String url) {
        String filenameFromURL = new Regex(url, BunkrAlbum.PATTERN_SINGLE_FILE).getMatch(1);
        if (filenameFromURL == null) {
            // name via n parameter from download URLs
            filenameFromURL = new Regex(url, "(?:\\?|&)n=([^&#]+)").getMatch(0);
        }
        if (filenameFromURL == null) {
            try {
                filenameFromURL = Plugin.getFileNameFromURL(new URL(url));
            } catch (MalformedURLException e) {
                plugin.getLogger().log(e);
            }
        }
        if (filenameFromURL != null) {
            return Encoding.htmlDecode(filenameFromURL).trim();
        } else {
            return null;
        }
    }

    private String getFidFromURL(final String url) {
        String fid = new Regex(url, PATTERN_FID).getMatch(1);
        if (fid != null) {
            return fid;
        }
        fid = new Regex(url, PATH_FUID_NUMBERS).getMatch(0);
        if (fid != null) {
            return fid;
        }
        return fid;
    }

    /**
     * Returns usable content-url. <br>
     * If added URL contains a dead domain, a working one will be used instead.
     */
    private String getContentURL(final DownloadLink link) {
        final String url = Encoding.htmlOnlyDecode(link.getPluginPatternMatcher());
        final Regex singleFileRegex = new Regex(url, BunkrAlbum.PATTERN_SINGLE_FILE);
        final Regex cdnUrlRegex;
        final String hostFromAddedURLWithoutSubdomain = Browser.getHost(url, false);
        if (singleFileRegex.patternFind()) {
            final List<String> deadDomains = BunkrAlbum.getDeadDomains();
            final String type = singleFileRegex.getMatch(0);
            final String filename = singleFileRegex.getMatch(1);
            final String host;
            if (deadDomains != null && deadDomains.contains(hostFromAddedURLWithoutSubdomain)) {
                /* We know given host is dead -> Use current main domain */
                host = getHost();
            } else {
                /* Use domain from given url */
                host = hostFromAddedURLWithoutSubdomain;
            }
            return "https://" + host + "/" + type + "/" + filename;
        } else if ((cdnUrlRegex = new Regex(url, BunkrAlbum.PATTERN_CDN_WITHOUT_EXT)).patternFind()) {
            final String filenameFromURL = cdnUrlRegex.getMatch(1);
            return this.generateSingleFileURL(filenameFromURL);
        } else {
            /* Do not touch URL-structure, only correct dead domains. */
            final List<String> deadDomains = getDeadCDNDomains();
            if (deadDomains != null && deadDomains.contains(hostFromAddedURLWithoutSubdomain)) {
                final String newurl = url.replaceFirst(Pattern.quote(hostFromAddedURLWithoutSubdomain) + "/", getHost() + "/");
                return newurl;
            }
            return url;
        }
    }

    private String generateSingleFileURL(final String filename) {
        return "https://" + getHost() + "/f/" + filename;
    }

    private String generateSingleImageURL(final String filename) {
        return "https://" + getHost() + "/i/" + filename;
    }

    private String generateSingleVideoURL(final String filename) {
        return "https://" + getHost() + "/v/" + filename;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        this.setBrowserExclusive();
        final boolean isDownload = this.getPluginEnvironment() == PluginEnvironment.DOWNLOAD;
        final String contenturl = this.getContentURL(link);
        final String filenameFromURL = getFilenameFromURL(link);
        if (filenameFromURL != null && !link.isNameSet()) {
            /* Set unsafe filename */
            setFilename(link, filenameFromURL, true, false);
        }
        final String lastGrabbedDirecturl = link.getStringProperty(PROPERTY_LAST_GRABBED_DIRECTURL);
        final String lastUsedSingleFileURL = link.getStringProperty(PROPERTY_LAST_USED_SINGLE_FILE_URL);
        Exception exceptionFromDirecturlCheck = null;
        if (lastGrabbedDirecturl != null && lastUsedSingleFileURL != null) {
            logger.info("Trying to re-use last cached directurl: " + lastGrabbedDirecturl);
            br.getHeaders().put("Referer", lastUsedSingleFileURL);
            URLConnectionAdapter con = null;
            try {
                if (isDownload) {
                    dl = jd.plugins.BrowserAdapter.openDownload(br, link, lastGrabbedDirecturl, isResumeable(link, null), this.getMaxChunks(null));
                    con = dl.getConnection();
                } else {
                    con = br.openGetConnection(lastGrabbedDirecturl);
                }
                handleConnectionErrors(link, br, con);
                final String filenameFromHeader = Plugin.getFileNameFromDispositionHeader(con);
                final String filenameFromDirecturl = getNameFromURL(this, lastGrabbedDirecturl);
                if (filenameFromHeader != null) {
                    setFilename(link, filenameFromHeader, true, true);
                } else if (filenameFromDirecturl != null) {
                    setFilename(link, filenameFromDirecturl, true, true);
                }
                logger.info("Successfully re-used last cached directurl");
                if (con.getCompleteContentLength() > 0) {
                    if (con.isContentDecoded()) {
                        link.setVerifiedFileSize(-1);
                        link.setDownloadSize(con.getCompleteContentLength());
                    } else {
                        link.setVerifiedFileSize(con.getCompleteContentLength());
                    }
                }
                return AvailableStatus.TRUE;
            } catch (final Exception e) {
                exceptionFromDirecturlCheck = e;
                logger.log(e);
                logger.info("Failed to re-use last cached directurl");
                try {
                    con.disconnect();
                } catch (final Throwable ignore) {
                }
            } finally {
                if (!isDownload) {
                    try {
                        con.disconnect();
                    } catch (final Throwable e) {
                    }
                }
            }
        }
        String officialDownloadurl;
        String preDownloadRefererHeader = null;
        if (new Regex(link.getPluginPatternMatcher(), BunkrAlbum.PATTERN_SINGLE_FILE).patternFind()) {
            officialDownloadurl = getDirecturlFromSingleFileAvailablecheck(link, contenturl, true, isDownload);
            if (!isDownload) {
                return AvailableStatus.TRUE;
            }
        } else if (new Regex(link.getPluginPatternMatcher(), BunkrAlbum.PATTERN_CDN_WITHOUT_EXT).patternFind()) {
            officialDownloadurl = getDirecturlFromSingleFileAvailablecheck(link, contenturl, true, isDownload);
            if (!isDownload) {
                return AvailableStatus.TRUE;
            }
        } else if (new Regex(link.getPluginPatternMatcher(), PATH_FUID_NUMBERS).patternFind()) {
            crawlFileInfoInternalFuid(link, contenturl, null, isDownload);
            officialDownloadurl = link.getStringProperty(PROPERTY_LAST_GRABBED_DIRECTURL);
            if (!isDownload) {
                return AvailableStatus.TRUE;
            }
        } else {
            officialDownloadurl = contenturl;
            /* Set referer for download */
            final String containerURL = link.getContainerUrl();
            if (containerURL != null) {
                preDownloadRefererHeader = containerURL;
            } else {
                preDownloadRefererHeader = "https://" + Browser.getHost(officialDownloadurl, false) + "/";
            }
        }
        final List<String> directurls = new ArrayList<String>();
        if (officialDownloadurl != null) {
            directurls.add(officialDownloadurl);
        }
        final String lastGrabbedVideoStreamDirecturl = link.getStringProperty(PROPERTY_LAST_GRABBED_VIDEO_STREAM_DIRECTURL);
        if (lastGrabbedVideoStreamDirecturl != null) {
            if (directurls.contains(lastGrabbedVideoStreamDirecturl)) {
                logger.info("Video streaming URL is the same as official downloadurl");
            } else {
                directurls.add(lastGrabbedVideoStreamDirecturl);
            }
        }
        final String lastGrabbedImageFullsizeViewDirecturl = link.getStringProperty(PROPERTY_LAST_GRABBED_IMAGE_FULLSIZE_VIEW_DIRECTURL);
        if (lastGrabbedImageFullsizeViewDirecturl != null) {
            if (directurls.contains(lastGrabbedImageFullsizeViewDirecturl)) {
                logger.info("Image fullsize URL is the same as official downloadurl");
            } else {
                directurls.add(lastGrabbedImageFullsizeViewDirecturl);
            }
        }
        if (directurls.isEmpty()) {
            handleHTMLErrors(link, br);
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        URLConnectionAdapter con = null;
        PluginException firstException = null;
        try {
            directurlsLoop: for (int i = 0; i < directurls.size(); i++) {
                final boolean isLastItem = (i == directurls.size() - 1);
                final String directurl = directurls.get(i);
                logger.info("Attempting download of URL " + (i + 1) + "/" + directurls.size() + " | " + directurl);
                if (preDownloadRefererHeader != null) {
                    br.getHeaders().put("Referer", preDownloadRefererHeader);
                }
                if (isDownload) {
                    dl = jd.plugins.BrowserAdapter.openDownload(br, link, directurl, isResumeable(link, null), this.getMaxChunks(null));
                    con = dl.getConnection();
                } else {
                    con = br.openGetConnection(directurl);
                }
                try {
                    handleConnectionErrors(link, br, con);
                } catch (final PluginException exception1) {
                    /* E.g. redirect from cdn8.bunkr.ru/... to bukrr.su/v/... resulting in new final URL media-files8.bunkr.ru/... */
                    try {
                        if (exception1.getLinkStatus() == LinkStatus.ERROR_FILE_NOT_FOUND) {
                            /* Dead end */
                            throw exception1;
                        } else if (new Regex(br.getURL(), BunkrAlbum.PATTERN_SINGLE_FILE).patternFind()) {
                            /* Unknown URL format -> We most likely won't be able to refresh directurl from this format. */
                            logger.info("Directurl redirected to URL with unknown format -> We most likely won't be able to refresh directurl from this format. URL: " + br.getURL());
                            throw exception1;
                        } else {
                            try {
                                final String singleFileURL = br.getURL();
                                try {
                                    con.disconnect();
                                } catch (final Throwable ignore) {
                                }
                                if (br.getHttpConnection().getResponseCode() == 416) {
                                    /* E.g. resume of download which does not work at this stage -> Access URL to get HTML code. */
                                    br.getPage(singleFileURL);
                                }
                                final String freshDirecturl = getDirecturlFromSingleFileAvailablecheck(link, br.getURL(), false, isDownload);
                                /* Avoid trying again with the same directurl if we already know the result. */
                                if (StringUtils.equals(freshDirecturl, lastGrabbedDirecturl) && exceptionFromDirecturlCheck != null) {
                                    throw exceptionFromDirecturlCheck;
                                }
                                br.getHeaders().put("Referer", singleFileURL); // Important!
                                if (isDownload) {
                                    dl = jd.plugins.BrowserAdapter.openDownload(br, link, freshDirecturl, isResumeable(link, null), this.getMaxChunks(null));
                                    con = dl.getConnection();
                                } else {
                                    con = br.openGetConnection(freshDirecturl);
                                }
                                handleConnectionErrors(link, br, con);
                            } catch (final Exception exception2) {
                                throw exception1;
                            }
                        }
                    } catch (final PluginException exception3) {
                        if (firstException == null) {
                            firstException = exception3;
                        }
                        if (isLastItem) {
                            /* Throw first exception since first try = official download and 2nd is usually stream download */
                            throw firstException;
                        } else {
                            continue directurlsLoop;
                        }
                    }
                }
                /* Success! */
                if (con.getCompleteContentLength() > 0) {
                    link.setVerifiedFileSize(con.getCompleteContentLength());
                }
                final String filenameFromHeader = Plugin.getFileNameFromConnection(con);
                setFilename(link, filenameFromHeader, true, true);
                break directurlsLoop;
            }
        } catch (final Exception e) {
            try {
                con.disconnect();
            } catch (final Throwable ignore) {
            }
            throw e;
        } finally {
            if (!isDownload) {
                try {
                    con.disconnect();
                } catch (final Throwable e) {
                }
            }
        }
        return AvailableStatus.TRUE;
    }

    @Override
    protected String getFileNameFromSource(FILENAME_SOURCE source, DownloadLink link, String customName, String customExtension, URLConnectionAdapter con) {
        if (source == FILENAME_SOURCE.URL && con != null) {
            final String ret = getNameFromURL(this, con.getURL().toExternalForm());
            if (ret != null) {
                return ret;
            }
        }
        return super.getFileNameFromSource(source, link, customName, customExtension, con);
    }

    private String getDirecturlFromSingleFileAvailablecheck(final DownloadLink link, final String singleFileURL, final boolean accessURL, final boolean isDownload) throws PluginException, IOException {
        setDirectURL(link, null);
        if (accessURL) {
            br.getPage(singleFileURL);
        }
        handleResponsecodeErrors(br.getHttpConnection());
        String album = br.getRegex(">\\s*More files in this\\s*<a[^>]*href\\s*=\\s*\"(\\.\\./a/[^\"]*)\"").getMatch(0);
        if (album != null && link.getContainerUrl() == null) {
            album = br.getURL(album).toExternalForm();
            link.setContainerUrl(album);
        }
        final String filenameFromURL = Plugin.getFileNameFromURL(br._getURL());
        String filenameFromHTML = br.getRegex("<h1[^>]*>([^<]+)</h1>").getMatch(0);
        if (filenameFromHTML == null) {
            filenameFromHTML = br.getRegex("data-v=\"([^\"]+)").getMatch(0);
            if (filenameFromHTML == null) {
                filenameFromHTML = br.getRegex("<title>([^<]+)</title>").getMatch(0);
            }
        }
        if (filenameFromHTML != null) {
            filenameFromHTML = Encoding.htmlDecode(filenameFromHTML).trim();
            filenameFromHTML = filenameFromHTML.replaceAll("(?i)\\s*\\|\\s*Bunkr\\s*", "");
            /* Check for unsafe name */
            final boolean canContainFileID;
            if (StringUtils.equalsIgnoreCase(filenameFromHTML, filenameFromURL)) {
                canContainFileID = true;
            } else {
                canContainFileID = false;
            }
            setFilename(link, filenameFromHTML, canContainFileID, false);
        }
        /* 2025-03-04: html contains "debug information" with precise file size as bytes */
        final String filesizeBytesStr = br.getRegex("Debug:[^\r\n]*Size\\s*=\\s*(\\d+)").getMatch(0);
        String filesizeStr = br.getRegex("Download\\s*(\\d+[^<]+)</a>").getMatch(0);
        if (filesizeStr == null) {
            filesizeStr = br.getRegex("class=\"[^>]*text[^>]*\"[^>]*>\\s*([0-9\\.]+\\s+[MKG]B)").getMatch(0);
        }
        if (filesizeBytesStr != null) {
            final long parsedFilesize = Long.parseLong(filesizeBytesStr);
            // link.setDownloadSize(parsedFilesize);
            link.setVerifiedFileSize(parsedFilesize);
            link.setProperty(PROPERTY_PARSED_FILESIZE, parsedFilesize);
        } else if (filesizeStr != null) {
            final long parsedFilesize = SizeFormatter.getSize(filesizeStr);
            link.setDownloadSize(parsedFilesize);
            link.setProperty(PROPERTY_PARSED_FILESIZE, parsedFilesize);
        }
        final String videoStreamDirecturl = br.getRegex("<source src\\s*=\\s*\"(https?://[^\"]+)\"[^>]*type=.video/mp4").getMatch(0);
        if (videoStreamDirecturl != null) {
            link.setProperty(PROPERTY_LAST_GRABBED_VIDEO_STREAM_DIRECTURL, videoStreamDirecturl);
        }
        final String imageFullsizeViewDirecturl = br.getRegex("<a[^>]*href=\"(https?://[^\"]+)\"[^>]*>\\s*Enlarge image").getMatch(0);
        if (imageFullsizeViewDirecturl != null) {
            link.setProperty(PROPERTY_LAST_GRABBED_IMAGE_FULLSIZE_VIEW_DIRECTURL, imageFullsizeViewDirecturl);
        }
        /* 2024-02-16: New: Additional step required to find official downloadurl */
        final String nextStepURL = br.getRegex("(https?://get\\.[^/]+/file/\\d+)").getMatch(0);
        String directurl = null;
        if (nextStepURL != null) {
            final String slug = br.getRegex("var jsSlug = '([^']+)").getMatch(0);
            crawlFileInfoInternalFuid(link, nextStepURL, slug, isDownload);
            directurl = link.getStringProperty(PROPERTY_LAST_GRABBED_DIRECTURL);
        }
        if (directurl == null) {
            directurl = br.getRegex("href\\s*=\\s*\"(https?://[^\"]+)[^>]*>\\s*Download").getMatch(0);
            if (directurl == null) {
                String unsafeDirecturlResultForFileWithoutExtOrUnknownExt = null;
                String unsafeDirecturlResult = null;
                final String[] urls = HTMLParser.getHttpLinks(br.getRequest().getHtmlCode(), br.getURL());
                for (final String url : urls) {
                    if (url.matches(BunkrAlbum.TYPE_MEDIA_FILES_WITH_EXT) || url.matches(BunkrAlbum.TYPE_CDN_WITH_EXT)) {
                        /* Safe result */
                        directurl = url;
                        break;
                    } else if (url.matches(BunkrAlbum.TYPE_MEDIA_FILES_WITHOUT_EXT) || new Regex(url, BunkrAlbum.PATTERN_CDN_WITHOUT_EXT).patternFind()) {
                        unsafeDirecturlResultForFileWithoutExtOrUnknownExt = url;
                    } else if (StringUtils.containsIgnoreCase(url, "download=true")) {
                        /* Image URLs: bunkr.bla/i/... */
                        directurl = url;
                        break;
                    } else if (!url.equals(br.getURL()) && (StringUtils.contains(url, filenameFromURL) || StringUtils.contains(url, filenameFromHTML))) {
                        unsafeDirecturlResult = url;
                    }
                }
                if (directurl == null && unsafeDirecturlResult != null) {
                    logger.info("Using unsafeDirecturlResult as directurl");
                    directurl = unsafeDirecturlResult;
                } else if (directurl == null && unsafeDirecturlResultForFileWithoutExtOrUnknownExt != null) {
                    /* File without extension or extension we don't know. */
                    logger.info("Using unsafeDirecturlResultForFileWithoutExtOrUnknownExt as directurl");
                    directurl = unsafeDirecturlResultForFileWithoutExtOrUnknownExt;
                }
            }
            /* Last chance */
            if (directurl == null) {
                directurl = br.getRegex("class=\"text-white hover:[^\"]*justify-center rounded[^\"]*\" href=\"(https?://[^\"]+)\">").getMatch(0);
                if (directurl == null && filenameFromURL != null) {
                    /* 2023-10-06 e.g. burger.bunkr.ru/... or pizza.bunkr.ru/... */
                    directurl = br.getRegex("(https?://[a-z0-9\\-]+\\.[^/]+/" + Pattern.quote(filenameFromURL) + ")").getMatch(0);
                }
                if (directurl == null && filenameFromHTML != null) {
                    /* 2023-10-06 e.g. burger.bunkr.ru/... or pizza.bunkr.ru/... */
                    directurl = br.getRegex("(https?://[a-z0-9\\-]+\\.[^/]+/" + Pattern.quote(filenameFromHTML) + ")").getMatch(0);
                }
            }
        }
        if (directurl != null) {
            directurl = Encoding.htmlOnlyDecode(directurl);
            final String filename = getNameFromURL(this, directurl);
            setFilename(link, filename, true, false);
            setDirectURL(link, directurl);
        } else {
            logger.warning("Failed to find download directurl");
        }
        if (directurl == null && imageFullsizeViewDirecturl == null) {
            logger.warning("Failed to find any directurl");
        }
        setDirectURL(link, singleFileURL);
        return directurl;
    }

    private void setDirectURL(final DownloadLink link, final String directURL) {
        if (directURL != null) {
            link.setProperty(PROPERTY_LAST_USED_SINGLE_FILE_URL, directURL);
        } else {
            link.removeProperty(PROPERTY_LAST_GRABBED_DIRECTURL);
        }
        link.removeProperty(PROPERTY_LAST_KNOWN_FID);
    }

    /** For links like this: https://get.bunkrr.su/file/123456... */
    private void crawlFileInfoInternalFuid(final DownloadLink link, final String contenturl, final String slug, final boolean isDownload) throws PluginException, IOException {
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* Check some more filename sources */
        String filenameFromHTML = br.getRegex("var ogname = \"([^\"]+)\"").getMatch(0);// javascript
        if (filenameFromHTML == null) {
            filenameFromHTML = br.getRegex("<title>Download ([^<]+)</title>").getMatch(0);// html
        }
        if (filenameFromHTML != null) {
            filenameFromHTML = PluginJSonUtils.unescape(filenameFromHTML);// for javascript
            filenameFromHTML = Encoding.htmlDecode(filenameFromHTML).trim(); // for html
            setFilename(link, filenameFromHTML, false, true);
        } else {
            logger.warning("Failed to find filename");
        }
        final String filesizeStr = br.getRegex("<p class=\"mt-1 text-xs\"[^>]*>(\\d+[^<]+)</p>").getMatch(0);
        if (filesizeStr != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesizeStr));
        } else {
            logger.info("Failed to find filesize");
        }
        if (!isDownload) {
            return;
        }
        final String internalFileID = br.getRegex("data-id=\"(\\d+)\"").getMatch(0);
        final Browser brc = br.cloneBrowser();
        brc.getHeaders().put("Content-Type", "application/json");
        /* Same request also works with the other fileID ([A-Za-z0-9]{8}) as parameter "slug" on current browser domain. */
        if (internalFileID != null) {
            // brc.postPageRaw("https://get.bunkrr.su/api/_001", "{\"id\":\"" + internalFileID + "\"}");
            brc.postPageRaw("https://apidl.bunkr.ru/api/_001_v2", "{\"id\":\"" + internalFileID + "\"}");
        } else if (slug != null) {
            /*
             * Fallback -> They are using this to obtain video streaming URLs but those URLs also just lead to the original file that we
             * want.
             */
            brc.getHeaders().put("Origin", "https://bunkr.si");
            brc.postPageRaw("https://bunkr.si/api/vs", "{\"slug\":\"" + PluginJSonUtils.escape(slug) + "\"}");
        } else {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        final Number timestamp = (Number) entries.get("timestamp");
        String url = entries.get("url").toString();
        String directurl;
        if (Boolean.TRUE.equals(entries.get("encrypted"))) {
            directurl = decryptData(url, timestamp.longValue());
        } else {
            directurl = url;
        }
        if (filenameFromHTML != null && !directurl.contains("?") && !directurl.contains(filenameFromHTML)) {
            /* n param value is returned via Content-Disposition header as filename */
            directurl += "?n=" + URLEncode.encodeURIComponent(filenameFromHTML);
        }
        setDirectURL(link, directurl);
    }

    /** See https://get.bunkrr.su/js/src.enc.js */
    public static String decryptData(final String encryptedBase64, final long timestamp) throws UnsupportedEncodingException {
        final byte[] encryptedBytes = Base64.decode(encryptedBase64);
        final String secretKey = "SECRET_KEY_" + (timestamp / 3600);
        return xorDecrypt(encryptedBytes, secretKey);
    }

    private static String xorDecrypt(final byte[] data, final String key) throws UnsupportedEncodingException {
        final byte[] keyBytes = key.getBytes("UTF-8");
        final byte[] result = new byte[data.length];
        for (int i = 0; i < data.length; i++) {
            result[i] = (byte) (data[i] ^ keyBytes[i % keyBytes.length]);
        }
        return new String(result, "UTF-8");
    }

    public static void setFilename(final DownloadLink link, final String filename, final boolean canContainFileID, final boolean setFinalName) {
        if (StringUtils.isEmpty(filename)) {
            return;
        } else if (filename.equals(link.getFinalFileName())) {
            /*
             * Final filename has already been set and new one to set is the same -> Regardless of the parameters we got, do not modify the
             * given name, do not set any new name on our DownloadLink item.
             */
            return;
        }
        final boolean fixFilename = PluginJsonConfig.get(BunkrConfig.class).isFixFilename();
        final String filenameFromAlbum = link.getStringProperty(PROPERTY_FILENAME_FROM_ALBUM);
        String correctedFilename;
        if (fixFilename && canContainFileID) {
            final String fileID = new Regex(filename, PATTERN_FID).getMatch(0);
            if (fileID != null) {
                correctedFilename = filename.replaceFirst(fileID, "");
            } else {
                /* No fileID in link while it should be given */
                correctedFilename = filename;
            }
            final String filenameWithMinusReplacement = correctedFilename.replace("-", " ");
            if (filenameFromAlbum != null) {
                /* We know they replace spaces with minus but if the original filename already contained minus that would be a mistake. */
                if (!correctedFilename.equals(filenameFromAlbum)) {
                    correctedFilename = filenameWithMinusReplacement;
                }
                if (correctedFilename.equals(filenameFromAlbum) && !filenameFromAlbum.contains("-")) {
                    /*
                     * We were able to re-create the desired filename without the need of the one we stored -> Remove that property to save
                     * memory.
                     */
                    link.removeProperty(PROPERTY_FILENAME_FROM_ALBUM);
                }
                correctedFilename = filenameFromAlbum;
            } else {
                /* No reference "original filename" given so let's do the default replacements. */
                correctedFilename = filenameWithMinusReplacement;
            }
        } else {
            /* Do not touch given filename */
            correctedFilename = filename;
        }
        if (setFinalName) {
            link.setFinalFileName(correctedFilename);
        } else {
            link.setName(correctedFilename);
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        if (this.dl == null) {
            /* Developer mistake! */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        /* Add a download slot */
        controlMaxFreeDownloads(null, link, +1);
        try {
            /* start the dl */
            dl.startDownload();
        } finally {
            /* remove download slot */
            controlMaxFreeDownloads(null, link, -1);
        }
    }

    protected void controlMaxFreeDownloads(final Account account, final DownloadLink link, final int num) {
        if (account != null) {
            return;
        }
        synchronized (freeRunning) {
            final int before = freeRunning.get();
            final int after = before + num;
            freeRunning.set(after);
            logger.info("freeRunning(" + link.getName() + ")|max:" + getMaxSimultanFreeDownloadNum() + "|before:" + before + "|after:" + after + "|num:" + num);
        }
    }

    private void handleConnectionErrors(final DownloadLink link, final Browser br, final URLConnectionAdapter con) throws PluginException, IOException {
        final long parsedExpectedFilesize = link.getLongProperty(PROPERTY_PARSED_FILESIZE, -1);
        if (!this.looksLikeDownloadableContent(con)) {
            br.followConnection(true);
            handleResponsecodeErrors(con);
            handleHTMLErrors(link, br);
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "File broken or temporarily unavailable", 2 * 60 * 60 * 1000l);
        } else if (con.getURL().getPath().equalsIgnoreCase("/maintenance-vid.mp4") || con.getURL().getPath().equalsIgnoreCase("/v/maintenance-kek-bunkr.webm") || con.getURL().getPath().equalsIgnoreCase("/maintenance.mp4")) {
            con.disconnect();
            /* https://bnkr.b-cdn.net/maintenance-vid.mp4 */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Media temporarily not available due to ongoing server maintenance.", 2 * 60 * 60 * 1000l);
        } else if (parsedExpectedFilesize > 0 && con.getCompleteContentLength() > 0 && con.getCompleteContentLength() < (parsedExpectedFilesize * 0.5)) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "File is too small: File under maintenance?", 1 * 60 * 60 * 1000l);
        }
    }

    private void handleHTMLErrors(final DownloadLink link, final Browser br) throws PluginException {
        final String downloadDisabledError = br.getRegex("class=\"down_disabled\"[^>]*>([^<]+)</div>").getMatch(0);
        if (downloadDisabledError != null) {
            /*
             * E.g. <div class="down_disabled">The server hosting this file is currently overloaded. We've limited the download of this file
             * for now until the server is no longer overloaded. Apologies.</div>
             */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, Encoding.htmlDecode(downloadDisabledError).trim(), 2 * 60 * 60 * 1000l);
        }
    }

    private void handleResponsecodeErrors(final URLConnectionAdapter con) throws PluginException {
        if (con.getResponseCode() == 403) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 10 * 60 * 1000l);
        } else if (con.getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (con.getResponseCode() == 416) {
            /* This should never happen! */
            throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Error 416", 30 * 1000l);
        } else if (con.getResponseCode() == 429) {
            throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Error 429 too many requests", 30 * 1000l);
        } else if (con.getResponseCode() == 502) {
            throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Error 502 Bad Gateway", 60 * 1000l);
        } else if (con.getResponseCode() == 503) {
            throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Error 503 too many connections", 60 * 1000l);
        }
    }

    @Override
    public Class<? extends BunkrConfig> getConfigInterface() {
        return BunkrConfig.class;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
        link.removeProperty(PROPERTY_LAST_KNOWN_FID);
    }
}
