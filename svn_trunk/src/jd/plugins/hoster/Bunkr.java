package jd.plugins.hoster;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Pattern;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Request;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.nutils.encoding.HTMLEntities;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.decrypter.BunkrAlbum;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.encoding.URLEncode.Decoder;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.components.config.BunkrConfig;
import org.jdownloader.plugins.config.PluginJsonConfig;

@HostPlugin(revision = "$Revision: 52884 $", interfaceVersion = 3, names = {}, urls = {})
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
        String fid = link.getStringProperty(PROPERTY_LAST_KNOWN_FID);
        if (fid != null) {
            return fid;
        }
        fid = getFID(link);
        if (fid != null) {
            fid = "bunkr://" + fid;
            link.setProperty(PROPERTY_LAST_KNOWN_FID, fid);
            return fid;
        } else {
            return super.getMirrorID(link);
        }
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        String fid = link.getStringProperty(PROPERTY_LAST_KNOWN_FID);
        if (fid != null) {
            return fid;
        }
        fid = getFID(link);
        if (fid != null) {
            fid = "bunkr://" + fid;
            link.setProperty(PROPERTY_LAST_KNOWN_FID, fid);
            return fid;
        } else {
            return super.getLinkID(link);
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

    private String getFID(final DownloadLink link) {
        final String lastStoredDirecturl = link.getStringProperty(PROPERTY_LAST_GRABBED_DIRECTURL);
        String fid = null;
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
        return fid;
    }

    private String getFilenameFromURL(final DownloadLink link) {
        final String filenameFromURL = getNameFromURL(this, link.getPluginPatternMatcher());
        return filenameFromURL;
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

    private boolean isExpired(final String url) throws IOException {
        final UrlQuery query = UrlQuery.parse(url);
        final String ex = query.get("ex");
        if (ex == null) {
            return false;
        }
        final long expireOn = Long.parseLong(ex) * 1000;
        if (System.currentTimeMillis() + (5 * 60 * 1000) < expireOn) {
            return false;
        } else {
            return true;
        }
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
        final String lastGrabbedDirecturl = getLastGrabbedDirectURL(link);
        final String lastUsedSingleFileURL = link.getStringProperty(PROPERTY_LAST_USED_SINGLE_FILE_URL);
        Exception exceptionFromDirecturlCheck = null;
        if (lastGrabbedDirecturl != null && !isExpired(lastGrabbedDirecturl) && lastUsedSingleFileURL != null) {
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
        final String pluginPatternMatcher = link.getPluginPatternMatcher();
        String preDownloadRefererHeader = null;
        Exception exceptionDuringDirecturlGeneration = null;
        final List<String> directurls = new ArrayList<String>();
        try {
            String officialDownloadurl = null;
            if (new Regex(pluginPatternMatcher, BunkrAlbum.PATTERN_SINGLE_FILE).patternFind()) {
                officialDownloadurl = getDirecturlFromSingleFileAvailablecheck(link, contenturl, true);
                if (!isDownload) {
                    return AvailableStatus.TRUE;
                }
            } else if (new Regex(pluginPatternMatcher, BunkrAlbum.PATTERN_CDN_WITHOUT_EXT).patternFind()) {
                officialDownloadurl = getDirecturlFromSingleFileAvailablecheck(link, contenturl, true);
                if (!isDownload) {
                    return AvailableStatus.TRUE;
                }
            } else if (new Regex(pluginPatternMatcher, PATH_FUID_NUMBERS).patternFind()) {
                crawlFileInfoInternalFuid(link, contenturl);
                officialDownloadurl = getLastGrabbedDirectURL(link);
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
            if (officialDownloadurl != null) {
                directurls.add(officialDownloadurl);
            }
        } catch (final Exception e) {
            if (!isDownload) {
                /* Throw exception if it happens during linkcheck */
                throw e;
            }
            exceptionDuringDirecturlGeneration = e;
            logger.info("Exception happened during attempted directurl generation -> If there are no other viable downloadlinks, this exception will be thrown later");
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
            if (exceptionDuringDirecturlGeneration != null) {
                throw exceptionDuringDirecturlGeneration;
            }
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
                                final String freshDirecturl = getDirecturlFromSingleFileAvailablecheck(link, br.getURL(), false);
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
                if (link.getFinalFileName() == null) {
                    final String filenameFromHeader = Plugin.getFileNameFromConnection(con);
                    setFilename(link, filenameFromHeader, true, true);
                }
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
    protected String getFileNameFromSource(FILENAME_SOURCE source, DownloadLink link, URLConnectionAdapter con, String... customValues) {
        if (source == FILENAME_SOURCE.URL && con != null) {
            final String ret = getNameFromURL(this, con.getURL().toExternalForm());
            if (ret != null) {
                return ret;
            }
        }
        return super.getFileNameFromSource(source, link, con, customValues);
    }

    private String getDirecturlFromSingleFileAvailablecheck(final DownloadLink link, String singleFileURL, final boolean accessURL) throws PluginException, IOException {
        setDirectURL(link, null);
        if (accessURL) {
            br.getPage(singleFileURL);
            if (br.getHttpConnection().getResponseCode() == 404) {
                /**
                 * dirty workaround for filenames that contain html encoded entities, see: <br>
                 * https://board.jdownloader.org/showthread.php?t=97785
                 */
                String htmlCoded = singleFileURL;
                htmlCoded = URLEncode.decodeURIComponent(htmlCoded, new Decoder() {
                    @Override
                    public String decode(final String value) throws UnsupportedEncodingException {
                        try {
                            String ret = URLDecoder.decode(value, "UTF-8");
                            ret = HTMLEntities.htmlentities(ret);
                            return ret;
                        } catch (IllegalArgumentException e) {
                            return value;
                        }
                    }
                });
                if (!StringUtils.equals(singleFileURL, htmlCoded)) {
                    br.getPage(htmlCoded);
                    if (br.getHttpConnection().getResponseCode() != 404) {
                        singleFileURL = htmlCoded;
                    }
                }
            }
        }
        handleResponsecodeErrors(br.getHttpConnection());
        link.setProperty(PROPERTY_LAST_USED_SINGLE_FILE_URL, singleFileURL);
        if (link.getContainerUrl() == null) {
            String album = br.getRegex(">\\s*More files in this\\s*<a[^>]*href\\s*=\\s*\"(\\.\\./a/[^\"]*)\"").getMatch(0);
            if (album != null) {
                album = br.getURL(album).toExternalForm();
                link.setContainerUrl(album);
            }
        }
        final Regex safeFileInfoFromHTML = br.getRegex("Debug: Original=(.+), Size=(\\d+)[\r\n]");
        String filename = null;
        if (safeFileInfoFromHTML.patternFind()) {
            /**
             * 2025-03-04: html contains "debug information" with original file name and file size as bytes <br>
             * This is the most trustworthy file information source we can get!
             */
            final String safeFilenameFromHTML = safeFileInfoFromHTML.getMatch(0);
            setFilename(link, safeFilenameFromHTML, false, true);
            final String filesizeBytesStr = safeFileInfoFromHTML.getMatch(1);
            /* Byte-precise file size */
            final long parsedFilesize = Long.parseLong(filesizeBytesStr);
            link.setVerifiedFileSize(parsedFilesize);
            link.setProperty(PROPERTY_PARSED_FILESIZE, parsedFilesize);
            filename = safeFilenameFromHTML;
        } else {
            logger.info("Failed to find safe/trustworthy file information in html code");
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
                final String filenameFromURL = Plugin.getFileNameFromURL(br._getURL());
                final boolean canContainFileID;
                if (StringUtils.equalsIgnoreCase(filenameFromHTML, filenameFromURL)) {
                    canContainFileID = true;
                } else {
                    canContainFileID = false;
                }
                setFilename(link, filenameFromHTML, canContainFileID, false);
            } else {
                logger.warning("Failed to find filename in html");
            }
            /* Vague file size */
            String filesizeStr = br.getRegex("Download\\s*(\\d+[^<]+)</a>").getMatch(0);
            if (filesizeStr == null) {
                filesizeStr = br.getRegex("class=\"[^>]*text[^>]*\"[^>]*>\\s*([0-9\\.]+\\s+[MKG]B)").getMatch(0);
            }
            if (filesizeStr != null) {
                final long parsedFilesize = SizeFormatter.getSize(filesizeStr);
                link.setDownloadSize(parsedFilesize);
                link.setProperty(PROPERTY_PARSED_FILESIZE, parsedFilesize);
            } else {
                logger.warning("Failed to find filesize in html");
            }
            filename = filenameFromHTML;
        }
        final String videoStreamDirecturl = br.getRegex("<source src\\s*=\\s*\"(https?://[^\"]+)\"[^>]*type=.video/mp4").getMatch(0);
        if (videoStreamDirecturl != null) {
            link.setProperty(PROPERTY_LAST_GRABBED_VIDEO_STREAM_DIRECTURL, videoStreamDirecturl);
        }
        final String imageFullsizeViewDirecturl = br.getRegex("<a[^>]*href=\"(https?://[^\"]+)\"[^>]*>\\s*Enlarge image").getMatch(0);
        if (imageFullsizeViewDirecturl != null) {
            link.setProperty(PROPERTY_LAST_GRABBED_IMAGE_FULLSIZE_VIEW_DIRECTURL, imageFullsizeViewDirecturl);
        }
        final boolean isDownload = this.getPluginEnvironment() == PluginEnvironment.DOWNLOAD;
        if (!isDownload) {
            /* We aren't trying to start a download atm -> No need to proceed to next step -> Speeds up linkcheck */
            return null;
        }
        fastLane: {
            try {
                final String ret = this.parseAndSetJsCDNDirectURL(link, br, filename);
                if (ret != null) {
                    return ret;
                }
            } catch (final PluginException e) {
                /* Silent ignore to allow fallback to upper handling */
                logger.info("Failed to find direct download link on file view page -> Official download button handling is needed");
            }
        }
        /* 2024-02-16: New: Additional step required to find official downloadurl */
        final String nextStepURL = br.getRegex("(https?://(?:get|dl)\\.[^/]+/file/\\d+)").getMatch(0);
        if (nextStepURL == null) {
            logger.warning("Failed to find nextStepURL");
            return null;
        }
        /* This slug value is mostly available for video items */
        crawlFileInfoInternalFuid(link, nextStepURL);
        final String directurl = getLastGrabbedDirectURL(link);
        if (directurl == null && imageFullsizeViewDirecturl == null && videoStreamDirecturl == null) {
            logger.warning("Failed to find any directurl");
        }
        return directurl;
    }

    private String getLastGrabbedDirectURL(final DownloadLink link) throws IOException {
        final String url = link.getStringProperty(PROPERTY_LAST_GRABBED_DIRECTURL);
        if (url == null || isExpired(url)) {
            return null;
        }
        return url;
    }

    private void setDirectURL(final DownloadLink link, final String directURL) {
        if (directURL != null) {
            link.setProperty(PROPERTY_LAST_GRABBED_DIRECTURL, directURL);
        } else {
            link.removeProperty(PROPERTY_LAST_GRABBED_DIRECTURL);
        }
        link.removeProperty(PROPERTY_LAST_KNOWN_FID);
    }

    /** For links like this: https://get.bunkrr.su/file/123456... */
    private String crawlFileInfoInternalFuid(final DownloadLink link, final String contenturl) throws PluginException, IOException {
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* Check some more filename sources */
        String fileName = br.getRegex("var\\s*ogname\\s*=\\s*(\"[^\"]+\")").getMatch(0);// javascript
        if (fileName != null) {
            fileName = restoreFromString(fileName, TypeRef.STRING);
        } else if ((fileName = br.getRegex("<title>Download ([^<]+)</title>").getMatch(0)) != null) {
            fileName = Encoding.htmlDecode(fileName).trim();
        }
        if (fileName != null) {
            setFilename(link, fileName, false, true);
        } else {
            logger.warning("Failed to find filename");
        }
        final String filesizeStr = br.getRegex("<p class=\"mt-1 text-xs\"[^>]*>(\\d+[^<]+)</p>").getMatch(0);
        if (filesizeStr != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesizeStr));
        } else {
            logger.info("Failed to find filesize");
        }
        final boolean isDownload = this.getPluginEnvironment() == PluginEnvironment.DOWNLOAD;
        if (!isDownload) {
            /* We aren't trying to download -> Early return to speed up linkcheck */
            return null;
        }
        final String internalFileID = br.getRegex("data-id=\"(\\d+)\"").getMatch(0);
        String rawURL = null;
        Map<String, Object> response = null;
        api: if (internalFileID != null) {
            final String SIGN_SERVICE_URL = br.getRegex("(?:var|const)\\s*SIGN_SERVICE_URL\\s*=\\s*'(https?:[^\']+)").getMatch(0);
            if (SIGN_SERVICE_URL == null) {
                break api;
            }
            Browser brc = br.cloneBrowser();
            brc.getHeaders().put(HTTPConstants.HEADER_REQUEST_ACCEPT, "*/*");
            brc.getHeaders().put("Content-Type", "application/json");
            brc.postPageRaw("/api/_001_v2", "{\"id\":\"" + internalFileID + "\"}");
            Map<String, Object> apiResponse = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            rawURL = (String) apiResponse.get("mediafiles") + (String) apiResponse.get("path");
            final String path = new URL(rawURL).getPath();
            brc = br.cloneBrowser();
            final Request fetch = brc.createGetRequest(SIGN_SERVICE_URL + "?path=" + path);
            fetch.getHeaders().put(HTTPConstants.HEADER_REQUEST_ACCEPT, "*/*");
            brc.getPage(fetch);
            response = restoreFromString(fetch.getHtmlCode(), TypeRef.MAP);
        }
        if (response == null) {
            /* 2026-06-03: This handling might be outdated, see function parseAndSetJsCDNDirectURL !!! */
            String jsCDN = br.getRegex("var\\s*jsCDN\\s*=\\s*(\"https?:[^\"]+\")").getMatch(0);
            String jsSlug = br.getRegex("var\\s*jsSlug\\s*=\\s*(\"[^\"]+\")").getMatch(0);
            String signUrl = br.getRegex("var\\s*signUrl\\s*=\\s*(\"[^\"]+\")").getMatch(0);
            if (!StringUtils.isAllNotEmpty(jsCDN, jsSlug, signUrl)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            jsCDN = restoreFromString(jsCDN, TypeRef.STRING);
            jsSlug = restoreFromString(jsSlug, TypeRef.STRING);
            signUrl = restoreFromString(signUrl, TypeRef.STRING);
            rawURL = jsCDN.replaceFirst("([/]+$)", "") + "/storage/media/" + jsSlug;
            final String path = new URL(rawURL).getPath();
            final Browser brc = br.cloneBrowser();
            final Request fetch = brc.createGetRequest(signUrl + "?path=" + path);
            fetch.getHeaders().put(HTTPConstants.HEADER_REQUEST_ACCEPT, "*/*");
            brc.getPage(fetch);
            response = restoreFromString(fetch.getHtmlCode(), TypeRef.MAP);
        }
        final String token = (String) response.get("token");
        final Object ex = response.get("ex");
        if (StringUtils.isEmpty(token) || ex == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        String directurl = rawURL + "?token=" + token + "&ex=" + ex;
        if (!StringUtils.isEmpty(fileName)) {
            directurl = directurl + "&n=" + URLEncode.encodeURIComponent(fileName);
        }
        setDirectURL(link, directurl);
        return directurl;
    }

    /** Extracts/generates and returns final downloadlink from download overview page. */
    private String parseAndSetJsCDNDirectURL(final DownloadLink link, final Browser br, final String fileName) throws PluginException, IOException {
        String jsCDN = br.getRegex("var\\s*jsCDN\\s*=\\s*(\"https?:[^\"]+\")").getMatch(0);
        String jsSlug = br.getRegex("var\\s*jsSlug\\s*=\\s*(\"[^\"]+\")").getMatch(0);
        String signUrl = br.getRegex("var\\s*signUrl\\s*=\\s*(\"[^\"]+\")").getMatch(0);
        if (!StringUtils.isAllNotEmpty(jsCDN, jsSlug, signUrl)) {
            return null;
        }
        jsCDN = restoreFromString(jsCDN, TypeRef.STRING);
        jsSlug = restoreFromString(jsSlug, TypeRef.STRING);
        signUrl = restoreFromString(signUrl, TypeRef.STRING);
        final String path = new URL(jsCDN).getPath();
        final Browser brc = br.cloneBrowser();
        final Request fetch = brc.createGetRequest(signUrl + "?path=" + path);
        fetch.getHeaders().put(HTTPConstants.HEADER_REQUEST_ACCEPT, "*/*");
        brc.getPage(fetch);
        final Map<String, Object> response = restoreFromString(fetch.getHtmlCode(), TypeRef.MAP);
        final String token = (String) response.get("token");
        final Object ex = response.get("ex");
        if (StringUtils.isEmpty(token) || ex == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        String directurl = jsCDN + "?token=" + token + "&ex=" + ex;
        /* 2026-06-03: n parameter is optional. Server will return value set as n parameter in Content-Disposition header. */
        if (!StringUtils.isEmpty(fileName)) {
            directurl += "&n=" + URLEncode.encodeURIComponent(fileName);
        }
        setDirectURL(link, directurl);
        return directurl;
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
        final String media_under_maintenance_text = "Media temporarily not available due to ongoing server maintenance.";
        final long media_under_maintenance_wait = 2 * 60 * 60 * 1000l;
        final String path = con.getURL().getPath();
        final int min_filesize_percent = this.get(this.getConfigInterface()).getMinimumFileSizePercentage();
        if (!this.looksLikeDownloadableContent(con)) {
            br.followConnection(true);
            handleResponsecodeErrors(con);
            handleHTMLErrors(link, br);
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "File broken or temporarily unavailable", 2 * 60 * 60 * 1000l);
        } else if (path.equalsIgnoreCase("/maintenance-vid.mp4") || path.equalsIgnoreCase("/v/maintenance-kek-bunkr.webm") || path.equalsIgnoreCase("/maintenance.mp4")) {
            con.disconnect();
            /* https://bnkr.b-cdn.net/maintenance-vid.mp4 */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, media_under_maintenance_text, media_under_maintenance_wait);
        } else if (path.endsWith("/maint.mp4") && con.getCompleteContentLength() <= 322509l) {
            /* 2026-01-09 */
            /* https://3d09xl1.b-cdn.net/c4f36040-bdd1-40b6-aea1-034dfbe88ba2/maint.mp4 */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, media_under_maintenance_text, media_under_maintenance_wait);
        } else if (parsedExpectedFilesize > 0 && con.getCompleteContentLength() > 0 && con.getCompleteContentLength() < (parsedExpectedFilesize * min_filesize_percent / 100)) {
            /* File size we got is smaller than user defined min threshold. */
            /*
             * Content-Type: image/jpeg
             *
             * Content-Length: 5534281
             *
             * Cf-Bgj: imgq:100,h2pri
             *
             * Cf-Polished: origSize=5817221
             */
            final String origSize = new Regex(con.getHeaderField("Cf-Polished"), "origSize=(\\d+)").getMatch(0);
            final String error_msg = "File is too small (" + con.getCompleteContentLength() * 100 / parsedExpectedFilesize + "% of expected size, minimum is " + min_filesize_percent + "%). Change threshold in plugin settings or try again later.";
            if (origSize == null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, error_msg, TimeUnit.HOURS.toMillis(1));
            }
            final long origSizeLong = Long.parseLong(origSize);
            if (origSizeLong == parsedExpectedFilesize) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, error_msg, TimeUnit.HOURS.toMillis(1));
            }
            logger.info("apply cloudflare polish workaround:" + origSizeLong + "!=" + parsedExpectedFilesize);
            link.setVerifiedFileSize(-1);
        }
    }

    private void handleHTMLErrors(final DownloadLink link, final Browser br) throws PluginException {
        if (br.containsHTML(">\\s*Server under maintenance\\s*<")) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "The server hosting this file is temporarily unavailable for maintenance", TimeUnit.HOURS.toMillis(2));
        }
        final String downloadDisabledError = br.getRegex("class=\"down_disabled\"[^>]*>([^<]+)</div>").getMatch(0);
        if (downloadDisabledError != null) {
            /*
             * E.g. <div class="down_disabled">The server hosting this file is currently overloaded. We've limited the download of this file
             * for now until the server is no longer overloaded. Apologies.</div>
             */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, Encoding.htmlDecode(downloadDisabledError).trim(), TimeUnit.HOURS.toMillis(2));
        }
    }

    private void handleResponsecodeErrors(final URLConnectionAdapter con) throws PluginException {
        if (con.getResponseCode() == 403) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 10 * 60 * 1000l);
        } else if (con.getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (con.getResponseCode() == 416) {
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
    public void resetDownloadlink(DownloadLink link) {
        link.removeProperty(PROPERTY_LAST_KNOWN_FID);
    }
}
