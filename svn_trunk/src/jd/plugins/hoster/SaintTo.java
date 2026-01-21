package jd.plugins.hoster;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 52134 $", interfaceVersion = 3, names = {}, urls = {})
public class SaintTo extends PluginForHost {
    public SaintTo(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.VIDEO_STREAMING };
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/faq";
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "turbo.cr", "turbovid.cr", "saint.to", "saint2.su", "saint2.cr" });
        return ret;
    }

    @Override
    public String rewriteHost(final String host) {
        return this.rewriteHost(getPluginDomains(), host);
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    private static final Pattern TYPE_EMBED          = Pattern.compile("/embed/([A-Za-z0-9-_]+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_VIDEO_NEW_SITE = Pattern.compile("/v/([A-Za-z0-9-_]+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_API_DIRECT     = Pattern.compile("/api/download\\.php\\?file=([a-zA-Z0-9_/\\+\\=\\-%]+)/?", Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_API_INDIRECT   = Pattern.compile("/d/([a-zA-Z0-9_/\\+\\=\\-%]+)", Pattern.CASE_INSENSITIVE);

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:\\w+\\.)?" + buildHostsPatternPart(domains) + "(" + TYPE_EMBED.pattern() + "|" + TYPE_VIDEO_NEW_SITE.pattern() + "|" + TYPE_API_DIRECT.pattern() + "|" + TYPE_API_INDIRECT.pattern() + ")");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 0;
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            return this.getHost() + "://" + fid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        String fid = new Regex(link.getPluginPatternMatcher(), TYPE_EMBED).getMatch(0);
        if (fid != null) {
            return fid;
        }
        fid = new Regex(link.getPluginPatternMatcher(), TYPE_VIDEO_NEW_SITE).getMatch(0);
        if (fid != null) {
            return fid;
        }
        fid = new Regex(link.getPluginPatternMatcher(), TYPE_API_INDIRECT).getMatch(0);
        if (fid != null) {
            return fid;
        }
        fid = new Regex(link.getPluginPatternMatcher(), TYPE_API_DIRECT).getMatch(0);
        return fid;
    }

    @Override
    protected String getDefaultFileName(DownloadLink link) {
        return this.getFID(link) + ".mp4";
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        final boolean isDownload = PluginEnvironment.DOWNLOAD.equals(this.getPluginEnvironment());
        final Regex type_api_indirect;
        final String contenturl = link.getPluginPatternMatcher();
        if (new Regex(contenturl, TYPE_EMBED).patternFind()) {
            br.getPage(contenturl);
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (br.containsHTML("\"(Video not found|Video has been removed)")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            String filename = br.getRegex("property=\"og:title\" content=\"([^\"]+)\"").getMatch(0);
            if (filename != null) {
                filename = Encoding.htmlDecode(filename).trim();
                link.setName(filename);
            }
        } else if (new Regex(contenturl, TYPE_VIDEO_NEW_SITE).patternFind()) {
            br.getPage(contenturl);
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            String filename = br.getRegex("id=\"videoTitle\"[^>]*>([^<]+)<").getMatch(0);
            if (filename != null) {
                filename = Encoding.htmlDecode(filename).trim();
                link.setName(filename);
            } else {
                logger.warning("Failed to find filename");
            }
            final String filesizeVagueStr = br.getRegex("id=\"videoSize\"[^>]*>(\\d+[^<]+)</span>").getMatch(0);
            if (filesizeVagueStr != null) {
                link.setDownloadSize(SizeFormatter.getSize(filesizeVagueStr));
            } else {
                logger.warning("Failed to find filesize");
            }
        } else if ((type_api_indirect = new Regex(contenturl, TYPE_API_INDIRECT)).patternFind()) {
            /* TYPE_API_INDIRECT */
            br.getPage(contenturl);
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (br.containsHTML("File not found in the database")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            /* Only new website contains filename and md5 hash in html code */
            String filename = br.getRegex("filename:\\s*\"([^\"]+)\"").getMatch(0);
            if (filename == null) {
                filename = br.getRegex("originalFilename:\\s*\"([^\"]+)\"").getMatch(0);
            }
            if (filename != null) {
                link.setFinalFileName(filename);
            } else {
                /* Data should be available via new website */
                logger.warning("Failed to find filename");
            }
            /**
             * Set file size: There is a server side bug where for some files, the precise file size is wrong (1 byte) while the other size
             * is correct. <br>
             * If both sizes are given, we will take the bigger one.
             */
            final String filesizeBytesStr = br.getRegex("id=\"fileSizeBytes\">\\s*(\\d+)").getMatch(0);
            final String filesizeVagueStr = br.getRegex(">\\s*Size\\s*</dt>\\s*<dd[^>]*>(\\d+[^<]+)</dd>").getMatch(0);
            if (filesizeBytesStr != null && filesizeVagueStr != null) {
                final long filesizeVague = SizeFormatter.getSize(filesizeVagueStr);
                final long filesizeBytes = Long.parseLong(filesizeBytesStr);
                if (filesizeBytes > filesizeVague) {
                    link.setVerifiedFileSize(filesizeBytes);
                } else {
                    logger.fine("Server side precise filesize looks to be wrong: " + filesizeBytesStr);
                    link.setDownloadSize(filesizeVague);
                }
            } else if (filesizeBytesStr != null) {
                link.setVerifiedFileSize(Long.parseLong(filesizeBytesStr));
            } else if (filesizeVagueStr != null) {
                link.setDownloadSize(SizeFormatter.getSize(filesizeVagueStr));
            } else {
                /* Data should be available via new website */
                logger.warning("Failed to find filesize");
            }
            final String md5 = br.getRegex("Checksum \\(MD5\\)\\s*</div>\\s*<div[^>]*>([a-f0-9]{32})").getMatch(0);
            if (md5 != null) {
                link.setMD5Hash(md5);
            } else {
                /* Data should be available via new website */
                logger.warning("Failed to find md5");
            }
        } else {
            /* TYPE_API_DIRECT */
            final Regex urlinfo = new Regex(contenturl, TYPE_API_DIRECT);
            final String filenameBase64 = urlinfo.getMatch(0);
            final String filename = Encoding.htmlDecode(filenameBase64);
            link.setName(filename);
            if (!isDownload) {
                this.basicLinkCheck(br, br.createHeadRequest(contenturl), link, filename, ".mp4");
            }
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link, "free_directlink");
    }

    private void handleDownload(final DownloadLink link, final String directlinkproperty) throws Exception, PluginException {
        final String storedDirecturl = link.getStringProperty(directlinkproperty);
        final String contenturl = link.getPluginPatternMatcher();
        String dllink = null;
        final boolean storeDirecturl;
        if (new Regex(contenturl, TYPE_API_DIRECT).patternFind()) {
            dllink = contenturl;
            storeDirecturl = false;
        } else if (storedDirecturl != null) {
            logger.info("Trying to re-use stored directurl: " + storedDirecturl);
            dllink = storedDirecturl;
            storeDirecturl = false;
        } else {
            /* TYPE_EMBED and TYPE_API_INDIRECT */
            requestFileInformation(link);
            dllink = br.getRegex("<source src\\s*=\\s*\"(https?://[^\"]+)\" type=\"video/mp4\">").getMatch(0);
            if (dllink == null) {
                // TYPE_API_INDIRECT
                dllink = br.getRegex("<a href=\"(https?://[^\"]+)\"[^>]*>\\s*Download Video").getMatch(0);
            }
            if (dllink == null) {
                /* New website version */
                final Browser brc = br.cloneBrowser();
                brc.getPage("/api/sign?v=" + this.getFID(link));
                final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
                if (Boolean.FALSE.equals(entries.get("success"))) {
                    throw new PluginException(LinkStatus.ERROR_FATAL, entries.get("error").toString());
                }
                dllink = entries.get("url").toString();
                final String filename = (String) entries.get("filename");
                if (!StringUtils.isEmpty(filename)) {
                    link.setFinalFileName(filename);
                }
            }
            if (dllink == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find final downloadurl");
            }
            storeDirecturl = true;
        }
        /* Important otherwise we can't re-use direct urls (hotlinking-block)! */
        br.getHeaders().put("Referer", contenturl);
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
            handleConnectionErrors(br, dl.getConnection());
        } catch (final Exception e) {
            if (storedDirecturl != null) {
                link.removeProperty(directlinkproperty);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Stored directurl expired", e);
            } else {
                throw e;
            }
        }
        if (storeDirecturl) {
            link.setProperty(directlinkproperty, dl.getConnection().getURL().toExternalForm());
        }
        dl.startDownload();
    }

    @Override
    protected void handleConnectionErrors(final Browser br, final URLConnectionAdapter con) throws PluginException, IOException {
        if (!this.looksLikeDownloadableContent(con)) {
            br.followConnection(true);
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            throwConnectionExceptions(br, con);
            throwFinalConnectionException(br, con);
        }
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        return false;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }
}