//jDownloader - Downloadmanager
//Copyright (C) 2010  JD-Team support@jdownloader.org
//
//This program is free software: you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation, either version 3 of the License, or
//(at your option) any later version.
//
//This program is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//GNU General Public License for more details.
//
//You should have received a copy of the GNU General Public License
//along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.hoster;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.plugins.components.config.GfycatConfig;
import org.jdownloader.plugins.components.config.GfycatConfig.PreferredFormat;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.http.requests.GetRequest;
import jd.http.requests.PostRequest;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.PluginJSonUtils;

@HostPlugin(revision = "$Revision: 51696 $", interfaceVersion = 2, names = {}, urls = {})
public class GfyCatCom extends PluginForHost {
    public GfyCatCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.IMAGE_HOST, LazyPlugin.FEATURE.XXX };
    }

    private final boolean websiteOffline = true; // 2024-01-23 gfycat.com offline

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "gfycat.com" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(?:ifr/)?([A-Za-z0-9]+)");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getAGBLink() {
        return "https://gfycat.com/terms";
    }

    @Override
    public void setBrowser(final Browser br) {
        this.br = br;
        this.br.getHeaders().put("User-Agent", "JDownloader");
    }

    protected String getContentURL(final DownloadLink link) {
        final String fid = this.getFID(link);
        final String url = link.getPluginPatternMatcher().replace("http://", "https://");
        if (Browser.getHost(url).equalsIgnoreCase("gifdeliverynetwork.com") && fid != null) {
            /*
             * 2020-06-18: Special: gfycat.com would redirect to gifdeliverynetwork.con in this case but redgifs.com will work fine and
             * return the expected json!
             */
            // TODO: Refactor plugin so that such kind of URLs will be handled by the plugin "RedGifsCom".
            return "https://www.redgifs.com/watch/" + fid;
        } else {
            return url;
        }
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

    public static final String PROPERTY_API_ITEM_ID = "api_item_id";

    protected String getFID(final DownloadLink link) {
        final String apiItemID = link.getStringProperty(PROPERTY_API_ITEM_ID);
        if (apiItemID != null) {
            return apiItemID;
        }
        final String idFromLegacyURL = new Regex(link.getPluginPatternMatcher(), "https?://[^/]+/watch/([A-Za-z0-9]+)").getMatch(0);
        if (idFromLegacyURL != null) {
            /*
             * For old redgifs.com URLs which were added before separate redgifs.com plugin was added (last rev with old handling: 47215).
             */
            return idFromLegacyURL;
        } else {
            final String ret = new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
            if (ret != null) {
                // ID must be lower case!
                return ret.toLowerCase(Locale.ENGLISH);
            } else {
                return null;
            }
        }
    }

    @Override
    public String getMirrorID(DownloadLink link) {
        String fid = null;
        if (link != null && StringUtils.equals(getHost(), link.getHost()) && (fid = getFID(link)) != null) {
            return getHost() + "://" + fid;
        } else {
            return super.getMirrorID(link);
        }
    }

    /*
     * Using API: http://gfycat.com/api 2020-06-18: Not using the API - wtf does this comment mean?? Maybe website uses the same json as API
     * ... but API needs authorization!
     */
    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return requestFileInformation(link, false);
    }

    private static AtomicReference<String> redgifsAccessKey             = new AtomicReference<String>(null);
    private static AtomicReference<String> redgifsAccessToken           = new AtomicReference<String>(null);
    private static AtomicLong              redgifsAccessTokenValidUntil = new AtomicLong(-1);
    public static final String             PROPERTY_DIRECTURL_SD        = "directurl_sd";
    public static final String             PROPERTY_DIRECTURL_HD        = "directurl_hd";

    private String[] getDownloadURL(final DownloadLink link, final Map<String, Object> video, final Map<String, Object> photo, final PreferredFormat format) throws Exception {
        // TODO: use JSON in complicatedJSON, it contains all available formats/qualities
        switch (format) {
        case WEBM:
            if (video == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            } else {
                String ret = (String) video.get("contentUrl");
                if (!StringUtils.isEmpty(ret)) {
                    final String verifyWebM = ret.replace(".mp4", ".webm");
                    if (verifyDownloadURL(link, verifyWebM)) {
                        ret = verifyWebM;
                    } else {
                        return getDownloadURL(link, video, photo, PreferredFormat.MP4);
                    }
                }
                return new String[] { format.name(), ret, ".webm" };
            }
        case GIF:
            if (photo == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            } else {
                final String ret = (String) photo.get("contentUrl");
                return new String[] { format.name(), ret, ".gif" };
            }
        case MP4:
        default:
            if (video == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            } else {
                /* MP4 */
                String ret = (String) video.get("contentUrl");
                if (!StringUtils.isEmpty(ret)) {
                    if (!verifyDownloadURL(link, ret) && !StringUtils.endsWithCaseInsensitive(ret, "mobile.mp4")) {
                        final String mobile = ret.replaceFirst("\\.mp4$", "-mobile.mp4");
                        if (verifyDownloadURL(link, mobile)) {
                            ret = mobile;
                        }
                    }
                }
                return new String[] { format.name(), ret, ".mp4" };
            }
        }
    }

    public AvailableStatus requestFileInformation(final DownloadLink link, final boolean isDownload) throws Exception {
        if (websiteOffline) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String fid = this.getFID(link);
        if (fid == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (!link.isNameSet()) {
            link.setName(fid);
        }
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        br.setAllowedResponseCodes(new int[] { 410, 500 });
        final String contentURL = this.getContentURL(link);
        br.getPage(contentURL);
        // gfycat/gifdeliverynetwork may redirect to redgifs
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getHttpConnection().getResponseCode() == 410) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getHttpConnection().getResponseCode() == 500) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String dllink = null;
        if (Browser.getHost(br.getHost()).equals("redgifs.com")) {
            /* 2021-11-29: New endpoint available and old one can be used without key/token: https://api.redgifs.com/v2/gifs/ */
            final boolean accessKeyRequired = false;
            String token = null;
            if (accessKeyRequired) {
                String key = null;
                synchronized (redgifsAccessKey) {
                    key = redgifsAccessKey.get();
                    if (StringUtils.isEmpty(key)) {
                        br.getPage(contentURL);
                        /* 2021-05-04: /assets/app.59be79d0c1811e38f695.js */
                        final String jsurl = br.getRegex("<script src=\"(/assets/app\\.[a-f0-9]+\\.js)\">").getMatch(0);
                        if (jsurl == null) {
                            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                        }
                        final Browser brc = br.cloneBrowser();
                        brc.getPage(jsurl);
                        key = brc.getRegex("webloginAccessKey=\"([^\"]+)\"").getMatch(0);
                        if (StringUtils.isEmpty(key)) {
                            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                        } else {
                            redgifsAccessKey.set(key);
                        }
                    }
                }
                synchronized (redgifsAccessToken) {
                    token = redgifsAccessToken.get();
                    if (StringUtils.isEmpty(token) || redgifsAccessTokenValidUntil.get() < System.currentTimeMillis()) {
                        if (redgifsAccessToken == null) {
                            logger.info("Creating token for the first time");
                        } else {
                            logger.info("Creating new token because the old one has expired");
                        }
                        final Browser brc = br.cloneBrowser();
                        // brc.getHeaders().put("Accept", "*/*");
                        // brc.getHeaders().put("Cache-Control", null);
                        // brc.getHeaders().put("TE", "Trailers");
                        // brc.getHeaders().put("Connection", "keep-alive");
                        brc.getHeaders().put("Origin", "https://www.redgifs.com");
                        brc.getHeaders().put("Referer", "https://www.redgifs.com/");
                        PostRequest request = brc.createJSonPostRequest("https://api.redgifs.com/v1/oauth/webtoken", "{\"access_key\":\"" + key + "\"}");
                        request.setContentType("application/json");// is important, default content-type with charset will fail in bad
                        // request
                        brc.getPage(request);
                        final Map<String, Object> entries = restoreFromString(brc.toString(), TypeRef.MAP);
                        token = (String) entries.get("access_token");
                        if (StringUtils.isEmpty(token)) {
                            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                        } else {
                            redgifsAccessToken.set(token);
                            redgifsAccessTokenValidUntil.set(System.currentTimeMillis() + ((Number) entries.get("expires_in")).longValue() * 1000l);
                        }
                    }
                }
            }
            final Browser brapi = br.cloneBrowser();
            brapi.setAllowedResponseCodes(410);
            /*
             * 2022-12-27 - this api no longer returns sound or higher quality! This API is going away, please upgrade your app:
             * https://github.com/Redgifs/api/wiki
             */
            final GetRequest request = brapi.createGetRequest("https://api.redgifs.com/v1/gfycats/" + fid);
            request.getHeaders().put("Origin", "https://redgifs.com/");
            if (token != null) {
                request.getHeaders().put("Referer", "https://redgifs.com/");
                request.getHeaders().put("Authorization", "Bearer " + token);
            }
            brapi.getPage(request);
            if (brapi.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (brapi.getHttpConnection().getResponseCode() == 410) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            Map<String, Object> entries = restoreFromString(brapi.toString(), TypeRef.MAP);
            entries = (Map<String, Object>) entries.get("gfyItem");
            final Map<String, Object> sources = (Map<String, Object>) entries.get("content_urls");
            String url = null;
            String ext = null;
            Number size = null;
            boolean trustSize = false;
            Map<String, Object> selectedSource = null;
            switch (getPreferredFormat(link)) {
            case GIF:
                selectedSource = (Map<String, Object>) sources.get("largeGif");
                if (selectedSource != null) {
                    url = (String) selectedSource.get("url");
                    size = (Number) selectedSource.get("size");
                }
                if (url == null) {
                    url = (String) selectedSource.get("gifUrl");
                }
                if (url != null) {
                    ext = ".gif";
                    break;
                }
                // fallthrough to next best quality
            case WEBM:
                selectedSource = (Map<String, Object>) sources.get("webm");
                if (selectedSource != null) {
                    url = (String) selectedSource.get("url");
                    size = (Number) selectedSource.get("size");
                    if (url != null) {
                        ext = ".webm";
                        break;
                    }
                }
                // fallthrough to next best quality
            case MP4: // MP4 == default
            default: {
                String selectedSourceID = null;
                for (final String sourceID : new String[] { "mp4", "mobile", "silent" }) {
                    final Map<String, Object> currentSource = (Map<String, Object>) sources.get("mp4");
                    if (currentSource != null) {
                        final String sourceURL = (String) currentSource.get("url");
                        if (sourceURL != null) {
                            if (selectedSourceID == null || sourceURL.equals(selectedSource.get("url"))) {
                                if (selectedSource != null) {
                                    logger.info(selectedSourceID + " and " + sourceID + " source share the same url -> use " + sourceID + " source!");
                                }
                                selectedSourceID = sourceID;
                                selectedSource = currentSource;
                            }
                        }
                    }
                }
                if (selectedSource != null) {
                    trustSize = selectedSource != null;
                    url = (String) selectedSource.get("url");
                    size = (Number) selectedSource.get("size");
                }
                if (url == null) {
                    url = (String) entries.get("mp4Url");
                    if (url == null) {
                        url = (String) entries.get("mobileUrl");
                    }
                }
                if (url != null) {
                    ext = ".mp4";
                    break;
                }
            }
            }
            if (selectedSource == null) {
                /* 2022-02-15: New: Maybe single image (not animated) */
                final String[] imageSources = new String[] { "large", "medium", "small" };
                for (final String imageSource : imageSources) {
                    selectedSource = (Map<String, Object>) sources.get(imageSource);
                    if (selectedSource != null) {
                        break;
                    }
                }
                if (selectedSource != null) {
                    url = (String) selectedSource.get("url");
                    ext = Plugin.getFileNameExtensionFromURL(url);
                }
            }
            final String gfyName = (String) entries.get("gfyName");
            final String username = (String) entries.get("userName");
            String filename = username;
            final Number createDate = (Number) entries.get("createDate");
            if (createDate != null) {
                final SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
                filename = sdf.format(new Date(createDate.longValue() * 1000)) + "_" + filename;
            }
            /* fid is used as fallback-title so in this case we don't want to have it twice in our filename! */
            if (gfyName != null) {
                if (!StringUtils.equalsIgnoreCase(gfyName, fid)) {
                    filename += " - " + fid;
                }
                filename += " - " + gfyName + ext;
            }
            if (url != null) {
                if (link.getFinalFileName() == null || (link.getFinalFileName() != null && !StringUtils.endsWithCaseInsensitive(link.getFinalFileName(), ext))) {
                    filename = filename + ext;
                    filename = filename.replaceFirst("(?i)((\\.(webm|mp4|gif)))?" + Pattern.quote(ext) + "$", ext);
                    link.setFinalFileName(filename);
                }
            } else {
                link.setName(filename);
            }
            if (size != null) {
                if (trustSize && DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                    link.setVerifiedFileSize(size.longValue());
                } else {
                    link.setDownloadSize(size.longValue());
                }
            }
            if (link.getComment() == null) {
                final List<Object> tags = (List<Object>) entries.get("tags");
                if (tags != null) {
                    final String description = StringUtils.join(tags.toArray(new Object[0]), " ") + " Porn GIF by " + username;
                    link.setComment(description);
                }
            }
            dllink = url;
        } else {
            if (br.getHost().equalsIgnoreCase("gifdeliverynetwork.com")) {
                /* 2020-06-18: New and should not be needed! */
                dllink = br.getRegex("\"(https?://[^<>\"]+\\.webm)\"").getMatch(0);
                if (dllink == null) {
                    dllink = br.getRegex("\"(https?://[^<>\"]+\\.mp4)\"").getMatch(0);
                }
                if (dllink == null || dllink.contains(".webm")) {
                    link.setName(fid + ".webm");
                } else {
                    link.setName(fid + ".mp4");
                }
            } else {
                final String simpleJSON = br.getRegex("<script data-react-helmet\\s*=\\s*\"true\"\\s*type\\s*=\\s*\"application/ld\\+json\">\\s*(.*?)\\s*</script>").getMatch(0);
                final String complicatedJSON = br.getRegex("___INITIAL_STATE__\\s*=\\s*(\\{.*?)\\s*</script").getMatch(0);
                if (simpleJSON != null) {
                    final Map<String, Object> entries = JavaScriptEngineFactory.jsonToJavaMap(simpleJSON);
                    final String datePublished = (String) entries.get("datePublished");
                    final String description = (String) entries.get("description");
                    final Map<String, Object> photo = (Map<String, Object>) entries.get("image");
                    final Map<String, Object> video = (Map<String, Object>) entries.get("video");
                    if (!StringUtils.isEmpty(description) && link.getComment() == null) {
                        link.setComment(description);
                    }
                    final String username = (String) entries.get("author");
                    String title = null;
                    if (DebugMode.TRUE_IN_IDE_ELSE_FALSE && !true) {
                        /* Alternative ways to find title */
                        if (complicatedJSON != null) {
                            try {
                                final Object rootO = JavaScriptEngineFactory.jsonToJavaObject(complicatedJSON);
                                // final List<Object> ressourcelist = restoreFromString(complicatedJSON, TypeRef.LIST);
                                final Map<String, Object> allMedia = (Map<String, Object>) JavaScriptEngineFactory.walkJson(rootO, "{0}/cache/gifs");
                                final Map<String, Object> thisMediaInfo = (Map<String, Object>) allMedia.get(fid);
                                title = (String) thisMediaInfo.get("title");
                            } catch (final Throwable e) {
                            }
                        }
                        if (StringUtils.isEmpty(title)) {
                            title = br.getRegex("<h1 class\\s*=\\s*\"title\">\\s*([^<>\"]+)\\s*</h1>").getMatch(0);
                        }
                    }
                    title = (String) entries.get("headline");
                    if (!StringUtils.isEmpty(title)) {
                        /* 2020-11-26: Remove stuff we don't want! */
                        title = title.replaceFirst("(\\s*Porn\\s*GIF\\s*(by.+)?)", "");
                    }
                    /* 2021-03-09: Fallback - title can be "" (empty) [after title-correction]! */
                    title = fid;
                    if (!StringUtils.isAllNotEmpty(datePublished, username, title)) {
                        /* Most likely content is not downloadable e.g. gyfcat.com/upload */
                        throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                    }
                    String dateFormatted = new Regex(datePublished, "(\\d{4}-\\d{2}-\\d{2})").getMatch(0);
                    if (dateFormatted == null) {
                        /* Fallback */
                        dateFormatted = datePublished;
                    }
                    final String downloadURL[] = getDownloadURL(link, video, photo, getPreferredFormat(link));
                    dllink = downloadURL[1];
                    final String ext = downloadURL[2];
                    if (link.getFinalFileName() == null) {
                        /*
                         * 2020-11-26: Include fid AND title inside filenames because different URLs can have the same title and can be
                         * published on the same date (very rare case).
                         */
                        String filename = dateFormatted + "_" + username;
                        /* fid is used as fallback-title so in this case we don't want to have it twice in our filename! */
                        if (!StringUtils.equalsIgnoreCase(title, fid)) {
                            filename += " - " + fid;
                        }
                        filename += " - " + title + ext;
                        link.setFinalFileName(filename);
                    }
                } else {
                    /* Old handling */
                    // final Map<String, Object> entries = (Map<String, Object>)
                    // JavaScriptEngineFactory.jsonToJavaMap(json);
                    if (StringUtils.isEmpty(complicatedJSON) || br.getHttpConnection().getResponseCode() == 404) {
                        throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                    }
                    final String username = PluginJSonUtils.getJsonValue(complicatedJSON, "author");
                    final String filesize = PluginJSonUtils.getJsonValue(complicatedJSON, "webmSize");
                    if (StringUtils.isEmpty(username)) {
                        /* Most likely content is not downloadable e.g. gyfcat.com/upload */
                        throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                    }
                    link.setFinalFileName(username + " - " + fid + ".webm");
                    if (!StringUtils.isEmpty(filesize)) {
                        link.setDownloadSize(SizeFormatter.getSize(filesize));
                    }
                    dllink = PluginJSonUtils.getJsonValue(complicatedJSON, "webmUrl");
                }
            }
            if (!StringUtils.isEmpty(dllink) && !isDownload) {
                if (!verifyDownloadURL(link, dllink)) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
            }
        }
        link.setProperty(PROPERTY_DIRECTURL_HD, dllink);
        return AvailableStatus.TRUE;
    }

    protected boolean verifyDownloadURL(final DownloadLink link, final String directurl) throws IOException {
        if (StringUtils.isEmpty(directurl)) {
            return false;
        }
        URLConnectionAdapter con = null;
        try {
            final Browser brc = br.cloneBrowser();
            brc.setFollowRedirects(true);
            con = brc.openHeadConnection(directurl);
            if (con.getResponseCode() == 404) {
                return false;
            } else if (looksLikeDownloadableContent(con)) {
                if (con.getCompleteContentLength() > 0) {
                    link.setVerifiedFileSize(con.getCompleteContentLength());
                }
                return true;
            } else {
                return false;
            }
        } finally {
            try {
                con.disconnect();
            } catch (final Throwable e) {
            }
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link, true);
        final Set<String> directurls = new LinkedHashSet<String>();
        directurls.add(link.getStringProperty(PROPERTY_DIRECTURL_HD));
        directurls.add(link.getStringProperty(PROPERTY_DIRECTURL_SD));
        directurls.remove(null);
        if (directurls.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        int counter = 0;
        for (final String directurl : directurls) {
            counter++;
            br.getHeaders().put("Referer", "https://" + getHost() + "/");
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, directurl, true, 1);
            final boolean success = this.looksLikeDownloadableContent(dl.getConnection());
            logger.info("Directurl " + counter + "/" + directurls.size() + ": success = " + success + " | URL: " + directurl);
            if (success) {
                break;
            } else {
                logger.info("Skipping unusable directurl: " + directurl);
                continue;
            }
        }
        if (!looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            if (dl.getConnection().getResponseCode() == 403) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 60 * 60 * 1000l);
            } else if (dl.getConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 60 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Unknown server error", 3 * 60 * 1000l);
            }
        }
        dl.startDownload();
    }

    private final String gfycatFormat = "gfycatFormat";

    private PreferredFormat getPreferredFormat(final DownloadLink link) {
        final String linkFormat = link.getStringProperty(gfycatFormat, null);
        if (linkFormat != null) {
            try {
                return PreferredFormat.valueOf(linkFormat);
            } catch (IllegalArgumentException e) {
                logger.exception("Invalid Format:" + linkFormat, e);
                link.removeProperty(gfycatFormat);
            }
        }
        final GfycatConfig cfg = PluginJsonConfig.get(GfycatConfig.class);
        return cfg.getPreferredFormat();
    }

    @Override
    public Class<? extends PluginConfigInterface> getConfigInterface() {
        return GfycatConfig.class;
    }

    @Override
    public void reset() {
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return -1;
    }

    @Override
    public void resetDownloadlink(final DownloadLink link) {
        link.removeProperty(gfycatFormat);
    }
}