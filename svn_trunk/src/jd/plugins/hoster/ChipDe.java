//    jDownloader - Downloadmanager
//    Copyright (C) 2009  JD-Team support@jdownloader.org
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
package jd.plugins.hoster;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.plugins.components.config.ChipDeConfig;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.PluginJSonUtils;

@HostPlugin(revision = "$Revision: 53986 $", interfaceVersion = 2, names = {}, urls = {})
public class ChipDe extends PluginForHost {
    public ChipDe(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(new String[] { "chip.de" });
        ret.add(new String[] { "chip.eu", "chip.asia" });
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
        return buildAnnotationUrls(getPluginDomains());
    }

    private static final Pattern PATTERN_FILE    = Pattern.compile("/downloads/[A-Za-z0-9_\\-]+_(\\d+)\\.html", Pattern.CASE_INSENSITIVE);
    /*
     * 2026-11-13: New URL format with UUID instead of numeric ID, example:
     * /download/android-apps/google-play-store-apk_842b45fc-4803-46d5-b95e-9981d992b517.html
     */
    private static final Pattern PATTERN_FILE_2  = Pattern.compile("/download/[a-z0-9\\-]+/[A-Za-z0-9_\\-]+_([a-f0-9\\-]+)\\.html", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_FILE_EU = Pattern.compile("/.{2}/[A-Za-z0-9_\\-]+_(\\d+)\\.html", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_VIDEO   = Pattern.compile("/[^/]+/[^/]+_(\\d+)\\.html", Pattern.CASE_INSENSITIVE);

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            if (domains[0].equals("chip.de")) {
                ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(" + PATTERN_FILE.pattern().substring(1) + "|" + PATTERN_FILE_2.pattern().substring(1) + "|" + PATTERN_VIDEO.pattern().substring(1) + ")");
            } else {
                // TODO: 2026-06-22: Check if these still exist, for me the eu domain redirects to de
                ret.add("https?://download\\." + buildHostsPatternPart(domains) + PATTERN_FILE_EU.pattern());
            }
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getAGBLink() {
        return "http://www." + getHost() + "/s_specials/c1_static_special_index_13162756.html";
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    private static final String type_chip_de_file         = "(?i)https?://(?:www\\.)?chip\\.de/(?:downloads/[^/]+_\\d+|download/[^/]+/[^/]+_[a-f0-9\\-]{36})\\.html";
    private static final String type_chip_eu_file         = "(?i)https?://(?:www\\.)?download\\.chip\\.(?:eu|asia)/.+";
    public static final String  type_chip_de_video        = "(?i)https?://(?:www\\.)?chip\\.de/video/[^/]+_(\\d+)\\.html";
    public static final String  type_chip_de_pictures     = "(?i)https?://(?:www\\.)?chip\\.de/bildergalerie/[^/]+_\\d+\\.html";
    private static final String type_chip_de_video_others = "(?i)https?://(?:[a-z0-9]+\\.)?chip\\.de/[^/]+/[^/]+_\\d+\\.html";
    private static final String host_chip_de              = "chip.de";
    private String              dllink                    = null;
    private static final String PROPERTY_DOWNLOAD_TARGET  = "download_target";

    /**
     * <b>Information for file (software)-downloads:</b> <br />
     * <b>Example URL:</b>
     * <a href="http://www.chip.de/downloads/Firefox-32-Bit_13014344.html">http://www.chip.de/downloads/Firefox-32-Bit_13014344.html</a>
     * <br />
     * <b>1.</b> Links are language dependant. Unfortunately we cannot just force a specified language as content is different for each
     * country. <br />
     * That means we have to make RegExes that can handle all languages. <br />
     * <b>2.</b> Via website "automatic download" users will get adware-infested installers - via "manual download" we will get the original
     * installers (usually without any adware). <br />
     *
     * <b>Information for video downloads:</b> <br />
     * <b>Example URL:</b>
     * <a href="http://www.chip.de/video/DSLR-fuer-die-Hosentasche-DxO-One-im-Test-Video_85225530.html">http://www.chip.de
     * /video/DSLR-fuer-die-Hosentasche-DxO-One-im-Test-Video_85225530.html</a> <br />
     * <b>Videoid or how they call it "containerIdBeitrag": 85225530</b><br />
     *
     */
    @SuppressWarnings({ "deprecation", "unchecked" })
    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        dllink = null;
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        br.setAllowedResponseCodes(410);
        boolean set_final_filename = false;
        String date = null;
        String date_formatted = null;
        String filename = null;
        /* Trustworthy original filename (if available) which can be set as final filename. */
        String filenameOriginal = null;
        String md5 = null;
        String title_URL = null;
        String description = null;
        final String contentID_URL;
        final Regex linkinfo = new Regex(link.getPluginPatternMatcher(), "(?i)/([^/]+)_(\\d+|[a-f0-9\\-]{36})\\.html$");
        title_URL = linkinfo.getMatch(0);
        contentID_URL = linkinfo.getMatch(1);
        if (link.getPluginPatternMatcher().matches(type_chip_de_file) || link.getPluginPatternMatcher().matches(type_chip_eu_file)) {
            set_final_filename = false;
            accessURL(this.br, link.getPluginPatternMatcher());
            if (link.getPluginPatternMatcher().matches(type_chip_eu_file) && !br.containsHTML("class=\"downloadnow_button")) {
                /* chip.eu url without download button --> No downloadable content --> URL is offline for us */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            String filesizeStr = null;
            final String json1 = br.getRegex("var digitalData = (\\{.*?\\});").getMatch(0);
            final String json2 = br.getRegex("var utag_data = (\\{.*?\\});").getMatch(0);
            String title = null;
            if (json1 != null) {
                /* 2021-07-22 */
                final Map<String, Object> entries = JavaScriptEngineFactory.jsonToJavaMap(json1);
                final Object downloadO = entries.get("download");
                if (downloadO == null) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
                final Map<String, Object> download = (Map<String, Object>) downloadO;
                final String productID = (String) download.get("productID");
                if (productID != null) {
                    link.setLinkID(this.getHost() + "://application/" + productID);
                }
                title = (String) download.get("name");
                /* Sometimes filesize is not given --> "n/a" */
                final String filesizeTmp = (String) download.get("fileSize");
                if (filesizeTmp.matches("\\d+(\\.\\d+)")) {
                    filesizeStr = filesizeTmp + "MB";
                }
                link.setProperty(PROPERTY_DOWNLOAD_TARGET, download.get("Target").toString());
            } else if (json2 != null) {
                /* 2023-10-18 */
                final Map<String, Object> entries = JavaScriptEngineFactory.jsonToJavaMap(json2);
                final String productID = (String) entries.get("downloadProductId");
                if (productID != null) {
                    link.setLinkID(this.getHost() + "://application/" + productID);
                }
                title = (String) entries.get("downloadName");
                /* Sometimes filesize is not given --> "n/a" */
                final String filesizeTmp = (String) entries.get("downloadFileSize");
                if (filesizeTmp.matches("\\d+(\\.\\d+)")) {
                    filesizeStr = filesizeTmp + "MB";
                }
            }
            /* Sometimes contains a comma and trash after that (wtf?) */
            final String filesizeBytesStr = br.getRegex("itemprop=\"fileSize\" content=\"(\\d+)(\\.\\d+)?\"").getMatch(0);
            final String filesizeMB = br.getRegex("\"downloadFileSize\":\"?(\\d+)").getMatch(0); // 2026-06-22
            /*
             * 2026-11-13: New website layout: The exact filesize in bytes is only contained in the compressed page context (flat array with
             * index references): <script id="vike_pageContext" type="application/json">{"islandsCompressed":"[...]"}</script> Example:
             * {"operatingSystems":519,"architecture":520,"sizeBytes":521,...} --> the value of "sizeBytes" is the index of the array
             * element which contains the filesize. The filesize is optional (index -1 or non-numeric value if not available). The same goes
             * for the original filename: {"headline":...,"originalName":515,...}
             */
            Long filesizeBytesPageContext = null;
            String originalFilename = null;
            extract_precise_info_from_json: try {
                /* Step 1: Get the page context json which contains the compressed data as a string. */
                final String pageContextJson = br.getRegex("<script id=\"vike_pageContext\" type=\"application/json\">(\\{.*?\\})</script>").getMatch(0);
                if (pageContextJson == null) {
                    break extract_precise_info_from_json;
                }
                /* Step 2: "islandsCompressed" is itself a json string containing a flat array. */
                final Map<String, Object> pageContext = restoreFromString(pageContextJson, TypeRef.MAP);
                final String islandsCompressed = (String) pageContext.get("islandsCompressed");
                if (islandsCompressed == null) {
                    break extract_precise_info_from_json;
                }
                final List<Object> compressedValues = restoreFromString(islandsCompressed, TypeRef.LIST);
                /*
                 * Step 3: Objects in this array do not contain values but indexes of the array element which holds the actual value. Find
                 * the objects containing "sizeBytes"/"originalName" and resolve their values via the given index.
                 */
                for (final Object compressedValue : compressedValues) {
                    if (!(compressedValue instanceof Map)) {
                        /* Only objects can contain the fields we are looking for. */
                        continue;
                    }
                    final Map<String, Object> compressedMap = (Map<String, Object>) compressedValue;
                    /* Filesize in bytes: Index is -1 if the information is not available. */
                    final Object sizeBytesIndex = compressedMap.get("sizeBytes");
                    if (filesizeBytesPageContext == null && sizeBytesIndex instanceof Number) {
                        final int index = ((Number) sizeBytesIndex).intValue();
                        if (index >= 0 && index < compressedValues.size() && compressedValues.get(index) instanceof Number) {
                            filesizeBytesPageContext = ((Number) compressedValues.get(index)).longValue();
                        }
                    }
                    /* Original filename of the file offered for download. */
                    final Object originalNameIndex = compressedMap.get("originalName");
                    if (originalFilename == null && originalNameIndex instanceof Number) {
                        final int index = ((Number) originalNameIndex).intValue();
                        if (index >= 0 && index < compressedValues.size() && compressedValues.get(index) instanceof String) {
                            originalFilename = (String) compressedValues.get(index);
                        }
                    }
                    if (filesizeBytesPageContext != null && originalFilename != null) {
                        /* Found everything we need. */
                        break;
                    }
                }
            } catch (final Exception e) {
                /* Optional information: Continue with the other filesize/filename sources if parsing of the page context fails. */
                logger.log(e);
                logger.warning("Failed to parse page context json");
            }
            /*
             * 2026-11-13: New website layout: The download URLs contain a tracking parameter "a017" which equals "chip" for downloads
             * hosted on chip.de servers and the (url-encoded) external target URL otherwise e.g. apkmirror.com. Such items are only offered
             * as a redirect to a third party thus cannot be downloaded via this plugin. Example:
             * https://www.chip.de/downloads/Google-Play-Store-APK_54923571.html
             */
            String downloadTarget = null;
            for (final String decodedUrl : findDecodedDownloadUrls(br.getRequest().getHtmlCode())) {
                final String downloadSource = new Regex(decodedUrl, "[&?]a017=([^&\"\\\\\\s]+)").getMatch(0);
                if (downloadSource == null) {
                    continue;
                } else if (!downloadSource.equalsIgnoreCase("chip")) {
                    /* External source: Final result. */
                    downloadTarget = downloadSource;
                    break;
                } else {
                    /* "intern" = hosted on chip.de servers, see isExternalDownload(). */
                    downloadTarget = "intern";
                }
            }
            if (downloadTarget != null) {
                link.setProperty(PROPERTY_DOWNLOAD_TARGET, downloadTarget);
            }
            if (StringUtils.isEmpty(title)) {
                title = br.getRegex("property=\"og:title\" content=\"([^<>\"]*?)\"").getMatch(0);
            }
            filesizeStr = br.getRegex("<dt>\\s*Datei-Größe:\\s*</dt>\\s*<dd>([^<]+)</dd>").getMatch(0); // 2026-08-31: New website layout
            if (filesizeStr == null) {
                filesizeStr = br.getRegex(">Dateigröße:\\s*</th>\\s*<td>([^<]+)</td>").getMatch(0); // 2026-06-22
            }
            if (filesizeStr == null) {
                filesizeStr = br.getRegex(">Dateigr\\&ouml;\\&szlig;e:</p>[\t\n\r ]+<p class=\"col2\">([^<>\"]*?)<meta itemprop=\"fileSize\"").getMatch(0);
                if (filesizeStr == null) {
                    /* For the international chip websites! */
                    filesizeStr = br.getRegex("<dt>(?:File size:|Размер файла:|Dimensioni:|Dateigröße:|Velikost:|Fájlméret:|Bestandsgrootte:|Rozmiar pliku:|Mărime fişier:|Dosya boyu:|文件大小：)<br /></dt>[\t\n\r ]+<dd>(.*?)<br /></dd>").getMatch(0);
                }
            }
            if (filesizeBytesStr != null) {
                link.setVerifiedFileSize(Long.parseLong(filesizeBytesStr));
            } else if (filesizeBytesPageContext != null && filesizeBytesPageContext.longValue() > 0) {
                link.setVerifiedFileSize(filesizeBytesPageContext);
            } else if (filesizeMB != null) {
                link.setDownloadSize(Long.parseLong(filesizeMB) * 1024 * 1024);
            } else if (filesizeStr != null) {
                filesizeStr = filesizeStr.replace("GByte", "GB");
                link.setDownloadSize(SizeFormatter.getSize(filesizeStr));
            } else {
                logger.warning("Failed to find filesize");
            }
            /* Checksum is usually only available for chip.eu downloads! */
            md5 = br.getRegex("<dt>(?:Контрольная сумма \\(MD 5\\):|Checksum:|Prüfsumme:|Kontrolní součet:|Szumma:|Suma kontrolna|Checksum|Kontrol toplamı:|校验码：)<br /></dt>[\t\n\r ]+<dd>(.*?)<br /></dd>").getMatch(0);
            date = br.getRegex("itemprop=\"datePublished\" datetime=\"(\\d{4}\\-\\d{2}\\-\\d{2}T\\d{2}:\\d{2}:\\d{2})\"").getMatch(0);
            if (date == null) {
                date = br.getRegex("\"datePublished\"\\s*:\\s*\"(\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2})\"").getMatch(0); // 2026-08-31:
                                                                                                                                 // New
                                                                                                                                 // website
                                                                                                                                 // layout
            }
            description = br.getRegex("description:\"([^\"]+)\"").getMatch(0);
            if (description == null) {
                description = br.getRegex("<p[^>]*itemprop=\"description\">(.*?)</p>").getMatch(0); // 2026-06-22
            }
            /*
             * Include linkid in this case because otherwise links could be identified as duplicates / mirrors wrongly e.g.
             *
             * http://download.chip.eu/en/Firefox_115074.html
             *
             * http://download.chip.eu/en/Firefox_106534.html
             */
            if (title == null) {
                /* Last chance! */
                filename = title_URL + "_" + contentID_URL;
            } else {
                filename = title + "_" + contentID_URL;
            }
            filenameOriginal = originalFilename;
        } else {
            /* type_chip_de_video and type_chip_de_video_others */
            /* 2021-07-23: This seems to be broken */
            if (contentID_URL != null) {
                link.setLinkID(this.getHost() + "://video/" + contentID_URL);
            }
            set_final_filename = true;
            accessURL(this.br, link.getPluginPatternMatcher());
            filename = br.getRegex("property=\"og:title\" content=\"([^<>]*?)\"").getMatch(0);
            date = br.getRegex("\"publishDateTime\":\"(\\d{4}\\-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\+\\d{2}:\\d{2})\"").getMatch(0);
            /*
             * 2021-07-23: Their website obtains this URL via a 3rd party CDN/API but they also expose their direct-video-URLs like this
             * (maybe a legacy way?) so we'll use the easy way for now.
             */
            dllink = PluginJSonUtils.getJson(this.br, "contentUrl");
            if (dllink == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            if (StringUtils.isEmpty(filename)) {
                /* Last chance! */
                filename = title_URL + "_" + contentID_URL;
            }
            filename = Encoding.htmlDecode(filename).trim();
            filename += ".mp4";
            br.setFollowRedirects(true);
            URLConnectionAdapter con = null;
            try {
                con = br.openHeadConnection(dllink);
                if (this.looksLikeDownloadableContent(con)) {
                    if (con.getCompleteContentLength() > 0) {
                        link.setVerifiedFileSize(con.getCompleteContentLength());
                    }
                } else {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
            } finally {
                try {
                    con.disconnect();
                } catch (final Throwable e) {
                }
            }
        }
        date_formatted = formatDate(date);
        if (date_formatted != null) {
            filename = date_formatted + "_" + filename;
        }
        if (!link.getDownloadURL().contains(host_chip_de + "/")) {
            /* Basically a warning for chip.eu downloads as there is no way around their adware installers! */
            filename = "THIS_FILE_CONTAINS_ADWARE_" + filename + ".exe";
        }
        /* Mark externally hosted items which cannot be downloaded via this plugin. */
        final String prefixExternal = this.isExternalDownload(link) ? "EXTERNAL_NOT_DOWNLOADABLE_" : "";
        filename = prefixExternal + Encoding.htmlDecode(filename).trim();
        if (set_final_filename) {
            link.setFinalFileName(filename);
        } else {
            link.setName(filename);
        }
        if (!StringUtils.isEmpty(filenameOriginal) && link.getDownloadURL().contains(host_chip_de + "/")) {
            /* Not for chip.eu: Those are adware installers which do not match the original filename. */
            link.setFinalFileName(prefixExternal + Encoding.htmlDecode(filenameOriginal).trim());
        }
        /*
         * Do not allow chip.eu hashes because at the moment we can only download their adware installers so even though they give us the
         * hash, we can never check it against the original file so it makes no sense to set it here.
         */
        if (md5 != null && link.getDownloadURL().contains(host_chip_de + "/")) {
            link.setMD5Hash(md5);
        }
        if (description != null && StringUtils.isEmpty(link.getComment())) {
            description = Encoding.htmlDecode(description);
            link.setComment(description);
        }
        if (PluginJsonConfig.get(ChipDeConfig.class).isDisplayExternalDownloadsAsOffline() && this.isExternalDownload(link)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        final String directlinkproperty = "directlink";
        final String contenturl = link.getPluginPatternMatcher();
        if (contenturl.matches(type_chip_de_video)) {
            requestFileInformation(link);
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, true, 0);
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                handleServerErrors();
                br.followConnection(true);
                /* We use APIs which we can trust so retrying is okay ;) */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Unknown server error", 5 * 60 * 1000l);
            }
        } else if (contenturl.matches(type_chip_eu_file)) {
            /* Re-Use saved direct-downloadlinks */
            final boolean resume = false;
            final int maxchunks = 1;
            if (!attemptStoredDownloadurlDownload(link, directlinkproperty, resume, maxchunks)) {
                requestFileInformation(link);
                if (isExternalDownload(link)) {
                    errorExternalDownloadImpossible();
                }
                String getfileUrl = br.getRegex("\"(/.{2}/download_getfile_[^<>\"]*?)\"").getMatch(0);
                if (getfileUrl == null) {
                    getfileUrl = br.getRegex("href=\"([^<>\"]*?)\" rel=\"nofollow\" class=\"dwnld\"").getMatch(0);
                }
                if (getfileUrl == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                if (getfileUrl.startsWith("http") && !getfileUrl.contains("chip.eu/")) {
                    errorExternalDownloadImpossible();
                }
                br.getPage(getfileUrl);
                dllink = br.getRegex("If not, please click <a href=\"(http[^<>\"]*?)\"").getMatch(0);
                if (dllink == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, false, 1);
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                handleServerErrors();
                if (!br.getHost().contains("chip")) {
                    errorExternalDownloadImpossible();
                    /* Unreachable code */
                    throw new WTFException();
                }
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final String fname = getFileNameFromConnection(dl.getConnection());
            if (fname != null) {
                link.setFinalFileName(Encoding.htmlDecode(fname));
            }
            link.setProperty(directlinkproperty, dl.getConnection().getURL().toExternalForm());
            dl.startDownload();
        } else {
            /* Normal chip.de downloads (application downloads) */
            final boolean resume = true;
            final int maxchunks = 1;
            /* Re-Use saved downloadlinks */
            if (attemptStoredDownloadurlDownload(link, directlinkproperty, resume, maxchunks)) {
                this.dl.startDownload();
                return;
            }
            requestFileInformation(link);
            if (isExternalDownload(link)) {
                /* This will throw an Exception. */
                errorExternalDownloadImpossible();
                /* Unreachable code */
                throw new WTFException();
            }
            final boolean looksLikeDownloadIsPossible = br.containsHTML("js_manual_installation") || br.containsHTML("DownloadPageWidget-DownloadButton");
            final String textNoDownloadAvailable = "No download button available";
            /*
             * 2026-08-31: New website layout. The download links point to an internal redirect URL of the following form:
             * https://x.chip.de/intern/dl/?url=<url-encoded getfile-url> </br> The "url" parameter contains the (percent-encoded) getfile
             * URL. </br> The same getfile URL exists with different "deliver" values: "deliver=web" leads to the original download while
             * "deliver=installer" leads to the Chip installer. The "deliver=web" variant is only present inside the embedded JSON (not in
             * the visible download button) and carries its own signature ("s" parameter) so we must use it as-is - modifying "deliver" of
             * another variant would invalidate that signature. </br> The inner "url" value is purely percent-encoded (no
             * quotes/backslashes) thus we can grab it directly regardless of the surrounding JSON escaping.
             */
            /*
             * 2026-09-01: Detect externally hosted downloads early (new website layout). The download button URL carries a tracking
             * parameter "a017" which equals "chip" for downloads hosted on chip.de servers and the (url-encoded) external target URL
             * otherwise, e.g. the Google Play Store / apkmirror.com. Such items are only offered as a redirect to a third party thus cannot
             * be downloaded via this plugin.
             */
            /*
             * 2026-11-13: The download button URLs are no longer present in plain text. The visible button only has
             * href="javascript:void(0)" and the real URLs are embedded as base64 strings (which themselves are percent-encoded) inside the
             * JSON of the page (attribute "data-cpx-submit"). Decode them and look for the wanted URL and the "a017" parameter in there.
             */
            final String getfilePatternStart = "url=(https%3A%2F%2F(?:www\\.)?chip\\.de%2Fdownloads%2Fgetfile%2F[^\"&\\\\]*";
            final String getfilePatternEnd = "[^\"&\\\\]*)";
            String getfileUrl = null;
            final Set<String> decodedurls = findDecodedDownloadUrls(br.getRequest().getHtmlCode());
            /*
             * Check for externally hosted items first: They also contain a "deliver=web" getfile URL so finding one does not mean that the
             * item is downloadable.
             */
            for (final String decodedurl : decodedurls) {
                final String downloadSource = new Regex(decodedurl, "[&?]a017=([^&\"\\\\\\s]+)").getMatch(0);
                if (downloadSource != null && !downloadSource.equalsIgnoreCase("chip")) {
                    /* Example: https://www.chip.de/downloads/Google-Play-Store-APK_54923571.html */
                    errorExternalDownloadImpossible();
                    /* Unreachable code */
                    throw new WTFException();
                }
            }
            for (final String decodedurl : decodedurls) {
                /* Look for the wanted getfile URL ("deliver=web") inside the decoded base64 strings. */
                getfileUrl = new Regex(decodedurl, getfilePatternStart + "deliver%3Dweb" + getfilePatternEnd).getMatch(0);
                if (getfileUrl != null) {
                    logger.info("Found getfileUrl in base64 string: " + decodedurl);
                    /* Early break */
                    break;
                }
            }
            if (getfileUrl == null) {
                logger.warning("Failed to find getfileUrl in " + decodedurls.size() + " decoded URLs");
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            /* isUrl=true only decodes the percent-sequences present in the URL and keeps the base64 "dl" parameter untouched. */
            getfileUrl = Encoding.urlDecode(getfileUrl, true);
            br.getPage(getfileUrl);
            /*
             * The getfile page embeds the final "securedl" download URL inside an HTML-entity-encoded JSON blob, e.g.
             * &quot;downloadUrl&quot;&#x3A;&quot;https&#x3A;&#x5C;&#x2F;&#x5C;&#x2F;securedl...&quot;
             */
            final String htmlDecoded = Encoding.htmlDecode(br.getRequest().getHtmlCode());
            dllink = new Regex(htmlDecoded, "\"downloadUrl\"\\s*:\\s*\"(https?:[^\"]*securedl[^\"]*)\"").getMatch(0);
            if (dllink != null) {
                /* Unescape JSON forward-slashes. */
                dllink = dllink.replace("\\/", "/");
            }
            if (dllink == null) {
                if (!looksLikeDownloadIsPossible) {
                    /* 2023-12-04: Example: https://www.chip.de/downloads/Adobe-Flash-Player_13003561.html */
                    throw new PluginException(LinkStatus.ERROR_FATAL, textNoDownloadAvailable);
                } else {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, true, 1);
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                handleServerErrors();
                if (!br.getHost().contains("chip")) {
                    /*
                     * Happens for software whos manufactors do not allow direct mirrors from chip servers e.g.
                     * http://www.chip.de/downloads/Windows-10-64-Bit_72189999.html
                     */
                    throw new PluginException(LinkStatus.ERROR_FATAL, "External download - not possible via JDownloader!");
                }
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            String etag = br.getRequest().getResponseHeader("ETag");
            if (etag != null) {
                /* chip.de servers will often return md5 file hash via headers! */
                try {
                    etag = etag.replace("\"", "");
                    final String[] etagInfo = etag.split(":");
                    final String md5 = etagInfo[0];
                    if (md5.matches("[A-Fa-f0-9]{32}")) {
                        link.setMD5Hash(md5);
                    }
                } catch (final Throwable ignore) {
                }
            }
            link.setProperty(directlinkproperty, dl.getConnection().getURL().toExternalForm());
        }
        dl.startDownload();
    }

    private boolean attemptStoredDownloadurlDownload(final DownloadLink link, final String directlinkproperty, final boolean resumable, final int maxchunks) throws Exception {
        final String url = link.getStringProperty(directlinkproperty);
        if (StringUtils.isEmpty(url)) {
            return false;
        }
        try {
            final Browser brc = br.cloneBrowser();
            dl = new jd.plugins.BrowserAdapter().openDownload(brc, link, url, resumable, maxchunks);
            if (this.looksLikeDownloadableContent(dl.getConnection())) {
                return true;
            } else {
                brc.followConnection(true);
                throw new IOException();
            }
        } catch (final Throwable e) {
            logger.log(e);
            try {
                dl.getConnection().disconnect();
            } catch (Throwable ignore) {
            }
            link.removeProperty(directlinkproperty);
            return false;
        }
    }

    @Deprecated
    public static void accesscontainerIdBeitrag(final Browser br, final String containerIdBeitrag) throws PluginException, IOException {
        accessURL(br, "https://apps-rest.chip.de/api/v2/containerIdBeitrag/" + containerIdBeitrag + "/");
    }

    /**
     * Returns all percent-decoded chip.de download redirect URLs ("https://x.chip.de/intern/dl/?url=...") which are embedded as base64
     * strings inside the html (new website layout, 2026-11-13).
     */
    private Set<String> findDecodedDownloadUrls(final String html) {
        final Set<String> ret = new HashSet<String>();
        /*
         * The base64 strings are located inside a json string which escapes forward slashes ("\/" or "\\\/"). Remove those escapes
         * otherwise base64 strings containing "/" would be cut off and decoded wrongly.
         */
        final String htmlUnescapedSlashes = html.replaceAll("\\\\+/", "/");
        final String[] base64Candidates = new Regex(htmlUnescapedSlashes, "(aHR0[A-Za-z0-9+/=_-]{40,})").getColumn(0);
        if (base64Candidates == null || base64Candidates.length == 0) {
            logger.info("Failed to find any base64 candidates");
            return ret;
        }
        /* Same base64 strings are contained multiple times in the html -> Decode each string only once. */
        final Set<String> dupes = new HashSet<String>();
        for (final String base64Candidate : base64Candidates) {
            if (!dupes.add(base64Candidate)) {
                continue;
            }
            final String decoded = Encoding.Base64Decode(base64Candidate);
            /* Ignore other base64 strings such as the ones used for ads/affiliate links. */
            if (decoded != null && decoded.contains("intern%2Fdl")) {
                /* Do NOT use isUrl=true here: It leaves "%25XX" untouched so the inner URL would stay double encoded. */
                ret.add(Encoding.urlDecode(decoded, false));
            }
        }
        return ret;
    }

    private void errorExternalDownloadImpossible() throws PluginException {
        /**
         * Happens for software whose manufacturers do not allow direct mirrors from chip servers e.g.
         * http://www.chip.de/downloads/Windows-10-64-Bit_72189999.html </br>
         * https://www.chip.de/downloads/WordPress-Android-App_54778552.html
         */
        throw new PluginException(LinkStatus.ERROR_FATAL, "External download - not possible via JDownloader!");
    }

    /** Externally hosted content is not directly downloadable via chip.de servers thus cannot be downloaded via this plugin! */
    private boolean isExternalDownload(final DownloadLink link) {
        final String dltarget = link.getStringProperty(PROPERTY_DOWNLOAD_TARGET);
        if (dltarget == null) {
            return false;
        }
        if (dltarget.equalsIgnoreCase("intern")) {
            return false;
        } else {
            return true;
        }
    }

    /* Handles general server errors. */
    private void handleServerErrors() throws PluginException {
        if (dl.getConnection().getResponseCode() == 403) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 5 * 60 * 1000l);
        } else if (dl.getConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 5 * 60 * 1000l);
        }
    }

    public static void accessURL(final Browser br, final String url) throws PluginException, IOException {
        URLConnectionAdapter con = null;
        try {
            con = br.openGetConnection(url);
            final long responsecode = con.getResponseCode();
            if (responsecode == 404 || responsecode == 410) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            br.followConnection();
        } finally {
            try {
                con.disconnect();
            } catch (Throwable e) {
            }
        }
    }

    public static final Browser prepBRAPI(final Browser br) {
        /* Needed! */
        br.setCustomCharset("UTF-8");
        /* Not necessarily needed! */
        br.getHeaders().put("Content-Type", "application/json; charset=utf-8");
        br.getHeaders().put("User-Agent", "Dalvik/2.1.0 (Linux; U; Android 5.0; Nexus 5 Build/LRX21O)");
        return br;
    }

    public static String formatDate(String input) {
        if (input == null) {
            return null;
        }
        final long date;
        if (input.matches("\\d{4}\\-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}")) {
            /* Typically for videos via API */
            date = TimeFormatter.getMilliSeconds(input, "yyyy-MM-dd'T'HH:mm:ss", Locale.GERMANY);
        } else if (input.matches("\\d{4}\\-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\+\\d{2}:\\d{2}")) {
            /* Typically for videos via website */
            /* First lets correct that date so we can easily parse it. */
            input = input.substring(0, input.lastIndexOf(":")) + "00";
            date = TimeFormatter.getMilliSeconds(input, "yyyy-MM-dd'T'HH:mm:ssZ", Locale.GERMANY);
        } else if (input.matches("\\d{4}\\-\\d{2}\\-\\d{2}\\d{2}:\\d{2}:\\d{2}")) {
            /* E.g. software/file downloads */
            date = TimeFormatter.getMilliSeconds(input, "yyyy-MM-dd'T'HH:mm:ss", Locale.GERMANY);
        } else if (input.matches("\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2}")) {
            /* 2026-08-31: E.g. software/file downloads (new website layout) */
            date = TimeFormatter.getMilliSeconds(input, "yyyy-MM-dd HH:mm:ss", Locale.GERMANY);
        } else {
            /* Unsupported input date format */
            return null;
        }
        String formattedDate = null;
        final String targetFormat = "yyyy-MM-dd";
        Date theDate = new Date(date);
        try {
            final SimpleDateFormat formatter = new SimpleDateFormat(targetFormat);
            formattedDate = formatter.format(theDate);
        } catch (Exception e) {
            /* prevent input error killing plugin */
            formattedDate = input;
        }
        return formattedDate;
    }

    @Override
    public Class<? extends PluginConfigInterface> getConfigInterface() {
        return ChipDeConfig.class;
    }

    @Override
    protected String getDefaultFileName(final DownloadLink link) {
        final Regex linkinfo = new Regex(link.getPluginPatternMatcher(), "(?i)/([^/]+)_(\\d+|[a-f0-9\\-]{36})\\.html$");
        final String title_URL = linkinfo.getMatch(0);
        final String contentID_URL = linkinfo.getMatch(1);
        if (title_URL != null && contentID_URL != null) {
            return title_URL + "_" + contentID_URL;
        }
        return super.getDefaultFileName(link);
    }
}