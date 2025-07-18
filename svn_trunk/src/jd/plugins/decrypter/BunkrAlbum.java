//jDownloader - Downloadmanager
//Copyright (C) 2009  JD-Team support@jdownloader.org
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
package jd.plugins.decrypter;

import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.html.HTMLParser;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.PluginForHost;
import jd.plugins.hoster.Bunkr;

import org.appwork.storage.TypeRef;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.net.URLHelper;
import org.jdownloader.controlling.filter.CompiledFiletypeFilter;
import org.jdownloader.controlling.filter.CompiledFiletypeFilter.CompiledFiletypeExtension;
import org.jdownloader.plugins.controller.host.HostPluginController;
import org.jdownloader.plugins.controller.host.LazyHostPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

@DecrypterPlugin(revision = "$Revision: 51164 $", interfaceVersion = 3, names = {}, urls = {})
public class BunkrAlbum extends PluginForDecrypt {
    public BunkrAlbum(PluginWrapper wrapper) {
        super(wrapper);
    }

    /** DEVELOPER: When changing this, do not forget to update the domain also in BunkrConfig.class!!! */
    public final static String MAIN_BUNKR_DOMAIN = "bunkr.si";

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { MAIN_BUNKR_DOMAIN, "bunkrr.su", "bunkr.su", "bunkr.si", "bunkr.ru", "bunkr.is", "bunkr.la", "bunkrr.ru", "bunkr.sk", "bunkr.black", "bunkr.cat", "bunkr.media", "bunkr.ac", "bunkr.ws", "bunkr.red", "bunkr.site", "bunkr.black", "bunkrrr.org", "bunkr.fi", "bunkr.ci", "bunkr.ax", "bunkr.ac", "bunkr.se", "bunkr.es", "bunkr.ps", "bunkr.pk", "bunkr.ph", "bunkr.cr" });
        return ret;
    }

    /**
     * These domains are dead and can't be used for main URLs/albums BUT some of them can still be used for downloading inside directurls.
     * </br> 2023-08-08: Example still working as CDN domain: bunkr.ru, bunkr.is
     */
    public static List<String> getDeadDomains() {
        return Arrays.asList(new String[] { "bunkr.su", "bunkr.ru", "bunkr.is", "bunkr.la", "bunkr.se" });
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

    private static final String EXTENSIONS = "(?i)(?:mp4|m4v|mp3|mov|jpe?g|zip|rar|png|gif|ts|webm|[a-z0-9]{3,4})";

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            String regex = "https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/a/[A-Za-z0-9]+";
            regex += "|https?://(\\w+\\.)?" + buildHostsPatternPart(domains) + "/(?:d|i|v|f)/[^/#\\?\"\\']+\\." + EXTENSIONS;
            regex += "|https?://(\\w+\\.)?" + buildHostsPatternPart(domains) + "/(?:d|i|v|f)/[^/#\\?\"\\']+"; // Same but wider
            regex += "|https?://c(?:dn)?(\\d+)?\\." + buildHostsPatternPart(domains) + "/[^/#\\?\"\\']+";// TYPE_CDN
            regex += "|https?://media-files\\d*\\." + buildHostsPatternPart(domains) + "/[^/#\\?\"\\']+";// TYPE_MEDIA_FILES
            ret.add(regex);
        }
        return ret.toArray(new String[0]);
    }

    public static final Pattern PATTERN_ALBUM                = Pattern.compile("/a/([A-Za-z0-9]+)", Pattern.CASE_INSENSITIVE);
    /* 2023-03-24: bunkr, files subdomain seems outdated? */
    public static final Pattern PATTERN_SINGLE_FILE          = Pattern.compile("/(d|i|v|f)/([^/#\\?\"\\']+).*", Pattern.CASE_INSENSITIVE);
    public static final Pattern PATTERN_CDN_WITHOUT_EXT      = Pattern.compile("https?://c(?:dn)?(\\d+)?\\.[^/]+/([^/#\\?\"\\']+)", Pattern.CASE_INSENSITIVE);
    public static final String  TYPE_CDN_WITH_EXT            = PATTERN_CDN_WITHOUT_EXT + "(\\." + EXTENSIONS + ")";
    /** 2025-05-16: psp: I believe that the "media-files..." URLs do not exist anymore. */
    @Deprecated
    public static final String  TYPE_MEDIA_FILES_WITHOUT_EXT = "(?i)https?://media-files(\\d*)\\.[^/]+/[^/#\\?\"\\']+";
    @Deprecated
    public static final String  TYPE_MEDIA_FILES_WITH_EXT    = TYPE_MEDIA_FILES_WITHOUT_EXT + "(\\." + EXTENSIONS + ")";
    private PluginForHost       plugin                       = null;

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        final String singleFileURL = isSingleMediaURL(contenturl);
        if (singleFileURL != null) {
            /* Direct downloadable URL. */
            add(ret, null, contenturl, null, null, null, null);
        } else if (new Regex(contenturl, PATTERN_ALBUM).patternFind()) {
            /* Most likely we have an album or similar: One URL which leads to more URLs. */
            String contentURL = param.getCryptedUrl();
            final String hostFromAddedURLWithoutSubdomain = Browser.getHost(contentURL, false);
            final List<String> deadDomains = getDeadDomains();
            if (deadDomains != null && deadDomains.contains(hostFromAddedURLWithoutSubdomain)) {
                contentURL = param.getCryptedUrl().replaceFirst(Pattern.quote(hostFromAddedURLWithoutSubdomain) + "/", getHost() + "/");
                logger.info("Corrected domain in added URL: " + hostFromAddedURLWithoutSubdomain + " --> " + getHost());
            }
            br.setFollowRedirects(true);
            br.getPage(contentURL);
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final HashSet<String> dups = new HashSet<String>();
            /* Double-check for offline / empty album. */
            final String albumID = new Regex(param.getCryptedUrl(), PATTERN_ALBUM).getMatch(0);
            int numberofFiles = -1;
            final String numberofFilesStr = br.getRegex(">\\s*(\\d+)\\s*files").getMatch(0);
            if (numberofFilesStr != null) {
                numberofFiles = Integer.parseInt(numberofFilesStr);
            } else {
                logger.warning("Failed to find number of files in this album");
            }
            String albumDescription = br.getRegex("<p class=\"text-base text-body-color dark:text-dark-text\"[^>]*>([^<]+)</p>").getMatch(0);
            if (albumDescription != null) {
                albumDescription = Encoding.htmlDecode(albumDescription).trim();
            }
            String albumTitle = br.getRegex("<title>([^<]+)</title>").getMatch(0);
            if (albumTitle != null) {
                albumTitle = Encoding.htmlDecode(albumTitle).trim();
                albumTitle = albumTitle.replaceFirst(" \\| Bunkr.*$", "");
            } else {
                logger.warning("Failed to find album title");
            }
            String json = br.getRegex("<script\\s*id\\s*=\\s*\"__NEXT_DATA__\"\\s*type\\s*=\\s*\"application/json\">\\s*(\\{.*?\\})\\s*</script").getMatch(0);
            if (json != null) {
                final Map<String, Object> map = restoreFromString(json, TypeRef.MAP);
                List<Map<String, Object>> files = (List<Map<String, Object>>) JavaScriptEngineFactory.walkJson(map, "props/pageProps/files");
                if (files == null) {
                    files = (List<Map<String, Object>>) JavaScriptEngineFactory.walkJson(map, "props/pageProps/album/files");
                }
                if (files == null) {
                    files = new ArrayList<Map<String, Object>>();
                }
                Map<String, Object> pagePropsFile = (Map<String, Object>) JavaScriptEngineFactory.walkJson(map, "props/pageProps/file");
                if (pagePropsFile == null) {
                    pagePropsFile = (Map<String, Object>) JavaScriptEngineFactory.walkJson(map, "props/pageProps/album/file");
                }
                if (pagePropsFile != null) {
                    files.add(pagePropsFile);
                }
                if (files != null) {
                    for (final Map<String, Object> file : files) {
                        final String name = (String) file.get("name");
                        String cdn = (String) file.get("cdn");
                        if (cdn == null) {
                            cdn = (String) file.get("mediafiles");
                        }
                        final String size = StringUtils.valueOfOrNull(file.get("size"));
                        if (name != null && cdn != null) {
                            final String directurl = URLHelper.parseLocation(new URL(cdn), name);
                            add(ret, dups, directurl, name, size != null && size.matches("[0-9]+") ? size : null, size != null && !size.matches("[0-9]+") ? size : null, true);
                        }
                    }
                }
            }
            final String[] htmls = br.getRegex("<div class=\"grid-images_box(?: rounded-lg)?[^\"]+\"(.*?)</div>\\s+</div>").getColumn(0);
            for (final String html : htmls) {
                String directurl = new Regex(html, "href\\s*=\\s*\"(https?://[^\"]+)\"").getMatch(0);
                if (directurl == null) {
                    final String[] urls = HTMLParser.getHttpLinks(html, br.getURL());
                    for (final String url : urls) {
                        if (new Regex(url, BunkrAlbum.PATTERN_SINGLE_FILE).patternFind()) {
                            directurl = url;
                            break;
                        }
                    }
                }
                if (directurl != null) {
                    String filesizeStr = new Regex(html, "<p class=\"mt-0 dark:text-white-900\"[^>]*>\\s*([^<]*?)\\s*</p>").getMatch(0);
                    if (filesizeStr == null) {
                        filesizeStr = new Regex(html, "<p class=\"[^\"]*theSize[^\"]*\"[^>]*>\\s*([^<]*?)\\s*</p>").getMatch(0);
                    }
                    String filename = new Regex(html, "<div\\s*class\\s*=\\s*\"[^\"]*details\"\\s*>\\s*<p\\s*class[^>]*>\\s*(.*?)\\s*<").getMatch(0);
                    if (filename == null) {
                        filename = new Regex(html, "<p class=\"[^\"]*theName[^\"]*\"[^>]*>\\s*([^<]*?)\\s*</p>").getMatch(0);
                    }
                    add(ret, dups, directurl, filename, null, filesizeStr, true);
                } else {
                    logger.warning("html Parser broken? HTML: " + html);
                }
            }
            if (ret.isEmpty()) {
                /* Check for empty album */
                if (numberofFiles == 0 || br.containsHTML("(?i)There are no files in the album")) {
                    throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
                } else {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
            if (numberofFiles > 0 && ret.size() != numberofFiles) {
                /* This should never happen. */
                logger.warning("Some files were not found: " + (numberofFiles - ret.size()));
            }
            final FilePackage fp = FilePackage.getInstance();
            if (albumTitle != null) {
                fp.setName(albumTitle);
            } else {
                /* Fallback */
                fp.setName(albumID);
            }
            fp.setAllowInheritance(true);
            if (!StringUtils.isEmpty(albumDescription)) {
                fp.setComment(albumDescription);
            }
            fp.addLinks(ret);
        } else {
            /* Invalid URL -> Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return ret;
    }

    private DownloadLink add(final List<DownloadLink> ret, final Set<String> dups, final String url, String filename, final String filesizeBytesStr, final String filesizeStr, final Boolean setOnlineStatus) throws Exception {
        if (dups != null && !dups.add(url)) {
            return null;
        }
        final DownloadLink dl = this.createDownloadlink(url);
        if (plugin == null) {
            try {
                final LazyHostPlugin lazyHostPlugin = HostPluginController.getInstance().get(getHost());
                if (lazyHostPlugin == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                plugin = lazyHostPlugin.getPrototype(null, false);
            } catch (final Throwable e) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, null, e);
            }
        }
        if (setOnlineStatus != null) {
            dl.setAvailable(setOnlineStatus.booleanValue());
        }
        dl.setDefaultPlugin(plugin);
        final String filenameFromURL = Bunkr.getNameFromURL(this, url);
        if (filename != null) {
            filename = Encoding.htmlDecode(filename).trim();
            final String fileExtensionNow = Plugin.getFileNameExtensionFromString(filename);
            /* Add file extension if it is missing */
            String betterFileExtension = null;
            if (filenameFromURL != null) {
                betterFileExtension = getFileNameExtensionFromString(filenameFromURL, null);
            }
            final boolean looksLikeVideoFile;
            if (betterFileExtension == null && url.matches("(?i)https?://[^/]+/v/.+")) {
                betterFileExtension = ".mp4";
                looksLikeVideoFile = true;
            } else {
                looksLikeVideoFile = false;
            }
            /* Only correct file-type if none is given or type is substantially different (e.g. old is type audio, new is video). */
            final CompiledFiletypeExtension cfe_old = CompiledFiletypeFilter.getExtensionsFilterInterface(fileExtensionNow);
            final CompiledFiletypeExtension cfe_new = CompiledFiletypeFilter.getExtensionsFilterInterface(betterFileExtension);
            if (betterFileExtension != null && looksLikeVideoFile && (cfe_old == null || !cfe_old.isSameExtensionGroup(cfe_new))) {
                filename = this.correctOrApplyFileNameExtension(filename, betterFileExtension, null);
            }
            dl.setProperty(Bunkr.PROPERTY_FILENAME_FROM_ALBUM, filename);
            Bunkr.setFilename(dl, filename, false, false);
        } else if (filenameFromURL != null) {
            /* Fallback */
            dl.setName(filenameFromURL);
        }
        long parsedFilesize = -1;
        if (filesizeBytesStr != null) {
            parsedFilesize = Long.parseLong(filesizeBytesStr);
            dl.setVerifiedFileSize(parsedFilesize);
        } else if (filesizeStr != null) {
            parsedFilesize = SizeFormatter.getSize(filesizeStr);
            dl.setDownloadSize(parsedFilesize);
        }
        if (parsedFilesize != -1) {
            dl.setProperty(Bunkr.PROPERTY_PARSED_FILESIZE, parsedFilesize);
        }
        ret.add(dl);
        return dl;
    }

    /**
     * Returns URL if given URL looks like it is pointing to a single file. </br> Returns null if given URL-structure is unknown or does not
     * seem to point to a single file.
     */
    private String isSingleMediaURL(final String url) {
        if (url == null) {
            return null;
        } else if (new Regex(url, PATTERN_SINGLE_FILE).patternFind()) {
            return url;
        } else if (new Regex(url, PATTERN_CDN_WITHOUT_EXT).patternFind() || url.matches(TYPE_CDN_WITH_EXT)) {
            /* cdn can be empty(!) -> cdn.bunkr.is -> media-files.bunkr.is */
            return url;
        } else if (url.matches(TYPE_MEDIA_FILES_WITHOUT_EXT) || url.matches(TYPE_MEDIA_FILES_WITH_EXT)) {
            // bunkr
            return url;
        } else {
            /* Unknown URL */
            return null;
        }
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        return 1;
    }
}
