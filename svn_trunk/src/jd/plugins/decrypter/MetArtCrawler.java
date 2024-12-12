package jd.plugins.decrypter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.components.config.MetartConfig;
import org.jdownloader.plugins.components.config.MetartConfig.PhotoQuality;
import org.jdownloader.plugins.components.config.MetartConfig.VideoCrawlMode;
import org.jdownloader.plugins.components.config.MetartConfig.VideoQuality;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.AccountRequiredException;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.GenericM3u8;
import jd.plugins.hoster.MetArtCom;

@DecrypterPlugin(revision = "$Revision: 50326 $", interfaceVersion = 2, names = {}, urls = {})
@PluginDependencies(dependencies = { MetArtCom.class })
public class MetArtCrawler extends PluginForDecrypt {
    public MetArtCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.IMAGE_GALLERY, LazyPlugin.FEATURE.XXX };
    }

    /** Sync this list for hoster + crawler plugin! */
    public static List<String[]> getPluginDomains() {
        return MetArtCom.getPluginDomains();
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/model/[^/]+/(gallery|movie)/\\d+/[A-Za-z0-9\\-_]+");
        }
        return ret.toArray(new String[0]);
    }

    private static final Pattern PATTERN_GALLERY = Pattern.compile("/model/([^/]+)/gallery/(\\d+)/([^/]+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_MOVIE   = Pattern.compile("/model/([^/]+)/movie/(\\d+)/([^/]+)", Pattern.CASE_INSENSITIVE);

    @Override
    public int getMaxConcurrentProcessingInstances() {
        /* 2020-12-07: Preventive measure */
        return 1;
    }

    @Override
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, final ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final MetartConfig cfg = PluginJsonConfig.get(getLazyC(), MetartConfig.class);
        final Regex urlinfo_gallery = new Regex(param.getCryptedUrl(), PATTERN_GALLERY);
        final Regex urlinfo_movie;
        final Set<PhotoQuality> qualitiesPhotosLoose = cfg.getMediaQualitiesPhotosLoose();
        final Set<PhotoQuality> qualitiesPhotosZip = cfg.getMediaQualitiesPhotosZip();
        final Set<VideoQuality> qualitiesVideos = cfg.getMediaQualitiesVideos();
        if (urlinfo_gallery.patternFind()) {
            if ((qualitiesPhotosLoose == null || qualitiesPhotosLoose.isEmpty()) && (qualitiesPhotosZip == null || qualitiesPhotosZip.isEmpty())) {
                logger.info("User has deselected all qualities -> Doing nothing");
                return ret;
            }
        } else {
            if (qualitiesVideos == null || qualitiesVideos.isEmpty()) {
                logger.info("User has deselected all video qualities -> Doing nothing");
                return ret;
            }
        }
        final ArrayList<Account> accounts = AccountController.getInstance().getAllAccounts(this.getHost());
        Account useAcc = null;
        if (accounts != null && accounts.size() != 0) {
            Iterator<Account> it = accounts.iterator();
            while (it.hasNext()) {
                Account n = it.next();
                if (n.isEnabled() && n.isValid()) {
                    useAcc = n;
                    break;
                }
            }
        }
        if (useAcc == null) {
            throw new AccountRequiredException();
        }
        final MetArtCom plg = (MetArtCom) this.getNewPluginForHostInstance(this.getHost());
        plg.setBrowser(this.br);
        plg.login(useAcc, false);
        br.setFollowRedirects(true);
        if (urlinfo_gallery.patternFind()) {
            final String modelname = urlinfo_gallery.getMatch(0);
            final String date = urlinfo_gallery.getMatch(1);
            final String galleryname = urlinfo_gallery.getMatch(2);
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(modelname + " - " + date + " - " + galleryname);
            if (qualitiesPhotosZip != null && qualitiesPhotosZip.size() > 0) {
                // for(final MediaQuality qual:qualitiesPhotosZip) {
                // qual.getInternalValue();
                // }
                br.getPage("https://www." + getHost() + "/api/gallery?name=" + galleryname + "&date=" + date + "&mediaFirst=42&page=1");
                if (br.getHttpConnection().getResponseCode() == 404) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
                final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                fp.setComment(entries.get("description").toString());
                final String uuid = (String) entries.get("UUID");
                final Map<String, Object> files = (Map<String, Object>) entries.get("files");
                final Map<String, Object> sizes = (Map<String, Object>) files.get("sizes");
                final List<Map<String, Object>> zipFiles = (List<Map<String, Object>>) sizes.get("zips");
                final HashSet<String> allowedQualityKeys = new HashSet<String>();
                for (final PhotoQuality quality : qualitiesPhotosZip) {
                    allowedQualityKeys.addAll(Arrays.asList(quality.getInternalValues()));
                }
                for (final Map<String, Object> zipFile : zipFiles) {
                    final String quality = zipFile.get("quality").toString();
                    if (!allowedQualityKeys.contains(quality)) {
                        /* Skip qualities deselected by user */
                        continue;
                    }
                    String filename = (String) zipFile.get("fileName");
                    filename = this.applyFilenameExtension(filename, ".zip");
                    final String filesizeStr = zipFile.get("size").toString();
                    final DownloadLink dl = this.createDownloadlink("https://www." + getHost() + "/api/download-media/" + uuid + "/photos/" + quality);
                    dl.setFinalFileName(filename);
                    if (!StringUtils.isEmpty(filesizeStr)) {
                        dl.setDownloadSize(SizeFormatter.getSize(filesizeStr));
                    }
                    dl.setAvailable(true);
                    dl._setFilePackage(fp);
                    dl.setProperty(MetArtCom.PROPERTY_UUID, uuid);
                    dl.setProperty(MetArtCom.PROPERTY_QUALITY, quality);
                    ret.add(dl);
                }
            }
            if (qualitiesPhotosLoose != null && qualitiesPhotosLoose.size() > 0) {
                br.getPage("https://www." + getHost() + "/api/image?name=" + galleryname + "&date=" + date + "&order=5&mediaType=gallery");
                if (br.getHttpConnection().getResponseCode() == 404) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
                final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                final List<Map<String, Object>> images = (List<Map<String, Object>>) entries.get("media");
                final HashSet<String> allowedQualityKeys = new HashSet<String>();
                for (final PhotoQuality quality : qualitiesPhotosLoose) {
                    allowedQualityKeys.addAll(Arrays.asList(quality.getInternalValues()));
                }
                for (final Map<String, Object> image : images) {
                    final String uuid = image.get("UUID").toString();
                    final Map<String, Object> src_downloadable = (Map<String, Object>) image.get("src_downloadable");
                    /* Add each image in all desired && available qualities */
                    for (final String quality : allowedQualityKeys) {
                        final String url = (String) src_downloadable.get(quality);
                        if (url == null) {
                            /* Quality not available */
                            continue;
                        }
                        final String filenameURL = UrlQuery.parse(url).get("filename");
                        final DownloadLink dl = new DownloadLink(plg, filenameURL, getHost(), url, true);
                        dl.setAvailable(true);
                        if (filenameURL != null) {
                            dl.setName(filenameURL);
                        }
                        dl._setFilePackage(fp);
                        dl.setProperty(MetArtCom.PROPERTY_UUID, uuid);
                        dl.setProperty(MetArtCom.PROPERTY_QUALITY, quality);
                        ret.add(dl);
                    }
                }
            }
        } else if ((urlinfo_movie = new Regex(param.getCryptedUrl(), PATTERN_MOVIE)).patternFind()) {
            /* New 2020-12-08 */
            final String modelname = urlinfo_movie.getMatch(0);
            final String date = urlinfo_movie.getMatch(1);
            final String galleryname = urlinfo_movie.getMatch(2);
            br.getPage("https://www." + this.getHost() + "/api/movie?name=" + galleryname + "&date=" + date);
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(modelname + " - " + date + " - " + galleryname);
            fp.setComment(entries.get("description").toString());
            final String title = entries.get("name").toString();
            final String description = (String) entries.get("description");
            final String uuid = entries.get("UUID").toString();
            final Map<String, Object> files = (Map<String, Object>) entries.get("files");
            final List<Object> teasersO = (List<Object>) files.get("teasers");
            final Map<String, Object> sizes = (Map<String, Object>) files.get("sizes");
            final List<Map<String, Object>> videoQualities = (List<Map<String, Object>>) sizes.get("videos");
            if (videoQualities.isEmpty() && !teasersO.isEmpty()) {
                /* No official downloads found -> Fallback to trailer download */
                final DownloadLink trailer = this.createDownloadlink(GenericM3u8.createURLForThisPlugin(br.getURL("/api/m3u8/" + uuid + "/720.m3u8").toString()));
                trailer.setAvailable(true);
                trailer.setFinalFileName(modelname + " - " + title + " - teaser.mp4");
                trailer._setFilePackage(fp);
                ret.add(trailer);
                return ret;
            }
            long bestFilesize = -1;
            DownloadLink bestQuality = null;
            final HashSet<String> allowedQualityKeys = new HashSet<String>();
            for (final VideoQuality quality : qualitiesVideos) {
                allowedQualityKeys.add(quality.getInternalValue());
            }
            final ArrayList<DownloadLink> selectedQualities = new ArrayList<DownloadLink>();
            for (final Map<String, Object> video : videoQualities) {
                final String qualityKey = video.get("id").toString();
                final String ext;
                if (qualityKey.matches("\\d+p")) {
                    ext = "mp4";
                } else if (qualityKey.equalsIgnoreCase("4k")) {
                    ext = "mp4";
                } else {
                    /* E.g. avi, wmv */
                    ext = qualityKey;
                }
                final String downloadurl = "https://www." + getHost() + "/api/download-media/" + uuid + "/film/" + qualityKey;
                String filename = modelname + " - " + title;
                /* Do not e.g. generate filenames like "title_avi.avi" */
                if (!ext.equals(qualityKey)) {
                    filename += "_" + qualityKey;
                }
                filename += "." + ext;
                final DownloadLink dl = new DownloadLink(plg, filename, this.getHost(), downloadurl, true);
                final long filesize = SizeFormatter.getSize(video.get("size").toString());
                dl.setDownloadSize(filesize);
                dl.setAvailable(true);
                /* Prefer server-filename which will be set on downloadstart. */
                // dl.setFinalFileName(modelname + " - " + title + "_" + id + "." + ext);
                if (!StringUtils.isEmpty(description)) {
                    dl.setComment(description);
                }
                dl._setFilePackage(fp);
                dl.setProperty(MetArtCom.PROPERTY_UUID, uuid);
                dl.setProperty(MetArtCom.PROPERTY_QUALITY, qualityKey);
                ret.add(dl);
                if (bestQuality == null || filesize > bestFilesize) {
                    bestFilesize = filesize;
                    bestQuality = dl;
                }
                if (allowedQualityKeys.contains(qualityKey)) {
                    selectedQualities.add(dl);
                }
            }
            if (cfg.getVideoCrawlMode() == VideoCrawlMode.ALL_SELECTED && selectedQualities.size() > 0) {
                return selectedQualities;
            } else if (ret.isEmpty() || cfg.getVideoCrawlMode() == VideoCrawlMode.BEST) {
                /* Fallback or best */
                ret.clear();
                ret.add(bestQuality);
            } else {
                /* Return all results */
                return ret;
            }
        } else {
            /* Unsupported URL */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }
}