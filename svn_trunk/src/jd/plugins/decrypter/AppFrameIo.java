package jd.plugins.decrypter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.appwork.storage.TypeRef;
import org.appwork.utils.Regex;
import org.jdownloader.plugins.components.config.AppFrameIoConfig;
import org.jdownloader.plugins.components.config.AppFrameIoConfig.MODE;
import org.jdownloader.plugins.config.PluginJsonConfig;

import jd.controlling.ProgressController;
import jd.controlling.linkcrawler.LinkCrawlerThread;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 50231 $", interfaceVersion = 3, names = { "app.frame.io" }, urls = { "https://app\\.frame\\.io/reviews/([a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12})/?([a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12})?" })
public class AppFrameIo extends PluginForDecrypt {
    @Override
    public ArrayList<DownloadLink> decryptIt(CryptedLink parameter, ProgressController progress) throws Exception {
        final String review_link_id = new Regex(parameter.getCryptedUrl(), getMatcher().pattern()).getMatch(0);
        br.getPage("https://api.frame.io/v2/review_links/" + review_link_id + "/items/shared");
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Object root = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.OBJECT);
        if (!(root instanceof List)) {
            /*
             * E.g. {"code":404,"errors":[{"code":404,"detail":"Could not find the requested resource","status":404,"title":"Not found"}],
             * "message":"Not found"}
             */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final List<Map<String, Object>> response = (List<Map<String, Object>>) root;
        Map<String, Object> video = response.get(0);
        video = (Map<String, Object>) video.get("asset");
        final Map<String, Map<String, Object>> transcode_statuses = (Map<String, Map<String, Object>>) video.get("transcode_statuses");
        final Map<String, Map<String, Object>> transcodes = (Map<String, Map<String, Object>>) video.get("transcodes");
        final String name = (String) video.get("name");
        // TODO: webm support
        final Map<String, DownloadLink> results = new HashMap<String, DownloadLink>();
        for (final String quality : new String[] { "original", "h264_360", "h264_540", "h264_720", "h264_1080_best", "h264_2160" }) {
            final Map<String, Object> transcode_status;
            final Number filesize;
            final String url;
            final Number width;
            final Number height;
            final String filename;
            if ("original".equals(quality)) {
                filesize = (Number) video.get("filesize");
                url = (String) video.get(quality);
                width = (Number) transcodes.get("original_width");
                height = (Number) transcodes.get("original_height");
                filename = name.replaceFirst("\\.([^\\.]+)$", "_" + quality + ".$1");
            } else if ((transcode_status = transcode_statuses.get(quality)) != null && "success".equals(transcode_status.get("encode_status"))) {
                filesize = (Number) transcode_status.get("filesize");
                width = (Number) transcode_status.get("width");
                height = (Number) transcode_status.get("height");
                url = (String) video.get(quality);
                filename = name.replaceFirst("\\.([^\\.]+)$", "_" + quality + ".$1");
            } else {
                continue;
            }
            final DownloadLink link = createDownloadlink(parameter.getCryptedUrl(), false);
            link.setAvailable(true);
            link.setFinalFileName(filename);
            link.setVerifiedFileSize(filesize.longValue());
            link.setProperty(jd.plugins.hoster.AppFrameIo.PROPERTY_NAME, name);
            link.setProperty(jd.plugins.hoster.AppFrameIo.PROPERTY_URL, url);
            link.setProperty(jd.plugins.hoster.AppFrameIo.PROPERTY_QUALITY, quality);
            results.put(quality, link);
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final AppFrameIoConfig config = PluginJsonConfig.get(getConfigInterface());
        final DownloadLink best;
        final MODE mode;
        if (!(Thread.currentThread() instanceof LinkCrawlerThread)) {
            mode = MODE.ALL;
        } else {
            mode = config.getQualityMode();
        }
        switch (mode) {
        case ALL:
            best = null;
            ret.addAll(filter(results, AppFrameIoConfig.QUALITY.values()));
            break;
        case SELECTED:
            best = null;
            ret.addAll(filter(results, sort(config.getPreferredQuality())));
            break;
        case BEST:
            ret.addAll(filter(results, AppFrameIoConfig.QUALITY.values()));
            best = ret.size() > 0 ? ret.get(0) : null;
            break;
        case BEST_SELECTED:
            ret.addAll(filter(results, sort(config.getPreferredQuality())));
            best = ret.size() > 0 ? ret.get(0) : null;
            break;
        default:
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unsupported:" + mode);
        }
        if (best != null) {
            ret.clear();
            ret.add(best);
        }
        if (ret.size() == 0 && results.size() > 0) {
            throw new DecrypterRetryException(RetryReason.PLUGIN_SETTINGS, name);
        }
        if (ret.size() > 1) {
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(name);
            fp.addLinks(ret);
        }
        return ret;
    }

    protected AppFrameIoConfig.QUALITY[] sort(final Set<AppFrameIoConfig.QUALITY> qualities) {
        final List<AppFrameIoConfig.QUALITY> ret = new ArrayList<AppFrameIoConfig.QUALITY>();
        for (final AppFrameIoConfig.QUALITY quality : AppFrameIoConfig.QUALITY.values()) {
            if (qualities.contains(quality)) {
                ret.add(quality);
            }
        }
        return ret.toArray(new AppFrameIoConfig.QUALITY[0]);
    }

    protected List<DownloadLink> filter(final Map<String, DownloadLink> results, final AppFrameIoConfig.QUALITY... qualities) throws Exception {
        final List<DownloadLink> ret = new ArrayList<DownloadLink>();
        for (AppFrameIoConfig.QUALITY quality : qualities) {
            final DownloadLink result;
            switch (quality) {
            case ORIGINAL:
                result = results.get("original");
                break;
            case Q2160P:
                result = results.get("h264_2160");
                break;
            case Q1080P:
                result = results.get("h264_1080_best");
                break;
            case Q720P:
                result = results.get("h264_720");
                break;
            case Q540P:
                result = results.get("h264_540");
                break;
            case Q360P:
                result = results.get("h264_360");
                break;
            default:
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unsupported:" + quality);
            }
            if (result != null && !ret.contains(result)) {
                ret.add(result);
            }
        }
        return ret;
    }

    @Override
    public Class<AppFrameIoConfig> getConfigInterface() {
        return AppFrameIoConfig.class;
    }
}
