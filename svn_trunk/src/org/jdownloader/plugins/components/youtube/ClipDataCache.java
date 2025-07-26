package org.jdownloader.plugins.components.youtube;

import java.io.IOException;
import java.net.URL;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map.Entry;
import java.util.WeakHashMap;

import jd.plugins.AccountRequiredException;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.net.httpconnection.HTTPProxy;
import org.jdownloader.translate._JDT;

public class ClipDataCache {
    public static final String THE_DOWNLOAD_IS_NOT_AVAILABLE_IN_YOUR_COUNTRY = "The Download is not available in your country";

    private static class CachedClipData {
        private volatile YoutubeClipData clipData  = null;
        private final long               timeStamp = Time.systemIndependentCurrentJVMTimeMillis();
        private final List<HTTPProxy>    proxyList;

        private CachedClipData(List<HTTPProxy> proxyListNew, YoutubeClipData youtubeClipData) {
            this.clipData = youtubeClipData;
            proxyList = proxyListNew;
        }

        private boolean hasValidProxyList(List<HTTPProxy> validateList) {
            if (proxyList != null && validateList != null) {
                for (final HTTPProxy proxy : proxyList) {
                    if (!validateList.contains(proxy)) {
                        return false;
                    }
                }
                return true;
            } else {
                return false;
            }
        }

        private boolean isExpired() {
            return Time.systemIndependentCurrentJVMTimeMillis() - timeStamp > (30 * 60 * 1000l);
        }
    }

    private static final WeakHashMap<CachedClipData, String> CACHE = new WeakHashMap<CachedClipData, String>();

    public static YoutubeClipData get(YoutubeHelper helper, DownloadLink downloadLink) throws Exception {
        final String videoID = downloadLink.getStringProperty(YoutubeHelper.YT_ID);
        final String playListID = downloadLink.getStringProperty(YoutubeHelper.YT_PLAYLIST_ID);
        final CachedClipData ret;
        final String previousPlayListID = helper.getPlaylistID();
        try {
            helper.setPlaylistID(playListID);
            ret = get(helper, new YoutubeClipData(videoID));
            ret.clipData.copyToDownloadLink(downloadLink);
        } finally {
            helper.setPlaylistID(previousPlayListID);
        }

        // put a reference to the link. if we remove all links with the ref, the cache will cleanup it self
        downloadLink.getTempProperties().setProperty("CLIP_DATA_REFERENCE", ret);
        return ret.clipData;
    }

    public static YoutubeClipData load(YoutubeHelper helper, YoutubeClipData vid) throws Exception {
        return get(helper, vid).clipData;
    }

    private static CachedClipData get(YoutubeHelper helper, YoutubeClipData vid) throws Exception {
        final String cachedID = vid.videoID;
        if (StringUtils.isEmpty(cachedID)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final List<HTTPProxy> proxyListNew = helper.getBr().selectProxies(new URL("https://youtube.com"));
        while (true) {
            CachedClipData cachedData = get(cachedID);
            if (cachedData != null) {
                synchronized (cachedData) {
                    if (cachedData.isExpired()) {
                        helper.getLogger().info("invalidate CachedClipData:" + cachedID + "|reason:expired");
                    } else if (!cachedData.hasValidProxyList(proxyListNew)) {
                        helper.getLogger().info("invalidate CachedClipData:" + cachedID + "|reason:no valid proxyList");
                    } else if (StringUtils.isEmpty(cachedData.clipData.title)) {
                        helper.getLogger().info("invalidate CachedClipData:" + cachedID + "|reason:missing title");
                    } else if (cachedData.clipData.datePublished == 0) {
                        helper.getLogger().info("invalidate CachedClipData:" + cachedID + "|reason:missing date");
                    } else if (cachedData.clipData == null) {
                        helper.getLogger().info("invalidate CachedClipData:" + cachedID + "|reason:missing data");
                    } else {
                        helper.getLogger().info("valid CachedClipData found:" + cachedID);
                        cachedData.clipData.copyTo(vid);
                        check(helper, cachedData);
                        return cachedData;
                    }
                }
            }
            CachedClipData newCachedData = null;
            synchronized (CACHE) {
                if (get(cachedID) == cachedData) {
                    newCachedData = new CachedClipData(proxyListNew, vid);
                    put(cachedID, newCachedData);
                }
                cachedData = get(cachedID);
            }
            synchronized (cachedData) {
                if (cachedData == newCachedData) {
                    try {
                        helper.getLogger().info("refresh CachedClipData:" + cachedID);
                        helper.loadVideo(cachedData.clipData);
                    } finally {
                        cachedData.notifyAll();
                    }
                } else {
                    cachedData.wait(500);
                }
            }
        }
    }

    private static void check(YoutubeHelper helper, CachedClipData cachedData) throws PluginException {
        if (cachedData.clipData.streams == null || StringUtils.isNotEmpty(cachedData.clipData.error)) {
            if (StringUtils.equalsIgnoreCase(cachedData.clipData.error, "This video is unavailable.") || StringUtils.equalsIgnoreCase(cachedData.clipData.error, "This video is not available.")) {
                // this is not region issue, its just not available.
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, cachedData.clipData.error);
            }
            if (StringUtils.containsIgnoreCase(cachedData.clipData.error, "This video has been removed")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, cachedData.clipData.error);
            }
            // private video.. login is required! assumption that account hasn't been used.. or wrong account has been used...
            if (StringUtils.containsIgnoreCase(cachedData.clipData.error, "This Video is Private")) {
                if (helper.getAccountLoggedIn() != null) {
                    // wrong account used?? try next??
                    // TODO: confirm with jiaz that this this type of exception will try the next account
                }
                throw new AccountRequiredException(cachedData.clipData.error); // .localizedMessage(_JDT.T.AccountRequiredException_createCandidateResult());
            }
            if (cachedData.clipData.error != null) {
                String lc = cachedData.clipData.error.toLowerCase(Locale.ENGLISH);
                if (lc.contains("is not available in your country") || lc.contains("geo blocked due to copyright grounds")) {
                    // 18.04.2016
                    // „Unfortunately, this video is not available in Germany because it may contain music for which GEMA has not
                    // granted the respective music rights.”
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, THE_DOWNLOAD_IS_NOT_AVAILABLE_IN_YOUR_COUNTRY).localizedMessage(_JDT.T.CountryIPBlockException_createCandidateResult());
                }
                if (lc.contains("content is not available in")) {
                    // „Unfortunately, this UMG-music-content is not available in Germany because GEMA has not granted the
                    // respective music publishing rights.”
                    // 18.04.2016
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, THE_DOWNLOAD_IS_NOT_AVAILABLE_IN_YOUR_COUNTRY).localizedMessage(_JDT.T.CountryIPBlockException_createCandidateResult());
                }
            }
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, cachedData.clipData.error);
        }
    }

    public static void clearCache(DownloadLink downloadLink) {
        final String videoID = downloadLink.getStringProperty(YoutubeHelper.YT_ID);
        clearCache(videoID);
    }

    public static void clearCache(String videoID) {
        put(videoID, null);
    }

    public static void referenceLink(YoutubeHelper helper, DownloadLink link, YoutubeClipData vid) {
        final String cachedID = vid.videoID;
        List<HTTPProxy> proxyListNew = null;
        try {
            proxyListNew = helper.getBr().selectProxies(new URL("https://youtube.com"));
        } catch (IOException e) {
            helper.getLogger().log(e);
        }
        while (true) {
            CachedClipData data = get(cachedID);
            if (data != null) {
                synchronized (data) {
                    if (!data.isExpired() && data.hasValidProxyList(proxyListNew)) {
                        data.clipData = vid;
                        link.getTempProperties().setProperty("CLIP_DATA_REFERENCE", data);
                        break;
                    }
                }
            }
            synchronized (CACHE) {
                if (get(cachedID) == data) {
                    data = new CachedClipData(proxyListNew, vid);
                    link.getTempProperties().setProperty("CLIP_DATA_REFERENCE", data);
                    put(cachedID, data);
                    break;
                }
            }
        }
    }

    private static CachedClipData get(final String videoID) {
        synchronized (CACHE) {
            final Iterator<Entry<CachedClipData, String>> it = CACHE.entrySet().iterator();
            while (it.hasNext()) {
                final Entry<CachedClipData, String> next = it.next();
                final CachedClipData ret = next.getKey();
                if (ret == null || ret.clipData == null) {
                    it.remove();
                } else if (StringUtils.equals(videoID, next.getValue())) {
                    return ret;
                }
            }
        }
        return null;
    }

    private static void put(final String videoID, final CachedClipData data) {
        synchronized (CACHE) {
            final Iterator<Entry<CachedClipData, String>> it = CACHE.entrySet().iterator();
            while (it.hasNext()) {
                final Entry<CachedClipData, String> next = it.next();
                final CachedClipData ret = next.getKey();
                if (ret == null || ret.clipData == null) {
                    it.remove();
                } else if (StringUtils.equals(videoID, next.getValue())) {
                    it.remove();
                }
            }
            if (data != null) {
                CACHE.put(data, videoID);
            }
        }
    }

    public static boolean hasCache(YoutubeHelper helper, String videoID) {
        final CachedClipData cachedData = get(videoID);
        if (cachedData != null) {
            synchronized (cachedData) {
                try {
                    final List<HTTPProxy> proxyListNew = helper.getBr().selectProxies(new URL("https://youtube.com"));
                    return !cachedData.isExpired() && cachedData.hasValidProxyList(proxyListNew);
                } catch (IOException e) {
                    helper.getLogger().log(e);
                }
            }
        }
        return false;
    }

    public static boolean hasCache(YoutubeHelper helper, DownloadLink downloadLink) {
        final String videoID = downloadLink.getStringProperty(YoutubeHelper.YT_ID);
        return hasCache(helper, videoID);
    }
}
