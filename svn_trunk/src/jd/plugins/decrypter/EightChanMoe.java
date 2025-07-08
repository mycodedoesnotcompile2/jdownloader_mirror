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

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.Cookie;
import jd.http.Request;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.components.config.EightChanMoeConfig;
import org.jdownloader.plugins.components.config.EightChanMoeConfig.POSTANCHORMODE;
import org.jdownloader.plugins.config.PluginJsonConfig;

@DecrypterPlugin(revision = "$Revision: 51187 $", interfaceVersion = 2, names = {}, urls = {})
public class EightChanMoe extends PluginForDecrypt {
    /**
     * https://gitgud.io/LynxChan/LynxChan/-/blob/master/doc/Json.txt
     *
     * @param wrapper
     */
    public EightChanMoe(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        return Arrays.asList(new String[][] { new String[] { "8chan.moe", "8chan.se", "8chan.ce" } });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/[^/]+/res/\\d+\\.html(#q?\\d+)?");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public void init() {
        super.init();
        for (final String site : siteSupportedNames()) {
            Browser.setRequestIntervalLimitGlobal(site, true, 1250);
        }
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        /* 2020-11-19: Preventive measure */
        return 1;
    }

    private static final String TYPE_THREAD = "https?://([^/]+)/([^/]+)/res/(\\d+)\\.html(#q?(\\d+))?";
    private EightChanMoeConfig  config      = null;

    public ArrayList<DownloadLink> decryptIt(CryptedLink param, ProgressController progress) throws Exception {
        config = PluginJsonConfig.get(this.getConfigInterface());
        if (param.getCryptedUrl().matches(TYPE_THREAD)) {
            return crawlSingleThreadAPI(param);
        }
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public void clean() {
        super.clean();
        config = null;
    }

    private Browser prepBrAPI(final Browser br) {
        br.setFollowRedirects(true);
        br.getHeaders().put("User-Agent", "JDownloader");
        return br;
    }

    private static AtomicReference<String> TOS_COOKIE = new AtomicReference<String>("TOS20250418");

    private void getPage(Browser br, String url) throws PluginException, IOException {
        synchronized (TOS_COOKIE) {
            final String tosCookie = TOS_COOKIE.get();
            if (tosCookie != null) {
                br.setCookie(br.getHost(url), tosCookie, "1");
            }
            br.getPage(url);
            if (br.getURL().endsWith("disclaimer.html")) {
                final String confirmed = br.getRegex("href\\s*=\\s*\"([^\"]*confirmed.html)\"").getMatch(0);
                if (confirmed == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                br.getPage(confirmed);
                for (Cookie cookie : br.getCookies(br.getHost()).getCookies()) {
                    if (StringUtils.startsWithCaseInsensitive(cookie.getKey(), "TOS") && (tosCookie == null || !tosCookie.equals(cookie.getKey()))) {
                        if (tosCookie != null) {
                            logger.info("New cookie key is:" + cookie.getKey());
                        }
                        TOS_COOKIE.set(cookie.getKey());
                        break;
                    }
                }
                br.getPage(url);
            }
        }
    }

    private List<DownloadLink> parseFiles(final Browser br, List<Map<String, Object>> files) throws PluginException, IOException {
        final boolean preferServerFilenames = config.isPreferServerFilenamesOverPluginDefaultFilenames();
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        if (files == null || files.size() == 0) {
            return ret;
        }
        for (final Map<String, Object> file : files) {
            final String path = (String) file.get("path");
            final String originalName = (String) file.get("originalName");
            final long size = ((Number) file.get("size")).longValue();
            final URL url = br.getURL(path);
            final DownloadLink dl = this.createDownloadlink(url.toExternalForm());
            final String setFileName;
            if (!preferServerFilenames && StringUtils.isNotEmpty(originalName)) {
                setFileName = originalName;
            } else {
                setFileName = getFileNameFromURL(url);
            }
            dl.setFinalFileName(setFileName);
            dl.setProperty(DirectHTTP.FIXNAME, setFileName);
            dl.setVerifiedFileSize(size);
            dl.setProperty(DirectHTTP.PROPERTY_COOKIES, TOS_COOKIE.get() + "=1");
            dl.setAvailable(true);
            ret.add(dl);
        }
        return ret;
    }

    private ArrayList<DownloadLink> parseResponse(final CryptedLink param, final Request request, POSTANCHORMODE postMode) throws IOException, PluginException, DecrypterRetryException {
        final String boardDomain = new Regex(param.getCryptedUrl(), TYPE_THREAD).getMatch(0);
        final String boardShort = new Regex(param.getCryptedUrl(), TYPE_THREAD).getMatch(1);
        final String threadID = new Regex(param.getCryptedUrl(), TYPE_THREAD).getMatch(2);
        String requestedPostId = new Regex(param.getCryptedUrl(), TYPE_THREAD).getMatch(4);
        if (POSTANCHORMODE.THREAD.equals(postMode)) {
            requestedPostId = null;
        }
        if (boardDomain == null || boardShort == null || threadID == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final String boardName = entries.get("boardName").toString();
        if (requestedPostId == null) {
            ret.addAll(parseFiles(br, (List<Map<String, Object>>) entries.get("files")));
        }
        final List<Map<String, Object>> posts = (List<Map<String, Object>>) entries.get("posts");
        if (posts != null) {
            for (final Map<String, Object> post : posts) {
                final String postID = post.get("postId").toString();
                if (requestedPostId != null && !requestedPostId.equals(postID)) {
                    continue;
                }
                final List<DownloadLink> postItems = parseFiles(br, (List<Map<String, Object>>) post.get("files"));
                for (DownloadLink postItem : postItems) {
                    postItem.setContainerUrl("https://" + boardDomain + "/" + boardShort + "/res/" + threadID + ".html#" + postID);
                }
                ret.addAll(postItems);
            }
        }
        if (ret.size() > 0) {
            final FilePackage fp = FilePackage.getInstance();
            if (requestedPostId != null) {
                fp.setName(getHost() + " - " + boardName + " - " + threadID + " - " + requestedPostId);
            } else {
                fp.setName(getHost() + " - " + boardName + " - " + threadID);
            }
            fp.addLinks(ret);
        }
        return ret;
    }

    private ArrayList<DownloadLink> crawlSingleThreadAPI(final CryptedLink param) throws IOException, PluginException, DecrypterRetryException {
        final String boardDomain = new Regex(param.getCryptedUrl(), TYPE_THREAD).getMatch(0);
        final String boardShort = new Regex(param.getCryptedUrl(), TYPE_THREAD).getMatch(1);
        final String threadID = new Regex(param.getCryptedUrl(), TYPE_THREAD).getMatch(2);
        final String requestedPostId = new Regex(param.getCryptedUrl(), TYPE_THREAD).getMatch(4);
        if (boardDomain == null || boardShort == null || threadID == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        prepBrAPI(this.br);
        getPage(br, "https://" + boardDomain + "/" + boardShort + "/res/" + threadID + ".json");
        final POSTANCHORMODE postMode = config.getPostAnchorMode();
        ArrayList<DownloadLink> ret = parseResponse(param, br.getRequest(), postMode);
        if (ret.size() == 0 && POSTANCHORMODE.POST_OR_THREAD.equals(postMode)) {
            ret = parseResponse(param, br.getRequest(), POSTANCHORMODE.THREAD);
        }
        if (ret.size() == 0) {
            if (requestedPostId != null) {
                throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER, "EMPTY_THREAD_POST" + threadID + "_" + requestedPostId, "Thread " + threadID + " doesn't contain any media for post " + requestedPostId);
            } else {
                throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER, "EMPTY_THREAD" + threadID, "Thread " + threadID + " doesn't contain any media");
            }
        }
        return ret;
    }

    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }

    @Override
    public Class<? extends EightChanMoeConfig> getConfigInterface() {
        return EightChanMoeConfig.class;
    }
}