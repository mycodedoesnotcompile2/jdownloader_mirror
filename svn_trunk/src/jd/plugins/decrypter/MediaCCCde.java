package jd.plugins.decrypter;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 52991 $", interfaceVersion = 3, names = {}, urls = {})
public class MediaCCCde extends PluginForDecrypt {
    public MediaCCCde(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(new String[] { "media.ccc.de" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    private static final Pattern PATTERN_VIDEO   = Pattern.compile("/v/([A-Za-z0-9\\-_]+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_CHANNEL = Pattern.compile("/c/([A-Za-z0-9\\-_]+)", Pattern.CASE_INSENSITIVE);

    public static String[] getAnnotationUrls() {
        return buildAnnotationUrls(getPluginDomains());
    }

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(" + PATTERN_VIDEO.pattern().substring(1) + "|" + PATTERN_CHANNEL.pattern().substring(1) + ")");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public ArrayList<DownloadLink> decryptIt(CryptedLink param, ProgressController progress) throws Exception {
        br.setFollowRedirects(true);
        br.getPage(param.getCryptedUrl());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String title = br.getRegex("<meta content=\"([^\"]*?)\"\\s*property=\"og:title\">").getMatch(0);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        if (StringUtils.containsIgnoreCase(param.getCryptedUrl(), "/v/")) {
            final String[][] withSize = br.getRegex("class=\"[^\"]*\\bdownload\\b[^\"]*\"\\s+href=\"(https?://[^\"]+)\"[^>]*>(?:(?!href=\").)*?<span class=\"size\">([^<]+)</span>").getMatches();
            if (withSize != null && withSize.length > 0) {
                for (final String[] videoURL : withSize) {
                    final DownloadLink link = createDownloadlink(DirectHTTP.createURLForThisPlugin(videoURL[0]));
                    link.setDownloadSize(SizeFormatter.getSize(videoURL[1]));
                    link.setAvailable(true);
                    ret.add(link);
                }
            } else {
                final String[] withoutSize = br.getRegex("class=\"[^\"]*\\bdownload\\b[^\"]*\"\\s+href=\"(https?://[^\"]+)\"").getColumn(0);
                if (withoutSize != null && withoutSize.length > 0) {
                    for (final String videoURL : withoutSize) {
                        final DownloadLink link = createDownloadlink(DirectHTTP.createURLForThisPlugin(videoURL));
                        link.setAvailable(true);
                        ret.add(link);
                    }
                } else {
                    final String[] fallbackURLs = br.getRegex("href=\"(https?://[^\"]+\\.(?:webm|mp4|mp3|opus))\"").getColumn(0);
                    if (fallbackURLs != null) {
                        for (final String videoURL : fallbackURLs) {
                            final DownloadLink link = createDownloadlink(DirectHTTP.createURLForThisPlugin(videoURL));
                            link.setAvailable(true);
                            ret.add(link);
                        }
                    }
                }
            }
            final FilePackage fp = FilePackage.getInstance();
            if (title != null) {
                fp.setName(title);
            } else {
                /* Fallback */
                fp.setName(br._getURL().getPath());
            }
            fp.addLinks(ret);
        } else {
            final String[] videoURLs = br.getRegex("<a href=\"(/v/.*?)\">").getColumn(0);
            if (videoURLs != null) {
                for (final String videoURL : videoURLs) {
                    final DownloadLink link = createDownloadlink(br.getURL(videoURL).toString());
                    ret.add(link);
                }
            }
        }
        return ret;
    }
}
