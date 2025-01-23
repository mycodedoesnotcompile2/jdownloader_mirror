package jd.plugins.decrypter;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 50487 $", interfaceVersion = 3, names = {}, urls = {})
public class VipergirlsToBoard extends PluginForDecrypt {
    public VipergirlsToBoard(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "vipergirls.to", "viper.to" });
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

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/threads/(\\d+)([a-z0-9\\-]+).*");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String threadID = new Regex(contenturl, "(?i)threads/(\\d+)").getMatch(0);
        final String targetPageStr = new Regex(contenturl, "(?i)/page(\\d+)").getMatch(0);
        int page = targetPageStr != null ? Integer.parseInt(targetPageStr) : 1;
        final String targetPostID = new Regex(contenturl, "(?i)(?:post|p=)(\\d+)").getMatch(0);
        final HashSet<String> dupes = new HashSet<String>();
        pagination: do {
            final String[] posts = br.getRegex("<li[^>]*id\\s*=\\s*\"post_[^>]*>(.*?)</li>\\s*<(li[^>]*id\\s*=\\s*\"post|/ol)").getColumn(0);
            if (posts == null || posts.length == 0) {
                logger.info("Stopping because: Found zero results on page " + page);
                break pagination;
            }
            int numberofNewItemsThisPage = 0;
            for (final String post : posts) {
                final String postID = new Regex(post, "name\\s*=\\s*\"post(\\d+)").getMatch(0);
                final ArrayList<DownloadLink> thisCrawledLinks = new ArrayList<DownloadLink>();
                // Get all post content and then filter it for the href links
                String postContent = new Regex(post, "<h2 class=\"title icon\">\\s*(.*?)\\s*<div\\s*class\\s*=\\s*\"(after_content|postfoot)\"").getMatch(0);
                if (postContent == null) {
                    postContent = new Regex(post, "<div\\s*class\\s*=\\s*\"content\"\\s*>\\s*(.*?)\\s*<div\\s*class\\s*=\\s*\"(after_content|postfoot)\"").getMatch(0);
                }
                final String titleArea = new Regex(postContent, "(.*?)<(a href|img src)").getMatch(0);
                String title = null;
                if (titleArea != null) {
                    String complexTitle = new Regex(titleArea, "<blockquote[^>]*>(.+)").getMatch(0);
                    if (complexTitle != null) {
                        complexTitle = complexTitle.replaceAll("<br\\s*/>\\s*<br\\s*/>[^<]+", "");
                        complexTitle = complexTitle.replaceAll("<[^>]*>", "");
                        complexTitle = complexTitle.replaceAll("[\r\n\t]+", " ");
                        complexTitle = complexTitle.replaceAll("^\\s+", "");
                        complexTitle = complexTitle.replaceAll("\\s+&", "");
                        if (StringUtils.isNotEmpty(complexTitle)) {
                            title = complexTitle;
                        }
                    }
                }
                if (title == null) {
                    title = new Regex(post, "<div style\\s*=\\s*\"text-align: center;\">\\s*<i>\\s*<b>\\s*<font color\\s*=\\s*\\s*\"red\"\\s*>\\s*<font[^>]*>(.*?)</font>\\s*</font>\\s*</b>\\s*</i>\\s*<br />").getMatch(0);
                    if (title == null) {
                        title = new Regex(post, "<b>\\s*<div style\\s*=\\s*\"text-align: center;\">\\s*<font[^>]*>\\s*(.*?)\\s*</font>\\s*</div>\\s*</b>").getMatch(0);
                        if (title == null) {
                            title = new Regex(post, "<blockquote[^>]*>\\s*<b>\\s*(.*?)\\s*</b>\\s*<br").getMatch(0);
                            if (title == null) {
                                // The title is in the H2 tag spanning 3 lines
                                title = new Regex(post, "<h2[^>]*>[\\r\\n\\s]*(.*?)[\\r\\n\\s]*</h2>").getMatch(0);
                                if (title == null) {
                                    title = threadID + "_" + postID;
                                }
                            }
                        }
                    }
                }
                FilePackage fp = null;
                if (title != null) {
                    title = title.replaceAll("(<img.*>)", "");
                    title = Encoding.htmlDecode(title).trim();
                    fp = FilePackage.getInstance();
                    fp.setName(title);
                    fp.setComment("https://" + br.getHost(true) + "/threads/" + threadID + "?p=" + postID + "&viewfull=1#post" + postID);
                }
                final String[] results = new Regex(postContent, "<a href=\"(https?://[^\"]+)").getColumn(0);
                for (final String result : results) {
                    if (!dupes.add(result)) {
                        continue;
                    }
                    numberofNewItemsThisPage++;
                    final DownloadLink link = createDownloadlink(result);
                    if (fp != null) {
                        link._setFilePackage(fp);
                    }
                    if (targetPostID == null) {
                        /* We aren't looking for a specific post -> Return all results instantly. */
                        distribute(link);
                    }
                    thisCrawledLinks.add(link);
                }
                ret.addAll(thisCrawledLinks);
                if (targetPostID != null && targetPostID.equals(postID)) {
                    /* Return only all results for user wished specific post. */
                    logger.info("Found user wished postID: " + targetPostID);
                    return thisCrawledLinks;
                }
            }
            final String nextPage = br.getRegex("(threads/" + threadID + "[^/]+/page" + (page + 1) + "[^\"]*)\"").getMatch(0);
            logger.info("Crawled page " + page + " | New items this page: " + numberofNewItemsThisPage + " | nextPage: " + nextPage);
            if (this.isAbort()) {
                logger.info("Stopping because: Aborted by user");
                break pagination;
            } else if (targetPageStr != null) {
                logger.info("Stopping because: User supplied desired page to crawl: " + targetPageStr);
                break pagination;
            } else if (nextPage == null) {
                logger.info("Stopping because: Reached end");
                break pagination;
            } else if (numberofNewItemsThisPage == 0) {
                logger.info("Stopping because: Failed to find any new items");
                break pagination;
            } else if (targetPostID != null) {
                logger.info("Stopping because: User wants specific post only but that was not found");
                break pagination;
            } else {
                /* Continue to next page */
                page++;
                br.getPage(nextPage);
                continue pagination;
            }
        } while (!this.isAbort());
        if (ret.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return ret;
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        return 1;
    }
}
