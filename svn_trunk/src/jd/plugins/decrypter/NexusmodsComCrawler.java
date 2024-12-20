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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.AccountRequiredException;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.PluginForHost;
import jd.utils.JDUtilities;

@DecrypterPlugin(revision = "$Revision: 50044 $", interfaceVersion = 3, names = { "nexusmods.com" }, urls = { "https?://(?:www\\.)?nexusmods\\.com/(?!contents)([^/]+)/mods/(\\d+)/?" })
public class NexusmodsComCrawler extends PluginForDecrypt {
    public NexusmodsComCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String url = param.getCryptedUrl().replaceFirst("^http://", "https://");
        final PluginForHost plugin = this.getNewPluginForHostInstance(this.getHost());
        ((jd.plugins.hoster.NexusmodsCom) plugin).setLogger(getLogger());
        ((jd.plugins.hoster.NexusmodsCom) plugin).setBrowser(br);
        final String game_domain_name = new Regex(url, this.getSupportedLinks()).getMatch(0);
        final String mod_id = new Regex(url, this.getSupportedLinks()).getMatch(1);
        if (game_domain_name == null || mod_id == null) {
            /* This should never happen */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "game_domain_name or mod_id missing");
        }
        final Account account = AccountController.getInstance().getValidAccount(plugin.getHost());
        final String apikey = jd.plugins.hoster.NexusmodsCom.getApikey(account);
        if (apikey != null) {
            ret = crawlAPI(param, account, game_domain_name, mod_id);
        } else {
            ret = crawlWebsite(param, game_domain_name, mod_id);
        }
        return ret;
    }

    private ArrayList<DownloadLink> crawlAPI(final CryptedLink param, final Account account, final String game_domain_name, final String mod_id) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        jd.plugins.hoster.NexusmodsCom.prepBrAPI(br, account);
        /* First check for offline, get game_id and name of the mod */
        br.getPage(jd.plugins.hoster.NexusmodsCom.API_BASE + String.format("/games/%s/mods/%s.json", game_domain_name, mod_id));
        /* 1st offline check */
        jd.plugins.hoster.NexusmodsCom.handleErrorsAPI(br);
        Map<String, Object> entries = JavaScriptEngineFactory.jsonToJavaMap(br.toString());
        final String game_id = Long.toString(JavaScriptEngineFactory.toLong(entries.get("game_id"), -1));
        if (game_id.equals("-1")) {
            return null;
        }
        String mod_name = (String) entries.get("name");
        if (StringUtils.isEmpty(mod_name)) {
            /* This should never happen */
            mod_name = "UNKNOWN";
        }
        br.getPage(jd.plugins.hoster.NexusmodsCom.API_BASE + String.format("/games/%s/mods/%s/files.json", game_domain_name, mod_id));
        /* 2nd offline check */
        jd.plugins.hoster.NexusmodsCom.handleErrorsAPI(br);
        entries = JavaScriptEngineFactory.jsonToJavaMap(br.toString());
        final List<Object> files = (List<Object>) entries.get("files");
        for (final Object fileO : files) {
            entries = ((Map<String, Object>) fileO);
            final String file_id = Long.toString(JavaScriptEngineFactory.toLong(entries.get("file_id"), -1));
            final String description = (String) entries.get("description");
            /* This was the old way to get the game_id */
            // String game_id = null;
            // final String content_preview_link = (String) entries.get("content_preview_link");
            // if (content_preview_link != null) {
            // /* That's a little sketchy as they do not provide a field for this 'game_id' but we need that! */
            // game_id = new Regex(content_preview_link, "nexus-files-meta/(\\d+)/").getMatch(0);
            // }
            if (file_id.equals("-1")) {
                /* Skip invalid items */
                continue;
            }
            final int category_id = (int) JavaScriptEngineFactory.toLong(entries.get("category_id"), 0);
            String category_name = (String) entries.get("category_name");
            if (StringUtils.isEmpty(category_name)) {
                /* Fallback/Workaround */
                category_name = apiCategoryIDToString(category_id);
            }
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(game_domain_name + " - " + mod_name + " - " + category_name);
            final DownloadLink link = createDownloadlink(generatePluginPatternMatcher(file_id, game_id));
            link.setContentUrl(generateContentURL(game_domain_name, mod_id, file_id));
            jd.plugins.hoster.NexusmodsCom.setFileInformationAPI(link, entries, game_domain_name, mod_id, file_id);
            link._setFilePackage(fp);
            /* Important! These properties are especially required for all API requests! */
            link.setProperty(jd.plugins.hoster.NexusmodsCom.PROPERTY_game_domain_name, game_domain_name);
            link.setProperty(jd.plugins.hoster.NexusmodsCom.PROPERTY_mod_id, mod_id);
            /* Every category goes into a subfolder */
            link.setRelativeDownloadFolderPath(game_domain_name + "/" + mod_name + "/" + category_name);
            link.setAvailable(true);
            if (!StringUtils.isEmpty(description)) {
                link.setComment(description);
            }
            ret.add(link);
        }
        return ret;
    }

    /** Crawls items from website (without account). */
    private ArrayList<DownloadLink> crawlWebsite(final CryptedLink param, final String game_domain_name, final String mod_id) throws Exception {
        final String contenturl = param.getCryptedUrl();
        final PluginForHost plugin = JDUtilities.getPluginForHost(this.getHost());
        br.setFollowRedirects(true);
        ((jd.plugins.hoster.NexusmodsCom) plugin).getPage(br, contenturl);
        if (jd.plugins.hoster.NexusmodsCom.isOfflineWebsite(br)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (((jd.plugins.hoster.NexusmodsCom) plugin).isLoginRequired(br)) {
            throw new AccountRequiredException();
        } else if (br.containsHTML(">\\s*This mod contains adult content")) {
            /* 2019-10-02: Account required + setting has to be enabled in account to be able to see/download such content! */
            logger.info("Adult content: Enable it in your account settings to be able to download such files via JD: Profile --> Settings --> Content blocking --> Show adult content");
            throw new AccountRequiredException("Adult content: Enable it in your account settings to be able to download such files via JD: Profile --> Settings --> Content blocking --> Show adult content");
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        String mod_name = br.getRegex("<meta property=\"og:title\" content=\"([^<>\"]+)\"").getMatch(0);
        if (mod_name == null) {
            /* This should never happen */
            mod_name = "UNKNOWN";
        }
        final String game_id = br.getRegex("game_id\\s*=\\s*(\\d+)").getMatch(0);
        if (game_id == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find game_id");
        }
        final Browser br2 = br.cloneBrowser();
        ((jd.plugins.hoster.NexusmodsCom) plugin).getPage(br2, "/" + game_domain_name + "/mods/" + mod_id + "?tab=files");
        final String[] categoryNames = br2.getRegex("<div class=\"file-category-header\">\\s*<h2>([^>]+)</h2>").getColumn(0);
        final String[] downloadCategoriesHTMLs = br2.getRegex("<dd[^>]*data-id=\"\\d+\"[^>]*>.*?</div>\\s*</dt>").getColumn(-1);
        String description = br2.getRegex("<meta name=\"description\" content=\"([^\"]+)").getMatch(0);
        if (downloadCategoriesHTMLs != null && downloadCategoriesHTMLs.length > 0) {
            int index = -1;
            for (final String downnloadTypeHTML : downloadCategoriesHTMLs) {
                index++;
                String category_name = null;
                if (categoryNames != null && categoryNames.length == downloadCategoriesHTMLs.length) {
                    category_name = categoryNames[index];
                }
                category_name = Encoding.htmlDecode(category_name).trim();
                ret.addAll(websiteCrawlFiles(downnloadTypeHTML, category_name, index, mod_id, game_id, game_domain_name, mod_name));
            }
        } else {
            /* Fallback: Don't care about the categories, just find the files */
            logger.warning("Failed to find categories");
            ret.addAll(websiteCrawlFiles(br2.getRequest().getHtmlCode(), null, 0, mod_id, game_id, game_domain_name, mod_name));
        }
        if (ret.isEmpty()) {
            /*
             * Effectively, items are only downloadable via (paid) account so if the crawler fails, let's suppress that and instead ask the
             * user to retry again with account -> User can then use the API so then the crawler will work fine.
             */
            // throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find game_id");
            throw new AccountRequiredException();
        }
        if (description != null) {
            /* Set description as package comment. */
            description = Encoding.htmlDecode(description).trim();
            for (final DownloadLink link : ret) {
                if (link.getFilePackage() == null) {
                    continue;
                }
                link.getFilePackage().setComment(description);
            }
        }
        return ret;
    }

    private List<DownloadLink> websiteCrawlFiles(final String html_source, final String category_name, final int categoryIndex, final String mod_id, final String game_id, final String game_domain_name, final String mod_name) throws PluginException {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final FilePackage fp = FilePackage.getInstance();
        if (category_name != null) {
            fp.setName(game_domain_name + " - " + mod_name + " - " + category_name);
        } else {
            fp.setName(game_domain_name + " - " + mod_name);
        }
        final String currentPath = game_domain_name + "/" + mod_name + "/" + category_name;
        final String[] htmls = new Regex(html_source, "<dt id=\"file-expander-header-\\d+\".*?/use>\\s*</svg>\\s*</div>").getColumn(-1);
        if (htmls == null || htmls.length == 0) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String extDefault = ".zip";
        int position = 0;
        for (final String html : htmls) {
            String file_id = new Regex(html, "\\?id=(\\d+)").getMatch(0);
            if (file_id == null) {
                file_id = new Regex(html, "data-id=\"(\\d+)\"").getMatch(0);
            }
            if (file_id == null) {
                logger.info("file_id is null");
                continue;
            }
            position++;
            final String filename = new Regex(html, "data-url=\"([^\"]+)\"").getMatch(0);
            final DownloadLink link = createDownloadlink(generatePluginPatternMatcher(file_id, game_id));
            link.setContentUrl(generateContentURL(game_domain_name, mod_id, file_id));
            final String filesizeStr = new Regex(html, ">\\s*File\\s*size\\s*</div>.*?\"stat\"\\s*>\\s*([0-9\\.TKGMB]+)").getMatch(0);
            if (filesizeStr != null) {
                link.setDownloadSize(SizeFormatter.getSize(filesizeStr));
            } else {
                logger.warning("Failed to find filesize");
            }
            if (filename != null) {
                link.setName(file_id + "_" + Encoding.htmlOnlyDecode(filename) + extDefault);
            } else {
                link.setName(fp.getName() + "_" + file_id + "_" + position + extDefault);
            }
            link.setAvailable(true);
            link.setRelativeDownloadFolderPath(currentPath);
            link._setFilePackage(fp);
            /* Important! These properties are especially required for all API requests! */
            link.setProperty("game_domain_name", game_domain_name);
            link.setProperty("mod_id", mod_id);
            if (category_name != null) {
                /* Every category goes into a subfolder */
                link.setRelativeDownloadFolderPath(game_domain_name + "/" + category_name);
            }
            ret.add(link);
        }
        return ret;
    }

    private String generateContentURL(final String game_domain_name, final String mod_id, final String file_id) {
        return String.format("https://www." + this.getHost() + "/%s/mods/%s?tab=files&file_id=%s", game_domain_name, mod_id, file_id);
    }

    private String generatePluginPatternMatcher(final String file_id, final String game_id) {
        return String.format("https://www." + this.getHost() + "/Core/Libs/Common/Widgets/DownloadPopUp?id=%s&nmm=0&game_id=%s&source=FileExpander", file_id, game_id);
    }

    /*
     * Sadly there is not always a mapping via API so we will have to keep this updated by hand but they will probably not add / change
     * these category IDs in the near future! Especially for files of category 6 their API will often return 'null' as 'category_name'.
     */
    private String apiCategoryIDToString(final int cetegoryID) {
        switch (cetegoryID) {
        case 1:
            return "MAIN";
        case 2:
            return "UPDATE";
        case 3:
            return "OPTIONAL";
        case 4:
            return "OLD_VERSION";
        case 5:
            return "MISCELLANEOUS";
        case 6:
            return "OLD FILES";
        default:
            return "UNKNOWN_CATEGORY_ID_" + cetegoryID;
        }
    }
}
