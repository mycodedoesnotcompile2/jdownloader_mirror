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
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;

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
import jd.plugins.hoster.NexusmodsCom;

@DecrypterPlugin(revision = "$Revision: 50505 $", interfaceVersion = 3, names = { "nexusmods.com" }, urls = { "https?://(?:www\\.)?nexusmods\\.com/(?!contents)([^/]+)/mods/(\\d+)/?(?:\\?tab=files&file_id=(\\d+))?" })
public class NexusmodsComCrawler extends PluginForDecrypt {
    public NexusmodsComCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final String contenturl = param.getCryptedUrl().replaceFirst("^http://", "https://");
        final PluginForHost plugin = this.getNewPluginForHostInstance(this.getHost());
        final String game_domain_name = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        final String mod_id = new Regex(contenturl, this.getSupportedLinks()).getMatch(1);
        final String file_id = new Regex(contenturl, this.getSupportedLinks()).getMatch(2);
        if (game_domain_name == null || mod_id == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "game_domain_name or mod_id missing");
        }
        final Account account = AccountController.getInstance().getValidAccount(plugin.getHost());
        final String apikey = NexusmodsCom.getApikey(account);
        final ArrayList<DownloadLink> ret;
        if (apikey != null) {
            ret = crawlAPI(param, account, game_domain_name, mod_id);
        } else {
            ret = crawlWebsite(param, game_domain_name, mod_id);
        }
        if (file_id != null) {
            /*
             * User wants a specific file but we may have crawled more -> Check if we found that specific file and return only that one if
             * we find it.
             */
            final Iterator<DownloadLink> it = ret.iterator();
            while (it.hasNext()) {
                final DownloadLink next = it.next();
                final String next_file_id = next.getStringProperty(NexusmodsCom.PROPERTY_file_id);
                if (next_file_id.equals(file_id)) {
                    ret.clear();
                    ret.add(next);
                    return ret;
                }
            }
            logger.info("Failed to find item with specific file_id -> Returning all results");
        }
        return ret;
    }

    private ArrayList<DownloadLink> crawlAPI(final CryptedLink param, final Account account, final String game_domain_name, final String mod_id) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        NexusmodsCom.prepBrAPI(br, account);
        /* First check for offline, get game_id and name of the mod */
        br.getPage(NexusmodsCom.API_BASE + String.format("/games/%s/mods/%s.json", game_domain_name, mod_id));
        /* 1st offline check */
        NexusmodsCom.handleErrorsAPI(br);
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final String mod_description = (String) entries.get("description");
        final String game_id = entries.get("game_id").toString();
        final String mod_name = entries.get("name").toString();
        br.getPage(NexusmodsCom.API_BASE + String.format("/games/%s/mods/%s/files.json", game_domain_name, mod_id));
        /* 2nd offline check */
        NexusmodsCom.handleErrorsAPI(br);
        final Map<String, Object> files_overview = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final List<Map<String, Object>> files = (List<Map<String, Object>>) files_overview.get("files");
        for (final Map<String, Object> fileinfo : files) {
            final String file_id = fileinfo.get("file_id").toString();
            final String description = (String) fileinfo.get("description");
            /* This was the old way to get the game_id */
            // String game_id = null;
            // final String content_preview_link = (String) entries.get("content_preview_link");
            // if (content_preview_link != null) {
            // /* That's a little sketchy as they do not provide a field for this 'game_id' but we need that! */
            // game_id = new Regex(content_preview_link, "nexus-files-meta/(\\d+)/").getMatch(0);
            // }
            final int category_id = ((Number) fileinfo.get("category_id")).intValue();
            String category_name = (String) fileinfo.get("category_name");
            if (StringUtils.isEmpty(category_name)) {
                /* Fallback/Workaround */
                category_name = apiCategoryIDToString(category_id);
            }
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(game_domain_name + " - " + mod_name + " - " + category_name);
            if (!StringUtils.isEmpty(mod_description)) {
                fp.setComment(mod_description);
            }
            final DownloadLink link = createDownloadlink(generatePluginPatternMatcher(game_id, file_id));
            link.setContentUrl(generateContentURL(game_domain_name, mod_id, file_id));
            NexusmodsCom.setFileInformationAPI(link, fileinfo, game_domain_name, mod_id, file_id);
            link._setFilePackage(fp);
            /* Important! These properties are especially required for all API requests! */
            link.setProperty(NexusmodsCom.PROPERTY_game_domain_name, game_domain_name);
            link.setProperty(NexusmodsCom.PROPERTY_game_id, game_id);
            link.setProperty(NexusmodsCom.PROPERTY_mod_id, mod_id);
            link.setProperty(NexusmodsCom.PROPERTY_file_id, file_id);
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
        final NexusmodsCom plugin = (NexusmodsCom) this.getNewPluginForHostInstance(this.getHost());
        br.getPage(contenturl);
        if (NexusmodsCom.isOfflineWebsite(br)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (plugin.isLoginRequiredWebsite(br)) {
            throw new AccountRequiredException();
        } else if (br.containsHTML(">\\s*This mod contains adult content")) {
            /* 2019-10-02: Account required + setting has to be enabled in account to be able to see/download such content! */
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
        br2.getPage("/" + game_domain_name + "/mods/" + mod_id + "?tab=files");
        final String[] categoryNames = br2.getRegex("<div class=\"file-category-header\">\\s*<h2>\\s*([^>]+)\\s*</h2>").getColumn(0);
        final String[] downloadCategoriesHTMLs = br2.getRegex("<dd[^>]*data-id=\"\\d+\"[^>]*>.*?</div>\\s*</d(t|d)>").getColumn(-1);
        if (downloadCategoriesHTMLs == null || downloadCategoriesHTMLs.length == 0) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        int index = -1;
        for (final String downloadTypeHTML : downloadCategoriesHTMLs) {
            index++;
            String category_name = null;
            final int downloadIndex = br2.getRequest().getHtmlCode().indexOf(downloadTypeHTML);
            for (String categoryName : categoryNames) {
                final Matcher m = br2.getRegex("<div class=\"file-category-header\">\\s*<h2>\\s*" + Pattern.quote(categoryName) + "\\s*</h2>").getMatcher();
                if (m.find() && downloadIndex > m.end()) {
                    category_name = categoryName;
                }
            }
            ret.addAll(websiteCrawlFiles(br2, downloadTypeHTML, category_name, index, mod_id, game_id, game_domain_name, mod_name));
        }
        if (ret.isEmpty()) {
            /*
             * Effectively, items are only downloadable via (paid) account so if the crawler fails, let's suppress that and instead ask the
             * user to retry again with account -> User can then use the API so then the crawler will work fine.
             */
            // throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find game_id");
            throw new AccountRequiredException();
        }
        String description = br2.getRegex("<meta name=\"description\" content=\"([^\"]+)").getMatch(0);
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

    private List<DownloadLink> websiteCrawlFiles(final Browser br, final String html_source, final String category_name, final int categoryIndex, final String mod_id, final String game_id, final String game_domain_name, final String mod_name) throws PluginException {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final FilePackage fp = FilePackage.getInstance();
        if (category_name != null) {
            fp.setName(game_domain_name + " - " + mod_name + " - " + category_name);
        } else {
            fp.setName(game_domain_name + " - " + mod_name);
        }
        final String currentPath = game_domain_name + "/" + mod_name + "/" + category_name;
        final String extDefault = ".zip";
        int position = 0;
        for (final String html : new String[] { html_source }) {
            String file_id = new Regex(html, "\\?id=(\\d+)").getMatch(0);
            if (file_id == null) {
                file_id = new Regex(html, "data-id=\"(\\d+)\"").getMatch(0);
            }
            if (file_id == null) {
                logger.info("file_id is null, skipping html: " + html);
                continue;
            }
            position++;
            final String filename = new Regex(html, "data-url=\"([^\"]+)\"").getMatch(0);
            final DownloadLink link = createDownloadlink(generatePluginPatternMatcher(game_id, file_id));
            link.setContentUrl(generateContentURL(game_domain_name, mod_id, file_id));
            final String filesizeStr = br.getRegex("<dt id\\s*=\\s*\"file-expander-header-" + file_id + "\"[^>]*data-size\\s*=\\s*\"(\\d+)\"[^>]*>").getMatch(0);
            if (filesizeStr != null) {
                link.setDownloadSize(SizeFormatter.getSize(filesizeStr + "kb"));
            }
            String name;
            if (filename != null) {
                name = file_id + "_" + Encoding.htmlOnlyDecode(filename);
            } else {
                name = fp.getName() + "_" + file_id + "_" + position;
            }
            name = correctOrApplyFileNameExtension(name, extDefault, null);
            link.setName(name);
            link.setAvailable(true);
            link.setRelativeDownloadFolderPath(currentPath);
            link._setFilePackage(fp);
            /* Important! These properties are especially required for all API requests! */
            link.setProperty(NexusmodsCom.PROPERTY_game_domain_name, game_domain_name);
            link.setProperty(NexusmodsCom.PROPERTY_mod_id, mod_id);
            link.setProperty(NexusmodsCom.PROPERTY_file_id, file_id);
            if (category_name != null) {
                /* Every category goes into a subfolder */
                link.setRelativeDownloadFolderPath(game_domain_name + "/" + category_name);
            }
            ret.add(link);
        }
        return ret;
    }

    public static String generateContentURL(final String game_domain_name, final String mod_id, final String file_id) {
        return String.format("https://www.nexusmods.com/%s/mods/%s?tab=files&file_id=%s", game_domain_name, mod_id, file_id);
    }

    public static String generatePluginPatternMatcher(final String game_id, final String file_id) {
        return String.format("https://www.nexusmods.com/Core/Libs/Common/Widgets/DownloadPopUp?id=%s&nmm=0&game_id=%s&source=FileExpander", file_id, game_id);
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
