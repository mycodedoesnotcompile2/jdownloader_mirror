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

import org.appwork.storage.TypeRef;
import org.appwork.utils.formatter.SizeFormatter;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.AccountFilter;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.requests.PostRequest;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterException;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.OneFichierCom;
import jd.utils.JDUtilities;

@DecrypterPlugin(revision = "$Revision: 51634 $", interfaceVersion = 3, names = {}, urls = {})
public class OneFichierComFolder extends PluginForDecrypt {
    public OneFichierComFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    private Browser prepareBrowserWebsite(final Browser br) {
        return OneFichierCom.prepareBrowserWebsite(br);
    }

    private Browser prepareBrowserAPI(final Browser br, final Account account) throws Exception {
        return OneFichierCom.prepareBrowserAPI(br, account);
    }

    @Override
    public void init() {
        OneFichierCom.setRequestIntervalLimits();
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(OneFichierCom.getPluginDomains());
    }

    public static String[] getAnnotationNames() {
        return OneFichierCom.getAnnotationNames();
    }

    /**
     * returns the annotation pattern array
     *
     */
    public static String[] getAnnotationUrls() {
        final String host = "https?://(?:www\\.)?" + buildHostsPatternPart(OneFichierCom.getPluginDomains().get(0));
        return new String[] { host + "/(?:(?:[a-z]{2})/)?dir/([A-Za-z0-9]+)" };
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        Account account = null;
        /**
         * 2019-04-05: Folder support via API does not work (serverside) as it requires us to have the internal folder-IDs which we do not
         * have! </br>
         * Basically their folder API call is only for internal folders of the current user -> Not useful for us! See also:
         * https://1fichier.com/api.html
         */
        final boolean internal_allow_api_usage_in_crawler = false;
        if (internal_allow_api_usage_in_crawler) {
            List<Account> filteredAccounts = AccountController.getInstance().listAccounts(new AccountFilter(this.getHost()).setEnabled(true).setValid(true).setAccountTypes(AccountType.PREMIUM).setMaxResultsNum(1));
            if (!filteredAccounts.isEmpty()) {
                account = filteredAccounts.get(0);
            }
        }
        if (account != null && OneFichierCom.canUseAPI(account) && internal_allow_api_usage_in_crawler) {
            /* Use premium API */
            return crawlAPI(param, account);
        } else {
            /* Use website */
            return crawlWebsite(param);
        }
    }

    private ArrayList<DownloadLink> crawlAPI(final CryptedLink param, final Account account) throws Exception {
        if (param == null) {
            throw new IllegalArgumentException();
        } else if (account == null) {
            throw new IllegalArgumentException();
        }
        final String folderID = new Regex(param.toString(), this.getSupportedLinks()).getMatch(0);
        if (folderID == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        prepareBrowserAPI(this.br, account);
        final PostRequest downloadReq = br.createJSonPostRequest(OneFichierCom.API_BASE + "/file/ls.cgi", "");
        downloadReq.setContentType("application/json");
        br.openRequestConnection(downloadReq);
        br.loadConnection(null);
        // TODO: Unfinished code
        return null;
    }

    private ArrayList<DownloadLink> crawlWebsite(final CryptedLink param) throws Exception {
        if (param == null) {
            throw new IllegalArgumentException();
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String folderID = new Regex(param.getCryptedUrl(), this.getSupportedLinks()).getMatch(0);
        final String folderURLWebsite = "https://" + this.getHost() + "/dir/" + folderID + "?lg=en";
        final String folderURLWebsiteJSON = "https://" + this.getHost() + "/dir/" + folderID + "?json=1";
        prepareBrowserWebsite(br);
        br.setLoadLimit(Integer.MAX_VALUE);
        final Browser jsonBR = br.cloneBrowser();
        jsonBR.getPage(folderURLWebsiteJSON);
        if (jsonBR.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* Access folder without API just to find foldername ... */
        br.getPage(folderURLWebsite);
        /* We prefer English but let's be prepared to parse both versions of their website, English and French. */
        final String title = br.getRegex(">(?:Shared folder|Dossier partagé)\\s*(.*?)</").getMatch(0);
        // password handling
        if ("application/json; charset=utf-8".equals(jsonBR.getHttpConnection().getContentType()) && jsonBR.getRequest().getHtmlCode().startsWith("[")) {
            /* json response -> Folder is not password protected */
            logger.info("Processing json response");
            final List<Object> ressourcelist = restoreFromString(jsonBR.getRequest().getHtmlCode(), TypeRef.LIST);
            for (final Object fileO : ressourcelist) {
                final Map<String, Object> fileInfo = (Map<String, Object>) fileO;
                final String filename = (String) fileInfo.get("filename");
                final long filesize = ((Number) fileInfo.get("size")).longValue();
                final String url = (String) fileInfo.get("link");
                final int pwProtected = ((Number) fileInfo.get("password")).intValue();
                final int accessControlLimitedLink = ((Number) fileInfo.get("acl")).intValue();
                final DownloadLink dl = createDownloadlink(url);
                dl.setFinalFileName(filename);
                dl.setVerifiedFileSize(filesize);
                if (pwProtected == 1) {
                    dl.setPasswordProtected(true);
                }
                if (accessControlLimitedLink == 1) {
                    dl.setProperty(OneFichierCom.PROPERTY_ACL_ACCESS_CONTROL_LIMIT, true);
                }
                dl.setAvailable(true);
                ret.add(dl);
            }
        } else {
            /* Parse data from html */
            final String password = handlePasswordWebsite(param, folderURLWebsite);
            final String[][] linkInfo = br.getRegex("<a href=(\"|')(" + JDUtilities.getPluginForHost("1fichier.com").getSupportedLinks() + ")\\1[^>]*>([^\r\n\t]+)</a>\\s*</td>\\s*<td[^>]*>([^\r\n\t]+)</td>").getMatches();
            if (linkInfo == null || linkInfo.length == 0) {
                throw new DecrypterException("Plugin broken");
            }
            for (String singleLinkInfo[] : linkInfo) {
                final DownloadLink dl = createDownloadlink(singleLinkInfo[1]);
                dl.setFinalFileName(Encoding.htmlDecode(singleLinkInfo[3]));
                dl.setDownloadSize(SizeFormatter.getSize(singleLinkInfo[4]));
                if (password != null) {
                    dl.setDownloadPassword(password);
                }
                dl.setAvailable(true);
                ret.add(dl);
            }
        }
        if (title != null) {
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(title);
            fp.addLinks(ret);
        }
        return ret;
    }

    private final String handlePasswordWebsite(final CryptedLink param, final String parameter) throws Exception {
        if (!browserContainsFolderPasswordForm(this.br)) {
            /* Item is not password protected */
            return null;
        }
        /* First try last crawler link password if available */
        String passCode = param.getDecrypterPassword();
        final int repeat = 3;
        for (int i = 0; i <= repeat; i++) {
            if (passCode == null) {
                passCode = getUserInput(null, param);
            }
            br.postPage(parameter + "?json=1", "pass=" + Encoding.urlEncode(passCode));
            if (browserContainsFolderPasswordForm(this.br)) {
                if (i + 1 >= repeat) {
                    throw new DecrypterException(DecrypterException.PASSWORD);
                }
                passCode = null;
                continue;
            }
            return passCode;
        }
        return null;
    }

    private boolean browserContainsFolderPasswordForm(final Browser br) {
        final Form pwform = br.getFormbyKey("pass");
        return pwform != null && this.canHandle(pwform.getAction());
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }
}