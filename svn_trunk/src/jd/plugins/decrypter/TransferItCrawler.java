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
import java.util.Random;

import org.appwork.storage.TypeRef;
import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.DebugMode;
import org.jdownloader.scripting.JavaScriptEngineFactory;
import org.jdownloader.settings.GeneralSettings;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 51669 $", interfaceVersion = 3, names = {}, urls = {})
public class TransferItCrawler extends PluginForDecrypt {
    public TransferItCrawler(PluginWrapper wrapper) {
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
        /* transfer.it is a service powered by MEGA -> mega.nz */
        ret.add(new String[] { "transfer.it" });
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
            ret.add("https?://" + buildHostsPatternPart(domains) + "/t/([a-zA-Z0-9]{12})");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        if (!DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            logger.info("Unfinished plugin -> Doing nothing in Stable JDownloader version");
            return ret;
        }
        final String folder_id = new Regex(param.getCryptedUrl(), this.getSupportedLinks()).getMatch(0);
        br.getPage(param.getCryptedUrl());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /**
         * URL: see "apipath" in: https://st.transfer.it/js/BDL-3_7ae552ce54d47cc67cbee9b5e560beb791e67c69a1888fea49827b5a4f02b1a5.js <br>
         * And: https://st.transfer.it/js/transferit-group1_dc1bee43e28bd7d4eedc513745c35bf4ff42d778d714e185868befbc418aca81.js
         */
        final String id = generateNineDigitNumber();
        br.postPageRaw("https://bt7.api.mega.co.nz/cs?id=" + id + "&v=3&lang=en&domain=transferit&x=" + folder_id + "&bc=1", "[{\"a\":\"xi\",\"xh\":\"" + folder_id + "\"}]");
        // TODO: Add decryption
        final List<Map<String, Object>> items_root = (List<Map<String, Object>>) this.checkErrorsAPI(br);
        final List<Map<String, Object>> items = (List<Map<String, Object>>) JavaScriptEngineFactory.walkJson(items_root, "{0}/f");
        /* First item = Folder information */
        final Map<String, Object> folder_info = items.remove(0);
        String folderTitle = folder_info.get("a").toString();
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(folderTitle);
        fp.setPackageKey("transfer_it_mega://folder/" + folder_id);
        for (final Map<String, Object> item : items) {
            // TODO: Add decryption
            final String filename = item.get("a").toString();
            final long filesize = ((Number) item.get("s")).longValue();
            final Number lastModifiedDate = (Number) item.get("ts");
            final DownloadLink file = this.createDownloadlink("TODO");
            file.setFinalFileName(filename);
            file.setVerifiedFileSize(filesize);
            if (lastModifiedDate != null && JsonConfig.create(GeneralSettings.class).isUseOriginalLastModified()) {
                /* set desired/original lastModified timestamp */
                file.setLastModifiedTimestamp(lastModifiedDate.longValue() * 1000);
            }
            file.setAvailable(true);
            ret.add(file);
        }
        return ret;
    }
    // public static String generateNineDigitNumber() {
    // Random random = new Random();
    // // Generate a number between 100000000 and 999999999 (inclusive)
    // int number = 100000000 + random.nextInt(900000000);
    // return String.valueOf(number);
    // }

    private Object checkErrorsAPI(final Browser br) throws PluginException {
        final Object object = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.OBJECT);
        if (!(object instanceof List)) {
            return object;
        }
        final List<Object> ressourcelist = (List<Object>) object;
        if (ressourcelist.size() != 1) {
            return ressourcelist;
        }
        // TODO: Add error handling e.g. code "-8" means "item not found", possible use the handling from MegaNz plugin.
        final Object errorO = ressourcelist.get(0);
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "ErrorCode " + errorO);
    }

    public static String generateNineDigitNumber() {
        String[] digits = { "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" };
        Random random = new Random();
        StringBuilder result = new StringBuilder();
        for (int i = 0; i < 9; i++) {
            int index = random.nextInt(10);
            result.append(digits[index]);
        }
        return result.toString();
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}
