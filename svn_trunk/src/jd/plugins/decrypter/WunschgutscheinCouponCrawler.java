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
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.WunschgutscheinCouponDownload;

@DecrypterPlugin(revision = "$Revision: 50867 $", interfaceVersion = 3, names = {}, urls = {})
public class WunschgutscheinCouponCrawler extends PluginForDecrypt {
    public WunschgutscheinCouponCrawler(PluginWrapper wrapper) {
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
        ret.add(new String[] { "wunschgutschein.de" });
        ret.add(new String[] { "wunschgutschein.at" });
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
            ret.add("https?://app\\." + buildHostsPatternPart(domains) + "/merchantcode/([a-f0-9]{32})");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final String contenturl = param.getCryptedUrl();
        final String redeem_id = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        br.getPage(getAPIBase(contenturl) + "/redeem/link/" + redeem_id);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final WunschgutscheinCouponDownload hosterplugin = (WunschgutscheinCouponDownload) this.getNewPluginForHostInstance(this.getHost());
        Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        List<Map<String, Object>> merchantCodes = (List<Map<String, Object>>) entries.get("merchantCodes");
        String comment = "Code war bereits geöffnet";
        if (merchantCodes.isEmpty()) {
            /* Gutscheincode "Entschlüsseln" */
            logger.info("Opening RedeemHash for the first time");
            if (this.isAbort()) {
                throw new InterruptedException();
            }
            br.postPageRaw(getAPIBase(contenturl) + "/redeem/merchantcode", String.format("{\"redeemLinkToken\":\"%s\"}", redeem_id));
            final Object responseO = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.OBJECT);
            if (!(responseO instanceof List)) {
                entries = (Map<String, Object>) responseO;
                final Object errors = entries.get("errors");
                if (errors != null) {
                    /* Treat any error as offline error */
                    logger.info("Found error(s): " + errors);
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            merchantCodes = (List<Map<String, Object>>) responseO;
            comment = "Code wurde soeben geöffnet";
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        int index = -1;
        for (final Map<String, Object> merchantCode : merchantCodes) {
            index++;
            final String url = (String) merchantCode.get("link");
            if (url == null || WunschgutscheinCouponDownload.isSelfhostedDownloadableURL(url)) {
                final DownloadLink result = this.createDownloadlink(contenturl);
                result.setDefaultPlugin(hosterplugin);
                result.setHost(this.getHost());
                if (url != null) {
                    result.setProperty(WunschgutscheinCouponDownload.PROPERTY_DOWNLOADLINK, url);
                }
                result.setProperty(WunschgutscheinCouponDownload.PROPERTY_VALUE_IN_CENT, merchantCode.get("valueInCent"));
                result.setProperty(WunschgutscheinCouponDownload.PROPERTY_CODE, merchantCode.get("code"));
                result.setProperty(WunschgutscheinCouponDownload.PROPERTY_PIN, merchantCode.get("pin"));
                result.setProperty(WunschgutscheinCouponDownload.PROPERTY_SERIAL_NUMBER, merchantCode.get("serialNumber"));
                result.setProperty(WunschgutscheinCouponDownload.PROPERTY_INDEX, index);
                result.setAvailable(true);
                ret.add(result);
            } else {
                /* Externally hosted voucher e.g. ecard.cadooz.com */
                final DownloadLink result = this.createDownloadlink(url);
                ret.add(result);
            }
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(entries.get("shopId") + "_" + entries.get("valueInCent") + "_" + redeem_id);
        fp.setPackageKey("wunschgutschein://redeemhash/" + redeem_id);
        for (final DownloadLink result : ret) {
            result.setContentUrl(contenturl);
            result.setProperty(WunschgutscheinCouponDownload.PROPERTY_REDEEM_URL, contenturl);
            result.setProperty(WunschgutscheinCouponDownload.PROPERTY_REDEEM_ID, redeem_id);
            result.setProperty(WunschgutscheinCouponDownload.PROPERTY_SHOP_ID, entries.get("shopId"));
            result.setProperty(WunschgutscheinCouponDownload.PROPERTY_INDEX_MAX, index);
            result.setComment(comment);
            result._setFilePackage(fp);
            hosterplugin.setFilename(result);
        }
        return ret;
    }

    public String getAPIBase(final String sourceURL) {
        final boolean isAT = StringUtils.containsIgnoreCase(sourceURL, "wunschgutschein.at");
        if (isAT) {
            return "https://app.wunschgutschein.at/api/v2";
        } else {
            return "https://app.wunschgutschein.de/api/v2";
        }
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        /* Limit to 1 to try to avoid running into any kind of rate limits. */
        return 1;
    }
}
