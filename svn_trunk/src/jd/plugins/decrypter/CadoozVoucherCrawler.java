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

import org.appwork.utils.Regex;

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
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 51668 $", interfaceVersion = 3, names = {}, urls = {})
public class CadoozVoucherCrawler extends PluginForDecrypt {
    public CadoozVoucherCrawler(PluginWrapper wrapper) {
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
        ret.add(new String[] { "cadooz.com" });
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
            ret.add("https?://ecard\\." + buildHostsPatternPart(domains) + "/frontend/(?:ecard\\.do\\?id=|barcode\\.do\\?hndshk=)([A-Za-z0-9]{20})");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final String content_id = new Regex(param.getCryptedUrl(), this.getSupportedLinks()).getMatch(0);
        final String contenturl = "https://ecard.cadooz.com/frontend/ecard.do?id=" + content_id;
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getRequest().getHtmlCode().length() < 300) {
            /**
             * E.g. plain text error "Die übergebene Ecard ist im System nicht bekannt. Bitte prüfen Sie die übergebene URL auf
             * Vollständigkeit. Eventuell ist der Link durch einen Zeilenumbruch unbrauchbar geworden." <br>
             * e.g. >Leider kam es zu einem Problem beim Versuch ihren PDF Gutschein anzuzeigen. Bitte versuchen sie es später erneut. Falls
             * das Problem weiter besteht, wenden sie sich bitte an den cadooz Kundendienst.
             */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String[] htmls = br.getRegex("<div class=\"ecard[^\"]+\">(.*?)--></div>").getColumn(0);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        String lastShopName = null;
        if (htmls != null && htmls.length > 0) {
            int index = 0;
            int skipped_un_downloadable_items = 0;
            for (final String html : htmls) {
                final String shopName = new Regex(html, "class=\"ecard-header-title\"[^>]*>([^<]+)</h3>").getMatch(0);
                final String download_url = new Regex(html, "\"(https?://[^\"]+)\"[^<]+title=\"Hier PDF anzeigen").getMatch(0);
                final String value = new Regex(html, ">\\s*Wert:\\s*</span>\\s*(\\d+,\\d{2})").getMatch(0);
                String valueMinimal = null;
                if (value != null) {
                    valueMinimal = value.replace(",00", "");
                }
                final String validUntilDate = new Regex(html, " Gültig bis:\\s*</span>\\s*(\\d{4}\\.\\d{2}\\.\\d{2})").getMatch(0);
                if (download_url == null) {
                    skipped_un_downloadable_items++;
                    continue;
                }
                final DownloadLink link = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(download_url));
                String filename = shopName + "_" + valueMinimal;
                if (htmls.length > 1) {
                    /* Add position to filename to ensure unique filenames if we got more than 1 item. */
                    filename += "_" + (index + 1);
                }
                filename += ".pdf";
                link.setFinalFileName(filename);
                /* Ensure that name does not change when item is reset by user. */
                link.setProperty(DirectHTTP.FIXNAME, filename);
                /* HEAD request is not possible -> Allow only GET */
                link.setProperty(DirectHTTP.PROPERTY_REQUEST_TYPE, "GET");
                if (validUntilDate != null) {
                    link.setComment("Gültig bis: " + validUntilDate);
                }
                link.setAvailable(true);
                ret.add(link);
                lastShopName = shopName;
                index++;
            }
        }
        if (ret.isEmpty()) {
            if (!br.containsHTML("class=\"ecard-data-code lazy\"")) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            /* Text based vouchers -> Download html page */
            final DownloadLink text = this.createDownloadlink(br.getURL() + ".jdeatme");
            text.setDownloadSize(br.getRequest().getHtmlCode().getBytes("UTF-8").length);
            text.setAvailable(true);
            ret.add(text);
        }
        final FilePackage fp = FilePackage.getInstance();
        if (lastShopName != null) {
            fp.setName(lastShopName);
        } else {
            /* Fallback */
        }
        fp.setPackageKey("cadooz://ecard/" + content_id);
        fp.addLinks(ret);
        return ret;
    }
}
