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
package jd.plugins.hoster;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;

import org.appwork.net.protocol.http.HTTPConstants;
import org.jdownloader.downloader.text.TextDownloader;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 50867 $", interfaceVersion = 3, names = {}, urls = {})
public class WunschgutscheinCouponDownload extends PluginForHost {
    public WunschgutscheinCouponDownload(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static final String PROPERTY_REDEEM_URL    = "redeem_url";
    public static final String PROPERTY_REDEEM_ID     = "redeem_id";
    public static final String PROPERTY_SHOP_ID       = "shop_id";
    public static final String PROPERTY_VALUE_IN_CENT = "value_in_cent";
    public static final String PROPERTY_CODE          = "code";
    public static final String PROPERTY_PIN           = "pin";
    public static final String PROPERTY_SERIAL_NUMBER = "serialNumber";
    public static final String PROPERTY_DOWNLOADLINK  = "downloadlink";
    public static final String PROPERTY_INDEX         = "index";
    public static final String PROPERTY_INDEX_MAX     = "index_max";

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_USER_AGENT, "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/134.0.0.0 Safari/537.36");
        // br.getHeaders().put("sec-ch-ua", "\"Google Chrome\";v=\"134\", \"Chromium\";v=\"134\", \"Not_A Brand\";v=\"24\"");
        br.getHeaders().put("sec-ch-ua-mobile", "?0");
        br.getHeaders().put("sec-ch-ua-platform", "\"Windows\"");
        br.getHeaders().put("Accept-Encoding", "gzip, deflate, br");
        br.getHeaders().put("Accept-Language", "de-DE,de");
        br.getHeaders().put("sec-fetch-dest", "empty");
        br.getHeaders().put("sec-fetch-mode", "cors");
        br.getHeaders().put("sec-fetch-site", "same-origin");
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost();
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
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
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/v2/redeem/merchant-code/pdf/([a-f0-9]{32})/(\\d+)");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            return this.getHost() + "://" + fid + "/index/" + link.getStringProperty(PROPERTY_INDEX);
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return false;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 1;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        if (!link.isNameSet()) {
            /* Fallback */
            link.setName(this.getFID(link));
        }
        this.setFilename(link);
        return AvailableStatus.TRUE;
    }

    public void setFilename(final DownloadLink link) {
        final String pdf_downloadlink = link.getStringProperty(PROPERTY_DOWNLOADLINK);
        final String redeem_id = link.getStringProperty(PROPERTY_REDEEM_ID);
        final int index = link.getIntegerProperty(PROPERTY_INDEX, 0);
        final int index_max = link.getIntegerProperty(PROPERTY_INDEX_MAX, 0);
        String title = link.getStringProperty(PROPERTY_SHOP_ID) + "_" + redeem_id + link.getIntegerProperty(PROPERTY_VALUE_IN_CENT) / 100;
        title += "_" + link.getStringProperty(PROPERTY_VALUE_IN_CENT);
        if (index_max > 0) {
            title += "_" + index;
        }
        if (pdf_downloadlink != null) {
            link.setFinalFileName(title + ".pdf");
        } else {
            link.setFinalFileName(title + ".txt");
        }
        if (!isSelfhostedDownloadableItem(link)) {
            try {
                link.setDownloadSize(this.getTextToWrite(link).getBytes("UTF-8").length);
            } catch (UnsupportedEncodingException e) {
                e.printStackTrace();
            }
        }
    }

    private boolean isSelfhostedDownloadableItem(final DownloadLink link) {
        final String downloadurl = link.getStringProperty(PROPERTY_DOWNLOADLINK);
        if (downloadurl == null) {
            return false;
        }
        return isSelfhostedDownloadableURL(downloadurl);
    }

    public static boolean isSelfhostedDownloadableURL(final String url) {
        if (url == null) {
            return false;
        }
        if (url.matches("(?i)https?://api\\.wunschgutschein\\.(at|de)/v2/redeem/merchant-code/pdf/([a-f0-9]{32})/(\\d+)")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link);
    }

    private void handleDownload(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        if (isSelfhostedDownloadableItem(link)) {
            /* Download PDF file */
            final String pdf_downloadlink = link.getStringProperty(PROPERTY_DOWNLOADLINK);
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, pdf_downloadlink, this.isResumeable(link, null), this.getMaxChunks(link, null));
            if (dl.getConnection().getResponseCode() == 404) {
                /* E.g. {"errors":[{"code":"RDMx1","message":"Redeem link could not be found","context":[]}]} */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            this.handleConnectionErrors(br, dl.getConnection());
        } else {
            /* Write text to file */
            final String text = getTextToWrite(link);
            dl = new TextDownloader(this, link, text);
        }
        dl.startDownload();
    }

    private String getTextToWrite(final DownloadLink link) {
        final String downloadlink = link.getStringProperty(PROPERTY_DOWNLOADLINK);
        final StringBuilder sb = new StringBuilder();
        sb.append("Link: " + link.getStringProperty(PROPERTY_REDEEM_URL));
        if (downloadlink != null) {
            sb.append("\nDownloadlink: " + downloadlink);
        }
        sb.append("\nRedeemID: " + link.getStringProperty(PROPERTY_REDEEM_ID));
        sb.append("\nShopID: " + link.getStringProperty(PROPERTY_SHOP_ID));
        sb.append("\nWert: " + String.format("%.2f", link.getIntegerProperty(PROPERTY_VALUE_IN_CENT) / 100.0));
        final String code = link.getStringProperty(PROPERTY_CODE);
        if (code != null) {
            sb.append("\nCode: " + code);
        }
        final String pin = link.getStringProperty(PROPERTY_PIN);
        if (pin != null) {
            sb.append("\nPIN: " + pin);
        }
        final String serialNumber = link.getStringProperty(PROPERTY_SERIAL_NUMBER);
        if (serialNumber != null) {
            sb.append("\nSeriennummer: " + serialNumber);
        }
        return sb.toString();
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        return false;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return 1;
    }
}