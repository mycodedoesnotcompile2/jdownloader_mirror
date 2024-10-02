//    jDownloader - Downloadmanager
//    Copyright (C) 2009  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.hoster;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.plugins.components.antiDDoSForHost;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.config.Property;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.components.MultiHosterManagement;
import jd.plugins.components.PluginJSonUtils;

@HostPlugin(revision = "$Revision: 49894 $", interfaceVersion = 2, names = { "multishare.cz" }, urls = { "https?://[\\w\\.]*?multishare\\.cz/((?:[a-z]{2}/)?stahnout/[0-9]+/|html/mms_process\\.php\\?(&?u_ID=\\d+|&?u_hash=[a-f0-9]+|(&?link=https?%3A%2F%2F[^&\\?]+|&?fid=\\d+)){3})" })
public class MultiShareCz extends antiDDoSForHost {
    public MultiShareCz(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www.multishare.cz/cenik/");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.getHeaders().put("Accept-Language", "en-gb, en;q=0.9");
        br.getHeaders().put("Accept-Encoding", "json");
        br.getHeaders().put("User-Agent", "JDownloader");
        br.setCookie(getHost(), "lang", "en");
        br.setCustomCharset("utf-8");
        br.setFollowRedirects(true);
        return br;
    }

    private final String                 mhLink     = "(?i)https?://[\\w\\.]*?multishare\\.cz/html/mms_process\\.php\\?.+";
    private Account                      currentAcc = null;
    private static MultiHosterManagement mhm        = new MultiHosterManagement("multishare.cz");

    private void setConstants(final Account acc) {
        this.currentAcc = acc;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST };
    }

    /**
     * They only have premium accounts. When you register as a free user you get 5 GB of traffic - after that you ll simply have an empty
     * premium account.
     */
    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        setConstants(account);
        final AccountInfo ai = new AccountInfo();
        final Map<String, Object> userinfo = login(account);
        account.setConcurrentUsePossible(true);
        final Object trafficleftO = userinfo.get("credit");
        final String trafficleftStr = PluginJSonUtils.getJsonValue(br, "credit");
        long trafficleftBytes = 0;
        if (trafficleftO instanceof Number) {
            trafficleftBytes = ((Number) trafficleftBytes).longValue() * 1024 * 1024;
        } else if (trafficleftO instanceof String) {
            /* 1 credit = 1 MB */
            trafficleftBytes = (long) Double.parseDouble(trafficleftStr) * (1024 * 1024l);
        }
        ai.setTrafficLeft(trafficleftBytes);
        if (trafficleftBytes > 0) {
            account.setType(AccountType.PREMIUM);
        } else {
            account.setType(AccountType.FREE);
        }
        getPage("https://www." + this.getHost() + "/api/?sub=supported-hosters");
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final List<String> supportedhosts = (List<String>) entries.get("server");
        ai.setMultiHostSupport(this, supportedhosts);
        return ai;
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/kontakt/";
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        if (link.getPluginPatternMatcher().matches(mhLink)) {
            dlGeneratedMhLink(link);
            return;
        }
        requestFileInformation(link);
        String dllink = "https://www." + this.getHost() + "/html/download_free.php?ID=" + getFuid(link);
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, false, 1);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            int timesFailed = link.getIntegerProperty("timesfailedmultisharecz_unknowndlerrorfree", 0);
            link.getLinkStatus().setRetryCount(0);
            if (timesFailed <= 2) {
                logger.info("multishare.cz: Unknown download error -> Retrying");
                timesFailed++;
                link.setProperty("timesfailedmultisharecz_unknowndlerrorfree", timesFailed);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Unknown download error");
            } else {
                logger.info("multishare.cz: Unknown download error -> Plugin is broken");
                link.setProperty("timesfailedmultisharecz_unknowndlerrorfree", Property.NULL);
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        dl.startDownload();
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        setConstants(account);
        if (link.getPluginPatternMatcher().matches(mhLink)) {
            dlGeneratedMhLink(link);
            return;
        }
        requestFileInformation(link);
        login(account);
        getPage(link.getPluginPatternMatcher());
        String dllink = "https://www." + account.getHoster() + "/html/download_premium.php?ID=" + getFuid(link);
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, true, 1);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            if (br.containsHTML("Soubor na zdrojovém serveru pravděpodobně neexistuje")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            int timesFailed = link.getIntegerProperty("timesfailedmultisharecz_unknowndlerrorpremium", 0);
            link.getLinkStatus().setRetryCount(0);
            if (timesFailed <= 2) {
                logger.info("multishare.cz: Unknown download error -> Retrying");
                timesFailed++;
                link.setProperty("timesfailedmultisharecz_unknowndlerrorpremium", timesFailed);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Unknown download error");
            } else {
                logger.info("multishare.cz: Unknown download error -> Plugin is broken");
                link.setProperty("timesfailedmultisharecz_unknowndlerrorpremium", Property.NULL);
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        dl.startDownload();
    }

    private void dlGeneratedMhLink(final DownloadLink link) throws Exception {
        requestFileInformationMh(link);
        handleDl(link, br.getURL(), 1);
    }

    public AvailableStatus requestFileInformationMh(final DownloadLink link) throws PluginException, IOException {
        final Browser brc = br.cloneBrowser();
        URLConnectionAdapter con = null;
        try {
            con = brc.openGetConnection(link.getPluginPatternMatcher());
            if (!this.looksLikeDownloadableContent(con)) {
                br.followConnection(true);
                link.setAvailable(false);
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final String serverFilename = Plugin.getFileNameFromConnection(con);
            if (link.getFinalFileName() == null && serverFilename != null) {
                link.setFinalFileName(serverFilename);
            }
            if (con.getCompleteContentLength() > 0) {
                if (con.isContentDecoded()) {
                    link.setDownloadSize(con.getCompleteContentLength());
                } else {
                    link.setVerifiedFileSize(con.getCompleteContentLength());
                }
            }
            link.setAvailable(true);
            return AvailableStatus.TRUE;
        } finally {
            try {
                /* make sure we close connection */
                con.disconnect();
            } catch (final Throwable e) {
            }
        }
    }

    /** no override to keep plugin compatible to old stable */
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        mhm.runCheck(account, link);
        this.setBrowserExclusive();
        /* login to get u_ID and u_HASH */
        getPage("https://www." + account.getHoster() + "/api/?sub=download-link&login=" + Encoding.urlEncode(account.getUser()) + "&password=" + Encoding.urlEncode(account.getPass()) + "&link=" + Encoding.urlEncode(link.getDownloadURL()));
        this.checkErrors(br);
        // TODO: Add json parser
        final String dllink = PluginJSonUtils.getJsonValue(br, "link");
        final String maxChunks = PluginJSonUtils.getJsonValue(br, "chunks");
        if (dllink == null) {
            mhm.handleErrorGeneric(account, link, "Failed to find final downloadurl", 50);
        }
        int chunks = 1;
        if (maxChunks != null) {
            try {
                final int tmp = Integer.parseInt(maxChunks);
                if (tmp > 1) {
                    chunks = -tmp;
                }
            } catch (final Throwable e) {
            }
        }
        handleDl(link, dllink, chunks);
    }

    private void checkErrors(final Browser br) throws AccountInvalidException {
        final String error = br.getRegex("^ERR: (.+)").getMatch(0);
        if (error != null) {
            throw new AccountInvalidException(Encoding.htmlDecode(error).trim());
        }
    }

    private void handleDl(final DownloadLink link, final String dllink, int chunks) throws Exception {
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, true, chunks);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            /*
             * download is not contentdisposition, so remove this host from premiumHosts list
             */
            br.followConnection(true);
            if (br.getURL().contains("typ=nedostatecny-kredit")) {
                logger.info("No traffic available -> Temporarily disabling account");
                throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_TEMP_DISABLE);
            } else if (br.containsHTML("Soubor na zdrojovém serveru pravděpodobně neexistuje")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (br.containsHTML("<h1>\\s*(Chyba stahování|Download error)\\s*</h1>") || br.getURL().contains("/chyba-stahovani")) {
                /* Check if downloadlink is internal (belongs to this MOCH and is not an external website) */
                if (link.getPluginPatternMatcher().contains("multishare.cz/")) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Unknown server error", 5 * 60 * 1000l);
                } else {
                    mhm.handleErrorGeneric(this.currentAcc, link, "Unknown download error 1", 50);
                }
            }
            logger.warning("Received unknown html code instead of file -> Unknown error");
            mhm.handleErrorGeneric(this.currentAcc, link, "Unknown download error 2", 50);
        }
        dl.startDownload();
    }

    private Map<String, Object> login(final Account account) throws Exception {
        this.setBrowserExclusive();
        getPage("https://www." + getHost() + "/api/?sub=account-details&login=" + Encoding.urlEncode(account.getUser()) + "&password=" + Encoding.urlEncode(account.getPass()));
        this.checkErrors(br);
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        return entries;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        if (link.getPluginPatternMatcher().matches(mhLink)) {
            return requestFileInformationMh(link);
        }
        this.setBrowserExclusive();
        // support English page as its easier to understand for all our programmers.
        getPage("https://www." + this.getHost() + "/en/stahnout/" + getFuid(link) + "/");
        // need to find the new error response in English!!
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("(Požadovaný soubor neexistuje|Je možné, že byl již tento soubor vymazán uploaderem nebo porušoval autorská práva)")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String title = "<title>Stáhnout (.*?)\\s*\\(([\\d\\.\\, a-zA-Z]+)\\)\\s*\\|\\s*MultiShare.cz</title>";
        String filename = br.getRegex(title).getMatch(0);
        if (filename == null) {
            filename = br.getRegex("<h1>(.*?)</h1>").getMatch(0);
        }
        String filesize = br.getRegex(title).getMatch(1);
        if (filesize == null) {
            filesize = br.getRegex("<span class=\"download-file-size\"><span>([\\d\\.\\, a-zA-Z]+)</span>").getMatch(0);
        }
        if (filename == null) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        link.setName(filename.trim());
        if (filesize != null) {
            filesize = Encoding.htmlOnlyDecode(filesize);
            link.setDownloadSize(SizeFormatter.getSize(filesize));
        }
        return AvailableStatus.TRUE;
    }

    private String getFuid(final DownloadLink link) {
        final String fuid = new Regex(link.getPluginPatternMatcher(), "/(\\d+)/?$").getMatch(0);
        return fuid;
    }

    @Override
    public boolean canHandle(DownloadLink link, Account account) throws Exception {
        if (account == null) {
            if (link.getPluginPatternMatcher().matches(mhLink)) {
                // multihoster link
                return true;
            }
            /* without account its not possible to download the link */
            return false;
        } else {
            mhm.runCheck(account, link);
            return true;
        }
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}