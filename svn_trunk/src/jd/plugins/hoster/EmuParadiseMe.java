//jDownloader - Downloadmanager
//Copyright (C) 2010  JD-Team support@jdownloader.org
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

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Pattern;

import javax.swing.JComponent;
import javax.swing.JLabel;

import jd.PluginWrapper;
import jd.config.ConfigContainer;
import jd.config.ConfigEntry;
import jd.config.Property;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.DefaultEditAccountPanelAPIKeyLogin;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

import org.appwork.swing.MigPanel;
import org.appwork.swing.components.ExtTextField;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.gui.InputChangedCallbackInterface;
import org.jdownloader.plugins.accounts.AccountBuilderInterface;

@HostPlugin(revision = "$Revision: 51817 $", interfaceVersion = 2, names = {}, urls = {})
public class EmuParadiseMe extends PluginForHost {
    public EmuParadiseMe(PluginWrapper wrapper) {
        super(wrapper);
        enablePremium();
        setConfigElements();
    }

    /* Config properties */
    private static final String  PROPERTY_CONFIG_SERVERS         = "servers";
    /* DownloadLink properties */
    public static final String   PROPERTY_DOWNLOAD_LINK_FILENAME = "cached_filename";
    private static AtomicInteger maxFree                         = new AtomicInteger(1);
    public static final Pattern  TYPE_ROM                        = Pattern.compile("/([^/]+)/([^/]+)/(\\d+)", Pattern.CASE_INSENSITIVE);
    public static final Pattern  TYPE_ROM_OLD                    = Pattern.compile("/roms/roms\\.php\\?gid=(\\d+)", Pattern.CASE_INSENSITIVE);
    // public static final Pattern TYPE_ROM_DOWNLOAD_OLD = Pattern.compile("/roms/get-download\\.php\\?gid=(\\d+)",
    // Pattern.CASE_INSENSITIVE);
    public static final Pattern  TYPE_DOWNLOAD                   = Pattern.compile("/([^/]+)/([^/]+)/(\\d+)-download(-(\\d+))?", Pattern.CASE_INSENSITIVE);
    public static final String   EXT_DEFAULT                     = ".zip";

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "emuparadise.me" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + TYPE_DOWNLOAD.pattern());
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    private void setConfigElements() {
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_COMBOBOX_INDEX, getPluginConfig(), PROPERTY_CONFIG_SERVERS, new String[] { "Off", "Auto", "Europe", "North America" }, "Select dl server; Note: Just because you select a region doesn't ensure data actually comes from that location!").setDefaultValue(0));
    }

    @Override
    public AccountBuilderInterface getAccountFactory(InputChangedCallbackInterface callback) {
        return new EmuParadiseMeAccountFactory(callback, this);
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/contact.php";
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        if (account != null && AccountType.PREMIUM.equals(account.getType())) {
            return -1;
        } else {
            return 1;
        }
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String rom_id = getRomID(link.getPluginPatternMatcher());
        final String file_id = getFileID(link.getPluginPatternMatcher());
        if (rom_id != null && file_id != null) {
            /* ROM with multiple files/variants */
            return this.getHost() + "://rom/" + rom_id + "/file/" + file_id;
        } else if (rom_id != null) {
            /* ROM with single file/variant */
            return this.getHost() + "://rom/" + rom_id;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getRomID(final DownloadLink link) {
        return getRomID(link.getPluginPatternMatcher());
    }

    private String getRomID(String url) {
        if (url == null) {
            return null;
        }
        Regex regex = new Regex(url, TYPE_DOWNLOAD);
        if (regex.patternFind()) {
            return regex.getMatch(2);
        }
        regex = new Regex(url, TYPE_ROM_OLD);
        if (regex.patternFind()) {
            return regex.getMatch(0);
        }
        regex = new Regex(url, TYPE_ROM);
        if (regex.patternFind()) {
            return regex.getMatch(2);
        }
        return null;
    }

    private String getFileID(final String url) {
        if (url == null) {
            return null;
        }
        Regex regex = new Regex(url, TYPE_DOWNLOAD);
        if (regex.patternFind()) {
            return regex.getMatch(4);
        }
        return null;
    }

    private String getTitleFromURL(final DownloadLink link) {
        if (link.getPluginPatternMatcher() == null) {
            return null;
        }
        String slug = new Regex(link.getPluginPatternMatcher(), TYPE_ROM).getMatch(1);
        if (slug == null) {
            return null;
        }
        return slug.replace("_", " ").trim();
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        final String rom_id = this.getRomID(link);
        final String file_id = this.getFileID(link.getPluginPatternMatcher());
        if (rom_id == null) {
            /* This should never happen */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (!link.isNameSet()) {
            final String titleFromURL = getTitleFromURL(link);
            if (titleFromURL != null) {
                link.setName(titleFromURL + EXT_DEFAULT);
            } else {
                link.setName(rom_id + EXT_DEFAULT);
            }
        }
        this.setBrowserExclusive();
        if (file_id != null) {
            /* We know that the URL we have leads to a specific file. */
            br.getPage(link.getPluginPatternMatcher());
        } else {
            br.getPage("https://www." + this.getHost() + "/roms/roms.php?gid=" + rom_id);
        }
        if (isOffline(br)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        parseFileInfo(link);
        if (!link.isAvailable()) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        return AvailableStatus.TRUE;
    }

    public void parseFileInfo(final DownloadLink link) throws PluginException {
        String filename = null;
        String filesize = null;
        if (new Regex(br.getURL(), TYPE_DOWNLOAD).patternFind()) {
            final Regex result = new Regex(br, ">\\s*Download\\s*(.*?)</a><font[^>]+>\\s*-\\s*File Size:\\s*(\\d+(?:\\.\\d+)?[KMG]{1}[B]{0,1})</font>");
            filename = result.getMatch(0);
            filesize = result.getMatch(1);
        } else {
            filename = br.getRegex("Download:\\s*<a[^>]*>([^<]+)</a>").getMatch(0);
            filesize = br.getRegex(">Size:\\s*(\\d+[^<]+)<").getMatch(0);// 2025-04-17
            if (filesize == null) {
                filesize = br.getRegex("\\s*title=\"Download.*?\\((\\d+(?:\\.\\d+)?[KMG]{1}[B]{0,1})\\)").getMatch(0);
            }
            if (filesize == null) {
                filesize = br.getRegex("\\((\\d+(?:\\.\\d+)?[KMG]{1}[B]{0,1})\\)").getMatch(0);
            }
        }
        if (filename != null) {
            filename = Encoding.htmlDecode(filename).trim();
            filename = filename.trim();
            filename = this.correctOrApplyFileNameExtension(filename, EXT_DEFAULT, null);
            link.setName(filename);
        }
        if (filesize != null) {
            filesize = Encoding.htmlDecode(filesize).trim();
            filesize = correctFilesize(filesize);
            link.setDownloadSize(SizeFormatter.getSize(filesize));
        }
    }

    public String correctFilesize(String filesizeStr) {
        if (StringUtils.endsWithCaseInsensitive(filesizeStr, "G") || StringUtils.endsWithCaseInsensitive(filesizeStr, "k") || StringUtils.endsWithCaseInsensitive(filesizeStr, "M")) {
            filesizeStr += "b";
        }
        return filesizeStr;
    }

    public boolean isOffline(final Browser br) {
        if (br.getHttpConnection().getResponseCode() == 404) {
            return true;
        } else if (br.containsHTML(">\\s*Unforunately this file has been removed")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        handleDownload(null, link, "directlink");
    }

    public void handleDownload(final Account account, final DownloadLink link, final String directlinkproperty) throws Exception, PluginException {
        String dllink = null;
        boolean unavailableWorkaroundUsed = false;
        if (dllink == null) {
            dllink = checkDirectLink(link, directlinkproperty);
            if (dllink == null) {
                dllink = br.getRegex(">\\s*Download:?\\s*<a href=\"(/[^\"]+)\"").getMatch(0);
                if (dllink == null) {
                    final String rom_id = this.getRomID(link);
                    if (!new Regex(br.getURL(), TYPE_DOWNLOAD).patternFind() && br.containsHTML(rom_id + "-download\"")) {
                        /* For older links from <= revision 51053 */
                        // TODO: Remove this after 2025-09
                        br.getPage(br.getURL() + "-download");
                    }
                    dllink = br.getRegex("\"(/roms/get\\-download\\.php[^\"]+)").getMatch(0);
                    if (dllink == null && br.containsHTML(">\\s*This game is unavailable")) {
                        logger.info("Attempting workaround for unavailable download");
                        dllink = getDirectDownloadurlViaWorkaround(this.br, link);
                        unavailableWorkaroundUsed = true;
                    }
                }
            }
            /* Check for happy hour when no account is used */
            if (account == null) {
                // TODO: Check if this happy hour still exists
                boolean happyHour = false;
                if (br.containsHTML("id=\"happy\\-hour\"")) {
                    if (br.containsHTML("src=\"/happy_hour.php\"")) {
                        final Browser clone = br.cloneBrowser();
                        clone.getPage("/happy_hour.php");
                        if (clone.containsHTML(".style\\.display=\"block\"")) {
                            happyHour = true;
                        }
                    }
                }
                if (happyHour) {
                    logger.info("Happy hour is active :)");
                    maxFree.set(2);
                } else {
                    maxFree.set(1);
                }
            }
        }
        setDownloadServerCookie();
        final String error_text_file_unavailable = "Item is not downloadable anymore?";
        if (dllink == null) {
            /* Example: /Atari_Jaguar_Emulators/Android/50 */
            throw new PluginException(LinkStatus.ERROR_FATAL, error_text_file_unavailable);
        }
        dllink = Encoding.htmlOnlyDecode(dllink);
        /* Without this the directlink won't be accepted! */
        br.getHeaders().put("Referer", br.getURL());
        try {
            dl = new jd.plugins.BrowserAdapter().openDownload(br, link, dllink, this.isResumeable(link, account), this.getMaxChunks(link, account));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                final long responsecode = dl.getConnection().getResponseCode();
                if (responsecode == 400) {
                    throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Server error 400", 2 * 60 * 1000l);
                } else if (responsecode == 503) {
                    /* Too many connections --> Happy hour is definitly not active --> Only allow 1 simultaneous download. */
                    maxFree.set(1);
                    throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Server error 503 - Too many concurrent connections - wait before starting new downloads", 1 * 60 * 1000l);
                } else {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
        } catch (final InterruptedException ie) {
            throw ie;
        } catch (final Exception e) {
            if (unavailableWorkaroundUsed) {
                throw new PluginException(LinkStatus.ERROR_FATAL, error_text_file_unavailable);
            } else {
                throw e;
            }
        }
        link.setProperty(directlinkproperty, dllink);
        link.setFinalFileName(Encoding.htmlDecode(getFileNameFromConnection(dl.getConnection())).trim());
        dl.startDownload();
    }

    /** Source: https://gist.github.com/infval/c69b479ff0bd590f2dd7e1975fe2fcad */
    private String getDirectDownloadurlViaWorkaround(final Browser br, final DownloadLink link) throws PluginException {
        /* 2021-09-13: No downloadlinks available for a lot of content. */
        final String gid = getRomID(br.getURL());
        if (gid == null) {
            errorNoDownloadlinkAvailable();
            /* Dead end */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        String dllink = null;
        if (StringUtils.containsIgnoreCase(br.getURL(), "Sega_Dreamcast_ISOs")) {
            /* Special case */
            final String filename = link.getStringProperty(PROPERTY_DOWNLOAD_LINK_FILENAME);
            if (filename == null) {
                errorNoDownloadlinkAvailable();
                /* Dead end */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            // dllink = "http://50.7.92.186/happyUUKAm8913lJJnckLiePutyNak/Dreamcast/" +
            // URLEncode.encodeURIComponent(downloadCandidates[0]);
            dllink = "https://dl.mprd.se/happyTjuaOAO11j4lf94cbvkNAwkm4nkkj/Dreamcast/" + URLEncode.encodeURIComponent(filename);
        } else {
            dllink = "/roms/get-download.php?gid=" + gid + "&test=true";
        }
        return dllink;
    }

    private void errorNoDownloadlinkAvailable() throws PluginException {
        throw new PluginException(LinkStatus.ERROR_FATAL, "No downloadlink available");
    }

    private void setDownloadServerCookie() {
        // 0 = off
        // 1 = auto
        // 2 = europe
        // 3 = north america
        final String cookieKey = "epdprefs";
        final String domain = br.getHost();
        final int server = getPluginConfig().getIntegerProperty(PROPERTY_CONFIG_SERVERS, 0);
        if (server == 0) {
            // do nothing!
            return;
        } else if (server == 1) {
            br.setCookie(domain, cookieKey, "ephttpdownload");
        } else if (server == 2) {
            br.setCookie(domain, cookieKey, "ephttpdownloadloceu");
        } else if (server == 3) {
            br.setCookie(domain, cookieKey, "ephttpdownloadlocna");
        }
    }

    private String checkDirectLink(final DownloadLink link, final String property) {
        String dllink = link.getStringProperty(property);
        if (dllink != null) {
            try {
                final Browser br2 = br.cloneBrowser();
                /* Without this the directlink won't be accepted! */
                br2.getHeaders().put("Referer", "http://www.emuparadise.me/");
                URLConnectionAdapter con = br2.openGetConnection(dllink);
                if (!this.looksLikeDownloadableContent(con)) {
                    link.setProperty(property, Property.NULL);
                    dllink = null;
                }
                con.disconnect();
            } catch (final Exception e) {
                link.setProperty(property, Property.NULL);
                dllink = null;
            }
        }
        return dllink;
    }

    public void login(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            final Cookies cookies = account.loadCookies("");
            if (cookies != null && !force) {
                br.setCookies(this.getHost(), cookies);
                br.getPage("http://www." + this.getHost() + "/");
                if (br.containsHTML("logout=1")) {
                    logger.info("Cookie login successful");
                    account.saveCookies(br.getCookies(br.getHost()), "");
                    return;
                }
                logger.info("Cookie login failed");
                /* Full login */
            }
            final String txnId = account.getStringProperty("txnid");
            if (txnId == null) {
                throw new AccountInvalidException();
            }
            br.getPage("http://www." + this.getHost() + "/premium-login.php?txn_id=" + txnId);
            /* premuser value == txnId */
            if (br.getCookie(this.getHost(), "premuser", Cookies.NOTDELETEDPATTERN) == null) {
                throw new AccountInvalidException();
            }
            account.saveCookies(br.getCookies(br.getHost()), "");
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        login(account, true);
        ai.setUnlimitedTraffic();
        /* 2016-12-14: Every account gets treated as premium - I guess if an account expires, login is not possible anymore. */
        account.setType(AccountType.PREMIUM);
        account.setMaxSimultanDownloads(this.getMaxSimultanPremiumDownloadNum());
        account.setConcurrentUsePossible(true);
        return ai;
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        requestFileInformation(link);
        final String url = br.getURL();
        login(account, false);
        br.getPage(url);
        if (account.getType() == AccountType.FREE) {
            handleDownload(account, link, "account_free_directlink");
        } else {
            handleDownload(account, link, "account_premium_directlink");
        }
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return 4;
    }

    public static class EmuParadiseMeAccountFactory extends MigPanel implements AccountBuilderInterface {
        /**
         *
         */
        private static final long serialVersionUID = 1L;
        private final String      txnIdHelp        = "Enter in your TxnId";

        private String getTxnId() {
            if (txnIdHelp.equals(this.txnId.getText())) {
                return null;
            }
            return StringUtils.trim(this.txnId.getText());
        }

        private final ExtTextField  txnId;
        private final JLabel        jlTxnId;
        private final EmuParadiseMe plg;

        public EmuParadiseMeAccountFactory(final InputChangedCallbackInterface callback, final EmuParadiseMe plg) {
            super("ins 0, wrap 2", "[][grow,fill]", "");
            this.plg = plg;
            // txnid
            add(jlTxnId = new JLabel("TxnId: (must be at least 8 digits)"));
            add(this.txnId = new ExtTextField() {
                @Override
                public void onChanged() {
                    callback.onChangedInput(this);
                }
            });
            txnId.setHelpText(txnIdHelp);
        }

        @Override
        public JComponent getComponent() {
            return this;
        }

        @Override
        public boolean handleClipboardAutoFill() {
            return DefaultEditAccountPanelAPIKeyLogin.handleClipboardAutoFill(txnId, null, this.plg);
        }

        @Override
        public void setAccount(Account defaultAccount) {
            txnId.setText(defaultAccount.getStringProperty("txnId", null));
        }

        @Override
        public boolean validateInputs() {
            final String txnId = getTxnId();
            /* Either username & password or txnId only. */
            if (!plg.looksLikeValidAPIKey(txnId)) {
                jlTxnId.setForeground(Color.RED);
                return false;
            } else {
                jlTxnId.setForeground(Color.BLACK);
                return true;
            }
        }

        @Override
        public Account getAccount() {
            final String txnId = getTxnId();
            final Account account = new Account(txnId, "");
            if (plg.looksLikeValidAPIKey(txnId)) {
                account.setProperty("txnid", txnId);
            } else {
                account.removeProperty("txnid");
            }
            return account;
        }

        public boolean updateAccount(Account input, Account output) {
            if (!StringUtils.equals(input.getUser(), output.getUser())) {
                output.setUser(input.getUser());
                return true;
            } else if (!StringUtils.equals(input.getPass(), output.getPass())) {
                output.setPass(input.getPass());
                return true;
            } else {
                return false;
            }
        }
    }

    @Override
    protected boolean looksLikeValidAPIKey(String txnId) {
        return txnId != null && txnId.matches("^\\d{8,}$");
    }

    @Override
    public void reset() {
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return maxFree.get();
    }

    @Override
    public void resetDownloadlink(final DownloadLink link) {
    }
}