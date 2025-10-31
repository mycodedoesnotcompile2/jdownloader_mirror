//    jDownloader - Downloadmanager
//    Copyright (C) 2014  JD-Team support@jdownloader.org
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

import java.util.Locale;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.plugins.components.config.FilestoreToConfig;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Cookies;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.parser.html.InputField;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountUnavailableException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.UserAgents;

@HostPlugin(revision = "$Revision: 51777 $", interfaceVersion = 2, names = { "filestore.to" }, urls = { "https?://(?:www\\.)?filestore\\.to/\\?d=([A-Z0-9]+)" })
public class FilestoreTo extends PluginForHost {
    public FilestoreTo(final PluginWrapper wrapper) {
        super(wrapper);
        enablePremium("https://" + getHost() + "/premium");
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/?p=terms";
    }

    @Override
    public void init() {
        Browser.setRequestIntervalLimitGlobal(getHost(), 500);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.USERNAME_IS_EMAIL };
    }

    private String getContentURL(final DownloadLink link) {
        return link.getPluginPatternMatcher().replaceFirst("(?i)http://", "https://");
    }

    /* Don't touch the following! */
    private static AtomicInteger freeRunning = new AtomicInteger(0);

    @Override
    public String getLinkID(final DownloadLink link) {
        final String linkid = getFID(link);
        if (linkid != null) {
            return this.getHost() + "://" + linkid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        if (PluginJsonConfig.get(FilestoreToConfig.class).isStartFreeDownloadsSequentially()) {
            // final int max = 100;
            final int running = freeRunning.get();
            // final int ret = Math.min(running + 1, max);
            // return ret;
            return running + 1;
        } else {
            /* Allow unlimited amount of downloads to start at the same time. */
            return Integer.MAX_VALUE;
        }
    }

    @Override
    public int getTimegapBetweenConnections() {
        return 2000;
    }

    private static AtomicReference<String> agent = new AtomicReference<String>(null);

    private Browser prepBrowser(final Browser br) {
        if (agent.get() == null) {
            final String customUserAgent = PluginJsonConfig.get(FilestoreToConfig.class).getUserAgent();
            if (StringUtils.isEmpty(customUserAgent) || StringUtils.equalsIgnoreCase(customUserAgent, "JDDEFAULT")) {
                agent.set(UserAgents.stringUserAgent());
            } else {
                agent.set(customUserAgent);
            }
        }
        br.getHeaders().put("User-Agent", agent.get());
        br.setCustomCharset("utf-8");
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        setBrowserExclusive();
        prepBrowser(br);
        Exception exception = null;
        final String contenturl = getContentURL(link);
        for (int i = 1; i < 3; i++) {
            try {
                br.getPage(contenturl);
            } catch (final Exception e) {
                logger.log(e);
                exception = e;
                continue;
            }
            final String html = getCorrectedHTML(this.br.toString());
            String filename = new Regex(html, "class=\"file\">\\s*(.*?)\\s*</").getMatch(0);
            if (filename == null) {
                filename = new Regex(html, "\\s*(File:|Filename:?|Dateiname:?)\\s*(.*?)\\s*(Dateigr??e|(File)?size|Gr??e):?\\s*(\\d+(,\\d+)? (B|KB|MB|GB))").getMatch(1);
            }
            String filesizeStr = new Regex(html, "<small>\\s*(?:Dateigröße|Filesize)\\s*</small>\\s*<div\\s*class\\s*=\\s*\"size\"\\s*>\\s*(\\d+[^<]+)<").getMatch(0);
            if (filesizeStr == null) {
                filesizeStr = new Regex(html, "class=\"size\">\\s*(\\d+[^<]+)<").getMatch(0);
                if (filesizeStr == null) {
                    filesizeStr = new Regex(html, "(\\d+(,\\d+)? (B|KB|MB|GB))").getMatch(0);
                }
            }
            if (filename != null) {
                link.setName(Encoding.htmlDecode(filename).trim());
            }
            if (filesizeStr != null) {
                link.setDownloadSize(SizeFormatter.getSize(filesizeStr.replaceAll(",", "\\.").trim()));
            }
            checkErrors(link);
            return AvailableStatus.TRUE;
        }
        if (exception != null) {
            throw exception;
        } else {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
    }

    private void checkErrors(final DownloadLink link) throws PluginException {
        if (br.containsHTML("Derzeit haben wir leider keinen freien Downloadslots frei\\. Bitte nochmal versuchen\\.")) {
            errorNoFreeSlots();
            /* This code should never be reached */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else if (br.containsHTML(">\\s*Leider sind aktuell keine freien Downloadslots")) {
            errorNoFreeSlots();
            /* This code should never be reached */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else if (br.getURL().contains("/error/limit")) {
            throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Wait before starting new downloads", 5 * 60 * 1000l);
        } else if (br.containsHTML(">\\s*Datei nicht gefunden")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*DIE DATEI EXISTIERT LEIDER NICHT MEHR")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*Datei gesperrt")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("Entweder wurde die Datei von unseren Servern entfernt oder der Download-Link war")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*Für diese Datei ist eine Take Down-Meldung eingegangen")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "File abused");
        } else if (br.containsHTML("Derzeit haben wir Serverprobleme und arbeiten daran\\. Bitte nochmal versuchen\\.")) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server issues", 15 * 60 * 1000l);
        } else if (br.containsHTML(">\\s*Ihr Download ist vorübergehend aufgrund des Verdachtes der")) {
            throw new AccountUnavailableException("Account blocked due to suspicion of account sharing", 30 * 60 * 1000);
        } else if (br.containsHTML(">\\s*Es steht aktuell kein Server zum Download zur Verfügung")) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Es steht aktuell kein Server zum Download zur Verfügung. Bitte versuche es später erneut!", 10 * 60 * 1000l);
        } else if (br.containsHTML(">\\s*503 - Service Temporarily Unavailable\\s*<")) {
            /* Goes along with correct header responsecode 503 */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 503", 5 * 60 * 1000l);
        }
        String errorMsg = br.getRegex("class=\"alert alert-danger page-alert mb-2\"[^>]*>\\s*<strong>Download-Fehler</strong>\\s*<br>([^<]+)<").getMatch(0);
        if (errorMsg == null) {
            errorMsg = br.getRegex("class=\"alert alert-danger page-alert mb-2\">\\s*<strong>([^>]+)</strong>").getMatch(0);
        }
        if (errorMsg != null) {
            if (errorMsg.matches("(?i)Datei noch nicht bereit")) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errorMsg, 5 * 60 * 1000l);
            } else {
                /* Unknown error: Display to user but do not retry */
                throw new PluginException(LinkStatus.ERROR_FATAL, errorMsg);
            }
        }
    }

    private void handleDownload(final DownloadLink link, final Account account, final boolean resume, int maxChunks) throws Exception {
        // form 1
        Form dlform = br.getFormByRegex(">\\s*Download\\s*</button>");
        if (dlform != null) {
            br.submitForm(dlform);
        }
        // form 2
        dlform = br.getFormByRegex(">\\s*Download starten\\s*</button>");
        if (dlform != null) {
            // not enforced
            if (account == null || AccountType.FREE.equals(account.getType())) {
                final String waitSecondsStr = br.getRegex("data-wait=\"(\\d+)\"").getMatch(0);
                int waitSeconds = 10;
                if (waitSecondsStr != null) {
                    waitSeconds = Integer.parseInt(waitSecondsStr);
                }
                sleep(waitSeconds * 1001l, getDownloadLink());
            }
            br.submitForm(dlform);
        }
        String dllink = br.getRegex("<a href\\s*=\\s*(\"|')([^>]*)\\1>hier</a>").getMatch(1);
        if (dllink == null) {
            dllink = br.getRegex("<iframe class\\s*=\\s*\"downframe\" src\\s*=\\s*\"(.*?)\"").getMatch(0);
        }
        if (StringUtils.isEmpty(dllink)) {
            checkErrors(link);
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if ((account == null || account.getType() == AccountType.PREMIUM) && PluginJsonConfig.get(FilestoreToConfig.class).isModifyFinalDownloadurls()) {
            /* See: https://board.jdownloader.org/showthread.php?t=91192 */
            dllink = dllink.replaceFirst("(?i)/free/", "/premium/");
        }
        if (!resume) {
            maxChunks = 1;
        }
        dl = new jd.plugins.BrowserAdapter().openDownload(br, link, dllink, resume, maxChunks);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            final String location = br.getRegex("top\\.location\\.href\\s*=\\s*\"(.*?)\"").getMatch(0);
            if (location != null) {
                br.getPage(location);
            }
            checkErrors(link);
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        /* Add a download slot */
        controlMaxFreeDownloads(account, link, +1);
        try {
            /* Start download */
            dl.startDownload();
        } finally {
            /* Remove download slot */
            controlMaxFreeDownloads(account, link, -1);
        }
    }

    protected void controlMaxFreeDownloads(final Account account, final DownloadLink link, final int num) {
        if (account == null) {
            synchronized (freeRunning) {
                final int before = freeRunning.get();
                final int after = before + num;
                freeRunning.set(after);
                logger.info("freeRunning(" + link.getName() + ")|max:" + getMaxSimultanFreeDownloadNum() + "|before:" + before + "|after:" + after + "|num:" + num);
            }
        }
    }

    private void errorNoFreeSlots() throws PluginException {
        final int waitMinutes = PluginJsonConfig.get(FilestoreToConfig.class).getWaittimeOnNoFreeSlotsMinutes();
        final String errorMsg = "No free slots available, wait or buy premium!";
        if (PluginJsonConfig.get(FilestoreToConfig.class).isGlobalNoFreeSlotsBlockModeEnabled()) {
            throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, errorMsg, waitMinutes * 60 * 1000l);
        } else {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errorMsg, waitMinutes * 60 * 1000l);
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        /** Wait user defined number of seconds between downloadstarts if downloads are supposed to start sequentially. */
        final FilestoreToConfig cfg = PluginJsonConfig.get(FilestoreToConfig.class);
        if (cfg.isStartFreeDownloadsSequentially() && freeRunning.get() > 0) {
            this.sleep(cfg.getWaittimeBetweenDownloadStartsSeconds() * 1000l, link);
        }
        requestFileInformation(link);
        handleDownload(link, null, true, 1);
    }

    @Override
    public AccountInfo fetchAccountInfo(Account account) throws Exception {
        login(account, true, "/konto");
        final AccountInfo ai = new AccountInfo();
        final String validUntilString = br.getRegex("Premium-Status\\s*</small>\\s*<div class=\"value text-success\">\\s*(.*?)\\s*Uhr").getMatch(0);
        if (validUntilString != null) {
            final long until = TimeFormatter.getMilliSeconds(validUntilString, "dd'.'MM'.'yyyy' - 'HH':'mm", Locale.ENGLISH);
            ai.setValidUntil(until);
            account.setType(AccountType.PREMIUM);
            account.setMaxSimultanDownloads(20);
            account.setConcurrentUsePossible(true);
            return ai;
        }
        account.setType(AccountType.FREE);
        account.setMaxSimultanDownloads(2);
        account.setConcurrentUsePossible(false);
        return ai;
    }

    private boolean isLoggedinHTML(final Browser br) {
        if (br.containsHTML("\"[^\"]*logout\"")) {
            return true;
        } else {
            return false;
        }
    }

    private boolean login(final Account account, final boolean validateCookies, String validateCookiesURL) throws Exception {
        synchronized (account) {
            final Cookies cookies = account.loadCookies("");
            this.prepBrowser(br);
            if (validateCookiesURL == null) {
                validateCookiesURL = "/konto";
            }
            if (validateCookiesURL.startsWith("/")) {
                validateCookiesURL = "https://" + this.getHost() + validateCookiesURL;
            }
            if (cookies != null) {
                br.setCookies(getHost(), cookies);
                if (!validateCookies) {
                    /* Do not validate cookies */
                    return false;
                }
                logger.info("Validating login cookies...");
                br.getPage(validateCookiesURL);
                if (this.isLoggedinHTML(br)) {
                    logger.info("Cookie login successful");
                    /* refresh saved cookies timestamp */
                    account.saveCookies(br.getCookies(br.getHost()), "");
                    return true;
                } else {
                    logger.info("Cookie login failed");
                    br.clearCookies(null);
                    account.clearCookies("");
                }
            }
            logger.info("Performing full login");
            br.getPage("https://" + this.getHost() + "/login");
            final Form form = br.getFormbyKey("Email");
            final InputField email = form.getInputFieldByNameRegex("(?i)Email");
            email.setValue(Encoding.urlEncode(account.getUser()));
            final InputField password = form.getInputFieldByNameRegex("(?i)Password");
            password.setValue(Encoding.urlEncode(account.getPass()));
            br.submitForm(form);
            /**
             * 2024-08-28: Small workaround: They sometimes redirect to http here which can cause some ISP blocks to engage. </br>
             * Especially from German provider vodafone.de which would interfere and redirect to: </br>
             * http://securenet.sicherheit.vodafone.de/campaign/botnet-fixed/get/message.html?url=http://filestore.to/konto
             */
            if (!br.getURL().equals(validateCookiesURL)) {
                br.getPage(validateCookiesURL);
            }
            if (!this.isLoggedinHTML(br)) {
                throw new AccountInvalidException();
            }
            account.saveCookies(br.getCookies(br.getHost()), "");
            return true;
        }
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        requestFileInformation(link);
        login(account, false, null);
        final String contenturl = this.getContentURL(link);
        br.getPage(contenturl);
        if (!isLoggedinHTML(br)) {
            login(account, true, contenturl);
        }
        if (AccountType.FREE.equals(account.getType())) {
            handleDownload(link, account, true, 1);
        } else {
            handleDownload(link, account, true, 0);
        }
    }

    // private Browser prepAjax(Browser prepBr) {
    // prepBr.getHeaders().put("Accept", "*/*");
    // prepBr.getHeaders().put("Accept-Charset", null);
    // prepBr.getHeaders().put("X-Requested-With:", "XMLHttpRequest");
    // return prepBr;
    // }
    private static String getCorrectedHTML(final String html) throws Exception {
        String ret = html;
        ret = ret.replaceAll("(<(p|div)[^>]+(display:none|top:-\\d+)[^>]+>.*?(<\\s*(/\\2\\s*|\\2\\s*/\\s*)>){2})", "");
        ret = ret.replaceAll("(<(table).*?class=\"hide\".*?<\\s*(/\\2\\s*|\\2\\s*/\\s*)>)", "");
        ret = ret.replaceAll("[\r\n\t]+", " ");
        ret = ret.replaceAll("&nbsp;", " ");
        // ret = ret.replaceAll("(<[^>]+>)", " ");
        return ret;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(final DownloadLink link) {
    }

    @Override
    public void resetPluginGlobals() {
    }

    @Override
    public Class<? extends FilestoreToConfig> getConfigInterface() {
        return FilestoreToConfig.class;
    }
}