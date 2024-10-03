//jDownloader - Downloadmanager
//Copyright (C) 2013  JD-Team support@jdownloader.org
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
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.config.Property;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountUnavailableException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.MultiHostHost;
import jd.plugins.MultiHostHost.MultihosterHostStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.MultiHosterManagement;

@HostPlugin(revision = "$Revision: 49905 $", interfaceVersion = 3, names = { "rapidox.pl" }, urls = { "" })
public class RapidoxPl extends PluginForHost {
    private final String                 NICE_HOST                 = "rapidox.pl";
    private final String                 NICE_HOSTproperty         = NICE_HOST.replaceAll("(\\.|\\-)", "");
    /* Connection limits */
    private final boolean                ACCOUNT_PREMIUM_RESUME    = true;
    private final int                    ACCOUNT_PREMIUM_MAXCHUNKS = 0;
    private static MultiHosterManagement mhm                       = new MultiHosterManagement("rapidox.pl");

    public RapidoxPl(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/promocje");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setCookiesExclusive(true);
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_USER_AGENT, "JDownloader");
        br.setCustomCharset("utf-8");
        br.setConnectTimeout(60 * 1000);
        br.setReadTimeout(60 * 1000);
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/regulamin";
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws PluginException {
        return AvailableStatus.UNCHECKABLE;
    }

    @Override
    public boolean canHandle(final DownloadLink link, final Account account) throws Exception {
        if (account == null) {
            return false;
        } else {
            mhm.runCheck(account, link);
            return true;
        }
    }

    @Override
    public void handleFree(DownloadLink downloadLink) throws Exception, PluginException {
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public void handlePremium(DownloadLink link, Account account) throws Exception {
        /* handle premium should never be called */
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST };
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        mhm.runCheck(account, link);
        login(account, false);
        String dllink = checkDirectLink(link, NICE_HOSTproperty + "directlink");
        if (dllink == null) {
            final String url = link.getDefaultPlugin().buildExternalDownloadURL(link, this);
            this.postPage(link, account, "https://" + this.getHost() + "/panel/pobierz-plik", "check_links=" + Encoding.urlEncode(url));
            final String downloadID = br.getRegex("rapidox\\.pl/panel/pobierz\\-plik/(\\d+)").getMatch(0);
            if (downloadID == null) {
                mhm.handleErrorGeneric(account, link, "Failed to generate downloadID", 50, 5 * 60 * 1000l);
            }
            String hash = null;
            String dlid = null;
            int counter = 0;
            int getreadymaxreloads = 5;
            final int wait_between_reload = 3;
            do {
                this.sleep(wait_between_reload * 1000l, link);
                br.getPage("/panel/pobierz-plik/" + downloadID);
                hash = br.getRegex("name=\"form_hash\" value=\"([a-z0-9]+)\"").getMatch(0);
                dlid = br.getRegex("name=\"download\\[\\]\" value=\"(\\d+)\"").getMatch(0);
                if (this.isAbort()) {
                    throw new InterruptedException();
                }
                if (hash != null && dlid != null) {
                    logger.info("Get ready loop: Success :)");
                    break;
                } else {
                    logger.info("Get ready loop: continue: Round: " + counter + "/" + getreadymaxreloads);
                    continue;
                }
            } while (counter <= getreadymaxreloads);
            if (hash == null || dlid == null) {
                mhm.handleErrorGeneric(account, link, "Failed to generate downloadHash/dlid", 50, 5 * 60 * 1000l);
            }
            /* Modify name so we can actually find our final downloadlink. */
            String fname = link.getName();
            fname = fname.replace(" ", "_");
            fname = fname.replace("'", "");
            fname = fname.replaceAll("(;|\\&)", "");
            fname = fname.replaceAll("ä", "a").replaceAll("Ä", "A");
            fname = fname.replaceAll("ü", "u").replaceAll("Ü", "U");
            fname = fname.replaceAll("ö", "o").replaceAll("Ö", "O");
            /* File is on their servers --> Chose download method */
            this.postPage(link, account, "/panel/lista-plikow", "download%5B%5D=" + dlid + "&form_hash=" + hash + "&download_files=Pobierz%21&pobieranie=posrednie");
            /* Access list of downloadable files/links & find correct final downloadlink */
            /* How long do we want to wait until the file is on their servers so we can download it? */
            final int maxreloads = 200;
            do {
                if (this.isAbort()) {
                    throw new InterruptedException();
                }
                this.sleep(wait_between_reload * 1000l, link);
                this.getPage(link, account, "/panel/lista-plikow");
                /* TODO: Find a more reliable way to get the final downloadlink... */
                final String[][] results = br.getRegex("<span title=\"(https?://.*?)\">(.*?)</span>.*?<a href=\"(https?://[a-z0-9]+\\.rapidox\\.pl/[A-Za-z0-9]+/[^<>\"]*?)\"").getMatches();
                if (results == null || results.length == 0) {
                    logger.info("Run: " + counter + " | Found zero results");
                    continue;
                }
                for (final String[] result : results) {
                    if (StringUtils.equals(url, result[0])) {
                        dllink = result[2];
                        break;
                    } else if (StringUtils.equals(fname, result[1])) {
                        dllink = result[2];
                        break;
                    }
                }
                logger.info("Run: " + counter + " | Found no matching results");
            } while (counter <= maxreloads && dllink == null);
            if (dllink == null) {
                /* Should never happen */
                mhm.handleErrorGeneric(account, link, "Failed to find final downloadurl", 50, 5 * 60 * 1000l);
            }
        }
        handleDL(account, link, dllink);
    }

    private void handleDL(final Account account, final DownloadLink link, final String dllink) throws Exception {
        if (dllink == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        link.setProperty(NICE_HOSTproperty + "directlink", dllink);
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, ACCOUNT_PREMIUM_RESUME, ACCOUNT_PREMIUM_MAXCHUNKS);
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                checkErrorsWebsite(link, account, br);
                mhm.handleErrorGeneric(account, link, "Final downloadurl did not lead to downloadable content", 10, 5 * 60 * 1000l);
            }
            this.dl.startDownload();
        } catch (final Exception e) {
            link.setProperty(NICE_HOSTproperty + "directlink", Property.NULL);
            throw e;
        }
    }

    private String checkDirectLink(final DownloadLink link, final String property) {
        String dllink = link.getStringProperty(property);
        if (dllink != null) {
            URLConnectionAdapter con = null;
            try {
                final Browser br2 = br.cloneBrowser();
                con = br2.openHeadConnection(dllink);
                if (this.looksLikeDownloadableContent(con)) {
                    return dllink;
                } else {
                    throw new IOException();
                }
            } catch (final Exception e) {
                link.removeProperty(property);
                logger.log(e);
                return null;
            } finally {
                if (con != null) {
                    con.disconnect();
                }
            }
        }
        return null;
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        login(account, true);
        final AccountInfo ai = new AccountInfo();
        String trafficMaxStr = br.getRegex("Dopuszczalny transfer\\s*:\\s*</b>[\t\n\r ]+([0-9 ]+ MB)").getMatch(0);
        long trafficMaxBytes = -1;
        if (trafficMaxStr != null) {
            /* Fix e.g. 500 000 MB (500 GB) --> 500000MB */
            trafficMaxStr = trafficMaxStr.replace(" ", "");
            trafficMaxBytes = SizeFormatter.getSize(trafficMaxStr);
        }
        long trafficAvailableBytes = -1;
        String trafficAvailableStr = br.getRegex("Do wykorzystania:[\t\n\r ]+(-?[0-9 ]+ MB)").getMatch(0);
        if (trafficAvailableStr != null) {
            /* Fix e.g. 500 000 MB (500 GB) --> 500000MB */
            trafficAvailableStr = trafficAvailableStr.replace(" ", "");
            trafficAvailableBytes = SizeFormatter.getSize(trafficAvailableStr);
        }
        /*
         * Free users = They have no package --> Accept them but set zero traffic left. Of couse traffic left <= 0 --> Also free account.
         */
        if (trafficMaxStr == null || trafficMaxBytes == 0 || trafficAvailableStr == null || trafficAvailableBytes <= 0) {
            account.setType(AccountType.FREE);
            /* Free accounts have no traffic - set this so they will not be used (accidently) but still accept them. */
            ai.setTrafficLeft(0);
            ai.setExpired(true);
        } else {
            account.setType(AccountType.PREMIUM);
            account.setMaxSimultanDownloads(Integer.MAX_VALUE);
            ai.setUnlimitedTraffic();
            ai.setTrafficLeft(trafficAvailableBytes);
            ai.setTrafficMax(trafficMaxBytes);
        }
        /* Only add hosts which are listed as 'on' (working) */
        this.getPage(null, account, "/panel/status_hostingow");
        final String hosttable_html = br.getRegex("</tr></thead><tr>(.*?)</tr></table>").getMatch(0);
        if (hosttable_html == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to obtain list of supported hosts #1");
        }
        final List<MultiHostHost> supportedhosts = new ArrayList<MultiHostHost>();
        final String[] hostDomainsInfo = hosttable_html.split("<td width=\"50px\"");
        for (final String domainhtml : hostDomainsInfo) {
            String crippledhost = new Regex(domainhtml, "title=\"([^\"]+)\"").getMatch(0);
            if (crippledhost == null) {
                logger.info("Skipping invalid entry: " + domainhtml);
                continue;
            }
            crippledhost = Encoding.htmlDecode(crippledhost).trim();
            crippledhost = crippledhost.toLowerCase(Locale.ENGLISH);
            /* Do some special corrections */
            if (crippledhost.equals("_4shared")) {
                crippledhost = "4shared.com";
            }
            final String[] trafficPercentages = new Regex(domainhtml, "(\\d+)\\s*%").getColumn(0);
            final MultiHostHost mhost = new MultiHostHost(crippledhost);
            final String status = new Regex(domainhtml, ">\\s*(on|off)\\s*<").getMatch(0);
            if (status == null) {
                logger.info("Failed to find status for entry: " + crippledhost);
            } else if (status.equalsIgnoreCase("off")) {
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
            } else if (!status.equalsIgnoreCase("on")) {
                logger.info("Found unknown status (treating it same as status 'off'): " + status);
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
            }
            if (trafficPercentages != null && trafficPercentages.length == 2) {
                final short trafficPercentNormal = Short.parseShort(trafficPercentages[0]);
                // final short trafficPercentHH = Short.parseShort(trafficPercentages[1]);
                mhost.setTrafficCalculationFactorPercent(trafficPercentNormal);
            }
            supportedhosts.add(mhost);
        }
        if (supportedhosts.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to obtain list of supported hosts #2");
        }
        ai.setMultiHostSupportV2(this, supportedhosts);
        return ai;
    }

    private void login(final Account account, final boolean validateCookies) throws Exception {
        synchronized (account) {
            /* Load cookies */
            br.setCookiesExclusive(true);
            final Cookies cookies = account.loadCookies("");
            final String urlRelativeHosterInfo = "/panel/twoje-informacje";
            if (cookies != null) {
                br.setCookies(this.getHost(), cookies);
                if (!validateCookies) {
                    /* Do not validate cookies */
                    return;
                }
                /* Even though login is forced first check if our cookies are still valid --> If not, force login! */
                logger.info("Checking login cookies");
                br.getPage("https://" + this.getHost() + urlRelativeHosterInfo);
                if (isLoggedIN(br)) {
                    logger.info("Successfully checked login cookies");
                    account.saveCookies(br.getCookies(br.getHost()), "");
                    return;
                } else {
                    /* Clear cookies to prevent unknown errors as we'll perform a full login below now. */
                    br.clearCookies(br.getHost());
                    account.clearCookies("");
                }
            }
            final String loginpage = "https://" + this.getHost() + "/zaloguj_sie";
            this.getPage(null, account, loginpage);
            final String captchaMarker = "class=\"g-recaptcha\"";
            boolean askedForCaptcha = false;
            int loginAttempt = 0;
            do {
                loginAttempt++;
                logger.info("Performing login attempt number: " + loginAttempt);
                Form loginform = br.getFormbyActionRegex(".+/login");
                if (loginform == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                loginform.put("login83", Encoding.urlEncode(account.getUser()));
                loginform.put("password83", Encoding.urlEncode(account.getPass()));
                if (br.containsHTML(captchaMarker)) {
                    logger.info("Failed to prevent captcha - asking user!");
                    /* Handle stupid login captcha */
                    final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br).getToken();
                    loginform.put("g-recaptcha-response", Encoding.urlEncode(recaptchaV2Response));
                    loginform.put("redirect", "");
                    askedForCaptcha = true;
                } else if (loginAttempt > 1) {
                    logger.info("No captcha required on 2nd login attempt -> Don't even try");
                    break;
                }
                br.submitForm(loginform);
                br.getPage(urlRelativeHosterInfo);
                checkErrorsWebsite(null, account, br);
                if (isLoggedIN(br)) {
                    /* Success */
                    break;
                } else if (askedForCaptcha) {
                    logger.info("Stopping because: User was already asked for captcha -> Looks like invalid login credentials");
                    break;
                } else {
                    logger.info("Retrying login - this time a captcha should be required");
                    this.getPage(null, account, loginpage);
                }
            } while (!askedForCaptcha && loginAttempt <= 1);
            if (!isLoggedIN(br)) {
                throw new AccountInvalidException();
            }
            account.saveCookies(br.getCookies(br.getHost()), "");
        }
    }

    private boolean isLoggedIN(final Browser br) {
        return br.containsHTML("panel/wyloguj\"");
    }

    private void getPage(final DownloadLink link, final Account account, final String accesslink) throws IOException, PluginException {
        br.getPage(accesslink);
        checkErrorsWebsite(link, account, br);
    }

    private void postPage(final DownloadLink link, final Account account, final String accesslink, final String postdata) throws IOException, PluginException {
        br.postPage(accesslink, postdata);
        checkErrorsWebsite(link, account, br);
    }

    private void checkErrorsWebsite(final DownloadLink link, final Account account, final Browser br) throws PluginException {
        if (br.containsHTML("Wybierz linki z innego hostingu")) {
            /* Host currently not supported */
            mhm.putError(account, getDownloadLink(), 5 * 60 * 1000l, "Host is currently not supported");
        } else if (br.containsHTML("Jeśli widzisz ten komunikat prosimy niezwłocznie skontaktować się z nami pod")) {
            throw new AccountUnavailableException("Your IP has been banned!", 5 * 60 * 1000);
        } else {
            /* No error */
        }
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}