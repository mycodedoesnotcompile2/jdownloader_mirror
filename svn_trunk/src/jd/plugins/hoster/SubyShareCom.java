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
import java.util.Calendar;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.TimeUnit;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;

import org.appwork.utils.StringUtils;
import org.appwork.utils.net.httpconnection.HTTPConnection.RequestMethod;
import org.jdownloader.plugins.components.XFileSharingProBasic;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Request;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountUnavailableException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

@HostPlugin(revision = "$Revision: 51855 $", interfaceVersion = 3, names = {}, urls = {})
public class SubyShareCom extends XFileSharingProBasic {
    public SubyShareCom(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
        /* 2021-03-08: Trying to avoid running into "/checkddos.php" */
        this.setStartIntervall(10 * 1000l);
    }

    private final String  PROPERTY_FAKE_OFFLINE_STATUS             = "fake_offline_status";
    private final boolean ALLOW_OLD_GEO_BLOCK_DETECTION_WORKAROUND = false;

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info: 2019-07-08: Premium untested, set FREE account limits <br />
     * captchatype-info: 2019-07-08: 4dignum --> xfilesharingprobasic_subysharecom_special <br />
     * other:<br />
     */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "subyshare.com" });
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
        return XFileSharingProBasic.buildAnnotationUrls(getPluginDomains());
    }

    @Override
    public AvailableStatus requestFileInformationWebsite(final DownloadLink link, final Account account) throws Exception {
        try {
            final AvailableStatus status = super.requestFileInformationWebsite(link, account);
            link.removeProperty(PROPERTY_FAKE_OFFLINE_STATUS);
            return status;
        } catch (final PluginException e) {
            if (e.getLinkStatus() != LinkStatus.ERROR_FILE_NOT_FOUND) {
                /* Some other Exception happened -> Forward it */
                throw e;
            } else if (!ALLOW_OLD_GEO_BLOCK_DETECTION_WORKAROUND) {
                throw e;
            }
            /* Check if the file is really offline */
            logger.info("Looks like file is offline --> Checking if it really is");
            final String filename = this.getFnameViaAbuseLink(br, link); // Throws exception if file is offline
            if (filename != null) {
                link.setName(filename);
            }
            link.setProperty(PROPERTY_FAKE_OFFLINE_STATUS, true);
            logger.info("File is not offline but GEO-blocked");
        }
        return AvailableStatus.TRUE;
    }

    public String regexFilenameAbuse(final Browser br) {
        final String betterFilename = br.getRegex(">\\s*Filename\\s*</label>\\s*<[^>]*>\\s*<p class=\"form-control-static\"[^>]*>([^<]+)<").getMatch(0);
        if (betterFilename != null) {
            return betterFilename;
        }
        return super.regexFilenameAbuse(br);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        if (account != null && account.getType() == AccountType.FREE) {
            /* Free Account */
            return true;
        } else if (account != null && account.getType() == AccountType.PREMIUM) {
            /* Premium account */
            return true;
        } else {
            /* Free(anonymous) and unknown account type */
            return true;
        }
    }

    @Override
    public int getMaxChunks(final Account account) {
        if (account != null && account.getType() == AccountType.FREE) {
            /* Free Account */
            return 1;
        } else if (account != null && account.getType() == AccountType.PREMIUM) {
            /* Premium account */
            return 1;
        } else {
            /* Free(anonymous) and unknown account type */
            return 1;
        }
    }

    @Override
    public int getMaxSimultaneousFreeAnonymousDownloads() {
        return 3;
    }

    @Override
    public int getMaxSimultaneousFreeAccountDownloads() {
        return 3;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return 5;
    }

    @Override
    protected void checkErrors(final Browser br, final String correctedBR, final DownloadLink link, final Account account) throws NumberFormatException, PluginException {
        /* 2019-07-08: Special */
        final String msg_vpn_blocked = "Dedicated servers/VPS/RDP/VPN/Tor/Proxies/Socks blocked by subyshare";
        final long time_vpn_blocked = 5 * 60 * 1000l;
        if (new Regex(correctedBR, "(?i)Sorry\\s*,\\s*we do not support downloading from Dedicated servers|Please download from your PC without using any above services|If this is our mistake\\s*,\\s*please contact").patternFind()) {
            if (account != null) {
                throw new AccountUnavailableException(msg_vpn_blocked, time_vpn_blocked);
            } else {
                throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, msg_vpn_blocked, time_vpn_blocked);
            }
        } else if (new Regex(correctedBR, "(?i)>\\s*The owner of this file blocked you to download it").patternFind()) {
            /*
             * 2020-07-17: This may sometimes happen in premium mode - user is then supposed to contact the uploader to ask for permission
             * to download the file (WTF?!)
             */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "The owner of this file blocked you to download it");
        } else if (new Regex(correctedBR, "(?i)>\\s*You do no have enough traffic to download this file").patternFind()) {
            /* 2023-01-25 */
            if (account != null) {
                throw new AccountUnavailableException("Traffic limit reached", 5 * 60 * 1000);
            } else {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Traffic limit reached", 5 * 60 * 1000);
            }
        } else if (new Regex(correctedBR, ">\\s*You have reached downloads limit today for free type").patternFind()) {
            /* 2025-10-08 */
            final long serverTime = br.getCurrentServerTime(-1);
            final long timeout;
            if (serverTime == -1) {
                timeout = TimeUnit.DAYS.toMillis(1);
            } else {
                // calculate remaining time until midnight
                final Calendar midnight = Calendar.getInstance();
                midnight.setTimeInMillis(serverTime);
                midnight.set(Calendar.HOUR_OF_DAY, 0);
                midnight.set(Calendar.MINUTE, 0);
                midnight.set(Calendar.SECOND, 0);
                midnight.set(Calendar.MILLISECOND, 0);
                midnight.add(Calendar.DAY_OF_MONTH, 1);
                timeout = midnight.getTimeInMillis() - serverTime;
            }
            if (account != null) {
                throw new AccountUnavailableException(timeout);
            } else {
                throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, timeout);
            }
        } else if (StringUtils.containsIgnoreCase(br.getURL(), "/?op=payments")) {
            /**
             * 2025-04-01: Usually a redirect to "/?op=payments" means that a file is downloadable for premium users only. <br>
             * However in this case it means that the user got blocked for using a non allowed IP such as a public VPN IP.
             */
            if (account != null) {
                throw new AccountUnavailableException(msg_vpn_blocked, time_vpn_blocked);
            } else {
                throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, msg_vpn_blocked, time_vpn_blocked);
            }
        }
        if (link != null && link.hasProperty(PROPERTY_FAKE_OFFLINE_STATUS)) {
            throw new PluginException(LinkStatus.ERROR_FATAL, "URL is referer protected or offline");
        }
        super.checkErrors(br, correctedBR, link, account);
    }

    @Override
    protected String regexWaittime(Browser br) {
        /* 2018-07-19: Special */
        String waitStr = new Regex(br.getRequest().getHtmlCode(), "id=\"countdown\"[^>]*>[^<]*<[^>]*>(\\d+)</span>").getMatch(0);
        if (waitStr != null) {
            return waitStr;
        } else {
            return super.regexWaittime(br);
        }
    }

    @Override
    public void handleCaptcha(final DownloadLink link, final Browser br, final Form captchaForm) throws Exception {
        /**
         * Their captchas differ from the XFS default captcha type. <br>
         * This avoids upper handling trying to auto recognize these captchas. <br>
         * 1. Upper handling won't find the '/captchas/' URL.<br>
         * 2. These captchas are special: 6 digits instead of XFS default captchas (4) and they are colored (orange instead of black) - thus
         * our standard XFS captcha-mathod won't be able to recognize them.
         */
        if (StringUtils.containsIgnoreCase(getCorrectBR(br), "/captchas/")) {
            logger.info("Detected captcha method \"Standard captcha\" for this host");
            final String captchaurl = new Regex(getCorrectBR(br), "(/captchas/[^<>\"\\']*)").getMatch(0);
            if (captchaurl == null) {
                logger.warning("Standard captcha captchahandling broken!");
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            String code = getCaptchaCode("xfilesharingprobasic_subysharecom_special", captchaurl, link);
            if (code.contains("0")) {
                logger.info("Replacing captcha result zero with lowercase o");
                code = code.replace("0", "o");
            }
            captchaForm.put("code", code);
            logger.info("Put captchacode " + code + " obtained by captcha metod \"Standard captcha\" in the form.");
        } else {
            super.handleCaptcha(link, br, captchaForm);
        }
    }

    @Override
    protected String regExTrafficLeft(final Browser br) {
        /* 2018-07-19: Special */
        String trafficleftStr = super.regExTrafficLeft(br);
        if (StringUtils.isEmpty(trafficleftStr)) {
            final String src = this.getCorrectBR(br);
            trafficleftStr = new Regex(src, "(?i)Usable Bandwidth\\s*<span class=\"[^\"]+\">\\s*(\\d+(?:\\.\\d{1,2})? [A-Za-z]{2,5}) / [^<]+<").getMatch(0);
        }
        return trafficleftStr;
    }

    @Override
    public boolean isPasswordProtectedHTML(final Browser br, final Form pwForm) {
        /* 2020-02-17: Special */
        boolean pwprotected = super.isPasswordProtectedHTML(br, pwForm);
        if (!pwprotected) {
            pwprotected = br.containsHTML("><b>\\s*Password\\s*</b>");
        }
        return pwprotected;
    }

    @Override
    protected void sendRequest(Browser br, final Request request) throws Exception {
        super.sendRequest(br, request);
        final boolean correctBr = wasCorrectBrowserFlagSet(br);
        handleAntiDdosChallenge(br, RequestMethod.GET.equals(request.getRequestMethod()) ? request.getUrl() : null);
        if (correctBr) {
            correctBR(br);
        }
    }

    private void handleAntiDdosChallenge(final Browser br, String targetPage) throws PluginException, IOException {
        final String checkddosPage = "/checkddos.php";
        if (!br.getURL().contains(checkddosPage)) {
            /* Do nothing */
            return;
        }
        /* 2021-03-08 */
        if (targetPage != null) {
            targetPage = br.getURL(targetPage).toString();
        }
        // throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Antiddos check triggered", 2 * 60 * 1000l);
        final Form form = br.getFormbyProperty("id", "checkDDOS");
        if (form == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else if (!form.hasInputFieldByName("b")) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        String calcChallenge = br.getRegex("Verify\\s*:\\s*</span>\\s*<strong>([0-9\\-\\+\\*x ]+)=\\?</strong>").getMatch(0);
        calcChallenge = calcChallenge.trim().toLowerCase(Locale.ENGLISH);
        /* E.g. "3 x 3" -> "3 * 3" */
        calcChallenge = calcChallenge.replace("x", "*");
        final ScriptEngineManager manager = JavaScriptEngineFactory.getScriptEngineManager(this);
        try {
            final ScriptEngine engine = manager.getEngineByName("javascript");
            final String js = "var res = " + calcChallenge + ";";
            engine.eval(js);
            final String res = engine.get("res").toString();
            form.put("b", res);
        } catch (final Exception e) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, null, e);
        }
        br.submitForm(form);
        if (br.getURL().contains(checkddosPage)) {
            logger.warning("Failed to solve challenge(?)");
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else {
            logger.info("Checkddos challenge solved successfully");
            if (br.getURL().matches("^https?://[^/]+/?$")) {
                logger.info("Redirect to mainpage happened");
                if (targetPage != null && !targetPage.equalsIgnoreCase(br.getURL())) {
                    logger.info("Trying to correct bad redirect to mainpage to: " + targetPage);
                    br.getPage(targetPage);
                }
            }
        }
    }

    @Override
    protected boolean supports_availablecheck_filename_abuse() {
        /* 2025-04-01: Disabled because website doesn't support it. */
        return false;
    }

    @Override
    protected boolean supports_availablecheck_alt() {
        /* 2025-04-01: Disabled because website doesn't support it. */
        return false;
    }

    @Override
    protected boolean supports_availablecheck_filesize_html() {
        /* 2025-04-01: Disabled to avoid upper code finding wrong filesize results. */
        return false;
    }

    @Override
    public String[] scanInfo(final String html, final String[] fileInfo) {
        super.scanInfo(html, fileInfo);
        final String betterFilesize = br.getRegex("<span class=\"label label-warning\"[^>]*>([^<]+)</span>").getMatch(0);
        if (betterFilesize != null) {
            fileInfo[1] = Encoding.htmlDecode(betterFilesize).trim();
        }
        return fileInfo;
    }
}