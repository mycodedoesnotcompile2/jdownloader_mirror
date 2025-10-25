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

import java.util.ArrayList;
import java.util.List;

import org.appwork.utils.StringUtils;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.plugins.components.XFileSharingProBasic;

import jd.PluginWrapper;
import jd.config.ConfigContainer;
import jd.config.ConfigEntry;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountRequiredException;
import jd.plugins.AccountUnavailableException;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

@HostPlugin(revision = "$Revision: 51727 $", interfaceVersion = 3, names = {}, urls = {})
public class FilefoxCc extends XFileSharingProBasic {
    public FilefoxCc(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
        this.setConfigElements();
    }

    @Override
    public Browser prepBrowser(final Browser prepBr, final String host) {
        super.prepBrowser(prepBr, host);
        final String custom_referer = this.getPluginConfig().getStringProperty("CUSTOM_REFERER", null);
        if (!StringUtils.isEmpty(custom_referer)) {
            prepBr.getHeaders().put("Referer", custom_referer);
        }
        return prepBr;
    }

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info: premium untested, set FREE account limits<br />
     * captchatype-info: 2019-06-06: reCaptchaV2<br />
     * other:<br />
     */
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

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "filefox.cc" });
        return ret;
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        if (account != null && account.getType() == AccountType.FREE) {
            /* Free Account */
            return false;
        } else if (account != null && account.getType() == AccountType.PREMIUM) {
            /* Premium account */
            return true;
        } else {
            /* Free(anonymous) and unknown account type */
            return false;
        }
    }

    @Override
    public int getMaxChunks(final Account account) {
        if (account != null && account.getType() == AccountType.FREE) {
            /* Free Account */
            return 1;
        } else if (account != null && account.getType() == AccountType.PREMIUM) {
            /* Premium account */
            return -4;
        } else {
            /* Free(anonymous) and unknown account type */
            return 1;
        }
    }

    @Override
    public int getMaxSimultaneousFreeAnonymousDownloads() {
        return 1;
    }

    @Override
    public int getMaxSimultaneousFreeAccountDownloads() {
        return 1;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return 8;
    }

    @Override
    public String[] scanInfo(final String[] fileInfo) {
        super.scanInfo(fileInfo);
        if (StringUtils.isEmpty(fileInfo[0])) {
            fileInfo[0] = new Regex(getCorrectBR(br), "<p>([^<>\"]+)</p>\\s*?<p class=\"file\\-size\"").getMatch(0);
        }
        return fileInfo;
    }

    @Override
    protected String regexWaittime(Browser br) {
        /* 2019-06-06: Special */
        String waitStr = super.regexWaittime(br);
        if (StringUtils.isEmpty(waitStr)) {
            waitStr = new Regex(br.getRequest().getHtmlCode(), "class\\s*=\\s*(\"|')time-remain\\1\\s*>\\s*(\\d+)\\s*</").getMatch(1);
        }
        return waitStr;
    }

    @Override
    protected String getDllink(final DownloadLink downloadLink, final Account account, final Browser br, String src) {
        /* 2019-06-06: Special */
        String dllink = super.getDllink(downloadLink, account, br, src);
        if (StringUtils.isEmpty(dllink)) {
            /* E.g. "https://sXX.filefox.cc/<hash>/<filename>" */
            dllink = new Regex(getCorrectBR(br), "class\\s*=\\s*\"btn btn-default\"\\s*href\\s*=\\s*\"(https?[^\"]+)\"").getMatch(0);
        }
        return dllink;
    }

    @Override
    protected AccountInfo fetchAccountInfoWebsite(final Account account) throws Exception {
        final AccountInfo accInfo = super.fetchAccountInfoWebsite(account);
        if (account.getType() == AccountType.FREE) {
            /* 2020-08-17: Special */
            final Regex expireRegex = new Regex(getCorrectBR(br), "Premium Account expires in (\\d+) days?, (\\d+) hours?");
            final String daysStr = expireRegex.getMatch(0);
            final String hoursStr = expireRegex.getMatch(1);
            if (daysStr != null && hoursStr != null) {
                account.setType(AccountType.PREMIUM);
                final long days = Long.parseLong(daysStr) * 24 * 60 * 60 * 1000;
                final long hours = Long.parseLong(hoursStr) * 60 * 60 * 1000;
                accInfo.setValidUntil(System.currentTimeMillis() + days + hours);
            }
        }
        return accInfo;
    }

    @Override
    protected void checkErrors(final Browser br, final String correctedBR, final DownloadLink link, final Account account) throws NumberFormatException, PluginException {
        /* 2019-06-06: Special */
        super.checkErrors(br, correctedBR, link, account);
        if (new Regex(correctedBR, "You have reached the daily download limit").matches()) {
            if (account != null) {
                throw new AccountUnavailableException("You have reached the daily download limit", 1 * 60 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, "You have reached the daily download limit", 1 * 60 * 60 * 1000l);
            }
        } else if (new Regex(correctedBR, "class\\s*=\\s*(\"|')paid-only(\"|')").matches()) {
            logger.info("Only downloadable via premium");
            throw new AccountRequiredException();
        } else if (new Regex(correctedBR, "You have reached your download limit per").matches()) {
            /* 2018-02-09: Special: Account trafficlimit reached */
            /*
             * E.g.
             * "You have reached your download limit per 3 days: 32000 MB <br>You can <a href='/premium' target='_blank'>Extend your Premium account</a> to reset download limits."
             */
            logger.info("Premium trafficlimit reached");
            throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_TEMP_DISABLE);
        } else if (new Regex(correctedBR, "You\\'ve tried to download from \\d+ different IPs").matches()) {
            /* 2018-02-09: Special: Accountsharing block message */
            /*
             * E.g. "<p class="
             * normal">You've tried to download from 2 different IPs in the last 3 hours. You will not be able to download for 3 hours.</p>"
             */
            logger.info("Premium account temporarily blocked because of too many IPs");
            throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_TEMP_DISABLE);
        }
        final String reconnectWait = new Regex(correctedBR, "Wait\\s*[^<>\"]+\\s*or\\s*<a href\\s*=\\s*(\"|')[^\"']*?\\1\\s*id\\s*=\\s*(\"|')tariff-scroll\\2\\s*>\\s*buy Premium\\s*</a>\\s*and download now").getMatch(-1);
        if (reconnectWait != null) {
            /* adjust this regex to catch the wait time string for COOKIE_HOST */
            String tmphrs = new Regex(reconnectWait, "(?i)\\s+(\\d+)\\s+hours?").getMatch(0);
            String tmpmin = new Regex(reconnectWait, "(?i)\\s+(\\d+)\\s+minutes?").getMatch(0);
            String tmpsec = new Regex(reconnectWait, "(?i)\\s+(\\d+)\\s+seconds?").getMatch(0);
            String tmpdays = new Regex(reconnectWait, "(?i)\\s+(\\d+)\\s+days?").getMatch(0);
            if (tmphrs == null && tmpmin == null && tmpsec == null && tmpdays == null) {
                logger.info("Waittime regexes seem to be broken");
                if (account != null) {
                    throw new AccountUnavailableException("Download limit reached", 60 * 60 * 1000l);
                } else {
                    throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, null, 60 * 60 * 1000l);
                }
            } else {
                int minutes = 0, seconds = 0, hours = 0, days = 0;
                if (tmphrs != null) {
                    hours = Integer.parseInt(tmphrs);
                }
                if (tmpmin != null) {
                    minutes = Integer.parseInt(tmpmin);
                }
                if (tmpsec != null) {
                    seconds = Integer.parseInt(tmpsec);
                }
                if (tmpdays != null) {
                    days = Integer.parseInt(tmpdays);
                }
                int waittime = ((days * 24 * 3600) + (3600 * hours) + (60 * minutes) + seconds + 1) * 1000;
                logger.info("Detected waittime #2, waiting " + waittime + "milliseconds");
                /* Not enough wait time to reconnect -> Wait short and retry */
                if (waittime < 180000) {
                    throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Wait until new downloads can be started", waittime);
                } else if (account != null) {
                    throw new AccountUnavailableException("Download limit reached", waittime);
                } else {
                    throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, null, waittime);
                }
            }
        } else if (new Regex(correctedBR, ">\\s*Premium Account Required for VPS/VPN/Proxy|>\\s*Free users are not allowed to download using VPS|>\\s*Upgrade to Premium to keep usage of your VPN").matches()) {
            /* 2019-12-16 */
            throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Free users are not allowed to download using VPS/VPN/Proxy/Dedicated Servers", 30 * 60 * 60 * 1000l);
        }
    }

    @Override
    public String getLoginURL() {
        /* 2019-09-12: Mainpage will also immediately display login mask but /login is safer! */
        return super.getMainPage() + "/login";
    }

    @Override
    public Form findFormDownload2Premium(final DownloadLink downloadLink, final Account account, final Browser br) throws Exception {
        /* 2019-06-13: Special */
        handleSecurityVerification();
        return super.findFormDownload2Premium(downloadLink, account, br);
    }

    @Override
    public Form findFormDownload1Free(final Browser br) throws Exception {
        /* 2019-06-13: Special */
        handleSecurityVerification();
        return super.findFormDownload1Free(br);
    }

    @Override
    protected void getPage(String page) throws Exception {
        super.getPage(page);
        handleSecurityVerification();
    }

    private void handleSecurityVerification() throws Exception {
        if (br.getURL() != null && br.getURL().contains("op=captcha&id=")) {
            /*
             * 2019-01-23: Special - this may also happen in premium mode! This will only happen when accessing downloadurl. It gets e.g.
             * triggered when accessing a lot of different downloadurls in a small timeframe.
             */
            /* Tags: XFS_IP_CHECK /ip_check/ */
            Form securityVerification = br.getFormbyProperty("name", "F1");
            if (securityVerification == null) {
                securityVerification = br.getFormbyProperty("id", "f1");
            }
            if (securityVerification != null && securityVerification.containsHTML("data-sitekey")) {
                logger.info("Handling securityVerification");
                final boolean redirectSetting = br.isFollowingRedirects();
                final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br).getToken();
                securityVerification.put("g-recaptcha-response", Encoding.urlEncode(recaptchaV2Response));
                br.setFollowRedirects(true);
                super.submitForm(securityVerification);
                br.setFollowRedirects(redirectSetting);
            }
        }
    }

    private void setConfigElements() {
        /* 2019-08-20: This host maybe grants (free-)users with special Referer values better downloadspeeds. */
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_TEXTFIELD, this.getPluginConfig(), "CUSTOM_REFERER", "Set custom Referer here").setDefaultValue(null));
    }
}