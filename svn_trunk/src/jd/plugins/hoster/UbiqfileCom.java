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
import java.util.regex.Pattern;

import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.components.XFileSharingProBasic;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.parser.html.HTMLParser;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

@HostPlugin(revision = "$Revision: 51353 $", interfaceVersion = 3, names = {}, urls = {})
public class UbiqfileCom extends XFileSharingProBasic {
    public UbiqfileCom(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        final LazyPlugin.FEATURE[] features = super.getFeatures();
        final LazyPlugin.FEATURE[] featuresNew = new LazyPlugin.FEATURE[features.length + 1];
        int index = 0;
        for (final LazyPlugin.FEATURE feature : features) {
            featuresNew[index] = feature;
            index++;
        }
        featuresNew[featuresNew.length - 1] = LazyPlugin.FEATURE.USERNAME_IS_EMAIL;
        return featuresNew;
    }

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info: 2019-06-27: Free untested, set default limits - host nearly only hosts PREMIUMONLY content! <br />
     * captchatype-info: 2019-06-27: Unknown<br />
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
        ret.add(new String[] { "ubiqfile.com" });
        return ret;
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        if (account != null && account.getType() == AccountType.FREE) {
            /* Free Account */
            return false;
        } else if (account != null && account.getType() == AccountType.PREMIUM) {
            /* Premium account */
            /* 2020-03-26: Disabled resume & chunkload as server answers with wrong range see jdlog: 2029715302851 */
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
            /* 2020-03-26: Disabled resume & chunkload as server answers with wrong range see jdlog: 2029715302851 */
            return 1;
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
        return -1;
    }

    @Override
    public boolean loginWebsite(final DownloadLink link, final Account account, final boolean force) throws Exception {
        if (super.loginWebsite(link, account, force)) {
            /* Special: User logs in via username + password but we need his email as a property! */
            /* Only access URL if we haven't accessed it before already. */
            if (br.getURL() == null || !br.getURL().contains("/?op=my_account")) {
                getPage(this.getMainPage() + "/?op=my_account");
            }
            final String mail = new Regex(getCorrectBR(br), "name=\"usr_email\"[^<>]*?value=\"([^\"]+)\"").getMatch(0);
            if (mail != null) {
                logger.info("Found users' mail: " + mail);
                /* Special: See crawler plugin onlyspanking.video */
                account.setProperty("PROPERTY_UBIQFILE_MAIL", mail);
            } else {
                logger.info("Failed to find users' mail");
            }
            return true;
        } else {
            return false;
        }
    }

    @Override
    protected boolean supports_availablecheck_alt() {
        /* 2023-01-01, no longer supported without captcha and different url */
        return false;
    }

    @Override
    public String[] scanInfo(final String[] fileInfo) {
        /* 2019-06-27: Special */
        super.scanInfo(fileInfo);
        if (StringUtils.isEmpty(fileInfo[0])) {
            fileInfo[0] = new Regex(this.getCorrectBR(br), "class=\"paneld\">([^<>\"]+)\\[\\d+\\.\\d+ [A-Za-z]{2,5}\\]</div>").getMatch(0);
            if (StringUtils.isEmpty(fileInfo[0])) {
                /* Weak attempt!! */
                fileInfo[0] = new Regex(this.getCorrectBR(br), "name=description content=\"Download File ([^<>\"]+)\"").getMatch(0);
            }
        }
        final String betterFilesize = br.getRegex(">\\s*Size\\s*:\\s*(\\d+[^<]+)</div>").getMatch(0);
        if (betterFilesize != null) {
            fileInfo[1] = betterFilesize;
        }
        return fileInfo;
    }

    @Override
    protected String scanGenericFileSize(final String html) {
        String cleanHtml = html.replaceAll("(?s)(Upgrade to premium\\s*</h.*</ul>)", "");
        cleanHtml = cleanHtml.replaceAll("(?s)(Slow download\\s*</h.*</ul>)", "");
        return super.scanGenericFileSize(cleanHtml);
    }

    @Override
    public boolean supports_availablecheck_filename_abuse() {
        /* 2019-06-27: Special */
        return false;
    }

    @Override
    protected String getPremiumOnlyErrorMessage(final Browser br) {
        String msg = br.getRegex(">\\s*(This file is not available for free download[^<]*)").getMatch(0);
        if (msg != null) {
            msg = Encoding.htmlDecode(msg).trim();
            return msg;
        } else {
            return super.getPremiumOnlyErrorMessage(br);
        }
    }

    @Override
    public void handleCaptcha(final DownloadLink link, Browser br, final Form captchaForm) throws Exception {
        /**
         * Special: Check for old image captcha first, then for other captchas. <br>
         * Reason: On the login page, their html code contains a reCaptcha siteKey that is not used thus upper handling would ask for the
         * wrong captcha.
         */
        if (StringUtils.containsIgnoreCase(getCorrectBR(br), "/captchas/")) {
            logger.info("Detected captcha method \"Standard captcha\" for this host");
            final String[] sitelinks = HTMLParser.getHttpLinks(br.getRequest().getHtmlCode(), "");
            if (sitelinks == null || sitelinks.length == 0) {
                logger.warning("Standard captcha captchahandling broken!");
                checkErrorsLastResort(br, link, null);
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            String captchaurl = null;
            for (final String linkTmp : sitelinks) {
                if (StringUtils.containsIgnoreCase(linkTmp, "/captchas/")) {
                    captchaurl = linkTmp;
                    break;
                }
            }
            if (StringUtils.isEmpty(captchaurl)) {
                /* Fallback e.g. for relative URLs (e.g. subyshare.com [bad example, needs special handling anways!]) */
                captchaurl = new Regex(getCorrectBR(br), Pattern.compile("(/captchas/[a-z0-9]+\\.jpe?g)", Pattern.CASE_INSENSITIVE)).getMatch(0);
            }
            if (captchaurl == null) {
                logger.warning("Standard captcha captchahandling broken2!");
                checkErrorsLastResort(br, link, null);
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final String code = getCaptchaCode(XFileSharingProBasic.CAPTCHA_METHOD_ID_XFS_DEFAULT, captchaurl, link);
            captchaForm.put("code", code);
            logger.info("Put captchacode " + code + " obtained by captcha metod \"Standard captcha\" in the form.");
        } else {
            super.handleCaptcha(link, br, captchaForm);
        }
    }

    @Override
    protected boolean handleRecaptchaV2(final DownloadLink link, Browser br, final Form captchaForm) throws Exception {
        /* 2025-08-21: Small workaround */
        if (br.containsHTML("//\\s*grecaptcha\\.execute\\(\\);")) {
            logger.info("Looks like reCaptcha handling is commented out in html code -> Doing nothing");
            return false;
        }
        return super.handleRecaptchaV2(link, br, captchaForm);
    }

    @Override
    protected String regExTrafficLeft(final Browser br) {
        final String traffic = br.getRegex("Premium Traffic\\s*</TD>\\s*<TD>\\s*<b[^>]*>([^<]+)</b>").getMatch(0);
        if (traffic != null) {
            return traffic;
        } else {
            return super.regExTrafficLeft(br);
        }
    }
}