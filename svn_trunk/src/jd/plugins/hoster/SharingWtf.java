//jDownloader - Downloadmanager
//Copyright (C) 2016  JD-Team support@jdownloader.org
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

import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.plugins.components.YetiShareCore;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.parser.html.Form.MethodType;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountRequiredException;
import jd.plugins.AccountUnavailableException;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

@HostPlugin(revision = "$Revision: 51263 $", interfaceVersion = 2, names = {}, urls = {})
public class SharingWtf extends YetiShareCore {
    public SharingWtf(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(getPurchasePremiumURL());
    }

    /**
     * DEV NOTES YetiShare<br />
     ****************************
     * mods: See overridden functions<br />
     * limit-info: Last update: 2021-02-10 <br />
     * captchatype-info: 2021-02-10: null<br />
     * other: <br />
     */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "sharing.wtf", "dirrtyshar.es", "filesharing.io" });
        return ret;
    }

    @Override
    protected List<String> getDeadDomains() {
        final ArrayList<String> deadDomains = new ArrayList<String>();
        deadDomains.add("dirrtyshar.es");
        return deadDomains;
    }

    @Override
    public String rewriteHost(String host) {
        /* 2020-01-17: Old domain was: dirrtyshar.es */
        return this.rewriteHost(getPluginDomains(), host, new String[0]);
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        final List<String[]> pluginDomains = getPluginDomains();
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + YetiShareCore.getDefaultAnnotationPatternPart());
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        if (account != null && account.getType() == AccountType.FREE) {
            /* Free Account */
            return false;
        } else if (account != null && account.getType() == AccountType.PREMIUM) {
            /* Premium account */
            return false;
        } else {
            /* Free(anonymous) and unknown account type */
            return false;
        }
    }

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
    public int getMaxSimultanFreeDownloadNum() {
        return 1;
    }

    public int getMaxSimultaneousFreeAccountDownloads() {
        return 1;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return 1;
    }

    @Override
    public String[] scanInfo(final DownloadLink link, final String[] fileInfo) {
        /* 2020-01-17: Special */
        super.scanInfo(link, fileInfo);
        final String betterFilesize = br.getRegex("class=\"fa fa-file-o\"></i>([^<>\"]+)<span").getMatch(0);
        if (!StringUtils.isEmpty(betterFilesize)) {
            fileInfo[1] = betterFilesize.trim();
            if (!fileInfo[1].contains("b")) {
                fileInfo[1] += "b";
            }
        }
        return fileInfo;
    }

    @Override
    protected String getContinueLink(final Browser br) throws Exception {
        /* 2020-01-18: Special */
        String continue_link = null;
        if (continue_link == null) {
            /* 2020-02-17: For premium mode: URL inside URL */
            final String specialURL = br.getRegex("(https?://transfer[a-z0-9]*\\.[^/]+/[^<>\"\\']*\\?url=[^\"]+)").getMatch(0);
            if (specialURL != null) {
                try {
                    final UrlQuery query = UrlQuery.parse(specialURL);
                    continue_link = Encoding.htmlDecode(query.get("url"));
                } catch (final Throwable ignore) {
                    logger.warning("Special premium downloadurl handling failed");
                }
            }
        }
        /* Free mode */
        if (continue_link == null) {
            /* Special: Find downloadurl first, then continue_url */
            continue_link = this.getDllink(br);
            if (continue_link != null) {
                /* 2021-02-09: Cat & mouse */
                continue_link = continue_link.replace("/jd2", "/");
                final String try2 = br.getRegex("window\\.open\\(\\$\\(this\\)\\.data\\(\"url\"\\)\\.replace\\('([^<>\"\\']+)','/'\\)\\)").getMatch(0);
                if (try2 != null) {
                    continue_link = continue_link.replace(try2, "/");
                }
            }
        }
        if (continue_link == null) {
            /*
             * Their html contains a commented-out line of code which is what our template code would normally pick up --> Endless loop
             */
            continue_link = br.getRegex("\\$\\(\\'\\.download-timer\\'\\)\\.html.+\\$\\(\\'\\.download-timer\\'\\)\\.html\\(\"[^\\)]+\\'(https://[^\\']+)").getMatch(0);
        }
        if (continue_link == null) {
            /* 2023-05-09 */
            continue_link = br.getRegex("href='(https?://[^<>\"\\']+)' target='_top'[^>]*>\\s*Download\\s*</a>").getMatch(0);
        }
        if (continue_link == null) {
            /* 2024-02-09: Premium */
            continue_link = br.getRegex("data-url=\"(https?://[^\"]+)\" data-fileserver").getMatch(0);
        }
        if (continue_link != null) {
            return continue_link;
        } else {
            return super.getContinueLink(br);
        }
    }

    @Override
    public void checkErrors(final Browser br, final DownloadLink link, final Account account) throws PluginException {
        /* 2020-02-17: Special */
        final String premiumonly_1 = br.getRegex(">\\s*(You must be a registered member account to[^<]+)").getMatch(0);
        final String premiumonly_2 = br.getRegex(">\\s*(You must be a premium member to download files more[^<]+)").getMatch(0);
        if (premiumonly_1 != null) {
            throw new AccountRequiredException(Encoding.htmlDecode(premiumonly_1).trim());
        } else if (br.containsHTML("you need to be a registered user to download any files")) {
            throw new AccountRequiredException("You need to be a registered user to download any files");
        } else if (premiumonly_2 != null) {
            /* 2024-02-09 */
            throw new AccountRequiredException(premiumonly_2);
        } else if (br.containsHTML(">\\s*You are now required to login in order to download")) {
            /* 2025-07-28 */
            throw new AccountRequiredException("You are now required to login in order to download");
        }
        final String dailyLimitReachedText = br.getRegex(">\\s*(You have reached the daily download limit of \\d+ files)").getMatch(0);
        if (dailyLimitReachedText != null) {
            ipBlockedOrAccountLimit(link, account, dailyLimitReachedText, 30 * 60 * 1000l);
        }
        super.checkErrors(br, link, account);
        final String errorMsg = getErrorMsgURL(br);
        if (errorMsg != null) {
            if (errorMsg.matches("(?i)You must be a registered member account to download files more than.+")) {
                throw new AccountRequiredException(errorMsg);
            } else if (errorMsg.matches("(?i)You need to be a member to download.*")) {
                /* 2020-08-05: Different premiumonly error */
                throw new AccountRequiredException(errorMsg);
            } else {
                if (link != null) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errorMsg);
                } else {
                    throw new AccountUnavailableException(errorMsg, 1 * 60 * 1000l);
                }
            }
        }
    }

    @Override
    protected void handleDownloadWebsite(final DownloadLink link, final Account account) throws Exception, PluginException {
        /* 2020-02-24: Hack/Workaround --> Can skip waittimes but will eventually download items in lower quality. */
        /* 2020-06-04: Disabled for now as normal download has been fixed! */
        final boolean attemptEmbedWorkaround = false;
        if (attemptEmbedWorkaround && (link.getName().contains(".mp3") || link.getName().contains(".m4a") || link.getName().contains(".mp4") | link.getName().contains(".mkv"))) {
            logger.info("Attempting embed workaround");
            final String fuid = getFUID(link);
            final Browser brc = br.cloneBrowser();
            String dllink = null;
            try {
                this.getPage(brc, String.format("https://www.sharing.wtf/embed.php?u=%s&source=sharingwtf", fuid));
                dllink = brc.getRegex("\\.jPlayer\\(\"setMedia\", \\{\\s*[a-z0-9]+\\s*:\\s*\"([^\"]+)").getMatch(0);
            } catch (final Throwable e) {
                logger.warning("Failure in embed handling");
            }
            if (dllink != null) {
                logger.info("Embed workaround successful");
                final String directlinkproperty = getDownloadModeDirectlinkProperty(account);
                link.setProperty(directlinkproperty, dllink);
                // final boolean resume = this.isResumeable(link, account);
                // final int maxchunks = this.getMaxChunks(account);
                /* 2020-02-24: Resume & chunkload not possible for embedded content */
                final boolean resume = false;
                final int maxchunks = 1;
                this.dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, resume, maxchunks);
                this.dl.startDownload();
            } else {
                logger.info("Embed workaround failed");
                super.handleDownloadWebsite(link, account);
            }
        }
        if (account != null) {
            /* Free account download */
            try {
                super.handleDownloadWebsite(link, account);
            } catch (final PluginException pe) {
                if (pe.getLinkStatus() != LinkStatus.ERROR_PLUGIN_DEFECT) {
                    throw pe;
                }
                logger.log(pe);
                /*
                 * Ugly implementation: Let previous code do its job until it fails, then check if we landet on the captcha page to continue
                 * lol
                 */
                final String filename = br.getRegex("data-filename=\"([^\"]+)").getMatch(0);
                final String urlhash = br.getRegex("data-urlhash=\"([^\"]+)").getMatch(0);
                final String sitekey = br.getRegex("data-sitekey=\"([^\"]+)").getMatch(0);
                if (urlhash == null || sitekey == null || filename == null) {
                    logger.warning("Special handling failed");
                    throw pe;
                }
                final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br).getToken();
                final Form captcha = new Form();
                captcha.setMethod(MethodType.POST);
                captcha.setAction("/recaptcha/");
                captcha.put("filename", Encoding.urlEncode(filename));
                captcha.put("urlhash", Encoding.urlEncode(urlhash));
                captcha.put("recaptcha", Encoding.urlEncode(recaptchaV2Response));
                this.submitForm(captcha);
                final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                final String directurl = entries.get("url").toString();
                if (StringUtils.isEmpty(directurl)) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                dl = jd.plugins.BrowserAdapter.openDownload(br, link, directurl, this.isResumeable(link, account), this.getMaxChunks(account));
                final String directlinkproperty = getDownloadModeDirectlinkProperty(account);
                final URLConnectionAdapter con = dl.getConnection();
                /*
                 * Save directurl before download-attempt as it should be valid even if it e.g. fails because of server issue 503 (= too
                 * many connections) --> Should work fine after the next try.
                 */
                link.setProperty(directlinkproperty, con.getURL().toExternalForm());
                checkResponseCodeErrors(con);
                if (!looksLikeDownloadableContent(con)) {
                    br.followConnection(true);
                    checkErrors(br, link, account);
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Final downloadurl did not lead to downloadable content");
                }
                dl.startDownload();
                return;
            }
        }
        super.handleDownloadWebsite(link, account);
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        if (account.getType() == AccountType.PREMIUM) {
            checkDirectLink(link, account);
            if (this.dl == null) {
                this.loginWebsite(account, false);
                getPage(this.getContentURL(link));
                this.checkErrors(br, link, account);
                final Form dlform = br.getFormbyKey("verify");
                // final String rcKey = br.getRegex("grecaptcha\\.execute\\('([^']+)'").getMatch(0);
                if (dlform.containsHTML("g-recaptcha-response")) {
                    /* 2024-03-13: Captcha required for premium downloads */
                    final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br).getToken();
                    dlform.put("g-recaptcha-response", Encoding.urlEncode(recaptchaV2Response));
                }
                dl = jd.plugins.BrowserAdapter.openDownload(br, link, dlform, this.isResumeable(link, account), this.getMaxChunks(account));
            }
            final String directlinkproperty = getDownloadModeDirectlinkProperty(account);
            final URLConnectionAdapter con = dl.getConnection();
            /*
             * Save directurl before download-attempt as it should be valid even if it e.g. fails because of server issue 503 (= too many
             * connections) --> Should work fine after the next try.
             */
            link.setProperty(directlinkproperty, con.getURL().toExternalForm());
            checkResponseCodeErrors(con);
            if (!looksLikeDownloadableContent(con)) {
                br.followConnection(true);
                checkErrors(br, link, account);
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Final downloadurl did not lead to downloadable content");
            }
            dl.startDownload();
        } else {
            super.handlePremium(link, account);
        }
    }

    @Override
    protected String getCorrectHost(final DownloadLink link, URL url) {
        /*
         * 2025-07-28: Small workaround to avoid problems when e.g. login happens via sharing.wtf but links to download contain domain
         * filesharing.io.
         */
        return this.getHost();
    }
}