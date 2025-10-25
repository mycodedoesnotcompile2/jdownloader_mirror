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

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.appwork.utils.StringUtils;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.plugins.components.XFileSharingProBasic;
import org.jdownloader.plugins.components.config.XFSConfigVideo.DownloadMode;
import org.jdownloader.plugins.components.config.XFSConfigVideoFilemoonSx;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.parser.html.Form.MethodType;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.decrypter.FilemoonSxCrawler;

@HostPlugin(revision = "$Revision: 51727 $", interfaceVersion = 3, names = {}, urls = {})
public class FilemoonSx extends XFileSharingProBasic {
    public FilemoonSx(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
    }

    @Override
    protected List<String> getDeadDomains() {
        final ArrayList<String> deadDomains = new ArrayList<String>();
        deadDomains.add("filemoon.eu");
        return deadDomains;
    }

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info: 2022-06-21: No limits at all <br />
     * captchatype-info: 2022-06-21: reCaptchaV2<br />
     * other:<br />
     */
    public static List<String[]> getPluginDomains() {
        return FilemoonSxCrawler.getPluginDomains();
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        return FilemoonSxCrawler.getAnnotationUrls();
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        final AccountType type = account != null ? account.getType() : null;
        if (AccountType.FREE.equals(type)) {
            /* Free Account */
            return true;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            /* Premium account */
            return true;
        } else {
            /* Free(anonymous) and unknown account type */
            return true;
        }
    }

    @Override
    public int getMaxChunks(final Account account) {
        final AccountType type = account != null ? account.getType() : null;
        if (AccountType.FREE.equals(type)) {
            /* Free Account */
            return 0;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            /* Premium account */
            return 0;
        } else {
            /* Free(anonymous) and unknown account type */
            return 0;
        }
    }

    @Override
    public int getMaxSimultaneousFreeAnonymousDownloads() {
        return -1;
    }

    @Override
    public int getMaxSimultaneousFreeAccountDownloads() {
        return -1;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return -1;
    }

    @Override
    protected boolean isVideohoster_enforce_video_filename() {
        /* 2022-06-21: Special */
        return true;
    }

    @Override
    public String getFUIDFromURL(final DownloadLink link) {
        try {
            final String url = link.getPluginPatternMatcher();
            if (url != null) {
                final String result = new Regex(new URL(url).getPath(), "/./([a-z0-9]+)").getMatch(0);
                return result;
            }
        } catch (MalformedURLException e) {
            logger.log(e);
        }
        return null;
    }

    @Override
    public String getFilenameFromURL(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), "https?://[^/]+/./[^/]+/(.*?)(?:\\.html|\\?|$)").getMatch(0);
    }

    @Override
    protected boolean supports_availablecheck_alt() {
        return false;
    }

    @Override
    protected boolean supports_availablecheck_filesize_alt_fast() {
        return false;
    }

    @Override
    protected boolean supports_availablecheck_filename_abuse() {
        return false;
    }

    @Override
    protected boolean isShortURL(DownloadLink link) {
        return false;
    }

    @Override
    public String[] scanInfo(final String html, final String[] fileInfo) {
        super.scanInfo(html, fileInfo);
        /* 2022-11-04 */
        final String betterFilename = new Regex(html, "<h3[^>]*>([^<]+)</h3>").getMatch(0);
        if (betterFilename != null) {
            fileInfo[0] = betterFilename;
        }
        return fileInfo;
    }

    @Override
    public void doFree(final DownloadLink link, final Account account) throws Exception, PluginException {
        /* First bring up saved final links */
        String dllink = checkDirectLink(link, account);
        String streamDownloadurl = null;
        grabOfficialVideoDownloadDirecturl: if (StringUtils.isEmpty(dllink)) {
            requestFileInformationWebsite(link, account);
            final DownloadMode mode = this.getPreferredDownloadModeFromConfig();
            streamDownloadurl = this.getDllink(link, account, br, br.getRequest().getHtmlCode());
            if (!StringUtils.isEmpty(streamDownloadurl) && (mode == DownloadMode.STREAM || mode == DownloadMode.AUTO)) {
                /* User prefers to download stream -> We can skip the captcha required to find official video downloadurl. */
                break grabOfficialVideoDownloadDirecturl;
            }
            this.checkErrors(br, this.getCorrectBR(br), link, account);
            if (!br.getURL().matches(".*/download/.*")) {
                this.getPage("/download/" + this.getFUIDFromURL(link));
            }
            final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br, "6LdiBGAgAAAAAIQm_arJfGYrzjUNP_TCwkvPlv8k").getToken();
            final Form dlform = new Form();
            dlform.setMethod(MethodType.POST);
            dlform.put("b", "download");
            dlform.put("file_code", this.getFUIDFromURL(link));
            dlform.put("g-recaptcha-response", Encoding.urlEncode(recaptchaV2Response));
            this.submitForm(dlform);
            if (br.containsHTML("class=\"error e404\"|>\\s*Page not found")) {
                /* 2023-05-04 */
                if (streamDownloadurl != null) {
                    logger.info("Official download is not possible -> Fallback to stream download");
                } else {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Error 404 download impossible at this moment");
                }
            }
            dllink = this.getDllink(link, account, br, br.getRequest().getHtmlCode());
            if (StringUtils.isEmpty(dllink) && !StringUtils.isEmpty(streamDownloadurl)) {
                logger.info("Failed to find official downloadurl -> Fallback to stream download");
                // dllink = streamDownloadurl;
                /* Fallback happens in upper code */
            }
        }
        handleDownload(link, account, dllink, streamDownloadurl);
    }

    @Override
    public ArrayList<String> getCleanupHTMLRegexes() {
        final ArrayList<String> ret = super.getCleanupHTMLRegexes();
        final Iterator<String> it = ret.iterator();
        while (it.hasNext()) {
            final String next = it.next();
            // download link (see getDllink method) is "display: none"
            if (next.contains("display")) {
                it.remove();
                break;
            }
        }
        return ret;
    }

    @Override
    protected String getFileNameFromConnection(URLConnectionAdapter connection, DownloadLink link) {
        final String ret = super.getFileNameFromConnection(connection, link);
        if (ret != null) {
            return ret.replaceFirst("\\s*mkv\\s*mp4$", "");
        } else {
            return null;
        }
    }

    @Override
    protected String getDllink(final DownloadLink link, final Account account, final Browser br, String src) {
        /* 2023-04-20: New */
        final String dllink = br.getRegex(">\\s*Your download link</div>\\s*<a href=\"(https?://[^\"]+)").getMatch(0);
        if (dllink != null) {
            return dllink;
        } else {
            return super.getDllink(link, account, br, src);
        }
    }

    @Override
    protected void checkErrors(final Browser br, final String html, final DownloadLink link, final Account account) throws NumberFormatException, PluginException {
        super.checkErrors(br, html, link, account);
        /* 2022-11-04: Website failure after captcha on "/download/..." page */
        if (isOffline(link, br)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (isRefererBlocked(br)) {
            throw new PluginException(LinkStatus.ERROR_FATAL, "Custom referer needed to download this item");
        } else if (br.containsHTML(">\\s*This video is not available in your country")) {
            throw new PluginException(LinkStatus.ERROR_FATAL, "GEO-blocked");
        }
    }

    private boolean isRefererBlocked(final Browser br) {
        return br.containsHTML(">\\s*This video cannot be watched under this domain");
    }

    @Override
    protected boolean isOffline(final DownloadLink link, final Browser br) {
        if (br.containsHTML("<h1>\\s*Page not found|class=\"error e404\"")) {
            return true;
        } else {
            return super.isOffline(link, br);
        }
    }

    @Override
    protected boolean trustAvailablecheckVideoEmbed() {
        return true;
    }

    @Override
    protected Boolean requiresCaptchaForOfficialVideoDownload() {
        return Boolean.TRUE;
    }

    @Override
    public Class<? extends XFSConfigVideoFilemoonSx> getConfigInterface() {
        return XFSConfigVideoFilemoonSx.class;
    }
}