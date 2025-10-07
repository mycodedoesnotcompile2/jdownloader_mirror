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
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountRequiredException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51620 $", interfaceVersion = 3, names = {}, urls = {})
public class PrimeplusPro extends PluginForHost {
    public PrimeplusPro(PluginWrapper wrapper) {
        super(wrapper);
    }

    private static final String PROPERTY_FREE_DOWNLOAD_ALLOWED = "free_download_allowed";

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost();
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "primeplus.pro" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/file/([a-f0-9]{13})(/([^/]+)?)?");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            return this.getHost() + "://" + fid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        if (account != null && AccountType.PREMIUM.equals(account.getType())) {
            return 0;
        } else {
            return 1;
        }
    }

    private String getAPIBase() {
        return "https://api.primeplus.pro/v1";
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        final String fid = this.getFID(link);
        if (!link.isNameSet()) {
            /* Fallback */
            link.setName(fid);
        }
        this.setBrowserExclusive();
        br.postPageRaw(getAPIBase() + "/auth/token", "{\"grant_type\":\"client_credentials\",\"client_id\":\"pp_web_app\",\"client_secret\":\"bNAujASUlxCGB4J39mwBZneM734FHwOn\"}");
        final Map<String, Object> resp = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final String accessToken = resp.get("access_token").toString();
        final String refreshToken = resp.get("refresh_token").toString();
        br.setCookie(br.getHost(), "accessToken", accessToken);
        br.setCookie(br.getHost(), "refreshToken", refreshToken);
        br.getPage(getAPIBase() + "/files/" + fid);
        if (br.getHttpConnection().getResponseCode() == 404) {
            /* e.g. plaintext response: File not found */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        String filename = entries.get("name").toString();
        link.setFinalFileName(filename);
        link.setVerifiedFileSize(((Number) entries.get("size")).longValue());
        if (Boolean.TRUE.equals(entries.get("isDeleted"))) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (Boolean.TRUE.equals(entries.get("isAvailableForFree"))) {
            link.setProperty(PROPERTY_FREE_DOWNLOAD_ALLOWED, true);
        } else {
            link.removeProperty(PROPERTY_FREE_DOWNLOAD_ALLOWED);
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link);
    }

    private void handleDownload(final DownloadLink link) throws Exception, PluginException {
        final String directlinkproperty = "directurl";
        if (!attemptStoredDownloadurlDownload(link, directlinkproperty)) {
            requestFileInformation(link);
            if (!link.hasProperty(PROPERTY_FREE_DOWNLOAD_ALLOWED)) {
                /* 2025-10-06: Free download hasn't been implemented yet. */
                throw new AccountRequiredException("Only users with premium access can download this file.");
            }
            // TODO: Obtain this value from API once we got account support
            final String fid = this.getFID(link);
            final boolean isOwnFile = false;
            String dllink = null;
            // TODO: Implement proper referer handling
            // final String referer = link.getReferrerUrl();
            final String daysStr = "90";
            if (!isOwnFile) {
                /* Captcha and pre download wait time required */
                /* Hardcoded reCaptcha key date: 2025-10-06 */
                final String siteUrl = link.getPluginPatternMatcher();
                final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br, "6LcsvPkhAAAAAIDzB5oGnzO5v7ucUxcabuRg907j") {
                    @Override
                    public TYPE getType() {
                        return TYPE.INVISIBLE;
                    }

                    @Override
                    public String getSiteUrl() {
                        /*
                         * Small workaround since our browser instance is on api.primeplus.pro subdomain which we cannot use for captcha
                         * solving.
                         */
                        return siteUrl;
                    }
                }.getToken();
                final UrlQuery query = new UrlQuery();
                query.appendEncoded("captchaType", "invisibleReCaptcha");
                query.appendEncoded("captchaValue", recaptchaV2Response);
                query.appendEncoded("guestSpeed", "false");
                query.appendEncoded("referer", "");
                query.appendEncoded("days", daysStr);
                br.getPage(getAPIBase() + "/files/" + fid + "/download?" + query.toString());
                final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                final Number timeRemainSeconds = (Number) entries.get("timeRemain");
                if (timeRemainSeconds != null) {
                    /* Along with http response code 403 */
                    final int seconds = timeRemainSeconds.intValue();
                    if (seconds >= 180) {
                        throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, timeRemainSeconds.longValue() * 1000);
                    }
                    this.sleep(seconds * 1000l, link);
                } else {
                    dllink = entries.get("downloadUrl").toString();
                }
            }
            if (StringUtils.isEmpty(dllink)) {
                br.getPage(getAPIBase() + "/files/" + fid + "/download?referer=&days=" + daysStr);
                final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                dllink = entries.get("downloadUrl").toString();
                if (StringUtils.isEmpty(dllink)) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find final downloadurl");
                }
            }
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
            this.handleConnectionErrors(br, dl.getConnection());
            link.setProperty(directlinkproperty, dl.getConnection().getURL().toExternalForm());
        }
        dl.startDownload();
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        if (acc == null) {
            /* no account, yes we can expect captcha */
            return true;
        } else if (acc.getType() == AccountType.FREE) {
            /* Free accounts can have captchas */
            return true;
        } else {
            /* Premium accounts do not have captchas */
            return false;
        }
    }

    private boolean attemptStoredDownloadurlDownload(final DownloadLink link, final String directlinkproperty) throws Exception {
        final String url = link.getStringProperty(directlinkproperty);
        if (StringUtils.isEmpty(url)) {
            return false;
        }
        try {
            final Browser brc = br.cloneBrowser();
            dl = new jd.plugins.BrowserAdapter().openDownload(brc, link, url, this.isResumeable(link, null), this.getMaxChunks(link, null));
            if (this.looksLikeDownloadableContent(dl.getConnection())) {
                return true;
            } else {
                brc.followConnection(true);
                throw new IOException();
            }
        } catch (final Throwable e) {
            logger.log(e);
            try {
                dl.getConnection().disconnect();
            } catch (Throwable ignore) {
            }
            return false;
        }
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return 1;
    }
}