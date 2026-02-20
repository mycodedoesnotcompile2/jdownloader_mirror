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
import java.util.List;
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CaptchaHelperHostPluginCloudflareTurnstile;
import org.jdownloader.plugins.components.XFileSharingProBasic;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

@HostPlugin(revision = "$Revision: 52341 $", interfaceVersion = 3, names = {}, urls = {})
public class VinovoTo extends XFileSharingProBasic {
    public VinovoTo(final PluginWrapper wrapper) {
        super(wrapper);
        // this.enablePremium(super.getPurchasePremiumURL());
    }

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info:<br />
     * captchatype-info: null 4dignum solvemedia reCaptchaV2, hcaptcha<br />
     * other:<br />
     */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "vinovo.to", "vinovo.si" });
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
        return VinovoTo.buildAnnotationUrls(getPluginDomains());
    }

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(?::\\d+)?" + "/(d|e)/([a-z0-9]{12,})");
        }
        return ret.toArray(new String[0]);
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
    public String[] scanInfo(final String html, final String[] fileInfo) {
        super.scanInfo(html, fileInfo);
        final String betterFilename = new Regex(html, "<h3>([^<]+)</h3>").getMatch(0);
        if (betterFilename != null) {
            fileInfo[0] = betterFilename;
        }
        return fileInfo;
    }

    @Override
    public void doFree(final DownloadLink link, final Account account) throws Exception, PluginException {
        /* First bring up saved final links */
        if (this.attemptStoredDownloadurlDownload(link, account)) {
            return;
        }
        requestFileInformationWebsite(link, account);
        final String fuid = this.getFUIDFromURL(link);
        if (fuid == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String token = br.getRegex("name=\"token\" content=\"([a-f0-9]{32})").getMatch(0);
        if (token == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String sitekey = br.getRegex("data-sitekey=\"(0x[^\"]+)").getMatch(0);
        if (sitekey == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String cdn_server = br.getRegex("data-base=\"(http[^\"]+)").getMatch(0);
        if (cdn_server == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final UrlQuery query1 = new UrlQuery();
        query1.appendEncoded("recaptcha", "");
        query1.appendEncoded("token", token);
        final Browser brc = br.cloneBrowser();
        brc.getHeaders().put("Origin", "https://" + br.getHost(true));
        brc.getHeaders().put("Referer", "https://" + br.getHost(true) + "/e/" + fuid);
        brc.getHeaders().put("x-requested-with", "XMLHttpRequest");
        brc.postPage("/api/file/url/" + fuid, query1);
        final Map<String, Object> resp1 = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        if (!"ok".equalsIgnoreCase(resp1.get("status").toString())) {
            final String msg = (String) resp1.get("message");
            String message = "Failed to prepare downloadlink generation";
            if (!StringUtils.isEmpty(msg)) {
                message = msg;
            }
            throw new PluginException(LinkStatus.ERROR_FATAL, message);
        }
        final String cfTurnstileResponse = new CaptchaHelperHostPluginCloudflareTurnstile(this, br, sitekey).getToken();
        final UrlQuery query2 = new UrlQuery();
        query2.appendEncoded("captcha", cfTurnstileResponse);
        query2.appendEncoded("token", token);
        brc.postPage("/api/file/urldown/" + fuid, query2);
        final Map<String, Object> resp2 = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        if (!"ok".equalsIgnoreCase(resp2.get("status").toString())) {
            final String msg = (String) resp2.get("message");
            String message = "Failed to generate final downloadurl";
            if (!StringUtils.isEmpty(msg)) {
                message = msg;
            }
            throw new PluginException(LinkStatus.ERROR_FATAL, message);
        }
        /* 2025-10-27: Browser waits 3 seconds now but this wait time can be skipped. */
        final String url = resp2.get("url").toString();
        String directurl = cdn_server + "/download/" + url;
        /* Append same parameters as done in browser (not mandatory). */
        if (!directurl.contains("?")) {
            directurl += "?a=0&r=";
        }
        handleDownload(link, account, directurl, null);
    }

    @Override
    protected String getFUID(final String url, URL_TYPE type) {
        if (url == null || type == null) {
            return null;
        }
        try {
            final String path = new URL(url).getPath();
            return new Regex(path, "/([a-z0-9]{12,})").getMatch(0);
        } catch (MalformedURLException e) {
            logger.log(e);
        }
        return null;
    }

    @Override
    protected boolean isOffline(final DownloadLink link, final Browser br) {
        if (br.containsHTML(">\\s*Video not found")) {
            return true;
        } else {
            return super.isOffline(link, br);
        }
    }
}