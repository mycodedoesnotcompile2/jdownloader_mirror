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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.components.XFileSharingProBasic;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

@HostPlugin(revision = "$Revision: 51934 $", interfaceVersion = 3, names = {}, urls = {})
public class FileupOrg extends XFileSharingProBasic {
    public FileupOrg(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
    }

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info: 2020-08-10: No limits at all <br />
     * captchatype-info: 2019-02-18: reCaptchaV2<br />
     * other: 2020-11-18: Broken because of Cloudflare <br />
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
        ret.add(new String[] { "file-upload.org", "file-up.org", "file-up.io", "file-up.cc", "file-up.com", "file-upload.io", "file-upload.cc", "file-upload.com", "file-upload.download" });
        return ret;
    }

    @Override
    public String rewriteHost(final String host) {
        /* They often switch domains. */
        return this.rewriteHost(getPluginDomains(), host);
    }

    @Override
    protected List<String> getDeadDomains() {
        final ArrayList<String> deadDomains = new ArrayList<String>();
        deadDomains.add("file-up.io");
        deadDomains.add("file-up.cc");
        deadDomains.add("file-up.com");
        deadDomains.add("file-up.org");
        return deadDomains;
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
            return 0;
        } else if (account != null && account.getType() == AccountType.PREMIUM) {
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
        final String betterFilesize = new Regex(html, "(?i)You have requested.*?https?://(?:www\\.)?[^/]+/\\s*?" + this.getFUIDFromURL(this.getDownloadLink()) + "</span>\\s*?\\((\\d+(?:\\.\\d{1,2})? [A-Za-z]{2,5})\\)</p>").getMatch(0);
        if (betterFilesize != null) {
            fileInfo[1] = betterFilesize;
        }
        return fileInfo;
    }

    @Override
    protected boolean supports_availablecheck_filesize_html() {
        /* 2019-07-02: Special */
        return false;
    }

    @Override
    public String regexWaittime(final String html) {
        String waitSeconds = new Regex(html, "<span id=\"countdown\">[^<>]*?<span class=\"label label\\-danger seconds\">(\\d+)</span>").getMatch(0);
        if (StringUtils.isEmpty(waitSeconds)) {
            // 2024-10-08
            waitSeconds = new Regex(html, "id=\"seconds\"[^>]*>\\s*(\\d+)\\s*<").getMatch(0);
        }
        if (waitSeconds != null) {
            return waitSeconds;
        } else {
            return super.regexWaittime(html);
        }
    }

    @Override
    protected boolean containsRecaptchaV2Class(String string) {
        /* 2020-03-31: Special */
        return super.containsRecaptchaV2Class(string) || string.contains("name='g-recaptcha-response'");
    }

    @Override
    public void doFree(final DownloadLink link, final Account account) throws Exception, PluginException {
        /* 2025-12-05: They've implemented a special advertisement step in between */
        try {
            super.doFree(link, account);
        } catch (final PluginException e) {
            if (e.getLinkStatus() != LinkStatus.ERROR_PLUGIN_DEFECT) {
                throw e;
            }
        }
        final Regex linkshortener_regex = br.getRegex("https://cashgrowth\\.online/\\?ssid=([a-f0-9]{32})");
        if (!linkshortener_regex.patternFind()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String ad_url = linkshortener_regex.getMatch(-1);
        final String ad_ssid = linkshortener_regex.getMatch(0);
        br.getPage(ad_url); // Cloudflare will most likely block here
        if (!DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            // TODO: Test this before release (requires Cloudflare clearance)
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String current_ip = br.getRegex("const currentIp = \"([^\"]+)\"").getMatch(0);
        String ipv4 = null;
        String ipv6 = null;
        if (current_ip != null) {
            if (current_ip.contains(":")) {
                ipv6 = current_ip;
            } else {
                ipv4 = current_ip;
            }
        }
        final Browser br3 = br.cloneBrowser();
        br3.getPage("https://ipv4.lookup.test-ipv6.com/ip.php?format=json");
        final Map<String, Object> ipinfo = restoreFromString(br3.getRequest().getHtmlCode(), TypeRef.MAP);
        final String iptype = ipinfo.get("type").toString();
        if (iptype.equalsIgnoreCase("")) {
            ipv4 = ipinfo.get("ip").toString();
        }
        final Browser brc = br.cloneBrowser();
        brc.getHeaders().put("Content-Type", "application/json");
        brc.getHeaders().put("Origin", "https://" + brc.getHost(true));
        final Map<String, Object> postdata = new HashMap<String, Object>();
        postdata.put("ssid", ad_ssid);
        postdata.put("currentIp", ipv6);
        postdata.put("ipType", ipinfo.get("type"));
        postdata.put("ipv4", ipv4);
        postdata.put("ipv6", ipv6);
        /* null value is fine here */
        postdata.put("hcaptchaToken", null);
        /* Website uses PATCH request, not POST request but for this test code it doesn't matter. */
        brc.postPageRaw(ad_url, JSonStorage.serializeToJson(postdata));
        final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        if (!Boolean.TRUE.equals(entries.get("success"))) {
            /* This should never happen */
            throw new PluginException(LinkStatus.ERROR_FATAL, "Link-Shortener " + brc.getHost() + " failed");
        }
        final String redirect = entries.get("redirect").toString();
        br.getPage(redirect);
        /* The website wants to force us to click through 5 ad pages. We can skip all of them and go right to the URL we need. */
        String final_link = br.getRegex("/file-redirect\\.html\\?url=(https[^/\"]+)&quot;").getMatch(0);
        if (final_link == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final_link = Encoding.htmlDecode(final_link);
        /* Finally, we can download. */
        handleDownload(link, account, null, final_link);
    }
}