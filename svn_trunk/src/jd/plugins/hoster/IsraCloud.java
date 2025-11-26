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
import java.util.regex.Pattern;

import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.components.XFileSharingProBasic;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Request;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountRequiredException;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.PluginException;

@HostPlugin(revision = "$Revision: 51872 $", interfaceVersion = 3, names = {}, urls = {})
public class IsraCloud extends XFileSharingProBasic {
    public IsraCloud(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
    }

    private static final Pattern PATTERN_SUPPORTED = Pattern.compile("/([a-z0-9]{12})(/([^/]+)(?:\\.html)?)?", Pattern.CASE_INSENSITIVE);

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info: 2019-04-15: No downloads possible via free(account), premium: max 10 overall connections<br />
     * captchatype-info: 2019-04-15: unknown as free(account) downloads are not possible<br />
     * other:<br />
     */
    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        final String[] supported_names_official = buildSupportedNames(getPluginDomains());
        final String[] supported_names_full = new String[supported_names_official.length + 1];
        int position = 0;
        for (final String supported_name : supported_names_official) {
            supported_names_full[position] = supported_name;
            position++;
        }
        /* 2019-08-27: For multihoster 'missing TLD handling' */
        supported_names_full[position] = "isra";
        return supported_names_full;
    }

    public static String[] getAnnotationUrls() {
        return buildAnnotationUrls(getPluginDomains());
    }

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + PATTERN_SUPPORTED.pattern());
        }
        return ret.toArray(new String[0]);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "isra.cloud", "isracloud.com" });
        return ret;
    }

    @Override
    protected List<String> getDeadDomains() {
        final ArrayList<String> deadDomains = new ArrayList<String>();
        deadDomains.add("isracloud.com");
        return deadDomains;
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
            return -2;
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
        return 5;
    }

    @Override
    public String[] scanInfo(final String html, final String[] fileInfo) {
        super.scanInfo(html, fileInfo);
        /* 2020-10-21: Special */
        if (StringUtils.isEmpty(fileInfo[0])) {
            fileInfo[0] = new Regex(html, "class=\"desc\"[^>]*>\\s*<span>([^<>\"]+)<").getMatch(0);
        }
        return fileInfo;
    }

    @Override
    protected void checkErrors(final Browser br, final String html, final DownloadLink link, final Account account) throws NumberFormatException, PluginException {
        super.checkErrors(br, html, link, account);
        if (br.containsHTML(">\\s*This file is available.{1,8}for Premium Users only")) {
            throw new AccountRequiredException("This file is available for Premium Users only");
        }
    }

    @Override
    protected void sendRequest(final Browser ibr, final Request request) throws Exception {
        prepReq(ibr, request);
        super.sendRequest(ibr, request);
    }

    @Override
    protected URLConnectionAdapter openAntiDDoSRequestConnection(final Browser ibr, Request request) throws Exception {
        prepReq(ibr, request);
        return super.openAntiDDoSRequestConnection(ibr, request);
    }

    private void prepReq(final Browser ibr, final Request request) throws MalformedURLException {
        /* Set special headers to allow users to download special "temporary file ids". */
        final boolean enableSpecialHandling = false;
        if (!DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            return;
        }
        if (!enableSpecialHandling) {
            return;
        }
        final String fid = new Regex(request.getURL().getPath(), PATTERN_SUPPORTED).getMatch(0);
        if (fid == null) {
            /* Not the expected URL -> Do not modify request */
            return;
        }
        final boolean accessDownloadPageDirectly = true;
        final String specialReferer = Encoding.Base64Decode("aHR0cHM6Ly93d3cuaXNyYngubWUv");
        final String host = Browser.getHost(request.getUrl());
        if (accessDownloadPageDirectly) {
            ibr.getHeaders().put("Referer", "https://isra.cloud/" + fid);
            request.setURL(new URL("https://" + host + "/download"));
        } else {
            ibr.getHeaders().put("Referer", specialReferer);
        }
        ibr.setCookie(host, "file_code", fid);
        ibr.setCookie(host, "ref_url", Encoding.urlEncode(specialReferer));
        // ibr.setCookie(host, "forceSplash", "1");
    }
}