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
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.decrypter.FilesterMeFolder;

@HostPlugin(revision = "$Revision: 52902 $", interfaceVersion = 3, names = {}, urls = {})
public class FilesterMe extends PluginForHost {
    public FilesterMe(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static final String PROPERTY_FOLDER_ACCESS_TOKEN = "folder_access_token";

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

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        /* Find current list of domains here: https://filester.me/proxy */
        ret.add(new String[] { "filester.me", "filester.si", "filester.sh", "filester.gg" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    private static final Pattern PATTERN_NORMAL = Pattern.compile("/d/([a-zA-Z0-9]{2,})");

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + PATTERN_NORMAL.pattern());
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
        return 0;
    }

    @Override
    protected String getDefaultFileName(DownloadLink link) {
        return this.getFID(link);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        this.setBrowserExclusive();
        String folder_access_token = link.getStringProperty(FilesterMe.PROPERTY_FOLDER_ACCESS_TOKEN);
        if (folder_access_token != null) {
            /* Reuse folder access token so we do not have to send password again (saves us one http request). */
            br.setCookie(getHost(), "folder_access_token", folder_access_token);
        }
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String filename = br.getRegex("window\\.fileName = \"([^\"]+)").getMatch(0);
        String filesize = br.getRegex("Size\\s*</span[^>]*>\\s*<span[^>]*>([^<]+)</span").getMatch(0);
        if (filename != null) {
            filename = restoreFromString("\"" + filename + "\"", TypeRef.STRING);
            link.setFinalFileName(filename);
        } else {
            logger.warning("Failed to find filename");
        }
        if (filesize != null) {
            link.setDownloadSize(SizeFormatter.getSize(null, filesize, false, false));
        } else {
            logger.warning("Failed to find filesize");
        }
        final String sha256 = br.getRegex("SHA-256</span.*>([a-f0-9]{64})").getMatch(0);
        if (sha256 != null) {
            link.setSha256Hash(sha256);
        } else {
            logger.warning("Failed to find sha256");
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link);
    }

    private void handleDownload(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        final Form passwordForm = findPasswordForm(br);
        if (passwordForm != null) {
            /* Handle download password */
            /* Use existing password if possible */
            String password = link.getDownloadPassword();
            if (password == null) {
                password = getUserInput("Password?", link);
            }
            passwordForm.put("password", URLEncoder.encode(encodePassword(password, passwordForm.getInputFieldByName("nonce").getValue()), "UTF-8"));
            br.submitForm(passwordForm);
            if (findPasswordForm(br) != null) {
                throw new PluginException(LinkStatus.ERROR_RETRY, "Wrong password entered");
            }
            link.setDownloadPassword(password);
            final String folder_access_token = br.getCookie(br.getHost(), "folder_access_token");
            link.setProperty(PROPERTY_FOLDER_ACCESS_TOKEN, folder_access_token);
        }
        final String fid = this.getFID(link);
        final Browser brc = br.cloneBrowser();
        brc.getPage("https://filester.me/js/file_dl.js?ver=1.0.5");
        String dl_host = brc.getRegex("const CDN_URL\\s*=\\s*'(https?://[^'/]+)';").getMatch(0);
        if (dl_host == null) {
            final String dl_hosts = brc.getRegex("const CDN_URLS\\s*=\\s*(\\[.*?\\])\\s*;").getMatch(0);
            if (dl_hosts != null) {
                final String cdnURLs[] = restoreFromString(dl_hosts.replace("'", "\""), TypeRef.STRING_ARRAY);
                dl_host = cdnURLs == null ? null : cdnURLs[(int) (Math.floor(Math.random() * cdnURLs.length))];
            }
            if (dl_host == null) {
                dl_host = brc.getRegex("const CDN_URLS\\s*=\\s*\\[\\s*'(https?://[^'/]+)'").getMatch(0);
            }
            if (StringUtils.isEmpty(dl_host)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find dl_host");
            }
        }
        brc.postPageRaw("/api/public/download", "{\"file_slug\":\"" + fid + "\"}");
        if (brc.getHttpConnection().getResponseCode() == 429) {
            throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Error 429 Too Many Requests", 1 * 60 * 1000l);
        }
        final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        if (Boolean.FALSE.equals(entries.get("success"))) {
            /* e.g. {"success":false,"error":"E4006","message":"Too many requests"} */
            throw new PluginException(LinkStatus.ERROR_FATAL, "Error " + entries.get("error") + " | " + entries.get("message"));
        }
        String dllink = entries.get("download_url").toString();
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find final downloadurl");
        }
        dllink = dl_host + dllink;
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
        this.handleConnectionErrors(br, dl.getConnection());
        dl.startDownload();
    }

    private Form findPasswordForm(final Browser br) {
        return FilesterMeFolder.findPasswordForm(br);
    }

    private String encodePassword(String password, String nonce) {
        return FilesterMeFolder.encodePassword(password, nonce);
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        return false;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }
}