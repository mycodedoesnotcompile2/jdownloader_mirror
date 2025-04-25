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
import java.util.Locale;
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.parser.UrlQuery;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51012 $", interfaceVersion = 3, names = {}, urls = {})
public class CehennempassPw extends PluginForHost {
    public CehennempassPw(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost();
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "cehennempass.pw" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/download/([a-z0-9]{12,})");
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
        return 1;
    }

    /** 2022-12-20: Attention: This website is GEO-blocking all IPs except turkish IPs via Cloudflare! */
    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        if (!link.isNameSet()) {
            /* Fallback */
            link.setName(this.getFID(link));
        }
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 403 && br.containsHTML(">\\s*404 - Sayfa bulunamadı")) {
            /* Response 403 with text error 404 in html code (e.g. reproducible with PT IP) */
            throw new PluginException(LinkStatus.ERROR_FATAL, "GEO-blocked");
        } else if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String limitMinutesStr = br.getRegex("İndirme yapmıyorsanız (\\d+) dakika içinde normale dönecektir").getMatch(0);
        // e.g. 2024-04-25T14:30:52.268Z
        final String waitUntilDate = br.getRegex("const expires = new Date\\(\"(\\d+[^\"]+)\"\\);").getMatch(0);
        if (waitUntilDate != null) {
            final long tokenTimestamp = TimeFormatter.getMilliSeconds(waitUntilDate, "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'", Locale.ENGLISH);
            long waitMillis = tokenTimestamp - System.currentTimeMillis();
            if (waitMillis <= 0) {
                /* Wait min 10 seconds */
                waitMillis = 10 * 1000;
            }
            throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, waitMillis);
        } else if (limitMinutesStr != null) {
            throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, Long.parseLong(limitMinutesStr) * 60 * 1000);
        }
        final Form pwform = getPasswordForm(br);
        if (pwform != null) {
            handlePasswordProtectedItem(link);
        }
        String filename = br.getRegex("<h5[^>]*>([^<]+)</h5>").getMatch(0);
        final String filesize = br.getRegex("\\d+p \\((\\d+[^\\)]+)\\)").getMatch(0);
        if (filename != null) {
            filename = Encoding.htmlDecode(filename).trim();
            link.setName(filename);
        } else {
            /* Filename is never shown for password protected items */
            if (pwform == null) {
                logger.warning("Failed to find filename");
            }
        }
        if (filesize != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesize));
        } else {
            /* Filesize is never shown for password protected items */
            if (pwform == null) {
                logger.warning("Failed to find filesize");
            }
        }
        return AvailableStatus.TRUE;
    }

    private void handlePasswordProtectedItem(final DownloadLink link) throws PluginException, IOException {
        final Form pwform = getPasswordForm(br);
        if (pwform == null) {
            /* Item is not password protected or password was solved before already. */
            return;
        }
        link.setPasswordProtected(true);
        final PluginEnvironment env = this.getPluginEnvironment();
        String passCode = link.getDownloadPassword();
        if (passCode == null) {
            /* Look for password in html source */
            passCode = br.getRegex("ndirme şifresi:\\s*<strong>([^<]+)</strong>").getMatch(0);
        }
        if (passCode == null) {
            passCode = link.getDownloadPassword();
        }
        if (passCode == null && env != PluginEnvironment.DOWNLOAD) {
            logger.info("Password needed && not given -> Do not ask used for password during linkcheck!");
            return;
        }
        if (passCode == null) {
            passCode = getUserInput("Password?", link);
        }
        pwform.put("password", Encoding.urlEncode(passCode));
        br.submitForm(pwform);
        if (this.getPasswordForm(br) != null) {
            link.setDownloadPassword(null);
            throw new PluginException(LinkStatus.ERROR_RETRY, "Wrong password entered");
        }
        /* Store valid download password */
        link.setDownloadPassword(passCode);
        final String filename = br.getRegex("<h4[^>]*>([^<]+)</h4>").getMatch(0);
        if (filename != null) {
            link.setFinalFileName(Encoding.htmlDecode(filename).trim());
        }
        final String[] filesizes = br.getRegex(">\\s*Boyut:([^<]+)</p>").getColumn(0);
        if (filesizes != null && filesizes.length > 0) {
            link.setDownloadSize(SizeFormatter.getSize(filesizes[0]));
        }
    }

    private Form getPasswordForm(final Browser br) {
        return br.getFormbyKey("password");
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link);
    }

    private void handleDownload(final DownloadLink link) throws Exception, PluginException {
        final String directlinkproperty = "free_directlink";
        if (!attemptStoredDownloadurlDownload(link, directlinkproperty)) {
            requestFileInformation(link);
            if (br.containsHTML(">\\s*Lütfen biraz bekleyin, ardından tekrar deneyin")) {
                /* File is temporarily unavailable */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE);
            }
            if (link.isPasswordProtected()) {
                this.handlePasswordProtectedItem(link);
            }
            String dllink = br.getRegex("(/download-redirect/[^<>\"\\']+)").getMatch(0);
            /* Maybe we got a video with multiple available qualities */
            final String[] qualities = br.getRegex("data-quality=\"([^\"]+)").getColumn(0);
            if (dllink == null && qualities != null && qualities.length > 0) {
                final UrlQuery query = new UrlQuery();
                query.appendEncoded("video_id", this.getFID(link));
                /* Assume that first item = Highest quality */
                query.appendEncoded("selected_quality", qualities[0]);
                br.postPage("/process_quality_selection.php", query);
                final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                dllink = entries.get("download_link").toString();
            }
            if (StringUtils.isEmpty(dllink)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                if (dl.getConnection().getResponseCode() == 403) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 5 * 60 * 1000l);
                } else if (dl.getConnection().getResponseCode() == 404) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 5 * 60 * 1000l);
                } else if (dl.getConnection().getResponseCode() == 503) {
                    throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Server error 503 too many connections", 5 * 60 * 1000l);
                } else {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
            link.setProperty(directlinkproperty, dl.getConnection().getURL().toExternalForm());
        }
        dl.startDownload();
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        return false;
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
        return Integer.MAX_VALUE;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}