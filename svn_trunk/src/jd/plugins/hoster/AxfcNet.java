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
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.List;

import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
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
import jd.plugins.download.HashInfo;

@HostPlugin(revision = "$Revision: 50681 $", interfaceVersion = 3, names = {}, urls = {})
public class AxfcNet extends PluginForHost {
    public AxfcNet(PluginWrapper wrapper) {
        super(wrapper);
    }

    private final String PROPERTY_ALLOW_DOWNLOAD_PASSWORD_FROM_URL = "allow_download_password_from_url";

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
        ret.add(new String[] { "axfc.net" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(?:u|uploader/[^/]+/so)/(\\d+)[^/]*");
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

    private String getDownloadPasswordFromURL(final DownloadLink link) {
        final boolean allowPassCodeFromURL = link.getBooleanProperty(PROPERTY_ALLOW_DOWNLOAD_PASSWORD_FROM_URL, true);
        if (!allowPassCodeFromURL) {
            return null;
        }
        try {
            final String passCodeFromURL = UrlQuery.parse(link.getPluginPatternMatcher()).get("key");
            return passCodeFromURL;
        } catch (MalformedURLException e) {
            e.printStackTrace();
            return null;
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        if (!link.isNameSet()) {
            /* Fallback */
            link.setName(this.getFID(link));
        }
        this.setBrowserExclusive();
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String filename = br.getRegex("name=\"download\"[^>]*>\\s*</a>\\s*<h2>Download \\d+\\.[^\\(]*\\(([^<]+)\\)\\s*</h2>").getMatch(0);
        String filesize = br.getRegex("<b>\\s*Size\\s*</b>\\s*</span>\\s*<span[^>]*>([^<]+)</span>").getMatch(0);
        if (filename != null) {
            filename = Encoding.htmlDecode(filename).trim();
            /**
             * Set final filename here because they return bad content-disposition header e.g. <br>
             * Content-Disposition: attachment; filename*=UTF-8''
             */
            link.setFinalFileName(filename);
        } else {
            logger.warning("Failed to find filename");
        }
        if (filesize != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesize));
        } else {
            logger.warning("Failed to find filesize");
        }
        final String hash_md5 = br.getRegex("'MD5 HASH',\\s*'([a-f0-9]{32})").getMatch(0);
        if (hash_md5 != null) {
            link.addHashInfo(HashInfo.newInstanceSafe(hash_md5, HashInfo.TYPE.MD5));
        }
        final String hash_sha1 = br.getRegex("'SHA-1 HASH',\\s*'([a-f0-9]{40})").getMatch(0);
        if (hash_sha1 != null) {
            link.addHashInfo(HashInfo.newInstanceSafe(hash_sha1, HashInfo.TYPE.SHA1));
        }
        final String hash_sha256 = br.getRegex("'SHA-256 HASH',\\s*'([a-f0-9]{64})").getMatch(0);
        if (hash_sha256 != null) {
            link.addHashInfo(HashInfo.newInstanceSafe(hash_sha256, HashInfo.TYPE.SHA256));
        }
        if (StringUtils.isEmpty(link.getComment())) {
            final String description = br.getRegex("<h3>\\s*File description\\s*</h3>\\s*<p>([^<]+)</p>").getMatch(0);
            if (description != null) {
                link.setComment(Encoding.htmlDecode(description).trim());
            }
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link);
    }

    private void handleDownload(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        final Form dlform = getDownloadform(br);
        if (dlform == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String downloadPasswordFromURL = this.getDownloadPasswordFromURL(link);
        String passCode = null;
        if (dlform.hasInputFieldByName("keyword")) {
            // TODO
            link.setPasswordProtected(true);
            passCode = this.getDownloadPasswordFromURL(link);
            if (passCode == null) {
                passCode = link.getDownloadPassword();
            }
            if (passCode == null) {
                passCode = getUserInput("Password?", link);
            }
            dlform.put("keyword", Encoding.urlEncode(passCode));
        } else {
            link.setPasswordProtected(false);
        }
        /* Captcha is not always required */
        final String captchaURL = br.getRegex("\"(/u/captcha\\.pl[^\"]+)").getMatch(0);
        if (captchaURL != null) {
            final String code = this.getCaptchaCode(captchaURL, link);
            dlform.put("cpt", Encoding.urlEncode(code));
        }
        br.submitForm(dlform);
        /* Check for invalid captcha */
        if (captchaURL != null && br.containsHTML(">\\s*Captcha authentication failed")) {
            throw new PluginException(LinkStatus.ERROR_CAPTCHA);
        }
        /* Check for invalid password */
        if (passCode != null) {
            if (getDownloadform(br) != null) {
                /* Wrong password was entered */
                link.setProperty(PROPERTY_ALLOW_DOWNLOAD_PASSWORD_FROM_URL, false);
                if (downloadPasswordFromURL != null) {
                    if (downloadPasswordFromURL.equals(link.getDownloadPassword())) {
                        /*
                         * Same [wrong] password was also set on DownloadLink -> Invalidate it here too so we don't retry with the same
                         * wrong password.
                         */
                        link.setDownloadPassword(null);
                    }
                } else {
                    link.setDownloadPassword(null);
                }
                throw new PluginException(LinkStatus.ERROR_RETRY, "Wrong password entered");
            }
            /* Correct password was entered -> Save it */
            link.setDownloadPassword(passCode);
        }
        final String continuelink = br.getRegex("a href=\"([^\"]+)\"[^>]*>\\s*＜\\s*Download").getMatch(0);
        if (StringUtils.isEmpty(continuelink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        br.getPage(continuelink);
        String dllink = br.getRegex("<a href=\"([^\"]+)\"[^>]*>\\s*To start download").getMatch(0);
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
            }
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl.startDownload();
    }

    final Form getDownloadform(final Browser br) {
        return br.getFormbyActionRegex(".*dl2\\.pl");
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