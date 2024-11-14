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

import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;

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

@HostPlugin(revision = "$Revision: 50120 $", interfaceVersion = 3, names = {}, urls = {})
public class GetsharedCom extends PluginForHost {
    public GetsharedCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setCookie(getHost(), "cookie_consent_user_accepted", "true");
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
        ret.add(new String[] { "getshared.com" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/([A-Za-z0-9]{8})");
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

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        if (!link.isNameSet()) {
            /* Fallback -> Mimic default server side filename */
            link.setName("file-" + this.getFID(link) + ".zip");
        }
        this.setBrowserExclusive();
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*Upload not found")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* Filename/size is hidden for password protected items until correct password is submitted. */
        final boolean isPasswordProtected = isPasswordProtected(br);
        final String totalNumberofFilesStr = br.getRegex("id=\"stats-total\"[^>]*>(\\d+)<").getMatch(0);
        String filesize = br.getRegex(">([^<]+)</b>\\s*<br>\\s*Total size").getMatch(0);
        if (totalNumberofFilesStr != null && Integer.parseInt(totalNumberofFilesStr) == 1) {
            String nameOfFirstFile = br.getRegex("class=\"name\"[^>]*>([^<]+)<").getMatch(0);
            if (nameOfFirstFile != null) {
                nameOfFirstFile = Encoding.htmlDecode(nameOfFirstFile).trim();
                /* In the end we are always downloading a .zip file so ensure that the name presented to the user ends with .zip. */
                nameOfFirstFile = this.correctOrApplyFileNameExtension(nameOfFirstFile, ".zip", null);
                link.setName(nameOfFirstFile);
            } else if (!isPasswordProtected) {
                logger.warning("Failed to find filename");
            }
        }
        if (filesize != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesize));
        } else if (!isPasswordProtected) {
            logger.warning("Failed to find filesize");
        }
        link.setPasswordProtected(isPasswordProtected);
        getAndSetUploadMessageAsComment(link);
        return AvailableStatus.TRUE;
    }

    private void getAndSetUploadMessageAsComment(final DownloadLink link) {
        if (!StringUtils.isEmpty(link.getComment())) {
            return;
        }
        final String uploadMessage = br.getRegex("class=\"upload-message\"[^>]*>([^<]+)</div>").getMatch(0);
        if (StringUtils.isEmpty(uploadMessage)) {
            return;
        }
        link.setComment(Encoding.htmlDecode(uploadMessage).trim());
    }

    private boolean isPasswordProtected(final Browser br) {
        if (getPasswordForm(br) != null) {
            return true;
        } else {
            return false;
        }
    }

    /** Returns form to enter download password. */
    private Form getPasswordForm(final Browser br) {
        return br.getFormbyAction("handler/password");
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link);
    }

    private void handleDownload(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        final Form pwform = this.getPasswordForm(br);
        if (pwform != null) {
            String passCode = link.getDownloadPassword();
            if (passCode == null) {
                passCode = getUserInput("Password?", link);
            }
            pwform.put("password", Encoding.urlEncode(passCode));
            br.submitForm(pwform);
            if (br.containsHTML("\"Invalid password") || this.getPasswordForm(br) != null) {
                link.setDownloadPassword(null);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Wrong password entered");
            }
            link.setDownloadPassword(passCode);
            getAndSetUploadMessageAsComment(link);
        }
        final Form dlform = br.getFormbyAction("handler/download");
        if (dlform == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dlform, this.isResumeable(link, null), this.getMaxChunks(link, null));
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

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        return false;
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