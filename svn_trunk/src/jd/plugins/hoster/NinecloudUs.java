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
import jd.plugins.Account;
import jd.plugins.AccountRequiredException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 52084 $", interfaceVersion = 3, names = {}, urls = {})
public class NinecloudUs extends PluginForHost {
    public NinecloudUs(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/terms/";
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "9cloud.us" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/download/(\\d+(/\\d+)?)");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 1;
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
    protected String getDefaultFileName(DownloadLink link) {
        return this.getFID(link) + ".zip";
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        this.setBrowserExclusive();
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* 2026-01-12: Filename is displayed in 2nd download step after 30 seconds of wait time which we can't do during availablecheck. */
        final boolean expectFilenameInHtmlCodeOfFirstStep = false;
        String filename = br.getRegex(">\\s*Filename\\s*:\\s*([^<>\"]+)<").getMatch(0);
        String filesize = br.getRegex(">\\s*Size\\s*:\\s*([^<>\"]+)<").getMatch(0);
        /* 2021-02-17: Filename is not visible e.g. when limit is currently reached. */
        if (filename != null) {
            filename = Encoding.htmlDecode(filename).trim();
            link.setName(filename);
        } else if (expectFilenameInHtmlCodeOfFirstStep) {
            logger.warning("Failed to find filename");
        }
        if (filesize != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesize));
        } else {
            logger.warning("Failed to find filesize");
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        doFree(link, "free_directlink");
    }

    private void doFree(final DownloadLink link, final String directlinkproperty) throws Exception, PluginException {
        final String waitMinutesStr = br.getRegex("You cannot download until (\\d+) minutes? from now").getMatch(0);
        if (waitMinutesStr != null) {
            throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, Long.parseLong(waitMinutesStr) * 60 * 1001l);
        }
        String premiumOnlyMessage = br.getRegex("<div class=\"box_notice\"[^>]*>(.*?)</div>").getMatch(0);
        if (premiumOnlyMessage != null) {
            /* E.g. This file contains XX pictures, and would make a zip of XXX.XMB. You cannot download it without a subscription. */
            premiumOnlyMessage = premiumOnlyMessage.replaceAll("<[^>]*>", "").trim();
            throw new AccountRequiredException(premiumOnlyMessage);
        }
        final String nextStepUrl = br.getRegex("\"(?:https?://)?(/download/fetch-buttons/[^\"]+)\"").getMatch(0);
        if (nextStepUrl == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        br.getPage(nextStepUrl);
        String filename = br.getRegex(">\\s*Filename:?\\s*</div>\\s*<div class=\"value\"[^>]*>([^<]+)</div>").getMatch(0);
        if (filename != null) {
            filename = Encoding.htmlDecode(filename).trim();
            link.setName(filename);
        } else {
            logger.warning("Failed to find filename");
        }
        String dllink = br.getRegex("\"(https?://(downloads|slow).[^/]+/[^\"]+)\"").getMatch(0);
        if (dllink == null) {
            dllink = br.getRegex("<a[^>]*href=\"(https?://[^\"]+)\"[^>]*>\\s*Download Slow").getMatch(0);
        }
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            if (dl.getConnection().getResponseCode() == 403) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 60 * 60 * 1000l);
            } else if (dl.getConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 60 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        dl.startDownload();
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        /* 2021-02-17: No captchas at all */
        return false;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        /* 2025-02-03: Max 1 file per hour */
        return 1;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}