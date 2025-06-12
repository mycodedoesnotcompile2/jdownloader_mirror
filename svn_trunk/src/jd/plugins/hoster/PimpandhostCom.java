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

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51138 $", interfaceVersion = 3, names = {}, urls = {})
public class PimpandhostCom extends PluginForHost {
    public PimpandhostCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    private static final Pattern PATTERN_NORMAL = Pattern.compile("/image/(\\d+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_OLD_1  = Pattern.compile("/image/show/id/(\\d+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_OLD_2  = Pattern.compile("/index\\.php/image/(\\d+)-[a-z]+\\.html", Pattern.CASE_INSENSITIVE);

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            return getHost() + "://" + fid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        String fid = new Regex(link.getPluginPatternMatcher(), PATTERN_NORMAL).getMatch(0);
        if (fid != null) {
            return fid;
        }
        fid = new Regex(link.getPluginPatternMatcher(), PATTERN_OLD_1).getMatch(0);
        if (fid != null) {
            return fid;
        }
        fid = new Regex(link.getPluginPatternMatcher(), PATTERN_OLD_2).getMatch(0);
        return fid;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.IMAGE_HOST };
    }

    private String dllink = null;

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 1;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/site/tos";
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "pimpandhost.com" });
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
        return buildAnnotationUrls(getPluginDomains());
    }

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:\\w+\\.)?" + buildHostsPatternPart(domains) + "(" + PATTERN_NORMAL.pattern() + "|" + PATTERN_OLD_1.pattern() + "|" + PATTERN_OLD_2.pattern() + ")");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        final String default_extension = ".jpg";
        dllink = null;
        this.setBrowserExclusive();
        if (!link.isNameSet()) {
            link.setName(this.getFID(link) + default_extension);
        }
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*Image not found")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String filesize = br.getRegex("&gt;Size:\\s*([^&]+)&").getMatch(0);
        String filename = br.getRegex("data-filename=\"([^\"]+)").getMatch(0);
        /* Alternative way to get highest quality: br.getPage("http://pimpandhost.com/image/" + picID + "-original.html"); */
        dllink = br.getRegex("data-src=\"([^\"]+)").getMatch(0);
        if (dllink == null) {
            dllink = br.getRegex("<img[^>]*class=\"normal\"[^>]*src=\"(https?[^<>\"]+)\"").getMatch(0);
            if (dllink == null) {
                dllink = br.getRegex("<img[^>]*class=\"normal\"[^>]*src=\"(//[^<>\"]+)\"").getMatch(0);
            }
        }
        final String ext;
        if (dllink != null) {
            ext = getFileNameExtensionFromString(dllink, default_extension);
        } else {
            ext = default_extension;
        }
        if (filename != null) {
            filename = Encoding.htmlDecode(filename);
            filename = filename.trim();
            filename = this.correctOrApplyFileNameExtension(filename, ext, null);
            link.setFinalFileName(filename);
        }
        final boolean isDownload = this.getPluginEnvironment() == PluginEnvironment.DOWNLOAD;
        if (filesize != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesize));
            link.setName(filename);
        } else if (!isDownload && !link.isSizeSet() && !StringUtils.isEmpty(this.dllink)) {
            basicLinkCheck(br.cloneBrowser(), br.createHeadRequest(dllink), link, filename, ext);
        }
        // could be password protected
        if (isDownload && br.containsHTML("<h4>\\s*Album\\s*'.*?'\\s*is protected with password\\s*</h4>")) {
            throw new PluginException(LinkStatus.ERROR_FATAL, "Password protected items are not yet supported  | Contact JDownloader support");
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = new jd.plugins.BrowserAdapter().openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
        handleConnectionErrors(br, dl.getConnection());
        dl.startDownload();
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetPluginGlobals() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}
