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

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;

import org.appwork.utils.StringUtils;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.download.HashInfo;

@HostPlugin(revision = "$Revision: 50434 $", interfaceVersion = 3, names = {}, urls = {})
public class KwikCx extends PluginForHost {
    public KwikCx(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/";
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "kwik.cx", "kwik.si" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(?:f|e)/([A-Za-z0-9]+)");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return -2;
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
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        if (!link.isNameSet()) {
            /* Fallback */
            link.setName(this.getFID(link) + ".mp4");
        }
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        br.getPage(link.getPluginPatternMatcher().replaceFirst("/e/", "/f/"));
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String filename = br.getRegex("File name\\s*</strong>\\s*:\\s*([^<>\"]+)").getMatch(0);
        String filesize = br.getRegex("Size\\s*</strong>\\s*:\\s*<abbr[^>]*title=\"(\\d+)\\s*Bytes").getMatch(0);
        if (filename != null) {
            link.setFinalFileName(Encoding.htmlDecode(filename).trim());
        } else {
            logger.info("Failed to find filename");
        }
        if (filesize != null) {
            link.setVerifiedFileSize(Long.parseLong(filesize));
        } else {
            logger.info("Failed to find filesize");
        }
        final String crc32 = br.getRegex("strong>\\s*CRC32\\s*</strong>\\s*:\\s*<code>([a-f0-9]+)</code>").getMatch(0);
        if (crc32 != null) {
            link.setHashInfo(HashInfo.newInstanceSafe(crc32, HashInfo.TYPE.CRC32C));
        } else {
            logger.info("Failed to find hash: crc32");
        }
        final String md5 = br.getRegex("strong>\\s*MD5\\s*</strong>\\s*:\\s*<code>([a-f0-9]+)</code>").getMatch(0);
        if (md5 != null) {
            link.setMD5Hash(md5);
        } else {
            logger.info("Failed to find hash: md5");
        }
        final String sha1 = br.getRegex("strong>\\s*SHA1\\s*</strong>\\s*:\\s*<code>([a-f0-9]+)</code>").getMatch(0);
        if (sha1 != null) {
            link.setSha1Hash(sha1);
        } else {
            logger.info("Failed to find hash: sha1");
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        doFree(link, "free_directlink");
    }

    private void doFree(final DownloadLink link, final String directlinkproperty) throws Exception, PluginException {
        if (!attemptStoredDownloadurlDownload(link, directlinkproperty)) {
            br.setFollowRedirects(true);
            String script = br.getRegex("<script>\\s*(var _0x[a-f0-9]+=\\[.*?)\\s*</script>").getMatch(0);
            if (script == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            script = script.replace("eval(", "var result=(");
            final ScriptEngineManager manager = JavaScriptEngineFactory.getScriptEngineManager(null);
            final ScriptEngine engine = manager.getEngineByName("javascript");
            engine.eval(script);
            script = engine.get("result").toString();
            final String token = new Regex(script, "name=\"_token\"\\s*value=\"(.*?)\"").getMatch(0);
            if (token == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, "/d/" + this.getFID(link), "_token=" + token, this.isResumeable(link, null), this.getMaxChunks(link, null));
            handleConnectionErrors(br, dl.getConnection());
            link.setProperty(directlinkproperty, dl.getConnection().getURL().toExternalForm());
        }
        dl.startDownload();
    }

    @Override
    protected void throwFinalConnectionException(Browser br, URLConnectionAdapter con) throws PluginException, IOException {
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
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