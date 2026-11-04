//jDownloader - Downloadmanager
//Copyright (C) 2015  JD-Team support@jdownloader.org
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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import org.appwork.utils.net.URLHelper;
import org.jdownloader.auth.AuthenticationController;
import org.jdownloader.gui.IconKey;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.jdserv.JDServUtils;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.controlling.downloadcontroller.DownloadSession;
import jd.controlling.downloadcontroller.DownloadWatchDog;
import jd.controlling.downloadcontroller.DownloadWatchDogJob;
import jd.http.AuthenticationFactory;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.AccountRequiredException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.download.DownloadInterface;

/**
 * Alternative AppWork log downloader for internal use
 *
 * @author raztoki
 *
 */
@HostPlugin(revision = "$Revision: 53872 $", interfaceVersion = 3, names = { "jdlog" }, urls = { "jdlog://(\\d+)" })
public class JdLog extends PluginForHost {
    /* Logs smaller than this are read into memory and checked for an "empty log" marker */
    private static final int LOG_CONTENT_CHECK_SIZE = 50;
    /* Logs smaller than this (but not matching the marker) are treated as unusable */
    private static final int MIN_VALID_LOG_SIZE     = 125;

    @Override
    public String getAGBLink() {
        return "https://jdownloader.org/impressum";
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public JdLog(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Object getFavIcon(final String host) throws IOException {
        return new AbstractIcon(IconKey.ICON_LOGO_JD_LOGO_64_64, 16);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.INTERNAL, LazyPlugin.FEATURE.FAVICON };
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String linkid = getFID(link);
        if (linkid != null) {
            return "jdlog://" + linkid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return false;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 1;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        final String uid = getFID(link);
        if (uid == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        link.setFinalFileName(uid + ".log");
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        final String uid = getFID(link);
        if (uid == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String url = JDServUtils.BASE + "logunsorted?" + uid;
        final boolean resumable = isResumeable(link, null);
        final int maxChunks = getMaxChunks(link, null);
        final List<AuthenticationFactory> authenticationFactories = AuthenticationController.getInstance().buildAuthenticationFactories(URLHelper.createURL(url), null);
        DownloadInterface connected = null;
        for (final AuthenticationFactory authenticationFactory : authenticationFactories) {
            br.setCustomAuthenticationFactory(authenticationFactory);
            final DownloadInterface candidate = jd.plugins.BrowserAdapter.openDownload(br, link, url, resumable, maxChunks);
            if (candidate.getConnection().getResponseCode() == 401) {
                candidate.getConnection().disconnect();
                continue;
            }
            connected = candidate;
            break;
        }
        if (connected == null) {
            /* No (working) authentication available -> try without authentication */
            br.setCustomAuthenticationFactory(null);
            connected = jd.plugins.BrowserAdapter.openDownload(br, link, url, resumable, maxChunks);
        }
        dl = connected;
        if (dl.getConnection().getResponseCode() == 401) {
            dl.getConnection().disconnect();
            throw new AccountRequiredException("BasicAuth needed");
        } else if (!dl.getConnection().isOK()) {
            dl.getConnection().disconnect();
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error " + dl.getConnection().getResponseCode());
        } else if (dl.getConnection().getCompleteContentLength() == 0) {
            dl.getConnection().disconnect();
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Empty log");
        }
        if (!dl.startDownload()) {
            /* Download failed -> Early return */
            return;
        }
        /* Check if file is empty. */
        final File file = new File(dl.getDownloadable().getFileOutput());
        if (!file.exists()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Downloaded file does not exist");
        }
        final long dlsize = dl.getDownloadable().getDownloadTotalBytes();
        /* Limit the check because it will use less memory than placing a 100+ MiB log into a String */
        if (dlsize < LOG_CONTENT_CHECK_SIZE) {
            final String out = parseLocalFile(file);
            if (("LogID: " + uid).equals(out.trim())) {
                deleteUnusableFile(link);
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
        } else if (dlsize < MIN_VALID_LOG_SIZE) {
            deleteUnusableFile(link);
            throw new PluginException(LinkStatus.ERROR_FATAL, "Log too small (between " + LOG_CONTENT_CHECK_SIZE + " and " + (MIN_VALID_LOG_SIZE - 1) + " bytes)");
        }
    }

    private void deleteUnusableFile(final DownloadLink link) {
        link.getDownloadLinkController().getJobsAfterDetach().add(new DownloadWatchDogJob() {
            @Override
            public void interrupt() {
            }

            @Override
            public void execute(DownloadSession currentSession) {
                final ArrayList<DownloadLink> delete = new ArrayList<DownloadLink>();
                delete.add(link);
                DownloadWatchDog.getInstance().delete(delete, null);
            }

            @Override
            public boolean isHighPriority() {
                return false;
            }
        });
    }

    private String parseLocalFile(final File file) {
        final StringBuilder sb = new StringBuilder();
        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new InputStreamReader(new FileInputStream(file), "UTF-8"));
            String line;
            while ((line = reader.readLine()) != null) {
                sb.append(line).append("\r\n");
            }
            return sb.toString();
        } catch (final IOException e) {
            logger.log(e);
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (final IOException ignore) {
                }
            }
        }
        return "";
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, Account acc) {
        return false;
    }

    @Override
    public Boolean siteTesterDisabled() {
        return Boolean.TRUE;
    }
}