package jd.plugins.hoster;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.utils.locale.JDL;

@HostPlugin(revision = "$Revision: 46973 $", interfaceVersion = 2, names = { "hornoxe.com" }, urls = { "https?://\\w+\\.hornoxedecrypted\\.com/.+" })
public class HornOxeCom extends PluginForHost {
    public HornOxeCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public String getAGBLink() {
        return "http://www.hornoxe.com/";
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return -1;
    }

    public void correctDownloadLink(DownloadLink link) {
        link.setUrlDownload(link.getDownloadURL().replace("hornoxedecrypted.com", "hornoxe.com"));
    }

    @Override
    public void handleFree(DownloadLink link) throws Exception {
        requestFileInformation(link);
        // stupid site doesn't always accept connection
        int repeat = 3;
        for (int i = 0; i <= repeat; i++) {
            Browser br2 = br.cloneBrowser();
            try {
                dl = jd.plugins.BrowserAdapter.openDownload(br2, link, link.getStringProperty("DDLink"), true, 1);
                if (!dl.getConnection().isContentDisposition() && !dl.getConnection().getContentType().startsWith("video")) {
                    br2.followConnection();
                    if (br2.containsHTML("No htmlCode read")) {
                        throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, JDL.L("plugins.hoster.hornoxe.videotemporaryunavailable", "This video is temporary unavailable!"), 60 * 60 * 1000l);
                    }
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                dl.startDownload();
                break;
            } catch (Throwable e) {
                if (i == repeat && (!dl.getConnection().isContentDisposition() && !dl.getConnection().getContentType().startsWith("video"))) {
                    br2.followConnection();
                    if (br2.containsHTML("No htmlCode read")) {
                        throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, JDL.L("plugins.hoster.hornoxe.videotemporaryunavailable", "This video is temporary unavailable!"), 60 * 60 * 1000l);
                    }
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                } else {
                    continue;
                }
            }
        }
    }

    private String getReferer(final DownloadLink link) {
        final String referOld = link.getStringProperty("Referer"); // backward compatibility
        if (referOld != null) {
            return referOld;
        } else {
            return link.getReferrerUrl();
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        this.setBrowserExclusive();
        URLConnectionAdapter con = null;
        br.getHeaders().put("Referer", getReferer(link));
        // stupid site doesn't always accept connection
        boolean worked = false;
        int repeat = 3;
        for (int i = 0; i <= repeat; i++) {
            final Browser br2 = br.cloneBrowser();
            try {
                con = br2.openGetConnection(link.getDownloadURL());
                if (this.looksLikeDownloadableContent(con)) {
                    link.setVerifiedFileSize(con.getCompleteContentLength());
                    link.setProperty("DDLink", br2.getURL());
                    worked = true;
                    break;
                }
            } catch (Throwable e) {
            } finally {
                try {
                    con.disconnect();
                } catch (Throwable e) {
                }
            }
        }
        if (!worked) {
            return AvailableStatus.UNCHECKED;
        } else {
            return AvailableStatus.TRUE;
        }
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}