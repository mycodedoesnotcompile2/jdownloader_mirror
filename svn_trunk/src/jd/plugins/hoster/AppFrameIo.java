package jd.plugins.hoster;

import java.io.IOException;
import java.util.ArrayList;

import jd.PluginWrapper;
import jd.controlling.linkcrawler.CrawledLink;
import jd.http.Browser;
import jd.plugins.CryptedLink;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.PluginForHost;

import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;

@HostPlugin(revision = "$Revision: 50249 $", interfaceVersion = 3, names = { "app.frame.io" }, urls = { "https://app\\.frame\\.io/reviews/([a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12})/?([a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12})?" })
public class AppFrameIo extends PluginForHost {

    public final static String PROPERTY_QUALITY = "selected_quality";
    public final static String PROPERTY_NAME    = "selected_name";
    public final static String PROPERTY_URL     = "selected_url";

    public AppFrameIo(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public String getAGBLink() {
        return null;
    }

    protected String getFilename(final DownloadLink link) {
        String name = link.getFinalFileName();
        if (name != null) {
            return name;
        }
        name = link.getStringProperty(PROPERTY_NAME, link.getName());
        final String quality = link.getStringProperty(PROPERTY_QUALITY);
        return name.replaceFirst("\\.([^\\.]+)$", "_" + quality + ".$1");
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        String url = link.getStringProperty(PROPERTY_URL, null);
        final Browser brc = br.cloneBrowser();
        if (url != null) {
            try {
                basicLinkCheck(brc, brc.createHeadRequest(url), link, getFilename(link), ".mp4");
                return AvailableStatus.TRUE;
            } catch (IOException ignore) {
                logger.log(ignore);
            } catch (PluginException ignore) {
                logger.log(ignore);
            }
            url = null;
        }
        final PluginForDecrypt decrypter = getNewPluginForDecryptInstance(getHost());
        final ArrayList<DownloadLink> results = decrypter.decryptLink(new CrawledLink(new CryptedLink(link)));
        String quality = link.getStringProperty(PROPERTY_QUALITY);
        for (final DownloadLink result : results) {
            if (quality == null || quality.equals(result.getProperty(PROPERTY_QUALITY))) {
                quality = result.getStringProperty(PROPERTY_QUALITY, null);
                url = result.getStringProperty(PROPERTY_URL, null);
                break;
            }
        }
        if (url != null) {
            basicLinkCheck(brc, brc.createHeadRequest(url), link, getFilename(link), ".mp4");
            link.setProperty(PROPERTY_URL, url);
            link.setProperty(PROPERTY_QUALITY, quality);
            return AvailableStatus.TRUE;
        }
        throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
    }

    @Override
    public String getLinkID(DownloadLink link) {
        if (link != null) {
            final String quality = link.getStringProperty(PROPERTY_QUALITY, null);
            final String review_link_id = new Regex(link.getPluginPatternMatcher(), getMatcher().pattern()).getMatch(0);
            if (quality != null && review_link_id != null) {
                return getHost() + "://" + review_link_id + "/" + quality;
            }
        }
        return super.getLinkID(link);
    }

    @Override
    public void handleFree(DownloadLink link) throws Exception {
        requestFileInformation(link);
        final String url = link.getStringProperty(PROPERTY_URL, null);
        if (StringUtils.isEmpty(url)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, url, true, -1);
        handleConnectionErrors(br, dl.getConnection());
        dl.startDownload();
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }

}
