package jd.plugins.hoster;

import java.io.IOException;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Cookies;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.decrypter.IdriveComFolder;

import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.URLEncode;

@HostPlugin(revision = "$Revision: 52348 $", interfaceVersion = 2, names = { "idrive.com" }, urls = { "" })
public class IDriveCom extends PluginForHost {

    public static final String PROPERTY_NAME    = "name_property";
    public static final String PROPERTY_SERVER  = "server_property";
    public static final String PROPERTY_PATH    = "resourcePath_property";
    public static final String PROPERTY_SHAREID = "shareid_property";

    public IDriveCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    protected String getDefaultFileName(DownloadLink link) {
        return link.getStringProperty(PROPERTY_NAME);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        final String shareid = link.getStringProperty(PROPERTY_SHAREID);
        final String name = link.getStringProperty(PROPERTY_NAME);
        final String file_path = link.getStringProperty(PROPERTY_PATH);
        final String serverAddress = link.getStringProperty(PROPERTY_SERVER);
        if (!StringUtils.isAllNotEmpty(shareid, name, file_path, serverAddress)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        link.setFinalFileName(name);
        br.getPage("https://www." + getHost() + "/idrive/sh/sh/" + shareid);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        Form pwform = IdriveComFolder.getPasswordProtectedForm(this, br);
        String passCode = link.getDownloadPassword();
        if (pwform != null) {
            logger.info("Item is password protected");
            boolean success = false;
            for (int i = 0; i <= 3; i++) {
                if (i > 0 || passCode == null) {
                    passCode = getUserInput("Password?", link);
                }
                pwform.put("password", Encoding.urlEncode(passCode));
                br.submitForm(pwform);
                pwform = IdriveComFolder.getPasswordProtectedForm(this, br);
                if (pwform == null) {
                    success = true;
                    break;
                }
            }
            if (!success) {
                throw new PluginException(LinkStatus.ERROR_RETRY, "Wrong password entered");
            }
            link.setDownloadPassword(passCode);
        }
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE || PluginEnvironment.DOWNLOAD.isCurrentPluginEnvironment()) {
            // EVSID token required for evssync domains
            final String evsTokenUrl = br.getRegex("var\\s*evsTokenUrl\\s*=\\s*(\"|')(.*?)(\"|')").getMatch(1);
            if (StringUtils.isNotEmpty(evsTokenUrl)) {
                final Browser evsCookies = br.cloneBrowser();
                evsCookies.openHeadConnection(evsTokenUrl).disconnect();
            } else if (br.getCookie(getHost(), "EVSID", Cookies.NOTDELETEDPATTERN) == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            dllink = "https://" + serverAddress + "/evs/downloadFile?version=0&p=" + URLEncode.encodeURIComponent(file_path);
            if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                basicLinkCheck(br.cloneBrowser(), br.createHeadRequest(dllink), link, name, null);
            }
        }
        return AvailableStatus.TRUE;
    }

    private String dllink = null;

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        if (dllink == null) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        try {
            // no resume/no chunks supported
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, false, 1);
            handleConnectionErrors(br, dl.getConnection());
            dl.startDownload();
        } finally {
            dllink = null;
        }
    }

    @Override
    public String getAGBLink() {
        return null;
    }

}