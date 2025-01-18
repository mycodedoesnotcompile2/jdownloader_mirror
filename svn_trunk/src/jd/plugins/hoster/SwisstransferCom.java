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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.decrypter.SwisstransferComFolder;

@HostPlugin(revision = "$Revision: 50467 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { SwisstransferComFolder.class })
public class SwisstransferCom extends PluginForHost {
    public SwisstransferCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static final String PROPERTY_LINK_UUID           = "link_uuid";
    public static final String PROPERTY_CONTAINER_UUID      = "container_uuid";
    public static final String PROPERTY_FILE_ID             = "file_id";
    public static final String PROPERTY_IS_ZIP_CONTAINER    = "is_zip";
    public static final String PROPERTY_DOWNLOAD_HOST       = "download_host";
    public static final String PROPERTY_PERMANENTLY_OFFLINE = "permanently_offline";

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
        return SwisstransferComFolder.getPluginDomains();
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
            /* No URL-pattern since all items get added via crawler plugin. */
            ret.add("");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        /* This may return "/file/null" at the end which is fine, that is for .zip containers. */
        return this.getHost() + "://link/" + link.getStringProperty(PROPERTY_LINK_UUID) + "/container/" + link.getStringProperty(PROPERTY_CONTAINER_UUID) + "/file/" + this.getFID(link);
    }

    private String getFID(final DownloadLink link) {
        return link.getStringProperty(PROPERTY_FILE_ID);
    }

    private boolean isSingleZippedContaienr(final DownloadLink link) {
        return link.getBooleanProperty(PROPERTY_IS_ZIP_CONTAINER, false);
    }

    private boolean isPermanentlyOffline(final DownloadLink link) {
        return link.getBooleanProperty(PROPERTY_PERMANENTLY_OFFLINE, false);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        if (isSingleZippedContaienr(link)) {
            return false;
        } else {
            return true;
        }
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        if (isSingleZippedContaienr(link)) {
            return 1;
        } else {
            return 0;
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        /*
         * We cannot (or well, should not) check items since every single file item-check counts as download which deducts a
         * "max downloads until file gets deleted counter".
         */
        if (isPermanentlyOffline(link)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else {
            return AvailableStatus.UNCHECKABLE;
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link);
    }

    private void handleDownload(final DownloadLink link) throws Exception, PluginException {
        final String containerUUID = link.getStringProperty(PROPERTY_CONTAINER_UUID);
        final String fileUUID = link.getStringProperty(PROPERTY_FILE_ID);
        /* Old format: */
        // final String directurl = String.format("https://www.swisstransfer.com/api/download/%s/%s", linkUUID, fileid);
        String dllink = "https://" + link.getStringProperty(PROPERTY_DOWNLOAD_HOST) + "/api/download/" + link.getStringProperty(PROPERTY_LINK_UUID);
        if (!this.isSingleZippedContaienr(link)) {
            dllink += "/" + link.getStringProperty(PROPERTY_FILE_ID);
        }
        final String downloadPassword = link.getDownloadPassword();
        if (downloadPassword != null) {
            /* Special token needed to download password protected items */
            final Map<String, Object> postdata = new HashMap<String, Object>();
            postdata.put("password", downloadPassword);
            postdata.put("containerUUID", containerUUID);
            /* Can be null. If this is null, we got a zip container item. */
            postdata.put("fileUUID", fileUUID);
            final Browser brc = br.cloneBrowser();
            brc.getHeaders().put("Accept", "application/json, text/plain, */*");
            brc.getHeaders().put("Content-Type", "application/json");
            brc.getHeaders().put("Origin", "https://www." + getHost());
            brc.getHeaders().put("Referer", "https://www." + getHost() + "/");
            brc.postPageRaw("https://www." + getHost() + "/api/generateDownloadToken", JSonStorage.serializeToJson(postdata));
            /* Example response on invalid password: "{}" (without "") */
            final String downloadToken = brc.getRegex("^\"([a-f0-9\\-]+)\"$").getMatch(0);
            if (downloadToken == null) {
                throw new PluginException(LinkStatus.ERROR_FATAL, "Invalid download password?");
            }
            dllink += "?token=" + downloadToken;
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            Map<String, Object> entries = null;
            try {
                /* Try to parse json in case we got a json response */
                entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            } catch (final Throwable e) {
            }
            handleJsonErrorResponse: if (entries != null) {
                /* Look for errors in parsed json response */
                final String errormessage = (String) entries.get("message");
                final Object errorCodeO = entries.get("errorCode");
                if (errormessage == null) {
                    /* No response we can work with */
                    break handleJsonErrorResponse;
                } else if (errorCodeO == null || !errorCodeO.toString().matches("\\d+")) {
                    /* No response we can work with */
                    break handleJsonErrorResponse;
                }
                final int errorCode = Integer.parseInt(errorCodeO.toString());
                if ("Download Number Exceeded".equals(errormessage)) {
                    /* {"message":"Download Number Exceeded","errorCode":"500"} */
                    link.setProperty(PROPERTY_PERMANENTLY_OFFLINE, true);
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, errormessage);
                } else {
                    /* Unknown error */
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errorCode + ":" + errormessage);
                }
            }
            this.handleConnectionErrors(br, dl.getConnection());
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

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }
}