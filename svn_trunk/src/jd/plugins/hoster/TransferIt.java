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
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Time;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.decrypter.TransferItCrawler;

/**
 * Most of the work on this plugin was done by Fabio Orlandi.
 */
@HostPlugin(revision = "$Revision: 53696 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { TransferItCrawler.class })
public class TransferIt extends PluginForHost {
    public TransferIt(PluginWrapper wrapper) {
        super(wrapper);
    }

    /* transfer.it is a service powered by MEGA and uses the MEGA API with "domain=transferit". */
    private static final String API_BASE             = "https://bt7.api.mega.co.nz/cs";
    public static final String  PROPERTY_TRANSFER_ID = "transfer_id";
    public static final String  PROPERTY_NODE_ID     = "node_id";
    private final AtomicLong    API_SEQUENCE         = new AtomicLong(Time.systemIndependentCurrentJVMTimeMillis() % 1000000000l);
    private String              dllink               = null;

    @Override
    public String getAGBLink() {
        return "https://" + getHost();
    }

    private static List<String[]> getPluginDomains() {
        return TransferItCrawler.getPluginDomains();
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
        final String transferID = link.getStringProperty(PROPERTY_TRANSFER_ID);
        final String nodeID = link.getStringProperty(PROPERTY_NODE_ID);
        if (transferID != null && nodeID != null) {
            /* Fixed identifier instead of getHost() so the ID stays stable even if the domain changes. */
            return "transferit://transfer/" + transferID + "/node/" + nodeID;
        } else {
            return super.getLinkID(link);
        }
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final Account account) {
        /* MEGA does not like much connections! */
        return -5;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        final String transferID = link.getStringProperty(PROPERTY_TRANSFER_ID);
        final String nodeID = link.getStringProperty(PROPERTY_NODE_ID);
        if (transferID == null || nodeID == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        /* "pt":1 = server delivers the file already decrypted. */
        final Map<String, Object> entries = callAPI(br, transferID, "{\"a\":\"g\",\"n\":\"" + nodeID + "\",\"pt\":1,\"g\":1,\"ssl\":1}");
        dllink = (String) entries.get("g");
        link.setVerifiedFileSize(((Number) entries.get("s")).longValue());
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(null));
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            if (dl.getConnection().getResponseCode() == 509) {
                /* MEGA transfer quota exceeded */
                br.followConnection(true);
                final String timeLeftString = dl.getConnection().getHeaderField("X-MEGA-Time-Left");
                long timeLeft = 30 * 60 * 1000l;
                if (timeLeftString != null && timeLeftString.matches("^\\d+$")) {
                    timeLeft = Math.max(timeLeft, Long.parseLong(timeLeftString) * 1000l);
                }
                throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, "Transfer quota exceeded", timeLeft);
            }
            this.handleConnectionErrors(br, dl.getConnection());
        }
        dl.startDownload();
    }

    /**
     * Sends one command to the MEGA API in transfer.it context and returns its result object. </br>
     * transferID: the "x" parameter scoping file related commands to one transfer, null for commands which take it as argument.
     */
    public Map<String, Object> callAPI(final Browser br, final String transferID, final String commandJson) throws Exception {
        String url = API_BASE + "?id=" + API_SEQUENCE.incrementAndGet() + "&v=3&domain=transferit";
        if (transferID != null) {
            url += "&x=" + transferID;
        }
        br.postPageRaw(url, "[" + commandJson + "]");
        final String html = br.getRequest().getHtmlCode();
        if (br.getHttpConnection().getResponseCode() != 200 || html == null || html.trim().isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "API unavailable", 5 * 60 * 1000l);
        }
        final String response = html.trim();
        /* Errors come as plain number e.g. "-9", as list e.g. "[-9]" or as object e.g. "[{"e":-9}]". */
        if (response.matches("^-?\\d+$")) {
            throwAPIError(Integer.parseInt(response));
            /* Unreachable code */
            throw new WTFException();
        }
        final List<?> parsed = (List<?>) JSonStorage.restoreFromString(response, TypeRef.OBJECT);
        final Object result = parsed.get(0);
        if (result instanceof Number) {
            throwAPIError(((Number) result).intValue());
            /* Unreachable code */
            throw new WTFException();
        }
        @SuppressWarnings("unchecked")
        final Map<String, Object> entries = (Map<String, Object>) result;
        /* Error code is only present in case of an error. */
        final Object errorcode = entries.get("e");
        if (errorcode instanceof Number && ((Number) errorcode).intValue() < 0) {
            throwAPIError(((Number) errorcode).intValue());
            /* Unreachable code */
            throw new WTFException();
        }
        return entries;
    }

    /**
     * See MEGA API error codes e.g. in MegaNz plugin. </br>
     * ERROR_FATAL is only used for -14 (EKEY: transfer.it asks for the password on this error), the crawler relies on that.
     */
    private static void throwAPIError(final int errorcode) throws PluginException {
        switch (errorcode) {
        case -8:
        case -9:
        case -16:
            /* -8 = expired, -9 = not found, -16 = blocked/taken down */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        case -3:
        case -4:
            /* -3 = try again, -4 = rate limit */
            throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "API error " + errorcode, 5 * 60 * 1000l);
        case -6:
        case -11:
        case -18:
            /* -6 = too many requests, -11 = access violation, -18 = temporarily unavailable */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "API error " + errorcode, 5 * 60 * 1000l);
        case -14:
            throw new PluginException(LinkStatus.ERROR_FATAL, "Password protected transfer.it transfers are not yet supported");
        case -17:
            throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, "Request over quota", 60 * 60 * 1000l);
        default:
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "API error " + errorcode);
        }
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }
}
