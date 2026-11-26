//jDownloader - Downloadmanager
//Copyright (C) 2026  JD-Team support@jdownloader.org
//
//This program is free software: you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation, either version 3 of the License, or
//(at your option) any later version.
//
//This program is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
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
import org.appwork.utils.StringUtils;

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

@HostPlugin(revision = "$Revision: 54198 $", interfaceVersion = 3, names = {}, urls = {})
public class FiledonCo extends PluginForHost {
    public FiledonCo(PluginWrapper wrapper) {
        super(wrapper);
    }

    private static final String PROPERTY_DIRECTURL = "free_directurl";

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/terms";
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry: new String[] {"domain.tld", "domain2.tld", ...}
        ret.add(new String[] { "filedon.co" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(?:view|embed)/([A-Za-z0-9]+)");
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

    /** Normalizes view and embed links to the view URL. */
    private String getViewURL(final DownloadLink link) {
        return "https://" + getHost() + "/view/" + getFID(link);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final Account account) {
        return 0;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        this.setBrowserExclusive();
        br.getPage(getViewURL(link));
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> page = getInertiaPage(br);
        parseFileInfo(link, page);
        return AvailableStatus.TRUE;
    }

    /**
     * Reads file information from the Inertia page object and throws matching exceptions for offline/special states.
     */
    private void parseFileInfo(final DownloadLink link, final Map<String, Object> page) throws PluginException {
        final String component = (String) page.get("component");
        final Map<String, Object> props = (Map<String, Object>) page.get("props");
        if (StringUtils.equalsIgnoreCase(component, "public/view-password")) {
            final String filename = props != null ? (String) props.get("file_name") : null;
            if (!StringUtils.isEmpty(filename)) {
                link.setName(filename);
            }
            link.setPasswordProtected(true);
            return;
        } else if (StringUtils.equalsIgnoreCase(component, "public/view-limit")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Download limit of this file has been reached");
        } else if (StringUtils.equalsIgnoreCase(component, "public/view-expired")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Link expired");
        } else if (StringUtils.equalsIgnoreCase(component, "public/view-private")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "File is private");
        } else if (StringUtils.equalsIgnoreCase(component, "public/view-quarantined")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "File has been quarantined");
        } else if (StringUtils.equalsIgnoreCase(component, "public/account-blocked")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Uploader account is blocked");
        } else if (!StringUtils.equalsIgnoreCase(component, "public/view")) {
            /* e.g. 404 page, maintenance, main page (redirect) etc. */
            if (StringUtils.equalsIgnoreCase(component, "public/maintenance")) {
                throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Maintenance", 30 * 60 * 1000l);
            }
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        link.setPasswordProtected(false);
        final Map<String, Object> sharing = (Map<String, Object>) props.get("sharing");
        Map<String, Object> file = sharing != null ? (Map<String, Object>) sharing.get("files") : null;
        if (file == null) {
            file = (Map<String, Object>) props.get("files");
        }
        if (file == null) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String status = (String) file.get("status");
        if (status != null && !StringUtils.equalsIgnoreCase(status, "active")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String filename = (String) file.get("name");
        if (!StringUtils.isEmpty(filename)) {
            link.setFinalFileName(filename);
        }
        final Object filesize = file.get("size");
        if (filesize instanceof Number) {
            link.setVerifiedFileSize(((Number) filesize).longValue());
        }
        /* Check for errors that can happen while file information is still given. */
        final Map<String, Object> sharingMeta = (Map<String, Object>) props.get("sharing_meta");
        if (sharingMeta != null) {
            if (Boolean.TRUE.equals(sharingMeta.get("is_expired"))) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Link expired");
            } else if (Boolean.TRUE.equals(sharingMeta.get("limit_reached"))) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Download limit of this file has been reached");
            }
        }
    }

    /** Extracts the Inertia.js page object from the data-page attribute. */
    private Map<String, Object> getInertiaPage(final Browser br) throws PluginException {
        final String json = br.getRegex("data-page=\"([^\"]+)\"").getMatch(0);
        if (json == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return restoreFromString(Encoding.htmlOnlyDecode(json), TypeRef.MAP);
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link, null);
    }

    private void handleDownload(final DownloadLink link, final Account account) throws Exception, PluginException {
        final String storedDirecturl = link.getStringProperty(PROPERTY_DIRECTURL);
        String dllink;
        if (storedDirecturl != null) {
            logger.info("Trying to re-use stored directurl: " + storedDirecturl);
            dllink = storedDirecturl;
        } else {
            requestFileInformation(link);
            Map<String, Object> page = getInertiaPage(br);
            if (link.isPasswordProtected()) {
                page = handlePassword(link);
            }
            final Map<String, Object> props = (Map<String, Object>) page.get("props");
            final Map<String, Object> sharingMeta = (Map<String, Object>) props.get("sharing_meta");
            if (sharingMeta != null && Boolean.FALSE.equals(sharingMeta.get("allow_download"))) {
                throw new PluginException(LinkStatus.ERROR_FATAL, "Uploader has disabled downloads for this file");
            }
            final String csrftoken = getCsrfToken(br);
            final Browser brc = br.cloneBrowser();
            brc.setFollowRedirects(false);
            brc.getHeaders().put("X-Requested-With", "XMLHttpRequest");
            brc.getHeaders().put("X-CSRF-TOKEN", csrftoken);
            brc.getHeaders().put("Content-Type", "application/json");
            brc.getHeaders().put("Accept", "text/html, application/xhtml+xml");
            brc.getHeaders().put("Origin", "https://" + getHost());
            brc.postPageRaw("/download/" + getFID(link), "{}");
            if (brc.getHttpConnection().getResponseCode() == 429) {
                throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Too many requests", 60 * 1000l);
            } else if (brc.getHttpConnection().getResponseCode() == 419) {
                /* Laravel: CSRF token mismatch / session expired */
                throw new PluginException(LinkStatus.ERROR_RETRY, "CSRF token mismatch");
            }
            /* POST responds with a 302 redirect back to the view page, which contains the download URL as a flash prop. */
            br.getPage(getViewURL(link));
            final Map<String, Object> page2 = getInertiaPage(br);
            final Map<String, Object> props2 = (Map<String, Object>) page2.get("props");
            final Map<String, Object> flash = (Map<String, Object>) props2.get("flash");
            dllink = flash != null ? (String) flash.get("download_url") : null;
            if (StringUtils.isEmpty(dllink)) {
                final Map<String, Object> errors = (Map<String, Object>) props2.get("errors");
                if (errors != null && !errors.isEmpty()) {
                    final Object msg = errors.values().iterator().next();
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, String.valueOf(msg), 5 * 60 * 1000l);
                }
                final String flashError = flash != null ? (String) flash.get("error") : null;
                if (!StringUtils.isEmpty(flashError)) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, flashError, 5 * 60 * 1000l);
                }
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, account), this.getMaxChunks(account));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                if (dl.getConnection().getResponseCode() == 403) {
                    throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Server error 403", 60 * 60 * 1000l);
                } else if (dl.getConnection().getResponseCode() == 404) {
                    throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Server error 404", 60 * 60 * 1000l);
                }
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Final downloadurl did not lead to downloadable content");
            }
        } catch (final Exception e) {
            if (storedDirecturl != null) {
                link.removeProperty(PROPERTY_DIRECTURL);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Stored directurl expired", e);
            } else {
                throw e;
            }
        }
        /* Signed R2 URL is valid for 3600 seconds. */
        link.setProperty(PROPERTY_DIRECTURL, dl.getConnection().getURL().toExternalForm());
        dl.startDownload();
    }

    /** Submits the password and returns the Inertia page object of the unlocked view page. */
    private Map<String, Object> handlePassword(final DownloadLink link) throws Exception {
        String passCode = link.getDownloadPassword();
        if (passCode == null) {
            passCode = getUserInput("Password?", link);
        }
        final String csrftoken = getCsrfToken(br);
        final Browser brc = br.cloneBrowser();
        brc.setFollowRedirects(false);
        brc.getHeaders().put("X-Requested-With", "XMLHttpRequest");
        brc.getHeaders().put("X-CSRF-TOKEN", csrftoken);
        brc.getHeaders().put("Content-Type", "application/json");
        brc.getHeaders().put("Origin", "https://" + getHost());
        final Map<String, Object> postdata = new HashMap<String, Object>();
        postdata.put("password", passCode);
        brc.postPageRaw("/view/" + getFID(link), JSonStorage.serializeToJson(postdata));
        br.getPage(getViewURL(link));
        final Map<String, Object> page = getInertiaPage(br);
        if (StringUtils.equalsIgnoreCase((String) page.get("component"), "public/view-password")) {
            link.setDownloadPassword(null);
            throw new PluginException(LinkStatus.ERROR_RETRY, "Wrong password entered");
        }
        link.setDownloadPassword(passCode);
        parseFileInfo(link, page);
        return page;
    }

    private String getCsrfToken(final Browser br) throws PluginException {
        final String csrftoken = br.getRegex("name=\"csrf-token\" content=\"([^\"]+)\"").getMatch(0);
        if (csrftoken == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find csrf-token");
        }
        return csrftoken;
    }

    @Override
    protected String getDefaultFileName(final DownloadLink link) {
        return this.getFID(link);
    }
}
