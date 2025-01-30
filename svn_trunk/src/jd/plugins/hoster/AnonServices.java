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
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.parser.UrlQuery;

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
import jd.plugins.download.HashInfo;

@HostPlugin(revision = "$Revision: 50526 $", interfaceVersion = 3, names = {}, urls = {})
public class AnonServices extends PluginForHost {
    public AnonServices(PluginWrapper wrapper) {
        super(wrapper);
    }

    /** Using API from: https://anon.services/static/anon_cli.py */
    public static String API_BASE                                  = "https://anon.services/api";
    private final String PROPERTY_ALLOW_DOWNLOAD_PASSWORD_FROM_URL = "allow_download_password_from_url";

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        br.getHeaders().put("User-Agent", "JDownloader");
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/terms";
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "anon.services" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(?:download|file)/([A-Fa-f0-9\\-]{32,}).*");
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

    public static boolean looksLikeValidContentID(final String str) {
        if (str == null) {
            return false;
        } else if (!str.replace("-", "").matches("[a-f0-9]{32}")) {
            return false;
        } else {
            return true;
        }
    }

    private String getFID(final DownloadLink link) {
        String fid = new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
        if (fid == null) {
            return null;
        }
        /* API does not like uppercase! */
        fid = fid.toLowerCase(Locale.ENGLISH);
        return fid;
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 0;
    }

    @Override
    public String getPluginContentURL(final DownloadLink link) {
        return getFileURL(link, this.getStoredDownloadPassword(link));
    }

    public String getFileURL(final DownloadLink link, final String passCode) {
        return getFileURL(this.getFID(link), passCode);
    }

    public String getFileURL(final String fileID, final String passCode) {
        String url = "https://" + getHost() + "/file/" + fileID;
        if (passCode != null) {
            url += "?file_pw=" + Encoding.urlEncode(passCode);
        }
        return url;
    }

    public String getFolderURL(final String folderID, final String passCode) {
        String url = "https://" + getHost() + "/folder/" + folderID;
        if (passCode != null) {
            url += "?folder_pw=" + Encoding.urlEncode(passCode);
        }
        return url;
    }

    private String getDownloadPasswordFromURL(final DownloadLink link) {
        final boolean allowPassCodeFromURL = link.getBooleanProperty(PROPERTY_ALLOW_DOWNLOAD_PASSWORD_FROM_URL, true);
        if (!allowPassCodeFromURL) {
            return null;
        }
        try {
            final String passCodeFromURL = UrlQuery.parse(link.getPluginPatternMatcher()).get("file_pw");
            return passCodeFromURL;
        } catch (MalformedURLException e) {
            e.printStackTrace();
            return null;
        }
    }

    private String getStoredDownloadPassword(final DownloadLink link) {
        final String passCodeFromURL = getDownloadPasswordFromURL(link);
        if (passCodeFromURL != null) {
            return passCodeFromURL;
        } else {
            return link.getDownloadPassword();
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        return requestFileInformationAPI(this.br, link);
    }

    private AvailableStatus requestFileInformationAPI(final Browser br, final DownloadLink link) throws IOException, PluginException {
        final String fid = this.getFID(link);
        if (!link.isNameSet()) {
            /* Fallback */
            link.setName(fid);
        } else if (!looksLikeValidContentID(fid)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        br.getPage(API_BASE + "/files/" + fid);
        if (br.getHttpConnection().getResponseCode() == 403) {
            /* We know that the file is online but we cannot get more information. */
            /* E.g. {"error":"Folder password required or invalid"} */
            link.setPasswordProtected(true);
            return AvailableStatus.TRUE;
        } else if (br.getHttpConnection().getResponseCode() == 404) {
            /* E.g. {"msg":"File not found"} */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* If we reach this, that file is definitely not (anymore) password protected. */
        link.setPasswordProtected(false);
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        /* Check for other/unknown errors */
        final String msg = (String) entries.get("msg");
        if (msg != null) {
            throw new PluginException(LinkStatus.ERROR_FATAL, msg);
        }
        link.setFinalFileName(entries.get("filename").toString());
        link.setVerifiedFileSize(((Number) entries.get("size")).longValue());
        if (Boolean.TRUE.equals(entries.get("banned"))) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "File has been banned");
        } else if (Boolean.TRUE.equals(entries.get("deleted"))) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "File has been deleted");
        }
        return AvailableStatus.TRUE;
    }

    private AvailableStatus requestFileInformationWebsite(final Browser br, final DownloadLink link, final String downloadPassword) throws IOException, PluginException {
        /* Ensure that this also works for password protected links. */
        br.getPage(this.getFileURL(link, downloadPassword));
        checkErrorsWebsite(br, link);
        String filename = br.getRegex("Filename\\s*</th><td[^>]*>([^<]+)</td>").getMatch(0);
        if (filename != null) {
            filename = Encoding.htmlDecode(filename).trim();
            link.setName(filename);
        } else {
            logger.warning("Failed to find filename");
        }
        final String filesizeStr = br.getRegex("Size\\s*</th>\\s*<td>([^<]+)</td>").getMatch(0);
        if (filesizeStr != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesizeStr));
        } else {
            logger.warning("Failed to find filesize");
        }
        final String hash_sha256 = br.getRegex("SHA256:\\s*([a-f0-9]{64})").getMatch(0);
        final String hash_sha1 = br.getRegex("SHA1:\\s*([a-f0-9]{40})").getMatch(0);
        final String hash_md5 = br.getRegex("MD5:\\s*([a-f0-9]{32})").getMatch(0);
        final String hash_crc32 = br.getRegex("CRC32:\\s*([a-f0-9]{8})").getMatch(0);
        /* Set hashes for CRC check */
        if (!StringUtils.isEmpty(hash_crc32)) {
            link.addHashInfo(HashInfo.newInstanceSafe(hash_crc32, HashInfo.TYPE.CRC32));
        }
        if (!StringUtils.isEmpty(hash_md5)) {
            link.addHashInfo(HashInfo.newInstanceSafe(hash_md5, HashInfo.TYPE.MD5));
        }
        if (!StringUtils.isEmpty(hash_sha1)) {
            link.addHashInfo(HashInfo.newInstanceSafe(hash_sha1, HashInfo.TYPE.SHA1));
        }
        if (!StringUtils.isEmpty(hash_sha256)) {
            link.addHashInfo(HashInfo.newInstanceSafe(hash_sha256, HashInfo.TYPE.SHA256));
        }
        if (link.getHashInfo() == null) {
            logger.warning("Failed to find any file hashes");
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link);
    }

    private void handleDownload(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        String dllink = "https://" + getHost() + "/download/" + this.getFID(link);
        String passCode = getStoredDownloadPassword(link);
        if (passCode == null && link.isPasswordProtected()) {
            /* Ask user to enter password. */
            passCode = getUserInput("Password?", link);
        }
        if (passCode != null) {
            dllink += "?file_pw=" + Encoding.urlEncode(passCode);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            checkErrorsWebsite(br, link);
            throwConnectionExceptions(br, dl.getConnection());
            throwFinalConnectionException(br, dl.getConnection());
            /* Dead-end */
        }
        if (passCode != null && !StringUtils.equals(link.getDownloadPassword(), passCode)) {
            /* Store for later usage */
            logger.info("Found valid download password: " + passCode);
            link.setDownloadPassword(passCode);
        }
        if (link.getHashInfo() == null && org.jdownloader.settings.staticreferences.CFG_GENERAL.CFG.isHashCheckEnabled()) {
            /* Crawl file hashes via website as API does not provide them. */
            logger.info("Performing linkcheck over website to find file hashes");
            requestFileInformationWebsite(this.createNewBrowserInstance(), link, passCode);
        }
        dl.startDownload();
    }

    private void checkErrorsWebsite(final Browser br, final DownloadLink link) throws PluginException {
        if (br.getHttpConnection().getResponseCode() == 404) {
            /* E.g. {"msg":"File not found"} */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (isPasswordProtectedWebsite(br)) {
            if (!link.isPasswordProtected()) {
                /* This shall never happen as the correct password protected status should have been set before. */
                logger.info("Got unespected 'file is password protected' message.");
                link.setPasswordProtected(true);
            }
            final String downloadPasswordFromURL = this.getDownloadPasswordFromURL(link);
            if (downloadPasswordFromURL != null) {
                /* Assume that password from URL was used -> Invalidate that */
                link.setProperty(PROPERTY_ALLOW_DOWNLOAD_PASSWORD_FROM_URL, false);
                if (downloadPasswordFromURL.equals(link.getDownloadPassword())) {
                    /*
                     * Same [wrong] password was also set on DownloadLink -> Invalidate it here too so we don't retry with the same wrong
                     * password.
                     */
                    link.setDownloadPassword(null);
                }
            } else {
                /* Assume that password from DownloadLink was used -> Invalidate that */
                link.setDownloadPassword(null);
            }
            throw new PluginException(LinkStatus.ERROR_RETRY, "Wrong password entered");
        }
    }

    private boolean isPasswordProtectedWebsite(final Browser br) {
        return br.containsHTML(">\\s*File is password protected");
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