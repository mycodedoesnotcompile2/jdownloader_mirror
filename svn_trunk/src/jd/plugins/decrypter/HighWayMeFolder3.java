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
package jd.plugins.decrypter;

import java.net.URL;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.appwork.exceptions.WTFException;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.plugins.Account;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountRequiredException;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.download.HashInfo;
import jd.plugins.hoster.HighWayCore;
import jd.plugins.hoster.HighWayMe2;

/**
 * Crawler for the "HIGHWAY DAV JSON API" (new /dav and /cloud links). </br>
 * It recursively walks the users' HIGHWAY cloud via the JSON API and returns all contained files. </br>
 * Docs: https://high-way.me/threads/highway-api.201/ (section "HIGHWAY DAV JSON API")
 */
@DecrypterPlugin(revision = "$Revision: 53147 $", interfaceVersion = 3, names = {}, urls = {})
public class HighWayMeFolder3 extends PluginForDecrypt {
    public HighWayMeFolder3(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.BUBBLE_NOTIFICATION };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        HighWayCore.prepBRHighway(br);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "high-way.me" });
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
            ret.add("https?://cloud\\." + buildHostsPatternPart(domains) + "/cloud(/.+)?");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final Account account = AccountController.getInstance().getValidAccount(this.getHost());
        if (account == null) {
            errorAccountNeeded();
            /* Unreachable code */
            throw new WTFException();
        }
        final HighWayMe2 hosterplugin = (HighWayMe2) this.getNewPluginForHostInstance(this.getHost());
        /* Basic auth for the DAV JSON API is done with the Usenet credentials. */
        final String usenetUsername = hosterplugin.getUseNetUsername(account);
        final String usenetPassword = hosterplugin.getUseNetPassword(account);
        if (StringUtils.isEmpty(usenetUsername) || StringUtils.isEmpty(usenetPassword)) {
            /* Missing Usenet credentials -> Treat this as if there was no account at all. */
            logger.info("Account is missing Usenet credentials -> Treating it as if there was no account");
            errorAccountNeeded();
            /* Unreachable code */
            throw new WTFException();
        }
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_AUTHORIZATION, "Basic " + Encoding.Base64Encode(usenetUsername + ":" + usenetPassword));
        br.getHeaders().put("Accept", "application/json");
        /*
         * Normalize the added URL.
         */
        final String contenturl = param.getCryptedUrl();
        final URL added = new URL(contenturl);
        String startPath = added.getPath();
        // startPath = startPath.replaceFirst("(?i)^/dav", "/cloud");
        if (!startPath.endsWith("/")) {
            startPath += "/";
        }
        final String startURL = added.getProtocol() + "://" + added.getHost() + startPath;
        /* Recursively walk the cloud via the JSON API. */
        /* Make sure each folder URL is only crawled once. */
        final Set<String> crawledFolderURLs = new HashSet<String>();
        final List<String> folderQueue = new ArrayList<String>();
        int numberofFolders = 0;
        int numberofFiles = 0;
        int numberofEmptyFolders = 0;
        folderQueue.add(startURL);
        crawledFolderURLs.add(startURL);
        do {
            if (this.isAbort()) {
                logger.info("Stopping because: Aborted by user");
                break;
            }
            final String folderURL = folderQueue.remove(0);
            br.getPage(folderURL);
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            checkErrors(entries);
            final String currentPath = entries.get("path").toString();
            final List<Map<String, Object>> items = (List<Map<String, Object>>) entries.get("entries");
            if (items.isEmpty()) {
                logger.info("Found empty folder: " + folderURL);
                numberofEmptyFolders++;
                logger.info("Progress so far: Folders found: " + numberofFolders + " | Files found: " + numberofFiles + " | Empty folders: " + numberofEmptyFolders + " | Folders left in queue: " + folderQueue.size());
                continue;
            }
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(currentPath);
            /* Stable package key so re-crawled items get merged into the existing package instead of creating duplicates. */
            fp.setPackageKey("highwaydav://" + currentPath);
            for (final Map<String, Object> item : items) {
                final String type = item.get("type").toString();
                if (StringUtils.equalsIgnoreCase(type, "directory")) {
                    /* Enqueue subfolder for the next JSON query. */
                    final String subfolderURL = resolveURL(folderURL, item.get("url").toString());
                    if (crawledFolderURLs.add(subfolderURL)) {
                        folderQueue.add(subfolderURL);
                        numberofFolders++;
                    }
                } else if (StringUtils.equalsIgnoreCase(type, "file")) {
                    final String path = item.get("path").toString();
                    /*
                     * Stable canonical URL used as identifier only. The real (fresh, expiring) download URL is fetched by the host plugin
                     * on demand via the DAV JSON API.
                     */
                    final String canonicalURL = added.getProtocol() + "://" + added.getHost() + path.replace(" ", "%20");
                    final DownloadLink link = this.createDownloadlink(canonicalURL);
                    link.setName(item.get("name").toString());
                    link.setVerifiedFileSize(((Number) item.get("size")).longValue());
                    link.setRelativeDownloadFolderPath(currentPath);
                    /* Set file hashes if available (both fields can be null). */
                    final List<HashInfo> hashInfos = new ArrayList<HashInfo>();
                    final String md5 = (String) item.get("md5");
                    if (md5 != null) {
                        hashInfos.add(HashInfo.newInstanceSafe(md5, HashInfo.TYPE.MD5));
                    }
                    final String sha256 = (String) item.get("sha256");
                    if (sha256 != null) {
                        hashInfos.add(HashInfo.newInstanceSafe(sha256, HashInfo.TYPE.SHA256));
                    }
                    link.setHashInfos(hashInfos);
                    /* Let our high-way.me host plugin handle check & download of this cloud/DAV file. */
                    link.setHost(hosterplugin.getHost());
                    link.setDefaultPlugin(hosterplugin);
                    link.setAvailable(true);
                    link._setFilePackage(fp);
                    ret.add(link);
                    numberofFiles++;
                }
            }
            logger.info("Progress so far: Folders found: " + numberofFolders + " | Files found: " + numberofFiles + " | Empty folders: " + numberofEmptyFolders + " | Folders left in queue: " + folderQueue.size());
        } while (folderQueue.size() > 0);
        if (ret.isEmpty()) {
            if (numberofEmptyFolders > 0) {
                throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
            }
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return ret;
    }

    private void errorAccountNeeded() throws AccountRequiredException {
        displayBubbleNotification("Account benötigt", "Account benötigt, um Dateien aus der eigenen High-Way Cloud einfügen zu können.");
        throw new AccountRequiredException();
    }

    /** Handles the JSON error responses documented for the DAV JSON API. */
    private void checkErrors(final Map<String, Object> entries) throws Exception {
        final Object errorO = entries.get("error");
        if (errorO == null) {
            return;
        }
        final String error = errorO.toString();
        final String message = entries.get("message").toString();
        if (StringUtils.equalsIgnoreCase(error, "unauthorized")) {
            /* Invalid/missing Usenet credentials. */
            throw new AccountInvalidException(message);
        } else if (StringUtils.equalsIgnoreCase(error, "not_found") && message.toLowerCase().contains("premium")) {
            /* Account has no active premium traffic. */
            displayBubbleNotification("Kein Premium Traffic", message);
            throw new AccountRequiredException(message);
        } else {
            logger.info("Unhandled error: " + error + " | message: " + message);
        }
    }

    /** Resolves a possibly relative URL against the given base URL. */
    private String resolveURL(final String baseurl, final String ref) throws Exception {
        if (StringUtils.isEmpty(ref)) {
            return null;
        } else if (ref.matches("(?i)^https?://.+")) {
            return ref;
        } else {
            return new URL(new URL(baseurl), ref).toExternalForm();
        }
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        return 1;
    }
}
