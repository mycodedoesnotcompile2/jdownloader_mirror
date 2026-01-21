package jd.plugins.decrypter;

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

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import org.appwork.utils.Regex;
import org.appwork.utils.formatter.SizeFormatter;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.http.requests.GetRequest;
import jd.nutils.encoding.Encoding;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 52135 $", interfaceVersion = 3, names = {}, urls = {})
public class MyrientEristaMeFolder extends PluginForDecrypt {
    public MyrientEristaMeFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        /* 2025-11-13: Try to avoid running into rate limits. */
        return 2;
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        /* html of such folders can be huge */
        br.setLoadLimit(Integer.MAX_VALUE);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(new String[] { "myrient.erista.me" });
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

    /**
     * Do not allow users "/files/" since this would crawl the whole website content. <br>
     * Prohibit "?" after "/files/" to try to prohibit user from adding main URL.
     */
    private static Pattern PATTERN_FOLDER = Pattern.compile("/files/(?!(?:\\?|#))(.+)", Pattern.CASE_INSENSITIVE);

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://" + buildHostsPatternPart(domains) + PATTERN_FOLDER.pattern());
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public ArrayList<DownloadLink> decryptIt(CryptedLink param, ProgressController progress) throws Exception {
        return this.crawlHTTPDirectory(param);
    }

    protected ArrayList<DownloadLink> crawlHTTPDirectory(final CryptedLink param) throws IOException, PluginException, DecrypterRetryException {
        final String url = param.getCryptedUrl();
        final GetRequest request = br.createGetRequest(url);
        URLConnectionAdapter con = null;
        try {
            con = br.openRequestConnection(request);
            if (this.looksLikeDownloadableContent(con)) {
                /* User has added a directURL. */
                final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
                final DownloadLink direct = getCrawler().createDirectHTTPDownloadLink(request, con);
                final String pathToFile = getCurrentDirectoryPath(url);
                /* Remove filename from path */
                final String pathToFolder = pathToFile.substring(0, pathToFile.lastIndexOf("/"));
                direct.setRelativeDownloadFolderPath(pathToFolder);
                final FilePackage fp = FilePackage.getInstance();
                fp.setName(pathToFolder);
                direct._setFilePackage(fp);
                ret.add(direct);
                return ret;
            }
            br.followConnection();
            con = br.getHttpConnection();
            if (con.getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            return this.parseHTTPDirectory(param, br);
        } finally {
            if (con != null) {
                con.disconnect();
            }
        }
    }

    /**
     * Does parsing only, without any HTTP requests!
     *
     * @throws DecrypterRetryException
     */
    public ArrayList<DownloadLink> parseHTTPDirectory(final CryptedLink param, final Browser br) throws IOException, PluginException, DecrypterRetryException {
        final String path = this.getCurrentDirectoryPath(br);
        if (path == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        /* Myrient specific parsing: <td class="link"><a href="...">...</a></td><td class="size">...</td><td class="date">...</td> */
        final String[] tableRows = br.getRegex("<tr>(.*?)</tr>").getColumn(0);
        if (tableRows == null || tableRows.length == 0) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(path);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        int numberofSkippedParentDirectories = 0;
        for (final String row : tableRows) {
            final String linkHtml = new Regex(row, "<td class=\"link\">(.*?)</td>").getMatch(0);
            if (linkHtml == null) {
                /* Skip invalid items */
                continue;
            }
            final String href = new Regex(linkHtml, "<a href=\"([^\"]+)\"").getMatch(0);
            if (href == null) {
                /* Skip invalid items */
                continue;
            }
            final String sizeText = new Regex(row, "<td class=\"size\">([^<]+)</td>").getMatch(0);
            if (sizeText == null) {
                /* Skip invalid items */
                continue;
            }
            /* Skip parent directory links */
            if (href.equals("./") || href.equals("../") || href.equals("..")) {
                numberofSkippedParentDirectories++;
                continue;
            }
            final DownloadLink link = parseEntry(br, href, sizeText.trim());
            link.setRelativeDownloadFolderPath(path);
            link._setFilePackage(fp);
            ret.add(link);
        }
        if (ret.isEmpty()) {
            if (numberofSkippedParentDirectories > 0) {
                /* Directory contained only links to parent directories but no files -> Assume we got an empty folder. */
                throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
            } else {
                /* We got zero results and no idea why */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        return ret;
    }

    protected DownloadLink parseEntry(final Browser br, final String href, final String filesizeStr) throws PluginException, IOException {
        final String decodedHref = Encoding.htmlOnlyDecode(href);
        final String url = br.getURL(decodedHref).toExternalForm();
        /* Is it a file or a folder? */
        if (filesizeStr.equals("-") || href.endsWith("/")) {
            /* Folder -> Will go back into this crawler */
            final DownloadLink dlfolder = this.createDownloadlink(url);
            return dlfolder;
        } else {
            /* File */
            final DownloadLink dlfile = new DownloadLink(null, null, "DirectHTTP", DirectHTTP.createURLForThisPlugin(url), true);
            /* Obtain filename from URL as displayed name may be truncated. */
            String name = url.substring(url.lastIndexOf("/") + 1);
            if (Encoding.isUrlCoded(name)) {
                name = Encoding.htmlDecode(name);
            } else {
                name = Encoding.htmlOnlyDecode(name);
            }
            dlfile.setName(name);
            final long fileSize = SizeFormatter.getSize(filesizeStr);
            dlfile.setDownloadSize(fileSize);
            dlfile.setAvailable(true);
            return dlfile;
        }
    }

    /**
     * Returns url-decoded directory path from browser URL.
     *
     * @throws PluginException
     */
    protected String getCurrentDirectoryPath(final Browser br) throws PluginException {
        final String path_ugly = br._getURL().getPath();
        final String path = new Regex(path_ugly, PATTERN_FOLDER).getMatch(0);
        if (path == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        String cleanPath = path;
        // Remove trailing slash if present
        if (cleanPath.endsWith("/")) {
            cleanPath = cleanPath.substring(0, cleanPath.length() - 1);
        }
        // Decode the path
        if (Encoding.isUrlCoded(cleanPath)) {
            cleanPath = Encoding.htmlDecode(cleanPath);
        } else {
            cleanPath = Encoding.htmlOnlyDecode(cleanPath);
        }
        return cleanPath;
    }

    /**
     * Returns url-DECODED path based on given url.
     *
     * @throws UnsupportedEncodingException
     */
    protected String getCurrentDirectoryPath(final String url) throws UnsupportedEncodingException {
        final String path = new Regex(url, PATTERN_FOLDER).getMatch(0);
        return URLDecoder.decode(path, "UTF-8");
    }
}