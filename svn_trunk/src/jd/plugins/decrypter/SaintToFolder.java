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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.SaintTo;

import org.jdownloader.scripting.JavaScriptEngineFactory;

@DecrypterPlugin(revision = "$Revision: 53687 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { SaintTo.class })
public class SaintToFolder extends PluginForDecrypt {
    public SaintToFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        return SaintTo.getPluginDomains();
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/a/([a-zA-Z0-9-_]{5,})");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {

        final String contenturl = param.getCryptedUrl();
        final String folderID = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*Album is private, removed")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*No files in this album\\s*<")) {
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
        }

        ArrayList<DownloadLink> ret = parseVeryOld(br);
        if (ret == null) {
            ret = parseOld(br);
            if (ret == null) {
                ret = parseJSON(br);
                if (ret == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
        }
        String title = br.getRegex("<title>([^<]+) - Saint Video Hosting</title>").getMatch(0);
        if (title == null) {
            title = br.getRegex("<h1[^>]*>([^<]+)</h1>").getMatch(0);
            if (title == null) {
                /* Fallback */
                logger.warning("Failed to find folder title");
                title = folderID;
            }
        }
        title = Encoding.htmlDecode(title).trim();
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(title);
        fp.setPackageKey(this.getHost() + "://folder/" + folderID);
        fp.addLinks(ret);
        return ret;
    }

    private ArrayList<DownloadLink> parseVeryOld(Browser br) throws Exception {
        final String[] htmls = br.getRegex("<tr class=\"file-list__file\">(.*?)</tr>").getColumn(0);
        if (htmls == null || htmls.length == 0) {
            return null;
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        /* Old website */
        for (final String html : htmls) {
            final String url = new Regex(html, "file_dwn\\('(https?://[^']+)'\\)").getMatch(0);
            final String filename = new Regex(html, "class=\"filename\"[^>]*>([^<]+)</a>").getMatch(0);
            final String filesizeBytesStr = new Regex(html, "class=\"fs\" data=\"(\\d+)\"").getMatch(0);
            if (url == null || filename == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final DownloadLink file = createDownloadlink(url);
            file.setName(Encoding.htmlDecode(filename).trim());
            if (filesizeBytesStr != null) {
                file.setVerifiedFileSize(Long.parseLong(filesizeBytesStr));
            } else {
                logger.warning("Failed to find filesize for item: " + url);
            }
            file.setAvailable(true);
            ret.add(file);
        }
        return ret;
    }

    private ArrayList<DownloadLink> parseJSON(Browser br) throws Exception {
        final String json = br.getRegex("const[^>]*FILES[^>]*=[^>]*(\\[.*?\\])\\s*;").getMatch(0);
        final List<Object> list = (List<Object>) JavaScriptEngineFactory.jsonToJavaObject(json);
        if (json == null || list == null || list.size() == 0) {
            if (br.containsHTML(">\\s*Nothing in this album matches that")) {
                throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
            }
            return null;
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        for (Object entry : list) {
            final Map<String, Object> map = (Map<String, Object>) entry;
            final String file_id = map.get("id").toString();
            final String url = "https://" + br.getHost() + "/d/" + file_id;
            final DownloadLink link = this.createDownloadlink(url);
            final String name = map.get("name").toString();
            link.setFinalFileName(name);
            final Object size = map.get("size");
            if (size instanceof Number) {
                link.setVerifiedFileSize(((Number) size).longValue());
            } else if (size instanceof String) {
                link.setVerifiedFileSize(Long.parseLong((String) size));
            }
            link.setAvailable(true);
            ret.add(link);
        }
        return ret;
    }

    private ArrayList<DownloadLink> parseOld(Browser br) throws Exception {
        final String[] file_ids = br.getRegex("data-id=\"([a-zA-Z0-9-_]+)\"").getColumn(0);
        if (file_ids == null || file_ids.length == 0) {
            return null;
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String[] filenames = br.getRegex("data-name=\"([^\"]+)\"").getColumn(0);
        final String[] filesizes = br.getRegex("data-size=\"(\\d+)\"").getColumn(0);
        int i = 0;
        for (final String file_id : file_ids) {
            final String url = "https://" + br.getHost() + "/d/" + file_id;
            final DownloadLink link = this.createDownloadlink(url);
            if (filenames != null && filenames.length == file_ids.length) {
                final String filename = filenames[i];
                link.setName(filename);
            } else if (i == 0) {
                /* Log only once */
                logger.warning("Failed to find filename information");
            }
            if (filesizes != null && filesizes.length == file_ids.length) {
                final String filesizeBytesStr = filesizes[i];
                link.setDownloadSize(Long.parseLong(filesizeBytesStr));
            } else if (i == 0) {
                /* Log only once */
                logger.warning("Failed to find filesize information");
            }
            link.setAvailable(true);
            ret.add(link);
            i++;
        }
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}
