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
import java.util.regex.Pattern;

import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.Request;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
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
import jd.plugins.hoster.EmuParadiseMe;

/**
 * @author raztoki
 */
@DecrypterPlugin(revision = "$Revision: 51708 $", interfaceVersion = 2, names = {}, urls = {})
public class EmuParadiseMeCrawler extends PluginForDecrypt {
    public EmuParadiseMeCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        return 1;
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "emuparadise.me" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(" + EmuParadiseMe.TYPE_ROM.pattern() + "|" + EmuParadiseMe.TYPE_ROM_OLD.pattern() + ")");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, final ProgressController progress) throws Exception {
        final EmuParadiseMe hosterplugin = (EmuParadiseMe) this.getNewPluginForHostInstance(this.getHost());
        final String contenturl = param.getCryptedUrl();
        br.getPage(contenturl);
        if (hosterplugin.isOffline(br)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String[] urls_single_files = br.getRegex(jd.plugins.hoster.EmuParadiseMe.TYPE_DOWNLOAD).getColumn(-1);
        if (urls_single_files != null && urls_single_files.length > 0) {
            String title = br.getRegex("\"name\":\\s*\"([^\"]+)").getMatch(0);
            final FilePackage fp = FilePackage.getInstance();
            if (title != null) {
                fp.setName(Encoding.htmlDecode(title.trim()));
            } else {
                /* Fallback */
                fp.setName(br._getURL().getPath());
            }
            logger.info("Found single file downloads: " + urls_single_files.length);
            for (String url : urls_single_files) {
                final String absolute_url = Request.getLocation(url, br.getRequest());
                final DownloadLink link = createDownloadlink(absolute_url);
                final Regex regex = new Regex(br, Pattern.quote(url) + "\"[^>]*>\\s*Download ([^<]+)</a>\\s*\\(([^\\)]+)\\)");
                if (regex.patternFind()) {
                    String filename = regex.getMatch(0);
                    filename = Encoding.htmlDecode(filename).trim();
                    String filesizeStr = regex.getMatch(1);
                    filesizeStr = hosterplugin.correctFilesize(filesizeStr);
                    final String filename_quoted = Pattern.quote(filename);
                    if (br.containsHTML(filename_quoted + "\\.7z")) {
                        filename += ".7z";
                    } else if (br.containsHTML(filename_quoted + "\\.zip")) {
                        filename += ".zip";
                    } else {
                        /* Having the correct file extension is important for the "unavailableWorkaroundUsed", see hoster plugin. */
                        logger.warning("Failed to find file extension for file: " + filename);
                        filename += EmuParadiseMe.EXT_DEFAULT;
                    }
                    link.setProperty(EmuParadiseMe.PROPERTY_DOWNLOAD_LINK_FILENAME, filename);
                    link.setFinalFileName(filename);
                    link.setDownloadSize(SizeFormatter.getSize(filesizeStr));
                } else {
                    logger.warning("Failed to find file information for: " + url);
                }
                link.setAvailable(true);
                link._setFilePackage(fp);
                ret.add(link);
            }
            return ret;
        }
        final String[] urls_roms = br.getRegex("Info and Download\" href=\"(/[^/]+/[^/]+/\\d+)\"").getColumn(0);
        if (urls_roms != null && urls_roms.length > 0) {
            /**
             * "Category" link e.g. /Atari_2600_ROMs/Games-Starting-With-C/49 <br>
             * e.g. <br>
             * -> Find all "ROMs" -> Links will go back into this crawler and it will look for single file URLs
             */
            if (param.getDownloadLink() != null) {
                logger.warning("Current link already came from another crawler -> Deeper level crawling is not allowed for this kind of links to prevent accidentally crawling the whole website");
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            for (String url : urls_roms) {
                final String absolute_url = Request.getLocation(url, br.getRequest());
                ret.add(this.createDownloadlink(absolute_url));
            }
            return ret;
        }
        final String[] directurls = br.getRegex("Download:\\s*<a href=\"([^\"]+)\"").getColumn(0);
        if (directurls != null && directurls.length > 0) {
            /**
             * Category type 2 <br>
             * Examples single file: <br>
             * /Amiga_Emulators/Windows/4 <br>
             * /Atari_7800_Emulators/Mac_OS_X/47 <br>
             * /Acorn_BBC_Micro_Emulators/Windows/59 <br>
             * Examples multiple files: <br>
             * /Atari_2600_Emulators/Windows/49
             */
            final String[] filesizes = br.getRegex("Size:([^<]+)<br>").getColumn(0);
            for (int i = 0; i < directurls.length; i++) {
                final String url = directurls[i];
                final String absolute_url = Request.getLocation(url, br.getRequest());
                final DownloadLink file = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(absolute_url));
                if (filesizes != null && filesizes.length == directurls.length) {
                    String filesizeStr = filesizes[i];
                    if (!StringUtils.endsWithCaseInsensitive(filesizeStr, "b")) {
                        filesizeStr += "b";
                    }
                    file.setDownloadSize(SizeFormatter.getSize(filesizeStr));
                }
                file.setAvailable(true);
                ret.add(file);
            }
            if (ret.size() > 1) {
                final FilePackage fp = FilePackage.getInstance();
                fp.setName(br._getURL().getPath());
                fp.addLinks(ret);
            }
            return ret;
        }
        /* Zero results -> Perform deeper check on why */
        if (br.containsHTML(">\\s*No games found")) {
            /* Example: /Atari_2600_ROMs/Genre/Soccer/49 */
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
        } else {
            logger.info("Found zero results -> Assume we got an empty category");
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
        }
        // throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }
}
