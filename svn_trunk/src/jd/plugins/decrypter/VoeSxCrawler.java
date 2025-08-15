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
import java.util.regex.Pattern;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.PluginForHost;
import jd.plugins.hoster.VoeSx;

import org.appwork.storage.TypeRef;
import org.appwork.utils.Regex;
import org.jdownloader.plugins.components.config.XFSConfigVideoVoeSx;
import org.jdownloader.plugins.config.PluginJsonConfig;

@DecrypterPlugin(revision = "$Revision: 51328 $", interfaceVersion = 3, names = {}, urls = {})
public class VoeSxCrawler extends PluginForDecrypt {
    public VoeSxCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(new String[] { "voe.sx", "voe-unblock.com", "voe-unblock.net", "voeunblock.com", "voeunblk.com", "voeunblck.com", "voe-un-block.com", "un-block-voe.net", "voeunbl0ck.com", "voeunblock1.com", "voeunblock2.com", "voeunblock3.com", "voeunblock4.com", "voeunblock5.com", "voeunblock6.com", "voeun-block.net", "v-o-e-unblock.com", "audaciousdefaulthouse.com", "launchreliantcleaverriver.com", "reputationsheriffkennethsand.com", "fittingcentermondaysunday.com", "housecardsummerbutton.com", "fraudclatterflyingcar.com", "bigclatterhomesguideservice.com", "uptodatefinishconferenceroom.com", "realfinanceblogcenter.com", "tinycat-voe-fashion.com", "20demidistance9elongations.com", "telyn610zoanthropy.com", "toxitabellaeatrebates306.com", "greaseball6eventual20.com", "745mingiestblissfully.com", "19turanosephantasia.com", "30sensualizeexpression.com", "321naturelikefurfuroid.com",
                "449unceremoniousnasoseptal.com", "cyamidpulverulence530.com", "boonlessbestselling244.com", "antecoxalbobbing1010.com", "matriculant401merited.com", "scatch176duplicities.com", "35volitantplimsoles5.com", "tummulerviolableness.com", "tubelessceliolymph.com", "availedsmallest.com", "counterclockwisejacky.com", "monorhinouscassaba.com", "tummulerviolableness.com", "urochsunloath.com", "simpulumlamerop.com", "wolfdyslectic.com", "metagnathtuggers.com", "gamoneinterrupted.com", "chromotypic.com", "crownmakermacaronicism.com", "generatesnitrosate.com", "yodelswartlike.com", "figeterpiazine.com", "cigarlessarefy.com", "valeronevijao.com", "apinchcaseation.com", "nectareousoverelate.com", "phenomenalityuniform.com", "nonesnanking.com", "troyyourlead.com", "stevenimaginelittle.com", "edwardarriveoften.com", "lukecomparetwo.com", "bradleyviewdoctor.com", "jamiesamewalk.com",
                "seanshowcould.com", "sandrataxeight.com", "jayservicestuff.com", "graceaddresscommunity.com", "loriwithinfamily.com", "roberteachfinal.com", "erikcoldperson.com", "jasminetesttry.com", "heatherdiscussionwhen.com", "robertplacespace.com", "alleneconomicmatter.com", "josephseveralconcern.com", "donaldlineelse.com", "bethshouldercan.com", "thomasalthoughhear.com", "richardstorehalf.com", "brittneystandardwestern.com", "sandratableother.com", "robertordercharacter.com", "maxfinishseveral.com", "alejandrocenturyoil.com", "heatherwholeinvolve.com", "kristiesoundsimply.com", "adrianmissionminute.com", "nathanfromsubject.com", "jennifercertaindevelopment.com", "richardsignfish.com", "diananatureforeign.com", "jonathansociallike.com", "sarahnewspaperbeat.com", "johnalwayssame.com", "kellywhatcould.com", "jilliandescribecompany.com" });
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

    private static Pattern PATTERN_FOLDER = Pattern.compile("/folder/([a-zA-Z0-9]+)", Pattern.CASE_INSENSITIVE);

    public static final String getDefaultAnnotationPatternPartVoeSx() {
        return "/(?:embed-|e/)?[a-z0-9]{12}(?:/[^/]+(?:\\.html)?)?";
    }

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(" + VoeSxCrawler.getDefaultAnnotationPatternPartVoeSx() + "|" + PATTERN_FOLDER.pattern() + ")");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        return 1;
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        final XFSConfigVideoVoeSx cfg = PluginJsonConfig.get(XFSConfigVideoVoeSx.class);
        final boolean crawlSubtitle = cfg.isCrawlSubtitle();
        final Regex regex_folder = new Regex(contenturl, PATTERN_FOLDER);
        if (regex_folder.patternFind()) {
            /* Folder */
            final String folderID = regex_folder.getMatch(0);
            br.getPage(contenturl);
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final FilePackage fp = FilePackage.getInstance();
            String title = br.getRegex("<h1[^>]*>([^<]+)</h1>").getMatch(0);
            if (title != null) {
                title = Encoding.htmlDecode(title).trim();
                fp.setName(title);
            } else {
                /* Fallback */
                fp.setName(folderID);
            }
            final HashSet<String> dupes = new HashSet<String>();
            int currentpage = 1;
            int maxPage = 1;
            pagination: do {
                int newItemsThisPage = 0;
                final String[] fileIDs = br.getRegex("/([a-z0-9]{12})\"").getColumn(0);
                if (fileIDs == null || fileIDs.length == 0) {
                    if (ret.size() > 0) {
                        logger.info("Stopping because: Current page contains zero file items");
                        break pagination;
                    }
                    final boolean maybeEmptyFolder = br.containsHTML("/folder/" + folderID);
                    if (maybeEmptyFolder) {
                        throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
                    } else {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                }
                for (final String fileID : fileIDs) {
                    if (!dupes.add(fileID)) {
                        continue;
                    }
                    final String url = br.getURL("/" + fileID).toExternalForm();
                    final DownloadLink link = this.createDownloadlink(url);
                    String thisVideoTitle = br.getRegex("/" + fileID + "\"[^>]*>\\s*<h6[^>]*>([^<]+)</h6>").getMatch(0);
                    if (thisVideoTitle != null) {
                        thisVideoTitle = Encoding.htmlDecode(thisVideoTitle).trim();
                        link.setName(this.correctOrApplyFileNameExtension(thisVideoTitle, ".mp4", null));
                    } else {
                        link.setName(fileID + ".mp4");
                    }
                    if (!crawlSubtitle) {
                        /* User does not want subtitle -> Link does not need to be processed by this crawler again! */
                        link.setAvailable(true);
                    }
                    link._setFilePackage(fp);
                    ret.add(link);
                    distribute(link);
                    newItemsThisPage++;
                }
                /* Update max value on each page */
                final String[] pages = br.getRegex("/folder/" + folderID + "\\?page=(\\d+)").getColumn(0);
                if (pages != null && pages.length > 0) {
                    for (final String pageStr : pages) {
                        final int page = Integer.parseInt(pageStr);
                        if (page > maxPage) {
                            maxPage = page;
                        }
                    }
                }
                logger.info("Crawled page: " + currentpage + "/" + maxPage + " | New items this page: " + newItemsThisPage + " | Total so far: " + ret.size());
                if (this.isAbort()) {
                    logger.info("Stopping because: Aborted by user");
                    break pagination;
                } else if (currentpage == maxPage) {
                    logger.info("Stopping because: Reached end");
                    break pagination;
                } else if (newItemsThisPage == 0) {
                    logger.info("Stopping because: Failed to find any new items on current page");
                    break pagination;
                } else {
                    /* Continue to next page */
                    currentpage++;
                    br.getPage("/folder/" + folderID + "?page=" + currentpage);
                }
            } while (!this.isAbort());
        } else {
            /* File */
            if (!crawlSubtitle) {
                /* User does not want subtitle -> Let hosterplugin do the linkcheck as crawler handling is not needed. */
                ret.add(this.createDownloadlink(param.getCryptedUrl()));
                return ret;
            }
            final PluginForHost hosterPlugin = this.getNewPluginForHostInstance(this.getHost());
            final DownloadLink link = new DownloadLink(hosterPlugin, this.getHost(), param.getCryptedUrl(), true);
            try {
                hosterPlugin.setDownloadLink(link);
                final AvailableStatus status = hosterPlugin.requestFileInformation(link);
                if (((VoeSx) hosterPlugin).getJavaScriptJSON() == null) {
                    // ensure processing of obfuscated json, embed vs non embed handling in XFileSharingProBasic
                    ((VoeSx) hosterPlugin).getDllinkVideohostJavaScript(link, null, br, null);
                }
                ret.add(link);
                link.setAvailableStatus(status);
                distribute(link);
            } catch (final Exception e) {
                // prefer fresh instance
                ret.add(this.createDownloadlink(param.getCryptedUrl()));
                logger.log(e);
                return ret;
            }
            final String videoFilename = link.getName();
            final String packagename;
            if (videoFilename.contains(".")) {
                packagename = videoFilename.substring(0, videoFilename.lastIndexOf("."));
            } else {
                packagename = videoFilename;
            }
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(packagename);
            String javaScriptJSON = null;
            String[] subtitleHTMLs = br.getRegex("<track kind=\"(captions|subtitles)\"[^<]+/>").getColumn(-1);
            if (subtitleHTMLs == null || subtitleHTMLs.length == 0) {
                /* 2023-09-25 */
                subtitleHTMLs = br.getRegex("<track[^<]+kind=\"(captions|subtitles)\"[^<]*/>").getColumn(-1);
            }
            if (subtitleHTMLs != null && subtitleHTMLs.length > 0) {
                for (final String subtitleHTML : subtitleHTMLs) {
                    final String subtitleURL = new Regex(subtitleHTML, "src=\"([^\"]+\\.vtt)\"").getMatch(0);
                    final URL subtitleURLFull = br.getURL(subtitleURL);
                    final DownloadLink subtitle = createDownloadlink(subtitleURLFull.toString());
                    if (subtitleHTMLs.length == 1) {
                        /* There is only one subtitle --> Set same title as video-file. */
                        subtitle.setFinalFileName(packagename + ".vtt");
                    } else {
                        /* There are multiple subtitles available -> Set different filename for each */
                        subtitle.setFinalFileName(packagename + "_" + Plugin.getFileNameFromURL(subtitleURLFull));
                    }
                    subtitle.setAvailable(true);
                    ret.add(subtitle);
                }
                logger.info("Found numberof subtitles: " + subtitleHTMLs.length);
            } else if ((javaScriptJSON = ((VoeSx) hosterPlugin).getJavaScriptJSON()) != null) {
                final Map<String, Object> jsMap = restoreFromString(javaScriptJSON, TypeRef.MAP);
                final List<Map<String, Object>> captions = (List<Map<String, Object>>) jsMap.get("captions");
                if (captions != null) {
                    for (Map<String, Object> caption : captions) {
                        final String file = caption.get("file").toString();
                        final URL subtitleURLFull = br.getURL(file);
                        final DownloadLink subtitle = createDownloadlink(subtitleURLFull.toString());
                        if (subtitleHTMLs.length == 1) {
                            /* There is only one subtitle --> Set same title as video-file. */
                            subtitle.setFinalFileName(packagename + ".vtt");
                        } else {
                            /* There are multiple subtitles available -> Set different filename for each */
                            subtitle.setFinalFileName(packagename + "_" + Plugin.getFileNameFromURL(subtitleURLFull));
                        }
                        subtitle.setAvailable(true);
                        ret.add(subtitle);
                    }
                    logger.info("Found numberof subtitles: " + captions.size());
                }
            } else {
                logger.info("This item does not have any subtitles");
            }
            if (ret.size() > 0) {
                fp.addLinks(ret);
            }
        }
        return ret;
    }
}
