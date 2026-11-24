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
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.AccountRequiredException;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.PluginForHost;
import jd.plugins.hoster.LiveMixTapesCom;
import jd.utils.JDUtilities;

@DecrypterPlugin(revision = "$Revision: 54120 $", interfaceVersion = 2, names = {}, urls = {})
@PluginDependencies(dependencies = { LiveMixTapesCom.class })
public class LiveMixtapesComDecrypter extends PluginForDecrypt {
    public LiveMixtapesComDecrypter(PluginWrapper wrapper) {
        super(wrapper);
    }

    /** Estimated audio bitrates in kbit/s, used to approximate track file sizes from their duration. */
    private static final int    BITRATE_FREE_KBITS    = 96;
    private static final int    BITRATE_ACCOUNT_KBITS = 256;
    private static final String REDIRECTLINK          = "https?://(?:www\\.)?livemixtap\\.es/[a-z0-9]+";

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(new String[] { "livemixtapes.com" });
        ret.add(new String[] { "livemixtap.es" });
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

    private static final Pattern PATTERN_MIXTAPE = Pattern.compile("/mixtape/([a-z0-9\\-]+)");

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            if (domains[0].equals("livemixtap.es")) {
                /* Short redirect-URLs */
                ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/[a-z0-9]+");
            } else {
                ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + PATTERN_MIXTAPE.pattern());
            }
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        /* 2020-04-22: Preventive measure to try to avoid captchas */
        return 1;
    }

    @SuppressWarnings("unchecked")
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        /** If link is a short link, correct it */
        if (contenturl.matches(REDIRECTLINK)) {
            br.setFollowRedirects(false);
            br.getPage(contenturl);
            String redirect = br.getRedirectLocation();
            if (redirect == null) {
                logger.warning("Decrypter broken for link: " + contenturl);
                return null;
            }
            br.getPage(redirect);
            redirect = br.getRedirectLocation();
            if (redirect == null) {
                logger.warning("Decrypter broken for link: " + contenturl);
                return null;
            }
            ret.add(this.createDownloadlink(redirect));
            /* Redirect will most likely go back into this crawler. */
            return ret;
        }
        final String slug = new Regex(contenturl, PATTERN_MIXTAPE).getMatch(0);
        if (slug == null) {
            /* This should never happen */
            logger.warning("Failed to find slug");
            return null;
        }
        final boolean accountAvailable = getUserLogin();
        /* File size estimation and file extension depend on account availability. */
        final int bitrate = accountAvailable ? BITRATE_ACCOUNT_KBITS : BITRATE_FREE_KBITS;
        final String extension = accountAvailable ? ".mp3" : ".mp4";
        br.setFollowRedirects(true);
        br.getPage(contenturl);
        if (br.getURL().contains("error/login.html")) {
            throw new AccountRequiredException();
        }
        final String buildID = br.getRegex("\"buildId\"\\s*:\\s*\"([^\"]+)\"").getMatch(0);
        if (buildID == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        br.getPage("https://" + this.getHost() + "/_next/data/" + buildID + "/mixtape/" + slug + ".json?slug=" + Encoding.urlEncode(slug));
        final String json = br.getRequest().getHtmlCode();
        /* Robustly grab the mixtape-ID so the .zip download can be added even if the detailed json handling fails below. */
        final String mixtapeID = new Regex(json, "\"id\"\\s*:\\s*(\\d+)").getMatch(0);
        if (mixtapeID == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(slug.replace("-", " ").trim());
        /* Add .zip with complete album download (host plugin). This is always added, regardless of the json handling below. */
        final DownloadLink zip = this.createDownloadlink("https://www." + this.getHost() + "/download/" + mixtapeID + "/" + slug + ".html");
        zip.setName(slug.replace("-", " ").trim() + ".zip");
        zip.setAvailable(true);
        zip._setFilePackage(fp);
        ret.add(zip);
        /* The detailed json handling is wrapped in try-catch so that a broken/changed structure never prevents adding the .zip above. */
        try {
            final Map<String, Object> root = restoreFromString(json, TypeRef.MAP);
            final Map<String, Object> data = (Map<String, Object>) JavaScriptEngineFactory.walkJson(root, "pageProps/initialMixtape/data");
            final String artist = (String) data.get("artist");
            final String title = (String) data.get("title");
            if (!StringUtils.isEmpty(artist) && !StringUtils.isEmpty(title)) {
                final String mixtapeName = artist + " - " + title;
                fp.setName(mixtapeName);
                zip.setName(mixtapeName + ".zip");
            }
            final String description = (String) data.get("description");
            if (!StringUtils.isEmpty(description)) {
                fp.setComment(description);
            }
            /* Only the "tracks" are processed here - "videos" elements are intentionally ignored. */
            final List<Map<String, Object>> tracks = (List<Map<String, Object>>) JavaScriptEngineFactory.walkJson(root, "pageProps/initialMixtape/included/tracks");
            long totalSize = 0;
            int position = 1;
            for (final Map<String, Object> track : tracks) {
                final Number trackID = (Number) track.get("id");
                final String trackArtist = (String) track.get("artist");
                final String trackTitle = (String) track.get("title");
                final Number duration = (Number) track.get("duration");
                /* Approximate file size in bytes based on the given duration and the estimated bitrate. */
                final long filesize;
                if (duration != null) {
                    filesize = duration.longValue() * bitrate * 1000 / 8;
                    /* The .zip always contains the original mp3s, so always account for the account/original bitrate there. */
                    totalSize += duration.longValue() * BITRATE_ACCOUNT_KBITS * 1000 / 8;
                } else {
                    filesize = 0;
                }
                final DownloadLink link = this.createDownloadlink("https://club.livemixtapes.com/play/" + trackID.longValue());
                final StringBuilder filename = new StringBuilder();
                filename.append(String.format("%02d. ", position));
                if (!StringUtils.isEmpty(trackArtist)) {
                    filename.append(trackArtist).append(" - ");
                }
                if (!StringUtils.isEmpty(trackTitle)) {
                    filename.append(trackTitle);
                } else {
                    filename.append(trackID.longValue());
                }
                filename.append(extension);
                link.setFinalFileName(filename.toString());
                if (filesize > 0) {
                    link.setDownloadSize(filesize);
                }
                link.setAvailable(true);
                link._setFilePackage(fp);
                ret.add(link);
                position++;
            }
            /* Set the sum of all track file sizes on the .zip download. */
            if (totalSize > 0) {
                zip.setDownloadSize(totalSize);
            }
        } catch (final Exception e) {
            logger.log(e);
            logger.warning("Single track handling failed");
        }
        return ret;
    }

    /**
     * JD2 CODE: DO NOIT USE OVERRIDE FÒR COMPATIBILITY REASONS!!!!!
     */
    public boolean isProxyRotationEnabledForLinkCrawler() {
        return false;
    }

    private boolean getUserLogin() throws Exception {
        final PluginForHost hostPlugin = JDUtilities.getPluginForHost("livemixtapes.com");
        final Account aa = AccountController.getInstance().getValidAccount(hostPlugin);
        if (aa == null) {
            logger.info("There is no account available, stopping...");
            return false;
        }
        hostPlugin.setBrowser(this.br);
        try {
            ((LiveMixTapesCom) hostPlugin).login(aa, false);
        } catch (final PluginException e) {
            aa.setValid(false);
            return false;
        }
        return true;
    }
}
