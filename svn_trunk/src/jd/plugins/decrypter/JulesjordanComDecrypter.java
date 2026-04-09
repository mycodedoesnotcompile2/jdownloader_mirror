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
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.JulesjordanCom;
import jd.plugins.hoster.JulesjordanCom.JulesjordanComConfigInterface;

@DecrypterPlugin(revision = "$Revision: 52633 $", interfaceVersion = 3, names = {}, urls = {})
public class JulesjordanComDecrypter extends PluginForDecrypt {
    public JulesjordanComDecrypter(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX };
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "julesjordan.com" });
        ret.add(new String[] { "manuelferrara.com" });
        ret.add(new String[] { "theassfactory.com" });
        ret.add(new String[] { "spermswallowers.com" });
        ret.add(new String[] { "girlgirl.com" });
        ret.add(new String[] { "auntjudysxxx.com" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(?:trial|members)/(?:movies|scenes)/[^/]+\\.html");
        }
        return ret.toArray(new String[0]);
    }

    /* Important: Keep this updated & keep this in order: Highest --> Lowest */
    private final List<String> all_known_qualities = Arrays.asList("4K", "1080P", "720P", "Mobile");

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final JulesjordanCom plg = (JulesjordanCom) this.getNewPluginForHostInstance(this.getHost());
        final Account account = AccountController.getInstance().getValidAccount(this.getHost());
        final String contenturl = param.getCryptedUrl();
        if (account == null) {
            /* Only MOCH download and/or trailer download --> Add link for hostplugin -> User might be able to download trailer */
            final DownloadLink dl = this.createDownloadlink(contenturl);
            ret.add(dl);
            return ret;
        }
        List<String> all_selected_qualities = new ArrayList<String>();
        final JulesjordanComConfigInterface cfg = PluginJsonConfig.get(JulesjordanCom.JulesjordanComConfigInterface.class);
        final String url_name = JulesjordanCom.getURLName(contenturl);
        final boolean grabBest = cfg.isGrabBESTEnabled();
        final boolean grabBestWithinUserSelection = cfg.isOnlyBestVideoQualityOfSelectedQualitiesEnabled();
        final boolean grabUnknownQualities = cfg.isAddUnknownQualitiesEnabled();
        final boolean grab4k = cfg.isGrabHTTPMp4_4kEnabled();
        final boolean grab1080p = cfg.isGrabHTTPMp4_1080pEnabled();
        final boolean grab720p = cfg.isGrabHTTPMp4_720pHDEnabled();
        final boolean grabMobileSD = cfg.isGrabHTTPMp4_MobileSDEnabled();
        if (grab4k) {
            all_selected_qualities.add("4K");
        }
        if (grab1080p) {
            all_selected_qualities.add("1080P");
        }
        if (grab720p) {
            all_selected_qualities.add("720P");
        }
        if (grabMobileSD) {
            all_selected_qualities.add("Mobile");
        }
        if (all_selected_qualities.isEmpty()) {
            logger.info("User has deselected all qualities -> Force-selecting all qualities");
            all_selected_qualities = all_known_qualities;
        }
        synchronized (account) {
            plg.login(account, false);
            br.getPage(JulesjordanCom.getURLPremium(contenturl));
            /**
             * 2022-02-21: This may happen randomly- or only for specific links. </br>
             * Once confirmed in JD (or browser!) we should be able to proceed. The website seems to do this by-IP which is why we do not
             * have to update the account cookies to prevent further activation-code-prompts.
             */
            if (isNewDeviceProtectionActive(br)) {
                String activationCode = null;
                do {
                    activationCode = getUserInput("Enter activation code sent by mail", param).trim();
                    if (this.isAbort()) {
                        throw new InterruptedException();
                    } else if (!activationCode.matches("[a-z0-9]{4,}")) {
                        logger.info("User entered invalid activation code format");
                        continue;
                    } else {
                        break;
                    }
                } while (true);
                br.getPage(br.getURL() + "?auth_code=" + Encoding.urlEncode(activationCode));
                if (isNewDeviceProtectionActive(br)) {
                    final String infoText = this.getHost() + " claims 'New Device or Location Detected!'.\r\nAn activation code was sent to your via E-Mail.\r\nYou seem to have entered the wrong activation code for this attempt!\r\nTry again later.";
                    throw new DecrypterRetryException(RetryReason.NO_ACCOUNT, "NEW_DEVICE_OR_LOCATION_DETECTION_NOT_PASSED_" + br._getURL().getPath(), infoText, null);
                }
                // refresh stored cookies in hope that we won't need another code next time.
                account.saveCookies(br.getCookies(br.getHost()), "");
            } else {
                // login successful -> refresh stored cookies
                account.saveCookies(br.getCookies(br.getHost()), "");
            }
        }
        if (isOffline(br)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String title = JulesjordanCom.getTitle(br);
        if (StringUtils.isEmpty(title)) {
            /* Fallback */
            title = url_name;
        }
        title = Encoding.htmlDecode(title).trim();
        final Map<String, DownloadLink> all_found_downloadlinks = new HashMap<String, DownloadLink>();
        final Map<String, String[]> allQualities = findAllQualities(br);
        if (allQualities == null || allQualities.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final Iterator<Entry<String, String[]>> it = allQualities.entrySet().iterator();
        while (it.hasNext()) {
            final Entry<String, String[]> entry = it.next();
            final String quality_url = entry.getKey();
            final String[] dlinfo = entry.getValue();
            final String dlurl = dlinfo[0];
            // final DownloadLink dl = this.createDownloadlink(dlurl);
            final DownloadLink video = new DownloadLink(plg, null, this.getHost(), dlurl, true);
            final String crawler_filename = title + "_" + quality_url + ".mp4";
            video.setName(crawler_filename);
            video.setProperty("fid", url_name);
            video.setProperty("quality", quality_url);
            video.setProperty("decrypter_filename", crawler_filename);
            video.setProperty("mainlink", contenturl);
            video.setLinkID(this.getHost() + "://" + url_name + "/" + quality_url);
            video.setAvailable(true);
            if (dlinfo.length == 2) {
                final String filesizeStr = dlinfo[1];
                video.setDownloadSize(SizeFormatter.getSize(filesizeStr));
            }
            all_found_downloadlinks.put(quality_url, video);
        }
        final Map<String, DownloadLink> all_selected_downloadlinks = handleQualitySelection(all_found_downloadlinks, all_selected_qualities, grabBest, grabBestWithinUserSelection, grabUnknownQualities);
        /* Finally add selected URLs */
        final Iterator<Entry<String, DownloadLink>> it_2 = all_selected_downloadlinks.entrySet().iterator();
        while (it_2.hasNext()) {
            final Entry<String, DownloadLink> entry = it_2.next();
            final DownloadLink keep = entry.getValue();
            ret.add(keep);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(Encoding.htmlDecode(title).trim());
        fp.addLinks(ret);
        return ret;
    }

    public static boolean isNewDeviceProtectionActive(final Browser br) {
        if (br.containsHTML(">\\s*New Device or Location Detected")) {
            return true;
        } else {
            return false;
        }
    }

    public static Map<String, String[]> findAllQualities(final Browser br) throws Exception {
        final Map<String, String[]> allQualities = new HashMap<String, String[]>();
        final String[] filesizes = br.getRegex("class=\"meta\">MP4 • (\\d+[^<]+)</div>").getColumn(0);
        final String[][] urls = br.getRegex("data-video-path=\"(https?[^\"]+)\" class=\"hover-menu-item[^\"]*\"[^>]*>([^<]+)").getMatches();
        int index = 0;
        for (final String[] urlinfo : urls) {
            final String url = urlinfo[0];
            final String quality = urlinfo[1];
            if (filesizes != null && filesizes.length == urls.length) {
                /* downloadurl + filesize */
                final String[] dlinfo = new String[2];
                dlinfo[0] = url;
                dlinfo[1] = filesizes[index];
                allQualities.put(quality, dlinfo);
            } else {
                /* downloadurl only */
                final String[] dlinfo = new String[1];
                dlinfo[0] = url;
                allQualities.put(quality, dlinfo);
            }
            index++;
        }
        return allQualities;
    }

    private Map<String, DownloadLink> handleQualitySelection(final Map<String, DownloadLink> all_found_downloadlinks, final List<String> all_selected_qualities, final boolean grab_best, final boolean grab_best_out_of_user_selection, final boolean grab_unknown) {
        Map<String, DownloadLink> all_selected_downloadlinks = new HashMap<String, DownloadLink>();
        if (all_found_downloadlinks.isEmpty()) {
            logger.info("Failed to find any qualities");
            return all_selected_downloadlinks;
        }
        final Iterator<Entry<String, DownloadLink>> iterator_all_found_downloadlinks = all_found_downloadlinks.entrySet().iterator();
        if (grab_best) {
            for (final String possibleQuality : this.all_known_qualities) {
                if (all_found_downloadlinks.containsKey(possibleQuality)) {
                    all_selected_downloadlinks.put(possibleQuality, all_found_downloadlinks.get(possibleQuality));
                    break;
                }
            }
            if (all_selected_downloadlinks.isEmpty()) {
                logger.info("Possible issue: Best selection found nothing --> Adding ALL");
                while (iterator_all_found_downloadlinks.hasNext()) {
                    final Entry<String, DownloadLink> dl_entry = iterator_all_found_downloadlinks.next();
                    all_selected_downloadlinks.put(dl_entry.getKey(), dl_entry.getValue());
                }
            }
        } else {
            boolean atLeastOneSelectedItemExists = false;
            while (iterator_all_found_downloadlinks.hasNext()) {
                final Entry<String, DownloadLink> dl_entry = iterator_all_found_downloadlinks.next();
                final String dl_quality_string = dl_entry.getKey();
                if (all_selected_qualities.contains(dl_quality_string)) {
                    atLeastOneSelectedItemExists = true;
                    all_selected_downloadlinks.put(dl_quality_string, dl_entry.getValue());
                } else if (!all_known_qualities.contains(dl_quality_string) && grab_unknown) {
                    logger.info("Found unknown quality: " + dl_quality_string);
                    if (grab_unknown) {
                        logger.info("Adding unknown quality: " + dl_quality_string);
                        all_selected_downloadlinks.put(dl_quality_string, dl_entry.getValue());
                    }
                }
            }
            if (!atLeastOneSelectedItemExists) {
                logger.info("Possible user error: User selected only qualities which are not available --> Adding ALL");
                while (iterator_all_found_downloadlinks.hasNext()) {
                    final Entry<String, DownloadLink> dl_entry = iterator_all_found_downloadlinks.next();
                    all_selected_downloadlinks.put(dl_entry.getKey(), dl_entry.getValue());
                }
            } else {
                if (grab_best_out_of_user_selection) {
                    all_selected_downloadlinks = findBESTInsideGivenMap(all_selected_downloadlinks);
                }
            }
        }
        return all_selected_downloadlinks;
    }

    private Map<String, DownloadLink> findBESTInsideGivenMap(final Map<String, DownloadLink> map_with_downloadlinks) {
        Map<String, DownloadLink> newMap = new HashMap<String, DownloadLink>();
        DownloadLink keep = null;
        if (map_with_downloadlinks.size() > 0) {
            for (final String quality : all_known_qualities) {
                keep = map_with_downloadlinks.get(quality);
                if (keep != null) {
                    newMap.put(quality, keep);
                    break;
                }
            }
        }
        if (newMap.isEmpty()) {
            /* Failover in case of bad user selection or general failure! */
            newMap = map_with_downloadlinks;
        }
        return newMap;
    }

    public static boolean isOffline(final Browser br) {
        return br.getHttpConnection().getResponseCode() == 404;
    }
}
