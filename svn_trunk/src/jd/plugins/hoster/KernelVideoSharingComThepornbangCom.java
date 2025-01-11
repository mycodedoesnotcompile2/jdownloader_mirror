//jDownloader - Downloadmanager
//Copyright (C) 2013  JD-Team support@jdownloader.org
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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.crypto.Cipher;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.SecretKeySpec;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;

import org.appwork.storage.TypeRef;
import org.appwork.utils.encoding.Base64;
import org.appwork.utils.formatter.HexFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.components.config.KVSConfig;
import org.jdownloader.plugins.components.config.KVSConfigThepornbangCom;

@HostPlugin(revision = "$Revision: 50427 $", interfaceVersion = 3, names = {}, urls = {})
public class KernelVideoSharingComThepornbangCom extends KernelVideoSharingComV2 {
    public KernelVideoSharingComThepornbangCom(final PluginWrapper wrapper) {
        super(wrapper);
    }

    /** Add all KVS hosts to this list that fit the main template without the need of ANY changes to this class. */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(new String[] { "thepornbang.com" });
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
        return KernelVideoSharingComThepornbangCom.buildAnnotationThepornbang(getPluginDomains());
    }

    public static String[] buildAnnotationThepornbang(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/video/([^/\\?#]+)/?");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    protected String generateContentURL(final String host, final String fuid, final String urlTitle) {
        return generateContentURLDefaultVideoNoFUID(host, fuid);
    }

    @Override
    protected boolean hasFUIDInsideURL(final String url) {
        return false;
    }

    @Override
    protected String handleQualitySelection(Browser br, DownloadLink link, HashMap<Integer, String> qualityMap) {
        if (qualityMap != null && !qualityMap.isEmpty()) {
            try {
                final String mp4varstext = br.getRegex("generate_mp4\\('([^\\)]+)").getMatch(0);
                final String[] mp4vars = mp4varstext.replace("'", "").split(", ");
                final String crypto0 = mp4vars[0];
                final String crypto1 = mp4vars[1];
                final String session_key = decrypt(crypto0, crypto1);
                final String okDotRuPrivateVideoVideoID = mp4vars[2];
                final UrlQuery query = new UrlQuery();
                query.appendEncoded("application_key", "CBAFJIICABABABABA");
                query.appendEncoded("fields", "video.url_tiny,video.url_low,video.url_high,video.url_medium,video.url_quadhd,video.url_mobile,video.url_ultrahd,video.url_fullhd,");
                query.appendEncoded("method", "video.get");
                query.appendEncoded("session_key", session_key);
                query.appendEncoded("vids", okDotRuPrivateVideoVideoID);
                final Browser brc = br.cloneBrowser();
                brc.getPage("https://api.ok.ru/fb.do?" + query.toString());
                /* Small hack: Replace items in our map with the "okcdn.ru" links. */
                final Map<String, Integer> qualityHeightMapping = new HashMap<String, Integer>();
                // TODO: Check/add "url_ultrahd"
                if (brc.containsHTML("url_ultrahd")) {
                    logger.info("Found unknown quality url_ultrahd DEV, ADD THIS!");
                }
                qualityHeightMapping.put("url_quadhd", 2160);
                qualityHeightMapping.put("url_fullhd", 1080);
                qualityHeightMapping.put("url_high", 720);
                qualityHeightMapping.put("url_medium", 480);
                qualityHeightMapping.put("url_low", 360);
                qualityHeightMapping.put("url_tiny", 240);
                qualityHeightMapping.put("url_mobile", 144);
                final HashMap<Integer, String> qualityMapNew = new HashMap<Integer, String>();
                for (final Entry<String, Integer> entry : qualityHeightMapping.entrySet()) {
                    final String qualityName = entry.getKey();
                    String url = brc.getRegex("<" + qualityName + ">(https?://[^<]+)").getMatch(0);
                    if (url == null) {
                        logger.info("Failed to find quality: " + qualityName);
                        continue;
                    }
                    url = Encoding.htmlOnlyDecode(url);
                    qualityMapNew.put(entry.getValue(), url);
                }
                if (!qualityMapNew.isEmpty()) {
                    return super.handleQualitySelection(br, link, qualityMapNew);
                } else {
                    logger.warning("Special handling failed");
                }
            } catch (final Throwable e) {
                logger.log(e);
            }
        }
        return super.handleQualitySelection(br, link, qualityMap);
    }

    @Override
    protected boolean preferTitleHTML() {
        return true;
    }

    @Override
    protected AvailableStatus requestFileInformationWebsite(final DownloadLink link, final Account account, final boolean isDownload) throws Exception {
        final AvailableStatus status = super.requestFileInformationWebsite(link, account, isDownload);
        final String htmlTitleTag = br.getRegex("<title>([^<]+)").getMatch(0);
        if (htmlTitleTag != null) {
            link.setProperty("title", Encoding.htmlDecode(htmlTitleTag).trim());
        }
        return status;
    }

    @Override
    public Class<? extends KVSConfig> getConfigInterface() {
        return KVSConfigThepornbangCom.class;
    }

    @Override
    protected boolean enableFastLinkcheck() {
        /* 2024-09-25: To counter problems reported here: https://board.jdownloader.org/showthread.php?t=96207 */
        return true;
    }

    public String decrypt(String encryptedData, String password) throws Exception {
        // Parse Base64-encoded JSON string
        final String jsonString = Encoding.Base64Decode(encryptedData);
        final Map<String, Object> entries = restoreFromString(jsonString, TypeRef.MAP);
        final String saltHex = entries.get("salt").toString();
        final String ivHex = entries.get("iv").toString();
        final String ciphertextBase64 = entries.get("ciphertext").toString();
        final int iterations = ((Number) entries.get("iterations")).intValue();
        // Convert salt and IV from Hex to byte array
        byte[] salt = HexFormatter.hexToByteArray(saltHex);
        byte[] iv = HexFormatter.hexToByteArray(ivHex);
        // Derive key
        SecretKeySpec key = deriveKey(password, salt, iterations);
        // Decrypt ciphertext
        Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
        IvParameterSpec ivSpec = new IvParameterSpec(iv);
        cipher.init(Cipher.DECRYPT_MODE, key, ivSpec);
        byte[] decryptedBytes = cipher.doFinal(Base64.decode(ciphertextBase64));
        return new String(decryptedBytes, "UTF-8");
    }

    private static SecretKeySpec deriveKey(String password, byte[] salt, int iterations) throws Exception {
        PBEKeySpec spec = new PBEKeySpec(password.toCharArray(), salt, iterations, 256);
        SecretKeyFactory factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA512");
        byte[] keyBytes = factory.generateSecret(spec).getEncoded();
        return new SecretKeySpec(keyBytes, "AES");
    }

}