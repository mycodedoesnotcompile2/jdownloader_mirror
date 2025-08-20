//    jDownloader - Downloadmanager
//    Copyright (C) 2009  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.hoster;

import java.io.IOException;
import java.security.InvalidKeyException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import org.jdownloader.downloader.hls.HLSDownloader;
import org.jdownloader.plugins.components.hls.HlsContainer;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.requests.PostRequest;
import jd.nutils.encoding.Base64;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.PluginJSonUtils;

@HostPlugin(revision = "$Revision: 51344 $", interfaceVersion = 3, names = {}, urls = {})
public class Tube8Com extends PluginForHost {
    /* DEV NOTES */
    /* Porn_plugin */
    private String dllink = null;

    public Tube8Com(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + getHost() + "/register/");
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/info.html#terms";
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX };
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "tube8.com", "tube8.es", "tube8.fr" });
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
            ret.add("https?://(?:(?:www|[a-z]{2})\\.)?" + buildHostsPatternPart(domains) + "/.+/([0-9]+)/?");
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
        return new Regex(link.getPluginPatternMatcher(), "(\\d+)/?$").getMatch(0);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return requestFileInformation(link, null, false);
    }

    private AvailableStatus requestFileInformation(final DownloadLink link, final Account account, final boolean isDownload) throws Exception {
        dllink = null;
        this.br.setAllowedResponseCodes(new int[] { 500 });
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        final String fid = this.getFID(link);
        if (fid == null) {
            /* This should never happen */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (!link.isNameSet()) {
            link.setName(fid + ".mp4");
        }
        if (account != null) {
            this.login(account, br);
        }
        /* 2020-03-18: Do this to avoid redirectloop */
        br.getPage("https://www." + this.getHost() + "/porn-video/" + fid + "/");
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (!br.getURL().contains(fid)) {
            /* E.g. redirect to mainpage */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getRequest().getHtmlCode().length() <= 100) {
            /* Empty page */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String verifyAge = br.getRegex("(<div class=\"enter-btn\">)").getMatch(0);
        if (verifyAge != null) {
            br.postPage(link.getDownloadURL(), "processdisclaimer=");
        }
        if (br.containsHTML("class=\"video-removed-div\"")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (this.br.getHttpConnection().getResponseCode() == 500) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 500");
        } else if (br.containsHTML("class=\"geo-blocked-container\"")) {
            throw new PluginException(LinkStatus.ERROR_FATAL, "GEO-blocked");
        }
        String title = br.getRegex("\"video_title\":\"([^\"]+)").getMatch(0);
        if (title != null) {
            /* Unescape title obtained from json source */
            title = PluginJSonUtils.unescape(title);
        } else {
            title = br.getRegex("<span class=\"item\">(.*?)</span>").getMatch(0);
            if (title == null) {
                title = br.getRegex("<title>([^<]+)<").getMatch(0);
            }
        }
        if (title != null) {
            title = Encoding.htmlDecode(title).trim();
            link.setFinalFileName((title + ".mp4"));
        }
        if (isDownload) {
            final boolean useEmbedWorkaround = false;
            if (useEmbedWorkaround) {
                br.getPage("/embed/" + fid + "/");
            }
            final String jssource = br.getRegex("\"?mediaDefinition\"?\\s*:\\s*(\\[[^\\]]+\\])").getMatch(0);
            final List<Map<String, Object>> qualities = (List<Map<String, Object>>) JavaScriptEngineFactory.jsonToJavaObject(jssource);
            if (qualities.isEmpty()) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final Map<String, Object> bestqualitymap = getBestQualityMap(qualities);
            br.getPage(bestqualitymap.get("videoUrl").toString());
            final List<Map<String, Object>> qualities2 = (List<Map<String, Object>>) JavaScriptEngineFactory.jsonToJavaObject(br.getRequest().getHtmlCode());
            if (qualities2.isEmpty()) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final Map<String, Object> bestqualitymap2 = getBestQualityMap(qualities2);
            final String hlsMaster = bestqualitymap2.get("videoUrl").toString();
            br.getPage(hlsMaster);
            final HlsContainer best = HlsContainer.findBestVideoByBandwidth(HlsContainer.getHlsQualities(br));
            this.dllink = best.getDownloadurl();
        }
        return AvailableStatus.TRUE;
    }

    private Map<String, Object> getBestQualityMap(final List<Map<String, Object>> qualities) {
        int heightMax = 0;
        Map<String, Object> result = null;
        for (final Map<String, Object> quality : qualities) {
            Number heightO = (Number) quality.get("height");
            final Object qualityO = quality.get("quality");
            if (heightO == null && qualityO != null && qualityO instanceof String && qualityO.toString().matches("\\d+")) {
                heightO = Integer.parseInt(qualityO.toString());
            }
            if (result == null) {
                result = quality;
            } else if (heightO != null && heightO.intValue() > heightMax) {
                heightMax = heightO.intValue();
                result = quality;
            }
        }
        logger.info("Chose quality: " + heightMax + "p");
        return result;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        handleDownload(link, null);
    }

    private void handleDownload(final DownloadLink link, final Account account) throws Exception {
        requestFileInformation(link, account, true);
        if (dllink == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        this.checkFFmpeg(link, "Download a HLS Stream");
        this.dl = new HLSDownloader(link, br, this.dllink);
        dl.startDownload();
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        handleDownload(link, account);
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        this.login(account, this.br);
        /* only support for free accounts at the moment */
        ai.setUnlimitedTraffic();
        account.setType(AccountType.FREE);
        return ai;
    }

    private void login(final Account account, final Browser br) throws IOException, PluginException {
        this.setBrowserExclusive();
        boolean follow = br.isFollowingRedirects();
        try {
            br.setFollowRedirects(true);
            br.getPage("https://www." + this.getHost());
            final PostRequest postRequest = new PostRequest("https://www.tube8.com/ajax2/login/");
            postRequest.addVariable("username", Encoding.urlEncode(account.getUser()));
            postRequest.addVariable("password", Encoding.urlEncode(account.getPass()));
            postRequest.getHeaders().put("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");
            postRequest.getHeaders().put("X-Requested-With", "XMLHttpRequest");
            postRequest.addVariable("rememberme", "NO");
            br.getPage(postRequest);
            if (br.containsHTML("invalid") || br.containsHTML("0\\|")) { // || br.getCookie(getHost(), "ubl") == null) {
                throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_DISABLE);
            }
        } finally {
            br.setFollowRedirects(follow);
        }
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }

    @Override
    public void resetPluginGlobals() {
    }
    // private void findStreamingLink() throws Exception {
    // String flashVars = br.getRegex("var flashvars\\s*=\\s*(\\{.*?\\});").getMatch(0);
    // if (flashVars == null) {
    // return;
    // }
    // final Map<String, Object> entries = restoreFromString(flashVars, TypeRef.MAP);
    // final String[] quals = new String[] { "quality_2160p", "quality_1440p", "quality_720p", "quality_480p", "quality_240p",
    // "quality_180p" };
    // for (final String qual : quals) {
    // final Object qualO = entries.get(qual);
    // if (qualO instanceof String) {
    // this.dllink = qualO.toString();
    // }
    // }
    // final boolean isEncrypted = ((Boolean) entries.get("encrypted")).booleanValue();
    // if (isEncrypted) {
    // final String decrypted = (String) entries.get("video_url");
    // String key = (String) entries.get("video_title");
    // /* Dirty workaround, needed for links with cyrillic titles/filenames. */
    // if (key == null) {
    // key = "";
    // }
    // try {
    // dllink = new BouncyCastleAESCounterModeDecrypt().decrypt(decrypted, key, 256);
    // } catch (Throwable e) {
    // /* Fallback for stable version */
    // dllink = AESCounterModeDecrypt(decrypted, key, 256);
    // }
    // if (dllink != null && (dllink.startsWith("Error:") || !dllink.startsWith("http"))) {
    // throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, dllink);
    // }
    // }
    // }

    /**
     * AES CTR(Counter) Mode for Java ported from AES-CTR-Mode implementation in JavaScript by Chris Veness
     *
     * @see <a href="http://csrc.nist.gov/publications/nistpubs/800-38a/sp800-38a.pdf">
     *      "Recommendation for Block Cipher Modes of Operation - Methods and Techniques"</a>
     */
    private String AESCounterModeDecrypt(String cipherText, String key, int nBits) throws Exception {
        if (!(nBits == 128 || nBits == 192 || nBits == 256)) {
            return "Error: Must be a key mode of either 128, 192, 256 bits";
        }
        if (cipherText == null || key == null) {
            return "Error: cipher and/or key equals null";
        }
        String res = null;
        nBits = nBits / 8;
        byte[] data = Base64.decode(cipherText.toCharArray());
        /* CHECK: we should always use getBytes("UTF-8") or with wanted charset, never system charset! */
        byte[] k = Arrays.copyOf(key.getBytes(), nBits);
        Cipher cipher = Cipher.getInstance("AES/CTR/NoPadding");
        SecretKey secretKey = generateSecretKey(k, nBits);
        byte[] nonceBytes = Arrays.copyOf(Arrays.copyOf(data, 8), nBits / 2);
        IvParameterSpec nonce = new IvParameterSpec(nonceBytes);
        cipher.init(Cipher.ENCRYPT_MODE, secretKey, nonce);
        /* CHECK: we should always use new String (bytes,charset) to avoid issues with system charset and utf-8 */
        res = new String(cipher.doFinal(data, 8, data.length - 8));
        return res;
    }

    private SecretKey generateSecretKey(byte[] keyBytes, int nBits) throws Exception {
        try {
            SecretKey secretKey = new SecretKeySpec(keyBytes, "AES");
            Cipher cipher = Cipher.getInstance("AES/ECB/NoPadding");
            cipher.init(Cipher.ENCRYPT_MODE, secretKey);
            keyBytes = cipher.doFinal(keyBytes);
        } catch (InvalidKeyException e) {
            throw new PluginException(LinkStatus.ERROR_FATAL, "Unlimited Strength JCE Policy Files needed!", e);
        } catch (Throwable e1) {
            return null;
        }
        System.arraycopy(keyBytes, 0, keyBytes, nBits / 2, nBits / 2);
        return new SecretKeySpec(keyBytes, "AES");
    }

    private class BouncyCastleAESCounterModeDecrypt {
        private String decrypt(String cipherText, String key, int nBits) throws Exception {
            if (!(nBits == 128 || nBits == 192 || nBits == 256)) {
                return "Error: Must be a key mode of either 128, 192, 256 bits";
            }
            if (cipherText == null || key == null) {
                return "Error: cipher and/or key equals null";
            }
            byte[] decrypted;
            nBits = nBits / 8;
            byte[] data = Base64.decode(cipherText.toCharArray());
            /* CHECK: we should always use getBytes("UTF-8") or with wanted charset, never system charset! */
            byte[] k = Arrays.copyOf(key.getBytes(), nBits);
            /* AES/CTR/NoPadding (SIC == CTR) */
            org.bouncycastle.crypto.BufferedBlockCipher cipher = new org.bouncycastle.crypto.BufferedBlockCipher(new org.bouncycastle.crypto.modes.SICBlockCipher(new org.bouncycastle.crypto.engines.AESEngine()));
            cipher.reset();
            SecretKey secretKey = generateSecretKey(k, nBits);
            byte[] nonceBytes = Arrays.copyOf(Arrays.copyOf(data, 8), nBits / 2);
            IvParameterSpec nonce = new IvParameterSpec(nonceBytes);
            /* true == encrypt; false == decrypt */
            cipher.init(true, new org.bouncycastle.crypto.params.ParametersWithIV(new org.bouncycastle.crypto.params.KeyParameter(secretKey.getEncoded()), nonce.getIV()));
            decrypted = new byte[cipher.getOutputSize(data.length - 8)];
            int decLength = cipher.processBytes(data, 8, data.length - 8, decrypted, 0);
            cipher.doFinal(decrypted, decLength);
            /* CHECK: we should always use new String (bytes,charset) to avoid issues with system charset and utf-8 */
            return new String(decrypted);
        }

        private SecretKey generateSecretKey(byte[] keyBytes, int nBits) throws Exception {
            try {
                SecretKey secretKey = new SecretKeySpec(keyBytes, "AES");
                /* AES/ECB/NoPadding */
                org.bouncycastle.crypto.BufferedBlockCipher cipher = new org.bouncycastle.crypto.BufferedBlockCipher(new org.bouncycastle.crypto.engines.AESEngine());
                cipher.init(true, new org.bouncycastle.crypto.params.KeyParameter(secretKey.getEncoded()));
                keyBytes = new byte[cipher.getOutputSize(secretKey.getEncoded().length)];
                int decLength = cipher.processBytes(secretKey.getEncoded(), 0, secretKey.getEncoded().length, keyBytes, 0);
                cipher.doFinal(keyBytes, decLength);
            } catch (Throwable e) {
                return null;
            }
            System.arraycopy(keyBytes, 0, keyBytes, nBits / 2, nBits / 2);
            return new SecretKeySpec(keyBytes, "AES");
        }
    }
}