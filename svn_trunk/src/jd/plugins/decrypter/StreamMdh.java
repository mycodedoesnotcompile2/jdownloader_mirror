package jd.plugins.decrypter;

import java.security.MessageDigest;
import java.util.ArrayList;

import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import org.appwork.utils.Files;
import org.appwork.utils.Regex;
import org.appwork.utils.encoding.Base64;
import org.jdownloader.controlling.filter.CompiledFiletypeFilter;
import org.jdownloader.controlling.filter.CompiledFiletypeFilter.ExtensionsFilterInterface;
import org.jdownloader.plugins.components.antiDDoSForDecrypt;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

@DecrypterPlugin(revision = "$Revision: 52655 $", interfaceVersion = 2, names = { "stream-mdh.co" }, urls = { "https?://(?:www\\.)?stream-mdh.co/(view|video)/.*?/.+" })
public class StreamMdh extends antiDDoSForDecrypt {
    public StreamMdh(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        final String streamID = new Regex(contenturl, "/view/.*?/(.+)").getMatch(0);
        br.setFollowRedirects(true);
        if (streamID != null) {
            getPage("https://stream-mdh.co/embed/" + streamID);
        } else {
            getPage(contenturl);
        }
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (!this.canHandle(br.getURL())) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String stored = br.getRegex("embedDecrypt\\('([^']+)").getMatch(0);
        final String evitem = br.getRegex("var _evitem = '([^']+)';").getMatch(0);
        if (stored != null && evitem != null) {
            /* 2026-04-14 */
            final String[] eparts = evitem.split("\\.");
            if (eparts.length == 2) {
                final String decrypted_url = embedDecrypt(stored, eparts[0], eparts[1]);
                ret.add(this.createDownloadlink(decrypted_url));
                return ret;
            }
        }
        logger.warning("Failed to find crapto stuff -> Fallback to old method");
        final String iFrameSrc = br.getRegex("iframe src\\s*=\\s*\"(https?://.+?)\"").getMatch(0);
        if (iFrameSrc == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String title = br.getRegex("<title>\\s*(.*?)\\s*</title>").getMatch(0);
        final DownloadLink link = createDownloadlink(iFrameSrc);
        if (title != null) {
            final ExtensionsFilterInterface extension = CompiledFiletypeFilter.getExtensionsFilterInterface(Files.getExtension(link.getName()));
            if (CompiledFiletypeFilter.DocumentExtensions.HTML.equals(extension) || extension == null || CompiledFiletypeFilter.VideoExtensions.MP4.isSameExtensionGroup(extension)) {
                link.setName(title);
            }
        }
        ret.add(link);
        return ret;
    }

    /** Java aequivalent of js function in html code */
    private String embedDecrypt(final String stored, final String secretKey, final String secretIv) throws Exception {
        final MessageDigest sha256 = MessageDigest.getInstance("SHA-256");
        final String keyStr = toHex(sha256.digest(secretKey.getBytes("UTF-8"))).substring(0, 32);
        sha256.reset();
        final String ivStr = toHex(sha256.digest(secretIv.getBytes("UTF-8"))).substring(0, 16);
        final byte[] innerBytes = Base64.decode(stored);
        final String innerB64 = new String(innerBytes, "ISO-8859-1");
        final byte[] cipherBytes = Base64.decode(innerB64);
        final Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
        cipher.init(Cipher.DECRYPT_MODE, new SecretKeySpec(keyStr.getBytes("UTF-8"), "AES"), new IvParameterSpec(ivStr.getBytes("UTF-8")));
        return new String(cipher.doFinal(cipherBytes), "UTF-8");
    }

    private static String toHex(byte[] bytes) {
        StringBuilder sb = new StringBuilder();
        for (byte b : bytes) {
            sb.append(String.format("%02x", b & 0xFF));
        }
        return sb.toString();
    }
}
