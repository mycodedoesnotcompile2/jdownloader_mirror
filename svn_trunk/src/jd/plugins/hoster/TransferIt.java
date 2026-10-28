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
package jd.plugins.hoster;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

import javax.crypto.Cipher;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.SecretKeySpec;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.decrypter.TransferItCrawler;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Time;

/**
 * Most of the work on this plugin was done by Fabio Orlandi.
 */
@HostPlugin(revision = "$Revision: 53773 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { TransferItCrawler.class })
public class TransferIt extends PluginForHost {
    public TransferIt(PluginWrapper wrapper) {
        super(wrapper);
    }

    /* transfer.it is a service powered by MEGA and uses the MEGA API with "domain=transferit". */
    private static final String API_BASE               = "https://bt7.api.mega.co.nz/cs";
    public static final String  PROPERTY_TRANSFER_ID   = "transfer_id";
    public static final String  PROPERTY_NODE_ID       = "node_id";
    public static final String  PROPERTY_NODE_KEY      = "node_key";
    /* Hash of the password of a password protected transfer. Sent as "pw" parameter. */
    public static final String  PROPERTY_PASSWORD_HASH = "password_hash";
    private final AtomicLong    API_SEQUENCE           = new AtomicLong(Time.systemIndependentCurrentJVMTimeMillis() % 1000000000l);
    private String              dllink                 = null;

    @Override
    public String getAGBLink() {
        return "https://" + getHost();
    }

    private static List<String[]> getPluginDomains() {
        return TransferItCrawler.getPluginDomains();
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
            /* No URL-pattern since all items get added via crawler plugin. */
            ret.add("");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String transferID = link.getStringProperty(PROPERTY_TRANSFER_ID);
        final String nodeID = link.getStringProperty(PROPERTY_NODE_ID);
        if (transferID != null && nodeID != null) {
            /* Fixed identifier instead of getHost() so the ID stays stable even if the domain changes. */
            return "transferit://transfer/" + transferID + "/node/" + nodeID;
        } else {
            return super.getLinkID(link);
        }
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final Account account) {
        /* MEGA does not like much connections! */
        return -5;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        final String transferID = link.getStringProperty(PROPERTY_TRANSFER_ID);
        final String nodeID = link.getStringProperty(PROPERTY_NODE_ID);
        if (transferID == null || nodeID == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        /* "pt":1 = server delivers the file already decrypted. */
        final String passwordHash = link.getStringProperty(PROPERTY_PASSWORD_HASH);
        final Map<String, Object> entries = callAPI(br, transferID, passwordHash, "{\"a\":\"g\",\"n\":\"" + nodeID + "\",\"pt\":1,\"g\":1,\"ssl\":1}");
        dllink = (String) entries.get("g");
        /* The node attributes ("at") contain the filename and are encrypted with the node key which was stored by the crawler. */
        final String nodeKey = link.getStringProperty(PROPERTY_NODE_KEY);
        if (nodeKey != null) {
            final Map<String, Object> attributes = decryptNodeAttributes(this, (String) entries.get("at"), nodeKey);
            if (attributes != null) {
                link.setFinalFileName((String) attributes.get("n"));
            }
        }
        link.setVerifiedFileSize(((Number) entries.get("s")).longValue());
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(null));
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            if (dl.getConnection().getResponseCode() == 509) {
                /* MEGA transfer quota exceeded */
                br.followConnection(true);
                final String timeLeftString = dl.getConnection().getHeaderField("X-MEGA-Time-Left");
                long timeLeft = TimeUnit.MINUTES.toMillis(30);
                if (timeLeftString != null && timeLeftString.matches("^\\d+$")) {
                    timeLeft = Math.max(timeLeft, Long.parseLong(timeLeftString) * 1000l);
                }
                throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, "Transfer quota exceeded", timeLeft);
            }
            this.handleConnectionErrors(br, dl.getConnection());
        }
        dl.startDownload();
    }

    /**
     * Sends one command to the MEGA API in transfer.it context and returns its result object. </br> transferID: the "x" parameter scoping
     * file related commands to one transfer, null for commands which take it as argument. </br> passwordHash: the "pw" parameter for
     * password protected transfers, null if not needed.
     */
    @SuppressWarnings("unchecked")
    public Map<String, Object> callAPI(final Browser br, final String transferID, final String passwordHash, final String commandJson) throws Exception {
        return (Map<String, Object>) callAPIRaw(br, transferID, passwordHash, commandJson);
    }

    /**
     * Checks if the given password hash is correct for the given transfer. </br> Does not work for transfers which are not password
     * protected.
     */
    public boolean verifyPassword(final Browser br, final String transferID, final String passwordHash) throws Exception {
        try {
            final Object result = callAPIRaw(br, null, null, "{\"a\":\"xv\",\"xh\":\"" + transferID + "\",\"pw\":\"" + passwordHash + "\"}");
            /* Returns 1 if the password is correct. */
            return result instanceof Number && ((Number) result).intValue() == 1;
        } catch (final PluginException e) {
            if (e.getLinkStatus() == LinkStatus.ERROR_FATAL) {
                /* API error -14: wrong password */
                return false;
            }
            throw e;
        }
    }

    /**
     * Decrypts node attributes (e.g. the filename in field "n") with the given plain node key. </br> Returns null if the key has an
     * unexpected length or the decrypted data is not valid.
     */
    public static Map<String, Object> decryptNodeAttributes(Plugin plugin, final String attributes, final String nodeKey) throws Exception {
        byte[] key = MegaNz.b64decode(nodeKey);
        if (key.length == 32) {
            /* file key: AES key = first half XOR second half */
            final byte[] fileKey = new byte[16];
            for (int i = 0; i < 16; i++) {
                fileKey[i] = (byte) (key[i] ^ key[i + 16]);
            }
            key = fileKey;
        } else if (key.length != 16) {
            return null;
        }
        final byte[] encrypted = MegaNz.b64decode(attributes);
        final byte[] padded = new byte[(encrypted.length + 15) / 16 * 16];
        System.arraycopy(encrypted, 0, padded, 0, encrypted.length);
        final Cipher cipher = Cipher.getInstance("AES/CBC/NoPadding");
        cipher.init(Cipher.DECRYPT_MODE, new SecretKeySpec(key, "AES"), new IvParameterSpec(new byte[16]));
        final String decrypted = new String(cipher.doFinal(padded), "UTF-8");
        final String json = new Regex(decrypted, "^MEGA(\\{.+\\})").getMatch(0);
        if (json == null) {
            return null;
        }
        return plugin.restoreFromString(json, TypeRef.MAP);
    }

    /**
     * Creates the password hash which is sent as "pw" parameter: base64url(PBKDF2-HMAC-SHA256(password, salt, 100000, 256 bit)). </br> The
     * salt is derived from the transfer ID.
     */
    public static String createPasswordHash(final String transferID, final String password) throws Exception {
        final byte[] idBytes = MegaNz.b64decode(transferID);
        final byte[] saltPart;
        if (transferID.length() == 12) {
            saltPart = Arrays.copyOfRange(idBytes, idBytes.length - 6, idBytes.length);
        } else {
            /* Longer IDs are base64 encoded twice. */
            final byte[] inner = MegaNz.b64decode(new String(idBytes, "ISO-8859-1"));
            saltPart = Arrays.copyOfRange(inner, inner.length - 7, inner.length - 1);
        }
        /* Salt: Those 6 bytes repeated 3 times. */
        final byte[] salt = new byte[saltPart.length * 3];
        for (int i = 0; i < 3; i++) {
            System.arraycopy(saltPart, 0, salt, i * saltPart.length, saltPart.length);
        }
        final SecretKeyFactory skf = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256");
        final PBEKeySpec spec = new PBEKeySpec(password.trim().toCharArray(), salt, 100000, 256);
        return MegaNz.b64encode(skf.generateSecret(spec).getEncoded());
    }

    /**
     * @return Number (positive API result) or Map (result object)
     */
    private Object callAPIRaw(final Browser br, final String transferID, final String passwordHash, final String commandJson) throws Exception {
        String url = API_BASE + "?id=" + API_SEQUENCE.incrementAndGet() + "&v=3&domain=transferit";
        if (transferID != null) {
            url += "&x=" + transferID;
        }
        if (passwordHash != null) {
            url += "&pw=" + passwordHash;
        }
        br.postPageRaw(url, "[" + commandJson + "]");
        final String html = br.getRequest().getHtmlCode();
        if (br.getHttpConnection().getResponseCode() != 200 || html == null || html.trim().isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "API unavailable", TimeUnit.MINUTES.toMillis(5));
        }
        final String response = html.trim();
        /* Errors come as plain number e.g. "-9", as list e.g. "[-9]" or as object e.g. "[{"e":-9}]". */
        if (response.matches("^-?\\d+$")) {
            final int number = Integer.parseInt(response);
            if (number < 0) {
                throwAPIError(number);
                /* Unreachable code */
                throw new WTFException();
            }
            return number;
        }
        final List<?> parsed = (List<?>) JSonStorage.restoreFromString(response, TypeRef.OBJECT);
        final Object result = parsed.get(0);
        if (result instanceof Number) {
            if (((Number) result).intValue() < 0) {
                throwAPIError(((Number) result).intValue());
                /* Unreachable code */
                throw new WTFException();
            }
            return result;
        }
        @SuppressWarnings("unchecked")
        final Map<String, Object> entries = (Map<String, Object>) result;
        /* Error code is only present in case of an error. */
        final Object errorcode = entries.get("e");
        if (errorcode instanceof Number && ((Number) errorcode).intValue() < 0) {
            throwAPIError(((Number) errorcode).intValue());
            /* Unreachable code */
            throw new WTFException();
        }
        return entries;
    }

    /**
     * See MEGA API error codes e.g. in MegaNz plugin. </br> ERROR_FATAL is only used for -14 (EKEY: transfer.it asks for the password on
     * this error and it also means "wrong password"), the crawler relies on that.
     */
    private static void throwAPIError(final int errorcode) throws PluginException {
        switch (errorcode) {
        case -8:
        case -9:
        case -16:
            /* -8 = expired, -9 = not found, -16 = blocked/taken down */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        case -3:
        case -4:
            /* -3 = try again, -4 = rate limit */
            throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "API error " + errorcode, TimeUnit.MINUTES.toMillis(5));
        case -6:
        case -11:
        case -18:
            /* -6 = too many requests, -11 = access violation, -18 = temporarily unavailable */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "API error " + errorcode, TimeUnit.MINUTES.toMillis(5));
        case -14:
            throw new PluginException(LinkStatus.ERROR_FATAL, "Password required or wrong password");
        case -17:
            throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, "Request over quota", TimeUnit.HOURS.toMillis(1));
        default:
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "API error " + errorcode);
        }
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }
}
