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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.StringUtils;
import org.jdownloader.settings.GeneralSettings;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
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
import jd.plugins.hoster.MegaNz;
import jd.plugins.hoster.TransferIt;

/**
 * Most of the work on this plugin was done by Fabio Orlandi.
 */
@DecrypterPlugin(revision = "$Revision: 53696 $", interfaceVersion = 3, names = {}, urls = {})
public class TransferItCrawler extends PluginForDecrypt {
    public TransferItCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    private static final String PASSWORD_NOT_SUPPORTED = "Password protected transfer.it transfers are not yet supported.";

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        /* transfer.it is a service powered by MEGA -> mega.nz */
        ret.add(new String[] { "transfer.it" });
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
            ret.add("https?://" + buildHostsPatternPart(domains) + "/t/([A-Za-z0-9_-]{12,})");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String transferID = new Regex(param.getCryptedUrl(), this.getSupportedLinks()).getMatch(0);
        /* Transfer information such as title and password protection */
        final TransferIt hosterplugin = (TransferIt) this.getNewPluginForHostInstance(this.getHost());
        final Map<String, Object> transferInfo = hosterplugin.callAPI(br, null, "{\"a\":\"xi\",\"xh\":\"" + transferID + "\"}");
        final Object passwordProtected = transferInfo.get("pw");
        if (passwordProtected != null && !Boolean.FALSE.equals(passwordProtected) && !"0".equals(passwordProtected.toString()) && !"".equals(passwordProtected.toString())) {
            throw new DecrypterRetryException(RetryReason.PASSWORD, "PASSWORD_PROTECTED_" + transferID, PASSWORD_NOT_SUPPORTED);
        }
        String transferTitle = null;
        if (transferInfo.get("t") instanceof String) {
            transferTitle = new String(MegaNz.b64decode((String) transferInfo.get("t")), "UTF-8").trim();
        }
        if (StringUtils.isEmpty(transferTitle)) {
            transferTitle = transferID;
        }
        /* Transfer description (message) is optional and encoded the same way as the title. */
        String transferDescription = null;
        if (transferInfo.get("m") instanceof String) {
            transferDescription = new String(MegaNz.b64decode((String) transferInfo.get("m")), "UTF-8").trim();
        }
        /* Nodes of this transfer: Node keys are plain AES keys, node attributes are encrypted like regular MEGA node attributes. */
        final Map<String, Object> listing;
        try {
            listing = hosterplugin.callAPI(br, transferID, "{\"a\":\"f\",\"c\":1,\"r\":1}");
        } catch (final PluginException e) {
            if (e.getLinkStatus() == LinkStatus.ERROR_FATAL) {
                /* API error -14: password required (in case the "pw" flag above was not set) */
                throw new DecrypterRetryException(RetryReason.PASSWORD, "PASSWORD_PROTECTED_" + transferID, PASSWORD_NOT_SUPPORTED);
            }
            throw e;
        }
        @SuppressWarnings("unchecked")
        final List<Map<String, Object>> nodes = (List<Map<String, Object>>) listing.get("f");
        final Map<String, Map<String, Object>> nodesByID = new HashMap<String, Map<String, Object>>();
        for (final Map<String, Object> node : nodes) {
            nodesByID.put((String) node.get("h"), node);
        }
        final boolean useOriginalLastModified = JsonConfig.create(GeneralSettings.class).isUseOriginalLastModified();
        final Map<String, FilePackage> packagesByPath = new HashMap<String, FilePackage>();
        for (final Map<String, Object> node : nodes) {
            if (((Number) node.get("t")).intValue() != 0) {
                /* Skip folders */
                continue;
            }
            final String nodeID = (String) node.get("h");
            final String path = getFolderPath(nodesByID, node);
            final Map<String, Object> attributes = decryptNodeAttributes(node);
            String filename = getName(attributes);
            if (filename == null) {
                logger.warning("Failed to decrypt name of node: " + nodeID);
                filename = nodeID;
            }
            final DownloadLink link = createDownloadlink("");
            link.setDefaultPlugin(hosterplugin);
            link.setHost(this.getHost());
            link.setContentUrl(param.getCryptedUrl());
            link.setProperty(TransferIt.PROPERTY_TRANSFER_ID, transferID);
            link.setProperty(TransferIt.PROPERTY_NODE_ID, nodeID);
            link.setFinalFileName(filename);
            link.setVerifiedFileSize(((Number) node.get("s")).longValue());
            if (useOriginalLastModified && attributes != null) {
                /* Original last modified timestamp is stored in the file fingerprint attribute */
                final long lastModified = MegaNz.getTimestamp(this, attributes);
                if (lastModified > 0) {
                    link.setLastModifiedTimestamp(lastModified * 1000);
                }
            }
            link.setAvailable(true);
            final String packagePath = path != null ? path : transferTitle;
            FilePackage fp = packagesByPath.get(packagePath);
            if (fp == null) {
                fp = FilePackage.getInstance();
                fp.setName(packagePath.substring(packagePath.lastIndexOf("/") + 1));
                /* Fixed identifier instead of getHost() so the key stays stable even if the domain changes. */
                fp.setPackageKey("transferit://transfer/" + transferID + "/" + packagePath);
                if (!StringUtils.isEmpty(transferDescription)) {
                    fp.setComment(transferDescription);
                }
                packagesByPath.put(packagePath, fp);
            }
            if (path != null) {
                link.setRelativeDownloadFolderPath(path);
            }
            link._setFilePackage(fp);
            ret.add(link);
        }
        if (ret.isEmpty()) {
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER, transferTitle);
        }
        return ret;
    }

    /** Returns path of all parent folders of given node e.g. "Root/Subfolder" or null if there is no such folder. */
    private String getFolderPath(final Map<String, Map<String, Object>> nodesByID, final Map<String, Object> node) {
        String path = null;
        final Set<String> visited = new HashSet<String>();
        Object folderID = node.get("p");
        while (folderID instanceof String && visited.add((String) folderID)) {
            final Map<String, Object> folder = nodesByID.get(folderID);
            if (folder == null) {
                break;
            }
            String name = getName(decryptNodeAttributes(folder));
            if (name == null) {
                name = (String) folderID;
            }
            name = name.trim();
            path = path == null ? name : name + "/" + path;
            folderID = folder.get("p");
        }
        return path;
    }

    private static String getName(final Map<String, Object> attributes) {
        final Object name = attributes != null ? attributes.get("n") : null;
        return name instanceof String ? (String) name : null;
    }

    /** Decrypts attributes of given node with its plain node key and returns them or null on failure. */
    private Map<String, Object> decryptNodeAttributes(final Map<String, Object> node) {
        final String attributes = (String) node.get("a");
        final String nodeKey = (String) node.get("k");
        try {
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
            return JSonStorage.restoreFromString(json, TypeRef.MAP);
        } catch (final Exception e) {
            logger.log(e);
            return null;
        }
    }
}
