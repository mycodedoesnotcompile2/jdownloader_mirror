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

import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.StringUtils;
import org.jdownloader.settings.GeneralSettings;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterException;
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
@DecrypterPlugin(revision = "$Revision: 53761 $", interfaceVersion = 3, names = {}, urls = {})
public class TransferItCrawler extends PluginForDecrypt {
    public TransferItCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

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
        final Map<String, Object> transferInfo = hosterplugin.callAPI(br, null, null, "{\"a\":\"xi\",\"xh\":\"" + transferID + "\"}");
        /* Optional field: A truthy "pw" value means that the transfer is password protected. */
        final Object passwordProtectedO = transferInfo.get("pw");
        boolean passwordRequired = passwordProtectedO != null && !Boolean.FALSE.equals(passwordProtectedO) && !"0".equals(passwordProtectedO.toString()) && !"".equals(passwordProtectedO.toString());
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
        final String listCommand = "{\"a\":\"f\",\"c\":1,\"r\":1}";
        String passCode = null;
        String passwordHash = null;
        Map<String, Object> listing = null;
        if (!passwordRequired) {
            try {
                listing = hosterplugin.callAPI(br, transferID, null, listCommand);
            } catch (final PluginException e) {
                if (e.getLinkStatus() != LinkStatus.ERROR_FATAL) {
                    throw e;
                }
                /* API error -14: password required (in case the "pw" flag above was not set) */
                passwordRequired = true;
            }
        }
        if (passwordRequired) {
            passCode = getPassword(param, hosterplugin, transferID);
            passwordHash = TransferIt.createPasswordHash(transferID, passCode);
            listing = hosterplugin.callAPI(br, transferID, passwordHash, listCommand);
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
            if (passwordHash != null) {
                /* Password required && user has entered correct password. */
                link.setProperty(TransferIt.PROPERTY_PASSWORD_HASH, passwordHash);
                link.setDownloadPassword(passCode);
            }
            link.setFinalFileName(filename);
            link.setProperty(TransferIt.PROPERTY_NODE_KEY, node.get("k"));
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

    /** Asks the user for the password of a password protected transfer until it is correct and returns it. */
    private String getPassword(final CryptedLink param, final TransferIt hosterplugin, final String transferID) throws Exception {
        String passCode = param.getDecrypterPassword();
        int counter = 0;
        do {
            if (StringUtils.isEmpty(passCode) || counter > 0) {
                passCode = getUserInput("Password?", param);
            }
            final String passwordHash = TransferIt.createPasswordHash(transferID, passCode);
            if (hosterplugin.verifyPassword(br, transferID, passwordHash)) {
                return passCode;
            }
            logger.info("User entered wrong password: " + passCode);
            passCode = null;
            counter++;
        } while (counter <= 2);
        throw new DecrypterException(DecrypterException.PASSWORD);
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
        try {
            return TransferIt.decryptNodeAttributes((String) node.get("a"), (String) node.get("k"));
        } catch (final Exception e) {
            logger.log(e);
            return null;
        }
    }
}
