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
import java.util.Locale;
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DefaultOnNull;
import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.config.Type;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterException;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.decrypter.SwisstransferComFolder.SwisstransferComConfig.CrawlMode;
import jd.plugins.hoster.SwisstransferCom;

@DecrypterPlugin(revision = "$Revision: 50474 $", interfaceVersion = 3, names = {}, urls = {})
public class SwisstransferComFolder extends PluginForDecrypt {
    public SwisstransferComFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        br.setLoadLimit(Integer.MAX_VALUE);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "swisstransfer.com" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/d/([a-z0-9\\-]+)");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String addedlink = param.getCryptedUrl();
        final String linkUUID = new Regex(addedlink, this.getSupportedLinks()).getMatch(0);
        String passCode = param.getDecrypterPassword();
        int pwcounter = 0;
        boolean pwsuccess = false;
        Map<String, Object> data;
        pwloop: do {
            if (pwcounter > 0) {
                passCode = getUserInput("Password?", param);
            }
            if (passCode != null) {
                br.getHeaders().put("Authorization", Encoding.Base64Encode(passCode));
            }
            final boolean useNewHandling = true;
            if (useNewHandling) {
                br.getPage("https://www." + this.getHost() + "/api/links/" + linkUUID);
                if (br.getHttpConnection().getResponseCode() == 404) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
                final Map<String, Object> root = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                data = (Map<String, Object>) root.get("data");
            } else {
                br.getHeaders().put("accept", "application/json, text/plain, */*");
                br.postPage("https://www." + this.getHost() + "/api/isPasswordValid", "linkUUID=" + linkUUID);
                if (br.getHttpConnection().getResponseCode() == 404) {
                    /* E.g. response "e034b988-de97-4333-956b-28ba66ed88888 Not found" (with "") */
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                } else if (br.containsHTML("^\"late\"$")) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
                data = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            }
            final String type = (String) data.get("type");
            if (type == null) {
                pwsuccess = true;
                break pwloop;
            } else if (!type.matches("need_password|wrong_password")) {
                pwsuccess = true;
                break pwloop;
            } else {
                /* Password wrong or password needed */
                pwcounter++;
            }
        } while (pwcounter <= 2);
        if (!pwsuccess) {
            throw new DecrypterException(DecrypterException.PASSWORD);
        }
        final String type = (String) data.get("type");
        if (StringUtils.equalsIgnoreCase(type, "expired")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /**
         * Every .zip container download deducts 1 credits. </br>
         * Every single file download also deducts 1 credits. </br>
         * This value can also be lower than zero!
         */
        final int dlCounterCredit = ((Number) data.get("downloadCounterCredit")).intValue();
        final String dlHost = data.get("downloadHost").toString();
        final SwisstransferComConfig cfg = PluginJsonConfig.get(SwisstransferComConfig.class);
        final CrawlMode crawlMode = cfg.getCrawlMode().getMode();
        final boolean hasReachedDownloadsLimit = dlCounterCredit <= 0;
        final String expiredDate = (String) data.get("expiredDate");
        final String deletedDate = (String) data.get("deletedDate");
        final String containerUUID = (String) data.get("containerUUID");
        boolean isExpired = false;
        if (expiredDate != null) {
            final long timeStamp = TimeFormatter.getMilliSeconds(expiredDate, "yyyy-MM-dd HH:mm:ss", Locale.ENGLISH);
            if (timeStamp < System.currentTimeMillis()) {
                isExpired = true;
            }
        }
        boolean isOnlineAndDownloadable = true;
        if (deletedDate != null) {
            /* Deleted */
            isOnlineAndDownloadable = false;
        } else if (isExpired) {
            isOnlineAndDownloadable = false;
        } else if (hasReachedDownloadsLimit) {
            isOnlineAndDownloadable = false;
        }
        final Map<String, Object> container = (Map<String, Object>) data.get("container");
        final List<Map<String, Object>> ressourcelist = (List<Map<String, Object>>) container.get("files");
        final Number dlLimit = (Number) container.get("downloadLimit");
        int totalDownloadsCounter = 0;
        for (final Map<String, Object> file : ressourcelist) {
            final String filename = file.get("fileName").toString();
            final String fileid = file.get("UUID").toString();
            Number filesize = (Number) file.get("fileSizeInBytes");
            if (filesize == null) {
                filesize = (Number) file.get("receivedSizeInBytes");
            }
            final String path = (String) file.get("path");
            final DownloadLink dl = createDownloadlink("");
            dl.setProperty(SwisstransferCom.PROPERTY_FILE_ID, fileid);
            if (filesize != null) {
                dl.setVerifiedFileSize(filesize.longValue());
            }
            dl.setFinalFileName(filename);
            dl.setContentUrl(addedlink);
            if (!StringUtils.isEmpty(path)) {
                dl.setComment(path);
            }
            ret.add(dl);
            final Number downloadCounter = (Number) file.get("downloadCounter");
            if (downloadCounter != null) {
                totalDownloadsCounter += downloadCounter.longValue();
            }
        }
        boolean forceZipOnly = false;
        if (isOnlineAndDownloadable && dlCounterCredit > 0 && ressourcelist.size() > dlCounterCredit) {
            /*
             * For example folder with 2500 files while global max downloads limit is set to only 250 -> User can never download all
             * individual files but can download the .zip which contains all files since that only counts as 1 download.
             */
            logger.info("The user will not be able to download all individual files of this folder as the download count limit will be reached before: Files: " + ressourcelist.size() + " > " + dlCounterCredit);
            forceZipOnly = true;
        }
        if (ressourcelist.size() > 1 && (!CrawlMode.FILES_FOLDERS.equals(crawlMode) || forceZipOnly)) {
            /* Server side created .zip file which contains all files of this folder */
            final DownloadLink zip = createDownloadlink("");
            zip.setProperty(SwisstransferCom.PROPERTY_IS_ZIP_CONTAINER, true);
            zip.setName("swisstransfer_" + containerUUID + ".zip");
            zip.setDownloadSize(((Number) container.get("sizeUploaded")).longValue());
            if (CrawlMode.ZIP.equals(crawlMode) || forceZipOnly) {
                /* Remove all non-zip files */
                ret.clear();
            }
            ret.add(zip);
        }
        logger.info("Found file items: " + ressourcelist.size() + " | downloadCounter = " + totalDownloadsCounter);
        /* Set additional properties */
        String containerMessage = (String) container.get("message");
        final FilePackage fp = FilePackage.getInstance();
        if (!StringUtils.isEmpty(containerMessage)) {
            fp.setName(containerMessage.trim());
        } else {
            /* Fallback */
            fp.setName(linkUUID);
        }
        fp.setPackageKey(this.getHost() + "/linkUUID/" + linkUUID);
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            fp.setComment("DLs total: " + totalDownloadsCounter + " | DLs left: " + dlCounterCredit + " of " + dlLimit + " | Expired: " + isExpired + " | deletedDate: " + deletedDate);
        }
        final SwisstransferCom hosterplugin = (SwisstransferCom) this.getNewPluginForHostInstance(this.getHost());
        for (final DownloadLink result : ret) {
            result.setDefaultPlugin(hosterplugin);
            result.setHost(this.getHost());
            result.setProperty(SwisstransferCom.PROPERTY_LINK_UUID, linkUUID);
            result.setProperty(SwisstransferCom.PROPERTY_CONTAINER_UUID, containerUUID);
            result.setProperty(SwisstransferCom.PROPERTY_DOWNLOAD_HOST, dlHost);
            if (passCode != null) {
                result.setDownloadPassword(passCode);
            }
            if (isOnlineAndDownloadable) {
                result.setAvailable(true);
            } else {
                result.setAvailable(false);
                result.setProperty(SwisstransferCom.PROPERTY_PERMANENTLY_OFFLINE, true);
            }
            result._setFilePackage(fp);
        }
        return ret;
    }

    @Override
    public Class<? extends PluginConfigInterface> getConfigInterface() {
        return SwisstransferComConfig.class;
    }

    @PluginHost(host = "swisstransfer.com", type = Type.CRAWLER)
    public static interface SwisstransferComConfig extends PluginConfigInterface {
        public static final TRANSLATION TRANSLATION  = new TRANSLATION();
        public static final CrawlMode   DEFAULT_MODE = CrawlMode.FILES_FOLDERS;

        public static class TRANSLATION {
            public String getCrawlMode2_label() {
                return "Crawl mode";
            }
        }

        public static enum CrawlMode implements LabelInterface {
            ZIP {
                @Override
                public String getLabel() {
                    return "Add .zip container only";
                }
            },
            FILES_FOLDERS {
                @Override
                public String getLabel() {
                    return "Add loose files";
                }
            },
            ALL {
                @Override
                public String getLabel() {
                    return "Add loose files and .zip container";
                }
            },
            DEFAULT {
                @Override
                public String getLabel() {
                    return "Default: " + DEFAULT_MODE.getLabel();
                }

                @Override
                public CrawlMode getMode() {
                    return DEFAULT_MODE.getMode();
                }
            };

            public CrawlMode getMode() {
                return this;
            }
        }

        @AboutConfig
        @DefaultEnumValue("DEFAULT")
        @Order(10)
        @DefaultOnNull
        CrawlMode getCrawlMode();

        void setCrawlMode(final CrawlMode mode);
    }
}
