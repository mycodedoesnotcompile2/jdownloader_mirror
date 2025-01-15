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

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DefaultOnNull;
import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.config.Type;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.decrypter.SwisstransferComFolder.SwisstransferComConfig.CrawlMode;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 50442 $", interfaceVersion = 3, names = { "swisstransfer.com" }, urls = { "https?://(?:www\\.)?swisstransfer\\.com/d/([a-z0-9\\-]+)" })
public class SwisstransferComFolder extends PluginForDecrypt {
    public SwisstransferComFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String addedlink = param.getCryptedUrl();
        final String folderUUID = new Regex(addedlink, this.getSupportedLinks()).getMatch(0);
        final boolean useNewHandling = true;
        final Map<String, Object> data;
        if (useNewHandling) {
            br.getPage("https://www." + this.getHost() + "/api/links/" + folderUUID);
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final Map<String, Object> root = JavaScriptEngineFactory.jsonToJavaMap(br.getRequest().getHtmlCode());
            data = (Map<String, Object>) root.get("data");
        } else {
            br.getHeaders().put("accept", "application/json, text/plain, */*");
            br.postPage("https://www." + this.getHost() + "/api/isPasswordValid", "linkUUID=" + folderUUID);
            if (br.getHttpConnection().getResponseCode() == 404) {
                /* E.g. response "e034b988-de97-4333-956b-28ba66ed88888 Not found" (with "") */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (br.containsHTML("^\"late\"$")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            data = JavaScriptEngineFactory.jsonToJavaMap(br.getRequest().getHtmlCode());
        }
        final String downloadHost = data.get("downloadHost").toString();
        final Map<String, Object> container = (Map<String, Object>) data.get("container");
        final List<Map<String, Object>> ressourcelist = (List<Map<String, Object>>) container.get("files");
        final Number downloadLimit = (Number) container.get("downloadLimit");
        final SwisstransferComConfig cfg = PluginJsonConfig.get(SwisstransferComConfig.class);
        final CrawlMode crawlMode = cfg.getCrawlMode().getMode();
        if (ressourcelist.size() > 1 && !CrawlMode.FILES_FOLDERS.equals(crawlMode)) {
            final String expiredDate = (String) data.get("expiredDate");
            final String deletedDate = (String) data.get("deletedDate");
            final String containerUUID = (String) data.get("containerUUID");
            final String directurl = "https://" + downloadHost + "/api/download/" + folderUUID;
            final DownloadLink zip = createDownloadlink(DirectHTTP.createURLForThisPlugin(directurl));
            zip.setProperty(DirectHTTP.NORESUME, false);
            zip.setProperty(DirectHTTP.PROPERTY_CUSTOM_HOST, getHost());
            zip.setFinalFileName("swisstransfer_" + containerUUID + ".zip");
            if (deletedDate != null) {
                /* Deleted */
                zip.setAvailable(false);
            } else if (expiredDate != null) {
                final long timeStamp = TimeFormatter.getMilliSeconds(expiredDate, "yyyy-MM-dd HH:mm:ss", Locale.ENGLISH);
                if (System.currentTimeMillis() < timeStamp) {
                    zip.setAvailable(true);
                } else {
                    zip.setAvailable(false);
                }
            } else {
                zip.setAvailable(true);
            }
            ret.add(zip);
            distribute(zip);
            if (CrawlMode.ZIP.equals(crawlMode)) {
                return ret;
            }
        }
        String fpName = (String) container.get("message");
        final FilePackage fp = FilePackage.getInstance();
        if (fpName != null) {
            fp.setName(Encoding.htmlDecode(fpName).trim());
        } else {
            /* Fallback */
            fp.setName(folderUUID);
        }
        int offset = 0;
        int page = 0;
        /* TODO: Add proper pagination support */
        boolean hasNext = false;
        do {
            // getPage("");
            // if (br.getHttpConnection().getResponseCode() == 404) {
            // decryptedLinks.add(this.createOfflinelink(parameter));
            // return decryptedLinks;
            // }
            for (final Map<String, Object> file : ressourcelist) {
                final String filename = (String) file.get("fileName");
                final String fileid = (String) file.get("UUID");
                if (StringUtils.isEmpty(filename) || StringUtils.isEmpty(fileid)) {
                    continue;
                }
                Number filesize = (Number) file.get("sizeUploaded");
                if (filesize == null) {
                    filesize = (Number) file.get("fileSizeInBytes");
                }
                final Number downloadCounter = (Number) file.get("downloadCounter");
                final String expiredDate = (String) file.get("expiredDate");
                final String deletedDate = (String) file.get("deletedDate");
                /* Old format: */
                // final String directurl = String.format("https://www.swisstransfer.com/api/download/%s/%s", linkUUID, fileid);
                final String directurl = "https://" + downloadHost + "/api/download/" + folderUUID + "/" + fileid;
                final DownloadLink dl = createDownloadlink(DirectHTTP.createURLForThisPlugin(directurl));
                dl.setProperty(DirectHTTP.PROPERTY_CUSTOM_HOST, getHost());
                if (filesize != null) {
                    dl.setVerifiedFileSize(filesize.longValue());
                }
                dl.setFinalFileName(filename);
                if (deletedDate != null) {
                    /* Deleted */
                    dl.setAvailable(false);
                } else if (expiredDate != null) {
                    final long timeStamp = TimeFormatter.getMilliSeconds(expiredDate, "yyyy-MM-dd HH:mm:ss", Locale.ENGLISH);
                    if (System.currentTimeMillis() < timeStamp) {
                        dl.setAvailable(true);
                    } else {
                        dl.setAvailable(false);
                    }
                } else {
                    dl.setAvailable(true);
                }
                if (downloadCounter != null && downloadLimit != null && downloadLimit.longValue() == downloadCounter.longValue()) {
                    dl.setAvailable(false);
                }
                dl._setFilePackage(fp);
                if (ressourcelist.size() > 1) {
                    dl.setContainerUrl(addedlink);
                } else {
                    dl.setContentUrl(addedlink);
                }
                ret.add(dl);
                distribute(dl);
                offset++;
            }
            logger.info("Crawled page " + page + " | Offset: " + offset + " | Found items so far: " + ret.size());
            if (this.isAbort()) {
                logger.info("Stopping because: Aborted by user");
                break;
            } else if (!hasNext) {
                logger.info("Stopping because: Reached last page");
                break;
            } else {
                page++;
            }
        } while (true);
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
                    return "Add .zip only";
                }
            },
            FILES_FOLDERS {
                @Override
                public String getLabel() {
                    return "Add loose files & folders";
                }
            },
            ALL {
                @Override
                public String getLabel() {
                    return "Add loose files & folders AND .zip with all items";
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
