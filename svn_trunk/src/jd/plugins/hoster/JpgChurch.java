//jDownloader - Downloadmanager
//Copyright (C) 2017  JD-Team support@jdownloader.org
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
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Pattern;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.parser.html.HTMLSearch;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.SiteType.SiteTemplate;
import jd.plugins.decrypter.JpgChurchCrawler;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.controller.LazyPlugin;

@HostPlugin(revision = "$Revision: 51630 $", interfaceVersion = 3, names = {}, urls = {})
public class JpgChurch extends PluginForHost {
    public JpgChurch(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.IMAGE_HOST };
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 1;
    }

    private String               dllink             = null;
    private final String         PROPERTY_USER      = "user";
    public static final String   PROPERTY_PHPSESSID = "phpsessid";
    /* Position of item if added as part of album (starts from 1). */
    public static final String   PROPERTY_POSITION  = "position";
    /* Don't touch the following! */
    private static AtomicInteger freeRunning        = new AtomicInteger(0);

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "jpg6.su", "jpg5.su", "jpg4.su", "jpg3.su", "jpg2.su", "jpg1.su", "jpeg.pet", "jpg.pet", "jpg.fishing", "jpg.fish", "jpg.church" });
        ret.add(new String[] { "imagepond.net" });
        return ret;
    }

    protected List<String> getDeadDomains() {
        final ArrayList<String> deadDomains = new ArrayList<String>();
        deadDomains.add("jpg.church"); // 2024-10-07
        deadDomains.add("jpg.fish"); // 2024-10-07
        deadDomains.add("jpg.fishing"); // 2024-10-07
        deadDomains.add("jpg.pet"); // 2024-10-07
        deadDomains.add("jpeg.pet"); // 2024-10-07
        deadDomains.add("jpg1.su"); // 2024-10-07
        deadDomains.add("jpg2.su"); // 2024-10-07)
        return deadDomains;
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(?:img|image|video)/([A-Za-z0-9\\-\\.%]+)");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String rewriteHost(final String host) {
        /* 2023-02-21: Main domain changed from jpg.church to jpg.fish. */
        /* 2023-06-20: Main domain changed from jpg.fishing to jpg.pet. */
        /* 2023-07-12: Main domain changed from jpg.pet to jpeg.pet */
        /* 2023-08-14: Main domain changed from jpeg.pet to jpg1.su */
        /* 2023-12-19: Main domain changed from jpg2.su to jpg3.su */
        /* 2024-09-02: Main domain changed from jpg4.su to jpg5.su */
        /* 2025-07-18: Main domain changed from jpg5.su to jpg6.su (current main) */
        return this.rewriteHost(getPluginDomains(), host);
    }

    @Override
    public String getAGBLink() {
        return "https://" + this.getHost() + "/page/tos";
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String linkid = getFID(link);
        if (linkid != null) {
            return this.getHost() + "://" + linkid;
        } else {
            return super.getLinkID(link);
        }
    }

    @Override
    public String getMirrorID(final DownloadLink link) {
        String fid = null;
        if (link != null && StringUtils.equals(getHost(), link.getHost()) && (fid = getFID(link)) != null) {
            return getHost() + "://" + fid;
        } else {
            return super.getMirrorID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    private String getContentURL(final DownloadLink link) {
        String contenturl = link.getPluginPatternMatcher();
        final String addedLinkDomain = Browser.getHost(contenturl, true);
        String domainToUse = addedLinkDomain;
        if (getDeadDomains().contains(addedLinkDomain)) {
            domainToUse = this.getHost();
            contenturl = contenturl.replaceFirst(Pattern.quote(addedLinkDomain), domainToUse);
        }
        return contenturl;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return requestFileInformation(link, false);
    }

    public AvailableStatus requestFileInformation(final DownloadLink link, final boolean isDownload) throws Exception {
        final boolean isVideo = StringUtils.containsIgnoreCase(link.getPluginPatternMatcher(), "/video/");
        if (!link.isNameSet()) {
            final String weakTitle = this.getFID(link).replaceAll("-+", " ").trim();
            final String extFallback;
            if (isVideo) {
                extFallback = ".mp4";
            } else {
                extFallback = ".jpg";
            }
            link.setName(this.applyFilenameExtension(weakTitle, extFallback));
        }
        this.setBrowserExclusive();
        String title = null;
        String filesizeStr = null;
        /*
         * Re-use old sessionID for password protected items. This can avoid the need of additional steps such as having to enter the
         * password and/or having to enter a captcha.
         */
        final String passwordSessionPhpsessid = link.getStringProperty(PROPERTY_PHPSESSID);
        if (passwordSessionPhpsessid != null) {
            br.setCookie(this.getHost(), "PHPSESSID", passwordSessionPhpsessid);
        }
        final String contentURL = getContentURL(link);
        boolean useWebsite = false;
        if (link.isPasswordProtected()) {
            useWebsite = true;
        } else if (isVideo) {
            /* 2025-10-07: oembed is not supported for video links (tested with imagepond.net) */
            useWebsite = true;
        } else {
            final UrlQuery query = new UrlQuery();
            query.add("url", URLEncode.encodeURIComponent(contentURL));
            query.add("format", "json");
            br.getPage("https://" + this.getHost() + "/oembed/?" + query.toString());
            if (br.getHttpConnection().getResponseCode() == 403) {
                /* Link is broken/invalid */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (br.getHttpConnection().getResponseCode() == 404) {
                /* Content is offline */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            /* Check if item is password protected */
            if (br.getHttpConnection().getResponseCode() == 401) {
                logger.info("This item is password protected");
                link.setPasswordProtected(true);
                /* Website needed to handle password stuff. */
                useWebsite = true;
                if (!isDownload) {
                    /* Do not ask for password during availablecheck. */
                    return AvailableStatus.TRUE;
                }
            }
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            if (entries == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            title = (String) entries.get("title");
            final String thumbnailURL = (String) entries.get("url");
            if (thumbnailURL == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            /* Remove part of this URL to get the full image. */
            this.dllink = thumbnailURL.replaceFirst("(?i)\\.md\\.(jpe?g|webp|gif|png|bmp)$", ".$1");
            final String author = (String) entries.get("author");
            if (author != null) {
                link.setProperty(PROPERTY_USER, author);
            }
            /* Set correct Referer header. */
            br.setCurrentURL(contentURL);
        }
        if (useWebsite) {
            br.getPage(contentURL);
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            Form pwform = JpgChurchCrawler.getPasswordForm(br);
            if (pwform != null) {
                logger.info("This item is password protected");
                link.setPasswordProtected(true);
                if (!isDownload) {
                    /* Do not ask for password during availablecheck. */
                    return AvailableStatus.TRUE;
                }
                String passCode = link.getDownloadPassword();
                int counter = 0;
                boolean success = false;
                do {
                    if (passCode == null || counter > 0) {
                        passCode = getUserInput("Password?", link);
                    }
                    pwform.put("content-password", Encoding.urlEncode(passCode));
                    br.submitForm(pwform);
                    // if (!this.canHandle(br.getURL())) {
                    // br.getPage(contentURLCleaned);
                    // }
                    pwform = JpgChurchCrawler.getPasswordForm(br);
                    if (pwform == null) {
                        logger.info("User entered valid password: " + passCode);
                        success = true;
                        break;
                    } else {
                        logger.info("User entered invalid password: " + passCode);
                        counter++;
                    }
                } while (counter <= 2);
                if (!success) {
                    /* Invalidate potentially previously saved password */
                    link.setDownloadPassword(null);
                    throw new PluginException(LinkStatus.ERROR_CAPTCHA);
                }
                link.setDownloadPassword(passCode);
                /* Save session cookie to speed-up next attempt. */
                link.setProperty(PROPERTY_PHPSESSID, br.getCookie(br.getHost(), "PHPSESSID"));
            }
            title = HTMLSearch.searchMetaTag("og:title", br.getRequest().getHtmlCode());
            /* Filesize in html code is available when file has an official download button. */
            filesizeStr = br.getRegex("btn-download default\"[^>]*rel=\"tooltip\"[^>]*title=\"\\d+ x \\d+ - [A-Za-z0-9]+ (\\d+[^\"]+)\"").getMatch(0);
            /* Prefer official download */
            dllink = br.getRegex("href\\s*=\\s*\"(https?://[^\"]+)\"[^>]*download=\"").getMatch(0);
            if (dllink == null) {
                dllink = br.getRegex("property\\s*=\\s*\"og:image\" content\\s*=\\s*\"(https?://[^\"]+)\"").getMatch(0);
            }
            if (dllink == null) {
                dllink = br.getRegex("<link rel\\s*=\\s*\"image_src\" href\\s*=\\s*\"(https?://[^\"]+)\">").getMatch(0);
            }
            final String author = br.getRegex("username\\s*:\\s*\"([^\"]+)\"").getMatch(0);
            if (author != null) {
                link.setProperty(PROPERTY_USER, author);
            }
        }
        String ext = null;
        if (!StringUtils.isEmpty(title)) {
            ext = dllink != null ? getFileNameExtensionFromURL(dllink) : null;
            title = Encoding.htmlDecode(title).trim();
            if (ext == null) {
                link.setName(title);
            } else {
                link.setFinalFileName(this.applyFilenameExtension(title, ext));
            }
        }
        if (!StringUtils.isEmpty(filesizeStr)) {
            link.setDownloadSize(SizeFormatter.getSize(filesizeStr));
        }
        if (!StringUtils.isEmpty(dllink) && StringUtils.isEmpty(filesizeStr) && !isDownload) {
            basicLinkCheck(br.cloneBrowser(), br.createHeadRequest(dllink), link, title, ext);
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link, true);
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
        handleConnectionErrors(br, dl.getConnection());
        /* Add a download slot */
        controlMaxFreeDownloads(null, link, +1);
        try {
            /* Start download */
            dl.startDownload();
        } finally {
            /* Remove download slot */
            controlMaxFreeDownloads(null, link, -1);
        }
    }

    protected void controlMaxFreeDownloads(final Account account, final DownloadLink link, final int num) {
        if (account == null) {
            synchronized (freeRunning) {
                final int before = freeRunning.get();
                final int after = before + num;
                freeRunning.set(after);
                logger.info("freeRunning(" + link.getName() + ")|max:" + getMaxSimultanFreeDownloadNum() + "|before:" + before + "|after:" + after + "|num:" + num);
            }
        }
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        // final int max = 100;
        final int running = freeRunning.get();
        // final int ret = Math.min(running + 1, max);
        // return ret;
        return running + 1;
    }

    @Override
    public SiteTemplate siteTemplateType() {
        return SiteTemplate.CheveretoImageHosting;
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        return false;
    }

    @Override
    public void resetPluginGlobals() {
    }
}