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
import java.util.List;

import jd.PluginWrapper;
import jd.config.SubConfiguration;
import jd.http.Browser;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.components.XFileSharingProBasic;

@HostPlugin(revision = "$Revision: 51136 $", interfaceVersion = 3, names = {}, urls = {})
public class ClicknuploadOrg extends XFileSharingProBasic {
    public ClicknuploadOrg(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
    }

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info: 2019-07-25: premium untested, set FREE account limits <br />
     * captchatype-info: 2019-07-25: null<br />
     * other:<br />
     */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "clicknupload.click", "clicknupload.link", "clicknupload.red", "clicknupload.to", "clicknupload.cc", "clicknupload.co", "clicknupload.org", "clicknupload.com", "clicknupload.me", "clicknupload.club", "clicknupload.online", "clicknupload.download", "clicknupload.vip", "clicknupload.site", "clicknupload.xyz", "clicknupload.one", "clicknupload.name", "clicknupload.space", "clickndownload.org", "clickndownload.space", "clickndownload.click", "clickndownload.link", "clickndownload.site", "clickndownload.online", "clicknupload.cfd", "clickndownload.cloud", "clickndownload.cfd" });
        return ret;
    }

    @Override
    protected List<String> getDeadDomains() {
        final List<String> deadDomains = new ArrayList<String>();
        deadDomains.add("clicknupload.red");
        deadDomains.add("clicknupload.link");
        deadDomains.add("clicknupload.com");
        deadDomains.add("clicknupload.club");
        deadDomains.add("clickndownload.cloud");
        return deadDomains;
    }

    @Override
    public String rewriteHost(final String host) {
        /* This host is frequently changing its main domain. */
        return this.rewriteHost(getPluginDomains(), host, new String[0]);
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        final String[] supported_names_official = buildSupportedNames(getPluginDomains());
        final String[] supported_names_full = new String[supported_names_official.length + 1];
        int position = 0;
        for (final String supported_name : supported_names_official) {
            supported_names_full[position] = supported_name;
            position++;
        }
        /* 2019-08-27: For multihoster 'missing TLD handling' */
        supported_names_full[position] = "clicknupload";
        return supported_names_full;
    }

    public static String[] getAnnotationUrls() {
        return XFileSharingProBasic.buildAnnotationUrls(getPluginDomains());
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        if (account != null && account.getType() == AccountType.FREE) {
            /* Free Account */
            return true;
        } else if (account != null && account.getType() == AccountType.PREMIUM) {
            /* Premium account */
            return true;
        } else {
            /* Free(anonymous) and unknown account type */
            return true;
        }
    }

    @Override
    public int getMaxChunks(final Account account) {
        if (account != null && account.getType() == AccountType.FREE) {
            /* Free Account */
            return 1;
        } else if (account != null && account.getType() == AccountType.PREMIUM) {
            /* Premium account */
            return -5;
        } else {
            /* Free(anonymous) and unknown account type */
            return 1;
        }
    }

    @Override
    protected void processFileInfo(String[] fileInfo, Browser altbr, DownloadLink link) {
        try {
            // 2021-07: ?op=check_files is broken, use file size from 2nd free download page
            final Form download1 = findFormDownload1Free(br);
            if (download1 != null && (link.getKnownDownloadSize() == -1 && StringUtils.isEmpty(fileInfo[1]))) {
                final Browser brc = br.cloneBrowser();
                logger.info("Found download1 Form");
                final String existingFilename = fileInfo[0];
                try {
                    submitForm(brc, download1);
                    scanInfo(brc.getRequest().getHtmlCode(), fileInfo);
                } finally {
                    if (existingFilename != null) {
                        // nice/full filename already parsed from fname in scaninfo
                        fileInfo[0] = existingFilename;
                    }
                }
            }
        } catch (Exception e) {
            logger.log(e);
        }
        // super.processFileInfo(fileInfo, altbr, link);
    }

    @Override
    public int getMaxSimultaneousFreeAnonymousDownloads() {
        /* 2024-02-02 */
        return 3;
    }

    @Override
    public int getMaxSimultaneousFreeAccountDownloads() {
        /* 2024-02-02 */
        return 3;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return 10;
    }

    @Override
    protected boolean supports_availablecheck_filesize_html() {
        /* 2019-07-25: Special */
        return false;
    }

    @Override
    protected boolean enableAccountApiOnlyMode() {
        // return DebugMode.TRUE_IN_IDE_ELSE_FALSE;
        return false;
    }

    @Override
    public String[] scanInfo(final String html, final String[] fileInfo) {
        // final String betterFilename = new Regex(html, "<title>Download ([^<]+)</title>").getMatch(0);
        // if (betterFilename != null) {
        // fileInfo[0] = betterFilename;
        // }
        final String betterFilesize = new Regex(html, ">\\s*size\\s*</span>\\s*<span>([^<]+)</span>").getMatch(0);
        if (betterFilesize != null) {
            fileInfo[1] = betterFilesize;
        }
        return fileInfo;
    }

    @Override
    protected String getFnameViaAbuseLink(final Browser br, final DownloadLink link) throws Exception {
        final String url = getMainPage() + "/?op=report_file&id=" + this.getFUIDFromURL(link);
        /* 2025-04-11: Referer required otherwise this request may fail */
        boolean success = false;
        for (int i = 0; i <= 3; i++) {
            logger.info("Attempt " + (i + 1) + "/3");
            getPage(br, url, false);
            if (StringUtils.containsIgnoreCase(br.getURL(), "op=report_file")) {
                success = true;
                break;
            }
        }
        if (!success) {
            logger.warning("Special handling failed");
            return null;
        }
        /*
         * 2019-07-10: ONLY "No such file" as response might always be wrong and should be treated as a failure! Example: xvideosharing.com
         */
        if (br.containsHTML(">\\s*No such file\\s*<")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String filename = regexFilenameAbuse(br);
        if (filename != null) {
            logger.info("Successfully found filename via report_file: " + filename);
            return filename;
        } else {
            logger.info("Failed to find filename via report_file");
            final boolean fnameViaAbuseUnsupported = br.getHttpConnection().getResponseCode() == 404 || br.getHttpConnection().getResponseCode() == 500 || !br.getURL().contains("report_file") || br.getRequest().getHtmlCode().trim().equalsIgnoreCase("No such file");
            if (fnameViaAbuseUnsupported) {
                logger.info("Seems like report_file availablecheck seems not to be supported by this host");
                final SubConfiguration config = this.getPluginConfig();
                config.setProperty(PROPERTY_PLUGIN_REPORT_FILE_AVAILABLECHECK_LAST_FAILURE_TIMESTAMP, System.currentTimeMillis());
                config.setProperty(PROPERTY_PLUGIN_REPORT_FILE_AVAILABLECHECK_LAST_FAILURE_VERSION, getPluginVersionHash());
            }
            return null;
        }
    }
}