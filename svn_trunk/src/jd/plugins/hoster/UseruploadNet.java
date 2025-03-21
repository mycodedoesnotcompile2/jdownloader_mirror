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

import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.components.XFileSharingProBasic;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;

@HostPlugin(revision = "$Revision: 50481 $", interfaceVersion = 3, names = {}, urls = {})
public class UseruploadNet extends XFileSharingProBasic {
    public UseruploadNet(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
    }

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info: 2019-02-22: untested, set FREE account limits<br />
     * captchatype-info: 2019-02-22: reCaptchaV2<br />
     * other:<br />
     */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "userupload.net", "proapk.net" });
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
        return UseruploadNet.buildAnnotationUrls(getPluginDomains());
    }

    public static String[] buildAnnotationUrls(List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(" + XFileSharingProBasic.getDefaultAnnotationPatternPart() + "|/d/[A-Za-z0-9]+)");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        if (account != null && account.getType() == AccountType.FREE) {
            /* Free Account */
            return false;
        } else if (account != null && account.getType() == AccountType.PREMIUM) {
            /* Premium account */
            return false;
        } else {
            /* Free(anonymous) and unknown account type */
            return false;
        }
    }

    @Override
    public int getMaxChunks(final Account account) {
        if (account != null && account.getType() == AccountType.FREE) {
            /* Free Account */
            return 1;
        } else if (account != null && account.getType() == AccountType.PREMIUM) {
            /* Premium account */
            return 1;
        } else {
            /* Free(anonymous) and unknown account type */
            return 1;
        }
    }

    @Override
    protected boolean supports_availablecheck_filesize_html() {
        /* 2019-07-15: Special */
        return false;
    }

    @Override
    public String[] scanInfo(final String[] fileInfo) {
        super.scanInfo(fileInfo);
        if (StringUtils.isEmpty(fileInfo[0])) {
            fileInfo[0] = br.getRegex("class=\"zmdi zmdi-file visible-xs\"></i>([^<>\"]+)<").getMatch(0);
        }
        if (StringUtils.isEmpty(fileInfo[0])) {
            /* 2020-02-04 */
            fileInfo[0] = br.getRegex("class=\"name\">\\s*<h4>([^<>\"]+)<").getMatch(0);
        }
        if (StringUtils.isEmpty(fileInfo[1])) {
            final String fileSize = br.getRegex("<span>\\s*Uploaded on.*?</span>\\s*(<span>[^<]+</span>)").getMatch(0);
            fileInfo[1] = scanGenericFileSize(fileSize);
        }
        if (StringUtils.isEmpty(fileInfo[1])) {
            fileInfo[1] = br.getRegex("<strong>Size:</strong>([^<>\"]+)</li>").getMatch(0);
        }
        fileInfo[0] = removeHostNameFromFilename(fileInfo[0]);
        return fileInfo;
    }

    @Override
    protected String getFnameViaAbuseLink(Browser br, DownloadLink link) throws Exception {
        final String ret = super.getFnameViaAbuseLink(br, link);
        return removeHostNameFromFilename(ret);
    }

    @Override
    public int getMaxSimultaneousFreeAnonymousDownloads() {
        return -1;
    }

    @Override
    public int getMaxSimultaneousFreeAccountDownloads() {
        return -1;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return -1;
    }

    @Override
    public void handleCaptcha(final DownloadLink link, final Browser br, final Form captchaForm) throws Exception {
        if (captchaForm != null && captchaForm.hasInputFieldByName("adblock_detected")) {
            captchaForm.remove("");
            captchaForm.put("adblock_detected", "0");
        }
        super.handleCaptcha(link, br, captchaForm);
    }
}