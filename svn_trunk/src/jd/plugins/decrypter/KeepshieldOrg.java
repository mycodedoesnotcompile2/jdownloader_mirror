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

import org.appwork.utils.StringUtils;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterException;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 52238 $", interfaceVersion = 3, names = {}, urls = {})
public class KeepshieldOrg extends PluginForDecrypt {
    public KeepshieldOrg(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "keepshield.org" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/safe/([a-f0-9]{8,})");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /**
         * TODO: Implement captcha once they've implemented it server side 2025-02-02: Captcha can be chosen when creating a link but link
         * won't have a captcha then -> Website is buggy
         */
        /* Some items are password protected */
        final Form pwform = this.getPasswordForm(br);
        if (pwform != null) {
            final String passCode = getUserInput("Password?", param);
            pwform.put("password", Encoding.urlEncode(passCode));
            br.submitForm(pwform);
            if (this.getPasswordForm(br) != null) {
                throw new DecrypterException(DecrypterException.PASSWORD);
            }
        }
        /* Some items need an additional step/form with a wait time in beforehand. */
        final Form waitform = br.getFormbyProperty("id", "timer-form");
        if (waitform != null) {
            /* 2026-02-02: Wait is skippable */
            final boolean skipWait = true;
            if (!skipWait) {
                final String waitSecondsStr = br.getRegex("seconds = (\\d{1,2})").getMatch(0);
                final int waitSeconds = Integer.parseInt(waitSecondsStr);
                this.sleep(waitSeconds * 1001l, param);
            }
            br.submitForm(waitform);
        }
        String title = br.getRegex("<title>([^<]+) - KeepShield</title>").getMatch(0);
        if (title != null) {
            title = Encoding.htmlDecode(title).trim();
            title = title.replace("Protected Links", "");
        }
        final String[] urls = br.getRegex("data-check-url=\"(http?://[^\"]+)").getColumn(0);
        if (urls == null || urls.length == 0) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final FilePackage fp = FilePackage.getInstance();
        if (!StringUtils.isEmpty(title)) {
            fp.setName(title);
        }
        for (final String url : urls) {
            final DownloadLink link = createDownloadlink(url);
            link._setFilePackage(fp);
            ret.add(link);
        }
        return ret;
    }

    private Form getPasswordForm(final Browser br) {
        return br.getFormbyKey("password_submit");
    }
}
