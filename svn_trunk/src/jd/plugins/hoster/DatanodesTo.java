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
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.captcha.v2.CaptchaHosterHelperInterface;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.plugins.components.XFileSharingProBasic;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.parser.html.Form.MethodType;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

@HostPlugin(revision = "$Revision: 51727 $", interfaceVersion = 3, names = {}, urls = {})
public class DatanodesTo extends XFileSharingProBasic {
    public DatanodesTo(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
    }

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info: 2022-12-05: No limits <br />
     * captchatype-info: 2022-12-05: null <br />
     * other:<br />
     */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "datanodes.to" });
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
        return XFileSharingProBasic.buildAnnotationUrls(getPluginDomains());
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        final AccountType type = account != null ? account.getType() : null;
        if (AccountType.FREE.equals(type)) {
            /* Free Account */
            return true;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            /* Premium account */
            return true;
        } else {
            /* Free(anonymous) and unknown account type */
            return true;
        }
    }

    @Override
    public int getMaxChunks(final Account account) {
        final AccountType type = account != null ? account.getType() : null;
        if (AccountType.FREE.equals(type)) {
            /* Free Account */
            return 0;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            /* Premium account */
            return 0;
        } else {
            /* Free(anonymous) and unknown account type */
            return 0;
        }
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
    public Form findFormDownload1Free(final Browser br) throws Exception {
        final Form ret = br == null ? null : br.getFormbyProperty("id", "downloadForm");
        if (ret != null) {
            return ret;
        } else {
            return super.findFormDownload1Free(br);
        }
    }

    @Override
    protected Form findFormDownload2Free(final Browser br) {
        Form form = super.findFormDownload2Free(br);
        if (form == null) {
            /* Special: Form gets added via JS. Assume that if we find the pre download wait, we can return the static form. */
            final String waitStr = regexWaittime(br);
            if (waitStr != null) {
                final String fuid = this.getFUIDFromURL(this.getDownloadLink());
                final String randStr = br.getRegex("rand=\"([^\"]+)\"").getMatch(0);
                form = new Form();
                form.setMethod(MethodType.POST);
                form.put("op", "download2");
                form.put("id", fuid);
                form.put("rand", randStr != null ? Encoding.urlEncode(randStr) : "");
                form.put("referer", Encoding.urlEncode(br.getURL()));
                form.put("method_free", "Free Download >>");
                form.put("method_premium", "");
            }
        }
        if (form != null) {
            form.put("__dl", "1");
        }
        return form;
    }

    @Override
    protected String regexWaittime(final Browser br) {
        final String waitSecondsStr = br.getRegex("countdown=\"(\\d+)\"").getMatch(0);
        if (waitSecondsStr != null) {
            return waitSecondsStr;
        } else {
            return super.regexWaittime(br);
        }
    }

    @Override
    protected void checkErrors(final Browser br, final String html, final DownloadLink link, final Account account) throws NumberFormatException, PluginException {
        if (br.containsHTML(">\\s*Not allowed from domain you")) {
            /* 2023-02-21 */
            throw new PluginException(LinkStatus.ERROR_FATAL, "Not allowed from domain you're coming from");
        }
        super.checkErrors(br, html, link, account);
    }

    @Override
    public Browser prepBrowser(final Browser prepBr, final String host) {
        if (!(this.browserPrepped.containsKey(prepBr) && this.browserPrepped.get(prepBr) == Boolean.TRUE)) {
            super.prepBrowser(prepBr, host);
            /* 2023-02-21: Bypasses their simple "referer protection" */
            prepBr.getHeaders().put("Referer", "https://datanodes.to/users");
        }
        return prepBr;
    }

    @Override
    public void handleCaptcha(final DownloadLink link, Browser br, final Form captchaForm) throws Exception {
        if (captchaForm != null && br.containsHTML("g-recaptcha-response")) {
            final CaptchaHelperHostPluginRecaptchaV2 rc2 = getCaptchaHelperHostPluginRecaptchaV2(this, br);
            logger.info("Detected captcha method \"RecaptchaV2\" type '" + rc2.getType() + "' for this host");
            final CaptchaHosterHelperInterface captchaHelper = rc2;
            this.waitBeforeInteractiveCaptcha(link, rc2.getSolutionTimeout());
            final String captchaResponse = captchaHelper.getToken();
            captchaForm.put("g-recaptcha-response", Encoding.urlEncode(captchaResponse));
        } else {
            super.handleCaptcha(link, br, captchaForm);
        }
    }

    @Override
    public boolean isPremiumOnly(final Browser br) {
        Form download1 = null;
        Form download2 = null;
        try {
            download1 = this.findFormDownload1Free(br);
            download2 = this.findFormDownload2Free(br);
        } catch (final Exception ignore) {
            ignore.printStackTrace();
        }
        if (br.containsHTML("/premium") && (download1 == null && download2 == null)) {
            return true;
        } else {
            return super.isPremiumOnly(br);
        }
    }

    @Override
    protected String getDllink(final DownloadLink link, final Account account, final Browser br, String src) {
        if (src != null && src.startsWith("{")) {
            try {
                /* Parse json response */
                final Map<String, Object> entries = restoreFromString(src, TypeRef.MAP);
                String url = entries.get("url").toString();
                /* Check for encoded value and decode it if needed. */
                if (StringUtils.startsWithCaseInsensitive(url, "https%3A%2F")) {
                    url = Encoding.htmlDecode(url);
                }
                /* url might have newlines in it */
                url = url.replace("\r", "").replace("\n", "");
                return url;
            } catch (final Throwable ignore) {
                logger.log(ignore);
            }
        }
        return super.getDllink(link, account, br, src);
    }
}