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

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;

import jd.PluginWrapper;
import jd.controlling.faviconcontroller.FavIcons;
import jd.http.Browser;
import jd.parser.html.Form;
import jd.parser.html.InputField;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.PluginException;

import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.plugins.components.XFileSharingProBasic;
import org.jdownloader.plugins.components.config.XFSConfigKatfile;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.plugins.controller.LazyPlugin.FEATURE;

@HostPlugin(revision = "$Revision: 53015 $", interfaceVersion = 3, names = {}, urls = {})
public class KatfileCom extends XFileSharingProBasic {
    public KatfileCom(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
    }

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info:2019-03-01: premium untested, set FREE account limits <br />
     * captchatype-info: 2019-03-01: reCaptchaV2<br />
     * other:<br />
     */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "katfile.biz", "katfile.space", "katfile.ws", "katfile.vip", "katfile.online", "katfile.cloud", "katfile.com" });
        return ret;
    }

    @Override
    public Object getFavIcon(String host) throws IOException {
        if (getHost().equals(host)) {
            try {
                // workaround for missing favicon
                final Browser br = this.createNewBrowserInstance();
                final BufferedImage ret = FavIcons.download_FavIconTag(br, "https://katfile.biz/images/logo.png", host);
                return colorize(ret, Color.decode("#0459ab"));
            } catch (Throwable ignore) {
                ignore.printStackTrace();
            }
        }
        return null;
    }

    private static BufferedImage colorize(BufferedImage original, Color color) {
        final BufferedImage ret = new BufferedImage(original.getWidth(), original.getHeight(), BufferedImage.TYPE_INT_RGB);
        Graphics2D g2d = ret.createGraphics();
        g2d.setColor(color);
        g2d.fillRect(0, 0, original.getWidth(), original.getHeight());
        g2d.drawImage(original, 0, 0, null);
        g2d.dispose();
        return ret;
    }

    @Override
    protected FEATURE[] customizeFeatures(Set<FEATURE> features) {
        features.add(FEATURE.FAVICON);
        return features.toArray(new LazyPlugin.FEATURE[0]);
    }

    private String getPremiumPackage(Browser br) {
        return new Regex(getCorrectBR(br), "<TR>\\s*<TD>\\s*(Premium\\s*Package.*?)</TD>\\s*</TR>").getMatch(0);
    }

    @Override
    protected void fetchAccountInfoWebsiteTraffic(final Browser br, final Account account, final AccountInfo ai) throws Exception {
        super.fetchAccountInfoWebsiteTraffic(br, account, ai);
        final String premiumPackage = getPremiumPackage(br);
        final String trafficMax = new Regex(premiumPackage, "<b>\\s*([0-9\\.]+\\s*[TGMB]+)\\s*<").getMatch(0);
        if (trafficMax != null) {
            ai.setTrafficMax(parseSize(Size.TRAFFIC, trafficMax));
        }
    }

    @Override
    protected String[] supportsPreciseExpireDate() {
        return null;
    }

    @Override
    protected Long findExpireTimestamp(final Account account, final Browser br, AtomicBoolean isPreciseTimestampFlag) throws Exception {
        final String premiumPackage = getPremiumPackage(br);
        final String expireStr = new Regex(premiumPackage, "until\\s*(?:<b>)?([\\d]+-[\\w{2}]+-[\\d]+\\s[\\d:]+)</").getMatch(0);
        if (expireStr != null) {
            final long ret = TimeFormatter.getMilliSeconds(expireStr, "yyyy-MM-dd HH:mm:ss", Locale.ENGLISH);
            if (ret > 0) {
                isPreciseTimestampFlag.set(true);
                return ret;
            }
        }
        return super.findExpireTimestamp(account, br, isPreciseTimestampFlag);
    }

    @Override
    public String rewriteHost(final String host) {
        /* 2025-09-09: Main domain changed to katfile.cloud */
        /* 2025-12-08: Main domain changed to katfile.online */
        /* 2026-04-13: Main domain changed to katfile.ws */
        /* 2026-05-xx: Main domain changed to katfile.space */
        /* 2026-07-xx: Main domain changed to katfile.biz */
        return this.rewriteHost(getPluginDomains(), host);
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
    protected boolean supports_lifetime_account() {
        return true;
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        final AccountType type = account != null ? account.getType() : null;
        if (AccountType.FREE.equals(type)) {
            /* Free Account */
            return false;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            /* Premium account */
            return true;
        } else {
            /* Free(anonymous) and unknown account type */
            return false;
        }
    }

    @Override
    public Form findFormDownload1Free(final Browser br) throws Exception {
        final Form download1 = super.findFormDownload1Free(br);
        final boolean formFixRequired = false;
        if (formFixRequired && download1 != null) {
            /* 2022-09-02 - fixed in Form class */
            final String formkey = "method_free";
            final InputField method_free = download1.getInputField(formkey);
            String value = method_free.getValue();
            if (value == null) {
                value = "Start Download";
            }
            download1.remove("method_free");
            download1.put("method_free", value);
        }
        return download1;
    }

    @Override
    public void doFree(final DownloadLink link, final Account account) throws Exception, PluginException {
        if (checkShowFreeDialog(getHost())) {
            showFreeDialog(getHost());
        }
        super.doFree(link, account);
    }

    @Override
    public Form findFormDownload2Premium(final DownloadLink link, final Account account, final Browser br) throws Exception {
        final Form ret = super.findFormDownload2Premium(link, account, br);
        if (ret != null) {
            /* 2022-01-07: Special antiBot handling / captcha in premium mode. */
            handleCaptcha(link, br, ret);
        }
        return ret;
    }

    @Override
    public int getMaxChunks(final Account account) {
        final AccountType type = account != null ? account.getType() : null;
        if (AccountType.FREE.equals(type)) {
            /* Free Account */
            return 1;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            /* Premium account */
            return -2;
        } else {
            /* Free(anonymous) and unknown account type */
            return 1;
        }
    }

    private int getMaxDownloadSelect() {
        return get(this.getConfigInterface()).getMaxSimultaneousFreeDownloads();
    }

    @Override
    public int getMaxSimultaneousFreeAnonymousDownloads() {
        return getMaxDownloadSelect();
    }

    @Override
    public int getMaxSimultaneousFreeAccountDownloads() {
        return getMaxDownloadSelect();
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return -1;
    }

    @Override
    protected boolean supportsAPIMassLinkcheck() {
        return looksLikeValidAPIKey(this.getAPIKey());
    }

    @Override
    protected boolean supportsAPISingleLinkcheck() {
        return looksLikeValidAPIKey(this.getAPIKey());
    }

    @Override
    protected boolean isOffline(final DownloadLink link, final Browser br) {
        if (br.containsHTML("/404-remove|>\\s*The file expired|>\\s*The file was deleted by its owner")) {
            return true;
        } else {
            return super.isOffline(link, br);
        }
    }

    // @Override
    // protected boolean isPremiumOnlyURL(final Browser br) {
    // final String url = br != null ? br.getURL() : null;
    // if (url == null) {
    // return false;
    // } else if (StringUtils.containsIgnoreCase(url, "/?op=registration&redirect=")) {
    // return true;
    // } else {
    // return super.isPremiumOnlyURL(br);
    // }
    // }

    @Override
    protected String getPremiumOnlyErrorMessage(final Browser br) {
        if (br.containsHTML(">\\s*This file is available for Premium")) {
            return "This file is available for Premium";
        } else if (StringUtils.containsIgnoreCase(br.getURL(), "/?op=registration&redirect=")) {
            return "Account required to download this file";
        } else {
            return super.getPremiumOnlyErrorMessage(br);
        }
    }

    @Override
    protected String regexWaittime(final String html) {
        final String waitStr = new Regex(html, "var estimated_time = (\\d+)").getMatch(0);
        if (waitStr != null) {
            /* Small hack: These aren't seconds but tenths of a second */
            final int realSeconds = Integer.parseInt(waitStr) / 10;
            return Integer.toString(realSeconds);
        }
        return super.regexWaittime(html);
    }

    @Override
    public Class<? extends XFSConfigKatfile> getConfigInterface() {
        return XFSConfigKatfile.class;
    }
}