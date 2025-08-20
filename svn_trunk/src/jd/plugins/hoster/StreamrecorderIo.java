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
package jd.plugins.hoster;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountRequiredException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51343 $", interfaceVersion = 3, names = {}, urls = {})
public class StreamrecorderIo extends PluginForHost {
    public StreamrecorderIo(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/purchase/plan");
    }

    public static final String PROPERTY_record_id                   = "record_id";
    public static final String PROPERTY_clip_id                     = "clip_id";
    public static final String PROPERTY_title                       = "title";
    public static final String PROPERTY_streamcategory              = "streamcategory";
    public static final String PROPERTY_recorded_at                 = "recorded_at";
    public static final String PROPERTY_downloaded_according_to_api = "downloaded_according_to_api";
    public static final String PROPERTY_duration_seconds            = "duration_seconds";
    public static final String PROPERTY_user_id                     = "user_id";
    public static final String PROPERTY_username                    = "username";
    public static final String PROPERTY_video_resolution_height     = "video_resolution_height";
    public static final String PROPERTY_downloadable_via_account_id = "downloadable_via_account_id";
    public static final String PROPERTY_directurl                   = "directurl";
    public static final String PROPERTY_type                        = "type";
    public static final String PROPERTY_TYPE_RECORDING              = "recording";
    public static final String PROPERTY_TYPE_CLIP                   = "clip";

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.COOKIE_LOGIN_OPTIONAL };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/";
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "streamrecorder.io" });
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
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            /* No regex as items get added via crawler plugin plus there are no individual links to items available. */
            ret.add("");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getPluginContentURL(DownloadLink link) {
        final String type = link.getStringProperty(PROPERTY_type, null);
        if (PROPERTY_TYPE_CLIP.equals(type)) {
            return "https://streamrecorder.io/clip/" + getClipID(link);
        }
        if (PROPERTY_TYPE_RECORDING.equals(type)) {
            return "https://streamrecorder.io/userrecordings/recording/" + getRecordID(link);
        }
        return super.getPluginContentURL(link);
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String type = link.getStringProperty(PROPERTY_type, null);
        if (PROPERTY_TYPE_CLIP.equals(type)) {
            return this.getHost() + "://clip/" + getClipID(link);
        }
        if (PROPERTY_TYPE_RECORDING.equals(type)) {
            return this.getHost() + "://recording/" + getRecordID(link) + "/" + link.getStringProperty(PROPERTY_video_resolution_height);
        }
        return super.getLinkID(link);
    }

    @Override
    public String getMirrorID(final DownloadLink link) {
        return getLinkID(link);
    }

    private String getRecordID(final DownloadLink link) {
        return link.getStringProperty(PROPERTY_record_id);
    }

    private String getClipID(final DownloadLink link) {
        return link.getStringProperty(PROPERTY_clip_id);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 0;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        return AvailableStatus.UNCHECKABLE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        throw new AccountRequiredException();
    }

    public void handleDownload(final DownloadLink link, final Account account) throws Exception, PluginException {
        final String dllink = link.getStringProperty(PROPERTY_directurl);
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
        handleConnectionErrors(br, dl.getConnection());
        dl.startDownload();
    }

    @Override
    protected void throwFinalConnectionException(Browser br, URLConnectionAdapter con) throws PluginException, IOException {
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    public boolean login(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            br.setFollowRedirects(true);
            br.setCookiesExclusive(true);
            final Cookies cookies = account.loadCookies("");
            final Cookies userCookies = account.loadUserCookies();
            final String url_relative_profilesettings = "/profilesettings";
            if (cookies != null || userCookies != null) {
                logger.info("Attempting cookie login");
                if (userCookies != null) {
                    br.setCookies(userCookies);
                } else {
                    br.setCookies(cookies);
                }
                if (!force) {
                    /* Don't validate cookies */
                    return false;
                }
                br.getPage("https://" + this.getHost() + url_relative_profilesettings);
                if (get2FAForm(br) == null && this.isLoggedin(br)) {
                    logger.info("Cookie login successful");
                    /* Refresh cookie timestamp */
                    account.saveCookies(br.getCookies(br.getHost()), "");
                    return true;
                } else {
                    logger.info("Cookie login failed");
                    br.clearCookies(null);
                    if (userCookies != null) {
                        /* Dead-end */
                        if (account.hasEverBeenValid()) {
                            throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_expired());
                        } else {
                            throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_invalid());
                        }
                    }
                }
            }
            logger.info("Performing full login");
            br.getPage("https://" + this.getHost() + "/login");
            final Form loginform = br.getFormbyActionRegex(".*login");
            if (loginform == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find loginform");
            }
            loginform.put("username", Encoding.urlEncode(account.getUser()));
            loginform.put("password", Encoding.urlEncode(account.getPass()));
            br.submitForm(loginform);
            final Form twoFAForm = br.getFormbyActionRegex(".*check-totp");
            if (twoFAForm != null) {
                logger.info("2FA code required");
                final String twoFACode = this.getTwoFACode(account, "\\d{6}");
                logger.info("Submitting 2FA code");
                twoFAForm.put("code", twoFACode);
                br.submitForm(twoFAForm);
                if (get2FAForm(br) != null || !isLoggedin(br)) {
                    throw new AccountInvalidException("Invalid TOTP code");
                }
            } else if (!isLoggedin(br)) {
                throw new AccountInvalidException();
            }
            br.getPage(url_relative_profilesettings);
            account.saveCookies(br.getCookies(br.getHost()), "");
            return true;
        }
    }

    private Form get2FAForm(final Browser br) {
        return br.getFormbyActionRegex(".*check-totp");
    }

    private boolean isLoggedin(final Browser br) {
        return br.containsHTML("/logout\"");
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        login(account, true);
        String expire = br.getRegex("Premium until (\\d{4}-\\d{2}-\\d{2})").getMatch(0);
        if (expire == null) {
            expire = br.getRegex("Premium account active until (\\d{4}-\\d{2}-\\d{2})").getMatch(0);
        }
        if (expire == null) {
            throw new AccountInvalidException("Free accounts are not supported");
        }
        final AccountInfo ai = new AccountInfo();
        ai.setValidUntil(TimeFormatter.getMilliSeconds(expire, "yyyy-MM-dd", Locale.ENGLISH));
        ai.setUnlimitedTraffic();
        account.setType(AccountType.PREMIUM);
        account.setConcurrentUsePossible(true);
        return ai;
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        this.handleDownload(link, account);
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final Account acc) {
        return false;
    }

    private String getDownloadableVia(final DownloadLink dl) {
        return dl.getStringProperty(PROPERTY_downloadable_via_account_id, null);
    }

    @Override
    public boolean canHandle(final DownloadLink link, final Account account) throws Exception {
        if (account == null) {
            /* Without account its not possible to download any link from this host. */
            return false;
        }
        final String account_username = account.getUser();
        final String account_username_from_which_url_was_added = getDownloadableVia(link);
        if (!StringUtils.equals(account_username_from_which_url_was_added, account_username)) {
            /* Link is from another account -> Users can only download the items they "recorded" with their account. */
            return false;
        }
        return super.canHandle(link, account);
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}