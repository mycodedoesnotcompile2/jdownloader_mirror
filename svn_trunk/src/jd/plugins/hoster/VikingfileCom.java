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

import java.awt.Color;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import javax.swing.JComponent;
import javax.swing.JLabel;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.TypeRef;
import org.appwork.swing.MigPanel;
import org.appwork.swing.components.ExtPasswordField;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CaptchaHelperHostPluginCloudflareTurnstile;
import org.jdownloader.gui.InputChangedCallbackInterface;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.accounts.AccountBuilderInterface;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.gui.swing.components.linkbutton.JLink;
import jd.http.Browser;
import jd.http.Cookies;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.DefaultEditAccountPanelAPIKeyLogin;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 54057 $", interfaceVersion = 3, names = {}, urls = {})
public class VikingfileCom extends PluginForHost {
    public VikingfileCom(PluginWrapper wrapper) {
        super(wrapper);
        /* Premium URL is only exposed in the IDE, see getBuyPremiumUrl(). */
        this.enablePremium();
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        /* Login via "Login Hash" which is handled like an API key (no username/password). */
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.API_KEY_LOGIN };
    }

    @Override
    public String getBuyPremiumUrl() {
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            /* 2026-11-18: TODO: Test with premium logins/"login hash", then remove this IDE-only check. */
            return "https://" + getHost() + "/premium";
        } else {
            return super.getBuyPremiumUrl();
        }
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/terms";
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "vikingfile.com", "vikingf1le.us.to", "vik1ngfile.site" });
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

    private static final Pattern PATTERN_FILE = Pattern.compile("/f/([A-Za-z0-9]{10})(?![A-Za-z0-9])", Pattern.CASE_INSENSITIVE);

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + PATTERN_FILE.pattern());
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            return this.getHost() + "://" + fid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), PATTERN_FILE).getMatch(0);
    }

    @Override
    protected String getDefaultFileName(final DownloadLink link) {
        return this.getFID(link);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 0;
    }

    /** Docs: https://vikingfile.com/api */
    private String getApiBase() {
        return "https://" + getHost() + "/api";
    }

    @Override
    public boolean checkLinks(final DownloadLink[] urls) {
        if (urls == null || urls.length == 0) {
            return false;
        }
        try {
            checkLinksAPI(urls);
        } catch (final Exception e) {
            logger.log(e);
            return false;
        }
        return true;
    }

    /** Checks the given links via API. Exceptions are passed through to the caller. */
    @SuppressWarnings("unchecked")
    private void checkLinksAPI(final DownloadLink[] urls) throws Exception {
        final StringBuilder sb = new StringBuilder();
        final List<DownloadLink> links = new ArrayList<DownloadLink>();
        int index = 0;
        while (true) {
            links.clear();
            while (true) {
                /* We check up to 100 links at once */
                if (index == urls.length || links.size() == 100) {
                    break;
                } else {
                    links.add(urls[index]);
                    index++;
                }
            }
            sb.setLength(0);
            for (final DownloadLink link : links) {
                if (sb.length() > 0) {
                    sb.append("&");
                }
                sb.append("hash[]=" + this.getFID(link));
            }
            br.postPageRaw(getApiBase() + "/check-file", sb.toString());
            final List<Map<String, Object>> items = (List<Map<String, Object>>) restoreFromString(br.getRequest().getHtmlCode(), TypeRef.OBJECT);
            if (items == null || items.size() != links.size()) {
                /* This should never happen */
                throw new WTFException();
            }
            for (int i = 0; i < links.size(); i++) {
                final DownloadLink link = links.get(i);
                final Map<String, Object> data = items.get(i);
                if (Boolean.TRUE.equals(data.get("exist"))) {
                    link.setAvailable(true);
                    link.setFinalFileName(data.get("name").toString());
                    link.setVerifiedFileSize(((Number) data.get("size")).longValue());
                } else {
                    /* Deleted file or invalid fileID. */
                    link.setAvailable(false);
                }
            }
            if (index == urls.length) {
                break;
            }
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return requestFileInformationAPI(link);
    }

    public AvailableStatus requestFileInformationAPI(final DownloadLink link) throws Exception {
        checkLinksAPI(new DownloadLink[] { link });
        if (!link.isAvailable()) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else {
            return AvailableStatus.TRUE;
        }
    }

    public AvailableStatus requestFileInformationWebsite(final DownloadLink link) throws IOException, PluginException {
        /* Do not clear cookies here: They may contain the login session of an account. */
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String filename = br.getRegex("id=\"filename\"[^>]*>([^<]+)<").getMatch(0);
        if (filename != null) {
            filename = Encoding.htmlDecode(filename).trim();
            link.setName(filename);
        } else {
            logger.warning("Failed to find filename");
        }
        final String filesize = br.getRegex("id=\"size\"[^>]*>([^<]+)<").getMatch(0);
        if (filesize != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesize));
        } else {
            logger.warning("Failed to find filesize");
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link, null);
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        /* Hash accounts are treated as free accounts for now. */
        login(account, false);
        handleDownload(link, account);
    }

    public void handleDownload(final DownloadLink link, final Account account) throws Exception, PluginException {
        final String directlinkproperty = "directurl_" + (account != null ? account.getType().getLabel() : "free");
        final String storedDirecturl = link.getStringProperty(directlinkproperty);
        final String dllink;
        if (storedDirecturl != null) {
            logger.info("Re-using stored directurl: " + storedDirecturl);
            dllink = storedDirecturl;
        } else {
            /* Availability was already checked via API by the core before. Only the website is needed for the captcha sitekey. */
            requestFileInformationWebsite(link);
            final String cfkey = br.getRegex("sitekey\\s*:\\s*'([^']+)'").getMatch(0);
            if (cfkey == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final CaptchaHelperHostPluginCloudflareTurnstile ts = new CaptchaHelperHostPluginCloudflareTurnstile(this, br, cfkey);
            final String cfTurnstileResponse = ts.getToken();
            final UrlQuery query = new UrlQuery();
            query.appendEncoded("cf-turnstile-response", cfTurnstileResponse);
            br.postPage(br.getURL(), query);
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            /* "link" is missing in error responses (e.g. captcha rejected) */
            final Object linkO = entries.get("link");
            if (linkO == null || StringUtils.isEmpty(linkO.toString())) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find directurl: " + entries);
            }
            dllink = linkO.toString();
        }
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, account), this.getMaxChunks(link, account));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                if (dl.getConnection().getResponseCode() == 403) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 60 * 60 * 1000l);
                } else if (dl.getConnection().getResponseCode() == 404) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 60 * 60 * 1000l);
                }
                try {
                    br.followConnection(true);
                } catch (final IOException e) {
                    logger.log(e);
                }
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        } catch (final InterruptedException e) {
            throw e;
        } catch (final Exception e) {
            if (storedDirecturl != null) {
                link.removeProperty(directlinkproperty);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Stored directurl expired", e);
            } else {
                throw e;
            }
        }
        if (storedDirecturl == null) {
            link.setProperty(directlinkproperty, dl.getConnection().getURL().toExternalForm());
        }
        dl.startDownload();
    }

    /**
     * Logs in via "Login Hash". Reuses stored cookies if possible. A full login requires a Cloudflare Turnstile captcha.
     *
     * @return true if a fresh login/cookie validation was performed, false if cookies were only set without validation.
     */
    public boolean login(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            br.setFollowRedirects(true);
            br.setCookiesExclusive(true);
            final Cookies cookies = account.loadCookies("");
            if (cookies != null) {
                logger.info("Attempting cookie login");
                br.setCookies(getHost(), cookies);
                if (!force) {
                    /* Do not validate cookies */
                    return false;
                }
                br.getPage("https://" + getHost() + "/account");
                if (isLoggedin(br)) {
                    logger.info("Cookie login successful");
                    /* Refresh cookie timestamp */
                    account.saveCookies(br.getCookies(getHost()), "");
                    return true;
                }
                logger.info("Cookie login failed");
                br.clearCookies(getHost());
            }
            logger.info("Performing full login");
            br.getPage("https://" + getHost() + "/login");
            final Form loginform = br.getFormbyKey("hash");
            if (loginform == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find loginform");
            }
            final String cfkey = br.getRegex("sitekey\\s*:\\s*'([^']+)'").getMatch(0);
            if (cfkey == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find Turnstile sitekey");
            }
            final CaptchaHelperHostPluginCloudflareTurnstile ts = new CaptchaHelperHostPluginCloudflareTurnstile(this, br, cfkey);
            loginform.put("hash", Encoding.urlEncode(account.getPass()));
            loginform.put("cf-turnstile-response", Encoding.urlEncode(ts.getToken()));
            br.submitForm(loginform);
            if (!isLoggedin(br)) {
                if (br.containsHTML(">\\s*Wrong hash")) {
                    throw new AccountInvalidException("Invalid login hash");
                }
                throw new AccountInvalidException("Invalid login hash or captcha rejected");
            }
            account.saveCookies(br.getCookies(getHost()), "");
            return true;
        }
    }

    private boolean isLoggedin(final Browser br) {
        /* The "Logout" link is only displayed for logged in users. */
        return br.containsHTML("href=\"/logout\"");
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        login(account, true);
        br.getPage("https://" + getHost() + "/account");
        /*
         * Premium account structure is unknown at the moment: All valid login hashes are handled as free accounts.
         */
        account.setType(AccountType.FREE);
        ai.setUnlimitedTraffic();
        return ai;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final Account acc) {
        return true;
    }

    @Override
    protected String getAPILoginHelpURL() {
        return "https://" + getHost() + "/login";
    }

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        return str != null && str.matches("[a-zA-Z0-9]{5,}");
    }

    @Override
    public AccountBuilderInterface getAccountFactory(final InputChangedCallbackInterface callback) {
        return new VikingfileAccountPanel(callback, this);
    }

    /** Login mask for the "Login Hash". The hash is stored in username and password. */
    public static class VikingfileAccountPanel extends MigPanel implements AccountBuilderInterface {
        private static final long      serialVersionUID = 1L;
        private final ExtPasswordField pass;
        private final JLabel           idLabel;
        private final VikingfileCom    plg;

        public VikingfileAccountPanel(final InputChangedCallbackInterface callback, final VikingfileCom plg) {
            super("ins 0, wrap 2", "[][grow,fill]", "");
            this.plg = plg;
            add(new JLabel(_GUI.T.jd_gui_swing_components_AccountDialog_generic_instructions()));
            add(new JLink(_GUI.T.jd_gui_swing_components_AccountDialog_generic_instructions_click_here_for_instructions(), plg.getAPILoginHelpURL()));
            add(this.idLabel = new JLink("<HTML><U>Login Hash</U></HTML>", plg.getAPILoginHelpURL()));
            add(this.pass = new ExtPasswordField() {
                private static final long serialVersionUID = 1L;

                @Override
                public void onChanged() {
                    callback.onChangedInput(pass);
                }
            }, "");
            pass.setHelpText("Login Hash");
        }

        private String getHash() {
            final String hash = new String(pass.getPassword());
            return plg.updateAccountPassword(new Account(null, hash), hash);
        }

        @Override
        public boolean validateInputs() {
            if (plg.looksLikeValidAPIKey(getHash())) {
                idLabel.setForeground(Color.BLACK);
                return true;
            } else {
                idLabel.setForeground(Color.RED);
                return false;
            }
        }

        @Override
        public Account getAccount() {
            final String hash = getHash();
            return new Account(hash, hash);
        }

        @Override
        public void setAccount(final Account defaultAccount) {
            pass.setText(defaultAccount.getPass());
        }

        @Override
        public boolean updateAccount(final Account input, final Account output) {
            boolean changed = false;
            if (!StringUtils.equals(input.getUser(), output.getUser())) {
                output.setUser(input.getUser());
                changed = true;
            }
            if (!StringUtils.equals(input.getPass(), output.getPass())) {
                output.setPass(input.getPass());
                changed = true;
            }
            return changed;
        }

        @Override
        public boolean handleClipboardAutoFill() {
            return DefaultEditAccountPanelAPIKeyLogin.handleClipboardAutoFill(pass, null, plg);
        }

        @Override
        public JComponent getComponent() {
            return this;
        }
    }
}