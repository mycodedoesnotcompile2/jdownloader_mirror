package jd.plugins.hoster;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.components.usenet.UsenetAccountConfigInterface;
import org.jdownloader.plugins.components.usenet.UsenetServer;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Cookies;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

@HostPlugin(revision = "$Revision: 50044 $", interfaceVersion = 3, names = { "usenetnow.net" }, urls = { "" })
public class UsenetNow extends UseNet {
    public UsenetNow(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://billing.usenetnow.net/signup");
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.USENET, LazyPlugin.FEATURE.COOKIE_LOGIN_OPTIONAL };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/dmca.html";
    }

    private final String USENET_USERNAME = "USENET_USERNAME";

    @Override
    protected String getUseNetUsername(final Account account) {
        return account.getStringProperty(USENET_USERNAME, account.getUser());
    }

    public static interface UsenetNowConfigInterface extends UsenetAccountConfigInterface {
    };

    @Override
    public AccountInfo fetchAccountInfo(Account account) throws Exception {
        setBrowserExclusive();
        final AccountInfo ai = new AccountInfo();
        final Cookies cookies = account.loadCookies("");
        final Cookies cookiesUser = account.loadUserCookies();
        boolean loginSuccess = false;
        final String page_login = "https://billing.usenetnow.net/login";
        final String page_member = "https://billing.usenetnow.net/member";
        br.getPage(page_login);
        if (cookiesUser != null) {
            br.setCookies(cookiesUser);
            br.getPage(page_member);
            if (isLoggedin(br)) {
                logger.info("User cookie login successful");
                loginSuccess = true;
            } else {
                logger.info("User cookie login failed");
                if (account.hasEverBeenValid()) {
                    throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_expired());
                } else {
                    throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_invalid());
                }
            }
        } else if (cookies != null) {
            br.setCookies(cookies);
            br.getPage(page_member);
            if (isLoggedin(br)) {
                logger.info("Cookie login successful");
                loginSuccess = true;
            } else {
                logger.info("Cookie login failed");
                account.clearCookies("");
                br.clearCookies(null);
            }
        }
        final String account_userName = account.getUser();
        if (!loginSuccess) {
            logger.info("Performing full login");
            br.getPage(page_login);
            final Form loginform = br.getFormbyActionRegex("/login");
            if (loginform == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final boolean captchaAlwaysNeeded = true;
            if (loginform.hasInputFieldByName("g-recaptcha-response") || loginform.containsHTML("recaptcha") || captchaAlwaysNeeded) {
                final String rcKey = br.getRegex("\"recaptchaPublicKey\":\"([^\"]+)").getMatch(0);
                final String recaptchaV2Response;
                if (rcKey != null) {
                    recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br, rcKey).getToken();
                } else {
                    /* Try to auto find reCaptcha key */
                    recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br).getToken();
                }
                loginform.put("g-recaptcha-response: ", Encoding.urlEncode(recaptchaV2Response));
            }
            loginform.put("amember_login", Encoding.urlEncode(account_userName));
            loginform.put("amember_pass", Encoding.urlEncode(account.getPass()));
            loginform.put("_referer", Encoding.urlEncode("https://" + br.getHost() + "/"));
            final Browser brc = br.cloneBrowser();
            brc.getHeaders().put("Accept", "application/json");
            brc.getHeaders().put("Origin", "https://billing." + br.getHost());
            brc.submitForm(loginform);
            final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            if (Boolean.FALSE.equals(entries.get("ok"))) {
                final List<String> errorlist = (List<String>) entries.get("error");
                if (errorlist != null) {
                    /*
                     * 2024-10-30: I'am running into problem "Anti Spam check failed" but dunno why and didn't invest more time into
                     * checking. User can use cookie login.
                     */
                    throw new AccountInvalidException(errorlist.get(0));
                } else {
                    throw new AccountInvalidException();
                }
            }
            /* Login state looks good -> Double-check */
            br.getPage(page_member);
            if (!this.isLoggedin(br)) {
                throw new AccountInvalidException();
            }
        }
        br.getPage("https://billing.usenetnow.net/member");
        account.saveCookies(br.getCookies(br.getHost()), "");
        final String userName = br.getRegex("<div class=\"am-user-identity-block\">(.*?)<a").getMatch(0);
        if (userName != null) {
            account.setProperty(USENET_USERNAME, Encoding.htmlDecode(userName).trim());
        } else if (!account_userName.contains("@")) {
            /* Information which user has entered in username field is not e-mail address -> We can use it as Usenet login credential. */
            account.setProperty(USENET_USERNAME, account_userName);
        } else {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find UseNet username");
        }
        account.setProperty(USENET_USERNAME, Encoding.htmlDecode(userName).trim());
        /**
         * https://billing.usenetnow.net/page/newsreader </br>
         * "You May Use Up To 100 Total Connections To Access These Servers - Average Users Should Use Between 40-50 Connections For Optimal
         * Speeds."
         */
        account.setMaxSimultanDownloads(50);
        final String validUntil = br.getRegex("expires\\s*<[^>]*>(\\d+/\\d+/\\d+)").getMatch(0);
        final String bucketType = br.getRegex("member-subscriptions\">.*?<li><strong>(.*?)</").getMatch(0);
        if (bucketType != null) {
            ai.setStatus("Bucket: " + Encoding.htmlDecode(bucketType).trim());
        }
        if (validUntil != null) {
            final long date = TimeFormatter.getMilliSeconds(validUntil, "MM/dd/yy", Locale.ENGLISH);
            ai.setValidUntil(date + (24 * 60 * 60 * 1000l));
        } else {
            ai.setExpired(true);
        }
        ai.setMultiHostSupport(this, Arrays.asList(new String[] { "usenet" }));
        return ai;
    }

    private boolean isLoggedin(final Browser br) {
        return br.containsHTML("logout\"");
    }

    @Override
    public List<UsenetServer> getAvailableUsenetServer() {
        /* See: https://billing.usenetnow.net/page/newsreader */
        final List<UsenetServer> ret = new ArrayList<UsenetServer>();
        final int[] ports_noSSL = new int[] { 20, 23, 53, 119, 2000, 8080, 9000, 9001, 9002 };
        final int[] ports_SSL = new int[] { 443, 563, 5563 };
        ret.addAll(UsenetServer.createServerList("usnews.usenetnow.net", false, ports_noSSL));
        ret.addAll(UsenetServer.createServerList("usnews.usenetnow.net", true, ports_SSL));
        ret.addAll(UsenetServer.createServerList("eunews.usenetnow.net", false, ports_noSSL));
        ret.addAll(UsenetServer.createServerList("eunews.usenetnow.net", true, ports_SSL));
        ret.addAll(UsenetServer.createServerList("eunews2.usenetnow.net", false, ports_noSSL));
        ret.addAll(UsenetServer.createServerList("eunews2.usenetnow.net", true, ports_SSL));
        return ret;
    }
}
