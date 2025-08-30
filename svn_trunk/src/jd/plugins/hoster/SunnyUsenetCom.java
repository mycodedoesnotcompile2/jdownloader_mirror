package jd.plugins.hoster;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;

import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
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

@HostPlugin(revision = "$Revision: 51406 $", interfaceVersion = 3, names = { "sunnyusenet.com" }, urls = { "" })
public class SunnyUsenetCom extends UseNet {
    public SunnyUsenetCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + getHost() + "/en/packages");
    }

    private final String PROPERTY_ACCOUNT_USENET_USERNAME = "usenetU";

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.USENET, LazyPlugin.FEATURE.USERNAME_IS_EMAIL };
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/en/terms-and-conditions";
    }

    public static interface SunnyUsenetComConfigInterface extends UsenetAccountConfigInterface {
    };

    @Override
    public AccountInfo fetchAccountInfo(Account account) throws Exception {
        setBrowserExclusive();
        final Cookies cookies = account.loadCookies("");
        boolean login_success = false;
        if (cookies != null) {
            br.setCookies(getHost(), cookies);
            br.getPage("https://www." + this.getHost() + "/en");
            br.getPage("/en/login");
            if (this.isLoggedin(br)) {
                logger.info("Cookie login successful");
                login_success = true;
            } else {
                logger.info("Cookie login failed");
                login_success = false;
            }
        }
        if (!login_success) {
            logger.info("Performing full login");
            br.getPage("https://www." + this.getHost() + "/en");
            br.getPage("/en/login");
            final Form loginform = br.getFormbyActionRegex(".*/login");
            if (loginform == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find loginform");
            }
            loginform.put("email", Encoding.urlEncode(account.getUser()));
            loginform.put("password", Encoding.urlEncode(account.getPass()));
            br.submitForm(loginform);
            if (!this.isLoggedin(br)) {
                throw new AccountInvalidException();
            }
            logger.info("Full login successful");
        }
        account.saveCookies(br.getCookies(br.getHost()), "");
        /* Find username required for internal NZB client. */
        String usenetUsername = br.getRegex("User ?Name</[^>]*>\\s*<div\\s*class=\"server-settings__value text-ellipsis\"[^>]*>([^<]+)</div>").getMatch(0);
        if (usenetUsername == null) {
            /* We cannot download without knowing this username! */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find UsenetUsername");
        }
        usenetUsername = Encoding.htmlDecode(usenetUsername).trim();
        account.setProperty(PROPERTY_ACCOUNT_USENET_USERNAME, usenetUsername);
        final String yourPackage = br.getRegex(">\\s*Plan Type\\s*:\\s*</h5>\\s*<div class=\"server-settings__value\"><b\\s*class=\"server-settings__value-b\">([^<>\"]+)</b>").getMatch(0);
        final String expireDate = br.getRegex("(\\d{2} [A-Za-z]+ 20\\d{2})").getMatch(0);
        final String expiresInDays = br.getRegex("class=\"margin-bottom-sm\"[^>]*>\\s*(\\d+)&nbsp;Days").getMatch(0); // Fallback
        final String connectionsStr = br.getRegex(">\\s*Connections\\s*</h5>\\s*<div class=\"server-settings__value\">\\s*(\\d+)").getMatch(0);
        final int packageConnections;
        if (StringUtils.containsIgnoreCase(yourPackage, "UNL")) {
            packageConnections = 20;
        } else if (StringUtils.containsIgnoreCase(yourPackage, "90")) {
            packageConnections = 15;
        } else if (StringUtils.containsIgnoreCase(yourPackage, "60")) {
            packageConnections = 15;
        } else if (StringUtils.containsIgnoreCase(yourPackage, "30")) {
            packageConnections = 10;
        } else if (StringUtils.containsIgnoreCase(yourPackage, "20")) {
            packageConnections = 10;
        } else if (StringUtils.containsIgnoreCase(yourPackage, "10")) {
            packageConnections = 5;
        } else if (StringUtils.containsIgnoreCase(yourPackage, "5")) {
            packageConnections = 5;
        } else {
            packageConnections = 1;
        }
        if (connectionsStr != null) {
            final int connections = Integer.parseInt(connectionsStr);
            if (connections != packageConnections) {
                logger.info("PackageConnections != connections -> DEV possibly update this! Connections: " + connections + " | PackageConnections: " + packageConnections);
            }
            account.setMaxSimultanDownloads(Math.min(connections, packageConnections));
        } else {
            account.setMaxSimultanDownloads(packageConnections);
        }
        final AccountInfo ai = new AccountInfo();
        ai.setStatus("Your package: " + yourPackage);
        /* 2020-02-25: They have packages without expire date e.g. "Sunny UNL" */
        if (expireDate != null) {
            ai.setValidUntil(TimeFormatter.getMilliSeconds(expireDate, "dd MMM yyyy", Locale.ENGLISH) + (24 * 60 * 60 * 1000l));
        } else if (expiresInDays != null) {
            ai.setValidUntil(System.currentTimeMillis() + Long.parseLong(expiresInDays) * 24 * 60 * 60 * 1000 + (24 * 60 * 60 * 1000l));
        } else if (yourPackage == null || !StringUtils.containsIgnoreCase(yourPackage, "UNL")) {
            logger.info("Failed to find expire date && account is not an unlimited account -> Assuming this is an expired account");
            ai.setExpired(true);
        }
        ai.setMultiHostSupport(this, Arrays.asList(new String[] { "usenet" }));
        account.setRefreshTimeout(5 * 60 * 60 * 1000l);
        return ai;
    }

    @Override
    protected String getUseNetUsername(final Account account) {
        return account.getStringProperty(PROPERTY_ACCOUNT_USENET_USERNAME);
    }

    private boolean isLoggedin(final Browser br) {
        return br.containsHTML("/member/logout\"");
    }

    @Override
    public List<UsenetServer> getAvailableUsenetServer() {
        final List<UsenetServer> ret = new ArrayList<UsenetServer>();
        ret.addAll(UsenetServer.createServerList("news.sunnyusenet.com", false, 119, 80));
        ret.addAll(UsenetServer.createServerList("news.sunnyusenet.com", true, 563, 443));
        return ret;
    }
}
