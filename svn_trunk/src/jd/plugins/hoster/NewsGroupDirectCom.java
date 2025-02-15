package jd.plugins.hoster;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.List;

import org.appwork.uio.CloseReason;
import org.appwork.uio.UIOManager;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.net.usenet.InvalidAuthException;
import org.jdownloader.gui.dialog.AskDownloadPasswordDialogInterface;
import org.jdownloader.gui.dialog.AskForDownloadLinkDialog;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.components.usenet.UsenetAccountConfigInterface;
import org.jdownloader.plugins.components.usenet.UsenetServer;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Cookies;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

@HostPlugin(revision = "$Revision: 49941 $", interfaceVersion = 3, names = { "newsgroupdirect.com" }, urls = { "" })
public class NewsGroupDirectCom extends UseNet {
    public NewsGroupDirectCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://newsgroupdirect.com/#pricing");
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.USENET, LazyPlugin.FEATURE.USERNAME_IS_EMAIL };
    }

    @Override
    public String getAGBLink() {
        return "https://newsgroupdirect.com/terms-of-service";
    }

    public static interface NewsGroupDirectComConfigInterface extends UsenetAccountConfigInterface {
    };

    private final String USENET_USERNAME = "USENET_USERNAME";
    private final String USENET_PASSWORD = "USENET_PASSWORD";

    @Override
    protected String getUseNetUsername(Account account) {
        return account.getStringProperty(USENET_USERNAME, account.getUser());
    }

    @Override
    protected String getUseNetPassword(Account account) {
        return account.getStringProperty(USENET_PASSWORD, account.getPass());
    }

    @Override
    public AccountInfo fetchAccountInfo(Account account) throws Exception {
        synchronized (account) {
            if (!account.getUser().matches(".+@.+\\..+")) {
                if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                    throw new PluginException(LinkStatus.ERROR_PREMIUM, "\r\nBitte gib deine E-Mail Adresse ins Benutzername Feld ein!", PluginException.VALUE_ID_PREMIUM_DISABLE);
                } else {
                    throw new PluginException(LinkStatus.ERROR_PREMIUM, "\r\nPlease enter your e-mail address in the username field!", PluginException.VALUE_ID_PREMIUM_DISABLE);
                }
            }
            setBrowserExclusive();
            final AccountInfo ai = new AccountInfo();
            br.setFollowRedirects(true);
            final Cookies cookies = account.loadCookies("");
            try {
                boolean freshLogin = true;
                if (cookies != null) {
                    br.setCookies(getHost(), cookies);
                    br.getPage("https://newsgroupdirect.com/member");
                    final Form login = br.getFormbyActionRegex("/login");
                    if (login != null && login.containsHTML("email") && login.containsHTML("login_password")) {
                        freshLogin = true;
                    } else if (!StringUtils.endsWithCaseInsensitive(br.getURL(), "/member")) {
                        freshLogin = true;
                    } else {
                        freshLogin = false;
                    }
                }
                if (freshLogin) {
                    account.clearCookies("");
                    final String userName = account.getUser();
                    br.getPage("https://newsgroupdirect.com/login-form");
                    Form login = br.getFormbyActionRegex("/login");
                    if (login == null) {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                    login.put("email", Encoding.urlEncode(userName));
                    login.put("login_password", Encoding.urlEncode(account.getPass()));
                    br.submitForm(login);
                    login = br.getFormbyActionRegex("/login");
                    if (login != null && login.containsHTML("email") && login.containsHTML("login_password")) {
                        throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_DISABLE);
                    } else if (!StringUtils.endsWithCaseInsensitive(br.getURL(), "/member")) {
                        throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_DISABLE);
                    }
                }
                account.saveCookies(br.getCookies(getHost()), "");
                final String username = br.getRegex("Username\\s*</th>\\s*<td>\\s*([^>]+)\\s*<").getMatch(0);
                if (StringUtils.isEmpty(username)) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                } else {
                    account.setProperty(USENET_USERNAME, username);
                }
                final String trafficLeft = br.getRegex("<p>\\s*You have\\s*<strong>\\s*(.*?\\s*[TGKB]+)\\s*</strong>\\s*left").getMatch(0);
                final String billedAgain = br.getRegex("You'll\\s*be\\s*billed\\s*again\\s*on\\s*(\\d+/\\d+/\\d+)").getMatch(0);
                final String ngd = br.getRegex("NGD\\s*Member\\s*</th>\\s*<td>\\s*(.*?)\\s*</").getMatch(0);
                final String currentPlan = br.getRegex("Current Plan\\s*</th>\\s*<td>\\s*(.*?)\\s*</td>").getMatch(0);
                if (StringUtils.equalsIgnoreCase(trafficLeft, "Unlimited")) {
                    // monthly unlimited
                    ai.setUnlimitedTraffic();
                    if (currentPlan != null) {
                        ai.setStatus(currentPlan);
                    } else {
                        ai.setStatus("Unknown unlimited monthly plan");
                    }
                    boolean expireDateSupported = false;
                    if (ngd != null) {
                        final String days = new Regex(ngd, "(\\d+)\\s*days").getMatch(0);
                        if (days != null) {
                            expireDateSupported = true;
                            ai.setValidUntil(System.currentTimeMillis() + (Integer.parseInt(days) * 24 * 60 * 60 * 1000l));
                        } else {
                            final String hours = new Regex(ngd, "(\\d+)\\s*hours").getMatch(0);
                            if (hours != null) {
                                expireDateSupported = true;
                                ai.setValidUntil(System.currentTimeMillis() + (Integer.parseInt(hours) * 60 * 60 * 1000l));
                            }
                        }
                    }
                    if ((expireDateSupported == false || ngd == null) && billedAgain != null) {
                        final String month = new Regex(billedAgain, "(\\d+)/(\\d+)/(\\d+)").getMatch(0);
                        final String day = new Regex(billedAgain, "(\\d+)/(\\d+)/(\\d+)").getMatch(1);
                        final String year = new Regex(billedAgain, "(\\d+)/(\\d+)/(\\d+)").getMatch(2);
                        final Calendar calendar = Calendar.getInstance();
                        calendar.set(Calendar.MONTH, Integer.parseInt(month) - 1);
                        calendar.set(Calendar.DAY_OF_MONTH, Integer.parseInt(day));
                        calendar.set(Calendar.YEAR, Integer.parseInt("20" + year));
                        ai.setValidUntil(calendar.getTimeInMillis());
                        expireDateSupported = true;
                    }
                    if (expireDateSupported == false) {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                } else if (trafficLeft != null) {
                    ai.setUnlimitedTraffic();
                    // block accounts
                    ai.setTrafficLeft(SizeFormatter.getSize(trafficLeft));
                    if (currentPlan != null) {
                        ai.setStatus(currentPlan);
                    } else {
                        ai.setStatus("Unknown block plan");
                    }
                } else {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                // https://newsgroupdirect.com/#pricing
                account.setMaxSimultanDownloads(50);
                account.setRefreshTimeout(5 * 60 * 60 * 1000l);
                ai.setMultiHostSupport(this, Arrays.asList(new String[] { "usenet" }));
                try {
                    verifyUseNetLogins(account);
                    return ai;
                } catch (final InvalidAuthException e) {
                    logger.log(e);
                    final DownloadLink dummyLink = new DownloadLink(this, "Account:" + getUseNetUsername(account), getHost(), "https://newsgroupdirect.com", true);
                    final AskDownloadPasswordDialogInterface handle = UIOManager.I().show(AskDownloadPasswordDialogInterface.class, new AskForDownloadLinkDialog(_GUI.T.AskForPasswordDialog_AskForPasswordDialog_title_(), "Please enter your newsgroupdirect Usenet Password", dummyLink));
                    if (handle.getCloseReason() == CloseReason.OK) {
                        final String password = handle.getText();
                        if (StringUtils.isNotEmpty(password)) {
                            account.setProperty(USENET_PASSWORD, password);
                            try {
                                verifyUseNetLogins(account);
                                return ai;
                            } catch (InvalidAuthException e2) {
                                logger.log(e2);
                            }
                        }
                    }
                }
                throw new PluginException(LinkStatus.ERROR_PREMIUM, null, PluginException.VALUE_ID_PREMIUM_DISABLE);
            } catch (final PluginException e) {
                if (e.getLinkStatus() == LinkStatus.ERROR_PREMIUM) {
                    account.clearCookies("");
                    account.removeProperty(USENET_PASSWORD);
                }
                throw e;
            }
        }
    }

    @Override
    public List<UsenetServer> getAvailableUsenetServer() {
        final List<UsenetServer> ret = new ArrayList<UsenetServer>();
        // https://newsgroupdirect.com/server-addresses
        ret.addAll(UsenetServer.createServerList("news.newsgroupdirect.com", false, 119, 23, 443, 3128, 7000, 8000, 9000));
        ret.addAll(UsenetServer.createServerList("news.newsgroupdirect.com", true, 563, 80, 81));
        ret.addAll(UsenetServer.createServerList("europe.newsgroupdirect.com", false, 119, 23, 443, 3128, 7000, 8000, 9000));
        ret.addAll(UsenetServer.createServerList("europe.newsgroupdirect.com", true, 563, 80, 81));
        return ret;
    }
}
