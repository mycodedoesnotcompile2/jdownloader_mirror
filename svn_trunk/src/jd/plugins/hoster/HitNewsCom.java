package jd.plugins.hoster;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.Request;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.parser.html.InputField;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

import org.appwork.storage.TypeRef;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.plugins.components.usenet.UsenetAccountConfigInterface;
import org.jdownloader.plugins.components.usenet.UsenetServer;

@HostPlugin(revision = "$Revision: 51041 $", interfaceVersion = 3, names = { "hitnews.com" }, urls = { "" })
public class HitNewsCom extends UseNet {
    public HitNewsCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://member.hitnews.com/signup.php");
    }

    public static interface HitNewsConfigInterface extends UsenetAccountConfigInterface {
    };

    @Override
    public String getAGBLink() {
        return "http://www.hitnews.com/index.php?id=41";
    }

    private AccountInfo parseAccountInfo(Browser br, Account account) throws Exception {
        br.getPage("https://my.hitnews.com/action.php?req=auth/status&lang=en");
        final Request request = br.getRequest();
        final Map<String, Object> response = restoreFromString(request.getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> user = (Map<String, Object>) response.get("user");
        final Map<String, Object> active = (Map<String, Object>) response.get("active");
        if (user == null) {
            throw new AccountInvalidException();
        } else if (active == null) {
            throw new AccountInvalidException();
        }
        account.saveCookies(br.getCookies(br.getHost()), "");
        final Number conns = (Number) user.get("conns");
        final Number bytes = (Number) user.get("bytes");
        account.setMaxSimultanDownloads(conns.intValue());
        account.setType(AccountType.PREMIUM);
        account.setUser(user.get("login").toString());
        final AccountInfo ai = new AccountInfo();
        if (bytes != null) {
            ai.setTrafficLeft(bytes.longValue());
            ai.setTrafficMax(bytes.longValue());
        }
        final String expire_date = active.get("expire_date").toString();
        if ("Lifetime".equals(expire_date)) {
            ai.setValidUntil(-1);
        } else {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unsupported account expire date \"" + expire_date + "\", please contact JDownloader support!");
        }
        final String prodtype = (String) active.get("prodtype");
        final String prod = (String) active.get("prod");
        if ("block".equals(prodtype)) {
            final long maxTraffic = SizeFormatter.getSize(prod, false);
            if (maxTraffic != -1) {
                ai.setTrafficMax(maxTraffic);
            } else if ("25gb".equals(prod)) {
                ai.setTrafficMax(25 * 1000 * 1000 * 1000l);
            } else if ("500gb".equals(prod)) {
                ai.setTrafficMax(500 * 1000 * 1000 * 1000l);
            } else if ("1000gb".equals(prod)) {
                ai.setTrafficMax(1000 * 1000 * 1000 * 1000l);
            } else if ("2000gb".equals(prod)) {
                ai.setTrafficMax(2000 * 1000 * 1000 * 1000);
            } else if ("5000gb".equals(prod)) {
                ai.setTrafficMax(5000 * 1000 * 1000 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unsupported product \"" + prod + "\", please contact JDownloader support!");
            }
            ai.setStatus("Block " + prod);
        } else {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unsupported product type \"" + prodtype + "\", please contact JDownloader support!");
        }
        ai.setMultiHostSupport(this, Arrays.asList(new String[] { "usenet" }));
        return ai;
    }

    @Override
    public AccountInfo fetchAccountInfo(Account account) throws Exception {
        setBrowserExclusive();
        br.setFollowRedirects(true);
        final Cookies cookies = account.loadCookies("");
        if (cookies != null) {
            br.setCookies(getHost(), cookies);
            try {
                return parseAccountInfo(br, account);
            } catch (PluginException e) {
                logger.log(e);
                br.getCookies(getHost()).clear();
            }
        }
        if (br.getCookie(getHost(), "sess") == null) {
            account.clearCookies("");
            br.getPage("https://member.hitnews.com/login.html");
            Form loginForm = br.getFormbyActionRegex("auth/login");
            InputField login = loginForm.getInputField("login");
            String minlength = login.getProperty("minlength", null);
            if (minlength != null && account.getUser().length() < Integer.parseInt(minlength)) {
                throw new AccountInvalidException("Please use username with at least " + minlength + " characters");
            }
            login.setValue(Encoding.urlEncode(account.getUser()));
            InputField pass = loginForm.getInputField("pass");
            minlength = pass.getProperty("minlength", null);
            if (minlength != null && account.getPass().length() < Integer.parseInt(minlength)) {
                throw new AccountInvalidException("Please use password with at least " + minlength + " characters");
            }
            pass.setValue(Encoding.urlEncode(account.getPass()));
            br.submitForm(loginForm);
            loginForm = br.getFormbyActionRegex("auth/login");
            if (loginForm != null && loginForm.containsHTML("login") && loginForm.containsHTML("pass")) {
                final String errmsg = br.getRegex("class\\s*=\\s*\"am-errors\">\\s*<li>\\s*(.*?)\\s*</li>").getMatch(0);
                if (errmsg != null) {
                    throw new PluginException(LinkStatus.ERROR_PREMIUM, errmsg, PluginException.VALUE_ID_PREMIUM_DISABLE);
                } else {
                    throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_DISABLE);
                }
            } else if (br.getCookie(getHost(), "sess", Cookies.NOTDELETEDPATTERN) == null) {
                throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_DISABLE);
            }
        }
        return parseAccountInfo(br, account);
    }

    @Override
    public List<UsenetServer> getAvailableUsenetServer() {
        final List<UsenetServer> ret = new ArrayList<UsenetServer>();
        ret.addAll(UsenetServer.createServerList("news.hitnews.com", false, 119));
        ret.addAll(UsenetServer.createServerList("news.hitnews.com", true, 563, 443));
        ret.addAll(UsenetServer.createServerList("ssl.hitnews.com", true, 563, 443));
        return ret;
    }
}
