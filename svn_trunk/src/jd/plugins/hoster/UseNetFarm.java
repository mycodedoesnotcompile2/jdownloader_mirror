package jd.plugins.hoster;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import javax.swing.JComponent;
import javax.swing.JLabel;

import org.appwork.storage.TypeRef;
import org.appwork.swing.MigPanel;
import org.appwork.swing.components.ExtTextField;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.gui.InputChangedCallbackInterface;
import org.jdownloader.plugins.accounts.AccountBuilderInterface;
import org.jdownloader.plugins.components.usenet.UsenetAccountConfigInterface;
import org.jdownloader.plugins.components.usenet.UsenetServer;

import jd.PluginWrapper;
import jd.http.Cookies;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

@HostPlugin(revision = "$Revision: 50770 $", interfaceVersion = 3, names = { "usenet.farm" }, urls = { "" })
public class UseNetFarm extends UseNet {
    public UseNetFarm(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://usenet.farm/");
    }

    @Override
    public String getAGBLink() {
        return "https://usenet.farm/";
    }

    public static interface UseNetFarmConfigInterface extends UsenetAccountConfigInterface {
    };

    private final String USENET_USERNAME = "USENET_USERNAME";
    private final String USENET_PASSWORD = "USENET_PASSWORD";

    @Override
    protected String getUseNetUsername(Account account) {
        return account.getStringProperty(USENET_USERNAME, account.getUser());
    }

    @Override
    protected String getUseNetPassword(Account account) {
        return account.getStringProperty(USENET_PASSWORD, account.getUser());
    }

    private final Number getNumber(Map<String, Object> map, final String key) {
        final Object value = map.get(key);
        if (value instanceof Number) {
            return (Number) value;
        } else if (value instanceof String) {
            return Long.parseLong(value.toString());
        } else {
            return null;
        }
    }

    @Override
    public AccountBuilderInterface getAccountFactory(InputChangedCallbackInterface callback) {
        return new UseNetFarmAccountFactory(callback);
    }

    public static class UseNetFarmAccountFactory extends MigPanel implements AccountBuilderInterface {
        /**
         *
         */
        private static final long serialVersionUID = 1L;
        private final String      help             = "Enter the login url provided by Usenet.Farm via e-mail";

        public boolean updateAccount(Account input, Account output) {
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

        private String getURL() {
            if (help.equals(this.url.getText())) {
                return null;
            }
            return this.url.getText();
        }

        private final ExtTextField url;

        public UseNetFarmAccountFactory(final InputChangedCallbackInterface callback) {
            super("ins 0, wrap 2", "[][grow,fill]", "");
            add(new JLabel("Usenet.Farm login url:"));
            add(this.url = new ExtTextField() {
                @Override
                public void onChanged() {
                    callback.onChangedInput(this);
                }
            });
            url.setHelpText(help);
        }

        @Override
        public JComponent getComponent() {
            return this;
        }

        @Override
        public void setAccount(Account defaultAccount) {
            if (defaultAccount != null) {
                url.setText(defaultAccount.getUser());
            }
        }

        @Override
        public boolean validateInputs() {
            final String url = getURL();
            return url != null && StringUtils.containsIgnoreCase(url, "uuid") && StringUtils.startsWithCaseInsensitive(url, "http");
        }

        @Override
        public Account getAccount() {
            return new Account(null, getURL());
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(Account account) throws Exception {
        setBrowserExclusive();
        final AccountInfo ai = new AccountInfo();
        br.setFollowRedirects(false);
        final Cookies cookies = account.loadCookies("");
        if (cookies != null) {
            br.setCookies(cookies);
            br.getPage("https://usenet.farm/action/auth/state");
            final Map<String, Object> state = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            if (state == null || !Boolean.TRUE.equals(state.get("state"))) {
                account.clearCookies("");
            }
        }
        if (br.getCookie(getHost(), "sessid") == null) {
            account.clearCookies("");
            if (!StringUtils.containsIgnoreCase(account.getPass(), "uuid") || !StringUtils.startsWithCaseInsensitive(account.getPass(), "http")) {
                throw new PluginException(LinkStatus.ERROR_PREMIUM, "Please enter the complete login URL!\r\nIf you are using JD in Headless mode, enter your login URL in the username & password fields.", PluginException.VALUE_ID_PREMIUM_DISABLE);
            }
            // login url
            br.setAllowedResponseCodes(400);// Bad Request on invalid Login
            br.getPage(account.getPass());
            if (br.getCookie(getHost(), "sessid") == null) {
                throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_DISABLE);
            }
            br.getPage("https://usenet.farm/action/auth/state");
            final Map<String, Object> state = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            if (state == null || !Boolean.TRUE.equals(state.get("state"))) {
                throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_DISABLE);
            }
        }
        account.saveCookies(br.getCookies(br.getHost()), "");
        br.getPage("https://usenet.farm/action/notify/dash");
        final Map<String, Object> dash = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        if (dash != null) {
            final String email = (String) dash.get("email");
            if (email != null) {
                account.setUser(email);
            }
        }
        br.getPage("https://usenet.farm/action/payment/list");
        final Map<String, Object> list = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        if (list != null) {
            if (Boolean.TRUE.equals(list.get("lifetime"))) {
                account.setType(AccountType.LIFETIME);
            } else {
                account.setType(AccountType.PREMIUM);
                final List<Map<String, Object>> lines = (List<Map<String, Object>>) list.get("lines");
                final Map<String, Object> current = lines.get(0);
                final String date_end = (String) current.get("date_end");
                final long date = TimeFormatter.getMilliSeconds(date_end, "yyyy'-'MM'-'dd", null);
                if (date > 0) {
                    ai.setValidUntil(date + (24 * 60 * 60 * 1000l));
                }
            }
            br.getPage("https://usenet.farm/action/metric/usage");
            final Map<String, Object> usage = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            if (usage != null) {
                final Number total = getNumber(usage, "total");
                final Number remain = getNumber(usage, "remain");
                if (total != null && remain != null) {
                    ai.setTrafficMax(total.longValue());
                    ai.setTrafficLeft(remain.longValue());
                } else if (remain != null) {
                    ai.setTrafficLeft(remain.longValue());
                }
            }
            br.getPage("https://usenet.farm/action/config/userpass");
            final Map<String, Object> userpass = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            if (userpass != null) {
                final String user = (String) userpass.get("user");
                final String pass = (String) userpass.get("pass");
                if (user != null && pass != null) {
                    account.setProperty(USENET_USERNAME, user);
                    account.setProperty(USENET_PASSWORD, pass);
                    final Number conns = getNumber(userpass, "conns");
                    if (conns != null) {
                        account.setConcurrentUsePossible(true);
                        account.setMaxSimultanDownloads(conns.intValue());
                    }
                    ai.setMultiHostSupport(this, Arrays.asList(new String[] { "usenet" }));
                    account.setRefreshTimeout(2 * 60 * 60 * 1000l);
                    return ai;
                }
            }
        }
        throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_DISABLE);
    }

    @Override
    public List<UsenetServer> getAvailableUsenetServer() {
        final List<UsenetServer> ret = new ArrayList<UsenetServer>();
        ret.addAll(UsenetServer.createServerList("news4.usenet.farm", false, 119));
        ret.addAll(UsenetServer.createServerList("news.usenet.farm", false, 119));
        ret.addAll(UsenetServer.createServerList("news.usenetfarm.eu", false, 119));
        ret.addAll(UsenetServer.createServerList("news4.usenet.farm", true, 443, 563));
        ret.addAll(UsenetServer.createServerList("news.usenet.farm", true, 443, 563));
        ret.addAll(UsenetServer.createServerList("news.usenetfarm.eu", true, 443, 563));
        return ret;
    }
}
