package jd.plugins.hoster;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.plugins.components.usenet.UsenetAccountConfigInterface;
import org.jdownloader.plugins.components.usenet.UsenetServer;
import org.jdownloader.scripting.JavaScriptEngineFactory;
import org.jdownloader.settings.GraphicalUserInterfaceSettings.SIZEUNIT;
import org.jdownloader.settings.staticreferences.CFG_GUI;

import jd.PluginWrapper;
import jd.controlling.proxy.AbstractProxySelectorImpl;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.Request;
import jd.http.requests.PostRequest;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.components.PluginJSonUtils;

@HostPlugin(revision = "$Revision: 51945 $", interfaceVersion = 3, names = { "usenext.com" }, urls = { "" })
public class UsenextCom extends UseNet {
    public UsenextCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + getHost() + "/signup");
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/terms";
    }

    private final String FLATRATE_DOMAIN = "flat.usenext.de";

    public static interface UsenextConfigInterface extends UsenetAccountConfigInterface {
    };

    @Override
    public void update(final DownloadLink link, final Account account, long bytesTransfered) {
        final UsenetServer server = getLastUsedUsenetServer();
        // If flatrate domain is in use, do not substract traffic from account
        if (FLATRATE_DOMAIN.equals(server.getHost())) {
            return;
        }
        // Substract traffic for non-flatrate domains or when no domain is given
        super.update(link, account, bytesTransfered);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser ret = super.createNewBrowserInstance();
        ret.getHeaders().put(HTTPConstants.HEADER_REQUEST_USER_AGENT, "jd");
        return ret;
    }

    @Override
    public int getMaxSimultanDownload(DownloadLink link, Account account, AbstractProxySelectorImpl proxy) {
        if (account != null) {
            final UsenetAccountConfigInterface config = getAccountJsonConfig(account);
            if (config != null && StringUtils.equalsIgnoreCase(FLATRATE_DOMAIN, config.getHost())) {
                /* Flatrate domain does not use up users' traffic but therefore has a connection limit in place. */
                return 4;
            }
        }
        return super.getMaxSimultanDownload(link, account, proxy);
    }

    private long parseNumber(Map<String, Object> map, String id, long def) throws Exception {
        final Object value = map.get(id);
        if (value == null) {
            if (def == -1) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "not available:" + id);
            } else {
                return def;
            }
        }
        String unit = (String) map.get("unitResourceStringKey");
        if (StringUtils.contains(unit, "GIGA") || StringUtils.equals(unit, "UNX_UNIT_GIGABYTES") || StringUtils.equals(unit, "UNIT_GIGABYTES")) {
            unit = "GB";
        } else if (StringUtils.contains(unit, "KILO") || StringUtils.equals(unit, "UNX_UNIT_KILOBYTES") || StringUtils.equals(unit, "UNIT_KILOBYTES")) {
            unit = "KB";
        } else if (StringUtils.contains(unit, "MEGA") || StringUtils.equals(unit, "UNX_UNIT_MEGABYTES") || StringUtils.equals(unit, "UNIT_MEGABYTES")) {
            unit = "MB";
        } else if (StringUtils.contains(unit, "TERA") || StringUtils.equals(unit, "UNX_UNIT_TERABYTES") || StringUtils.equals(unit, "UNIT_TERABYTES")) {
            unit = "TB";
        } else if (StringUtils.equals(unit, "UNX_UNIT_BYTES") || StringUtils.equals(unit, "UNIT_BYTES")) {
            unit = "B";
        } else {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unsupported unit:" + unit);
        }
        return SizeFormatter.getSize(value + unit);
    }

    private Map<String, Object> queryAPI(final Account account, final Browser br) throws Exception {
        /* At this point login was successful and all that's left to do is to obtain account information. */
        final String api_url = "https://janus.usenext.com";
        final PostRequest postRequest = br.createJSonPostRequest(api_url + "/graphql",
                "{\"query\":\"query Subscription {\\n  radiusData {\\n    volume {\\n      remaining\\n      total\\n      unitResourceStringKey\\n    }\\n    extraBoost {\\n      remaining\\n      total\\n      unitResourceStringKey\\n    }\\n  }\\n  currentServiceRoundUpgradeData {\\n    hasPendingUpgrade\\n    isLastUpgrade\\n    totalServiceRoundUpgradeNumbers\\n    currentServiceRoundUpgradeNumber\\n    accountingPeriod {\\n      remaining\\n      total\\n      unitResourceStringKey\\n    }\\n  }\\n  serviceInformation {\\n    contractCreateDate\\n    hasServiceRoundRenewOrder\\n    currentServiceRound {\\n      startDate\\n      currEndDate\\n      roundNr\\n      article {\\n        id\\n        name\\n        runtimeUnit\\n        runtime\\n        volumeGb\\n        priceNet\\n        priceGross\\n        articleTypeId\\n      }\\n      invoice {\\n        id\\n        uuid\\n        sumGrossPrice\\n        createDate\\n        invoiceStatePaths {\\n          invoiceStateId\\n          isCurrent\\n        }\\n      }\\n    }\\n    currentServiceRoundCalculatedEndDate\\n    nextServiceRoundBeginDate\\n    nextArticle {\\n      id\\n      name\\n      runtime\\n      runtimeUnit\\n      volumeGb\\n      priceNet\\n      priceGross\\n      articleTypeId\\n    }\\n  }\\n  cancellationInformation {\\n    cancellationProcess {\\n      cancellationTypeId\\n      createDate\\n    }\\n    isContractLocked\\n    hasWithdrawableCancellation\\n    isInCancellationPeriod\\n  }\\n}\",\"variables\":{}}");
        postRequest.getHeaders().put("x-ui-language", "en-US");
        postRequest.getHeaders().put("Origin", "https://www." + br.getHost());
        // postRequest.getHeaders().put("Referer", "https://www." + getHost() + "/");
        br.setCurrentURL("https://www." + br.getHost() + "/");
        br.getPage(postRequest);
        if (br.containsHTML("\"AUTH_NOT_AUTHENTICATED\"")) {
            throw new AccountInvalidException();
        }
        synchronized (account) {
            account.saveCookies(br.getCookies(br.getHost()), "");
        }
        final Map<String, Object> json = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        return json;
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        setBrowserExclusive();
        synchronized (account) {
            if (account.getUser() == null || !account.getUser().matches("^avi-\\d+-[a-z0-9]+$")) {
                final String allowed_format_human_readable = "avi-123456-xxxx...";
                final String userLanguage = System.getProperty("user.language");
                String errorMessage;
                if ("de".equalsIgnoreCase(userLanguage)) {
                    errorMessage = "\r\nDer Nutzername muss folgendes Format haben: " + allowed_format_human_readable + "!";
                } else if ("fr".equalsIgnoreCase(userLanguage)) {
                    errorMessage = "\r\nLe nom d'utilisateur doit être au format suivant: " + allowed_format_human_readable + "!";
                } else if ("es".equalsIgnoreCase(userLanguage)) {
                    errorMessage = "\r\nEl nombre de usuario debe tener el siguiente formato: " + allowed_format_human_readable + "!";
                } else if ("it".equalsIgnoreCase(userLanguage)) {
                    errorMessage = "\r\nIl nome utente deve avere il seguente formato: " + allowed_format_human_readable + "!";
                } else if ("nl".equalsIgnoreCase(userLanguage)) {
                    errorMessage = "\r\nDe gebruikersnaam moet het volgende formaat hebben: " + allowed_format_human_readable + "!";
                } else if ("pt".equalsIgnoreCase(userLanguage)) {
                    errorMessage = "\r\nO nome de usuário deve estar no seguinte formato: " + allowed_format_human_readable + "!";
                } else if ("ru".equalsIgnoreCase(userLanguage)) {
                    errorMessage = "\r\nИмя пользователя должно быть в следующем формате: " + allowed_format_human_readable + "!";
                } else if ("ja".equalsIgnoreCase(userLanguage)) {
                    errorMessage = "\r\nユーザー名は次の形式である必要があります: " + allowed_format_human_readable + "!";
                } else if ("zh".equalsIgnoreCase(userLanguage)) {
                    errorMessage = "\r\n用户名必须为以下格式: " + allowed_format_human_readable + "!";
                } else {
                    errorMessage = "\r\nUsername must be in the following format: " + allowed_format_human_readable + "!";
                }
                throw new AccountInvalidException(errorMessage);
            }
            final AccountInfo ai = new AccountInfo();
            br.setFollowRedirects(true);
            final Cookies cookies = account.loadCookies("");
            Map<String, Object> json = null;
            final String dashboardUrlRelative = "/ma/dashboard";
            if (cookies != null) {
                logger.info("Attempting cookie login");
                br.setCookies(cookies);
                br.getPage("https://www." + getHost() + dashboardUrlRelative);
                try {
                    json = queryAPI(account, br);
                    logger.info("Cookie login successful");
                } catch (final PluginException e) {
                    logger.info("Cookie login failed");
                    br.clearCookies(null);
                    account.clearCookies("");
                }
            }
            if (json == null) {
                logger.info("Performing full login");
                br.getPage("https://www." + getHost() + "/signin?returnTo=%2Fma%2Fdashboard");
                final Request req = br.createJSonPostRequest(br.getURL(), "[{\"password\":\"" + PluginJSonUtils.escape(account.getPass()) + "\",\"username\":\"" + PluginJSonUtils.escape(account.getUser()) + "\"},\"/ma/dashboard\"]");
                /* 2025-11-13: Mandatory static value */
                /**
                 * 6742: function (e, t, n) {...var r = (0, n(9976).$) ('5c76fc0dd6c3af6c32ede62d92596a9adb1b943b')
                 *
                 */
                req.getHeaders().put("next-action", "5c76fc0dd6c3af6c32ede62d92596a9adb1b943b");
                br.getPage(req);
                /* Returns http response code 303 on success */
                if (br.getHttpConnection().getResponseCode() == 500) {
                    /* Wrong credentials -> http response code 500 */
                    throw new AccountInvalidException();
                }
                logger.info("Login looks to be successful");
                json = queryAPI(account, br);
            }
            final Map<String, Object> volume = (Map<String, Object>) JavaScriptEngineFactory.walkJson(json, "data/radiusData/volume");
            final long trafficTotal = parseNumber(volume, "total", -1);
            final long trafficRemaining = parseNumber(volume, "remaining", -1);
            final Map<String, Object> extraBoost = (Map<String, Object>) JavaScriptEngineFactory.walkJson(json, "data/radiusData/extraBoost");
            String accountStatusAdditionalText = null;
            if (extraBoost != null) {
                final long boostTotal = parseNumber(extraBoost, "total", -1);
                final long boostRemaining = parseNumber(extraBoost, "remaining", -1);
                ai.setTrafficMax(trafficTotal + boostTotal);
                ai.setTrafficLeft(trafficRemaining + boostRemaining);
                final SIZEUNIT maxSizeUnit = (SIZEUNIT) CFG_GUI.MAX_SIZE_UNIT.getValue();
                accountStatusAdditionalText = "Boost remaining: " + SIZEUNIT.formatValue(maxSizeUnit, boostRemaining) + "/" + SIZEUNIT.formatValue(maxSizeUnit, boostTotal);
            } else {
                ai.setTrafficMax(trafficTotal);
                ai.setTrafficLeft(trafficRemaining);
            }
            final Map<String, Object> currentServiceRound = (Map<String, Object>) JavaScriptEngineFactory.walkJson(json, "data/serviceInformation/currentServiceRound");
            final String currEndDate = (String) currentServiceRound.get("currEndDate");
            final Date expireDate = TimeFormatter.parseDateString(currEndDate);
            String accountStatusPackageText = null;
            if (expireDate != null) {
                ai.setValidUntil(expireDate.getTime());
                accountStatusPackageText = (String) JavaScriptEngineFactory.walkJson(currentServiceRound, "article/name");
                account.setType(AccountType.PREMIUM);
            } else {
                account.setType(AccountType.FREE);
            }
            if (accountStatusPackageText == null) {
                accountStatusPackageText = account.getType().getLabel();
            }
            account.setMaxSimultanDownloads(30);
            account.setRefreshTimeout(2 * 60 * 60 * 1000l);
            ai.setMultiHostSupport(this, Arrays.asList(new String[] { "usenet" }));
            if (accountStatusAdditionalText != null) {
                ai.setStatus(accountStatusPackageText + " | " + accountStatusAdditionalText);
            } else {
                ai.setStatus(accountStatusPackageText);
            }
            return ai;
        }
    }

    @Override
    public List<UsenetServer> getAvailableUsenetServer() {
        /*
         * Current list of servers can be found here: https://www.usenext.com/en-US/support/faq -> See
         * "I have a Mac or Linux computer – is there special software available?"
         */
        final List<UsenetServer> ret = new ArrayList<UsenetServer>();
        ret.addAll(UsenetServer.createServerList("flat.usenext.de", false, 119, 443));// speed limited to 2Mbyte/s
        ret.addAll(UsenetServer.createServerList("flat.usenext.de", true, 563));// speed limited to 2Mbyte/s
        ret.addAll(UsenetServer.createServerList("high.usenext.de", false, 119, 443));// max speed
        ret.addAll(UsenetServer.createServerList("high.usenext.de", true, 563));// max speed
        /**
         * 2023-01-04: Moved entries for news.usenext.de to bottom as their FAQ does not list this entry anymore but it still seems to work.
         * <br>
         * 2025-11-13: Still working.
         */
        ret.addAll(UsenetServer.createServerList("news.usenext.de", false, 119, 443));
        ret.addAll(UsenetServer.createServerList("news.usenext.de", true, 563));
        return ret;
    }
}
