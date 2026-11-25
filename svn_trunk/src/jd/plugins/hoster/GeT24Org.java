package jd.plugins.hoster;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;
import java.util.concurrent.TimeUnit;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Hash;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountRequiredException;
import jd.plugins.AccountUnavailableException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.MultiHostHost;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.MultiHosterManagement;

@HostPlugin(revision = "$Revision: 41665 $", interfaceVersion = 3, names = { "get24.org" }, urls = { "" })
public class GeT24Org extends PluginForHost {
    private static final String          VERSION = "0.0.3";
    private static MultiHosterManagement mhm     = new MultiHosterManagement("get24.org");

    public GeT24Org(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(getBaseURL() + "/pricing");
    }

    private String getBaseURL() {
        return "https://" + getHost();
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        br.getHeaders().put("User-Agent", "Jdownloader " + VERSION);
        return br;
    }

    @Override
    public String getAGBLink() {
        return getBaseURL() + "/terms";
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST, LazyPlugin.FEATURE.USERNAME_IS_EMAIL };
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        /* Chunk limit the API gave us for the stored directurl, default: 1 */
        final int apiChunks = link.getIntegerProperty(getMaxChunksProperty(), 1);
        /* Negative value = "up to x chunks" */
        return apiChunks > 1 ? -apiChunks : 1;
    }

    private String getMaxChunksProperty() {
        return this.getHost() + "maxchunks";
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return 1;
    }

    @Override
    public AvailableStatus requestFileInformation(DownloadLink parameter) throws Exception {
        return AvailableStatus.UNCHECKABLE;
    }

    @Override
    public void handleFree(DownloadLink link) throws Exception {
        throw new AccountRequiredException();
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        if (StringUtils.isEmpty(account.getUser()) || StringUtils.isEmpty(account.getPass())) {
            throw new AccountInvalidException("Wrong login or password");
        }
        final String postData = "email=" + Encoding.urlEncode(account.getUser()) + "&passwd_sha256=" + Hash.getSHA256(account.getPass());
        final Map<String, Object> entries = postApi("/api/login", postData, account, null);
        final AccountInfo ai = new AccountInfo();
        final String plan = entries.get("plan").toString();
        if ("free".equalsIgnoreCase(plan)) {
            account.setType(AccountType.FREE);
        } else {
            account.setType(AccountType.PREMIUM);
            ai.setValidUntil(parseUtcTimestamp(entries.get("date_expire").toString()));
        }
        ai.setStatus("Plan: " + plan);
        Object leftRaw = entries.get("transfer_left_gib");
        if (leftRaw == null) {
            leftRaw = entries.get("transfer_left");
        }
        Object maxRaw = entries.get("transfer_max_gib");
        if (maxRaw == null) {
            maxRaw = entries.get("transfer_max");
        }
        final Number leftObj = (Number) leftRaw;
        final Number maxObj = (Number) maxRaw;
        final double transferLeftGib = Math.max(0.0d, leftObj.doubleValue());
        final double transferMaxGib = Math.max(0.0d, maxObj.doubleValue());
        ai.setTrafficLeft((long) (transferLeftGib * 1024d * 1024d * 1024d));
        ai.setTrafficMax((long) (transferMaxGib * 1024d * 1024d * 1024d));
        account.setMaxSimultanDownloads(3);
        account.setConcurrentUsePossible(true);
        br.getPage(getBaseURL() + "/api/hosts/enabled");
        final List<Object> rawHosts;
        try {
            rawHosts = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.LIST);
        } catch (final JSonMapperException | ClassCastException e) {
            throw new AccountUnavailableException(e, "Invalid hosts list response", TimeUnit.MINUTES.toMillis(5));
        }
        if (rawHosts == null) {
            throw new AccountUnavailableException("Invalid hosts list response", TimeUnit.MINUTES.toMillis(5));
        }
        final List<MultiHostHost> supportedHosts = new ArrayList<MultiHostHost>();
        for (final Object hostObj : rawHosts) {
            if (hostObj != null) {
                supportedHosts.add(new MultiHostHost(hostObj.toString()));
            }
        }
        ai.setMultiHostSupportV2(this, supportedHosts);
        return ai;
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        final String directurlproperty = this.getHost() + "directurl";
        final String storedDirecturl = link.getStringProperty(directurlproperty);
        String directurl = null;
        if (storedDirecturl != null) {
            logger.info("Re-using stored directurl: " + storedDirecturl);
            try {
                dl = jd.plugins.BrowserAdapter.openDownload(this.br, link, storedDirecturl, this.isResumeable(link, account), this.getMaxChunks(link, account));
                if (this.looksLikeDownloadableContent(dl.getConnection())) {
                    directurl = storedDirecturl;
                } else {
                    br.followConnection(true);
                    dl = null;
                    link.removeProperty(directurlproperty);
                }
            } catch (final Exception e) {
                if (e instanceof InterruptedException) {
                    throw e;
                }
                logger.log(e);
                dl = null;
                link.removeProperty(directurlproperty);
            }
        }
        if (directurl == null) {
            final String targetUrl = link.getDefaultPlugin().buildExternalDownloadURL(link, this);
            final String postData = "email=" + Encoding.urlEncode(account.getUser()) + "&passwd_sha256=" + Hash.getSHA256(account.getPass()) + "&link=" + Encoding.urlEncode(targetUrl);
            final Map<String, Object> entries = postApi("/api/debrid/geturl", postData, account, link);
            final Object urlObj = entries.get("url");
            directurl = urlObj != null ? urlObj.toString() : null;
            if (StringUtils.isEmpty(directurl)) {
                mhm.handleErrorGeneric(account, link, "Failed to find final downloadurl", 5, TimeUnit.MINUTES.toMillis(5));
                /* Unreachable code */
                throw new WTFException();
            }
            /* Optional field per API: fall back to plugin default if absent */
            link.removeProperty(getMaxChunksProperty());
            final Object maxChunksObj = entries.get("max_chunks");
            if (maxChunksObj instanceof Number) {
                final int apiChunks = ((Number) maxChunksObj).intValue();
                link.setProperty(getMaxChunksProperty(), apiChunks);
            }
            dl = jd.plugins.BrowserAdapter.openDownload(this.br, link, directurl, this.isResumeable(link, account), this.getMaxChunks(link, account));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                mhm.handleErrorGeneric(account, link, "Final downloadlink did not lead to file", 5, TimeUnit.MINUTES.toMillis(5));
                /* Unreachable code */
                throw new WTFException();
            }
            link.setProperty(directurlproperty, directurl);
        }
        dl.startDownload();
    }

    private Map<String, Object> postApi(final String path, final String postData, final Account account, final DownloadLink link) throws Exception {
        br.setAllowedResponseCodes(new int[] { 400, 403, 429 });
        try {
            br.postPage(getBaseURL() + path, postData);
        } finally {
            br.setAllowedResponseCodes(new int[0]);
        }
        final Map<String, Object> entries;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException | ClassCastException e) {
            if (link == null) {
                throw new AccountUnavailableException(e, "Invalid API response", TimeUnit.MINUTES.toMillis(5));
            }
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Invalid API response", TimeUnit.MINUTES.toMillis(5), e);
        }
        if (entries == null) {
            if (link == null) {
                throw new AccountUnavailableException("Invalid API response", TimeUnit.MINUTES.toMillis(5));
            }
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Invalid API response", TimeUnit.MINUTES.toMillis(5));
        }
        if (Boolean.TRUE.equals(entries.get("ok"))) {
            return entries;
        }
        handleApiError(entries, account, link);
        /* Unreachable code */
        throw new WTFException();
    }

    private void handleApiError(final Map<String, Object> entries, final Account account, final DownloadLink link) throws PluginException, InterruptedException {
        /*
         * TODO: Ask the API developer for error codes or string enums instead of human readable (possibly localized) error strings.
         * Matching on these strings breaks as soon as a text is reworded or translated.
         */
        /* reason may be absent on unexpected server errors */
        final Object reasonObj = entries.get("reason");
        final String reason = reasonObj != null ? reasonObj.toString() : "unknown error";
        switch (reason) {
        case "invalid credentials":
        case "missing credentials":
            throw new AccountInvalidException("Wrong login or password");
        case "user not activated":
            throw new AccountInvalidException("Email not activated");
        case "too many failed login attempts, please try again later":
        case "api login access blocked for your ip, use browser or contact support":
            throw new AccountUnavailableException(reason, TimeUnit.MINUTES.toMillis(10));
        case "no transfer":
            throw new AccountUnavailableException("Daily limit exceeded", TimeUnit.MINUTES.toMillis(5));
        case "daily request limit exceeded":
            throw new AccountUnavailableException("Daily request limit exceeded", TimeUnit.MINUTES.toMillis(15));
        case "premium required":
        case "premiumX required":
        case "host daily limit exceeded":
        case "host not supported":
        case "host disabled":
            if (link != null) {
                mhm.putError(account, link, TimeUnit.MINUTES.toMillis(5), reason);
                /* Unreachable code */
                throw new WTFException();
            }
            throw new AccountUnavailableException(reason, TimeUnit.MINUTES.toMillis(5));
        case "wrong url":
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Wrong URL", TimeUnit.MINUTES.toMillis(5));
        case "file removed":
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "File offline according to multihoster");
        default:
            if (link != null) {
                mhm.handleErrorGeneric(account, link, reason, 5, TimeUnit.MINUTES.toMillis(5));
                /* Unreachable code */
                throw new WTFException();
            }
            throw new AccountUnavailableException(reason, TimeUnit.MINUTES.toMillis(5));
        }
    }

    private long parseUtcTimestamp(final String rawDate) throws ParseException {
        /*
         * TODO: A date without time ("yyyy-MM-dd") is parsed as 00:00 UTC of that day, so the account expires one day too early. Probably
         * add one day for this format.
         */
        final String[] patterns = new String[] { "yyyy-MM-dd HH:mm:ss", "yyyy-MM-dd" };
        for (final String pattern : patterns) {
            try {
                final SimpleDateFormat sdf = new SimpleDateFormat(pattern, Locale.ENGLISH);
                sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
                sdf.setLenient(false);
                return sdf.parse(rawDate.trim()).getTime();
            } catch (final ParseException ignore) {
            }
        }
        throw new ParseException("Unparseable date_expire: " + rawDate, 0);
    }
}
