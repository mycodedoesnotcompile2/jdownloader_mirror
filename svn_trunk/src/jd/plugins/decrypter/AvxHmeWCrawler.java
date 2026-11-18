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
package jd.plugins.decrypter;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.jdownloader.captcha.v2.challenge.hcaptcha.CaptchaHelperCrawlerPluginHCaptcha;
import org.jdownloader.plugins.components.config.AvxHmeWConfig;
import org.jdownloader.plugins.config.PluginJsonConfig;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.Request;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.parser.html.Form.MethodType;
import jd.parser.html.HTMLParser;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.PluginForHost;

/**
 * @author typek_pb
 */
@DecrypterPlugin(revision = "$Revision: 54027 $", interfaceVersion = 2, names = {}, urls = {})
public class AvxHmeWCrawler extends PluginForDecrypt {
    @SuppressWarnings("deprecation")
    public AvxHmeWCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        /* Always add current domain to first position! */
        /* List of more/current domains: https://avaxhome-mirrors.pw/index.html */
        ret.add(new String[] { "avxhome.st", "xsava.xyz", "avxhm.in", "zavat.pw", "avh.world", "avaxhome.ws", "avaxhome.bz", "avaxhome.cc", "avaxhome.in", "avaxhome.pro", "avaxho.me", "avaxhm.com", "avxhm.is", "avxhm.se", "avxhome.se", "avxhome.in", "avxde.org", "avxlive.icu" });
        return ret;
    }

    protected List<String> getDeadDomains() {
        final ArrayList<String> deadDomains = new ArrayList<String>();
        deadDomains.add("avh.world");
        deadDomains.add("avaxhome.ws");
        deadDomains.add("avaxhome.bz");
        deadDomains.add("avaxhome.cc");
        deadDomains.add("avaxhome.in");
        deadDomains.add("avaxhome.pro");
        deadDomains.add("avaxho.me");
        deadDomains.add("avaxhm.com");
        deadDomains.add("avxhm.is");
        deadDomains.add("avxhm.se");
        deadDomains.add("avxhome.se");
        deadDomains.add("avxhome.in");
        deadDomains.add("avxde.org");
        deadDomains.add("avxlive.icu");
        return deadDomains;
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

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            String pattern = "https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/";
            pattern += "(";
            pattern += PATTERN_REDIRECT_STR;
            pattern += "|" + PATTERN_RELEASE_STR;
            pattern += "|" + PATTERN_RELEASE_OVERVIEW_STR;
            pattern += ")";
            ret.add(pattern);
        }
        return ret.toArray(new String[0]);
    }

    private static final String            PATTERN_REDIRECT_STR         = "go/([a-f0-9]{32}/\\d+/?|[A-Za-z0-9\\-_:%]+/?|\\d+/[^\"]+)";
    private static final Pattern           PATTERN_REDIRECT             = Pattern.compile("(?i)https?://[^/]+/" + PATTERN_REDIRECT_STR);
    private static final String            PATTERN_RELEASE_STR          = "(ebooks|music|software|video|magazines|newspapers|games|graphics|misc|hraphile|comics|audiobooks|girls|tvseries|anime|sheet_music|musik|fernsehserie|zeitschriften|zeitungen|grafik|madchen|horbucher)/(?!facet/).+";
    private static final Pattern           PATTERN_RELEASE              = Pattern.compile("(?i)https?://[^/]+/" + PATTERN_RELEASE_STR);
    private static final String            PATTERN_RELEASE_OVERVIEW_STR = "m/(\\d+)/.*";
    private static final Pattern           PATTERN_RELEASE_OVERVIEW     = Pattern.compile("(?i)https?://[^/]+/" + PATTERN_RELEASE_OVERVIEW_STR);
    private static AtomicLong              LAST_DIRECT                  = new AtomicLong(-1);
    /*
     * "dltoken" returned by /api/download/ after a successfully solved captcha. The website stores it in the localStorage and sends it
     * instead of a captcha for the next links. It is refreshed with every response.
     */
    private static final String            PROPERTY_CSRFTOKEN           = "csrftoken";
    private static AtomicReference<String> CACHED_DLTOKEN               = new AtomicReference<String>();

    @Override
    public int getMaxConcurrentProcessingInstances() {
        return 1;
    }

    @SuppressWarnings("deprecation")
    @Override
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        br.setAllowedResponseCodes(new int[] { 401 });
        /* 2021-03-11: Do not replace hosts inside URLs anymore as this can lead to wrong redirectURLs breaking the crawling process. */
        // final String parameter = param.toString().replaceFirst("(?i)" + Regex.escape(Browser.getHost(param.toString())), this.getHost());
        if (new Regex(param.getCryptedUrl(), PATTERN_REDIRECT).patternFind()) {
            /* 2021-01-20: Login whenever possible -> No captchas required then */
            final Account acc = AccountController.getInstance().getValidAccount("avxhm.se");
            if (acc != null) {
                /* Logged in users can often skip the captchas but sometimes they will be asked to solve captchas too! */
                final PluginForHost hostPlugin = this.getNewPluginForHostInstance("avxhm.se");
                ((jd.plugins.hoster.AvxHmeW) hostPlugin).login(acc, false);
            }
            br.setFollowRedirects(false);
            /* 2022-01-18: Some tests */
            // br.getHeaders().put("sec-ch-ua", "\" Not;A Brand\";v=\"99\", \"Google Chrome\";v=\"97\", \"Chromium\";v=\"97\"");
            // br.getHeaders().put("sec-ch-ua-mobile", "?0");
            // br.getHeaders().put("sec-ch-ua-platform", "\"Windows\"");
            // br.getHeaders().put("Upgrade-Insecure-Requests", "1");
            // br.getHeaders().put("Accept",
            // "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9");
            // br.getHeaders().put("Sec-Fetch-Site", "same-origin");
            // br.getHeaders().put("Sec-Fetch-Mode", "navigate");
            // br.getHeaders().put("Sec-Fetch-User", "?1");
            // br.getHeaders().put("Sec-Fetch-Dest", "document");
            // br.getHeaders().put("Accept-Language", "de-DE,de;q=0.9,en;q=0.8,en-US;q=0.7");
            synchronized (LAST_DIRECT) {
                /* Wait 10 seconds between crawling single "direct" URLs to try to avoid captchas. */
                final long last = LAST_DIRECT.get();
                final int userDefinedWaitSecondsBetweenDirectlinkCrawlProcesses = PluginJsonConfig.get(AvxHmeWConfig.class).getDirectLinkCrawlerWaitSecondsBetweenLinks();
                final long waitMillis = (userDefinedWaitSecondsBetweenDirectlinkCrawlProcesses * 1000) - (Time.systemIndependentCurrentJVMTimeMillis() - last);
                if (waitMillis > 0) {
                    logger.info("Sleep millis until next 'direct' url will be processed: " + waitMillis);
                    sleep(waitMillis, param);
                }
                br.getPage(param.getCryptedUrl());
                followInternalRedirects();
                LAST_DIRECT.set(Time.systemIndependentCurrentJVMTimeMillis());
            }
            String link = br.getRedirectLocation();
            if (link == null) {
                boolean captchaError = false;
                int counter = 0;
                do {
                    counter++;
                    logger.info("Attempt " + counter);
                    /*
                     * 2026-11-17: The captcha page contains no <form>. The hCaptcha is rendered via JavaScript and its token is sent via
                     * fetch() to /api/download/ together with "link_data". The response is JSON: {"redirect":true,"url":"..."} or
                     * {"captcha":true} if the token was not accepted.
                     */
                    final String linkData = br.getRegex("append\\(\\s*'link_data'\\s*,\\s*\"([^\"]+)\"").getMatch(0);
                    if (linkData == null) {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find link_data");
                    }
                    final String siteURL = br.getURL("/").toString();
                    final String captchaPageURL = br.getURL();
                    /*
                     * The captcha page itself does not set the "csrftoken" cookie but the API request needs it (Django CSRF: cookie and
                     * X-CSRFToken header must match, else HTTP 403). Do this before the captcha is solved so the captcha token does not
                     * expire meanwhile.
                     */
                    String csrf = br.getCookie(br.getHost(), "csrftoken", Cookies.NOTDELETEDPATTERN);
                    if (StringUtils.isEmpty(csrf)) {
                        /* Token which was stored on the link by the crawler of the release page -> Use it as cookie and header. */
                        final DownloadLink sourceLink = param.getDownloadLink();
                        final String storedCsrf = sourceLink != null ? sourceLink.getStringProperty(PROPERTY_CSRFTOKEN) : null;
                        if (!StringUtils.isEmpty(storedCsrf)) {
                            logger.info("Using stored csrftoken");
                            csrf = storedCsrf;
                            setCsrfCookie(csrf);
                        }
                    }
                    if (StringUtils.isEmpty(csrf)) {
                        /* A real browser already has the cookie from other pages -> Get it from the mainpage. */
                        logger.info("csrftoken missing -> Trying to obtain it from the mainpage");
                        final Browser csrfBrowser = br.cloneBrowser();
                        csrfBrowser.setFollowRedirects(true);
                        csrfBrowser.getPage(siteURL);
                        csrf = csrfBrowser.getCookie(csrfBrowser.getHost(), "csrftoken", Cookies.NOTDELETEDPATTERN);
                        if (StringUtils.isEmpty(csrf)) {
                            csrf = csrfBrowser.getRegex("name='csrfmiddlewaretoken' value='([^']+)'").getMatch(0);
                        }
                        if (StringUtils.isEmpty(csrf)) {
                            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find csrftoken");
                        }
                        setCsrfCookie(csrf);
                    }
                    /* Build the form which would normally be sent by the JavaScript code. */
                    final Form apiForm = new Form();
                    apiForm.setMethod(MethodType.POST);
                    apiForm.setAction("/api/download/");
                    apiForm.setEncoding("multipart/form-data");
                    apiForm.put("link_data", linkData);
                    final String cachedDltoken = CACHED_DLTOKEN.get();
                    if (cachedDltoken != null) {
                        /* Token from a previously solved captcha -> No captcha needed */
                        logger.info("Trying cached dltoken");
                        apiForm.put("dltoken", cachedDltoken);
                    } else {
                        final String siteKey = br.getRegex("sitekey\\s*:\\s*['\"]([a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12})['\"]").getMatch(0);
                        final String hCaptchaResponse = new CaptchaHelperCrawlerPluginHCaptcha(this, br, siteKey) {
                            protected String getSiteUrl() {
                                /* Being logged in can result in auto redirect/no captcha */
                                return siteURL;
                            };
                        }.getToken();
                        apiForm.put("hcaptcha_token", hCaptchaResponse);
                    }
                    /* Django also checks Referer/Origin on HTTPS -> Use the captcha page as Referer like the browser does. */
                    br.getHeaders().put("X-CSRFToken", csrf);
                    br.getHeaders().put("Referer", captchaPageURL);
                    br.getHeaders().put("Origin", siteURL.replaceFirst("/$", ""));
                    try {
                        br.submitForm(apiForm);
                    } finally {
                        br.getHeaders().remove("X-CSRFToken");
                        br.getHeaders().remove("Referer");
                        br.getHeaders().remove("Origin");
                    }
                    final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                    final String newDltoken = (String) entries.get("dltoken");
                    if (newDltoken != null) {
                        /* Same as the website: Store token so next time we should be able to proceed without captcha. */
                        CACHED_DLTOKEN.set(newDltoken);
                    }
                    if (Boolean.TRUE.equals(entries.get("redirect"))) {
                        LAST_DIRECT.set(Time.systemIndependentCurrentJVMTimeMillis());
                        link = entries.get("url").toString();
                        captchaError = false;
                    } else {
                        /* Captcha/dltoken was not accepted -> Remove token, reload page and try again (with captcha). */
                        logger.info("Captcha or dltoken failed");
                        captchaError = true;
                        CACHED_DLTOKEN.set(null);
                        br.getPage(param.getCryptedUrl());
                        followInternalRedirects();
                        link = br.getRedirectLocation();
                    }
                } while (link == null && captchaError && counter <= 3);
                if (link == null && captchaError) {
                    /* This should never happen */
                    throw new PluginException(LinkStatus.ERROR_CAPTCHA);
                }
            }
            if (link == null || this.canHandle(link)) {
                /* Item offline or plugin broken. */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            ret.add(createDownloadlink(link));
        } else if (new Regex(param.getCryptedUrl(), PATTERN_RELEASE).patternFind()) {
            br.setFollowRedirects(true);
            br.getPage(param.getCryptedUrl());
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final String notThis = "(?:https?:)?" + buildHostsPatternPart(getPluginDomains().get(0)) + "[\\S&]+";
            final HashSet<String> dupe = new HashSet<String>();
            // 1.st try: <a href="LINK" target="_blank" rel="nofollow"> but ignore
            // images/self site refs + imdb refs
            String[] links = br.getRegex("<a href=\"(" + notThis + ")\"(?:\\s+[^>]*target=\"_blank\" rel=\"nofollow[^>]*|>Download from)").getColumn(0);
            if (links != null && links.length != 0) {
                for (String link : links) {
                    if (!dupe.add(link)) {
                        continue;
                    }
                    if (!this.canHandle(link)) {
                        ret.add(createDownloadlink(br.getURL(link).toString()));
                    }
                }
            }
            /* Now find single redirect-URLs */
            int numberofRedirectURLs = 0;
            final String[] allURLs = HTMLParser.getHttpLinks(br.getRequest().getHtmlCode(), br.getURL());
            /* Store the csrftoken on the redirect-links so the redirect-crawler can reuse it. */
            String csrf = br.getCookie(br.getHost(), "csrftoken", Cookies.NOTDELETEDPATTERN);
            if (StringUtils.isEmpty(csrf)) {
                csrf = br.getRegex("name='csrfmiddlewaretoken' value='([^']+)'").getMatch(0);
            }
            for (final String url : allURLs) {
                if (new Regex(url, PATTERN_REDIRECT).patternFind()) {
                    final DownloadLink redirectLink = createDownloadlink(url);
                    if (!StringUtils.isEmpty(csrf)) {
                        redirectLink.setProperty(PROPERTY_CSRFTOKEN, csrf);
                    }
                    ret.add(redirectLink);
                    numberofRedirectURLs++;
                }
            }
            if (numberofRedirectURLs == 0) {
                logger.warning("Failed to find any redirect-URLs -> Possible crawler failure");
            }
            // try also LINK</br>, but ignore self site refs + imdb refs
            links = br.getRegex("(" + notThis + ")<br\\s*/\\s*>").getColumn(0);
            if (links.length > 0) {
                for (String link : links) {
                    // strip html tags
                    link = link.replaceAll("<[^>]+>", "");
                    if (!dupe.add(link)) {
                        continue;
                    }
                    if (!this.canHandle(link)) {
                        ret.add(createDownloadlink(link));
                    }
                }
            }
            final String[] covers = br.getRegex("\"((?:https?:)?//(pi?xhst|pixhost)\\.(com|co|icu)[^<>\"]*?)\"").getColumn(0);
            if (covers != null && covers.length != 0) {
                for (String coverlink : covers) {
                    coverlink = Request.getLocation(coverlink, br.getRequest());
                    if (!dupe.add(coverlink)) {
                        continue;
                    }
                    final DownloadLink cover = createDownloadlink(coverlink);
                    cover.setAvailable(true);
                    ret.add(cover);
                }
            }
            String title = br.getRegex("<title>(.*?)\\s*[\\|/]\\s*AvaxHome.*?</title>").getMatch(0);
            if (title == null) {
                title = br.getRegex("<h1>(.*?)</h1>").getMatch(0);
            }
            if (title != null) {
                title = Encoding.htmlOnlyDecode(title).trim();
                final FilePackage fp = FilePackage.getInstance();
                /* Two releases can have the same title but we want them in different packages. */
                fp.setAllowMerge(false);
                final String greenLabel = br.getRegex("<span class=\"label label-success\"[^>]*>([^<]+)</span>").getMatch(0);
                if (greenLabel != null) {
                    fp.setName(title + " " + Encoding.htmlDecode(greenLabel).trim());
                } else {
                    fp.setName(title);
                }
                fp.addLinks(ret);
            }
        } else {
            /* PATTERN_RELEASE_OVERVIEW */
            br.setFollowRedirects(true);
            br.getPage(param.getCryptedUrl());
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            logger.info("Performing fallback");
            final String[] urls = HTMLParser.getHttpLinks(br.getRequest().getHtmlCode(), br.getURL());
            for (final String url : urls) {
                if (new Regex(url, PATTERN_RELEASE).patternFind()) {
                    ret.add(this.createDownloadlink(url));
                }
            }
            if (ret.isEmpty()) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        return ret;
    }

    /** Sets the csrftoken cookie for all known plugin domains. */
    private void setCsrfCookie(final String csrftoken) {
        for (final String domain : getPluginDomains().get(0)) {
            br.setCookie(domain, "csrftoken", csrftoken);
        }
    }

    private void followInternalRedirects() throws IOException {
        while (true) {
            final String link = br.getRedirectLocation();
            if (link != null && link.matches(this.getSupportedLinks().pattern()) && link.matches("^https?://.+")) {
                br.followRedirect();
            } else {
                break;
            }
        }
    }

    @Override
    public boolean hasCaptcha(final CryptedLink link, jd.plugins.Account acc) {
        return true;
    }
}