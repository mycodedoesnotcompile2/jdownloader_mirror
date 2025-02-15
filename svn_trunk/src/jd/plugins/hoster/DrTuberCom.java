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
import java.util.regex.Pattern;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;

import org.appwork.utils.StringUtils;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.config.Property;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.URLConnectionAdapter;
import jd.nutils.JDHash;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.PluginJSonUtils;
import jd.plugins.components.SiteType.SiteTemplate;

@HostPlugin(revision = "$Revision: 49985 $", interfaceVersion = 2, names = { "drtuber.com" }, urls = { "https?://(www\\.|m\\.)?drtuber\\.com/(?:embed/\\d+|video/\\d+(?:[a-z0-9\\-_]+)?)" })
public class DrTuberCom extends PluginForHost {
    public DrTuberCom(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + getHost() + "/signup?track=top_menu");
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX };
    }

    /* Similar sites: drtuber.com, proporn.com, viptube.com, tubeon.com, winporn.com */
    /*
     * Last revision with old filecheck handling: 29195. Checking the directlinks inside requestFileInformation may lead to 404 errors and
     * offline links which are online via browser so better avoid that.
     */
    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/static/terms";
    }

    @SuppressWarnings("deprecation")
    public void correctDownloadLink(final DownloadLink link) {
        String newlink = link.getDownloadURL().toLowerCase();
        newlink = newlink.replaceFirst("m\\.drtuber\\.com/", "drtuber.com/").replaceFirst("http://", "https://");
        link.setUrlDownload(newlink);
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

    private String getFID(final DownloadLink dl) {
        return new Regex(dl.getPluginPatternMatcher(), "(?:embed|video)/(\\d+)$").getMatch(0);
    }

    /** Items with different FUIDs but same filenames should not get treated as mirrors! */
    @Override
    public String getMirrorID(DownloadLink link) {
        final String fuid = getFID(link);
        if (link != null && StringUtils.equals(getHost(), link.getHost()) && fuid != null) {
            return getHost() + "://" + fuid;
        } else {
            return super.getMirrorID(link);
        }
    }

    /*
     * IMPORTANT: This can be used as a workaround if the normal handling fails and there is no time to fix it or it's not easily fixable...
     */
    private boolean              use_mobile                   = false;
    /*
     * Allow usage of uncrypted finallinks - quality might be lower than when using the complicated way but overall it might stability..
     */
    private boolean              allow_uncrypted_downloadlink = false;
    private static final String  normalUA                     = "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:35.0) Gecko/20100101 Firefox/35.0";
    private static final String  mobileUA                     = "Mozilla/5.0 (Linux; U; Android 2.2.1; en-us; Nexus One Build/FRG83) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile";
    private final String         type_embed                   = "https?://[^/]+/embed/(\\d+)";
    private final String         type_normal                  = "https?://[^/]+/video/(\\d+)(?:/[a-z0-9\\-_]+)?";
    private String               dllink                       = null;
    /* Connection stuff */
    private static final boolean FREE_RESUME                  = true;
    private static final int     FREE_MAXCHUNKS               = 0;
    private static final int     FREE_MAXDOWNLOADS            = 20;
    private static final boolean ACCOUNT_FREE_RESUME          = true;
    private static final int     ACCOUNT_FREE_MAXCHUNKS       = 0;
    private static final int     ACCOUNT_FREE_MAXDOWNLOADS    = 20;

    /* don't touch the following! */
    private String getContinueLink(String fun) {
        if (fun == null) {
            return null;
        }
        fun = fun.replaceAll("s1\\.addVariable\\(\\'config\\',", "var result = ").replaceAll("params\\);", "params;");
        Object result = new Object();
        ScriptEngineManager manager = JavaScriptEngineFactory.getScriptEngineManager(this);
        ScriptEngine engine = manager.getEngineByName("javascript");
        try {
            engine.eval(fun);
            result = engine.get("result");
        } catch (Throwable e) {
            return null;
        }
        return result.toString();
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return FREE_MAXDOWNLOADS;
    }

    @SuppressWarnings("deprecation")
    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        dllink = null;
        setBrowserExclusive();
        br.setFollowRedirects(true);
        prepBR(br);
        br.setCookie(this.getHost(), "lang", "en");
        String continueLink = null, filename = null;
        // Check if link is an embedded link e.g. from a decrypter
        /* embed v3 */
        String vk = new Regex(link.getPluginPatternMatcher(), "vkey=(\\w+)").getMatch(0);
        if (vk != null) {
            logger.info("Accessing embedded video - trying to find original video URL");
            br.getPage(link.getPluginPatternMatcher() + "&pkey=" + JDHash.getMD5(vk + Encoding.Base64Decode("S0s2Mml5aUliWFhIc2J3")));
            if (br.containsHTML("Invalid video key\\!") || br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final String original_video_link = br.getRegex("type=video_click\\&amp;target_url=(http.*?)</url>").getMatch(0);
            if (original_video_link == null) {
                logger.warning("Failed to find original link for: " + link.getPluginPatternMatcher());
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            link.setUrlDownload(Encoding.htmlDecode(original_video_link));
        }
        br.getPage(link.getPluginPatternMatcher());
        if (br.containsHTML("This video was deleted") || br.getURL().contains("missing=true") || br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        // No account support -> No support for private videos
        if (br.containsHTML("Sorry\\.\\.\\. Video is private")) {
            logger.info("Private video --> Unsupported --> Offline");
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (use_mobile) {
            /* Basically a way to avoid all the crypto stuff but eventually gives you lower quality than the normal site. */
            final String fid = getFID(link);
            br.clearCookies("https://drtuber.com");
            br.clearCookies("https://www.drtuber.com");
            br.getHeaders().put("User-Agent", mobileUA);
            br.setCookie("https://drtuber.com", "lang", "en");
            br.setCookie("https://drtuber.com", "no_popups", "1");
            br.setCookie("https://drtuber.com", "no_ads", "1");
            br.setCookie("https://drtuber.com", "_gat", "1");
            br.setCookie("https://m.drtuber.com", "traffic_type", "3");
            br.setCookie("https://m.drtuber.com", "adv_show", "1");
            br.setCookie("https://m.drtuber.com", "dwnld_speed", "7.436709219858156");
            /*
             * There are 2 mobile versions: 'light' and 'full'. 'light' seems to be a low quality .3gp and 'full' a higher quality .mp4
             * though not necessarily as high as via the non-mobile site.
             */
            br.getPage("https://m.drtuber.com/video/" + fid + "/");
            filename = br.getRegex("<title>Download Free Mobile Porn \\- ([^<>\"]*?) \\- DrTuber\\.com</title>").getMatch(0);
            br.getPage("https://m.drtuber.com/play/" + fid + "?from=video_bottom");
            filename = br.getRegex("<title>Download Free Mobile Porn \\-([^<>\"]*?)\\- Download Preview \\- DrTuber\\.com</title>").getMatch(0);
            dllink = br.getRegex("\"(https?://[a-z0-9\\.\\-]+/(mp4|3gp)/[^<>\"]*?)\"").getMatch(0);
        } else {
            /* 2016-04-29: They're playing games with us with their embed html --> Avoid that! */
            if (link.getPluginPatternMatcher().matches(type_embed)) {
                String source_url = br.getRegex("target_url=(http[^<>\"\\'=\\&]+)").getMatch(0);
                if (source_url != null) {
                    source_url = Encoding.htmlDecode(source_url);
                }
                if (source_url != null && source_url.matches(type_normal)) {
                    link.setUrlDownload(source_url);
                    br.getPage(source_url);
                }
            }
            String vkey = null;
            /* Normal links */
            if (new Regex(link.getPluginPatternMatcher(), Pattern.compile(type_normal)).matches()) {
                filename = br.getRegex("<title>(.*?) (@ |\\- Free Porn.*?)DrTuber(\\.com)?</title>").getMatch(0);
                if (filename == null) {
                    filename = br.getRegex("<title>([^<>\"]*?) \\- \\d+ \\- DrTuber\\.com</title>").getMatch(0);
                }
                if (filename == null) {
                    filename = br.getRegex("<h1 class=\"(?:name|title)\">(.*?)</h1>").getMatch(0);
                }
                if (filename == null) {
                    filename = br.getRegex("class=\"hd_title\" style=\"text-align:left;\">([^<>\"]*?)</h1>").getMatch(0);
                }
                if (allow_uncrypted_downloadlink) {
                    dllink = getUncryptedFinallink();
                }
                if (dllink == null) {
                    final String vid = new Regex(link.getDownloadURL(), "(?:embed|video)/(\\d+)").getMatch(0);
                    final Browser br1 = br.cloneBrowser();
                    br1.getPage("https://www.drtuber.com/player_config_json/?vid=" + vid);
                    // dllink = br1.getRegex("(https://acdn.*?mp4)").getMatch(0);
                    // String files = PluginJSonUtils.getJsonValue(br1.toString(), "files");
                    final String files = br1.getRegex("files\":(.*?\\}),").getMatch(0);
                    if (files != null) {
                        dllink = PluginJSonUtils.getJsonValue(files, "hq");
                        if (dllink == null) {
                            dllink = PluginJSonUtils.getJsonValue(files, "lq");
                        }
                        if (dllink != null) {
                            link.setProperty("directlink", dllink);
                            checkDirectLink(link, "directlink");
                        }
                    }
                }
                if (dllink == null) {
                    final boolean new_handling = true;
                    if (new_handling) {
                        /*
                         * Very very very very bad js workaround
                         *
                         * IMPORTANT: If we find no other way to fix this in the future, switch to /embed/ links, old handling still works
                         * fine for them
                         */
                        continueLink = "https://www.drtuber.com/player_config/?";
                        final String[] params = br.getRegex("params \\+= ([^<>\"]*?);").getColumn(0);
                        for (String param : params) {
                            param = param.replace("'", "");
                            param = param.replace("+", "");
                            param = param.replace(" ", "");
                            param = Encoding.htmlDecode(param);
                            if (vkey == null) {
                                vkey = new Regex(param, "vkey=([a-z0-9]+)").getMatch(0);
                            }
                            continueLink += Encoding.htmlDecode(param);
                        }
                        if (vkey != null) {
                            continueLink += "&pkey=" + JDHash.getMD5(vkey + Encoding.Base64Decode("UFQ2bDEzdW1xVjhLODI3"));
                        }
                        continueLink += "&aid=&domain_id=";
                    } else {
                        continueLink = getContinueLink(br.getRegex("(var configPath.*?addVariable\\(\\'config\\',.*?;)").getMatch(0));
                        vkey = new Regex(continueLink, "vkey=(\\w+)").getMatch(0);
                        if (continueLink == null || vkey == null) {
                            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                        }
                        if (!continueLink.startsWith("https://")) {
                            continueLink = "https://drtuber.com" + Encoding.htmlDecode(continueLink) + "&pkey=" + JDHash.getMD5(vkey + Encoding.Base64Decode("UFQ2bDEzdW1xVjhLODI3"));
                        } else {
                            continueLink = Encoding.htmlDecode(continueLink) + "&pkey=" + JDHash.getMD5(vkey + Encoding.Base64Decode("UFQ2bDEzdW1xVjhLODI3"));
                        }
                    }
                }
            } else if (link.getPluginPatternMatcher().matches(type_embed)) {
                /* embed v4 */
                /* 2016-04-29: They're playing games with us with their embed html ... */
                String nextUrl = br.getRegex("config=(http%3A%2F%2F(www\\.)?drtuber\\.com%2Fplayer_config%2F[^<>\"]*?)\"").getMatch(0);
                if (nextUrl == null) {
                    String[] hashEncValues = br.getRegex("flashvars=\"id_video=(\\d+)\\&t=(\\d+)").getRow(0);
                    if (hashEncValues == null) {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                    continueLink = "/player/config_embed4.php?id_video=" + hashEncValues[0] + "&t=" + hashEncValues[1] + "&pkey=" + JDHash.getMD5(hashEncValues[0] + hashEncValues[1] + Encoding.Base64Decode("RXMxaldDemZOQmRsMlk4"));
                } else {
                    nextUrl = Encoding.htmlDecode(nextUrl);
                    vk = new Regex(nextUrl, "vkey=(\\w+)").getMatch(0);
                    continueLink = nextUrl + "&pkey=" + JDHash.getMD5(vk + Encoding.Base64Decode("UFQ2bDEzdW1xVjhLODI3"));
                }
                filename = br.getRegex("<title>(.*?)\\s+\\-\\s+Free Porn Videos").getMatch(0);
            }
            if (dllink == null) {
                if (continueLink == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                br.getPage(continueLink);
                dllink = br.getRegex("<video_file>(<\\!\\[CDATA\\[)?(http.*?)(\\]\\]>)?</video_file>").getMatch(1);
            }
        }
        if (filename == null || dllink == null) {
            logger.info("filename: " + filename + ", dllink: " + dllink);
            if (br.containsHTML("<video_file><\\!\\[CDATA\\[\\]\\]></video_file>")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dllink = Encoding.htmlDecode(dllink.trim());
        filename = Encoding.htmlDecode(filename);
        filename = filename.trim();
        String ext;
        if (dllink.contains(".flv")) {
            ext = ".flv";
        } else if (dllink.contains(".mp4")) {
            ext = ".mp4";
        } else {
            /* Should usually not happen. */
            ext = ".3gp";
        }
        link.setProperty("ftitle", filename);
        link.setProperty("fext", ext);
        link.setName(filename + ext);
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        dl = new jd.plugins.BrowserAdapter().openDownload(br, link, dllink, FREE_RESUME, FREE_MAXCHUNKS);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            handleServerErrors();
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String ftitle_saved = link.getStringProperty("ftitle", null);
        final String fext_saved = link.getStringProperty("fext", null);
        link.setFinalFileName(ftitle_saved + fext_saved);
        dl.startDownload();
    }

    private String checkDirectLink(final DownloadLink link, final String property) {
        String dllink = link.getStringProperty(property);
        if (dllink != null) {
            URLConnectionAdapter con = null;
            try {
                final Browser br2 = br.cloneBrowser();
                br2.setFollowRedirects(true);
                con = br2.openHeadConnection(dllink);
                if (this.looksLikeDownloadableContent(con)) {
                    if (con.getCompleteContentLength() > 0) {
                        link.setVerifiedFileSize(con.getCompleteContentLength());
                    }
                    return dllink;
                } else {
                    throw new IOException();
                }
            } catch (final Exception e) {
                logger.log(e);
                return null;
            } finally {
                if (con != null) {
                    con.disconnect();
                }
            }
        }
        return null;
    }

    private void login(final Account account) throws Exception {
        synchronized (account) {
            try {
                final Cookies cookies = account.loadCookies("");
                if (cookies != null) {
                    prepBR(br);
                    br.setCookies(this.getHost(), cookies);
                    br.getPage("https://www." + this.getHost() + "/");
                    if (!invalidedSession(false)) {
                        /* Save new cookie timestamp */
                        br.setCookies(this.getHost(), cookies);
                        return;
                    } else {
                        br.clearCookies(br.getHost());
                    }
                }
                prepBR(br);
                br.setFollowRedirects(false);
                br.getHeaders().put("X-Requested-With", "XMLHttpRequest");
                br.getPage("https://www." + this.getHost() + "/ajax/popup_forms?form=login");
                final String brcontent = br.toString();
                br.postPage("/ajax/login", "submit_login=true&login_remember=true&username=" + Encoding.urlEncode(account.getUser()) + "&password=" + Encoding.urlEncode(account.getPass()));
                if (!PluginJSonUtils.parseBoolean(PluginJSonUtils.getJson(br, "success")) && PluginJSonUtils.parseBoolean(PluginJSonUtils.getJson(br, "captcha"))) {
                    // recaptchav2
                    final DownloadLink orig = this.getDownloadLink();
                    try {
                        final DownloadLink dummyLink = new DownloadLink(this, "Account Login!", getHost(), getHost(), true);
                        this.setDownloadLink(dummyLink);
                        final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br) {
                            public String getSiteKey() {
                                return getSiteKey(brcontent);
                            }
                        }.getToken();
                        br.postPage("/ajax/login", "submit_login=true&login_remember=true&username=" + Encoding.urlEncode(account.getUser()) + "&password=" + Encoding.urlEncode(account.getPass()) + "&g-recaptcha-response=" + Encoding.urlEncode(recaptchaV2Response));
                    } finally {
                        this.setDownloadLink(orig);
                    }
                }
                if (invalidedSession(true)) {
                    throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_DISABLE);
                }
                // Save cookies
                account.saveCookies(br.getCookies(this.getHost()), "");
            } catch (final PluginException e) {
                account.setProperty("cookies", Property.NULL);
                throw e;
            }
        }
    }

    private boolean invalidedSession(final boolean postLogin) {
        if (br.getCookie(this.getHost(), "remember", Cookies.NOTDELETEDPATTERN) == null || "deleted".equals(br.getCookie(this.getHost(), "remember", Cookies.NOTDELETEDPATTERN))) {
            return true;
        } else if (postLogin && !br.containsHTML("\"success\":true")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        login(account);
        ai.setUnlimitedTraffic();
        account.setType(AccountType.FREE);
        account.setMaxSimultanDownloads(ACCOUNT_FREE_MAXDOWNLOADS);
        account.setConcurrentUsePossible(true);
        return ai;
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        final String ftitle_saved = link.getStringProperty("ftitle", null);
        requestFileInformation(link);
        login(account);
        br.setFollowRedirects(true);
        br.getPage(link.getPluginPatternMatcher());
        /* Try not to waste new link generation as we can only download 3 videos a day... */
        dllink = this.checkDirectLink(link, "account_free_directlink");
        if (dllink == null) {
            dllink = getUncryptedFinallink();
        }
        if (dllink == null) {
            /* Only 'use the download button' if we cannot find any stream URL as filesizes/video quality should be the same. */
            br.getHeaders().put("X-Requested-With", "XMLHttpRequest");
            br.getPage("https://www.drtuber.com/video/download/" + getFID(link));
            if (br.containsHTML("you have reached the download limit")) {
                throw new PluginException(LinkStatus.ERROR_PREMIUM, "Daily downloadlimit reached!", PluginException.VALUE_ID_PREMIUM_TEMP_DISABLE);
            }
            dllink = "https://www.drtuber.com/video/download/save/" + getFID(link);
        }
        dl = new jd.plugins.BrowserAdapter().openDownload(br, link, dllink, ACCOUNT_FREE_RESUME, ACCOUNT_FREE_MAXCHUNKS);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            handleServerErrors();
            if (br.containsHTML("You have exceeded free downloads count")) {
                throw new PluginException(LinkStatus.ERROR_PREMIUM, "Daily downloadlimit reached!", PluginException.VALUE_ID_PREMIUM_TEMP_DISABLE);
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        link.setFinalFileName(ftitle_saved + ".mp4");
        link.setProperty("account_free_directlink", dl.getConnection().getURL().toString());
        dl.startDownload();
    }

    private String getUncryptedFinallink() {
        return br.getRegex("<source src=\"(https?://[^<>\"]*?)\" type=\"video/mp4\"").getMatch(0);
    }

    private void handleServerErrors() throws PluginException {
        if (dl.getConnection().getResponseCode() == 403) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 60 * 60 * 1000l);
        } else if (dl.getConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 60 * 60 * 1000l);
        }
    }

    private void prepBR(final Browser br) {
        br.getHeaders().put("User-Agent", normalUA);
        br.getHeaders().put("Accept-Language", "de,en-US;q=0.7,en;q=0.3");
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }

    @Override
    public void resetPluginGlobals() {
    }

    @Override
    public SiteTemplate siteTemplateType() {
        return SiteTemplate.UnknownPornScript9;
    }
}