//    jDownloader - Downloadmanager
//    Copyright (C) 2009  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.hoster;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Random;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Browser.BrowserException;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

import org.appwork.utils.formatter.SizeFormatter;

@HostPlugin(revision = "$Revision: 47474 $", interfaceVersion = 2, names = { "dl.free.fr" }, urls = { "http://(www\\.)?dl\\.free\\.fr/(getfile\\.pl\\?file=/[\\w]+|[\\w]+/?)" })
public class DlFreeFr extends PluginForHost {
    @Override
    public String rewriteHost(String host) {
        if (host == null || "dl.free.fr".equals(host) || "free.fr".equals(host)) {
            return "dl.free.fr";
        }
        return super.rewriteHost(host);
    }

    private enum CaptchaTyp {
        image,
        audio,
        video,
        notDetected;
    }

    public DlFreeFr(PluginWrapper wrapper) {
        super(wrapper);
    }

    private boolean HTML = false;

    @Override
    public String getAGBLink() {
        return "http://dl.free.fr/";
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return -1;
    }

    private boolean breakCaptcha(Form form, DownloadLink downloadLink) throws Exception {
        if (!form.containsHTML("Adyoulike\\.create")) {
            return false;
        }
        HashMap<String, String> c = new HashMap<String, String>();
        for (String[] s : form.getRegex("\"([^\\{\"]+)\":\"([^,\"]+)\"").getMatches()) {
            c.put(s[0], s[1]);
        }
        if (c == null || c.size() == 0) {
            return false;
        }
        /* create challenge url */
        final Browser ayl = br.cloneBrowser();
        ayl.getPage("http://api-ayl.appspot.com/challenge?key=" + c.get("key") + "&env=" + c.get("env") + "&callback=Adyoulike.g._jsonp_" + (int) (Math.random() * (99999 - 10000) + 10000));
        final String[][] allValues = ayl.getRegex("\"([^\\{\\}\"]+)\":\"?([^,\"\\}\\{]+)\"?").getMatches();
        if (allValues == null || allValues.length == 0) {
            return false;
        }
        for (String[] s : allValues) {
            c.put(s[0], s[1]);
        }
        String cType = c.get("medium_type");
        cType = cType == null ? "notDetected" : cType;
        cType = cType.split("/")[0];
        String instructions = null, cCode = null;
        if (ayl.getRegex("adyoulike\":\\{\"disabled\":(true)").matches()) {
            br.submitForm(form);
            return true;
        }
        /* Only available in France */
        if ("notDetected".equals(cType) && c.containsKey("disabled") && "true".equals(c.get("disabled"))) {
            throw new PluginException(LinkStatus.ERROR_FATAL, "Only available in France. Please use a french proxy!");
        }
        switch (CaptchaTyp.valueOf(cType)) {
        case image:
            ayl.setFollowRedirects(true);
            instructions = c.get("instructions_visual");
            // Captcha also broken via browser
            if (c.get("token") == null) {
                throw new PluginException(LinkStatus.ERROR_FATAL, "No captcha shown, please contact the dl.free.fr support!");
            }
            final String responseUrl = "http://api-ayl.appspot.com/resource?token=" + c.get("token") + "&env=" + c.get("env");
            if (instructions != null) {
                cCode = new Regex(instructions, "Recopiez « (.*?) » ci\\-dessous").getMatch(0);
                if (cCode == null) {
                    final File captchaFile = this.getLocalCaptchaFile();
                    Browser.download(captchaFile, ayl.openGetConnection(responseUrl));
                    cCode = getCaptchaCode(null, captchaFile, downloadLink);
                }
            } else {
                final File captchaFile = this.getLocalCaptchaFile();
                Browser.download(captchaFile, ayl.openGetConnection(responseUrl));
                cCode = getCaptchaCode(null, captchaFile, downloadLink);
            }
            break;
        case audio:
            break;
        case video:
            break;
        case notDetected:
            break;
        default:
            logger.warning("Unknown captcha typ: " + cType);
            return false;
        }
        if (cCode == null) {
            return false;
        }
        form.put("_ayl_captcha_engine", "adyoulike");
        form.put("_ayl_response", cCode);
        form.put("_ayl_utf8_ie_fix", "%E2%98%83");
        form.put("_ayl_env", c.get("env"));
        form.put("_ayl_token_challenge", c.get("token"));
        form.put("_ayl_tid", c.get("tid"));
        br.submitForm(form);
        return true;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        if (HTML) {
            logger.info("InDirect download");
            br.setFollowRedirects(false);
            if (br.containsHTML("Trop de slots utilis")) {
                throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, null, 10 * 60 * 1001l);
            }
            // These are not used, so why throw exception?
            // final Form captchaForm = br.getForm(1);
            // if (captchaForm == null) throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            // Old
            // /* special captcha handling */
            // boolean isCaptchaResolved = false;
            // for (int i = 0; i < 5; i++) {
            // isCaptchaResolved = breakCaptcha(captchaForm, downloadLink);
            // if (isCaptchaResolved) break;
            // }
            // if (!isCaptchaResolved) throw new PluginException(LinkStatus.ERROR_CAPTCHA);
            // Really old
            // PluginForHost recplug =
            // JDUtilities.getPluginForHost("DirectHTTP");
            // jd.plugins.hoster.DirectHTTP.Recaptcha rc = ((DirectHTTP)
            // recplug).getReCaptcha(br);
            // rc.setForm(captchaForm);
            // String id =
            // br.getRegex("\\?k=([A-Za-z0-9%_\\+\\- ]+)\\'").getMatch(0);
            // // No captcha displayed but we have to tner it->Hoster bug
            // if (id == null &&
            // br.containsHTML("Valider et t\\&eacute;l\\&eacute;charger le fichier"))
            // throw new
            // PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE,
            // "Server error", 30 * 60 * 1000l);
            // rc.setId(id);
            // rc.load();
            // rc.getForm().put("_ayl_captcha_engine", "recaptcha");
            // rc.getForm().put("_ayl_utf8_ie_fix", "%E2%98%83");
            // rc.getForm().put("_ayl_env", "prod");
            // rc.getForm().put("_ayl_token_challenge", "undefined");
            // rc.getForm().put("_ayl_tid", "undefined");
            // File cf = rc.downloadCaptcha(getLocalCaptchaFile());
            // String c = getCaptchaCode(cf, downloadLink);
            // rc.setCode(c);
            // if
            // (br.containsHTML("(api\\.recaptcha\\.net|google\\.com/recaptcha/api/)"))
            // throw new PluginException(LinkStatus.ERROR_CAPTCHA);
            String dlLink = null;
            final int repeat = 3;
            for (int i = 0; i != repeat; i++) {
                // small sleep?
                sleep((new Random().nextInt(10) + 1) * 1000, link);
                final String file = br.getRegex("type=\"hidden\" name=\"file\" value=\"([^<>\"]*?)\"").getMatch(0);
                if (file == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                br.postPage("/getfile.pl", "file=" + Encoding.urlEncode(file));
                dlLink = br.getRedirectLocation();
                if (dlLink != null) {
                    break;
                } else if (dlLink == null && (i + 1 != repeat)) {
                    // lets put a small wait in here
                    link.wait(2563l * new Random().nextInt(4));
                    continue;
                } else {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Unknown server error");
                }
            }
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dlLink, true, 1);
        } else {
            logger.info("Direct download");
            br.setFollowRedirects(true);
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, link.getDownloadURL(), true, 1);
        }
        /*
         * 2020-07-21: Because this is basic auth, browser may ask for username AND password --> This is irritating - any username will
         * work! They've mis-used basic auth as a password prompt!
         */
        String passCode = link.getDownloadPassword();
        if (!dl.getConnection().isContentDisposition() && br.getHttpConnection().getResponseCode() == 401) {
            br.followConnection(true);
            logger.info("Password required");
            if (passCode == null) {
                passCode = getUserInput("Password?", link);
            }
            final String passB64 = Encoding.Base64Encode(":" + passCode);
            br.getHeaders().put("Authorization", "Basic " + passB64);
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dl.getConnection().getURL().toString(), true, 1);
        }
        if (!dl.getConnection().isContentDisposition()) {
            br.followConnection(true);
            if (br.getHttpConnection().getResponseCode() == 401) {
                link.setDownloadPassword(null);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Wrong password");
            } else if (br.getURL().contains("overload")) {
                throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, 60 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        if (passCode != null) {
            link.setDownloadPassword(passCode);
        }
        dl.startDownload();
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        this.setBrowserExclusive();
        br.setReadTimeout(3 * 60 * 1000);
        br.setFollowRedirects(true);
        URLConnectionAdapter con = null;
        try {
            con = br.openGetConnection(link.getDownloadURL());
            if (con.isOK() && con.isContentDisposition()) {
                link.setFinalFileName(Plugin.getFileNameFromHeader(con));
                link.setDownloadSize(con.getCompleteContentLength());
                return AvailableStatus.TRUE;
            } else {
                br.followConnection(true);
                HTML = true;
            }
        } catch (final BrowserException e) {
            if (br.getRequest() != null && br.getRequest().getHttpConnection().getResponseCode() == 503) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            throw e;
        } finally {
            try {
                con.disconnect();
            } catch (final Throwable e) {
            }
        }
        final String filename = br.getRegex("Fichier</span>\\s*(.*?)\\s*<").getMatch(0);
        final String filesize = br.getRegex("Taille</span>\\s*(.*?)\\s*<").getMatch(0);
        if (filename == null) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        link.setName(filename.trim());
        link.setDownloadSize(SizeFormatter.getSize(filesize.replaceAll("o", "byte").replaceAll("Ko", "Kb").replaceAll("Mo", "Mb").replaceAll("Go", "Gb")));
        return AvailableStatus.TRUE;
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

    /* NO OVERRIDE!! We need to stay 0.9*compatible */
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        return true;
    }
}