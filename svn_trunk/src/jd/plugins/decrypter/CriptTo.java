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
package jd.plugins.decrypter;

import java.util.ArrayList;
import java.util.Collections;

import org.jdownloader.captcha.v2.challenge.clickcaptcha.ClickedPoint;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperCrawlerPluginRecaptchaV2;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterException;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 51695 $", interfaceVersion = 3, names = { "cript.to" }, urls = { "https?://(?:www\\.)?cript\\.to/folder/([A-Za-z0-9]+)" })
public class CriptTo extends PluginForDecrypt {
    public CriptTo(PluginWrapper wrapper) {
        super(wrapper);
    }

    public ArrayList<DownloadLink> decryptIt(CryptedLink param, ProgressController progress) throws Exception {
        ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        ArrayList<String> dupelist = new ArrayList<String>();
        final String contenturl = param.getCryptedUrl();
        final String folder_id = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        this.br.setFollowRedirects(true);
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (this.br.containsHTML("Inhalt im Usenet gefunden - Weiterleitung erfolgt sofort ...")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        handle_captcha: {
            Form captchaform = getCaptchaForm(br);
            if (captchaform == null) {
                logger.info("No captcha required");
                break handle_captcha;
            }
            boolean failed = true;
            for (int i = 0; i <= 3; i++) {
                if (i > 0) {
                    br.getPage(contenturl);
                }
                if (this.br.containsHTML("circlecaptcha")) {
                    final String captchaurl = br.getRegex("<input type=\"image\" style=\"cursor:crosshair;\" src=\"([^\"]*)\" alt=\"Circle Captcha\"").getMatch(0);
                    if (captchaurl == null || !captchaurl.contains("circlecaptcha")) {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                    final ClickedPoint cp = getCaptchaClickedPoint(captchaurl, param, "Click on the open circle");
                    if (cp == null) {
                        if (i < 3) {
                            continue;
                        } else {
                            throw new PluginException(LinkStatus.ERROR_CAPTCHA);
                        }
                    }
                    captchaform.put("button.x", Integer.toString(cp.getX()));
                    captchaform.put("button.y", Integer.toString(cp.getY()));
                } else if (this.br.containsHTML("Simple Captcha")) {
                    final String captchaurl = br.getRegex("<img src=\"([^\"]*)\" alt=\"Simple Captcha\"").getMatch(0);
                    if (captchaurl == null || !captchaurl.contains("simplecaptcha")) {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                    final String captchacode = this.getCaptchaCode(captchaurl, param);
                    captchaform.put("simplecaptcha", Encoding.urlEncode(captchacode));
                } else {
                    /* Assume that a reCaptcha is required */
                    /**
                     * 2025-10-20: Website is buggy and sometimes doesn't display a captcha although one is required. <br>
                     * We can simply work around this by hard-coding the recaptcha-site-key and always submitting a captcha in this case.
                     */
                    // captcha_driver=recaptcha
                    // final String recaptchaV2Response = new CaptchaHelperCrawlerPluginRecaptchaV2(this, br).getToken();
                    final String recaptchaV2Response = new CaptchaHelperCrawlerPluginRecaptchaV2(this, br, "6LdWbDEUAAAAAH2Df7gKk-cyZhvH9aXlzQ0VFDRe").getToken();
                    captchaform.put("g-recaptcha-response", Encoding.urlEncode(recaptchaV2Response));
                    captchaform.put("captcha_driver", "recaptcha");
                }
                // TODO: Check if this is still needed
                final String linksafe_csrf_token = br.getRegex("<input type=\"hidden\" name=\"linksafe_csrf_token\" value=\"([^\"]*)\"").getMatch(0);
                if (linksafe_csrf_token != null) {
                    captchaform.put("linksafe_csrf_token", Encoding.urlEncode(linksafe_csrf_token));
                }
                br.submitForm(captchaform);
                captchaform = getCaptchaForm(br);
                if (br.containsHTML("Wrong captcha solution") || captchaform != null) {
                    /* Invalid captcha */
                    invalidateLastChallengeResponse();
                    this.br.getPage(contenturl);
                    continue;
                }
                /* Valid captcha has been entered */
                validateLastChallengeResponse();
                failed = false;
                break;
            }
            if (failed) {
                throw new PluginException(LinkStatus.ERROR_CAPTCHA);
            }
        }
        handle_folder_password: {
            Form pwform = getCaptchaForm(br);
            if (pwform == null) {
                logger.info("No folder password required");
                break handle_folder_password;
            }
            boolean success = false;
            for (int i = 0; i <= 2; i++) {
                final String passCode = getUserInput("Password?", param);
                pwform.put("password", Encoding.urlEncode(passCode));
                br.submitForm(pwform);
                pwform = getFolderpwForm(this.br);
                if (pwform == null) {
                    logger.info("User entered correct password: " + passCode);
                    success = true;
                    break;
                } else {
                    logger.info("User entered wrong password: " + passCode);
                }
            }
            if (!success) {
                throw new DecrypterException(DecrypterException.PASSWORD);
            }
        }
        String title = br.getRegex("var folder = '([^']+)';").getMatch(0);
        if (title == null) {
            title = br.getRegex("class=\"col-md-11\">\\s*<h3>([^<]+)").getMatch(0);
        }
        final FilePackage fp = FilePackage.getInstance();
        if (title != null) {
            fp.setName(title);
        } else {
            /* Fallback */
            fp.setName(folder_id);
        }
        fp.setPackageKey("cryptto://folder/" + folder_id);
        /* Try to add links via DLC since this is much faster than crawling the links one by one. */
        crawl_dlc: {
            final String dlc_url = br.getRegex("'(https?://cript\\.to/dlc/[^\"']+)").getMatch(0);
            if (dlc_url == null) {
                logger.info("No DLC container available");
                break crawl_dlc;
            }
            final Browser brc = br.cloneBrowser();
            final ArrayList<DownloadLink> dlcResults = loadContainerFile(brc, brc.createGetRequest(dlc_url), Collections.singletonMap("extension", ".dlc"));
            if (dlcResults == null || dlcResults.isEmpty()) {
                logger.warning("DLC for current mirror is empty or something is broken!");
                break crawl_dlc;
            }
            fp.addLinks(dlcResults);
            return dlcResults;
        }
        final String[] linkkeys = br.getRegex("href=\"javascript:void\\(0\\);\" onclick=\"popup\\('([^\"]*)'").getColumn(0);
        if (linkkeys == null || linkkeys.length == 0) {
            logger.warning("Decrypter broken for link: " + contenturl);
            return null;
        }
        br.setFollowRedirects(false);
        for (final String linkkey : linkkeys) {
            if (this.isAbort()) {
                throw new InterruptedException("Aborted by user");
            }
            if (dupelist.contains(linkkey)) {
                /* Avoid dupes */
                continue;
            }
            dupelist.add(linkkey);
            this.br.getPage(linkkey);
            final String finallink = this.br.getRedirectLocation();
            if (finallink.matches(".+cript\\.to/bot")) {// only one click captcha? No captcha rotation?
                for (int i = 0; i <= 3; i++) {
                    String postData = "";
                    this.br.setFollowRedirects(true);
                    br.getPage(linkkey);
                    this.br.setFollowRedirects(false);
                    if (this.br.containsHTML("circlecaptcha")) {
                        final String captcha = br.getRegex("<input type=\"image\" style=\"cursor:crosshair;\" src=\"([^\"]*)\" alt=\"Circle Captcha\"").getMatch(0);
                        if (captcha != null && captcha.contains("circlecaptcha")) {
                            final ClickedPoint cp = getCaptchaClickedPoint(captcha, param, "Click on the open circle or single color circle");
                            if (cp == null) {
                                if (i < 3) {
                                    continue;
                                } else {
                                    throw new PluginException(LinkStatus.ERROR_CAPTCHA);
                                }
                            }
                            postData += "button.x=" + cp.getX();
                            postData += "&button.y=" + cp.getY();
                        } else {
                            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                        }
                        final String linksafe_csrf_token = br.getRegex("<input type=\"hidden\" name=\"linksafe_csrf_token\" value=\"([^\"]*)\"").getMatch(0);
                        postData += "&linksafe_csrf_token=" + linksafe_csrf_token;
                        this.br.postPage(finallink, postData);
                        final String finallink2 = this.br.getRedirectLocation();
                        if (finallink2 == null || finallink2.matches(".+cript\\.to/.+")) {
                            continue;
                        }
                        validateLastChallengeResponse();
                        final DownloadLink dl2 = createDownloadlink(finallink2);
                        dl2._setFilePackage(fp);
                        ret.add(dl2);
                        distribute(dl2);
                        break;
                    } else {
                        logger.warning("Unknown captcha: " + contenturl);
                    }
                }
            } else {
                if (finallink == null || finallink.matches(".+cript\\.to/.+")) {
                    continue;
                }
                final DownloadLink dl1 = createDownloadlink(finallink);
                dl1._setFilePackage(fp);
                ret.add(dl1);
                distribute(dl1);
            }
        }
        return ret;
    }

    private Form getCaptchaForm(final Browser br) {
        return br.getFormByInputFieldKeyValue("do", "captcha");
    }

    private Form getFolderpwForm(final Browser br) {
        return br.getFormByInputFieldKeyValue("do", "password");
    }
}