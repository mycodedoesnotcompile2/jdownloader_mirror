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
import java.util.Map;
import java.util.Random;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CaptchaHelperCrawlerPluginCloudflareTurnstile;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 52784 $", interfaceVersion = 3, names = { "hotpornfile.org" }, urls = { "https?://(?:www\\.)?hotpornfile\\.org/(?!page)([^/]+)/(\\d+)" })
public class HotpornfileOrg extends PluginForDecrypt {
    public HotpornfileOrg(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX };
    }

    private final String PROPERTY_captcha_response = "captcha_response";
    private final String PROPERTY_cid              = "cid";

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        final Regex urlinfo = new Regex(contenturl, this.getSupportedLinks());
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String title = br.getRegex("<title>\\s*(.*?)\\s*(\\s*-\\s*Hotpornfile\\s*)?</title>").getMatch(0);
        if (title != null) {
            title = Encoding.htmlDecode(title).trim();
            /* Fix title */
            title = title.replace(" - Rapidgator and ddownload.com download / stream", "");
        } else {
            logger.info("Failed to find title -> Obtaining title from url");
            title = urlinfo.getMatch(0);
            /* Fix title */
            title = Encoding.htmlDecode(title);
            title = title.replace("-", " ").trim();
        }
        final String postID = urlinfo.getMatch(1);
        final String lastCaptchaResponse = this.getPluginConfig().getStringProperty(PROPERTY_captcha_response);
        boolean allowReUseLastCaptchaResponse = true;
        boolean hasEverReUsedFreshCaptchaToken = false;
        String freshCaptchaResponse = null;
        int attempt = 0;
        Map<String, Object> entries = null;
        String lastNext = null;
        String next = null;
        String links_text = null;
        String cid = this.getPluginConfig().getStringProperty(PROPERTY_cid);
        do {
            attempt++;
            final String captchaResponse;
            if (lastCaptchaResponse != null && allowReUseLastCaptchaResponse) {
                logger.info("Trying to re-use last captchaResponse: " + lastCaptchaResponse);
                br.setCookie(br.getHost(), "cPass", lastCaptchaResponse);
                captchaResponse = lastCaptchaResponse;
            } else if (freshCaptchaResponse != null) {
                /* E.g. first attempt: cid is null -> Obtain freshReCaptchaV2Response -> Fresh cid is needed -> Try again with same */
                if (hasEverReUsedFreshCaptchaToken) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                logger.info("Trying to re-use freshReCaptchaV2Response created in this session reCaptchaV2Response: " + freshCaptchaResponse);
                captchaResponse = freshCaptchaResponse;
                hasEverReUsedFreshCaptchaToken = true;
            } else {
                br.getPage(contenturl);
                // captchaResponse = new CaptchaHelperCrawlerPluginRecaptchaV2(this, br,
                // "6Lf1jhYUAAAAAN8kNxOBBEUu3qBPcy4UNu4roO5K").getToken();
                /**
                 * 2026-05-07: They switched from reCaptcha to Turnstile <br>
                 * Key see: https://www.hotpornfile.org/wp-content/cache/autoptimize/js/autoptimize_6a35a81dab693b5a2076c316a89d12ff.js
                 */
                captchaResponse = new CaptchaHelperCrawlerPluginCloudflareTurnstile(this, br, "0x4AAAAAACGxJ80GkzeneMgr").getToken();
                freshCaptchaResponse = captchaResponse;
            }
            final UrlQuery query = new UrlQuery();
            /* 2026-05-07: value was changed from "get_links" to "unlock_links" */
            query.add("action", "unlock_links");
            query.add("type", "download");
            query.add("postId", postID);
            if (cid == null) {
                /* E.g. first request */
                query.add("cid", "null");
            } else {
                query.add("cid", cid);
                query.add("fb", "true");
            }
            query.appendEncoded("challenge", captchaResponse);
            /*
             * Fallback value for when challenge fails. Website is also using this but only when a certain amount of time has passed (45
             * seconds in last test).
             */
            query.add("fjdw", "true");
            br.postPage("/wp-admin/admin-ajax.php", query);
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            lastNext = next;
            next = (String) entries.get("next");
            final Object error = entries.get("error");
            links_text = (String) entries.get("links");
            if ("links".equalsIgnoreCase(next) && links_text != null) {
                // old
                logger.info("Stopping because: next=links and links not empty. error=" + error);
                break;
            } else if ("completed".equalsIgnoreCase(next)) {
                /* 2026-05-07: New */
                links_text = entries.get("result").toString();
                logger.info("Stopping because: next=completed");
                break;
            }
            if (attempt > 10) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            } else if ("ash".equalsIgnoreCase(next)) {
                /* [New] cid value is needed. */
                if (cid == null) {
                    logger.info("New [first] cid value is needed");
                } else {
                    logger.info("New [fresh] cid value is needed");
                }
                cid = generateCID();
                logger.info("Continue because: next=ash");
                continue;
            } else if ("challenge".equalsIgnoreCase(next)) {
                /* Fresh captcha needs to be solved. */
                logger.info("Continue because: lastNext=" + lastNext + "|next=challenge");
                allowReUseLastCaptchaResponse = false;
                continue;
            } else if (Boolean.FALSE.equals(error)) {
                logger.info("Stopping because error == FALSE | lastNext=" + lastNext + "|next=" + next + "|error=" + error + "|attempt=" + attempt);
                break;
            } else {
                logger.info("Stopping because attempt > 0 |  lastNext=" + lastNext + "|next=" + next + "|attempt=" + attempt);
                break;
            }
        } while (!isAbort());
        if (StringUtils.isEmpty(links_text)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String[] links = new Regex(links_text, "\"(https?://[^\"]+)").getColumn(0);
        if (links == null || links.length == 0) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        for (final String singleLink : links) {
            ret.add(createDownloadlink(singleLink));
        }
        if (title != null) {
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(title);
            fp.addLinks(ret);
        }
        if (ret.size() > 0 && freshCaptchaResponse != null) {
            /* Save reCaptchaV2 response and cid as we might be able to use it multiple times! */
            logger.info("Saving reCaptchaV2Response for next time: " + freshCaptchaResponse);
            this.getPluginConfig().setProperty(PROPERTY_captcha_response, freshCaptchaResponse);
            this.getPluginConfig().setProperty(PROPERTY_cid, cid);
        }
        return ret;
    }

    private static String generateCID() {
        // https://www.hotpornfile.org/wp-content/cache/autoptimize/js/autoptimize_cc7f9ec72ccfaed2d3e5faa655373b57.js
        // return generateRandomString("0123456789", 4) + "XXXX" + generateRandomString("0123456789abcdef", 16);
        return "1002XXXX" + generateRandomString("0123456789abcdef", 16);
    }

    public static String generateRandomString(final String chars, final int length) {
        final StringBuilder sb = new StringBuilder();
        for (int i = 0; i < length; i++) {
            sb.append(chars.charAt(new Random().nextInt(chars.length())));
        }
        return sb.toString();
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        /* 2023-03-27: Attempt to avoid captchas. */
        return 1;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return true;
    }
}
