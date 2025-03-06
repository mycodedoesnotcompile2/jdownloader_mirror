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

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.appwork.utils.StringUtils;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperCrawlerPluginRecaptchaV2;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 50745 $", interfaceVersion = 3, names = {}, urls = {})
public class SafegoCc extends PluginForDecrypt {
    public SafegoCc(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "safego.cc" });
        return ret;
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/safe\\.php\\?url=([a-zA-Z0-9_/\\+\\=\\-%]+)");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        br.getPage(param.getCryptedUrl());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String finallink = this.findFinalLink(br);
        if (finallink == null) {
            /* Captcha required */
            Form form = br.getFormByInputFieldKeyValue("id", "comment_form");
            if (form == null) {
                form = br.getForm(0);
            }
            final String imagecaptchakey = "captch4";
            if (CaptchaHelperCrawlerPluginRecaptchaV2.containsRecaptchaV2Class(br)) {
                final String recaptchaV2Response = new CaptchaHelperCrawlerPluginRecaptchaV2(this, br).getToken();
                form.put("g-recaptcha-response", Encoding.urlEncode(recaptchaV2Response));
            } else {
                /* Maybe simple image captcha needed. */
                final String base64captchaimage = br.getRegex("img src=\"data:image/png;base64,([^\"]+)").getMatch(0);
                if (form == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                } else if (!form.hasInputFieldByName(imagecaptchakey)) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                } else if (base64captchaimage == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                final String code = this.getCaptchaCodeBase64ImageString(base64captchaimage, param);
                if (!code.matches("\\d+")) {
                    throw new PluginException(LinkStatus.ERROR_CAPTCHA);
                }
                form.put(imagecaptchakey, code);
            }
            br.submitForm(form);
            finallink = this.findFinalLink(br);
            if (finallink == null) {
                if (br.containsHTML("Proceed to video")) {
                    /* Item is broken or offline e.g. https://safego.cc/safe.php?url=bla */
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                } else if (br.containsHTML(">\\s*Incorrect " + imagecaptchakey)) {
                    throw new PluginException(LinkStatus.ERROR_CAPTCHA);
                } else {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
        }
        ret.add(createDownloadlink(finallink));
        return ret;
    }

    private String getCaptchaCodeBase64ImageString(final String captchaImageBase64, final CryptedLink param) throws Exception {
        if (StringUtils.isEmpty(captchaImageBase64)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "No captcha base64 string given");
        }
        final byte[] image = org.appwork.utils.encoding.Base64.decode(captchaImageBase64);
        if (image == null || image.length == 0) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Given String is not a base64 encoded String!");
        }
        final File captchaFile = getLocalCaptchaFile();
        if (captchaFile.isFile()) {
            if (captchaFile.exists() && !captchaFile.delete()) {
                throw new IOException("Could not overwrite file: " + captchaFile);
            }
        }
        final File parentFile = captchaFile.getParentFile();
        if (parentFile != null && !parentFile.exists()) {
            parentFile.mkdirs();
        }
        FileOutputStream fos = null;
        boolean okay = false;
        try {
            captchaFile.createNewFile();
            fos = new FileOutputStream(captchaFile);
            fos.write(image, 0, image.length);
            okay = true;
        } catch (IOException e) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, null, e);
        } finally {
            try {
                fos.close();
            } catch (final Throwable e) {
            }
            if (okay == false) {
                captchaFile.delete();
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to write captchafile");
            }
        }
        return getCaptchaCode(captchaFile, param);
    }

    private String findFinalLink(final Browser br) {
        return br.getRegex("href=\"(https?://[^\"]+)\"[^>]*>\\s*<button[^>]*>\\s*Proceed to video").getMatch(0);
    }
}
