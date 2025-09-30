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
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.captcha.v2.challenge.hcaptcha.CaptchaHelperHostPluginHCaptcha;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51587 $", interfaceVersion = 3, names = {}, urls = {})
public class SwiftuploadsCom extends PluginForHost {
    public SwiftuploadsCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        // br.setCookie(getHost(), "adb", "0");
        br.setCookie(getHost(), "adb", "1");
        br.setCookie(getHost(), "gdpr_cookie", "1");
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/terms-conditions";
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "swiftuploads.com" });
        ret.add(new String[] { "uptoearn.xyz" });
        ret.add(new String[] { "uploadzap.com" });
        ret.add(new String[] { "akirabox.com", "akirabox.to" });
        ret.add(new String[] { "filearn.top" });
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
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/([A-Za-z0-9]+)/file");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 0;
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

    private String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    /** Docs: https://akirabox.com/api */
    private boolean supports_linkcheck_api() {
        return getHost().equals("akirabox.com");
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        if (this.getPluginEnvironment() == PluginEnvironment.LINK_CHECK && this.supports_linkcheck_api()) {
            return requestFileInformationAPI(link);
        } else {
            return requestFileInformationWebsite(link);
        }
    }

    private AvailableStatus requestFileInformationWebsite(final DownloadLink link) throws IOException, PluginException {
        if (!link.isNameSet()) {
            /* Fallback */
            link.setName(this.getFID(link));
        }
        this.setBrowserExclusive();
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String filename = br.getRegex("span class=\"text-break h3\"[^>]*>([^<]+)").getMatch(0);
        String filesize = br.getRegex(">\\s*Size\\s*:\\s*</span>([^<]+)</p>").getMatch(0);
        if (filename != null) {
            filename = Encoding.htmlDecode(filename).trim();
            link.setName(filename);
        } else {
            logger.warning("Failerd to find filename");
        }
        if (filesize != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesize));
        } else {
            logger.warning("Failed to find filesize");
        }
        return AvailableStatus.TRUE;
    }

    private AvailableStatus requestFileInformationAPI(final DownloadLink link) throws IOException, PluginException {
        if (!link.isNameSet()) {
            /* Fallback */
            link.setName(this.getFID(link));
        }
        this.setBrowserExclusive();
        br.getPage("https://" + getHost() + "/api/files?url=" + Encoding.urlEncode(link.getPluginPatternMatcher()));
        if (br.getHttpConnection().getResponseCode() == 404) {
            /* E.g. {"status":404,"message":"File not found"} */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        link.setFinalFileName(entries.get("name").toString());
        String filesizeStr = (String) entries.get("size");
        if (filesizeStr != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesizeStr));
        } else {
            logger.warning("Failed to find filesize");
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link);
    }

    private void handleDownload(final DownloadLink link) throws Exception, PluginException {
        final String directlinkproperty = "free_directlink";
        final String storedDirecturl = link.getStringProperty(directlinkproperty);
        final String dllink;
        if (storedDirecturl != null) {
            logger.info("Re-using stored directurl: " + storedDirecturl);
            dllink = storedDirecturl;
        } else {
            requestFileInformation(link);
            final Form continueform = br.getFormbyKey("method");
            if (continueform == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            br.setCookie(br.getHost(), "adb", "1");
            br.getHeaders().put("Origin", "https://www." + br.getHost());
            br.submitForm(continueform);
            Form dlform2 = br.getFormbyProperty("id", "down_2Form");
            if (dlform2 == null) {
                /* Fallback */
                final Form[] forms = br.getForms();
                if (forms != null && forms.length == 1) {
                    dlform2 = forms[0];
                }
            }
            if (dlform2 == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final String downloadConfigJson = br.getRegex("const downloadConfig = (\\{.*?\\})\\s+").getMatch(0);
            if (downloadConfigJson == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final Map<String, Object> dlconfig = JSonStorage.restoreFromString(downloadConfigJson, TypeRef.MAP);
            final Number dlconfig_captcha = (Number) dlconfig.get("captcha");
            if (dlconfig_captcha != null && dlconfig_captcha.intValue() == 1) {
                // e.g. uptoearn.xyz
                if (br.containsHTML("class=\"h-captcha\"")) {
                    /* 2025-01-07: uploadzap.com */
                    final String hcaptchaResponse = new CaptchaHelperHostPluginHCaptcha(this, br).getToken();
                    dlform2.put("g-recaptcha-response", Encoding.urlEncode(hcaptchaResponse));
                    dlform2.put("h-captcha-response", Encoding.urlEncode(hcaptchaResponse));
                } else {
                    final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br).getToken();
                    dlform2.put("g-recaptcha-response", Encoding.urlEncode(recaptchaV2Response));
                }
            }
            br.submitForm(dlform2);
            final String csrftoken = br.getRegex("name=\"csrf-token\" content=\"([^\"]+)\"").getMatch(0);
            if (csrftoken == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final String fid = this.getFID(link);
            final Browser brc = br.cloneBrowser();
            brc.getHeaders().put("Accept", "application/json, text/javascript, */*; q=0.01");
            brc.getHeaders().put("x-requested-with", "XMLHttpRequest");
            brc.getHeaders().put("Origin", "https://www." + br.getHost());
            brc.getHeaders().put("x-csrf-token", csrftoken);
            brc.setCookie(br.getHost(), "rqf", fid);
            brc.postPageRaw("/" + fid + "/file/generate", "");
            final Map<String, Object> entries = JSonStorage.restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            dllink = (String) entries.get("download_link");
            if (StringUtils.isEmpty(dllink)) {
                logger.warning("Failed to find final downloadurl");
                String msg = (String) entries.get("message");
                if (msg == null) {
                    msg = (String) entries.get("error");
                }
                if (msg != null) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg);
                } else {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
        }
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
            this.handleConnectionErrors(br, dl.getConnection());
        } catch (final Exception e) {
            if (storedDirecturl != null) {
                link.removeProperty(directlinkproperty);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Stored directurl expired", e);
            } else {
                throw e;
            }
        }
        if (storedDirecturl == null) {
            link.setProperty(directlinkproperty, dl.getConnection().getURL().toExternalForm());
        }
        dl.startDownload();
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        return true;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}