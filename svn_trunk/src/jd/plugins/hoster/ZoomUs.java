//jDownloader - Downloadmanager
//Copyright (C) 2017  JD-Team support@jdownloader.org
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

import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.parser.html.Form.MethodType;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 50859 $", interfaceVersion = 3, names = {}, urls = {})
public class ZoomUs extends PluginForHost {
    public ZoomUs(PluginWrapper wrapper) {
        super(wrapper);
    }

    private final String PROPERTY_DIRECTURL = "directurl";

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "zoom.us", "zoom.com" });
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
            ret.add("https?://(?:[A-Za-z0-9-]+\\.)?" + buildHostsPatternPart(domains) + "/rec/(?:play|share)/([A-Za-z0-9\\-_\\.]+)");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/en-us/terms.html";
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String linkid = getFID(link);
        if (linkid != null) {
            return this.getHost() + "://" + linkid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    /** See also: https://github.com/Battleman/zoomdl */
    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return requestFileInformation(link, false);
    }

    public AvailableStatus requestFileInformation(final DownloadLink link, final boolean handleDownloadPassword) throws Exception {
        final String ext = ".mp4";
        if (!link.isNameSet()) {
            link.setName(this.getFID(link) + ext);
        }
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String errorFromHTML = br.getRegex("class=\"error-message\"[^>]*>(.*?)<").getMatch(0);
        if (errorFromHTML != null) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String meetingID = br.getRegex("meeting_id\\s*:\\s*\"([^\"]+)\"").getMatch(0);
        if (meetingID == null) {
            meetingID = br.getRegex("meetingID\\s*:\\s*(?:\"|\\')([^\"\\']+)").getMatch(0);
        }
        String fileId = br.getRegex("fileId\\s*:\\s*(?:\"|\\')([^\"\\']+)").getMatch(0);
        String topic = null;
        String displayFileName = null;
        if (fileId == null) {
            if (meetingID == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            Map<String, Object> entries = accessPlayShareOnfo(br, meetingID);
            Map<String, Object> result = (Map<String, Object>) entries.get("result");
            final String componentName = result.get("componentName").toString();
            if (!Boolean.TRUE.equals(result.get("canPlayFromShare"))) {
                /* Usually password protected item */
                if (!componentName.equals("need-password")) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                final String action = result.get("action").toString();
                final String useWhichPasswd = result.get("useWhichPasswd").toString();
                final String sharelevel = result.get("sharelevel").toString();
                final UrlQuery query = new UrlQuery();
                query.add("action", action);
                query.add("sharelevel", sharelevel);
                query.add("useWhichPasswd", useWhichPasswd);
                query.add("clusterId", result.get("clusterId").toString());
                query.add("componentName", componentName);
                query.add("meetingId", Encoding.urlEncode(meetingID));
                query.add("originRequestUrl", Encoding.urlEncode(link.getPluginPatternMatcher()));
                br.getPage("/rec/component-page?" + query.toString());
                if (!passwordRequired(br)) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                /* Do not ask user for password during availablecheck. */
                link.setPasswordProtected(true);
                if (!handleDownloadPassword) {
                    /* Do not ask user for password here but at least find file title if possible. */
                    logger.info("Trying to find title of password protected item");
                    final Form prepwform = new Form();
                    prepwform.setAction("/nws/recording/1.0/validate-context");
                    prepwform.setMethod(MethodType.POST);
                    prepwform.put("meetingId", Encoding.urlEncode(meetingID));
                    prepwform.put("fileId", "");
                    prepwform.put("useWhichPasswd", Encoding.urlEncode(useWhichPasswd));
                    prepwform.put("sharelevel", Encoding.urlEncode(sharelevel));
                    final Browser br2 = br.cloneBrowser();
                    br2.submitForm(prepwform);
                    final Map<String, Object> entries3 = restoreFromString(br2.getRequest().getHtmlCode(), TypeRef.MAP);
                    final Map<String, Object> result3 = (Map<String, Object>) entries3.get("result");
                    if (result3 != null) {
                        topic = (String) result3.get("topic");
                        if (!StringUtils.isEmpty(topic)) {
                            link.setName(topic + ext);
                        }
                    }
                    return AvailableStatus.TRUE;
                }
                String passCode = link.getDownloadPassword();
                if (passCode == null) {
                    passCode = getUserInput("Password?", link);
                }
                final Form pwform = new Form();
                pwform.setAction("/nws/recording/1.0/validate-meeting-passwd");
                pwform.setMethod(MethodType.POST);
                pwform.put("id", Encoding.urlEncode(meetingID));
                pwform.put("passwd", Encoding.urlEncode(passCode));
                pwform.put("action", action);
                String recaptchaV2Response = "";
                if (this.requiresCaptcha(br)) {
                    final String reCaptchaSiteKey = br.getRegex("var gRecaptchaVisible\\s*=\\s*\"([^\"]+)").getMatch(0);
                    if (reCaptchaSiteKey == null) {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                    recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br, reCaptchaSiteKey).getToken();
                }
                pwform.put("recaptcha", Encoding.urlEncode(recaptchaV2Response));
                br.submitForm(pwform);
                final Map<String, Object> entries5 = checkErrorsWebAPI(br);
                /* Correct password! Item should now be downloadable. */
                link.setDownloadPassword(passCode);
                // br.getPage(link.getPluginPatternMatcher());
                entries = accessPlayShareOnfo(br, meetingID);
                result = (Map<String, Object>) entries.get("result");
            }
            final String redirectUrl = result.get("redirectUrl").toString();
            if (StringUtils.isEmpty(redirectUrl)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            br.getPage(redirectUrl);
            fileId = br.getRegex("fileId\\s*:\\s*(?:\"|\\')([^\"\\']+)").getMatch(0);
            if (fileId == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        final String originRequestUrl = link.getPluginPatternMatcher();
        final UrlQuery query0 = new UrlQuery();
        query0.appendEncoded("canPlayFromShare", "true");
        query0.appendEncoded("from", "share_recording_detail");
        query0.appendEncoded("continueMode", "true");
        query0.appendEncoded("componentName", "rec-play");
        query0.appendEncoded("originRequestUrl", originRequestUrl);
        br.getPage("/nws/recording/1.0/play/info/" + fileId + "?" + query0.toString());
        final Map<String, Object> entries2 = checkErrorsWebAPI(br);
        final Map<String, Object> result2 = (Map<String, Object>) entries2.get("result");
        final String componentName = (String) result2.get("componentName");
        if (StringUtils.equalsIgnoreCase(componentName, "vanity-url-check")) {
            if (!DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                // TODO: Finish implementation
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final UrlQuery query = new UrlQuery();
            query.appendEncoded("accessLevel", "");
            query.appendEncoded("vanityURL", result2.get("vanityURL").toString());
            query.appendEncoded("emid", result2.get("emid").toString());
            query.appendEncoded("componentName", "vanity-url-check");
            query.appendEncoded("meetingId", result2.get("emid").toString());
            query.appendEncoded("originRequestUrl", originRequestUrl);
            br.getPage("https://" + result2.get("vanityURL") + "/rec/component-page?" + query.toString());
        }
        link.setProperty(PROPERTY_DIRECTURL, result2.get("viewMp4Url"));
        final Map<String, Object> meet = (Map<String, Object>) result2.get("meet");
        if (meet == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        topic = (String) meet.get("topic");
        final Map<String, Object> recording = (Map<String, Object>) result2.get("recording");
        displayFileName = (String) recording.get("displayFileName");
        final String fileSizeInMB = (String) recording.get("fileSizeInMB");
        if (fileSizeInMB != null) {
            link.setDownloadSize(SizeFormatter.getSize(fileSizeInMB));
        }
        if (topic != null && displayFileName != null) {
            link.setFinalFileName(topic + " - " + displayFileName + ext);
        } else if (displayFileName != null) {
            link.setFinalFileName(displayFileName + ext);
        } else if (topic != null) {
            link.setFinalFileName(topic + ext);
        }
        return AvailableStatus.TRUE;
    }

    private Map<String, Object> accessPlayShareOnfo(final Browser br, final String meetingID) throws IOException, PluginException {
        br.getPage("/nws/recording/1.0/play/share-info/" + Encoding.urlEncode(meetingID));
        return checkErrorsWebAPI(br);
    }

    private Map<String, Object> checkErrorsWebAPI(final Browser br) throws IOException, PluginException {
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        /* E.g. invalid password: {"status":false,"errorCode":3302,"errorMessage":"Falscher Kenncode","result":null} */
        if (Boolean.FALSE.equals(entries.get("status"))) {
            final Number errorCodeO = (Number) entries.get("errorCode");
            final String errorMessage = (String) entries.get("errorMessage");
            if (errorCodeO != null) {
                final int errorCode = errorCodeO.intValue();
                if (errorCode == 3301) {
                    /* {"status":false,"errorCode":3301,"errorMessage":"Diese Aufzeichnung ist nicht vorhanden.","result":null} */
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, errorMessage);
                } else if (errorCode == 3302) {
                    this.getDownloadLink().setDownloadPassword(null);
                    throw new PluginException(LinkStatus.ERROR_RETRY, "Wrong password entered");
                } else {
                    /* Unknown error */
                    throw new PluginException(LinkStatus.ERROR_FATAL, errorMessage);
                }
            }
        }
        final Map<String, Object> result = (Map<String, Object>) entries.get("result");
        if (result != null) {
            final String componentName = (String) result.get("componentName");
            if (StringUtils.equalsIgnoreCase(componentName, "play-forbidden")) {
                /**
                 * E.g. {"status":true,"errorCode":0,"errorMessage":null,"result":{"accessLevel":"","message":"Diese Aufzeichnung ist
                 * abgelaufen.","redirectUrl":"/rec/component-page","componentName":"play-forbidden","meetingId":"REDACTED","needRedirect":true}}
                 */
                /*
                 * E.g. {"status":true,"errorCode":0,"errorMessage":null,"result":{"accessLevel":"","redirectUrl":"/rec/component-page",
                 * "componentName":"play-forbidden","meetingId":
                 * "t4PioJBgTixwd8D5YQJZKh06iVF0wFJs3DZ0YMFpRako760l-KqrIodqNyLS2kqM.6XJax0XtQFFEXjUD","needRedirect":true}}
                 */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
        }
        return entries;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link, true);
        final String dllink = link.getStringProperty(PROPERTY_DIRECTURL);
        if (StringUtils.isEmpty(dllink)) {
            throwExceptionOnCaptcha(br);
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, true, 0);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            if (dl.getConnection().getResponseCode() == 403) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 60 * 60 * 1000l);
            } else if (dl.getConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 60 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error");
            }
        }
        dl.startDownload();
    }

    private void throwExceptionOnCaptcha(final Browser br) throws PluginException {
        if (requiresCaptcha(br)) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Captcha required: Try again later");
        }
    }

    private boolean requiresCaptcha(final Browser br) {
        return br.containsHTML("(?i)needRecaptcha\\s*:\\s*true");
    }

    private boolean passwordRequired(final Browser br) {
        if (br.containsHTML("componentName\\s*:\\s*\"need-password\"")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetPluginGlobals() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}