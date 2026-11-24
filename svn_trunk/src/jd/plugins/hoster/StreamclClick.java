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
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;

import javax.script.ScriptEngine;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.encoding.Base64;
import org.jdownloader.downloader.hls.HLSDownloader;
import org.jdownloader.plugins.components.hls.HlsContainer;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

/**
 * Hoster plugin for the video player/streaming host streamcl.click.
 * <p>
 * Relation to the JavCL crawler ({@link jd.plugins.decrypter.JavCL}): javcl.com does not host any videos itself. Every "server" button on a
 * javcl.com post contains an obfuscated embed link. JavCL decodes these and hands the resulting URLs to JDownloader. For "SERVER 1" this is
 * always a https://streamcl.click/play/&lt;slug&gt;.html link, which is handled by this plugin. Other servers (streamtape, emturbovid, vtbe
 * ...) are handled by their own plugins.
 * <p>
 * The crawler also sets the property {@link #PROPERTY_JAVCL_NAME} (title of the javcl.com post, with a suffix if the post contains multiple
 * links) and the referrer URL (the javcl.com post). This plugin uses the property for the filename, because the title delivered by
 * streamcl.click itself is only the short product code (e.g. "FLAV-432"). The referrer is not required: the API works without it, so
 * streamcl.click links also work when added directly.
 * <p>
 * How it works: the embed page POSTs "slug=&lt;id&gt;" to /stream/play and gets back JSON with "name", "poster" and "data". "data" is the
 * URL of the HLS master playlist in encrypted form, see {@link #decryptData(String, String)}. The video segments are delivered as fake PNG
 * images (the PNG header is skipped by the {@link HLSDownloader}).
 */
@HostPlugin(revision = "$Revision: 54104 $", interfaceVersion = 3, names = {}, urls = {})
public class StreamclClick extends PluginForHost {
    /* Set by the crawler plugin JavCL (see class comment) */
    public static final String             PROPERTY_JAVCL_NAME          = "javclName";
    /* Key of the player JavaScript used to decrypt the "data" field. It is extracted at runtime and cached for 24 hours. */
    private static AtomicReference<String> CACHED_DECRYPT_KEY           = new AtomicReference<String>();
    private static AtomicLong              CACHED_DECRYPT_KEY_TIMESTAMP = new AtomicLong(-1);
    /* JSON response of /stream/play, set in requestFileInformation and reused in handleFree to avoid parsing it twice */
    private Map<String, Object>            entries                      = null;

    public StreamclClick(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX, LazyPlugin.FEATURE.VIDEO_STREAMING, LazyPlugin.FEATURE.FAVICON };
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "streamcl.click" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/play/([A-Za-z0-9]+)\\.html");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public Object getFavIcon(final String host) throws IOException {
        /* streamcl.click is only an embedded player without an own website: Use the favicon of javcl.com (see class comment). */
        return "javcl.com";
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/";
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

    private static final String extDefault = ".mp4";

    @Override
    protected String getDefaultFileName(final DownloadLink link) {
        /* Title from the javcl.com post if the link was added by the JavCL crawler, else the slug */
        final String title = link.getStringProperty(PROPERTY_JAVCL_NAME);
        if (!StringUtils.isEmpty(title)) {
            return title + extDefault;
        } else {
            return this.getFID(link) + extDefault;
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        entries = null;
        final String fid = this.getFID(link);
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        br.getHeaders().put("X-Requested-With", "XMLHttpRequest");
        br.getHeaders().put("Referer", link.getPluginPatternMatcher());
        br.postPage("https://" + this.getHost() + "/stream/play", "slug=" + fid);
        if (br.getHttpConnection().getResponseCode() == 404 || !br.getRequest().getHtmlCode().startsWith("{")) {
            /* Unknown slug: The server does not return JSON in this case. */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final String title;
        if (link.hasProperty(PROPERTY_JAVCL_NAME)) {
            /* Title from the javcl.com post, see class comment */
            title = link.getStringProperty(PROPERTY_JAVCL_NAME);
        } else {
            /* Only the short product code e.g. "FLAV-432" */
            title = (String) entries.get("name");
        }
        if (!StringUtils.isEmpty(title)) {
            link.setFinalFileName(title + extDefault);
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        final String hlsMaster = decryptData(entries.get("data").toString(), getDecryptKey(link));
        if (!hlsMaster.startsWith("http")) {
            /* Decryption failed e.g. because the player algorithm has been changed. */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to decrypt stream URL");
        }
        br.getHeaders().remove("X-Requested-With");
        br.getPage(hlsMaster);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final HlsContainer hlsbest = HlsContainer.findBestVideoByBandwidth(HlsContainer.getHlsQualities(this.br));
        if (hlsbest == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find HLS qualities");
        }
        checkFFmpeg(link, "Download a HLS Stream");
        dl = new HLSDownloader(link, br, hlsbest.getStreamURL());
        dl.startDownload();
    }

    /**
     * Port of the decryption of the player JavaScript: The "data" field is Base64. For each byte at position i, i%256 is subtracted and the
     * result is XORed with the key character at position i%keyLength.
     */
    private static String decryptData(final String data, final String key) {
        final byte[] bytes = Base64.decode(data);
        final StringBuilder sb = new StringBuilder();
        for (int i = 0; i < bytes.length; i++) {
            final int value = (bytes[i] & 0xff) - (i % 256);
            sb.append((char) ((value ^ key.charAt(i % key.length())) & 0xffff));
        }
        return sb.toString();
    }

    /**
     * Returns the key of the player JavaScript. The key is extracted from the embed page at runtime and cached for 24 hours. Only called
     * when it is needed, see {@link #handleFree(DownloadLink)}.
     * <p>
     * The player script is obfuscated and builds the key from an encoded string array, so it is not possible to simply grep it. Instead,
     * the script is executed in a JavaScript engine (with minimal browser stubs) and its own decrypt function client.inits is used as an
     * oracle: for each position i it computes (charCode(i) - i%256) XOR key[i%keyLength]. If the input contains the character i%256 at
     * position i, the first part is 0 and the result is the key repeated.
     */
    private String getDecryptKey(final DownloadLink link) throws Exception {
        synchronized (CACHED_DECRYPT_KEY) {
            final String cachedKey = CACHED_DECRYPT_KEY.get();
            if (cachedKey != null && Time.systemIndependentCurrentJVMTimeMillis() - CACHED_DECRYPT_KEY_TIMESTAMP.get() < TimeUnit.HOURS.toMillis(24)) {
                return cachedKey;
            }
            final Browser brc = br.cloneBrowser();
            brc.getHeaders().remove("X-Requested-With");
            brc.getPage(link.getPluginPatternMatcher());
            final String script = brc.getRegex("<script>(function _0x[a-f0-9]+\\(\\)\\s*\\{.*?)</script>").getMatch(0);
            if (script == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find player script");
            }
            final String host = getHost();
            final StringBuilder sb = new StringBuilder();
            /* Minimal stubs of the browser environment used by the player script */
            sb.append("var window = this;");
            sb.append("window.location = {origin:'https://" + host + "', href:'https://" + host + "/', hostname:'" + host + "', pathname:'/', search:''};");
            sb.append("var document = {location:window.location, cookie:'', getElementById:function(){return {};}, querySelector:function(){return {};}};");
            sb.append("var navigator = {userAgent:'Mozilla/5.0'};");
            sb.append("var console = {log:function(){}, clear:function(){}};");
            sb.append("var $ = function(){return {};};");
            sb.append("var devtoolsDetector = {addListener:function(){}, launch:function(){}};");
            /*
             * The JavaScript engine (Rhino) does not support let/const properly and does not know async arrow functions (the player setup
             * contains one, it is not needed for the key).
             */
            sb.append(script.replaceAll("\\b(?:const|let) ", "var ").replaceAll("async\\s+(_0x[0-9a-f]+)\\s*=>\\s*\\{", "function($1){"));
            final ScriptEngine engine = JavaScriptEngineFactory.getScriptEngineManager(this).getEngineByName("javascript");
            try {
                engine.eval(sb.toString());
            } catch (final Exception e) {
                /* Expected: The script fails after the definition of "client" because of the missing browser environment (player setup). */
                logger.log(e);
            }
            try {
                engine.eval("client.atoburl = function(s) { return s; }; var probe = ''; for (var i = 0; i < 1024; i++) { probe += String.fromCharCode(i % 256); } var probeResult = client.inits(probe);");
            } catch (final Exception e) {
                logger.log(e);
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to extract decrypt key");
            }
            final Object probeResult = engine.get("probeResult");
            if (probeResult == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to extract decrypt key");
            }
            /* The result is the key repeated: Find the shortest repeating unit. */
            final String repeated = probeResult.toString();
            String key = null;
            for (int period = 1; period <= repeated.length() / 2; period++) {
                boolean matches = true;
                for (int i = period; i < repeated.length(); i++) {
                    if (repeated.charAt(i) != repeated.charAt(i - period)) {
                        matches = false;
                        break;
                    }
                }
                if (matches) {
                    key = repeated.substring(0, period);
                    break;
                }
            }
            if (StringUtils.isEmpty(key)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to extract decrypt key");
            }
            logger.info("Extracted decrypt key: " + key);
            CACHED_DECRYPT_KEY.set(key);
            CACHED_DECRYPT_KEY_TIMESTAMP.set(Time.systemIndependentCurrentJVMTimeMillis());
            return key;
        }
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }
}
