package jd.plugins.decrypter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.Request;
import jd.http.requests.PostRequest;
import jd.nutils.encoding.Encoding;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.components.PluginJSonUtils;
import jd.plugins.hoster.DirectHTTP;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.scripting.JavaScriptEngineFactory;

@DecrypterPlugin(revision = "$Revision: 53387 $", interfaceVersion = 2, names = { "thingiverse.com" }, urls = { "https?://(www\\.)?thingiverse\\.com/(thing:\\d+|make:\\d+|[^/]+/(about|designs|collections(/[^/]+)?|makes|likes|things)|groups/[^/]+(/(things|about))?)" })
public class ThingiverseCom extends PluginForDecrypt {
    public ThingiverseCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    private final String API_BASE = "https://www.thingiverse.com/api";

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        br.setFollowRedirects(true);
        String fpName = null;
        String description = null;
        final String thingID = new Regex(param.getCryptedUrl(), "(?i)thing:(\\d+).*").getMatch(0);
        if (new Regex(param.getCryptedUrl(), "/([^/]+/(about|designs|collections(/[^/]+)?|makes|likes|things)|groups/[^/]+(/(things|about))?)").patternFind()) {
            br.getPage(param.getCryptedUrl());
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final String[] links = getAPISearchLinks(br);
            if (links != null && links.length > 0) {
                for (String link : links) {
                    ret.add(createDownloadlink(br.getURL(link).toExternalForm()));
                }
            }
        } else if (thingID != null) {
            /* a thing */
            return crawlThingAPI(thingID);
        } else if (StringUtils.containsIgnoreCase(param.getCryptedUrl(), "/make:")) {
            // a make
            final String contentID = new Regex(param.getCryptedUrl(), "(\\d+)$").getMatch(0);
            if (contentID == null) {
                /* Developer mistake */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final String authtoken = this.getAuthToken(this.br);
            if (authtoken == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final Request request = br.createGetRequest(API_BASE + "/copies/" + contentID);
            request.getHeaders().put(HTTPConstants.HEADER_REQUEST_AUTHORIZATION, "Bearer " + authtoken);
            request.getHeaders().put(HTTPConstants.HEADER_REQUEST_ORIGIN, "https://www.thingiverse.com/");
            br.getPage(request);
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final String thingURL = JavaScriptEngineFactory.walkJson(entries, "thing/public_url").toString();
            if (StringUtils.isEmpty(thingURL)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            /* This result will go back into this crawler to find the .zip files. */
            ret.add(createDownloadlink(thingURL));
        } else {
            /* Unsupported URL --> Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final FilePackage fp = FilePackage.getInstance();
        if (fpName != null) {
            fp.setName(Encoding.htmlDecode(fpName).trim());
        }
        if (!StringUtils.isEmpty(description)) {
            fp.setComment(description);
        }
        fp.addLinks(ret);
        return ret;
    }

    private ArrayList<DownloadLink> crawlThingAPI(final String thingID) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String authtoken = this.getAuthToken(this.br);
        if (authtoken == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final Request request = br.createGetRequest(API_BASE + "/things/" + thingID);
        request.getHeaders().put(HTTPConstants.HEADER_REQUEST_AUTHORIZATION, "Bearer " + authtoken);
        request.getHeaders().put(HTTPConstants.HEADER_REQUEST_ORIGIN, "https://www.thingiverse.com/");
        br.getPage(request);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final String fpName = entries.get("name").toString();
        final String description = (String) entries.get("description");
        final Map<String, Object> zip_data = (Map<String, Object>) entries.get("zip_data");
        final String[] targetmapnames = new String[] { "files", "images" };
        for (final String targetmapname : targetmapnames) {
            final List<Map<String, Object>> fileitems = (List<Map<String, Object>>) zip_data.get(targetmapname);
            if (fileitems == null || fileitems.isEmpty()) {
                continue;
            }
            for (final Map<String, Object> fileitem : fileitems) {
                final DownloadLink file = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(fileitem.get("url").toString()));
                file.setName(fileitem.get("name").toString());
                file.setAvailable(true);
                ret.add(file);
            }
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(Encoding.htmlDecode(fpName).trim());
        if (!StringUtils.isEmpty(description)) {
            fp.setComment(description);
        }
        fp.addLinks(ret);
        return ret;
    }

    private static AtomicReference<String> AUTH_TOKEN = new AtomicReference<String>();

    private String getAuthToken(final Browser br) throws Exception {
        String ret = AUTH_TOKEN.get();
        if (ret != null) {
            return ret;
        }
        final Browser brc = br.cloneBrowser();
        brc.getPage("https://www.thingiverse.com/_next/static/chunks/8894.de789bbe5b3daaa5.js");
        ret = brc.getRegex("\"\".concat\\(\"([a-fA-F0-9]{32})\"\\)").getMatch(0);
        if (StringUtils.isEmpty(ret)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else {
            AUTH_TOKEN.set(ret);
            return ret;
        }
    }

    private String[] getAPISearchLinks(Browser br) throws Exception {
        String[] results = null;
        String sourceData = PluginJSonUtils.getJsonNested(br, "data");
        sourceData = new Regex(sourceData, "(\\{[^\\}]+\\})").getMatch(0);
        if (sourceData != null) {
            HashMap<String, String> searchValues = restoreFromString(sourceData, TypeRef.HASHMAP_STRING);
            if (searchValues != null && searchValues.keySet().size() > 0) {
                searchValues.put("page", "1");
                searchValues.put("per_page", "999999999");
                Browser br2 = br.cloneBrowser();
                String postURL = br2.getURL(searchValues.get("source")).toExternalForm();
                PostRequest post = new PostRequest(postURL);
                UrlQuery postQuery = new UrlQuery();
                for (String key : searchValues.keySet()) {
                    post.addVariable(key, String.valueOf(searchValues.get(key)));
                    postQuery.add(key, String.valueOf(searchValues.get(key)));
                }
                post.getHeaders().put("Origin", "https://www.thingiverse.com");
                post.getHeaders().put("X-Requested-With", "XMLHttpRequest");
                post.setContentType("application/x-www-form-urlencoded; charset=UTF-8");
                br2.setRequest(post);
                br2.postPage(postURL, postQuery);
                results = br2.getRegex("a href=\"([^\"]+)\" class=\"card-img-holder").getColumn(0);
            }
        }
        return results;
    }
}