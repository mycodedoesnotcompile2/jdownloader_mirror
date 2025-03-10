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

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.PluginForDecrypt;
import jd.plugins.components.PluginJSonUtils;

import org.jdownloader.scripting.JavaScriptEngineFactory;

@DecrypterPlugin(revision = "$Revision: 50622 $", interfaceVersion = 3, names = { "younow.com" }, urls = { "https?://(?:www\\.)?younow\\.com/[^/]+(?:/\\d+)?" })
public class YounowComChannel extends PluginForDecrypt {
    public YounowComChannel(PluginWrapper wrapper) {
        super(wrapper);
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String username = new Regex(param.getCryptedUrl(), "younow\\.com/([^/]+)").getMatch(0);
        if (param.getCryptedUrl().matches("https?://(?:www\\.)?younow\\.com/[^/]+/\\d+")) {
            /* Single link that goes into hosterplugin */
            ret.add(this.createDownloadlink(param.getCryptedUrl().replace("younow.com/", "younowdecrypted.com/")));
        } else {
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(username);
            fp.addLinks(ret);
            br.getPage(param.getCryptedUrl());
            br.getPage("https://api.younow.com/php/api/broadcast/info/user=" + username + "/curId=0");
            int addedlinks = 0;
            int addedlinks_temp = 0;
            int maxItemsPerPage = 20;
            String userid = this.br.getRegex("\"userId\":\"(\\d+)\"").getMatch(0);
            if (userid == null) {
                userid = PluginJSonUtils.getJsonValue(br, "userId");
            }
            String timestampValue = "0";
            if (inValidate(userid)) {
                /* Probably that user does not exist */
                return ret;
            }
            final ArrayList<String> dupecheck = new ArrayList<String>();
            boolean done = false;
            boolean hasMore = false;
            long lastDateUploaded = 0;
            do {
                if (this.isAbort()) {
                    return ret;
                }
                addedlinks_temp = 0;
                if (addedlinks > 0) {
                    timestampValue = Long.toString(lastDateUploaded);
                }
                this.br.getHeaders().put("Accept", "application/json, text/plain, */*");
                this.br.getHeaders().put("Referer", "https://www.younow.com/" + username + "/channel");
                // this.br.getHeaders().put("Origin", "https://www.younow.com");
                // br.getPage("https://cdn2.younow.com/php/api/post/getBroadcasts/channelId=" + userid + "/startFrom=" + (addedlinks + 1));
                br.getPage("https://cdn.younow.com/php/api/moment/profile/channelId=" + userid + "/createdBefore=" + timestampValue + "/records=" + maxItemsPerPage);
                Map<String, Object> entries = (Map<String, Object>) JavaScriptEngineFactory.jsonToJavaObject(br.toString());
                hasMore = ((Boolean) entries.get("hasMore")).booleanValue();
                final List<Object> ressourcelist = (List) entries.get("items");
                if (ressourcelist == null) {
                    break;
                }
                for (final Object objecto : ressourcelist) {
                    addedlinks++;
                    addedlinks_temp++;
                    entries = (Map<String, Object>) objecto;
                    final String type = (String) entries.get("type");
                    // final long mediatype = JavaScriptEngineFactory.toLong(entries.get("type"), 0);
                    if (type == null || !type.equalsIgnoreCase("collection")) {
                        /* Skip non-video-content */
                        continue;
                    }
                    final long broadcastID = JavaScriptEngineFactory.toLong(entries.get("broadcastId"), 0);
                    lastDateUploaded = JavaScriptEngineFactory.toLong(entries.get("created"), 0);
                    String broadcasttitle = jd.plugins.hoster.YounowCom.getbroadcastTitle(entries);
                    if (broadcastID == 0 || lastDateUploaded == 0) {
                        /* This should never happen! */
                        done = true;
                        break;
                    }
                    if (dupecheck.contains(Long.toString(broadcastID))) {
                        /* Same content again? We're done! */
                        done = true;
                        break;
                    }
                    dupecheck.add(Long.toString(broadcastID));
                    final DownloadLink dl = this.createDownloadlink("https://www.younowdecrypted.com/" + username + "/" + broadcastID);
                    dl.setContainerUrl(param.getCryptedUrl());
                    /* There are no individual links to open specific videos in browser. */
                    dl.setContentUrl(param.getCryptedUrl());
                    String temp_filename;
                    if (!inValidate(broadcasttitle)) {
                        /* We might not be able to easily get this information later --> Save it on our DownloadLink */
                        dl.setProperty("decryptedbroadcasttitle", broadcasttitle);
                        temp_filename = username + "_" + broadcastID + "_" + broadcasttitle;
                    } else if (lastDateUploaded > 0) {
                        final SimpleDateFormat df = new SimpleDateFormat("MMMM dd, yyyy", Locale.ENGLISH);
                        final String date = df.format(new Date(lastDateUploaded * 1000));
                        temp_filename = username + "_" + broadcastID + " - " + date;
                    } else {
                        temp_filename = username + "_" + broadcastID;
                    }
                    temp_filename = temp_filename + ".mp4";
                    dl.setName(temp_filename);
                    dl.setAvailable(true);
                    dl._setFilePackage(fp);
                    ret.add(dl);
                    distribute(dl);
                }
            } while (addedlinks_temp >= 1 && !done);
            if (ret.isEmpty() && !hasMore) {
                logger.info("Channel is empty / does not contain any recorded and downloadable content");
            }
        }
        return ret;
    }

    /**
     * Validates string to series of conditions, null, whitespace, or "". This saves effort factor within if/for/while statements
     *
     * @param s
     *            Imported String to match against.
     * @return <b>true</b> on valid rule match. <b>false</b> on invalid rule match.
     * @author raztoki
     */
    protected boolean inValidate(final String s) {
        if (s == null || s.matches("\\s+") || s.equals("")) {
            return true;
        } else {
            return false;
        }
    }
}
