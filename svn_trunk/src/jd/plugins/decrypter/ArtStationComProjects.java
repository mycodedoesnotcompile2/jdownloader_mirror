package jd.plugins.decrypter;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import org.appwork.utils.Regex;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 54120 $", interfaceVersion = 3, names = { "artstation.com" }, urls = { "https?://[a-z0-9\\-]+\\.artstation\\.com/(projects/[A-Z0-9]+|(?!artwork)$)" })
public class ArtStationComProjects extends PluginForDecrypt {
    public ArtStationComProjects(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public ArrayList<DownloadLink> decryptIt(CryptedLink parameter, ProgressController progress) throws Exception {
        final String artwork = new Regex(parameter.getCryptedUrl(), "/projects/([A-Z0-9]+)").getMatch(0);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        if (artwork != null) {
            ret.add(createDownloadlink("https://www.artstation.com/artwork/" + artwork));
        } else {
            br.setFollowRedirects(true);
            final Set<String> dups = new HashSet<String>();
            br.getPage(parameter.getCryptedUrl());
            String[] projects = br.getRegex("/projects/([A-Z0-9]+)").getColumn(0);
            if (projects != null) {
                for (final String project : projects) {
                    if (dups.add(project)) {
                        ret.add(createDownloadlink("https://www.artstation.com/artwork/" + project));
                    }
                }
            }
            br.getPage("/projects");
            /* Projects == "artwork" URLs */
            projects = br.getRegex("/projects/([A-Z0-9]+)").getColumn(0);
            if (projects != null) {
                for (final String project : projects) {
                    if (dups.add(project)) {
                        ret.add(createDownloadlink("https://www.artstation.com/artwork/" + project));
                    }
                }
            }
        }
        return ret;
    }
}
