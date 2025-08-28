//jDownloader - Downloadmanager
//Copyright (C) 2013  JD-Team support@jdownloader.org
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

import jd.PluginWrapper;
import jd.http.Browser;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.PluginException;

import org.appwork.utils.Exceptions;

@HostPlugin(revision = "$Revision: 51391 $", interfaceVersion = 3, names = {}, urls = {})
public class KernelVideoSharingComV2HostsDefault extends KernelVideoSharingComV2 {
    public KernelVideoSharingComV2HostsDefault(final PluginWrapper wrapper) {
        super(wrapper);
        if ("bootyexpo.net".equals(getHost())) {
            // account required for higher qualities, see https://board.jdownloader.org/showthread.php?t=97550
            enablePremium();
        }
    }

    /** Add all KVS hosts to this list that fit the main template without the need of ANY changes to this class. */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "sleazyneasy.com" });
        ret.add(new String[] { "pornwhite.com" });
        ret.add(new String[] { "voyeurhit.com" });
        ret.add(new String[] { "hotmovs.com", "hotmovs.tube" });
        ret.add(new String[] { "porndr.com" });
        ret.add(new String[] { "momvids.com", "motherporno.com" });
        ret.add(new String[] { "wankoz.com" });
        /* zbporn.com belongs to the same "porn network" as sexvid.xxx. */
        ret.add(new String[] { "zbporn.com" });
        ret.add(new String[] { "xozilla.com", "xozilla.xxx" });
        ret.add(new String[] { "femdomtb.com" });
        ret.add(new String[] { "fetishshrine.com" });
        ret.add(new String[] { "sheshaft.com" });
        ret.add(new String[] { "yeswegays.com" });
        ret.add(new String[] { "analdin.com", "analdin.xxx" });
        ret.add(new String[] { "tryboobs.com" });
        ret.add(new String[] { "vikiporn.com" });
        ret.add(new String[] { "katestube.com" });
        ret.add(new String[] { "bravoteens.com" });
        ret.add(new String[] { "onlygayvideo.com" });
        ret.add(new String[] { "mylust.com" });
        ret.add(new String[] { "yourporngod.com" });
        ret.add(new String[] { "everydayporn.co" });
        ret.add(new String[] { "upornia.com", "upornia.tube" });
        ret.add(new String[] { "vr.pornhat.com" });
        ret.add(new String[] { "xxxymovies.com" });
        ret.add(new String[] { "needgayporn.com" });
        ret.add(new String[] { "submityourflicks.com" });
        ret.add(new String[] { "pornicom.com" });
        ret.add(new String[] { "tubepornclassic.com" });
        ret.add(new String[] { "deviants.com" });
        ret.add(new String[] { "faapy.com" });
        ret.add(new String[] { "nudez.com" });
        ret.add(new String[] { "pornomovies.com" });
        ret.add(new String[] { "3movs.com" });
        /* 2021-01-18 */
        ret.add(new String[] { "gaysearch.com" });
        /* 2021-04-06 */
        ret.add(new String[] { "punishbang.com" });
        /* 2021-09-06 */
        ret.add(new String[] { "pornfd.com" });
        /* 2022-01-17 */
        ret.add(new String[] { "fpo.xxx" });
        /* 2022-03-24 */
        ret.add(new String[] { "footstockings.com" });
        /* 2022-03-24 */
        ret.add(new String[] { "wild-pornstars.com" });
        /* 2022-03-28 */
        ret.add(new String[] { "4kporn.xxx" });
        /* 2022-03-28 */
        ret.add(new String[] { "bigwank.com" });
        /* 2022-03-28 */
        ret.add(new String[] { "xxxshake.com" });
        /* 2022-03-28 */
        ret.add(new String[] { "xgroovy.com" });
        /* 2022-05-02 */
        ret.add(new String[] { "heavyfetish.com" });
        /* 2022-06-05 */
        ret.add(new String[] { "fapcat.com" });
        /* 2022-07-12 */
        ret.add(new String[] { "hoes.tube" });
        /* 2022-07-12 */
        ret.add(new String[] { "fapnado.xxx" });
        /* 2022-07-12 */
        ret.add(new String[] { "maturetubehere.com" });
        /* 2022-07-12 */
        ret.add(new String[] { "homemade.xxx" });
        /* 2022-07-12 */
        ret.add(new String[] { "xhand.com" });
        /* 2022-09-28 */
        ret.add(new String[] { "pornditt.com" });
        /* 2022-10-17 */
        ret.add(new String[] { "donkparty.com" });
        /* 2022-11-14 */
        ret.add(new String[] { "femefun.com" });
        /* 2022-11-21 */
        ret.add(new String[] { "pervertium.com" });
        /* 2022-11-25 */
        ret.add(new String[] { "havefunporn.com" });
        /* 2022-12-22 */
        ret.add(new String[] { "bdsmx.tube" });
        /* 2022-01-16 */
        ret.add(new String[] { "thebussybandit.com" });
        /* 2023-05-16 */
        ret.add(new String[] { "baddiesonly.tv" });
        ret.add(new String[] { "shemale6.com" });
        /* 2023-07-13 */
        ret.add(new String[] { "urgayporn.com" });
        /* 2023-07-31 */
        ret.add(new String[] { "gotgayporn.com", "bonertube.com" });
        ret.add(new String[] { "ah-me.com" });
        ret.add(new String[] { "pornve.com" });
        ret.add(new String[] { "thegay.com" });
        ret.add(new String[] { "mrgay.com" });
        ret.add(new String[] { "gayboystube.com" });
        ret.add(new String[] { "pornwex.tv" });
        ret.add(new String[] { "x-tg.tube" });
        /* 2024-10-21 */
        ret.add(new String[] { "severeporn.com" });
        ret.add(new String[] { "x-fetish.tube" });
        ret.add(new String[] { "gayporntube.com" });
        ret.add(new String[] { "cartoonporn.com" });
        ret.add(new String[] { "cloudbate.com" });
        ret.add(new String[] { "xhand.net" });
        ret.add(new String[] { "megatube.xxx" });
        ret.add(new String[] { "cluset.com" });
        ret.add(new String[] { "bootyexpo.net" });
        ret.add(new String[] { "xxbrits.com" });
        return ret;
    }

    @Override
    protected ArrayList<String> getDeadDomains() {
        final ArrayList<String> domains = new ArrayList<String>();
        domains.add("motherporno.com"); // 2025-01-07
        return domains;
    }

    @Override
    protected String getDllink(final DownloadLink link, final Browser br) throws PluginException, IOException {
        PluginException exception = null;
        try {
            final String dllink = super.getDllink(link, br);
            if (dllink != null) {
                return dllink;
            }
        } catch (final PluginException e) {
            if (isEmbedURL(br.getURL())) {
                throw e;
            }
            logger.log(e);
            exception = e;
        }
        final String selfEmbed = br.getRegex("<iframe[^>]*src=\"(https?://[^\"]+/embed/[^\"]+)\"").getMatch(0);
        if (selfEmbed == null || !canHandle(selfEmbed)) {
            throw exception;
        }
        /**
         * 2025-06-23: e.g. cluset.com <br>
         * /videos/11045/anmacherinnen-7-gluhende-fotzchen/
         */
        logger.info("Processing selfembed");
        try {
            br.getPage(selfEmbed);
            return super.getDllink(link, br);
        } catch (PluginException e) {
            throw Exceptions.addSuppressed(exception, e);
        }
    }

    @Override
    protected boolean useAPI() {
        return ("upornia.com".equals(getHost()) || "upornia.tube".equals(getHost())) || "tubepornclassic.com".equals(getHost()) || "thegay.com".equals(getHost()) || super.useAPI();
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    protected boolean preferTitleHTML() {
        if ("bigwank.com".equals(getHost()) || "fpo.xxx".equals(getHost()) | "cluset.com".equals(getHost())) {
            /* cluset.com example with bad title in URL: /videos/10824/100038/ */
            return true;
        } else {
            return super.preferTitleHTML();
        }
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        return KernelVideoSharingComV2.buildAnnotationUrlsDefaultVideosPattern(getPluginDomains());
    }

    @Override
    protected String generateContentURL(final String host, final String fuid, final String urlTitle) {
        return generateContentURLDefaultVideosPattern(host, fuid, urlTitle);
    }
}