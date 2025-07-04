//    jDownloader - Downloadmanager
//    Copyright (C) 2012  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
package jd.plugins.hoster;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;

import org.jdownloader.plugins.components.TurbobitCore;
import org.jdownloader.plugins.components.config.TurbobitCoreConfigHitfileNet;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginJsonConfig;

import jd.PluginWrapper;
import jd.plugins.HostPlugin;

@HostPlugin(revision = "$Revision: 51174 $", interfaceVersion = 2, names = {}, urls = {})
public class HitFileNet extends TurbobitCore {
    public HitFileNet(PluginWrapper wrapper) {
        super(wrapper);
    }

    /* Keep this up2date! */
    public static String[] domains = new String[] { "hitfile.net" };

    public static String[] getAnnotationNames() {
        return new String[] { domains[0] };
    }

    @Override
    public int minimum_pre_download_waittime_seconds() {
        return 38;
    }

    @Override
    public boolean downloadurls_need_html_ending() {
        return false;
    }

    /**
     * returns the annotation pattern array
     *
     */
    public static String[] getAnnotationUrls() {
        // construct pattern
        final String host = getHostsPattern();
        return new String[] { host + "/(download/free/[a-z0-9]+|/?download/redirect/[A-Za-z0-9]+/[a-z0-9]+|(?!abuse|faq|files|impressum|linkchecker|premium|reseller|rules|rulesdownload|favicon|locale|login|reg|upload)[A-Za-z0-9]+)" };
    }

    private static String getHostsPattern() {
        final StringBuilder pattern = new StringBuilder();
        for (final String name : domains) {
            pattern.append((pattern.length() > 0 ? "|" : "") + Pattern.quote(name));
        }
        final String hosts = "https?://(?:www\\.|new\\.|m\\.)?" + "(?:" + pattern.toString() + ")";
        return hosts;
    }

    @Override
    public String[] siteSupportedNames() {
        final List<String> ret = new ArrayList<String>(Arrays.asList(domains));
        ret.add("hitfile");
        return ret.toArray(new String[0]);
    }

    @Override
    protected boolean isFastLinkcheckEnabled() {
        return PluginJsonConfig.get(TurbobitCoreConfigHitfileNet.class).isEnableFastLinkcheck();
    }

    @Override
    public Class<? extends PluginConfigInterface> getConfigInterface() {
        return TurbobitCoreConfigHitfileNet.class;
    }

    @Override
    protected boolean allowWebsiteV2Handling() {
        return true;
    }
}