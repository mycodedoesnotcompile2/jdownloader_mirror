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

import java.util.ArrayList;
import java.util.List;

import jd.PluginWrapper;
import jd.plugins.HostPlugin;

@HostPlugin(revision = "$Revision: 53521 $", interfaceVersion = 3, names = {}, urls = {})
public class KernelVideoSharingComV2HostsDefault3 extends KernelVideoSharingComV2 {
    public KernelVideoSharingComV2HostsDefault3(final PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(new String[] { "pisshamster.com" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    protected KVSUrlType[] getKVSUrlType(String url) {
        // wrong KVSUrlType can lead for false FUID
        return new KVSUrlType[] { KVSUrlType.SLUG_FUID_AT_START };
    }

    @Override
    protected Integer labelToHeight(String label) {
        if ("high".equalsIgnoreCase(label)) {
            return 1080;
        } else if ("low".equalsIgnoreCase(label)) {
            return 240;
        } else {
            return super.labelToHeight(label);
        }
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        return KernelVideoSharingComV2.buildAnnotationUrlsDefault(getPluginDomains(), KVSUrlType.SLUG_FUID_AT_START);
    }

    @Override
    protected String generateContentURL(final String host, final String fuid, final String urlSlug) {
        return this.getProtocol() + appendWWWIfRequired(host) + "/" + fuid + "-" + urlSlug + "/";
    }
}