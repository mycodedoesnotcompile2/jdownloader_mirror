//jDownloader - Downloadmanager
//Copyright (C) 2026  JD-Team support@jdownloader.org
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

import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import org.appwork.utils.formatter.SizeFormatter;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 53126 $", interfaceVersion = 3, names = {}, urls = {})
public class FileDitchCom extends PluginForHost {
    public FileDitchCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost();
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "fileditchfiles.st", "fileditchfiles.me", "fileditch.com", "theditch.st" });
        return ret;
    }

    @Override
    public String rewriteHost(final String host) {
        /* 2026-08-04: Main domain changed from fileditchfiles.me to fileditchfiles.st */
        return this.rewriteHost(getPluginDomains(), host);
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

    /** New URL type e.g. https://fileditchfiles.st/balpha1/43339e614f5b3f28ebb9/00_Testdata.part3.rar */
    private static final Pattern PATTERN_FILE       = Pattern.compile("/[^/]+/([a-f0-9]+)/([^/\\?#]+)");
    /** Old URL type e.g. https://fileditchfiles.me/file.php?f=/abc123/00_Testdata.part3.rar */
    private static final Pattern PATTERN_FILE_OLD   = Pattern.compile("/file\\.php\\?f=/([a-z0-9]{3,})/([^/#\\?]+)");
    private static final Pattern PATTERN_FILE_SHORT = Pattern.compile("/([a-z0-9]{8})");

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            final String hostsPattern = buildHostsPatternPart(domains);
            ret.add("https?://" + hostsPattern + "/(" + PATTERN_FILE.pattern().substring(1) + "|" + PATTERN_FILE_OLD.pattern().substring(1) + "|" + PATTERN_FILE_SHORT.pattern().substring(0) + ")");
        }
        return ret.toArray(new String[0]);
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
        final String url = link.getPluginPatternMatcher();
        String fid = new Regex(url, PATTERN_FILE).getMatch(0);
        if (fid != null) {
            return fid;
        }
        fid = new Regex(url, PATTERN_FILE_OLD).getMatch(0);
        if (fid != null) {
            return fid;
        }
        fid = new Regex(url, PATTERN_FILE_SHORT).getMatch(0);
        return fid;
    }

    @Override
    protected String getDefaultFileName(DownloadLink link) {
        final String fname = new Regex(link.getPluginPatternMatcher(), PATTERN_FILE_OLD).getMatch(1);
        if (fname != null) {
            return fname;
        }
        /* Other (new) kinds of links contain the filename at the end of their path so auto handling will handle these just fine. */
        return super.getDefaultFileName(link);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        br.getPage(link.getPluginPatternMatcher());
        handleAntiBotChallenge(br);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("<h2>\\s*File unreachable\\s*</h2>")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        link.setDownloadSize(SizeFormatter.getSize(null, br.getRegex("<span class=\"size\">([^<]+)</span>").getMatch(0), true, false));
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        final String js = br.getRegex("var u = \\[(.*?)\\]\\.join\\(\"\"\\)").getMatch(0);
        if (js == null) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String dllink = js.replace("\",\"", "").replace("\"", "").replace("\\", "");
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, true, 0);
        handleConnectionErrors(br, dl.getConnection());
        dl.startDownload();
    }

    /**
     * Detects and solves the JavaScript proof-of-work anti-bot challenge ("Verifying your browser…"). </br>
     * The site hands out a hidden form which requires a nonce so that SHA-256(pow_challenge + ":" + nonce) has "pow_diff" leading zero
     * bits. Once solved, the form is posted back and the real page is returned.
     */
    private void handleAntiBotChallenge(final Browser br) throws Exception {
        int round = 0;
        while (br.containsHTML("name\\s*=\\s*\"pow_challenge\"")) {
            if (round++ >= 3) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Failed to solve anti-bot challenge");
            }
            if (this.isAbort()) {
                throw new InterruptedException();
            }
            final Form pow = br.getFormbyKey("pow_challenge");
            if (pow == null) {
                break;
            }
            final String challenge = pow.getInputFieldByName("pow_challenge").getValue();
            final String diffStr = pow.getInputFieldByName("pow_diff").getValue();
            if (challenge == null || diffStr == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final int diff = Integer.parseInt(diffStr.trim());
            final String nonce = solveProofOfWork(challenge, diff);
            pow.put("pow_nonce", nonce);
            br.submitForm(pow);
        }
    }

    /** Brute-forces a nonce so that SHA-256(challenge + ":" + nonce) has the requested number of leading zero bits. */
    private String solveProofOfWork(final String challenge, final int diff) throws Exception {
        final MessageDigest md = MessageDigest.getInstance("SHA-256");
        final String prefix = challenge + ":";
        long nonce = 0;
        while (true) {
            if (this.isAbort()) {
                throw new InterruptedException();
            }
            md.reset();
            final byte[] hash = md.digest((prefix + nonce).getBytes("US-ASCII"));
            if (hasLeadingZeroBits(hash, diff)) {
                return Long.toString(nonce);
            }
            nonce++;
        }
    }

    private boolean hasLeadingZeroBits(final byte[] bytes, final int need) {
        final int full = need >> 3;
        final int rem = need & 7;
        for (int i = 0; i < full; i++) {
            if (bytes[i] != 0) {
                return false;
            }
        }
        if (rem != 0) {
            if ((bytes[full] & ((0xFF << (8 - rem)) & 0xFF)) != 0) {
                return false;
            }
        }
        return true;
    }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final Account acc) {
        return false;
    }
}
