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

import java.io.File;
import java.util.ArrayList;

import org.jdownloader.captcha.v2.challenge.multiclickcaptcha.MultiClickedPoint;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 50091 $", interfaceVersion = 3, names = { "vipergirls.to" }, urls = { "https?://(?:www\\.)?vipergirls\\.to/secure/([A-F0-9]+)" })
public class VipergirlsTo extends PluginForDecrypt {
    public VipergirlsTo(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        final String fid = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404 || br.containsHTML("Generate fresh links by refreshing the source page")) {
            /* 2020-01-21: "Invalid or expired cypher. Generate fresh links by refreshing the source page." */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String captchaURL = "https://" + this.getHost() + "/secure/";
        final File captcha = this.getLocalCaptchaFile(".gif");
        final URLConnectionAdapter con = br.openGetConnection(captchaURL);
        try {
            Browser.download(captcha, con);
        } finally {
            con.disconnect();
        }
        /* html coordinates for rectangle objects */
        final int expectedNumberofClicks = 3;
        final String[] coords = br.getRegex("coords=\"([0-9, ]+)\"").getColumn(0);
        if (coords == null || coords.length < expectedNumberofClicks) {
            return null;
        }
        final MultiClickedPoint c = getMultiCaptchaClickedPoint(captcha, param, "Click on all 3 mentioned digits and confirm afterwards");
        String selectedLetterNumbers = "";
        final int[] x = c.getX();
        final int[] y = c.getY();
        final int clickCount = x.length;
        if (clickCount != expectedNumberofClicks) {
            logger.info("Click-count mismatch! Expected " + expectedNumberofClicks + " but got " + clickCount);
            throw new PluginException(LinkStatus.ERROR_CAPTCHA);
        }
        for (int index = 0; index < clickCount; index++) {
            final int xnow = x[index];
            final int ynow = y[index];
            int letterPosition = 0;
            for (final String coord : coords) {
                final String[] numbers = coord.replace(" ", "").split(",");
                final int x1 = Integer.parseInt(numbers[0]);
                final int x2 = Integer.parseInt(numbers[2]);
                final int y1 = Integer.parseInt(numbers[1]);
                final int y2 = Integer.parseInt(numbers[3]);
                final boolean xOK = xnow >= x1 && xnow <= x2;
                final boolean yOK = ynow >= y1 && ynow <= y2;
                if (xOK && yOK) {
                    logger.info("Found selected letter: " + letterPosition);
                    selectedLetterNumbers += Integer.toString(letterPosition);
                    break;
                }
                letterPosition++;
            }
        }
        if (selectedLetterNumbers.length() != clickCount) {
            logger.info("Result has not the expected length of " + clickCount);
        }
        final Browser brc = br.cloneBrowser();
        brc.setFollowRedirects(false);
        brc.postPage(br.getURL(), "o=" + fid + "&clicks=" + selectedLetterNumbers);
        final String finallink = brc.getRedirectLocation();
        if (finallink == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        ret.add(createDownloadlink(finallink));
        return ret;
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        return 1;
    }
}
