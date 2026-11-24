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

import java.util.ArrayList;


import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.parser.html.Form;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.UploaderJp;

@DecrypterPlugin(revision = "$Revision: 54120 $", interfaceVersion = 3, names = { "uploader.jp" }, urls = { "https?://u[a-z0-9]\\.getuploader\\.com/.+" })
public class UploaderJpFolder extends PluginForDecrypt {
    public UploaderJpFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        if (param.getCryptedUrl().matches("(?i)https?://[^/]+/[^/]+/download/\\d+.*")) {
            /* Single file -> Handle via host plugin */
            ret.add(createDownloadlink(param.getCryptedUrl()));
        } else {
            br.setFollowRedirects(true);
            br.getPage(param.getCryptedUrl());
            final Form form = br.getFormByInputFieldKeyValue("q", "age_confirmation");
            if (form != null) {
                br.submitForm(form);
            }
            if (UploaderJp.isOffline(this.br)) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final String[] links = br.getRegex("(?i)\"(https?://[^/]+/[^/]+/download/\\d+)\"").getColumn(0);
            if (links == null || links.length == 0) {
                /* Content offline, plugin broken or unsupported URL. */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            for (final String singleLink : links) {
                ret.add(createDownloadlink(singleLink));
            }
        }
        return ret;
    }
}
