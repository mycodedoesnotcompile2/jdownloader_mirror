//    jDownloader - Downloadmanager
//    Copyright (C) 2013  JD-Team support@jdownloader.org
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
package jd.plugins.hoster;

import java.util.ArrayList;
import java.util.List;

import jd.PluginWrapper;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.HostPlugin;
import jd.plugins.components.MultiHosterManagement;

import org.jdownloader.plugins.components.usenet.UsenetAccountConfigInterface;
import org.jdownloader.plugins.components.usenet.UsenetServer;

@HostPlugin(revision = "$Revision: 52755 $", interfaceVersion = 3, names = { "premiumize.me" }, urls = { "https?://(?:[a-z0-9\\.\\-]+)?premiumize\\.me/file\\?id=([A-Za-z0-9\\-_]+)" })
public class PremiumizeMe extends ZeveraCore {
    protected static MultiHosterManagement mhm = new MultiHosterManagement("premiumize.me");

    public PremiumizeMe(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + this.getHost() + "/premium");
    }

    @Override
    public String getClientID() {
        return "616325511";
    }

    public static interface PremiumizeMeAccountConfigInterface extends UsenetAccountConfigInterface {
    };

    @Override
    public Class<PremiumizeMeAccountConfigInterface> getAccountConfigInterface(Account account) {
        return PremiumizeMeAccountConfigInterface.class;
    }

    @Override
    public int getDownloadModeMaxChunks(final Account account) {
        if (account != null && account.getType() == AccountType.FREE) {
            /* Free Account */
            return 0;
        } else if (account != null && account.getType() == AccountType.PREMIUM) {
            /* Premium account */
            return 0;
        } else {
            /* Free(anonymous) and unknown account type */
            return 0;
        }
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return -1;
    }

    @Override
    public int getMaxSimultaneousFreeAccountDownloads() {
        /* 2019-02-19: premiumize.me/free */
        return 1;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return -1;
    }

    @Override
    public boolean supportsUsenet(final Account account) {
        /**
         * 2019-12-18: At the moment usenet client support is only working with apikey login, not yet in pairing login mode. </br>
         * Premiumize is working on making this possible in pairing mode as well.
         */
        if (usePairingLogin(account)) {
            return false;
        } else {
            return true;
        }
    }

    @Override
    public boolean usePairingLogin(final Account account) {
        return false;
    }

    @Override
    public List<UsenetServer> getAvailableUsenetServer() {
        final List<UsenetServer> ret = new ArrayList<UsenetServer>();
        ret.addAll(UsenetServer.createServerList("usenet.premiumize.me", false, 119));
        ret.addAll(UsenetServer.createServerList("usenet.premiumize.me", true, 563));
        return ret;
    }

    @Override
    protected MultiHosterManagement getMultiHosterManagement() {
        return mhm;
    }
}