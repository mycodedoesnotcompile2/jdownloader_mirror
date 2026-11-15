/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.testframework.executer;

import java.io.File;
import java.net.Socket;
import java.util.List;
import java.util.Set;

import org.appwork.utils.singleapp.IncommingMessageListener;
import org.appwork.utils.singleapp.SingleAppInstance;

/**
 * SingleAppInstance subclass that enforces remote PID restriction: only connections from a given set of PIDs (e.g. parent chain) or a
 * single PID are accepted. Used by {@link AdminHelperProcess} to restrict the elevated helper to callers in its parent process chain.
 * Delegates to {@link AdminExecuter#isIncomingSocketAllowed(Socket, Set)} for the actual check.
 */
public class AdminHelperSingleAppInstance extends SingleAppInstance {
    private List<Integer> allowedRemotePIDs = null;
    private final String  idRoot;

    public AdminHelperSingleAppInstance(String appID, File directory, IncommingMessageListener listener) {
        this(appID, directory, listener, null);
    }

    /**
     * @param idRoot
     *            root used for client/server ID (see {@link #createID(String, String)}); if null, default Application.getRoot() is used
     */
    public AdminHelperSingleAppInstance(String appID, File directory, IncommingMessageListener listener, String idRoot) {
        super(appID, directory, listener);
        this.idRoot = idRoot;
    }

    @Override
    protected String createID(String singleApp, String appID) {
        if (idRoot != null && idRoot.length() > 0) {
            return createID(singleApp, appID, idRoot);
        }
        return super.createID(singleApp, appID);
    }

    /**
     * Restrict accepted connections to these remote PIDs (e.g. full parent chain). When set, overrides single allowedRemotePID.
     */
    public void setAllowedRemotePIDs(List<Integer> pids) {
        this.allowedRemotePIDs = pids;
    }

    @Override
    protected boolean isIncomingSocketAllowed(Socket client) {
        if (allowedRemotePIDs == null) {
            return false;
        }
        return AdminExecuter.isIncomingSocketAllowed(client, allowedRemotePIDs);
    }
}
