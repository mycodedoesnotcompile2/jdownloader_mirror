/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 *         (License header abbreviated; see project license.)
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
