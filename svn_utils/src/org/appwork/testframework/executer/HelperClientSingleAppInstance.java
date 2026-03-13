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

import org.appwork.utils.singleapp.IncommingMessageListener;
import org.appwork.utils.singleapp.SingleAppInstance;

/**
 * SingleAppInstance used as a client when connecting to the admin helper. Uses a configurable app root (e.g. from
 * {@link AdminExecuter#ENV_HELPER_APP_ROOT}) so that multiple test processes can connect to the same helper with matching client/server ID,
 * each opening its own channel (socket) without GO_AWAY_INVALID_ID.
 */
public class HelperClientSingleAppInstance extends SingleAppInstance {
    private final String idRoot;

    /**
     * @param appID
     *            same as helper's APP_ID (e.g. {@link AdminHelperProcess#APP_ID})
     * @param directory
     *            lock directory (e.g. from {@link AdminExecuter#getHelperLockDirectory()})
     * @param listener
     *            optional; null for pure client usage
     * @param idRoot
     *            root used for {@link #createID(String, String)} so client ID matches helper's server ID; null = default
     *            Application.getRoot()
     */
    public HelperClientSingleAppInstance(String appID, File directory, IncommingMessageListener listener, String idRoot) {
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
}
