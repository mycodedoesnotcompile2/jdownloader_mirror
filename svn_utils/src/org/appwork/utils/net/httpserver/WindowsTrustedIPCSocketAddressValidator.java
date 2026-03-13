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
package org.appwork.utils.net.httpserver;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketAddress;

import org.appwork.builddecision.BuildDecisionRequired;
import org.appwork.loggingv3.LogV3;
import org.appwork.processes.windows.jna.WindowsJNAProcessUtils;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Exceptions;
import org.appwork.utils.StringUtils;
import org.appwork.utils.os.WindowsUtils;

/**
 * Validator for Windows: only the same Windows user may connect; a non-elevated client must not connect to an elevated server. Used by
 * {@link DefaultSocketAddressValidator#get()} on Windows.
 * <p>
 * Rules: (1) Same user only (SID match). The same user has the same SID whether the process is elevated or not; UAC does not change user
 * identity. (2) If this process (server) is elevated, the remote client must also be elevated. So: elevated client may connect to
 * non-elevated server; non-elevated client may not connect to elevated server. (3) If the connection was not accepted on a loopback address
 * ({@link Socket#getLocalSocketAddress()} is not a loopback), the connection is always allowed; SID and elevation checks apply only for
 * connections accepted on localhost.
 * </p>
 */
@BuildDecisionRequired(tags = { WindowsTrustedIPCSocketAddressValidator.WINDOWS_TRUSTED_IPC_VALIDATOR_YES, WindowsTrustedIPCSocketAddressValidator.WINDOWS_TRUSTED_IPC_VALIDATOR_NO }, imports = { WindowsTrustedIPCSocketAddressValidator.ORG_APPWORK_UTILS_NET_HTTPSERVER_WINDOWSTRUSTEDIPCSOCKETADDRESSVALIDATOR, "" }, dependsOn = { org.appwork.JNAHelper.JNA_HELPER_USE_JNA, "" })
public final class WindowsTrustedIPCSocketAddressValidator implements SocketAddressValidator {
    static final String        ORG_APPWORK_UTILS_NET_HTTPSERVER_WINDOWSTRUSTEDIPCSOCKETADDRESSVALIDATOR = "org.appwork.utils.net.httpserver.WindowsTrustedIPCSocketAddressValidator";
    public static final String WINDOWS_TRUSTED_IPC_VALIDATOR_NO                                         = "WindowsTrustedIPCSocketAddressValidator.no";
    public static final String WINDOWS_TRUSTED_IPC_VALIDATOR_YES                                        = "WindowsTrustedIPCSocketAddressValidator.yes";

    @Override
    public boolean isAllowed(final Socket socket, final Object context) throws IOException {
        if (socket == null) {
            return false;
        }
        final SocketAddress local = socket.getLocalSocketAddress();
        if (local instanceof InetSocketAddress) {
            final InetAddress localAddr = ((InetSocketAddress) local).getAddress();
            if (!localAddr.isLoopbackAddress()) {
                return true;
            }
        }
        final SocketAddress remote = socket.getRemoteSocketAddress();
        try {
            final int remotePid = WindowsJNAProcessUtils.getPIDForRemoteAddress(remote);
            if (remotePid <= 0) {
                DebugMode.debugger();
                return false;
            }
            String mySid = WindowsUtils.getCurrentUserSID();
            final String remoteSid = WindowsUtils.getUserSIDForProcess(remotePid);
            if (!StringUtils.equals(mySid, remoteSid)) {
                return false;
            }
            if (WindowsUtils.isElevated()) {
                if (!WindowsUtils.isProcessElevated(remotePid)) {
                    return false;
                }
            }
            return true;
        } catch (final Throwable e) {
            Exceptions.resetInterruptFlag(e);
            LogV3.log(e);
            throw new IOException(e);
        }
    }
}
