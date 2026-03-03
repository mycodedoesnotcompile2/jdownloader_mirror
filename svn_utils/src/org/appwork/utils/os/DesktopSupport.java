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
package org.appwork.utils.os;

import java.io.File;
import java.io.IOException;
import java.net.SocketAddress;
import java.net.URISyntaxException;
import java.net.URL;

/**
 * @author daniel
 *
 */
public interface DesktopSupport {
    public void browseURL(URL url) throws IOException, URISyntaxException;

    boolean isBrowseURLSupported();

    boolean isOpenFileSupported();

    public void openFile(File file, boolean trySingleInstance) throws IOException;

    public boolean shutdown(boolean force) throws InterruptedException;

    public boolean standby() throws InterruptedException;

    public boolean hibernate() throws InterruptedException;

    public String getDefaultDownloadDirectory();

    /**
     * root prefix is different for each oth. windows: c:\ =3 \\=2
     *
     * @param path
     * @return
     */
    public int getPrefixLength(String path);

    /**
     * @param force
     * @param waitms
     * @return
     * @throws InterruptedException
     */
    boolean reboot(boolean force, int waitms) throws InterruptedException;

    /**
     * Resolves the process ID of the peer for a TCP connection. Given the remote (peer) socket address from this process's
     * perspective (e.g. {@code socket.getRemoteSocketAddress()} on the server side), returns the PID of the process that owns
     * the peer's side of the connection. Use case: server wants the client's PID (e.g. browser) to resolve the executable path.
     * Not supported on all platforms; returns -1 when not available.
     *
     * @param adr
     *            remote socket address of the peer (e.g. client address from server's view)
     * @return PID of the peer process, or -1 if not found or not supported
     * @throws InterruptedException
     * @throws NotSupportedException 
     */
    int getPIDForRemoteAddress(SocketAddress adr) throws InterruptedException, NotSupportedException;

}
