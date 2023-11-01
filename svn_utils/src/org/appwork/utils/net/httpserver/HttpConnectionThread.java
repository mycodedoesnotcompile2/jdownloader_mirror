/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
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
import java.net.Socket;

/**
 * @author daniel
 *
 */
public class HttpConnectionThread extends Thread {

    private HttpConnection currentConnection = null;
    private Socket         socket            = null;
    private int            serverThreadID    = 0;

    public HttpConnectionThread(final HttpServer server, final Runnable r) {
        super(r);
        this.setDaemon(true);
    }

    protected void setName(final HttpServer server, Socket socket) {
        if (socket != null) {
            this.setName("HttpConnectionThread:" + serverThreadID + ":" + socket.getLocalSocketAddress() + "<-" + socket.getRemoteSocketAddress());
        } else {
            this.setName("HttpConnectionThread:" + serverThreadID + ":" + server.getPort() + ":" + server.isLocalhostOnly());
        }
    }

    protected void setServerThreadID(int serverThreadID) {
        this.serverThreadID = serverThreadID;
    }

    public HttpConnection getCurrentConnection() {
        return this.currentConnection;
    }

    public Socket getCurrentSocket() {
        final Socket socket = this.socket;
        if (socket != null) {
            return socket;
        } else {
            final HttpConnection connection = this.currentConnection;
            if (connection != null) {
                return connection.getClientSocket();
            } else {
                return null;
            }
        }
    }

    @Override
    public void interrupt() {
        try {
            final HttpConnection lcurrentConnection = this.getCurrentConnection();
            if (lcurrentConnection != null) {
                lcurrentConnection.closeConnection();
                lcurrentConnection.close();
            } else {
                final Socket lSocket = this.getCurrentSocket();
                if (lSocket != null) {
                    try {
                        lSocket.close();
                    } catch (IOException ignore) {
                    }
                }
            }
        } finally {
            super.interrupt();
        }
    }

    public void setCurrentConnection(final HttpConnection currentConnection, Socket socket) {
        this.currentConnection = currentConnection;
        this.socket = socket;
    }
}
