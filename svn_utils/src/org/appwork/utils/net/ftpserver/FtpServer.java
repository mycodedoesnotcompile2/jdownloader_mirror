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
package org.appwork.utils.net.ftpserver;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketAddress;
import java.net.UnknownHostException;

import org.appwork.utils.net.httpconnection.HTTPConnectionUtils;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils.IPVERSION;

/**
 * @author daniel
 *
 */
public class FtpServer implements Runnable {

    private final FtpConnectionHandler<? extends FtpFile> handler;
    private final int                                     port;
    private ServerSocket                                  controlSocket;
    private Thread                                        controlThread = null;
    private ThreadGroup                                   threadGroup   = null;
    private boolean                                       localhostOnly = false;
    private boolean                                       debug         = false;

    public FtpServer(final FtpConnectionHandler<? extends FtpFile> handler, final int port) {
        this.handler = handler;
        this.port = port;
        this.threadGroup = new ThreadGroup("FTPServer");
    }

    /**
     * @return
     */
    public FtpConnectionHandler<? extends FtpFile> getFtpCommandHandler() {
        return this.handler;
    }

    /**
     * TODO: update to support IPv4 and IPv6 at the same time
     *
     * @return
     */
    protected InetAddress getLocalHost() throws UnknownHostException {
        return HTTPConnectionUtils.getLoopback(IPVERSION.IPV4_IPV6)[0];
    }

    /**
     * @return the clientThreadGroup
     */
    protected ThreadGroup getThreadGroup() {
        return this.threadGroup;
    }

    /**
     * @return the debug
     */
    public boolean isDebug() {
        return this.debug;
    }

    /**
     * @return the localhostOnly
     */
    public boolean isLocalhostOnly() {
        return this.localhostOnly;
    }

    public void run() {
        final Thread current = this.controlThread;
        final ServerSocket socket = this.controlSocket;
        try {
            while (true) {
                try {
                    final Socket clientSocket = socket.accept();
                    /* TODO: handle max client connections here */
                    new FtpConnection(this, clientSocket);
                } catch (final IOException e) {
                    break;
                }
                if (current == null || current.isInterrupted()) {
                    break;
                }
            }
        } finally {
            try {
                socket.close();
            } catch (final Throwable e) {
            }
        }
    }

    /**
     * @param debug
     *            the debug to set
     */
    public void setDebug(final boolean debug) {
        this.debug = debug;
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Runnable#run()
     */

    /**
     * @param localhostOnly
     *            the localhostOnly to set
     */
    public void setLocalhostOnly(final boolean localhostOnly) {
        this.localhostOnly = localhostOnly;
    }

    public synchronized void start() throws IOException {
        if (this.isLocalhostOnly()) {
            /* we only want localhost bound here */
            final SocketAddress socketAddress = new InetSocketAddress(this.getLocalHost(), this.port);
            this.controlSocket = new ServerSocket();
            this.controlSocket.bind(socketAddress);
        } else {
            this.controlSocket = new ServerSocket(this.port);
        }
        this.controlThread = new Thread(this.threadGroup, this);
        this.controlThread.setName("FtpServerThread");
        this.controlThread.start();
    }

    public synchronized void stop() {
        try {
            this.controlSocket.close();
        } catch (final Throwable e) {
        }
        this.threadGroup.interrupt();
    }
}
