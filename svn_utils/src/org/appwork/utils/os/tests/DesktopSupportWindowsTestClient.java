/**
 * Helper for {@link DesktopSupportWindowsTest}: run as a separate process to act as TCP client.
 * Args: [port] or [host] [port] (e.g. "2471" or "::1" "2471"). Connects to host:port and keeps connection open for a few seconds.
 * ====================================================================================================================================================
 * Same license as AppWork Utilities (see DesktopSupportWindowsTest).
 * ====================================================================================================================================================
 */
package org.appwork.utils.os.tests;

import java.net.InetSocketAddress;
import java.net.Socket;

/**
 * Run as separate JVM process to test getPIDForRemoteAddress: this process is the "client";
 * the test process runs the server and resolves PID for this client's remote address.
 */
public class DesktopSupportWindowsTestClient {
    public static void main(String[] args) throws Exception {
        String host = "127.0.0.1";
        int port = 0;
        if (args.length >= 2) {
            host = args[0];
            port = Integer.parseInt(args[1], 10);
        } else if (args.length >= 1) {
            port = Integer.parseInt(args[0], 10);
        } else {
            System.err.println("Usage: DesktopSupportWindowsTestClient <port> | <host> <port>");
            System.exit(3);
        }
        Socket socket = new Socket();
        try {
            socket.connect(new InetSocketAddress(host, port), 10000);
            Thread.sleep(3000);
        } finally {
            socket.close();
        }
    }
}
