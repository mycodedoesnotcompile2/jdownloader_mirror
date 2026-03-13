/**
 * Helper for {@link ProtectedSingleAppInstanceTest}: run as a separate process to act as SingleAppInstance client.
 * Connects to the given port; if the server closes the connection immediately (rejection), writes "REJECTED" to result file and exits 0.
 * If the connection is accepted and we receive data, writes "ACCEPTED" and exits 1.
 * <p>
 * Args: port [resultFile]
 * - port: server port (e.g. from SingleAppInstance lock file)
 * - resultFile: optional; if present, write "REJECTED" or "ACCEPTED" to this file (one line)
 */
package org.appwork.utils.singleapp.tests;

import java.io.File;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;

import org.appwork.testframework.executer.AdminExecuter;
import org.appwork.utils.IO;
import org.appwork.storage.TypeRef;

/**
 * Run as separate JVM process; the test asserts that when this client runs in another session (or non-elevated
 * against elevated server), the server closes the connection and this process exits with REJECTED.
 * <p>
 * For use with {@link AdminExecuter#runAsLocalSystem(Class, String, TypeRef, Object...)},
 * use {@link #run(int)} to run in the LocalSystem context and get "REJECTED" or "ACCEPTED" as return value.
 */
public class ProtectedSingleAppInstanceTestClient {

    /**
     * Connects to the given port; if the server closes the connection without data, returns "REJECTED", else "ACCEPTED".
     * For use with runAsLocalSystem(ProtectedSingleAppInstanceTestClient.class, "run", TypeRef.STRING, Integer.valueOf(port)).
     *
     * @param port
     *            server port
     * @return "REJECTED" or "ACCEPTED"
     */
    public static String run(final int port) {
        return runClient(port);
    }

    public static void main(String[] args) {
        int port = 0;
        String resultFile = null;
        if (args.length >= 2) {
            port = Integer.parseInt(args[0], 10);
            resultFile = args[1];
        } else if (args.length >= 1) {
            port = Integer.parseInt(args[0], 10);
        } else {
            System.err.println("Usage: ProtectedSingleAppInstanceTestClient <port> [resultFile]");
            System.exit(2);
        }
        String result = runClient(port);
        if (resultFile != null) {
            try {
                IO.writeStringToFile(new File(resultFile), result + "\r\n");
            } catch (Throwable e) {
                System.err.println("Failed to write result file: " + e.getMessage());
                System.exit(3);
            }
        }
        System.exit("REJECTED".equals(result) ? 0 : 1);
    }

    /**
     * Connect to port, try to read; if connection is closed by server before we get data, return "REJECTED".
     */
    private static String runClient(int port) {
        Socket socket = null;
        try {
            socket = new Socket();
            socket.connect(new InetSocketAddress("127.0.0.1", port), 10000);
            socket.setSoTimeout(3000);
            int b = socket.getInputStream().read();
            if (b < 0) {
                return "REJECTED";
            }
            return "ACCEPTED";
        } catch (IOException e) {
            return "REJECTED";
        } finally {
            if (socket != null) {
                try {
                    socket.close();
                } catch (Throwable t) {
                }
            }
        }
    }
}
