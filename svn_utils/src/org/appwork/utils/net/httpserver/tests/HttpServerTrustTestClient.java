/**
 * Helper for HttpServer trust rejection tests: run as separate process (e.g. as LocalSystem via
 * schtasks). Connects to 127.0.0.1:port, sends a minimal HTTP GET, then checks if the server sent
 * any response. If the connection was closed without sending any byte (rejected by the server's socket address validator), exits 0. If any response byte was received, exits 1.
 * ====================================================================================================================================================
 * Same license as AppWork Utilities (see HttpServerLocalhostBindingTest).
 * ====================================================================================================================================================
 */
package org.appwork.utils.net.httpserver.tests;

import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.Socket;

import org.appwork.loggingv3.LogV3;
import org.appwork.storage.TypeRef;
import org.appwork.testframework.executer.AdminExecuter;

/**
 * Run as separate JVM process to test HttpServer trust rejection: this process is the HTTP client; exit 0 = connection was rejected (closed
 * without response), exit 1 = connection was accepted. Optional: if resultFilePath is given as second argument, writes "0" or "1" to that
 * file before exit.
 * <p>
 * For use with
 * {@link AdminExecuter#runAsLocalSystem(Class, String, TypeRef, Object...)}, use the
 * static {@link #run(int)} method instead of starting a separate process.
 */
public class HttpServerTrustTestClient {
    /**
     * Connects to 127.0.0.1:port, sends GET / HTTP/1.0, and returns 0 if the connection was closed without response (rejected), 1 if any
     * response byte was received. For use with runAsLocalSystem(Class, "run", TypeRef.INT, port).
     *
     * @param port
     *            server port
     * @return 0 if rejected, 1 if accepted, 2 on unexpected error
     */
    public static int run(final int port) {
        LogV3.info("Run " + HttpServerTrustTestClient.class.getName());
        int exitCode = 2;
        Socket socket = null;
        try {
            socket = new Socket();
            socket.setSoTimeout(8000);
            socket.connect(new InetSocketAddress("127.0.0.1", port), 10000);
            OutputStream out = socket.getOutputStream();
            out.write("GET / HTTP/1.0\r\n\r\n".getBytes("ISO-8859-1"));
            out.flush();
            InputStream in = socket.getInputStream();
            int firstByte = in.read();
            if (firstByte == -1) {
                exitCode = 0;
            } else {
                exitCode = 1;
            }
        } catch (java.net.SocketException e) {
            exitCode = 0;
        } catch (java.io.IOException e) {
            exitCode = 0;
        } catch (Throwable e) {
            System.err.println(e.getMessage());
            exitCode = 2;
        } finally {
            if (socket != null) {
                try {
                    socket.close();
                } catch (Throwable t) {
                }
            }
        }
        return exitCode;
    }

    public static void main(String[] args) {
        if (args.length < 1) {
            System.err.println("Usage: HttpServerTrustTestClient <port> [resultFilePath]");
            System.exit(3);
        }
        int port = Integer.parseInt(args[0], 10);
        String resultFilePath = args.length >= 2 ? args[1] : null;
        int exitCode = run(port);
        if (resultFilePath != null && (exitCode == 0 || exitCode == 1)) {
            try {
                java.io.File f = new java.io.File(resultFilePath);
                org.appwork.utils.IO.writeStringToFile(f, String.valueOf(exitCode));
            } catch (Throwable t) {
            }
        }
        System.exit(exitCode);
    }
}
