/**
 * Helper for HttpServer elevated-server trust test: run inside the elevated AdminRunner helper.
 * Starts an HttpServer on a free port (localhost only), writes the actual port to the given file
 * path, then keeps the server running for a fixed time so the non-elevated test can connect.
 * ====================================================================================================================================================
 * Same license as AppWork Utilities (see HttpServerLocalhostBindingTest).
 * ====================================================================================================================================================
 */
package org.appwork.utils.net.httpserver.tests;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.storage.TypeRef;
import org.appwork.testframework.executer.AdminExecuter;
import org.appwork.testframework.executer.ProcessOptions;
import org.appwork.utils.IO;
import org.appwork.utils.net.httpserver.HttpServer;

/**
 * Run as separate JVM process (elevated) to host HttpServer; writes port to file for test to read.
 * <p>
 * For use with {@link AdminExecuter#runAsAdmin(Class, String, TypeRef, Object...)}, use {@link #startServerInBackgroundAndReturnPort()} to
 * start the server in the elevated process and get the port directly.
 */
public class HttpServerTrustTestServer {
    private static final ConcurrentHashMap<String, AtomicReference<HttpServer>> SERVER_BY_APP_ID = new ConcurrentHashMap<String, AtomicReference<HttpServer>>();

    /**
     * Starts an HttpServer (localhost only) in a background thread and returns a {@link ResultWithCancel} with the port and cancel info.
     * Use {@link AdminExecuter#runAsAdmin(Class, String, TypeRef, ProcessOptions, Object...)} with
     * {@link ProcessOptions#cancelCallback(CancelCallback)} to get a runnable that calls {@link #stopServer(String)}.
     *
     * @return ResultWithCancel containing the server port and stopServer(appID) info
     * @throws IOException
     */
    public static int startServerInBackgroundAndReturnPort() throws IOException {
        final String appID = "HttpServerTrustTest_" + System.currentTimeMillis();
        final AtomicReference<HttpServer> serverRef = new AtomicReference<HttpServer>(null);
        SERVER_BY_APP_ID.put(appID, serverRef);
        HttpServer server = new HttpServer(0);
        server.setLocalhostOnly(true);
        server.start();
        serverRef.set(server);
        final int port = server.getActualPort();
        return port;
    }

    /**
     * Stops the server previously started with {@link #startServerInBackgroundAndReturnPort()}.
     */
    public static void stopServer(String appID) {
        AtomicReference<HttpServer> ref = SERVER_BY_APP_ID.remove(appID);
        if (ref != null) {
            HttpServer server = ref.get();
            if (server != null) {
                try {
                    server.shutdown();
                } catch (Throwable t) {
                }
            }
        }
    }

    public static void main(String[] args) {
        if (args.length < 1) {
            System.err.println("Usage: HttpServerTrustTestServer <portFilePath>");
            System.exit(3);
        }
        String portFilePath = args[0];
        HttpServer server = null;
        try {
            server = new HttpServer(0);
            server.setLocalhostOnly(true);
            server.start();
            int port = server.getActualPort();
            IO.writeStringToFile(new File(portFilePath), String.valueOf(port));
            Thread.sleep(25000);
        } catch (IOException e) {
            System.err.println("HttpServerTrustTestServer: " + e.getMessage());
            System.exit(2);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            System.exit(2);
        } finally {
            if (server != null) {
                try {
                    server.shutdown();
                } catch (Throwable t) {
                }
            }
        }
        System.exit(0);
    }
}
