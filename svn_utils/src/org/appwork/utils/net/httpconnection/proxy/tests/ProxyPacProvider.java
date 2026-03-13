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
 *     See full license in project root.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.net.httpconnection.proxy.tests;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import org.appwork.builddecision.BuildDecisions;
import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.processes.ProcessHandlerFactory;
import org.appwork.processes.ProcessInfo;
import org.appwork.processes.windows.jna.WindowsJNAProcessUtils;
import org.appwork.utils.Application;
import org.appwork.utils.IO;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.httpserver.HttpServer;
import org.appwork.utils.net.httpserver.HttpServerConnection;
import org.appwork.utils.net.httpserver.handler.HttpRequestHandler;
import org.appwork.utils.net.httpserver.requests.AbstractGetRequest;
import org.appwork.utils.net.httpserver.requests.AbstractPostRequest;
import org.appwork.utils.net.httpserver.responses.HttpResponse;

/**
 * IDE helper: when started, serves the <code>proxy.pac</code> resource from this package at <code>http://localhost:12846/proxy.pac</code>.
 * Use this URL as "Automatic proxy configuration" (PAC) in your IDE or system to test PAC-based proxy resolution.
 * <p>
 * Start via main(), then set your IDE proxy to "Use PAC" with URL <code>http://localhost:12846/proxy.pac</code>. Edit the
 * <code>proxy.pac</code> file in this package to customize the script.
 */
public class ProxyPacProvider {
    /** Default port for the PAC HTTP server. */
    public static final int    DEFAULT_PORT  = 9672;
    /** PAC MIME type (Proxy Auto-Config). */
    public static final String PAC_MIME_TYPE = "application/x-ns-proxy-autoconfig";
    private final HttpServer   server;
    private final int          port;

    /**
     * Creates a provider that will serve proxy.pac on the default port {@value #DEFAULT_PORT}.
     */
    public ProxyPacProvider() {
        this(DEFAULT_PORT);
    }

    /**
     * Creates a provider that will serve proxy.pac on the given port.
     *
     * @param port
     *            port to bind (e.g. 12846)
     */
    public ProxyPacProvider(final int port) {
        this.port = port;
        this.server = new HttpServer(port);
        server.getHeaderValidationRules().removeMandatoryHeader(HTTPConstants.X_APPWORK);
        server.getHeaderValidationRules().removeForbiddenHeader(HTTPConstants.HEADER_REQUEST_SEC_FETCH_SITE);
        server.getHeaderValidationRules().removeForbiddenHeader(HTTPConstants.HEADER_REQUEST_SEC_FETCH_MODE);
        server.getHeaderValidationRules().removeForbiddenHeader(HTTPConstants.HEADER_REQUEST_SEC_FETCH_USER);
        server.getHeaderValidationRules().removeForbiddenHeader(HTTPConstants.HEADER_REQUEST_SEC_FETCH_DEST);
        this.server.setLocalhostOnly(true);
        this.server.registerRequestHandler(new ProxyPacHandler());
    }

    /**
     * Starts the HTTP server. proxy.pac is then available at http://localhost:&lt;port&gt;/proxy.pac.
     *
     * @throws IOException
     *             if the server could not bind to the port
     */
    public void start() throws IOException {
        server.start();
    }

    /**
     * Stops the HTTP server.
     */
    public void stop() {
        server.shutdown();
    }

    /**
     * Returns whether the server is currently running.
     *
     * @return true if running
     */
    public boolean isRunning() {
        return server.isRunning();
    }

    /**
     * Returns the port the server is bound to (after start).
     *
     * @return actual port, or wished port if not yet started
     */
    public int getPort() {
        try {
            return server.getActualPort();
        } catch (final Throwable e) {
            return port;
        }
    }

    /**
     * Returns the URL at which proxy.pac is served (e.g. http://localhost:12846/proxy.pac).
     *
     * @return PAC URL
     */
    public String getProxyPacUrl() {
        return "http://localhost:" + getPort() + "/proxy.pac";
    }

    /**
     * HTTP handler that serves /proxy.pac from the classpath resource in this package.
     */
    private static class ProxyPacHandler implements HttpRequestHandler {
        @Override
        public boolean onGetRequest(final AbstractGetRequest request, final HttpResponse response) {
            if (!"/proxy.pac".equals(request.getRequestedPath())) {
                return false;
            }
            // request.getConnection().get
            int PID;
            try {
                PID = WindowsJNAProcessUtils.getPIDForRemoteAddress(((HttpServerConnection) request.getConnection()).getClientSocket().getRemoteSocketAddress());
                ProcessInfo process = ProcessHandlerFactory.getProcessHandler().listByPids(PID).get(0);
                LogV3.info("Requesting Process:" + process.getExecutableName());
            } catch (InterruptedException e) {
                LogV3.log(e);
            } catch (Exception e) {
                LogV3.log(e);
            }
            LogV3.info(request + "");
            final URL resource = ProxyPacProvider.class.getResource("proxy.pac");
            if (resource == null) {
                response.setResponseCode(ResponseCode.ERROR_NOT_FOUND);
                return true;
            }
            InputStream is = null;
            try {
                is = resource.openStream();
                final byte[] body = IO.readStream(-1, is);
                response.setResponseCode(ResponseCode.SUCCESS_OK);
                response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, PAC_MIME_TYPE));
                response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, String.valueOf(body.length)));
                response.getOutputStream(true).write(body);
                response.getOutputStream(true).flush();
                return true;
            } catch (final IOException e) {
                response.setResponseCode(ResponseCode.SERVERERROR_INTERNAL);
                return true;
            } finally {
                LogV3.info("" + response);
                try {
                    if (is != null) {
                        is.close();
                    }
                } catch (final IOException ignore) {
                }
            }
        }

        @Override
        public boolean onPostRequest(final AbstractPostRequest request, final HttpResponse response) {
            return false;
        }
    }

    public static void main(final String[] args) {
        BuildDecisions.setEnabled(false);
        Application.setApplication(".proxy");
        final ProxyPacProvider provider = new ProxyPacProvider();
        try {
            provider.start();
            System.out.println("Proxy PAC server running: " + provider.getProxyPacUrl());
            System.out.println("Set your IDE/system proxy to use PAC: " + provider.getProxyPacUrl());
            Thread.currentThread().join();
        } catch (final IOException e) {
            System.err.println("Failed to start PAC server: " + e.getMessage());
            e.printStackTrace();
        } catch (final InterruptedException e) {
            Thread.currentThread().interrupt();
        } finally {
            provider.stop();
        }
    }
}
