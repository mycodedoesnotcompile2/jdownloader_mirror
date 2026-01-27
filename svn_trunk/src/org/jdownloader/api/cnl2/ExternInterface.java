package org.jdownloader.api.cnl2;

import java.io.IOException;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

import org.appwork.remoteapi.RemoteAPI;
import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpserver.CorsHandler;
import org.appwork.utils.net.httpserver.HttpHandlerInfo;
import org.appwork.utils.net.httpserver.HttpServer;
import org.appwork.utils.net.httpserver.ResponseSecurityHeaders;
import org.appwork.utils.net.httpserver.XContentTypeOptions;
import org.jdownloader.api.DeprecatedAPIHttpServerController;
import org.jdownloader.api.RemoteAPIConfig;

public class ExternInterface {
    private static ExternInterface INSTANCE = new ExternInterface();

    private ExternInterface() {
        final RemoteAPIConfig config = JsonConfig.create(RemoteAPIConfig.class);
        if (config.isExternInterfaceEnabled()) {
            final Thread serverInit = new Thread() {
                @Override
                public void run() {
                    final RemoteAPI remoteAPI = new RemoteAPI();
                    try {
                        remoteAPI.register(new ExternInterfaceImpl());
                        while (config.isExternInterfaceEnabled() && !Thread.currentThread().isInterrupted()) {
                            try {
                                final HttpHandlerInfo handler = DeprecatedAPIHttpServerController.getInstance().registerRequestHandler(9666, config.isExternInterfaceLocalhostOnly(), remoteAPI);
                                // Configure server for ExternInterface: CORS and Security Headers
                                configureServerForExternInterface(handler.getHttpServer());
                                break;
                            } catch (IOException e) {
                                Thread.sleep(30 * 1000l);
                            }
                        }
                    } catch (Throwable e) {
                        org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().log(e);
                    }
                }
            };
            serverInit.setDaemon(true);
            serverInit.setName("ExternInterface: init");
            serverInit.start();
        }
    }

    public static ExternInterface getINSTANCE() {
        return INSTANCE;
    }

    /**
     * Configures the HTTPServer for ExternInterface with special CORS and Security Header settings.
     *
     * <p>
     * This method overrides the default configuration from DeprecatedAPIServer with ExternInterface-specific settings. ExternInterface must
     * be accessible from any website (cross-origin requests, framed, etc.) to allow browser extensions and websites to send links to
     * JDownloader.
     * </p>
     *
     * @param server
     *            The HTTPServer to configure
     */
    private static void configureServerForExternInterface(HttpServer server) {
        // CORS configuration for ExternInterface: Allow all origins for browser extensions and websites
        CorsHandler corsHandler = new CorsHandler();
        // Java 1.6 compatible: Explicit HashSet creation instead of Collections.singleton()
        Set<String> allowedOrigins = new HashSet<String>();
        allowedOrigins.add("*");
        // OLD: response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN, "*"));
        // (manually set in onBeforeSendHeaders() hook)
        // NEW: Configure via CorsHandler API - cleaner, more maintainable, and configured once at server initialization
        corsHandler.setAllowedOrigins(allowedOrigins); // Allow all origins
        corsHandler.setAllowMethods(EnumSet.of(RequestMethod.OPTIONS, RequestMethod.GET, RequestMethod.POST));
        corsHandler.setMaxAge(TimeUnit.MINUTES.toSeconds(30)); // 30 minutes
        corsHandler.setAllowHeadersFromRequest(true); // Dynamically take from request
        // Private Network Access: Allow for all origins (browser extensions and websites)
        corsHandler.addPrivateNetworkRequestRule(Pattern.compile(".*"), true);
        server.setCorsHandler(corsHandler);
        // Security Headers for ExternInterface: Allow framing from any origin (websites need to embed/frame ExternInterface)
        ResponseSecurityHeaders securityHeaders = new ResponseSecurityHeaders();
        // OLD: response.getResponseHeaders().remove(HTTPConstants.HEADER_RESPONSE_X_FRAME_OPTIONS);
        // (manually removed in onBeforeSendHeaders() to allow framing from any origin)
        // NEW: Set to null via ResponseSecurityHeaders API - no header will be sent, allowing framing from any origin
        // This allows websites to embed ExternInterface in iframes, which is required for browser extensions
        securityHeaders.setXFrameOptions(null);
        // OLD: response.getResponseHeaders().remove(HTTPConstants.HEADER_RESPONSE_X_CONTENT_TYPE_OPTIONS);
        // response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_X_CONTENT_TYPE_OPTIONS, "nosniff"));
        // (manually removed then re-added in onBeforeSendHeaders())
        // NEW: Set via ResponseSecurityHeaders API - keep nosniff for security (prevents MIME-sniffing attacks)
        securityHeaders.setXContentTypeOptions(XContentTypeOptions.NOSNIFF);
        // OLD: response.getResponseHeaders().remove(HTTPConstants.HEADER_RESPONSE_CONTENT_SECURITY_POLICY);
        // (manually removed in onBeforeSendHeaders() - browser extensions need to load external resources)
        // NEW: Set to null via ResponseSecurityHeaders API - browser extensions need to be able to load external resources
        // Also, we don't want CSP frame-ancestors to block framing
        securityHeaders.setContentSecurityPolicy(null);
        // OLD: response.getResponseHeaders().remove(HTTPConstants.HEADER_RESPONSE_REFERRER_POLICY);
        // (manually removed in onBeforeSendHeaders())
        // NEW: Set to null via ResponseSecurityHeaders API - not critical for ExternInterface
        securityHeaders.setReferrerPolicy(null);
        server.setResponseSecurityHeaders(securityHeaders);
    }
}
