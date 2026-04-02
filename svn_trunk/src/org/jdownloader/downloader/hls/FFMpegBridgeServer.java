package org.jdownloader.downloader.hls;

import java.util.HashMap;
import java.util.concurrent.TimeUnit;

import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpserver.AllowAllSocketAddressValidator;
import org.appwork.utils.net.httpserver.CorsHandler;
import org.appwork.utils.net.httpserver.HeaderValidationRules;
import org.appwork.utils.net.httpserver.HttpServer;

/**
 * HttpServer implementation specifically configured for FFmpeg bridge usage.
 *
 * <p>
 * This server is used as a bridge between JDownloader and FFmpeg. FFmpeg makes HTTP requests to this server to download media segments or
 * meta data. Since FFmpeg is an external tool that cannot send custom headers like "x-appwork", this server is configured to:
 * </p>
 * <ul>
 * <li>Only accept connections from localhost (localhostOnly=true)</li>
 * <li>Not require the x-appwork header (FFmpeg cannot send this header)</li>
 * <li>Use default security headers (sufficient for local-only server)</li>
 * </ul>
 *
 * <p>
 * <b>Usage:</b> Use this class instead of {@link HttpServer} when creating a server for FFmpeg.
 * </p>
 *
 * <p>
 * <b>Example:</b>
 * </p>
 *
 * <pre>
 * {
 *     &#064;code
 *     FFMpegBridgeServer server = new FFMpegBridgeServer(0); // Port 0 = automatic port
 *     server.start();
 *     // Register request handlers...
 * }
 * </pre>
 *
 * @author JDownloader Team
 */
public class FFMpegBridgeServer extends HttpServer {
    /**
     * Creates a new FFMpegBridgeServer with the specified port.
     *
     * <p>
     * The server is automatically configured for FFmpeg usage:
     * </p>
     * <ul>
     * <li>localhostOnly is set to true</li>
     * <li>x-appwork header requirement is removed (FFmpeg cannot send this header)</li>
     * <li>Default security headers are used (sufficient for local-only server)</li>
     * </ul>
     *
     * @param port
     *            The port to bind to. Use 0 for automatic port assignment.
     */
    public FFMpegBridgeServer(final int port) {
        super(port);
        // Configure server for FFmpeg bridge usage
        this.setLocalhostOnly(true);
        setAllowedMethods(RequestMethod.OPTIONS, RequestMethod.GET, RequestMethod.HEAD);
        final HeaderValidationRules header = new HeaderValidationRules(new HashMap<String, String>(), new HeaderValidationRules().getForbiddenHeaders());
        setHeaderValidationRules(header);
        CorsHandler corsHandler = new CorsHandler();
        corsHandler.setAllowMethods();
        corsHandler.setMaxAge(TimeUnit.MINUTES.toSeconds(30)); // 30 minutes
        setSocketAddressValidator(new AllowAllSocketAddressValidator());
        setCorsHandler(corsHandler);
    }
}
