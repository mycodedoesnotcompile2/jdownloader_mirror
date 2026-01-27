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
package org.appwork.utils.net.httpserver;

/**
 * Configuration class for connection timeouts.
 * 
 * <p>
 * This class allows configuring socket timeouts for HTTP server connections. The socket timeout determines how long the server will wait
 * for data from a client before timing out the connection.
 * </p>
 *
 * @author AppWork
 */
public class ConnectionTimeouts {

    /**
     * Default socket timeout: 30 seconds (30000 milliseconds). This is a reasonable balance between allowing legitimate slow connections
     * and preventing resource exhaustion from slow or hanging connections.
     */
    public static final int DEFAULT_SOCKET_TIMEOUT_MS = 30 * 1000;

    /**
     * Socket timeout in milliseconds. This is the timeout for reading data from the client socket. Set to -1 to disable timeout (not
     * recommended for production).
     */
    private final int socketTimeoutMs;

    /**
     * Creates a new ConnectionTimeouts configuration with the default socket timeout (30 seconds).
     */
    public ConnectionTimeouts() {
        this.socketTimeoutMs = DEFAULT_SOCKET_TIMEOUT_MS;
    }

    /**
     * Creates a new ConnectionTimeouts configuration.
     *
     * @param socketTimeoutMs
     *            Socket timeout in milliseconds. Use -1 to disable timeout (not recommended). Must be >= -1.
     * @throws IllegalArgumentException
     *             if socketTimeoutMs is less than -1
     */
    public ConnectionTimeouts(final int socketTimeoutMs) {
        if (socketTimeoutMs < -1) {
            throw new IllegalArgumentException("Socket timeout must be >= -1, was: " + socketTimeoutMs);
        }
        this.socketTimeoutMs = socketTimeoutMs;
    }

    /**
     * Returns the socket timeout in milliseconds.
     *
     * @return Socket timeout in milliseconds, or -1 if timeout is disabled
     */
    public int getSocketTimeoutMs() {
        return socketTimeoutMs;
    }

    /**
     * Returns a string representation of this connection timeouts configuration for debugging purposes.
     * 
     * <p>
     * The format shows the configured timeout in a readable format:
     * </p>
     * <pre>
     * ConnectionTimeouts[socketTimeoutMs=30000]
     * </pre>
     * 
     * @return A string representation of this connection timeouts configuration
     */
    @Override
    public String toString() {
        return "ConnectionTimeouts[socketTimeoutMs=" + getSocketTimeoutMs() + "]";
    }
}
