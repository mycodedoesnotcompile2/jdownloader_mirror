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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;

import org.appwork.utils.net.httpserver.requests.HttpRequest;

/**
 * Base class for SSL-wrapped HTTP connections.
 * 
 * <p>
 * This class is similar to {@code AutoWrappedSSLHttpConnection} in CoreAPIServer but does not depend on BouncyCastle.
 * Implementations (e.g., in CoreAPIServer) can extend this class and provide the actual client certificate type.
 * </p>
 * 
 * <p>
 * This connection wrapper automatically sets the HTTPS flag on requests to indicate they came over an encrypted connection.
 * </p>
 * 
 * @author AppWork
 */
public class SSLConnectionWrapper extends HttpServerConnection {
    private final boolean ssl;
    private final Object clientCertificate; // Object to avoid BouncyCastle dependency
    
    /**
     * Creates a new SSL-wrapped HTTP connection.
     * 
     * @param server
     *            The HTTP server instance
     * @param ssl
     *            true if this is an SSL/TLS connection
     * @param clientSocket
     *            The client socket
     * @param is
     *            The wrapped input stream (TLS-decrypted)
     * @param os
     *            The wrapped output stream (TLS-encrypted)
     * @param clientCertificate
     *            Optional client certificate (can be null), implementation-specific type
     * @throws IOException
     *             if the connection cannot be created
     */
    public SSLConnectionWrapper(final AbstractServerBasics server, final boolean ssl, final Socket clientSocket, final InputStream is, final OutputStream os, final Object clientCertificate) throws IOException {
        super(server, clientSocket, is, os);
        this.ssl = ssl;
        this.clientCertificate = clientCertificate;
    }
    
    @Override
    protected HttpRequest buildRequest() throws IOException {
        final HttpRequest ret = super.buildRequest();
        ret.setHttps(this.ssl);
        return ret;
    }
    
    /**
     * Returns the client certificate if available.
     * 
     * <p>
     * The actual type depends on the SSL implementation (e.g., BouncyCastle Certificate in CoreAPIServer).
     * </p>
     * 
     * @return The client certificate (implementation-specific type, can be null)
     */
    public Object getClientCertificate() {
        return this.clientCertificate;
    }
    
    /**
     * @return true if this is an SSL/TLS connection
     */
    public boolean isSsl() {
        return this.ssl;
    }
}
