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
package org.appwork.utils.net.httpserver.requests;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

import org.appwork.utils.net.HeaderCollection;
import org.appwork.utils.net.httpserver.HttpConnection;
import org.appwork.utils.net.httpserver.RawHttpConnectionInterface;

/**
 * @author daniel
 *
 */
public abstract class HttpRequest implements HttpRequestInterface {
    protected String           requestedURL   = null;
    protected HeaderCollection requestHeaders = null;
    protected String           requestedPath  = null;

    public abstract HttpConnection.HttpConnectionType getHttpConnectionType();

    public boolean isHttps() {
        return https;
    }

    public void setHttps(boolean https) {
        this.https = https;
    }

    protected boolean                          https                  = false;
    protected List<KeyValuePair>               requestedURLParameters = null;
    private List<String>                       remoteAddress          = new ArrayList<String>();
    protected final RawHttpConnectionInterface connection;
    private HTTPBridge                         bridge;
    private final long                         id;

    public RawHttpConnectionInterface getConnection() {
        return connection;
    }

    private static AtomicLong REQUEST_ID_COUNTER = new AtomicLong(0);

    public HttpRequest(final RawHttpConnectionInterface connection) {
        this.connection = connection;
        id = REQUEST_ID_COUNTER.incrementAndGet();
    }

    public long getId() {
        return id;
    }

    /**
     * @see http://en.wikipedia.org/wiki/X-Forwarded-For There may be several Remote Addresses if the connection is piped through several
     *      proxies.<br>
     *      [0] is always the direct address.<br>
     *      if remoteAdresses.size>1 then<br>
     *      [1] is the actuall clients ip.<br>
     *      [2] is the proxy next to him..<br>
     *      [3] is the proxy next to [2]<br>
     *      ..<br>
     *      [size-1] should be the address next to [0]<br>
     * @param inetAddress
     */
    public List<String> getRemoteAddress() {
        return remoteAddress;
    }

    /**
     * The requestpath is the resourcepath WITHOUT NAMESPACES like /jcgi/ or anything like this. If you need the full path, use
     * {@link #getRequestedURL(String)}
     */
    public String getRequestedPath() {
        return requestedPath;
    }

    /**
     * the requestedURL is the full resourcepath WITH NAMESPACES like /jcgi/ or anything like this. If you need the cropped normalized path
     * use {@link #getRequestedPath(String)}
     */
    public String getRequestedURL() {
        return requestedURL;
    }

    /**
     * @return the requestedURLParameters
     */
    public List<KeyValuePair> getRequestedURLParameters() {
        return requestedURLParameters;
    }

    public HeaderCollection getRequestHeaders() {
        return requestHeaders;
    }

    /**
     * @see http://en.wikipedia.org/wiki/X-Forwarded-For There may be several Remote Addresses if the connection is piped through several
     *      proxies.<br>
     *      [0] is always the direct address.<br>
     *      if remoteAdresses.size>1 then<br>
     *      [1] is the actuall clients ip.<br>
     *      [2] is the proxy next to him..<br>
     *      [3] is the proxy next to [2]<br>
     *      ..<br>
     *      [size-1] should be the address next to [0]<br>
     * @param inetAddress
     */
    public void setRemoteAddress(final List<String> remoteAddress) {
        this.remoteAddress = remoteAddress;
    }

    /**
     * @param requestedPath
     *            The requestpath is the resourcepath WITHOUT NAMESPACES like /jcgi/ or anything like this. If you need the full path, use
     *            {@link #setRequestedURL(String)} the requestedPath to set
     */
    public void setRequestedPath(final String requestedPath) {
        this.requestedPath = requestedPath;
    }

    /**
     * @param requestedURL
     *
     *            the requestedURL is the full resourcepath WITH NAMESPACES like /jcgi/ or anything like this. If you need the cropped
     *            normalized path use {@link #setRequestedPath(String)} the requestedURL to set
     */
    public void setRequestedURL(final String requestedURL) {
        this.requestedURL = requestedURL;
    }

    /**
     * @param requestedURLParameters
     *            the requestedURLParameters to set
     */
    public void setRequestedURLParameters(final List<KeyValuePair> requestedURLParameters) {
        this.requestedURLParameters = requestedURLParameters;
    }

    public void setRequestHeaders(final HeaderCollection requestHeaders) {
        this.requestHeaders = requestHeaders;
    }

    /**
     * tries to return the actual ip address of the user, even if he is behind a proxy. This does only work if the reuqest has proper
     * x-forwarded-for headers {@link #getRemoteAddress()}
     *
     * @return
     */
    public String getActuallRemoteAddress() {
        final List<String> addresses = getRemoteAddress();
        if (addresses == null || addresses.size() == 0) {
            return null;
        } else {
            if (addresses.size() == 1) {
                return addresses.get(0);
            } else {
                return addresses.get(1);
            }
        }
    }

    /**
     * @param string
     */
    public void setBridge(HTTPBridge bridge) {
        this.bridge = bridge;
    }

    /**
     * returns the server bridge. Exmaple: FCGI for the FCGI bridge
     *
     * @return
     */
    public HTTPBridge getBridge() {
        return bridge;
    }
}
