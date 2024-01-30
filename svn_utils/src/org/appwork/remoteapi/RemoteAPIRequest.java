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
package org.appwork.remoteapi;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.util.List;

import org.appwork.loggingv3.LogV3;
import org.appwork.remoteapi.exceptions.ApiCommandNotAvailable;
import org.appwork.remoteapi.exceptions.BadParameterException;
import org.appwork.remoteapi.exceptions.BasicRemoteAPIException;
import org.appwork.storage.JSonStorage;
import org.appwork.utils.net.HeaderCollection;
import org.appwork.utils.net.httpserver.requests.ConnectRequest;
import org.appwork.utils.net.httpserver.requests.GetRequest;
import org.appwork.utils.net.httpserver.requests.HeadRequest;
import org.appwork.utils.net.httpserver.requests.HttpRequest;
import org.appwork.utils.net.httpserver.requests.HttpRequestInterface;
import org.appwork.utils.net.httpserver.requests.KeyValuePair;
import org.appwork.utils.net.httpserver.requests.OptionsRequest;
import org.appwork.utils.net.httpserver.requests.PostRequest;

/**
 * @author daniel
 *
 */
public class RemoteAPIRequest implements HttpRequestInterface {
    public static enum REQUESTTYPE {
        CONNECT,
        HEAD,
        POST,
        OPTIONS,
        GET,
        UNKNOWN
    }

    private final InterfaceHandler<?> iface;
    private final String[]            parameters;
    protected final HttpRequest       request;
    private final Method              method;
    private final String              jqueryCallback;
    private final String              methodName;

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {        
        return request + "\r\n" + "Method: " + method + "\r\nParameters:" + JSonStorage.serializeToJson(parameters);
    }

    public RemoteAPIRequest(final InterfaceHandler<?> iface, final String methodName, final String[] parameters, final HttpRequest request, final String jqueryCallback) throws BasicRemoteAPIException {
        this.iface = iface;
        this.parameters = parameters;
        this.request = request;
        this.methodName = methodName;
        this.jqueryCallback = jqueryCallback;
        this.method = this.iface.getMethod(methodName, this.parameters.length);
        if (method == null) {
            LogV3.info("IP " + request.getRemoteAddress());
            if (this.iface.hasMethodName(methodName)) {
                throw new BadParameterException(request.getRequestedURL());
            } else {
                throw new ApiCommandNotAvailable(request.getRequestedURL());
            }
        }
    }

    public HttpRequest getHttpRequest() {
        return this.request;
    }

    public InterfaceHandler<?> getIface() {
        return this.iface;
    }

    public InputStream getInputStream() throws IOException {
        if (this.request instanceof PostRequest) {
            return ((PostRequest) this.request).getInputStream();
        }
        return null;
    }

    /**
     * @return the jqueryCallback
     */
    public String getJqueryCallback() {
        return this.jqueryCallback;
    }

    /**
     * @return
     */
    public Method getMethod() {
        return this.method;
    }

    /**
     * @return the methodName
     */
    public String getMethodName() {
        return this.methodName;
    }

    @Override
    public String getParameterbyKey(final String key) throws IOException {
        if (request != null) {
            return request.getParameterbyKey(key);
        } else {
            return null;
        }
    }

    @Override
    public String[] getParametersbyKey(String key) throws IOException {
        if (request != null) {
            return request.getParametersbyKey(key);
        } else {
            return null;
        }
    }

    public String[] getParameters() {
        return this.parameters;
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
    public List<String> getRemoteAddresses() {
        return this.request.getRemoteAddress();
    }

    /**
     * tries to return the actual ip address of the user, even if he is behind a proxy. This does only work if the reuqest has proper
     * x-forwarded-for headers {@link #getRemoteAddresses()}
     *
     * @return
     */
    public String getRemoteAddress() {
        return request.getActuallRemoteAddress();
    }

    public String getRequestedPath() {
        return this.request.getRequestedPath();
    }

    public String getRequestedURL() {
        return this.request.getRequestedURL();
    }

    /**
     * @return the requestedURLParameters
     */
    public List<KeyValuePair> getRequestedURLParameters() {
        return this.request.getRequestedURLParameters();
    }

    public HeaderCollection getRequestHeaders() {
        return this.request.getRequestHeaders();
    }

    /**
     * @return
     */
    public long getRequestID() {
        return -1;
    }

    public REQUESTTYPE getRequestType() {
        if (this.request instanceof OptionsRequest) {
            return REQUESTTYPE.OPTIONS;
        } else if (this.request instanceof HeadRequest) {
            return REQUESTTYPE.HEAD;
        } else if (this.request instanceof PostRequest) {
            return REQUESTTYPE.POST;
        } else if (this.request instanceof GetRequest) {
            return REQUESTTYPE.GET;
        } else if (this.request instanceof ConnectRequest) {
            return REQUESTTYPE.CONNECT;
        } else {
            return REQUESTTYPE.UNKNOWN;
        }
    }

    /**
     * @return
     */
    public String getSignature() {
        return null;
    }

    public boolean validateRID() {
        return true;
    }

    /**
     *
     */
    public boolean isHttps() {
        return request.isHttps();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.net.httpserver.requests.HttpRequestInterface#getID()
     */
    @Override
    public long getId() {        
        return request.getId();
    }
}
