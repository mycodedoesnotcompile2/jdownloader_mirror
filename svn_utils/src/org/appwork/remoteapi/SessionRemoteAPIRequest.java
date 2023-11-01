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

import org.appwork.remoteapi.exceptions.ApiCommandNotAvailable;
import org.appwork.remoteapi.exceptions.BasicRemoteAPIException;
import org.appwork.utils.net.HeaderCollection;
import org.appwork.utils.net.httpserver.requests.HttpRequest;
import org.appwork.utils.net.httpserver.requests.KeyValuePair;
import org.appwork.utils.net.httpserver.session.HttpSession;

/**
 * @author daniel
 * 
 */
public class SessionRemoteAPIRequest<T extends HttpSession> extends RemoteAPIRequest {
    private final T                session;
    private final RemoteAPIRequest apiRequest;

    /**
     * @param apiRequest
     * @param session
     * @return
     * @throws ApiCommandNotAvailable
     */
    public SessionRemoteAPIRequest(final HttpRequest request, final RemoteAPIRequest apiRequest, final T session) throws BasicRemoteAPIException {
        super(apiRequest.getIface(), apiRequest.getMethodName(), apiRequest.getParameters(), request, apiRequest.getJqueryCallback());
        this.apiRequest = apiRequest;
        this.session = session;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        // TODO Auto-generated method stub
        return "SessionAPIRequest_" + (session == null ? null : session.getSessionID()) + "\r\n" + apiRequest;
    }

    public RemoteAPIRequest getApiRequest() {
        return this.apiRequest;
    }

    @Override
    public InterfaceHandler<?> getIface() {
        return this.apiRequest.getIface();
    }

    @Override
    public InputStream getInputStream() throws IOException {
        return this.apiRequest.getInputStream();
    }

    @Override
    public String getJqueryCallback() {
        return this.apiRequest.getJqueryCallback();
    }

    @Override
    public Method getMethod() {
        return this.apiRequest.getMethod();
    }

    @Override
    public String getMethodName() {
        return this.apiRequest.getMethodName();
    }

    @Override
    public String[] getParameters() {
        return this.apiRequest.getParameters();
    }

    // @Override
    // public List<KeyValuePair> getPostParameter() throws IOException {
    //
    // return apiRequest.getPostParameter();
    // }
    @Override
    public List<String> getRemoteAddresses() {
        return this.apiRequest.getRemoteAddresses();
    }

    @Override
    public String getRequestedPath() {
        return this.apiRequest.getRequestedPath();
    }

    @Override
    public String getRequestedURL() {
        return this.apiRequest.getRequestedURL();
    }

    @Override
    public List<KeyValuePair> getRequestedURLParameters() {
        return this.apiRequest.getRequestedURLParameters();
    }

    @Override
    public HeaderCollection getRequestHeaders() {
        return this.apiRequest.getRequestHeaders();
    }

    @Override
    public REQUESTTYPE getRequestType() {
        return this.apiRequest.getRequestType();
    }

    public T getSession() {
        return this.session;
    }
}
