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
package org.appwork.utils.net.httpserver.session;

import java.io.IOException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import org.appwork.remoteapi.ParseException;
import org.appwork.remoteapi.RemoteAPI;
import org.appwork.remoteapi.RemoteAPIRequest;
import org.appwork.remoteapi.RemoteAPIResponse;
import org.appwork.remoteapi.SessionRemoteAPIRequest;
import org.appwork.remoteapi.exceptions.ApiInterfaceNotAvailable;
import org.appwork.remoteapi.exceptions.AuthException;
import org.appwork.remoteapi.exceptions.BasicRemoteAPIException;
import org.appwork.remoteapi.exceptions.SessionException;
import org.appwork.storage.JSonStorage;
import org.appwork.utils.StringUtils;
import org.appwork.utils.net.httpserver.handler.HttpRequestHandler;
import org.appwork.utils.net.httpserver.requests.GetRequest;
import org.appwork.utils.net.httpserver.requests.HttpRequest;
import org.appwork.utils.net.httpserver.requests.KeyValuePair;
import org.appwork.utils.net.httpserver.requests.PostRequest;
import org.appwork.utils.net.httpserver.responses.HttpResponse;
import org.appwork.utils.reflection.Clazz;

/**
 * @author daniel
 *
 */
public abstract class AbstractSessionRemoteAPI<T extends HttpSession> extends RemoteAPI implements HttpRequestHandler, LoginAPIInterface {
    public AbstractSessionRemoteAPI() throws ParseException {
        // this.handler = new ArrayList<HttpSessionRequestHandler<T>>();
        this.register(this);
    }

    protected SessionRemoteAPIRequest<T> createSessionRemoteAPIRequest(final T session, final HttpRequest request, final RemoteAPIRequest apiRequest) throws BasicRemoteAPIException {
        return new SessionRemoteAPIRequest<T>(request, apiRequest, session);
    }

    @SuppressWarnings("unchecked")
    @Override
    public boolean disconnect(final RemoteAPIRequest request) {
        final SessionRemoteAPIRequest<T> req = (SessionRemoteAPIRequest<T>) request;
        final T session = req.getSession();
        if (session != null) {
            return this.removeSession(session);
        }
        return false;
    }

    private String extractSessionID(final HttpRequest request) throws IOException {
        ArrayList<KeyValuePair> params = new ArrayList<KeyValuePair>();
        String token = null;
        final List<KeyValuePair> get = request.getRequestedURLParameters();
        if (get != null) {
            for (final KeyValuePair next : get) {
                if ("token".equalsIgnoreCase(next.key)) {
                    token = next.value;
                } else {
                    params.add(next);
                }
            }
            request.setRequestedURLParameters(params);
        }
        if (StringUtils.isEmpty(token) && request instanceof PostRequest) {
            final List<KeyValuePair> post = ((PostRequest) request).getPostParameter();
            params = new ArrayList<KeyValuePair>();
            for (final KeyValuePair next : post) {
                if ("token".equalsIgnoreCase(next.key)) {
                    token = next.value;
                } else {
                    params.add(next);
                }
            }
            ((PostRequest) request).setPostParameter(params);
        }
        return token;
    }

    /**
     * get session for given sessionID or null in case session is invalid/not found
     *
     * @param request
     * @param sessionID
     * @return
     */
    protected abstract T getSession(org.appwork.utils.net.httpserver.requests.HttpRequest request, final String sessionID);

    @Override
    protected Object handleVoidMethods(Object responseData, final Method method) {
        if (Clazz.isVoid(method.getReturnType())) {
            // void return
            responseData = null;
        }
        return responseData;
    }

    @Override
    public String handshake(final RemoteAPIRequest request, final String user, final String password) throws AuthException {
        final T session = this.newSession(request, user, password);
        if (session == null) {
            throw new AuthException();
        }
        return session.getSessionID();
    }

    /**
     * create new session for given username, password.
     *
     * @param username
     * @param password
     * @return
     */
    protected abstract T newSession(final RemoteAPIRequest request, String username, String password);

    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.utils.net.httpserver.handler.HttpRequestHandler#onGetRequest (org.appwork.utils.net.httpserver.requests.GetRequest,
     * org.appwork.utils.net.httpserver.responses.HttpResponse)
     */
    @Override
    public boolean onGetRequest(final GetRequest request, final HttpResponse response) throws BasicRemoteAPIException {
        return this.onRequest(request, response);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.utils.net.httpserver.handler.HttpRequestHandler#onPostRequest
     * (org.appwork.utils.net.httpserver.requests.PostRequest, org.appwork.utils.net.httpserver.responses.HttpResponse)
     */
    @Override
    public boolean onPostRequest(final PostRequest request, final HttpResponse response) throws BasicRemoteAPIException {
        return this.onRequest(request, response);
    }

    /**
     * @param request
     * @param response
     * @return
     * @throws BasicRemoteAPIException
     */
    private boolean onRequest(final HttpRequest request, final HttpResponse response) throws BasicRemoteAPIException {
        try {
            final String token = this.extractSessionID(request);
            RemoteAPIRequest apiRequest = this.createRemoteAPIRequestObject(request);
            if (apiRequest == null) {
                throw new ApiInterfaceNotAvailable();
            }
            if (apiRequest.getMethod() == null) {
                throw new ApiInterfaceNotAvailable();
            }
            final Class<?> declaringClass = apiRequest.getMethod().getDeclaringClass();
            if (declaringClass != LoginAPIInterface.class && apiRequest.getIface().isSessionRequired()) {
                // session required
                final T session = this.getSession(request, token);
                if (session == null || !session.isAlive()) {
                    throw new SessionException();
                }
                apiRequest = this.createSessionRemoteAPIRequest(session, request, apiRequest);
            }
            this._handleRemoteAPICall(apiRequest, this.createRemoteAPIResponseObject(apiRequest, response));
            return true;
        } catch (final IOException e) {
            throw new BasicRemoteAPIException(e);
        }
    }

    protected abstract boolean removeSession(final T session);

    @Override
    public String toString(final RemoteAPIRequest request, final RemoteAPIResponse response, final Object responseData) {
        return JSonStorage.serializeToJson(responseData);
    }
}
