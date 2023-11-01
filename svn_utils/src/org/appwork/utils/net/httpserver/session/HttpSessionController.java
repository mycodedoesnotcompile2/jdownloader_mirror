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

import java.util.ArrayList;
import java.util.Iterator;

import org.appwork.remoteapi.RemoteAPIRequest;
import org.appwork.remoteapi.SessionRemoteAPIRequest;
import org.appwork.remoteapi.exceptions.ApiCommandNotAvailable;
import org.appwork.remoteapi.exceptions.AuthException;
import org.appwork.remoteapi.exceptions.BasicRemoteAPIException;
import org.appwork.utils.net.httpserver.handler.HttpRequestHandler;
import org.appwork.utils.net.httpserver.handler.HttpSessionRequestHandler;
import org.appwork.utils.net.httpserver.requests.GetRequest;
import org.appwork.utils.net.httpserver.requests.HttpRequest;
import org.appwork.utils.net.httpserver.requests.KeyValuePair;
import org.appwork.utils.net.httpserver.requests.PostRequest;
import org.appwork.utils.net.httpserver.responses.HttpResponse;

/**
 * @author daniel
 *
 */
public abstract class HttpSessionController<T extends HttpSession> implements HttpRequestHandler, LoginAPIInterface {
    private java.util.List<HttpSessionRequestHandler<T>> handler = null;

    public HttpSessionController() {
        this.handler = new ArrayList<HttpSessionRequestHandler<T>>();
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

    protected String extractSessionID(final HttpRequest request) {
        final Iterator<KeyValuePair> it = request.getRequestedURLParameters().iterator();
        while (it.hasNext()) {
            final KeyValuePair next = it.next();
            if ("token".equalsIgnoreCase(next.key)) {
                it.remove();
                return next.value;
            }
        }
        return null;
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
        final java.util.List<HttpSessionRequestHandler<T>> handlers = this.handler;
        final T session = this.getSession(request, this.extractSessionID(request));
        ApiCommandNotAvailable notFound = null;
        for (final HttpSessionRequestHandler<T> handler : handlers) {
            try {
                if (handler.onGetSessionRequest(session, request, response)) {
                    return true;
                }
            } catch (ApiCommandNotAvailable e) {
                notFound = e;
            }
        }
        if (notFound != null) {
            throw notFound;
        }
        return false;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.net.httpserver.handler.HttpRequestHandler#onPostRequest
     * (org.appwork.utils.net.httpserver.requests.PostRequest, org.appwork.utils.net.httpserver.responses.HttpResponse)
     */
    @Override
    public boolean onPostRequest(final PostRequest request, final HttpResponse response) throws BasicRemoteAPIException {
        final java.util.List<HttpSessionRequestHandler<T>> handlers = this.handler;
        final T session = this.getSession(request, this.extractSessionID(request));
        ApiCommandNotAvailable notFound = null;
        for (final HttpSessionRequestHandler<T> handler : handlers) {
            try {
                if (handler.onPostSessionRequest(session, request, response)) {
                    return true;
                }
            } catch (ApiCommandNotAvailable e) {
                notFound = e;
            }
        }
        if (notFound != null) {
            throw notFound;
        }
        return false;
    }

    public void registerSessionRequestHandler(final HttpSessionRequestHandler<T> handler) {
        synchronized (this) {
            if (!this.handler.contains(handler)) {
                final java.util.List<HttpSessionRequestHandler<T>> newhandler = new ArrayList<HttpSessionRequestHandler<T>>(this.handler);
                newhandler.add(handler);
                this.handler = newhandler;
            }
        }
    }

    protected abstract boolean removeSession(final T session);

    public void unregisterSessionRequestHandler(final HttpSessionRequestHandler<T> handler) {
        synchronized (this) {
            final java.util.List<HttpSessionRequestHandler<T>> newhandler = new ArrayList<HttpSessionRequestHandler<T>>(this.handler);
            newhandler.remove(handler);
            this.handler = newhandler;
        }
    }
}
