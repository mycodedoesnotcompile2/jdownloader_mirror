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

import org.appwork.remoteapi.exceptions.BasicRemoteAPIException;
import org.appwork.remoteapi.exceptions.SessionException;
import org.appwork.utils.net.httpserver.handler.HttpSessionRequestHandler;
import org.appwork.utils.net.httpserver.requests.GetRequest;
import org.appwork.utils.net.httpserver.requests.HttpRequest;
import org.appwork.utils.net.httpserver.requests.PostRequest;
import org.appwork.utils.net.httpserver.responses.HttpResponse;
import org.appwork.utils.net.httpserver.session.HttpSession;

/**
 * @author daniel
 *
 */
public class SessionRemoteAPI<T extends HttpSession> extends RemoteAPI implements HttpSessionRequestHandler<T> {
    public SessionRemoteAPI() {
        super();
    }

    protected SessionRemoteAPIRequest<T> createSessionRemoteAPIRequest(final T session, final HttpRequest request, final RemoteAPIRequest apiRequest) throws BasicRemoteAPIException {
        return new SessionRemoteAPIRequest<T>(request, apiRequest, session);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.utils.net.httpserver.handler.HttpSessionRequestHandler#
     * onGetSessionRequest(org.appwork.utils.net.httpserver.session.HttpSession, org.appwork.utils.net.httpserver.requests.GetRequest,
     * org.appwork.utils.net.httpserver.responses.HttpResponse)
     */
    @Override
    public boolean onGetSessionRequest(final T session, final GetRequest request, final HttpResponse response) throws BasicRemoteAPIException {
        RemoteAPIRequest apiRequest = this.createRemoteAPIRequestObject(request);
        if (apiRequest == null) {
            return this.onUnknownRequest(request, response);
        }
        apiRequest = this.createSessionRemoteAPIRequest(session, request, apiRequest);
        if (apiRequest.getIface().isSessionRequired() && (session == null || !session.isAlive())) {
            throw new SessionException();
        }
        try {
            this._handleRemoteAPICall(apiRequest, this.createRemoteAPIResponseObject(apiRequest, response));
        } catch (final IOException e) {
            throw new BasicRemoteAPIException(e);
        }
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.utils.net.httpserver.handler.HttpSessionRequestHandler# onPostSessionRequest
     * (org.appwork.utils.net.httpserver.session.HttpSession, org.appwork.utils.net.httpserver.requests.PostRequest,
     * org.appwork.utils.net.httpserver.responses.HttpResponse)
     */
    @Override
    public boolean onPostSessionRequest(final T session, final PostRequest request, final HttpResponse response) throws BasicRemoteAPIException {
        RemoteAPIRequest apiRequest = this.createRemoteAPIRequestObject(request);
        if (apiRequest == null) {
            return this.onUnknownRequest(request, response);
        }
        apiRequest = this.createSessionRemoteAPIRequest(session, request, apiRequest);
        if (apiRequest.getIface().isSessionRequired() && (session == null || !session.isAlive())) {
            throw new SessionException();
        }
        try {
            this._handleRemoteAPICall(apiRequest, this.createRemoteAPIResponseObject(apiRequest, response));
        } catch (final IOException e) {
            throw new BasicRemoteAPIException(e);
        }
        return true;
    }
}
