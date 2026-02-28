package org.jdownloader.api;

import java.util.HashMap;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.remoteapi.RemoteAPIRequest;
import org.appwork.remoteapi.exceptions.BasicRemoteAPIException;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpserver.HttpServerConnection.ConnectionHook;
import org.appwork.utils.net.httpserver.RawHttpConnectionInterface;
import org.appwork.utils.net.httpserver.handler.ExtendedHttpRequestHandler;
import org.appwork.utils.net.httpserver.requests.AbstractGetRequest;
import org.appwork.utils.net.httpserver.requests.HttpRequest;
import org.appwork.utils.net.httpserver.requests.OptionsRequest;
import org.appwork.utils.net.httpserver.responses.HttpResponse;
import org.appwork.utils.net.httpserver.session.HttpSessionController;
import org.jdownloader.api.myjdownloader.MyJDownloaderAPISession;
import org.jdownloader.api.myjdownloader.MyJDownloaderController;
import org.jdownloader.api.myjdownloader.MyJDownloaderDirectHttpConnection;
import org.jdownloader.api.myjdownloader.MyJDownloaderHttpConnection;
import org.jdownloader.myjdownloader.client.json.ServerErrorType;

public class RemoteAPISessionControllerImp extends HttpSessionController<RemoteAPISession> implements ExtendedHttpRequestHandler, ConnectionHook {
    public RemoteAPISessionControllerImp() {
    }

    @Override
    public void onBeforeRequest(HttpRequest request, HttpResponse response) throws BasicRemoteAPIException {
        response.setHook(this);
        checkDirectConnectionToken(request);
    }

    @Override
    public void onBeforeSendHeaders(HttpResponse response) {
    }

    protected void checkDirectConnectionToken(HttpRequest request) throws BasicRemoteAPIException {
        final RawHttpConnectionInterface connection = request.getConnection();
        if (connection instanceof MyJDownloaderDirectHttpConnection && request.getRequestMethod() != RequestMethod.OPTIONS) {
            final MyJDownloaderDirectHttpConnection myConnection = (MyJDownloaderDirectHttpConnection) connection;
            final String sessionToken = myConnection.getRequestConnectToken();
            if (!MyJDownloaderController.getInstance().isSessionValid(sessionToken)) {
                final ServerErrorType type = ServerErrorType.TOKEN_INVALID;
                final ResponseCode code = ResponseCode.get(type.getCode());
                throw new BasicRemoteAPIException(null, type.name(), code, null);
            }
        }
    }

    @Override
    public void onAfterRequest(HttpRequest request, HttpResponse response, boolean handled) {
    }

    @Override
    public boolean onGetRequest(AbstractGetRequest request, HttpResponse response) throws BasicRemoteAPIException {
        if (request instanceof OptionsRequest) {
            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, "0"));
            response.setResponseCode(ResponseCode.SUCCESS_OK);
            return true;
        } else {
            return super.onGetRequest(request, response);
        }
    }

    @Override
    public void onAfterRequestException(HttpRequest request, HttpResponse response, Throwable e) {
    }

    private final HashMap<String, RemoteAPISession> sessions = new HashMap<String, RemoteAPISession>();

    @Override
    public RemoteAPISession getSession(final org.appwork.utils.net.httpserver.requests.HttpRequest request, final String id) {
        if (request.getConnection() instanceof MyJDownloaderHttpConnection) {
            return new MyJDownloaderAPISession(this, ((MyJDownloaderHttpConnection) (request.getConnection())));
        }
        synchronized (this.sessions) {
            return this.sessions.get(id);
        }
    }

    @Override
    protected RemoteAPISession newSession(RemoteAPIRequest request, final String username, final String password) {
        if (!"wron".equals(password)) {
            final RemoteAPISession session = new RemoteAPISession(this);
            synchronized (this.sessions) {
                this.sessions.put(session.getSessionID(), session);
            }
            return session;
        } else {
            return null;
        }
    }

    @Override
    protected boolean removeSession(final RemoteAPISession session) {
        if (session == null) {
            return false;
        }
        synchronized (this.sessions) {
            final RemoteAPISession ret = this.sessions.remove(session.getSessionID());
            if (ret == null) {
                return false;
            }
            ret.setAlive(false);
            return true;
        }
    }

}
