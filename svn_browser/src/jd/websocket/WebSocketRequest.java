package jd.websocket;

import java.io.IOException;
import java.net.URL;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import jd.http.Request;
import jd.http.RequestHeader;
import jd.http.URLConnectionAdapterDirectImpl;
import jd.http.requests.GetRequest;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.utils.Hash;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.Base64;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.httpconnection.HTTPConnectionImpl.KEEPALIVE;

public class WebSocketRequest extends GetRequest {
    private final static HTTPHeader SEC_WEBSOCKET_VERSION_HEADER = new HTTPHeader("Sec-WebSocket-Version", "13");
    private final static String     UPGRADE_VALUE                = "Upgrade";
    private final static HTTPHeader CONNECTION_UPGRADE_HEADER    = new HTTPHeader(HTTPConstants.HEADER_REQUEST_CONNECTION, WebSocketRequest.UPGRADE_VALUE);
    private final static HTTPHeader UPGRADE_HEADER               = new HTTPHeader("Upgrade", "websocket");
    public final static String      WEBSOCKET_ACCEPT_HASH_PREFIX = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";
    private HTTPHeader              secWebSocketKeyHeader        = null;

    public WebSocketRequest(Request cloneRequest) {
        super(cloneRequest);
    }

    public WebSocketRequest(final String url) throws IOException {
        super(url.replaceFirst("(^ws)", "http"));
    }

    public WebSocketRequest(final URL url) throws IOException {
        super(url);
    }

    protected HTTPHeader getSecWebSocketKeyHeader() {
        if (this.secWebSocketKeyHeader == null) {
            this.secWebSocketKeyHeader = new HTTPHeader("Sec-WebSocket-Key", Base64.encodeToString(Hash.getSHA1(Long.toString(System.nanoTime())).getBytes(), false).substring(0, 24));
        }
        return this.secWebSocketKeyHeader;
    }

    private HTTPHeader getConnectionUpgradeHeader(RequestHeader requestHeaders) {
        final String key = WebSocketRequest.CONNECTION_UPGRADE_HEADER.getKey();
        if (requestHeaders.contains(key)) {
            final HTTPHeader ret = requestHeaders.getHeader(key);
            if (StringUtils.contains(ret.getValue(), WebSocketRequest.UPGRADE_VALUE)) {
                return ret;
            } else {
                return new HTTPHeader(WebSocketRequest.CONNECTION_UPGRADE_HEADER.getKey(), ret.getValue() + ", " + WebSocketRequest.UPGRADE_VALUE);
            }
        } else {
            return WebSocketRequest.CONNECTION_UPGRADE_HEADER;
        }
    }

    @Override
    protected RequestHeader getDefaultRequestHeader(URL url) {
        final RequestHeader ret = super.getDefaultRequestHeader(url);
        ret.put(this.getConnectionUpgradeHeader(ret));
        ret.put(WebSocketRequest.SEC_WEBSOCKET_VERSION_HEADER);
        ret.put(this.getSecWebSocketKeyHeader());
        ret.put(WebSocketRequest.UPGRADE_HEADER);
        return ret;
    }

    @Override
    public RequestHeader getHeaders() {
        final RequestHeader ret = super.getHeaders();
        ret.put(WebSocketRequest.SEC_WEBSOCKET_VERSION_HEADER);
        ret.put(this.getSecWebSocketKeyHeader());
        ret.put(WebSocketRequest.UPGRADE_HEADER);
        ret.put(this.getConnectionUpgradeHeader(ret));
        return ret;
    }

    protected boolean verifySecWebSocketAccept(final String accept) throws IOException {
        final MessageDigest md;
        try {
            md = MessageDigest.getInstance("SHA1");
        } catch (NoSuchAlgorithmException e) {
            throw new IOException(e);
        }
        final byte[] digest = md.digest((this.getSecWebSocketKeyHeader().getValue() + WebSocketRequest.WEBSOCKET_ACCEPT_HASH_PREFIX).getBytes("UTF-8"));
        return StringUtils.equalsIgnoreCase(Base64.encodeToString(digest, false), accept);
    }

    @Override
    public void preRequest() throws IOException {
        if (this.httpConnection instanceof URLConnectionAdapterDirectImpl) {
            final URLConnectionAdapterDirectImpl httpConnection = (URLConnectionAdapterDirectImpl) this.httpConnection;
            httpConnection.setKeepAlive(KEEPALIVE.DISABLED);
        }
    }

    @Override
    protected boolean requireOutputStream() {
        return true;
    }

    @Override
    protected Request connect() throws IOException {
        final Request ret = super.connect();
        if (this.getHttpConnection().getResponseCode() != 101) {
            //
            throw new IOException("Switching Protocols failed:" + this.getHttpConnection().getResponseCode() + " " + this.getHttpConnection().getResponseMessage());
        }
        final String secWebSocketAccept = this.getResponseHeader("Sec-WebSocket-Accept");
        if (!this.verifySecWebSocketAccept(secWebSocketAccept)) {
            //
            throw new IOException("Sec-WebSocket-Accept: invalid");
        }
        if (!StringUtils.equalsIgnoreCase(this.getResponseHeader(WebSocketRequest.UPGRADE_HEADER.getKey()), "websocket")) {
            //
            throw new IOException("Invalid/Missing Upgrade header");
        }
        if (!StringUtils.containsIgnoreCase(this.getResponseHeader(WebSocketRequest.CONNECTION_UPGRADE_HEADER.getKey()), WebSocketRequest.UPGRADE_VALUE)) {
            //
            throw new IOException("Invalid/Missing Connection header");
        }
        return ret;
    }
}
