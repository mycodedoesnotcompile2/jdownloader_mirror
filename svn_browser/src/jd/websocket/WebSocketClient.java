package jd.websocket;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.util.concurrent.atomic.AtomicBoolean;

import jd.http.Browser;

import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.net.websocket.ReadWebSocketFrame;
import org.appwork.utils.net.websocket.WebSocketFrame;

public class WebSocketClient extends org.appwork.utils.net.websocket.WebSocketEndPoint {

    protected final URL           url;
    protected final Browser       br;
    protected WebSocketRequest    webSocketRequest = null;
    protected final AtomicBoolean closed           = new AtomicBoolean(false);

    public WebSocketClient(final Browser br, final URL url) {
        this.url = url;
        this.br = br;
        br.setAllowedResponseCodes(new int[] { 101 });
    }

    public URL getURL() {
        return this.url;
    }

    protected WebSocketRequest buildWebSocketRequest(final URL url) throws IOException {
        return new WebSocketRequest(url);
    }

    public void connect() throws IOException {
        if (this.webSocketRequest != null) {
            throw new IOException("already connected!");
        } else {
            this.webSocketRequest = this.buildWebSocketRequest(this.getURL());
            this.br.openRequestConnection(this.webSocketRequest);
        }
    }

    @Override
    protected void log(WebSocketFrame webSocketFrame) {
        final LogInterface logger = this.br.getLogger();
        if (logger != null && this.br.isDebug()) {
            final StringBuilder sb = new StringBuilder();
            try {
                final StackTraceElement[] stackTrace = new Exception().getStackTrace();
                for (final StackTraceElement stack : stackTrace) {
                    if ("jd.websocket.WebSocketClient".equals(stack.getClassName())) {
                        continue;
                    }
                    if (sb.length() > 0) {
                        sb.append("\r\n");
                    }
                    sb.append(stack.toString());
                }
            } catch (final Throwable e) {
            }
            if (sb.length() > 0) {
                sb.insert(0, "\r\nCaller:");
                sb.append("\r\n");
            }
            sb.append("BrowserID:" + this.br.getBrowserID() + "|RequestID:" + this.webSocketRequest.getRequestID() + "|URL:" + this.webSocketRequest.getURL());
            if (webSocketFrame instanceof ReadWebSocketFrame) {
                sb.append("\r\n----------------READ WebSocketFrame Content-------------\r\n");
            } else {
                sb.append("\r\n----------------WRITE WebSocketFrame Content-------------\r\n");
            }
            if (this.br.isVerbose()) {
                sb.append(String.valueOf(webSocketFrame));
            } else if (this.br.isDebug()) {
                sb.append(String.valueOf(webSocketFrame.getFrameHeader()));
            }
            logger.finest(sb.toString());
        }
    }

    @Override
    protected void onOpCode_Close(ReadWebSocketFrame close) throws IOException {
        this.disconnect();
    }

    @Override
    protected void onOpCode_Ping(ReadWebSocketFrame ping) throws IOException {
        this.writeFrame(this.buildPongFrame(ping));
    }

    @Override
    protected InputStream getInputStream() throws IOException {
        this.checkWebSocket();
        return this.webSocketRequest.getHttpConnection().getInputStream();
    }

    protected void checkWebSocket() throws IOException {
        if (this.webSocketRequest == null) {
            throw new IOException("Websocket is unconnected!");
        } else if (this.closed.get()) {
            throw new IOException("Websocket is closed!");
        }
    }

    @Override
    protected OutputStream getOutputStream() throws IOException {
        this.checkWebSocket();
        return this.webSocketRequest.getHttpConnection().getOutputStream();
    }

    public void close() throws IOException {
        try {
            this.writeFrame(this.buildCloseFrame());
        } finally {
            this.disconnect();
        }
    }

    public void disconnect() throws IOException {
        try {
            final WebSocketRequest webSocketRequest = this.webSocketRequest;
            if (webSocketRequest != null) {
                webSocketRequest.disconnect();
            }
        } finally {
            this.closed.set(true);
        }
    }

}
