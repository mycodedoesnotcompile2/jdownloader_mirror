package org.appwork.utils.net.websocket;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.util.Random;

import org.appwork.utils.IO;
import org.appwork.utils.net.LimitedInputStream;
import org.appwork.utils.net.websocket.WebSocketFrameHeader.OP_CODE;

public abstract class WebSocketEndPoint {
    // https://tools.ietf.org/html/rfc6455
    // http://www.websocket.org/echo.html
    // https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API/Writing_WebSocket_servers
    protected byte[] fill(final InputStream is, final byte[] buffer) throws IOException {
        final int length = buffer.length;
        int done = 0;
        int read = 0;
        while (done < length && (read = is.read(buffer, done, length - done)) != -1) {
            done += read;
        }
        if (done != length) {
            throw new EOFException(done + "!=" + length);
        }
        return buffer;
    }

    public byte[] nextMask() {
        final byte[] ret = new byte[4];
        new Random().nextBytes(ret);
        return ret;
    }

    protected ReadWebSocketFrame readWebSocketFrame(InputStream is) throws IOException {
        final WebSocketFrameHeader frameHeader = readWebSocketFrameHeader(is);
        if (frameHeader != null) {
            if (frameHeader.getPayloadLength() > 0) {
                final byte[] payLoad = IO.readStream(-1, new LimitedInputStream(is, frameHeader.getPayloadLength()) {
                    @Override
                    public void close() throws IOException {
                    }
                });
                return new ReadWebSocketFrame(frameHeader, payLoad);
            } else {
                return new ReadWebSocketFrame(frameHeader);
            }
        } else {
            return null;
        }
    }

    protected WebSocketFrameHeader readWebSocketFrameHeader(final InputStream is) throws IOException {
        byte[] buf = fill(is, new byte[2]);
        final boolean fin = 1 == (buf[0] & 0xff) >>> 7;// fin, finrsv1rsv2rsv3xxxx 7 rightshift
        final int opCode = buf[0] & 15;// opCode, xxxx1111
        final boolean mask = 1 == (buf[1] & 0xff) >>> 7;// mask, fxxxxxxx 7 rightshift
        long payloadLength = buf[1] & 127; // length, x1111111
        if (payloadLength == 126) {
            buf = fill(is, new byte[2]);// 16 bit unsigned
            payloadLength = ((buf[0] & 255) << 8) + ((buf[1] & 255) << 0);
        } else if (payloadLength == 127) {
            buf = fill(is, new byte[8]);// 64 bit unsigned
            payloadLength = ((long) buf[0] << 56) + ((long) (buf[1] & 255) << 48) + ((long) (buf[2] & 255) << 40) + ((long) (buf[3] & 255) << 32) + ((long) (buf[4] & 255) << 24) + ((buf[5] & 255) << 16) + ((buf[6] & 255) << 8) + ((buf[7] & 255) << 0);
        }
        final OP_CODE op_Code = OP_CODE.get(opCode);
        if (op_Code == null) {
            //
            throw new IOException("Unsupported opCode:" + opCode);
        }
        if (mask) {
            return new WebSocketFrameHeader(fin, op_Code, payloadLength, fill(is, new byte[4]));
        } else {
            return new WebSocketFrameHeader(fin, op_Code, payloadLength);
        }
    }

    /**
     * //https://tools.ietf.org/html/rfc6455#section-5.5.1
     *
     * @return
     */
    public WriteWebSocketFrame buildCloseFrame() {
        return new WriteWebSocketFrame(new WebSocketFrameHeader(true, OP_CODE.CLOSE, 0, this.nextMask()));
    }

    /**
     * https://tools.ietf.org/html/rfc6455#section-5.5.2
     */
    public WriteWebSocketFrame buildPingFrame() {
        return this.buildPingFrame(null);
    }

    /**
     * https://tools.ietf.org/html/rfc6455#section-5.5.2
     */
    public WriteWebSocketFrame buildPingFrame(byte[] payLoad) {
        if (payLoad != null && payLoad.length > 0) {
            if (payLoad.length > 125) {
                throw new IllegalArgumentException("Payload length must be <=125!");
            }
            return new WriteWebSocketFrame(new WebSocketFrameHeader(true, OP_CODE.PING, payLoad.length, this.nextMask()), payLoad);
        } else {
            return new WriteWebSocketFrame(new WebSocketFrameHeader(true, OP_CODE.PING, 0, null));
        }
    }

    /**
     * https://tools.ietf.org/html/rfc6455#section-5.6
     *
     * @param text
     * @return
     */
    public WriteWebSocketFrame buildUTF8TextFrame(final String text) {
        final byte[] bytes = text.getBytes(Charset.forName("UTF-8"));
        return new WriteWebSocketFrame(new WebSocketFrameHeader(true, OP_CODE.UTF8_TEXT, bytes.length, this.nextMask()), bytes);
    }

    public void writeFrame(WriteWebSocketFrame webSocketFrame) throws IOException {
        this.log(webSocketFrame);
        final OutputStream os = this.getOutputStream();
        os.write(webSocketFrame.getHeader());
        if (webSocketFrame.hasPayLoad()) {
            os.write(webSocketFrame.getPayload());
        }
    }

    /**
     * https://tools.ietf.org/html/rfc6455#section-5.5.1
     *
     * @param ping
     * @return
     */
    protected void onOpCode_Close(ReadWebSocketFrame close) throws IOException {
    }

    protected void onOpCode_Pong(ReadWebSocketFrame pong) throws IOException {
    }

    /**
     * https://tools.ietf.org/html/rfc6455#section-5.5.3
     *
     * @param ping
     * @return
     */
    public WriteWebSocketFrame buildPongFrame(ReadWebSocketFrame ping) {
        if (OP_CODE.PING.equals(ping.getOpCode())) {
            if (ping.hasPayLoad()) {
                return new WriteWebSocketFrame(new WebSocketFrameHeader(true, OP_CODE.PONG, ping.getPayloadLength(), ping.getMask()), ping.getPayload());
            } else {
                return new WriteWebSocketFrame(new WebSocketFrameHeader(true, OP_CODE.PONG, 0), null);
            }
        } else {
            throw new IllegalArgumentException("Wrong OpCode:" + ping.getOpCode());
        }
    }

    /**
     * https://tools.ietf.org/html/rfc6455#section-5.5.2
     *
     * @param ping
     * @return
     */
    protected void onOpCode_Ping(ReadWebSocketFrame ping) throws IOException {
    }

    protected abstract void log(WebSocketFrame webSocketFrame);

    public ReadWebSocketFrame readNextFrame() throws IOException {
        final ReadWebSocketFrame webSocketFrame = readWebSocketFrame(getInputStream());
        if (webSocketFrame != null) {
            this.log(webSocketFrame);
            switch (webSocketFrame.getOpCode()) {
            case PING:
                this.onOpCode_Ping(webSocketFrame);
                break;
            case PONG:
                this.onOpCode_Pong(webSocketFrame);
                break;
            case CLOSE:
                this.onOpCode_Close(webSocketFrame);
                break;
            default:
                break;
            }
            return webSocketFrame;
        } else {
            return null;
        }
    }

    protected abstract InputStream getInputStream() throws IOException;

    protected abstract OutputStream getOutputStream() throws IOException;
}
