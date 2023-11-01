package org.appwork.utils.net.websocket;

import java.nio.charset.Charset;

import org.appwork.utils.formatter.HexFormatter;
import org.appwork.utils.net.websocket.WebSocketFrameHeader.OP_CODE;

public class WriteWebSocketFrame extends WebSocketFrame {
    protected final byte[] unmaskedPayload;

    public WriteWebSocketFrame(WebSocketFrameHeader frameHeader, byte[] payload) {
        super(frameHeader, payload);
        this.unmaskedPayload = payload;
    }

    public WriteWebSocketFrame(WebSocketFrameHeader frameHeader) {
        this(frameHeader, null);
    }

    public byte[] getUnMaskedPayload() {
        return this.unmaskedPayload;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append("WriteFrame|Fin:").append(this.isFin());
        sb.append("|OpCode:").append(this.getOpCode());
        if (this.getMask() != null) {
            sb.append("|Mask:").append(HexFormatter.byteArrayToHex(this.getMask()));
        }
        sb.append("|PayLoadLength:" + this.getPayloadLength());
        if (OP_CODE.UTF8_TEXT.equals(this.getOpCode()) && this.hasPayLoad()) {
            sb.append("|UTF8_TEXT:" + new String(this.getUnMaskedPayload(), Charset.forName("UTF-8")));
        }
        return sb.toString();
    }
}
