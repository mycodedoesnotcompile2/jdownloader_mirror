package org.appwork.utils.net.websocket;

import java.nio.charset.Charset;

import org.appwork.utils.formatter.HexFormatter;
import org.appwork.utils.net.websocket.WebSocketFrameHeader.OP_CODE;

public class ReadWebSocketFrame extends WebSocketFrame {
    protected final byte[] maskedPayload;

    public byte[] getMaskedPayload() {
        return this.maskedPayload;
    }

    public ReadWebSocketFrame(WebSocketFrameHeader frameHeader, byte[] payload) {
        super(frameHeader, payload);
        this.maskedPayload = payload;
    }

    public ReadWebSocketFrame(WebSocketFrameHeader frameHeader) {
        this(frameHeader, null);
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append("ReadFrame|Fin:").append(this.isFin());
        sb.append("|OpCode:").append(this.getOpCode());
        if (this.getMask() != null) {
            sb.append("|Mask:").append(HexFormatter.byteArrayToHex(this.getMask()));
        }
        sb.append("|PayLoadLength:" + this.getPayloadLength());
        if (OP_CODE.UTF8_TEXT.equals(this.getOpCode()) && this.hasPayLoad()) {
            sb.append("|UTF8_TEXT:" + new String(this.getPayload(), Charset.forName("UTF-8")));
        }
        return sb.toString();
    }
}
