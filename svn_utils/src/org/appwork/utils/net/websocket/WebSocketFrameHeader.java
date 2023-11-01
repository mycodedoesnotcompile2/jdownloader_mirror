package org.appwork.utils.net.websocket;

import org.appwork.utils.formatter.HexFormatter;

public class WebSocketFrameHeader {
    public static enum OP_CODE {
        CONTINUATION(0x0),
        UTF8_TEXT(0x1),
        BINARY(0x2),
        CLOSE(0x8),
        PING(0x9),
        PONG(0xA);
        private final int opCode;

        final int getOpCode() {
            return this.opCode;
        }

        private OP_CODE(int opCode) {
            this.opCode = opCode;
        }

        static OP_CODE get(int opCode) {
            for (final OP_CODE value : OP_CODE.values()) {
                if (value.getOpCode() == opCode) {
                    return value;
                }
            }
            return null;
        }
    }

    private final boolean fin;

    public boolean isFin() {
        return this.fin;
    }

    public OP_CODE getOpcode() {
        return this.opCode;
    }

    public boolean hasPayLoad() {
        return this.getPayloadLength() > 0;
    }

    public long getPayloadLength() {
        return this.payloadLength;
    }

    public byte[] getMask() {
        return this.mask;
    }

    private final OP_CODE opCode;
    private final long    payloadLength;
    private final byte[]  mask;

    public byte[] getBytes() {
        int length = 1;// fin and opCode
        if (this.payloadLength <= 125) {
            length += 1;// mask|length;
        } else if (this.payloadLength > 125 && this.payloadLength <= ((1 << 16) - 1)) {
            length += 3;// mask|length + 2 bytes, 16 bit unsigned
        } else {
            length += 9;// mask|length + 8 bytes,64 bit unsigned
        }
        if (this.mask != null) {
            length += 4;// mask, 4 bytes
        }
        int writeIndex = 0;
        final byte[] ret = new byte[length];
        ret[writeIndex++] = (byte) ((this.isFin() ? 1 << 7 : 0) + (this.getOpcode().getOpCode() & 0xFF));
        if (this.payloadLength <= 125) {
            ret[writeIndex++] = (byte) ((this.mask != null ? 1 << 7 : 0) + (Math.max(0, this.payloadLength) & 0xFF));
        } else if (this.payloadLength > 125 && this.payloadLength <= ((1 << 16) - 1)) {
            ret[writeIndex++] = (byte) ((this.mask != null ? 1 << 7 : 0) + 126);
            ret[writeIndex++] = (byte) (this.payloadLength >>> 8 & 0xFF);
            ret[writeIndex++] = (byte) (this.payloadLength >>> 0 & 0xFF);
        } else {
            ret[writeIndex++] = (byte) ((this.mask != null ? 1 << 7 : 0) + 127);
            for (int shift = 56; shift >= 0; shift -= 8) {
                ret[writeIndex++] = (byte) (this.payloadLength >>> shift & 0xFF);
            }
        }
        if (this.mask != null) {
            ret[writeIndex++] = this.mask[0];
            ret[writeIndex++] = this.mask[1];
            ret[writeIndex++] = this.mask[2];
            ret[writeIndex++] = this.mask[3];
        }
        return ret;
    }

    public WebSocketFrameHeader(boolean fin, OP_CODE opCode, long payloadLength, byte[] mask) {
        this.fin = fin;
        this.opCode = opCode;
        this.payloadLength = payloadLength;
        this.mask = mask;
        if (mask != null && mask.length != 4) {
            //
            throw new IllegalArgumentException("mask length must be 4 bytes!");
        }
    }

    public WebSocketFrameHeader(boolean fin, OP_CODE opCode, long payloadLength) {
        this(fin, opCode, payloadLength, null);
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append("Fin:").append(this.isFin());
        sb.append("|OpCode:").append(this.getOpcode());
        if (this.getMask() != null) {
            sb.append("|Mask:").append(HexFormatter.byteArrayToHex(this.getMask()));
        }
        sb.append("|PayLoadLength:" + this.getPayloadLength());
        return sb.toString();
    }
}