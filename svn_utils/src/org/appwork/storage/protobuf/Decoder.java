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
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.storage.protobuf;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * @author daniel
 * @date May 23, 2023
 *
 *
 *       https://protobuf.dev/programming-guides/
 */
public class Decoder {

    private final static Charset UTF8 = Charset.forName("UTF-8");

    public static class RecordTag {
        private final WireType type;

        public WireType getType() {
            return type;
        }

        public int getFieldNum() {
            return fieldNum;
        }

        private final int fieldNum;

        private RecordTag(WireType type, int fieldNum) {
            this.type = type;
            this.fieldNum = fieldNum;
        }

        @Override
        public String toString() {
            return getType() + "@" + getFieldNum();
        }
    }

    public static class Record {

        private final RecordTag tag;

        public Object getPayLoad() {
            return payLoad;
        }

        protected void setPayLoad(Object payLoad) {
            this.payLoad = payLoad;
        }

        public RecordTag getTag() {
            return tag;
        }

        public WireType getType() {
            return getTag().getType();
        }

        public int getFieldNum() {
            return getTag().getFieldNum();
        }

        private Object payLoad;

        private Record(RecordTag tag) {
            this.tag = tag;
        }

        @Override
        public String toString() {
            if (getPayLoad() instanceof byte[]) {
                return getTag() + "=" + new String((byte[]) getPayLoad());
            } else {
                return getTag() + "=" + getPayLoad();
            }
        }
    }

    public Map<Integer, Object> toMap(List<Record> records, final boolean byteArrayToString) {
        if (records != null) {
            final LinkedHashMap<Integer, Object> ret = new LinkedHashMap<Integer, Object>();
            for (Record record : records) {
                final Integer fieldNum = record.getFieldNum();
                Object entry = ret.get(fieldNum);
                Object payLoad = record.getPayLoad();
                if (payLoad instanceof List) {
                    payLoad = toMap((List<Record>) payLoad, byteArrayToString);
                } else if (payLoad instanceof byte[] && byteArrayToString) {
                    payLoad = new String((byte[]) payLoad, UTF8);
                }
                if (entry == null) {
                    ret.put(fieldNum, payLoad);
                } else if (entry instanceof List) {
                    ((List) entry).add(payLoad);
                } else {
                    final List<Object> list = new ArrayList<Object>();
                    list.add(entry);
                    list.add(payLoad);
                    ret.put(fieldNum, list);
                }
            }
            return ret;
        } else {
            return null;
        }
    }

    /**
     * https://protobuf.dev/programming-guides/encoding/
     *
     * https://protobuf-decoder.netlify.app/
     *
     */
    public List<Record> decode(final InputStream is) throws Exception {
        final List<Record> records = new ArrayList<Decoder.Record>();
        while (true) {
            RecordTag next = nextField(is);
            if (next == null) {
                break;
            } else {
                final Record record = new Record(next);
                records.add(record);
                switch (next.getType()) {
                case VARINT: {
                    final long value = readVarInt(is, true);
                    if (value <= Byte.MAX_VALUE && value >= Byte.MIN_VALUE) {
                        record.setPayLoad(Byte.valueOf((byte) value));
                    } else if (value <= Short.MAX_VALUE && value >= Short.MIN_VALUE) {
                        record.setPayLoad(Short.valueOf((short) value));
                    } else if (value <= Integer.MAX_VALUE && value >= Integer.MIN_VALUE) {
                        record.setPayLoad(Integer.valueOf((int) value));
                    } else {
                        record.setPayLoad(value);
                    }
                    continue;
                }
                case I32:
                case I64: {
                    final int len = WireType.I64.equals(next.getType()) ? 8 : 4;
                    final byte[] bytes = new byte[len];
                    new DataInputStream(is).readFully(bytes);
                    final ByteBuffer buffer = ByteBuffer.wrap(bytes);
                    final long value = len == 8 ? buffer.getLong() : buffer.getInt();
                    if (value <= Byte.MAX_VALUE && value >= Byte.MIN_VALUE) {
                        record.setPayLoad(Byte.valueOf((byte) value));
                    } else if (value <= Short.MAX_VALUE && value >= Short.MIN_VALUE) {
                        record.setPayLoad(Short.valueOf((short) value));
                    } else if (value <= Integer.MAX_VALUE && value >= Integer.MIN_VALUE) {
                        record.setPayLoad(Integer.valueOf((int) value));
                    } else {
                        record.setPayLoad(value);
                    }
                    continue;
                }
                case LEN: {
                    final long len = readVarInt(is, true);
                    if (len > Integer.MAX_VALUE) {
                        throw new ArithmeticException(len + " doesn't fit into Integer");
                    }
                    final byte[] bytes = new byte[(int) len];
                    new DataInputStream(is).readFully(bytes);
                    record.setPayLoad(bytes);
                    try {
                        final List<Record> value = decode(new ByteArrayInputStream(bytes));
                        if (value != null) {
                            record.setPayLoad(value);
                        }
                    } catch (Exception e) {
                        // keep raw byte[] data
                        // e.printStackTrace();
                    }
                    continue;
                }
                default:
                    throw new Exception("Unsupported:" + next);
                }
            }
        }
        if (records.size() == 0) {
            return null;
        } else {
            return records;
        }
    }

    public RecordTag nextField(InputStream is) throws IOException {
        final long tag = readVarInt(is, false);
        if (tag == -1) {
            return null;
        } else {
            final int wireTypeID = (int) tag & 7;
            final int fieldNum = (int) tag >> 3;
            final WireType wireType = WireType.parse(wireTypeID);
            if (wireType != null) {
                return new RecordTag(wireType, fieldNum);
            } else {
                throw new IOException("Unsupported wireType:" + wireTypeID);
            }
        }
    }

    public long readVarInt(InputStream is, final boolean alwaysThrowEOF) throws IOException {
        long ret = 0;
        int shift = 0;
        while (true) {
            final int read = is.read();
            if (read == -1) {
                if (shift > 0 && alwaysThrowEOF) {
                    throw new EOFException();
                } else {
                    return -1;
                }
            } else {
                final long value = read & 0x7f;
                final int msb = (read & 0xff) >> 7;
                ret |= (value << shift);
                if (msb == 0) {
                    return ret;
                } else {
                    shift += 7;
                }
            }
        }
    }

}
