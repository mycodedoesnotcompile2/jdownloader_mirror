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
package org.appwork.storage.flexijson.stringify;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CharsetEncoder;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.flexijson.FlexiComment;
import org.appwork.storage.flexijson.FlexiComment.Type;
import org.appwork.storage.flexijson.FlexiCommentJsonNode;
import org.appwork.storage.flexijson.FlexiJSonArray;
import org.appwork.storage.flexijson.FlexiJSonComments;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.FlexiJSonObject;
import org.appwork.storage.flexijson.FlexiJSonValue;
import org.appwork.storage.flexijson.KeyValueElement;
import org.appwork.utils.JDK8BufferHelper;
import org.appwork.utils.net.CountingOutputStream;

/**
 * @author thomas
 * @date 30.04.2021
 *
 */
public class FlexiJSonStringBuilder {
    /**
     * @author thomas
     * @date 12.05.2021
     *
     */
    public static class JSONBuilderOutputStream extends CountingOutputStream {
        /**
         *
         */
        private final ByteArrayOutputStream baos;
        private long                        lastNewLineIndex = -1;
        private final byte                  space;
        private final byte[]                newLineRN;
        private final byte[]                lookBackBuffer;
        public boolean                      compactArrayMode = false;
        private final byte[]                writeBuf         = new byte[1];
        public final Charset                charset;

        /**
         * @param charset
         * @param byteArrayOutputStream
         */
        public JSONBuilderOutputStream(OutputStream out, Charset charset) {
            super(out);
            space = " ".getBytes(charset)[0];
            newLineRN = "\r\n".getBytes(charset);
            lookBackBuffer = new byte[newLineRN.length];
            if (out instanceof ByteArrayOutputStream) {
                baos = (ByteArrayOutputStream) out;
            } else {
                baos = null;
            }
            this.charset = charset;
        }

        @Override
        public void write(byte[] b) throws IOException {
            write(b, 0, b.length);
        }

        @Override
        public void write(byte[] b, int off, int len) throws IOException {
            if (len > 0) {
                super.write(b, off, len);
                if (len >= lookBackBuffer.length) {
                    System.arraycopy(b, off + len - lookBackBuffer.length, lookBackBuffer, 0, lookBackBuffer.length);
                } else {
                    final int leftShift = lookBackBuffer.length - len;
                    System.arraycopy(lookBackBuffer, leftShift, lookBackBuffer, 0, len);
                    System.arraycopy(b, off, lookBackBuffer, leftShift, len);
                }
                // if the last was newline, lastNewLineIndex is set to the newline index, else -1
                if (lastNewLineIndex >= 0) {
                    for (int i = off; i < off + len; i++) {
                        if (b[i] != space) {
                            lastNewLineIndex = -1;
                        }
                    }
                }
                if (Arrays.equals(lookBackBuffer, newLineRN)) {
                    lastNewLineIndex = transferedBytes();
                }
            }
        }

        public long getLastNewLineIndex() {
            return lastNewLineIndex;
        }

        public byte[] getLookBackBuffer() {
            return lookBackBuffer;
        }

        public byte getLastByteFromBackBuffer() {
            return lookBackBuffer[lookBackBuffer.length - 1];
        }

        @Override
        public void write(int b) throws IOException {
            writeBuf[0] = (byte) b;
            write(writeBuf, 0, 1);
        }

        @Override
        public String toString() {
            if (baos == null) {
                return super.toString();
            } else {
                // debug only
                try {
                    return baos.toString(charset.name());
                } catch (UnsupportedEncodingException e) {
                    return super.toString();
                }
            }
        }

        /**
         * @return
         */
        public boolean lastWasNewline() {
            return transferedBytes() == 0 || Arrays.equals(lookBackBuffer, newLineRN);
        }
    }

    protected final Charset      charset;
    protected byte[]             ifNotEOL;
    private final CharsetEncoder charEncoder;
    private ByteBuffer           byteBuffer;
    private CharBuffer           charBuffer;
    protected byte[]             lineCommentStart;
    protected byte[]             lineCommentEnd;
    protected byte[]             inlineCommentStart;
    protected byte[]             inlineCommendEnd;
    protected byte[]             spaceString;
    protected byte[]             commaString;
    protected byte[]             nullString;
    protected byte[]             undefinedString;
    protected byte[]             collonString;
    protected byte[]             zeroString;
    protected byte[]             escapedEscaped;
    protected byte[]             escapedSolidus;
    protected byte[]             escapedEscapedUString;
    protected byte[]             escapedEscapedNString;
    protected byte[]             escapedEscapedRString;
    protected byte[]             escapedEscapedTString;
    protected byte[]             escapedEscapedFString;
    protected byte[]             escapedEscapedBString;
    protected byte[]             escapedQuotationMarks;
    protected byte[]             escapedEscapedQuotationMarks;
    protected byte[]             arrayOpenTag;
    protected byte[]             arrayCloseTag;
    protected byte[]             objectOpenTag;
    protected byte[]             objectCloseTag;
    protected byte[]             trueString;
    protected byte[]             falseString;
    protected byte[]             newLineRN;
    private byte[]               referenceStartTag;
    private byte[]               referenceEndTag;

    /**
    *
    */
    public FlexiJSonStringBuilder() {
        this.charset = Charset.forName("UTF-8");
        charEncoder = charset.newEncoder();
        byteBuffer = ByteBuffer.allocate((int) charEncoder.averageBytesPerChar() * 16);
        // byteBuffer.flip();
        charBuffer = CharBuffer.allocate(16);
        // charBuffer.flip();
        initTags(charset);
    }

    /**
     *
     */
    protected void initTags(Charset charset) {
        lineCommentStart = "//".getBytes(charset);
        lineCommentEnd = "\r\n".getBytes(charset);
        inlineCommentStart = "/*".getBytes(charset);
        inlineCommendEnd = "*/".getBytes(charset);
        spaceString = " ".getBytes(charset);
        commaString = ",".getBytes(charset);
        nullString = "null".getBytes(charset);
        undefinedString = "undefined".getBytes(charset);
        collonString = ":".getBytes(charset);
        zeroString = "0".getBytes(charset);
        escapedEscaped = "\\\\".getBytes(charset);
        escapedSolidus = "\\/".getBytes(charset);
        escapedEscapedUString = "\\u".getBytes(charset);
        escapedEscapedNString = "\\n".getBytes(charset);
        escapedEscapedRString = "\\r".getBytes(charset);
        escapedEscapedTString = "\\t".getBytes(charset);
        escapedEscapedBString = "\\b".getBytes(charset);
        escapedEscapedFString = "\\f".getBytes(charset);
        escapedQuotationMarks = "\"".getBytes(charset);
        escapedEscapedQuotationMarks = "\\\"".getBytes(charset);
        arrayOpenTag = "[".getBytes(charset);
        arrayCloseTag = "]".getBytes(charset);
        objectOpenTag = "{".getBytes(charset);
        objectCloseTag = "}".getBytes(charset);
        referenceStartTag = "${".getBytes(charset);
        referenceEndTag = "}".getBytes(charset);
        trueString = "true".getBytes(charset);
        falseString = "false".getBytes(charset);
        newLineRN = "\r\n".getBytes(charset);
    }

    /**
     * @param linkedList
     * @param parsed
     * @return
     * @throws IOException
     */
    public String toJSONString(FlexiJSonNode node, LinkedList<String> path) {
        final ByteArrayOutputStream os = new ByteArrayOutputStream() {
            @Override
            public synchronized String toString() {
                return new String(buf, 0, count, charset);
            }
        };
        toJSONString(node, os, path);
        return os.toString();
    }

    public void toJSONString(FlexiJSonNode node, OutputStream out, LinkedList<String> path) {
        try {
            if (path == null) {
                path = new LinkedList<String>();
            }
            ifNotEOL = null;
            node.writeToStream(this, new JSONBuilderOutputStream(out, charset), 0, path);
        } catch (IOException e) {
            // IOException for ByteArrayOutputStream?
            throw new WTFException(e);
        }
    }

    /**
     * @param out
     * @param layer
     *            TODO
     * @param flexiJSonValue
     * @throws IOException
     */
    public void appendPrimitiveValue(FlexiJSonValue value, JSONBuilderOutputStream out, int layer, LinkedList<String> path) throws IOException {
        writeIfNotEOL(out);
        FlexiJSonComments b = value.getCommentsBefore();
        FlexiJSonComments a = value.getCommentsAfter();
        if (b != null && b.size() > 0) {
            // value.toString();
            b.writeToStream(this, out, layer, path);
        }
        appendPrimitiveValueWithoutComments(value, out, path, layer);
        if (a != null && a.size() > 0) {
            a.writeToStream(this, out, layer, path);
        }
    }

    protected void appendPrimitiveValueWithoutComments(FlexiJSonValue value, JSONBuilderOutputStream out, LinkedList<String> path, int layer) throws IOException {
        writeIfNotEOL(out);
        switch (value.getType()) {
        case BOOLEAN:
            appendBoolean(value, out);
            break;
        case DOUBLE:
            appendDouble(value, out);
            break;
        case LONG:
            appendLong(value, out);
            break;
        case STRING:
            appendString(value, out);
            break;
        case NULL:
            appendNull(value, out);
            break;
        case UNDEFINED:
            appendUndefined(value, out);
            break;
        case REFERENCE:
            appendReference(value, out);
            break;
        default:
            new WTFException("Not supported:" + value.getType());
        }
    }

    /**
     * @param value
     * @param out
     * @throws IOException
     */
    private void appendReference(FlexiJSonValue value, JSONBuilderOutputStream out) throws IOException {
        bytesToStream(out, referenceStartTag);
        appendStringWithoutQuotes(out, value.getStringValue());
        bytesToStream(out, referenceEndTag);
    }

    /**
     * @param value
     * @param out
     * @throws IOException
     */
    protected void appendUndefined(FlexiJSonValue value, JSONBuilderOutputStream out) throws IOException {
        bytesToStream(out, undefinedString);
    }

    /**
     * @param out
     * @param string
     * @throws IOException
     */
    protected void stringToStream(JSONBuilderOutputStream out, String string) throws IOException {
        out.write(string.getBytes(charset));
    }

    /**
     * @param value
     * @param out
     * @throws IOException
     */
    protected void appendNull(FlexiJSonValue value, JSONBuilderOutputStream out) throws IOException {
        bytesToStream(out, nullString);
    }

    /**
     * @param out
     * @param bytes
     * @throws IOException
     */
    protected void bytesToStream(JSONBuilderOutputStream out, byte[] bytes) throws IOException {
        out.write(bytes);
    }

    /**
     * @param value
     * @param out
     * @throws IOException
     */
    protected void appendLong(FlexiJSonValue value, JSONBuilderOutputStream out) throws IOException {
        stringToStream(out, value.getValue().toString());
    }

    /**
     * @param value
     * @param out
     * @throws IOException
     */
    protected void appendString(FlexiJSonValue value, JSONBuilderOutputStream out) throws IOException {
        String s = ((String) value.getValue());
        appendString(out, s);
    }

    protected void finalizeAppendString(JSONBuilderOutputStream out) throws IOException {
        if (charBuffer.hasRemaining()) {
            JDK8BufferHelper.clear(byteBuffer);
            JDK8BufferHelper.flip(charBuffer);
            charEncoder.encode(charBuffer, byteBuffer, true);
            JDK8BufferHelper.flip(byteBuffer);
            out.write(byteBuffer.array(), byteBuffer.position(), byteBuffer.remaining());
            JDK8BufferHelper.clear(charBuffer);
        }
    }

    protected void appendString(JSONBuilderOutputStream out, String s) throws IOException, CharacterCodingException {
        bytesToStream(out, escapedQuotationMarks);
        appendStringWithoutQuotes(out, s);
        bytesToStream(out, escapedQuotationMarks);
    }

    private void appendStringWithoutQuotes(JSONBuilderOutputStream out, String s) throws IOException {
        try {
            for (int i = 0; i < s.length(); i++) {
                final char ch = s.charAt(i);
                switch (ch) {
                case '"':
                    finalizeAppendString(out);
                    bytesToStream(out, escapedEscapedQuotationMarks);
                    continue;
                // case '\'':
                // We support only " in the stringifier
                // bytesToStream(out, "\\'".getBytes(charset));
                // continue;
                // to discuss. escaping solidus would make comments look very strange.
                // case '/':
                // finalizeAppendString(out);
                // bytesToStream(out, escapedSolidus);
                // continue;
                case '\\':
                    finalizeAppendString(out);
                    bytesToStream(out, escapedEscaped);
                    continue;
                case '\b':
                    finalizeAppendString(out);
                    bytesToStream(out, escapedEscapedBString);
                    continue;
                case '\f':
                    finalizeAppendString(out);
                    bytesToStream(out, escapedEscapedFString);
                    continue;
                case '\n':
                    finalizeAppendString(out);
                    bytesToStream(out, escapedEscapedNString);
                    continue;
                case '\r':
                    finalizeAppendString(out);
                    bytesToStream(out, escapedEscapedRString);
                    continue;
                case '\t':
                    finalizeAppendString(out);
                    bytesToStream(out, escapedEscapedTString);
                    continue;
                }
                // '\u0000' && ch <= '\u001F' are controll characters )(
                // https://www.ietf.org/rfc/rfc8259.txt 7. Strings)
                // the text says U+0000 >>> to U+001F but the syntax diagram just
                // says control character, which in >>> Unicode 6.3 also includes
                // U+007F to U+009F
                // http://www.unicode.org/charts/PDF/U2000.pdf
                // todo. implement \u1D11E and other chars that require more \ u sequences
                // newer RFC: https://www.ietf.org/rfc/rfc8259.txt
                if (ch >= '\u0000' && ch <= '\u001F' || ch >= '\u007F' && ch <= '\u009F' || ch >= '\u2000' && ch <= '\u20FF') {
                    finalizeAppendString(out);
                    final String ss = Integer.toHexString(ch);
                    bytesToStream(out, escapedEscapedUString);
                    for (int k = 0; k < 4 - ss.length(); k++) {
                        bytesToStream(out, zeroString);
                    }
                    stringToStream(out, ss.toUpperCase(Locale.ENGLISH));
                    continue;
                } else {
                    charBuffer.append(ch);
                    if (!Character.isHighSurrogate(ch) && !Character.isLowSurrogate(ch)) {
                        finalizeAppendString(out);
                    }
                }
            }
        } finally {
            // ensure that the buffers are cleared
            finalizeAppendString(out);
        }
    }

    /**
     * @param value
     * @param out
     * @throws IOException
     */
    protected void appendDouble(FlexiJSonValue value, JSONBuilderOutputStream out) throws IOException {
        stringToStream(out, value.getValue().toString());
    }

    /**
     * @param value
     * @param out
     * @throws IOException
     */
    protected void appendBoolean(FlexiJSonValue value, JSONBuilderOutputStream out) throws IOException {
        if (Boolean.TRUE.equals(value.getValue())) {
            bytesToStream(out, trueString);
        } else {
            bytesToStream(out, falseString);
        }
    }

    /**
     * @param out
     * @param layer
     *            TODO
     * @param flexiComment
     * @throws IOException
     */
    public void appendComment(FlexiComment comment, JSONBuilderOutputStream out, int layer, LinkedList<String> path) throws IOException {
        writeIfNotEOL(out);
        if (comment.getType() == Type.LINE) {
            bytesToStream(out, lineCommentStart);
            stringToStream(out, comment.getText());
            ifNotEOL = lineCommentEnd;
        } else {
            bytesToStream(out, inlineCommentStart);
            stringToStream(out, comment.getText());
            bytesToStream(out, inlineCommendEnd);
        }
    }

    protected boolean writeIfNotEOL(JSONBuilderOutputStream out) throws IOException {
        if (ifNotEOL != null) {
            bytesToStream(out, ifNotEOL);
            ifNotEOL = null;
            return true;
        } else {
            return false;
        }
    }

    /**
     * @param flexiJSonArray
     * @param out
     * @param layer
     * @throws IOException
     */
    public void appendArray(FlexiJSonArray array, JSONBuilderOutputStream out, int layer, LinkedList<String> path) throws IOException {
        writeIfNotEOL(out);
        FlexiJSonComments b = array.getCommentsBefore();
        FlexiJSonComments a = array.getCommentsAfter();
        if (b != null) {
            b.writeToStream(this, out, 0, path);
        }
        bytesToStream(out, arrayOpenTag);
        boolean first = true;
        for (int i = 0; i < array.size(); i++) {
            FlexiJSonNode n = array.get(i);
            if (first) {
                first = false;
            } else {
                bytesToStream(out, commaString);
            }
            n.writeToStream(this, out, 0, path);
        }
        // inside comments in arrays musst be after the trailing comma. in javascri0t, a trailing comma is not a new element, we can add the
        // comment here without adding a new element.
        // before the trailing comma, this would be an after comment for the last element.
        // at begin of array, this would be a before comment for the first element.
        if (array.getCommentsInside() != null && array.getCommentsInside().size() > 0) {
            if (!first) {
                // requires a trailing coma, else it would not be a inside comment, but a "after" comment in the last element
                bytesToStream(out, commaString);
            }
            array.getCommentsInside().writeToStream(this, out, 0, path);
        }
        bytesToStream(out, arrayCloseTag);
        if (a != null) {
            a.writeToStream(this, out, 0, path);
        }
    }

    /**
     * @param out
     * @param layer
     *            TODO
     * @param flexiJSonObject
     * @throws IOException
     */
    public void appendObject(FlexiJSonObject object, JSONBuilderOutputStream out, int layer, LinkedList<String> path) throws IOException {
        writeIfNotEOL(out);
        FlexiJSonComments b = object.getCommentsBefore();
        FlexiJSonComments a = object.getCommentsAfter();
        if (b != null) {
            b.writeToStream(this, out, 0, path);
        }
        writeIfNotEOL(out);
        bytesToStream(out, objectOpenTag);
        if (object.getCommentsInside() != null) {
            object.getCommentsInside().writeToStream(this, out, 0, path);
        }
        boolean first = true;
        for (KeyValueElement es : getObjectElementsList(object)) {
            writeIfNotEOL(out);
            if (first) {
                first = false;
            } else {
                bytesToStream(out, commaString);
            }
            first = false;
            if (es.getCommentsBeforeKey() != null) {
                es.getCommentsBeforeKey().writeToStream(this, out, layer + 1, path);
            }
            if (es.getKey() != null) {
                writeIfNotEOL(out);
                appendString(out, es.getKey());
                if (es.getCommentsAfterKey() != null) {
                    es.getCommentsAfterKey().writeToStream(this, out, layer + 1, path);
                }
                writeIfNotEOL(out);
                bytesToStream(out, collonString);
                es.getValue().writeToStream(this, out, layer, path);
            }
        }
        bytesToStream(out, objectCloseTag);
        if (a != null) {
            a.writeToStream(this, out, 0, path);
        }
    }

    protected List<KeyValueElement> getObjectElementsList(FlexiJSonObject object) {
        return object.getElements();
    }

    /**
     * @param out
     * @param layer
     *            TODO
     * @param flexiJSonComments
     * @throws IOException
     */
    public void appendComments(FlexiJSonComments comments, JSONBuilderOutputStream out, int layer, LinkedList<String> path) throws IOException {
        for (FlexiCommentJsonNode c : comments) {
            writeIfNotEOL(out);
            c.writeToStream(this, out, 0, path);
        }
    }

    /**
     * @param parsed
     * @return
     */
    public String toJSONString(FlexiJSonNode node) {
        return toJSONString(node, new LinkedList<String>());
    }
}
