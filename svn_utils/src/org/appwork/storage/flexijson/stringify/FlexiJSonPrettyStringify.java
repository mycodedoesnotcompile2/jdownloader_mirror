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
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.flexijson.FlexiComment;
import org.appwork.storage.flexijson.FlexiComment.Type;
import org.appwork.storage.flexijson.FlexiCommentJsonNode;
import org.appwork.storage.flexijson.FlexiCommentJsonNode.AttachLocation;
import org.appwork.storage.flexijson.FlexiJSonArray;
import org.appwork.storage.flexijson.FlexiJSonComments;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.FlexiJSonObject;
import org.appwork.storage.flexijson.FlexiJSonValue;
import org.appwork.storage.flexijson.KeyValueElement;
import org.appwork.storage.flexijson.mapper.FlexiMapperTags;
import org.appwork.utils.ReusableByteArrayOutputStream;
import org.appwork.utils.StringUtils;

/**
 * @author thomas
 * @date 30.04.2021
 *
 */
public class FlexiJSonPrettyStringify extends FlexiJSonStringBuilder {
    /**
     *
     */
    private static final int COMPACT_ARRAY_THRESHOLD = 40;

    @Override
    protected void initTags(Charset charset) {
        super.initTags(charset);
        lineCommentStart = "// ".getBytes(charset);
        inlineCommentStart = "/* ".getBytes(charset);
        inlineCommendEnd = " */".getBytes(charset);
    }

    @Override
    public String toJSONString(FlexiJSonNode node, LinkedList<String> path) {
        return super.toJSONString(node, path);
    }

    @Override
    public void appendObject(FlexiJSonObject object, JSONBuilderOutputStream out, int layer, LinkedList<String> path) throws IOException {
        FlexiJSonComments b = object.getCommentsBefore();
        FlexiJSonComments a = object.getCommentsAfter();
        if (b != null && b.size() > 0) {
            b.writeToStream(this, out, layer, path);
            newLineIfRequired(out, layer);
        }
        writeIfNotEOL(out);
        // newLineIfRequired(out, layer);
        if (lastWasNewlineOrIndent(out)) {
            spaces(out, layer);
        }
        if (object.getElements().size() == 0) {
            out.write(objectOpenTag);
            if (object.getCommentsInside() != null && object.getCommentsInside().size() > 0) {
                // if (!firstElement) {
                // // ",\r\n"
                // // out.write(commaString);
                // // out.write(newLineRN);
                // }
                newLineIfRequired(out, layer + getLayerIndent());
                boolean firstComment = true;
                for (FlexiCommentJsonNode c : object.getCommentsInside()) {
                    if (firstComment) {
                        firstComment = false;
                    } else {
                        out.write(newLineRN);
                    }
                    c.writeToStream(this, out, layer + +getLayerIndent(), path);
                }
                newLineIfRequired(out, layer);
            }
            out.write(objectCloseTag);
        } else {
            out.write(objectOpenTag);
            if (object.getCommentsInside() != null && object.getCommentsInside().size() > 0) {
                boolean firstComment = true;
                for (FlexiCommentJsonNode c : object.getCommentsInside()) {
                    if (firstComment) {
                        firstComment = false;
                    } else {
                        out.write(newLineRN);
                    }
                    c.writeToStream(this, out, layer + 1, path);
                }
            }
            boolean singleLine = true;
            boolean firstElement = true;
            boolean afterKeyValueEntry = false;
            int i = -1;
            for (KeyValueElement es : object.getElements()) {
                if (skipElement(object, es)) {
                    continue;
                }
                i++;
                writeIfNotEOL(out);
                if (firstElement) {
                    firstElement = false;
                    singleLine = !(object.size() > 1 || !(es.getValue() instanceof FlexiJSonValue));
                    if (object.hasCommentsInside()) {
                        singleLine = false;
                    }
                    if (es.getValue().hasComments()) {
                        singleLine = false;
                    }
                    if (es.hasComments()) {
                        singleLine = false;
                    }
                } else {
                    // TODO: check for other encodings!
                    if (out.getLastByteFromBackBuffer() == '{') {
                        newLineIfRequired(out, layer);
                        if (out.getLastByteFromBackBuffer() != '{') {
                            singleLine = false;
                        }
                    }
                    if (afterKeyValueEntry) {
                        if (lastWasNewlineOrIndent(out)) {
                            // e.g. after line end comment, or empty elements
                            if (out.lastWasNewline()) {
                                spaces(out, layer + getLayerIndent());
                            }
                            out.write(commaString);
                        } else {
                            out.write(commaString);
                        }
                        afterKeyValueEntry = false;
                    }
                }
                afterKeyValueEntry = true;
                if (es.getCommentsBeforeKey() != null && es.getCommentsBeforeKey().size() > 0) {
                    newLineIfRequired(out, layer + getLayerIndent());
                    es.getCommentsBeforeKey().writeToStream(this, out, layer + getLayerIndent(), path);
                }
                if (es.getKey() != null) {
                    // align keys left and
                    writeIfNotEOL(out);
                    if (!singleLine) {
                        newLineIfRequired(out, layer + getLayerIndent());
                    }
                    // out.write(":".getBytes(charset));
                    Set<FlexiMapperTags> tags = es.getValue().getTags();
                    if (isWriteDefaultValuesAsComment(object, es.getValue(), path) && tags != null && tags.contains(FlexiMapperTags.DEFAULT_VALUE)) {
                        // WARNING:THIS IS PRETTY SLOW; AND THE SUB ELEMENT IS WRITTEN TO RAM AND NOT STREAMED
                        ReusableByteArrayOutputStream baos = new ReusableByteArrayOutputStream(60);
                        JSONBuilderOutputStream tmp = new JSONBuilderOutputStream(baos, out.charset);
                        appendString(tmp, es.getKey());
                        if (es.getCommentsAfterKey() != null && es.getCommentsAfterKey().size() > 0) {
                            es.getCommentsAfterKey().writeToStream(this, tmp, 1, path);
                        }
                        if (writeIfNotEOL(tmp)) {
                            newLineIfRequired(tmp, getLayerIndent());
                        }
                        tmp.write(collonString);
                        es.getValue().writeToStream(this, tmp, getLayerIndent(), path);
                        boolean thereIsAnotherKeyValueEntry = false;
                        for (int forward = i + 1; forward < object.getElements().size(); forward++) {
                            KeyValueElement el = object.getElements().get(forward);
                            if (el.getKey() != null) {
                                thereIsAnotherKeyValueEntry = true;
                                break;
                            }
                        }
                        if (thereIsAnotherKeyValueEntry) {
                            tmp.write(commaString);
                            afterKeyValueEntry = false;
                        }
                        // new FlexiJSonComments();
                        FlexiComment dummyComment = new FlexiComment(new String(baos.toByteArray(), out.charset), FlexiComment.Type.LINE, FlexiMapperTags.DEFAULT_VALUE);
                        dummyComment.setLocation(AttachLocation.INSIDE_OBJECT);
                        dummyComment.writeToStream(this, out, layer, path);
                    } else {
                        appendString(out, es.getKey());
                        // if (isAlignColons()) {
                        // ByteArrayOutputStream baos = new ByteArrayOutputStream();
                        // appendString(new JSONBuilderOutputStream(baos), es.getKey());
                        // spaces(out, keyLength - new String(baos.toByteArray(), charset).length());
                        // }
                        if (es.getCommentsAfterKey() != null && es.getCommentsAfterKey().size() > 0) {
                            es.getCommentsAfterKey().writeToStream(this, out, layer + 1, path);
                        }
                        if (writeIfNotEOL(out)) {
                            newLineIfRequired(out, layer + getLayerIndent());
                        }
                        out.write(collonString);
                        es.getValue().writeToStream(this, out, layer + getLayerIndent(), path);
                    }
                }
            }
            // if(!lastWasNewline(out)) out.write("\r\n".getBytes(charset));
            // spaces(out, layer);
            if (!singleLine || out.lastWasNewline()) {
                newLineIfRequired(out, layer);
            }
            out.write(objectCloseTag);
        }
        if (a != null && a.size() > 0) {
            a.writeToStream(this, out, layer, path);
        }
    }

    /**
     * @param object
     * @param es
     * @return
     */
    protected boolean skipElement(FlexiJSonObject object, KeyValueElement es) {
        return false;
    }

    /**
     * @param array
     * @param i
     * @return
     */
    protected boolean skipElement(FlexiJSonArray array, int i) {
        return false;
    }

    /**
     * @param object
     * @param value
     * @param path
     * @return
     */
    public boolean isWriteDefaultValuesAsComment(FlexiJSonObject object, FlexiJSonNode value, LinkedList<String> path) {
        return false;
    }

    protected boolean lastWasNewlineOrIndent(JSONBuilderOutputStream out) {
        return out.getLastNewLineIndex() >= 0;
    }

    protected int getLayerIndent() {
        return 2;
    }

    /**
     * @param out
     * @param layer
     * @throws IOException
     */
    private int spaces(JSONBuilderOutputStream out, int layer) throws IOException {
        if (layer > 0) {
            writeIfNotEOL(out);
        }
        for (int i = 0; i < layer; i++) {
            out.write(spaceString);
        }
        return layer;
    }

    @Override
    protected void appendPrimitiveValueWithoutComments(FlexiJSonValue value, JSONBuilderOutputStream out, LinkedList<String> path, int layer) throws IOException {
        writeIfNotEOL(out);
        if (out.lastWasNewline()) {
            spaces(out, layer);
        }
        super.appendPrimitiveValueWithoutComments(value, out, path, layer);
    }

    public void appendPrimitiveValue(FlexiJSonValue value, JSONBuilderOutputStream out, int layer, LinkedList<String> path) throws IOException {
        writeIfNotEOL(out);
        FlexiJSonComments b = value.getCommentsBefore();
        FlexiJSonComments a = value.getCommentsAfter();
        if (b != null && b.size() > 0) {
            // value.toString();
            b.writeToStream(this, out, layer, path);
            if (value.getParent() instanceof FlexiJSonArray && !out.compactArrayMode) {
                newLineIfRequired(out, layer);
            }
        }
        appendPrimitiveValueWithoutComments(value, out, path, layer);
        if (a != null && a.size() > 0) {
            a.writeToStream(this, out, layer, path);
        }
    }

    @Override
    public void appendArray(FlexiJSonArray array, JSONBuilderOutputStream out, int layer, LinkedList<String> path) throws IOException {
        try {
            ReusableByteArrayOutputStream baos = new ReusableByteArrayOutputStream(60);
            JSONBuilderOutputStream tmp = new JSONBuilderOutputStream(baos, charset);
            tmp.compactArrayMode = true;
            out.compactArrayMode = true;
            FlexiJSonComments b = array.getCommentsBefore();
            FlexiJSonComments a = array.getCommentsAfter();
            boolean singleLine = array.size() <= 1;
            // loop to print small arrays compact singleline
            compactloop: for (int t = 0; t < 2; t++) {
                if (b != null && b.size() > 0) {
                    b.writeToStream(this, (tmp == null ? out : tmp), layer, path);
                    writeIfNotEOL((tmp == null ? out : tmp));
                    if (tmp == null && !lastWasNewlineOrIndent((tmp == null ? out : tmp))) {
                        // not in compact mode
                        (tmp == null ? out : tmp).write(newLineRN);
                    }
                }
                if ((lastWasNewlineOrIndent(out) || (tmp != null && lastWasNewlineOrIndent(tmp)))) {
                    if (false && tmp != null) {
                        tmp.close();
                        baos = null;
                        tmp = null;
                        out.compactArrayMode = false;
                        continue compactloop;
                    }
                    spaces((tmp == null ? out : tmp), layer);
                }
                (tmp == null ? out : tmp).write(arrayOpenTag);
                // for small primitive arrays, we write in a single line. else multiline
                boolean firstElement = true;
                for (int i = 0; i < array.size(); i++) {
                    if (skipElement(array, i)) {
                        continue;
                    }
                    try {
                        path.add("[" + i + "]");
                        final FlexiJSonNode n = array.get(i);
                        if (firstElement) {
                            firstElement = false;
                            if (!(tmp == null ? out : tmp).compactArrayMode && !singleLine) {
                                (tmp == null ? out : tmp).write(newLineRN);
                            }
                        } else {
                            final JSONBuilderOutputStream os = tmp == null ? out : tmp;
                            os.write(commaString);
                            if (out.compactArrayMode || singleLine) {
                                // ", "
                                os.write(spaceString);
                            } else {
                                // ",\r\n"
                                os.write(newLineRN);
                            }
                        }
                        if (tmp != null && !(n instanceof FlexiJSonValue)) {
                            tmp.close();
                            baos = null;
                            tmp = null;
                            out.compactArrayMode = false;
                            continue compactloop;
                        }
                        if (out.compactArrayMode) {
                            n.writeToStream(this, (tmp == null ? out : tmp), 0, path);
                        } else {
                            // n.writeToStream(this, (tmp == null ? out : tmp), layer + getLayerIndent() + 1, path); 3 sind zu viel
                            // array
                            // intend
                            n.writeToStream(this, (tmp == null ? out : tmp), layer + (singleLine ? 0 : getLayerIndent()), path);
                        }
                        if (tmp != null && tmp.transferedBytes() > COMPACT_ARRAY_THRESHOLD) {
                            // out.write(baos.toByteArray());
                            tmp.close();
                            baos = null;
                            tmp = null;
                            out.compactArrayMode = false;
                            continue compactloop;
                        }
                    } finally {
                        path.removeLast();
                    }
                }
                if (array.getCommentsInside() != null && array.getCommentsInside().size() > 0) {
                    if (!firstElement) {
                        final JSONBuilderOutputStream os = tmp == null ? out : tmp;
                        // requires a trailing coma, else it would not be a inside comment, but a "after" comment in the last element
                        // ", "
                        os.write(commaString);
                        os.write(spaceString);
                    }
                    array.getCommentsInside().writeToStream(this, (tmp == null ? out : tmp), layer + getLayerIndent(), path);
                }
                if (out.compactArrayMode) {
                    if (lastWasNewlineOrIndent((tmp == null ? out : tmp))) {
                        spaces(tmp == null ? out : tmp, layer);
                    }
                    (tmp == null ? out : tmp).write(arrayCloseTag);
                } else {
                    // TODO: check for other encodings!
                    if ((tmp == null ? out : tmp).getLastByteFromBackBuffer() != '[' && !singleLine) {
                        // write [] instead of [\r\n]
                        newLineIfRequired((tmp == null ? out : tmp), layer);
                    }
                    // if (!lastWasNewline(out)) {
                    // (tmp == null ? out : tmp).write("\r\n".getBytes(charset));
                    // }
                    // spaces((tmp == null ? out : tmp), layer + getLayerIndent());
                    (tmp == null ? out : tmp).write(arrayCloseTag);
                }
                if (a != null && a.size() > 0) {
                    if (tmp == null) {
                        out.write(newLineRN);
                    }
                    a.writeToStream(this, (tmp == null ? out : tmp), layer, path);
                }
                if (tmp == null) {
                    break;
                }
                // System.out.println(tmp.transferedBytes());
                if (tmp != null && tmp.transferedBytes() > COMPACT_ARRAY_THRESHOLD || hasNewline(baos)) {
                    tmp.close();
                    baos = null;
                    tmp = null;
                    out.compactArrayMode = false;
                    continue compactloop;
                } else if (tmp != null) {
                    tmp.close();
                    out.write(baos.toByteArray());
                    break;
                }
            }
            out.compactArrayMode = false;
        } catch (Throwable e) {
            e.printStackTrace();
            throw new WTFException(e);
        }
    }

    /**
     * @param baos
     * @return
     */
    private boolean hasNewline(ByteArrayOutputStream baos) {
        if (baos.size() >= newLineRN.length) {
            if (baos instanceof ReusableByteArrayOutputStream) {
                final byte[] bytes = ((ReusableByteArrayOutputStream) baos).getInternalBuffer();
                loop: for (int i = 0; i < baos.size() - newLineRN.length + 1; i++) {
                    for (int check = 0; check < newLineRN.length; check++) {
                        if (bytes[i + check] != newLineRN[check]) {
                            continue loop;
                        }
                    }
                    return true;
                }
            } else {
                final byte[] bytes = baos.toByteArray();
                loop: for (int i = 0; i < bytes.length - newLineRN.length + 1; i++) {
                    for (int check = 0; check < newLineRN.length; check++) {
                        if (bytes[i + check] != newLineRN[check]) {
                            continue loop;
                        }
                    }
                    return true;
                }
            }
        }
        return false;
    }

    @Override
    public void appendComment(FlexiComment comment, JSONBuilderOutputStream out, int layer, LinkedList<String> path) throws IOException {
        writeIfNotEOL(out);
        if (comment.getType() == Type.LINE) {
            if (out.lastWasNewline()) {
                spaces(out, layer);
            }
            String[] lines = getCommentLines(comment);
            for (int i = 0; i < lines.length; i++) {
                if (i > 0) {
                    newLineIfRequired(out, layer);
                }
                writeIfNotEOL(out);
                bytesToStream(out, lineCommentStart);
                stringToStream(out, lines[i]);
                ifNotEOL = lineCommentEnd;
            }
        } else {
            writeIfNotEOL(out);
            String[] lines = getCommentLines(comment);
            if (lines.length > 1) {
                newLineIfRequired(out, layer);
                stringToStream(out, "/*");
                for (int i = 0; i < lines.length; i++) {
                    stringToStream(out, "\r\n");
                    spaces(out, layer + 1);
                    stringToStream(out, "* ");
                    stringToStream(out, lines[i]);
                }
                stringToStream(out, "\r\n");
                spaces(out, layer + 1);
                stringToStream(out, "*/");
                ifNotEOL = lineCommentEnd;
            } else {
                boolean newline = !out.compactArrayMode;
                if (comment.getLocation() == null) {
                    throw new WTFException();
                }
                switch (comment.getLocation()) {
                case AFTER_KEY:
                case BEFORE_VALUE:
                case AFTER_VALUE:
                    newline = false;
                    break;
                default:
                    break;
                }
                if (newline) {
                    newLineIfRequired(out, layer);
                } else if (out.lastWasNewline()) {
                    spaces(out, layer);
                }
                stringToStream(out, "/* ");
                stringToStream(out, comment.getText().trim());
                stringToStream(out, " */");
            }
        }
    }

    protected String[] getCommentLines(FlexiComment comment) {
        ArrayList<String> ret = new ArrayList<String>();
        int max = 200;
        for (final String line : comment.getText().split("[\r\n]{1,2}")) {
            int offset = 0;
            int lastAssign = -1;
            while (true) {
                int end = line.length();
                if (end - offset > max) {
                    end = offset + max;
                }
                if (offset > 0) {
                    if (lastAssign < 0) {
                        Pattern assignPattern = Pattern.compile("(^\\s*[\\w\\d]+: )");
                        Matcher matcher = assignPattern.matcher(line);
                        if (matcher.find()) {
                            lastAssign = matcher.group(1).length();
                        } else {
                            lastAssign = 0;
                        }
                    }
                    if (end - offset + lastAssign > max) {
                        end -= lastAssign;
                    }
                }
                ret.add(StringUtils.fillPost("", " ", lastAssign) + line.substring(offset, end));
                offset = end;
                if (end == line.length()) {
                    break;
                }
            }
        }
        return ret.toArray(new String[] {});
    }

    protected void newLineIfRequired(JSONBuilderOutputStream out, int layer) throws IOException {
        writeIfNotEOL(out);
        if (!out.lastWasNewline() && out.transferedBytes() > 0) {
            if (lastWasNewlineOrIndent(out)) {
                return;// we already have newline--space sequence
            }
            stringToStream(out, "\r\n");
            spaces(out, layer);
        } else {
            spaces(out, layer);
        }
    }

    /**
     * @param out
     * @param flexiJSonComments
     * @throws IOException
     */
    public void appendComments(FlexiJSonComments comments, JSONBuilderOutputStream out, int layer, LinkedList<String> path) throws IOException {
        for (FlexiCommentJsonNode c : comments) {
            FlexiJSonNode p = comments.getParent();
            c.writeToStream(this, out, layer, path);
        }
    }
}
