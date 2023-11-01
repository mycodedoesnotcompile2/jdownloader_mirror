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

import java.io.IOException;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.Set;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.flexijson.FlexiComment;
import org.appwork.storage.flexijson.FlexiCommentJsonNode;
import org.appwork.storage.flexijson.FlexiJSonArray;
import org.appwork.storage.flexijson.FlexiJSonComments;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.FlexiJSonObject;
import org.appwork.storage.flexijson.FlexiJSonValue;
import org.appwork.storage.flexijson.KeyValueElement;
import org.appwork.storage.flexijson.mapper.FlexiMapperTags;

/**
 * @author thomas
 * @date 30.04.2021
 *
 */
public class PropertyJSonPrettyStringify extends FlexiJSonStringBuilder {

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.FlexiJSonStringBuilder#initTags()
     */
    @Override
    protected void initTags(Charset charset) {
        super.initTags(charset);
        lineCommentStart = "// ".getBytes(charset);
        inlineCommentStart = "/* ".getBytes(charset);
        inlineCommendEnd = " */".getBytes(charset);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.FlexiJSonStringBuilder#toJSONString(org.appwork.storage.flexijson.FlexiJSonNode)
     */
    @Override
    public String toJSONString(FlexiJSonNode node) {
        return super.toJSONString(node);
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * org.appwork.storage.flexijson.stringify.FlexiJSonStringBuilder#appendPrimitiveValue(org.appwork.storage.flexijson.FlexiJSonValue,
     * org.appwork.utils.net.JSONBuilderOutputStream, java.util.LinkedList, java.lang.String)
     */
    @Override
    public void appendPrimitiveValue(FlexiJSonValue value, JSONBuilderOutputStream out, int layer, LinkedList<String> path) throws IOException {
        writeIfNotEOL(out);
        FlexiJSonComments b = value.getCommentsBefore();
        FlexiJSonComments a = value.getCommentsAfter();
        if (b != null) {
            b.writeToStream(this, out, 0, path);
        }
        if (out.transferedBytes() > 0) {
            out.write(newLineRN);
        }
        boolean first = true;
        for (Object o : path) {
            String n = (String) o;
            if (n.charAt(0) != '[') {
                if (!n.matches("^[a-zA-Z_$][a-zA-Z_$0-9]*$")) {
                    n = "[" + JSonStorage.serializeToJson(n) + "]";
                }
            }
            if (first) {
                first = false;
            } else {
                if (n.charAt(0) != '[') {
                    out.write(".".getBytes(charset));
                }
            }
            out.write(n.getBytes(charset));
        }
        if (path.size() == 0) {
            out.write("value".getBytes(charset));
        }
        out.write(" = ".getBytes(charset));
        super.appendPrimitiveValueWithoutComments(value, out, path, layer);
        if (a != null) {
            a.writeToStream(this, out, 0, path);
        }
    }

    @Override
    protected void appendString(JSONBuilderOutputStream out, String s) throws IOException, CharacterCodingException {
        stringToStream(out, s.replace("\r", "\\r").replace("\n", "\\n"));
    }

    @Override
    public void appendObject(FlexiJSonObject object, JSONBuilderOutputStream out, int layer, LinkedList<String> path) throws IOException {
        FlexiJSonComments b = object.getCommentsBefore();
        FlexiJSonComments a = object.getCommentsAfter();
        if (b != null) {
            b.writeToStream(this, out, layer, path);
        }
        for (KeyValueElement es : object.getElements()) {
            path.add(es.getKey());
            try {
                if (es.getCommentsBeforeKey() != null) {
                    es.getCommentsBeforeKey().writeToStream(this, out, layer + 1, path);
                }
                if (es.getKey() != null) {
                    // appendString(out, es.getKey());
                    // if (isAlignColons()) {
                    // ByteArrayOutputStream baos = new ByteArrayOutputStream();
                    // appendString(new JSONBuilderOutputStream(baos), es.getKey());
                    // spaces(out, keyLength - new String(baos.toByteArray(), charset).length());
                    // }
                    if (es.getCommentsAfterKey() != null) {
                        es.getCommentsAfterKey().writeToStream(this, out, layer + 1, path);
                    }
                    // out.write(":".getBytes(charset));
                    Set<FlexiMapperTags> tags = es.getValue().getTags();
                    if (isWriteDefaultValuesAsComment(object, es.getValue(), path) && tags != null && tags.contains(FlexiMapperTags.DEFAULT_VALUE)) {
                        String comment = toJSONString(es.getValue());
                        // new FlexiJSonComments();
                        new FlexiComment(comment, FlexiComment.Type.INLINE, FlexiMapperTags.DEFAULT_VALUE).writeToStream(this, out, layer, path);
                    } else {

                        es.getValue().writeToStream(this, out, layer + getLayerIndent(), path);

                    }
                }
            } finally {
                path.removeLast();
            }
        }
        if (object.getCommentsInside() != null) {
            for (FlexiCommentJsonNode c : object.getCommentsInside()) {
                c.writeToStream(this, out, layer + 1, path);
            }
        }
        if (a != null) {
            a.writeToStream(this, out, layer, path);
        }
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

    protected int getLayerIndent() {
        return 2;
    }

    @Override
    public void appendArray(FlexiJSonArray array, JSONBuilderOutputStream out, int layer, LinkedList<String> path) throws IOException {
        try {
            FlexiJSonComments b = array.getCommentsBefore();
            FlexiJSonComments a = array.getCommentsAfter();
            if (b != null) {
                b.writeToStream(this, out, layer, path);
            }
            // ByteArrayOutputStream baos;
            // JSONBuilderOutputStream tmp = new JSONBuilderOutputStream(baos = new ByteArrayOutputStream(60));
            // for small primitive arrays, we write in a single line. else multiline
            boolean firstElement = true;
            for (int i = 0; i < array.size(); i++) {
                FlexiJSonNode n = array.get(i);
                try {
                    path.add("[" + i + "]");
                    n.writeToStream(this, out, layer + 1, path);
                } finally {
                    path.removeLast();
                }
            }
            if (array.getCommentsInside() != null) {
                if (!firstElement) {
                    // requires a trailing coma, else it would not be a inside comment, but a "after" comment in the last element
                    // ", "
                    out.write(commaString);
                    out.write(spaceString);
                }
                array.getCommentsInside().writeToStream(this, out, layer, path);
            }
            if (a != null) {
                a.writeToStream(this, out, layer, path);
            }
        } catch (Throwable e) {
            System.out.println(1);
            throw new WTFException(e);
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.stringify.FlexiJSonStringBuilder#appendComment(org.appwork.storage.flexijson.FlexiComment,
     * org.appwork.storage.flexijson.stringify.FlexiJSonStringBuilder.JSONBuilderOutputStream, java.util.LinkedList)
     */
    @Override
    public void appendComment(FlexiComment comment, JSONBuilderOutputStream out, int layer, LinkedList<String> path) throws IOException {
        writeIfNotEOL(out);
        for (String line : comment.getText().split("[\r\n]{1,2}")) {
            if (Arrays.equals(out.getLookBackBuffer(), newLineRN) || out.transferedBytes() == 0) {
                stringToStream(out, "# " + line.trim());
            } else {
                stringToStream(out, "\r\n# " + line.trim());
            }
        }
    }

    /**
     * @param out
     * @param flexiJSonComments
     * @throws IOException
     */
    public void appendComments(FlexiJSonComments comments, JSONBuilderOutputStream out, int layer, LinkedList<String> path) throws IOException {
        for (FlexiCommentJsonNode c : comments) {
            c.writeToStream(this, out, 0, path);
        }
    }

}
