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
import java.util.LinkedList;

import org.appwork.storage.flexijson.FlexiComment;
import org.appwork.storage.flexijson.FlexiJSonArray;
import org.appwork.storage.flexijson.FlexiJSonComments;
import org.appwork.storage.flexijson.FlexiJSonObject;
import org.appwork.storage.flexijson.FlexiJSonValue;
import org.appwork.storage.flexijson.KeyValueElement;
import org.appwork.storage.flexijson.NodeFilter;

/**
 * @author thomas
 * @date 16.12.2022
 *
 */
public class FlexiJSonPrettyPrinterForConfig extends FlexiJSonPrettyStringify {
    /**
     *
     */
    private final NodeFilter filter;

    /**
     * @param filter
     */
    public FlexiJSonPrettyPrinterForConfig(NodeFilter filter) {
        this.filter = filter;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.stringify.FlexiJSonPrettyStringify#appendArray(org.appwork.storage.flexijson.FlexiJSonArray,
     * org.appwork.storage.flexijson.stringify.FlexiJSonStringBuilder.JSONBuilderOutputStream, int, java.util.LinkedList)
     */
    @Override
    public void appendArray(FlexiJSonArray array, JSONBuilderOutputStream out, int layer, LinkedList<String> path) throws IOException {
        super.appendArray(array, out, layer, path);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.stringify.FlexiJSonPrettyStringify#appendPrimitiveValue(org.appwork.storage.flexijson.
     * FlexiJSonValue, org.appwork.storage.flexijson.stringify.FlexiJSonStringBuilder.JSONBuilderOutputStream, int, java.util.LinkedList)
     */
    @Override
    public void appendPrimitiveValue(FlexiJSonValue value, JSONBuilderOutputStream out, int layer, LinkedList<String> path) throws IOException {
        super.appendPrimitiveValue(value, out, layer, path);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.stringify.FlexiJSonPrettyStringify#skipElement(org.appwork.storage.flexijson.FlexiJSonArray, int)
     */
    @Override
    protected boolean skipElement(FlexiJSonArray array, int i) {
        if (filter != null) {
            if (filter.skipNode(array, i)) {
                return true;
            }
        }
        return super.skipElement(array, i);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.stringify.FlexiJSonPrettyStringify#skipElement(org.appwork.storage.flexijson.FlexiJSonObject,
     * org.appwork.storage.flexijson.KeyValueElement)
     */
    @Override
    protected boolean skipElement(FlexiJSonObject object, KeyValueElement es) {
        if (filter != null) {
            if (filter.skipNode(object, es)) {
                return true;
            }
        }
        return super.skipElement(object, es);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.stringify.FlexiJSonPrettyStringify#appendComments(org.appwork.storage.flexijson.
     * FlexiJSonComments, org.appwork.storage.flexijson.stringify.FlexiJSonStringBuilder.JSONBuilderOutputStream, int, java.util.LinkedList)
     */
    @Override
    public void appendComments(FlexiJSonComments comments, JSONBuilderOutputStream out, int layer, LinkedList<String> path) throws IOException {
        super.appendComments(comments, out, layer, path);
    }

    @Override
    public void appendComment(final FlexiComment comment, final JSONBuilderOutputStream out, final int layer, final LinkedList<String> path) throws IOException {
        if (filter != null) {
            if (filter.skipCommentNode(comment)) {
                return;
            }
        }
        super.appendComment(comment, out, layer, path);
    }
}