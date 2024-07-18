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
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.storage.flexijson;

import java.io.IOException;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;

import org.appwork.storage.flexijson.mapper.FlexiMapperTags;
import org.appwork.storage.flexijson.stringify.FlexiJSonStringBuilder;
import org.appwork.storage.flexijson.stringify.FlexiJSonStringBuilder.JSONBuilderOutputStream;
import org.appwork.storage.simplejson.ValueType;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.StringUtils;
import org.appwork.utils.reflection.Clazz;

/**
 * @author thomas
 *
 */
public class FlexiJSonValue implements FlexiJSonNode {
    protected Object value;

    public void setValue(final Object value) {
        this.value = value;
    }

    public String getStringValue() {
        return StringUtils.valueOfOrNull(value);
    }

    public void setType(final ValueType type) {
        this.type = type;
    }

    protected ValueType type;

    public ValueType getType() {
        return type;
    }

    public Object getValue() {
        return value;
    }

    private FlexiJSonComments commentsBefore;

    @Override
    public FlexiJSonComments getCommentsBefore() {
        return commentsBefore;
    }

    private HashSet<FlexiMapperTags> tags;

    @Override
    public void tag(FlexiMapperTags tag) {
        if (tags == null) {
            tags = new HashSet<FlexiMapperTags>();
        }
        if (tag != null) {
            tags.add(tag);
        }
    }

    @Override
    public int hashCode() {
        return FlexiJSonValue.class.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        if (obj == null || !(obj instanceof FlexiJSonValue)) {
            return false;
        }
        FlexiJSonStringBuilder stringify = new FlexiJSonStringBuilder();
        return stringify.toJSONString((FlexiJSonValue) obj).equals(stringify.toJSONString(this));
    }

    @Override
    public Set<FlexiMapperTags> getTags() {
        return tags;
    }

    @Override
    public void setCommentsBefore(FlexiJSonComments commentsBefore) {
        if (commentsBefore != null) {
            commentsBefore.setParent(this);
            for (FlexiCommentJsonNode comment : commentsBefore) {
                comment.setLocation(FlexiCommentJsonNode.AttachLocation.BEFORE_VALUE);
            }
        }
        this.commentsBefore = commentsBefore;
    }

    @Override
    public FlexiJSonComments getCommentsAfter() {
        return commentsAfter;
    }

    public boolean hasComments() {
        if (getCommentsAfter() != null && getCommentsAfter().size() > 0) {
            return true;
        } else if (getCommentsBefore() != null && getCommentsBefore().size() > 0) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public void setCommentsAfter(FlexiJSonComments commentsAfter) {
        this.commentsAfter = commentsAfter;
        if (commentsAfter != null) {
            commentsAfter.setParent(this);
            for (FlexiCommentJsonNode comment : commentsAfter) {
                comment.setLocation(FlexiCommentJsonNode.AttachLocation.AFTER_VALUE);
            }
        }
    }

    @Override
    public void addCommentsBefore(FlexiJSonComments comments) {
        if (comments == null) {
            return;
        }
        comments.setParent(this);
        for (FlexiCommentJsonNode comment : comments) {
            comment.setLocation(FlexiCommentJsonNode.AttachLocation.BEFORE_VALUE);
        }
        if (commentsBefore == null) {
            commentsBefore = comments;
        } else {
            commentsBefore.addAll(comments);
        }
    }

    @Override
    public void addCommentsAfter(FlexiJSonComments comments) {
        if (comments == null) {
            return;
        }
        comments.setParent(this);
        for (FlexiCommentJsonNode comment : comments) {
            comment.setLocation(FlexiCommentJsonNode.AttachLocation.AFTER_VALUE);
        }
        if (commentsAfter == null) {
            commentsAfter = comments;
        } else {
            commentsAfter.addAll(comments);
        }
    }

    private FlexiJSonComments commentsAfter;

    public FlexiJSonValue(/* Storable */) {
        value = null;
        setType(org.appwork.storage.simplejson.ValueType.UNDEFINED);
    }

    public FlexiJSonValue(final boolean value) {
        this.value = value;
        type = ValueType.BOOLEAN;
    }

    @Override
    public String toString() {
        try {
            return FlexiUtils.fromFlexiNode(this).toPathString(false) + ": " + new FlexiJSonStringBuilder().toJSONString(this);
        } catch (InvalidPathException e) {
            return "ERROR:" + e.getMessage() + ": " + new FlexiJSonStringBuilder().toJSONString(this);
        }
    }

    public FlexiJSonValue(final Number value) {
        this.value = value;
        if (Clazz.isFloatingPointNumber(value.getClass())) {
            type = ValueType.DOUBLE;
        } else {
            type = ValueType.LONG;
        }
    }

    public FlexiJSonValue(final String value) {
        this(value, false);
    }

    /**
     * @param value2
     * @param b
     */
    public FlexiJSonValue(final String value, final boolean reference) {
        this.value = value;
        if (value == null) {
            type = ValueType.NULL;
        } else if (reference) {
            type = ValueType.REFERENCE;
        } else {
            type = ValueType.STRING;
        }
    }

    @Override
    public void writeToStream(FlexiJSonStringBuilder stringifier, JSONBuilderOutputStream out, int layer, LinkedList<String> path) throws IOException {
        stringifier.appendPrimitiveValue(this, out, layer, path);
    }

    private FlexiJSonNode parent;

    @Override
    public void setParent(FlexiJSonNode parent) {
        this.parent = parent;
    }

    @Override
    public FlexiJSonNode getParent() {
        return parent;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.FlexiJSonNode#remove(org.appwork.storage.flexijson.FlexiJSonNode)
     */
    @Override
    public boolean remove(FlexiJSonNode node) {
        if (removeCommentsFromNode(node, this)) {
            return true;
        }
        return false;
    }

    protected static boolean removeCommentsFromNode(FlexiJSonNode toRemove, FlexiJSonNode removeFrom) {
        if (toRemove instanceof FlexiJSonComments) {
            if (toRemove == removeFrom.getCommentsAfter()) {
                removeFrom.setCommentsAfter(null);
                return true;
            }
            if (toRemove == removeFrom.getCommentsBefore()) {
                removeFrom.setCommentsBefore(null);
                return true;
            }
            FlexiJSonNode par = removeFrom.getParent();
            if (par instanceof FlexiJSonObject) {
                KeyValueElement element = ((FlexiJSonObject) par).getElementByNode(removeFrom);
                if (toRemove == element.getCommentsAfterKey()) {
                    element.setCommentsAfterKey(null);
                    return true;
                }
                if (toRemove == element.getCommentsBeforeKey()) {
                    element.setCommentsBeforeKey(null);
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * @param <T>
     * @param class1
     * @return
     */
    public <T> T getValue(Class<T> type) {
        Object v = getValue();
        if (Clazz.objectIsTypeOf(v, type)) {
            return (T) v;
        } else {
            return (T) ReflectionUtils.cast(v, type);
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.FlexiJSonNode#resolvePath(org.appwork.storage.flexijson.JSPath)
     */
    @Override
    public FlexiJSonNode resolvePath(JSPath parent) {
        return null;
    }
}
