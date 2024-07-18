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
package org.appwork.storage.flexijson;

import java.io.IOException;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;

import org.appwork.storage.flexijson.mapper.FlexiMapperTags;
import org.appwork.storage.flexijson.stringify.FlexiJSonStringBuilder;
import org.appwork.storage.flexijson.stringify.FlexiJSonStringBuilder.JSONBuilderOutputStream;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.StringUtils;

/**
 * @author thomas
 * @date 23.04.2021
 *
 */
public class FlexiComment implements FlexiCommentJsonNode {
    public static enum Type {
        LINE,
        INLINE
    }

    @Override
    public FlexiJSonNode resolvePath(JSPath parent) {
        return null;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.FlexiJSonNode#setParent(org.appwork.storage.flexijson.FlexiJSonNode)
     */
    private FlexiJSonNode parent;

    @Override
    public void setParent(FlexiJSonNode parent) {
        this.parent = parent;
    }

    public boolean hasComments() {
        return false;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.FlexiJSonNode#getParent()
     */
    @Override
    public FlexiJSonNode getParent() {
        return parent;
    }

    private String         text;
    private Type           type;
    private AttachLocation location;

    public Type getType() {
        return type;
    }

    public void setType(Type type) {
        this.type = type;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    /**
     * @param string
     * @param inline
     * @param tag
     */
    public FlexiComment(String string, Type inline, FlexiMapperTags tag) {
        this(string, inline);
        if (tag != null) {
            tag(tag);
        }
    }

    /**
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        if (!(obj instanceof FlexiComment)) {
            return false;
        }
        FlexiComment other = (FlexiComment) obj;
        if (other.getType() != getType()) {
            return false;
        }
        if (other.getLocation() != getLocation()) {
            return false;
        }
        if (!CompareUtils.equals(other.getTags(), getTags())) {
            return false;
        }
        if (!StringUtils.equals(getText(), other.getText())) {
            return false;
        }
        return true;
    }

    /**
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        return (getText() + getType() + getLocation() + getTags()).hashCode();
    }

    private FlexiComment(String string, Type inline) {
        this.text = string;
        this.type = inline;
        if (text.contains("*/")) {
            // */ is not allowed in text. we would have to escape this
            type = Type.LINE;
        }
    }

    /**
     * @param string
     * @param type2
     * @param tags2
     */
    public FlexiComment(String string, Type type, Set<FlexiMapperTags> tags2) {
        this(string, type);
        this.tags = tags2;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.FlexiJSonNode#toJSONString(org.appwork.storage.flexijson.FlexiJSonStringBuilder,
     * java.io.OutputStream)
     */
    @Override
    public void writeToStream(FlexiJSonStringBuilder stringifier, JSONBuilderOutputStream out, int layer, LinkedList<String> path) throws IOException {
        stringifier.appendComment(this, out, layer, path);
    }

    @Override
    public String toString() {
        return new FlexiJSonStringBuilder().toJSONString(this);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.FlexiJSonNode#setCommentsAfter(org.appwork.storage.flexijson.FlexiJSonComments)
     */
    @Override
    public void setCommentsAfter(FlexiJSonComments commentsAfter) {
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.FlexiJSonNode#getCommentsAfter()
     */
    @Override
    public FlexiJSonComments getCommentsAfter() {
        return null;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.FlexiJSonNode#setCommentsBefore(org.appwork.storage.flexijson.FlexiJSonComments)
     */
    @Override
    public void setCommentsBefore(FlexiJSonComments commentsBefore) {
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.FlexiJSonNode#getCommentsBefore()
     */
    @Override
    public FlexiJSonComments getCommentsBefore() {
        return null;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.FlexiJSonNode#addCommentsBefore(org.appwork.storage.flexijson.FlexiJSonComments)
     */
    @Override
    public void addCommentsBefore(FlexiJSonComments comments) {
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.FlexiJSonNode#addCommentsAfter(org.appwork.storage.flexijson.FlexiJSonComments)
     */
    @Override
    public void addCommentsAfter(FlexiJSonComments comments) {
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * org.appwork.storage.flexijson.FlexiCommentJsonNode#setLocation(org.appwork.storage.flexijson.FlexiCommentJsonNode.AttachLocation)
     */
    @Override
    public void setLocation(AttachLocation afterKey) {
        this.location = afterKey;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.FlexiCommentJsonNode#getLocation()
     */
    @Override
    public AttachLocation getLocation() {
        return location;
    }

    private Set<FlexiMapperTags> tags;

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
    public Set<FlexiMapperTags> getTags() {
        return tags;
    }

    /**
     * @param tag
     * @return
     */
    public boolean hasTag(FlexiMapperTags tag) {
        if (tags == null) {
            return false;
        }
        return tags.contains(tag);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.FlexiJSonNode#remove(org.appwork.storage.flexijson.FlexiJSonNode)
     */
    @Override
    public boolean remove(FlexiJSonNode value) {
        throw new IllegalStateException("Not Supported. This Type may not have any children");
    }
}
