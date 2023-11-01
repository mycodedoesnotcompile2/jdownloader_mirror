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
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;
import java.util.function.UnaryOperator;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.flexijson.mapper.FlexiMapperTags;
import org.appwork.storage.flexijson.stringify.FlexiJSonPrettyStringify;
import org.appwork.storage.flexijson.stringify.FlexiJSonStringBuilder;
import org.appwork.storage.flexijson.stringify.FlexiJSonStringBuilder.JSONBuilderOutputStream;

/**
 * @author thomas
 *
 */
public class FlexiJSonArray extends ArrayList<FlexiJSonNode> implements FlexiJSonNode {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    public FlexiJSonArray(int size) {
        super(size);
    }

    public FlexiJSonArray() {
        super();
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
    public Set<FlexiMapperTags> getTags() {
        return tags;
    }

    @Override
    public int hashCode() {
        return FlexiJSonArray.class.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        if (obj == null || !(obj instanceof FlexiJSonArray)) {
            return false;
        }
        FlexiJSonArray other = (FlexiJSonArray) obj;
        if (other.size() != this.size()) {
            return false;
        }
        FlexiJSonStringBuilder stringify = new FlexiJSonStringBuilder();
        for (int i = 0; i < size(); i++) {
            if (!stringify.toJSONString(other.get(i)).equals(stringify.toJSONString(this.get(i)))) {
                return false;
            }
        }
        return stringify.toJSONString(other).equals(stringify.toJSONString(this));
    }

    private FlexiJSonComments commentsBefore;
    private FlexiJSonComments commentsInside;

    @Override
    public String toString() {
        return getClass().getSimpleName() + " - " + new FlexiJSonPrettyStringify().toJSONString(this);
    }

    public String toPrettyString() {
        return new FlexiJSonPrettyStringify().toJSONString(this);
    }

    @Override
    public boolean add(FlexiJSonNode e) {
        e.setParent(this);
        return super.add(e);
    }

    @Override
    public void add(int index, FlexiJSonNode e) {
        e.setParent(this);
        super.add(index, e);
    }

    /*
     * (non-Javadoc)
     *
     * @see java.util.ArrayList#set(int, java.lang.Object)
     */
    @Override
    public FlexiJSonNode set(int index, FlexiJSonNode e) {
        e.setParent(this);
        return super.set(index, e);
    }

    /*
     * (non-Javadoc)
     *
     * @see java.util.ArrayList#addAll(java.util.Collection)
     */
    @Override
    public boolean addAll(Collection<? extends FlexiJSonNode> c) {
        throw new WTFException("Not supported");
    }

    /*
     * (non-Javadoc)
     *
     * @see java.util.ArrayList#addAll(int, java.util.Collection)
     */
    @Override
    public boolean addAll(int index, Collection<? extends FlexiJSonNode> c) {
        throw new WTFException("Not supported");
    }

    /*
     * (non-Javadoc)
     *
     * @see java.util.ArrayList#replaceAll(java.util.function.UnaryOperator)
     */
    @Override
    public void replaceAll(UnaryOperator<FlexiJSonNode> operator) {
        throw new WTFException("Not supported");
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.FlexiJSonNode#addCommentsBefore(org.appwork.storage.flexijson.FlexiJSonComments)
     */
    @Override
    public void addCommentsBefore(FlexiJSonComments comments) {
        if (comments == null) {
            return;
        }
        comments.setParent(this);
        for (FlexiCommentJsonNode comment : comments) {
            comment.setLocation(FlexiCommentJsonNode.AttachLocation.BEFORE_ARRAY);
        }
        if (commentsBefore == null) {
            commentsBefore = comments;
        } else {
            commentsBefore.addAll(comments);
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.FlexiJSonNode#addCommentsAfter(org.appwork.storage.flexijson.FlexiJSonComments)
     */
    @Override
    public void addCommentsAfter(FlexiJSonComments comments) {
        if (comments == null) {
            return;
        }
        comments.setParent(this);
        for (FlexiCommentJsonNode comment : comments) {
            comment.setLocation(FlexiCommentJsonNode.AttachLocation.AFTER_ARRAY);
        }
        if (commentsAfter == null) {
            commentsAfter = comments;
        } else {
            commentsAfter.addAll(comments);
        }
    }

    public void addCommentsInside(FlexiJSonComments comments) {
        if (comments == null) {
            return;
        }
        comments.setParent(this);
        for (FlexiCommentJsonNode comment : comments) {
            comment.setLocation(FlexiCommentJsonNode.AttachLocation.INSIDE_ARRAY);
        }
        if (commentsInside == null) {
            commentsInside = comments;
        } else {
            commentsInside.addAll(comments);
        }
    }

    public FlexiJSonComments getCommentsInside() {
        return commentsInside;
    }

    public void setCommentsInside(FlexiJSonComments commentsInside) {
        if (commentsInside != null) {
            commentsInside.setParent(this);
            for (FlexiCommentJsonNode comment : commentsInside) {
                comment.setLocation(FlexiCommentJsonNode.AttachLocation.INSIDE_ARRAY);
            }
        }
        this.commentsInside = commentsInside;
    }

    public boolean hasComments() {
        if (getCommentsInside() != null && getCommentsInside().size() > 0) {
            return true;
        } else if (getCommentsAfter() != null && getCommentsAfter().size() > 0) {
            return true;
        } else if (getCommentsBefore() != null && getCommentsBefore().size() > 0) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public FlexiJSonComments getCommentsBefore() {
        return commentsBefore;
    }

    @Override
    public void setCommentsBefore(FlexiJSonComments commentsBefore) {
        if (commentsBefore != null) {
            commentsBefore.setParent(this);
            for (FlexiCommentJsonNode comment : commentsBefore) {
                comment.setLocation(FlexiCommentJsonNode.AttachLocation.BEFORE_ARRAY);
            }
        }
        this.commentsBefore = commentsBefore;
    }

    @Override
    public FlexiJSonComments getCommentsAfter() {
        return commentsAfter;
    }

    @Override
    public void setCommentsAfter(FlexiJSonComments commentsAfter) {
        if (commentsAfter != null) {
            commentsAfter.setParent(this);
            for (FlexiCommentJsonNode comment : commentsAfter) {
                comment.setLocation(FlexiCommentJsonNode.AttachLocation.AFTER_ARRAY);
            }
        }
        this.commentsAfter = commentsAfter;
    }

    private FlexiJSonComments commentsAfter;

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.FlexiJSonNode#toJSONString(org.appwork.storage.flexijson.FlexiJSonStringBuilder,
     * java.io.OutputStream)
     */
    @Override
    public void writeToStream(FlexiJSonStringBuilder stringifier, JSONBuilderOutputStream out, int layer, LinkedList<String> path) throws IOException {
        stringifier.appendArray(this, out, layer, path);
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

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.FlexiJSonNode#getParent()
     */
    @Override
    public FlexiJSonNode getParent() {
        return parent;
    }

    /**
     * resolves a path like [3]
     *
     * @param s
     * @return
     * @throws InvalidPathException
     */
    public FlexiJSonNode resolvePath(String path) throws InvalidPathException {
        return resolvePath(JSPath.fromPathString(path));
    }

    /**
     * @param splitPath
     * @return
     */
    public FlexiJSonNode resolvePath(JSPath splitPath) {
        return FlexiJSonObject.resolvePath(splitPath, this);
    }

    @Override
    public boolean remove(FlexiJSonNode node) {
        org.appwork.storage.flexijson.FlexiJSonValue.removeCommentsFromNode(node, this);
        if (node instanceof FlexiJSonComments) {
            if (node == getCommentsInside()) {
                setCommentsInside(null);
                return true;
            }
        }
        return remove((Object) node);
    }
}
