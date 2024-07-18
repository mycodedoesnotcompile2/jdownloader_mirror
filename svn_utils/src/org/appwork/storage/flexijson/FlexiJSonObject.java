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
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.SimpleTypeRef;
import org.appwork.storage.TypeRef;
import org.appwork.storage.flexijson.JSPath.MetaElement;
import org.appwork.storage.flexijson.mapper.FlexiJSonMapper;
import org.appwork.storage.flexijson.mapper.FlexiMapperException;
import org.appwork.storage.flexijson.mapper.FlexiMapperTags;
import org.appwork.storage.flexijson.stringify.FlexiJSonPrettyStringify;
import org.appwork.storage.flexijson.stringify.FlexiJSonStringBuilder;
import org.appwork.storage.flexijson.stringify.FlexiJSonStringBuilder.JSONBuilderOutputStream;
import org.appwork.storage.simplejson.JSonUtils;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.reflection.CompiledType;

/**
 * @author thomas
 *
 */
public class FlexiJSonObject implements FlexiJSonNode {
    public static final SimpleTypeRef<FlexiJSonObject> TYPE = new SimpleTypeRef<FlexiJSonObject>(FlexiJSonObject.class);
    private final LinkedList<KeyValueElement>          elements;

    public LinkedList<KeyValueElement> getElements() {
        return this.elements;
    }

    private HashSet<FlexiMapperTags> tags;

    @Override
    public void tag(final FlexiMapperTags tag) {
        if (this.tags == null) {
            this.tags = new HashSet<FlexiMapperTags>();
        }
        if (tag != null) {
            this.tags.add(tag);
        }
    }

    @Override
    public Set<FlexiMapperTags> getTags() {
        return this.tags;
    }

    private int                                                size = 0;
    private final LinkedHashMap<String, List<KeyValueElement>> keys;
    private FlexiJSonComments                     commentsBefore;
    private FlexiJSonComments                                  commentsInside;

    public FlexiJSonComments getCommentsInside() {
        return this.commentsInside;
    }

    public void setCommentsInside(final FlexiJSonComments commentsInside) {
        if (commentsInside != null) {
            commentsInside.setParent(this);
            for (final FlexiCommentJsonNode comment : commentsInside) {
                comment.setLocation(FlexiCommentJsonNode.AttachLocation.INSIDE_OBJECT);
            }
        }
        this.commentsInside = commentsInside;
    }

    @Override
    public FlexiJSonComments getCommentsBefore() {
        return this.commentsBefore;
    }

    @Override
    public void setCommentsBefore(final FlexiJSonComments commentsBefore) {
        if (commentsBefore != null) {
            commentsBefore.setParent(this);
            for (final FlexiCommentJsonNode comment : commentsBefore) {
                comment.setLocation(FlexiCommentJsonNode.AttachLocation.BEFORE_OBJECT);
            }
        }
        this.commentsBefore = commentsBefore;
    }

    @Override
    public FlexiJSonComments getCommentsAfter() {
        return this.commentsAfter;
    }

    @Override
    public void setCommentsAfter(final FlexiJSonComments commentsAfter) {
        if (commentsAfter != null) {
            commentsAfter.setParent(this);
            for (final FlexiCommentJsonNode comment : commentsAfter) {
                comment.setLocation(FlexiCommentJsonNode.AttachLocation.AFTER_OBJECT);
            }
        }
        this.commentsAfter = commentsAfter;
    }

    private FlexiJSonComments commentsAfter;

    public FlexiJSonObject() {
        super();
        this.elements = new LinkedList<KeyValueElement>();
        this.keys = new LinkedHashMap<String, List<KeyValueElement>>();
    }

    /**
     * @param mapKey
     * @return
     */
    public boolean containsKey(final String key) {
        return this.keys.containsKey(key);
    }

    /**
     * @param element
     * @return
     */
    public KeyValueElement add(final KeyValueElement element) {
        return add(element, true);
    }

    public KeyValueElement add(final KeyValueElement element, boolean removeExisting) {
        int addAt = -1;
        final String elementKey = element.getKey();
        KeyValueElement removed = null;
        if (removeExisting && containsKey(elementKey)) {
            for (final KeyValueElement e : this.elements) {
                addAt++;
                if (StringUtils.equals(e.getKey(), elementKey)) {
                    break;
                }
            }
        }
        element.getValue().setParent(this);
        // forward correct parent to comments
        // before and after key ocmments are related to the key/value element and use the value as parent
        FlexiJSonComments comment = element.getCommentsAfterKey();
        if (comment != null) {
            comment.setParent(element.getValue());
        }
        comment = element.getCommentsBeforeKey();
        if (comment != null) {
            comment.setParent(element.getValue());
        }
        if (addAt < 0) {
            this.elements.add(element);
        } else {
            removed = this.elements.set(addAt, element);
        }
        if (elementKey != null) {
            List<KeyValueElement> list = keys.get(elementKey);
            if (list == null) {
                list = new LinkedList<KeyValueElement>();
                keys.put(elementKey, list);
            }
            if (removed != null && list.remove(removed)) {
                size--;
            }
            list.add(element);
            size++;
        }
        return removed;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.FlexiJSonNode#setParent(org.appwork.storage.flexijson.FlexiJSonNode)
     */
    private FlexiJSonNode parent;

    @Override
    public void setParent(final FlexiJSonNode parent) {
        this.parent = parent;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.FlexiJSonNode#getParent()
     */
    @Override
    public FlexiJSonNode getParent() {
        return this.parent;
    }

    /**
     * returns the actual size of the object - does NOT include empty entries or comments
     *
     * @return
     */
    public int size() {
        return size;
    }

    protected String     close                    = "\r\n}";
    protected String     keyValueDeliminator      = " : ";
    protected String     empty                    = "{}";
    protected String     open                     = "{\r\n";
    protected String     fieldDeliminator         = ",\r\n";
    /**
     *
     */
    public static String PRETTY_PRINT_LAYER_INSET = " ";

    public void addCommentsInside(final FlexiJSonComments comments) {
        if (comments == null) {
            return;
        }
        comments.setParent(this);
        for (final FlexiCommentJsonNode comment : comments) {
            comment.setLocation(FlexiCommentJsonNode.AttachLocation.INSIDE_OBJECT);
        }
        if (this.commentsInside == null) {
            this.commentsInside = comments;
        } else {
            this.commentsInside.addAll(comments);
        }
    }

    @Override
    public String toString() {
        String pretty = new FlexiJSonPrettyStringify().toJSONString(this);
        try {
            return FlexiUtils.fromFlexiNode(this).toPathString(false) + ": " + (pretty.contains("\r") ? "\r\n" : "") + pretty;
        } catch (InvalidPathException e) {
            return "ERROR:" + e.getMessage() + ": " + (pretty.contains("\r") ? "\r\n" : "") + pretty;
        }
    }

    protected String toStringKey(final String key) {
        return "\"" + JSonUtils.escape(key) + "\"";
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.FlexiJSonNode#addCommentsBefore(org.appwork.storage.flexijson.FlexiJSonComments)
     */
    @Override
    public void addCommentsBefore(final FlexiJSonComments comments) {
        if (comments == null) {
            return;
        }
        comments.setParent(this);
        for (final FlexiCommentJsonNode comment : comments) {
            comment.setLocation(FlexiCommentJsonNode.AttachLocation.BEFORE_OBJECT);
        }
        if (this.commentsBefore == null) {
            this.commentsBefore = comments;
        } else {
            this.commentsBefore.addAll(comments);
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.FlexiJSonNode#addCommentsAfter(org.appwork.storage.flexijson.FlexiJSonComments)
     */
    @Override
    public void addCommentsAfter(final FlexiJSonComments comments) {
        if (comments == null) {
            return;
        }
        comments.setParent(this);
        for (final FlexiCommentJsonNode comment : comments) {
            comment.setLocation(FlexiCommentJsonNode.AttachLocation.AFTER_OBJECT);
        }
        if (this.commentsAfter == null) {
            this.commentsAfter = comments;
        } else {
            this.commentsAfter.addAll(comments);
        }
    }

    /**
     *
     */
    public KeyValueElement last() {
        if (this.elements.size() == 0) {
            return null;
        } else {
            return this.elements.get(this.elements.size() - 1);
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.FlexiJSonNode#writeToStream(org.appwork.storage.flexijson.FlexiJSonStringBuilder,
     * org.appwork.utils.net.JSONBuilderOutputStream, int)
     */
    @Override
    public void writeToStream(final FlexiJSonStringBuilder stringifier, final JSONBuilderOutputStream out, final int layer, final LinkedList<String> path) throws IOException {
        stringifier.appendObject(this, out, layer, path);
    }

    /**
     * @return
     */
    public boolean hasCommentsInside() {
        return this.getCommentsInside() != null && this.getCommentsInside().size() > 0;
    }

    @Override
    public boolean hasComments() {
        if (this.getCommentsInside() != null && this.getCommentsInside().size() > 0) {
            return true;
        } else if (this.getCommentsAfter() != null && this.getCommentsAfter().size() > 0) {
            return true;
        } else if (this.getCommentsBefore() != null && this.getCommentsBefore().size() > 0) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * @param string
     * @return
     */
    public KeyValueElement getElement(final String key) {
        for (final KeyValueElement e : this.elements) {
            if (StringUtils.equals(e.getKey(), key)) {
                return e;
            }
        }
        return null;
    }

    /**
     * resolves a path like a.b.c[3].h
     *
     * @param s
     * @return
     * @throws InvalidPathException
     */
    public FlexiJSonNode resolvePath(final String path) throws InvalidPathException {
        return this.resolvePath(JSPath.fromPathString(path));
    }

    /**
     * @param splitPath
     * @return
     */
    @Override
    public FlexiJSonNode resolvePath(final JSPath path) {
        final FlexiJSonNode obj = this;
        return resolvePath(path, obj);
    }

    public static FlexiJSonNode resolvePath(final JSPath path, FlexiJSonNode obj) {
        int depths = 0;
        KeyValueElement el = null;
        for (Object key : path.getElements()) {
            if (obj == null) {
                return null;
            }
            if ("".equals(key)) {
                depths++;
                continue;
            }
            if (key instanceof MetaElement) {
                String commentTag = ((MetaElement) key).getString().substring(1);
                if (commentTag.equals(FlexiCommentJsonNode.AttachLocation.BEFORE_KEY.prefix)) {
                    obj = el.getCommentsBeforeKey();
                    depths++;
                } else if (commentTag.equals(FlexiCommentJsonNode.AttachLocation.AFTER_KEY.prefix)) {
                    obj = el.getCommentsAfterKey();
                    depths++;
                } else if (commentTag.equals(FlexiCommentJsonNode.AttachLocation.BEFORE_VALUE.prefix)) {
                    obj = obj.getCommentsBefore();
                    depths++;
                } else if (commentTag.equals(FlexiCommentJsonNode.AttachLocation.AFTER_VALUE.prefix)) {
                    obj = obj.getCommentsAfter();
                    depths++;
                } else if (commentTag.equals(FlexiCommentJsonNode.AttachLocation.INSIDE_OBJECT.prefix)) {
                    if (obj instanceof FlexiJSonArray) {
                        obj = ((FlexiJSonArray) obj).getCommentsInside();
                    } else if (obj instanceof FlexiJSonObject) {
                        obj = ((FlexiJSonObject) obj).getCommentsInside();
                    } else {
                        throw new WTFException("Not supported");
                    }
                    depths++;
                } else {
                    throw new WTFException("Not supported");
                }
            } else if (obj instanceof FlexiJSonComments) {
                obj = ((FlexiJSonComments) obj).get(JSPath.toArrayIndex(key));
                depths++;
            } else if (obj instanceof FlexiJSonObject) {
                el = ((FlexiJSonObject) obj).getElement(String.valueOf(key));
                if (el == null) {
                    return null;
                }
                obj = el.getValue();
                depths++;
            } else if (obj instanceof FlexiJSonArray) {
                obj = ((FlexiJSonArray) obj).get(JSPath.toArrayIndex(key));
                depths++;
            } else {
                if (depths == path.size() - 1) {
                    // hä?
                    DebugMode.debugger();
                    return null;
                } else {
                    return null;
                }
            }
        }
        return obj;
    }

    /**
     * @param indexOf
     * @param indexOf2
     * @return
     */
    public static int minIndex(final int... options) {
        int ret = Integer.MAX_VALUE;
        for (final int i : options) {
            if (i >= 0) {
                ret = Math.min(ret, i);
            }
        }
        return ret == Integer.MAX_VALUE ? -1 : ret;
    }

    /**
     * @param string
     */
    public KeyValueElement remove(final String key) {
        if (!containsKey(key)) {
            return null;
        }
        final Iterator<KeyValueElement> it = this.elements.iterator();
        while (it.hasNext()) {
            final KeyValueElement next = it.next();
            if (StringUtils.equals(next.getKey(), key)) {
                it.remove();
                if (key != null) {
                    final List<KeyValueElement> entries = keys.get(key);
                    if (entries != null) {
                        if (entries.remove(next)) {
                            size--;
                        }
                        if (entries.size() == 0) {
                            keys.remove(key);
                        }
                    }
                }
                return next;
            }
        }
        return null;
    }

    @Override
    public int hashCode() {
        return FlexiJSonObject.class.hashCode();
    }

    @Override
    public boolean equals(final Object obj) {
        if (obj == this) {
            return true;
        } else if (obj == null || !(obj instanceof FlexiJSonObject)) {
            return false;
        }
        final FlexiJSonObject other = (FlexiJSonObject) obj;
        if (other.size() != this.size()) {
            return false;
        } else if (!other.keys.keySet().equals(this.keys.keySet())) {
            return false;
        } else {
            final FlexiJSonStringBuilder stringify = new FlexiJSonStringBuilder();
            return stringify.toJSONString(other).equals(stringify.toJSONString(this));
        }
    }

    /**
     * @param string
     * @param flexiJSonValue
     * @throws InvalidPathException
     */
    public void replace(final String path, final FlexiJSonNode newValue) throws InvalidPathException {
        final JSPath split = JSPath.fromPathString(path);
        final FlexiJSonNode old = this.resolvePath(split);
        final FlexiJSonNode oldParent = old.getParent();
        newValue.setParent(oldParent);
        final Object last = split.getLast();
        if (oldParent instanceof FlexiJSonObject && last instanceof String) {
            ((FlexiJSonObject) oldParent).getElement((String) last).setValue(newValue);
        } else if (oldParent instanceof FlexiJSonArray && last instanceof Number) {
            ((FlexiJSonArray) oldParent).set(((Number) last).intValue(), newValue);
        } else {
            throw new WTFException("Unknown container type");
        }
    }

    @Override
    public boolean remove(final FlexiJSonNode node) {
        org.appwork.storage.flexijson.FlexiJSonValue.removeCommentsFromNode(node, this);
        if (node instanceof FlexiJSonComments) {
            if (node == this.getCommentsInside()) {
                this.setCommentsInside(null);
                return true;
            }
        }
        for (final Iterator<KeyValueElement> it = this.elements.iterator(); it.hasNext();) {
            final KeyValueElement next = it.next();
            if (next.getValue() == node) {
                it.remove();
                final String key = next.getKey();
                if (key != null) {
                    final List<KeyValueElement> entries = keys.get(key);
                    if (entries != null) {
                        if (entries.remove(next)) {
                            size--;
                        }
                        if (entries.size() == 0) {
                            keys.remove(key);
                        }
                    }
                }
                return true;
            }
        }
        return false;
    }

    /**
     * @param node
     * @return
     */
    public KeyValueElement getElementByNode(final FlexiJSonNode node) {
        for (final KeyValueElement e : this.elements) {
            if (e.getValue() == node) {
                return e;
            }
        }
        return null;
    }

    /**
     * @param string
     * @param currentTimeMillis
     * @throws FlexiMapperException
     */
    public void put(final String key, final Object obj) throws FlexiMapperException {
        if (obj instanceof FlexiJSonNode) {
            this.add(new KeyValueElement(this, key, (FlexiJSonNode) obj));
        } else {
            this.add(new KeyValueElement(this, key, new FlexiJSonMapper().objectToJsonNode(obj)));
        }
    }

    /**
     * @param key
     * @return
     */
    public FlexiJSonNode getNode(final String key) {
        final KeyValueElement el = this.getElement(key);
        if (el != null) {
            return el.getValue();
        }
        return null;
    }

    /**
     * @return
     */
    public Set<String> getKeys() {
        final LinkedHashSet<String> ret = new LinkedHashSet<String>(keys.keySet());
        return ret;
    }

    /**
     * @param comparator
     */
    public void sort(final Comparator<KeyValueElement> comparator) {
        Collections.sort(this.elements, comparator);
    }

    /**
     * @param <T>
     * @param string
     * @param class1
     * @return
     * @throws FlexiMapperException
     * @throws InvalidPathException
     */
    public <T> T get(final String path, final Class<T> type) throws FlexiMapperException, InvalidPathException {
        return this.get(JSPath.fromPathString(path), type);
    }

    public <T> T get(final JSPath path, final Class<T> type) throws FlexiMapperException {
        return this.get(path, new SimpleTypeRef<T>(type));
    }

    public <T> T get(final JSPath path, final TypeRef<T> type) throws FlexiMapperException {
        final FlexiJSonNode value = this.resolvePath(path);
        final CompiledType ct = CompiledType.create(type);
        if (value == null || ct.isInstanceOf(value.getClass())) {
            return (T) value;
        } else {
            return (T) new FlexiJSonMapper().jsonToObject(value, ct);
        }
    }

    /**
     * @param county
     * @param class1
     * @return
     */
    public <T> T getNE(final String path, final Class<T> type) {
        try {
            return this.get(path, type);
        } catch (final FlexiMapperException e) {
            throw new WTFException(e);
        } catch (final InvalidPathException e) {
            throw new WTFException(e);
        }
    }

    /**
     * @param string
     * @param flexiJSonValue
     */
    public void replaceNE(final String path, final FlexiJSonValue newNode) {
        try {
            this.replace(path, newNode);
        } catch (final InvalidPathException e) {
            throw new WTFException(e);
        }
    }
}
