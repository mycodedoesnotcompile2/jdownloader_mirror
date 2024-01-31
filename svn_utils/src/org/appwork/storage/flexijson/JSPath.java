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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.appwork.exceptions.WTFException;
import org.appwork.moncompare.Condition;
import org.appwork.moncompare.ConditionException;
import org.appwork.moncompare.Scope;
import org.appwork.moncompare.fromjson.FlexiConditionMapper;
import org.appwork.storage.flexijson.mapper.FlexiMapperException;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.ConcatIterator;
import org.appwork.utils.StringUtils;
import org.appwork.utils.reflection.Clazz;

/**
 * @author thomas
 * @date 27.06.2022
 *
 */
public class JSPath implements Iterable<Object> {
    private LinkedList<Object> elements;

    /**
     * @param subList
     */
    public JSPath(List<? extends Object> subList) {
        this();
        elements.addAll(subList);
    }

    public JSPath(Object... path) {
        this();
        elements.addAll(Arrays.asList(path));
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        return elements.hashCode();
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof JSPath) {
            JSPath other = (JSPath) obj;
            if (elements.equals(other.elements)) {
                return true;
            }
            if (elements.size() != other.elements.size()) {
                return false;
            }
            for (int i = 0; i < elements.size(); i++) {
                if (!StringUtils.equals(StringUtils.valueOfOrNull(elements.get(i)), StringUtils.valueOfOrNull(other.elements.get(i)))) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    /**
     *
     */
    public JSPath() {
        elements = new LinkedList<Object>();
    }

    public Iterator<Object> iterator() {
        return elements.iterator();
    }

    /**
     * @param path
     * @return
     * @throws InvalidPathException
     */
    public static JSPath fromPathString(String path) throws InvalidPathException {
        JSPath ret = new JSPath();
        if (path.startsWith(".")) {
            path = path.substring(1);
        }
        main: while (true) {
            String key = null;
            if ("".equals(path)) {
                return ret;
            }
            if (path.startsWith("[")) {
                try {
                    FlexiJSONParser parser = new FlexiJSONParser(path) {
                    }.setDebug(new StringBuilder());
                    parser.setBreakAtEndOfObject(true);
                    parser.setIgnoreIssues(FlexiJSONParser.IGNORE_LIST_JS);
                    FlexiJSonNode parsed = ((FlexiJSonArray) parser.parse()).get(0);
                    if (parsed instanceof FlexiJSonValue) {
                        FlexiJSonValue token = (FlexiJSonValue) parsed;
                        ret.add(token.getValue());
                        if (parser.isEndOfStreamReached()) {
                            path = path.substring(parser.index);
                        } else {
                            path = path.substring(parser.index - 1);
                        }
                        if (path.startsWith(".")) {
                            path = path.substring(1);
                        }
                        continue main;
                    } else if (parsed instanceof FlexiJSonObject) {
                        // conditional Property list[{a:true}] << all list entries with this.a:true
                        try {
                            ret.add(new FlexiConditionMapper<Condition>(Condition.class).jsonToObject(parsed, Condition.TYPE));
                        } catch (FlexiMapperException e) {
                            throw new InvalidPathException(e);
                        }
                        if (parser.isEndOfStreamReached()) {
                            path = path.substring(parser.index);
                        } else {
                            path = path.substring(parser.index - 1);
                        }
                        if (path.startsWith(".")) {
                            path = path.substring(1);
                        }
                    } else {
                        throw new InvalidPathException("Invalid []-escaping");
                    }
                } catch (FlexiParserException e) {
                    throw new InvalidPathException(e);
                }
            } else {
                int pointIndex = path.indexOf(".");
                int parenthesisIndexIndex = path.indexOf("[");
                if (pointIndex < 0 && parenthesisIndexIndex < 0) {
                    ret.add(path);
                    return ret;
                } else if (parenthesisIndexIndex > 0 && (parenthesisIndexIndex < pointIndex || pointIndex < 0)) {
                    key = path.substring(0, parenthesisIndexIndex);
                    ret.add(key);
                    path = path.substring(parenthesisIndexIndex);
                    continue main;
                } else {
                    key = path.substring(0, pointIndex);
                    ret.add(key);
                    path = path.substring(pointIndex + 1);
                    continue main;
                }
            }
        }
    }

    public static final String COMMENT_PATH_PREFIX = "//#";

    /**
     * @param json
     * @return
     * @throws InvalidPathException
     */
    public static JSPath fromFlexiNode(FlexiJSonNode org) throws InvalidPathException {
        if (org == null) {
            throw new IllegalArgumentException("Node is null");
        }
        FlexiJSonNode json = org;
        JSPath path = new JSPath();
        next: while (json != null) {
            FlexiJSonNode parent = json.getParent();
            if (parent == json) {
                throw new InvalidPathException("Bad Path");
            }
            if (parent == null) {
                break;
            }
            if (json instanceof FlexiComment) {
                if (parent instanceof FlexiJSonComments) {
                    parent = parent.getParent();
                }
                if (parent instanceof FlexiJSonValue) {
                    int index = 0;
                    for (FlexiCommentJsonNode c : new ConcatIterator<FlexiCommentJsonNode>(parent.getCommentsBefore(), parent.getCommentsAfter())) {
                        if (c == json) {
                            path.add(0, COMMENT_PATH_PREFIX + index);
                            json = parent;
                            continue next;
                        }
                        index++;
                    }
                    parent = parent.getParent();
                }
                if (parent instanceof FlexiJSonObject) {
                    ArrayList<FlexiJSonComments> collect = new ArrayList<FlexiJSonComments>();
                    collect.add(((FlexiJSonObject) parent).getCommentsBefore());
                    collect.add(((FlexiJSonObject) parent).getCommentsInside());
                    for (KeyValueElement e : ((FlexiJSonObject) parent).getElements()) {
                        if (e.getCommentsBeforeKey() != null) {
                            collect.add(e.getCommentsBeforeKey());
                        }
                        if (e.getCommentsAfterKey() != null) {
                            collect.add(e.getCommentsAfterKey());
                        }
                    }
                    collect.add(((FlexiJSonObject) parent).getCommentsAfter());
                    int index = 0;
                    for (FlexiCommentJsonNode c : new ConcatIterator<FlexiCommentJsonNode>(collect.toArray(new FlexiJSonComments[] {}))) {
                        if (c == json) {
                            path.add(0, COMMENT_PATH_PREFIX + index);
                            json = parent;
                            continue next;
                        }
                        index++;
                    }
                } else if (parent instanceof FlexiJSonArray) {
                    int index = 0;
                    for (FlexiCommentJsonNode c : new ConcatIterator<FlexiCommentJsonNode>(((FlexiJSonArray) parent).getCommentsBefore(), ((FlexiJSonArray) parent).getCommentsInside(), ((FlexiJSonArray) parent).getCommentsAfter())) {
                        if (c == json) {
                            path.add(0, COMMENT_PATH_PREFIX + index);
                            json = parent;
                            continue next;
                        }
                        index++;
                    }
                }
                throw new InvalidPathException("Bad Path");
            }
            if (parent instanceof FlexiJSonObject) {
                for (KeyValueElement e : ((FlexiJSonObject) parent).getElements()) {
                    if (e.getValue() == json) {
                        json = parent;
                        path.add(0, e.getKey());
                        continue next;
                    }
                }
                throw new InvalidPathException("Bad Path: Element is not a child of its own parent");
            } else if (parent instanceof FlexiJSonArray) {
                for (int i = 0; i < ((FlexiJSonArray) parent).size(); i++) {
                    if (((FlexiJSonArray) parent).get(i) == json) {
                        path.add(0, i);
                        json = parent;
                        continue next;
                    }
                }
                throw new InvalidPathException("Bad Path");
            } else if (parent instanceof FlexiJSonValue) {
                throw new InvalidPathException("Bad Path");
            } else {
                throw new InvalidPathException("Unknown entry");
            }
        }
        // if (org instanceof FlexiJSonComments || org instanceof FlexiCommentJsonNode) {
        // path.add("//");
        // }
        return path;
    }

    /**
     * @param i
     * @param string
     * @return
     */
    public JSPath add(int i, Object element) {
        elements.add(i, element);
        return this;
    }

    @Deprecated
    public String toPathString() {
        return toPathString(true);
    }

    public String toPathString(boolean leadingPoint) {
        StringBuilder sb = new StringBuilder();
        for (Object o : this.elements) {
            if (isArrayKey(o)) {
                sb.append("[" + o + "]");
            } else if (o instanceof String) {
                if (((String) o).matches("//\\#\\d+") && o == this.getLast()) {
                    sb.append(".");
                    sb.append(o);
                } else {
                    if (!String.valueOf(o).matches("^[a-zA-Z_$][a-zA-Z_$0-9]*$")) {
                        try {
                            sb.append("[" + FlexiUtils.serializeMinimized(o) + "]");
                        } catch (FlexiMapperException e) {
                            throw new WTFException(e);
                        }
                    } else {
                        sb.append(".");
                        sb.append(o);
                    }
                }
            } else if (o instanceof Condition) {
                sb.append("[" + FlexiUtils.serializeMinimizedWithWTF(o) + "]");
            } else {
                throw new WTFException("Unsupported Path entry");
            }
        }
        String ret = sb.toString();
        if (!leadingPoint && ret.startsWith(".")) {
            return ret.substring(1);
        } else {
            return ret;
        }
    }

    /**
     * @return
     */
    public Object getLast() {
        if (elements.size() == 0) {
            return null;
        }
        return elements.getLast();
    }

    /*
     * (non-Javadoc)
     *
     * @see java.util.AbstractCollection#toString()
     */
    @Override
    public String toString() {
        return "JSPath:" + toPathString(true);
    }

    /**
     * @return
     */
    public JSPath getParent() {
        try {
            if (size() == 0) {
                return null;
            }
            JSPath ret = new JSPath(elements.subList(0, size() - 1));
            return ret;
        } catch (java.lang.IllegalArgumentException e) {
            throw new WTFException(e);
        }
    }

    /**
     * @return
     */
    public int size() {
        return elements.size();
    }

    /**
     * @param key
     * @return
     */
    public JSPath derive(Object key) {
        JSPath ret = new JSPath(this.elements);
        ret.add(key);
        return ret;
    }

    /**
     * Returns a new instance that contains the elements of this and appended the elements of the parametr
     *
     * @param path
     * @return
     */
    public JSPath append(JSPath path) {
        // this works only, because we create a new list in the JSPath constructor
        JSPath ret = new JSPath(this.elements);
        ret.elements.addAll(path.elements);
        return ret;
    }

    /**
     * @param key
     * @return
     */
    public JSPath add(Object key) {
        elements.add(key);
        return this;
    }

    /**
     * @return
     */
    public List<Object> getElements() {
        return Collections.unmodifiableList(elements);
    }

    /**
     * @return
     */
    public Object getFirst() {
        return elements.get(0);
    }

    /**
     * @return
     */
    public boolean isEmpty() {
        return elements.isEmpty();
    }

    /**
     * @param $$this
     * @return
     * @throws InvalidPathException
     */
    public JSPath withPrefix(String path) throws InvalidPathException {
        JSPath ret = fromPathString(path);
        ret.elements.addAll(this.elements);
        return ret;
    }

    /**
     * @param path
     * @return
     */
    public boolean startsWith(JSPath path) {
        if (path.size() > size()) {
            return false;
        }
        for (int i = 0; i < path.size(); i++) {
            if (!String.valueOf(elements.get(i)).equals(String.valueOf(path.elements.get(i)))) {
                return false;
            }
        }
        return true;
    }

    /**
     * @param string
     * @return
     */
    public static JSPath getNE(String string) {
        try {
            return fromPathString(string);
        } catch (InvalidPathException e) {
            throw new WTFException(e);
        }
    }

    /**
     * @param key
     * @return
     */
    public static JSPath fromPathElements(Object... elements) {
        return new JSPath(Arrays.asList(elements));
    }

    public static JSPath fromPathElements(List<Object> elements) {
        return new JSPath(elements);
    }

    /**
     * @param nextPathElement
     * @return
     */
    public static boolean isArrayKey(Object e) {
        if (e == null) {
            return false;
        }
        if (Clazz.isFixedPointNumber(e.getClass())) {
            return true;
        }
        if (String.valueOf(e).matches("^[0-9]+$")) {
            return true;
        }
        return false;
    }

    /**
     * @param pathElement
     * @return
     */
    public static int toArrayIndex(Object e) throws NumberFormatException {
        return e instanceof Number ? ((Number) e).intValue() : Integer.parseInt(String.valueOf(e));
    }

    /**
     * @param o
     * @return
     * @throws ConditionException
     */
    public Object resolve(Object o) throws ConditionException {
        return new Condition().resolveKeyPath(new Scope(o), this).getLast();
    }

    /**
     * return the nth element. if index is <0, it is the nth element from the back. -1 is the last
     *
     * @param i
     * @return
     */
    public Object get(int index) {
        if (index < 0) {
            index = size() + index;
        }
        return this.elements.get(index);
    }

    /**
     * @param keyPath
     * @return
     */
    public JSPath getRelative(JSPath keyPath) {
        JSPath ret = new JSPath();
        int sameUntil = 0;
        for (sameUntil = 0; sameUntil < keyPath.size(); sameUntil++) {
            if (!CompareUtils.equals(elements.get(sameUntil), keyPath.elements.get(sameUntil))) {
                sameUntil--;
                break;
            }
        }
        if (sameUntil >= 0) {
            ret.elements.addAll(this.elements.subList(sameUntil, elements.size()));
        }
        return ret;
    }
}
