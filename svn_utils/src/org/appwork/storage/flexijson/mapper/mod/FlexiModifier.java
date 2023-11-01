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
package org.appwork.storage.flexijson.mapper.mod;

import java.util.List;
import java.util.Set;

import org.appwork.exceptions.WTFException;
import org.appwork.moncompare.Condition;
import org.appwork.storage.flexijson.FlexiComment;
import org.appwork.storage.flexijson.FlexiJSonArray;
import org.appwork.storage.flexijson.FlexiJSonComments;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.FlexiJSonObject;
import org.appwork.storage.flexijson.FlexiJSonValue;
import org.appwork.storage.flexijson.FlexiUtils;
import org.appwork.storage.flexijson.InvalidPathException;
import org.appwork.storage.flexijson.JSPath;
import org.appwork.storage.flexijson.KeyValueElement;
import org.appwork.storage.flexijson.mapper.FlexiJSonMapper;
import org.appwork.storage.flexijson.mapper.FlexiMapperException;
import org.appwork.storage.simplejson.ValueType;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.logging2.ConsoleLogImpl;
import org.appwork.utils.logging2.LogInterface;

/**
 * @author thomas
 * @date 06.07.2023
 *
 */
public class FlexiModifier<T extends FlexiJSonNode, MatcherType> {
    private final T targetObject;

    public T getTargetObject() {
        return this.targetObject;
    }

    private boolean               breakOnError = true;
    private final FlexiJSonMapper mapper;

    public FlexiJSonMapper getMapper() {
        return this.mapper;
    }

    private MatcherType conditionMapper;

    public boolean isBreakOnError() {
        return this.breakOnError;
    }

    public void setBreakOnError(final boolean breakOnError) {
        this.breakOnError = breakOnError;
    }

    /**
     * @param <T>
     * @param ret
     */
    public FlexiModifier(final T base) {
        this.targetObject = base;
        this.mapper = this.createMapper();
    }

    protected FlexiJSonMapper createMapper() {
        return new FlexiJSonMapper();
    }

    /**
     * @param modInstance
     * @return
     * @throws MergeException
     */
    public T merge(final List<JsonModification<FlexiJSonObject, MatcherType>> mod) throws MergeException {
        for (final JsonModification<FlexiJSonObject, MatcherType> m : mod) {
            this.merge(m);
        }
        return this.targetObject;
    }

    /**
     * @param modInstance
     * @return
     * @throws MergeException
     */
    public int merge(final JsonModification<FlexiJSonObject, MatcherType> mod) throws MergeException {
        final Condition[] conditions = mod.getConditions();
        if (conditions != null && conditions.length > 0 && !this.evalConditions(mod.getConditions())) {
            return -1;
        }
        final int modsBefore = this.changes;
        final FlexiJSonObject set = mod.getSet();
        if (set != null) {
            this.set(set, false);
        }
        final FlexiJSonObject setIfUnset = mod.getSetIfUnset();
        if (setIfUnset != null) {
            this.set(setIfUnset, true);
        }
        final Set<String> unset = mod.getUnset();
        if (unset != null) {
            this.unset(unset);
        }
        return this.changes - modsBefore;
    }

    /**
     * @param unset
     * @throws MergeException
     */
    private int unset(final Set<String> unset) throws MergeException {
        for (final String path : unset) {
            try {
                JSPath jsPath;
                jsPath = this.fromPathString(path);
                this.unset(jsPath);
            } catch (final Exception e1) {
                if (this.isBreakOnError(e1)) {
                    throw MergeException.wrap(e1);
                }
            }
        }
        return this.changes;
    }

    public void unset(final JSPath jsPath) throws IllegalPathException {
        final FlexiJSonNode parent = this.targetObject.resolvePath(jsPath.getParent());
        if (parent == null) {
            return;
        }
        final Object pathElement = jsPath.getLast();
        if (parent instanceof FlexiJSonArray) {
            try {
                final int index = JSPath.toArrayIndex(pathElement);
                if (((FlexiJSonArray) parent).size() > index) {
                    ((FlexiJSonArray) parent).remove(index);
                    if (this.logger != null) {
                        this.logger.info("Removed from array " + jsPath.toPathString(true));
                    }
                    this.changes++;
                    return;
                }
            } catch (final NumberFormatException e1) {
                throw new IllegalPathException("Element is an array, but the path does NOT contain an int key" + jsPath.toPathString(true));
            }
        } else if (parent instanceof FlexiJSonObject) {
            if (JSPath.isArrayKey(pathElement)) {
                throw new IllegalPathException("Element is an object, but the path contains an int key" + jsPath.toPathString(true));
            }
            if (((FlexiJSonObject) parent).remove(String.valueOf(pathElement)) != null) {
                if (this.logger != null) {
                    this.logger.info("Removed from object " + jsPath.toPathString(true));
                }
                this.changes++;
                return;
            }
            return;
        } else {
            throw new IllegalPathException("Path exists, but contains an unsupported element: " + jsPath.getParent() + "; Type:" + parent.getClass());
        }
        return;
    }

    public int getChanges() {
        return this.changes;
    }

    public LogInterface getLogger() {
        return this.logger;
    }

    public void setLogger(final LogInterface logger) {
        this.logger = logger;
    }

    private boolean autoCreateStructures = true;

    // create Objects and arrays of required to set a property
    // like a.b[9]c=1 << auto create object a, array b, 0-8 null entries
    public boolean isAutoCreateStructures() {
        return this.autoCreateStructures;
    }

    public void setAutoCreateStructures(final boolean autoCreateStructures) {
        this.autoCreateStructures = autoCreateStructures;
    }

    /**
     * @param set
     * @param onlyIfUnset
     * @throws MergeException
     */
    private void set(final FlexiJSonObject set, final boolean onlyIfUnset) throws MergeException {
        for (final KeyValueElement e : set.getElements()) {
            final FlexiJSonNode value = e.getValue();
            if (value == null) {
                continue;
            }
            if (value instanceof FlexiJSonComments) {
                continue;
            }
            if (value instanceof FlexiComment) {
                continue;
            }
            try {
                JSPath jsPath;
                jsPath = this.fromPathString(e.getKey());
                this.set(jsPath, value, onlyIfUnset);
            } catch (final Exception e1) {
                if (this.isBreakOnError(e1)) {
                    throw MergeException.wrap(e1);
                }
            }
        }
    }

    protected JSPath fromPathString(final String key) throws InvalidPathException {
        return JSPath.fromPathString(key);
    }

    private LogInterface logger = new ConsoleLogImpl();
    private int          changes;

    protected void set(final JSPath jsPath, final FlexiJSonNode value, final boolean onlyIfUnset) throws IllegalPathException, FlexiMapperException {
        final FlexiJSonNode parent = this.resolveParent(this.targetObject, jsPath);
        final Object pathElement = jsPath.getLast();
        if (parent instanceof FlexiJSonArray) {
            try {
                final int index = JSPath.toArrayIndex(pathElement);
                if (((FlexiJSonArray) parent).size() < index) {
                    if (this.isAutoCreateStructures()) {
                        for (int ii = ((FlexiJSonArray) parent).size(); ii < index; ii++) {
                            ((FlexiJSonArray) parent).add(this.mapper.createFlexiJSonValue());
                            if (this.logger != null) {
                                this.logger.info("Add undefined array " + jsPath.getParent().derive(ii).toPathString(true));
                            }
                            this.changes++;
                        }
                    } else {
                        throw new IllegalPathException("Path does not exist: " + jsPath.toPathString(true));
                    }
                }
                if (((FlexiJSonArray) parent).size() <= index) {
                    ((FlexiJSonArray) parent).add(value);
                    if (this.logger != null) {
                        this.logger.info("Add to array " + jsPath.toPathString(true) + " = " + FlexiUtils.serializeMinimizedWithWTF(value));
                    }
                    this.changes++;
                    return;
                } else {
                    if (onlyIfUnset) {
                        return;
                    }
                    ((FlexiJSonArray) parent).set(index, value);
                    if (this.logger != null) {
                        this.logger.info("Set in array " + jsPath.toPathString(true));
                    }
                    this.changes++;
                    return;
                }
            } catch (final NumberFormatException e1) {
                throw new IllegalPathException("Element is an array, but the path does NOT contain an int key" + jsPath.toPathString(true));
            }
        } else if (parent instanceof FlexiJSonObject) {
            if (JSPath.isArrayKey(pathElement)) {
                throw new IllegalPathException("Element is an object, but the path contains an int key" + jsPath.toPathString(true));
            }
            if (onlyIfUnset && ((FlexiJSonObject) parent).getElement(String.valueOf(pathElement)) != null) {
                return;
            }
            // old.get
            ((FlexiJSonObject) parent).put(String.valueOf(pathElement), value);
            if (this.logger != null) {
                final Object old = ((FlexiJSonObject) parent).getNode(String.valueOf(pathElement));
                if (!CompareUtils.equalsDeep(value, old)) {
                    this.logger.info("Put in object " + jsPath.toPathString(true) + " = " + FlexiUtils.serializeMinimizedWithWTF(value) + "/old: " + old);
                }
            }
            this.changes++;
            return;
        } else {
            throw new IllegalPathException("Path exists, but contains an unsupported element: " + jsPath.getParent() + "; Type:" + parent.getClass());
        }
    }

    /**
     * @param base2
     * @param jsPath
     * @return
     * @throws IllegalPathException
     */
    private FlexiJSonNode resolveParent(FlexiJSonNode parent, final JSPath jsPath) throws IllegalPathException {
        final JSPath currentPath = new JSPath();
        for (int i = 0; i < jsPath.size() - 1; i++) {
            final Object pathElement = jsPath.getElements().get(i);
            final Object nextPathElement = jsPath.getElements().get(i + 1);
            currentPath.add(pathElement);
            if (parent instanceof FlexiJSonArray) {
                final FlexiJSonArray oldParent = (FlexiJSonArray) parent;
                try {
                    final int index = JSPath.toArrayIndex(pathElement);
                    if (((FlexiJSonArray) parent).size() <= index) {
                        parent = null;
                    } else {
                        parent = ((FlexiJSonArray) parent).get(index);
                    }
                    if (parent instanceof FlexiJSonValue) {
                        if (((FlexiJSonValue) parent).getType() == ValueType.NULL || ((FlexiJSonValue) parent).getType() == ValueType.UNDEFINED) {
                            parent = null;
                        } else {
                            throw new IllegalPathException("Path exists, but directs to a value. Expected: " + (nextPathElement instanceof Number ? "Array" : "Object"));
                        }
                    }
                    if (parent == null) {
                        if (this.isAutoCreateStructures()) {
                            if (JSPath.isArrayKey(nextPathElement)) {
                                parent = this.mapper.createFlexiJSonArray(10);
                            } else {
                                parent = this.mapper.createFlexiJSonObject();
                            }
                            for (int ii = oldParent.size(); ii < index; ii++) {
                                oldParent.add(this.mapper.createFlexiJSonValue());
                            }
                            oldParent.add(parent);
                        } else {
                            throw new IllegalPathException("Path does not exist: " + currentPath.toPathString(true));
                        }
                    }
                } catch (final NumberFormatException e1) {
                    throw new IllegalPathException("Element is an array, but the path does NOT contain an int key" + currentPath.toPathString(true));
                }
            } else if (parent instanceof FlexiJSonObject) {
                final FlexiJSonObject oldParent = (FlexiJSonObject) parent;
                if (JSPath.isArrayKey(pathElement)) {
                    throw new IllegalPathException("Element is an object, but the path contains an int key" + currentPath.toPathString(true));
                }
                final KeyValueElement element = ((FlexiJSonObject) parent).getElement(String.valueOf(pathElement));
                if (element == null) {
                    parent = null;
                } else {
                    parent = element.getValue();
                }
                if (parent instanceof FlexiJSonValue) {
                    if (((FlexiJSonValue) parent).getType() == ValueType.NULL || ((FlexiJSonValue) parent).getType() == ValueType.UNDEFINED) {
                        parent = null;
                    } else {
                        throw new IllegalPathException("Path exists, but directs to a value. Expected: " + (nextPathElement instanceof Number ? "Array" : "Object"));
                    }
                }
                if (parent == null) {
                    if (this.isAutoCreateStructures()) {
                        if (JSPath.isArrayKey(nextPathElement)) {
                            parent = this.mapper.createFlexiJSonArray(10);
                        } else {
                            parent = this.mapper.createFlexiJSonObject();
                        }
                        try {
                            oldParent.put(String.valueOf(pathElement), parent);
                        } catch (final FlexiMapperException e) {
                            throw new WTFException("Should not happen. Parent is a node");
                        }
                    } else {
                        throw new IllegalPathException("Path does not exist: " + currentPath.toPathString(true));
                    }
                }
            } else {
                throw new IllegalPathException("Path exists, but contains an unsupported element: " + currentPath.getParent() + "; Type:" + parent.getClass());
            }
        }
        return parent;
    }

    /**
     * @param e
     * @return
     */
    public boolean isBreakOnError(final Exception e) {
        return this.breakOnError;
    }

    /**
     * returns false
     *
     * @param conditions
     * @return
     */
    protected boolean evalConditions(final Condition[] conditions) {
        for (final Condition c : conditions) {
            if (!c.matchesWithoutExceptions(this.conditionMapper)) {
                if (this.logger != null) {
                    this.logger.info("Condition failed: " + FlexiUtils.serializeMinimizedWithWTF(c));
                }
                return false;
            }
        }
        if (this.logger != null) {
            this.logger.info("Conditions matched: " + FlexiUtils.serializeMinimizedWithWTF(conditions));
        }
        return true;
    }

    /**
     * @param conditionMapper
     * @return
     */
    public FlexiModifier<T, MatcherType> conditionMatcher(final MatcherType conditionMapper) {
        this.conditionMapper = conditionMapper;
        return this;
    }
}
