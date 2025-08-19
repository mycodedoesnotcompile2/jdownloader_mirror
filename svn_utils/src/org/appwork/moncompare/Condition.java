/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
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
package org.appwork.moncompare;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.moncompare.list.ArrayAccessor;
import org.appwork.moncompare.list.CollectionAccessor;
import org.appwork.moncompare.list.ListAccessor;
import org.appwork.moncompare.list.ListAccessorInterface;
import org.appwork.moncompare.object.MapAccessor;
import org.appwork.moncompare.object.MapAccessorInterface;
import org.appwork.moncompare.typehandler.DateHandler;
import org.appwork.moncompare.typehandler.TimeSpanHandler;
import org.appwork.remoteapi.annotations.ApiDoc;
import org.appwork.remoteapi.annotations.ApiDocExample;
import org.appwork.storage.Storable;
import org.appwork.storage.StorableDoc;
import org.appwork.storage.StorableExample;
import org.appwork.storage.flexijson.InvalidPathException;
import org.appwork.storage.flexijson.JSPath;
import org.appwork.storage.simplejson.mapper.ClassCache;
import org.appwork.storage.simplejson.mapper.Getter;
import org.appwork.testframework.AWTestValidateClassReference;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.ConcatIterator;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Exceptions;
import org.appwork.utils.StringUtils;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.reflection.Clazz;

/**
 * A compare class inspired by the mongodb queries. https://docs.mongodb.com/manual/reference/operator/expression/in/
 *
 *
 *
 *
 *
 *
 *
 *
 * @author Thomas
 * @date 06.05.2019
 *
 */
// * "Query Filter Mode" vs "Strict Compare Mode"
// *
// * "Strict Compare Mode"
// * compares condition vs. matcher one one one. That means ALL keys must match.
// * {prop:{a:true}} matches {prop:{a:true,b:true }} ==> FALSE
// * All comparisms except some exceptions work like this.
// *
// * Exceptions: "Query Filter Mode"
// * - The Root Condition works in Filter mode.
// * - All direct children of Operators that act as Filter Root run in filter mode. (like $or)
// * {a:true} matches {a:true,b:true } ==> TRUE
// * {$or:[{a:true},{b:true}]} matches {a:true} and {b:true}
// * HOWEVER:
// * {$or:[{§§THIS:{a:true}},{b:true}]} matches only {b:true}, because for {§§THIS:{a:true}} a:true is not a direct children
@ApiDoc("A Condition Object. See the WIKI for more details.")
@StorableExample("{\"$eq\":\"MyValue\"}")
public class Condition<MatcherType> extends LinkedHashMap<String, Object> implements Storable {
    /**
     *
     */
    @AWTestValidateClassReference
    public static final String CLASS_ORG_APPWORK_STORAGE_COMMON_INTERFACE_SERIALIZER_INTERFACE = "org.appwork.storage.commonInterface.SerializerInterface";
    /**
     *
     */
    @AWTestValidateClassReference
    public static final String CLASS_ORG_APPWORK_SERIALIZER_DESER                              = "org.appwork.serializer.Deser";

    public static Condition<Object> C() {
        return new Condition<Object>();
    }

    public static Condition<Object> C(final String key, final Object b) {
        return new Condition<Object>(key, b);
    }

    /**
     *
     */
    public static final String  CASE_INSENSITIVE = "caseInsensitive";
    /**
     *
     */
    private static final String $KEY             = "§key";
    public static final String  FILTER_ROOT      = "filterRoot";
    /**
     *
     */
    public static final String  $PROJECT         = "§project";

    /**
     * @author Thomas
     * @date 07.05.2019
     *
     */
    public static class AndOp implements Operator {
        /**
         * See "Query Filter mode" vs. "Strict Compare mode"
         */
        @Override
        public boolean isFilterRoot() {
            return true;
        }

        @Override
        public Object opEval(final Condition<?> container, final Object expression, final Scope scope) throws ConditionException {
            // no reason to unwrap - KEY_DOES_NOT_EXIST is never wrapped
            if (scope.getLast() == KEY_DOES_NOT_EXIST) {
                return false;
            } else {
                for (final Object exp : container.getListWrapper(expression)) {
                    final Object resolved = container.resolveValue(container, exp, scope, false);
                    if (container.isFalse(resolved)) {
                        return false;
                    }
                }
                return true;
            }
        }
    }

    public static class AnyOp implements Operator {
        /**
         * See "Query Filter mode" vs. "Strict Compare mode"
         */
        @Override
        public boolean isFilterRoot() {
            return true;
        }

        @Override
        public Object opEval(final Condition<?> container, final Object expression, final Scope scope) throws ConditionException {
            final Object last = scope.getLast();
            if (last == KEY_DOES_NOT_EXIST) {
                return false;
            } else if (container.isValue(last)) {
                return container.resolveValue(container, expression, scope, false);
            } else {
                try {
                    for (Iterator<KeyValue> it = container.iterator(last); it.hasNext();) {
                        KeyValue next = it.next();
                        final Scope newScope = scope.copy();
                        newScope.add(next.value, next.key);
                        if (container.equalsDeep(container, expression, newScope.getLast(), newScope)) {
                            // {§any:1}
                            if (container._isDebug()) {
                                container.log(newScope.getPath(), this.getClass().getSimpleName() + ": " + container.resolveValue(container, expression, newScope, false) + ".matches(" + debugToString(expression) + ")");
                            }
                            return true;
                        }
                    }
                } catch (final SecurityException e) {
                    throw new ConditionException(e);
                }
            }
            if (container._isDebug()) {
                container.log(scope.getPath(), this.getClass().getSimpleName() + ": no element matches(" + expression + ")");
            }
            return false;
        }
    }

    public static class ConcatOp implements Operator {
        /**
         * See "Query Filter mode" vs. "Strict Compare mode"
         */
        @Override
        public boolean isFilterRoot() {
            return false;
        }

        @Override
        public Object opEval(final Condition<?> container, final Object expression, final Scope scope) throws ConditionException {
            try {
                ListAccessorInterface access = container.getListWrapper(expression);
                if (access != null) {
                    final StringBuilder sb = new StringBuilder();
                    for (final Object exp : access) {
                        sb.append(toCharSequence(container.resolveValue(container, exp, scope, true)));
                    }
                    return sb.toString();
                }
            } catch (final ConditionException e) {
                throw new AggregationException(e);
            }
            throw new AggregationException();
        }
    }

    public static class ToUpperCaseOp implements Operator {
        /**
         * See "Query Filter mode" vs. "Strict Compare mode"
         */
        @Override
        public boolean isFilterRoot() {
            return false;
        }

        @Override
        public Object opEval(final Condition<?> container, final Object expression, final Scope scope) throws ConditionException {
            try {
                return String.valueOf(container.resolveValue(container, expression, scope, true)).toUpperCase(Locale.ROOT);
            } catch (final ConditionException e) {
                throw new AggregationException(e);
            }
        }
    }

    public static class ToLowerCaseOp implements Operator {
        /**
         * See "Query Filter mode" vs. "Strict Compare mode"
         */
        @Override
        public boolean isFilterRoot() {
            return false;
        }

        @Override
        public Object opEval(final Condition<?> container, final Object expression, final Scope scope) throws ConditionException {
            try {
                return String.valueOf(container.resolveValue(container, expression, scope, true)).toLowerCase(Locale.ROOT);
            } catch (final ConditionException e) {
                throw new AggregationException(e);
            }
        }
    }

    public static class GetOp implements Operator {
        /**
         * See "Query Filter mode" vs. "Strict Compare mode"
         */
        @Override
        public boolean isFilterRoot() {
            return false;
        }

        @Override
        public Object opEval(final Condition<?> container, final Object expression, final Scope scope) throws ConditionException {
            try {
                return container.resolveValue(container, expression, scope, true);
            } catch (final ConditionException e) {
                throw new AggregationException(e);
            }
        }
    }

    public static class SearchAndReplaceOp implements Operator {
        /**
         *
         */

        /**
         * See "Query Filter mode" vs. "Strict Compare mode"
         */
        @Override
        public boolean isFilterRoot() {
            return false;
        }

        @Override
        public Object opEval(final Condition<?> container, final Object expression, final Scope scope) throws ConditionException {
            try {
                boolean allowReferences = false;
                Object options = container.getOptions(Object.class);

                if (options instanceof Map) {
                    allowReferences = Boolean.TRUE.equals(((Map) options).get(OPTIONS_ALLOW_REFERENCES));
                }

                final ListAccessorInterface access = container.getListWrapper(expression);
                final CharSequence str = toCharSequence(container.resolveValue(container, access.get(0), scope, true));
                final Pattern searchPattern = Pattern.compile(String.valueOf(container.resolveValue(container, access.get(1), scope, true)));
                String replacement = String.valueOf(container.resolveValue(container, access.get(2), scope, true));
                replacement = (allowReferences ? replacement : Matcher.quoteReplacement(replacement));
                final Matcher matcher = searchPattern.matcher(str);
                if (access.size() > 3) {
                    boolean match = false;
                    // replace group ID only
                    final int groupIndex = ((Number) container.resolveValue(container, access.get(3), scope, true)).intValue();
                    final StringBuffer sb = new StringBuffer();
                    while (matcher.find()) {
                        match = true;
                        final String fullMatch = matcher.group(0);
                        final int start = matcher.start(groupIndex) - matcher.start();
                        final int end = matcher.end(groupIndex) - matcher.start();
                        // only replace the group's content inside the match
                        // by default references are disabled. else we would get issues e.g. if we replace XXX by a directory path (which
                        // contains \ or $ .. add §options:{allowReferences:true} to allow references
                        final String updatedMatch = Matcher.quoteReplacement(fullMatch.substring(0, start)) + replacement + Matcher.quoteReplacement(fullMatch.substring(end));
                        matcher.appendReplacement(sb, updatedMatch);
                    }
                    if (!match) {
                        return str.toString();
                    } else {
                        matcher.appendTail(sb);
                        return sb.toString();
                    }
                } else {
                    final String result = matcher.replaceAll(replacement);
                    return result;
                }
            } catch (final ConditionException e) {
                throw new AggregationException(e);
            }
        }
    }

    public abstract interface ConditionResolver<Input, Output> {
        /**
         * @param parameters
         * @return
         */
        public Output resolve(Input options) throws ConditionException;
    }

    public static class DivideOp implements Operator {
        /**
         * See "Query Filter mode" vs. "Strict Compare mode"
         */
        @Override
        public boolean isFilterRoot() {
            return false;
        }

        @Override
        public Object opEval(final Condition<?> container, final Object expressions, final Scope scope) throws ConditionException {
            ListAccessorInterface list = container.getListWrapper(expressions);
            if (list != null && list.size() == 2) {
                final Number dividend = (Number) container.resolveValue(container, list.get(0), scope, true);
                final Number divisor = (Number) container.resolveValue(container, list.get(1), scope, true);
                final Number result;
                if (Clazz.isDouble(dividend.getClass()) || Clazz.isDouble(divisor.getClass())) {
                    result = dividend.doubleValue() / divisor.doubleValue();
                } else if (Clazz.isFloat(dividend.getClass()) || Clazz.isFloat(divisor.getClass())) {
                    result = dividend.floatValue() / divisor.floatValue();
                } else {
                    result = dividend.longValue() / divisor.longValue();
                }
                return new AggregationResult(result);
            }
            throw new AggregationException("DivideOp:" + (container) + "|" + (expressions));
        }
    }

    public static class ProjectOp implements Operator {
        /**
         * See "Query Filter mode" vs. "Strict Compare mode"
         */
        @Override
        public boolean isFilterRoot() {
            return true;
        }

        @Override
        public Object opEval(final Condition<?> container, final Object expression, final Scope scope) throws ConditionException {
            final Object last = scope.getLast();
            if (last == KEY_DOES_NOT_EXIST) {
                if (container._isDebug()) {
                    container.log(scope.getPath(), this.getClass().getSimpleName() + " -> KEY_DOES_NOT_EXIST ");
                }
                return last;
            } else {
                if (last == null || last instanceof String || Clazz.isPrimitive(last.getClass()) || Clazz.isEnum(last.getClass())) {
                    if (container.isTrue(container.resolveValue(container, expression, scope, false))) {
                        return last;
                    } else {
                        return null;
                    }
                }
                MapAccessorInterface map = container.getMapWrapper(last);
                if (map != null) {
                    final HashMap<Object, Object> result = new HashMap<Object, Object>();
                    final Scope newScope = scope.copy();
                    for (final Map.Entry<?, ?> entry : map) {
                        final Object key = entry.getKey();
                        final Object o = entry.getValue();
                        newScope.add(o, key);
                        try {
                            if (container.isTrue(container.resolveValue(container, expression, newScope, false))) {
                                result.put(key, o);
                            }
                        } catch (ConditionException e) {
                            // failed to project - continue; the filter condition may not match to o -> Error. However, others may match.
                        } finally {
                            newScope.removeLast();
                        }
                    }
                    if (container._isDebug()) {
                        container.log(scope.getPath(), this.getClass().getSimpleName() + " -> Map with " + result.size() + " entries: " + result.keySet());
                    }
                    return container.createMap(result);
                }
                Iterable<Object> listIter = container.getListWrapper(last);
                if (listIter != null) {
                    final LinkedList<Object> result = new LinkedList<Object>();
                    int i = 0;
                    final Scope newScope = scope.copy();
                    for (final Object o : listIter) {
                        newScope.add(o, i++);
                        try {
                            if (container.isTrue(container.resolveValue(container, expression, newScope, false))) {
                                result.add(o);
                            }
                        } catch (ConditionException e) {
                            // failed to project - continue; the filter condition may not match to the entry -> Error. However, others may
                            // match.
                        } finally {
                            newScope.removeLast();
                        }
                    }
                    return container.createList(result);
                } else {
                    final HashMap<Object, Object> result = new HashMap<Object, Object>();
                    try {
                        final Scope newScope = scope.copy();
                        for (final Getter key : ClassCache.getClassCache(last.getClass()).getGetter()) {
                            final Object o = key.getValue(last);
                            newScope.add(o, key);
                            try {
                                if (container.isTrue(container.resolveValue(container, expression, newScope, false))) {
                                    result.put(key.getKey(), o);
                                }
                            } catch (ConditionException e) {
                                // failed to project - continue; the filter condition may not match to the entry -> Error. However, others
                                // may match.
                            } finally {
                                newScope.removeLast();
                            }
                        }
                    } catch (final SecurityException e) {
                        throw new ConditionException(e);
                    } catch (final NoSuchMethodException e) {
                        throw new ConditionException(e);
                    } catch (final IllegalArgumentException e) {
                        throw new ConditionException(e);
                    } catch (final IllegalAccessException e) {
                        throw new ConditionException(e);
                    } catch (final InvocationTargetException e) {
                        if (e.getTargetException() instanceof InterruptedException) {
                            Thread.currentThread().interrupt();
                        }
                    }
                    return container.createMap(result);
                }
            }
        }
    }

    public static class EachOp implements Operator {
        /**
         * See "Query Filter mode" vs. "Strict Compare mode"
         */
        @Override
        public boolean isFilterRoot() {
            return true;
        }

        @Override
        public Object opEval(final Condition<?> container, final Object expression, final Scope scope) throws ConditionException {
            final Object last = container.unwrapType(scope.getLast());
            if (last == KEY_DOES_NOT_EXIST) {
                return false;
            } else if (last == null || Clazz.isPrimitive(last.getClass()) || last instanceof String) {
                return container.resolveValue(container, expression, scope, false);
            } else {
                try {
                    Condition filter = container.getOptions(Condition.class, "filter");
                    for (Iterator<KeyValue> it = container.iterator(last); it.hasNext();) {
                        KeyValue next = it.next();
                        final Scope newScope = scope.copy();
                        newScope.add(next.value, next.key);
                        if (filter != null) {
                            if (!filter.matchesWithoutExceptions(newScope.getLast())) {
                                continue;
                            }
                        }
                        final Object resolved = container.resolveValue(container, expression, newScope, false);
                        if (!container.isTrue(resolved)) {
                            return false;
                        }
                    }
                } catch (final SecurityException e) {
                    throw new ConditionException(e);
                }
            }
            return true;
        }
    }

    /**
     * @author Thomas
     * @date 07.05.2019
     *
     */
    public static class EqOp implements Operator {
        /**
         *
         */
        public static final String CASE_INSENSITIVE = "caseInsensitive";

        /**
         * See "Query Filter mode" vs. "Strict Compare mode"
         */
        @Override
        public boolean isFilterRoot() {
            return false;
        }

        /**
         *
         */
        /**
         *
         */
        public static final String DISABLE_LIST_SPECIAL_HANDLING = "disableListSpecialHandling";

        /**
         * https://docs.mongodb.com/manual/reference/operator/expression/eq/
         */
        @Override
        public Object opEval(final Condition<?> container, final Object expression, final Scope scope) throws ConditionException {
            final boolean isNEQ = false;
            return this.opEvalInternal(container, expression, scope, isNEQ);
        }

        /**
         * @param container
         * @param expression
         * @param scope
         * @param isNEQ
         * @param path
         * @return
         * @throws ConditionException
         */
        protected Object opEvalInternal(final Condition container, Object expression, final Scope scope, final boolean isNEQ) throws ConditionException {
            boolean queryMode = true;
            final Object matcherValue = scope.getLast();
            if (Boolean.TRUE.equals(container.getOptions(Boolean.class, OPTIONS_AGGREGATE))) {
                queryMode = false;
            }
            if (queryMode) {
                // { tags: { $eq: "B" } } equals
                if (container.equalsDeep(container, expression, matcherValue, scope)) {
                    if (container._isDebug()) {
                        container.log(scope.getPath(), this.getClass().getSimpleName() + " " + expression + ".equals(" + scope.getLast() + ")" + " = true " + "|Options: " + container.getOptions(Condition.class));
                    }
                    return true;
                }
                boolean disableMongoDBListSpecial = false;
                if (Boolean.TRUE.equals(container.getOptions(Boolean.class, DISABLE_LIST_SPECIAL_HANDLING))) {
                    disableMongoDBListSpecial = true;
                }
                // this is not available for $ne
                Iterable listIter = container.getListWrapper(matcherValue);
                if (listIter != null && !disableMongoDBListSpecial && !isNEQ) {
                    // Match an Array Value
                    // If the specified <value> is an array, MongoDB matches documents where the <field> matches the array exactly or
                    // the
                    // <field> contains an element that matches the array exactly. The order of the elements matters. For an example,
                    // see
                    // Equals
                    // an Array Value.
                    // the expression matches ANY of the matcherValues list entries
                    int i = 0;
                    for (final Object obj : listIter) {
                        // { tags: { $eq: "B" } } matches { tags: [ "A", "B" ] }
                        scope.add(obj, i++);
                        try {
                            if (container.equalsDeep(container, expression, obj, scope)) {
                                if (container._isDebug()) {
                                    container.log(scope.getPath(), this.getClass().getSimpleName() + " (Matcher List contains expression Mode)\"" + scope.getLast() + "\".equals(" + expression + ")" + " = true |Options: " + container.getOptions(Condition.class));
                                }
                                return true;
                            }
                        } finally {
                            scope.removeLast();
                        }
                    }
                    if (container._isDebug()) {
                        container.log(scope.getPath(), this.getClass().getSimpleName() + " (Matcher List contains expression Mode)\"" + scope.getLast() + "\".contains(" + expression + ")" + " = false |Options: " + container.getOptions(Condition.class));
                    }
                    return false;
                }
                if (container._isDebug()) {
                    container.log(scope.getPath(), this.getClass().getSimpleName() + " " + expression + ".equals(" + scope.getLast() + ")" + " = false |Options: " + container.getOptions(Condition.class));
                }
                return false;
            } else {
                /**
                 * https://docs.mongodb.com/manual/reference/operator/aggregation/eq/
                 *
                 * @throws CompareException
                 */
                ListAccessorInterface list = container.getListWrapper(expression);
                if (list != null && list.size() == 2) {
                    final Object x = (container.resolveValue(container, list.get(0), scope, true));
                    final Object y = (container.resolveValue(container, list.get(1), scope, true));
                    return new AggregationResult(CompareUtils.equals(y, x));
                }
                return new AggregationResult(false);
            }
        }
    }

    /**
     * https://docs.mongodb.com/manual/reference/operator/expression/exists/
     *
     * @author Thomas
     * @date 07.05.2019
     *
     */
    public static class ExistsOp implements Operator {
        /**
         * See "Query Filter mode" vs. "Strict Compare mode"
         */
        @Override
        public boolean isFilterRoot() {
            return false;
        }

        @Override
        public Object opEval(final Condition<?> container, Object expression, final Scope scope) throws ConditionException {
            Object matcherValue;
            ListAccessorInterface list = container.getListWrapper(expression);
            if (list != null) {
                // aggregation
                matcherValue = (container.resolveValue(container, list.get(0), scope, true));
                expression = (container.resolveValue(container, list.get(1), scope, true));
                boolean exists = container.isTrue(expression);
                // {$exists:true} == {$exists:1}
                exists |= expression instanceof Number && ((Number) expression).intValue() != 0;
                if (exists) {
                    return matcherValue != KEY_DOES_NOT_EXIST;
                } else {
                    return matcherValue == KEY_DOES_NOT_EXIST;
                }
            } else {
                expression = (container.resolveValue(container, expression, scope, true));
                boolean exists = Boolean.TRUE.equals(expression);
                // {$exists:true} == {$exists:1}
                exists |= expression instanceof Number && ((Number) expression).intValue() != 0;
                if (exists) {
                    return scope.getLast() != KEY_DOES_NOT_EXIST ? Boolean.TRUE : Boolean.FALSE;
                } else {
                    return scope.getLast() == KEY_DOES_NOT_EXIST ? Boolean.TRUE : Boolean.FALSE;
                }
            }
        }
    }

    public static class IsRegexOp implements Operator {
        /**
         * See "Query Filter mode" vs. "Strict Compare mode"
         */
        @Override
        public boolean isFilterRoot() {
            return false;
        }

        @Override
        public Object opEval(final Condition<?> container, Object expression, final Scope scope) throws ConditionException {
            if (container.getListWrapper(scope.getLast()) != null) {
                return false;
            } else {
                final Object last = container.unwrapType(scope.getLast());
                expression = (container.resolveValue(container, expression, scope, true));
                boolean isPattern = false;
                if (last instanceof Pattern) {
                    isPattern = true;
                } else if (last instanceof String) {
                    try {
                        Pattern.compile((String) last);
                        isPattern = true;
                    } catch (final Exception e) {
                        // return false;
                    }
                }
                if (container.isTrue(expression)) {
                    return isPattern;
                } else {
                    return !isPattern;
                }
            }
        }
    }

    public static class NumberOp implements Operator {
        /**
         * See "Query Filter mode" vs. "Strict Compare mode"
         */
        @Override
        public boolean isFilterRoot() {
            return false;
        }

        public static enum OP {
            LTE {
                @Override
                protected boolean opEval(final int compareResult) {
                    return compareResult <= 0;
                }
            },
            LT {
                @Override
                protected boolean opEval(final int compareResult) {
                    return (compareResult < 0);
                }
            },
            GTE {
                @Override
                protected boolean opEval(final int compareResult) {
                    return (compareResult >= 0);
                }
            },
            GT {
                @Override
                protected boolean opEval(final int compareResult) {
                    return (compareResult > 0);
                }
            };

            protected abstract boolean opEval(int compareResult);
        }

        private final NumberOp.OP op;

        public NumberOp(final NumberOp.OP op) {
            this.op = op;
        }

        @Override
        public Object opEval(final Condition<?> container, Object expression, final Scope scope) throws ConditionException {
            // final Object before = expression;
            // final Condition root = ROOT_CONDITION.get();
            ListAccessorInterface list = container.getListWrapper(expression);
            if (list != null) {
                // aggregation
                final Number a = (Number) (container.resolveValue(container, list.get(0), scope, true));
                final Number b = (Number) (container.resolveValue(container, list.get(1), scope, true));
                final boolean ret = this.op.opEval(CompareUtils.compareNumber(a, b));
                if (container._isDebug()) {
                    container.log(scope.getPath(), "%s: %s.%s(%s)" + " = %s", this.getClass().getSimpleName(), list.get(0), this.op.name(), list.get(1), ret);
                }
                return new AggregationResult(ret);
            } else {
                expression = container.resolveValue(container, expression, scope, true);
                if (KEY_DOES_NOT_EXIST == container.unwrapType(scope.getLast())) {
                    if (container._isDebug()) {
                        container.log(scope.getPath(), "%s \"%s\".%s(%s)" + " = false", this.getClass().getSimpleName(), KEY_DOES_NOT_EXIST, this.op.name(), expression);
                        ;
                    }
                    return false;
                } else if (expression instanceof Condition) {
                    DebugMode.debugger();
                    final Object b = container.resolveValue(container, expression, scope, true);
                    Integer result = container.compare(scope.getLast(), b);
                    if (result == null) {
                        throw new ConditionException("Unsupported expression: " + container.toLog(b) + " on " + container.toLog(scope.getLast()));
                    }
                    final boolean ret = this.op.opEval(result);
                    if (container._isDebug()) {
                        container.log(scope.getPath(), "%s: %s.%s(%s)" + " = %s", this.getClass().getSimpleName(), scope.getLast(), this.op.name(), b, ret);
                        ;
                    }
                    return ret;
                } else {
                    try {
                        Integer result = container.compare(scope.getLast(), expression);
                        if (result == null) {
                            throw new ConditionException("Unsupported expression: " + container.toLog(expression) + " on " + container.toLog(scope.getLast()));
                        }
                        final boolean ret = this.op.opEval(result.intValue());
                        if (container._isDebug()) {
                            container.log(scope.getPath(), "%s: %s.%s(%s)" + " = %s", this.getClass().getSimpleName(), scope.getLast(), this.op.name(), expression, ret);
                        }
                        return ret;
                    } catch (ClassCastException e) {
                        throw new ConditionException("Unsupported expression: " + container.toLog(expression) + " on " + container.toLog(scope.getLast()));
                    }
                }
            }
        }
    }

    /**
     * @author Thomas
     * @date 07.05.2019
     *
     */
    public static class InOp implements Operator {
        /**
         * See "Query Filter mode" vs. "Strict Compare mode"
         */
        @Override
        public boolean isFilterRoot() {
            return false;
        }

        @Override
        public Object opEval(final Condition<?> container, Object expression, final Scope scope) throws ConditionException {
            // final Object matcherValue = scope.getLast();
            Object mappedMatcher = container.unwrapType(scope.getLast());
            expression = container.resolveValue(container, expression, scope, true);
            if (mappedMatcher == KEY_DOES_NOT_EXIST) {
                if (container._isDebug()) {
                    container.log(scope.getPath(), "%s: KEY_DOES_NOT_EXIST -> false", this.getClass().getSimpleName());
                }
                return false;
            }
            Iterable<Object> itExpression = container.getListWrapper(expression);
            if (itExpression == null) {
                throw new ConditionException("Operator expects an array as parameter");
            }
            Iterable<Object> itScope = container.getListWrapper(scope.getLast());
            if (itScope == null) {
                // Use the $in Operator to Match Values
                for (final Object exp : itExpression) {
                    if (container.equalsDeep(container, exp, scope.getLast(), scope)) {
                        if (container._isDebug()) {
                            container.log(scope.getPath(), "%s: 'does matcher match any expression-list-element-mode':  %s.matches(%s) = true", this.getClass().getSimpleName(), scope.getLast(), exp);
                        }
                        return true;
                    }
                }
                if (container._isDebug()) {
                    container.log(scope.getPath(), "%s: 'does matcher match any expression-list-element-mode':  %s does not match any of %s", this.getClass().getSimpleName(), scope.getLast(), expression);
                }
                return false;
            } else {
                for (final Object exp : itExpression) {
                    for (final Object matcher : itScope) {
                        if (container.equalsDeep(container, exp, matcher, scope)) {
                            if (container._isDebug()) {
                                container.log(scope.getPath(), "%s: 'does any matcher-list-element match any expression-list-element-mode':  %s.matches(%s) = true", this.getClass().getSimpleName(), matcher, exp);
                            }
                            return true;
                        }
                    }
                }
                if (container._isDebug()) {
                    container.log(scope.getPath(), "%s: 'oes any matcher-list-element match any expression-list-element-mode':  %s has no element that matches any element of %s", this.getClass().getSimpleName(), scope.getLast(), expression);
                }
                return false;
            }
        }
    }

    public static class MaxOp implements Operator {
        /**
         * See "Query Filter mode" vs. "Strict Compare mode"
         */
        @Override
        public boolean isFilterRoot() {
            return false;
        }

        @Override
        public Object opEval(final Condition<?> container, final Object expression, final Scope scope) throws ConditionException {
            if (expression instanceof Number) {
                return expression;
            }
            Number result = null;
            {
                Iterable<Object> it = container.getListWrapper(expression);
                if (it != null) {
                    int count = 0;
                    boolean singleListMode = false;
                    for (Object value : it) {
                        /**
                         * If some, but not all, documents for the $max operation have either a null value for the field or are missing the
                         * field, the $max operator only considers the non-null and the non-missing values for the field.
                         */
                        Number num = null;
                        /**
                         * With a single expression as its operand, if the expression resolves to an array, $max traverses into the array to
                         * operate on the numerical elements of the array to return a single value. With a list of expressions as its
                         * operand, if any of the expressions resolves to an array, $max does not traverse into the array but instead treats
                         * the array as a non-numerical value
                         */
                        if (singleListMode && count > 0) {
                            // reset because we have more than 1 entry
                            result = 0;
                            singleListMode = false;
                        }
                        ListAccessorInterface list = container.getListWrapper(value);
                        if (list != null && count == 0) {
                            // num = aggregate(container, value, matcherValue);
                            num = (Number) this.opEval(container, value, scope);
                            singleListMode = true;
                        } else if (list != null && count == 0) {
                            num = (Number) this.opEval(container, value, scope);
                            singleListMode = true;
                        } else {
                            value = (container.resolveValue(container, value, scope, true));
                            // value = getNumber(container, value, obj, false);
                            if (value instanceof Number) {
                                num = (Number) value;
                            }
                        }
                        if (result == null) {
                            result = num;
                        } else if (Clazz.isDouble(result.getClass()) || Clazz.isDouble(num.getClass())) {
                            result = Math.max(result.doubleValue(), num.doubleValue());
                        } else if (Clazz.isFloat(result.getClass()) || Clazz.isFloat(num.getClass())) {
                            result = Math.max(result.floatValue(), num.floatValue());
                        } else {
                            result = Math.max(result.longValue(), num.longValue());
                        }
                    }
                    count++;
                }
            }
            /**
             * If all documents for the $max operation have null value for the field or are missing the field, the $max operator returns
             * null for the minimum value
             */
            // max does not evaluate against its scope - > return result als Aggregation Result
            return new AggregationResult(result);
        }
    }

    public static class MinOp implements Operator {
        /**
         * See "Query Filter mode" vs. "Strict Compare mode"
         */
        @Override
        public boolean isFilterRoot() {
            return false;
        }

        @Override
        public Object opEval(final Condition<?> container, final Object expression, final Scope scope) throws ConditionException {
            if (expression instanceof Number) {
                return expression;
            }
            Number result = null;
            {
                Iterable<Object> it = container.getListWrapper(expression);
                if (it != null) {
                    int count = 0;
                    boolean singleListMode = false;
                    for (Object value : it) {
                        /**
                         * If some, but not all, documents for the $max operation have either a null value for the field or are missing the
                         * field, the $max operator only considers the non-null and the non-missing values for the field.
                         */
                        Number num = null;
                        try {
                            /**
                             * With a single expression as its operand, if the expression resolves to an array, $max traverses into the
                             * array to operate on the numerical elements of the array to return a single value. With a list of expressions
                             * as its operand, if any of the expressions resolves to an array, $max does not traverse into the array but
                             * instead treats the array as a non-numerical value
                             */
                            if (singleListMode && count > 0) {
                                // reset because we have more than 1 entry
                                result = 0;
                                singleListMode = false;
                            }
                            if (container.getListWrapper(value) != null && count == 0) {
                                num = (Number) this.opEval(container, value, scope);
                                singleListMode = true;
                            } else {
                                value = container.resolveValue(container, value, scope, true);
                                // value = getNumber(container, value, obj, false);
                                if (value instanceof Number) {
                                    num = (Number) value;
                                }
                            }
                        } catch (final AggregationException e) {
                            continue;
                        }
                        if (result == null) {
                            result = num;
                        } else if (Clazz.isDouble(result.getClass()) || Clazz.isDouble(num.getClass())) {
                            result = Math.min(result.doubleValue(), num.doubleValue());
                        } else if (Clazz.isFloat(result.getClass()) || Clazz.isFloat(num.getClass())) {
                            result = Math.min(result.floatValue(), num.floatValue());
                        } else {
                            result = Math.min(result.longValue(), num.longValue());
                        }
                    }
                    count++;
                }
            }
            /**
             * If all documents for the $min operation have null value for the field or are missing the field, the $min operator returns
             * null for the minimum value
             */
            // only aggregation mode - there is no query mode
            return new AggregationResult(result);
        }
    }

    public static class MultiplyOp implements Operator {
        /**
         * See "Query Filter mode" vs. "Strict Compare mode"
         */
        @Override
        public boolean isFilterRoot() {
            return false;
        }

        @Override
        public Object opEval(final Condition<?> container, final Object expressions, final Scope scope) throws ConditionException {
            Iterable<Object> it = container.getListWrapper(expressions);
            if (it != null) {
                Number result = null;
                for (final Object expression : it) {
                    Object ret = container.unwrapType(container.resolveValue(container, expression, scope, true));
                    if (!(ret instanceof Number)) {
                        throw new AggregationException("MultiplyOp:" + (container) + "|" + (expression) + " does not resolve to a number but " + ret);
                    }
                    final Number num = (Number) ret;
                    if (result == null) {
                        result = num;
                    } else if (Clazz.isDouble(result.getClass()) || Clazz.isDouble(num.getClass())) {
                        result = result.doubleValue() * num.doubleValue();
                    } else if (Clazz.isFloat(result.getClass()) || Clazz.isFloat(num.getClass())) {
                        result = result.floatValue() * num.floatValue();
                    } else {
                        result = result.longValue() * num.longValue();
                    }
                }
                // only aggregation mode - there is no query mode
                return new AggregationResult(result);
            }
            throw new AggregationException("MultiplyOp:" + (container) + "|" + (expressions));
        }
    }

    /**
     * @author Thomas
     * @date 07.05.2019 https://docs.mongodb.com/manual/reference/operator/query/nin/
     */
    public static class NinOp implements Operator {
        /**
         * See "Query Filter mode" vs. "Strict Compare mode"
         */
        @Override
        public boolean isFilterRoot() {
            return true;
        }

        @Override
        public Object opEval(Condition<?> container, Object expression, Scope scope) throws ConditionException {
            // the field does not exist.
            final Object matcherValue = scope.getLast();
            if (container.unwrapType(scope.getLast()) == KEY_DOES_NOT_EXIST) {
                return true;
            }
            Iterable<Object> expressionIt = container.getListWrapper(expression);
            if (expressionIt == null) {
                throw new ConditionException("Operator expects an array as parameter");
            }
            Iterable<Object> matcherValueIt = container.getListWrapper(matcherValue);
            if (matcherValueIt == null) {
                // value is not a list
                for (final Object exp : expressionIt) {
                    if (container.equalsDeep(container, exp, matcherValue, scope)) {
                        return false;
                    }
                }
                return true;
            } else {
                for (final Object exp : expressionIt) {
                    for (Object matcher : matcherValueIt) {
                        if (container.equalsDeep(container, exp, matcher, scope)) {
                            return false;
                        }
                    }
                }
                return true;
            }
        }
    }

    /**
     * @author Thomas
     * @date 07.05.2019
     */
    public static class NotEqualsOp implements Operator {
        /**
         * See "Query Filter mode" vs. "Strict Compare mode"
         */
        @Override
        public boolean isFilterRoot() {
            return false;
        }

        @Override
        public Object opEval(final Condition<?> container, final Object expression, final Scope scope) throws ConditionException {
            if (scope.getLast() == KEY_DOES_NOT_EXIST) {
                // KEY_DOES_NOT_EXIST can never match anything. It does not match null neither!
                return true;
            } else {
                final Object result = EQOP.opEvalInternal(container, expression, scope, true);
                // inverted
                return result == Boolean.TRUE ? false : true;
            }
        }
    }

    /**
     * @author Thomas
     * @date 07.05.2019
     *
     */
    public static class NotOp implements Operator {
        /**
         * See "Query Filter mode" vs. "Strict Compare mode"
         */
        @Override
        public boolean isFilterRoot() {
            return false;
        }

        @Override
        public Object opEval(final Condition<?> container, final Object expression, final Scope scope) throws ConditionException {
            return !container.equalsDeep(container, expression, scope.getLast(), scope);
        }
    }

    public static interface OpHandler extends Operator {
    }

    /**
     * @author Thomas
     * @date 07.05.2019
     *
     */
    public static class OrOp implements Operator {
        /**
         * See "Query Filter mode" vs. "Strict Compare mode"
         */
        @Override
        public boolean isFilterRoot() {
            return true;
        }

        @Override
        public Object opEval(final Condition<?> container, final Object expression, final Scope scope) throws ConditionException {
            for (final Object exp : container.getListWrapper(expression)) {
                final Object resolved = container.resolveValue(container, exp, scope, false);
                if (container.isTrue(resolved)) {
                    return true;
                }
            }
            return false;
        }
    }

    public static interface PathHandler {
        /**
         * @param scope
         * @param ret
         * @param value
         * @param key
         * @return
         * @throws CannotGetValueException
         */
        Scope resolve(Scope oldScope, Scope newScope, Object key) throws CannotGetValueException;
    }

    public static class RegexFindOp implements Operator {
        /**
         * See "Query Filter mode" vs. "Strict Compare mode"
         */
        @Override
        public boolean isFilterRoot() {
            return false;
        }

        @Override
        public Object opEval(final Condition<?> container, final Object expression, final Scope scope) throws ConditionException {
            try {
                ListAccessorInterface list = container.getListWrapper(expression);
                if (list != null) {
                    // container.resolveValue(expression, scope)
                    // }
                    final Object matcherObject = container.resolveValue(container, list.get(0), scope, true);
                    if (matcherObject == KEY_DOES_NOT_EXIST) {
                        return null;
                    }
                    Pattern pattern = null;
                    if (pattern == null) {
                        final Object patternString = container.resolveValue(container, list.get(1), scope, true);
                        pattern = Pattern.compile(String.valueOf(patternString));
                    }
                    int groupIndex = 1;
                    if (list.size() > 2) {
                        final Object obj = container.resolveValue(container, list.get(2), scope, true);
                        groupIndex = Integer.parseInt(String.valueOf(obj));
                    }
                    final Matcher matcher = pattern.matcher(toCharSequence(matcherObject));
                    if (matcher.find()) {
                        return matcher.group(groupIndex);
                    } else {
                        return null;
                    }
                }
            } catch (final ConditionException e) {
                throw new AggregationException(e);
            }
            throw new AggregationException();
        }
    }

    public static class IfOp implements Operator {
        /**
         * See "Query Filter mode" vs. "Strict Compare mode"
         */
        @Override
        public boolean isFilterRoot() {
            return true;
        }

        @Override
        public Object opEval(final Condition<?> container, final Object expression, final Scope scope) throws ConditionException {
            try {
                ListAccessorInterface list = container.getListWrapper(expression);
                if (list != null) {
                    final Object ifCondition = container.resolveValue(container, list.get(0), scope, true);
                    if (ifCondition == KEY_DOES_NOT_EXIST) {
                        return false;
                    }
                    if (container.isTrue(ifCondition)) {
                        return container.resolveValue(container, list.get(1), scope, true);
                    } else {
                        return container.resolveValue(container, list.get(2), scope, true);
                    }

                }
            } catch (final ConditionException e) {
                throw new AggregationException(e);
            }
            throw new AggregationException();
        }
    }

    /**
     * @author Thomas
     * @date 07.05.2019
     *
     */
    public static class RegexOp implements Operator {
        /**
         * See "Query Filter mode" vs. "Strict Compare mode"
         */
        @Override
        public boolean isFilterRoot() {
            return false;
        }

        /**
         * @param container
         * @return
         * @throws ConditionException
         */
        private int getFlags(final Condition<Object> container) throws ConditionException {
            int flags = 0;
            Object options = container.getOptions(Object.class);
            String stringFlags = null;
            if (options == null) {
                return flags;
            }
            if (options instanceof String) {
                stringFlags = (String) options;
            } else {
                stringFlags = (String) ((Map) options).get("flags");
            }
            if (stringFlags == null) {
                return flags;
            }
            for (int i = 0; i < stringFlags.length(); i++) {
                switch (stringFlags.charAt(i)) {
                case 'i':
                    flags |= Pattern.CASE_INSENSITIVE;
                    break;
                case 'm':
                    flags |= Pattern.MULTILINE;
                    break;
                case 's':
                    flags |= Pattern.DOTALL;
                    break;
                default:
                    throw new ConditionException("Unsupported Regex Option");
                }
            }
            return flags;
        }

        protected boolean matchPattern(final Condition<?> container, final Object matcherValue, final Pattern pattern) {
            // no resolve, because matcherValue is from the matcherObject and does not get resolved/evaluated
            if (matcherValue == null) {
                return false;
            } else {
                final Matcher matcher = pattern.matcher("");
                final Iterable<Object> matcherValues = container.getListWrapper(matcherValue);
                if (matcherValues != null) {
                    // the expression matches ANY of the matcherValues list entries
                    for (final Object matcherObject : matcherValues) {
                        if (matcherObject != null && matcher.reset(toCharSequence(container.unwrapType(matcherObject))).matches()) {
                            return true;
                        }
                    }
                }
                if (matcher.reset(toCharSequence(container.unwrapType(matcherValue))).matches()) {
                    return true;
                }
                return false;
            }
        }

        /**
         *
         */
        @Override
        public Object opEval(final Condition<?> container, Object expression, final Scope scope) throws ConditionException {
            final Object matcherValue = scope.getLast();
            if (matcherValue == KEY_DOES_NOT_EXIST) {
                return false;
            } else {
                try {
                    expression = container.resolveValue(container, expression, scope, true);
                    Pattern pattern = null;
                    ListAccessorInterface list;
                    if ((list = container.getListWrapper(expression)) != null) {
                        // aggregation mode
                        if (pattern == null) {
                            pattern = Pattern.compile(String.valueOf(container.resolveValue(container, list.get(0), scope, true)), this.getFlags((Condition<Object>) container));
                        }
                        return this.matchPattern(container, container.resolveValue(container, list.get(1), scope, true), pattern);
                    } else if (expression instanceof Pattern) {
                        pattern = (Pattern) expression;
                        return this.matchPattern(container, matcherValue, pattern);
                    } else if (expression instanceof String) {
                        if (pattern == null) {
                            expression = container.resolveValue(container, expression, scope, true);
                            pattern = Pattern.compile((String) expression, this.getFlags((Condition<Object>) container));
                        }
                        return this.matchPattern(container, matcherValue, pattern);
                    }
                } catch (final PatternSyntaxException e) {
                    throw new ConditionException(e);
                } catch (final IllegalArgumentException e) {
                    throw new ConditionException(e);
                }
                throw new ConditionException("Operator expects a String or a Pattern(is not serializable) type as parameter");
            }
        }
    }

    /**
     * // Resolver convert types to a "Wrapper" Type, (FileContext?) the original instance must be removed from the scope hirarchy, //
     * because $parent would not work
     *
     * @author thomas
     * @date 13.07.2022
     *
     * @param <Input>
     * @param <Output>
     */
    public static class ResolverOPHandler<Input, Output> implements OpHandler {
        /**
         *
         */
        private final String                           key;
        /**
         *
         */
        private final ConditionResolver<Input, Output> resolver;

        /**
         * @param key
         * @param resolver
         */
        public ResolverOPHandler(final String key, final ConditionResolver<Input, Output> resolver) {
            this.key = key;
            this.resolver = resolver;
        }

        @Override
        public Object opEval(final Condition<?> container, final Object query, final Scope matcher) throws ConditionException {
            if (query instanceof Condition) {
                final Condition<?> cQuery = (Condition) query;
                @SuppressWarnings("unchecked")
                Condition options = cQuery.getOptions(Condition.class);
                if (options == null) {
                    throw new ConditionException("§options property is missing. Add §options." + this.key.substring(1));
                }
                final Input parameters = (Input) options.get(this.key.substring(1));
                if (parameters == null) {
                    throw new ConditionException("§options." + this.key.substring(1) + " property is missing");
                }
                final Output resolved = this.resolver.resolve(parameters);
                final ArrayList<Object> newScopeObjects = new ArrayList<Object>(matcher.getScope());
                // Resolver convert types to a "Wrapper" Type, (FileContext?) the original instance must be removed from the scope hirarchy,
                // because $parent would not work
                newScopeObjects.set(newScopeObjects.size() - 1, resolved);
                return container.resolveValue(container, cQuery, new Scope(newScopeObjects, matcher.getPath()), false);
            }
            throw new ConditionException();
        }

        /*
         * (non-Javadoc)
         *
         * @see org.appwork.moncompare.Operator#isFilterRoot()
         */
        @Override
        public boolean isFilterRoot() {
            return false;
        }
    }

    public static class SubtractOp implements Operator {
        /**
         * See "Query Filter mode" vs. "Strict Compare mode"
         */
        @Override
        public boolean isFilterRoot() {
            return false;
        }

        @Override
        public Object opEval(final Condition<?> container, final Object expressions, final Scope scope) throws ConditionException {
            ListAccessorInterface list;
            if ((list = container.getListWrapper(expressions)) != null) {
                Number result = null;
                for (final Object expression : list) {
                    final Object ret = container.unwrapType(container.resolveValue(container, expression, scope, true));
                    if (!(ret instanceof Number)) {
                        throw new AggregationException("SubtractOp:" + (container) + "|" + (expression) + " does not resolve to a number but " + ret);
                    }
                    final Number num = (Number) ret;
                    if (result == null) {
                        result = num;
                    } else if (Clazz.isDouble(result.getClass()) || Clazz.isDouble(num.getClass())) {
                        result = result.doubleValue() - num.doubleValue();
                    } else if (Clazz.isFloat(result.getClass()) || Clazz.isFloat(num.getClass())) {
                        result = result.floatValue() - num.floatValue();
                    } else {
                        result = result.longValue() - num.longValue();
                    }
                }
                // no scope evaluation here
                return new AggregationResult(result);
            }
            throw new AggregationException("SubtractOp:" + (container) + "|" + (expressions));
        }
    }

    public static class SumOp implements Operator {
        /**
         * See "Query Filter mode" vs. "Strict Compare mode"
         */
        @Override
        public boolean isFilterRoot() {
            return false;
        }

        @Override
        public Object opEval(final Condition<?> container, final Object expression, final Scope scope) throws ConditionException {
            if (container.unwrapType(expression) instanceof Number) {
                return expression;
            }
            final Iterable<Object> it = container.getListWrapper(expression);
            if (it != null) {
                Number result = null;
                int count = 0;
                boolean singleListMode = false;
                for (Object value : it) {
                    /**
                     * If used on a field that does not exist in any document in the collection, $sum returns 0 for that field.
                     */
                    Number num = 0;
                    /**
                     * With a single expression as its operand, if the expression resolves to an array, $sum traverses into the array to
                     * operate on the numerical elements of the array to return a single value. With a list of expressions as its operand,
                     * if any of the expressions resolves to an array, $sum does not traverse into the array but instead treats the array as
                     * a non-numerical value
                     */
                    if (singleListMode && count > 0) {
                        // reset because we have more than 1 entry
                        result = 0;
                        singleListMode = false;
                    }
                    if (container.getListWrapper(value) != null && count == 0) {
                        // num = aggregate(container, value, matcherValue);
                        num = (Number) this.opEval(container, value, scope);
                        singleListMode = true;
                    } else {
                        value = container.unwrapType(container.resolveValue(container, value, scope, true));
                        // value = getNumber(container, value, matcherValue, false);
                        if (value instanceof Number) {
                            num = (Number) value;
                        }
                    }
                    if (result == null) {
                        result = num;
                    } else if (Clazz.isDouble(result.getClass()) || Clazz.isDouble(num.getClass())) {
                        result = result.doubleValue() + num.doubleValue();
                    } else if (Clazz.isFloat(result.getClass()) || Clazz.isFloat(num.getClass())) {
                        result = result.floatValue() + num.floatValue();
                    } else {
                        result = result.longValue() + num.longValue();
                    }
                    count++;
                }
                return result;
            }
            /**
             * If all operands are non-numeric, $sum returns 0.
             */
            return 0;
        }
    }

    /**
     * References the start of the field path being processed in the aggregation pipeline stage. Unless documented otherwise, all stages
     * start with CURRENT the same as ROOT.
     *
     * CURRENT is modifiable. However, since $<field> is equivalent to $$CURRENT.<field>, rebinding CURRENT changes the meaning of $
     * accesses.
     */
    public static final String                              $$CURRENT                = "§§CURRENT";
    @StorableDoc("Path Modifier: References the root document, i.e. the top-level document.")
    public static final String                              $$ROOT                   = "§§ROOT";
    @ApiDoc("Path Modifier:  §§THIS references the current scope object or field. Note: All field identifiers that start with §§ are absolute identifiers and MUST be places as first key element")
    @ApiDocExample("{'c':{'§gt':[{'§sum':['§§this',1]},0}}  '§§this' references to the field c, adds 1 and checks if the result is greater than 0")
    public static final String                              $$THIS                   = "§§THIS";
    public static final String                              $AND                     = "§and";
    /**
     *
     */
    public static final String                              $ANY                     = "§any";
    @ApiDoc("Aggregation OP: concat all values to a single string")
    public static final String                              $CONCAT                  = "§concat";
    public static final String                              $TO_UPPER_CASE           = "§toUpperCase";
    public static final String                              $GET                     = "§get";
    public static final String                              $TO_LOWER_CASE           = "§toLowerCase";
    public static final String                              $SEARCH_AND_REPLACE      = "§searchAndReplace";
    public static final String                              $DIVIDE                  = "§divide";
    public static final String                              $EACH                    = "§each";
    public static final String                              $IF                      = "§if";
    /**
     * Specifies equality condition. The $eq operator matches documents where the value of a field equals the specified value. WARNING:
     * There is a difference to MongoDb $EQ: If the specified <value> is a document, the order of the fields in the document DOES NOT
     * MATTER! <br>
     * Match an Array Value If the specified <value> is an array, MongoDB matches documents where the <field> matches the array exactly or
     * the <field> contains an element that matches the array exactly. The order of the elements matters. For an example, see Equals an
     * Array Value.
     */
    public static final String                              $EQ                      = "§eq";
    /**
     *
     */
    public static final String                              $EXISTS                  = "§exists";
    public static final String                              $IS_REGEX                = "§isRegex";
    /**
     * $gt selects those documents where the value of the field is greater than (i.e. >) the specified value.
     */
    public static final String                              $GT                      = "§gt";
    /**
     * $gte selects the documents where the value of the field is greater than or equal to (i.e. >=) a specified value (e.g. value.)
     */
    public static final String                              $GTE                     = "§gte";
    /**
     * The $in operator selects the documents where the value of a field equals any value in the specified array. To specify an $in
     * expression, use the following prototype:
     *
     * If the field holds an array, then the $in operator selects the documents whose field holds an array that contains at least one
     * element that matches a value in the specified array (e.g. <value1>, <value2>, etc.)
     *
     *
     */
    public static final String                              $IN                      = "§in";
    /**
     *
     */
    @ApiDoc("Path modifier. a.§keys returns all keys of the map a or all indizes of the list a")
    @ApiDocExample("a.§keys")
    public static final String                              $KEYS                    = "§keys";
    /**
     *
     */
    public static final String                              $LT                      = "§lt";
    /**
     *
     */
    public static final String                              $LTE                     = "§lte";
    public static final String                              $MAX                     = "§max";
    public static final String                              $MIN                     = "§min";
    public static final String                              $MULTIPLY                = "§multiply";
    /**
     * $ne selects the documents where the value of the field is not equal to the specified value. This includes documents that do not
     * contain the field.
     */
    public static final String                              $NE                      = "§ne";
    /**
     *
     */
    public static final String                              $NIN                     = "§nin";
    /**
     *
     */
    public static final String                              $NOT                     = "§not";
    /**
     * Returns the current datetime value, which is same across all members of the deployment and remains constant throughout the
     * aggregation pipeline.
     */
    public static final String                              $NOW                     = "§NOW";
    /**
     * Regex options https://docs.mongodb.com/manual/reference/operator/expression/regex/#op._S_options
     */
    public static final String                              $OPTIONS                 = "§options";
    /**
     *
     */
    public static final String                              $OR                      = "§or";
    /**
     *
     */
    @ApiDoc("Path traversal identifier. Used to access parent fields in the matcher object - relative to the current scope. Usage is like ../../ in directory pathes")
    public static final String                              $PARENT                  = "§PARENT";
    public static final String                              $REGEX                   = "§regex";
    @ApiDoc("Aggregation OP: find a match in a string via regex\r\nParam 1:string\r\nParam 2:regex\r\nParam 3: matching group index(optional)")
    public static final String                              $REGEX_FIND_ONE          = "§regexFindOne";
    @ApiDoc("Virtual field identifier. a.§size references length of an array, string, map or other objects")
    @ApiDocExample("a.§size")
    public static final String                              $SIZE                    = "§size";
    public static final String                              $SUBTRACT                = "§subtract";
    public static final String                              $SUM                     = "§sum";
    /**
     *
     */
    @ApiDoc("Virtual field identifier. a.§type references the ClassName of 'a'")
    @ApiDocExample("a.§type")
    public static final String                              $TYPE                    = "§type";
    private static final Class[]                            EMPTY                    = new Class[] {};
    protected static final EqOp                             EQOP                     = new EqOp();
    private static HashSet<String>                          IGNORE                   = new HashSet<String>();
    public static final Object                              KEY_DOES_NOT_EXIST       = new Object() {
                                                                                         @Override
                                                                                         public String toString() {
                                                                                             return "KEY_DOES_NOT_EXIST";
                                                                                         }
                                                                                     };
    private final static ThreadLocal<Long>                  now                      = new ThreadLocal<Long>();
    private final static ThreadLocal<Condition>             ROOT_CONDITION           = new ThreadLocal<Condition>();
    public static final ThreadLocal<Map<String, OpHandler>> OPERATIONS               = new ThreadLocal<Map<String, OpHandler>>();
    private static final HashMap<String, Operator>          OPS                      = new HashMap<String, Operator>();
    /**
     * used as $options to force a Condition get handled as Aggregate COndition
     */
    @ApiDoc("Set this option to true to force all operators in the same layer to work in aggregation mode.")
    @ApiDocExample("{§eq:['§a',1],§options:{'aggregate':true}}")
    public static final String                              OPTIONS_AGGREGATE        = "aggregate";

    public static final String                              OPTIONS_ALLOW_REFERENCES = "allowReferences";

    protected List<TypeHandler>                             typeHandlers             = new ArrayList<TypeHandler>();

    protected List<TypeHandler> getTypeHandler() {
        return typeHandlers;
    }

    public void setTypeHandler(List<TypeHandler> typeHandlers) {
        this.typeHandlers = typeHandlers;
    }

    public static class KeyValue {
        public final Object  value;
        public final Object  key;
        public final boolean validKey;

        public KeyValue(Object key, Object value, boolean validKey) {
            this.key = key;
            this.value = value;
            this.validKey = validKey;
        }
    }

    /**
     * @param last
     * @return
     */
    public Iterator<KeyValue> iterator(final Object obj) {
        final ListAccessorInterface listWrapper;
        final MapAccessorInterface mapWrapper;
        if (obj == null || Clazz.isPrimitive(obj.getClass()) || obj instanceof String) {
            return null;
        } else if (obj instanceof ConditionObjectValueView) {
            final ConditionObjectValueView view = (ConditionObjectValueView) obj;
            return view.iterator();
        } else if ((mapWrapper = getMapWrapper(obj)) != null) {
            return new Iterator<KeyValue>() {
                final Iterator<Map.Entry<String, Object>> it = mapWrapper.iterator();

                @Override
                public boolean hasNext() {
                    return it.hasNext();
                }

                @Override
                public KeyValue next() {
                    final Map.Entry<String, Object> nextKey = it.next();
                    return new KeyValue(nextKey.getKey(), nextKey.getValue(), true);
                }
            };
        } else if ((listWrapper = getListWrapper(obj)) != null) {
            return new Iterator<KeyValue>() {
                final boolean          validKeys = listWrapper instanceof ArrayAccessor || listWrapper instanceof ListAccessor;
                final Iterator<Object> it        = listWrapper.iterator();
                private int            index     = 0;

                @Override
                public boolean hasNext() {
                    return it.hasNext();
                }

                @Override
                public KeyValue next() {
                    final Object value = it.next();
                    return new KeyValue(index++, value, validKeys);
                }
            };
        } else {
            try {
                final ClassCache cc = ClassCache.getClassCache(obj.getClass());
                return new Iterator<KeyValue>() {
                    final Iterator<String> it = cc.getKeys().iterator();

                    @Override
                    public boolean hasNext() {
                        return it.hasNext();
                    }

                    @Override
                    public KeyValue next() {
                        try {
                            final String nextKey = it.next();
                            return new KeyValue(nextKey, cc.getGetter(nextKey).getValue(obj), true);
                        } catch (IllegalArgumentException e) {
                            throw new WTFException(e);
                        } catch (IllegalAccessException e) {
                            throw new WTFException(e);
                        } catch (InvocationTargetException e) {
                            throw new WTFException(e);
                        }
                    }
                };
            } catch (IllegalArgumentException e) {
                throw new WTFException(e);
            } catch (SecurityException e) {
                throw new WTFException(e);
            } catch (NoSuchMethodException e) {
                throw new WTFException(e);
            }
        }
    }

    /**
     * @param last
     * @return
     */
    public boolean isValue(Object last) {
        if (last == null || last == KEY_DOES_NOT_EXIST || Clazz.isPrimitive(last.getClass()) || last instanceof String || Clazz.isEnum(last.getClass())) {
            return true;
        }
        Object unwrapped = unwrap(last);
        if (unwrapped != null) {
            if (unwrapped == null || last == KEY_DOES_NOT_EXIST || Clazz.isPrimitive(unwrapped.getClass()) || unwrapped instanceof String || Clazz.isEnum(unwrapped.getClass())) {
                return true;
            }
        }
        return false;
    }

    public static final ThreadLocal<List<TypeHandler>> TYPE_HANDLERS        = new ThreadLocal<List<TypeHandler>>();
    public static final List<TypeHandler>              GLOBAL_TYPE_HANDLERS = new ArrayList<TypeHandler>();
    static {
        GLOBAL_TYPE_HANDLERS.add(new TimeSpanHandler());
        GLOBAL_TYPE_HANDLERS.add(new DateHandler());
    }
    public static final ThreadLocal<Map<String, PathHandler>>  PATH_HANDLERS    = new ThreadLocal<Map<String, PathHandler>>();
    /**
     *
     */
    private static final long                                  serialVersionUID = 1L;
    public static final org.appwork.storage.TypeRef<Condition> TYPE             = new org.appwork.storage.TypeRef<Condition>(Condition.class) {
                                                                                };
    static {
        IGNORE.add($OPTIONS);
    }
    static {
        OPS.put($GTE, new NumberOp(NumberOp.OP.GTE));
        OPS.put($GT, new NumberOp(NumberOp.OP.GT));
        OPS.put($LTE, new NumberOp(NumberOp.OP.LTE));
        OPS.put($LT, new NumberOp(NumberOp.OP.LT));
        OPS.put($EQ, EQOP);
        // https://docs.mongodb.com/manual/reference/operator/expression/in/
        OPS.put($IN, new InOp());
        OPS.put($EXISTS, new ExistsOp());
        OPS.put($IS_REGEX, new IsRegexOp());
        OPS.put($NE, new NotEqualsOp());
        OPS.put($NIN, new NinOp());
        OPS.put($REGEX, new RegexOp());
        OPS.put($OR, new OrOp());
        OPS.put($AND, new AndOp());
        OPS.put($EACH, new EachOp());
        OPS.put($PROJECT, new ProjectOp());
        OPS.put($ANY, new AnyOp());
        OPS.put($NOT, new NotOp());
        // aggregration
        OPS.put($SUBTRACT, new SubtractOp());
        OPS.put($SUM, new SumOp());
        OPS.put($MIN, new MinOp());
        OPS.put($MAX, new MaxOp());
        OPS.put($DIVIDE, new DivideOp());
        OPS.put($MULTIPLY, new MultiplyOp());
        OPS.put($CONCAT, new ConcatOp());
        OPS.put($TO_UPPER_CASE, new ToUpperCaseOp());
        OPS.put($TO_LOWER_CASE, new ToLowerCaseOp());
        OPS.put($GET, new GetOp());
        OPS.put($SEARCH_AND_REPLACE, new SearchAndReplaceOp());
        OPS.put($REGEX_FIND_ONE, new RegexFindOp());
        OPS.put($IF, new IfOp());
    }

    public static OpHandler putThreadOPHandler(final String key, final OpHandler handler) {
        Map<String, OpHandler> map = Condition.OPERATIONS.get();
        if (map == null) {
            map = new HashMap<String, Condition.OpHandler>();
            Condition.OPERATIONS.set(map);
        }
        return map.put(key, handler);
    }

    /**
     * @param result
     * @return
     */
    public Object createMap(HashMap<Object, Object> result) {
        return result;
    }

    /**
     * @param result
     * @return
     */
    public Object createList(LinkedList<Object> result) {
        return result;
    }

    /**
     * @param matcherValue
     * @param b
     * @return
     */
    public Integer compare(Object a, Object b) {
        for (final TypeHandler e : getTypeHandlerIterable()) {
            final Integer ret = e.compare(a, b);
            if (ret != null) {
                return ret.intValue();
            }
        }
        return CompareUtils.tryToCompare(a, b);
    }

    public ListAccessorInterface getListWrapper(final Object expression) {
        if (expression == null) {
            return null;
        } else if (expression instanceof ListAccessorInterface) {
            return (ListAccessorInterface) expression;
        }
        for (final TypeHandler e : getTypeHandlerIterable()) {
            final ListAccessorInterface ret = e.getListAccessor(expression);
            if (ret != null) {
                return ret;
            }
        }
        if (expression.getClass().isArray()) {
            return new ArrayAccessor<MatcherType>(expression);
        } else if (expression instanceof List) {
            return new ListAccessor((List<Object>) expression);
        } else if (expression instanceof Collection) {
            return new CollectionAccessor(((Collection<Object>) expression));
        } else {
            return null;
        }
    }

    public MapAccessorInterface getMapWrapper(final Object expression) {
        if (expression == null) {
            return null;
        } else if (expression instanceof MapAccessorInterface) {
            return (MapAccessorInterface) expression;
        }
        for (final TypeHandler e : getTypeHandlerIterable()) {
            final MapAccessorInterface ret = e.getMapAccessor(expression);
            if (ret != null) {
                return ret;
            }
        }
        if (expression instanceof Map) {
            return new MapAccessor((Map<String, Object>) expression);
        } else {
            return null;
        }
    }

    /**
     * @param resolveValue
     * @return
     */
    public Object unwrap(final Object resolve) {
        if (resolve instanceof ConditionResult) {
            return ((ConditionResult) resolve).getValue();
        }
        return resolve;
    }

    public static <Input, Output> OpHandler putThreadResolver(final String key, final ConditionResolver<Input, Output> resolver) {
        Map<String, OpHandler> map = Condition.OPERATIONS.get();
        if (map == null) {
            map = new HashMap<String, Condition.OpHandler>();
        }
        final OpHandler ret = map.put(key, new ResolverOPHandler<Input, Output>(key, resolver));
        Condition.OPERATIONS.set(map);
        return ret;
    }

    /**
     * @param handler
     * @return
     */
    public static boolean removeThreadOPHandler(final String key) {
        final Map<String, OpHandler> map = Condition.OPERATIONS.get();
        if (map == null) {
            return false;
        }
        if (map.remove(key) != null) {
            Condition.OPERATIONS.set(map);
        }
        return false;
    }

    public static boolean removeThreadResolver(final String key) {
        return removeThreadOPHandler(key);
    }

    protected HashMap<String, PathHandler> customPathhandlers;
    private LogInterface                   logger;
    private int                            logPathLength;

    public LogInterface _getLogger() {
        return this.logger;
    }

    public void _setLogger(final LogInterface logger) {
        this.logger = logger;
    }

    public Condition<MatcherType> logger(final LogInterface logger) {
        this.logger = logger;
        return this;
    }

    public static final String $FIRST = "§first";

    /**
     *
     */
    public Condition() {
    }

    public Condition(final Map<String, Object> condition) {
        super(condition);
    }

    /**
     * @param string
     * @param i
     */
    public Condition(final String key, final Object o) {
        this.append(key, o);
    }

    /**
     * @param key
     * @param o
     */
    public Condition append(final String key, final Object o) {
        this.put(key, o);
        return this;
    }

    public boolean equalsDeep(final Condition container, final Object expressionA, final Object expressionB, final Scope scope) throws ConditionException {
        ListAccessorInterface listA;
        ListAccessorInterface listB;
        MapAccessorInterface mapA;
        MapAccessorInterface mapB;
        if (expressionA == expressionB) {
            return true;
        } else if (expressionA == null && expressionB != null) {
            return false;
        } else if (expressionB == null) {
            return false;
        } else if (this.equalsShallow(container, expressionA, expressionB, scope)) {
            // if equals says these objects equal, we trust
            return true;
        } else if ((listA = container.getListWrapper(expressionA)) != null && (listB = container.getListWrapper(expressionB)) != null) {
            if (listA.size() != listB.size()) {
                return false;
            } else {
                final Iterator e1 = listA.iterator();
                final Iterator e2 = listB.iterator();
                int i = 0;
                while (e1.hasNext() && e2.hasNext()) {
                    final Object o1 = e1.next();
                    final Object o2 = e2.next();
                    if (o1 == o2) {
                        continue;
                    }
                    scope.add(o2, i++);
                    try {
                        if (o1 instanceof Condition) {
                            final Object result = ((Condition) o1).evaluateInternal(scope);
                            if (this.isFalse(result)) {
                                return false;
                            }
                        } else {
                            if (!this.equalsDeep(container, o1, o2, scope)) {
                                return false;
                            }
                        }
                    } finally {
                        scope.removeLast();
                    }
                }
                return true;
            }
        } else if (expressionA instanceof Condition) {
            // is it possible that expressionA is a Map that is actually a condition?
            final Object result = ((Condition) expressionA).evaluateInternal(scope);
            return ((Condition) expressionA).isTrue(result);
        } else if ((mapA = container.getMapWrapper(expressionA)) != null && (mapB = container.getMapWrapper(expressionB)) != null) {
            DebugMode.debugger();
            // this cannot be reached?
            if (mapA.size() != mapB.size()) {
                return false;
            }
            if (!CompareUtils.equals(mapA.keySet(), mapB.keySet())) {
                return false;
            }
            for (final java.util.Map.Entry<String, ?> es : mapA) {
                try {
                    // scope.add(es.getValue());
                    // discuss...change scope`?
                    if (!this.equalsDeep(container, es.getValue(), mapB.get(es.getKey()), scope)) {
                        return false;
                    }
                } finally {
                    // scope.removeLast();
                }
            }
            return true;
        } else if (expressionA != null && expressionA.getClass() == expressionB.getClass()) {
            if (Clazz.isPrimitive(expressionA.getClass()) || Clazz.isEnum(expressionA.getClass()) || Clazz.isString(expressionA.getClass())) {
                // if true, this would have exited in the } else if (objectX.equals(objectY)) { block above
                return false;
            }
            ClassCache cc;
            try {
                cc = ClassCache.getClassCache(expressionA.getClass());
                for (final Getter c : cc.getGetter()) {
                    try {
                        // scope.add(c.getValue(matcherValue));
                        // discuss...change scope`?
                        if (!this.equalsDeep(container, c.getValue(expressionA), c.getValue(expressionB), scope)) {
                            return false;
                        }
                    } finally {
                        // scope.removeLast();
                    }
                }
                return true;
            } catch (final SecurityException e) {
                throw new WTFException(e);
            } catch (final NoSuchMethodException e) {
                return false;
            } catch (final IllegalArgumentException e) {
                throw new WTFException(e);
            } catch (final IllegalAccessException e) {
                throw new WTFException(e);
            } catch (final InvocationTargetException e) {
                throw new WTFException(e);
            }
        } else {
            return expressionA != null && expressionA.equals(expressionB);
        }
    }

    public boolean equalsShallow(final Condition container, final Object expression, final Object matcherValue, final Scope scope) throws ConditionException {
        if (expression == matcherValue) {
            return true;
        }
        if (expression == null || matcherValue == null) {
            return false;
        }
        Object resolvedExpression = this.resolveValue(container, expression, scope, false);
        if (resolvedExpression == matcherValue) {
            // DebugMode.debugger();
            return true;
        }
        if (resolvedExpression instanceof AggregationResult && ((AggregationResult) resolvedExpression) == matcherValue) {
            DebugMode.debugger();
            // same logic as above?
            return true;
        }
        if (resolvedExpression instanceof Number && matcherValue instanceof Number) {
            return CompareUtils.equalsNumber((Number) resolvedExpression, (Number) matcherValue);
        }
        if (matcherValue.getClass().isEnum() && resolvedExpression instanceof String) {
            // Conditions are often stored as JSON- which does not know enums.
            return StringUtils.equals(matcherValue.toString(), (String) resolvedExpression);
        }
        if (resolvedExpression instanceof String && matcherValue instanceof String) {
            // no isMap - options is part of the conditions. TypeHandlers should handle the matcher only
            if (Boolean.TRUE.equals(getOptions(Boolean.class, CASE_INSENSITIVE))) {
                return StringUtils.equalsIgnoreCase((String) resolvedExpression, (String) matcherValue);
            }
        }
        if (resolvedExpression instanceof ConditionResult) {
            // DebugMode.debugger();
            resolvedExpression = container.unwrap(resolvedExpression);
        }
        for (final TypeHandler entry : getTypeHandlerIterable()) {
            final Boolean res = entry.equals(resolvedExpression, matcherValue);
            if (res != null) {
                return res == Boolean.TRUE;
            }
        }
        return CompareUtils.equals(matcherValue, resolvedExpression);
    }

    /**
     * @param test
     * @return
     * @throws ConditionException
     */
    public Object evaluate(final Object matcher) throws ConditionException {
        return this.unwrap(this.evaluateInternal(new Scope(matcher)));
    }

    // MAY return AggregationResults!
    public Object evaluateInternal(final Scope scope) throws ConditionException {
        final boolean rootFlag;
        if (now.get() == null) {
            now.set(System.currentTimeMillis());
            rootFlag = true;
        } else {
            rootFlag = false;
        }
        if (_isDebug()) {
            log(scope.getPath(), " Root is  %s", ROOT_CONDITION.get());
        }
        boolean unsetRoot = initRoot(scope);
        try {
            this.fixInternalMapToCondition();
            Object lastResult = null;
            boolean lastResultDefined = false;
            HashSet<String> keysMustBeEvaluated = null;
            final Object options = this.getOptions(Object.class);
            SKIP: if (scope.getPath().size() > 0) {
                final Object last = this.unwrapType(scope.getLast());
                // use map interface no map accessor required because this is part of the condition
                if (options instanceof Map) {
                    if (((Map) options).get(FILTER_ROOT) == Boolean.TRUE) {
                        break SKIP;
                    } else if (((Map) options).get(FILTER_ROOT) == Boolean.FALSE) {
                        keysMustBeEvaluated = new HashSet<String>(this.listKeys(last));
                        break SKIP;
                    }
                }
                if (last == null) {
                    break SKIP;
                } else if (last instanceof String) {
                    break SKIP;
                } else if (getListWrapper(scope.getLast()) != null) {
                    break SKIP;
                } else if (last == KEY_DOES_NOT_EXIST) {
                    break SKIP;
                } else if (Clazz.isPrimitive(last.getClass())) {
                    break SKIP;
                } else if (Clazz.isEnum(last.getClass())) {
                    break SKIP;
                }
                if (scope.getPath().getLast() instanceof String) {
                    final String lastKey = (String) scope.getPath().getLast();
                    final Operator lastOP = OPS.get(lastKey);
                    if (lastOP != null && lastOP.isFilterRoot()) {
                        break SKIP;
                    }
                }
                if (scope.getPath().size() >= 2) {
                    // Project Op adds the filtered key to the scope path, and thus the last element is never the OP itself
                    final Object maybeProjectOp = scope.getPath().get(-2);
                    if (maybeProjectOp instanceof String) {
                        final Operator lastOP = OPS.get(maybeProjectOp);
                        if (lastOP != null) {
                            if (lastOP instanceof ProjectOp) {
                                break SKIP;
                            }
                            if (lastOP instanceof AnyOp) {
                                break SKIP;
                            }
                            if (lastOP instanceof EachOp) {
                                break SKIP;
                            }
                            if (lastOP instanceof AndOp) {
                                break SKIP;
                            }
                        }
                    }
                }
                // if we are not in root,ALL keys must match - only the condition root works like a filter
                keysMustBeEvaluated = new HashSet<String>(this.listKeys(scope.getLast()));
            }
            boolean couldHandleAllKeys = true;
            for (final java.util.Map.Entry<String, Object> es : this.entrySet()) {
                String key = es.getKey();
                if (IGNORE.contains(key)) {
                    // for internal use only.
                    continue;
                }
                // Object value = evaluateAggregationExpression(this, es.getValue(), obj);
                final Map<String, OpHandler> localOps = OPERATIONS.get();
                Operator op = null;
                if (localOps != null) {
                    op = localOps.get(key);
                }
                if (op == null) {
                    op = OPS.get(key);
                }
                if (op != null) {
                    // contains operators - object does not have to match complete.
                    if (this._isDebug()) {
                        this.log(scope.getPath(), "Operator-Start " + this.toLog(scope.getLast()) + "." + key + "(" + es.getValue() + ")");
                    }
                    keysMustBeEvaluated = null;
                    lastResultDefined = true;
                    scope.add(scope.getLast(), key);
                    try {
                        lastResult = op.opEval(this, es.getValue(), scope);
                        if (this._isDebug()) {
                            this.log(scope.getPath(), "Operator-Result " + key + " = " + lastResult);
                        }
                    } finally {
                        scope.removeLast();
                    }
                    // DebugMode.breakIf(lastResult instanceof AggregationResult, EMPTY);
                    if (this.isFalse(lastResult)) {
                        if (this._isDebug()) {
                            this.log(scope.getPath(), "Operator-Result " + key + " on " + scope.getLast() + " = false");
                        }
                        return false;
                    }
                } else {
                    // we cannot simply extend scope, because the key might be absolute and not related to the current scope
                    Scope newScope;
                    try {
                        final JSPath keyPath = JSPath.fromPathString(key);
                        if (keysMustBeEvaluated != null) {
                            couldHandleAllKeys &= keysMustBeEvaluated.remove(String.valueOf(keyPath.getFirst()));
                        }
                        newScope = this.resolveKeyPath(scope, keyPath);
                        if (this._isDebug()) {
                            this.log(newScope.getPath(), "Resolved to " + this.toLog(newScope.getLast()));
                        }
                    } catch (final InvalidPathException e) {
                        throw new ConditionException(e);
                    }
                    final Object expression = es.getValue();
                    if (expression instanceof Condition) {
                        lastResultDefined = true;
                        lastResult = convert(((Condition<?>) expression)).evaluateInternal(newScope/* matcherValue, value */);
                        if (lastResult instanceof ConditionResult) {
                            // aggregation does not compare to the current path - this must be done here
                            lastResult = ((ConditionResult) lastResult).implicitEquals(this, newScope);
                        }
                        if (lastResult == Boolean.FALSE) {
                            return false;
                        }
                        // if (lastResult instanceof QueryOPResult) {
                        // if (lastResult == Boolean.FALSE) {
                        // return false;
                        // } else {
                        // continue NEXT_ENTRY;
                        // }
                        // }
                        // lastResult = EQOP.opEval(this, expression, newScope);
                        // if (Boolean.FALSE.equals(lastResult)) {
                        // return false;
                        // } else if (lastResult == Boolean.FALSE) {
                        // return false;
                        // }
                    } else {
                        // not an expression but a static value. we assume an implicit eq operator
                        lastResultDefined = true;
                        lastResult = EQOP.opEval(this, expression, newScope/* matcherValue, value */);
                        if (lastResult == Boolean.FALSE) {
                            return false;
                        }
                    }
                }
                if (!couldHandleAllKeys) {
                    // expression does not match the matcher object
                    // condition {a:true,b:true} equals matcher {a:true} -->False
                    if (this._isDebug()) {
                        this.log(scope.getPath(), " = false - too many properties in condition -> " + KEY_DOES_NOT_EXIST);
                    }
                    return false;
                }
            }
            if (keysMustBeEvaluated != null && keysMustBeEvaluated.size() > 0) {
                // expression did not evaluate all of the matcher Objects keys
                // condition{a:true} equals matcher {a:true,b:true} --> False
                if (this._isDebug()) {
                    this.log(scope.getPath(), " = false - Keys not evaluated. Container is no filterRoot: " + keysMustBeEvaluated);
                }
                return false;
            }
            if (lastResultDefined) {
                return lastResult;
            }
            return true;
        } finally {
            if (rootFlag) {
                now.set(null);
            }
            clearRoot(scope, unsetRoot);
        }
    }

    public void clearRoot(final Scope scope, boolean unsetRoot) {
        if (unsetRoot) {
            if (_isDebug()) {
                log(scope.getPath(), "Unset Root %s", this);
            }
            ROOT_CONDITION.set(null);
        }
    }

    public boolean initRoot(final Scope scope) {
        if (ROOT_CONDITION.get() == null) {
            if (_isDebug()) {
                log(scope.getPath(), "Set Root to %s", this);
            }
            ROOT_CONDITION.set(this);
            return true;
        }
        return false;
    }

    /**
     * overwrite to use a different type of condition - e.g. a extended one.
     *
     * @param condition
     * @return
     */
    protected Condition<?> convert(Condition<?> condition) {
        return condition;
    }

    /**
     * @param last
     * @return
     */
    private Object toLog(final Object last) {
        final Object mapped = this.unwrapType(last);
        if (mapped != last) {
            return last + "( -> " + mapped + ")";
        }
        return last;
    }

    /**
     * uses the type Handlers to return the actual type that should be used for operations.
     *
     * @param last
     * @return
     */
    protected Object unwrapType(final Object value) {
        for (final TypeHandler v : getTypeHandlerIterable()) {
            final Object ret = v.unwrapType(value);
            if (ret != value) {
                return ret;
            }
        }
        return value;
    }

    /**
     * returns true if lastResult is a false expression
     *
     * @param lastResult
     * @return
     */
    protected boolean isFalse(final Object lastResult) {
        if (lastResult instanceof ConditionResult) {
            return ((ConditionResult) lastResult).isFalse();
        }
        return this.unwrapType(lastResult) == Boolean.FALSE;
    }

    protected void fixInternalMapToCondition() throws ConditionException {
        for (final java.util.Map.Entry<String, Object> es : this.entrySet()) {
            // convert HashMap to Condition. Internal HashMaps may be created by deserializing Conditions. This is fixed in the first
            // run
            if (es.getValue() instanceof Map && !(es.getValue() instanceof Condition)) {
                Condition condition;
                condition = this.newInstance();
                condition.putAll((Map) es.getValue());
                this.replace(es.getKey(), es.getValue(), condition);
                es.setValue(condition);
            }
        }
    }

    /**
     * @param field
     * @return
     */
    private boolean isForbiddenField(final Field field) {
        if (!Modifier.isPublic(field.getModifiers())) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * @param method
     * @return
     */
    private boolean isForbiddenMethod(final Method method) {
        if (!Modifier.isPublic(method.getModifiers())) {
            return true;
        } else if (Clazz.isVoid(method.getReturnType())) {
            return true;
        } else {
            return false;
        }
    }

    public Scope resolveKeyPath(final Scope scope, final JSPath keyPath) throws ConditionException {
        if (_isDebug()) {
            this.log(keyPath, "Resolve Path. Base: %s", scope.toString().replaceAll("\r\n", " -> "));
        }
        // because resolveKeyPath might get called externaly just to resolve a value
        boolean unset = initRoot(scope);
        try {
            MapAccessorInterface map;
            ListAccessorInterface list;
            Scope ret = scope.copy();
            if (keyPath.getFirst() instanceof String && ((String) keyPath.getFirst()).equals("§")) {
                // §[0] -->ference to 0. element
                keyPath.remove(0);
            }
            NEXT_PATH_ELEMENT: for (final Object keyOrg : keyPath.getElements()) {
                try {
                    if (keyOrg instanceof Condition) {
                        final Condition filter = (Condition) keyOrg;
                        ret.add(convert(filter).evaluateInternal(ret), keyOrg);
                        continue NEXT_PATH_ELEMENT;
                    }
                    String keyAsString = StringUtils.valueOfOrNull(keyOrg);
                    {
                        // custom Pathhandlers
                        final ConcatIterator<java.util.Map.Entry<String, PathHandler>> it = this.getPathHandlerIterator();
                        if (it != null) {
                            for (final java.util.Map.Entry<String, PathHandler> es : it) {
                                if (es.getKey() == null || es.getKey().equalsIgnoreCase(keyAsString)) {
                                    final Scope r = es.getValue().resolve(scope, ret, keyOrg);
                                    if (r != null) {
                                        ret = r;
                                        continue NEXT_PATH_ELEMENT;
                                    }
                                }
                            }
                        }
                    }
                    if ($$ROOT.equalsIgnoreCase(keyAsString)) {
                        if (keyAsString != keyPath.getFirst()) {
                            throw new ConditionException("PathLink §§... must always be the first key element");
                        }
                        ret = new Scope(scope.getFirst());
                        continue;
                    } else if ($$THIS.equalsIgnoreCase(keyAsString) || $$CURRENT.equalsIgnoreCase(keyAsString)) {
                        if (keyAsString != keyPath.getFirst()) {
                            throw new ConditionException("PathLink §§... must always be the first key element");
                        }
                        ret.add(scope.getLast(), keyOrg);
                        continue;
                    } else if ("§this".equalsIgnoreCase(keyAsString) || "§current".equalsIgnoreCase(keyAsString)) {
                        throw new WTFException(keyAsString + " is not allowed. Use §§this and §§current");
                    } else if ($PARENT.equalsIgnoreCase(keyAsString)) {
                        ret = ret.getParent();
                        if (ret == null) {
                            ret = new Scope(KEY_DOES_NOT_EXIST);
                        }
                        continue;
                    } else if ($NOW.equalsIgnoreCase(keyAsString)) {
                        ret = new Scope(now.get());
                        continue;
                    } else if ($TYPE.equalsIgnoreCase(keyAsString)) {
                        Object last = ret.getLast();
                        if (last == null) {
                            ret.add(KEY_DOES_NOT_EXIST, keyOrg);
                        } else {
                            ret.add(ret.getLast().getClass().getName(), keyAsString);
                        }
                        continue;
                    } else if ($KEYS.equalsIgnoreCase(keyAsString)) {
                        ret.add(this.listKeys(ret.getLast()), keyOrg);
                        continue;
                    } else if ($KEY.equalsIgnoreCase(keyAsString)) {
                        final List<Object> loop = ret.getPath().getElements();
                        int backlog = 0;
                        while (backlog < loop.size()) {
                            // search last non-op key
                            final Object el = loop.get(loop.size() - backlog - 1);
                            if (el instanceof String && OPS.containsKey(el)) {
                                backlog++;
                                continue;
                            } else {
                                ret.add(el, keyOrg);
                                break;
                            }
                        }
                        continue;
                    } else if ($FIRST.equalsIgnoreCase(keyAsString)) {
                        if ((list = getListWrapper(ret.getLast())) != null) {
                            if (list.size() == 0) {
                                ret.add(KEY_DOES_NOT_EXIST, keyOrg);
                            } else {
                                ret.add(list.get(0), keyOrg);
                            }
                        } else if ((map = getMapWrapper(ret.getLast())) != null) {
                            if (map.size() == 0) {
                                ret.add(KEY_DOES_NOT_EXIST, keyOrg);
                            } else {
                                ret.add(map.iterator().next().getValue(), keyOrg);
                            }
                        } else {
                            ret.add((ret.getLast()), keyOrg);
                        }
                        continue;
                    } else if ($SIZE.equalsIgnoreCase(keyAsString)) {
                        if ((list = getListWrapper(ret.getLast())) != null) {
                            ret.add(list.size(), keyOrg);
                        } else if ((map = getMapWrapper(ret.getLast())) != null) {
                            ret.add(map.size(), keyOrg);
                        } else if (ret.getLast() instanceof String) {
                            ret.add(((String) ret.getLast()).length(), keyOrg);
                        } else {
                            ret.add(this.listKeys(ret.getLast()).size(), keyOrg);
                        }
                        continue;
                    } else if (ret.getLast() == null) {
                        ret.add(KEY_DOES_NOT_EXIST, keyOrg);
                        continue;
                    } else if (ret.getLast() == KEY_DOES_NOT_EXIST) {
                        // we need the full path - there may be any pathmodifier that resolves keynotfound to anything else, and some
                        // operators
                        // may need the fullpath
                        ret.add(KEY_DOES_NOT_EXIST, keyOrg);
                        continue;
                    }
                    if (keyAsString != null && keyAsString.startsWith("§")) {
                        keyAsString = keyAsString.substring(1);
                    }
                    if (ret.getLast() instanceof ConditionObjectValueView) {
                        final ConditionObjectValueView view = (ConditionObjectValueView) ret.getLast();
                        final Object newValue = view.getConditionObjectValue(keyAsString);
                        if (newValue == null) {
                            if (view.containsConditionObjectKey(keyAsString)) {
                                ret.add(null, keyOrg);
                                continue;
                            } else if (!view.isConditionObjectVisible()) {
                                ret.add(KEY_DOES_NOT_EXIST, keyOrg);
                                continue;
                            }
                        } else {
                            ret.add(newValue, keyOrg);
                            continue;
                        }
                    } else if ((map = getMapWrapper(ret.getLast())) != null) {
                        // do not use isMap here - this is for actual Map elements - not for TypeHandlers
                        try {
                            ret.add(map.get(keyAsString), keyOrg);
                        } catch (Exception e) {
                            throw new CannotGetValueException(e);
                        }
                        continue;
                    } else if ((list = getListWrapper(ret.getLast())) != null) {
                        try {
                            ret.add(list.get(((Number) keyOrg).intValue()), keyOrg);
                        } catch (Exception e) {
                            throw new CannotGetValueException(e);
                        }
                        continue;
                    }
                    // Search for methods that have the exact key name
                    Class<? extends Object> cls = ret.getLast().getClass();
                    Method method = null;
                    try {
                        final ClassCache cc = ClassCache.getClassCache(cls);
                        final Getter getter = cc.getGetter(keyAsString);
                        if (getter != null) {
                            if (!this.isForbiddenMethod(getter.method)) {
                                method = getter.method;
                            }
                        }
                    } catch (final Exception e) {
                        throw new CannotGetValueException(e);
                    }
                    if (method != null) {
                        method.setAccessible(true);
                        try {
                            ret.add(method.invoke(ret.getLast(), new Object[0]), keyOrg);
                        } catch (InvocationTargetException e) {
                            if (e.getTargetException() instanceof InterruptedException) {
                                Thread.currentThread().interrupt();
                            }
                            throw new CannotGetValueException(e);
                        } catch (Exception e) {
                            throw new CannotGetValueException(e);
                        }
                        continue;
                    }
                    ret.add(KEY_DOES_NOT_EXIST, keyOrg);
                    continue;
                } catch (final CannotGetValueException e) {
                    ret.add(KEY_DOES_NOT_EXIST, keyOrg);
                } finally {
                    if (ret.getLast() == KEY_DOES_NOT_EXIST) {
                        this.onKeyDoesNotExist(scope, keyPath, ret);
                    }
                }
            }
            return ret;
        } finally {
            clearRoot(scope, unset);
        }
    }

    private ConcatIterator<java.util.Map.Entry<String, PathHandler>> getPathHandlerIterator() {
        ConcatIterator<java.util.Map.Entry<String, PathHandler>> it = null;
        final HashMap<String, PathHandler> custom = this.customPathhandlers;
        if (custom != null) {
            if (it == null) {
                it = new ConcatIterator<java.util.Map.Entry<String, PathHandler>>();
            }
            it.add(custom.entrySet().iterator());
        }
        final Map<String, PathHandler> threadPathHandlers = PATH_HANDLERS.get();
        if (threadPathHandlers != null) {
            if (it == null) {
                it = new ConcatIterator<java.util.Map.Entry<String, PathHandler>>();
            }
            it.add(threadPathHandlers.entrySet().iterator());
        }
        return it;
    }

    /**
     * @param scope
     * @param keyPath
     * @param ret
     * @throws ConditionException
     */
    protected void onKeyDoesNotExist(final Scope scope, final JSPath keyPath, final Scope ret) throws ConditionException {
        if (this._isDebug()) {
            this.log(ret.getPath(), "Key Does not Exist: Base: %s -> %s", scope.getPath().toPathString(true), keyPath.toPathString(false));
        }
        if (!_isAutoCreateMissingNodes()) {
            return;
        }
        if (ret.getLast() == KEY_DOES_NOT_EXIST && ret.getParent().getLast() == null) {
            List<Object> path = ret.getPath().getElements();
            // next Element is the next key Element after the missing one - we need it to decide if we need an array or objevt
            Object nextElement = null;
            // Missing key is the key for the element we create
            Object missingKey = null;
            int indexNext = -1;
            int indexMissing = -1;
            for (int i = path.size() - 1; i >= 0; i--) {
                // Search key Elements - skip operators
                Object el = path.get(i);
                if (el instanceof String) {
                    if (((String) el).trim().startsWith("§")) {
                        // Operator
                        continue;
                    }
                }
                if (nextElement == null) {
                    nextElement = el;
                    indexNext = i;
                    continue;
                }
                if (missingKey == null) {
                    if (ret.getScope().get(i + 1) != null) {
                        ret.replaceLast(null);
                        // the real parent already exists
                        return;
                    }
                    missingKey = el;
                    indexMissing = i;
                    break;
                }
            }
            // remember: path.size() is always < than scope.size() - the scope contains the root elements
            // search parent: it may not be the direct parent because the scope contains operators etc.
            List<Object> scopeList = ret.getScope();
            Object parent = null;
            for (int i = indexMissing; i >= 0; i--) {
                parent = scopeList.get(i);
                if (parent != null) {
                    break;
                }
            }
            DebugMode.breakIf(parent == null);
            Object autoCreated = autoCreatePathElement(ret, parent, missingKey, nextElement);
            if (autoCreated != null) {
                if (_isDebug()) {
                    this.log(ret.getPath(), "Missing Element: %s - Next Element (defines object type): %s", missingKey, nextElement);
                }
                ret.set(indexMissing + 1, autoCreated);
                ret.set(indexNext + 1, null);
                if (_isDebug()) {
                    this.log(ret.getPath(), "Auto Created %s", ret.getParent());
                }
            }
        } else {
            ret.replaceLast(null);
        }
    }

    public boolean _isAutoCreateMissingNodes() throws ConditionException {
        boolean auto = false;
        ;
        Boolean fromOptions = getOptions(Boolean.class, "create");
        if (fromOptions != null) {
            auto = fromOptions == Boolean.TRUE;
        }
        if (!auto && ROOT_CONDITION.get() != this) {
            auto |= ROOT_CONDITION.get()._isAutoCreateMissingNodes();
        }
        return auto;
    }

    public Object autoCreatePathElement(Scope scope, Object parent, Object missingKeyElement, Object nextPathElement) {
        ListAccessorInterface list = getListWrapper(parent);
        MapAccessorInterface map;
        Object newNode;
        if (list != null) {
            // DebugMode.debugger();
            int index = JSPath.toArrayIndex(missingKeyElement);
            if (list.size() > index && list.get(index) != KEY_DOES_NOT_EXIST) {
                DebugMode.breakIf(true, "should not happen");
            }
            if (JSPath.isArrayKey(nextPathElement)) {
                newNode = newAutoCreateArray();
            } else {
                newNode = newAutoCreateMap();
            }
            for (int ii = list.size(); ii < index; ii++) {
                list.add(null);
                if (_isDebug()) {
                    log(scope.getPath(), "AutoCreate: Add to array[%d] = %s", ii, list.get(ii));
                }
            }
            list.add(newNode);
            if (_isDebug()) {
                log(scope.getPath(), "AutoCreate: Set array[%d] = %s", index, newNode);
            }
            return newNode;
        } else if ((map = getMapWrapper(parent)) != null) {
            Object existsButNotInScopeYet = map.get(String.valueOf(missingKeyElement));
            if (existsButNotInScopeYet != KEY_DOES_NOT_EXIST) {
                return existsButNotInScopeYet;
            }
            if (JSPath.isArrayKey(nextPathElement)) {
                newNode = newAutoCreateArray();
            } else {
                newNode = newAutoCreateMap();
            }
            map.put(String.valueOf(missingKeyElement), newNode);
            if (_isDebug()) {
                log(scope.getPath(), "AutoCreate: Put object[%s] = %s", String.valueOf(missingKeyElement), newNode);
            }
            return newNode;
        }
        return null;
    }

    /**
     * @return
     */
    protected Object newAutoCreateMap() {
        for (final TypeHandler e : getTypeHandlerIterable()) {
            final Object ret = e.newAutoCreateMap();
            if (ret != null) {
                return ret;
            }
        }
        return new HashMap<String, Object>();
    }

    /**
     * @return
     */
    protected Object newAutoCreateArray() {
        for (final TypeHandler e : getTypeHandlerIterable()) {
            final Object ret = e.newAutoCreateArray();
            if (ret != null) {
                return ret;
            }
        }
        return new LinkedList<Object>();
    }

    /**
     * @param class1
     * @return
     * @throws ConditionException
     */
    public List<String> listKeys(final Object obj) throws ConditionException {
        final ListAccessorInterface listWrapper;
        final MapAccessorInterface mapWrapper;
        if (obj == null || Clazz.isPrimitive(obj.getClass()) || obj instanceof String) {
            return new ArrayList<String>(0);
        } else if (obj instanceof ConditionObjectValueView) {
            final ConditionObjectValueView view = (ConditionObjectValueView) obj;
            return view.listConditionKeys();
        } else if (obj instanceof Map) {
            // faster than getMapIterable
            return new ArrayList<String>(((Map) obj).keySet());
        } else if ((mapWrapper = getMapWrapper(obj)) != null) {
            return new ArrayList<String>(mapWrapper.keySet());
        } else if ((listWrapper = getListWrapper(obj)) != null) {
            if (listWrapper instanceof ArrayAccessor || listWrapper instanceof ListAccessor) {
                final int size = listWrapper.size();
                final ArrayList<String> ret = new ArrayList<String>(size);
                for (int i = 0; i < size; i++) {
                    ret.add(String.valueOf(i));
                }
                return ret;
            } else {
                return new ArrayList<String>(0);
            }
        } else {
            try {
                return new ArrayList<String>(ClassCache.getClassCache(obj.getClass()).getKeys());
            } catch (final SecurityException e) {
                throw new ConditionException(e);
            } catch (final NoSuchMethodException e) {
                throw new ConditionException(e);
            }
        }
    }

    /**
     * @return
     */
    private ConcatIterator<TypeHandler> getTypeHandlerIterable() {
        ConcatIterator<TypeHandler> it = new ConcatIterator<TypeHandler>(GLOBAL_TYPE_HANDLERS.iterator());
        final List<TypeHandler> thread = TYPE_HANDLERS.get();
        if (thread != null && thread.size() > 0) {
            it.add(thread.iterator());
        }
        List<TypeHandler> local = getTypeHandler();
        if (local != null && local.size() > 0) {
            it.add(local.iterator());
        }
        final Condition root = ROOT_CONDITION.get();
        if (root != null && root != this) {
            List<TypeHandler> rootLocal = root.getTypeHandler();
            if (rootLocal != null && rootLocal.size() > 0) {
                it.add(rootLocal.iterator());
            }
        }
        return it;
    }

    /**
     * @param value
     * @return
     * @throws ConditionException
     */
    public boolean matches(final MatcherType obj) throws ConditionException {
        if (this._isDebug()) {
            this.log(new JSPath(), "Run " + this + ".matches(" + debugToString(obj) + ")");
        }
        final Object result = this.evaluateInternal(new Scope(obj));
        return this.isTrue(result);
    }

    private boolean isTrue(final Object result) {
        if (result instanceof ConditionResult) {
            return ((ConditionResult) result).isTrue();
        }
        return Boolean.TRUE == result;
    }

    /**
     * @param string
     * @param exp
     * @param object
     * @param string2
     */
    public void log(final JSPath path, String string, Object... args) {
        for (int i = 0; i < args.length; i++) {
            args[i] = toLog(args[i]);
        }
        if (this.logger != null) {
            if (args != null && args.length > 0) {
                string = String.format(string, args);
            }
            String pathString = path.toPathString(false);
            logPathLength = Math.max(logPathLength, pathString.length());
            this.logger.info(StringUtils.fillPost(pathString, " ", logPathLength) + " : " + string);
            return;
        } else {
            final Condition root = ROOT_CONDITION.get();
            if (root != null && root != this && root.logger != null) {
                root.log(path, string, args);
                return;
            }
        }
        final LogInterface log = THREAD_LOGGER.get();
        if (log != null) {
            if (args != null && args.length > 0) {
                string = String.format(string, args);
            }
            String pathString = path.toPathString(false);
            logPathLength = Math.max(logPathLength, pathString.length());
            // LogV3.info(log, string, args);
            log.info(StringUtils.fillPost(pathString, " ", logPathLength) + " : " + string);
            return;
        }
    }

    private boolean                               debug         = false;
    public static final ThreadLocal<Boolean>      THREAD_DEBUG  = new ThreadLocal<Boolean>();
    public static final ThreadLocal<LogInterface> THREAD_LOGGER = new ThreadLocal<LogInterface>();

    public boolean _isDebug() {

        if (this.debug) {
            return true;
        }
        Object options = get($OPTIONS);
        if (options != null) {
            if (options instanceof Map) {

                if (Boolean.TRUE.equals(((Map) options).get("debug"))) {
                    return true;
                }
            }

        }
        final Condition root = ROOT_CONDITION.get();
        if (root != null && root != this && root._isDebug()) {
            return true;
        }
        return THREAD_DEBUG.get() == Boolean.TRUE;
    }

    public void _setDebug(final boolean debug) {
        this.debug = debug;
    }

    public Condition<MatcherType> debug(final boolean debug) {
        this.debug = debug;
        return this;
    }

    /**
     * @param request
     * @return
     */
    public boolean matchesWithoutExceptions(final MatcherType test) {
        try {
            return this.matches(test);
        } catch (final ConditionException e) {
            LogV3.log(e);
            return false;
        }
    }

    /**
     * @return
     */
    public Condition newInstance() {
        try {
            return getClass().getDeclaredConstructor().newInstance();
        } catch (Exception e) {
            Exceptions.resetInterruptFlag(e);
            throw new WTFException("Implement a proper newINstance method!");
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see java.util.HashMap#put(java.lang.Object, java.lang.Object)
     */
    @Override
    public Object put(String key, final Object value) {
        key = this.correctKey(key);
        return super.put(key, value);
    }

    public String correctKey(String key) {
        if (key.startsWith("$$")) {
            key = "§§" + key.substring(2);
        } else if (key.startsWith("$")) {
            key = "§" + key.substring(1);
        }
        return key;
    }

    /*
     * (non-Javadoc)
     *
     * @see java.util.HashMap#putAll(java.util.Map)
     */
    @Override
    public void putAll(final Map<? extends String, ? extends Object> m) {
        for (final java.util.Map.Entry<? extends String, ? extends Object> es : m.entrySet()) {
            this.put(es.getKey(), es.getValue());
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see java.util.HashMap#remove(java.lang.Object)
     */
    @Override
    public Object remove(final Object key) {
        return super.remove(key);
    }

    /*
     * (non-Javadoc)
     *
     * @see java.util.HashMap#replace(java.lang.Object, java.lang.Object, java.lang.Object)
     */
    @Override
    public boolean replace(final String key, final Object oldValue, final Object newValue) {
        return super.replace(key, oldValue, newValue);
    }

    protected final static CharSequence toCharSequence(final Object value) {
        if (value instanceof CharSequence) {
            return (CharSequence) value;
        } else {
            return String.valueOf(value);
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see java.util.HashMap#replaceAll(java.util.function.BiFunction)
     */
    @Override
    public void replaceAll(final BiFunction<? super String, ? super Object, ? extends Object> function) {
        super.replaceAll(function);
    }

    /**
     * Resolves a value expression - MAY RETURN AGGREGATIONRESULT
     *
     * @param container
     * @param unwrapConditionResults
     * @param path
     **/
    public Object resolveValue(final Condition container, Object expression, final Scope scope, final boolean unwrapConditionResults) throws ConditionException {
        final Object before = expression;
        expression = autoConvertMapToCondition(expression);
        if (expression instanceof Condition) {
            expression = convert(((Condition) expression)).evaluateInternal(scope);
        }
        if (expression instanceof String && ((String) expression).startsWith("§")) {
            try {
                expression = this.resolveKeyPath(scope, JSPath.fromPathString(((String) expression))).getLast();
            } catch (final InvalidPathException e) {
                throw new ConditionException(e);
            }
        }
        if (before != expression) {
            if (container._isDebug()) {
                container.log(scope.getPath(), "Resolved " + before + " = " + this.toLog(expression));
            }
        }
        if (unwrapConditionResults) {
            return container.unwrap(expression);
        }
        return expression;
    }

    public Object autoConvertMapToCondition(Object expression) {
        // no reason to check for isMap(...) here, this is for real maps - no TypeHandlers
        if (expression instanceof Map && !(expression instanceof Condition)) {
            // fix map to condition. json deserializer will map internal conditions to maps
            final Map old = (Map) expression;
            expression = this.newInstance();
            ((Condition) expression).putAll(old);
        }
        return expression;
    }

    /*
     * (non-Javadoc)
     *
     * @see java.util.AbstractMap#toString()
     */
    @Override
    public String toString() {
        // do not use Jsonstorage here - bad coupling
        return debugToString(this);
    }

    /**
     * @param condition
     * @return
     * @throws Exception
     */
    private static String debugToString(Object obj) {
        try {
            Class<?> deser = Class.forName(CLASS_ORG_APPWORK_SERIALIZER_DESER);
            Class<?> serializerInterface = Class.forName(CLASS_ORG_APPWORK_STORAGE_COMMON_INTERFACE_SERIALIZER_INTERFACE);
            Method method = deser.getMethod("get", new Class[] {});
            method.setAccessible(true);
            Object serializer = method.invoke(method, new Object[] {});
            Method toString = serializerInterface.getMethod("toString", new Class[] { Object.class, Object[].class });
            return (String) toString.invoke(serializer, obj, new Object[] {});
        } catch (Exception e) {
            return String.valueOf(obj);
        }
    }

    /**
     * @param values
     * @param condition
     * @throws ConditionException
     */
    public static <T> List<T> find(final Collection<T> values, final Condition condition) throws ConditionException {
        final ArrayList<T> ret = new ArrayList<T>();
        for (final T v : values) {
            if (condition.matches(v)) {
                ret.add(v);
            }
        }
        return ret;
    }

    /**
     * @param string
     * @param regex
     * @return
     * @throws InvalidPathException
     * @throws ConditionException
     */
    public static Object resolve(final String keyString, final Object object) throws ConditionException, InvalidPathException {
        final Condition c = new Condition();
        final Scope ret = c.resolveKeyPath(new Scope(object), JSPath.fromPathString(keyString));
        return ret.getLast();
    }

    /**
     * @param flexiTypeHandler
     * @return
     */
    public static <T extends TypeHandler> T addThreadTypeHandler(T typeHandler) {
        List<TypeHandler> list = TYPE_HANDLERS.get();
        if (list == null) {
            list = new ArrayList<TypeHandler>();
            TYPE_HANDLERS.set(list);
        }
        if (!list.contains(typeHandler)) {
            list.add(typeHandler);
            return typeHandler;
        }
        return null;
    }

    /**
     * @param remove
     */
    public static void removeThreadTypeHandler(TypeHandler remove) {
        if (remove == null) {
            return;
        }
        List<TypeHandler> list = TYPE_HANDLERS.get();
        if (list == null) {
            return;
        }
        list.remove(remove);
    }

    public <TYPE> TYPE getOptions(Class<TYPE> cls) throws ConditionException {
        fixInternalMapToCondition();
        Object options = get($OPTIONS);
        if (options == null) {
            return null;
        }
        return (TYPE) options;
    }

    /**
     * @param <T>
     * @return
     * @throws InvalidPathException
     * @throws ConditionException
     */
    public <TYPE> TYPE getOptions(Class<TYPE> cls, String key) throws ConditionException {
        Object options = get($OPTIONS);
        if (options == null) {
            return null;
        }
        if (key != null) {
            for (java.util.Map.Entry<String, Object> es : getMapWrapper(options)) {
                if (StringUtils.equalsIgnoreCase(es.getKey(), key)) {
                    return (TYPE) es.getValue();
                }
            }
            return null;
        }
        return (TYPE) options;
    }

    /**
     * @param optionsAllowReferences
     * @param b
     * @return
     */
    public Condition<MatcherType> option(String key, Object value) {
        Object map = get($OPTIONS);
        if (map == null) {
            map = new Condition();
            put($OPTIONS, map);
        }
        ((Map) map).put(key, value);
        return this;
    }
}
