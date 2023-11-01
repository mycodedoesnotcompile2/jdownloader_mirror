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
package org.appwork.moncompare;

import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Locale;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.remoteapi.annotations.ApiDoc;
import org.appwork.remoteapi.annotations.ApiDocExample;
import org.appwork.storage.Storable;
import org.appwork.storage.StorableDoc;
import org.appwork.storage.StorableExample;
import org.appwork.storage.flexijson.InvalidPathException;
import org.appwork.storage.flexijson.JSPath;
import org.appwork.storage.flexijson.mapper.FlexiMapperException;
import org.appwork.storage.flexijson.mapper.typemapper.DateMapper;
import org.appwork.storage.simplejson.mapper.ClassCache;
import org.appwork.storage.simplejson.mapper.Getter;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.DebugMode;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.StringUtils;
import org.appwork.utils.duration.InvalidTimeSpanException;
import org.appwork.utils.duration.TimeSpan;
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
     * @date 10.05.2019
     *
     */
    public static class AccessByField implements AccessMethod {
        private final Field   field;
        private final boolean isStatic;

        /**
         * @param field
         */
        public AccessByField(Field field) {
            this.field = field;
            isStatic = Modifier.isStatic(field.getModifiers());
        }

        @Override
        public Object getValue(Object value, String key) throws CannotGetValueException {
            try {
                if (isStatic) {
                    return field.get(null);
                } else {
                    return field.get(value);
                }
            } catch (IllegalAccessException e) {
                throw new CannotGetValueException(e);
            }
        }
    }

    /**
     * @author Thomas
     * @date 10.05.2019
     *
     */
    public static class AccessByMethod implements AccessMethod {
        private final boolean         isStatic;
        private final Method          method;
        private final static Object[] EMPTY_ARGS = new Object[] {};

        /**
         * @param method
         */
        public AccessByMethod(Method method) {
            this.method = method;
            isStatic = Modifier.isStatic(method.getModifiers());
        }

        @Override
        public Object getValue(Object value, String key) throws CannotGetValueException {
            try {
                if (isStatic) {
                    return method.invoke(null, EMPTY_ARGS);
                } else {
                    return method.invoke(value, EMPTY_ARGS);
                }
            } catch (IllegalAccessException e) {
                throw new CannotGetValueException(e);
            } catch (IllegalArgumentException e) {
                throw new CannotGetValueException(e);
            } catch (InvocationTargetException e) {
                if (e.getTargetException() instanceof InterruptedException) {
                    Thread.currentThread().interrupt();
                }
                throw new CannotGetValueException(e);
            } catch (RuntimeException e) {
                throw new CannotGetValueException(e);
            }
        }
    }

    /**
     * @author Thomas
     * @date 10.05.2019
     *
     */
    public static class AccessListElement implements AccessMethod {
        /**
         *
         */
        public AccessListElement() {
        }

        @Override
        public Object getValue(Object value, String key) throws CannotGetValueException {
            try {
                final int index = Integer.parseInt(key);
                return ((List<?>) value).get(index);
            } catch (Throwable e) {
                throw new CannotGetValueException(e);
            }
        }
    }

    public static class AccessCollectionElement implements AccessMethod {
        /**
         *
         */
        public AccessCollectionElement() {
        }

        @Override
        public Object getValue(Object value, String key) throws CannotGetValueException {
            try {
                int index = Integer.parseInt(key);
                for (Object r : ((Collection) value)) {
                    if (index == 0) {
                        return r;
                    }
                    index--;
                }
            } catch (Throwable e) {
                throw new CannotGetValueException(e);
            }
            throw new CannotGetValueException("Index out of bounds");
        }
    }

    public static class AccessArrayElement implements AccessMethod {
        /**
         *
         */
        public AccessArrayElement() {
        }

        @Override
        public Object getValue(Object value, String key) throws CannotGetValueException {
            try {
                final int index = Integer.parseInt(key);
                return Array.get(value, index);
            } catch (Throwable e) {
                throw new CannotGetValueException(e);
            }
        }
    }

    public static class AccessMapElement implements AccessMethod {
        @Override
        public Object getValue(Object value, String key) throws CannotGetValueException {
            try {
                final Map<?, ?> map = (Map<?, ?>) value;
                final Object ret = map.get(key);
                if (ret == null) {
                    if (map.containsKey(key)) {
                        return null;
                    } else {
                        return KEY_DOES_NOT_EXIST;
                    }
                } else {
                    return ret;
                }
            } catch (Throwable e) {
                throw new CannotGetValueException(e);
            }
        }
    }

    public interface AccessMethod {
        /**
         * @param obj
         * @return
         * @throws CompareException
         */
        public abstract Object getValue(Object value, String key) throws CannotGetValueException;
    }

    /**
     * @author Thomas
     * @date 10.05.2019
     *
     */
    public static class AccessNotFound implements AccessMethod {
        @Override
        public Object getValue(Object value, String key) throws CannotGetValueException {
            return KEY_DOES_NOT_EXIST;
        }
    }

    public static class AddOp implements Operator {
        @Override
        public Object opEval(Condition<?> container, Object expressions, Scope scope) throws CompareException {
            if (ReflectionUtils.isListOrArray(expressions.getClass())) {
                final List<Object> listExpressions = ReflectionUtils.wrapUnmodifiableList(expressions, Object.class);
                Number result = null;
                for (final Object expression : listExpressions) {
                    final Number num = (Number) container.resolveValue(container, expression, scope, true);
                    if (result == null) {
                        result = num;
                    } else if (Clazz.isDouble(result.getClass()) || Clazz.isDouble(num.getClass())) {
                        result = result.doubleValue() + num.doubleValue();
                    } else if (Clazz.isFloat(result.getClass()) || Clazz.isFloat(num.getClass())) {
                        result = result.floatValue() + num.floatValue();
                    } else {
                        result = result.longValue() + num.longValue();
                    }
                }
                return result;
            }
            throw new AggregationException("AddOp:" + (container) + "|" + (expressions));
        }

        /**
         * returns false if this operator acts as filter root - See "Query Filter mode" vs. "Strict Compare mode"
         */
        @Override
        public boolean isFilterRoot() {
            return false;
        }
    }

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
        public Object opEval(Condition<?> container, Object expression, Scope scope) throws CompareException {
            if (scope.getLast() == KEY_DOES_NOT_EXIST) {
                return false;
            } else {
                final List<Object> listExpression = ReflectionUtils.wrapUnmodifiableList(expression, Object.class);
                for (Object exp : listExpression) {
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
        public Object opEval(Condition<?> container, Object expression, Scope scope) throws CompareException {
            final Object last = scope.getLast();
            if (last == KEY_DOES_NOT_EXIST) {
                return false;
            } else if (last == null || Clazz.isPrimitive(last.getClass()) || last instanceof String) {
                return container.resolveValue(container, expression, scope, false);
            } else {
                try {
                    for (String g : container.listKeys(last)) {
                        Scope newScope = container.resolveKeyPath(scope, JSPath.fromPathElements(g));
                        if (container.equalsDeep(container, expression, newScope.getLast(), newScope)) {
                            // {§any:1}
                            if (container._isDebug()) {
                                container.log(newScope.getPath(), getClass().getSimpleName() + ": " + container.resolveValue(container, expression, newScope, false) + ".matches(" + expression + ")");
                            }
                            return true;
                        }
                    }
                } catch (SecurityException e) {
                    throw new CompareException(e);
                }
            }
            if (container._isDebug()) {
                container.log(scope.getPath(), getClass().getSimpleName() + ": no element matches(" + expression + ")");
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
        public Object opEval(Condition<?> container, Object expression, Scope scope) throws CompareException {
            try {
                if (ReflectionUtils.isListOrArray(expression)) {
                    final StringBuilder sb = new StringBuilder();
                    final List<Object> listExpression = ReflectionUtils.wrapUnmodifiableList(expression, Object.class);
                    for (Object exp : listExpression) {
                        sb.append(container.resolveValue(container, exp, scope, true));
                    }
                    return sb.toString();
                }
            } catch (CompareException e) {
                throw new AggregationException(e);
            }
            throw new AggregationException();
        }
    }

    public abstract interface ConditionResolver<Input, Output> {
        /**
         * @param parameters
         * @return
         */
        public Output resolve(Input options) throws CompareException;
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
        public Object opEval(Condition<?> container, Object expressions, Scope scope) throws CompareException {
            if (expressions != null && ReflectionUtils.isListOrArray(expressions.getClass())) {
                final List<Object> division = ReflectionUtils.wrapUnmodifiableList(expressions, Object.class);
                if (division.size() == 2) {
                    final Number dividend = (Number) container.resolveValue(container, division.get(0), scope, true);
                    final Number divisor = (Number) container.resolveValue(container, division.get(1), scope, true);
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
        public Object opEval(Condition<?> container, Object expression, final Scope scope) throws CompareException {
            final Object last = scope.getLast();
            if (last == KEY_DOES_NOT_EXIST) {
                if (container._isDebug()) {
                    container.log(scope.getPath(), getClass().getSimpleName() + " -> KEY_DOES_NOT_EXIST ");
                }
                return last;
            } else {
                if (last == null || last instanceof String || Clazz.isPrimitive(last.getClass()) || Clazz.isEnum(last.getClass())) {
                    if (container.isTrue(container.resolveValue(container, expression, scope, false))) {
                        return last;
                    } else {
                        return null;
                    }
                } else if (last instanceof Map) {
                    final HashMap<Object, Object> result = new HashMap<Object, Object>();
                    final Scope newScope = scope.copy();
                    for (final Map.Entry<?, ?> entry : ((Map<?, ?>) last).entrySet()) {
                        final Object key = entry.getKey();
                        final Object o = entry.getValue();
                        newScope.add(o, key);
                        if (container.isTrue(container.resolveValue(container, expression, newScope, false))) {
                            result.put(key, o);
                        }
                        newScope.removeLast();
                    }
                    if (container._isDebug()) {
                        container.log(scope.getPath(), getClass().getSimpleName() + " -> Map with " + result.size() + " entries: " + result.keySet());
                    }
                    return result;
                } else if (Clazz.isArray(last.getClass()) || last instanceof List || last instanceof Collection) {
                    final Collection<Object> collection = ReflectionUtils.wrapCollection(last, false, Object.class);
                    final LinkedList<Object> result = new LinkedList<Object>();
                    int i = 0;
                    final Scope newScope = scope.copy();
                    for (final Object o : collection) {
                        newScope.add(o, i++);
                        if (container.isTrue(container.resolveValue(container, expression, newScope, false))) {
                            result.add(o);
                        }
                        newScope.removeLast();
                    }
                    return result;
                } else {
                    final HashMap<String, Object> result = new HashMap<String, Object>();
                    try {
                        final Scope newScope = scope.copy();
                        for (final Getter key : ClassCache.getClassCache(last.getClass()).getGetter()) {
                            final Object o = key.getValue(last);
                            newScope.add(o, key);
                            if (container.isTrue(container.resolveValue(container, expression, newScope, false))) {
                                result.put(key.getKey(), o);
                            }
                            newScope.removeLast();
                        }
                    } catch (SecurityException e) {
                        throw new CompareException(e);
                    } catch (NoSuchMethodException e) {
                        throw new CompareException(e);
                    } catch (IllegalArgumentException e) {
                        throw new CompareException(e);
                    } catch (IllegalAccessException e) {
                        throw new CompareException(e);
                    } catch (InvocationTargetException e) {
                        if (e.getTargetException() instanceof InterruptedException) {
                            Thread.currentThread().interrupt();
                        }
                    } catch (CompareException e) {
                        throw new CompareException(e);
                    }
                    return result;
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
        public Object opEval(Condition<?> container, Object expression, Scope scope) throws CompareException {
            Object last = scope.getLast();
            if (last == KEY_DOES_NOT_EXIST) {
                return false;
            } else if (last == null || Clazz.isPrimitive(last.getClass()) || last instanceof String) {
                return container.resolveValue(container, expression, scope, false);
            } else {
                try {
                    Condition filter = null;
                    Object options = container.get($OPTIONS);
                    if (options != null && options instanceof Condition) {
                        filter = (Condition) ((Condition) options).get("filter");
                    }
                    for (String g : container.listKeys(last)) {
                        final Scope newScope = container.resolveKeyPath(scope, JSPath.fromPathString(g));
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
                } catch (SecurityException e) {
                    throw new CompareException(e);
                } catch (InvalidPathException e) {
                    throw new CompareException(e);
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
        public Object opEval(Condition<?> container, Object expression, Scope scope) throws CompareException {
            boolean isNEQ = false;
            return opEvalInternal(container, expression, scope, isNEQ);
        }

        /**
         * @param container
         * @param expression
         * @param scope
         * @param isNEQ
         * @param path
         *            TODO
         * @return
         * @throws CompareException
         */
        protected Object opEvalInternal(Condition container, Object expression, Scope scope, boolean isNEQ) throws CompareException {
            boolean queryMode = true;
            Object matcherValue = scope.getLast();
            Object options = container.get($OPTIONS);
            if (options != null && options instanceof Condition) {
                if (Boolean.TRUE.equals(((Condition) options).get(OPTIONS_AGGREGATE))) {
                    queryMode = false;
                }
            }
            // Object resolvedExpression = container.resolveValue(expression, matcher);
            if (queryMode) {
                // { tags: { $eq: "B" } } equals
                expression = container.convertSpecialTypes(expression, matcherValue);
                if (container.equalsDeep(container, expression, matcherValue, scope)) {
                    if (container._isDebug()) {
                        container.log(scope.getPath(), getClass().getSimpleName() + " " + expression + ".equals(" + scope.getLast() + ")" + " = true " + "|Options: " + options);
                    }
                    return true;
                }
                boolean disableMongoDBListSpecial = false;
                if (options != null && options instanceof Condition) {
                    if (Boolean.TRUE.equals(((Condition) options).get(DISABLE_LIST_SPECIAL_HANDLING))) {
                        disableMongoDBListSpecial = true;
                    }
                }
                // this is not available for $ne
                if (ReflectionUtils.isListOrArray(matcherValue) && !disableMongoDBListSpecial && !isNEQ) {
                    // Match an Array Value
                    // If the specified <value> is an array, MongoDB matches documents where the <field> matches the array exactly or
                    // the
                    // <field> contains an element that matches the array exactly. The order of the elements matters. For an example,
                    // see
                    // Equals
                    // an Array Value.
                    // the expression matches ANY of the matcherValues list entries
                    final List<Object> list = ReflectionUtils.wrapList(matcherValue, false, Object.class);
                    int i = 0;
                    for (final Object obj : list) {
                        // { tags: { $eq: "B" } } matches { tags: [ "A", "B" ] }
                        scope.add(obj, i++);
                        try {
                            if (container.equalsDeep(container, expression, obj, scope)) {
                                if (container._isDebug()) {
                                    container.log(scope.getPath(), getClass().getSimpleName() + " (Matcher List contains expression Mode)\"" + scope.getLast() + "\".equals(" + expression + ")" + " = true |Options: " + options);
                                }
                                return true;
                            }
                        } finally {
                            scope.removeLast();
                        }
                    }
                    if (container._isDebug()) {
                        container.log(scope.getPath(), getClass().getSimpleName() + " (Matcher List contains expression Mode)\"" + scope.getLast() + "\".contains(" + expression + ")" + " = false |Options: " + options);
                    }
                    return false;
                }
                if (container._isDebug()) {
                    container.log(scope.getPath(), getClass().getSimpleName() + " " + expression + ".equals(" + scope.getLast() + ")" + " = false |Options: " + options);
                }
                return false;
            } else {
                /**
                 * https://docs.mongodb.com/manual/reference/operator/aggregation/eq/
                 *
                 * @throws CompareException
                 */
                if (ReflectionUtils.isListOrArray(expression.getClass())) {
                    final List<Object> aggregation = ReflectionUtils.wrapList(expression, false, Object.class);
                    if (aggregation.size() == 2) {
                        Object x = (container.resolveValue(container, aggregation.get(0), scope, true));
                        Object y = (container.resolveValue(container, aggregation.get(1), scope, true));
                        return new AggregationResult(CompareUtils.equals(y, x));
                    }
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
        public Object opEval(Condition<?> container, Object expression, Scope scope) throws CompareException {
            Object matcherValue;
            if (ReflectionUtils.isListOrArray(expression)) {
                // aggregation
                matcherValue = (container.resolveValue(container, ReflectionUtils.getListElement(expression, 0), scope, true));
                expression = (container.resolveValue(container, ReflectionUtils.getListElement(expression, 1), scope, true));
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
        public Object opEval(Condition<?> container, Object expression, Scope scope) throws CompareException {
            final Object last = scope.getLast();
            if (ReflectionUtils.isListOrArray(last)) {
                return false;
            } else {
                expression = (container.resolveValue(container, expression, scope, true));
                boolean isPattern = false;
                if (last instanceof Pattern) {
                    isPattern = true;
                } else if (last instanceof String) {
                    try {
                        Pattern.compile((String) last);
                        isPattern = true;
                    } catch (Exception e) {
                        // return false;
                    }
                }
                if (expression == Boolean.TRUE) {
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
                protected boolean opEval(int compareResult) {
                    return compareResult <= 0;
                }
            },
            LT {
                @Override
                protected boolean opEval(int compareResult) {
                    return (compareResult < 0);
                }
            },
            GTE {
                @Override
                protected boolean opEval(int compareResult) {
                    return (compareResult >= 0);
                }
            },
            GT {
                @Override
                protected boolean opEval(int compareResult) {
                    return (compareResult > 0);
                }
            };
            protected abstract boolean opEval(int compareResult);
        }

        private final NumberOp.OP op;

        public NumberOp(NumberOp.OP op) {
            this.op = op;
        }

        @Override
        public Object opEval(Condition<?> container, Object expression, Scope scope) throws CompareException {
            Object before = expression;
            final Object matcherValue = scope.getLast();
            Condition root = ROOT_CONDITION.get();
            if (ReflectionUtils.isListOrArray(expression)) {
                // aggregation
                final List<Number> aggregation = ReflectionUtils.wrapList(expression, false, Number.class);
                final Number a = (Number) (container.resolveValue(container, aggregation.get(0), scope, true));
                final Number b = (Number) (container.resolveValue(container, aggregation.get(1), scope, true));
                boolean ret = op.opEval(CompareUtils.compareNumber(a, b));
                if (container._isDebug()) {
                    container.log(scope.getPath(), getClass().getSimpleName() + " \"" + aggregation.get(0) + "\"." + op.name() + "(" + aggregation.get(1) + ")" + " = " + ret);
                    ;
                }
                return new AggregationResult(ret);
            } else {
                expression = (container.resolveValue(container, expression, scope, true));
                if (matcherValue == KEY_DOES_NOT_EXIST) {
                    if (container._isDebug()) {
                        container.log(scope.getPath(), getClass().getSimpleName() + " \"" + KEY_DOES_NOT_EXIST + "\"." + op.name() + "(" + expression + ")" + " = " + false);
                        ;
                    }
                    return false;
                } else if (expression instanceof Condition) {
                    final Number a = (Number) matcherValue;
                    final Number b = (Number) (container.resolveValue(container, expression, scope, true));
                    boolean ret = op.opEval(CompareUtils.compareNumber(a, b));
                    if (container._isDebug()) {
                        container.log(scope.getPath(), getClass().getSimpleName() + ": " + a + "." + op.name() + "(" + b + ")" + " = " + ret);
                        ;
                    }
                    return ret;
                } else {
                    expression = container.convertSpecialTypes(expression, matcherValue);
                    if (matcherValue instanceof Number && expression instanceof Number) {
                        final Number a = (Number) matcherValue;
                        final Number b = (Number) expression;
                        boolean ret = op.opEval(CompareUtils.compareNumber(a, b));
                        if (container._isDebug()) {
                            container.log(scope.getPath(), getClass().getSimpleName() + ": " + a + "." + op.name() + "(" + b + ")" + " = " + ret);
                        }
                        return ret;
                    } else if (matcherValue instanceof Comparable && expression instanceof Comparable) {
                        boolean ret = op.opEval(CompareUtils.compare((Comparable) matcherValue, (Comparable) expression));
                        if (container._isDebug()) {
                            container.log(scope.getPath(), getClass().getSimpleName() + " \"" + matcherValue + "\"." + op.name() + "(" + expression + ")" + " = " + ret);
                        }
                        return ret;
                    }
                }
            }
            throw new CompareException("Unsupported expression: " + expression + " on " + matcherValue);
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
        public Object opEval(Condition<?> container, Object expression, Scope scope) throws CompareException {
            Object matcherValue = scope.getLast();
            expression = container.resolveValue(container, expression, scope, true);
            if (matcherValue == KEY_DOES_NOT_EXIST) {
                if (container._isDebug()) {
                    container.log(scope.getPath(), getClass().getSimpleName() + ": KEY_DOES_NOT_EXIST -> false");
                }
                return false;
            } else if (!ReflectionUtils.isListOrArray(expression)) {
                throw new CompareException("Operator expects an array as parameter");
            } else if (!ReflectionUtils.isListOrArray(matcherValue)) {
                // Use the $in Operator to Match Values
                final List<Object> listExpression = ReflectionUtils.wrapUnmodifiableList(expression, Object.class);
                for (final Object exp : listExpression) {
                    if (container.equalsDeep(container, exp, matcherValue, scope)) {
                        if (container._isDebug()) {
                            container.log(scope.getPath(), getClass().getSimpleName() + ": 'does matcher match any expression-list-element-mode':  " + matcherValue + ".matches(" + exp + ") = true");
                        }
                        return true;
                    }
                }
                if (container._isDebug()) {
                    container.log(scope.getPath(), getClass().getSimpleName() + ": 'does matcher match any expression-list-element-mode':  " + matcherValue + " does not match any of " + expression);
                }
                return false;
            } else {
                final List<Object> listExpression = ReflectionUtils.wrapUnmodifiableList(expression, Object.class);
                final List<Object> listMatcherValues = ReflectionUtils.wrapUnmodifiableList(matcherValue, Object.class);
                for (final Object exp : listExpression) {
                    for (Object matcher : listMatcherValues) {
                        if (container.equalsDeep(container, exp, matcher, scope)) {
                            if (container._isDebug()) {
                                container.log(scope.getPath(), getClass().getSimpleName() + ": 'does any matcher-list-element match any expression-list-element-mode':  " + matcher + ".matches(" + exp + ") = true");
                            }
                            return true;
                        }
                    }
                }
                if (container._isDebug()) {
                    container.log(scope.getPath(), getClass().getSimpleName() + ": 'oes any matcher-list-element match any expression-list-element-mode':  " + matcherValue + " has no element that matches any element of " + expression);
                }
                return false;
            }
        }
    }

    /**
     * @param value
     * @param key
     * @return
     * @throws CompareException
     * @throws InvocationTargetException
     * @throws IllegalArgumentException
     * @throws IllegalAccessException
     */
    public static class KeyOnClass {
        private final Class<? extends Object> class1;
        private final int                     hashCode;
        private final String                  key;

        /**
         * @param class1
         * @param key
         */
        public KeyOnClass(Class<? extends Object> class1, String key) {
            this.class1 = class1;
            this.key = key;
            this.hashCode = class1.hashCode() + key.hashCode();
        }

        /*
         * (non-Javadoc)
         *
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (obj == this) {
                return true;
            } else if (obj == null) {
                return false;
            } else if (!(obj instanceof KeyOnClass)) {
                return false;
            } else if (!class1.equals(((KeyOnClass) obj).class1)) {
                return false;
            } else if (!key.equals(((KeyOnClass) obj).key)) {
                return false;
            } else {
                return true;
            }
        }

        /*
         * (non-Javadoc)
         *
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            return hashCode;
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
        public Object opEval(Condition<?> container, Object expression, Scope scope) throws CompareException {
            if (expression instanceof Number) {
                return expression;
            }
            Number result = null;
            if (ReflectionUtils.isListOrArray(expression.getClass())) {
                final int length = ReflectionUtils.getListLength(expression);
                for (int index = 0; index < length; index++) {
                    Object value = ReflectionUtils.getListElement(expression, index);
                    /**
                     * If some, but not all, documents for the $max operation have either a null value for the field or are missing the
                     * field, the $max operator only considers the non-null and the non-missing values for the field.
                     */
                    Number num = null;
                    try {
                        /**
                         * With a single expression as its operand, if the expression resolves to an array, $max traverses into the array to
                         * operate on the numerical elements of the array to return a single value. With a list of expressions as its
                         * operand, if any of the expressions resolves to an array, $max does not traverse into the array but instead treats
                         * the array as a non-numerical value
                         */
                        if (ReflectionUtils.isListOrArray(value.getClass()) && length == 1) {
                            num = (Number) opEval(container, value, scope);
                            // num = aggregate(container, value, obj);
                        } else {
                            value = (container.resolveValue(container, value, scope, true));
                            // value = getNumber(container, value, obj, false);
                            if (value instanceof Number) {
                                num = (Number) value;
                            }
                            if (ReflectionUtils.isListOrArray(value.getClass())) {
                                num = (Number) opEval(container, value, scope);
                            }
                        }
                    } catch (AggregationException e) {
                        continue;
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
        public Object opEval(Condition<?> container, Object expression, Scope scope) throws CompareException {
            if (expression instanceof Number) {
                return expression;
            }
            Number result = null;
            if (ReflectionUtils.isListOrArray(expression.getClass())) {
                final int length = ReflectionUtils.getListLength(expression);
                for (int index = 0; index < length; index++) {
                    Object value = ReflectionUtils.getListElement(expression, index);
                    /**
                     * If some, but not all, documents for the $max operation have either a null value for the field or are missing the
                     * field, the $max operator only considers the non-null and the non-missing values for the field.
                     */
                    Number num = null;
                    try {
                        /**
                         * With a single expression as its operand, if the expression resolves to an array, $max traverses into the array to
                         * operate on the numerical elements of the array to return a single value. With a list of expressions as its
                         * operand, if any of the expressions resolves to an array, $max does not traverse into the array but instead treats
                         * the array as a non-numerical value
                         */
                        if (ReflectionUtils.isListOrArray(value.getClass()) && length == 1) {
                            num = (Number) opEval(container, value, scope);
                            // num = aggregate(container, value, obj);
                        } else {
                            value = container.resolveValue(container, value, scope, true);
                            // value = getNumber(container, value, obj, false);
                            if (value instanceof Number) {
                                num = (Number) value;
                            }
                            if (ReflectionUtils.isListOrArray(value.getClass())) {
                                num = (Number) opEval(container, value, scope);
                            }
                        }
                    } catch (AggregationException e) {
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
        public Object opEval(Condition<?> container, Object expressions, Scope scope) throws CompareException {
            if (ReflectionUtils.isListOrArray(expressions.getClass())) {
                final List<Object> listExpressions = ReflectionUtils.wrapUnmodifiableList(expressions, Object.class);
                Number result = null;
                for (final Object expression : listExpressions) {
                    final Number num = (Number) container.resolveValue(container, expression, scope, true);
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
        public Object opEval(Condition<?> container, Object expression, Scope scope) throws CompareException {
            // the field does not exist.
            final Object matcherValue = scope.getLast();
            if (matcherValue == KEY_DOES_NOT_EXIST) {
                return true;
            } else if (!ReflectionUtils.isListOrArray(expression)) {
                throw new CompareException("Operator expects an array as parameter");
            } else if (!ReflectionUtils.isListOrArray(matcherValue)) {
                // value is not a list
                final List<Object> listExpressions = ReflectionUtils.wrapUnmodifiableList(expression, Object.class);
                for (final Object exp : listExpressions) {
                    if (container.equalsDeep(container, exp, matcherValue, scope)) {
                        return false;
                    }
                }
                return true;
            } else {
                final List<Object> listExpression = ReflectionUtils.wrapUnmodifiableList(expression, Object.class);
                final List<Object> listMatcherValues = ReflectionUtils.wrapUnmodifiableList(matcherValue, Object.class);
                for (final Object exp : listExpression) {
                    for (Object matcher : listMatcherValues) {
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
        public Object opEval(Condition<?> container, Object expression, Scope scope) throws CompareException {
            if (scope.getLast() == KEY_DOES_NOT_EXIST) {
                // KEY_DOES_NOT_EXIST can never match anything. It does not match null neither!
                return true;
            } else {
                Object result = EQOP.opEvalInternal(container, expression, scope, true);
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
        public Object opEval(Condition<?> container, Object expression, Scope scope) throws CompareException {
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
        public Object opEval(Condition<?> container, Object expression, Scope scope) throws CompareException {
            final List<Object> listExpression = ReflectionUtils.wrapUnmodifiableList(expression, Object.class);
            for (Object exp : listExpression) {
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
         */
        Scope resolve(Scope oldScope, Scope newScope, Object key);
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
        public Object opEval(Condition<?> container, Object expression, Scope scope) throws CompareException {
            try {
                if (ReflectionUtils.isListOrArray(expression)) {
                    // container.resolveValue(expression, scope)
                    // }
                    Object matcherObject = container.resolveValue(container, ReflectionUtils.getListElement(expression, 0), scope, true);
                    if (matcherObject == KEY_DOES_NOT_EXIST) {
                        return null;
                    }
                    Pattern pattern = (Pattern) container.getCache(RegexFindOp.class);
                    if (pattern == null) {
                        Object patternString = container.resolveValue(container, ReflectionUtils.getListElement(expression, 1), scope, true);
                        pattern = Pattern.compile(String.valueOf(patternString));
                        container.putCache(RegexFindOp.class, pattern);
                    }
                    int groupIndex = 1;
                    if (ReflectionUtils.getListLength(expression) > 2) {
                        Object obj = container.resolveValue(container, ReflectionUtils.getListElement(expression, 2), scope, true);
                        groupIndex = Integer.parseInt(String.valueOf(obj));
                    }
                    Matcher matcher = pattern.matcher(toCharSequence(matcherObject));
                    if (matcher.find()) {
                        return matcher.group(groupIndex);
                    } else {
                        return null;
                    }
                }
            } catch (CompareException e) {
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
         * @throws CompareException
         */
        private int getFlags(Condition container) throws CompareException {
            int flags = 0;
            Object options = container.get($OPTIONS);
            if (options == null) {
                return flags;
            }
            if (options instanceof Condition) {
                options = ((Condition) options).get("flags");
                if (options == null) {
                    return flags;
                }
            }
            for (int i = 0; i < ((String) options).length(); i++) {
                switch (((String) options).charAt(i)) {
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
                    throw new CompareException("Unsupported Regex Option");
                }
            }
            return flags;
        }

        protected boolean matchPattern(final Object matcherValue, final Pattern pattern) {
            // no resolve, because matcherValue is from the matcherObject and does not get resolved/evaluated
            if (matcherValue == null) {
                return false;
            } else {
                final Matcher matcher = pattern.matcher("");
                final List<Object> matcherValues = ReflectionUtils.wrapUnmodifiableList(matcherValue, Object.class);
                if (matcherValues != null) {
                    // the expression matches ANY of the matcherValues list entries
                    for (final Object matcherObject : matcherValues) {
                        if (matcherObject != null && matcher.reset(toCharSequence(matcherObject)).matches()) {
                            return true;
                        }
                    }
                }
                if (matcher.reset(toCharSequence(matcherValue)).matches()) {
                    return true;
                }
                return false;
            }
        }

        /**
         *
         */
        @Override
        public Object opEval(Condition<?> container, Object expression, Scope scope) throws CompareException {
            final Object matcherValue = scope.getLast();
            if (matcherValue == KEY_DOES_NOT_EXIST) {
                return false;
            } else {
                try {
                    expression = container.resolveValue(container, expression, scope, true);
                    Pattern pattern = (Pattern) container.getCache(RegexOp.class);
                    if (ReflectionUtils.isListOrArray(expression)) {
                        // aggregation mode
                        if (pattern == null) {
                            pattern = Pattern.compile(String.valueOf(container.resolveValue(container, ReflectionUtils.getListElement(expression, 0), scope, true)), getFlags(container));
                        }
                        container.putCache(RegexOp.class, pattern);
                        return matchPattern(container.resolveValue(container, ReflectionUtils.getListElement(expression, 1), scope, true), pattern);
                    } else if (expression instanceof Pattern) {
                        pattern = (Pattern) expression;
                        container.putCache(RegexOp.class, pattern);
                        return matchPattern(matcherValue, pattern);
                    } else if (expression instanceof String) {
                        if (pattern == null) {
                            expression = container.resolveValue(container, expression, scope, true);
                            pattern = Pattern.compile((String) expression, getFlags(container));
                        }
                        container.putCache(RegexOp.class, pattern);
                        return matchPattern(matcherValue, pattern);
                    }
                } catch (PatternSyntaxException e) {
                    throw new CompareException(e);
                } catch (IllegalArgumentException e) {
                    throw new CompareException(e);
                }
                throw new CompareException("Operator expects a String or a Pattern(is not serializable) type as parameter");
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
        public ResolverOPHandler(String key, ConditionResolver<Input, Output> resolver) {
            this.key = key;
            this.resolver = resolver;
        }

        @Override
        public Object opEval(Condition<?> container, Object query, Scope matcher) throws CompareException {
            if (query instanceof Condition) {
                final Condition cQuery = (Condition) query;
                @SuppressWarnings("unchecked")
                final Map<String, Object> options = (Map<String, Object>) cQuery.get(Condition.$OPTIONS);
                if (options == null) {
                    throw new CompareException("§options property is missing. Add §options." + key.substring(1));
                }
                final Input parameters = (Input) options.get(key.substring(1));
                if (parameters == null) {
                    throw new CompareException("§options." + key.substring(1) + " property is missing");
                }
                final Output resolved = resolver.resolve(parameters);
                final ArrayList<Object> newScopeObjects = new ArrayList<Object>(matcher.getScope());
                // Resolver convert types to a "Wrapper" Type, (FileContext?) the original instance must be removed from the scope hirarchy,
                // because $parent would not work
                newScopeObjects.set(newScopeObjects.size() - 1, resolved);
                return container.resolveValue(container, cQuery, new Scope(newScopeObjects, matcher.getPath()), false);
            }
            throw new CompareException();
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
        public Object opEval(Condition<?> container, Object expressions, Scope scope) throws CompareException {
            if (ReflectionUtils.isListOrArray(expressions.getClass())) {
                final List<Object> listExpressions = ReflectionUtils.wrapUnmodifiableList(expressions, Object.class);
                Number result = null;
                for (final Object expression : listExpressions) {
                    final Number num = (Number) container.resolveValue(container, expression, scope, true);
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
            throw new AggregationException("SubstracOp:" + (container) + "|" + (expressions));
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
        public Object opEval(Condition<?> container, Object expression, Scope scope) throws CompareException {
            if (expression instanceof Number) {
                return expression;
            }
            if (ReflectionUtils.isListOrArray(expression.getClass())) {
                final int length = ReflectionUtils.getListLength(expression);
                Number result = null;
                for (int index = 0; index < length; index++) {
                    Object value = ReflectionUtils.getListElement(expression, index);
                    /**
                     * If used on a field that does not exist in any document in the collection, $sum returns 0 for that field.
                     */
                    Number num = 0;
                    try {
                        /**
                         * With a single expression as its operand, if the expression resolves to an array, $sum traverses into the array to
                         * operate on the numerical elements of the array to return a single value. With a list of expressions as its
                         * operand, if any of the expressions resolves to an array, $sum does not traverse into the array but instead treats
                         * the array as a non-numerical value
                         */
                        if (ReflectionUtils.isListOrArray(value.getClass()) && length == 1) {
                            // num = aggregate(container, value, matcherValue);
                            num = (Number) opEval(container, value, scope);
                        } else {
                            value = container.resolveValue(container, value, scope, true);
                            // value = getNumber(container, value, matcherValue, false);
                            if (value instanceof Number) {
                                num = (Number) value;
                            }
                            if (ReflectionUtils.isListOrArray(value.getClass())) {
                                // num = aggregate(container, value, obj);
                                num = (Number) opEval(container, value, scope);
                            }
                        }
                    } catch (AggregationException e) {
                        continue;
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
    public static final String                                 $$CURRENT                 = "§§CURRENT";
    @StorableDoc("Path Modifier: References the root document, i.e. the top-level document.")
    public static final String                                 $$ROOT                    = "§§ROOT";
    @ApiDoc("Path Modifier:  §§THIS references the current scope object or field. Note: All field identifiers that start with §§ are absolute identifiers and MUST be places as first key element")
    @ApiDocExample("{'c':{'§gt':[{'§sum':['§§this',1]},0}}  '§§this' references to the field c, adds 1 and checks if the result is greater than 0")
    public static final String                                 $$THIS                    = "§§THIS";
    public static final String                                 $ADD                      = "§add";
    public static final String                                 $AND                      = "§and";
    /**
     *
     */
    public static final String                                 $ANY                      = "§any";
    @ApiDoc("Aggregation OP: concat all values to a single string")
    private static final String                                $CONCAT                   = "§concat";
    public static final String                                 $DIVIDE                   = "§divide";
    public static final String                                 $EACH                     = "§each";
    /**
     * Specifies equality condition. The $eq operator matches documents where the value of a field equals the specified value. WARNING:
     * There is a difference to MongoDb $EQ: If the specified <value> is a document, the order of the fields in the document DOES NOT
     * MATTER! <br>
     * Match an Array Value If the specified <value> is an array, MongoDB matches documents where the <field> matches the array exactly or
     * the <field> contains an element that matches the array exactly. The order of the elements matters. For an example, see Equals an
     * Array Value.
     */
    public static final String                                 $EQ                       = "§eq";
    /**
     *
     */
    public static final String                                 $EXISTS                   = "§exists";
    public static final String                                 $IS_REGEX                 = "§isRegex";
    /**
     * $gt selects those documents where the value of the field is greater than (i.e. >) the specified value.
     */
    public static final String                                 $GT                       = "§gt";
    /**
     * $gte selects the documents where the value of the field is greater than or equal to (i.e. >=) a specified value (e.g. value.)
     */
    public static final String                                 $GTE                      = "§gte";
    public static final String                                 $IGNORE_GETTER_EXCEPTIONS = "§ignoreGetterErrors";
    /**
     * The $in operator selects the documents where the value of a field equals any value in the specified array. To specify an $in
     * expression, use the following prototype:
     *
     * If the field holds an array, then the $in operator selects the documents whose field holds an array that contains at least one
     * element that matches a value in the specified array (e.g. <value1>, <value2>, etc.)
     *
     *
     */
    public static final String                                 $IN                       = "§in";
    /**
     *
     */
    @ApiDoc("Path modifier. a.§keys returns all keys of the map a or all indizes of the list a")
    @ApiDocExample("a.§keys")
    public static final String                                 $KEYS                     = "§keys";
    /**
     *
     */
    public static final String                                 $LT                       = "§lt";
    /**
     *
     */
    public static final String                                 $LTE                      = "§lte";
    public static final String                                 $MAX                      = "§max";
    public static final String                                 $MIN                      = "§min";
    public static final String                                 $MULTIPLY                 = "§multiply";
    /**
     * $ne selects the documents where the value of the field is not equal to the specified value. This includes documents that do not
     * contain the field.
     */
    public static final String                                 $NE                       = "§ne";
    /**
     *
     */
    public static final String                                 $NIN                      = "§nin";
    /**
     *
     */
    public static final String                                 $NOT                      = "§not";
    /**
     * Returns the current datetime value, which is same across all members of the deployment and remains constant throughout the
     * aggregation pipeline.
     */
    public static final String                                 $NOW                      = "§§NOW";
    /**
     * Regex options https://docs.mongodb.com/manual/reference/operator/expression/regex/#op._S_options
     */
    public static final String                                 $OPTIONS                  = "§options";
    /**
     *
     */
    public static final String                                 $OR                       = "§or";
    /**
     *
     */
    @ApiDoc("Path traversal identifier. Used to access parent fields in the matcher object - relative to the current scope. Usage is like ../../ in directory pathes")
    public static final String                                 $PARENT                   = "§PARENT";
    public static final String                                 $REGEX                    = "§regex";
    @ApiDoc("Aggregation OP: find a match in a string via regex\r\nParam 1:string\r\nParam 2:regex\r\nParam 3: matching group index(optional)")
    public static final String                                 $REGEX_FIND_ONE           = "§regexFindOne";
    @ApiDoc("Virtual field identifier. a.§size references length of an array, string, map or other objects")
    @ApiDocExample("a.§size")
    public static final String                                 $SIZE                     = "§size";
    public static final String                                 $SUBTRACT                 = "§subtract";
    public static final String                                 $SUM                      = "§sum";
    /**
     *
     */
    @ApiDoc("Virtual field identifier. a.§type references the ClassName of 'a'")
    @ApiDocExample("a.§type")
    public static final String                                 $TYPE                     = "§type";
    private static final Class[]                               EMPTY                     = new Class[] {};
    protected static final EqOp                                EQOP                      = new EqOp();
    private static HashSet<String>                             IGNORE                    = new HashSet<String>();
    private static final Object                                KEY_DOES_NOT_EXIST        = new Object() {
                                                                                             @Override
                                                                                             public String toString() {
                                                                                                 return "KEY_DOES_NOT_EXIST";
                                                                                             }
                                                                                         };
    private final static ThreadLocal<Long>                     now                       = new ThreadLocal<Long>();
    private final static ThreadLocal<Condition>                ROOT_CONDITION            = new ThreadLocal<Condition>();
    public static final ThreadLocal<Map<String, OpHandler>>    OPERATIONS                = new ThreadLocal<Map<String, OpHandler>>();
    private static final HashMap<String, Operator>             OPS                       = new HashMap<String, Operator>();
    /**
     * used as $options to force a Condition get handled as Aggregate COndition
     */
    @ApiDoc("Set this option to true to force all operators in the same layer to work in aggregation mode.")
    @ApiDocExample("{§eq:['§a',1],§options:{'aggregate':true}}")
    public static final String                                 OPTIONS_AGGREGATE         = "aggregate";
    public static final ThreadLocal<Map<String, PathHandler>>  PATH_HANDLERS             = new ThreadLocal<Map<String, PathHandler>>();
    /**
     *
     */
    private static final long                                  serialVersionUID          = 1L;
    public static final org.appwork.storage.TypeRef<Condition> TYPE                      = new org.appwork.storage.TypeRef<Condition>(Condition.class) {
                                                                                         };
    static {
        IGNORE.add($OPTIONS);
        IGNORE.add($IGNORE_GETTER_EXCEPTIONS);
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
        OPS.put($ADD, new AddOp());
        OPS.put($SUBTRACT, new SubtractOp());
        OPS.put($SUM, new SumOp());
        OPS.put($MIN, new MinOp());
        OPS.put($MAX, new MaxOp());
        OPS.put($DIVIDE, new DivideOp());
        OPS.put($MULTIPLY, new MultiplyOp());
        OPS.put($CONCAT, new ConcatOp());
        OPS.put($REGEX_FIND_ONE, new RegexFindOp());
    }

    public static OpHandler putThreadOPHandler(final String key, final OpHandler handler) {
        Map<String, OpHandler> map = Condition.OPERATIONS.get();
        if (map == null) {
            map = new HashMap<String, Condition.OpHandler>();
        }
        return map.put(key, handler);
    }

    /**
     * @param resolveValue
     * @return
     */
    public Object unwrap(Object resolve) {
        if (resolve instanceof ConditionResult) {
            return ((ConditionResult) resolve).getValue();
        }
        return resolve;
    }

    /**
     * @param expression
     * @return
     * @throws CompareException
     */
    public Date toDate(Object expression) throws CompareException {
        if (expression == null) {
            return null;
        }
        if (expression instanceof Date) {
            return ((Date) expression);
        }
        if (expression instanceof String) {
            try {
                // coupling to flexi.. not nice, but I do not want code duplication as well
                return DateMapper.parseJsonDefault(String.valueOf(expression));
            } catch (FlexiMapperException e) {
                throw new CompareException(e);
            }
        }
        if (expression instanceof Number) {
            return new Date(((Number) expression).longValue());
        }
        throw new CompareException("Cannot compare " + expression + " to a Date instance");
    }

    /**
     * @param expression
     * @return
     * @throws CompareException
     */
    public TimeSpan toTimeSpan(Object expression) throws CompareException {
        if (expression == null) {
            return null;
        }
        if (expression instanceof TimeSpan) {
            return ((TimeSpan) expression);
        }
        if (expression instanceof String) {
            try {
                return TimeSpan.parse(String.valueOf(expression));
            } catch (InvalidTimeSpanException e) {
                throw new CompareException(e);
            }
        }
        if (expression instanceof Number) {
            return TimeSpan.fromMillis(((Number) expression).longValue());
        }
        throw new CompareException("Cannot compare " + expression + " to a TimeSpan instance");
    }

    public static <Input, Output> OpHandler putThreadResolver(final String key, final ConditionResolver<Input, Output> resolver) {
        Map<String, OpHandler> map = Condition.OPERATIONS.get();
        if (map == null) {
            map = new HashMap<String, Condition.OpHandler>();
        }
        OpHandler ret = map.put(key, new ResolverOPHandler<Input, Output>(key, resolver));
        Condition.OPERATIONS.set(map);
        return ret;
    }

    /**
     * @param handler
     * @return
     */
    private static boolean removeThreadOPHandler(String key) {
        Map<String, OpHandler> map = Condition.OPERATIONS.get();
        if (map == null) {
            return false;
        }
        if (map.remove(key) != null) {
            Condition.OPERATIONS.set(map);
        }
        return false;
    }

    public static boolean removeThreadResolver(String key) {
        return removeThreadOPHandler(key);
    }

    protected volatile HashMap<KeyOnClass, Condition.AccessMethod> accessCache    = new HashMap<KeyOnClass, AccessMethod>();
    protected volatile HashMap<Object, Object>                     cache          = new HashMap<Object, Object>();
    protected HashMap<String, PathHandler>                         customPathhandlers;
    protected final boolean                                        useAccessCache = true;
    private LogInterface                                           logger;

    public LogInterface _getLogger() {
        return logger;
    }

    public void _setLogger(LogInterface logger) {
        this.logger = logger;
    }

    public static final String $FIRST = "§first";

    /**
     *
     */
    public Condition() {
    }

    public Condition(Map<String, Object> condition) {
        super(condition);
    }

    /**
     * @param string
     * @param i
     */
    public Condition(String key, Object o) {
        append(key, o);
    }

    /**
     * @param key
     * @param o
     */
    public Condition append(String key, Object o) {
        put(key, o);
        return this;
    }

    /*
     * (non-Javadoc)
     *
     * @see java.util.HashMap#clear()
     */
    @Override
    public void clear() {
        clearCache();
        super.clear();
    }

    protected void clearCache() {
        if (cache.size() > 0) {
            cache = new HashMap<Object, Object>();
        }
        if (accessCache.size() > 0) {
            accessCache = new HashMap<KeyOnClass, AccessMethod>();
        }
    }

    public boolean equalsDeep(Condition container, final Object expressionA, Object expressionB, Scope scope) throws CompareException {
        if (expressionA == expressionB) {
            return true;
        } else if (expressionA == null && expressionB != null) {
            return false;
        } else if (expressionB == null) {
            return false;
        } else if (equalsShallow(container, expressionA, expressionB, scope)) {
            // if equals says these objects equal, we trust
            return true;
        } else if (ReflectionUtils.isListOrArray(expressionA) && ReflectionUtils.isListOrArray(expressionB)) {
            final List<Object> listExpression = ReflectionUtils.wrapUnmodifiableList(expressionA, Object.class);
            final List<Object> listMatcher = ReflectionUtils.wrapUnmodifiableList(expressionB, Object.class);
            if (listExpression.size() != listMatcher.size()) {
                return false;
            } else {
                final ListIterator<Object> e1 = listExpression.listIterator();
                final ListIterator<Object> e2 = listMatcher.listIterator();
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
                            Object result = ((Condition) o1).evaluateInternal(scope);
                            if (isFalse(result)) {
                                return false;
                            }
                        } else {
                            if (!equalsDeep(container, o1, o2, scope)) {
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
            Object result = ((Condition) expressionA).evaluateInternal(scope);
            return ((Condition) expressionA).isTrue(result);
        } else if (expressionA instanceof Map && expressionB instanceof Map) {
            DebugMode.debugger();
            // this cannot be reached?
            if (((Map<?, ?>) expressionA).size() != ((Map<?, ?>) expressionB).size()) {
                return false;
            }
            if (!CompareUtils.equals(((Map<?, ?>) expressionA).keySet(), ((Map<?, ?>) expressionB).keySet())) {
                return false;
            }
            for (java.util.Map.Entry<?, ?> es : ((Map<?, ?>) expressionA).entrySet()) {
                try {
                    // scope.add(es.getValue());
                    // discuss...change scope`?
                    if (!equalsDeep(container, es.getValue(), ((Map<?, ?>) expressionB).get(es.getKey()), scope)) {
                        return false;
                    }
                } finally {
                    // scope.removeLast();
                }
            }
            return true;
        } else if (expressionA.getClass() == expressionB.getClass()) {
            if (Clazz.isPrimitive(expressionA.getClass()) || Clazz.isEnum(expressionA.getClass()) || Clazz.isString(expressionA.getClass())) {
                // if true, this would have exited in the } else if (objectX.equals(objectY)) { block above
                return false;
            }
            ClassCache cc;
            try {
                cc = ClassCache.getClassCache(expressionA.getClass());
                for (Getter c : cc.getGetter()) {
                    try {
                        // scope.add(c.getValue(matcherValue));
                        // discuss...change scope`?
                        if (!equalsDeep(container, c.getValue(expressionA), c.getValue(expressionB), scope)) {
                            return false;
                        }
                    } finally {
                        // scope.removeLast();
                    }
                }
                return true;
            } catch (SecurityException e) {
                throw new WTFException(e);
            } catch (NoSuchMethodException e) {
                return false;
            } catch (IllegalArgumentException e) {
                throw new WTFException(e);
            } catch (IllegalAccessException e) {
                throw new WTFException(e);
            } catch (InvocationTargetException e) {
                throw new WTFException(e);
            }
        } else {
            return expressionA.equals(expressionB);
        }
    }

    public boolean equalsShallow(Condition container, Object expression, Object matcherValue, Scope scope) throws CompareException {
        if (expression == matcherValue) {
            return true;
        }
        if (expression == null || matcherValue == null) {
            return false;
        }
        Object resolvedExpression = resolveValue(container, expression, scope, false);
        if (resolvedExpression == matcherValue) {
            DebugMode.debugger();
            return true;
        }
        if (resolvedExpression instanceof AggregationResult && ((AggregationResult) resolvedExpression) == matcherValue) {
            DebugMode.debugger();
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
            Object options = get($OPTIONS);
            if (options != null && options instanceof Map) {
                if (Boolean.TRUE.equals(((Condition) options).get(CASE_INSENSITIVE))) {
                    return StringUtils.equalsIgnoreCase((String) resolvedExpression, (String) matcherValue);
                }
            }
        }
        if (resolvedExpression instanceof ConditionResult) {
            // DebugMode.debugger();
            resolvedExpression = container.unwrap(resolvedExpression);
        }
        return CompareUtils.equals(matcherValue, resolvedExpression);
    }

    /**
     * @param test
     * @return
     * @throws CompareException
     */
    public Object evaluate(Object matcher) throws CompareException {
        return unwrap(evaluateInternal(new Scope(matcher)));
    }

    // MAY return AggregationResults!
    public Object evaluateInternal(Scope scope) throws CompareException {
        final boolean rootFlag;
        if (now.get() == null) {
            now.set(System.currentTimeMillis());
            rootFlag = true;
        } else {
            rootFlag = false;
        }
        if (ROOT_CONDITION.get() == null) {
            ROOT_CONDITION.set(this);
        }
        try {
            fixInternalMapToCondition();
            Object lastResult = null;
            boolean lastResultDefined = false;
            HashSet<String> keysMustBeEvaluated = null;
            Object options = get($OPTIONS);
            SKIP: if (scope.getPath().size() > 0) {
                final Object last = scope.getLast();
                if (options instanceof Map) {
                    if (((Map) options).get(FILTER_ROOT) == Boolean.TRUE) {
                        break SKIP;
                    } else if (((Map) options).get(FILTER_ROOT) == Boolean.FALSE) {
                        keysMustBeEvaluated = new HashSet<String>(listKeys(last));
                        break SKIP;
                    }
                }
                if (last == null) {
                    break SKIP;
                } else if (last instanceof String) {
                    break SKIP;
                } else if (ReflectionUtils.isListOrArray(scope.getLast())) {
                    break SKIP;
                } else if (last instanceof Collection) {
                    break SKIP;
                } else if (last == KEY_DOES_NOT_EXIST) {
                    break SKIP;
                } else if (Clazz.isPrimitive(last.getClass())) {
                    break SKIP;
                } else if (Clazz.isEnum(last.getClass())) {
                    break SKIP;
                }
                if (scope.getPath().getLast() instanceof String) {
                    String lastKey = (String) scope.getPath().getLast();
                    Operator lastOP = OPS.get(lastKey);
                    if (lastOP != null && lastOP.isFilterRoot()) {
                        break SKIP;
                    }
                }
                if (scope.getPath().size() >= 2) {
                    // Project Op adds the filtered key to the scope path, and thus the last element is never the OP itself
                    Object maybeProjectOp = scope.getPath().get(-2);
                    if (maybeProjectOp instanceof String) {
                        Operator lastOP = OPS.get(maybeProjectOp);
                        if (lastOP != null) {
                            if (lastOP instanceof ProjectOp) {
                                break SKIP;
                            }
                            if (lastOP instanceof AnyOp) {
                                break SKIP;
                            }
                            if (lastOP instanceof AndOp) {
                                break SKIP;
                            }
                        }
                    }
                }
                // if we are not in root,ALL keys must match - only the condition root works like a filter
                keysMustBeEvaluated = new HashSet<String>(listKeys(scope.getLast()));
            }
            boolean couldHandleAllKeys = true;
            NEXT_ENTRY: for (java.util.Map.Entry<String, Object> es : entrySet()) {
                final String key = es.getKey();
                if (IGNORE.contains(key)) {
                    // for internal use only.
                    continue;
                }
                // Object value = evaluateAggregationExpression(this, es.getValue(), obj);
                Map<String, OpHandler> localOps = OPERATIONS.get();
                Operator op = null;
                if (localOps != null) {
                    op = localOps.get(key);
                }
                if (op == null) {
                    op = OPS.get(key);
                }
                if (op != null) {
                    // contains operators - object does not have to match complete.
                    if (_isDebug()) {
                        log(scope.getPath(), "Operator " + scope.getLast() + "." + key + "(" + es.getValue() + ")");
                    }
                    keysMustBeEvaluated = null;
                    lastResultDefined = true;
                    scope.add(scope.getLast(), key);
                    try {
                        lastResult = op.opEval(this, es.getValue(), scope);
                        if (_isDebug()) {
                            log(scope.getPath(), "Operator " + key + " = " + lastResult);
                        }
                    } finally {
                        scope.removeLast();
                    }
                    // DebugMode.breakIf(lastResult instanceof AggregationResult, EMPTY);
                    if (isFalse(lastResult)) {
                        if (_isDebug()) {
                            log(scope.getPath(), "Operator " + key + " on " + scope.getLast() + " = false");
                        }
                        return false;
                    }
                } else {
                    // we cannot simply extend scope, because the key might be absolute and not related to the current scope
                    Scope newScope;
                    try {
                        JSPath keyPath = JSPath.fromPathString(key);
                        if (keysMustBeEvaluated != null) {
                            couldHandleAllKeys &= keysMustBeEvaluated.remove(String.valueOf(keyPath.getFirst()));
                        }
                        newScope = resolveKeyPath(scope, keyPath);
                        if (_isDebug()) {
                            log(newScope.getPath(), "Resolved to " + newScope.getLast());
                        }
                    } catch (InvalidPathException e) {
                        throw new CompareException(e);
                    }
                    Object expression = es.getValue();
                    if (expression instanceof Condition) {
                        lastResultDefined = true;
                        lastResult = ((Condition<?>) expression).evaluateInternal(newScope/* matcherValue, value */);
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
                    if (_isDebug()) {
                        log(scope.getPath(), " = false - too many properties in condition -> " + KEY_DOES_NOT_EXIST);
                    }
                    return false;
                }
            }
            if (keysMustBeEvaluated != null && keysMustBeEvaluated.size() > 0) {
                // expression did not evaluate all of the matcher Objects keys
                // condition{a:true} equals matcher {a:true,b:true} --> False
                if (_isDebug()) {
                    log(scope.getPath(), " = false - Keys not evaluated. Container is no filterRoot: " + keysMustBeEvaluated);
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
            if (ROOT_CONDITION.get() == this) {
                ROOT_CONDITION.set(null);
            }
        }
    }

    /**
     * returns true if lastResult is a false expression
     *
     * @param lastResult
     * @return
     */
    protected boolean isFalse(Object lastResult) {
        if (lastResult instanceof ConditionResult) {
            return ((ConditionResult) lastResult).isFalse();
        }
        return lastResult == Boolean.FALSE;
    }

    protected void fixInternalMapToCondition() throws CompareException {
        for (java.util.Map.Entry<String, Object> es : entrySet()) {
            // convert HashMap to Condition. Internal HashMaps may be created by deserializing Conditions. This is fixed in the first
            // run
            if (es.getValue() instanceof Map && !(es.getValue() instanceof Condition)) {
                Condition condition;
                condition = newInstance();
                condition.putAll((Map) es.getValue());
                replace(es.getKey(), es.getValue(), condition);
                es.setValue(condition);
            }
        }
    }

    protected AccessMethod getAccessMethod(final KeyOnClass key) {
        return useAccessCache ? accessCache.get(key) : null;
    }

    protected Object getCache(final Object key) {
        return useAccessCache ? cache.get(key) : null;
    }

    /**
     * @param field
     * @return
     */
    private boolean isForbiddenField(Field field) {
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
    private boolean isForbiddenMethod(Method method) {
        if (!Modifier.isPublic(method.getModifiers())) {
            return true;
        } else if (Clazz.isVoid(method.getReturnType())) {
            return true;
        } else {
            return false;
        }
    }

    public Scope resolveKeyPath(Scope scope, JSPath keyPath) throws CompareException {
        Scope ret = scope.copy();
        NEXT_PATH_ELEMENT: for (Object keyOrg : keyPath.getElements()) {
            if (keyOrg instanceof Condition) {
                Condition filter = (Condition) keyOrg;
                ret.add(filter.evaluateInternal(ret), keyOrg);
                continue NEXT_PATH_ELEMENT;
            }
            String key = StringUtils.valueOfOrNull(keyOrg);
            if (customPathhandlers != null) {
                for (java.util.Map.Entry<String, PathHandler> es : customPathhandlers.entrySet()) {
                    if (es.getKey() == null || es.getKey().equalsIgnoreCase(key)) {
                        Scope r = es.getValue().resolve(scope, ret, keyOrg);
                        if (r != null) {
                            ret = r;
                            continue NEXT_PATH_ELEMENT;
                        }
                    }
                }
            }
            Map<String, PathHandler> threadPathHandlers = PATH_HANDLERS.get();
            if (threadPathHandlers != null) {
                for (java.util.Map.Entry<String, PathHandler> es : threadPathHandlers.entrySet()) {
                    if (es.getKey() == null || es.getKey().equalsIgnoreCase(key)) {
                        Scope r = es.getValue().resolve(scope, ret, keyOrg);
                        if (r != null) {
                            ret = r;
                            continue NEXT_PATH_ELEMENT;
                        }
                    }
                }
            }
            if ($$ROOT.equalsIgnoreCase(key)) {
                if (key != keyPath.getFirst()) {
                    throw new CompareException("PathLink §§... must always be the first key element");
                }
                ret = new Scope(scope.getFirst());
                continue;
            } else if ($$THIS.equalsIgnoreCase(key) || $$CURRENT.equalsIgnoreCase(key)) {
                if (key != keyPath.getFirst()) {
                    throw new CompareException("PathLink §§... must always be the first key element");
                }
                // DebugMode.debugger();
                ret.add(scope.getLast(), keyOrg);
                // DebugMode.debugger();
                continue;
            } else if ($PARENT.equalsIgnoreCase(key)) {
                ret = ret.getParent();
                if (ret == null) {
                    ret = new Scope(KEY_DOES_NOT_EXIST);
                    onKeyDoesNotExist(scope, keyPath, ret);
                    return ret;
                }
                continue;
            } else if ($NOW.equalsIgnoreCase(key)) {
                ret = new Scope(now.get());
                continue;
            } else if ($TYPE.equalsIgnoreCase(key)) {
                ret.add(ret.getLast().getClass().getName(), key);
                continue;
            } else if ($KEYS.equalsIgnoreCase(key)) {
                ret.add(listKeys(ret.getLast()), keyOrg);
                continue;
            } else if ($KEY.equalsIgnoreCase(key)) {
                List<Object> loop = ret.getPath().getElements();
                int backlog = 0;
                while (backlog < loop.size()) {
                    // search last non-op key
                    Object el = loop.get(loop.size() - backlog - 1);
                    if (el instanceof String && OPS.containsKey(el)) {
                        backlog++;
                        continue;
                    } else {
                        ret.add(el, keyOrg);
                        break;
                    }
                }
                continue;
            } else if ($FIRST.equalsIgnoreCase(key)) {
                if (ReflectionUtils.isListOrArray(ret.getLast())) {
                    if (ReflectionUtils.getListLength(ret.getLast()) == 0) {
                        ret.add(KEY_DOES_NOT_EXIST, keyOrg);
                    } else {
                        ret.add(ReflectionUtils.getListElement(ret.getLast(), 0), keyOrg);
                    }
                } else if (ret.getLast() instanceof Collection) {
                    Iterator it = ((Collection) ret.getLast()).iterator();
                    if (!it.hasNext()) {
                        ret.add(KEY_DOES_NOT_EXIST, keyOrg);
                    } else {
                        ret.add(it.next(), keyOrg);
                    }
                } else if (ret.getLast() instanceof Map) {
                    if (((Map) ret.getLast()).size() == 0) {
                        ret.add(KEY_DOES_NOT_EXIST, keyOrg);
                    } else {
                        ret.add(((Map) ret.getLast()).values().iterator().next(), keyOrg);
                    }
                } else {
                    ret.add((ret.getLast()), keyOrg);
                }
                continue;
            } else if ($SIZE.equalsIgnoreCase(key)) {
                if (ReflectionUtils.isListOrArray(ret.getLast())) {
                    ret.add(ReflectionUtils.getListLength(ret.getLast()), keyOrg);
                } else if (ret.getLast() instanceof Collection) {
                    int i = 0;
                    for (Object e : ((Collection) ret.getLast())) {
                        i++;
                    }
                    ret.add(i, keyOrg);
                } else if (ret.getLast() instanceof Map) {
                    ret.add(((Map) ret.getLast()).size(), keyOrg);
                } else if (ret.getLast() instanceof String) {
                    ret.add(((String) ret.getLast()).length(), keyOrg);
                } else {
                    ret.add(listKeys(ret.getLast()).size(), keyOrg);
                }
                continue;
            } else if (ret.getLast() == null) {
                ret.add(KEY_DOES_NOT_EXIST, keyOrg);
                onKeyDoesNotExist(scope, keyPath, ret);
                return ret;
            }
            if (key != null && key.startsWith("§")) {
                key = key.substring(1);
            }
            final KeyOnClass cacheKey = new KeyOnClass(ret.getLast().getClass(), key);
            AccessMethod accessMethod = getAccessMethod(cacheKey);
            if (accessMethod != null) {
                try {
                    ret.add(accessMethod.getValue(ret.getLast(), key), keyOrg);
                    if (ret.getLast() == KEY_DOES_NOT_EXIST) {
                        onKeyDoesNotExist(scope, keyPath, ret);
                    }
                    continue;
                } catch (CannotGetValueException e) {
                    DebugMode.debugger();
                    if (Boolean.TRUE.equals(get($IGNORE_GETTER_EXCEPTIONS))) {
                        ret.add(KEY_DOES_NOT_EXIST, keyOrg);
                        if (ret.getLast() == KEY_DOES_NOT_EXIST) {
                            onKeyDoesNotExist(scope, keyPath, ret);
                        }
                        return ret;
                    } else {
                        throw new CompareException(e);
                    }
                }
            }
            if (ret.getLast() instanceof ConditionObjectValueView) {
                final ConditionObjectValueView view = (ConditionObjectValueView) ret.getLast();
                final Object newValue = view.getConditionObjectValue(key);
                if (newValue == null) {
                    if (view.containsConditionObjectKey(key)) {
                        // really return?
                        System.out.println("Check of we can return here 1");
                        ret.add(null, keyOrg);
                        return ret;
                    } else if (!view.isConditionObjectVisible()) {
                        ret.add(KEY_DOES_NOT_EXIST, keyOrg);
                        onKeyDoesNotExist(scope, keyPath, ret);
                        return ret;
                    }
                } else {
                    ret.add(newValue, keyOrg);
                    if (ret.getLast() == KEY_DOES_NOT_EXIST) {
                        onKeyDoesNotExist(scope, keyPath, ret);
                    }
                    continue;
                }
            }
            if (ret.getLast() instanceof Map) {
                accessMethod = new AccessMapElement();
                putAccessMethod(cacheKey, accessMethod);
                try {
                    ret.add(accessMethod.getValue(ret.getLast(), key), keyOrg);
                    if (ret.getLast() == KEY_DOES_NOT_EXIST) {
                        onKeyDoesNotExist(scope, keyPath, ret);
                    }
                    continue;
                } catch (CannotGetValueException e) {
                    if (Boolean.TRUE.equals(get($IGNORE_GETTER_EXCEPTIONS))) {
                        ret.add(KEY_DOES_NOT_EXIST, keyOrg);
                        onKeyDoesNotExist(scope, keyPath, ret);
                        return ret;
                    } else {
                        throw new CompareException(e);
                    }
                }
            } else if (ReflectionUtils.isListOrArray(ret.getLast())) {
                final Class<?> raw = ReflectionUtils.getRaw(ret.getLast().getClass());
                if (raw.isArray()) {
                    accessMethod = new AccessArrayElement();
                } else {
                    accessMethod = new AccessListElement();
                }
                putAccessMethod(cacheKey, accessMethod);
                try {
                    ret.add(accessMethod.getValue(ret.getLast(), key), keyOrg);
                    if (ret.getLast() == KEY_DOES_NOT_EXIST) {
                        onKeyDoesNotExist(scope, keyPath, ret);
                    }
                    continue;
                } catch (CannotGetValueException e) {
                    if (Boolean.TRUE.equals(get($IGNORE_GETTER_EXCEPTIONS))) {
                        ret.add(KEY_DOES_NOT_EXIST, keyOrg);
                        onKeyDoesNotExist(scope, keyPath, ret);
                        return ret;
                    } else {
                        throw new CompareException(e);
                    }
                }
            } else if (ret.getLast() instanceof Collection) {
                accessMethod = new AccessCollectionElement();
                putAccessMethod(cacheKey, accessMethod);
                try {
                    ret.add(accessMethod.getValue(ret.getLast(), key), keyOrg);
                    if (ret.getLast() == KEY_DOES_NOT_EXIST) {
                        onKeyDoesNotExist(scope, keyPath, ret);
                    }
                    continue;
                } catch (CannotGetValueException e) {
                    if (Boolean.TRUE.equals(get($IGNORE_GETTER_EXCEPTIONS))) {
                        ret.add(KEY_DOES_NOT_EXIST, keyOrg);
                        onKeyDoesNotExist(scope, keyPath, ret);
                        return ret;
                    } else {
                        throw new CompareException(e);
                    }
                }
            }
            // Search for methods that have the exact key name
            Class<? extends Object> cls = ret.getLast().getClass();
            Method method = null;
            try {
                ClassCache cc = ClassCache.getClassCache(cls);
                Getter getter = cc.getGetter(key);
                if (getter != null) {
                    if (!isForbiddenMethod(getter.method)) {
                        method = getter.method;
                    }
                }
            } catch (SecurityException e) {
                throw new CompareException("Cannot get value", e);
            } catch (NoSuchMethodException e) {
                throw new CompareException("Cannot get value", e);
            }
            if (method == null) {
                // check getters with the key. get/is<Key>
                method = null;
                final String methodKey = Character.toUpperCase(key.charAt(0)) + key.substring(1);
                while (cls != null) {
                    try {
                        method = cls.getDeclaredMethod("is" + methodKey, EMPTY);
                        if (isForbiddenMethod(method)) {
                            method = null;
                        } else {
                            break;
                        }
                    } catch (SecurityException e) {
                        throw new CompareException("Cannot get value", e);
                    } catch (IllegalArgumentException e) {
                        throw new CompareException("Cannot get value", e);
                    } catch (NoSuchMethodException ignore) {
                    }
                    try {
                        method = cls.getDeclaredMethod("get" + methodKey, EMPTY);
                        if (isForbiddenMethod(method)) {
                            method = null;
                        } else {
                            break;
                        }
                    } catch (SecurityException e) {
                        throw new CompareException("Cannot get value", e);
                    } catch (IllegalArgumentException e) {
                        throw new CompareException("Cannot get value", e);
                    } catch (NoSuchMethodException ignore) {
                    }
                    cls = cls.getSuperclass();
                }
            }
            if (method != null) {
                method.setAccessible(true);
                accessMethod = new AccessByMethod(method);
                putAccessMethod(cacheKey, accessMethod);
                try {
                    ret.add(accessMethod.getValue(ret.getLast(), key), keyOrg);
                    if (ret.getLast() == KEY_DOES_NOT_EXIST) {
                        onKeyDoesNotExist(scope, keyPath, ret);
                    }
                    continue;
                } catch (CannotGetValueException e) {
                    if (Boolean.TRUE.equals(get($IGNORE_GETTER_EXCEPTIONS))) {
                        ret.add(KEY_DOES_NOT_EXIST, keyOrg);
                        onKeyDoesNotExist(scope, keyPath, ret);
                        return ret;
                    } else {
                        throw new CompareException(e);
                    }
                }
            }
            cls = ret.getLast().getClass();
            // check fields
            Field field = null;
            while (cls != null) {
                try {
                    field = cls.getDeclaredField(key);
                    if (isForbiddenField(field)) {
                        field = null;
                    } else {
                        break;
                    }
                } catch (SecurityException e) {
                    throw new CompareException("Cannot get value", e);
                } catch (IllegalArgumentException e) {
                    throw new CompareException("Cannot get value", e);
                } catch (NoSuchFieldException ignore) {
                }
                cls = cls.getSuperclass();
            }
            if (field != null) {
                field.setAccessible(true);
                accessMethod = new AccessByField(field);
                putAccessMethod(cacheKey, accessMethod);
                try {
                    ret.add(accessMethod.getValue(ret.getLast(), key), keyOrg);
                    if (ret.getLast() == KEY_DOES_NOT_EXIST) {
                        onKeyDoesNotExist(scope, keyPath, ret);
                    }
                    continue;
                } catch (CannotGetValueException e) {
                    if (Boolean.TRUE.equals(get($IGNORE_GETTER_EXCEPTIONS))) {
                        ret.add(KEY_DOES_NOT_EXIST, keyOrg);
                        onKeyDoesNotExist(scope, keyPath, ret);
                        return ret;
                    } else {
                        throw new CompareException(e);
                    }
                }
            }
            if (useAccessCache) {
                // only put Accessnotfound in the cache if the class it not a map or list and the key is not found in the class
                // declaration.
                putAccessMethod(cacheKey, new AccessNotFound());
            }
            ret.add(KEY_DOES_NOT_EXIST, keyOrg);
            onKeyDoesNotExist(scope, keyPath, ret);
            return ret;
        }
        return ret;
    }

    /**
     * @param scope
     * @param keyPath
     * @param ret
     */
    protected void onKeyDoesNotExist(Scope scope, JSPath keyPath, Scope ret) {
        if (_isDebug()) {
            log(ret.getPath(), "Key Does not Exist: " + scope.getPath().toPathString(false) + " + " + keyPath.toPathString(false));
        }
    }

    /**
     * @param class1
     * @return
     * @throws CompareException
     */
    public List<String> listKeys(Object obj) throws CompareException {
        if (obj == null || Clazz.isPrimitive(obj.getClass()) || obj instanceof String) {
            return Arrays.asList();
        }
        if (obj instanceof ConditionObjectValueView) {
            final ConditionObjectValueView view = (ConditionObjectValueView) obj;
            return view.listConditionKeys();
        }
        if (obj instanceof Map) {
            return new ArrayList<String>(((Map) obj).keySet());
        } else if (ReflectionUtils.isListOrArray(obj)) {
            final int length = ReflectionUtils.getListLength(obj);
            final ArrayList<String> ret = new ArrayList<String>(length);
            for (int i = 0; i < length; i++) {
                ret.add(String.valueOf(i));
            }
            return ret;
        } else if (obj instanceof Collection) {
            int i = 0;
            final ArrayList<String> ret = new ArrayList<String>(0);
            for (Object o : ((Collection) obj)) {
                ret.add(String.valueOf(i++));
            }
            return ret;
        }
        HashSet<String> ret = new HashSet<String>();
        Class<? extends Object> cls = obj.getClass();
        // Scan method names
        while (cls != null) {
            try {
                for (final Method method : cls.getDeclaredMethods()) {
                    if (ClassCache.getParameterCount(method) > 0) {
                        continue;
                    } else if (!isForbiddenMethod(method)) {
                        ret.add(method.getName());
                        if (method.getName().startsWith("is")) {
                            ret.add(method.getName().substring(2, 3).toLowerCase(Locale.ROOT) + method.getName().substring(3));
                        } else if (method.getName().startsWith("get")) {
                            ret.add(method.getName().substring(3, 4).toLowerCase(Locale.ROOT) + method.getName().substring(4));
                        }
                    }
                }
                for (Field field : cls.getDeclaredFields()) {
                    if (isForbiddenField(field)) {
                        continue;
                    }
                    ret.add(field.getName());
                }
            } catch (SecurityException e) {
                throw new CompareException("Cannot get value", e);
            } catch (IllegalArgumentException e) {
                throw new CompareException("Cannot get value", e);
            }
            cls = cls.getSuperclass();
        }
        return new ArrayList<String>(ret);
    }

    /**
     * @param value
     * @return
     * @throws CompareException
     */
    public boolean matches(final MatcherType obj) throws CompareException {
        if (_isDebug()) {
            log(new JSPath(), "Run " + this + ".matches(" + obj + ")");
        }
        Object result = evaluateInternal(new Scope(obj));
        return isTrue(result);
    }

    private boolean isTrue(Object result) {
        if (result instanceof ConditionResult) {
            return ((ConditionResult) result).isTrue();
        }
        return Boolean.TRUE == result;
    }

    /**
     * @param string
     */
    private void log(JSPath path, String string) {
        if (logger != null) {
            logger.info(path.toPathString(false) + " : " + string);
        } else {
            Condition root = ROOT_CONDITION.get();
            if (root != null && root != this && root.logger != null) {
                root.log(path, string);
                return;
            }
        }
        LogInterface log = THREAD_LOGGER.get();
        if (log != null) {
            log.info(path.toPathString(false) + " : " + string);
        }
    }

    private boolean                               debug         = false;
    public static final ThreadLocal<Boolean>      THREAD_DEBUG  = new ThreadLocal<Boolean>();
    public static final ThreadLocal<LogInterface> THREAD_LOGGER = new ThreadLocal<LogInterface>();

    public boolean _isDebug() {
        if (debug) {
            return true;
        }
        Condition root = ROOT_CONDITION.get();
        if (root != null && root != this && root._isDebug()) {
            return true;
        }
        return THREAD_DEBUG.get() == Boolean.TRUE;
    }

    public void _setDebug(boolean debug) {
        this.debug = debug;
    }

    /**
     * @param request
     * @return
     */
    public boolean matchesWithoutExceptions(MatcherType test) {
        try {
            return matches(test);
        } catch (CompareException e) {
            LogV3.log(e);
            return false;
        }
    }

    /**
     * @return
     */
    public Condition newInstance() {
        return new Condition();
    }

    /*
     * (non-Javadoc)
     *
     * @see java.util.HashMap#put(java.lang.Object, java.lang.Object)
     */
    @Override
    public Object put(String key, Object value) {
        key = correctKey(key);
        clearCache();
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

    protected void putAccessMethod(final KeyOnClass key, AccessMethod method) {
        if (useAccessCache) {
            final HashMap<KeyOnClass, AccessMethod> newCache = new HashMap<KeyOnClass, AccessMethod>(accessCache);
            newCache.put(key, method);
            accessCache = newCache;
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see java.util.HashMap#putAll(java.util.Map)
     */
    @Override
    public void putAll(Map<? extends String, ? extends Object> m) {
        for (java.util.Map.Entry<? extends String, ? extends Object> es : m.entrySet()) {
            put(es.getKey(), es.getValue());
        }
    }

    protected void putCache(final Object key, Object object) {
        if (useAccessCache) {
            final HashMap<Object, Object> newCache = new HashMap<Object, Object>(cache);
            newCache.put(key, object);
            cache = newCache;
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see java.util.HashMap#remove(java.lang.Object)
     */
    @Override
    public Object remove(Object key) {
        clearCache();
        return super.remove(key);
    }

    /*
     * (non-Javadoc)
     *
     * @see java.util.HashMap#replace(java.lang.Object, java.lang.Object, java.lang.Object)
     */
    @Override
    public boolean replace(String key, Object oldValue, Object newValue) {
        clearCache();
        return super.replace(key, oldValue, newValue);
    }

    protected final static CharSequence toCharSequence(Object value) {
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
    public void replaceAll(BiFunction<? super String, ? super Object, ? extends Object> function) {
        clearCache();
        super.replaceAll(function);
    }

    /**
     * Resolves a value expression - MAY RETURN AGGREGATIONRESULT
     *
     * @param container
     *            TODO
     * @param unwrapConditionResults
     * @param path
     *            TODO
     **/
    public Object resolveValue(Condition container, Object expression, Scope scope, boolean unwrapConditionResults) throws CompareException {
        Object before = expression;
        if (expression instanceof Map && !(expression instanceof Condition)) {
            // fix map to condition. json deserializer will map internal conditions to maps
            Map old = (Map) expression;
            expression = newInstance();
            ((Condition) expression).putAll(old);
        }
        if (expression instanceof Condition) {
            expression = ((Condition) expression).evaluateInternal(scope);
        }
        if (expression instanceof String && ((String) expression).startsWith("§")) {
            try {
                expression = resolveKeyPath(scope, JSPath.fromPathString((String) expression)).getLast();
            } catch (InvalidPathException e) {
                throw new CompareException(e);
            }
        }
        if (before != expression) {
            if (container._isDebug()) {
                container.log(scope.getPath(), "Resolved " + before + " = " + expression);
            }
        }
        if (unwrapConditionResults) {
            return container.unwrap(expression);
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
        return "Condition: " + super.toString();
    }

    /**
     * @param values
     * @param condition
     * @throws CompareException
     */
    public static <T> List<T> find(Collection<T> values, Condition condition) throws CompareException {
        ArrayList<T> ret = new ArrayList<T>();
        for (T v : values) {
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
     * @throws CompareException
     */
    public static Object resolve(String keyString, Object object) throws CompareException, InvalidPathException {
        Condition c = new Condition();
        Scope ret = c.resolveKeyPath(new Scope(object), JSPath.fromPathString(keyString));
        return ret.getLast();
    }

    public Object convertSpecialTypes(Object expression, final Object matcherValue) throws CompareException {
        if (matcherValue instanceof TimeSpan) {
            expression = toTimeSpan(expression);
        } else if (matcherValue instanceof Date) {
            expression = toDate(expression);
        }
        return expression;
    }
}
