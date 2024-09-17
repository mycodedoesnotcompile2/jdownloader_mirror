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
package org.appwork.storage;

import java.lang.annotation.Annotation;
import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Type;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.moncompare.Condition;
import org.appwork.moncompare.Condition.PathHandler;
import org.appwork.moncompare.Scope;
import org.appwork.moncompare.fromjson.FlexiCondition;
import org.appwork.storage.BuildsInfo.TargetBuildIssue;
import org.appwork.storage.flexijson.CannotResolvePathException;
import org.appwork.storage.flexijson.FlexiComment;
import org.appwork.storage.flexijson.FlexiJSonArray;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.FlexiJSonObject;
import org.appwork.storage.flexijson.FlexiJSonValue;
import org.appwork.storage.flexijson.FlexiJsonMapperForConfig;
import org.appwork.storage.flexijson.FlexiUtils;
import org.appwork.storage.flexijson.InvalidPathException;
import org.appwork.storage.flexijson.JSPath;
import org.appwork.storage.flexijson.KeyValueElement;
import org.appwork.storage.flexijson.NodeFilter;
import org.appwork.storage.flexijson.mapper.ClassCastFlexiMapperException;
import org.appwork.storage.flexijson.mapper.FlexiEnumFallback;
import org.appwork.storage.flexijson.mapper.FlexiJSonMapper;
import org.appwork.storage.flexijson.mapper.FlexiMapperException;
import org.appwork.storage.flexijson.mapper.FlexiTypeMapper;
import org.appwork.storage.flexijson.mapper.typemapper.DateMapper;
import org.appwork.storage.flexijson.stringify.FlexiJSonPrettyPrinterForConfig;
import org.appwork.storage.flexijson.stringify.FlexiJSonPrettyStringify;
import org.appwork.storage.flexijson.stringify.FlexiJSonStringBuilder;
import org.appwork.storage.simplejson.ValueType;
import org.appwork.storage.simplejson.mapper.ClassCache;
import org.appwork.storage.simplejson.mapper.Property;
import org.appwork.storage.simplejson.mapper.Setter;
import org.appwork.storage.validator.classvalidator.StorableAbstractValidator;
import org.appwork.storage.validator.classvalidator.StorableClassValidator1;
import org.appwork.storage.validator.classvalidator.StorableClassValidator2;
import org.appwork.storage.validator.classvalidator.StorableClassValidator3;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Exceptions;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.RuntimeInterruptedException;
import org.appwork.utils.StringUtils;
import org.appwork.utils.duration.TimeSpan;
import org.appwork.utils.reflection.CompiledType;
import org.appwork.utils.reflection.JsonSyntax;

/**
 * @author thomas
 * @param <T>
 * @date 22.06.2022
 *
 */
public class StorableValidator<T> {
    /**
     * @author thomas
     * @date 25.07.2023
     *
     */
    public static class FallbackKeyException extends ValidatorException {
        private long   deprecatedSinceTimestamp;
        private String correctKey;

        public long getDeprecatedSinceTimestamp() {
            return deprecatedSinceTimestamp;
        }

        public String getCorrectKey() {
            return correctKey;
        }

        /**
         * @param validator
         * @param correctKey
         * @param path
         * @param value
         * @param targetType
         * @param message
         * @param string
         * @param failLevel
         */
        public FallbackKeyException(StorableValidator validator, long deprecatedSince, String correctKey, JSPath path, FlexiJSonNode value, CompiledType targetType, String message, FailLevel failLevel) {
            super(validator, path, value, targetType, message, failLevel);
            this.correctKey = correctKey;
            this.deprecatedSinceTimestamp = deprecatedSince;
        }

        /*
         * (non-Javadoc)
         *
         * @see org.appwork.storage.StorableValidator.ValidatorException#getErrorMessage()
         */
        @Override
        public String getErrorMessage() {
            if (getMessage() != null) {
                return getMessage();
            }
            return "Property deprecated since " + DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.MEDIUM).format(new Date(deprecatedSinceTimestamp));
        }

        /**
         * @param codebaseVersion
         * @return
         */
        public TargetBuildIssue handle(BuildsInfo buildsInfo) {
            final Date minimum = buildsInfo.getMinimumBuildDate();
            if (minimum != null && minimum.getTime() < deprecatedSinceTimestamp) {
                return TargetBuildIssue.ALL_TARGET_BUILDS_AFFECTED;
            }
            final Date maximum = buildsInfo.getMaximumBuildDate();
            if (maximum != null) {
                if (deprecatedSinceTimestamp < maximum.getTime()) {
                    return TargetBuildIssue.SOME_TARGET_BUILDS_AFFECTED;
                }
            }
            return TargetBuildIssue.NO_TARGET_BUILDS_AFFECTED;
        }
    }

    /**
     * @author thomas
     * @date 30.06.2022
     *
     */
    public class CheckerMapper extends AddDefaultsMapper {
        {
            setIgnoreIllegalEnumMappings(false);
            setIgnoreIllegalArgumentMappings(false);
        }

        /**
         * @param exceptions
         * @param toDos
         * @param exceptions2
         */
        public CheckerMapper() {
            super();
        }

        @Override
        protected Object createProxy(CompiledType cType, FlexiJSonObject obj) throws IllegalArgumentException, SecurityException, NoSuchMethodException {
            Object ret = super.createProxy(cType, obj);
            return ret;
        }

        /*
         * (non-Javadoc)
         *
         * @see org.appwork.storage.flexijson.mapper.FlexiJSonMapper#onClassFieldMissing(java.lang.Object, java.lang.String,
         * org.appwork.storage.flexijson.FlexiJSonNode)
         */
        @Override
        protected void onClassFieldMissing(Object inst, String key, FlexiJSonNode value, CompiledType cc) throws FlexiMapperException {
            if (cc.getClassCache().getAnnotations(key, StorableValidatorIgnoreKey.class).size() > 0) {
                return;
            }
            if (cc.getClassCache().getAnnotations(key, StorableValidatorIgnoresMissingSetter.class).size() > 0) {
                return;
            }
            RuntimeInterruptedException.throwWithoutClear();
        }

        /*
         * (non-Javadoc)
         *
         * @see org.appwork.storage.flexijson.mapper.FlexiJSonMapper#returnFallbackOrThrowException(org.appwork.storage.flexijson.mapper.
         * FlexiMapperException)
         */
        @Override
        protected Object returnFallbackOrThrowException(FlexiMapperException ex) throws FlexiMapperException {
            RuntimeInterruptedException.throwWithoutClear();
            try {
                if (ex.getCause() instanceof ClassCastException) {
                    String structure;
                    if (ex.node instanceof FlexiJSonObject) {
                        structure = "Object/Map";
                    } else if (ex.node instanceof FlexiJSonArray) {
                        structure = "Array/List/Set";
                    } else {
                        structure = "Number/String/Enum/Boolean/null";
                    }
                    add(new ValidatorException(StorableValidator.this, ex, fromFlexiNode(ex.node), ex.node, ex.type, "Unexpected data structure: '" + structure + "' for target type '" + ex.type.toString(new JsonSyntax()) + "" + "'", FailLevel.ERROR));
                } else {
                    add(ex);
                }
            } catch (InterruptedException e1) {
                Thread.currentThread().interrupt();
                RuntimeInterruptedException.throwWithoutClear();
            }
            return null;
        }

        /*
         * (non-Javadoc)
         *
         * @see org.appwork.storage.flexijson.mapper.FlexiJSonMapper#nodeToEnum(org.appwork.storage.flexijson.FlexiJSonNode,
         * java.lang.reflect.Type)
         */
        @Override
        protected Enum nodeToEnum(FlexiJSonNode node, CompiledType type) throws FlexiMapperException {
            try {
                return super.nodeToEnum(node, type);
            } catch (IllegalArgumentException e) {
                Enum alt = findAlternative(node, type.type);
                try {
                    add(new UnknownEnumException(StorableValidator.this, node, type, alt));
                } catch (InterruptedException e1) {
                    Thread.currentThread().interrupt();
                    RuntimeInterruptedException.throwWithoutClear();
                }
                return alt;
            }
        }

        @Override
        public Object jsonToObject(final FlexiJSonNode json, CompiledType type, Setter setter) throws FlexiMapperException {
            RuntimeInterruptedException.throwWithoutClear();
            type = StorableValidator.this.dynamicTypeMapping(json, type, setter);
            final Object ret = super.jsonToObject(json, type, setter);
            return ret;
        }

        @Override
        protected void setValueToObject(final FlexiJSonNode node, final Object inst, final CompiledType cType, final Object value, final Setter setter) throws IllegalAccessException, InvocationTargetException, FlexiMapperException {
            RuntimeInterruptedException.throwWithoutClear();
            super.setValueToObject(node, inst, cType, value, setter);
        }
    }

    protected void add(FlexiMapperException ex) throws InterruptedException {
        if (Thread.interrupted()) {
            throw new InterruptedException();
        }
        if (ex instanceof ValidatorException) {
            exceptions.add((ValidatorException) ex);
        } else {
            exceptions.add(new ValidatorException(this, ex, fromFlexiNode(ex.node), ex.node, ex.type, null, FailLevel.ERROR));
        }
    }

    /**
     * overwrite to change type depending on the current json content
     *
     * @param json
     * @param type
     * @param setter
     * @return
     */
    public CompiledType dynamicTypeMapping(FlexiJSonNode json, CompiledType type, Setter setter) {
        return type;
    }

    /**
     * @author thomas
     * @date 30.06.2022
     *
     */
    public class AddDefaultsMapper extends FlexiJSonMapper {
        /**
         * @param exceptions
         */
        public AddDefaultsMapper() {
        }

        /*
         * (non-Javadoc)
         *
         * @see org.appwork.storage.flexijson.mapper.FlexiJSonMapper#convert(java.lang.Object, org.appwork.storage.TypeRef)
         */
        @Override
        public <T> T convert(Object obj, TypeRef<T> targetType) throws FlexiMapperException {
            return super.convert(obj, targetType);
        }

        @Override
        protected Object convertStringToNumber(FlexiJSonValue node, String value, CompiledType destType) {
            try {
                add(new InvalidTypeException(StorableValidator.this, fromFlexiNode(node), node, destType, null, FailLevel.ERROR));
            } catch (InterruptedException e1) {
                Thread.currentThread().interrupt();
                RuntimeInterruptedException.throwWithoutClear();
            }
            return super.convertStringToNumber(node, value, destType);
        }

        /*
         * (non-Javadoc)
         *
         * @see org.appwork.storage.flexijson.mapper.FlexiJSonMapper#convertStringToBoolean(org.appwork.storage.flexijson.FlexiJSonValue,
         * java.lang.String, java.lang.Class)
         */
        @Override
        protected Object convertStringToBoolean(FlexiJSonValue node, String value, CompiledType destType) {
            try {
                add(new InvalidTypeException(StorableValidator.this, fromFlexiNode(node), node, destType, null, FailLevel.ERROR));
            } catch (InterruptedException e1) {
                Thread.currentThread().interrupt();
                RuntimeInterruptedException.throwWithoutClear();
            }
            return super.convertStringToBoolean(node, value, destType);
        }

        @Override
        protected Object convertToString(FlexiJSonValue node) {
            try {
                add(new InvalidTypeException(StorableValidator.this, fromFlexiNode(node), node, CompiledType.STRING, null, FailLevel.ERROR));
            } catch (InterruptedException e1) {
                Thread.currentThread().interrupt();
                RuntimeInterruptedException.throwWithoutClear();
            }
            return super.convertToString(node);
        }

        @Override
        protected Object convertNullToBoolean(FlexiJSonValue node) {
            try {
                add(new InvalidTypeException(StorableValidator.this, fromFlexiNode(node), node, CompiledType.BOOLEAN_PRIMITIVE, null, FailLevel.ERROR));
            } catch (InterruptedException e1) {
                Thread.currentThread().interrupt();
                RuntimeInterruptedException.throwWithoutClear();
            }
            return super.convertNullToBoolean(node);
        }

        @Override
        protected Object convertNullToNumber(FlexiJSonValue node, CompiledType destType) {
            try {
                add(new InvalidTypeException(StorableValidator.this, fromFlexiNode(node), node, destType, null, FailLevel.ERROR));
            } catch (InterruptedException e1) {
                Thread.currentThread().interrupt();
                RuntimeInterruptedException.throwWithoutClear();
            }
            return super.convertNullToNumber(node, destType);
        }

        @Override
        protected Enum nodeToEnum(FlexiJSonNode node, CompiledType type) throws FlexiMapperException {
            try {
                return super.nodeToEnum(node, type);
            } catch (IllegalArgumentException e) {
                Enum alt = findAlternative(node, type.type);
                try {
                    add(new UnknownEnumException(StorableValidator.this, node, type, alt));
                } catch (InterruptedException e1) {
                    Thread.currentThread().interrupt();
                    RuntimeInterruptedException.throwWithoutClear();
                }
                return alt;
            }
        }

        protected Enum findAlternative(FlexiJSonNode node, Type type) {
            if (node instanceof FlexiJSonValue && ((FlexiJSonValue) node).getValue() != null) {
                int best = Integer.MAX_VALUE;
                String bestmatch = null;
                for (Object en : ReflectionUtils.getRaw(type).getEnumConstants()) {
                    int r = StringUtils.levenshtein(String.valueOf(((FlexiJSonValue) node).getValue()), ((Enum) en).name(), true);
                    if (r < best) {
                        best = r;
                        bestmatch = ((Enum) en).name();
                    }
                }
                if (bestmatch == null) {
                    bestmatch = ((Enum) ReflectionUtils.getRaw(type).getEnumConstants()[0]).name();
                }
                try {
                    return Enum.valueOf((Class<Enum>) ReflectionUtils.getRaw(type), bestmatch);
                } catch (Exception e1) {
                    return null;
                }
            } else {
                try {
                    return ((Enum) ReflectionUtils.getRaw(type).getEnumConstants()[0]);
                } catch (Exception e1) {
                    return null;
                }
            }
        }

        // adding defaults will removed missing fields
        @Override
        protected void onClassFieldMissing(Object inst, String key, FlexiJSonNode value, CompiledType cc) throws FlexiMapperException {
            try {
                add(new UnknownPropertyException(StorableValidator.this, fromFlexiNode(value), value, null));
            } catch (InterruptedException e1) {
                Thread.currentThread().interrupt();
                RuntimeInterruptedException.throwWithoutClear();
            }
        }
    }

    public static class UnknownEnumException extends ValidatorException {
        public final Enum bestGuess;

        public UnknownEnumException(StorableValidator validator, FlexiJSonNode value, CompiledType targetType, Enum bestGuess) {
            super(validator, fromFlexiNode(value), value, targetType, (String) null, FailLevel.ERROR);
            this.bestGuess = bestGuess;
        }

        /*
         * (non-Javadoc)
         *
         * @see org.appwork.storage.StorableValidator.ValidatorException#getErrorMessage()
         */
        @Override
        public String getErrorMessage() {
            if (getMessage() != null) {
                return getMessage();
            }
            String ret = "";
            ret += "The value " + new FlexiJSonStringBuilder().toJSONString(node) + " is not valid. Please use one of " + StringUtils.join(type.raw.getEnumConstants(), "|");
            if (bestGuess != null) {
                ret += "\r\nDid you mean \"" + bestGuess.name() + "\"?";
            }
            return ret;
        }
    }

    public static class InvalidRegularExpressionException extends ValidatorException {
        private Exception parsingException;

        public Exception getParsingException() {
            return parsingException;
        }

        public InvalidRegularExpressionException(StorableValidator validator, Exception e, JSPath path, FlexiJSonNode value, CompiledType targetType, String message, FailLevel failLevel) {
            super(validator, path, value, targetType, message, failLevel);
            this.parsingException = e;
        }
    }

    public static class UnknownPropertyException extends ValidatorException {
        public UnknownPropertyException(StorableValidator validator, JSPath path, FlexiJSonNode value, String message) {
            super(validator, path, value, null, message, FailLevel.WARNING);
        }

        /*
         * (non-Javadoc)
         *
         * @see org.appwork.storage.StorableValidator.ValidatorException#getErrorMessage()
         */
        @Override
        public String getErrorMessage() {
            if (getMessage() != null) {
                return getMessage();
            }
            return "This property name is unknown. Check for type errors!";
        }
    }

    public static class ValidatorException extends FlexiMapperException {
        /**
         *
         */
        private static final long serialVersionUID = 1L;
        private FailLevel         failLevel;

        public FailLevel getFailLevel() {
            return failLevel;
        }

        public void setFailLevel(FailLevel failLevel) {
            this.failLevel = failLevel;
        }

        public final StorableValidator owner;
        public final JSPath            path;

        public ValidatorException(StorableValidator validator, JSPath path, FlexiJSonNode value, CompiledType targetType, String message) {
            this(validator, path, value, targetType, message, FailLevel.ERROR);
        }

        public ValidatorException(StorableValidator validator, Throwable cause, JSPath path, FlexiJSonNode value, CompiledType targetType, String message, FailLevel failLevel) {
            super(value, targetType, message, cause);
            this.failLevel = failLevel;
            this.owner = validator;
            this.path = path;
        }

        public ValidatorException(StorableValidator validator, JSPath path, FlexiJSonNode value, CompiledType targetType, String message, FailLevel failLevel) {
            this(validator, null, path, value, targetType, message, failLevel);
        }

        /**
         * @return
         */
        public String getErrorMessage() {
            return "";
        }

        /**
         *
         */
        public String getDetailedMessage() {
            String ret = failLevel.name();
            ret += "\r\nJson Node: " + path.toPathString(true);
            boolean header = false;
            String add = getErrorMessage();
            //
            //
            if (!StringUtils.isNotEmpty(add)) {
                add = getClass().getSimpleName();
            }
            if (StringUtils.isNotEmpty(add)) {
                if (!header) {
                    switch (failLevel) {
                    case ERROR:
                        ret += "\r\nError: ";
                        break;
                    case INFO:
                        ret += "\r\nInformation: ";
                        break;
                    case WARNING:
                        ret += "\r\nWarning: ";
                        break;
                    }
                }
                header = true;
                ret += "\r\n" + add;
            }
            if (!StringUtils.isEmpty(getMessage()) && !ret.contains(getMessage())) {
                if (!header) {
                    switch (failLevel) {
                    case ERROR:
                        ret += "\r\nError: ";
                        break;
                    case INFO:
                        ret += "\r\nInformation: ";
                        break;
                    case WARNING:
                        ret += "\r\nWarning: ";
                        break;
                    }
                    header = true;
                    ret += "" + getMessage().replace("] ", "]\r\n");
                } else {
                    ret += "\r\n" + getMessage().replace("] ", "]\r\n");
                }
            }
            try {
                // final ArrayList<String> allowed = new ArrayList<String>();
                // allowed.add(FlexiUtils.getPathString(node));
                // allowed.add(FlexiUtils.getPathString(node.getParent()) + ".//");
                final JSPath nodePath = node == null ? null : fromFlexiNode(node);
                final FlexiJSonPrettyStringify toString = new FlexiJSonPrettyPrinterForConfig(new NodeFilter() {
                    @Override
                    public boolean skipNode(FlexiJSonArray array, int i) {
                        FlexiJSonNode lNode = array.get(i);
                        JSPath path = fromFlexiNode(lNode);
                        if (nodePath == null || nodePath.startsWith(path)) {
                            return false;
                        }
                        return true;
                    }

                    @Override
                    public boolean skipNode(FlexiJSonObject object, KeyValueElement es) {
                        JSPath path = fromFlexiNode(es.getValue());
                        if (nodePath == null || nodePath.startsWith(path)) {
                            return false;
                        }
                        return true;
                    }

                    @Override
                    public boolean skipCommentNode(FlexiComment comment) {
                        JSPath path = fromFlexiNode(comment);
                        if (nodePath == null || nodePath.getParent().equals(path.getParent())) {
                            return false;
                        }
                        return true;
                    }
                });
                final FlexiJSonMapper mapper = new FlexiJsonMapperForConfig();
                List<FlexiTypeMapper> customMappers = owner.createMapper().getTypeMapper();
                mapper.setTypeMapper(customMappers);
                final FlexiJSonNode node = mapper.objectToJsonNode(owner.getResult());
                String docs = toString.toJSONString(node);
                if (StringUtils.isNotEmpty(docs)) {
                    ret += "\r\nDocumentation: ";
                    ret += "\r\n" + docs;
                }
                // if (getCause() != null) {
                // ret += "\r\nCause: " + StringUtils.multiLineIntend(Exceptions.getStackTrace(getCause()), "", " ");
                // }
            } catch (final FlexiMapperException e1) {
                e1.printStackTrace();
            }
            return ret;
        }
    }

    public static class ConditionException extends ValidatorException {
        private Condition condition;

        public Condition getCondition() {
            return condition;
        }

        public ConditionException(StorableValidator validator, Condition condition, JSPath path, FlexiJSonNode value, CompiledType targetType, String message, FailLevel failLevel) {
            super(validator, path, value, targetType, message, failLevel);
            this.condition = condition;
        }

        public ConditionException(StorableValidator validator, Condition condition, Throwable cause, JSPath path, FlexiJSonNode value, CompiledType targetType, String message, FailLevel failLevel) {
            super(validator, cause, path, value, targetType, message, failLevel);
            this.condition = condition;
        }
    }

    public static class AvailableSinceException extends ValidatorException {
        public final long availableSinceTimestamp;

        public AvailableSinceException(StorableValidator validator, long availableSinceTimestamp, JSPath path, FlexiJSonNode value, CompiledType targetType, String message, FailLevel failLevel) {
            super(validator, path, value, targetType, message, failLevel);
            this.availableSinceTimestamp = availableSinceTimestamp;
        }

        /*
         * (non-Javadoc)
         *
         * @see org.appwork.storage.StorableValidator.ValidatorException#getErrorMessage()
         */
        @Override
        public String getErrorMessage() {
            if (getMessage() != null) {
                return getMessage();
            }
            return "Property available since " + DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.MEDIUM).format(new Date(availableSinceTimestamp)) + "(" + availableSinceTimestamp + ")";
        }

        /**
         * @param buildsInfo
         * @return
         */
        public TargetBuildIssue handle(BuildsInfo buildsInfo) {
            final Date maximum = buildsInfo.getMaximumBuildDate();
            if (maximum != null) {
                if (availableSinceTimestamp >= maximum.getTime()) {
                    return TargetBuildIssue.ALL_TARGET_BUILDS_AFFECTED;
                }
            }
            final Date minimum = buildsInfo.getMinimumBuildDate();
            if (minimum != null && minimum.getTime() < availableSinceTimestamp) {
                return TargetBuildIssue.SOME_TARGET_BUILDS_AFFECTED;
            }
            return TargetBuildIssue.NO_TARGET_BUILDS_AFFECTED;
        }
    }

    public static class InvalidTimestampException extends ValidatorException {
        public final long min;
        public final long max;
        public final long value;

        public InvalidTimestampException(StorableValidator validator, JSPath path, FlexiJSonNode value, CompiledType targetType, String message, FailLevel failLevel, long min, long max, long value2) {
            super(validator, path, value, targetType, message, failLevel);
            this.min = min;
            this.max = max;
            this.value = value2;
        }
    }

    public static class ValidatorInvalidTimeSpanException extends ValidatorException {
        public final TimeSpan min;
        public final TimeSpan max;
        public final TimeSpan value;

        public ValidatorInvalidTimeSpanException(StorableValidator validator, JSPath path, FlexiJSonNode value, CompiledType targetType, String message, FailLevel failLevel, TimeSpan min, TimeSpan max, TimeSpan value2) {
            super(validator, path, value, targetType, message, failLevel);
            this.min = min;
            this.max = max;
            this.value = value2;
        }
    }

    public static class ValidatorMandatoryPropertyMissingException extends ValidatorException {
        public final StorableValidateMandatoryInJson annotation;

        /**
         * @param storableValidator
         * @param type
         * @param path
         * @param a
         */
        public ValidatorMandatoryPropertyMissingException(StorableValidator storableValidator, CompiledType type, JSPath path, StorableValidateMandatoryInJson a) {
            super(storableValidator, path, (FlexiJSonNode) null, type, StringUtils.isEmpty(a.message()) ? ("The property " + path.getLast() + " must be set!") : a.message(), a.level());
            this.annotation = a;
        }
    }

    public static class ValidatorNonUniqueKeyException extends ValidatorException {
        public final StorableUnique annotation;
        public final Object         dupe;

        /**
         * @param storableValidator
         * @param type
         * @param path
         * @param a
         * @param value
         */
        public ValidatorNonUniqueKeyException(StorableValidator storableValidator, CompiledType type, JSPath path, FlexiJSonNode flexiJSonNode, StorableUnique a, Object value) {
            this(storableValidator, path, flexiJSonNode, type, StringUtils.isEmpty(a.message()) ? ("There may be only a single(" + (a.caseInSensitive() ? "Case INsensitive" : "Case Sensitive") + ") entry with " + a.value() + "=" + FlexiUtils.serializeMinimizedWithWTF(value)) : a.message(), a, value, a.level());
        }

        /**
         * @param storableValidator
         * @param path
         * @param flexiJSonNode
         * @param type
         * @param a
         * @param value
         * @param string
         * @param level
         */
        public ValidatorNonUniqueKeyException(StorableValidator storableValidator, JSPath path, FlexiJSonNode flexiJSonNode, CompiledType type, String message, StorableUnique a, Object value, FailLevel level) {
            super(storableValidator, path, flexiJSonNode, type, message, level);
            this.annotation = a;
            this.dupe = value;
        }
    }

    public static class ValidatorValueIsNullException extends ValidatorException {
        public final StorableValidateNotNull annotation;
        public final JSPath                  path;

        /**
         * @param storableValidator
         * @param type
         * @param path
         * @param a
         */
        public ValidatorValueIsNullException(StorableValidator storableValidator, JSPath path, FlexiJSonNode node, CompiledType type, StorableValidateNotNull a) {
            super(storableValidator, path, node, type, StringUtils.isEmpty(a.description()) ? ("The property " + path.getLast() + " must not be null!") : a.description(), a.level());
            this.annotation = a;
            this.path = path;
        }

        public ValidatorValueIsNullException(StorableValidator storableValidator, JSPath path, FlexiJSonNode node, CompiledType type, String message, FailLevel level) {
            super(storableValidator, path, node, type, message, level);
            this.annotation = null;
            this.path = path;
        }
    }

    public static class DeprecatedException extends ValidatorException {
        public final long deprecatedSinceTimestamp;

        public DeprecatedException(StorableValidator validator, long deprecatedSince, JSPath path, FlexiJSonNode value, CompiledType targetType, String message, FailLevel level) {
            super(validator, path, value, targetType, message, level);
            this.deprecatedSinceTimestamp = deprecatedSince;
        }

        /*
         * (non-Javadoc)
         *
         * @see org.appwork.storage.StorableValidator.ValidatorException#getErrorMessage()
         */
        @Override
        public String getErrorMessage() {
            if (getMessage() != null) {
                return getMessage();
            }
            return "Property deprecated since " + DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.MEDIUM).format(new Date(deprecatedSinceTimestamp));
        }

        /**
         * @param codebaseVersion
         * @return
         */
        public TargetBuildIssue handle(BuildsInfo buildsInfo) {
            final Date minimum = buildsInfo.getMinimumBuildDate();
            if (minimum != null && minimum.getTime() < deprecatedSinceTimestamp) {
                return TargetBuildIssue.ALL_TARGET_BUILDS_AFFECTED;
            }
            final Date maximum = buildsInfo.getMaximumBuildDate();
            if (maximum != null) {
                if (deprecatedSinceTimestamp < maximum.getTime()) {
                    return TargetBuildIssue.SOME_TARGET_BUILDS_AFFECTED;
                }
            }
            return TargetBuildIssue.NO_TARGET_BUILDS_AFFECTED;
        }
    }

    public static class NullException extends ValidatorException {
        public NullException(StorableValidator validator, JSPath path, FlexiJSonNode value, CompiledType targetType, String message, FailLevel failLevel) {
            super(validator, path, value, targetType, message, failLevel);
        }

        /*
         * (non-Javadoc)
         *
         * @see org.appwork.storage.StorableValidator.ValidatorException#getErrorMessage()
         */
        @Override
        public String getErrorMessage() {
            if (getMessage() != null) {
                return getMessage();
            }
            return "null is not allowed for this property";
        }
    }

    public static class EmptyStringException extends ValidatorException {
        public EmptyStringException(StorableValidator validator, JSPath path, FlexiJSonNode value, CompiledType targetType, String message, FailLevel failLevel) {
            super(validator, path, value, targetType, message, failLevel);
        }

        @Override
        public String getErrorMessage() {
            if (getMessage() != null) {
                return getMessage();
            }
            return "An empty string is not allowed for this property";
        }
    }

    public static class InvalidTypeException extends ValidatorException {
        public InvalidTypeException(StorableValidator validator, JSPath path, FlexiJSonNode value, CompiledType targetType, String message, FailLevel failLevel) {
            super(validator, path, value, targetType, message, failLevel);
        }

        @Override
        public String getErrorMessage() {
            if (getMessage() != null) {
                return getMessage();
            }
            String ret = "";
            if (node instanceof FlexiJSonValue) {
                String typeName = type.toString();
                if (type.raw instanceof Class) {
                    typeName = ((Class) type.raw).getSimpleName();
                }
                ret += "Invalid type! Cannot map " + new FlexiJSonStringBuilder().toJSONString(node) + "(" + ((FlexiJSonValue) node).getType() + ")" + " to type " + typeName + ".";
                try {
                    ret += " \r\nTry this instead: " + FlexiUtils.serializeToPrettyJson(new FlexiJSonMapper().jsonToObject(node, new SimpleTypeRef<Object>(type.type))) + "";
                } catch (final FlexiMapperException e1) {
                    // no auto correction possible
                }
            } else if (node instanceof FlexiJSonArray) {
                ret += "Invalid type! Cannot map an array [...] to type " + type + ".";
            } else {
                ret += "Invalid type! Cannot map an object {...} to type " + type + ".";
            }
            return ret;
        }
    }

    private FlexiJSonNode rootNode;
    private CompiledType  rootType;

    public CompiledType getRootType() {
        return rootType;
    }

    private T                                              result;
    private ArrayList<StorableValidator<T>.ValidatetoDoss> toDos;
    private ArrayList<ValidatorException>                  exceptions;
    private String                                         targetBuildsProperty = "targetBuilds";
    private Object                                         context;

    public Object getContext() {
        return context;
    }

    public void setContext(Object context) {
        this.context = context;
    }

    public String getTargetBuildsProperty() {
        return targetBuildsProperty;
    }

    public void setTargetBuildsProperty(String targetBuildsProperty) {
        this.targetBuildsProperty = targetBuildsProperty;
    }

    public StorableValidator(FlexiJSonNode node, TypeRef<T> type) {
        this(node, CompiledType.create(type.getType()));
    }

    /**
     * @param <T>
     * @param node
     * @param type
     */
    public StorableValidator(FlexiJSonNode node, CompiledType type) {
        this.rootNode = node;
        this.rootType = type;
    }

    protected class ValidatetoDoss {
        private FlexiJSonNode node;

        public FlexiJSonNode getNode() {
            return node;
        }

        public CompiledType getType() {
            return type;
        }

        public List<Annotation> getAnnotations() {
            return annotations;
        }

        public JSPath getPath() {
            return path;
        }

        public void setValue(Object value) {
            this.value = value;
        }

        private Object           value;
        private CompiledType     type;
        private List<Annotation> annotations = new ArrayList<Annotation>();
        private JSPath           path;
        private Property         context;
        //
        // /**
        // * @param node
        // * @param value
        // * @param type
        // * @param annotations
        // * @param annotations2
        // * @param annotations3
        // * @param regexValidator
        // */
        // public ValidatetoDoss(FlexiJSonNode node, Object value, CompiledType type, ClassCache cc, String key, JSPath path) {
        // this.node = node;
        // this.path = path;
        // this.value = value;
        // this.type = type;
        // add(cc.getAnnotations(key, StorableValidateNotNull.class));
        // add(cc.getAnnotations(key, StorableValidateMandatoryInJson.class));
        // add(cc.getAnnotations(key, StorableValidateCondition.class));
        // add(cc.getAnnotations(key, StorableValidateCondition2.class));
        // add(cc.getAnnotations(key, StorableValidateCondition3.class));
        // add(cc.getAnnotations(key, StorableValidateRegex.class));
        // add(cc.getAnnotations(key, StorableDeprecatedSince.class));
        // add(cc.getAnnotations(key, StorableAvailableSince.class));
        // add(cc.getAnnotations(key, StorableValidateTimestamp.class));
        // add(cc.getAnnotations(key, StorableValidateTimestampRelative.class));
        // add(cc.getAnnotations(key, StorableValidateTimeSpan.class));
        // }

        public Property getContext() {
            return context;
        }

        /**
         * @param annotations2
         */
        private void add(List<? extends Annotation> a) {
            if (a != null && a.size() > 0) {
                annotations.addAll(a);
            }
        }
        // public ValidatetoDoss(FlexiJSonNode node, Object value, CompiledType type, ClassCache cc, String key) {
        // this(node, value, type, cc, key, fromFlexiNode(node));
        // }

        /**
         * @param node2
         * @param object
         * @param type2
         * @param context
         * @param path2
         */
        public ValidatetoDoss(FlexiJSonNode node, Object value, CompiledType type, Property context, JSPath path) {
            this.node = node;
            // DebugMode.breakIf(node == null);
            this.context = context;
            this.path = path;
            this.value = value;
            this.type = type;
            DebugMode.breakIf(type == null);
            ClassCache cc = type.getClassCache();
            if (cc != null) {
                if (value != null) {
                    // this adds annotations for the type - no reason to evaluate them if the value is null.
                    add(cc.getAnnotations(null, StorableClassValidator1.class));
                    add(cc.getAnnotations(null, StorableClassValidator2.class));
                    add(cc.getAnnotations(null, StorableClassValidator3.class));
                    add(cc.getAnnotations(null, StorableValidateNotNull.class));
                    add(cc.getAnnotations(null, StorableValidateMandatoryInJson.class));
                    add(cc.getAnnotations(null, StorableValidateCondition.class));
                    add(cc.getAnnotations(null, StorableValidateCondition2.class));
                    add(cc.getAnnotations(null, StorableValidateCondition3.class));
                    add(cc.getAnnotations(null, StorableValidateRegex.class));
                    add(cc.getAnnotations(null, StorableDeprecatedSince.class));
                    add(cc.getAnnotations(null, StorableAvailableSince.class));
                    add(cc.getAnnotations(null, StorableUnique.class));
                    add(cc.getAnnotations(null, StorableValidateTimestamp.class));
                    add(cc.getAnnotations(null, StorableValidateTimestampRelative.class));
                    add(cc.getAnnotations(null, StorableValidateTimeSpan.class));
                }
                if (context != null) {
                    cc = context.getter.classCache;
                    add(cc.getAnnotations(context.key, FlexiEnumFallback.class));
                    add(cc.getAnnotations(context.key, StorableClassValidator1.class));
                    add(cc.getAnnotations(context.key, StorableClassValidator2.class));
                    add(cc.getAnnotations(context.key, StorableClassValidator3.class));
                    add(cc.getAnnotations(context.key, StorableValidateNotNull.class));
                    add(cc.getAnnotations(context.key, StorableValidateMandatoryInJson.class));
                    add(cc.getAnnotations(context.key, StorableValidateCondition.class));
                    add(cc.getAnnotations(context.key, StorableValidateCondition2.class));
                    add(cc.getAnnotations(context.key, StorableValidateCondition3.class));
                    add(cc.getAnnotations(context.key, StorableValidateRegex.class));
                    add(cc.getAnnotations(context.key, StorableDeprecatedSince.class));
                    add(cc.getAnnotations(context.key, StorableUnique.class));
                    add(cc.getAnnotations(context.key, StorableAvailableSince.class));
                    add(cc.getAnnotations(context.key, StorableValidateTimestamp.class));
                    add(cc.getAnnotations(context.key, StorableValidateTimestampRelative.class));
                    add(cc.getAnnotations(context.key, StorableValidateTimeSpan.class));
                }
            }
        }
    }

    /**
     * @return
     * @throws InterruptedException
     * @throws FlexiMapperException
     */
    public List<ValidatorException> validate() throws InterruptedException {
        toDos = new ArrayList<ValidatetoDoss>();
        exceptions = new ArrayList<ValidatorException>();
        final FlexiJSonMapper mapper = createMapper();
        extendMapper(mapper);
        try {
            FlexiJSonNode extended = rootNode;
            // catch type mappings enum mappin errors etc
            // if (rootType.isPrimitive() || rootType.isEnum(true) || rootType.isString()) {
            // throw new IllegalArgumentException("Type is " + rootType);
            // }
            // if (rootType.raw != null) {
            // if (rootType.isArray() || rootType.isCollection() || rootType.isMap()) {
            // // hmm
            // } else {
            // // clone - this code creates a new node structure, that contains all default values as well and removes unknown
            // // properties. We need defaultvalues for cross field validations.
            // StorableValidator<T>.AddDefaultsMapper defMapper = new AddDefaultsMapper();
            // extendMapper(defMapper);
            // T deserialized = (T) defMapper.jsonToObject(rootNode, rootType);
            // FlexiJSonNode withDefaultValues = new FlexiJSonMapper().objectToJsonNode(deserialized);
            // extended = withDefaultValues;
            // }
            // }
            result = (T) mapper.jsonToObject(extended, rootType);
            HashMap<String, PathHandler> customPathhandlers = new HashMap<String, Condition.PathHandler>();
            // resolve top a list or map that contains only non-default values
            customPathhandlers.put("§nondefaults", new PathHandler() {
                public Scope resolve(Scope oldScope, Scope scope, Object key) {
                    // remove default values
                    FlexiJSonMapper mapper = new FlexiJSonMapper();
                    mapper.setIgnoreDefaultValuesEnabled(true);
                    try {
                        FlexiJSonNode newValue = mapper.objectToJsonNode(scope.getLast());
                        if (newValue instanceof FlexiJSonArray) {
                            scope.add(mapper.jsonToObject(newValue, TypeRef.OBJECT_ARRAY), key);
                            return scope;
                        } else {
                            scope.add(mapper.jsonToObject(newValue, TypeRef.OBJECT), key);
                            return scope;
                        }
                    } catch (FlexiMapperException e) {
                        throw new WTFException(e);
                    }
                }
            });
            // Resolve to the json node instead of the mapped object. NOTE: §node cannot be used everywhere, since the scope path may
            // contain operators
            customPathhandlers.put("§node", new PathHandler() {
                public Scope resolve(Scope oldScope, Scope scope, Object key) {
                    // remove default values
                    JSPath path = scope.getPath();
                    LinkedList<Object> elements = new LinkedList<Object>(path.getElements());
                    if ("§§THIS".equals(elements.removeFirst())) {
                        FlexiJSonNode targetNode = rootNode.resolvePath(JSPath.fromPathElements(elements.toArray(new Object[] {})));
                        scope.add(targetNode, key);
                        return scope;
                    }
                    throw new WTFException("Path not supported");
                }
            });
            customPathhandlers.put("§§context", new PathHandler() {
                public Scope resolve(Scope oldScope, Scope scope, Object key) {
                    scope.add(getContext(), key);
                    return scope;
                }
            });
            customPathhandlers.put(null, new PathHandler() {
                public Scope resolve(Scope oldScope, Scope scope, Object key) {
                    if (key instanceof String && "§type".equalsIgnoreCase((String) key)) {
                        if (scope.getLast() instanceof FlexiJSonObject) {
                            scope.add("OBJECT", key);
                            return scope;
                        } else if (scope.getLast() instanceof FlexiJSonArray) {
                            scope.add("ARRAY", key);
                            return scope;
                        } else if (scope.getLast() instanceof FlexiJSonValue) {
                            scope.add(((FlexiJSonValue) scope.getLast()).getType().name(), key);
                            return scope;
                        }
                    }
                    if (key instanceof String && ((String) key).startsWith("§")) {
                        return null;
                    }
                    if (scope.getLast() instanceof FlexiJSonObject) {
                        scope.add(((FlexiJSonObject) scope.getLast()).getNode(String.valueOf(key)), key);
                        return scope;
                    } else if (scope.getLast() instanceof FlexiJSonArray) {
                        scope.add(((FlexiJSonArray) scope.getLast()).get(((Number) key).intValue()), key);
                        return scope;
                    }
                    return null;
                }
            });
            Map<String, PathHandler> before = Condition.PATH_HANDLERS.get();
            try {
                Condition.PATH_HANDLERS.set(customPathhandlers);
                run(result, extended, rootType, new JSPath(), null);
            } finally {
                Condition.PATH_HANDLERS.set(before);
            }
        } catch (ClassCastFlexiMapperException e) {
            add(new InvalidTypeException(StorableValidator.this, fromFlexiNode(e.node), e.node, e.type, e.getMessage(), FailLevel.ERROR));
        } catch (FlexiMapperException e) {
            add(e);
        } catch (RuntimeInterruptedException e) {
            // we throw an actual interruptexception - time to clear the flag
            Thread.interrupted();
            throw new InterruptedException();
        }
        return exceptions;
    }

    /**
     * @param node
     * @return
     */
    private static JSPath fromFlexiNode(FlexiJSonNode node) {
        try {
            return FlexiUtils.fromFlexiNode(node);
        } catch (InvalidPathException e) {
            throw new WTFException(e);
        }
    }

    /**
     * @param path
     * @param result2
     * @param extended
     * @param rootType2
     * @throws InterruptedException
     * @throws InvocationTargetException
     * @throws IllegalAccessException
     * @throws IllegalArgumentException
     */
    private void run(Object object, FlexiJSonNode node, CompiledType type, JSPath path, Property context) throws InterruptedException {
        if (type.raw == null) {
            // unresolvable generics
            type = CompiledType.OBJECT;
            DebugMode.debugger();
        }
        StorableValidator<T>.ValidatetoDoss todo = new ValidatetoDoss(node, object, type, context, path);
        if (type.hasAnnotation(StorableHidden.class)) {
            return;
        }
        if (context != null) {
            if (context.getter.classCache.getAnnotations(context.key, StorableValidatorIgnoreKey.class).size() > 0) {
                return;
            }
            if (context.getter.classCache.getAnnotations(context.key, StorableHidden.class).size() > 0) {
                return;
            }
        }
        // System.out.println(todo.path);
        validateToDo(todo);
        if (type.isInstanceOf(TimeSpan.class, Date.class) && node instanceof FlexiJSonValue) {
            // nothing to do
        } else if (type.isPrimitive() || type.isString()) {
            // stop here
        } else if (type.isObject()) {
            // there is nothing we can do from now on
        } else if (type.isMap()) {
            if (!(node instanceof FlexiJSonObject)) {
                // The mapper will create an error for this issue
                return;
            }
            if (object == null) {
                // if the object is null, there is no reason to go deeper
                return;
            }
            for (KeyValueElement e : ((FlexiJSonObject) node).getElements()) {
                Object newObject = ((Map) object).get(e.getKey());
                JSPath newPath = path.derive(e.getKey());
                FlexiJSonNode newNode = e.getValue();
                run(newObject, newNode, type.getComponentTypeFor(Map.class), newPath, null);
            }
        } else if (type.isListContainer()) {
            if (!(node instanceof FlexiJSonArray)) {
                // The mapper will create an error for this issue
                return;
            }
            if (object == null) {
                // if the object is null, there is no reason to go deeper
                return;
            }
            List<Object> values = type.getListElements(object);
            for (int i = 0; i < values.size(); i++) {
                Object newObject = values.get(i);
                JSPath newPath = path.derive(i);
                FlexiJSonNode newNode = ((FlexiJSonArray) node).get(i);
                run(newObject, newNode, type.getComponentTypeFor(Array.class, Collection.class), newPath, null);
            }
        } else if (type.isEnum(true)) {
            // stop here
        } else {
            if (!(node instanceof FlexiJSonObject)) {
                // The mapper will create an error for this issue
                return;
            }
            if (object == null) {
                // if the object is null, there is no reason to go deeper
                return;
            }
            ClassCache cc = type.getClassCache();
            Set<String> keys = ((FlexiJSonObject) node).getKeys();
            for (String key : cc.getKeys()) {
                keys.remove(key);
                Property property = cc.getProperty(key);
                JSPath newPath = path.derive(key);
                Object newObject;
                try {
                    newObject = property.getter.getValue(object);
                    FlexiJSonNode newNode = ((FlexiJSonObject) node).getNode(key);
                    CompiledType newType = null;
                    newType = type.resolve(JSPath.fromPathString(key));
                    if (newType.raw == null && newObject != null) {
                        // unresolvable generics
                        newType = CompiledType.create(newObject.getClass(), type);
                    }
                    if (newType.raw == null) {
                        // unresolvable generics
                        newType = CompiledType.OBJECT;
                    }
                    run(newObject, newNode, newType, newPath, property);
                } catch (IllegalArgumentException e) {
                    throw new WTFException(e);
                } catch (IllegalAccessException e) {
                    throw new WTFException(e);
                } catch (InvocationTargetException e) {
                    throw new WTFException(e);
                } catch (CannotResolvePathException e) {
                    throw new WTFException(e);
                } catch (InvalidPathException e) {
                    throw new WTFException(e);
                }
            }
            for (String key : keys) {
                FlexiJSonNode subNode = ((FlexiJSonObject) node).resolvePath(JSPath.fromPathElements(key));
                add(new UnknownPropertyException(StorableValidator.this, path.derive(key), subNode, null));
            }
        }
    }

    public void scanType(CompiledType orgType) {
        CompiledType type = orgType;
        ClassCache cc = type.getClassCache();
        for (String key : type.getClassCache().getKeys()) {
            Property property = type.getClassCache().getProperty(key);
        }
    }

    protected StorableValidator<T>.CheckerMapper createMapper() {
        return new CheckerMapper();
    }

    /**
     * @param defMapper
     */
    protected void extendMapper(FlexiJSonMapper mapper) {
    }

    public T getResult() {
        return result;
    }

    protected void validateToDo(StorableValidator<T>.ValidatetoDoss toDo) throws InterruptedException {
        for (final Annotation c : toDo.annotations) {
            if (Thread.interrupted()) {
                throw new InterruptedException();
            }
            if (isIgnore(toDo, c)) {
                continue;
            }
            try {
                if (c instanceof StorableDeprecatedSince && toDo.node != null) {
                    StorableDeprecatedSince condition = (StorableDeprecatedSince) c;
                    if (rootNode instanceof FlexiJSonObject) {
                        if (((FlexiJSonObject) rootNode).resolvePath(toDo.path) == null) {
                            // do not evaluate deprecated, because the property is not part of the rootNode
                            DebugMode.debugger();
                            continue;
                        }
                    }
                    String desc = condition.message();
                    long ts = ((Date) new DateMapper().json2Obj(null, new FlexiJSonValue(condition.value()), CompiledType.create(Date.class), null)).getTime();
                    BuildsInfo buildInfo = getTargetBuildsinfo(toDo.node);
                    if (StringUtils.isEmpty(desc)) {
                        desc = "[Deprecated since " + DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.MEDIUM).format(ts) + "]";
                    } else {
                        desc = "[Deprecated since " + DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.MEDIUM).format(ts) + "] " + desc;
                    }
                    DeprecatedException ex = new DeprecatedException(StorableValidator.this, ts, toDo.path, toDo.node, toDo.type, desc, condition.level());
                    if (buildInfo != null && ex.handle(buildInfo) == TargetBuildIssue.NO_TARGET_BUILDS_AFFECTED) {
                        continue;
                    }
                    add(ex);
                }
                if (c instanceof StorableAvailableSince && toDo.node != null) {
                    StorableAvailableSince condition = (StorableAvailableSince) c;
                    String path = FlexiUtils.getPathString(toDo.node);
                    if (rootNode instanceof FlexiJSonObject) {
                        if (((FlexiJSonObject) rootNode).resolvePath(path) == null) {
                            // do not evaluate availableValidator, because the property is not part of the rootNode
                            continue;
                        }
                    }
                    String desc = condition.message();
                    long ts = ((Date) new DateMapper().json2Obj(null, new FlexiJSonValue(condition.value()), CompiledType.create(Date.class), null)).getTime();
                    if (StringUtils.isEmpty(desc)) {
                        desc = "[Available since " + DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.MEDIUM).format(new Date(ts)) + "]";
                    } else {
                        desc = "[Available since " + DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.MEDIUM).format(new Date(ts)) + "] " + desc;
                    }
                    BuildsInfo buildInfo = getTargetBuildsinfo(toDo.node);
                    AvailableSinceException ex = new AvailableSinceException(StorableValidator.this, ts, toDo.path, toDo.node, toDo.type, desc, condition.level());
                    if (buildInfo != null && ex.handle(buildInfo) == TargetBuildIssue.NO_TARGET_BUILDS_AFFECTED) {
                        continue;
                    }
                    add(ex);
                }
                if (c instanceof StorableValidateTimestampRelative) {
                    StorableValidateTimestampRelative condition = (StorableValidateTimestampRelative) c;
                    if (!(toDo.node instanceof FlexiJSonValue)) {
                        add(new InvalidTypeException(StorableValidator.this, toDo.path, toDo.node, toDo.type, condition.message(), condition.level()));
                        continue;
                    }
                    Date value = (Date) new FlexiJSonMapper().jsonToObject(toDo.node, CompiledType.create(Date.class));
                    if (value == null) {
                        add(new NullException(StorableValidator.this, toDo.path, toDo.node, toDo.type, condition.message(), condition.level()));
                        continue;
                    }
                    long max = System.currentTimeMillis() + condition.max();
                    long min = System.currentTimeMillis() + condition.min();
                    if (value.getTime() >= min && value.getTime() <= max) {
                        continue;
                    }
                    add(new InvalidTimestampException(StorableValidator.this, toDo.path, toDo.node, toDo.type, condition.message(), condition.level(), min, max, value.getTime()));
                }
                if (c instanceof StorableValidateTimestamp) {
                    StorableValidateTimestamp condition = (StorableValidateTimestamp) c;
                    if (!(toDo.node instanceof FlexiJSonValue)) {
                        add(new InvalidTypeException(StorableValidator.this, toDo.path, toDo.node, toDo.type, condition.message(), condition.level()));
                        continue;
                    }
                    // if (((FlexiJSonValue) toDo.node).getType() != org.appwork.storage.simplejson.Type.LONG) {
                    // add(exceptions, new InvalidTypeException(StorableValidator.this, toDo.node, toDo.type, condition.message(),
                    // condition.level()));
                    // continue;
                    // }
                    // long value = ((Number) ((FlexiJSonValue) toDo.node).getValue()).longValue();
                    Date value = (Date) new FlexiJSonMapper().jsonToObject(toDo.node, CompiledType.create(Date.class));
                    if (value == null) {
                        add(new NullException(StorableValidator.this, toDo.path, toDo.node, toDo.type, condition.message(), condition.level()));
                        continue;
                    }
                    long now = System.currentTimeMillis();
                    long max = condition.max();
                    if (max == -1) {
                        max = now;
                    }
                    long min = condition.min();
                    if (min == -1) {
                        min = now;
                    }
                    if (value.getTime() >= min && value.getTime() <= max) {
                        continue;
                    }
                    add(new InvalidTimestampException(StorableValidator.this, toDo.path, toDo.node, toDo.type, condition.message(), condition.level(), min, max, value.getTime()));
                }
                // if (toDo.timespanvalidator != null) {
                // for (final StorableValidateTimeSpan condition : toDo.timespanvalidator) {
                if (c instanceof StorableValidateTimeSpan) {
                    StorableValidateTimeSpan condition = (StorableValidateTimeSpan) c;
                    TimeSpan value = (TimeSpan) toDo.value;
                    if (value == null) {
                        add(new NullException(StorableValidator.this, toDo.path, toDo.node, toDo.type, condition.message(), condition.level()));
                        continue;
                    }
                    TimeSpan max = null;
                    TimeSpan min = null;
                    if (StringUtils.isNotEmpty(condition.max())) {
                        max = TimeSpan.parse(condition.max());
                    }
                    if (StringUtils.isNotEmpty(condition.min())) {
                        min = TimeSpan.parse(condition.min());
                    }
                    if (value.isMoreThan(max)) {
                        add(new ValidatorInvalidTimeSpanException(StorableValidator.this, toDo.path, toDo.node, toDo.type, condition.message(), condition.level(), min, max, value));
                    }
                    if (value.isLessThan(min)) {
                        add(new ValidatorInvalidTimeSpanException(StorableValidator.this, toDo.path, toDo.node, toDo.type, condition.message(), condition.level(), min, max, value));
                    }
                }
                // if (toDo.regexValidator != null) {
                // for (final StorableValidateRegex condition : toDo.regexValidator) {
                if (c instanceof StorableValidateRegex) {
                    StorableValidateRegex condition = (StorableValidateRegex) c;
                    if (toDo.node instanceof FlexiJSonValue) {
                        if (((FlexiJSonValue) toDo.node).getType() != org.appwork.storage.simplejson.ValueType.STRING && ((FlexiJSonValue) toDo.node).getType() != org.appwork.storage.simplejson.ValueType.NULL) {
                            add(new InvalidTypeException(StorableValidator.this, toDo.path, toDo.node, toDo.type, condition.message(), condition.level()));
                            continue;
                        }
                    }
                    CompiledType ct = toDo.value == null ? toDo.type : CompiledType.create(toDo.value.getClass());
                    if (toDo.value instanceof String) {
                        if (toDo.value == null) {
                            if (condition.nullAllowed()) {
                                return;
                            }
                            add(new NullException(StorableValidator.this, toDo.path, toDo.node, toDo.type, condition.message(), condition.level()));
                        }
                        if (StringUtils.isEmpty((String) toDo.value) && !condition.nullAllowed()) {
                            add(new EmptyStringException(StorableValidator.this, toDo.path, toDo.node, toDo.type, condition.message(), condition.level()));
                        } else {
                            try {
                                Pattern.compile((String) toDo.value);
                            } catch (final Exception e) {
                                add(new InvalidRegularExpressionException(StorableValidator.this, e, toDo.path, toDo.node, toDo.type, condition.message(), condition.level()));
                            }
                        }
                    } else if (ct.isListContainer()) {
                        // Check if all list elements are valid regexes.
                        // Do not do a nullcheck on the list itself - this can be done with NotNull Annotation
                        if (toDo.value != null) {
                            FlexiJSonArray array = (FlexiJSonArray) toDo.node;
                            for (int i = 0; i < ct.getListLength(toDo.value); i++) {
                                Object v = ct.getListElement(toDo.value, i);
                                FlexiJSonNode newNode = array == null ? null : array.get(i);
                                if (v == null && !condition.nullAllowed()) {
                                    add(new NullException(StorableValidator.this, toDo.path.derive(i), newNode, toDo.type.getComponentType(), condition.message(), condition.level()));
                                    continue;
                                }
                                if (!(v instanceof String)) {
                                    add(new InvalidTypeException(StorableValidator.this, toDo.path.derive(i), newNode, toDo.type.getComponentType(), condition.message(), condition.level()));
                                    continue;
                                }
                                if (StringUtils.isEmpty((String) v) && !condition.nullAllowed()) {
                                    add(new EmptyStringException(StorableValidator.this, toDo.path.derive(i), newNode, toDo.type.getComponentType(), condition.message(), condition.level()));
                                    continue;
                                }
                                try {
                                    Pattern.compile((String) v);
                                } catch (final Exception e) {
                                    add(new InvalidRegularExpressionException(StorableValidator.this, e, toDo.path.derive(i), newNode, toDo.type.getComponentType(), condition.message(), condition.level()));
                                }
                            }
                        }
                    }
                }
                if (c instanceof StorableUnique) {
                    validateUnique(toDo, (StorableUnique) c);
                }
                if (c instanceof StorableValidateMandatoryInJson) {
                    validateMandatory(toDo, (StorableValidateMandatoryInJson) c);
                }
                if (c instanceof StorableClassValidator1) {
                    validateClassValidator(toDo, ((StorableClassValidator1) c).cls(), ((StorableClassValidator1) c).parameter(), ((StorableClassValidator1) c).level(), ((StorableClassValidator1) c).message());
                }
                if (c instanceof StorableClassValidator2) {
                    validateClassValidator(toDo, ((StorableClassValidator2) c).cls(), ((StorableClassValidator2) c).parameter(), ((StorableClassValidator2) c).level(), ((StorableClassValidator2) c).message());
                }
                if (c instanceof StorableClassValidator3) {
                    validateClassValidator(toDo, ((StorableClassValidator3) c).cls(), ((StorableClassValidator3) c).parameter(), ((StorableClassValidator3) c).level(), ((StorableClassValidator3) c).message());
                }
                if (c instanceof StorableValidateNotNull) {
                    validateNotNull(toDo, (StorableValidateNotNull) c);
                }
                if (c instanceof StorableValidateCondition) {
                    StorableValidateCondition condition = (StorableValidateCondition) c;
                    validateCondition(toDo, condition.value(), condition.level(), condition.logic(), condition.description());
                }
                if (c instanceof StorableValidateCondition2) {
                    StorableValidateCondition2 condition = (StorableValidateCondition2) c;
                    validateCondition(toDo, condition.value(), condition.level(), condition.logic(), condition.description());
                }
                if (c instanceof StorableValidateCondition3) {
                    StorableValidateCondition3 condition = (StorableValidateCondition3) c;
                    validateCondition(toDo, condition.value(), condition.level(), condition.logic(), condition.description());
                }
            } catch (final Exception e) {
                add(new ValidatorException(StorableValidator.this, e, toDo.path, toDo.node, toDo.type, null, FailLevel.ERROR));
            }
        }
    }

    /**
     * @param toDo
     * @param c
     * @return
     */
    protected boolean isIgnore(StorableValidator<T>.ValidatetoDoss toDo, Annotation c) {
        return false;
    }

    private FlexiJSonObject getTargetBuildsNode(FlexiJSonNode node) {
        while (node != null) {
            if (node instanceof FlexiJSonObject) {
                final FlexiJSonNode targetBuilds = ((FlexiJSonObject) node).getNode(getTargetBuildsProperty());
                if (targetBuilds != null && targetBuilds instanceof FlexiJSonObject) {
                    return (FlexiJSonObject) targetBuilds;
                }
            }
            node = node.getParent();
        }
        return null;
    }

    /**
     * @param node
     * @return
     * @throws FlexiMapperException
     */
    private BuildsInfo getTargetBuildsinfo(FlexiJSonNode node) throws FlexiMapperException {
        FlexiJSonObject ret = getTargetBuildsNode(node);
        if (ret != null) {
            return createMapper().jsonToObject(ret, BuildsInfo.TYPE);
        }
        return null;
    }

    /**
     * @param toDo
     * @param cls
     * @param parameter
     * @param level
     * @param message
     * @throws SecurityException
     * @throws NoSuchMethodException
     * @throws IllegalAccessException
     * @throws InstantiationException
     * @throws InterruptedException
     */
    private void validateClassValidator(StorableValidator<T>.ValidatetoDoss toDo, Class<? extends StorableAbstractValidator> cls, String parameter, FailLevel level, String message) throws NoSuchMethodException, SecurityException, InstantiationException, IllegalAccessException, InterruptedException {
        List<? extends ValidatorException> toadd = ((StorableAbstractValidator) cls.newInstance()).validate(this, result, toDo.value, toDo.node, toDo.path, toDo.type, parameter, level, message);
        if (toadd != null) {
            for (ValidatorException e : toadd) {
                add(e);
            }
        }
    }

    /**
     * @param result2
     * @param exceptions2
     * @param toDo
     * @param a
     */
    private void validateNotNull(StorableValidator<T>.ValidatetoDoss toDo, StorableValidateNotNull a) {
        try {
            final Condition c = new Condition(toDo.path.withPrefix(Condition.$$THIS).toPathString(true), FlexiCondition.parse("{$ne:null}", true, Condition.class));
            boolean result = c.matches(this.result);
            if (!result) {
                add(new ValidatorValueIsNullException(StorableValidator.this, toDo.path, toDo.node, toDo.type, a));
            }
        } catch (final Exception e) {
            throw new WTFException(e);
        }
    }

    /**
     * @param result2
     * @param exceptions2
     * @param toDo
     * @param a
     * @throws InterruptedException
     */
    private void validateMandatory(StorableValidator<T>.ValidatetoDoss toDo, StorableValidateMandatoryInJson a) throws InterruptedException {
        if (toDo.node != null) {
            return;
        }
        add(new ValidatorMandatoryPropertyMissingException(this, toDo.type, toDo.path, a));
    }

    private void validateUnique(StorableValidator<T>.ValidatetoDoss toDo, StorableUnique a) throws InterruptedException {
        if (toDo.node == null) {
            return;
        }
        HashSet<Object> dupeCheck = new HashSet<Object>();
        if (toDo.node instanceof FlexiJSonArray) {
            NEXT_ENTRY: for (FlexiJSonNode entry : (FlexiJSonArray) toDo.node) {
                try {
                    Object uniqueValue;
                    uniqueValue = JSPath.fromPathString(a.value()).resolve(entry);
                    if (a.caseInSensitive()) {
                        if (uniqueValue instanceof String) {
                            uniqueValue = ((String) uniqueValue).toLowerCase(Locale.ROOT);
                        } else if (uniqueValue instanceof FlexiJSonValue && ((FlexiJSonValue) uniqueValue).getType() == ValueType.STRING) {
                            uniqueValue = ((String) (((FlexiJSonValue) uniqueValue).getValue())).toLowerCase(Locale.ROOT);
                        }
                    }
                    try {
                        if (dupeCheck.contains(uniqueValue)) {
                            add(new ValidatorNonUniqueKeyException(this, toDo.type, toDo.path, toDo.node, a, uniqueValue));
                            continue NEXT_ENTRY;
                        }
                        for (Object o : dupeCheck) {
                            if (CompareUtils.equalsDeep(o, uniqueValue)) {
                                add(new ValidatorNonUniqueKeyException(this, toDo.path, toDo.node, toDo.type, "Invalid Key", a, null, a.level()));
                                continue NEXT_ENTRY;
                            }
                        }
                    } finally {
                        dupeCheck.add(uniqueValue);
                    }
                } catch (Exception e) {
                    add(new ValidatorNonUniqueKeyException(this, toDo.path, toDo.node, toDo.type, "Invalid Key", a, null, a.level()));
                    break;
                }
            }
        }
        // add(new ValidatorNonUniqueKeyException(this, toDo.type, toDo.path, a, ""));
    }

    /**
     * @param exceptions
     * @param toDo
     * @param value
     * @param level
     * @param logic
     * @param description
     * @throws InterruptedException
     */
    private void validateCondition(StorableValidator<T>.ValidatetoDoss toDo, String value, FailLevel level, StorableValidationLogic logic, String desc) throws InterruptedException {
        if (StringUtils.isEmpty(desc)) {
            switch (logic) {
            case FAIL_ON_MATCH:
                desc = "Must not match the condition " + value;
                break;
            default:
                desc = "Must match the condition " + value;
            }
        }
        try {
            Condition condition = FlexiCondition.parse(value, true, Condition.class);
            Condition options = (Condition) condition.get(Condition.$OPTIONS);
            if (options == null) {
                options = new Condition();
            }
            options.put(Condition.FILTER_ROOT, true);
            condition.put(Condition.$OPTIONS, options);
            final Condition c = new Condition(toDo.path.withPrefix(Condition.$$THIS).toPathString(true), condition);
            c._setDebug(true);
            c._setLogger(LogV3.defaultLogger());
            boolean result = c.matches(this.result);
            Matcher matcher = Pattern.compile("[^\\\\](§[\\w\\.\\[\\]\\d\\§]+)").matcher(desc);
            while (matcher.find()) {
                String g = matcher.group(1);
                // ;
                // JSPath.fromPathString(g).resolve(options)
                final Condition c2 = new Condition();
                JSPath abs = toDo.path.withPrefix(Condition.$$THIS);
                for (Object e : JSPath.fromPathString(g).getElements()) {
                    if (e instanceof String && StringUtils.equalsIgnoreCase(Condition.$$THIS, (String) e)) {
                        // ignore §§this. this would result in an exception
                    } else {
                        abs.add(e);
                    }
                }
                final Scope ret = c2.resolveKeyPath(new Scope(this.result), abs);
                Object replacement = ret.getLast();
                if (replacement != null && replacement != Condition.KEY_DOES_NOT_EXIST) {
                    desc = desc.replace(g, String.valueOf(replacement));
                }
            }
            desc = desc.replace("\\§", "§");
            if ((!result && logic == StorableValidationLogic.OK_ON_MATCH) || (result && logic == StorableValidationLogic.FAIL_ON_MATCH)) {
                add(new ConditionException(StorableValidator.this, c, toDo.path, toDo.node, toDo.type, desc, level));
            }
        } catch (final Exception e) {
            LogV3.warning("Bad Validation Condition: " + this.result.getClass() + ":\r\n" + value);
            LogV3.log(e);
            add(new ConditionException(StorableValidator.this, null, e, toDo.path, toDo.node, toDo.type, "Invalid Condition: " + Exceptions.getStackTrace(e), level));
        }
    }
}
