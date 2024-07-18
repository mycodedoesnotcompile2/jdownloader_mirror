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

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.appwork.loggingv3.LogV3;
import org.appwork.moncompare.Condition;
import org.appwork.storage.BuildsInfo;
import org.appwork.storage.FailLevel;
import org.appwork.storage.MapperType;
import org.appwork.storage.Storable;
import org.appwork.storage.StorableDoc;
import org.appwork.storage.StorableSupportedMappers;
import org.appwork.storage.StorableValidateNotNull;
import org.appwork.storage.StorableValidator;
import org.appwork.storage.StorableValidator.ValidatorException;
import org.appwork.storage.StorableValidator.ValidatorValueIsNullException;
import org.appwork.storage.flexijson.CannotResolvePathException;
import org.appwork.storage.flexijson.FlexiJSonArray;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.FlexiJSonValue;
import org.appwork.storage.flexijson.InvalidPathException;
import org.appwork.storage.flexijson.JSPath;
import org.appwork.storage.validator.classvalidator.StorableAbstractValidator;
import org.appwork.storage.validator.classvalidator.StorableClassValidator1;
import org.appwork.utils.reflection.CompiledType;
import org.appwork.utils.reflection.JavaSyntax;

/**
 * @author thomas
 * @date 03.07.2023
 *
 */
@StorableSupportedMappers(MapperType.AW_FLEXI)
public class JsonModification<TargetType, MatcherType> implements Storable {
    public static class UnsetValidator extends StorableAbstractValidator {
        /**
         *
         */
        public UnsetValidator() {
            // TODO Auto-generated constructor stub
        }

        /*
         * (non-Javadoc)
         *
         * @see org.appwork.storage.validator.classvalidator.StorableAbstractValidator#validate(org.appwork.storage.StorableValidator,
         * java.lang.Object, java.lang.Object, org.appwork.storage.flexijson.FlexiJSonNode, org.appwork.storage.flexijson.JSPath,
         * org.appwork.utils.reflection.CompiledType, java.lang.String, org.appwork.storage.FailLevel, java.lang.String)
         */
        @Override
        public List<? extends ValidatorException> validate(StorableValidator validator, Object root, Object value, FlexiJSonNode node, JSPath path, CompiledType type, String parameter, FailLevel level, String message) {
            ArrayList<ValidatorException> ret = new ArrayList<ValidatorException>();
            if (node == null) {
                return ret;
                // ret.add(new ValidatorValueIsNullException(validator, path, node, type, "May not be null", FailLevel.ERROR));
            }
            if (!(node instanceof FlexiJSonArray)) {
                ret.add(new org.appwork.storage.StorableValidator.InvalidTypeException(validator, path, node, type, "Array<String> expected!", FailLevel.ERROR));
            }
            JSPath mod = path.getParent();
            CompiledType toCheckType;
            try {
                toCheckType = validator.getRootType().resolve(mod).getComponentType();
                // CompiledType toCheckType = validator.getRootType().getComponentType().getComponentType();
                ;
                for (int i = 0; i < ((FlexiJSonArray) node).size(); i++) {
                    FlexiJSonNode element = ((FlexiJSonArray) node).get(i);
                    String key = ((FlexiJSonValue) element).getStringValue();
                    if (key == null) {
                        ret.add(new ValidatorValueIsNullException(validator, path.derive(i), element, CompiledType.STRING, "May not be null", FailLevel.ERROR));
                    } else {
                        try {
                            try {
                                CompiledType exists = toCheckType.resolve(JSPath.fromPathString(key));
                                ret.add(new ValidatorException(validator, path.derive(i), element, null, "Will delete '" + JSPath.fromPathString(key).toPathString(false) + "' of type " + exists.toString(new JavaSyntax()), FailLevel.INFO));
                            } catch (CannotResolvePathException e) {
                                LogV3.log(e);
                                ret.add(new ValidatorException(validator, path.derive(i), element, null, "Unknown element - the path " + JSPath.fromPathString(key) + " does not exist in " + toCheckType.toString(new JavaSyntax())));
                            }
                        } catch (InvalidPathException e) {
                            // TODO Auto-generated catch block
                            e.printStackTrace();
                        }
                    }
                }
            } catch (CannotResolvePathException e) {
                LogV3.log(e);
                ret.add(new ValidatorException(validator, path, node, null, "Unknown Error in JsonModification.unset validator"));
            }
            return ret;
        }

        /*
         * (non-Javadoc)
         *
         * @see org.appwork.storage.validator.classvalidator.StorableAbstractValidator#getDocsDescription(java.lang.String,
         * java.lang.Object)
         */
        @Override
        public String getDocsDescription(String parameter, Object anno) {
            return null;
        }
    }

    private BuildsInfo targetBuilds;

    @StorableDoc("Information about the target Clients this configuration should be used with. This information is used to create warnings or to avoid that unsupported or deprecated features are used.")
    @StorableValidateNotNull(description = "It is recommended to set up a proper targetClients property.", level = FailLevel.WARNING)
    public BuildsInfo getTargetBuilds() {
        return this.targetBuilds;
    }

    public void setTargetBuilds(final BuildsInfo clientInfo) {
        this.targetBuilds = clientInfo;
    }

    private Condition<MatcherType>[] conditions;

    @StorableDoc("The Conditions property is used to specify under which circumstances a modification is applied. All conditions must match - if a single condition does not match, the mod will not get applied")
    public Condition<MatcherType>[] getConditions() {
        return conditions;
    }

    public void setConditions(Condition<MatcherType>[] conditions) {
        this.conditions = conditions;
    }

    @StorableDoc("Can be used to store a comment about the modification. This comment is not used in application logic.")
    private String comment;

    public String getComment() {
        return comment;
    }

    public void setComment(String comment) {
        this.comment = comment;
    }

    @StorableDoc("A int from -2147483648 to 2147483647. Modifications with higher priority are applied first.")
    private int priority = 0;

    public int getPriority() {
        return priority;
    }

    public void setPriority(int priority) {
        this.priority = priority;
    }

    private TargetType setIfUnset;

    @StorableDoc("Define fields to set if the path is not available in the target object - this can be used to define defaults")
    public TargetType getSetIfUnset() {
        return setIfUnset;
    }

    public void setSetIfUnset(TargetType setIfUnset) {
        this.setIfUnset = setIfUnset;
    }

    private TargetType set;

    @StorableDoc("Define fields to set. Fields will get overwrittten")
    public TargetType getSet() {
        return set;
    }

    public void setSet(TargetType set) {
        this.set = set;
    }

    private Set<String> unset;

    @StorableClassValidator1(cls = UnsetValidator.class)
    public Set<String> getUnset() {
        return unset;
    }

    public void setUnset(Set<String> unset) {
        this.unset = unset;
    }
}
