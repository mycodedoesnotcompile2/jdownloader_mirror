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
package org.appwork.storage.validator.classvalidator;

import java.util.ArrayList;
import java.util.List;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.FailLevel;
import org.appwork.storage.StorableValidator;
import org.appwork.storage.StorableValidator.ValidatorException;
import org.appwork.storage.flexijson.FlexiJSONParser;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.FlexiJSonObject;
import org.appwork.storage.flexijson.FlexiParserException;
import org.appwork.storage.flexijson.JSPath;
import org.appwork.utils.StringUtils;
import org.appwork.utils.reflection.CompiledType;

/**
 * @author thomas
 * @date 15.12.2022
 *
 */
public class StorableNumberValidator extends StorableAbstractValidator {
    public static class InvalidNumberException extends ValidatorException {
        /**
         * @param validator
         * @param cause
         * @param path
         * @param value
         * @param targetType
         * @param message
         * @param failLevel
         */
        public InvalidNumberException(StorableValidator validator, Throwable cause, JSPath path, FlexiJSonNode value, CompiledType targetType, String message, FailLevel failLevel) {
            super(validator, cause, path, value, targetType, message, failLevel);
            // TODO Auto-generated constructor stub
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.StorableAbstractValidator#validate(java.lang.Object, java.lang.Object,
     * org.appwork.storage.flexijson.FlexiJSonNode, org.appwork.storage.flexijson.JSPath, org.appwork.utils.reflection.CompiledType,
     * java.lang.String)
     */
    @Override
    public List<? extends ValidatorException> validate(StorableValidator validator, Object root, Object value, FlexiJSonNode node, JSPath path, CompiledType type, String parameter, FailLevel level, String message) {
        try {
            ArrayList<InvalidNumberException> ret = new ArrayList<InvalidNumberException>();
            if (value instanceof Number) {
                FlexiJSonObject params = (FlexiJSonObject) new FlexiJSONParser(parameter).parse();
                Double gt = params.getNE(">", Double.class);
                if (gt != null) {
                    int comResult = gt.compareTo(((Number) value).doubleValue());
                    if (comResult >= 0) {
                        ret.add(new InvalidNumberException(validator, null, path, node, type, StringUtils.isEmpty(message) ? ("The value must be bigger than " + gt) : message, level));
                    }
                }
            } else {
                ret.add(new InvalidNumberException(validator, null, path, node, type, StringUtils.isEmpty(message) ? ("The value must be a number") : message, level));
            }
            return ret;
        } catch (FlexiParserException e) {
            throw new WTFException(e);
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.StorableAbstractValidator#getDocsDescription(java.lang.String, java.lang.Object)
     */
    @Override
    public String getDocsDescription(String parameter, Object anno) {
        FlexiJSonObject params;
        try {
            params = (FlexiJSonObject) new FlexiJSONParser(parameter).parse();
            Double gt = params.getNE(">", Double.class);
            if (gt != null) {
                return "Value must be greater than (>) " + gt;
            }
            return null;
        } catch (FlexiParserException e) {
            throw new WTFException(e);
        }
    }
}
