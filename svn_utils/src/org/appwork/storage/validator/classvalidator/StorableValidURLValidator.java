/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2024, AppWork GmbH <e-mail@appwork.org>
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
package org.appwork.storage.validator.classvalidator;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.appwork.storage.FailLevel;
import org.appwork.storage.StorableValidator;
import org.appwork.storage.StorableValidator.ValidatorException;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.JSPath;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.StringUtils;
import org.appwork.utils.net.URLHelper;
import org.appwork.utils.reflection.CompiledType;

/**
 * @author thomas
 * @date 15.12.2022
 *
 */
public class StorableValidURLValidator extends StorableAbstractValidator {
    public static class InvalidHTTPURLException extends ValidatorException {
        public InvalidHTTPURLException(StorableValidator validator, Throwable cause, JSPath path, FlexiJSonNode value, CompiledType targetType, String message, FailLevel failLevel) {
            super(validator, cause, path, value, targetType, message, failLevel);
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.StorableAbstractValidator#validate(org.appwork.storage.StorableValidator, java.lang.Object,
     * java.lang.Object, org.appwork.storage.flexijson.FlexiJSonNode, org.appwork.storage.flexijson.JSPath,
     * org.appwork.utils.reflection.CompiledType, java.lang.String, org.appwork.storage.FailLevel, java.lang.String)
     */
    @Override
    public List<? extends ValidatorException> validate(StorableValidator validator, Object root, Object value, FlexiJSonNode node, JSPath path, CompiledType type, String parameter, FailLevel level, String message) {
        final ArrayList<InvalidHTTPURLException> ret = new ArrayList<InvalidHTTPURLException>();
        final List<String> urls = new ArrayList<String>();
        if (value instanceof String) {
            urls.add((String) value);
        } else {
            final List<Object> list = ReflectionUtils.wrapList(value, false, Object.class);
            if (list != null) {
                for (Object entry : list) {
                    if (entry instanceof String) {
                        urls.add((String) entry);
                    }
                }
            }
        }
        if (urls.size() == 0) {
            ret.add(new InvalidHTTPURLException(validator, null, path, node, type, StringUtils.isEmpty(message) ? getDocsDescription(parameter, this) : message, level));
        }
        for (final String url : urls) {
            try {
                URLHelper.verifyURL(new URL(url));
            } catch (MalformedURLException e) {
                ret.add(new InvalidHTTPURLException(validator, e, path, node, type, StringUtils.isEmpty(message) ? ("Malformed URL") : message, level));
            }
        }
        return ret;
    }

    @Override
    public String getDocsDescription(String parameter, Object anno) {
        return "The value must be a single or multiple valid URL (String,Array,Set,List,Collection)";
    }
}
