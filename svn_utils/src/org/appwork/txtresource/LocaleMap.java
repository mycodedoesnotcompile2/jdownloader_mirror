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
package org.appwork.txtresource;

import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.appwork.storage.FailLevel;
import org.appwork.storage.SimpleTypeRef;
import org.appwork.storage.Storable;
import org.appwork.storage.StorableDoc;
import org.appwork.storage.StorableExample;
import org.appwork.storage.StorableValidateCondition;
import org.appwork.storage.StorableValidateCondition2;
import org.appwork.storage.StorableValidateCondition3;
import org.appwork.storage.StorableValidationLogic;
import org.appwork.storage.TypeRef;

/**
 * @author Thomas
 * @date 11.05.2019
 *
 */
@StorableDoc("Localised text map. {\"en\":\"...\"} is the default and fallback language, and must always be set!")
@StorableValidateCondition(value = "{en:{§exists:true,§ne:null}}", description = "At least an english translation is required")
@StorableValidateCondition2(value = "{§keys:{§each:{§or:[{§regex:\"\\\\p{Lower}{2}\"},{§regex:\"\\\\p{Lower}{2}_\\\\p{Upper}{2}\"},{§regex:\"\\\\p{Lower}{2}_\\\\p{Upper}{2}_\\\\S+\"}]}}}", description = "All keys must match the format \r\n<2 char language code lower case>_<2 char country code upper case>_<Variant id>, \r\nwhile country code and variant id are optional.")
// §node -> run on flexinode instead of actual types. §node->§type -> Flexi TYpe not java type
@StorableValidateCondition3(value = "{§node.§type:STRING}", description = "You should use the format {\"en\":\"...\"} instead", level = FailLevel.WARNING, logic = StorableValidationLogic.FAIL_ON_MATCH)
@StorableExample("{\"en\":\"Changelog\",\"de\":\"Änderungen\"}")
public class LocaleMap extends LinkedHashMap<String, String> implements Storable {
    /**
     *
     */
    private static final long              serialVersionUID = 1L;
    public static final TypeRef<LocaleMap> TYPE             = new SimpleTypeRef<LocaleMap>(LocaleMap.class);

    /**
     *
     */
    public LocaleMap(String locale, String msg) {
        this.put(locale, msg);
    }

    public LocaleMap append(String locale, String msg) {
        put(locale, msg);
        return this;
    }

    /**
     *
     */
    public LocaleMap() {
        // TODO Auto-generated constructor stub
    }

    /**
     * @param string
     */
    public LocaleMap(String def) {
        this("en", def);
    }

    /**
     * @param message
     */
    public LocaleMap(Map<String, String> message) {
        if (message != null) {
            putAll(message);
        }
    }

    public static String getBestMatch(Map<String, String> message) {
        if (message == null) {
            return null;
        }
        return getBestMatch(message, TranslationFactory.getDesiredLanguage());
    }

    /**
     * Gets the translation with the best local match from message
     *
     * @param message
     * @return
     */
    public static String getBestMatch(Map<String, String> message, String localeID) {
        if (message == null || message.size() == 0) {
            return null;
        }
        String ret = message.get(localeID);
        if (ret == null) {
            HashSet<String> variants = new HashSet<String>(TranslationFactory.getVariantsOf(localeID));
            String best = null;
            String lng = null;
            String first = null;
            for (Entry<String, String> es : message.entrySet()) {
                if (es.getValue() != null && first == null) {
                    first = es.getValue();
                }
                for (String v : TranslationFactory.getVariantsOf(es.getKey())) {
                    if (variants.contains(v) && (best == null || v.length() > best.length())) {
                        best = v;
                        lng = es.getKey();
                    }
                }
            }
            if (lng != null) {
                ret = message.get(lng);
            }
            if (ret == null) {
                ret = message.get("en");
            }
            if (ret == null) {
                ret = first;
            }
        }
        if (ret == null) {
            return null;
        }
        ret = ret.trim();
        return ret;
    }

    /**
     * @return
     */
    public String resolve() {
        return TranslationFactory.handleLocaleMap(this);
    }
}
