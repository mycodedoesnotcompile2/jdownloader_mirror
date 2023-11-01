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
package org.appwork.moncompare.fromjson;

import org.appwork.moncompare.BadFormatException;
import org.appwork.moncompare.Condition;
import org.appwork.storage.SimpleTypeRef;
import org.appwork.storage.flexijson.FlexiJSONParser;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.FlexiJSonValue;
import org.appwork.storage.flexijson.FlexiParserException;
import org.appwork.storage.flexijson.ParsingError;
import org.appwork.storage.flexijson.mapper.FlexiMapperException;

/**
 * @author thomas
 * @date 30.06.2021
 *
 */
public class FlexiCondition {
    public static Condition parse(String json) throws BadFormatException {
        return parse(json, false);
    }

    public static Condition parse(String json, final boolean convertDollar) throws BadFormatException {
        final Class<Condition> target = Condition.class;
        return parse(json, convertDollar, target);
    }

    public static <T extends Condition> T parse(String json, final boolean convertDollar, final Class<T> target) throws BadFormatException {
        try {
            FlexiJSonNode jsonNode = new FlexiJSONParser(json) {
                protected void throwParserException(ParsingError error, Object path, Throwable cause, org.appwork.storage.flexijson.FlexiJSonNode parent, org.appwork.storage.flexijson.FlexiJSonNode value) throws FlexiParserException {
                    switch (error) {
                    case ERROR_NUMBERFORMAT_BINARY:
                    case ERROR_NUMBERFORMAT_HEXADECIMAL:
                    case ERROR_NUMBERFORMAT_LEADING_PLUS:
                    case ERROR_NUMBERFORMAT_OCTAL:
                    case ERROR_STRING_TOKEN_WITH_SINGLE_QUOTES:
                    case ERROR_STRING_VALUE_WITH_SINGLE_QUOTES:
                    case ERROR_STRING_TOKEN_WITHOUT_QUOTES:
                    case ERROR_STRING_VALUE_WITHOUT_QUOTES:
                    case ERROR_KEY_WITHOUT_QUOTES:
                    case ERROR_KEY_WITH_SINGLE_QUOTES:
                        // accepted issues
                        return;
                    default:
                        super.throwParserException(error, path, cause, parent, value);
                    }
                };

                protected String modifyKey(String keyString) {
                    if (convertDollar) {
                        keyString = keyString.replaceAll("^\\$\\$", "§§").replaceAll("^\\$", "§");
                    }
                    return super.modifyKey(keyString);
                };

                public FlexiJSonValue createJSonValue(String value) {
                    if (value == null) {
                        return super.createJSonValue(value);
                    }
                    if (convertDollar) {
                        value = value.replaceAll("^\\$\\$", "§§").replaceAll("^\\$", "§");
                    }
                    return super.createJSonValue(value);
                };
            }.setDebug(new StringBuilder()).parse();
            return new FlexiConditionMapper(target).jsonToObject(jsonNode, new SimpleTypeRef<T>(target));
        } catch (FlexiParserException e) {
            throw new BadFormatException(e);
        } catch (FlexiMapperException e) {
            throw new BadFormatException(e);
        }
    }
}
