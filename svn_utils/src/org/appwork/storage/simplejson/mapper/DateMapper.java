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
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.storage.simplejson.mapper;

import java.util.Date;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.flexijson.FlexiJSonValue;
import org.appwork.storage.flexijson.mapper.FlexiMapperException;
import org.appwork.storage.simplejson.JSonNode;
import org.appwork.storage.simplejson.JSonValue;
import org.appwork.utils.reflection.CompiledType;

/**
 * @author thomas
 *
 */
public class DateMapper extends TypeMapper<Date> {
    private static final CompiledType                                        DATE_TYPE = CompiledType.create(Date.class);
    private final org.appwork.storage.flexijson.mapper.typemapper.DateMapper DELEGATE  = new org.appwork.storage.flexijson.mapper.typemapper.DateMapper();

    @Override
    public JSonNode obj2Json(Date obj) {
        return new JSonValue(obj.getTime());
    }

    @Override
    public Date json2Obj(final JSonNode json) {
        try {
            final JSonValue value = (JSonValue) json;
            switch (value.getType()) {
            case STRING:
                return (Date) DELEGATE.json2Obj(null, new FlexiJSonValue(((String) value.getValue())), DATE_TYPE, null);
            case LONG:
                return (Date) DELEGATE.json2Obj(null, new FlexiJSonValue(((Number) value.getValue())), DATE_TYPE, null);
            case NULL:
                return (Date) DELEGATE.json2Obj(null, new FlexiJSonValue((String) null), DATE_TYPE, null);
            default:
                final Object nodeValue = value.getValue();
                if (nodeValue == null) {
                    return (Date) DELEGATE.json2Obj(null, new FlexiJSonValue((String) null), DATE_TYPE, null);
                } else if (nodeValue instanceof String) {
                    return (Date) DELEGATE.json2Obj(null, new FlexiJSonValue(((String) nodeValue)), DATE_TYPE, null);
                } else if (nodeValue instanceof Number) {
                    return (Date) DELEGATE.json2Obj(null, new FlexiJSonValue(((Number) nodeValue)), DATE_TYPE, null);
                } else {
                    return null;
                }
            }
        } catch (FlexiMapperException e) {
            throw new WTFException(e);
        }
    }
}
