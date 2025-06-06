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
package org.appwork.storage.flexijson.mapper.typemapper;

import java.math.BigDecimal;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.FlexiJSonValue;
import org.appwork.storage.flexijson.mapper.DefaultObjectToJsonContext;
import org.appwork.storage.flexijson.mapper.FlexiJSonMapper;
import org.appwork.storage.flexijson.mapper.FlexiTypeMapper;
import org.appwork.storage.simplejson.mapper.Getter;
import org.appwork.storage.simplejson.mapper.Setter;
import org.appwork.utils.reflection.CompiledType;

/**
 * @author thomas
 *
 */
public class BigDecimalMapper implements FlexiTypeMapper {
    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.simplejson.mapper.FlexiTypeMapper#obj2Json(java.lang.Object )
     */
    public FlexiJSonNode obj2JSon(FlexiJSonMapper mapper, Object obj, Getter reference, DefaultObjectToJsonContext typeHirarchy) {
        final BigDecimal c = (BigDecimal) obj;
        if (mapper != null) {
            return mapper.createFlexiJSonValue(c.toString());
        } else {
            return new FlexiJSonValue(c.toString());
        }
    }
    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.simplejson.mapper.FlexiTypeMapper#json2Obj(org.appwork .storage.simplejson.JSonNode)
     */

    public Object json2Obj(FlexiJSonMapper mapper, FlexiJSonNode json, CompiledType type, Setter setter) {
        final Object value = ((FlexiJSonValue) json).getValue();
        if (value instanceof String) {
            return new BigDecimal((String) value);
        } else if (value instanceof Double) {
            return new BigDecimal(Double.toString(((Number) value).doubleValue()));
        } else if (value instanceof Float) {
            return new BigDecimal(Float.toString(((Number) value).floatValue()));
        } else if (value instanceof Number) {
            return new BigDecimal(((Number) value).longValue());
        } else {
            throw new WTFException("Unsupported");
        }
    }

    @Override
    public boolean canConvert2Json(Object obj, Getter getter) {
        return obj != null && obj instanceof BigDecimal;
    }

    @Override
    public boolean canConvert2Object(FlexiJSonNode node, CompiledType type, Setter setter) {
        return type.type == BigDecimal.class;
    }
}
