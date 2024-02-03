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
package org.appwork.storage.simplejson.mapper;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.SimpleTypeRef;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.FlexiJSonValue;
import org.appwork.storage.flexijson.mapper.FlexiMapperException;
import org.appwork.storage.flexijson.mapper.FlexiTypeMapper;
import org.appwork.storage.simplejson.JSonNode;
import org.appwork.storage.simplejson.JSonValue;
import org.appwork.utils.reflection.CompiledType;

/**
 * @author thomas
 * @date 20.09.2023
 *
 */
public class FlexiDeligateMapper<T> extends TypeMapper<T> {
    private final CompiledType    type;
    private final FlexiTypeMapper delegate;

    /**
     * @param simpleTypeRef
     * @param readableBytesMapper
     */
    public FlexiDeligateMapper(SimpleTypeRef<T> type, FlexiTypeMapper delegate) {
        this.type = CompiledType.create(type);
        this.delegate = delegate;
    }

    @Override
    public JSonNode obj2Json(T obj) {
        try {
            final FlexiJSonNode mapped = delegate.obj2JSon(null, obj, null, null);
            if (mapped instanceof FlexiJSonValue) {
                final FlexiJSonValue node = (FlexiJSonValue) mapped;
                switch (node.getType()) {
                case BOOLEAN:
                    return new JSonValue((Boolean) node.getValue());
                case DOUBLE:
                case LONG:
                    return new JSonValue((Number) node.getValue());
                case NULL:
                    return new JSonValue((String) null);
                case UNDEFINED:
                    return new JSonValue();
                case STRING:
                    return new JSonValue((String) node.getValue());
                default:
                    throw new WTFException("Unexpected Type:" + node.getType());
                }
            } else {
                throw new WTFException("Not supported yet");
            }
        } catch (FlexiMapperException e) {
            throw new WTFException(e);
        }
    }

    @Override
    public T json2Obj(final JSonNode json) {
        try {
            final JSonValue value = (JSonValue) json;
            switch (value.getType()) {
            case BOOLEAN:
                return (T) delegate.json2Obj(null, new FlexiJSonValue(((Boolean) value.getValue())), type, null);
            case DOUBLE:
            case LONG:
                return (T) delegate.json2Obj(null, new FlexiJSonValue(((Number) value.getValue())), type, null);
            case NULL:
                return (T) delegate.json2Obj(null, new FlexiJSonValue((String) null), type, null);
            case UNDEFINED:
                return (T) delegate.json2Obj(null, new FlexiJSonValue(), type, null);
            case STRING:
                return (T) delegate.json2Obj(null, new FlexiJSonValue(((String) value.getValue())), type, null);
            default:
                final Object nodeValue = value.getValue();
                if (nodeValue == null) {
                    return (T) delegate.json2Obj(null, new FlexiJSonValue((String) null), type, null);
                } else if (nodeValue instanceof String) {
                    return (T) delegate.json2Obj(null, new FlexiJSonValue(((String) nodeValue)), type, null);
                } else if (nodeValue instanceof Number) {
                    return (T) delegate.json2Obj(null, new FlexiJSonValue(((Number) nodeValue)), type, null);
                } else {
                    return null;
                }
            }
        } catch (FlexiMapperException e) {
            throw new WTFException(e);
        }
    }
}
