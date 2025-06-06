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
package org.appwork.storage.flexijson.mapper;

import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.simplejson.mapper.Getter;
import org.appwork.storage.simplejson.mapper.Setter;
import org.appwork.utils.reflection.CompiledType;

/**
 * @author thomas
 * @date 18.01.2023
 *
 */
public abstract class FlexiTypeMapperAdapter implements FlexiTypeMapper {
    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.mapper.FlexiTypeMapper#obj2JSon(org.appwork.storage.flexijson.mapper.FlexiJSonMapper,
     * java.lang.Object, org.appwork.storage.simplejson.mapper.Getter, java.util.List)
     */
    @Override
    public FlexiJSonNode obj2JSon(FlexiJSonMapper mapper, Object obj, Getter reference, DefaultObjectToJsonContext context) throws FlexiMapperException {
		return null;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.mapper.FlexiTypeMapper#json2Obj(org.appwork.storage.flexijson.mapper.FlexiJSonMapper,
     * org.appwork.storage.flexijson.FlexiJSonNode, org.appwork.utils.reflection.CompiledType, org.appwork.storage.simplejson.mapper.Setter)
     */
    @Override
    public Object json2Obj(FlexiJSonMapper mapper, FlexiJSonNode node, CompiledType type, Setter reference) throws FlexiMapperException {
		return null;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.mapper.FlexiTypeMapper#canConvert2Json(java.lang.Object,
     * org.appwork.storage.simplejson.mapper.Getter)
     */
    @Override
    public boolean canConvert2Json(Object obj, Getter getter) {
		return false;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.flexijson.mapper.FlexiTypeMapper#canConvert2Object(org.appwork.storage.flexijson.FlexiJSonNode,
     * org.appwork.utils.reflection.CompiledType, org.appwork.storage.simplejson.mapper.Setter)
     */
    @Override
    public boolean canConvert2Object(FlexiJSonNode node, CompiledType type, Setter setter) {
		return false;
    }
}
