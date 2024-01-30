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
package org.appwork.utils.reflection;

import java.util.Collection;
import java.util.Map;

import org.appwork.utils.reflection.CompiledType.AbstractNameScheme;

/**
 * @author thomas
 * @date 08.11.2023
 *
 */
public class JsonSyntax extends AbstractNameScheme {
    protected String comma = ",";

    /**
     *
     */
    public JsonSyntax() {
    }

    /**
     * @see org.appwork.utils.reflection.CompiledType.AbstractNameScheme#getCollectionName(org.appwork.utils.reflection.CompiledType,
     *      java.lang.Class)
     */
    @Override
    public String getCollectionName(CompiledType type, Class<?> raw) {
        return "Array";
    }

    /**
     * @see org.appwork.utils.reflection.CompiledType.AbstractNameScheme#getMapName(org.appwork.utils.reflection.CompiledType,
     *      java.lang.Class)
     */
    @Override
    public String getMapName(CompiledType type, Class<?> raw) {
        if (!type.isImplementing(Map.class)) {
            return getClassName(type, raw);
        } else {
            return "Map";
        }
    }

    /**
     * @see org.appwork.utils.reflection.CompiledType.AbstractNameScheme#getArrayName(org.appwork.utils.reflection.CompiledType,
     *      java.lang.Class)
     */
    @Override
    public String getArrayName(CompiledType type, Class<?> raw) {
        return "Array";
    }

    /**
     * @see org.appwork.utils.reflection.CompiledType.AbstractNameScheme#getClassName(org.appwork.utils.reflection.CompiledType,
     *      java.lang.Class)
     */
    @Override
    public String getClassName(CompiledType type, Class<?> raw) {
        return raw.getSimpleName();
    }

    /**
     * @see org.appwork.utils.reflection.CompiledType.AbstractNameScheme#getEnumName(org.appwork.utils.reflection.CompiledType,
     *      java.lang.Class)
     */
    @Override
    public String getEnumName(CompiledType type, Class<?> raw) {
        return getClassName(type, raw) + "-Enum";
    }

    /**
     * @see org.appwork.utils.reflection.CompiledType.AbstractNameScheme#getComponentTypes(org.appwork.utils.reflection.CompiledType)
     */
    @Override
    public CompiledType[] getComponentTypes(CompiledType type, Class<?> raw) {
        if (raw != null) {
            // Maps and Collections are handled a bit different by json Mappers and thus need a bit different handling to return the JSON
            // Name
            if (type.isImplementing(Map.class)) {
                // LocaleMap extends HashMap<String,String>
                return type.getComponentTypes(Map.class);
            } else if (Collection.class.isAssignableFrom(raw)) {
                // thisincludes set
                return type.getComponentTypes(Collection.class);
            }
        }
        return type.getComponentTypes();
    }

    /**
     * @see org.appwork.utils.reflection.CompiledType.AbstractNameScheme#appendComponents(org.appwork.utils.reflection.CompiledType,
     *      java.lang.Class, java.lang.StringBuilder, org.appwork.utils.reflection.CompiledType[])
     */
    @Override
    public void appendComponents(CompiledType type, Class<?> raw, StringBuilder sb, CompiledType[] components) {
        if (components.length > 0) {
            sb.append("<");
            for (CompiledType t : components) {
                if (sb.charAt(sb.length() - 1) != '<') {
                    sb.append(comma);
                }
                t.appendName(this, sb);
            }
            sb.append(">");
        }
    }
}
