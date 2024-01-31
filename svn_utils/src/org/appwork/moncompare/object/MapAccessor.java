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
package org.appwork.moncompare.object;

import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.appwork.moncompare.Condition;
import org.appwork.utils.JVMVersion;
import org.appwork.utils.JavaVersion;

/**
 * @author thomas
 * @date 14.10.2023
 *
 */
public class MapAccessor implements MapAccessorInterface {
    final private Map<String, Object> map;

    /**
     * @param expression
     */
    public MapAccessor(Map<String, Object> expression) {
        this.map = expression;
    }

    /**
     * @see java.lang.Iterable#iterator()
     */
    @Override
    public Iterator<Entry<String, Object>> iterator() {
        return map.entrySet().iterator();
    }

    /**
     * @see org.appwork.moncompare.object.MapAccessorInterface#size()
     */
    @Override
    public int size() {
        return map.size();
    }

    /**
     * @see org.appwork.moncompare.object.MapAccessorInterface#keySet()
     */
    @Override
    public Set<String> keySet() {
        return map.keySet();
    }

    /**
     * @see org.appwork.moncompare.object.MapAccessorInterface#getOrDefault(java.lang.String, java.lang.Object)
     */
    @Override
    public Object get(String key) {
        if (JVMVersion.isAtLeast(JavaVersion.JVM_1_8)) {
            return map.getOrDefault(key, Condition.KEY_DOES_NOT_EXIST);
        } else {
            Object ret = map.get(key);
            if (ret == null) {
                if (map.containsKey(key)) {
                    return null;
                } else {
                    return Condition.KEY_DOES_NOT_EXIST;
                }
            } else {
                return ret;
            }
        }
    }

    /**
     * @see org.appwork.moncompare.object.MapAccessorInterface#put(java.lang.String, java.lang.Object)
     */
    @Override
    public Object put(String key, Object value) {
        boolean contains = map.containsKey(key);
        Object ret = map.put(key, value);
        return contains ? ret : Condition.KEY_DOES_NOT_EXIST;
    }

    /**
     * @see org.appwork.moncompare.object.MapAccessorInterface#remove(java.lang.String)
     */
    @Override
    public Object remove(String key) {
        boolean contains = map.containsKey(key);
        Object ret = map.remove(key);
        return contains ? ret : Condition.KEY_DOES_NOT_EXIST;
    }
}
