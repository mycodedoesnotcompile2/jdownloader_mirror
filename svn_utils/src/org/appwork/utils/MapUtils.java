/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
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
package org.appwork.utils;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * @author thomas
 * @date 22.05.2025
 *
 */
public class MapUtils {
    public static <K, V> Map<K, V> of(K k1, V v1) {
        LinkedHashMap<K, V> ret = new LinkedHashMap<K, V>();
        ret.put(k1, v1);
        return ret;
    }

    public static <K, V> Map<K, V> of(K k1, V v1, K k2, V v2) {
        LinkedHashMap<K, V> ret = new LinkedHashMap<K, V>();
        ret.put(k1, v1);
        ret.put(k2, v2);
        return ret;
    }

    public static <K, V> Map<K, V> of(K k1, V v1, K k2, V v2, K k3, V v3) {
        LinkedHashMap<K, V> ret = new LinkedHashMap<K, V>();
        ret.put(k1, v1);
        ret.put(k2, v2);
        ret.put(k3, v3);
        return ret;
    }

    public static <K, V> Map<K, V> of(K k1, V v1, K k2, V v2, K k3, V v3, K k4, V v4) {
        LinkedHashMap<K, V> ret = new LinkedHashMap<K, V>();
        ret.put(k1, v1);
        ret.put(k2, v2);
        ret.put(k3, v3);
        ret.put(k4, v4);
        return ret;
    }

    public static <K, V> Map<K, V> of(K k1, V v1, K k2, V v2, K k3, V v3, K k4, V v4, K k5, V v5) {
        LinkedHashMap<K, V> ret = new LinkedHashMap<K, V>();
        ret.put(k1, v1);
        ret.put(k2, v2);
        ret.put(k3, v3);
        ret.put(k4, v4);
        ret.put(k5, v5);
        return ret;
    }

    public static <K, V> Map<K, V> of(K k1, V v1, K k2, V v2, K k3, V v3, K k4, V v4, K k5, V v5, K k6, V v6) {
        LinkedHashMap<K, V> ret = new LinkedHashMap<K, V>();
        ret.put(k1, v1);
        ret.put(k2, v2);
        ret.put(k3, v3);
        ret.put(k4, v4);
        ret.put(k5, v5);
        ret.put(k6, v6);
        return ret;
    }

    public static <K, V> Map<K, V> of(K k1, V v1, K k2, V v2, K k3, V v3, K k4, V v4, K k5, V v5, K k6, V v6, K k7, V v7) {
        LinkedHashMap<K, V> ret = new LinkedHashMap<K, V>();
        ret.put(k1, v1);
        ret.put(k2, v2);
        ret.put(k3, v3);
        ret.put(k4, v4);
        ret.put(k5, v5);
        ret.put(k6, v6);
        ret.put(k7, v7);
        return ret;
    }

    public static <K, V> Map<K, V> of(K k1, V v1, K k2, V v2, K k3, V v3, K k4, V v4, K k5, V v5, K k6, V v6, K k7, V v7, K k8, V v8) {
        LinkedHashMap<K, V> ret = new LinkedHashMap<K, V>();
        ret.put(k1, v1);
        ret.put(k2, v2);
        ret.put(k3, v3);
        ret.put(k4, v4);
        ret.put(k5, v5);
        ret.put(k6, v6);
        ret.put(k7, v7);
        ret.put(k8, v8);
        return ret;
    }

    public static <K, V> Map<K, V> of(K k1, V v1, K k2, V v2, K k3, V v3, K k4, V v4, K k5, V v5, K k6, V v6, K k7, V v7, K k8, V v8, K k9, V v9) {
        LinkedHashMap<K, V> ret = new LinkedHashMap<K, V>();
        ret.put(k1, v1);
        ret.put(k2, v2);
        ret.put(k3, v3);
        ret.put(k4, v4);
        ret.put(k5, v5);
        ret.put(k6, v6);
        ret.put(k7, v7);
        ret.put(k8, v8);
        ret.put(k9, v9);
        return ret;
    }
}
