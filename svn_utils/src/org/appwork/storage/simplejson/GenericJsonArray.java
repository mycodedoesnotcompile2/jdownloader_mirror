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
package org.appwork.storage.simplejson;

import java.util.ArrayList;

import org.appwork.exceptions.WTFException;

/**
 * @author thomas
 * @date 21.04.2021
 *
 */
public class GenericJsonArray<T extends JSonNode> extends ArrayList<T> {

    public GenericJsonArray() {
        super();
    }

    public GenericJsonArray(int size) {
        super(size);
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append("[");
        for (final T n : this) {
            if (sb.length() > 1) {
                sb.append(",");
            }
            sb.append(n.toString());
        }
        sb.append("]");
        return sb.toString();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.simplejson.JSonNode#toPrettyString()
     */

    public String toPrettyString() {
        if (size() == 0) {
            return "[]";
        }
        final StringBuilder sb = new StringBuilder();
        try {
            sb.append("[\r\n");
            for (final JSonNode n : this) {
                if (sb.length() > 3) {
                    sb.append(",\r\n");
                }
                String[] lines = n.toPrettyString().split("[\r\n]+");
                boolean first = true;
                for (String line : lines) {
                    if (first) {
                    } else {
                        sb.append("\r\n");
                    }
                    first = false;
                    sb.append(org.appwork.storage.simplejson.JSonObject.PRETTY_PRINT_LAYER_INSET + line);
                }
            }
            sb.append("\r\n]");
            if (sb.length() < 40) {
                return toString();
            }
        } catch (Throwable e) {
            System.out.println(1);
            throw new WTFException(e);
        }
        return sb.toString();
    }
}
