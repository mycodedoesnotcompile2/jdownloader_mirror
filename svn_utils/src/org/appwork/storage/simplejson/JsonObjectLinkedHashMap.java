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
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.appwork.utils.StringUtils;

/**
 * @author daniel
 * @date Jun 24, 2022
 *
 */
public class JsonObjectLinkedHashMap extends LinkedHashMap<String, JSonNode> implements JSonObject {
    protected String          close                    = "\r\n}";
    protected String          keyValueDeliminator      = " : ";
    protected String          empty                    = "{}";
    protected String          open                     = "{\r\n";
    protected String          fieldDeliminator         = ",\r\n";
    /**
     *
     */
    public static String      PRETTY_PRINT_LAYER_INSET = " ";
    /**
     *
     */
    private static final long serialVersionUID         = 1L;

    /**
     *
     */
    public JsonObjectLinkedHashMap() {
        super();
    }

    public JsonObjectLinkedHashMap(Map<String, JSonNode> map) {
        super(map);
    }

    /**
     * This to String method MUST ALWAYS return proper JSON and without ANY whitespace or fillings. This method is used for signature
     * validations and should not get changed
     */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append("{");
        Map.Entry<String, JSonNode> next;
        for (final Iterator<Map.Entry<String, JSonNode>> it = this.entrySet().iterator(); it.hasNext();) {
            if (sb.length() > 1) {
                sb.append(",");
            }
            next = it.next();
            sb.append(toStringKey(next.getKey()));
            sb.append(":");
            sb.append(next.getValue().toString());
        }
        sb.append("}");
        return sb.toString();
    }

    protected String toStringKey(String key) {
        return "\"" + JSonUtils.escape(key) + "\"";
    }

    public String toPrettyString() {
        if (size() == 0) {
            return empty;
        }
        final StringBuilder sb = new StringBuilder();
        sb.append(open);
        int keyLength = 0;
        get(null);
        for (java.util.Map.Entry<String, JSonNode> es : entrySet()) {
            keyLength = Math.max(JSonUtils.escape(es.getKey()).length() + 2/* quotes */, keyLength);
        }
        Map.Entry<String, JSonNode> next;
        for (String key : getPrettyPrintKeys()) {
            if (sb.length() > 3) {
                sb.append(fieldDeliminator);
            }
            sb.append(PRETTY_PRINT_LAYER_INSET + StringUtils.fillPost(toStringKey(key), " ", keyLength));
            sb.append(keyValueDeliminator);
            String value = get(key).toPrettyString();
            String[] lines = value.split("[\r\n]+");
            boolean first = true;
            if (lines.length > 1) {
                for (String line : lines) {
                    if (!first) {
                        sb.append("\r\n" + PRETTY_PRINT_LAYER_INSET + StringUtils.fillPre("", " ", keyLength + 3/* : */) + line);
                    } else {
                        sb.append(line);
                    }
                    first = false;
                }
            } else {
                sb.append(value);
            }
        }
        sb.append(close);
        return sb.toString();
    }

    /**
     * can be used to print the json object fields in a desired order
     *
     * @return
     */
    protected List<String> getPrettyPrintKeys() {
        return new ArrayList<String>(keySet());
    }
}
