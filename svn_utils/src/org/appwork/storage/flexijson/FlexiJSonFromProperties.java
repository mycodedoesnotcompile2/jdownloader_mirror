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
package org.appwork.storage.flexijson;

import java.io.StringReader;
import java.util.Map.Entry;
import java.util.Properties;

import org.appwork.exceptions.WTFException;
import org.appwork.utils.StringUtils;

/**
 * @author thomas
 * @date 10.06.2021
 *
 */
public class FlexiJSonFromProperties {

    /**
     * Reads a Properties object and converts it into JSON
     *
     * @param p
     * @return
     * @throws FlexiParserException
     */

    public static FlexiJSonNode convert(Properties p) throws FlexiParserException {
        try {
            FlexiJSonNode root = null;

            root = merge(p, root);

            return root;
        } catch (Throwable e) {
            e.printStackTrace();
            throw new WTFException(e);
        }
    }

    public static FlexiJSonNode merge(Properties p, FlexiJSonNode root) throws FlexiParserException {
        for (Entry<Object, Object> e : p.entrySet()) {
            String key = e.getKey().toString();
            String valueString = StringUtils.valueOfOrNull(e.getValue());
            root = merge(root, key, valueString);

        }
        return root;
    }

    public static FlexiJSonNode merge(FlexiJSonNode root, String key, String valueString) throws FlexiParserException {
        FlexiJSonNode container = null;
        String[] parts = key.split("(\\.|(?=\\[))");
        partLoop: for (int i = 0; i < parts.length; i++) {
            if (i == 0 && root != null) {
                container = root;
                continue partLoop;
            }
            if (container != null) {
                if (container instanceof FlexiJSonObject) {

                    for (KeyValueElement el : ((FlexiJSonObject) container).getElements()) {
                        if (el.getKey().equals(parts[i - 1])) {
                            container = el.getValue();
                            continue partLoop;
                        }
                    }

                } else {
                    int index = Integer.parseInt(parts[i - 1].substring(1, parts[i - 1].length() - 1));
                    if (((FlexiJSonArray) container).size() > index) {
                        FlexiJSonNode ele = ((FlexiJSonArray) container).get(index);
                        if (!(ele instanceof FlexiJSonValue)) {
                            container = ele;
                            continue partLoop;
                        }

                    }

                }

            }
            FlexiJSonNode newContainer;
            if (parts[i].startsWith("[")) {
                try {
                    Integer.parseInt(parts[i].substring(1, parts[i].length() - 1));
                    newContainer = new FlexiJSonArray();
                } catch (NumberFormatException ex) {
                    // ["strange key"]
                    newContainer = new FlexiJSonObject();
                }

            } else {
                newContainer = new FlexiJSonObject();
            }
            if (root == null) {
                root = newContainer;
            }
            if (container != null) {
                if (container instanceof FlexiJSonObject) {

                    ((FlexiJSonObject) container).add(new KeyValueElement((FlexiJSonObject) container, parts[i - 1], newContainer));
                } else {
                    int index = Integer.parseInt(parts[i - 1].substring(1, parts[i - 1].length() - 1));
                    while (((FlexiJSonArray) container).size() < index) {
                        ((FlexiJSonArray) container).add(new FlexiJSonValue());
                    }
                    if (((FlexiJSonArray) container).size() == index) {
                        ((FlexiJSonArray) container).add(newContainer);

                    } else {
                        ((FlexiJSonArray) container).set(index, newContainer);

                    }
                }

            }
            container = newContainer;

        }

        FlexiJSonNode value = new FlexiJSONParser(new StringReader(valueString)) {
            protected void throwParserException(ParsingError error, Object path, Throwable cause, FlexiJSonNode parent, FlexiJSonNode value) throws FlexiParserException {
            };
        }.parse();
        if (parts[parts.length - 1].startsWith("[")) {
            try {
                int index = Integer.parseInt(parts[parts.length - 1].substring(1, parts[parts.length - 1].length() - 1));
                while (((FlexiJSonArray) container).size() < index) {
                    ((FlexiJSonArray) container).add(new FlexiJSonValue());
                }
                if (((FlexiJSonArray) container).size() == index) {
                    ((FlexiJSonArray) container).add(value);

                } else {
                    ((FlexiJSonArray) container).set(index, value);

                }
            } catch (NumberFormatException ex) {
                // ["strange key"]
                ((FlexiJSonObject) container).add(new KeyValueElement((FlexiJSonObject) container, parts[parts.length - 1], value));

            }
        } else {
            ((FlexiJSonObject) container).add(new KeyValueElement((FlexiJSonObject) container, parts[parts.length - 1], value));

        }
        return root;
    }

}
