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
package org.appwork.txtresource;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map.Entry;

import org.appwork.exceptions.WTFException;
import org.appwork.utils.DebugMode;

/**
 * @author Thomas
 * @date 15.05.2020
 *
 */
public class TranslatedEntry {

    // private static final boolean CHAR_ARRAY_IMPLEMENTATION = false;

    private final String raw;
    private Object[]     parts;
    private volatile int expectedSize;

    /**
     * @param string
     */
    public TranslatedEntry(String raw) {
        this.raw = raw;
    }

    /**
     * @return
     */
    public String getRaw() {
        return raw;
    }

    public String format(Method method, Object[] args) {
        if (args != null && args.length > 0) {
            try {
                precompile(method);
                final TranslatedStringBuilder sb = new TranslatedStringBuilder(expectedSize);
                for (int i = 0; i < parts.length; i++) {
                    final Object p = parts[i];
                    if (p instanceof Integer) {
                        // args[Index]
                        Object arg = args[((Integer) p).intValue()];
                        if (arg != null && arg instanceof LocaleMap) {
                            arg = ((LocaleMap) arg).resolve();
                        }
                        sb.append(arg);

                    } else if (p instanceof int[]) {
                        sb.append(raw, ((int[]) p)[0], ((int[]) p)[1]);
                    } else if (p instanceof String) {
                        sb.append((String) p);
                    } else if (p instanceof char[]) {
                        sb.append((char[]) p);
                    } else {
                        throw new WTFException("Unsupported:" + p.getClass());
                    }
                }
                expectedSize = sb.length();
                return sb.toString();
            } catch (final RuntimeException e) {
                if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                    throw e;
                } else {
                    return "FIXME:" + raw;
                }
            }
        } else {
            return raw;
        }
    }

    protected void precompile(Method method) {
        if (parts == null) {
            final Annotation[][] annotations = method.getParameterAnnotations();
            final HashMap<String, Integer> splitOn = new HashMap<String, Integer>();
            for (int i = 0; i < method.getParameterTypes().length; i++) {
                splitOn.put("%s" + (i + 1), i);
                for (Annotation ap : annotations[i]) {
                    if (ap instanceof Name) {
                        splitOn.put("%" + ((Name) ap).value() + "%", (i));
                        // do not break. Multiple Names may be ok
                    }
                }
            }
            final ArrayList<Object> parts = new ArrayList<Object>();
            int last = 0;
            while (true) {
                int first = -1;
                String split = null;
                for (Entry<String, Integer> s : splitOn.entrySet()) {
                    int index = raw.indexOf(s.getKey(), last);
                    if (index >= 0) {
                        if (first < 0 || first > index) {
                            first = index;
                            split = s.getKey();
                        }
                    }
                }
                if (first >= 0) {
                    if (first - last > 0) {
                        // if (CHAR_ARRAY_IMPLEMENTATION) {
                        // char[] ch = new char[first - last];
                        // raw.getChars(last, first, ch, 0);
                        // parts.add(ch);
                        // } else {
                        parts.add(new int[] { last, first });
                        // }
                    }
                    Integer index = splitOn.get(split);
                    parts.add(index.intValue());
                    last = first + split.length();
                    if (last == raw.length()) {
                        break;
                    }
                } else {
                    // if (CHAR_ARRAY_IMPLEMENTATION) {
                    // char[] ch = new char[raw.length() - last];
                    // raw.getChars(last, raw.length(), ch, 0);
                    // parts.add(ch);
                    // } else {
                    parts.add(new int[] { last, raw.length() });
                    // }
                    break;
                }
            }
            this.expectedSize = raw.length() * 2;
            this.parts = parts.toArray(new Object[] {});
            // if (CHAR_ARRAY_IMPLEMENTATION) {
            // this.raw = null;
            // }
        }
    }

}
