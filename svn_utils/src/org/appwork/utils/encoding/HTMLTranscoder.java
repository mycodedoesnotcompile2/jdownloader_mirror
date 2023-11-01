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
package org.appwork.utils.encoding;

import java.text.CharacterIterator;
import java.text.StringCharacterIterator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map.Entry;

/**
 * @author thomas
 * 
 */
public class HTMLTranscoder {
    private static final HashMap<Character, String> MAP         = new HashMap<Character, String>();
    private static final HashMap<Character, String> MAP_SIMPLE  = new HashMap<Character, String>();
    private static final HashMap<String, Character> MAP_REVERSE = new HashMap<String, Character>();
    static {
        HTMLTranscoder.MAP_SIMPLE.put('<', "&lt;");
        HTMLTranscoder.MAP_SIMPLE.put('>', "&gt;");
        HTMLTranscoder.MAP_SIMPLE.put('&', "&amp;");
        HTMLTranscoder.MAP_SIMPLE.put('"', "&quot;");
        HTMLTranscoder.MAP_SIMPLE.put('\n', "<br>");
        HTMLTranscoder.MAP_SIMPLE.put('\r', "<br>");

        HTMLTranscoder.MAP.put('\n', "<br>");
        HTMLTranscoder.MAP.put('\r', "<br>");
        HTMLTranscoder.MAP.put('<', "&lt;");
        HTMLTranscoder.MAP.put('>', "&gt;");
        HTMLTranscoder.MAP.put('&', "&amp;");
        HTMLTranscoder.MAP.put('"', "&quot;");
        HTMLTranscoder.MAP.put('\t', "&#009;");
        HTMLTranscoder.MAP.put('!', "&#033;");
        HTMLTranscoder.MAP.put('#', "&#035;");
        HTMLTranscoder.MAP.put('$', "&#036;");
        HTMLTranscoder.MAP.put('%', "&#037;");
        HTMLTranscoder.MAP.put('\'', "&#039;");
        HTMLTranscoder.MAP.put('(', "&#040;");
        HTMLTranscoder.MAP.put(')', "&#041;");
        HTMLTranscoder.MAP.put('*', "&#042;");
        HTMLTranscoder.MAP.put('+', "&#043;");
        HTMLTranscoder.MAP.put(',', "&#044;");
        HTMLTranscoder.MAP.put('-', "&#045;");
        HTMLTranscoder.MAP.put('.', "&#046;");
        HTMLTranscoder.MAP.put('/', "&#047;");
        HTMLTranscoder.MAP.put(':', "&#058;");
        HTMLTranscoder.MAP.put(';', "&#059;");
        HTMLTranscoder.MAP.put('=', "&#061;");
        HTMLTranscoder.MAP.put('?', "&#063;");
        HTMLTranscoder.MAP.put('@', "&#064;");
        HTMLTranscoder.MAP.put('[', "&#091;");
        HTMLTranscoder.MAP.put('\\', "&#092;");
        HTMLTranscoder.MAP.put(']', "&#093;");
        HTMLTranscoder.MAP.put('^', "&#094;");
        HTMLTranscoder.MAP.put('_', "&#095;");
        HTMLTranscoder.MAP.put('`', "&#096;");
        HTMLTranscoder.MAP.put('{', "&#123;");
        HTMLTranscoder.MAP.put('|', "&#124;");
        HTMLTranscoder.MAP.put('}', "&#125;");
        HTMLTranscoder.MAP.put('~', "&#126;");

        // reverse map for decode
        Entry<Character, String> next;
        for (final Iterator<Entry<Character, String>> it = HTMLTranscoder.MAP.entrySet().iterator(); it.hasNext();) {
            next = it.next();
            HTMLTranscoder.MAP_REVERSE.put(next.getValue(), next.getKey());

        }

    }

    /**
     * @param text
     * @return
     */
    public static String encode(final String text) {

        final StringBuilder result = new StringBuilder();
        final StringCharacterIterator iterator = new StringCharacterIterator(text);
        char character = iterator.current();

        String rep = null;
        while (character != CharacterIterator.DONE) {
            rep = HTMLTranscoder.MAP.get(character);
            if (rep == null) {
                result.append(character);
            } else {
                result.append(rep);
            }

            character = iterator.next();
        }
        return result.toString();
    }

    /**
     * 
     * encodes only newline, <>" and &
     * 
     * @param text
     * 
     * @return
     */
    public static String encodeSimple(final String text) {

        final StringBuilder result = new StringBuilder();
        final StringCharacterIterator iterator = new StringCharacterIterator(text);
        char character = iterator.current();
        char lastC = 0;

        String rep = null;
        while (character != CharacterIterator.DONE) {
            if (character == '\n' && lastC == '\r') {
                lastC = character;
                character = iterator.next();
            }
            rep = HTMLTranscoder.MAP_SIMPLE.get(character);
            if (rep == null) {
                result.append(character);
            } else {
                result.append(rep);
            }
            lastC = character;
            character = iterator.next();
        }
        return result.toString();
    }

    public static void main(final String[] args) {
        final String text = "<b>I'm a fat & htmled test</b>";

        final String encoded = HTMLTranscoder.encode(text);
        System.out.println(encoded);
        // final String decoded = HTMLTranscoder.decode(encoded);
        // System.out.println(decoded);
        // System.out.println(decoded.equals(text));
    }

}
