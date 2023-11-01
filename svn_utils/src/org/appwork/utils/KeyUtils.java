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
package org.appwork.utils;

import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;

import javax.swing.KeyStroke;

import org.appwork.utils.os.CrossSystem;

public class KeyUtils {
    /**
     * 
     */
    public static final String  MAC_TRANSLATED_REPLACE_SHIFT = KeyEvent.getKeyModifiersText(InputEvent.SHIFT_MASK) + "+";
    /**
     * 
     */
    private static final String MAC_TRANSLATED_REPLACE_ALT   = KeyEvent.getKeyModifiersText(InputEvent.ALT_MASK) + "+";
    /**
     * 
     */
    private static final String MAC_TRANSLATED_REPLACE_META  = KeyEvent.getKeyModifiersText(InputEvent.META_MASK) + "+";
    /**
     * 
     */
    public static final String  MAC_TRANSLATED_REPLACE_CTRL  = KeyEvent.getKeyModifiersText(InputEvent.CTRL_MASK) + "+";

    public static final String  MAC_REPLACE_SHIFT            = KeyUtils.getKeyModifiersText(InputEvent.SHIFT_MASK) + "+";
    /**
     * 
     */
    private static final String MAC_REPLACE_ALT              = KeyUtils.getKeyModifiersText(InputEvent.ALT_MASK) + "+";
    /**
     * 
     */
    private static final String MAC_REPLACE_META             = KeyUtils.getKeyModifiersText(InputEvent.META_MASK) + "+";
    /**
     * 
     */
    public static final String  MAC_REPLACE_CTRL             = KeyUtils.getKeyModifiersText(InputEvent.CTRL_MASK) + "+";
    /**
     * http://macbiblioblog.blogspot.de/2005/05/special-key-symbols.html
     */
    // ⎋ Escape U+238B
    // ⇥ Tab forward U+21E5
    // ⇤ Tab back U+21E4
    // ⇪ Capslock U+21EA
    // ⇧ Shift U+21E7
    // ⌃ Control U+2303
    // ⌥ Option (Alt, Alternative) U+2325
    //  Apple symbol 1 U+F8FF
    // ⌘ Command (Open Apple) 2 U+2318
    // ␣ Space U+2423
    // ⏎
    // ↩ Return U+23CE
    // U+21A9
    // ⌫ Delete back U+232B
    // ⌦ Delete forward U+2326
    // ﹖⃝ Help U+003F & U+20DD
    // ⇱
    // ↖
    // ↸ Home U+21F1
    // U+2196
    // U+21B8
    // ⇲
    // ↘ End U+21F2
    // U+2198
    // ⇞ Pageup U+21DE
    // ⇟ Pagedown U+21DF
    // ↑
    // ⇡ Up arrow U+2191
    // U+21E1
    // ↓
    // ⇣ Down arrow U+2193
    // U+21E3
    // ←
    // ⇠ Left arrow U+2190
    // U+21E0
    // →
    // ⇢ Right arrow U+2192
    // U+21E2
    // ⌧ Clear U+2327
    // ⇭ Numberlock U+21ED
    // ⌤ Enter U+2324
    // ⏏ Eject U+23CF
    // ⌽ Power 3 U+233D
    public static final String  MAC_META                     = "\u2318";

    public static final String  MAC_CTRL                     = "\u2303";

    public static final String  MAC_ALT                      = "\u2325";

    public static final String  MAC_SHIFT                    = "\u21E7";

    public static String getShortcutString(final KeyEvent event, final boolean translated) {
        return getShortcutString(event.getModifiers(), event.getKeyCode(), translated);
    }

    public static void main(final String[] args) {
        final KeyStroke ks = KeyStroke.getKeyStroke(KeyEvent.VK_F5, 0);
        System.out.println(ks);
        System.out.println(getShortcutString(ks, false));
        System.out.println(getShortcutString(ks, true));
    }

    // Taken and modified from the KeyEvent Class
    public static String getKeyText(final int keyCode) {
        if (keyCode >= KeyEvent.VK_0 && keyCode <= KeyEvent.VK_9 || keyCode >= KeyEvent.VK_A && keyCode <= KeyEvent.VK_Z) { return String.valueOf((char) keyCode); }

        switch (keyCode) {
        case KeyEvent.VK_ENTER:
            return ("Enter");
        case KeyEvent.VK_BACK_SPACE:
            return ("Backspace");
        case KeyEvent.VK_TAB:
            return ("Tab");
        case KeyEvent.VK_CANCEL:
            return ("Cancel");
        case KeyEvent.VK_CLEAR:
            return ("Clear");
        case KeyEvent.VK_COMPOSE:
            return ("Compose");
        case KeyEvent.VK_PAUSE:
            return ("Pause");
        case KeyEvent.VK_CAPS_LOCK:
            return ("Caps Lock");
        case KeyEvent.VK_ESCAPE:
            return ("Escape");
        case KeyEvent.VK_SPACE:
            return ("Space");
        case KeyEvent.VK_PAGE_UP:
            return ("Page Up");
        case KeyEvent.VK_PAGE_DOWN:
            return ("Page Down");
        case KeyEvent.VK_END:
            return ("End");
        case KeyEvent.VK_HOME:
            return ("Home");
        case KeyEvent.VK_LEFT:
            return ("Left");
        case KeyEvent.VK_UP:
            return ("Up");
        case KeyEvent.VK_RIGHT:
            return ("Right");
        case KeyEvent.VK_DOWN:
            return ("Down");
        case KeyEvent.VK_BEGIN:
            return ("Begin");

            // modifiers
        case KeyEvent.VK_SHIFT:
            return ("Shift");
        case KeyEvent.VK_CONTROL:
            return ("Control");
        case KeyEvent.VK_ALT:
            return ("Alt");
        case KeyEvent.VK_META:
            return CrossSystem.isMac() ? MAC_META : "Meta";
        case KeyEvent.VK_ALT_GRAPH:
            return ("Alt Graph");

            // punctuation
        case KeyEvent.VK_COMMA:
            return ("Comma");
        case KeyEvent.VK_PERIOD:
            return ("Period");
        case KeyEvent.VK_SLASH:
            return ("Slash");
        case KeyEvent.VK_SEMICOLON:
            return ("Semicolon");
        case KeyEvent.VK_EQUALS:
            return ("Equals");
        case KeyEvent.VK_OPEN_BRACKET:
            return ("Open Bracket");
        case KeyEvent.VK_BACK_SLASH:
            return ("Back Slash");
        case KeyEvent.VK_CLOSE_BRACKET:
            return ("Close Bracket");

            // numpad numeric keys handled below
        case KeyEvent.VK_MULTIPLY:
            return ("NumPad *");
        case KeyEvent.VK_ADD:
            return ("NumPad +");
        case KeyEvent.VK_SEPARATOR:
            return ("NumPad ,");
        case KeyEvent.VK_SUBTRACT:
            return ("NumPad -");
        case KeyEvent.VK_DECIMAL:
            return ("NumPad .");
        case KeyEvent.VK_DIVIDE:
            return ("NumPad /");
        case KeyEvent.VK_DELETE:
            return ("Delete");
        case KeyEvent.VK_NUM_LOCK:
            return ("Num Lock");
        case KeyEvent.VK_SCROLL_LOCK:
            return ("Scroll Lock");

        case KeyEvent.VK_WINDOWS:
            return ("Windows");
        case KeyEvent.VK_CONTEXT_MENU:
            return ("Context Menu");

        case KeyEvent.VK_F1:
            return ("F1");
        case KeyEvent.VK_F2:
            return ("F2");
        case KeyEvent.VK_F3:
            return ("F3");
        case KeyEvent.VK_F4:
            return ("F4");
        case KeyEvent.VK_F5:
            return ("F5");
        case KeyEvent.VK_F6:
            return ("F6");
        case KeyEvent.VK_F7:
            return ("F7");
        case KeyEvent.VK_F8:
            return ("F8");
        case KeyEvent.VK_F9:
            return ("F9");
        case KeyEvent.VK_F10:
            return ("F10");
        case KeyEvent.VK_F11:
            return ("F11");
        case KeyEvent.VK_F12:
            return ("F12");
        case KeyEvent.VK_F13:
            return ("F13");
        case KeyEvent.VK_F14:
            return ("F14");
        case KeyEvent.VK_F15:
            return ("F15");
        case KeyEvent.VK_F16:
            return ("F16");
        case KeyEvent.VK_F17:
            return ("F17");
        case KeyEvent.VK_F18:
            return ("F18");
        case KeyEvent.VK_F19:
            return ("F19");
        case KeyEvent.VK_F20:
            return ("F20");
        case KeyEvent.VK_F21:
            return ("F21");
        case KeyEvent.VK_F22:
            return ("F22");
        case KeyEvent.VK_F23:
            return ("F23");
        case KeyEvent.VK_F24:
            return ("F24");

        case KeyEvent.VK_PRINTSCREEN:
            return ("Print Screen");
        case KeyEvent.VK_INSERT:
            return ("Insert");
        case KeyEvent.VK_HELP:
            return ("Help");
        case KeyEvent.VK_BACK_QUOTE:
            return ("Back Quote");
        case KeyEvent.VK_QUOTE:
            return ("Quote");

        case KeyEvent.VK_KP_UP:
            return ("Up");
        case KeyEvent.VK_KP_DOWN:
            return ("Down");
        case KeyEvent.VK_KP_LEFT:
            return ("Left");
        case KeyEvent.VK_KP_RIGHT:
            return ("Right");

        case KeyEvent.VK_DEAD_GRAVE:
            return ("Dead Grave");
        case KeyEvent.VK_DEAD_ACUTE:
            return ("Dead Acute");
        case KeyEvent.VK_DEAD_CIRCUMFLEX:
            return ("Dead Circumflex");
        case KeyEvent.VK_DEAD_TILDE:
            return ("Dead Tilde");
        case KeyEvent.VK_DEAD_MACRON:
            return ("Dead Macron");
        case KeyEvent.VK_DEAD_BREVE:
            return ("Dead Breve");
        case KeyEvent.VK_DEAD_ABOVEDOT:
            return ("Dead Above Dot");
        case KeyEvent.VK_DEAD_DIAERESIS:
            return ("Dead Diaeresis");
        case KeyEvent.VK_DEAD_ABOVERING:
            return ("Dead Above Ring");
        case KeyEvent.VK_DEAD_DOUBLEACUTE:
            return ("Dead Double Acute");
        case KeyEvent.VK_DEAD_CARON:
            return ("Dead Caron");
        case KeyEvent.VK_DEAD_CEDILLA:
            return ("Dead Cedilla");
        case KeyEvent.VK_DEAD_OGONEK:
            return ("Dead Ogonek");
        case KeyEvent.VK_DEAD_IOTA:
            return ("Dead Iota");
        case KeyEvent.VK_DEAD_VOICED_SOUND:
            return ("Dead Voiced Sound");
        case KeyEvent.VK_DEAD_SEMIVOICED_SOUND:
            return ("Dead Semivoiced Sound");

        case KeyEvent.VK_AMPERSAND:
            return ("Ampersand");
        case KeyEvent.VK_ASTERISK:
            return ("Asterisk");
        case KeyEvent.VK_QUOTEDBL:
            return ("Double Quote");
        case KeyEvent.VK_LESS:
            return ("Less");
        case KeyEvent.VK_GREATER:
            return ("Greater");
        case KeyEvent.VK_BRACELEFT:
            return ("Left Brace");
        case KeyEvent.VK_BRACERIGHT:
            return ("Right Brace");
        case KeyEvent.VK_AT:
            return ("At");
        case KeyEvent.VK_COLON:
            return ("Colon");
        case KeyEvent.VK_CIRCUMFLEX:
            return ("Circumflex");
        case KeyEvent.VK_DOLLAR:
            return ("Dollar");
        case KeyEvent.VK_EURO_SIGN:
            return ("Euro");
        case KeyEvent.VK_EXCLAMATION_MARK:
            return ("Exclamation Mark");
        case KeyEvent.VK_INVERTED_EXCLAMATION_MARK:
            return ("Inverted Exclamation Mark");
        case KeyEvent.VK_LEFT_PARENTHESIS:
            return ("Left Parenthesis");
        case KeyEvent.VK_NUMBER_SIGN:
            return ("Number Sign");
        case KeyEvent.VK_MINUS:
            return ("Minus");
        case KeyEvent.VK_PLUS:
            return ("Plus");
        case KeyEvent.VK_RIGHT_PARENTHESIS:
            return ("Right Parenthesis");
        case KeyEvent.VK_UNDERSCORE:
            return ("Underscore");

        case KeyEvent.VK_FINAL:
            return ("Final");
        case KeyEvent.VK_CONVERT:
            return ("Convert");
        case KeyEvent.VK_NONCONVERT:
            return ("No Convert");
        case KeyEvent.VK_ACCEPT:
            return ("Accept");
        case KeyEvent.VK_MODECHANGE:
            return ("Mode Change");
        case KeyEvent.VK_KANA:
            return ("Kana");
        case KeyEvent.VK_KANJI:
            return ("Kanji");
        case KeyEvent.VK_ALPHANUMERIC:
            return ("Alphanumeric");
        case KeyEvent.VK_KATAKANA:
            return ("Katakana");
        case KeyEvent.VK_HIRAGANA:
            return ("Hiragana");
        case KeyEvent.VK_FULL_WIDTH:
            return ("Full-Width");
        case KeyEvent.VK_HALF_WIDTH:
            return ("Half-Width");
        case KeyEvent.VK_ROMAN_CHARACTERS:
            return ("Roman Characters");
        case KeyEvent.VK_ALL_CANDIDATES:
            return ("All Candidates");
        case KeyEvent.VK_PREVIOUS_CANDIDATE:
            return ("Previous Candidate");
        case KeyEvent.VK_CODE_INPUT:
            return ("Code Input");
        case KeyEvent.VK_JAPANESE_KATAKANA:
            return ("Japanese Katakana");
        case KeyEvent.VK_JAPANESE_HIRAGANA:
            return ("Japanese Hiragana");
        case KeyEvent.VK_JAPANESE_ROMAN:
            return ("Japanese Roman");
        case KeyEvent.VK_KANA_LOCK:
            return ("Kana Lock");
        case KeyEvent.VK_INPUT_METHOD_ON_OFF:
            return ("Input Method On/Off");

        case KeyEvent.VK_AGAIN:
            return ("Again");
        case KeyEvent.VK_UNDO:
            return ("Undo");
        case KeyEvent.VK_COPY:
            return ("Copy");
        case KeyEvent.VK_PASTE:
            return ("Paste");
        case KeyEvent.VK_CUT:
            return ("Cut");
        case KeyEvent.VK_FIND:
            return ("Find");
        case KeyEvent.VK_PROPS:
            return ("Props");
        case KeyEvent.VK_STOP:
            return ("Stop");
        }

        return "";
    }

    // Taken and modified from the KeyEvent Class
    public static String getKeyModifiersText(final int modifiers) {
        final StringBuilder buf = new StringBuilder();
        if ((modifiers & InputEvent.META_MASK) != 0) {

            buf.append("Command");
            buf.append("+");

        }
        if ((modifiers & InputEvent.CTRL_MASK) != 0) {

            buf.append("Ctrl");
            buf.append("+");

        }
        if ((modifiers & InputEvent.ALT_MASK) != 0) {

            buf.append("Alt");
            buf.append("+");

        }
        if ((modifiers & InputEvent.SHIFT_MASK) != 0) {

            buf.append("Shift");
            buf.append("+");

        }
        if ((modifiers & InputEvent.ALT_GRAPH_MASK) != 0) {
            buf.append("Alt Graph");
            buf.append("+");
        }
        if (buf.length() > 0 && buf.charAt(buf.length() - 1) == '+') {
            buf.setLength(buf.length() - 1);
        }
        return buf.toString();
    }

    public static String getShortcutString(final KeyStroke event, final boolean translated) {
        final int mod = event.getModifiers();
        final int code = event.getKeyCode();
        return getShortcutString(mod, code, translated);
    }

    public static String getShortcutString(final int mod, final int code, final boolean translated) {

        String msg1 = "";
        msg1 = translated ? KeyEvent.getKeyModifiersText(mod) : KeyUtils.getKeyModifiersText(mod);

        switch (code) {
        case KeyEvent.VK_SHIFT:
        case KeyEvent.VK_CONTROL:
        case KeyEvent.VK_ALT:
        case KeyEvent.VK_ALT_GRAPH:

            break;

        default:
            if (msg1.length() > 0) {
                msg1 = msg1 + "+" + (translated ? KeyEvent.getKeyText(code) : KeyUtils.getKeyText(code));
            } else {
                msg1 = (translated ? KeyEvent.getKeyText(code) : KeyUtils.getKeyText(code));
            }

        }
        if (CrossSystem.isMac()) {
            if (translated) {
                msg1 = msg1.replace(MAC_TRANSLATED_REPLACE_CTRL, MAC_CTRL);
                msg1 = msg1.replace(MAC_TRANSLATED_REPLACE_META, MAC_META);
                msg1 = msg1.replace(MAC_TRANSLATED_REPLACE_ALT, MAC_ALT);
                msg1 = msg1.replace(MAC_TRANSLATED_REPLACE_SHIFT, MAC_SHIFT);
            } else {
                msg1 = msg1.replace(MAC_REPLACE_CTRL, MAC_CTRL);
                msg1 = msg1.replace(MAC_REPLACE_META, MAC_META);
                msg1 = msg1.replace(MAC_REPLACE_ALT, MAC_ALT);
                msg1 = msg1.replace(MAC_REPLACE_SHIFT, MAC_SHIFT);
            }
        }

        return msg1;
    }

}
