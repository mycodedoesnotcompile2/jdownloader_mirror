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
package org.appwork.utils.parser;

import java.io.File;
import java.util.ArrayList;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.appwork.exceptions.WTFException;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.os.CrossSystem;

/**
 * @author thomas
 *
 */
public class ShellParser {
    public static enum Style {
        WINDOWS
    }

    private static int min(int space, int q, int dq) {
        if (space == -1) {
            space = Integer.MAX_VALUE;
        }
        if (q == -1) {
            q = Integer.MAX_VALUE;
        }
        if (dq == -1) {
            dq = Integer.MAX_VALUE;
        }
        return Math.min(Math.min(space, q), dq);
    }

    public static enum ShellParserHint {

        STYLE_UNIX,
        STYLE_WINDOWS_CMDEXE,
        STYLE_WINDOWS_POWERSHELL
    }

    /**
     * Splits a Commandstring it its single commands <br>
     * <code>java -jar
     * ghd\"dfs "bjhn\"bdsa hgf" 'bn\"la' "" ' \\' 'bla'<br>
     * "java"<br>
     * "-jar"<br>
     * "ghd\"dfs"<br>
     * "bjhn\"bdsa hgf"<br>
     * "bn\"la"<br>
     * ""<br>
     * "  \\"<br>
     * "bla"<br>
     * </code>
     *
     * @param command
     * @return
     */

    private static String unescapeSpecials(String s, char escChar, boolean allowEscapedWhitespace) {
        if (s.indexOf(escChar) < 0) {
            return s;
        }
        StringBuilder sb = new StringBuilder(s.length());
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c == escChar && i + 1 < s.length()) {
                char next = s.charAt(i + 1);
                if (allowEscapedWhitespace && Character.isWhitespace(next)) {
                    sb.append(next);
                    i++;
                    continue;
                }
                if (next == '\'' || next == '"' || next == escChar) {
                    sb.append(next);
                    i++;
                    continue;
                }
            }
            sb.append(c);
        }
        return sb.toString();
    }

    public static java.util.List<String> splitCommandString(String command, ShellParserHint... hints) {
        // Style selection; default = Windows CMD
        boolean styleUnix = false;
        boolean styleCmd = true;
        boolean stylePs = false;

        if (hints != null) {
            for (ShellParserHint h : hints) {
                if (h == ShellParserHint.STYLE_UNIX) {
                    styleUnix = true;
                    styleCmd = false;
                    stylePs = false;
                } else if (h == ShellParserHint.STYLE_WINDOWS_CMDEXE) {
                    styleUnix = false;
                    styleCmd = true;
                    stylePs = false;
                } else if (h == ShellParserHint.STYLE_WINDOWS_POWERSHELL) {
                    styleUnix = false;
                    styleCmd = false;
                    stylePs = true;
                }
            }
        }

        // Behavior per style
        final boolean singleQuoteActive = !styleCmd; // in CMD, single quotes are literal
        final boolean doubleQuoteActive = true; // all styles
        final char escForQuote = styleUnix ? '\\' : (stylePs ? '`' : '^'); // CMD: ^ only escapes quotes
        final char escGeneral = styleUnix ? '\\' : (stylePs ? '`' : '^');

        final java.util.List<String> ret = new ArrayList<String>();
        final StringBuilder acc = new StringBuilder(); // current token (concatenates across quote segments)

        while (true) {
            // 1) find next splitting whitespace (style-aware)
            int whitespace = -1;
            for (int i = 0; i < command.length(); i++) {
                char ch = command.charAt(i);
                if (!Character.isWhitespace(ch)) {
                    continue;
                }

                if (styleUnix || stylePs) {
                    // In UNIX/PS, escGeneral can escape whitespace (e.g., "\ " or "` ")
                    int escapes = 0, ec = 1;
                    while (i - ec >= 0 && command.charAt(i - ec) == escGeneral) {
                        escapes++;
                        ec++;
                    }
                    if ((escapes & 1) == 1) {
                        continue; // odd => escaped whitespace -> do not split here
                    }
                }
                // In CMD caret does not escape whitespace -> always split
                whitespace = i;
                break;
            }

            // 2) locate next unescaped single quote
            int q = singleQuoteActive ? command.indexOf('\'') : -1;
            if (singleQuoteActive) {
                while (q != -1) {
                    if (escForQuote == 0) {
                        break;
                    }
                    int escapes = 0, ec = 1;
                    while (q - ec >= 0 && command.charAt(q - ec) == escForQuote) {
                        escapes++;
                        ec++;
                    }
                    if ((escapes & 1) == 0) {
                        break; // found unescaped '
                    }
                    q = command.indexOf('\'', q + 1);
                }
            }

            // 3) locate next unescaped double quote
            int dq = doubleQuoteActive ? command.indexOf('"') : -1;
            if (doubleQuoteActive) {
                while (dq != -1) {
                    if (escForQuote == 0) {
                        break; // CMD: first " counts (no quote-escaper for detection)
                    }
                    int escapes = 0, ec = 1;
                    while (dq - ec >= 0 && command.charAt(dq - ec) == escForQuote) {
                        escapes++;
                        ec++;
                    }
                    if ((escapes & 1) == 0) {
                        break; // found unescaped "
                    }
                    dq = command.indexOf('"', dq + 1);
                }
            }

            final int min = ShellParser.min(whitespace, q, dq);

            // 4) nothing left -> flush & return
            if (min == Integer.MAX_VALUE) {
                if (command.length() > 0) {
                    acc.append(command);
                }
                if (acc.length() > 0) {
                    final String tok = unescapeSpecials(acc.toString(), styleUnix, styleCmd, stylePs, escGeneral).trim();
                    if (!tok.isEmpty()) {
                        ret.add(tok);
                    }
                }
                return ret;
            }

            // 5) consume next element
            if (min == whitespace) {
                String p = command.substring(0, min);
                // CMD: caret right before the splitting space is eaten by cmd.exe
                if (styleCmd && p.endsWith("^")) {
                    p = p.substring(0, p.length() - 1);
                }
                acc.append(p);

                String tok = unescapeSpecials(acc.toString(), styleUnix, styleCmd, stylePs, escGeneral).trim();
                if (!tok.isEmpty()) {
                    ret.add(tok);
                }
                acc.setLength(0);

                command = command.substring(min + 1);
            } else if (min == q) {
                // single-quoted block (UNIX/PS only)
                if (min > 0) {
                    acc.append(command.substring(0, min));
                }

                int nq = command.indexOf('\'', min + 1);
                if (escForQuote != 0) {
                    while (nq != -1) {
                        int escapes = 0, ec = 1;
                        while (nq - ec >= 0 && command.charAt(nq - ec) == escForQuote) {
                            escapes++;
                            ec++;
                        }
                        if ((escapes & 1) == 0) {
                            break;
                        }
                        nq = command.indexOf('\'', nq + 1);
                    }
                }
                if (nq == -1) {
                    org.appwork.loggingv3.LogV3.warning("Malformed commandstring");
                    nq = command.length();
                }
                if (nq > min + 1) {
                    acc.append(command.substring(min + 1, nq));
                }
                command = command.substring(Math.min(nq + 1, command.length()));
            } else if (min == dq) {
                // double-quoted block
                if (min > 0) {
                    String prefix = command.substring(0, min);
                    // CMD: caret directly before " is eaten by cmd.exe
                    if (styleCmd && prefix.endsWith("^")) {
                        prefix = prefix.substring(0, prefix.length() - 1);
                    }
                    acc.append(prefix);
                }

                int nq = command.indexOf('"', min + 1);
                if (escForQuote != 0) {
                    while (nq != -1) {
                        int escapes = 0, ec = 1;
                        while (nq - ec >= 0 && command.charAt(nq - ec) == escForQuote) {
                            escapes++;
                            ec++;
                        }
                        if ((escapes & 1) == 0) {
                            break;
                        }
                        nq = command.indexOf('"', nq + 1);
                    }
                }
                if (nq == -1) {
                    org.appwork.loggingv3.LogV3.warning("Malformed commandstring");
                    nq = command.length();
                }
                if (nq > min + 1) {
                    acc.append(command.substring(min + 1, nq));
                }
                command = command.substring(Math.min(nq + 1, command.length()));
            }
        }
    }

    /**
     * Style-aware unescape inside a finished token. - UNIX: \<ws>, \", \', \\ => literal char - CMD: ^" => " (caret before space is handled
     * at split time) - PS: `x => x (backtick escapes next char)
     */
    private static String unescapeSpecials(String s, boolean styleUnix, boolean styleCmd, boolean stylePs, char escGeneral) {
        if (s.indexOf(escGeneral) < 0) {
            return s;
        }
        StringBuilder sb = new StringBuilder(s.length());
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (styleUnix && c == '\\' && i + 1 < s.length()) {
                char n = s.charAt(i + 1);
                if (Character.isWhitespace(n) || n == '"' || n == '\'' || n == '\\') {
                    sb.append(n);
                    i++;
                    continue;
                }
            } else if (stylePs && c == '`' && i + 1 < s.length()) {
                sb.append(s.charAt(++i)); // backtick escapes next char
                continue;
            } else if (styleCmd && c == '^' && i + 1 < s.length()) {
                char n = s.charAt(i + 1);
                if (n == '"') {
                    sb.append('"');
                    i++;
                    continue;
                } // ^" -> "
                // caret before space is already removed when splitting;
                // any other caret remains as literal.
            }
            sb.append(c);
        }
        return sb.toString();
    }

    public static String createCommandLine(Style style, String... commandline) {
        if (style == null) {
            switch (CrossSystem.getOSFamily()) {
            case WINDOWS:
                style = Style.WINDOWS;
                break;
            default:
                DebugMode.debugger("OS not supported!");
            }
        }
        String[] cmd = commandline;
        String exe = cmd[0];
        exe = removeQuotes(exe);
        if (exe.contains("\"")) {
            // "\"C:\\Program Files\\application path with \" quotes"
            final String joined = StringUtils.join(cmd, " ");
            final ArrayList<String> matchList = new ArrayList<String>();
            // WRANING: ShellParser does not always work here
            // cmd = ShellParser.splitCommandString(joined).toArray(new String[] {});
            final Matcher matcher = Pattern.compile("[^\\s\"]+|\"[^\"]*\"").matcher(joined);
            while (matcher.find()) {
                matchList.add(matcher.group());
            }
            cmd = matchList.toArray(new String[matchList.size()]);
            exe = cmd[0];
            if (exe.contains("\"")) {
                throw new WTFException("Exe path contains double quotes");
            }
        }
        final boolean isExe = exe.toUpperCase(Locale.ROOT).endsWith(".EXE") || !new File(exe).getName().contains(".");
        final StringBuilder commandLine = new StringBuilder(80);
        commandLine.append("\"" + exe + "\"");
        for (int i = 1; i < cmd.length; ++i) {
            commandLine.append(' ');
            boolean escapingRequired = StringUtils.isEmpty(cmd[i]);
            final String ensureNoQuotes = removeQuotes(cmd[i]);
            final boolean parameterIsAlreadyQuoted = !cmd[i].equals(ensureNoQuotes);
            final boolean parameterContainsInternalQuotes = ensureNoQuotes.contains("\"");
            // handle forbidden quoptes constellations
            if (isExe && parameterIsAlreadyQuoted && parameterContainsInternalQuotes) {
                throw new IllegalArgumentException("Parameter " + i + " for *.exe call is quoted and contains internal quotes");
            } else if (!isExe && parameterContainsInternalQuotes) {
                throw new IllegalArgumentException("Parameter " + i + " for *.bat/cmd call contains internal quotes");
            }
            if (!escapingRequired && !parameterIsAlreadyQuoted) {
                for (final char c : isExe ? new char[] { ' ', '\t', '\"', '<', '>' } : new char[] { ' ', '\t', '\"', '<', '>', '&', '|', '^' }) {
                    if (cmd[i].indexOf(c) >= 0) {
                        escapingRequired = true;
                        break;
                    }
                }
            }
            if (escapingRequired) {
                commandLine.append('"');
                if (isExe) {
                    int backslashCount = 0;
                    for (int j = 0; j < cmd[i].length(); j++) {
                        final char c = cmd[i].charAt(j);
                        if (c == '"') {
                            while (backslashCount-- > 0) {
                                commandLine.append('\\');
                            }
                            backslashCount = 0;
                            commandLine.append("\\\"");
                        } else {
                            if (c == '\\') {
                                backslashCount++;
                            } else {
                                backslashCount = 0;
                            }
                            commandLine.append(c);
                        }
                    }
                    // trailing \\
                    while (backslashCount-- > 0) {
                        commandLine.append('\\');
                    }
                } else {
                    commandLine.append(cmd[i]);
                }
                commandLine.append('"');
            } else {
                commandLine.append(cmd[i]);
            }
        }
        return commandLine.toString();
    }

    protected static String removeQuotes(String exe) {
        if (exe.startsWith("\"") && exe.endsWith("\"") && exe.length() >= 2) {
            // escaped end double quote - consider as unquoted
            if (!exe.endsWith("\\\"")) {
                exe = exe.substring(1, exe.length() - 1);
            }
        }
        return exe;
    }
}
