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
    public static java.util.List<String> splitCommandString(String command) {
        final java.util.List<String> ret = new ArrayList<String>();
        while (true) {
            final int space = command.indexOf(" ");
            int q = command.indexOf("'");
            while (true) {
                if (q == -1) {
                    break;
                }
                int escapes = 0;
                int ec = 1;
                while (q - ec >= 0 && command.charAt(q - ec++) == '\\') {
                    escapes++;
                }
                if (escapes % 2 == 0) {
                    break;
                }
                q = command.indexOf("'", q + 1);
            }
            int dq = command.indexOf("\"");
            while (true) {
                if (dq == -1) {
                    break;
                }
                int escapes = 0;
                int ec = 1;
                while (dq - ec >= 0 && command.charAt(dq - ec++) == '\\') {
                    escapes++;
                }
                if (escapes % 2 == 0) {
                    break;
                }
                dq = command.indexOf("\"", dq + 1);
            }
            final int min = ShellParser.min(space, q, dq);
            if (min == Integer.MAX_VALUE) {
                if (command.trim().length() > 0) {
                    ret.add(command);
                }
                return ret;
            } else {
                if (min == space) {
                    final String p = command.substring(0, min).trim();
                    if (p.length() > 0) {
                        ret.add(p);
                    }
                    command = command.substring(min + 1);
                } else if (min == q) {
                    int nq = command.indexOf("'", min + 1);
                    while (true) {
                        if (nq == -1) {
                            nq = command.length() - 1;
                            org.appwork.loggingv3.LogV3.warning("Malformed commandstring");
                            break;
                        }
                        int escapes = 0;
                        int ec = 1;
                        while (command.charAt(nq - ec++) == '\\') {
                            escapes++;
                        }
                        if (escapes % 2 == 0) {
                            break;
                        }
                        nq = command.indexOf("'", nq + 1);
                    }
                    ret.add(command.substring(min + 1, nq));
                    command = command.substring(Math.min(nq + 2, command.length()));
                } else if (min == dq) {
                    int nq = command.indexOf("\"", min + 1);
                    while (true) {
                        if (nq == -1) {
                            nq = command.length() - 1;
                            org.appwork.loggingv3.LogV3.warning("Malformed commandstring");
                            break;
                        }
                        int escapes = 0;
                        int ec = 1;
                        while (command.charAt(nq - ec++) == '\\') {
                            escapes++;
                        }
                        if (escapes % 2 == 0) {
                            break;
                        }
                        nq = command.indexOf("\"", nq + 1);
                    }
                    ret.add(command.substring(min + 1, nq));
                    command = command.substring(Math.min(nq + 2, command.length()));
                }
            }
        }
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
