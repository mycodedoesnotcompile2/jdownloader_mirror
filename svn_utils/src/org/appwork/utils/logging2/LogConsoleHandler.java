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
package org.appwork.utils.logging2;

import java.util.HashSet;
import java.util.Locale;
import java.util.logging.ConsoleHandler;
import java.util.logging.Filter;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import org.appwork.utils.logging.LogFormatter;

/**
 * @author daniel
 *
 */
public class LogConsoleHandler extends ConsoleHandler {
    private HashSet<String> allowedLoggerNames = new HashSet<String>();

    public LogConsoleHandler() {
        setLevel(Level.ALL);
        setFormatter(new LogFormatter() {
            private final int lineLength = 400;

            @Override
            protected CharSequence getMessage(LogRecord record) {
                final CharSequence ret = super.getMessage(record);
                if (ret != null && ret.length() > lineLength) {
                    // eclipse issue with very long lines, causing console to freeze -> stalling application as writing to console blocks
                    final int length = ret.length();
                    int position = 0;
                    outerLoop: while (position < length) {
                        final int max = Math.min(length, position + lineLength);
                        for (int index = position; index < max; index++) {
                            final char ch = ret.charAt(index);
                            if (ch == '\r' || ch == '\n' || index == length - 1) {
                                if (index + 1 - position > lineLength) {
                                    position = 0;
                                    break outerLoop;
                                } else {
                                    position = index + 1;
                                    continue outerLoop;
                                }
                            }
                        }
                        position = 0;
                        break;
                    }
                    if (position >= length) {
                        // line check okay, we can use the original string
                        return ret;
                    }
                    // line check failed, we have to auto wrap long lines
                    StringBuilder sb = new StringBuilder(length + ((length / lineLength) * 12));
                    position = 0;
                    boolean wrapText = false;
                    boolean isWrapped = false;
                    boolean wrappedSb = false;
                    outerLoop: while (position < length) {
                        if (sb.length() > 0 && position > 0) {
                            final char lastChar = sb.charAt(sb.length() - 1);
                            if (lastChar != '\r' && lastChar != '\n') {
                                sb.append("\r\n");
                            }
                        }
                        final int max = Math.min(length, position + lineLength);
                        for (int index = position; index < max; index++) {
                            final char ch = ret.charAt(index);
                            if (ch == '\r' || ch == '\n' || index == length - 1) {
                                if (position != index) {
                                    if (wrapText) {
                                        isWrapped = true;
                                        if (wrappedSb == false) {
                                            wrappedSb = true;
                                            if (sb.length() > 0) {
                                                final StringBuilder newSb = new StringBuilder(length + ((length / lineLength) * 12));
                                                newSb.append("AUTO_WRAPPED>\r\n");
                                                newSb.append(sb);
                                                sb = newSb;
                                            } else {
                                                sb.append("AUTO_WRAPPED>\r\n");
                                            }
                                        }
                                        wrapText = false;
                                    }
                                    sb.append(ret.subSequence(position, index));
                                }
                                position = index + 1;
                                continue outerLoop;
                            }
                        }
                        wrapText = true;
                        isWrapped = true;
                        if (wrappedSb == false) {
                            wrappedSb = true;
                            if (sb.length() > 0) {
                                final StringBuilder newSb = new StringBuilder(length + ((length / lineLength) * 12));
                                newSb.append("AUTO_WRAPPED>\r\n");
                                newSb.append(sb);
                                sb = newSb;
                            } else {
                                sb.append("AUTO_WRAPPED>\r\n");
                            }
                        }
                        sb.append(ret.subSequence(position, max));
                        position = position + lineLength;
                    }
                    return sb;
                } else {
                    return ret;
                }
            }
        });
    }

    public HashSet<String> getAllowedLoggerNames() {
        return allowedLoggerNames;
    }

    @Override
    public boolean isLoggable(final LogRecord record) {
        final HashSet<String> lallowedLoggerNames = allowedLoggerNames;
        if (lallowedLoggerNames == null) {
            return false;
        } else if (lallowedLoggerNames.size() == 0) {
            return true;
        } else if (allowedLoggerNames.contains(record.getLoggerName().toLowerCase(Locale.ENGLISH))) {
            return true;
        } else {
            return false;
        }
    }

    public void setAllowedLoggerNames(final String... strings) {
        if (strings == null) {
            allowedLoggerNames = null;
            return;
        } else {
            final HashSet<String> tmp = new HashSet<String>();
            for (final String s : strings) {
                tmp.add(s.toLowerCase(Locale.ENGLISH));
            }
            allowedLoggerNames = tmp;
        }
    }

    @Override
    public void setFilter(final Filter newFilter) throws SecurityException {
    }
}
