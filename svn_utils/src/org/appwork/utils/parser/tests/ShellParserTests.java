package org.appwork.utils.parser.tests;

import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.List;

import org.appwork.testframework.AWTest;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.parser.ShellParser;
import org.appwork.utils.parser.ShellParser.ShellParserHint;

public class ShellParserTests extends AWTest {
    public static void main(final String[] args) throws NoSuchMethodException, SecurityException, ClassNotFoundException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        System.out.println(Arrays.asList(args));
        run();
    }

    @Override
    public void runTest() throws Exception {
        // --- Windows CMD (explicit style) ---
        {
            // single quote is literal
            List<String> r = ShellParser.splitCommandString("abc'foo'def", ShellParserHint.STYLE_WINDOWS_CMDEXE);
            assertEqualsDeep(r, Arrays.asList("abc'foo'def"));
        }
        {
            // double quotes group
            List<String> r = ShellParser.splitCommandString("cmd \"arg with space\" plain", ShellParserHint.STYLE_WINDOWS_CMDEXE);
            assertEqualsDeep(r, Arrays.asList("cmd", "arg with space", "plain"));
        }
        {
            // caret-escaped space
            List<String> r = ShellParser.splitCommandString("foo^ bar baz", ShellParserHint.STYLE_WINDOWS_CMDEXE);
            assertEqualsDeep(r, Arrays.asList("foo", "bar", "baz"));
        }
        {
            // caret-escaped quote
            List<String> r = ShellParser.splitCommandString("abc^\"def", ShellParserHint.STYLE_WINDOWS_CMDEXE);
            assertEqualsDeep(r, Arrays.asList("abc\"def"));
        }
        {
            // multiple spaces/tabs split
            List<String> r = ShellParser.splitCommandString("a   b\tc", ShellParserHint.STYLE_WINDOWS_CMDEXE);
            assertEqualsDeep(r, Arrays.asList("a", "b", "c"));
        }
        {
            // single quotes are literal, trailing backslash literal too
            List<String> result = ShellParser.splitCommandString("'foo\\", ShellParserHint.STYLE_WINDOWS_CMDEXE);
            assertEqualsDeep(result, Arrays.asList("'foo\\"));
        }
        {
            // typical WMIC/cmd style: apostrophe is part of token
            List<String> result = ShellParser.splitCommandString("bla.exe -test abc'def", ShellParserHint.STYLE_WINDOWS_CMDEXE);
            assertEqualsDeep(result, Arrays.asList("bla.exe", "-test", "abc'def"));
        }

        // --- Windows PowerShell ---
        {
            // single quotes group literal content
            List<String> r = ShellParser.splitCommandString("'foo bar' baz", ShellParserHint.STYLE_WINDOWS_POWERSHELL);
            assertEqualsDeep(r, Arrays.asList("foo bar", "baz"));
        }
        {
            // backtick-escaped space
            List<String> r = ShellParser.splitCommandString("foo` bar baz", ShellParserHint.STYLE_WINDOWS_POWERSHELL);
            assertEqualsDeep(r, Arrays.asList("foo bar", "baz"));
        }
        {
            // both inner quotes escaped via backtick -> both kept
            List<String> r = ShellParser.splitCommandString("\"a`\"b`\" c\"", ShellParserHint.STYLE_WINDOWS_POWERSHELL);
            assertEqualsDeep(r, Arrays.asList("a\"b\" c"));
        }
        {
            // concatenation across single-quoted segment
            List<String> r = ShellParser.splitCommandString("abc'foo'def", ShellParserHint.STYLE_WINDOWS_POWERSHELL);
            assertEqualsDeep(r, Arrays.asList("abcfoodef"));
        }

        // --- Unix (POSIX-like) ---
        {
            // quoted segments concatenate literally
            List<String> r = ShellParser.splitCommandString("abc'foo'def", ShellParserHint.STYLE_UNIX);
            assertEqualsDeep(r, Arrays.asList("abcfoodef"));
        }
        {
            // classic POSIX trick to include a literal apostrophe
            List<String> r = ShellParser.splitCommandString("bla.exe -test 'abc'\\''def'", ShellParserHint.STYLE_UNIX);
            assertEqualsDeep(r, Arrays.asList("bla.exe", "-test", "abc'def"));
        }
        {
            // double-quoted group
            List<String> r = ShellParser.splitCommandString("\"foo bar\" baz", ShellParserHint.STYLE_UNIX);
            assertEqualsDeep(r, Arrays.asList("foo bar", "baz"));
        }
        {
            // escaped whitespace with backslash
            List<String> r = ShellParser.splitCommandString("foo\\ bar baz", ShellParserHint.STYLE_UNIX);
            assertEqualsDeep(r, Arrays.asList("foo bar", "baz"));
        }
        {
            // escaped quotes remain literal (\" -> ")
            List<String> r = ShellParser.splitCommandString("abc\\\"foo\\\"def", ShellParserHint.STYLE_UNIX);
            assertEqualsDeep(r, Arrays.asList("abc\"foo\"def"));
        }
        // ===== CMD.EXE (explicit style) =====
        {
            // simple split
            List<String> r = ShellParser.splitCommandString("a b c", ShellParserHint.STYLE_WINDOWS_CMDEXE);
            assertEqualsDeep(r, Arrays.asList("a", "b", "c"));
        }
        {
            // multiple spaces and tabs
            List<String> r = ShellParser.splitCommandString("a   b\tc", ShellParserHint.STYLE_WINDOWS_CMDEXE);
            assertEqualsDeep(r, Arrays.asList("a", "b", "c"));
        }
        {
            // single quote is literal
            List<String> r = ShellParser.splitCommandString("abc'foo'def", ShellParserHint.STYLE_WINDOWS_CMDEXE);
            assertEqualsDeep(r, Arrays.asList("abc'foo'def"));
        }
        {
            // double quotes group
            List<String> r = ShellParser.splitCommandString("cmd \"arg with space\" plain", ShellParserHint.STYLE_WINDOWS_CMDEXE);
            assertEqualsDeep(r, Arrays.asList("cmd", "arg with space", "plain"));
        }
        {
            // concat around double quotes
            List<String> r = ShellParser.splitCommandString("abc\"foo\"def", ShellParserHint.STYLE_WINDOWS_CMDEXE);
            assertEqualsDeep(r, Arrays.asList("abcfoodef"));
        }
        {
            // caret before space DOES NOT join in cmd -> split into separate args
            List<String> r = ShellParser.splitCommandString("foo^ bar baz", ShellParserHint.STYLE_WINDOWS_CMDEXE);
            assertEqualsDeep(r, Arrays.asList("foo", "bar", "baz"));
        }
        {
            // caret-escaped quote -> literal quote, not a group opener
            List<String> r = ShellParser.splitCommandString("abc^\"def", ShellParserHint.STYLE_WINDOWS_CMDEXE);
            assertEqualsDeep(r, Arrays.asList("abc\"def"));
        }
        {
            // dangling caret at end -> kept as literal
            List<String> r = ShellParser.splitCommandString("foo^", ShellParserHint.STYLE_WINDOWS_CMDEXE);
            assertEqualsDeep(r, Arrays.asList("foo^"));
        }
        {
            // unmatched double quote -> take rest as part of same token
            List<String> r = ShellParser.splitCommandString("abc\"def", ShellParserHint.STYLE_WINDOWS_CMDEXE);
            assertEqualsDeep(r, Arrays.asList("abcdef"));
        }
        {
            // leading/trailing whitespace gets trimmed per token
            List<String> r = ShellParser.splitCommandString("  a   \"b c\"   d  ", ShellParserHint.STYLE_WINDOWS_CMDEXE);
            assertEqualsDeep(r, Arrays.asList("a", "b c", "d"));
        }
        {
            // literal apostrophe inside password-like token
            List<String> r = ShellParser.splitCommandString("-DPASSWORD*=!n{-noworriesthisisnotreal-*%ny6'8Y+/: -DSERVER*=x", ShellParserHint.STYLE_WINDOWS_CMDEXE);
            assertEqualsDeep(r, Arrays.asList("-DPASSWORD*=!n{-noworriesthisisnotreal-*%ny6'8Y+/:", "-DSERVER*=x"));
        }
        {
            // single quotes literal + trailing backslash literal (cmd style)
            List<String> r = ShellParser.splitCommandString("'foo\\", ShellParserHint.STYLE_WINDOWS_CMDEXE);
            assertEqualsDeep(r, Arrays.asList("'foo\\"));
        }

        // ===== POWERSHELL =====
        {
            // single-quoted literal content
            List<String> r = ShellParser.splitCommandString("'foo bar' baz", ShellParserHint.STYLE_WINDOWS_POWERSHELL);
            assertEqualsDeep(r, Arrays.asList("foo bar", "baz"));
        }
        {
            // concatenate around single quotes
            List<String> r = ShellParser.splitCommandString("abc'foo'def", ShellParserHint.STYLE_WINDOWS_POWERSHELL);
            assertEqualsDeep(r, Arrays.asList("abcfoodef"));
        }
        {
            // backtick-escaped space
            List<String> r = ShellParser.splitCommandString("foo` bar baz", ShellParserHint.STYLE_WINDOWS_POWERSHELL);
            assertEqualsDeep(r, Arrays.asList("foo bar", "baz"));
        }
        {
            // double quotes with backtick-escaped inner quotes (both kept)
            List<String> r = ShellParser.splitCommandString("\"a`\"b`\" c\"", ShellParserHint.STYLE_WINDOWS_POWERSHELL);
            assertEqualsDeep(r, Arrays.asList("a\"b\" c"));
        }
        {
            // backtick escapes any next char
            List<String> r = ShellParser.splitCommandString("x`&y", ShellParserHint.STYLE_WINDOWS_POWERSHELL);
            assertEqualsDeep(r, Arrays.asList("x&y"));
        }
        {
            // unmatched single quote -> take rest (tolerant)
            List<String> r = ShellParser.splitCommandString("'foo", ShellParserHint.STYLE_WINDOWS_POWERSHELL);
            assertEqualsDeep(r, Arrays.asList("foo"));
        }
        {
            // trailing backtick -> kept literal
            List<String> r = ShellParser.splitCommandString("foo`", ShellParserHint.STYLE_WINDOWS_POWERSHELL);
            assertEqualsDeep(r, Arrays.asList("foo`"));
        }

        // ===== UNIX (POSIX-like) =====
        {
            // quoted segments concatenate literally
            List<String> r = ShellParser.splitCommandString("abc'foo'def", ShellParserHint.STYLE_UNIX);
            assertEqualsDeep(r, Arrays.asList("abcfoodef"));
        }
        {
            // classic POSIX trick to include a literal apostrophe
            List<String> r = ShellParser.splitCommandString("bla.exe -test 'abc'\\''def'", ShellParserHint.STYLE_UNIX);
            assertEqualsDeep(r, Arrays.asList("bla.exe", "-test", "abc'def"));
        }
        {
            // double-quoted group
            List<String> r = ShellParser.splitCommandString("\"foo bar\" baz", ShellParserHint.STYLE_UNIX);
            assertEqualsDeep(r, Arrays.asList("foo bar", "baz"));
        }
        {
            // backslash-escaped whitespace stays in same token
            List<String> r = ShellParser.splitCommandString("foo\\ bar baz", ShellParserHint.STYLE_UNIX);
            assertEqualsDeep(r, Arrays.asList("foo bar", "baz"));
        }
        {
            // escaped quotes become literal quotes
            List<String> r = ShellParser.splitCommandString("abc\\\"foo\\\"def", ShellParserHint.STYLE_UNIX);
            assertEqualsDeep(r, Arrays.asList("abc\"foo\"def"));
        }
        {
            // unmatched double quote -> take rest (tolerant)
            List<String> r = ShellParser.splitCommandString("\"foo", ShellParserHint.STYLE_UNIX);
            assertEqualsDeep(r, Arrays.asList("foo"));
        }
        {
            // trailing backslash not escaping a special -> kept
            List<String> r = ShellParser.splitCommandString("foo\\", ShellParserHint.STYLE_UNIX);
            assertEqualsDeep(r, Arrays.asList("foo\\"));
        }

        // ===== DEFAULT STYLE PER OS FAMILY =====
        {
            // default parser style changes with OS family:
            // - Windows: CMD style (single quote is literal)
            // - non-Windows: UNIX style (single quotes group / concatenate)
            List<String> r = ShellParser.splitCommandString("abc'foo'def");
            if (CrossSystem.isWindows()) {
                assertEqualsDeep(r, Arrays.asList("abc'foo'def"));
            } else {
                assertEqualsDeep(r, Arrays.asList("abcfoodef"));
            }
        }

        // ===== GENERIC EDGE CASES =====
        {
            // empty input -> empty list
            List<String> r = ShellParser.splitCommandString("");
            assertEqualsDeep(r, Arrays.asList());
        }
        {
            // only whitespace -> empty list
            List<String> r = ShellParser.splitCommandString("   \t  ");
            assertEqualsDeep(r, Arrays.asList());
        }
        {
            // single token with spaces around
            List<String> r = ShellParser.splitCommandString("   lone   ");
            assertEqualsDeep(r, Arrays.asList("lone"));
        }

        // ===== createCommandLine UNIX style =====
        {
            // simple case without special characters
            String cmd = ShellParser.createCommandLine(ShellParser.Style.UNIX, "echo", "hello");
            assertEquals("echo hello", cmd);
        }
        {
            // arguments with spaces are single-quoted
            String cmd = ShellParser.createCommandLine(ShellParser.Style.UNIX, "echo", "hello world", "foo");
            assertEquals("echo 'hello world' foo", cmd);
        }
        {
            // executable path with spaces is single-quoted as well
            String cmd = ShellParser.createCommandLine(ShellParser.Style.UNIX, "/usr/bin/my tool", "arg");
            assertEquals("'/usr/bin/my tool' arg", cmd);
        }

    }

}
