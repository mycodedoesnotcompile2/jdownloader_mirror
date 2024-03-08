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
package org.appwork.storage.flexijson.tests;

import static org.appwork.storage.flexijson.ParsingError.ERROR_INLINE_COMMENT;
import static org.appwork.storage.flexijson.ParsingError.ERROR_KEY_IS_BOOLEAN;
import static org.appwork.storage.flexijson.ParsingError.ERROR_KEY_IS_NUMBER;
import static org.appwork.storage.flexijson.ParsingError.ERROR_KEY_WITHOUT_QUOTES;
import static org.appwork.storage.flexijson.ParsingError.ERROR_KEY_WITH_SINGLE_QUOTES;
import static org.appwork.storage.flexijson.ParsingError.ERROR_LEADING_COMMA_IN_ARRAY;
import static org.appwork.storage.flexijson.ParsingError.ERROR_LEADING_COMMA_IN_OBJECT;
import static org.appwork.storage.flexijson.ParsingError.ERROR_LEADING_DECIMAL_POINT;
import static org.appwork.storage.flexijson.ParsingError.ERROR_LINE_COMMENT;
import static org.appwork.storage.flexijson.ParsingError.ERROR_MULTIPLE_ELEMENTS_IN_JSON_STRING;
import static org.appwork.storage.flexijson.ParsingError.ERROR_MULTIPLE_LEADING_COMMA_IN_ARRAY;
import static org.appwork.storage.flexijson.ParsingError.ERROR_MULTIPLE_LEADING_COMMA_IN_OBJECT;
import static org.appwork.storage.flexijson.ParsingError.ERROR_MULTIPLE_SUBSEQUENT_COMMAS_IN_ARRAY;
import static org.appwork.storage.flexijson.ParsingError.ERROR_MULTIPLE_SUBSEQUENT_COMMAS_IN_OBJECT;
import static org.appwork.storage.flexijson.ParsingError.ERROR_MULTIPLE_TRAILING_COMMA_IN_ARRAY;
import static org.appwork.storage.flexijson.ParsingError.ERROR_MULTIPLE_TRAILING_COMMA_IN_OBJECT;
import static org.appwork.storage.flexijson.ParsingError.ERROR_NEWLINE_IN_TOKEN;
import static org.appwork.storage.flexijson.ParsingError.ERROR_NO_CONTENT;
import static org.appwork.storage.flexijson.ParsingError.ERROR_NUMBERFORMAT_BINARY;
import static org.appwork.storage.flexijson.ParsingError.ERROR_NUMBERFORMAT_HEXADECIMAL;
import static org.appwork.storage.flexijson.ParsingError.ERROR_NUMBERFORMAT_LEADING_PLUS;
import static org.appwork.storage.flexijson.ParsingError.ERROR_NUMBERFORMAT_OCTAL;
import static org.appwork.storage.flexijson.ParsingError.ERROR_NUMBER_INFINITY;
import static org.appwork.storage.flexijson.ParsingError.ERROR_NUMBER_NAN;
import static org.appwork.storage.flexijson.ParsingError.ERROR_STRING_TOKEN_WITHOUT_QUOTES;
import static org.appwork.storage.flexijson.ParsingError.ERROR_STRING_TOKEN_WITH_SINGLE_QUOTES;
import static org.appwork.storage.flexijson.ParsingError.ERROR_STRING_VALUE_WITHOUT_QUOTES;
import static org.appwork.storage.flexijson.ParsingError.ERROR_STRING_VALUE_WITH_SINGLE_QUOTES;
import static org.appwork.storage.flexijson.ParsingError.ERROR_TRAILING_COMMA_IN_ARRAY;
import static org.appwork.storage.flexijson.ParsingError.ERROR_TRAILING_COMMA_IN_OBJECT;
import static org.appwork.storage.flexijson.ParsingError.ERROR_TRAILING_DECIMAL_POINT;
import static org.appwork.storage.flexijson.ParsingError.ERROR_UNEXPECTED_CONTENT_AFTER_END_OF_JSON;
import static org.appwork.storage.flexijson.ParsingError.ERROR_VALUE_INSTEAD_OF_UNDEFINED_EXPECTED;
import static org.appwork.storage.flexijson.ParsingError.EXPECTED_ANY_CHARACTER_OR_COMMENT_CLOSE_TAGS;
import static org.appwork.storage.flexijson.ParsingError.EXPECTED_ANY_VALUE;
import static org.appwork.storage.flexijson.ParsingError.EXPECTED_ARRAY_VALUE_COMMA_OR_ARRAY_CLOSE_TAG;
import static org.appwork.storage.flexijson.ParsingError.EXPECTED_COLON;
import static org.appwork.storage.flexijson.ParsingError.EXPECTED_COMMA_OR_ARRAY_END_TAG;
import static org.appwork.storage.flexijson.ParsingError.EXPECTED_COMMA_OR_OBJECT_END_TAG;
import static org.appwork.storage.flexijson.ParsingError.EXPECTED_KEY_VALUE_PAIR_COMMA_OR_OBJECT_END_TAG;
import static org.appwork.storage.flexijson.ParsingError.EXPECTED_STRING_OR_DOUBLE_QUOTE;
import static org.appwork.storage.flexijson.ParsingError.EXPECTED_STRING_OR_SINGLE_QUOTE;
import static org.appwork.storage.flexijson.ParsingError.EXPECTED_UNICODE_SEQUENCE;
import static org.appwork.storage.flexijson.ParsingError.EXPECTED_VALID_ESCAPE_CHARACTER;

import java.io.File;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.nio.CharBuffer;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.flexijson.CharSequenceReader;
import org.appwork.storage.flexijson.FlexiJSONParser;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.FlexiJSonObject;
import org.appwork.storage.flexijson.FlexiJSonValue;
import org.appwork.storage.flexijson.FlexiParserException;
import org.appwork.storage.flexijson.FlexiUtils;
import org.appwork.storage.flexijson.KeyValueElement;
import org.appwork.storage.flexijson.ParsingError;
import org.appwork.storage.flexijson.parserextension.TimeSpanParserExtension;
import org.appwork.storage.flexijson.stringify.FlexiJSonPrettyStringify;
import org.appwork.storage.flexijson.stringify.FlexiJSonStringBuilder;
import org.appwork.storage.flexijson.stringify.PropertyJSonPrettyStringify;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.IDETestRunner;

/**
 * @author thomas
 * @date 26.03.2021
 *
 */
public class ParserTest extends AWTest {
    private static final ParsingError SVWQ  = ERROR_STRING_VALUE_WITHOUT_QUOTES;
    private static final ParsingError STWQ  = ERROR_STRING_TOKEN_WITHOUT_QUOTES;
    private static final ParsingError KWQ   = ERROR_KEY_WITHOUT_QUOTES;
    private static final ParsingError STWSQ = ERROR_STRING_TOKEN_WITH_SINGLE_QUOTES;

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.tests.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        LogV3.info("Run parsing, serializer and pretty printer Tests");
        FlexiJSONParser parser = new FlexiJSONParser("1W2D3h4m5s");
        parser.addExtension(new TimeSpanParserExtension());
        assertThat(((FlexiJSonValue) parser.parse()).getValue()).is("1W2D3h4m5s");
        expectError(" /* comment start */ {/*leading*/,start:true,/* my comment */\r\n// my single line comment\r\nend:true,/*end*/} /* comment end*/", "/*comment start*/{/*leading*/,\"start\":true,/*my comment*///my single line comment\r\n\"end\":true,/*end*/}/*comment end*/", "/* comment start */\r\n{\r\n  /* leading */,\r\n  \"start\":true,\r\n  /* my comment */// my single line comment\r\n  \"end\":true,\r\n  /* end */\r\n}\r\n/* comment end */", ERROR_TRAILING_COMMA_IN_OBJECT, ERROR_LEADING_COMMA_IN_OBJECT, KWQ, STWQ, ERROR_LINE_COMMENT, ERROR_INLINE_COMMENT);
        expectError("{},[],{/*comment*/},{/*comment\r\nnewline*/}  ", "[{},[],{/*comment*/},{/*comment\r\nnewline*/}]", "[\r\n  {},\r\n  [],\r\n  {\r\n    /* comment */\r\n  },\r\n  {\r\n    /*\r\n     * comment\r\n     * newline\r\n     */\r\n  }\r\n]", ERROR_MULTIPLE_ELEMENTS_IN_JSON_STRING, ERROR_UNEXPECTED_CONTENT_AFTER_END_OF_JSON, ERROR_INLINE_COMMENT);
        expectError("{a:b,}", "{\"a\":\"b\",}", "{\"a\":\"b\",}", ERROR_TRAILING_COMMA_IN_OBJECT, KWQ, SVWQ, STWQ);
        expectError("{a:b ,  ,  }", "{\"a\":\"b\",,}", "{\"a\":\"b\",,}", ERROR_MULTIPLE_SUBSEQUENT_COMMAS_IN_OBJECT, ERROR_MULTIPLE_TRAILING_COMMA_IN_OBJECT, ERROR_TRAILING_COMMA_IN_OBJECT, KWQ, SVWQ, STWQ);
        assertThat(((FlexiJSonObject) new FlexiJSONParser("{}").parse()).getElements().size()).is(0);
        {
            FlexiJSonObject obj = ((FlexiJSonObject) new FlexiJSONParser("{/*testcomment*/\"key\":true}").enableComments().parse());
            assertThat(FlexiUtils.serializeMinimized(obj)).is("{/*testcomment*/\"key\":true}");
        }
        {
            FlexiJSonObject obj = ((FlexiJSonObject) new FlexiJSONParser("{/*testcomment*/}").enableComments().parse());
            obj.add(new KeyValueElement(obj, "key", new FlexiJSonValue(true)));
            assertThat(FlexiUtils.serializeMinimized(obj)).is("{/*testcomment*/\"key\":true}");
        }
        expectError("\"^\\\\.\\\\%\\\\S+$\"", "\"^\\\\.\\\\%\\\\S+$\"", "\"^\\\\.\\\\%\\\\S+$\"");
        expectError("\"\"", "\"\"", "\"\"");
        expectError("{key}", "{\"key\":undefined}", "{\"key\":undefined}", EXPECTED_COLON, ERROR_STRING_TOKEN_WITHOUT_QUOTES, ERROR_VALUE_INSTEAD_OF_UNDEFINED_EXPECTED, ERROR_KEY_WITHOUT_QUOTES);
        expectError("{key\\}:0}", "{\"key}\":0}", "{\"key}\":0}", ERROR_STRING_TOKEN_WITHOUT_QUOTES, ERROR_KEY_WITHOUT_QUOTES);
        expectError("{\"key\"}", "{\"key\":undefined}", "{\"key\":undefined}", EXPECTED_COLON, ERROR_VALUE_INSTEAD_OF_UNDEFINED_EXPECTED);
        expectError("{key:}", "{\"key\":undefined}", "{\"key\":undefined}", ERROR_STRING_TOKEN_WITHOUT_QUOTES, ERROR_VALUE_INSTEAD_OF_UNDEFINED_EXPECTED, ERROR_KEY_WITHOUT_QUOTES);
        expectError("{[]}", "{\"[]\":undefined}", "{\"[]\":undefined}", ERROR_STRING_TOKEN_WITHOUT_QUOTES, EXPECTED_COLON, ERROR_VALUE_INSTEAD_OF_UNDEFINED_EXPECTED, ERROR_KEY_WITHOUT_QUOTES);
        expectError("'MeinText", "\"MeinText\"", "\"MeinText\"", ERROR_STRING_TOKEN_WITH_SINGLE_QUOTES, EXPECTED_STRING_OR_SINGLE_QUOTE, ERROR_STRING_VALUE_WITH_SINGLE_QUOTES);
        expectError("\"MeinText", "\"MeinText\"", "\"MeinText\"", EXPECTED_STRING_OR_DOUBLE_QUOTE);
        expectError("\"MeinText\"", "\"MeinText\"", "\"MeinText\"");
        expectError("\"'MeinText'\"", "\"'MeinText'\"", "\"'MeinText'\"");
        expectError("'\"MeinText\"'", "\"\\\"MeinText\\\"\"", "\"\\\"MeinText\\\"\"", ERROR_STRING_TOKEN_WITH_SINGLE_QUOTES, ERROR_STRING_VALUE_WITH_SINGLE_QUOTES);
        expectError("'MeinText'", "\"MeinText\"", "\"MeinText\"", ERROR_STRING_TOKEN_WITH_SINGLE_QUOTES, ERROR_STRING_VALUE_WITH_SINGLE_QUOTES);
        expectError("\"\\\\'\"", "\"\\\\'\"", "\"\\\\'\"");
        expectError("'\\\\\\'", "\"\\\\'\"", "\"\\\\'\"", ERROR_STRING_TOKEN_WITH_SINGLE_QUOTES, EXPECTED_STRING_OR_SINGLE_QUOTE, ERROR_STRING_VALUE_WITH_SINGLE_QUOTES);
        expectError("\\\"ab\\\"c\\\"", "\"\\\"ab\\\"c\\\"\"", "\"\\\"ab\\\"c\\\"\"", ERROR_STRING_TOKEN_WITHOUT_QUOTES, ERROR_STRING_VALUE_WITHOUT_QUOTES);
        expectError("\\\"ab\"c\\\"", "\"\\\"ab\\\"c\\\"\"", "\"\\\"ab\\\"c\\\"\"", ERROR_STRING_TOKEN_WITHOUT_QUOTES, ERROR_STRING_VALUE_WITHOUT_QUOTES);
        expectError("\"ab\\\"c\"", "\"ab\\\"c\"", "\"ab\\\"c\"");
        expectError("'\\'", "\"'\"", "\"'\"", ERROR_STRING_TOKEN_WITH_SINGLE_QUOTES, EXPECTED_STRING_OR_SINGLE_QUOTE, ERROR_STRING_VALUE_WITH_SINGLE_QUOTES);
        expectError("\\\\\\", "\"\\\\\\\\\"", "\"\\\\\\\\\"", STWQ, SVWQ, EXPECTED_VALID_ESCAPE_CHARACTER);
        expectError("Z:\\meinpfad", "\"Z:\\\\meinpfad\"", "\"Z:\\\\meinpfad\"", SVWQ, STWQ, EXPECTED_VALID_ESCAPE_CHARACTER);
        expectError("{key\r\nunescaped newline: 0}", "{\"key\\r\\nunescaped newline\":0}", "{\"key\\r\\nunescaped newline\":0}", KWQ, STWQ);
        expectError("{,a:b,,,de\\:f2:\\\"abc\r\nwith new\rlines with escaped \\,\\}\\] etc,c:d,def:abc\r\nwith new\rlines}", "{,\"a\":\"b\",,,\"de:f2\":\"\\\"abc\\r\\nwith new\\rlines with escaped ,}] etc\",\"c\":\"d\",\"def\":\"abc\\r\\nwith new\\rlines\"}", "{\r\n  ,\r\n  \"a\":\"b\",,,\r\n  \"de:f2\":\"\\\"abc\\r\\nwith new\\rlines with escaped ,}] etc\",\r\n  \"c\":\"d\",\r\n  \"def\":\"abc\\r\\nwith new\\rlines\"\r\n}", SVWQ, STWQ, KWQ, ERROR_LEADING_COMMA_IN_OBJECT, ERROR_MULTIPLE_SUBSEQUENT_COMMAS_IN_OBJECT);
        expectError("{\"key with space\": 0}", "{\"key with space\":0}", "{\"key with space\":0}");
        expectError("{\"key\r\nunescaped newlineandquotes\": 0}", "{\"key\\r\\nunescaped newlineandquotes\":0}", "{\"key\\r\\nunescaped newlineandquotes\":0}", ERROR_NEWLINE_IN_TOKEN);
        expectError("{\"key\\r\\nunescaped newlineandquotes\": 0}", "{\"key\\r\\nunescaped newlineandquotes\":0}", "{\"key\\r\\nunescaped newlineandquotes\":0}");
        expectError("{\r\n\"dec\": 0\r\n\r\n\t\t\t}", "{\"dec\":0}", "{\"dec\":0}");
        expectError("{\r\n\"dec\": 01\r\n\r\n\t\t\t}", "{\"dec\":1}", "{\"dec\":1}", ERROR_NUMBERFORMAT_OCTAL);
        expectError("{\r\n\"dec\": 010\r\n\r\n\t\t\t}", "{\"dec\":8}", "{\"dec\":8}", ERROR_NUMBERFORMAT_OCTAL);
        expectError(" /* comment start */ [1,2,3] /* comment end*/", "/*comment start*/[1,2,3]/*comment end*/", "/* comment start */\r\n[\r\n  1,\r\n  2,\r\n  3\r\n]\r\n/* comment end */", ERROR_INLINE_COMMENT);
        expectError("{\"a\":1}/*com*/\u0000", "[{\"a\":1}/*com*/,\"\\u0000\"]", "[\r\n  {\"a\":1}\r\n  /* com */,\r\n  \"\\u0000\"\r\n]", ERROR_INLINE_COMMENT, ERROR_UNEXPECTED_CONTENT_AFTER_END_OF_JSON, ERROR_STRING_VALUE_WITHOUT_QUOTES, ERROR_STRING_TOKEN_WITHOUT_QUOTES, ERROR_MULTIPLE_ELEMENTS_IN_JSON_STRING);
        expectError("{\"a\":1}\u0000", "[{\"a\":1},\"\\u0000\"]", "[\r\n  {\"a\":1},\r\n  \"\\u0000\"\r\n]", ERROR_UNEXPECTED_CONTENT_AFTER_END_OF_JSON, ERROR_STRING_VALUE_WITHOUT_QUOTES, ERROR_STRING_TOKEN_WITHOUT_QUOTES, ERROR_MULTIPLE_ELEMENTS_IN_JSON_STRING);
        expectError("{,a:b,,c:d,def:abc\r\nwith new\rlines}", "{,\"a\":\"b\",,\"c\":\"d\",\"def\":\"abc\\r\\nwith new\\rlines\"}", "{\r\n  ,\r\n  \"a\":\"b\",,\r\n  \"c\":\"d\",\r\n  \"def\":\"abc\\r\\nwith new\\rlines\"\r\n}", SVWQ, STWQ, KWQ, ERROR_LEADING_COMMA_IN_OBJECT, ERROR_MULTIPLE_SUBSEQUENT_COMMAS_IN_OBJECT);
        expectError("異體字字", "\"異體字字\"", "\"異體字字\"", STWQ, SVWQ); // NO Surrogate
        expectError("𩸽", "\"𩸽\"", "\"𩸽\"", STWQ, SVWQ);// YES Surrogate
        expectError("𩸽 and 體", "\"𩸽 and 體\"", "\"𩸽 and 體\"", STWQ, SVWQ);// YES AND NO Surrogate
        expectError("\\\\", "\"\\\\\"", "\"\\\\\"", STWQ, SVWQ);
        expectError("{\"test\"://huhu\r\ntrue}", "{\"test\"://huhu\r\ntrue}", "{\r\n  \"test\":// huhu\r\n  true\r\n}", ERROR_LINE_COMMENT);
        expectError("{\"test\"://\"huhu\"\r\ntrue}", "{\"test\"://\"huhu\"\r\ntrue}", "{\r\n  \"test\":// \"huhu\"\r\n  true\r\n}", ERROR_LINE_COMMENT);
        expectError("/*comment*\\/ start*/[]/*comment end*/", "/*comment*\\/ start*/[]/*comment end*/", "/* comment*\\/ start */\r\n[]\r\n/* comment end */", ERROR_INLINE_COMMENT);
        expectError("{\"defaultdownloadfolder\":\"C:\\\\Users\\\\thomas\\\\down\\rloads\"}", "{\"defaultdownloadfolder\":\"C:\\\\Users\\\\thomas\\\\down\\rloads\"}", "{\"defaultdownloadfolder\":\"C:\\\\Users\\\\thomas\\\\down\\rloads\"}");
        expectError("[1,2,\"23\",4,5,[true,false]]", "[1,2,\"23\",4,5,[true,false]]", "[\r\n  1,\r\n  2,\r\n  \"23\",\r\n  4,\r\n  5,\r\n  [true, false]\r\n]");
        expectError("{[1]:[2]}", "{\"[1]\":[2]}", "{\r\n  \"[1]\":[2]\r\n}", STWQ, KWQ);
        expectError("{  }", "{}", "{}");
        expectError("[\r\n {\r\n  \"map\"              : {\r\n                        \"2\" : {\r\n                               \"map\"              : {},\r\n                               \"string\"           : \"affe232\",\r\n                               \"genericClass\"     : {\r\n                                                     \"key\"   : \"mykey\",\r\n                                                     \"value\" : \"myValue\"\r", "[{\"map\":{\"2\":{\"map\":{},\"string\":\"affe232\",\"genericClass\":{\"key\":\"mykey\",\"value\":\"myValue\"}}}}]", "[{\r\n  \"map\":{\r\n    \"2\":{\r\n      \"map\":{},\r\n      \"string\":\"affe232\",\r\n      \"genericClass\":{\r\n        \"key\":\"mykey\",\r\n        \"value\":\"myValue\"\r\n      }\r\n    }\r\n  }\r\n}]", EXPECTED_COMMA_OR_OBJECT_END_TAG, EXPECTED_COMMA_OR_ARRAY_END_TAG);
        expectError("[/*first*/1,/*second*/2]", "[/*first*/1,/*second*/2]", "[/* first */1, /* second */2]", ERROR_INLINE_COMMENT);
        expectError(" /* comment start */ [/*inside*/ ,,] /* comment end*/", "/*comment start*/[/*inside*/undefined,undefined]/*comment end*/", "/* comment start */\r\n[\r\n  /* inside */\r\n  undefined,\r\n  undefined\r\n]\r\n/* comment end */", ERROR_MULTIPLE_LEADING_COMMA_IN_ARRAY, ERROR_LEADING_COMMA_IN_ARRAY, ERROR_TRAILING_COMMA_IN_ARRAY, ERROR_MULTIPLE_TRAILING_COMMA_IN_ARRAY, ERROR_INLINE_COMMENT, ERROR_MULTIPLE_SUBSEQUENT_COMMAS_IN_ARRAY);
        // expectError(null, null, "/* comment start */\r\n{\r\n /* leading */,\r\n \"start\":true,\r\n /* my comment */// my single line
        // comment\r\n \"end\":true,\r\n /* end */\r\n}\r\n/* comment end */", ERROR_TRAILING_COMMA_IN_OBJECT,
        // ERROR_LEADING_COMMA_IN_OBJECT, KWQ, STWQ, ERROR_LINE_COMMENT, ERROR_INLINE_COMMENT);
        expectError(" {  a/*comment*/:b,c:d}", "{\"a\"/*comment*/:\"b\",\"c\":\"d\"}", "{\r\n  \"a\"/* comment */:\"b\",\r\n  \"c\":\"d\"\r\n}", ERROR_INLINE_COMMENT, SVWQ, STWQ, KWQ);
        expectError("  //comment ", "undefined//comment", "undefined// comment", ERROR_LINE_COMMENT, ERROR_NO_CONTENT);
        expectError(" {b:{a:/* comment start\r\n with newline */ [] /* comment end*/}}", "{\"b\":{\"a\":/*comment start\r\nwith newline*/[]/*comment end*/}}", "{\r\n  \"b\":{\r\n    \"a\":\r\n    /*\r\n     * comment start\r\n     * with newline\r\n     */\r\n    []\r\n    /* comment end */\r\n  }\r\n}", ERROR_INLINE_COMMENT, ERROR_STRING_TOKEN_WITHOUT_QUOTES, ERROR_KEY_WITHOUT_QUOTES);
        expectError("///* i1 /*\r\n1", "///* i1 /*\r\n1", "// /* i1 /*\r\n1", ERROR_LINE_COMMENT);
        expectError(" /* comment start */ [/*1*/ ,,/*2*/] /* comment end*/", "/*comment start*/[/*1*/undefined,undefined,/*2*/]/*comment end*/", "/* comment start */\r\n[\r\n  /* 1 */\r\n  undefined,\r\n  undefined, \r\n  /* 2 */\r\n]\r\n/* comment end */", ERROR_MULTIPLE_TRAILING_COMMA_IN_ARRAY, ERROR_TRAILING_COMMA_IN_ARRAY, ERROR_MULTIPLE_SUBSEQUENT_COMMAS_IN_ARRAY, ERROR_LEADING_COMMA_IN_ARRAY, ERROR_MULTIPLE_LEADING_COMMA_IN_ARRAY, ERROR_INLINE_COMMENT);
        expectError("{,,}", "{,,}", "{\r\n  ,,\r\n}", ERROR_LEADING_COMMA_IN_OBJECT, ERROR_MULTIPLE_LEADING_COMMA_IN_OBJECT, ERROR_MULTIPLE_SUBSEQUENT_COMMAS_IN_OBJECT, ERROR_MULTIPLE_TRAILING_COMMA_IN_OBJECT, ERROR_TRAILING_COMMA_IN_OBJECT);
        expectError("'Unicode pattern testblabla\\u1", "\"Unicode pattern testblabla\\u0001\"", "\"Unicode pattern testblabla\\u0001\"", EXPECTED_UNICODE_SEQUENCE, ERROR_STRING_VALUE_WITH_SINGLE_QUOTES, ERROR_STRING_TOKEN_WITH_SINGLE_QUOTES, EXPECTED_STRING_OR_SINGLE_QUOTE, ERROR_STRING_TOKEN_WITH_SINGLE_QUOTES);
        expectError("{_a:{\"_\\\"b2\":{_c:{_d342:{\"test\":},s:1,loooooong:2}}}}", "{\"_a\":{\"_\\\"b2\":{\"_c\":{\"_d342\":{\"test\":undefined},\"s\":1,\"loooooong\":2}}}}", "{\r\n  \"_a\":{\r\n    \"_\\\"b2\":{\r\n      \"_c\":{\r\n        \"_d342\":{\"test\":undefined},\r\n        \"s\":1,\r\n        \"loooooong\":2\r\n      }\r\n    }\r\n  }\r\n}", STWQ, KWQ, ERROR_VALUE_INSTEAD_OF_UNDEFINED_EXPECTED);
        expectError(" {b:{a:/* comment start\r\n with newline */ [1,'Array with a long string that exeeds the 40 chars limit for compact mode',3,'second long string with \r\n newline'] /* comment end*/}}", "{\"b\":{\"a\":/*comment start\r\nwith newline*/[1,\"Array with a long string that exeeds the 40 chars limit for compact mode\",3,\"second long string with \\r\\n newline\"]/*comment end*/}}", "{\r\n  \"b\":{\r\n    \"a\":\r\n    /*\r\n     * comment start\r\n     * with newline\r\n     */\r\n    [\r\n      1,\r\n      \"Array with a long string that exeeds the 40 chars limit for compact mode\",\r\n      3,\r\n      \"second long string with \\r\\n newline\"\r\n    ]\r\n    /* comment end */\r\n  }\r\n}", ERROR_STRING_VALUE_WITH_SINGLE_QUOTES, ERROR_STRING_TOKEN_WITHOUT_QUOTES, ERROR_KEY_WITHOUT_QUOTES, ERROR_INLINE_COMMENT, ERROR_STRING_TOKEN_WITH_SINGLE_QUOTES,
                ERROR_NEWLINE_IN_TOKEN);
        expectError(" {b:{a:/* comment start\r\n with newline\r\nArraywith object */ [1,{a:b,c:[true,false]},3] /* comment end*/}}", "{\"b\":{\"a\":/*comment start\r\nwith newline\r\nArraywith object*/[1,{\"a\":\"b\",\"c\":[true,false]},3]/*comment end*/}}", "{\r\n  \"b\":{\r\n    \"a\":\r\n    /*\r\n     * comment start\r\n     * with newline\r\n     * Arraywith object\r\n     */\r\n    [\r\n      1,\r\n      {\r\n        \"a\":\"b\",\r\n        \"c\":[true, false]\r\n      },\r\n      3\r\n    ]\r\n    /* comment end */\r\n  }\r\n}", ERROR_INLINE_COMMENT, ERROR_STRING_TOKEN_WITHOUT_QUOTES, ERROR_KEY_WITHOUT_QUOTES, ERROR_STRING_VALUE_WITHOUT_QUOTES);
        expectError(" {b:{a:/* comment start\r\n with newline */ [1,2,3] /* comment end*/}}", "{\"b\":{\"a\":/*comment start\r\nwith newline*/[1,2,3]/*comment end*/}}", "{\r\n  \"b\":{\r\n    \"a\":\r\n    /*\r\n     * comment start\r\n     * with newline\r\n     */\r\n    [\r\n      1,\r\n      2,\r\n      3\r\n    ]\r\n    /* comment end */\r\n  }\r\n}", ERROR_INLINE_COMMENT, ERROR_STRING_TOKEN_WITHOUT_QUOTES, ERROR_KEY_WITHOUT_QUOTES);
        expectError(" /* comment start */ [] /* comment end*/", "/*comment start*/[]/*comment end*/", "/* comment start */[]/* comment end */", ERROR_INLINE_COMMENT);
        expectError("///* i1 /*\r\n1", "///* i1 /*\r\n1", "// /* i1 /*\r\n1", ERROR_LINE_COMMENT);
        expectError("{,a:b,,c:d,def:abc}", "{,\"a\":\"b\",,\"c\":\"d\",\"def\":\"abc\"}", "{\r\n  ,\r\n  \"a\":\"b\",,\r\n  \"c\":\"d\",\r\n  \"def\":\"abc\"\r\n}", SVWQ, STWQ, KWQ, ERROR_LEADING_COMMA_IN_OBJECT, ERROR_MULTIPLE_SUBSEQUENT_COMMAS_IN_OBJECT);
        expectError("{_a:{_b2:{_c:{_d342:{\"test\":},s:1,loooooong:2}}}}", "{\"_a\":{\"_b2\":{\"_c\":{\"_d342\":{\"test\":undefined},\"s\":1,\"loooooong\":2}}}}", "{\r\n  \"_a\":{\r\n    \"_b2\":{\r\n      \"_c\":{\r\n        \"_d342\":{\"test\":undefined},\r\n        \"s\":1,\r\n        \"loooooong\":2\r\n      }\r\n    }\r\n  }\r\n}", STWQ, KWQ, ERROR_VALUE_INSTEAD_OF_UNDEFINED_EXPECTED);
        expectError("\"{ich bin ein key mit space:ich bin ein value mit space}", "\"{ich bin ein key mit space:ich bin ein value mit space}\"", "\"{ich bin ein key mit space:ich bin ein value mit space}\"", EXPECTED_STRING_OR_DOUBLE_QUOTE);
        expectError("{ich bin ein key mit space:ich bin ein value mit space}", "{\"ich bin ein key mit space\":\"ich bin ein value mit space\"}", "{\"ich bin ein key mit space\":\"ich bin ein value mit space\"}", SVWQ, STWQ, KWQ);
        expectError("/* i1 /* i2 */", "undefined/*i1 /* i2*/", "undefined/* i1 /* i2 */", ERROR_INLINE_COMMENT, ERROR_NO_CONTENT);
        // comments not allowed - parse as string without quotes
        expectError(" /* comment start */ [/*1*/ ,,/*2*/] /* comment end*/", "\"/* comment start */ [/*1*/ ,,/*2*/] /* comment end*/\"", "\"/* comment start */ [/*1*/ ,,/*2*/] /* comment end*/\"", ERROR_STRING_TOKEN_WITHOUT_QUOTES, ERROR_STRING_VALUE_WITHOUT_QUOTES);
        expectError("123 //int", "\"123 //int\"", "\"123 //int\"", ERROR_STRING_TOKEN_WITHOUT_QUOTES, ERROR_STRING_VALUE_WITHOUT_QUOTES);
        expectError(" /* comment start */ [] /* comment end*/", "/*comment start*/[]/*comment end*/", "/* comment start */[]/* comment end */", ERROR_INLINE_COMMENT);
        expectError(" /* comment start */ {start:true,/* my comment */\r\n// my single line comment\r\nend:true} /* comment end*/", "/*comment start*/{\"start\":true,/*my comment*///my single line comment\r\n\"end\":true}/*comment end*/", "/* comment start */\r\n{\r\n  \"start\":true,\r\n  /* my comment */// my single line comment\r\n  \"end\":true\r\n}\r\n/* comment end */", KWQ, STWQ, ERROR_LINE_COMMENT, ERROR_INLINE_COMMENT);
        expectError(" /* comment start */ {start:true,/* my comment */\r\n// my single line comment\r\nend:true,/*end*/} /* comment end*/", "/*comment start*/{\"start\":true,/*my comment*///my single line comment\r\n\"end\":true,/*end*/}/*comment end*/", "/* comment start */\r\n{\r\n  \"start\":true,\r\n  /* my comment */// my single line comment\r\n  \"end\":true,\r\n  /* end */\r\n}\r\n/* comment end */", ERROR_TRAILING_COMMA_IN_OBJECT, KWQ, STWQ, ERROR_LINE_COMMENT, ERROR_INLINE_COMMENT);
        expectError("/**/  ", "undefined/**/", "undefined/*  */", ERROR_INLINE_COMMENT, ERROR_NO_CONTENT);
        expectError("//", "undefined//", "undefined// ", ERROR_LINE_COMMENT, ERROR_NO_CONTENT);
        expectError("  /*comment*  ", "undefined/*comment**/", "undefined/* comment* */", ERROR_INLINE_COMMENT, ERROR_NO_CONTENT, EXPECTED_ANY_CHARACTER_OR_COMMENT_CLOSE_TAGS);
        expectError(" \"string\" //comment ", "\"string\"//comment", "\"string\"// comment", ERROR_LINE_COMMENT);
        expectError(" /* comment*/ string", "/*comment*/\"string\"", "/* comment */\"string\"", SVWQ, ERROR_INLINE_COMMENT, STWQ);
        expectError(" /* before value*/ 1", "/*before value*/1", "/* before value */1", ERROR_INLINE_COMMENT);
        expectError("  1 /* after value*/", "1/*after value*/", "1/* after value */", ERROR_INLINE_COMMENT);
        expectError("/*comment*/ {  a:b,c:d}", "/*comment*/{\"a\":\"b\",\"c\":\"d\"}", "/* comment */\r\n{\r\n  \"a\":\"b\",\r\n  \"c\":\"d\"\r\n}", ERROR_INLINE_COMMENT, SVWQ, STWQ, KWQ);
        expectError(" { /*comment*/ a:b,c:d}", "{/*comment*/\"a\":\"b\",\"c\":\"d\"}", "{\r\n  /* comment */\r\n  \"a\":\"b\",\r\n  \"c\":\"d\"\r\n}", ERROR_INLINE_COMMENT, SVWQ, STWQ, KWQ);
        expectError(" {  a/*comment*/:b,c:d}", "{\"a\"/*comment*/:\"b\",\"c\":\"d\"}", "{\r\n  \"a\"/* comment */:\"b\",\r\n  \"c\":\"d\"\r\n}", ERROR_INLINE_COMMENT, SVWQ, STWQ, KWQ);
        expectError(" {  a:/*comment*/b,c:d}", "{\"a\":/*comment*/\"b\",\"c\":\"d\"}", "{\r\n  \"a\":/* comment */\"b\",\r\n  \"c\":\"d\"\r\n}", ERROR_INLINE_COMMENT, SVWQ, STWQ, KWQ);
        expectError(" {  a:b/*comment*/,c:d}", "{\"a\":\"b\"/*comment*/,\"c\":\"d\"}", "{\r\n  \"a\":\"b\"/* comment */,\r\n  \"c\":\"d\"\r\n}", ERROR_INLINE_COMMENT, SVWQ, STWQ, KWQ);
        expectError(" {  a:b,/*comment*/c:d}", "{\"a\":\"b\",/*comment*/\"c\":\"d\"}", "{\r\n  \"a\":\"b\",\r\n  /* comment */\r\n  \"c\":\"d\"\r\n}", ERROR_INLINE_COMMENT, SVWQ, STWQ, KWQ);
        expectError(" {  a:b,c:d/*comment*/}", "{\"a\":\"b\",\"c\":\"d\"/*comment*/}", "{\r\n  \"a\":\"b\",\r\n  \"c\":\"d\"/* comment */\r\n}", ERROR_INLINE_COMMENT, SVWQ, STWQ, KWQ);
        expectError(" {  a:b,c:d} /*comment*/ ", "{\"a\":\"b\",\"c\":\"d\"}/*comment*/", "{\r\n  \"a\":\"b\",\r\n  \"c\":\"d\"\r\n}\r\n/* comment */", ERROR_INLINE_COMMENT, SVWQ, STWQ, KWQ);
        expectError("/*com1*/  /* com2 */  /*comm3*/ string//comment ", "/*com1*//*com2*//*comm3*/\"string\"//comment", "/* com1 *//* com2 *//* comm3 */\"string\"// comment", SVWQ, ERROR_LINE_COMMENT, STWQ, ERROR_INLINE_COMMENT);
        expectError("0", "0", "0");
        expectError("  '1','2'  ", "[\"1\",\"2\"]", "[\"1\", \"2\"]", ERROR_STRING_VALUE_WITH_SINGLE_QUOTES, ERROR_MULTIPLE_ELEMENTS_IN_JSON_STRING, ERROR_UNEXPECTED_CONTENT_AFTER_END_OF_JSON, ERROR_STRING_TOKEN_WITH_SINGLE_QUOTES);
        expectError("{},[],{}  ", "[{},[],{}]", "[\r\n  {},\r\n  [],\r\n  {}\r\n]", ERROR_MULTIPLE_ELEMENTS_IN_JSON_STRING, ERROR_UNEXPECTED_CONTENT_AFTER_END_OF_JSON);
        expectError("{a:1,'b':'1''c':2}", "{\"a\":1,\"b\":\"1\",\"c\":2}", "{\r\n  \"a\":1,\r\n  \"b\":\"1\",\r\n  \"c\":2\r\n}", ERROR_STRING_TOKEN_WITH_SINGLE_QUOTES, ERROR_KEY_WITH_SINGLE_QUOTES, ERROR_STRING_TOKEN_WITHOUT_QUOTES, ERROR_KEY_WITHOUT_QUOTES, EXPECTED_COMMA_OR_OBJECT_END_TAG, ERROR_STRING_VALUE_WITH_SINGLE_QUOTES);
        expectError("{342.187124E-3:true}", "{\"0.342187124\":true}", "{\"0.342187124\":true}", KWQ, ERROR_KEY_IS_NUMBER);
        expectError("{true:\"^\\\\.\\\\%\\\\S+$\"}", "{\"true\":\"^\\\\.\\\\%\\\\S+$\"}", "{\"true\":\"^\\\\.\\\\%\\\\S+$\"}", KWQ, ERROR_KEY_IS_BOOLEAN);
        expectError("{true:^\\\\.\\\\%\\\\S+$}", "{\"true\":\"^\\\\.\\\\%\\\\S+$\"}", "{\"true\":\"^\\\\.\\\\%\\\\S+$\"}", STWQ, KWQ, SVWQ, ERROR_KEY_IS_BOOLEAN);
        expectError("{\"true\":^\\\\.\\\\%\\\\S+$}", "{\"true\":\"^\\\\.\\\\%\\\\S+$\"}", "{\"true\":\"^\\\\.\\\\%\\\\S+$\"}", ERROR_STRING_TOKEN_WITHOUT_QUOTES, ERROR_STRING_VALUE_WITHOUT_QUOTES);
        expectError("{true:\"^\\\\.\\\\%\\\\S+$\"}", "{\"true\":\"^\\\\.\\\\%\\\\S+$\"}", "{\"true\":\"^\\\\.\\\\%\\\\S+$\"}", KWQ, ERROR_KEY_IS_BOOLEAN);
        expectError("['Z:\\\\']", "[\"Z:\\\\\"]", "[\"Z:\\\\\"]", ERROR_STRING_VALUE_WITH_SINGLE_QUOTES, ERROR_STRING_TOKEN_WITH_SINGLE_QUOTES);
        expectError("string'", "\"string'\"", "\"string'\"", ERROR_STRING_TOKEN_WITHOUT_QUOTES, ERROR_STRING_VALUE_WITHOUT_QUOTES);
        expectError("'string", "\"string\"", "\"string\"", ERROR_STRING_VALUE_WITH_SINGLE_QUOTES, ERROR_STRING_TOKEN_WITH_SINGLE_QUOTES, EXPECTED_STRING_OR_SINGLE_QUOTE);
        expectError("'string'", "\"string\"", "\"string\"", STWSQ, ERROR_STRING_VALUE_WITH_SINGLE_QUOTES);
        expectError("{leadingCOmmaInArray: [,,1,,,]}", "{\"leadingCOmmaInArray\":[undefined,undefined,1,undefined,undefined]}", "{\r\n  \"leadingCOmmaInArray\":[\r\n    undefined,\r\n    undefined,\r\n    1,\r\n    undefined,\r\n    undefined\r\n  ]\r\n}", STWQ, KWQ, ERROR_MULTIPLE_LEADING_COMMA_IN_ARRAY, ERROR_TRAILING_COMMA_IN_ARRAY, ERROR_LEADING_COMMA_IN_ARRAY, ERROR_MULTIPLE_SUBSEQUENT_COMMAS_IN_ARRAY, ERROR_MULTIPLE_TRAILING_COMMA_IN_ARRAY);
        expectError("{trailingCOmmaInArray: [1,,,]}", "{\"trailingCOmmaInArray\":[1,undefined,undefined]}", "{\r\n  \"trailingCOmmaInArray\":[1, undefined, undefined]\r\n}", STWQ, KWQ, ERROR_TRAILING_COMMA_IN_ARRAY, ERROR_MULTIPLE_SUBSEQUENT_COMMAS_IN_ARRAY, ERROR_MULTIPLE_TRAILING_COMMA_IN_ARRAY);
        expectError("{trailingCOmmaInArray: [1,,,2]}", "{\"trailingCOmmaInArray\":[1,undefined,undefined,2]}", "{\r\n  \"trailingCOmmaInArray\":[1, undefined, undefined, 2]\r\n}", STWQ, KWQ, ERROR_MULTIPLE_SUBSEQUENT_COMMAS_IN_ARRAY);
        expectError("{onlyEmptys: [,,,]}", "{\"onlyEmptys\":[undefined,undefined,undefined]}", "{\r\n  \"onlyEmptys\":[undefined, undefined, undefined]\r\n}", STWQ, KWQ, ERROR_TRAILING_COMMA_IN_ARRAY, ERROR_LEADING_COMMA_IN_ARRAY, ERROR_MULTIPLE_LEADING_COMMA_IN_ARRAY, ERROR_MULTIPLE_SUBSEQUENT_COMMAS_IN_ARRAY, ERROR_MULTIPLE_TRAILING_COMMA_IN_ARRAY);
        expectError("{a:{}}", "{\"a\":{}}", "{\r\n  \"a\":{}\r\n}", KWQ, STWQ);
        expectError("{,,}", "{,,}", "{\r\n  ,,\r\n}", ERROR_LEADING_COMMA_IN_OBJECT, ERROR_MULTIPLE_LEADING_COMMA_IN_OBJECT, ERROR_MULTIPLE_SUBSEQUENT_COMMAS_IN_OBJECT, ERROR_MULTIPLE_TRAILING_COMMA_IN_OBJECT, ERROR_TRAILING_COMMA_IN_OBJECT);
        expectError("{a:b,,}", "{\"a\":\"b\",,}", "{\"a\":\"b\",,}", ERROR_MULTIPLE_SUBSEQUENT_COMMAS_IN_OBJECT, ERROR_MULTIPLE_TRAILING_COMMA_IN_OBJECT, ERROR_TRAILING_COMMA_IN_OBJECT, KWQ, SVWQ, STWQ);
        expectError("{a:b ,  ,  c:d}", "{\"a\":\"b\",,\"c\":\"d\"}", "{\r\n  \"a\":\"b\",,\r\n  \"c\":\"d\"\r\n}", ERROR_MULTIPLE_SUBSEQUENT_COMMAS_IN_OBJECT, KWQ, SVWQ, STWQ);
        expectError("{,a:b}", "{,\"a\":\"b\"}", "{\r\n  ,\r\n  \"a\":\"b\"\r\n}", SVWQ, STWQ, KWQ, ERROR_LEADING_COMMA_IN_OBJECT);
        expectError("{,,a:b}", "{,,\"a\":\"b\"}", "{\r\n  ,,\r\n  \"a\":\"b\"\r\n}", SVWQ, STWQ, KWQ, ERROR_LEADING_COMMA_IN_OBJECT, ERROR_MULTIPLE_LEADING_COMMA_IN_OBJECT, ERROR_MULTIPLE_SUBSEQUENT_COMMAS_IN_OBJECT);
        expectError("{  ,  ,a:b}", "{,,\"a\":\"b\"}", "{\r\n  ,,\r\n  \"a\":\"b\"\r\n}", SVWQ, STWQ, KWQ, ERROR_LEADING_COMMA_IN_OBJECT, ERROR_MULTIPLE_LEADING_COMMA_IN_OBJECT, ERROR_MULTIPLE_SUBSEQUENT_COMMAS_IN_OBJECT);
        expectError("{, , a:b}", "{,,\"a\":\"b\"}", "{\r\n  ,,\r\n  \"a\":\"b\"\r\n}", SVWQ, STWQ, KWQ, ERROR_LEADING_COMMA_IN_OBJECT, ERROR_MULTIPLE_LEADING_COMMA_IN_OBJECT, ERROR_MULTIPLE_SUBSEQUENT_COMMAS_IN_OBJECT);
        expectError("{,a:b}", "{,\"a\":\"b\"}", "{\r\n  ,\r\n  \"a\":\"b\"\r\n}", SVWQ, STWQ, KWQ, ERROR_LEADING_COMMA_IN_OBJECT);
        expectError("{a: -  NaN }", "{\"a\":NaN}", "{\"a\":NaN}", KWQ, STWQ, ERROR_NUMBER_NAN);
        expectError("string/* comment */ ", "\"string\"/*comment*/", "\"string\"/* comment */", SVWQ, ERROR_INLINE_COMMENT, STWQ);
        expectError("string//comment ", "\"string\"//comment", "\"string\"// comment", SVWQ, ERROR_LINE_COMMENT, STWQ);
        expectError("'string'/* comment */ ", "\"string\"/*comment*/", "\"string\"/* comment */", ERROR_STRING_VALUE_WITH_SINGLE_QUOTES, STWSQ, ERROR_INLINE_COMMENT);
        // span[@id='price_inside_buybox']
        // div[@id='ddmDeliveryMessage']
        expectError("  .1  ", "0.1", "0.1", ParsingError.ERROR_LEADING_DECIMAL_POINT);
        expectError("  1.  ", "1.0", "1.0", ParsingError.ERROR_TRAILING_DECIMAL_POINT);
        expectError("{a: -  Infinity }", "{\"a\":-Infinity}", "{\"a\":-Infinity}", ERROR_NUMBER_INFINITY, KWQ, STWQ);
        expectError("{a: -Infinity }", "{\"a\":-Infinity}", "{\"a\":-Infinity}", ERROR_NUMBER_INFINITY, KWQ, STWQ);
        expectError("{a:b , }", "{\"a\":\"b\",}", "{\"a\":\"b\",}", ERROR_TRAILING_COMMA_IN_OBJECT, KWQ, SVWQ, STWQ);
        expectError("[ 1 ,   2  ,   3         ]", "[1,2,3]", "[1, 2, 3]");
        expectError("[ string with space,   2   , 2 number with string of space     ]", "[\"string with space\",2,\"2 number with string of space\"]", "[\r\n  \"string with space\",\r\n  2,\r\n  \"2 number with string of space\"\r\n]", SVWQ, STWQ);
        expectError("[ true ,   false  ,   NaN , null         ]", "[true,false,NaN,null]", "[true, false, NaN, null]", ERROR_NUMBER_NAN);
        expectError("[true,false,NaN,null,true false,null true,0 1 2 3]", "[true,false,NaN,null,\"true false\",\"null true\",\"0 1 2 3\"]", "[\r\n  true,\r\n  false,\r\n  NaN,\r\n  null,\r\n  \"true false\",\r\n  \"null true\",\r\n  \"0 1 2 3\"\r\n]", SVWQ, STWQ, ERROR_NUMBER_NAN);
        expectError("[Z:\\pfad]", "[\"Z:\\\\pfad\"]", "[\"Z:\\\\pfad\"]", SVWQ, STWQ, EXPECTED_VALID_ESCAPE_CHARACTER);
        expectError("{\"pByte\":20}", "{\"pByte\":20}", "{\"pByte\":20}");
        expectError("\\\\u7570\\\\u9AD4\\\\u5B57\\\\u5B57", "\"\\\\u7570\\\\u9AD4\\\\u5B57\\\\u5B57\"", "\"\\\\u7570\\\\u9AD4\\\\u5B57\\\\u5B57\"", STWQ, SVWQ);
        expectError("\"\\\\u7570\\\\u9AD4\\\\u5B57\\\\u5B57\"", "\"\\\\u7570\\\\u9AD4\\\\u5B57\\\\u5B57\"", "\"\\\\u7570\\\\u9AD4\\\\u5B57\\\\u5B57\"");
        expectError("異體字字", "\"異體字字\"", "\"異體字字\"", STWQ, SVWQ);
        expectError("\\\\", "\"\\\\\"", "\"\\\\\"", STWQ, SVWQ);
        expectError("\"\\\\\"", "\"\\\\\"", "\"\\\\\"");
        expectError("'\\\\'", "\"\\\\\"", "\"\\\\\"", STWSQ, ERROR_STRING_VALUE_WITH_SINGLE_QUOTES);
        expectError("\\\\", "\"\\\\\"", "\"\\\\\"", STWQ, SVWQ);
        expectError("'\\\\\\\\'", "\"\\\\\\\\\"", "\"\\\\\\\\\"", ERROR_STRING_TOKEN_WITH_SINGLE_QUOTES, ERROR_STRING_VALUE_WITH_SINGLE_QUOTES);
        expectError("'\\\\\\\\\\\\'", "\"\\\\\\\\\\\\\"", "\"\\\\\\\\\\\\\"", ERROR_STRING_TOKEN_WITH_SINGLE_QUOTES, ERROR_STRING_VALUE_WITH_SINGLE_QUOTES);
        expectError("1,2,3", "\"1,2,3\"", "\"1,2,3\"", STWQ, SVWQ);
        expectError("{trailingCOmmaInObject: true,}", "{\"trailingCOmmaInObject\":true,}", "{\"trailingCOmmaInObject\":true,}", STWQ, KWQ, ERROR_TRAILING_COMMA_IN_OBJECT);
        expectError("[\"Z:\\\\\"]", "[\"Z:\\\\\"]", "[\"Z:\\\\\"]");
        expectError("['Z\\\\']", "[\"Z\\\\\"]", "[\"Z\\\\\"]", STWSQ, ERROR_STRING_VALUE_WITH_SINGLE_QUOTES);
        expectError("[Z\\\\]", "[\"Z\\\\\"]", "[\"Z\\\\\"]", ERROR_STRING_TOKEN_WITHOUT_QUOTES, ERROR_STRING_VALUE_WITHOUT_QUOTES);
        expectError("[1,2,3]", "[1,2,3]", "[1, 2, 3]");
        expectError("{leadingPoint:.01}", "{\"leadingPoint\":0.01}", "{\"leadingPoint\":0.01}", STWQ, KWQ, ERROR_LEADING_DECIMAL_POINT);
        expectError("{dec:2070}", "{\"dec\":2070}", "{\"dec\":2070}", STWQ, KWQ);
        expectError("{dec:-164730}", "{\"dec\":-164730}", "{\"dec\":-164730}", STWQ, KWQ);
        expectError("{dec:-164730at}", "{\"dec\":\"-164730at\"}", "{\"dec\":\"-164730at\"}", SVWQ, STWQ, KWQ);
        expectError("{dec:28.12}", "{\"dec\":28.12}", "{\"dec\":28.12}", STWQ, KWQ);
        expectError("{hex:0x816}", "{\"hex\":2070}", "{\"hex\":2070}", STWQ, KWQ, ERROR_NUMBERFORMAT_HEXADECIMAL);
        expectError("{hex:-0x2837a}", "{\"hex\":-164730}", "{\"hex\":-164730}", STWQ, KWQ, ERROR_NUMBERFORMAT_HEXADECIMAL);
        // invalid number format. parse as string
        expectError("{hex:-0x2837at}", "{\"hex\":\"-0x2837at\"}", "{\"hex\":\"-0x2837at\"}", SVWQ, STWQ, KWQ);
        // . in hex is not allowed. should be parsed as a string
        expectError("{hex:0x28.12}", "{\"hex\":\"0x28.12\"}", "{\"hex\":\"0x28.12\"}", STWQ, KWQ, ERROR_STRING_VALUE_WITHOUT_QUOTES);
        // whitespace between - and digits
        expectError("{hex:+   0x03}", "{\"hex\":3}", "{\"hex\":3}", STWQ, KWQ, ERROR_NUMBERFORMAT_HEXADECIMAL, ERROR_NUMBERFORMAT_LEADING_PLUS);
        expectError("{hex:-   0x03}", "{\"hex\":-3}", "{\"hex\":-3}", STWQ, KWQ, ERROR_NUMBERFORMAT_HEXADECIMAL);
        expectError("{bin:0b0101}", "{\"bin\":5}", "{\"bin\":5}", STWQ, KWQ, ERROR_NUMBERFORMAT_BINARY);
        expectError("{bin:+0b0101}", "{\"bin\":5}", "{\"bin\":5}", STWQ, KWQ, ERROR_NUMBERFORMAT_BINARY, ERROR_NUMBERFORMAT_LEADING_PLUS);
        expectError("{bin:-0b1101}", "{\"bin\":-13}", "{\"bin\":-13}", STWQ, KWQ, ERROR_NUMBERFORMAT_BINARY);
        // parse as a string. binary format does not allow 3
        expectError("{bin:+   0b3101}", "{\"bin\":\"+   0b3101\"}", "{\"bin\":\"+   0b3101\"}", STWQ, KWQ, SVWQ);
        expectError("{oct:071}", "{\"oct\":57}", "{\"oct\":57}", STWQ, KWQ, ERROR_NUMBERFORMAT_OCTAL);
        expectError("{oct:-001}", "{\"oct\":-1}", "{\"oct\":-1}", STWQ, KWQ, ERROR_NUMBERFORMAT_OCTAL);
        expectError("{oct:+   0201}", "{\"oct\":129}", "{\"oct\":129}", STWQ, KWQ, ERROR_NUMBERFORMAT_OCTAL, ERROR_NUMBERFORMAT_LEADING_PLUS);
        expectError("{trailingCOmmaInArray: [1,2,]}", "{\"trailingCOmmaInArray\":[1,2]}", "{\r\n  \"trailingCOmmaInArray\":[1, 2]\r\n}", STWQ, KWQ, ERROR_TRAILING_COMMA_IN_ARRAY);
        expectError("{singleQuotes: 'I can use \"double quotes\" here'}", "{\"singleQuotes\":\"I can use \\\"double quotes\\\" here\"}", "{\"singleQuotes\":\"I can use \\\"double quotes\\\" here\"}", STWQ, KWQ, ERROR_STRING_VALUE_WITH_SINGLE_QUOTES, ERROR_STRING_TOKEN_WITH_SINGLE_QUOTES);
        expectError("{oct:001}", "{\"oct\":1}", "{\"oct\":1}", STWQ, KWQ, ERROR_NUMBERFORMAT_OCTAL);
        expectError("{plus:+1}", "{\"plus\":1}", "{\"plus\":1}", STWQ, KWQ, ERROR_NUMBERFORMAT_LEADING_PLUS);
        expectError("{trailingPoint:1.}", "{\"trailingPoint\":1.0}", "{\"trailingPoint\":1.0}", STWQ, KWQ, ERROR_TRAILING_DECIMAL_POINT);
        expectError("{1:^\\.\\%\\S+$}", "{\"1\":\"^\\\\.\\\\%\\\\S+$\"}", "{\"1\":\"^\\\\.\\\\%\\\\S+$\"}", EXPECTED_VALID_ESCAPE_CHARACTER, ERROR_KEY_IS_NUMBER, STWQ, KWQ, ERROR_STRING_VALUE_WITHOUT_QUOTES);
        expectError("{§regex:^\\\\.\\\\%\\\\S+$}", "{\"§regex\":\"^\\\\.\\\\%\\\\S+$\"}", "{\"§regex\":\"^\\\\.\\\\%\\\\S+$\"}", STWQ, KWQ, ERROR_STRING_VALUE_WITHOUT_QUOTES);
        expectError("Unicode pattern testblabla\\\\u003ebl\r\na", "\"Unicode pattern testblabla\\\\u003ebl\\r\\na\"", "\"Unicode pattern testblabla\\\\u003ebl\\r\\na\"", STWQ, SVWQ);
        expectError("\"Unicode pattern testblabla\\u003ebl\\r\\na\"", "\"Unicode pattern testblabla>bl\\r\\na\"", "\"Unicode pattern testblabla>bl\\r\\na\"");
        expectError("Unicode pattern testblabla\u003ebl\r\na", "\"Unicode pattern testblabla>bl\\r\\na\"", "\"Unicode pattern testblabla>bl\\r\\na\"", STWQ, SVWQ);
        expectError(
                "{\"OBoolean\" :     false, \"pChar\" : 19, \"pDouble\" : 0.3, \"oByte\" : 68, \"pFloat\" : 0.4230000078678131, \"oFloat\" : 0.4122999906539917, \"oLong\" : 5435443543, \"pInt\" : 2435253, \"list\" : [1,2,3], \"pBoolean\" : false, \"oChar\" : 16, \"num\" : \"BLUMM\", \"oDouble\" : 0.52, \"oInt\" : 45343253, \"pLong\" : 4355543543, \"string\" : \"affe232\", \"intArray\" : [3,2,1], \"map\" : {\"3\" : {\"oBoolean\" : false, \"pChar\" : 19, \"pDouble\" : 0.3, \"oByte\" : 68, \"pFloat\" : 0.4230000078678131, \"oFloat\" : 0.4122999906539917, \"oLong\" : 5435443543, \"pInt\" : 2435253, \"list\" : [], \"pBoolean\" : false, \"oChar\" : 16, \"num\" : \"BLUMM\", \"oDouble\" : 0.52, \"oInt\" : 45343253, \"pLong\" : 4355543543, \"string\" : \"affe232\", \"intArray\" : [3,2,1], \"map\" : {}, \"objArray\" : [{\"oBoolean\" : true, \"pChar\" : 18, \"pDouble\" : 0.5, \"oByte\" : 36, \"pFloat\" : 0.4000000059604645, \"oFloat\" : 0.4000000059604645, \"oLong\" : 43543, \"pInt\" : 43253, \"list\" : [], \"pBoolean\" : true, \"oChar\" : 18, \"num\" : \"TEST\", \"oDouble\" : 0.5, \"oInt\" : 43253, \"pLong\" : 43543, \"string\" : \"affe\", \"intArray\" : [1,2], \"map\" : {}, \"objArray\" : null, \"pByte\" : 36, \"obj\" : null},{\"oBoolean\" : true, \"pChar\" : 18, \"pDouble\" : 0.5, \"oByte\" : 36, \"pFloat\" : 0.4000000059604645, \"oFloat\" : 0.4000000059604645, \"oLong\" : 43543, \"pInt\" : 43253, \"list\" : [], \"pBoolean\" : true, \"oChar\" : 18, \"num\" : \"TEST\", \"oDouble\" : 0.5, \"oInt\" : 43253, \"pLong\" : 43543, \"string\" : \"affe\", \"intArray\" : [1,2], \"map\" : {}, \"objArray\" : null, \"pByte\" : 36, \"obj\" : null},{\"oBoolean\" : true, \"pChar\" : 18, \"pDouble\" : 0.5, \"oByte\" : 36, \"pFloat\" : 0.4000000059604645, \"oFloat\" : 0.4000000059604645, \"oLong\" : 43543, \"pInt\" : 43253, \"list\" : [], \"pBoolean\" : true, \"oChar\" : 18, \"num\" : \"TEST\", \"oDouble\" : 0.5, \"oInt\" : 43253, \"pLong\" : 43543, \"string\" : \"affe\", \"intArray\" : [1,2], \"map\" : {}, \"objArray\" : null, \"pByte\" : 36, \"obj\" : null}], \"pByte\" : 20, \"obj\" : null}, \"2\" : {\"oBoolean\" : false, \"pChar\" : 19, \"pDouble\" : 0.3, \"oByte\" : 68, \"pFloat\" : 0.4230000078678131, \"oFloat\" : 0.4122999906539917, \"oLong\" : 5435443543, \"pInt\" : 2435253, \"list\" : [], \"pBoolean\" : false, \"oChar\" : 16, \"num\" : \"BLUMM\", \"oDouble\" : 0.52, \"oInt\" : 45343253, \"pLong\" : 4355543543, \"string\" : \"affe232\", \"intArray\" : [3,2,1], \"map\" : {}, \"objArray\" : [{\"oBoolean\" : true, \"pChar\" : 18, \"pDouble\" : 0.5, \"oByte\" : 36, \"pFloat\" : 0.4000000059604645, \"oFloat\" : 0.4000000059604645, \"oLong\" : 43543, \"pInt\" : 43253, \"list\" : [], \"pBoolean\" : true, \"oChar\" : 18, \"num\" : \"TEST\", \"oDouble\" : 0.5, \"oInt\" : 43253, \"pLong\" : 43543, \"string\" : \"affe\", \"intArray\" : [1,2], \"map\" : {}, \"objArray\" : null, \"pByte\" : 36, \"obj\" : null},{\"oBoolean\" : true, \"pChar\" : 18, \"pDouble\" : 0.5, \"oByte\" : 36, \"pFloat\" : 0.4000000059604645, \"oFloat\" : 0.4000000059604645, \"oLong\" : 43543, \"pInt\" : 43253, \"list\" : [], \"pBoolean\" : true, \"oChar\" : 18, \"num\" : \"TEST\", \"oDouble\" : 0.5, \"oInt\" : 43253, \"pLong\" : 43543, \"string\" : \"affe\", \"intArray\" : [1,2], \"map\" : {}, \"objArray\" : null, \"pByte\" : 36, \"obj\" : null},{\"oBoolean\" : true, \"pChar\" : 18, \"pDouble\" : 0.5, \"oByte\" : 36, \"pFloat\" : 0.4000000059604645, \"oFloat\" : 0.4000000059604645, \"oLong\" : 43543, \"pInt\" : 43253, \"list\" : [], \"pBoolean\" : true, \"oChar\" : 18, \"num\" : \"TEST\", \"oDouble\" : 0.5, \"oInt\" : 43253, \"pLong\" : 43543, \"string\" : \"affe\", \"intArray\" : [1,2], \"map\" : {}, \"objArray\" : null, \"pByte\" : 36, \"obj\" : null}], \"pByte\" : 20, \"obj\" : null}, \"5\" : {\"oBoolean\" : false, \"pChar\" : 19, \"pDouble\" : 0.3, \"oByte\" : 68, \"pFloat\" : 0.4230000078678131, \"oFloat\" : 0.4122999906539917, \"oLong\" : 5435443543, \"pInt\" : 2435253, \"list\" : [], \"pBoolean\" : false, \"oChar\" : 16, \"num\" : \"BLUMM\", \"oDouble\" : 0.52, \"oInt\" : 45343253, \"pLong\" : 4355543543, \"string\" : \"affe232\", \"intArray\" : [3,2,1], \"map\" : {}, \"objArray\" : [{\"oBoolean\" : true, \"pChar\" : 18, \"pDouble\" : 0.5, \"oByte\" : 36, \"pFloat\" : 0.4000000059604645, \"oFloat\" : 0.4000000059604645, \"oLong\" : 43543, \"pInt\" : 43253, \"list\" : [], \"pBoolean\" : true, \"oChar\" : 18, \"num\" : \"TEST\", \"oDouble\" : 0.5, \"oInt\" : 43253, \"pLong\" : 43543, \"string\" : \"affe\", \"intArray\" : [1,2], \"map\" : {}, \"objArray\" : null, \"pByte\" : 36, \"obj\" : null},{\"oBoolean\" : true, \"pChar\" : 18, \"pDouble\" : 0.5, \"oByte\" : 36, \"pFloat\" : 0.4000000059604645, \"oFloat\" : 0.4000000059604645, \"oLong\" : 43543, \"pInt\" : 43253, \"list\" : [], \"pBoolean\" : true, \"oChar\" : 18, \"num\" : \"TEST\", \"oDouble\" : 0.5, \"oInt\" : 43253, \"pLong\" : 43543, \"string\" : \"affe\", \"intArray\" : [1,2], \"map\" : {}, \"objArray\" : null, \"pByte\" : 36, \"obj\" : null},{\"oBoolean\" : true, \"pChar\" : 18, \"pDouble\" : 0.5, \"oByte\" : 36, \"pFloat\" : 0.4000000059604645, \"oFloat\" : 0.4000000059604645, \"oLong\" : 43543, \"pInt\" : 43253, \"list\" : [], \"pBoolean\" : true, \"oChar\" : 18, \"num\" : \"TEST\", \"oDouble\" : 0.5, \"oInt\" : 43253, \"pLong\" : 43543, \"string\" : \"affe\", \"intArray\" : [1,2], \"map\" : {}, \"objArray\" : null, \"pByte\" : 36, \"obj\" : null}], \"pByte\" : 20, \"obj\" : null}, \"4\" : {\"oBoolean\" : false, \"pChar\" : 19, \"pDouble\" : 0.3, \"oByte\" : 68, \"pFloat\" : 0.4230000078678131, \"oFloat\" : 0.4122999906539917, \"oLong\" : 5435443543, \"pInt\" : 2435253, \"list\" : [], \"pBoolean\" : false, \"oChar\" : 16, \"num\" : \"BLUMM\", \"oDouble\" : 0.52, \"oInt\" : 45343253, \"pLong\" : 4355543543, \"string\" : \"affe232\", \"intArray\" : [3,2,1], \"map\" : {}, \"objArray\" : [{\"oBoolean\" : true, \"pChar\" : 18, \"pDouble\" : 0.5, \"oByte\" : 36, \"pFloat\" : 0.4000000059604645, \"oFloat\" : 0.4000000059604645, \"oLong\" : 43543, \"pInt\" : 43253, \"list\" : [], \"pBoolean\" : true, \"oChar\" : 18, \"num\" : \"TEST\", \"oDouble\" : 0.5, \"oInt\" : 43253, \"pLong\" : 43543, \"string\" : \"affe\", \"intArray\" : [1,2], \"map\" : {}, \"objArray\" : null, \"pByte\" : 36, \"obj\" : null},{\"oBoolean\" : true, \"pChar\" : 18, \"pDouble\" : 0.5, \"oByte\" : 36, \"pFloat\" : 0.4000000059604645, \"oFloat\" : 0.4000000059604645, \"oLong\" : 43543, \"pInt\" : 43253, \"list\" : [], \"pBoolean\" : true, \"oChar\" : 18, \"num\" : \"TEST\", \"oDouble\" : 0.5, \"oInt\" : 43253, \"pLong\" : 43543, \"string\" : \"affe\", \"intArray\" : [1,2], \"map\" : {}, \"objArray\" : null, \"pByte\" : 36, \"obj\" : null},{\"oBoolean\" : true, \"pChar\" : 18, \"pDouble\" : 0.5, \"oByte\" : 36, \"pFloat\" : 0.4000000059604645, \"oFloat\" : 0.4000000059604645, \"oLong\" : 43543, \"pInt\" : 43253, \"list\" : [], \"pBoolean\" : true, \"oChar\" : 18, \"num\" : \"TEST\", \"oDouble\" : 0.5, \"oInt\" : 43253, \"pLong\" : 43543, \"string\" : \"affe\", \"intArray\" : [1,2], \"map\" : {}, \"objArray\" : null, \"pByte\" : 36, \"obj\" : null}], \"pByte\" : 20, \"obj\" : null}}, \"objArray\" : [{\"oBoolean\" : true, \"pChar\" : 18, \"pDouble\" : 0.5, \"oByte\" : 36, \"pFloat\" : 0.4000000059604645, \"oFloat\" : 0.4000000059604645, \"oLong\" : 43543, \"pInt\" : 43253, \"list\" : [], \"pBoolean\" : true, \"oChar\" : 18, \"num\" : \"TEST\", \"oDouble\" : 0.5, \"oInt\" : 43253, \"pLong\" : 43543, \"string\" : \"affe\", \"intArray\" : [1,2], \"map\" : {}, \"objArray\" : null, \"pByte\" : 36, \"obj\" : null},{\"oBoolean\" : true, \"pChar\" : 18, \"pDouble\" : 0.5, \"oByte\" : 36, \"pFloat\" : 0.4000000059604645, \"oFloat\" : 0.4000000059604645, \"oLong\" : 43543, \"pInt\" : 43253, \"list\" : [], \"pBoolean\" : true, \"oChar\" : 18, \"num\" : \"TEST\", \"oDouble\" : 0.5, \"oInt\" : 43253, \"pLong\" : 43543, \"string\" : \"affe\", \"intArray\" : [1,2], \"map\" : {}, \"objArray\" : null, \"pByte\" : 36, \"obj\" : null},{\"oBoolean\" : true, \"pChar\" : 18, \"pDouble\" : 0.5, \"oByte\" : 36, \"pFloat\" : 0.4000000059604645, \"oFloat\" : 0.4000000059604645, \"oLong\" : 43543, \"pInt\" : 43253, \"list\" : [], \"pBoolean\" : true, \"oChar\" : 18, \"num\" : \"TEST\", \"oDouble\" : 0.5, \"oInt\" : 43253, \"pLong\" : 43543, \"string\" : \"affe\", \"intArray\" : [1,2], \"map\" : {}, \"objArray\" : null, \"pByte\" : 36, \"obj\" : null}], \"pByte\" : 20, \"obj\" : null}",
                "{\"OBoolean\":false,\"pChar\":19,\"pDouble\":0.3,\"oByte\":68,\"pFloat\":0.4230000078678131,\"oFloat\":0.4122999906539917,\"oLong\":5435443543,\"pInt\":2435253,\"list\":[1,2,3],\"pBoolean\":false,\"oChar\":16,\"num\":\"BLUMM\",\"oDouble\":0.52,\"oInt\":45343253,\"pLong\":4355543543,\"string\":\"affe232\",\"intArray\":[3,2,1],\"map\":{\"3\":{\"oBoolean\":false,\"pChar\":19,\"pDouble\":0.3,\"oByte\":68,\"pFloat\":0.4230000078678131,\"oFloat\":0.4122999906539917,\"oLong\":5435443543,\"pInt\":2435253,\"list\":[],\"pBoolean\":false,\"oChar\":16,\"num\":\"BLUMM\",\"oDouble\":0.52,\"oInt\":45343253,\"pLong\":4355543543,\"string\":\"affe232\",\"intArray\":[3,2,1],\"map\":{},\"objArray\":[{\"oBoolean\":true,\"pChar\":18,\"pDouble\":0.5,\"oByte\":36,\"pFloat\":0.4000000059604645,\"oFloat\":0.4000000059604645,\"oLong\":43543,\"pInt\":43253,\"list\":[],\"pBoolean\":true,\"oChar\":18,\"num\":\"TEST\",\"oDouble\":0.5,\"oInt\":43253,\"pLong\":43543,\"string\":\"affe\",\"intArray\":[1,2],\"map\":{},\"objArray\":null,\"pByte\":36,\"obj\":null},{\"oBoolean\":true,\"pChar\":18,\"pDouble\":0.5,\"oByte\":36,\"pFloat\":0.4000000059604645,\"oFloat\":0.4000000059604645,\"oLong\":43543,\"pInt\":43253,\"list\":[],\"pBoolean\":true,\"oChar\":18,\"num\":\"TEST\",\"oDouble\":0.5,\"oInt\":43253,\"pLong\":43543,\"string\":\"affe\",\"intArray\":[1,2],\"map\":{},\"objArray\":null,\"pByte\":36,\"obj\":null},{\"oBoolean\":true,\"pChar\":18,\"pDouble\":0.5,\"oByte\":36,\"pFloat\":0.4000000059604645,\"oFloat\":0.4000000059604645,\"oLong\":43543,\"pInt\":43253,\"list\":[],\"pBoolean\":true,\"oChar\":18,\"num\":\"TEST\",\"oDouble\":0.5,\"oInt\":43253,\"pLong\":43543,\"string\":\"affe\",\"intArray\":[1,2],\"map\":{},\"objArray\":null,\"pByte\":36,\"obj\":null}],\"pByte\":20,\"obj\":null},\"2\":{\"oBoolean\":false,\"pChar\":19,\"pDouble\":0.3,\"oByte\":68,\"pFloat\":0.4230000078678131,\"oFloat\":0.4122999906539917,\"oLong\":5435443543,\"pInt\":2435253,\"list\":[],\"pBoolean\":false,\"oChar\":16,\"num\":\"BLUMM\",\"oDouble\":0.52,\"oInt\":45343253,\"pLong\":4355543543,\"string\":\"affe232\",\"intArray\":[3,2,1],\"map\":{},\"objArray\":[{\"oBoolean\":true,\"pChar\":18,\"pDouble\":0.5,\"oByte\":36,\"pFloat\":0.4000000059604645,\"oFloat\":0.4000000059604645,\"oLong\":43543,\"pInt\":43253,\"list\":[],\"pBoolean\":true,\"oChar\":18,\"num\":\"TEST\",\"oDouble\":0.5,\"oInt\":43253,\"pLong\":43543,\"string\":\"affe\",\"intArray\":[1,2],\"map\":{},\"objArray\":null,\"pByte\":36,\"obj\":null},{\"oBoolean\":true,\"pChar\":18,\"pDouble\":0.5,\"oByte\":36,\"pFloat\":0.4000000059604645,\"oFloat\":0.4000000059604645,\"oLong\":43543,\"pInt\":43253,\"list\":[],\"pBoolean\":true,\"oChar\":18,\"num\":\"TEST\",\"oDouble\":0.5,\"oInt\":43253,\"pLong\":43543,\"string\":\"affe\",\"intArray\":[1,2],\"map\":{},\"objArray\":null,\"pByte\":36,\"obj\":null},{\"oBoolean\":true,\"pChar\":18,\"pDouble\":0.5,\"oByte\":36,\"pFloat\":0.4000000059604645,\"oFloat\":0.4000000059604645,\"oLong\":43543,\"pInt\":43253,\"list\":[],\"pBoolean\":true,\"oChar\":18,\"num\":\"TEST\",\"oDouble\":0.5,\"oInt\":43253,\"pLong\":43543,\"string\":\"affe\",\"intArray\":[1,2],\"map\":{},\"objArray\":null,\"pByte\":36,\"obj\":null}],\"pByte\":20,\"obj\":null},\"5\":{\"oBoolean\":false,\"pChar\":19,\"pDouble\":0.3,\"oByte\":68,\"pFloat\":0.4230000078678131,\"oFloat\":0.4122999906539917,\"oLong\":5435443543,\"pInt\":2435253,\"list\":[],\"pBoolean\":false,\"oChar\":16,\"num\":\"BLUMM\",\"oDouble\":0.52,\"oInt\":45343253,\"pLong\":4355543543,\"string\":\"affe232\",\"intArray\":[3,2,1],\"map\":{},\"objArray\":[{\"oBoolean\":true,\"pChar\":18,\"pDouble\":0.5,\"oByte\":36,\"pFloat\":0.4000000059604645,\"oFloat\":0.4000000059604645,\"oLong\":43543,\"pInt\":43253,\"list\":[],\"pBoolean\":true,\"oChar\":18,\"num\":\"TEST\",\"oDouble\":0.5,\"oInt\":43253,\"pLong\":43543,\"string\":\"affe\",\"intArray\":[1,2],\"map\":{},\"objArray\":null,\"pByte\":36,\"obj\":null},{\"oBoolean\":true,\"pChar\":18,\"pDouble\":0.5,\"oByte\":36,\"pFloat\":0.4000000059604645,\"oFloat\":0.4000000059604645,\"oLong\":43543,\"pInt\":43253,\"list\":[],\"pBoolean\":true,\"oChar\":18,\"num\":\"TEST\",\"oDouble\":0.5,\"oInt\":43253,\"pLong\":43543,\"string\":\"affe\",\"intArray\":[1,2],\"map\":{},\"objArray\":null,\"pByte\":36,\"obj\":null},{\"oBoolean\":true,\"pChar\":18,\"pDouble\":0.5,\"oByte\":36,\"pFloat\":0.4000000059604645,\"oFloat\":0.4000000059604645,\"oLong\":43543,\"pInt\":43253,\"list\":[],\"pBoolean\":true,\"oChar\":18,\"num\":\"TEST\",\"oDouble\":0.5,\"oInt\":43253,\"pLong\":43543,\"string\":\"affe\",\"intArray\":[1,2],\"map\":{},\"objArray\":null,\"pByte\":36,\"obj\":null}],\"pByte\":20,\"obj\":null},\"4\":{\"oBoolean\":false,\"pChar\":19,\"pDouble\":0.3,\"oByte\":68,\"pFloat\":0.4230000078678131,\"oFloat\":0.4122999906539917,\"oLong\":5435443543,\"pInt\":2435253,\"list\":[],\"pBoolean\":false,\"oChar\":16,\"num\":\"BLUMM\",\"oDouble\":0.52,\"oInt\":45343253,\"pLong\":4355543543,\"string\":\"affe232\",\"intArray\":[3,2,1],\"map\":{},\"objArray\":[{\"oBoolean\":true,\"pChar\":18,\"pDouble\":0.5,\"oByte\":36,\"pFloat\":0.4000000059604645,\"oFloat\":0.4000000059604645,\"oLong\":43543,\"pInt\":43253,\"list\":[],\"pBoolean\":true,\"oChar\":18,\"num\":\"TEST\",\"oDouble\":0.5,\"oInt\":43253,\"pLong\":43543,\"string\":\"affe\",\"intArray\":[1,2],\"map\":{},\"objArray\":null,\"pByte\":36,\"obj\":null},{\"oBoolean\":true,\"pChar\":18,\"pDouble\":0.5,\"oByte\":36,\"pFloat\":0.4000000059604645,\"oFloat\":0.4000000059604645,\"oLong\":43543,\"pInt\":43253,\"list\":[],\"pBoolean\":true,\"oChar\":18,\"num\":\"TEST\",\"oDouble\":0.5,\"oInt\":43253,\"pLong\":43543,\"string\":\"affe\",\"intArray\":[1,2],\"map\":{},\"objArray\":null,\"pByte\":36,\"obj\":null},{\"oBoolean\":true,\"pChar\":18,\"pDouble\":0.5,\"oByte\":36,\"pFloat\":0.4000000059604645,\"oFloat\":0.4000000059604645,\"oLong\":43543,\"pInt\":43253,\"list\":[],\"pBoolean\":true,\"oChar\":18,\"num\":\"TEST\",\"oDouble\":0.5,\"oInt\":43253,\"pLong\":43543,\"string\":\"affe\",\"intArray\":[1,2],\"map\":{},\"objArray\":null,\"pByte\":36,\"obj\":null}],\"pByte\":20,\"obj\":null}},\"objArray\":[{\"oBoolean\":true,\"pChar\":18,\"pDouble\":0.5,\"oByte\":36,\"pFloat\":0.4000000059604645,\"oFloat\":0.4000000059604645,\"oLong\":43543,\"pInt\":43253,\"list\":[],\"pBoolean\":true,\"oChar\":18,\"num\":\"TEST\",\"oDouble\":0.5,\"oInt\":43253,\"pLong\":43543,\"string\":\"affe\",\"intArray\":[1,2],\"map\":{},\"objArray\":null,\"pByte\":36,\"obj\":null},{\"oBoolean\":true,\"pChar\":18,\"pDouble\":0.5,\"oByte\":36,\"pFloat\":0.4000000059604645,\"oFloat\":0.4000000059604645,\"oLong\":43543,\"pInt\":43253,\"list\":[],\"pBoolean\":true,\"oChar\":18,\"num\":\"TEST\",\"oDouble\":0.5,\"oInt\":43253,\"pLong\":43543,\"string\":\"affe\",\"intArray\":[1,2],\"map\":{},\"objArray\":null,\"pByte\":36,\"obj\":null},{\"oBoolean\":true,\"pChar\":18,\"pDouble\":0.5,\"oByte\":36,\"pFloat\":0.4000000059604645,\"oFloat\":0.4000000059604645,\"oLong\":43543,\"pInt\":43253,\"list\":[],\"pBoolean\":true,\"oChar\":18,\"num\":\"TEST\",\"oDouble\":0.5,\"oInt\":43253,\"pLong\":43543,\"string\":\"affe\",\"intArray\":[1,2],\"map\":{},\"objArray\":null,\"pByte\":36,\"obj\":null}],\"pByte\":20,\"obj\":null}",
                "{\r\n  \"OBoolean\":false,\r\n  \"pChar\":19,\r\n  \"pDouble\":0.3,\r\n  \"oByte\":68,\r\n  \"pFloat\":0.4230000078678131,\r\n  \"oFloat\":0.4122999906539917,\r\n  \"oLong\":5435443543,\r\n  \"pInt\":2435253,\r\n  \"list\":[1, 2, 3],\r\n  \"pBoolean\":false,\r\n  \"oChar\":16,\r\n  \"num\":\"BLUMM\",\r\n  \"oDouble\":0.52,\r\n  \"oInt\":45343253,\r\n  \"pLong\":4355543543,\r\n  \"string\":\"affe232\",\r\n  \"intArray\":[3, 2, 1],\r\n  \"map\":{\r\n    \"3\":{\r\n      \"oBoolean\":false,\r\n      \"pChar\":19,\r\n      \"pDouble\":0.3,\r\n      \"oByte\":68,\r\n      \"pFloat\":0.4230000078678131,\r\n      \"oFloat\":0.4122999906539917,\r\n      \"oLong\":5435443543,\r\n      \"pInt\":2435253,\r\n      \"list\":[],\r\n      \"pBoolean\":false,\r\n      \"oChar\":16,\r\n      \"num\":\"BLUMM\",\r\n      \"oDouble\":0.52,\r\n      \"oInt\":45343253,\r\n      \"pLong\":4355543543,\r\n      \"string\":\"affe232\",\r\n      \"intArray\":[3, 2, 1],\r\n      \"map\":{},\r\n      \"objArray\":[\r\n        {\r\n          \"oBoolean\":true,\r\n          \"pChar\":18,\r\n          \"pDouble\":0.5,\r\n          \"oByte\":36,\r\n          \"pFloat\":0.4000000059604645,\r\n          \"oFloat\":0.4000000059604645,\r\n          \"oLong\":43543,\r\n          \"pInt\":43253,\r\n          \"list\":[],\r\n          \"pBoolean\":true,\r\n          \"oChar\":18,\r\n          \"num\":\"TEST\",\r\n          \"oDouble\":0.5,\r\n          \"oInt\":43253,\r\n          \"pLong\":43543,\r\n          \"string\":\"affe\",\r\n          \"intArray\":[1, 2],\r\n          \"map\":{},\r\n          \"objArray\":null,\r\n          \"pByte\":36,\r\n          \"obj\":null\r\n        },\r\n        {\r\n          \"oBoolean\":true,\r\n          \"pChar\":18,\r\n          \"pDouble\":0.5,\r\n          \"oByte\":36,\r\n          \"pFloat\":0.4000000059604645,\r\n          \"oFloat\":0.4000000059604645,\r\n          \"oLong\":43543,\r\n          \"pInt\":43253,\r\n          \"list\":[],\r\n          \"pBoolean\":true,\r\n          \"oChar\":18,\r\n          \"num\":\"TEST\",\r\n          \"oDouble\":0.5,\r\n          \"oInt\":43253,\r\n          \"pLong\":43543,\r\n          \"string\":\"affe\",\r\n          \"intArray\":[1, 2],\r\n          \"map\":{},\r\n          \"objArray\":null,\r\n          \"pByte\":36,\r\n          \"obj\":null\r\n        },\r\n        {\r\n          \"oBoolean\":true,\r\n          \"pChar\":18,\r\n          \"pDouble\":0.5,\r\n          \"oByte\":36,\r\n          \"pFloat\":0.4000000059604645,\r\n          \"oFloat\":0.4000000059604645,\r\n          \"oLong\":43543,\r\n          \"pInt\":43253,\r\n          \"list\":[],\r\n          \"pBoolean\":true,\r\n          \"oChar\":18,\r\n          \"num\":\"TEST\",\r\n          \"oDouble\":0.5,\r\n          \"oInt\":43253,\r\n          \"pLong\":43543,\r\n          \"string\":\"affe\",\r\n          \"intArray\":[1, 2],\r\n          \"map\":{},\r\n          \"objArray\":null,\r\n          \"pByte\":36,\r\n          \"obj\":null\r\n        }\r\n      ],\r\n      \"pByte\":20,\r\n      \"obj\":null\r\n    },\r\n    \"2\":{\r\n      \"oBoolean\":false,\r\n      \"pChar\":19,\r\n      \"pDouble\":0.3,\r\n      \"oByte\":68,\r\n      \"pFloat\":0.4230000078678131,\r\n      \"oFloat\":0.4122999906539917,\r\n      \"oLong\":5435443543,\r\n      \"pInt\":2435253,\r\n      \"list\":[],\r\n      \"pBoolean\":false,\r\n      \"oChar\":16,\r\n      \"num\":\"BLUMM\",\r\n      \"oDouble\":0.52,\r\n      \"oInt\":45343253,\r\n      \"pLong\":4355543543,\r\n      \"string\":\"affe232\",\r\n      \"intArray\":[3, 2, 1],\r\n      \"map\":{},\r\n      \"objArray\":[\r\n        {\r\n          \"oBoolean\":true,\r\n          \"pChar\":18,\r\n          \"pDouble\":0.5,\r\n          \"oByte\":36,\r\n          \"pFloat\":0.4000000059604645,\r\n          \"oFloat\":0.4000000059604645,\r\n          \"oLong\":43543,\r\n          \"pInt\":43253,\r\n          \"list\":[],\r\n          \"pBoolean\":true,\r\n          \"oChar\":18,\r\n          \"num\":\"TEST\",\r\n          \"oDouble\":0.5,\r\n          \"oInt\":43253,\r\n          \"pLong\":43543,\r\n          \"string\":\"affe\",\r\n          \"intArray\":[1, 2],\r\n          \"map\":{},\r\n          \"objArray\":null,\r\n          \"pByte\":36,\r\n          \"obj\":null\r\n        },\r\n        {\r\n          \"oBoolean\":true,\r\n          \"pChar\":18,\r\n          \"pDouble\":0.5,\r\n          \"oByte\":36,\r\n          \"pFloat\":0.4000000059604645,\r\n          \"oFloat\":0.4000000059604645,\r\n          \"oLong\":43543,\r\n          \"pInt\":43253,\r\n          \"list\":[],\r\n          \"pBoolean\":true,\r\n          \"oChar\":18,\r\n          \"num\":\"TEST\",\r\n          \"oDouble\":0.5,\r\n          \"oInt\":43253,\r\n          \"pLong\":43543,\r\n          \"string\":\"affe\",\r\n          \"intArray\":[1, 2],\r\n          \"map\":{},\r\n          \"objArray\":null,\r\n          \"pByte\":36,\r\n          \"obj\":null\r\n        },\r\n        {\r\n          \"oBoolean\":true,\r\n          \"pChar\":18,\r\n          \"pDouble\":0.5,\r\n          \"oByte\":36,\r\n          \"pFloat\":0.4000000059604645,\r\n          \"oFloat\":0.4000000059604645,\r\n          \"oLong\":43543,\r\n          \"pInt\":43253,\r\n          \"list\":[],\r\n          \"pBoolean\":true,\r\n          \"oChar\":18,\r\n          \"num\":\"TEST\",\r\n          \"oDouble\":0.5,\r\n          \"oInt\":43253,\r\n          \"pLong\":43543,\r\n          \"string\":\"affe\",\r\n          \"intArray\":[1, 2],\r\n          \"map\":{},\r\n          \"objArray\":null,\r\n          \"pByte\":36,\r\n          \"obj\":null\r\n        }\r\n      ],\r\n      \"pByte\":20,\r\n      \"obj\":null\r\n    },\r\n    \"5\":{\r\n      \"oBoolean\":false,\r\n      \"pChar\":19,\r\n      \"pDouble\":0.3,\r\n      \"oByte\":68,\r\n      \"pFloat\":0.4230000078678131,\r\n      \"oFloat\":0.4122999906539917,\r\n      \"oLong\":5435443543,\r\n      \"pInt\":2435253,\r\n      \"list\":[],\r\n      \"pBoolean\":false,\r\n      \"oChar\":16,\r\n      \"num\":\"BLUMM\",\r\n      \"oDouble\":0.52,\r\n      \"oInt\":45343253,\r\n      \"pLong\":4355543543,\r\n      \"string\":\"affe232\",\r\n      \"intArray\":[3, 2, 1],\r\n      \"map\":{},\r\n      \"objArray\":[\r\n        {\r\n          \"oBoolean\":true,\r\n          \"pChar\":18,\r\n          \"pDouble\":0.5,\r\n          \"oByte\":36,\r\n          \"pFloat\":0.4000000059604645,\r\n          \"oFloat\":0.4000000059604645,\r\n          \"oLong\":43543,\r\n          \"pInt\":43253,\r\n          \"list\":[],\r\n          \"pBoolean\":true,\r\n          \"oChar\":18,\r\n          \"num\":\"TEST\",\r\n          \"oDouble\":0.5,\r\n          \"oInt\":43253,\r\n          \"pLong\":43543,\r\n          \"string\":\"affe\",\r\n          \"intArray\":[1, 2],\r\n          \"map\":{},\r\n          \"objArray\":null,\r\n          \"pByte\":36,\r\n          \"obj\":null\r\n        },\r\n        {\r\n          \"oBoolean\":true,\r\n          \"pChar\":18,\r\n          \"pDouble\":0.5,\r\n          \"oByte\":36,\r\n          \"pFloat\":0.4000000059604645,\r\n          \"oFloat\":0.4000000059604645,\r\n          \"oLong\":43543,\r\n          \"pInt\":43253,\r\n          \"list\":[],\r\n          \"pBoolean\":true,\r\n          \"oChar\":18,\r\n          \"num\":\"TEST\",\r\n          \"oDouble\":0.5,\r\n          \"oInt\":43253,\r\n          \"pLong\":43543,\r\n          \"string\":\"affe\",\r\n          \"intArray\":[1, 2],\r\n          \"map\":{},\r\n          \"objArray\":null,\r\n          \"pByte\":36,\r\n          \"obj\":null\r\n        },\r\n        {\r\n          \"oBoolean\":true,\r\n          \"pChar\":18,\r\n          \"pDouble\":0.5,\r\n          \"oByte\":36,\r\n          \"pFloat\":0.4000000059604645,\r\n          \"oFloat\":0.4000000059604645,\r\n          \"oLong\":43543,\r\n          \"pInt\":43253,\r\n          \"list\":[],\r\n          \"pBoolean\":true,\r\n          \"oChar\":18,\r\n          \"num\":\"TEST\",\r\n          \"oDouble\":0.5,\r\n          \"oInt\":43253,\r\n          \"pLong\":43543,\r\n          \"string\":\"affe\",\r\n          \"intArray\":[1, 2],\r\n          \"map\":{},\r\n          \"objArray\":null,\r\n          \"pByte\":36,\r\n          \"obj\":null\r\n        }\r\n      ],\r\n      \"pByte\":20,\r\n      \"obj\":null\r\n    },\r\n    \"4\":{\r\n      \"oBoolean\":false,\r\n      \"pChar\":19,\r\n      \"pDouble\":0.3,\r\n      \"oByte\":68,\r\n      \"pFloat\":0.4230000078678131,\r\n      \"oFloat\":0.4122999906539917,\r\n      \"oLong\":5435443543,\r\n      \"pInt\":2435253,\r\n      \"list\":[],\r\n      \"pBoolean\":false,\r\n      \"oChar\":16,\r\n      \"num\":\"BLUMM\",\r\n      \"oDouble\":0.52,\r\n      \"oInt\":45343253,\r\n      \"pLong\":4355543543,\r\n      \"string\":\"affe232\",\r\n      \"intArray\":[3, 2, 1],\r\n      \"map\":{},\r\n      \"objArray\":[\r\n        {\r\n          \"oBoolean\":true,\r\n          \"pChar\":18,\r\n          \"pDouble\":0.5,\r\n          \"oByte\":36,\r\n          \"pFloat\":0.4000000059604645,\r\n          \"oFloat\":0.4000000059604645,\r\n          \"oLong\":43543,\r\n          \"pInt\":43253,\r\n          \"list\":[],\r\n          \"pBoolean\":true,\r\n          \"oChar\":18,\r\n          \"num\":\"TEST\",\r\n          \"oDouble\":0.5,\r\n          \"oInt\":43253,\r\n          \"pLong\":43543,\r\n          \"string\":\"affe\",\r\n          \"intArray\":[1, 2],\r\n          \"map\":{},\r\n          \"objArray\":null,\r\n          \"pByte\":36,\r\n          \"obj\":null\r\n        },\r\n        {\r\n          \"oBoolean\":true,\r\n          \"pChar\":18,\r\n          \"pDouble\":0.5,\r\n          \"oByte\":36,\r\n          \"pFloat\":0.4000000059604645,\r\n          \"oFloat\":0.4000000059604645,\r\n          \"oLong\":43543,\r\n          \"pInt\":43253,\r\n          \"list\":[],\r\n          \"pBoolean\":true,\r\n          \"oChar\":18,\r\n          \"num\":\"TEST\",\r\n          \"oDouble\":0.5,\r\n          \"oInt\":43253,\r\n          \"pLong\":43543,\r\n          \"string\":\"affe\",\r\n          \"intArray\":[1, 2],\r\n          \"map\":{},\r\n          \"objArray\":null,\r\n          \"pByte\":36,\r\n          \"obj\":null\r\n        },\r\n        {\r\n          \"oBoolean\":true,\r\n          \"pChar\":18,\r\n          \"pDouble\":0.5,\r\n          \"oByte\":36,\r\n          \"pFloat\":0.4000000059604645,\r\n          \"oFloat\":0.4000000059604645,\r\n          \"oLong\":43543,\r\n          \"pInt\":43253,\r\n          \"list\":[],\r\n          \"pBoolean\":true,\r\n          \"oChar\":18,\r\n          \"num\":\"TEST\",\r\n          \"oDouble\":0.5,\r\n          \"oInt\":43253,\r\n          \"pLong\":43543,\r\n          \"string\":\"affe\",\r\n          \"intArray\":[1, 2],\r\n          \"map\":{},\r\n          \"objArray\":null,\r\n          \"pByte\":36,\r\n          \"obj\":null\r\n        }\r\n      ],\r\n      \"pByte\":20,\r\n      \"obj\":null\r\n    }\r\n  },\r\n  \"objArray\":[\r\n    {\r\n      \"oBoolean\":true,\r\n      \"pChar\":18,\r\n      \"pDouble\":0.5,\r\n      \"oByte\":36,\r\n      \"pFloat\":0.4000000059604645,\r\n      \"oFloat\":0.4000000059604645,\r\n      \"oLong\":43543,\r\n      \"pInt\":43253,\r\n      \"list\":[],\r\n      \"pBoolean\":true,\r\n      \"oChar\":18,\r\n      \"num\":\"TEST\",\r\n      \"oDouble\":0.5,\r\n      \"oInt\":43253,\r\n      \"pLong\":43543,\r\n      \"string\":\"affe\",\r\n      \"intArray\":[1, 2],\r\n      \"map\":{},\r\n      \"objArray\":null,\r\n      \"pByte\":36,\r\n      \"obj\":null\r\n    },\r\n    {\r\n      \"oBoolean\":true,\r\n      \"pChar\":18,\r\n      \"pDouble\":0.5,\r\n      \"oByte\":36,\r\n      \"pFloat\":0.4000000059604645,\r\n      \"oFloat\":0.4000000059604645,\r\n      \"oLong\":43543,\r\n      \"pInt\":43253,\r\n      \"list\":[],\r\n      \"pBoolean\":true,\r\n      \"oChar\":18,\r\n      \"num\":\"TEST\",\r\n      \"oDouble\":0.5,\r\n      \"oInt\":43253,\r\n      \"pLong\":43543,\r\n      \"string\":\"affe\",\r\n      \"intArray\":[1, 2],\r\n      \"map\":{},\r\n      \"objArray\":null,\r\n      \"pByte\":36,\r\n      \"obj\":null\r\n    },\r\n    {\r\n      \"oBoolean\":true,\r\n      \"pChar\":18,\r\n      \"pDouble\":0.5,\r\n      \"oByte\":36,\r\n      \"pFloat\":0.4000000059604645,\r\n      \"oFloat\":0.4000000059604645,\r\n      \"oLong\":43543,\r\n      \"pInt\":43253,\r\n      \"list\":[],\r\n      \"pBoolean\":true,\r\n      \"oChar\":18,\r\n      \"num\":\"TEST\",\r\n      \"oDouble\":0.5,\r\n      \"oInt\":43253,\r\n      \"pLong\":43543,\r\n      \"string\":\"affe\",\r\n      \"intArray\":[1, 2],\r\n      \"map\":{},\r\n      \"objArray\":null,\r\n      \"pByte\":36,\r\n      \"obj\":null\r\n    }\r\n  ],\r\n  \"pByte\":20,\r\n  \"obj\":null\r\n}");
        expectError("-    23.432e-4", "-0.0023432", "-0.0023432");
        expectError("+    23.432e-4", "0.0023432", "0.0023432", ERROR_NUMBERFORMAT_LEADING_PLUS);
        expectError("23.432e-4", "0.0023432", "0.0023432");
        expectError("[23.432e-4]", "[0.0023432]", "[0.0023432]");
        expectError("{}", "{}", "{}");
        expectError("{key:\"inner \\\" Object {} test\"}", "{\"key\":\"inner \\\" Object {} test\"}", "{\"key\":\"inner \\\" Object {} test\"}", STWQ, KWQ);
        expectError("{key:with\r\nunescapednewline}", "{\"key\":\"with\\r\\nunescapednewline\"}", "{\"key\":\"with\\r\\nunescapednewline\"}", STWQ, KWQ, SVWQ);
        expectError("{key:with\\r\\nescapednewline}", "{\"key\":\"with\\\\r\\\\nescapednewline\"}", "{\"key\":\"with\\\\r\\\\nescapednewline\"}", STWQ, KWQ, SVWQ, EXPECTED_VALID_ESCAPE_CHARACTER);
        expectError("{key:with\"quotes}", "{\"key\":\"with\\\"quotes\"}", "{\"key\":\"with\\\"quotes\"}", STWQ, KWQ, SVWQ);
        expectError("{key:spaces in pseudo String,key2:newvalue}", "{\"key\":\"spaces in pseudo String\",\"key2\":\"newvalue\"}", "{\r\n  \"key\":\"spaces in pseudo String\",\r\n  \"key2\":\"newvalue\"\r\n}", STWQ, KWQ, SVWQ);
        expectError("{key:\"value\"}", "{\"key\":\"value\"}", "{\"key\":\"value\"}", STWQ, KWQ);
        expectError("{key:value}", "{\"key\":\"value\"}", "{\"key\":\"value\"}", STWQ, KWQ, SVWQ);
        expectError("{key:true}", "{\"key\":true}", "{\"key\":true}", STWQ, KWQ);
        expectError("[true,false,null,1,affenbaum]", "[true,false,null,1,\"affenbaum\"]", "[true, false, null, 1, \"affenbaum\"]", STWQ, SVWQ);
        expectError("[true,false,{},1,affenbaum]", "[true,false,{},1,\"affenbaum\"]", "[\r\n  true,\r\n  false,\r\n  {},\r\n  1,\r\n  \"affenbaum\"\r\n]", STWQ, SVWQ);
        expectError("[true,false,{},-  1,affenbaum]", "[true,false,{},-1,\"affenbaum\"]", "[\r\n  true,\r\n  false,\r\n  {},\r\n  -1,\r\n  \"affenbaum\"\r\n]", STWQ, SVWQ);
        expectError("[true,false,{},23.432e-4,affenbaum]", "[true,false,{},0.0023432,\"affenbaum\"]", "[\r\n  true,\r\n  false,\r\n  {},\r\n  0.0023432,\r\n  \"affenbaum\"\r\n]", STWQ, SVWQ);
        expectError("[true,false,{},[-    23.432e-4],affenbaum]", "[true,false,{},[-0.0023432],\"affenbaum\"]", "[\r\n  true,\r\n  false,\r\n  {},\r\n  [-0.0023432],\r\n  \"affenbaum\"\r\n]", STWQ, SVWQ);
        expectError("{1:true}", "{\"1\":true}", "{\"1\":true}", KWQ, ERROR_KEY_IS_NUMBER);
        expectError("{true:true}", "{\"true\":true}", "{\"true\":true}", KWQ, ERROR_KEY_IS_BOOLEAN);
        expectError("342.187124E-3", "0.342187124", "0.342187124");
        expectError("value", "\"value\"", "\"value\"", ERROR_STRING_TOKEN_WITHOUT_QUOTES, ERROR_STRING_VALUE_WITHOUT_QUOTES);
        expectError("true", "true", "true");
        expectError("false", "false", "false");
        expectError("null", "null", "null");
        expectError("\"true\"", "\"true\"", "\"true\"");
        expectError("12", "12", "12");
        expectError("[\r", "[]", "[]", EXPECTED_ARRAY_VALUE_COMMA_OR_ARRAY_CLOSE_TAG);
        expectError("{value : 2", "{\"value\":2}", "{\"value\":2}", ERROR_STRING_TOKEN_WITHOUT_QUOTES, ERROR_KEY_WITHOUT_QUOTES, EXPECTED_COMMA_OR_OBJECT_END_TAG);
        expectError("[\r\n {\r\n  \"map\"              : {\r\n                        \"2\" : {\r\n                               \"map\"              : {},\r\n                               \"string\"           : \"affe232\",\r\n                               \"genericLongClass\" : {\r\n                                                     \"key\"   : 1,\r\n                                                     \"value\" : 2", "[{\"map\":{\"2\":{\"map\":{},\"string\":\"affe232\",\"genericLongClass\":{\"key\":1,\"value\":2}}}}]", "[{\r\n  \"map\":{\r\n    \"2\":{\r\n      \"map\":{},\r\n      \"string\":\"affe232\",\r\n      \"genericLongClass\":{\r\n        \"key\":1,\r\n        \"value\":2\r\n      }\r\n    }\r\n  }\r\n}]", EXPECTED_COMMA_OR_OBJECT_END_TAG, EXPECTED_COMMA_OR_ARRAY_END_TAG);
        expectError("[\r\n {\r\n  \"map\"              : {\r\n                        \"2\" : {\r\n                               \"map\"              : {},\r\n                               \"string\"           : \"affe232\"", "[{\"map\":{\"2\":{\"map\":{},\"string\":\"affe232\"}}}]", "[{\r\n  \"map\":{\r\n    \"2\":{\r\n      \"map\":{},\r\n      \"string\":\"affe232\"\r\n    }\r\n  }\r\n}]", EXPECTED_COMMA_OR_OBJECT_END_TAG, EXPECTED_COMMA_OR_ARRAY_END_TAG);
        expectError("[   ", "[]", "[]", EXPECTED_ARRAY_VALUE_COMMA_OR_ARRAY_CLOSE_TAG);
        expectError("[[1],[2]", "[[1],[2]]", "[\r\n  [1],\r\n  [2]\r\n]", EXPECTED_COMMA_OR_ARRAY_END_TAG);
        expectError("'1''2'", "[\"1\",\"2\"]", "[\"1\", \"2\"]", ERROR_STRING_VALUE_WITH_SINGLE_QUOTES, ERROR_MULTIPLE_ELEMENTS_IN_JSON_STRING, ERROR_UNEXPECTED_CONTENT_AFTER_END_OF_JSON, ERROR_STRING_TOKEN_WITH_SINGLE_QUOTES);
        expectError("[1][2]", "[[1],[2]]", "[\r\n  [1],\r\n  [2]\r\n]", ERROR_MULTIPLE_ELEMENTS_IN_JSON_STRING, ERROR_UNEXPECTED_CONTENT_AFTER_END_OF_JSON);
        expectError("{a:1}{b:1}", "[{\"a\":1},{\"b\":1}]", "[\r\n  {\"a\":1},\r\n  {\"b\":1}\r\n]", ERROR_STRING_TOKEN_WITHOUT_QUOTES, ERROR_MULTIPLE_ELEMENTS_IN_JSON_STRING, ERROR_UNEXPECTED_CONTENT_AFTER_END_OF_JSON, ERROR_KEY_WITHOUT_QUOTES, ERROR_MULTIPLE_ELEMENTS_IN_JSON_STRING, ERROR_UNEXPECTED_CONTENT_AFTER_END_OF_JSON);
        expectError("{in1:{in2:{}}", "{\"in1\":{\"in2\":{}}}", "{\r\n  \"in1\":{\r\n    \"in2\":{}\r\n  }\r\n}", ERROR_STRING_TOKEN_WITHOUT_QUOTES, EXPECTED_COMMA_OR_OBJECT_END_TAG, ERROR_KEY_WITHOUT_QUOTES);
        // ERROR_STRING_VALUE_WITHOUT_QUOTES is found due to missing close tags and missinterpreting the following chars
        expectError("{in1:{in2:{ar1:[[]}}", "{\"in1\":{\"in2\":{\"ar1\":[[],\"}}\"]}}}", "{\r\n  \"in1\":{\r\n    \"in2\":{\r\n      \"ar1\":[\r\n        [],\r\n        \"}}\"\r\n      ]\r\n    }\r\n  }\r\n}", ERROR_STRING_VALUE_WITHOUT_QUOTES, EXPECTED_COMMA_OR_ARRAY_END_TAG, ERROR_STRING_TOKEN_WITHOUT_QUOTES, EXPECTED_COMMA_OR_OBJECT_END_TAG, ERROR_KEY_WITHOUT_QUOTES);
        // illegal escape sequences
        expectError("'Unicode pattern testblabla\\u1 anythign else", "\"Unicode pattern testblabla\\u0001anythign else\"", "\"Unicode pattern testblabla\\u0001anythign else\"", EXPECTED_UNICODE_SEQUENCE, ERROR_STRING_VALUE_WITH_SINGLE_QUOTES, EXPECTED_STRING_OR_SINGLE_QUOTE, ERROR_STRING_TOKEN_WITH_SINGLE_QUOTES);
        expectError("'this is an error", "\"this is an error\"", "\"this is an error\"", ERROR_STRING_VALUE_WITH_SINGLE_QUOTES, EXPECTED_STRING_OR_SINGLE_QUOTE, ERROR_STRING_TOKEN_WITH_SINGLE_QUOTES);
        expectError("[", "[]", "[]", EXPECTED_ARRAY_VALUE_COMMA_OR_ARRAY_CLOSE_TAG);
        expectError("[,1,\"2\" \"2\"]", "[undefined,1,\"2\",\"2\"]", "[undefined, 1, \"2\", \"2\"]", ERROR_LEADING_COMMA_IN_ARRAY, EXPECTED_COMMA_OR_ARRAY_END_TAG);
        expectError("[,1,\"2\" 2]", "[undefined,1,\"2\",2]", "[undefined, 1, \"2\", 2]", ERROR_LEADING_COMMA_IN_ARRAY, EXPECTED_COMMA_OR_ARRAY_END_TAG);
        expectError("[,1,2,", "[undefined,1,2]", "[undefined, 1, 2]", ERROR_LEADING_COMMA_IN_ARRAY, EXPECTED_ARRAY_VALUE_COMMA_OR_ARRAY_CLOSE_TAG);
        expectError("{", "{}", "{}", EXPECTED_KEY_VALUE_PAIR_COMMA_OR_OBJECT_END_TAG);
        expectError("{key:", "{\"key\":undefined}", "{\"key\":undefined}", ERROR_STRING_TOKEN_WITHOUT_QUOTES, EXPECTED_COMMA_OR_OBJECT_END_TAG, ERROR_KEY_WITHOUT_QUOTES, EXPECTED_ANY_VALUE);
        expectError("{key:1", "{\"key\":1}", "{\"key\":1}", ERROR_STRING_TOKEN_WITHOUT_QUOTES, EXPECTED_COMMA_OR_OBJECT_END_TAG, ERROR_KEY_WITHOUT_QUOTES);
        expectError("{key}", "{\"key\":undefined}", "{\"key\":undefined}", ERROR_STRING_TOKEN_WITHOUT_QUOTES, EXPECTED_COLON, ERROR_VALUE_INSTEAD_OF_UNDEFINED_EXPECTED, ERROR_KEY_WITHOUT_QUOTES);
        LogV3.info("SUCCESS: Run parsing, serializer and pretty printer Tests");
    }

    public static void main(String[] args) throws FlexiParserException, UnsupportedEncodingException, InterruptedException {
        IDETestRunner.run(ParserTest.class);
        // runWayToExtensiveWhateverTest();
        LogV3.disableSysout();
    }

    /**
     * @param expectedPrettyPrint
     *            TODO
     * @param string
     * @param string2
     * @throws Exception
     * @throws UnsupportedEncodingException
     */
    private static void expectError(String json, String expectedStringify, String expectedPrettyPrint, final ParsingError... errors) throws Exception {
        HashSet<ParsingError> allowSet;
        {
            // check if the expected values are correct JSON format
            FlexiJSONParser parser = new FlexiJSONParser(expectedStringify).enableComments().enableExtraCommas().enableUndefinedValues().enableNaNValues().enableInfinityValues();
            FlexiJSonNode node = parser.parse();
            allowSet = parser.getIgnoreIssues();
            String checkit = new FlexiJSonStringBuilder().toJSONString(node);
            assertEquals(checkit, expectedStringify);
        }
        {
            // check if the expected values are correct JSON format
            FlexiJSonNode node = new FlexiJSONParser(expectedPrettyPrint).setIgnoreIssues(allowSet).parse();
            String checkit = new FlexiJSonPrettyStringify().toJSONString(node);
            assertEquals(checkit, expectedPrettyPrint);
        }
        FlexiJSonNode parsed = null;
        final HashSet<ParsingError> expectedErrors = new HashSet<ParsingError>(Arrays.asList(errors));
        final HashMap<ParsingError, Throwable> foundErrors = new HashMap<ParsingError, Throwable>();
        try {
            parsed = new FlexiJSONParser(new StringReader(json)) {
                public boolean isParseInlineCommentEnabled() {
                    return Arrays.asList(errors).contains(ERROR_INLINE_COMMENT);
                };

                public boolean isParseLineCommentEnabled() {
                    return Arrays.asList(errors).contains(ERROR_LINE_COMMENT);
                };

                protected void throwParserException(ParsingError string, Object path, Throwable cause, FlexiJSonNode parent, FlexiJSonNode value) throws FlexiParserException {
                    if (!expectedErrors.contains(string)) {
                        System.out.println("Unexpected Exception: " + string);
                    }
                    foundErrors.put(string, cause == null ? new Exception() : cause);
                };
            }.setDebug(new StringBuilder()).parse();
            LogV3.info("Test on " + JSonStorage.serializeToJson(json));
            for (ParsingError s : foundErrors.keySet()) {
                if (!expectedErrors.contains(s)) {
                    throw new Exception(json + " Unexpected Error: " + JSonStorage.serializeToJson(s), foundErrors.get(s));
                }
            }
            for (ParsingError s : expectedErrors) {
                if (!foundErrors.containsKey(s)) {
                    throw new Exception(json + " Expected Error did not happen: " + JSonStorage.serializeToJson(s));
                }
            }
            FlexiJSonStringBuilder serializer = new FlexiJSonStringBuilder();
            final String shortV = serializer.toJSONString(parsed);
            final String pretty = new FlexiJSonPrettyStringify().toJSONString(parsed);
            String property = new PropertyJSonPrettyStringify().toJSONString(parsed);
            boolean contentCorrect = false;
            FlexiJSonNode reparsedStringify = new FlexiJSONParser(pretty) {
                public boolean isParseInlineCommentEnabled() {
                    return expectedErrors.contains(ERROR_INLINE_COMMENT);
                };

                public boolean isParseLineCommentEnabled() {
                    return expectedErrors.contains(ERROR_LINE_COMMENT);
                };

                protected void throwParserException(ParsingError error, Object path, Throwable cause, FlexiJSonNode parent, FlexiJSonNode value) throws FlexiParserException {
                    if (expectedErrors.contains(error)) {
                        return;
                    }
                    LogV3.info(error + "");
                    // super.throwParserException(error, path, cause, parent, value);
                };
            }.setDebug(new StringBuilder()).parse();
            FlexiJSonNode reparsedPretty = new FlexiJSONParser(shortV) {
                public boolean isParseInlineCommentEnabled() {
                    return expectedErrors.contains(ERROR_INLINE_COMMENT);
                };

                public boolean isParseLineCommentEnabled() {
                    return expectedErrors.contains(ERROR_LINE_COMMENT);
                };

                protected void throwParserException(ParsingError error, Object path, Throwable cause, FlexiJSonNode parent, FlexiJSonNode value) throws FlexiParserException {
                    if (expectedErrors.contains(error)) {
                        return;
                    }
                    LogV3.info(error + "");
                    // super.throwParserException(error, path, cause, parent, value);
                };
            }.setDebug(new StringBuilder()).parse();
            String a = new FlexiJSonStringBuilder().toJSONString(reparsedStringify);
            String b = new FlexiJSonStringBuilder().toJSONString(reparsedPretty);
            contentCorrect = a.equals(b);
            // LogV3.info(new PropertyJSonPrettyStringify().toJSONString(parsed));
            if (contentCorrect && expectedStringify.equals(shortV) && pretty.equals(expectedPrettyPrint)) {
                LogV3.info("Pretty:\r\n" + pretty);
                LogV3.info("Stringify: " + shortV);
                // LogV3.info(shortV);
                // LogV3.info(new PropertyJSonPrettyStringify().toJSONString(parsed));
                LogV3.info((expectedStringify.equals(shortV) ? "SUCCESS" : "FAIL") + " Stringify Result: " + JSonStorage.serializeToJson(serializer.toJSONString(parsed)));
                LogV3.info(((expectedPrettyPrint != null && pretty.equals(expectedPrettyPrint)) ? "SUCCESS" : "FAIL") + " PrettyPrint Result: " + JSonStorage.serializeToJson(pretty));
                if (!expectedStringify.equals(shortV)) {
                    throw new Exception("Test Failed");
                }
                if (!(expectedPrettyPrint != null && pretty.equals(expectedPrettyPrint))) {
                    throw new Exception("Test Failed");
                }
            } else {
                LogV3.info("Pretty:\r\n" + pretty + "\r\n" + pretty.replace("\r\n", "\\r\\n").replace("\"", "\\\""));
                LogV3.info("Stringify: " + shortV + "\r\n" + shortV.replace("\r\n", "\\r\\n").replace("\"", "\\\""));
                LogV3.info("Content Compare: " + (contentCorrect ? "SUCCESS" : ("FAIL \r\n" + a + " \r\n" + b)));
                LogV3.info((expectedStringify.equals(shortV) ? "SUCCESS" : "FAIL") + " Stringify Result: \r\n" + JSonStorage.serializeToJson(serializer.toJSONString(parsed)) + " - Expected: \r\n" + JSonStorage.serializeToJson(expectedStringify));
                // LogV3.info(expectedPrettyPrint);
                LogV3.info(((expectedPrettyPrint != null && pretty.equals(expectedPrettyPrint)) ? "SUCCESS" : "FAIL") + " PrettyPrint Result: \r\n" + JSonStorage.serializeToJson(pretty) + " - Expected: \r\n" + JSonStorage.serializeToJson(expectedPrettyPrint));
                if (!expectedStringify.equals(shortV)) {
                    throw new Exception("Test Failed");
                }
                if (!(expectedPrettyPrint != null && pretty.equals(expectedPrettyPrint))) {
                    throw new Exception("Test Failed");
                }
            }
        } catch (FlexiParserException e) {
            throw new Exception("FAILED no exception should happen here", e);
        }
        if (expectedErrors.size() > 0) {
            try {
                new FlexiJSONParser(new StringReader(json)).setDebug(new StringBuilder()).parse();
                throw new WTFException("Exception expected");
            } catch (FlexiParserException e) {
                // if (expect.equals(e.toString())) {
                // LogV3.info("SUCCESS " + expect);
                // } else {
                // LogV3.info(e.toString());
                // LogV3.info("FAILED " + expect + " vs. " + e.toString());
                // }
            }
        }
        clearLoggerCache();
    }

    protected static void runWayToExtensiveWhateverTest() throws InterruptedException {
        LogV3.info("Run parsing invalid/incomplete JSON Tests");
        final String longJson = JSonStorage.serializeToJson(org.appwork.storage.simplejson.mapper.test.TestClass.createList());
        final ThreadPoolExecutor queue = new ThreadPoolExecutor(8, 8, 10 * 1000l, TimeUnit.MILLISECONDS, new ArrayBlockingQueue<Runnable>(10000));
        final CharBuffer longJsonCharBuffer = CharBuffer.wrap(longJson);
        for (int i = 1; i <= longJson.length(); i++) {
            final int finalI = i;
            Runnable r = new Runnable() {
                @Override
                public void run() {
                    final AtomicBoolean exception = new AtomicBoolean();
                    try {
                        final CharSequence sub = longJsonCharBuffer.subSequence(0, finalI);
                        if (finalI % 1000 == 0) {
                            LogV3.info(finalI + "/" + longJson.length() + "|" + queue.getQueue().size());
                        }
                        final HashSet<ParsingError> exceptions = new HashSet<ParsingError>();
                        FlexiJSonNode parsed = new FlexiJSONParser(new CharSequenceReader(sub)) {
                            protected void throwParserException(ParsingError expected, Object path, Throwable e, FlexiJSonNode parent, FlexiJSonNode value) throws FlexiParserException {
                                exceptions.add(expected);
                                exception.set(true);
                                throw new WTFException();
                            };
                        }.setDebug(new StringBuilder()).parse();
                        // / LogV3.info(parsed);
                        if (exceptions.size() == 0 && finalI != longJson.length()) {
                            LogV3.info(JSonStorage.serializeToJson(sub));
                            LogV3.info("ERROR: Exceptions expected");
                            System.exit(1);
                        }
                    } catch (WTFException e) {
                        if (!exception.get()) {
                            e.printStackTrace();
                            System.exit(1);
                        }
                    } catch (Exception e) {
                        e.printStackTrace();
                        System.exit(1);
                    }
                }
            };
            while (true) {
                try {
                    queue.submit(r);
                    break;
                } catch (RejectedExecutionException e) {
                    Thread.sleep(10);
                }
            }
        }
        while (true) {
            try {
                if (queue.getQueue().size() > 0) {
                    LogV3.info("Waiting for:" + queue.getQueue().size());
                    Thread.sleep(1000);
                } else {
                    LogV3.info("Finished");
                    Thread.sleep(1000);
                    System.exit(0);
                    break;
                }
            } catch (InterruptedException e) {
            }
        }
        LogV3.info("SUCCESS: Run parsing invalid/incomplete JSON Tests");
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.tests.PostBuildTestInterface#runPostBuildTest(java.lang.String[], java.io.File)
     */
    @Override
    public void runPostBuildTest(String[] args, File workingDirectory) throws Exception {
        runTest();
    }
}
