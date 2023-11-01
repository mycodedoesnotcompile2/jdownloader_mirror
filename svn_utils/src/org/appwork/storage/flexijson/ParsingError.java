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

@SuppressWarnings("unchecked")
public enum ParsingError {
    ERROR_DUPLICATED_KEY_PROPERTY("Duplicated key property"),
    ERROR_INLINE_COMMENT("An inline comment was found!"),
    ERROR_IO_EXCEPTION_IN_STREAM("An IOException occurred while reading the json stream"),
    ERROR_KEY_WITHOUT_QUOTES("A key without proper quotes was found"),
    ERROR_LEADING_COMMA_IN_OBJECT("A leading comma was found in an object"),
    ERROR_LEADING_COMMA_IN_ARRAY("A leading comma was found in an array"),
    ERROR_LEADING_DECIMAL_POINT("Number has a leading decimal point"),
    ERROR_LINE_COMMENT("A line comment was found!"),
    ERROR_MULTIPLE_ELEMENTS_IN_JSON_STRING("Multiple elements in json string"),
    ERROR_UNEXPECTED_CONTENT_AFTER_END_OF_JSON("Unexpected, non-whitespace byte(s) after EndOfJson (End of object/String/Integer/Array/...)"),
    ERROR_MULTIPLE_LEADING_COMMA_IN_OBJECT("Multiple leading commas were found in an object"),
    ERROR_MULTIPLE_LEADING_COMMA_IN_ARRAY("Multiple leading commas were found in an array"),
    ERROR_MULTIPLE_SUBSEQUENT_COMMAS_IN_OBJECT("Multiple subsequent commas were found in an object"),
    ERROR_MULTIPLE_SUBSEQUENT_COMMAS_IN_ARRAY("Multiple subsequent commas were found in an array"),
    ERROR_MULTIPLE_TRAILING_COMMA_IN_OBJECT("An object ends with multiple commas"),
    ERROR_MULTIPLE_TRAILING_COMMA_IN_ARRAY("An array ends with multiple commas"),
    ERROR_NUMBER_INFINITY("Number is Infinity"),
    ERROR_NUMBER_NAN("A number NaN value was found"),
    ERROR_NUMBERFORMAT_BINARY("Number is binary format"),
    ERROR_NUMBERFORMAT_HEXADECIMAL("Number is in hexadecimal format"),
    ERROR_NUMBERFORMAT_LEADING_PLUS("Number has a leading plus character"),
    ERROR_NUMBERFORMAT_OCTAL("Number is octal format"),
    ERROR_STRING_TOKEN_WITH_SINGLE_QUOTES("A string token (key or value) with single quotes was found"),
    ERROR_STRING_TOKEN_WITHOUT_QUOTES("A string token (key or value) without proper surrounding quotes was found"),
    ERROR_STRING_VALUE_WITHOUT_QUOTES("A string value without proper quotes was found"),
    ERROR_TRAILING_COMMA_IN_ARRAY("Trailing comma in array"),
    ERROR_TRAILING_COMMA_IN_OBJECT("Trailing comma in object"),
    ERROR_TRAILING_DECIMAL_POINT("Number has a trailing decimal point"),
    ERROR_VALUE_INSTEAD_OF_UNDEFINED_EXPECTED("Undefined value"),
    EXPECTED_ANY_CHARACTER("Expected: any character"),
    EXPECTED_ANY_VALUE("Expected: Any value"),
    EXPECTED_ARRAY_VALUE_COMMA_OR_ARRAY_CLOSE_TAG("Expected: array value,comma or array close tag"),
    EXPECTED_COLON("Expected: Colon"),
    EXPECTED_COMMA_OR_ARRAY_END_TAG("Expected: Comma or array end tag"),
    EXPECTED_COMMA_OR_OBJECT_END_TAG("Expected: Comma or object end tag"),
    EXPECTED_INLINE_COMMENT_END_TAG("Expected: Inline comment end tag"),
    EXPECTED_KEY_OR_OBJECT_END_TAG("Expected: key or object end tag"),
    EXPECTED_KEY_VALUE_PAIR_COMMA_OR_OBJECT_END_TAG("Expected: Key-Value pair, comma or object end tag"),
    EXPECTED_STRING_OR_DOUBLE_QUOTE("Expected: string or double quote"),
    EXPECTED_STRING_OR_SINGLE_QUOTE("Expected: string or single quote"),
    EXPECTED_UNICODE_SEQUENCE("Expected: unicode sequence: 4 hex characters"),
    EXPECTED_VALID_ESCAPE_CHARACTER("Expected: escaped character"),
    ERROR_STRING_VALUE_WITH_SINGLE_QUOTES("A string value with single quotes was found"),
    ERROR_KEY_IS_UNDEFINED("undefined has been found as a key"),
    ERROR_KEY_IS_NULL("null has been found as a key"),
    ERROR_KEY_IS_BOOLEAN("boolean has been found as a key"),
    // numbers are not allowed as unqoted keys in json5
    ERROR_KEY_IS_NUMBER("number has been found as a key"),
    ERROR_KEY_WITH_SINGLE_QUOTES("string key with single quotes has been found"),
    EXPECTED_ANY_CHARACTER_OR_COMMENT_CLOSE_TAGS("Expected: inline comment close tags or any further comment characters"),
    ERROR_NO_CONTENT("No content"),
    ERROR_NEWLINE_IN_TOKEN("\r or \n has been found in a token");

    public final String description;

    private ParsingError(String error) {
        this.description = error;
    }
}