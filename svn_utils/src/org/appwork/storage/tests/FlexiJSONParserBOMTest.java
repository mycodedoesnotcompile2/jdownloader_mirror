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
package org.appwork.storage.tests;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;

import org.appwork.storage.flexijson.FlexiJSONParser;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.testframework.AWTest;

/**
 * Test for BOM handling in FlexiJSONParser constructors
 *
 * @author thomas
 * @date 2024
 */
public class FlexiJSONParserBOMTest extends AWTest {
    public static void main(String[] args) {
        run();
    }

    public void runTest() throws Exception {
        System.out.println("Testing BOM handling in FlexiJSONParser...");
        // Test JSON content
        String jsonContent = "{\"test\": \"value\"}";
        // Test different BOM types
        String jsonWithUnicodeBOM = "\uFEFF" + jsonContent; // Unicode BOM (U+FEFF)
        // Test String constructor
        testStringConstructor(jsonContent, jsonWithUnicodeBOM);
        // Test CharSequence constructor
        testCharSequenceConstructor(jsonContent, jsonWithUnicodeBOM);
        // Test Reader constructor
        testReaderConstructor(jsonContent, jsonWithUnicodeBOM);
        // Test InputStream constructor with different BOM types
        testInputStreamConstructorWithDifferentBOMs(jsonContent);
        System.out.println("All BOM tests passed!");
    }

    private void testStringConstructor(String jsonContent, String jsonWithBOM) throws Exception {
        System.out.println("Testing String constructor...");
        // Test without BOM
        FlexiJSONParser parser1 = new FlexiJSONParser(jsonContent);
        FlexiJSonNode result1 = parser1.parse();
        assert result1 != null : "Parser should parse JSON without BOM";
        // Test with BOM
        FlexiJSONParser parser2 = new FlexiJSONParser(jsonWithBOM);
        FlexiJSonNode result2 = parser2.parse();
        assert result2 != null : "Parser should parse JSON with BOM";
        // Both should produce the same result
        assert result1.toString().equals(result2.toString()) : "Results should be identical";
        System.out.println("String constructor BOM test passed");
    }

    private void testCharSequenceConstructor(String jsonContent, String jsonWithBOM) throws Exception {
        System.out.println("Testing CharSequence constructor...");
        // Test without BOM
        FlexiJSONParser parser1 = new FlexiJSONParser((CharSequence) jsonContent);
        FlexiJSonNode result1 = parser1.parse();
        assert result1 != null : "Parser should parse JSON without BOM";
        // Test with BOM
        FlexiJSONParser parser2 = new FlexiJSONParser((CharSequence) jsonWithBOM);
        FlexiJSonNode result2 = parser2.parse();
        assert result2 != null : "Parser should parse JSON with BOM";
        // Both should produce the same result
        assert result1.toString().equals(result2.toString()) : "Results should be identical";
        System.out.println("CharSequence constructor BOM test passed");
    }

    private void testReaderConstructor(String jsonContent, String jsonWithBOM) throws Exception {
        System.out.println("Testing Reader constructor...");
        // Test without BOM
        Reader reader1 = new StringReader(jsonContent);
        FlexiJSONParser parser1 = new FlexiJSONParser(reader1);
        FlexiJSonNode result1 = parser1.parse();
        assert result1 != null : "Parser should parse JSON without BOM";
        // Test with BOM
        Reader reader2 = new StringReader(jsonWithBOM);
        FlexiJSONParser parser2 = new FlexiJSONParser(reader2);
        FlexiJSonNode result2 = parser2.parse();
        assert result2 != null : "Parser should parse JSON with BOM";
        // Both should produce the same result
        assert result1.toString().equals(result2.toString()) : "Results should be identical";
        System.out.println("Reader constructor BOM test passed");
    }

    private void testInputStreamConstructorWithDifferentBOMs(String jsonContent) throws Exception {
        System.out.println("Testing InputStream constructor with different BOM types...");
        // Test without BOM
        InputStream is1 = new ByteArrayInputStream(jsonContent.getBytes(StandardCharsets.UTF_8));
        FlexiJSONParser parser1 = new FlexiJSONParser(is1);
        FlexiJSonNode result1 = parser1.parse();
        assert result1 != null : "Parser should parse JSON without BOM";
        // Test with UTF-8 BOM (EF BB BF)
        byte[] utf8BOM = { (byte) 0xEF, (byte) 0xBB, (byte) 0xBF };
        byte[] jsonWithUTF8BOM = new byte[utf8BOM.length + jsonContent.getBytes(StandardCharsets.UTF_8).length];
        System.arraycopy(utf8BOM, 0, jsonWithUTF8BOM, 0, utf8BOM.length);
        System.arraycopy(jsonContent.getBytes(StandardCharsets.UTF_8), 0, jsonWithUTF8BOM, utf8BOM.length, jsonContent.getBytes(StandardCharsets.UTF_8).length);
        InputStream is2 = new ByteArrayInputStream(jsonWithUTF8BOM);
        FlexiJSONParser parser2 = new FlexiJSONParser(is2);
        FlexiJSonNode result2 = parser2.parse();
        assert result2 != null : "Parser should parse JSON with UTF-8 BOM";
        // Both should produce the same result
        assert result1.toString().equals(result2.toString()) : "Results should be identical";
        // Test with UTF-16 BE BOM (FE FF)
        byte[] utf16BEBOM = { (byte) 0xFE, (byte) 0xFF };
        byte[] jsonWithUTF16BEBOM = new byte[utf16BEBOM.length + jsonContent.getBytes(StandardCharsets.UTF_16BE).length];
        System.arraycopy(utf16BEBOM, 0, jsonWithUTF16BEBOM, 0, utf16BEBOM.length);
        System.arraycopy(jsonContent.getBytes(StandardCharsets.UTF_16BE), 0, jsonWithUTF16BEBOM, utf16BEBOM.length, jsonContent.getBytes(StandardCharsets.UTF_16BE).length);
        InputStream is3 = new ByteArrayInputStream(jsonWithUTF16BEBOM);
        FlexiJSONParser parser3 = new FlexiJSONParser(is3);
        FlexiJSonNode result3 = parser3.parse();
        assert result3 != null : "Parser should parse JSON with UTF-16 BE BOM";
        // Both should produce the same result
        assert result1.toString().equals(result3.toString()) : "Results should be identical";
        // Test with UTF-16 LE BOM (FF FE)
        byte[] utf16LEBOM = { (byte) 0xFF, (byte) 0xFE };
        byte[] jsonWithUTF16LEBOM = new byte[utf16LEBOM.length + jsonContent.getBytes(StandardCharsets.UTF_16LE).length];
        System.arraycopy(utf16LEBOM, 0, jsonWithUTF16LEBOM, 0, utf16LEBOM.length);
        System.arraycopy(jsonContent.getBytes(StandardCharsets.UTF_16LE), 0, jsonWithUTF16LEBOM, utf16LEBOM.length, jsonContent.getBytes(StandardCharsets.UTF_16LE).length);
        InputStream is4 = new ByteArrayInputStream(jsonWithUTF16LEBOM);
        FlexiJSONParser parser4 = new FlexiJSONParser(is4);
        FlexiJSonNode result4 = parser4.parse();
        assert result4 != null : "Parser should parse JSON with UTF-16 LE BOM";
        // Both should produce the same result
        assert result1.toString().equals(result4.toString()) : "Results should be identical";
        System.out.println("InputStream constructor BOM test passed for all BOM types");
    }
}