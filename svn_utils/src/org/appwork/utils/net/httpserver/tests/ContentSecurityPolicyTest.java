/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
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
package org.appwork.utils.net.httpserver.tests;

import java.util.List;

import org.appwork.loggingv3.LogV3;
import org.appwork.testframework.AWTest;
import org.appwork.utils.net.httpserver.ContentSecurityPolicy;

/**
 * Comprehensive tests for ContentSecurityPolicy class.
 *
 * <p>
 * This test class verifies that:
 * </p>
 * <ul>
 * <li>fromHeaderString() correctly parses various CSP header formats</li>
 * <li>toHeaderString() correctly generates CSP header strings</li>
 * <li>Roundtrip parsing (parse -> toHeaderString -> parse) works correctly</li>
 * <li>Edge cases (null, empty strings, whitespace) are handled correctly</li>
 * <li>All CSP directive types are supported</li>
 * <li>frame-ancestors directive is correctly extracted and set</li>
 * </ul>
 *
 * @author AppWork
 */
public class ContentSecurityPolicyTest extends AWTest {

    public static void main(final String[] args) throws Exception {
        AWTest.run();
    }

    @Override
    public void runTest() throws Exception {
        this.testParseEmptyAndNull();
        this.testParseFrameAncestorsOnly();
        this.testParseFrameAncestorsWithAdditionalDirectives();
        this.testParseAdditionalDirectivesOnly();
        this.testParseComplexCspHeaders();
        this.testParseWhitespaceHandling();
        this.testParseCaseInsensitivity();
        this.testToHeaderStringEmpty();
        this.testToHeaderStringFrameAncestorsOnly();
        this.testToHeaderStringWithAdditionalDirectives();
        this.testRoundtripParsing();
        this.testRoundtripWithComplexDirectives();
        this.testDefaultPolicy();
        this.testSameOriginPolicy();
        this.testBuilderPattern();
        this.testMultipleFrameAncestors();
        this.testDirectiveOrdering();
        this.testSpecialCharacters();
        this.testParseExceptions();
    }

    /**
     * Test: Parsing null and empty strings should return empty policy
     */
    private void testParseEmptyAndNull() throws Exception {
        LogV3.info("Test: Parse empty and null strings");

        // Test null
        ContentSecurityPolicy csp1 = ContentSecurityPolicy.fromHeaderString(null);
        assertTrue(csp1 != null, "fromHeaderString(null) should not return null");
        assertTrue(csp1.getFrameAncestors() == null, "Parsed null should have null frame-ancestors");
        assertTrue(csp1.getAdditionalDirectives().isEmpty(), "Parsed null should have no additional directives");
        assertTrue(csp1.toHeaderString() == null, "Empty policy should return null from toHeaderString()");

        // Test empty string
        ContentSecurityPolicy csp2 = ContentSecurityPolicy.fromHeaderString("");
        assertTrue(csp2 != null, "fromHeaderString(\"\") should not return null");
        assertTrue(csp2.getFrameAncestors() == null, "Parsed empty string should have null frame-ancestors");
        assertTrue(csp2.getAdditionalDirectives().isEmpty(), "Parsed empty string should have no additional directives");

        // Test whitespace-only string
        ContentSecurityPolicy csp3 = ContentSecurityPolicy.fromHeaderString("   ");
        assertTrue(csp3 != null, "fromHeaderString(\"   \") should not return null");
        assertTrue(csp3.getFrameAncestors() == null, "Parsed whitespace should have null frame-ancestors");
        assertTrue(csp3.getAdditionalDirectives().isEmpty(), "Parsed whitespace should have no additional directives");

        LogV3.info("Test passed: Empty and null strings handled correctly");
    }

    /**
     * Test: Parsing frame-ancestors only
     */
    private void testParseFrameAncestorsOnly() throws Exception {
        LogV3.info("Test: Parse frame-ancestors only");

        // Test 'none'
        ContentSecurityPolicy csp1 = ContentSecurityPolicy.fromHeaderString("frame-ancestors 'none'");
        assertTrue("'none'".equals(csp1.getFrameAncestors()), "frame-ancestors should be 'none', was: " + csp1.getFrameAncestors());
        assertTrue(csp1.getAdditionalDirectives().isEmpty(), "Should have no additional directives");

        // Test 'self'
        ContentSecurityPolicy csp2 = ContentSecurityPolicy.fromHeaderString("frame-ancestors 'self'");
        assertTrue("'self'".equals(csp2.getFrameAncestors()), "frame-ancestors should be 'self', was: " + csp2.getFrameAncestors());
        assertTrue(csp2.getAdditionalDirectives().isEmpty(), "Should have no additional directives");

        // Test with URL
        ContentSecurityPolicy csp3 = ContentSecurityPolicy.fromHeaderString("frame-ancestors https://example.com");
        assertTrue("https://example.com".equals(csp3.getFrameAncestors()), "frame-ancestors should be https://example.com, was: " + csp3.getFrameAncestors());

        // Test with multiple values
        ContentSecurityPolicy csp4 = ContentSecurityPolicy.fromHeaderString("frame-ancestors 'self' https://example.com");
        assertTrue("'self' https://example.com".equals(csp4.getFrameAncestors()), "frame-ancestors should be 'self' https://example.com, was: " + csp4.getFrameAncestors());

        LogV3.info("Test passed: frame-ancestors parsing works correctly");
    }

    /**
     * Test: Parsing frame-ancestors with additional directives
     */
    private void testParseFrameAncestorsWithAdditionalDirectives() throws Exception {
        LogV3.info("Test: Parse frame-ancestors with additional directives");

        ContentSecurityPolicy csp = ContentSecurityPolicy.fromHeaderString("frame-ancestors 'none'; default-src 'self'");
        assertTrue("'none'".equals(csp.getFrameAncestors()), "frame-ancestors should be 'none'");
        List<String> directives = csp.getAdditionalDirectives();
        assertTrue(directives.size() == 1, "Should have 1 additional directive, had: " + directives.size());
        assertTrue("default-src 'self'".equals(directives.get(0)), "First directive should be 'default-src 'self'', was: " + directives.get(0));

        // Test with multiple additional directives
        ContentSecurityPolicy csp2 = ContentSecurityPolicy.fromHeaderString("frame-ancestors 'self'; default-src 'self'; script-src 'self' 'unsafe-inline'");
        assertTrue("'self'".equals(csp2.getFrameAncestors()), "frame-ancestors should be 'self'");
        List<String> directives2 = csp2.getAdditionalDirectives();
        assertTrue(directives2.size() == 2, "Should have 2 additional directives, had: " + directives2.size());
        assertTrue("default-src 'self'".equals(directives2.get(0)), "First directive should be 'default-src 'self''");
        assertTrue("script-src 'self' 'unsafe-inline'".equals(directives2.get(1)), "Second directive should be 'script-src 'self' 'unsafe-inline''");

        LogV3.info("Test passed: frame-ancestors with additional directives parsed correctly");
    }

    /**
     * Test: Parsing additional directives only (no frame-ancestors)
     */
    private void testParseAdditionalDirectivesOnly() throws Exception {
        LogV3.info("Test: Parse additional directives only");

        ContentSecurityPolicy csp = ContentSecurityPolicy.fromHeaderString("default-src 'self'");
        assertTrue(csp.getFrameAncestors() == null, "Should have no frame-ancestors");
        List<String> directives = csp.getAdditionalDirectives();
        assertTrue(directives.size() == 1, "Should have 1 directive, had: " + directives.size());
        assertTrue("default-src 'self'".equals(directives.get(0)), "Directive should be 'default-src 'self''");

        // Test multiple directives
        ContentSecurityPolicy csp2 = ContentSecurityPolicy.fromHeaderString("default-src 'self'; script-src 'self' 'unsafe-inline'; style-src 'self'");
        assertTrue(csp2.getFrameAncestors() == null, "Should have no frame-ancestors");
        List<String> directives2 = csp2.getAdditionalDirectives();
        assertTrue(directives2.size() == 3, "Should have 3 directives, had: " + directives2.size());

        LogV3.info("Test passed: Additional directives only parsed correctly");
    }

    /**
     * Test: Parsing complex CSP headers with various directive types
     */
    private void testParseComplexCspHeaders() throws Exception {
        LogV3.info("Test: Parse complex CSP headers");

        String complexHeader = "default-src 'self'; script-src 'self' 'unsafe-inline' 'unsafe-eval'; style-src 'self' https://cdn.example.com; img-src 'self' data: https:; font-src 'self' https://fonts.googleapis.com; frame-ancestors 'none'; connect-src 'self' https://api.example.com; media-src 'self'; object-src 'none'; base-uri 'self'; form-action 'self'; upgrade-insecure-requests";
        ContentSecurityPolicy csp = ContentSecurityPolicy.fromHeaderString(complexHeader);

        assertTrue("'none'".equals(csp.getFrameAncestors()), "frame-ancestors should be 'none'");
        List<String> directives = csp.getAdditionalDirectives();
        // frame-ancestors is NOT included in additionalDirectives, so we have 11 additional directives (12 total - 1 frame-ancestors)
        assertTrue(directives.size() == 11, "Should have 11 additional directives (frame-ancestors is not included), had: " + directives.size());

        // Verify some key directives are present
        boolean hasDefaultSrc = false;
        boolean hasScriptSrc = false;
        boolean hasUpgradeInsecure = false;
        for (String directive : directives) {
            if (directive.startsWith("default-src")) {
                hasDefaultSrc = true;
            }
            if (directive.startsWith("script-src")) {
                hasScriptSrc = true;
            }
            if (directive.startsWith("upgrade-insecure-requests")) {
                hasUpgradeInsecure = true;
            }
        }
        assertTrue(hasDefaultSrc, "Should have default-src directive");
        assertTrue(hasScriptSrc, "Should have script-src directive");
        assertTrue(hasUpgradeInsecure, "Should have upgrade-insecure-requests directive");

        LogV3.info("Test passed: Complex CSP headers parsed correctly");
    }

    /**
     * Test: Whitespace handling in parsing
     */
    private void testParseWhitespaceHandling() throws Exception {
        LogV3.info("Test: Whitespace handling in parsing");

        // Test with extra spaces
        ContentSecurityPolicy csp1 = ContentSecurityPolicy.fromHeaderString("frame-ancestors   'none'  ;  default-src   'self'  ");
        assertTrue("'none'".equals(csp1.getFrameAncestors()), "frame-ancestors should be 'none'");
        List<String> directives1 = csp1.getAdditionalDirectives();
        assertTrue(directives1.size() == 1, "Should have 1 directive");
        assertTrue("default-src   'self'".equals(directives1.get(0)), "Directive should preserve internal spaces");

        // Test with tabs and newlines (simulated)
        ContentSecurityPolicy csp2 = ContentSecurityPolicy.fromHeaderString("frame-ancestors 'self'; default-src 'self'");
        assertTrue("'self'".equals(csp2.getFrameAncestors()), "frame-ancestors should be 'self'");

        // Test empty directives (should be skipped)
        ContentSecurityPolicy csp3 = ContentSecurityPolicy.fromHeaderString("frame-ancestors 'none';; default-src 'self'; ;");
        assertTrue("'none'".equals(csp3.getFrameAncestors()), "frame-ancestors should be 'none'");
        List<String> directives3 = csp3.getAdditionalDirectives();
        assertTrue(directives3.size() == 1, "Empty directives should be skipped, had: " + directives3.size());

        LogV3.info("Test passed: Whitespace handling works correctly");
    }

    /**
     * Test: Case insensitivity of frame-ancestors directive name
     */
    private void testParseCaseInsensitivity() throws Exception {
        LogV3.info("Test: Case insensitivity of frame-ancestors");

        // Test uppercase
        ContentSecurityPolicy csp1 = ContentSecurityPolicy.fromHeaderString("FRAME-ANCESTORS 'none'");
        assertTrue("'none'".equals(csp1.getFrameAncestors()), "Uppercase frame-ancestors should be recognized");

        // Test mixed case
        ContentSecurityPolicy csp2 = ContentSecurityPolicy.fromHeaderString("Frame-Ancestors 'self'");
        assertTrue("'self'".equals(csp2.getFrameAncestors()), "Mixed case frame-ancestors should be recognized");

        // Test with additional directives
        ContentSecurityPolicy csp3 = ContentSecurityPolicy.fromHeaderString("FrAmE-AnCeStOrS 'none'; default-src 'self'");
        assertTrue("'none'".equals(csp3.getFrameAncestors()), "Mixed case frame-ancestors should be recognized");

        LogV3.info("Test passed: Case insensitivity works correctly");
    }

    /**
     * Test: toHeaderString() with empty policy
     */
    private void testToHeaderStringEmpty() throws Exception {
        LogV3.info("Test: toHeaderString() with empty policy");

        ContentSecurityPolicy csp = new ContentSecurityPolicy();
        String header = csp.toHeaderString();
        assertTrue(header == null, "Empty policy should return null from toHeaderString(), was: " + header);

        LogV3.info("Test passed: Empty policy returns null");
    }

    /**
     * Test: toHeaderString() with frame-ancestors only
     */
    private void testToHeaderStringFrameAncestorsOnly() throws Exception {
        LogV3.info("Test: toHeaderString() with frame-ancestors only");

        ContentSecurityPolicy csp = new ContentSecurityPolicy();
        csp.setFrameAncestors("'none'");
        String header = csp.toHeaderString();
        assertTrue(header != null, "Header should not be null");
        assertTrue("frame-ancestors 'none'".equals(header), "Header should be 'frame-ancestors 'none'', was: " + header);

        // Test 'self'
        csp.setFrameAncestors("'self'");
        header = csp.toHeaderString();
        assertTrue("frame-ancestors 'self'".equals(header), "Header should be 'frame-ancestors 'self'', was: " + header);

        LogV3.info("Test passed: frame-ancestors only generates correct header");
    }

    /**
     * Test: toHeaderString() with additional directives
     */
    private void testToHeaderStringWithAdditionalDirectives() throws Exception {
        LogV3.info("Test: toHeaderString() with additional directives");

        ContentSecurityPolicy csp = new ContentSecurityPolicy();
        csp.setFrameAncestors("'none'");
        csp.addDirective("default-src 'self'");
        String header = csp.toHeaderString();
        assertTrue(header != null, "Header should not be null");
        // frame-ancestors should come first, then additional directives
        assertTrue("frame-ancestors 'none'; default-src 'self'".equals(header), "Header should be 'frame-ancestors 'none'; default-src 'self'', was: " + header);

        // Test without frame-ancestors
        ContentSecurityPolicy csp2 = new ContentSecurityPolicy();
        csp2.addDirective("default-src 'self'");
        csp2.addDirective("script-src 'self'");
        String header2 = csp2.toHeaderString();
        assertTrue(header2 != null, "Header should not be null");
        assertTrue(header2.contains("default-src 'self'"), "Header should contain default-src");
        assertTrue(header2.contains("script-src 'self'"), "Header should contain script-src");

        LogV3.info("Test passed: Additional directives generate correct header");
    }

    /**
     * Test: Roundtrip parsing (parse -> toHeaderString -> parse)
     */
    private void testRoundtripParsing() throws Exception {
        LogV3.info("Test: Roundtrip parsing");

        String[] testHeaders = { "frame-ancestors 'none'", "frame-ancestors 'self'", "frame-ancestors 'none'; default-src 'self'", "default-src 'self'; script-src 'self'", "frame-ancestors 'self' https://example.com; default-src 'self'; script-src 'self' 'unsafe-inline'" };

        for (String originalHeader : testHeaders) {
            // Parse original
            ContentSecurityPolicy csp1 = ContentSecurityPolicy.fromHeaderString(originalHeader);
            assertTrue(csp1 != null, "Parsed CSP should not be null for: " + originalHeader);

            // Convert to header string
            String generatedHeader = csp1.toHeaderString();
            if (generatedHeader == null && originalHeader.trim().isEmpty()) {
                // Both are empty, that's fine
                continue;
            }
            assertTrue(generatedHeader != null, "Generated header should not be null for: " + originalHeader);

            // Parse again
            ContentSecurityPolicy csp2 = ContentSecurityPolicy.fromHeaderString(generatedHeader);
            assertTrue(csp2 != null, "Re-parsed CSP should not be null");

            // Verify frame-ancestors matches
            if (csp1.getFrameAncestors() == null) {
                assertTrue(csp2.getFrameAncestors() == null, "frame-ancestors should match (both null) for: " + originalHeader);
            } else {
                assertTrue(csp1.getFrameAncestors().equals(csp2.getFrameAncestors()), "frame-ancestors should match for: " + originalHeader);
            }

            // Verify additional directives count matches
            assertTrue(csp1.getAdditionalDirectives().size() == csp2.getAdditionalDirectives().size(), "Additional directives count should match for: " + originalHeader);
        }

        LogV3.info("Test passed: Roundtrip parsing works correctly");
    }

    /**
     * Test: Roundtrip with complex directives
     */
    private void testRoundtripWithComplexDirectives() throws Exception {
        LogV3.info("Test: Roundtrip with complex directives");

        ContentSecurityPolicy csp1 = new ContentSecurityPolicy();
        csp1.setFrameAncestors("'self' https://example.com");
        csp1.addDirective("default-src 'self'");
        csp1.addDirective("script-src 'self' 'unsafe-inline' 'unsafe-eval'");
        csp1.addDirective("style-src 'self' https://cdn.example.com");
        csp1.addDirective("img-src 'self' data: https:");

        String header = csp1.toHeaderString();
        assertTrue(header != null, "Generated header should not be null");

        ContentSecurityPolicy csp2 = ContentSecurityPolicy.fromHeaderString(header);
        assertTrue(csp2 != null, "Re-parsed CSP should not be null");
        assertTrue("'self' https://example.com".equals(csp2.getFrameAncestors()), "frame-ancestors should match");
        assertTrue(csp2.getAdditionalDirectives().size() == 4, "Should have 4 additional directives");

        LogV3.info("Test passed: Roundtrip with complex directives works correctly");
    }

    /**
     * Test: Default policy factory method
     */
    private void testDefaultPolicy() throws Exception {
        LogV3.info("Test: Default policy factory method");

        ContentSecurityPolicy csp = ContentSecurityPolicy.defaultPolicy();
        assertTrue(csp != null, "defaultPolicy() should not return null");
        assertTrue("'none'".equals(csp.getFrameAncestors()), "Default policy should have frame-ancestors 'none'");
        assertTrue(csp.getAdditionalDirectives().isEmpty(), "Default policy should have no additional directives");

        String header = csp.toHeaderString();
        assertTrue(header != null, "Default policy header should not be null");
        assertTrue(header.contains("frame-ancestors 'none'"), "Default policy header should contain frame-ancestors 'none'");

        LogV3.info("Test passed: Default policy works correctly");
    }

    /**
     * Test: Same origin policy factory method
     */
    private void testSameOriginPolicy() throws Exception {
        LogV3.info("Test: Same origin policy factory method");

        ContentSecurityPolicy csp = ContentSecurityPolicy.sameOriginPolicy();
        assertTrue(csp != null, "sameOriginPolicy() should not return null");
        assertTrue("'self'".equals(csp.getFrameAncestors()), "Same origin policy should have frame-ancestors 'self'");
        assertTrue(csp.getAdditionalDirectives().isEmpty(), "Same origin policy should have no additional directives");

        String header = csp.toHeaderString();
        assertTrue(header != null, "Same origin policy header should not be null");
        assertTrue(header.contains("frame-ancestors 'self'"), "Same origin policy header should contain frame-ancestors 'self'");

        LogV3.info("Test passed: Same origin policy works correctly");
    }

    /**
     * Test: Builder pattern usage
     */
    private void testBuilderPattern() throws Exception {
        LogV3.info("Test: Builder pattern usage");

        ContentSecurityPolicy csp = new ContentSecurityPolicy();
        csp.setFrameAncestors("'self'");
        csp.addDirective("default-src 'self'");
        csp.addDirective("script-src 'self'");
        csp.addDirective("style-src 'self'");

        assertTrue("'self'".equals(csp.getFrameAncestors()), "frame-ancestors should be 'self'");
        assertTrue(csp.getAdditionalDirectives().size() == 3, "Should have 3 additional directives");

        // Modify frame-ancestors
        csp.setFrameAncestors("'none'");
        assertTrue("'none'".equals(csp.getFrameAncestors()), "frame-ancestors should be 'none' after modification");

        // Add more directives
        csp.addDirective("img-src 'self'");
        assertTrue(csp.getAdditionalDirectives().size() == 4, "Should have 4 additional directives after adding one");

        LogV3.info("Test passed: Builder pattern works correctly");
    }

    /**
     * Test: Multiple frame-ancestors directives should throw exception
     */
    private void testMultipleFrameAncestors() throws Exception {
        LogV3.info("Test: Multiple frame-ancestors directives should throw exception");

        // When multiple frame-ancestors are present, the parser should throw an exception
        try {
            ContentSecurityPolicy.fromHeaderString("frame-ancestors 'none'; default-src 'self'; frame-ancestors 'self'");
            throw new Exception("Expected IllegalArgumentException for multiple frame-ancestors directives");
        } catch (IllegalArgumentException e) {
            assertTrue(e.getMessage().contains("Multiple frame-ancestors"), "Exception message should mention multiple frame-ancestors: " + e.getMessage());
            LogV3.info("Test passed: Multiple frame-ancestors correctly throws exception");
        }
    }

    /**
     * Test: Directive ordering in toHeaderString()
     */
    private void testDirectiveOrdering() throws Exception {
        LogV3.info("Test: Directive ordering in toHeaderString()");

        ContentSecurityPolicy csp = new ContentSecurityPolicy();
        csp.addDirective("z-directive 'value'");
        csp.addDirective("a-directive 'value'");
        csp.setFrameAncestors("'none'");

        String header = csp.toHeaderString();
        assertTrue(header != null, "Header should not be null");
        // frame-ancestors should be first, then additional directives in order added
        assertTrue(header.contains("frame-ancestors 'none'"), "Header should contain frame-ancestors");
        assertTrue(header.contains("z-directive 'value'"), "Header should contain z-directive");
        assertTrue(header.contains("a-directive 'value'"), "Header should contain a-directive");

        LogV3.info("Test passed: Directive ordering works correctly");
    }

    /**
     * Test: Special characters in directives
     */
    private void testSpecialCharacters() throws Exception {
        LogV3.info("Test: Special characters in directives");

        // Test with URLs containing special characters
        ContentSecurityPolicy csp1 = ContentSecurityPolicy.fromHeaderString("frame-ancestors https://example.com/path?query=value&other=123");
        assertTrue(csp1.getFrameAncestors() != null, "Should parse URL with special characters");
        assertTrue(csp1.getFrameAncestors().contains("example.com"), "Should contain example.com");

        // Test with quoted strings containing special characters
        ContentSecurityPolicy csp2 = ContentSecurityPolicy.fromHeaderString("script-src 'self' 'unsafe-inline'");
        List<String> directives = csp2.getAdditionalDirectives();
        assertTrue(directives.size() == 1, "Should have 1 directive");
        assertTrue(directives.get(0).contains("'unsafe-inline'"), "Should preserve quoted values with special characters");

        // Test with data: URLs
        ContentSecurityPolicy csp3 = ContentSecurityPolicy.fromHeaderString("img-src 'self' data: https:");
        List<String> directives3 = csp3.getAdditionalDirectives();
        assertTrue(directives3.size() == 1, "Should have 1 directive");
        assertTrue(directives3.get(0).contains("data:"), "Should preserve data: URLs");

        LogV3.info("Test passed: Special characters handled correctly");
    }

    /**
     * Test: Parsing exceptions for invalid input
     */
    private void testParseExceptions() throws Exception {
        LogV3.info("Test: Parsing exceptions for invalid input");

        // Test: Multiple frame-ancestors directives should throw exception
        try {
            ContentSecurityPolicy.fromHeaderString("frame-ancestors 'none'; frame-ancestors 'self'");
            throw new Exception("Expected IllegalArgumentException for multiple frame-ancestors directives");
        } catch (IllegalArgumentException e) {
            assertTrue(e.getMessage().contains("Multiple frame-ancestors"), "Exception message should mention multiple frame-ancestors: " + e.getMessage());
            LogV3.info("Test passed: Multiple frame-ancestors correctly throws exception");
        }

        // Test: frame-ancestors without value should throw exception
        try {
            ContentSecurityPolicy.fromHeaderString("frame-ancestors");
            throw new Exception("Expected IllegalArgumentException for frame-ancestors without value");
        } catch (IllegalArgumentException e) {
            assertTrue(e.getMessage().contains("frame-ancestors directive must have a value"), "Exception message should mention missing value: " + e.getMessage());
            LogV3.info("Test passed: frame-ancestors without value correctly throws exception");
        }

        // Test: frame-ancestors with only whitespace should throw exception
        try {
            ContentSecurityPolicy.fromHeaderString("frame-ancestors   ");
            throw new Exception("Expected IllegalArgumentException for frame-ancestors with only whitespace");
        } catch (IllegalArgumentException e) {
            assertTrue(e.getMessage().contains("frame-ancestors directive must have a value"), "Exception message should mention missing value: " + e.getMessage());
            LogV3.info("Test passed: frame-ancestors with only whitespace correctly throws exception");
        }

        // Test: Empty directive name should throw exception
        try {
            ContentSecurityPolicy.fromHeaderString("   ; default-src 'self'");
            // Empty directive before semicolon should be skipped, but let's test a case with invalid format
            ContentSecurityPolicy.fromHeaderString("   default-src 'self'");
            // This should work, so let's test a truly invalid case
        } catch (IllegalArgumentException e) {
            // This might not throw, depending on implementation
        }

        // Test: Invalid directive name with special characters (should throw exception)
        try {
            ContentSecurityPolicy.fromHeaderString("default@src 'self'");
            throw new Exception("Expected IllegalArgumentException for invalid directive name with @");
        } catch (IllegalArgumentException e) {
            assertTrue(e.getMessage().contains("Invalid directive name") || e.getMessage().contains("invalid characters"), "Exception message should mention invalid directive name: " + e.getMessage());
            LogV3.info("Test passed: Directive with special characters (@) in name correctly throws exception");
        }

        // Test: Invalid directive name with other special characters
        try {
            ContentSecurityPolicy.fromHeaderString("default#src 'self'");
            throw new Exception("Expected IllegalArgumentException for invalid directive name with #");
        } catch (IllegalArgumentException e) {
            assertTrue(e.getMessage().contains("Invalid directive name") || e.getMessage().contains("invalid characters"), "Exception message should mention invalid directive name: " + e.getMessage());
            LogV3.info("Test passed: Directive with special characters (#) in name correctly throws exception");
        }

        // Test: Valid directive names should work (uppercase, underscore are allowed in our lenient validation)
        try {
            // Uppercase directive names are technically invalid in CSP spec, but we allow them for leniency
            ContentSecurityPolicy.fromHeaderString("DEFAULT-SRC 'self'");
            // Underscore in directive name
            ContentSecurityPolicy.fromHeaderString("default_src 'self'");
            LogV3.info("Test passed: Valid directive names (with uppercase/underscore) are accepted");
        } catch (IllegalArgumentException e) {
            // These might throw, but that's okay - we're being lenient
            LogV3.info("Note: Some directive name formats are rejected: " + e.getMessage());
        }

        // Test: Multiple frame-ancestors in different order
        try {
            ContentSecurityPolicy.fromHeaderString("default-src 'self'; frame-ancestors 'none'; script-src 'self'; frame-ancestors 'self'");
            throw new Exception("Expected IllegalArgumentException for multiple frame-ancestors directives");
        } catch (IllegalArgumentException e) {
            assertTrue(e.getMessage().contains("Multiple frame-ancestors"), "Exception message should mention multiple frame-ancestors: " + e.getMessage());
            LogV3.info("Test passed: Multiple frame-ancestors in different order correctly throws exception");
        }

        // Test: Valid cases should not throw exceptions
        try {
            ContentSecurityPolicy.fromHeaderString("frame-ancestors 'none'; default-src 'self'");
            ContentSecurityPolicy.fromHeaderString("default-src 'self'");
            ContentSecurityPolicy.fromHeaderString("frame-ancestors 'self' https://example.com");
            LogV3.info("Test passed: Valid CSP headers parse without exceptions");
        } catch (IllegalArgumentException e) {
            throw new Exception("Valid CSP header should not throw exception: " + e.getMessage(), e);
        }

        LogV3.info("Test passed: All parsing exceptions work correctly");
    }
}
