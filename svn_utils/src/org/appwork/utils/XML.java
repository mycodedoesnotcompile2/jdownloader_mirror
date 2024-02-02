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
package org.appwork.utils;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.appwork.loggingv3.LogV3;

/**
 * @author thomas
 * @date 01.02.2024
 *
 */
public class XML {
    /**
     * @return
     */
    public static DocumentBuilderFactory newSecureFactory() {
        final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        // Enabling secure processing feature to prevent XML vulnerabilities
        try {
            factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);
        } catch (final ParserConfigurationException e) {
            LogV3.exception(XML.class, e);
        }
        // Disallowing Doctype Declarations to prevent XML External Entity (XXE) attacks
        try {
            factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
        } catch (final ParserConfigurationException e) {
            LogV3.exception(XML.class, e);
        }
        // Disabling external general entities to enhance security against XXE
        try {
            factory.setFeature("http://xml.org/sax/features/external-general-entities", false);
        } catch (final ParserConfigurationException e) {
            LogV3.exception(XML.class, e);
        }
        // Disabling external parameter entities for additional security
        try {
            factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false);
        } catch (final ParserConfigurationException e) {
            LogV3.exception(XML.class, e);
        }
        // Preventing the loading of external DTDs to secure against XXE attacks
        try {
            factory.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
        } catch (final ParserConfigurationException e) {
            LogV3.exception(XML.class, e);
        }
        // Setting the factory to not be XInclude-aware and not to validate XML documents
        factory.setXIncludeAware(false);
        // Setting the factory to not validate XML documents against DTDs
        factory.setValidating(false);
        return factory;
    }
}
